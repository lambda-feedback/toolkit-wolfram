(* ::Package:: *)

(* ---- Shared, transport-agnostic execution core ----
   These functions call the user's eval/preview functions safely and return a
   normalized outcome association (`"ok" -> True/False`), independent of how
   the calling transport eventually formats that outcome on the wire. *)

(* Bound (seconds) on how long a single eval/preview call may run, before
   shimmy's own RPC-level timeout would otherwise give up on it and leave
   the (persistent, shared) dedicated worker wedged on the still-running
   call for every later request -- shimmy never recycles a worker after a
   Send timeout, so a runaway computation (e.g. Simplify/FullSimplify on an
   expression with free transcendental parameters, or pathological pattern
   matching) must abort inside the kernel first.

   Derived from shimmy's own default worker-send-timeout (30s -- see
   FUNCTION_WORKER_SEND_TIMEOUT / "worker-send-timeout" in shimmy's
   cmd/root.go) minus a safety buffer, rather than picked independently, so
   the two timeouts can't silently drift apart. *)
$shimmyDefaultSendTimeout = 30;
$executionTimeoutBuffer = 5;
$defaultExecutionTimeout = $shimmyDefaultSendTimeout - $executionTimeoutBuffer;

(* Reads EVAL_EXECUTION_TIMEOUT (seconds) as an operator-facing override,
   mirroring the Environment[...] reading pattern in Dispatch.wl's
   resolveDispatchTarget. Falls back to $defaultExecutionTimeout for
   anything unset or not a positive number. *)
executionTimeout[] := Module[{raw, parsed},
  raw = Environment["EVAL_EXECUTION_TIMEOUT"];
  If[raw === $Failed || raw === "", Return[$defaultExecutionTimeout]];

  parsed = Quiet@Check[ToExpression[raw], $Failed];
  If[NumericQ[parsed] && parsed > 0, parsed, $defaultExecutionTimeout]
];

(* Catches Wolfram Messages raised by user code so a crash still produces a
   normalized failure outcome instead of propagating, and bounds execution
   time so a runaway computation can't hang the (persistent, shared) kernel
   indefinitely. *)
safeCall[fn_, args___] := Quiet@Check[
  TimeConstrained[fn[args], executionTimeout[], $TimedOut],
  $Failed
];

runEval[evalFn_, answer_, response_, params_] := Module[{result, errorMsg},
  result = safeCall[evalFn, answer, response, params];

  If[result === $TimedOut,
    Return[<| "ok" -> False, "message" -> "Evaluation function timed out" |>]
  ];

  If[result === $Failed,
    Return[<| "ok" -> False, "message" -> "Evaluation function raised an error" |>]
  ];

  errorMsg = Lookup[result, "error", Null];
  If[errorMsg =!= Null,
    Return[<| "ok" -> False, "message" -> ToString[errorMsg] |>]
  ];

  <| "ok" -> True, "data" -> <|
    "is_correct" -> result["is_correct"],
    "feedback" -> result["feedback"]
  |> |>
];

runPreview[previewFn_, response_, params_] := Module[{result},
  result = safeCall[previewFn, response, params];

  If[result === $TimedOut,
    Return[<| "ok" -> False, "message" -> "Preview function timed out" |>]
  ];

  If[result === $Failed,
    Return[<| "ok" -> False, "message" -> "Preview function raised an error" |>]
  ];

  <| "ok" -> True, "data" -> <| "preview" -> result |> |>
];
