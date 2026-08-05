(* ::Package:: *)

(* ---- Shared, transport-agnostic execution core ----
   These functions call the user's eval/preview functions safely and return a
   normalized outcome association (`"ok" -> True/False`), independent of how
   the calling transport eventually formats that outcome on the wire. *)

(* Catches Wolfram Messages raised by user code so a crash still produces a
   normalized failure outcome instead of propagating. *)
safeCall[fn_, args___] := Quiet@Check[fn[args], $Failed];

runEval[evalFn_, answer_, response_, params_] := Module[{result, errorMsg},
  result = safeCall[evalFn, answer, response, params];

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

  If[result === $Failed,
    Return[<| "ok" -> False, "message" -> "Preview function raised an error" |>]
  ];

  <| "ok" -> True, "data" -> <| "preview" -> result |> |>
];
