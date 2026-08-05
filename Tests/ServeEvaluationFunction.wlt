(* ::Package:: *)

Needs["LambdaFeedback`EvaluationFunctionToolkit`"]

dispatchTransport = LambdaFeedback`EvaluationFunctionToolkit`Private`dispatchTransport;
resolveDispatchTarget = LambdaFeedback`EvaluationFunctionToolkit`Private`resolveDispatchTarget;

VerificationTest[
  dispatchTransport["FILE", ""],
  ServeFile,
  TestID -> "Dispatch-file-uppercase"
]

VerificationTest[
  dispatchTransport["", ""],
  ServeFile,
  TestID -> "Dispatch-unset-falls-back-to-file"
]

VerificationTest[
  dispatchTransport["garbage", ""],
  ServeFile,
  TestID -> "Dispatch-unrecognized-eval-io-falls-back-to-file"
]

VerificationTest[
  dispatchTransport["rpc", "tcp"],
  Serve,
  TestID -> "Dispatch-rpc-tcp"
]

VerificationTest[
  FailureQ[dispatchTransport["rpc", "stdio"]],
  True,
  TestID -> "Dispatch-rpc-stdio-not-yet-implemented"
]

(* Regression test: Failure[tag, <|"Message" -> "..."|>] does NOT make that
   text retrievable via failure["Message"] -- Failure only recognizes
   "MessageTemplate" for that. Silently building Failures with the wrong key
   meant every "FATAL: ..." exit printed a useless generic
   "A failure of type ... occurred." instead of the actual diagnostic. *)
VerificationTest[
  dispatchTransport["rpc", "stdio"]["Message"],
  "EVAL_RPC_TRANSPORT=stdio is a recognized Shimmy transport, but toolkit-wolfram does not implement it yet.",
  TestID -> "Dispatch-rpc-stdio-message-is-not-generic"
]

VerificationTest[
  FailureQ[dispatchTransport["rpc", "ipc"]],
  True,
  TestID -> "Dispatch-rpc-ipc-not-yet-implemented"
]

VerificationTest[
  dispatchTransport["rpc", "http"],
  ServeHttp,
  TestID -> "Dispatch-rpc-http"
]

VerificationTest[
  FailureQ[dispatchTransport["rpc", "ws"]],
  True,
  TestID -> "Dispatch-rpc-ws-not-yet-implemented"
]

VerificationTest[
  FailureQ[dispatchTransport["rpc", "bogus"]],
  True,
  TestID -> "Dispatch-rpc-unknown-transport"
]

VerificationTest[
  FailureQ[dispatchTransport["rpc", ""]],
  True,
  TestID -> "Dispatch-rpc-missing-transport"
]

(* Exercises Environment[] reading (resolveDispatchTarget), without invoking
   Serve/ServeFile. Restores the vars it touches afterward since Tests/*.wlt
   run in one shared kernel session per build-and-test.yml. *)
withEnv[vars_List, testFn_] := Module[{saved, result},
  saved = Environment /@ vars[[All, 1]];
  Scan[SetEnvironment[#[[1]] -> #[[2]]] &, vars];
  result = testFn[];
  MapThread[
    SetEnvironment[#1 -> If[#2 === $Failed, "", #2]] &,
    {vars[[All, 1]], saved}
  ];
  result
];

VerificationTest[
  withEnv[{"EVAL_IO" -> "rpc", "EVAL_RPC_TRANSPORT" -> "tcp"}, resolveDispatchTarget],
  Serve,
  TestID -> "Dispatch-resolveDispatchTarget-reads-environment"
]
