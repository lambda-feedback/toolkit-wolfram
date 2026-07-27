(* ::Package:: *)

Needs["LambdaFeedback`EvaluationFunctionToolkit`"]

evalOk[answer_, response_, params_] := <|
  "is_correct" -> True, "feedback" -> "Correct!", "error" -> Null
|>;

evalFail[answer_, response_, params_] := <|
  "is_correct" -> False, "feedback" -> "", "error" -> "bad answer"
|>;

evalCrash[answer_, response_, params_] := 1/0;

previewOk[response_, params_] := <|"latex" -> "x^2", "sympy" -> "x**2"|>;

previewNestedError[response_, params_] := <|
  "error" -> <|"message" -> "Failed to parse"|>
|>;

withRequestResponse[requestAssoc_, testFn_] := Module[
  {requestPath, responsePath, result},
  requestPath = FileNameJoin[{$TemporaryDirectory, "servefile-test-request-" <> ToString[RandomInteger[10^9]] <> ".json"}];
  responsePath = FileNameJoin[{$TemporaryDirectory, "servefile-test-response-" <> ToString[RandomInteger[10^9]] <> ".json"}];
  Export[requestPath, requestAssoc, "JSON", "Compact" -> True];
  result = testFn[requestPath, responsePath];
  Quiet[DeleteFile[{requestPath, responsePath}]];
  result
];

VerificationTest[
  withRequestResponse[
    <|"command" -> "eval", "params" -> <|"answer" -> "x", "response" -> "x", "params" -> <||>|>|>,
    Function[{req, resp},
      ServeFile[evalOk, previewOk, req, resp];
      Import[resp, "RawJSON"]
    ]
  ],
  <|"command" -> "eval", "result" -> <|"is_correct" -> True, "feedback" -> "Correct!"|>|>,
  TestID -> "ServeFile-eval-success"
]

VerificationTest[
  withRequestResponse[
    <|"command" -> "eval", "params" -> <|"answer" -> "x", "response" -> "y", "params" -> <||>|>|>,
    Function[{req, resp},
      ServeFile[evalFail, previewOk, req, resp];
      Import[resp, "RawJSON"]
    ]
  ],
  <|"command" -> "eval", "error" -> <|"message" -> "bad answer"|>|>,
  TestID -> "ServeFile-eval-error"
]

VerificationTest[
  withRequestResponse[
    <|"command" -> "eval", "params" -> <|"answer" -> "x", "response" -> "y", "params" -> <||>|>|>,
    Function[{req, resp},
      ServeFile[evalCrash, previewOk, req, resp];
      Import[resp, "RawJSON"]
    ]
  ],
  <|"command" -> "eval", "error" -> <|"message" -> "Evaluation function raised an error"|>|>,
  TestID -> "ServeFile-eval-crash-is-caught"
]

VerificationTest[
  withRequestResponse[
    <|"command" -> "preview", "params" -> <|"response" -> "x^2"|>|>,
    Function[{req, resp},
      ServeFile[evalOk, previewOk, req, resp];
      Import[resp, "RawJSON"]
    ]
  ],
  <|"command" -> "preview", "result" -> <|"preview" -> <|"latex" -> "x^2", "sympy" -> "x**2"|>|>|>,
  TestID -> "ServeFile-preview-success"
]

VerificationTest[
  withRequestResponse[
    <|"command" -> "preview", "params" -> <|"response" -> "x^2"|>|>,
    Function[{req, resp},
      ServeFile[evalOk, previewNestedError, req, resp];
      Import[resp, "RawJSON"]
    ]
  ],
  <|"command" -> "preview", "result" -> <|"preview" -> <|"error" -> <|"message" -> "Failed to parse"|>|>|>|>,
  TestID -> "ServeFile-preview-nested-error-is-passthrough"
]

VerificationTest[
  withRequestResponse[
    <|"command" -> "frobnicate", "params" -> <||>|>,
    Function[{req, resp},
      ServeFile[evalOk, previewOk, req, resp];
      Import[resp, "RawJSON"]
    ]
  ],
  <|"command" -> "frobnicate", "error" -> <|"message" -> "Unknown command: frobnicate"|>|>,
  TestID -> "ServeFile-unknown-command"
]
