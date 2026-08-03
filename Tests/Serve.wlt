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

handleRPC[evalFn_, previewFn_, requestAssoc_] := Module[{requestStr},
  requestStr = ExportString[requestAssoc, "JSON", "Compact" -> True];
  LambdaFeedback`EvaluationFunctionToolkit`Private`handleRequest[evalFn, previewFn, requestStr]
];

VerificationTest[
  handleRPC[
    evalOk, previewOk,
    <|"jsonrpc" -> "2.0", "method" -> "eval", "id" -> 1,
      "params" -> {<|"answer" -> "x", "response" -> "x", "params" -> <||>|>}|>
  ],
  <|"jsonrpc" -> "2.0", "result" -> <|"is_correct" -> True, "feedback" -> "Correct!"|>, "id" -> 1|>,
  TestID -> "Serve-eval-success"
]

VerificationTest[
  handleRPC[
    evalFail, previewOk,
    <|"jsonrpc" -> "2.0", "method" -> "eval", "id" -> 2,
      "params" -> {<|"answer" -> "x", "response" -> "y", "params" -> <||>|>}|>
  ],
  <|"jsonrpc" -> "2.0", "error" -> <|"code" -> -32000, "message" -> "bad answer"|>, "id" -> 2|>,
  TestID -> "Serve-eval-domain-error-not-nested-in-result"
]

VerificationTest[
  handleRPC[
    evalCrash, previewOk,
    <|"jsonrpc" -> "2.0", "method" -> "eval", "id" -> 3,
      "params" -> {<|"answer" -> "x", "response" -> "y", "params" -> <||>|>}|>
  ],
  <|"jsonrpc" -> "2.0", "error" -> <|"code" -> -32000, "message" -> "Evaluation function raised an error"|>, "id" -> 3|>,
  TestID -> "Serve-eval-crash-is-caught"
]

VerificationTest[
  handleRPC[
    evalOk, previewOk,
    <|"jsonrpc" -> "2.0", "method" -> "preview", "id" -> 4,
      "params" -> {<|"response" -> "x^2", "params" -> <||>|>}|>
  ],
  <|"jsonrpc" -> "2.0", "result" -> <|"preview" -> <|"latex" -> "x^2", "sympy" -> "x**2"|>|>, "id" -> 4|>,
  TestID -> "Serve-preview-success"
]

VerificationTest[
  handleRPC[
    evalOk, previewOk,
    <|"jsonrpc" -> "2.0", "method" -> "frobnicate", "id" -> 5,
      "params" -> {<||>}|>
  ],
  <|"jsonrpc" -> "2.0", "error" -> <|"code" -> -32601, "message" -> "Method not found"|>, "id" -> 5|>,
  TestID -> "Serve-unknown-method"
]

VerificationTest[
  LambdaFeedback`EvaluationFunctionToolkit`Private`handleRequest[evalOk, previewOk, "not json"],
  <|"jsonrpc" -> "2.0", "error" -> <|"code" -> -32700, "message" -> "Invalid JSON"|>, "id" -> Null|>,
  TestID -> "Serve-malformed-json"
]
