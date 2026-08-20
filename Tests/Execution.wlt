(* ::Package:: *)

Needs["LambdaFeedback`EvaluationFunctionToolkit`"]

runEval = LambdaFeedback`EvaluationFunctionToolkit`Private`runEval;
runPreview = LambdaFeedback`EvaluationFunctionToolkit`Private`runPreview;

(* Tests/*.wlt run in one shared kernel session per build-and-test.yml, so
   restore EVAL_EXECUTION_TIMEOUT afterward. Set well below the real default
   so timeout cases don't slow the suite down. *)
withExecutionTimeout[seconds_, testFn_] := Module[{saved, result},
  saved = Environment["EVAL_EXECUTION_TIMEOUT"];
  SetEnvironment["EVAL_EXECUTION_TIMEOUT" -> ToString[seconds]];
  result = testFn[];
  SetEnvironment["EVAL_EXECUTION_TIMEOUT" -> If[saved === $Failed, "", saved]];
  result
];

fastEvalFn[answer_, response_, params_] := <|
  "error" -> Null,
  "is_correct" -> TrueQ[answer == response]
|>;

(* 1/0 reliably raises a genuine Wolfram Message (Power::infy) for Check to
   catch, matching how a careless user eval function might fail. *)
erroringEvalFn[answer_, response_, params_] := 1/0;

hangingEvalFn[answer_, response_, params_] := (Pause[10]; <|
  "error" -> Null,
  "is_correct" -> True
|>);

fastPreviewFn[response_, params_] := <| "latex" -> response, "sympy" -> response |>;

hangingPreviewFn[response_, params_] := (Pause[10]; <| "latex" -> response, "sympy" -> response |>);

VerificationTest[
  runEval[fastEvalFn, "1", "1", <||>]["ok"],
  True,
  TestID -> "Execution-runEval-fast-call-succeeds"
]

VerificationTest[
  runEval[erroringEvalFn, "1", "1", <||>],
  <| "ok" -> False, "message" -> "Evaluation function raised an error" |>,
  TestID -> "Execution-runEval-message-still-caught-as-error"
]

VerificationTest[
  withExecutionTimeout[2, runEval[hangingEvalFn, "1", "1", <||>] &],
  <| "ok" -> False, "message" -> "Evaluation function timed out" |>,
  TestID -> "Execution-runEval-timeout"
]

VerificationTest[
  runPreview[fastPreviewFn, "x+1", <||>]["ok"],
  True,
  TestID -> "Execution-runPreview-fast-call-succeeds"
]

VerificationTest[
  withExecutionTimeout[2, runPreview[hangingPreviewFn, "x+1", <||>] &],
  <| "ok" -> False, "message" -> "Preview function timed out" |>,
  TestID -> "Execution-runPreview-timeout"
]

(* Regression: the timeout must actually bound wall-clock time, not just
   the eventual outcome -- guards against a future change accidentally
   dropping TimeConstrained while still returning the right-shaped result
   some other way. *)
VerificationTest[
  First[withExecutionTimeout[2, AbsoluteTiming[runEval[hangingEvalFn, "1", "1", <||>]] &]] < 9,
  True,
  TestID -> "Execution-runEval-timeout-bounds-wall-clock-time"
]