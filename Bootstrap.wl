(* ::Package:: *)

(* Fixed entry point every Wolfram evaluation-function image runs, via
   FUNCTION_ARGS set in evaluation-function-base/wolfram/Dockerfile
   (which also git-clones this repo to LF_TOOLKIT_PATH, so this file ships
   for free alongside the rest of the toolkit -- no separate COPY needed).

   Handles the one bit of setup that has to happen before the toolkit's own
   package loading can take over: pointing PacletDirectoryLoad at this
   checkout so Needs can resolve LambdaFeedback`EvaluationFunctionToolkit`.
   Every evaluation-function repo then just needs to provide evaluate.m/
   preview.m defining evaluate`EvaluationFunction and preview`PreviewFunction
   -- no per-repo wiring code required. *)

PacletDirectoryLoad[Environment["LF_TOOLKIT_PATH"]];
Needs["LambdaFeedback`EvaluationFunctionToolkit`"];

<< "evaluate.m";
<< "preview.m";

ServeEvaluationFunction[evaluate`EvaluationFunction, preview`PreviewFunction]
