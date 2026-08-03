(* ::Package:: *)

BeginPackage["LambdaFeedback`EvaluationFunctionToolkit`"]

(* Export public symbols *)

Serve
ServeFile

Begin["`Private`"]

$packageDir = DirectoryName[$InputFileName];

Get[FileNameJoin[{$packageDir, "EvaluationFunctionToolkit", "Execution.wl"}]];
Get[FileNameJoin[{$packageDir, "EvaluationFunctionToolkit", "FileTransport.wl"}]];
Get[FileNameJoin[{$packageDir, "EvaluationFunctionToolkit", "JsonRpc.wl"}]];
Get[FileNameJoin[{$packageDir, "EvaluationFunctionToolkit", "TcpTransport.wl"}]];

End[] (* End `Private` *)

EndPackage[]
