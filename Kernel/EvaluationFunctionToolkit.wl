(* ::Package:: *)

BeginPackage["LambdaFeedback`EvaluationFunctionToolkit`"]

(* Export public symbols *)

Serve
ServeFile
ServeHttp
ServeEvaluationFunction

Begin["`Private`"]

$packageDir = DirectoryName[$InputFileName];

Get[FileNameJoin[{$packageDir, "EvaluationFunctionToolkit", "Execution.wl"}]];
Get[FileNameJoin[{$packageDir, "EvaluationFunctionToolkit", "FileTransport.wl"}]];
Get[FileNameJoin[{$packageDir, "EvaluationFunctionToolkit", "JsonRpc.wl"}]];
Get[FileNameJoin[{$packageDir, "EvaluationFunctionToolkit", "TcpTransport.wl"}]];
Get[FileNameJoin[{$packageDir, "EvaluationFunctionToolkit", "HttpTransport.wl"}]];
Get[FileNameJoin[{$packageDir, "EvaluationFunctionToolkit", "Dispatch.wl"}]];

End[] (* End `Private` *)

EndPackage[]
