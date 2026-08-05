(* ::Package:: *)

(* ---- File-based transport ---- *)

buildFileResponse[command_, outcome_] := If[outcome["ok"],
  <| "command" -> command, "result" -> outcome["data"] |>,
  <| "command" -> command, "error" -> <| "message" -> outcome["message"] |> |>
];

processFileRequest[evalFn_, previewFn_, requestData_] := Module[{command, params},
  command = Lookup[requestData, "command", "unknown"];
  params = Lookup[requestData, "params", <||>];
  Which[
    command === "eval",
      buildFileResponse["eval", runEval[
        evalFn, params["answer"], params["response"], Lookup[params, "params", <||>]
      ]],
    command === "preview",
      buildFileResponse["preview", runPreview[
        previewFn, params["response"], Lookup[params, "params", <||>]
      ]],
    True,
      <| "command" -> command, "error" -> <| "message" -> "Unknown command: " <> ToString[command] |> |>
  ]
];

ServeFile[evalFn_, previewFn_, requestPath_String, responsePath_String] := Module[
  {requestData, responseData},
  requestData = Import[requestPath, "JSON"] //. List :> Association;

  Print["Input"];
  Print[requestData];

  responseData = processFileRequest[evalFn, previewFn, requestData];

  Print["Output"];
  Print[responseData];

  Export[responsePath, responseData, "JSON", "Compact" -> True];
];

ServeFile[evalFn_, previewFn_] := Module[{argv},
  argv = Rest[$ScriptCommandLine];
  ServeFile[evalFn, previewFn, argv[[1]], argv[[2]]]
];
