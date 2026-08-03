(* ::Package:: *)

BeginPackage["LambdaFeedback`EvaluationFunctionToolkit`"]

(* Export public symbols *)

Serve
ServeFile

Begin["`Private`"]

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

(* ---- RPC (JSON-RPC 2.0) transport core, shared across rpc transports ----
   Note: unlike the file transport, shimmy's rpc adapter takes whatever comes
   back in the JSON-RPC response's "result" field and forwards it verbatim as
   {"command": method, "result": <that>} -- it does not inspect it for a
   nested "error" key. So domain errors and crashes here must surface as real
   JSON-RPC-level errors, never nested inside "result". *)

createError[code_, msg_, id_] := <|
  "jsonrpc" -> "2.0",
  "error" -> <|
    "code" -> code,
    "message" -> msg
  |>,
  "id" -> id
|>;

createResponse[result_, id_] := <|
  "jsonrpc" -> "2.0",
  "result" -> result,
  "id" -> id
|>;

createErrorResponse[code_, msg_, id_] :=
  ExportString[createError[code, msg, id], "JSON", "Compact" -> True];

handleEvalRPC[evalFn_, data_, id_] := Module[{answer, response, evalParams, outcome},
  answer = Lookup[data, "answer", Null];
  If[answer === Null, Return[createError[-32602, "Missing answer", id]]];

  response = Lookup[data, "response", Null];
  If[response === Null, Return[createError[-32602, "Missing response", id]]];

  evalParams = Lookup[data, "params", <||>];

  outcome = runEval[evalFn, answer, response, evalParams];

  If[outcome["ok"],
    createResponse[outcome["data"], id],
    createError[-32000, outcome["message"], id]
  ]
];

handlePreviewRPC[previewFn_, data_, id_] := Module[{response, previewParams, outcome},
  response = Lookup[data, "response", Null];
  If[response === Null, Return[createError[-32602, "Missing response", id]]];

  previewParams = Lookup[data, "params", <||>];

  outcome = runPreview[previewFn, response, previewParams];

  If[outcome["ok"],
    createResponse[outcome["data"], id],
    createError[-32000, outcome["message"], id]
  ]
];

(* Function to handle JSON-RPC 2.0 request and response *)
handleJSONRPCRequest[evalFn_, previewFn_, req_] := Module[
  {method, params, id, version, data},
  (* Get the request id *)
  id = req["id"];
  If[!IntegerQ[id],
    Return[createError[-32600, "Missing request id", Null]]
  ];

  (* Return error if version is not "2.0" *)
  version = req["jsonrpc"];
  If[version =!= "2.0",
    Return[createError[-32600, "Missing jsonrpc version", id]]
  ];

  (* Return error if method is not "eval" or "preview" *)
  method = req["method"];
  If[method =!= "eval" && method =!= "preview",
    Return[createError[-32601, "Method not found", id]]
  ];

  params = req["params"];

  (* Return error if params has not length of 1 *)
  If[Length[params] != 1,
    Return[createError[-32602, "Invalid params", id]]
  ];

  (* Return error if data is not an association *)
  data = params[[1]];
  If[!AssociationQ[data],
    Return[createError[-32602, "Invalid params", id]]
  ];

  If[method === "eval",
    handleEvalRPC[evalFn, data, id],
    handlePreviewRPC[previewFn, data, id]
  ]
];

handleRequest[evalFn_, previewFn_, data_] := Module[{request, response},
  (* Try to parse message as JSON *)
  request = Quiet[ImportString[data, "RawJSON"]];
  If[request === $Failed,
    Return[createError[-32700, "Invalid JSON", Null]]
  ];

  (* Try to handle message *)
  response = handleJSONRPCRequest[evalFn, previewFn, request];
  If[response === $Failed,
    Return[createError[-32001, "Function error", request["id"]]],
    Return[response]
  ];
];

(* Function to handle incoming messages *)
createMessageHandler[evalFn_, previewFn_] := Module[{},
  handleMessage[msg_] := Module[{str, response, socket, responseStr},
    (* Convert input bytes to string *)
    str = ByteArrayToString[msg["DataByteArray"]];

    (* Handle request *)
    response = handleRequest[evalFn, previewFn, str];

    (* Get the source socket *)
    socket = msg["SourceSocket"];

    (* Stringify the response *)
    responseStr = ExportString[response, "JSON", "Compact" -> True];
    If[responseStr === $Failed,
      WriteString[socket, createErrorResponse[-32000, "Encoding error", Null] <> "\n"];
      Return[]
    ];

    (* Reply with the stringified response *)
    WriteString[socket, responseStr <> "\n"];
  ];

  handleMessage
]

Serve[evalFn_, previewFn_] := Module[{socketAddress, socket, handler, listener},
  socketAddress = Environment["EVAL_RPC_TCP_ADDRESS"];
  If[socketAddress === $Failed, socketAddress = "127.0.0.1:7321"];

  socket = SocketOpen[socketAddress];

  handler = createMessageHandler[evalFn, previewFn];

  listener = SocketListen[socket, handler, RecordSeparators -> {"\n"}];

  (* Print["Listening on ", socketAddress]; *)

  While[True, Pause[60]];

  (* Print["Closing connection"]; *)

  DeleteObject[listener];
  Close[socket];
];

End[] (* End `Private` *)

EndPackage[]
