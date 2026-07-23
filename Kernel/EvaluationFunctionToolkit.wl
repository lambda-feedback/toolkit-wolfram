(* ::Package:: *)

BeginPackage["LambdaFeedback`EvaluationFunctionToolkit`"]

(* Export public symbols *)

Serve
ServeFile

Begin["`Private`"]

createError[code_, msg_, id_] := Module[{},
  <|
    "jsonrpc" -> "2.0",
    "error" -> <|
      "code" -> code,
      "message" -> msg
    |>,
    "id" -> id
  |>
];

createResponse[result_, id_] := Module[{},
  <|
    "jsonrpc" -> "2.0",
    "result" -> result,
    "id" -> id
  |>
];

createErrorResponse[code_, msg_, id_] := Module[{},
  ExportString[createError[code, msg, id], "JSON", "Compact" -> True]
];

(* Function to handle JSON-RPC 2.0 request and response *)
handleJSONRPCRequest[eval_, req_] := Module[{method, params, id, result},
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

  (* Return error if method is not "eval" *)
  method = req["method"];
  If[method =!= "eval",
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

  (* Return error if answer is empty *)
  answer = Lookup[data, "answer", Null];
  If[answer === Null,
    Return[createError[-32602, "Missing answer", id]]
  ];

  (* Return error if response is empty *)
  response = Lookup[data, "response", Null];
  If[response === Null,
    Return[createError[-32602, "Missing response", id]]
  ];

  (* Fall back to empty association if params is empty *)
  params = Lookup[data, "params", <||>];

  (* Run evaluation *)
  result = eval[answer, response, params];

  createResponse[result, id]
];

handleRequest[eval_, data_] := Module[{request, response},
  (* Try to parse message as JSON *)
  request = ImportString[data, "RawJSON"];
  If[request === $Failed,
    Return[createError[-32700, "Invalid JSON", Null]]
  ];

  (* Try to handle message *)
  response = handleJSONRPCRequest[eval, request];
  If[response === $Failed,
    Return[createError[-32001, "Function error", request["id"]]],
    Return[response]
  ];
];

(* Function to handle incoming messages *)
createMessageHandler[eval_] := Module[{handle},
  handleMessage[msg_] := Module[{message},
    (* Convert input bytes to string *)
    str = ByteArrayToString[msg["DataByteArray"]];

    (* Handle request *)
    response = handleRequest[eval, str];

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

Serve[eval_] := Module[{},
  socketAddress = Environment["EVAL_RPC_TCP_ADDRESS"];
  If[socketAddress === $Failed, socketAddress = "127.0.0.1:7321"];

  socket = SocketOpen[socketAddress];

  handler = createMessageHandler[eval];

  listener = SocketListen[socket, handler, RecordSeparators -> {"\n"}];

  (* Print["Listening on ", socketAddress]; *)

  While[True, Pause[60]];

  (* Print["Closing connection"]; *)

  DeleteObject[listener];
  Close[socket];
];

(* ---- File-based transport ---- *)

buildErrorResult[msg_] := <| "message" -> ToString[msg] |>;

(* Catches Wolfram Messages raised by user code so a crash still produces a
   JSON response instead of leaving the response file unwritten. *)
safeCall[fn_, args___] := Quiet@Check[fn[args], $Failed];

processEvalRequest[evalFn_, requestData_] := Module[
  {params, answer, response, evalParams, result, errorMsg},
  params = requestData["params"];
  answer = params["answer"];
  response = params["response"];
  evalParams = params["params"];

  Print["Running eval"];
  result = safeCall[evalFn, answer, response, evalParams];

  If[result === $Failed,
    Return[<| "command" -> "eval", "error" -> buildErrorResult["Evaluation function raised an error"] |>]
  ];

  errorMsg = Lookup[result, "error", Null];
  If[errorMsg =!= Null,
    Return[<| "command" -> "eval", "error" -> buildErrorResult[errorMsg] |>]
  ];

  <|
    "command" -> "eval",
    "result" -> <|
      "is_correct" -> result["is_correct"],
      "feedback" -> result["feedback"]
    |>
  |>
];

processPreviewRequest[previewFn_, requestData_] := Module[
  {params, response, previewParams, result},
  params = requestData["params"];
  response = params["response"];
  previewParams = Lookup[params, "params", <||>];

  Print["Running preview"];
  result = safeCall[previewFn, response, previewParams];

  If[result === $Failed,
    Return[<| "command" -> "preview", "error" -> buildErrorResult["Preview function raised an error"] |>]
  ];

  <| "command" -> "preview", "result" -> <| "preview" -> result |> |>
];

processFileRequest[evalFn_, previewFn_, requestData_] := Module[{command},
  command = Lookup[requestData, "command", "unknown"];
  Which[
    command === "eval", processEvalRequest[evalFn, requestData],
    command === "preview", processPreviewRequest[previewFn, requestData],
    True, <| "command" -> command, "error" -> buildErrorResult["Unknown command: " <> ToString[command]] |>
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

End[] (* End `Private` *)

EndPackage[]
