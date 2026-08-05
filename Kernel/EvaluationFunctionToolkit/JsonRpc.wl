(* ::Package:: *)

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
