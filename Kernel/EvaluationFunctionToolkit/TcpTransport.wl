(* ::Package:: *)

(* ---- tcp RPC transport ---- *)

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
