(* ::Package:: *)

(* ---- http RPC transport ----
   Shimmy's http transport (github.com/lambda-feedback/shimmy,
   internal/execution/supervisor/adapter_rpc.go) is go-ethereum's generic
   JSON-RPC HTTP client: it POSTs a single JSON-RPC 2.0 request per call to
   the URL given via EVAL_RPC_HTTP_URL, and requires a 2xx status or it
   surfaces an opaque transport-level error instead of reading the body as
   JSON-RPC. So, exactly like the tcp transport, every outcome -- success,
   domain error, caught crash -- must come back as HTTP 200 with a JSON-RPC
   envelope; never as a non-2xx status. The client sends one request per
   connection and is fine with the server closing the connection afterward
   (it just redials next call), so this implementation makes no attempt at
   keep-alive.

   Wolfram has no built-in local HTTP server primitive, so this hand-rolls
   minimal HTTP/1.1 framing over a raw SocketListen, the same way
   TcpTransport.wl hand-rolls JSON-RPC framing over a raw socket -- except
   here there's no RecursionSeparators-style delimiter to lean on, since a
   request's end is determined by the Content-Length header, not a fixed
   terminator. accumulateHttpRequest is kept as a pure function of bytes in,
   state out (no socket access) specifically so the framing logic is
   testable without a live connection. *)

(* accumulateHttpRequest[buffer, newBytes] -> the buffer's new state:
   <|"complete" -> False, "buffer" -> ByteArray[...]|> if more bytes are
   still needed (haven't seen the end of headers yet, or the body isn't
   fully buffered yet), or <|"complete" -> True, "body" -> "..."|> once the
   full request body has arrived. Never inspects method/path/other headers
   -- Shimmy's real client only ever sends well-formed JSON-RPC POSTs, so
   the only thing that matters here is where the body starts and ends. *)
accumulateHttpRequest[buffer_ByteArray, newBytes_ByteArray] := Module[
  {combined, bytes, sepMatch, sepStart, sepEnd, headerStr, contentLength, bodyEnd},
  combined = Join[buffer, newBytes];
  bytes = Normal[combined];

  sepMatch = SequencePosition[bytes, {13, 10, 13, 10}, 1];
  If[sepMatch === {},
    Return[<|"complete" -> False, "buffer" -> combined|>]
  ];

  {sepStart, sepEnd} = First[sepMatch];
  headerStr = ByteArrayToString[ByteArray[bytes[[1 ;; sepStart - 1]]]];
  contentLength = parseContentLength[headerStr];

  bodyEnd = sepEnd + contentLength;
  If[Length[bytes] < bodyEnd,
    Return[<|"complete" -> False, "buffer" -> combined|>]
  ];

  <|"complete" -> True, "body" -> ByteArrayToString[ByteArray[bytes[[sepEnd + 1 ;; bodyEnd]]]]|>
];

(* Case-insensitive Content-Length lookup out of a raw "\r\n"-joined header
   block. Deliberately avoids ToExpression on header text (untrusted input)
   -- pulls digits out with a regex and FromDigits instead. Missing/malformed
   header defaults to 0, matching the "don't validate, let the JSON-RPC core
   surface the resulting parse error" minimal-handling decision. *)
parseContentLength[headerStr_String] := Module[{lines, line, valueStr, digits},
  lines = StringSplit[headerStr, "\r\n"];
  line = SelectFirst[lines, StringMatchQ[#, RegularExpression["(?i)^content-length\\s*:.*"]] &, ""];
  If[line === "", Return[0]];

  valueStr = StringTrim[StringReplace[line, RegularExpression["(?i)^content-length\\s*:"] -> ""]];
  digits = StringCases[valueStr, DigitCharacter ..];

  If[digits === {}, 0, FromDigits[First[digits]]]
];

(* Extracts a "host:port" SocketOpen address out of EVAL_RPC_HTTP_URL. *)
parseHttpUrlHostPort[url_String] := Module[{parsed, host, port},
  parsed = URLParse[url];

  host = Lookup[parsed, "Domain", "127.0.0.1"];
  If[host === None || host === "", host = "127.0.0.1"];

  port = Lookup[parsed, "Port", 8000];
  If[port === None, port = 8000];

  ToString[host] <> ":" <> ToString[port]
];

buildHttpResponse[bodyStr_String] := Module[{bodyBytes},
  bodyBytes = StringToByteArray[bodyStr, "UTF8"];
  "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: " <>
    ToString[Length[bodyBytes]] <> "\r\nConnection: close\r\n\r\n" <> bodyStr
];

(* Holds one accumulation buffer per open socket, scoped to a single
   createHttpMessageHandler call (i.e. one per ServeHttp listener), the same
   closure-over-Module-local-state shape as TcpTransport.wl's
   createMessageHandler. *)
createHttpMessageHandler[evalFn_, previewFn_] := Module[{buffers, handleMessage},
  buffers = <||>;

  handleMessage[msg_] := Module[{socket, incoming, state, jsonResponse, responseStr},
    socket = msg["SourceSocket"];
    incoming = msg["DataByteArray"];

    (* Any event that doesn't carry data bytes (e.g. the connection closing)
       just drops whatever partial state we had for this socket. *)
    If[!ByteArrayQ[incoming],
      buffers = KeyDrop[buffers, socket];
      Return[]
    ];

    state = accumulateHttpRequest[Lookup[buffers, socket, ByteArray[{}]], incoming];

    If[!state["complete"],
      buffers[socket] = state["buffer"];
      Return[]
    ];

    buffers = KeyDrop[buffers, socket];

    jsonResponse = handleRequest[evalFn, previewFn, state["body"]];
    responseStr = ExportString[jsonResponse, "JSON", "Compact" -> True];
    If[responseStr === $Failed,
      responseStr = createErrorResponse[-32000, "Encoding error", Null];
    ];

    BinaryWrite[socket, StringToByteArray[buildHttpResponse[responseStr], "UTF8"]];
    Close[socket];
  ];

  handleMessage
];

(* Opens the listening socket and wires up the handler, without blocking --
   split out from ServeHttp so tests can exercise a live listener without
   invoking the infinite Pause loop below.

   Both SocketOpen and SocketListen return a Failure[...] (not $Failed) on
   error -- e.g. SocketOpen on a port already in use -- so this checks head
   types rather than === $Failed. A prior version of this code didn't check
   either result at all: a failed bind would silently fall through to the
   While loop below and sit there indefinitely looking alive to Shimmy (its
   HTTP dial is lazy and never verifies connectivity at Start time) while
   nothing was actually listening -- an undiagnosable hang from outside.
   This also prints a positive "listening" confirmation on success, since
   without it there's no way to tell from the process's own output whether
   it ever got this far. *)
startHttpListener[evalFn_, previewFn_] := Module[{url, socketAddress, socket, handler, listener},
  url = Environment["EVAL_RPC_HTTP_URL"];
  If[url === $Failed || url === "", url = "http://127.0.0.1:8000/"];

  socketAddress = parseHttpUrlHostPort[url];

  socket = SocketOpen[socketAddress];
  If[Head[socket] =!= SocketObject,
    Return[Failure["SocketOpenFailed", <|
      "MessageTemplate" -> "Could not open a listening socket on " <> socketAddress <>
        " (from EVAL_RPC_HTTP_URL=" <> url <> "): " <> ToString[socket]
    |>]]
  ];

  handler = createHttpMessageHandler[evalFn, previewFn];
  listener = SocketListen[socket, handler];
  If[Head[listener] =!= SocketListener,
    Close[socket];
    Return[Failure["SocketListenFailed", <|
      "MessageTemplate" -> "Could not start listening on " <> socketAddress <> ": " <> ToString[listener]
    |>]]
  ];

  Print["ServeHttp: listening on ", socketAddress, " (EVAL_RPC_HTTP_URL=", url, ")"];

  <|"Socket" -> socket, "Listener" -> listener|>
];

ServeHttp[evalFn_, previewFn_] := Module[{running},
  running = startHttpListener[evalFn, previewFn];

  If[FailureQ[running],
    Print["FATAL: ", running["Message"]];
    Exit[1]
  ];

  While[True, Pause[60]];

  DeleteObject[running["Listener"]];
  Close[running["Socket"]];
];
