(* ::Package:: *)

Needs["LambdaFeedback`EvaluationFunctionToolkit`"]

accumulateHttpRequest = LambdaFeedback`EvaluationFunctionToolkit`Private`accumulateHttpRequest;
startHttpListener = LambdaFeedback`EvaluationFunctionToolkit`Private`startHttpListener;

evalOk[answer_, response_, params_] := <|
  "is_correct" -> True, "feedback" -> "Correct!", "error" -> Null
|>;

previewOk[response_, params_] := <|"latex" -> "x^2", "sympy" -> "x**2"|>;

handleRPC[evalFn_, previewFn_, requestAssoc_] := Module[{requestStr},
  requestStr = ExportString[requestAssoc, "JSON", "Compact" -> True];
  LambdaFeedback`EvaluationFunctionToolkit`Private`handleRequest[evalFn, previewFn, requestStr]
];

(* Builds a raw HTTP/1.1 POST request as a ByteArray for a given JSON body,
   for feeding either directly into accumulateHttpRequest or over a real
   socket in the end-to-end tests below. *)
buildRawHttpRequest[bodyStr_String] := Module[{bodyBytes, headerStr},
  bodyBytes = StringToByteArray[bodyStr, "UTF8"];
  headerStr = "POST / HTTP/1.1\r\nContent-Type: application/json\r\nContent-Length: " <>
    ToString[Length[bodyBytes]] <> "\r\n\r\n";
  Join[StringToByteArray[headerStr, "UTF8"], bodyBytes]
];

(* ---- accumulateHttpRequest: pure framing-parser unit tests, no sockets ---- *)

VerificationTest[
  Module[{req, state},
    req = buildRawHttpRequest["{\"jsonrpc\":\"2.0\"}"];
    state = accumulateHttpRequest[ByteArray[{}], req];
    {state["complete"], state["body"]}
  ],
  {True, "{\"jsonrpc\":\"2.0\"}"},
  TestID -> "ServeHttp-accumulate-single-chunk"
]

VerificationTest[
  Module[{req, part1, part2, state1, state2},
    req = buildRawHttpRequest["{\"jsonrpc\":\"2.0\"}"];
    part1 = ByteArray[Normal[req][[1 ;; 10]]];
    part2 = ByteArray[Normal[req][[11 ;;]]];
    state1 = accumulateHttpRequest[ByteArray[{}], part1];
    state2 = accumulateHttpRequest[state1["buffer"], part2];
    {state1["complete"], state2["complete"], state2["body"]}
  ],
  {False, True, "{\"jsonrpc\":\"2.0\"}"},
  TestID -> "ServeHttp-accumulate-split-across-chunks"
]

VerificationTest[
  Module[{req, headerAndPartial, rest, state1, state2},
    req = buildRawHttpRequest["{\"a\":1}"];
    (* split so the header/body boundary and the body itself both get cut
       mid-content across chunks *)
    headerAndPartial = ByteArray[Normal[req][[1 ;; -3]]];
    rest = ByteArray[Normal[req][[-2 ;;]]];
    state1 = accumulateHttpRequest[ByteArray[{}], headerAndPartial];
    state2 = accumulateHttpRequest[state1["buffer"], rest];
    {state1["complete"], state2["complete"], state2["body"]}
  ],
  {False, True, "{\"a\":1}"},
  TestID -> "ServeHttp-accumulate-body-split-mid-content"
]

VerificationTest[
  Module[{req, state},
    req = StringToByteArray["GET / HTTP/1.1\r\nHost: x\r\n\r\n", "UTF8"];
    state = accumulateHttpRequest[ByteArray[{}], req];
    {state["complete"], state["body"]}
  ],
  {True, ""},
  TestID -> "ServeHttp-accumulate-missing-content-length-defaults-empty-body"
]

(* ---- handleRequest reuse: confirms the shared JSON-RPC core is unchanged;
   Tests/Serve.wlt already covers this logic thoroughly, so this is just
   enough to confirm the http transport is wired to the same core. ---- *)

VerificationTest[
  handleRPC[
    evalOk, previewOk,
    <|"jsonrpc" -> "2.0", "method" -> "eval", "id" -> 1,
      "params" -> {<|"answer" -> "x", "response" -> "x", "params" -> <||>|>}|>
  ],
  <|"jsonrpc" -> "2.0", "result" -> <|"is_correct" -> True, "feedback" -> "Correct!"|>, "id" -> 1|>,
  TestID -> "ServeHttp-handleRequest-eval-success"
]

VerificationTest[
  handleRPC[
    evalOk, previewOk,
    <|"jsonrpc" -> "2.0", "method" -> "frobnicate", "id" -> 5, "params" -> {<||>}|>
  ],
  <|"jsonrpc" -> "2.0", "error" -> <|"code" -> -32601, "message" -> "Method not found"|>, "id" -> 5|>,
  TestID -> "ServeHttp-handleRequest-unknown-method"
]

(* startHttpListener returns a real Failure (not a silent hang) when the
   socket can't be bound -- e.g. the port is already in use. Regression
   coverage for both the failure detection itself, and for the
   "MessageTemplate" vs "Message" Failure-construction bug: building a
   Failure with a "Message" key does NOT make that text retrievable via
   failure["Message"], so this also guards against that regressing. *)
VerificationTest[
  Module[{blocker, savedUrl, result},
    blocker = SocketOpen["127.0.0.1:8793"];
    savedUrl = Environment["EVAL_RPC_HTTP_URL"];
    SetEnvironment["EVAL_RPC_HTTP_URL" -> "http://127.0.0.1:8793/"];

    result = startHttpListener[evalOk, previewOk];

    Quiet[Close[blocker]];
    SetEnvironment["EVAL_RPC_HTTP_URL" -> If[savedUrl === $Failed, "", savedUrl]];

    {FailureQ[result], StringContainsQ[result["Message"], "127.0.0.1:8793"]}
  ],
  {True, True},
  TestID -> "ServeHttp-startHttpListener-reports-bind-failure"
]

(* ---- live end-to-end test: a real ServeHttp listener driven over a real
   socket. This is the one piece none of the pure-function tests above can
   cover -- the actual SocketListen wiring and HTTP response bytes written
   back to a real client. ---- *)

withHttpServer[port_Integer, testFn_] := Module[{savedUrl, running, result},
  savedUrl = Environment["EVAL_RPC_HTTP_URL"];
  SetEnvironment["EVAL_RPC_HTTP_URL" -> "http://127.0.0.1:" <> ToString[port] <> "/"];

  running = startHttpListener[evalOk, previewOk];

  result = testFn[];

  Quiet[DeleteObject[running["Listener"]]];
  Quiet[Close[running["Socket"]]];
  SetEnvironment["EVAL_RPC_HTTP_URL" -> If[savedUrl === $Failed, "", savedUrl]];

  result
];

readHttpResponse[socket_, timeoutSeconds_: 5] := Module[{deadline, buffer, chunk},
  deadline = AbsoluteTime[] + timeoutSeconds;
  buffer = ByteArray[{}];
  While[AbsoluteTime[] < deadline,
    If[SocketReadyQ[socket],
      chunk = SocketReadMessage[socket];
      If[ByteArrayQ[chunk] && Length[chunk] > 0,
        buffer = Join[buffer, chunk],
        Break[]
      ],
      Pause[0.01]
    ]
  ];
  buffer
];

sendHttpRequest[port_Integer, bodyAssoc_Association] := withHttpServer[port, Function[
  Module[{requestStr, client, rawResponse, responseStr, headerEnd, statusLine, bodyStr},
    requestStr = ExportString[bodyAssoc, "JSON", "Compact" -> True];

    client = SocketConnect["127.0.0.1:" <> ToString[port]];
    BinaryWrite[client, buildRawHttpRequest[requestStr]];

    rawResponse = readHttpResponse[client];
    Quiet[Close[client]];

    responseStr = ByteArrayToString[rawResponse, "UTF8"];
    statusLine = First[StringSplit[responseStr, "\r\n"]];
    headerEnd = First[First[StringPosition[responseStr, "\r\n\r\n"]]];
    bodyStr = StringDrop[responseStr, headerEnd + 3];

    <|"status" -> statusLine, "body" -> ImportString[bodyStr, "RawJSON"]|>
  ]
]];

VerificationTest[
  sendHttpRequest[8791, <|"jsonrpc" -> "2.0", "method" -> "eval", "id" -> 1,
    "params" -> {<|"answer" -> "x", "response" -> "x", "params" -> <||>|>}|>],
  <|"status" -> "HTTP/1.1 200 OK",
    "body" -> <|"jsonrpc" -> "2.0", "result" -> <|"is_correct" -> True, "feedback" -> "Correct!"|>, "id" -> 1|>|>,
  TestID -> "ServeHttp-e2e-eval-success"
]

(* Confirms the critical Shimmy-compatibility contract end-to-end: a
   JSON-RPC-level error still comes back as HTTP 200, never a non-2xx
   status, since Shimmy's client treats non-2xx as an opaque transport
   error rather than unpacking it as JSON-RPC. *)
VerificationTest[
  sendHttpRequest[8792, <|"jsonrpc" -> "2.0", "method" -> "frobnicate", "id" -> 5, "params" -> {<||>}|>],
  <|"status" -> "HTTP/1.1 200 OK",
    "body" -> <|"jsonrpc" -> "2.0", "error" -> <|"code" -> -32601, "message" -> "Method not found"|>, "id" -> 5|>|>,
  TestID -> "ServeHttp-e2e-error-still-200"
]
