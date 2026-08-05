(* ::Package:: *)

(* ---- Transport dispatch ----
   Decides, from the shimmy-supplied EVAL_IO / EVAL_RPC_TRANSPORT environment
   variables, which underlying Serve-family function a consumer's
   evaluation_function.wl should call. The decision itself (dispatchTransport)
   is a pure function of two strings, kept separate from environment reading,
   so it can be unit-tested directly -- mirrors the safeCall/runEval split in
   Execution.wl and the parse/dispatch split in JsonRpc.wl. *)

(* Rpc sub-transports shimmy may report via EVAL_RPC_TRANSPORT that this
   toolkit does not implement yet. Listed explicitly so the error can say
   "this is a real transport shimmy supports, just not wired up here yet"
   rather than lumping it in with a genuinely unrecognized value.

   "stdio" belongs here, not as a dispatch target: Wolfram Engine has no
   supported way to get a readable stream handle onto a process's real
   inherited stdin -- ReadLine/BinaryReadList on "stdin" or "/dev/stdin"
   both fail (confirmed on macOS and in the Linux worker image, over both
   wolframscript and a bare WolframKernel, in every invocation mode tried:
   -file, -script, -code, -run). The kernel's own top-level loop clearly
   *can* read real stdin internally, but never exposes it as a Streams[]
   object user code can Read from. Revisit only alongside a LibraryLink C
   shim (raw read(2)/write(2) on fd 0/1), not as a pure-WL fix. *)
$unimplementedRpcTransports = {"stdio", "ipc", "ws"};

(* dispatchTransport[evalIO, rpcTransport] -> the Serve-family function to
   call, or Failure[...] describing why none could be selected. Never talks
   to the environment or the outside world. *)
dispatchTransport[evalIO_String, rpcTransport_String] := Which[
  evalIO =!= "rpc",
    (* Shimmy's file adapter sets EVAL_IO=FILE (uppercase); running the
       script directly without shimmy leaves it unset. Both, and any other
       unrecognized value, fall back to the file transport -- preserving
       today's evaluation_function.wl behavior of treating "not rpc" as
       file-like. *)
    ServeFile,
  rpcTransport === "tcp",
    Serve,
  rpcTransport === "http",
    ServeHttp,
  MemberQ[$unimplementedRpcTransports, rpcTransport],
    Failure["UnimplementedRpcTransport", <|
      "MessageTemplate" -> "EVAL_RPC_TRANSPORT=" <> rpcTransport <>
        " is a recognized Shimmy transport, but toolkit-wolfram does not implement it yet."
    |>],
  True,
    Failure["UnknownRpcTransport", <|
      "MessageTemplate" -> "Unrecognized EVAL_RPC_TRANSPORT value: " <> rpcTransport
    |>]
];

(* Reads Environment[...] (normalizing unset/$Failed to "") and resolves the
   dispatch target, without invoking it. Kept separate from
   ServeEvaluationFunction so tests can exercise env-var reading without
   also running a transport. *)
resolveDispatchTarget[] := Module[{evalIO, rpcTransport},
  evalIO = Environment["EVAL_IO"];
  If[evalIO === $Failed, evalIO = ""];

  rpcTransport = Environment["EVAL_RPC_TRANSPORT"];
  If[rpcTransport === $Failed, rpcTransport = ""];

  dispatchTransport[evalIO, rpcTransport]
];

(* Picks and runs the right Serve-family function for the current process's
   environment. A grading worker silently hanging or misbehaving because of
   an unimplemented/unrecognized transport is worse than a clear nonzero
   exit shimmy's supervisor can observe and log -- so an unresolvable
   transport fails the whole process rather than falling back to anything. *)
ServeEvaluationFunction[evalFn_, previewFn_] := Module[{target},
  target = resolveDispatchTarget[];

  If[FailureQ[target],
    Print["FATAL: ", target["Message"]];
    Exit[1]
  ];

  target[evalFn, previewFn]
];
