# Evaluation Function Toolkit for Wolfram

## Introduction

A collection of utilities for creating Lambda Feedback evaluation functions for the Wolfram Language.

## Usage

Evaluation function repos built on
[`evaluation-function-base/wolfram`](https://github.com/lambda-feedback/evaluation-function-base)
don't need to call anything in this toolkit directly: that base image's
`FUNCTION_COMMAND`/`FUNCTION_ARGS` already point at this repo's
`Bootstrap.wl`, which loads the toolkit and wires it up automatically. Such a
repo only needs to provide an `evaluate.m` and `preview.m` (in the image's
working directory) defining `evaluate\`EvaluationFunction` and
`preview\`PreviewFunction` — see `Bootstrap.wl` for the exact contract.

For anything else (custom wiring, local/manual testing, a different base
image), call `ServeEvaluationFunction` directly, which reads
Shimmy's environment-variable contract and dispatches to the right transport
— consumers don't need to know which transport Shimmy is running them under:

```wolfram
Needs["LambdaFeedback`EvaluationFunctionToolkit`"]

EvaluationFunction[answer_, response_, params_] := <|
  "is_correct" -> answer == response,
  "feedback" -> If[answer == response, "Correct!", "Incorrect!"],
  "error" -> Null
|>;

PreviewFunction[response_, params_] := <|
  "latex" -> ToString[ToExpression[response], TeXForm],
  "sympy" -> ToString[ToExpression[response], InputForm]
|>;

ServeEvaluationFunction[EvaluationFunction, PreviewFunction]
```

`ServeEvaluationFunction` reads:

- `EVAL_IO` — `"rpc"` selects an RPC transport (below); anything else
  (Shimmy's `"FILE"`, unset, or unrecognized) falls back to the file
  transport.
- `EVAL_RPC_TRANSPORT` (only consulted when `EVAL_IO="rpc"`) — selects which
  RPC transport to run. Currently only `"tcp"` is implemented. `"stdio"`,
  `"ipc"`, `"http"`, `"ws"` are recognized Shimmy transports not yet
  implemented in this toolkit; any other value is unrecognized. Either case
  exits the process with a clear message and nonzero status rather than
  silently falling back to a different transport — a grading worker doing
  the wrong thing silently is worse than failing loudly where Shimmy's
  supervisor can observe it.

Internally, this dispatches to one function per Shimmy comms transport:

- `ServeFile[EvaluationFunction, PreviewFunction]` — the file-based transport
  (`FUNCTION_INTERFACE="file"`): reads a request JSON file and writes a
  response JSON file, as invoked by `wolframscript -f evaluation_function.wl
  request.json response.json`.
- `Serve[EvaluationFunction, PreviewFunction]` — the `tcp` RPC transport
  (`EVAL_RPC_TRANSPORT="tcp"`): a persistent JSON-RPC 2.0 socket server
  supporting the `eval` and `preview` methods. `healthcheck` is not yet
  implemented.

These remain exported for testing, but `ServeEvaluationFunction` is the
supported entry point for evaluation function repos — calling `ServeFile`/
`Serve` directly means hand-rolling the transport-selection logic they exist
to avoid.

More Shimmy transports (stdio, ipc) are expected to be added over time,
mirroring [`toolkit-python`](https://github.com/lambda-feedback/toolkit-python)'s
`lf_toolkit/io/`, each plugged into `ServeEvaluationFunction`'s dispatch as it
lands.

`EvaluationFunction` must return an association with `is_correct`,
`feedback`, and `error` (`Null` on success, an error message otherwise).
`PreviewFunction`'s return value is passed straight through under
`result.preview` — it is not inspected for an `"error"` key, so preview
functions are free to embed their own inline error/unavailable state.

If `EvaluationFunction` or `PreviewFunction` raises a Wolfram error/message
(not a `Throw`/`Abort`), both `ServeFile` and `Serve` catch it rather than
crashing. `ServeFile` returns a normal `{"command", "error"}` JSON response;
`Serve` returns a JSON-RPC 2.0 error object (`{"error": {"code", "message"}}`),
since a nested `"error"` key inside the JSON-RPC `"result"` would not be
recognized as an error by Shimmy on the RPC transports.

## Development

### Consuming this toolkit

This toolkit is not published to the Wolfram Paclet Repository. Wolfram-based
evaluation functions consume it by `git clone`ing a tagged version and pointing
`PacletDirectoryLoad` at the checkout — see
`evaluation-function-base/wolfram/Dockerfile`'s `TOOLKIT_WOLFRAM_VERSION` build
arg, and its `FUNCTION_COMMAND`/`FUNCTION_ARGS`, which run `Bootstrap.wl` from
that same checkout.

To release a new version, tag the commit (`git tag vX.Y.Z && git push origin vX.Y.Z`)
and bump `TOOLKIT_WOLFRAM_VERSION` in `evaluation-function-base/wolfram/Dockerfile`.
