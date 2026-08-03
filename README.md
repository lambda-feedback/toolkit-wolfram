# Evaluation Function Toolkit for Wolfram

## Introduction

A collection of utilities for creating Lambda Feedback evaluation functions for the Wolfram Language.

## Usage

The toolkit exposes one function per Shimmy comms transport. Currently:

- `ServeFile[EvaluationFunction, PreviewFunction]` — the file-based transport
  (`FUNCTION_INTERFACE="file"`): reads a request JSON file and writes a
  response JSON file, as invoked by `wolframscript -f evaluation_function.wl
  request.json response.json`.
- `Serve[EvaluationFunction, PreviewFunction]` — the `tcp` RPC transport
  (`EVAL_RPC_TRANSPORT="tcp"`): a persistent JSON-RPC 2.0 socket server
  supporting the `eval` and `preview` methods. `healthcheck` is not yet
  implemented.

More Shimmy transports (stdio, ipc) are expected to be added over time,
mirroring [`toolkit-python`](https://github.com/lambda-feedback/toolkit-python)'s
`lf_toolkit/io/`.

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

ServeFile[EvaluationFunction, PreviewFunction]
```

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
`evaluation-function-base/wolfram/Dockerfile`'s `TOOLKIT_WOLFRAM_VERSION` build arg.

To release a new version, tag the commit (`git tag vX.Y.Z && git push origin vX.Y.Z`)
and bump `TOOLKIT_WOLFRAM_VERSION` in `evaluation-function-base/wolfram/Dockerfile`.
