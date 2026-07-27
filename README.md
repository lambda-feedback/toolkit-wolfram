# Evaluation Function Toolkit for Wolfram

## Introduction

A collection of utilities for creating Lambda Feedback evaluation functions for the Wolfram Language.

## Usage

The toolkit exposes one function per Shimmy comms transport. Currently:

- `ServeFile[EvaluationFunction, PreviewFunction]` — the file-based transport
  (`FUNCTION_INTERFACE="file"`): reads a request JSON file and writes a
  response JSON file, as invoked by `wolframscript -f evaluation_function.wl
  request.json response.json`.
- `Serve[EvaluationFunction]` — a socket/JSON-RPC transport for the `eval`
  method only.

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
(not a `Throw`/`Abort`), `ServeFile` catches it and returns a normal
`{"command", "error"}` JSON response rather than crashing.

## Development

### Publishing

For building and publishing the Paclet, the [`build-paclet`](https://github.com/marketplace/actions/build-paclet) GitHub action is used.

This action uses the [`Wolfram/PacletCICD`](https://resources.wolframcloud.com/PacletRepository/resources/Wolfram/PacletCICD/version/0.36.0/) utilities to build and publish the Paclet.

Please refer to the documentation of the action and the utilities for more information.
