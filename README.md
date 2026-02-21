# pyparse

`pyparse` is a small Python-like language implementation in Rust with multiple execution backends.

## What it contains

- Frontend: lexer + parser that build an AST.
- Interpreter backend: direct AST execution.
- VM backend: bytecode compiler + virtual machine.
- JIT backend: Cranelift-based execution.
- Fixture-driven runtime tests under `tests/programs/`.

## Development

Use project `just` commands:

```bash
just test
just lint
```

## Scope

This project aims to keep language behavior compatible with CPython semantics.
