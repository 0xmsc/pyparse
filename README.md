# pyparse

`pyparse` is a small Python-like language implementation in Rust focused on
CPython-compatible semantics (within the supported subset).

## What it contains

- Frontend: lexer + parser that build an AST.
- Interpreter backend: direct AST execution.
- VM backend: bytecode compiler + virtual machine.
- Fixture-driven runtime tests under `tests/programs/` (including parity checks
  against CPython, PyPy, and MicroPython where applicable).

## Current language support (subset highlights)

- Functions and classes (subset)
- Lists and dictionaries
- `for` loops via the iterator protocol (`__iter__` / `__next__`)
- Exceptions in interpreter + VM (`raise`, `try/except/finally`)
- Built-in exception classes: `BaseException`, `Exception`, `StopIteration`

## Development

Use project `just` commands:

```bash
just test
just lint
```

## Scope

This project aims to match behavior in the overlap shared by CPython, PyPy, and
MicroPython. Behavior outside that shared overlap is not in scope.
