# Agent Instructions

## Project Overview

`pyparse` is a small Python-like language implementation focused on CPython-compatible semantics.

The frontend is:
`src/lexer.rs` -> `src/parser.rs` + `src/parser/ast.rs` -> `Program` AST.

Backends:
- `interpreter`: AST-walking runtime (`src/interpreter.rs` + `src/interpreter/`)
- `vm`: bytecode compiler + VM (`src/bytecode.rs`, `src/vm.rs`)
- `jit`: Cranelift-based execution path (`src/jit.rs`)

End-to-end behavior is validated primarily through fixture-based runtime tests in `tests/programs/`.

- Prefer project `just` commands for routine validation tasks.
- Run tests with `just test`.
- Run lint/format checks with `just lint`.
- Do not add language features that are incompatible with CPython semantics.

## Compatibility Policy

- The compatibility goal is language behavior matching the overlap shared by CPython, PyPy, and MicroPython.
- Behavior outside that shared overlap is not in scope.
