# Minimal Class + Instance Method Support Plan

## Objective
- Implement the minimum class/instance runtime slice required to make `tests/programs/classes/` pass on pyparse backends.

## Approach
- Add class syntax and AST support first, then implement runtime objects for class/instance/method binding.
- Use a type-centered model from the start: methods live on the class/type object (via `TypeInfo`/type metadata), not on instances.
- Keep scope intentionally narrow: only behavior needed for class declaration, instance creation, and instance method calls.
- Class/type method table is authoritative (MicroPython/CPython-style): instance lookup may bind from class/type, but does not own method definitions.

## Terminology
- `TypeInfo` (or type metadata): slot table + type name used for protocol dispatch.
- `ClassObject`: runtime type object that owns class attributes/method table.
- `InstanceObject`: runtime object that points at its `ClassObject` and stores instance attributes.

## Steps
- [x] Milestone 1: Frontend support for `class`
  - Add `class` token/keyword in lexer.
  - Add `Statement::ClassDef { name, body }` in AST.
  - Parse `class Name:` with indented body (only method `def` + `pass` initially).
- [x] Milestone 2: Runtime model (minimal)
  - Add `ClassObject` (name + method table + `TypeInfo`) and `InstanceObject` (class ref + instance attrs + `TypeInfo`).
  - At top level, evaluate class body methods into class method table.
- [x] Milestone 3: Attribute lookup + method binding (before class call semantics)
  - Instance attribute lookup order (minimal): instance attrs, then class methods.
  - When class method is accessed via instance, return bound callable with `self` prepended.
  - Keep method ownership on class/type metadata; instance lookup only resolves/binds.
  - Ensure `g.hello()` path works in interpreter and VM.
- [x] Milestone 4: Class call semantics (minimal)
  - `ClassObject` call creates `InstanceObject`.
  - Invoke `__init__` if present (minimal arity/dispatch only).
- [x] Milestone 5: Backend wiring + fixture enablement
  - Compile/execute `ClassDef` in bytecode/interpreter paths.
  - Remove `interpreter` and `vm` from `unsupported_backends` for `classes` once passing.
  - Keep `jit`/`transpiler` unsupported if class bytecode/runtime is not implemented there yet.

## Verification
- Per milestone: run `just test`.
- Required fixture: `tests/programs/classes/` passes for interpreter + VM.
- Keep Python parity runners green for CPython/PyPy/MicroPython.

## Clarifying Questions (with defaults)
1. Should minimal class scope exclude inheritance and descriptors?
- Default: yes, defer both.
2. Should `__init__` be included in this first slice?
- Default: yes, minimal `__init__` call after instance creation to match expected class-call shape.
3. Should instance attribute assignment (`obj.x = ...`) be in scope now?
- Default: no, defer until after method binding works.

## Approval Gate
- Do not implement until explicit approval (for example: "approved", "go ahead", or "implement Milestone 1").
