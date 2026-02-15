# Function/Class Definition Unification Plan

## Objective
- Unify `def` and `class` runtime binding so both are defined through explicit runtime definition instructions instead of split compile-time/runtime behavior.
- Align class definition flow with Python semantics: execute class body in a temporary namespace, then construct/bind class from that namespace.

## Approach
- Introduce runtime definition opcode(s) for functions and classes.
- Remove special-case lazy function resolution from `LoadName`.
- Keep scope minimal: top-level definitions first, preserving current behavior.

## Steps
- [ ] Milestone 1: Bytecode model unification
  - Add `Instruction::DefineFunction { name, symbol }`.
  - Keep `Instruction::DefineClass` and align both under a common "define-and-bind" execution pattern.
  - Compile top-level `def` statements into `DefineFunction` instead of compile-time-only side tables.
- [ ] Milestone 2: VM runtime binding
  - Implement `DefineFunction` execution to bind `FunctionObject` into current environment.
  - Keep `DefineClass` execution path consistent with `DefineFunction` structure.
  - Remove/trim `LoadName` special-case fallback that creates functions from program tables.
- [ ] Milestone 3: Interpreter parity and naming consistency
  - Align interpreter top-level statement execution semantics with VM (`def` and `class` both bind at statement execution).
  - Keep existing function/class visibility behavior unchanged.
  - Introduce class-body temporary namespace execution model (class locals map).
- [ ] Milestone 4: Validation and cleanup
  - Remove `mangle_class_method_name`-style method symbol indirection once class locals namespace binding is in place.
  - Store methods in class namespace under real method names.
  - Add/adjust tests proving both `def` and `class` names are bound by statement execution.
  - Ensure no regressions in existing function/class fixtures.
  - Keep JIT/transpiler behavior explicit (unsupported or adapted) for new opcode shape.

## Verification
- Run `just test`.
- Add bytecode unit tests for emitted `DefineFunction` and `DefineClass` order.
- Add VM tests for name binding and callability after runtime definition execution.

## Clarifying Questions (with defaults)
1. Should we keep separate opcodes (`DefineFunction`, `DefineClass`) or use one generic opcode?
- Default: keep separate opcodes; unify semantics, not payload shape.
2. Should this unification include nested `def`/`class` now?
- Default: no, top-level only.
3. Should interpreter be fully switched to runtime statement binding in the same change?
- Default: yes, to keep backend semantics aligned.

## Approval Gate
- Do not implement until explicit approval (for example: "approved", "go ahead", or "implement Milestone 1").
