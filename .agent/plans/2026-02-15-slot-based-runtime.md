# Slot-Based Runtime Migration Plan

## Objective
- Move runtime dispatch to CPython-style slots and reach CPython-compatible semantics.

## CPython Semantics (Brief)
- Protocol operations (for example `obj()`, operators, `len`, indexing) are dispatched from the object's type slots/MRO, not by normal instance attribute lookup.
- `obj()` is therefore type-driven call dispatch; `obj.__call__` is normal attribute access and is not the dispatch source for call syntax.
- Attribute behavior follows ordered protocol rules (`__getattribute__`, descriptor precedence, instance dict, `__getattr__`).

## Current Compatibility Gap (Brief)
- Current runtime still has attribute-driven dispatch paths (especially around call and special methods) instead of strict slot protocol dispatch.
- Operator/attribute protocol ordering is not fully centralized and guaranteed identical across interpreter/VM/JIT.
- Class/descriptor/MRO semantics are only partially implemented, so edge cases can diverge from CPython.
- Repro fixture for current CPython gap: `tests/programs/cpython_bool_compare_gap/program.py` (CPython-only until slot migration).
- Why this repro matters: it shows bool numeric/comparison behavior differs from CPython; this can be patched locally, but full parity requires type-driven slot dispatch and subtype-aware semantics to avoid repeated one-off fixes.

## Approach
- Ship in small milestones, keep behavior stable, and validate with fixtures each step.
- Use `TypeInfo` early as internal runtime metadata, even before language-level types/classes are complete.

## Steps
- [ ] Milestone 1: Call protocol
  - Make `obj()` use call slot/`invoke`, not instance lookup of `"__call__"`.
  - Keep `obj.__call__` as normal attribute access.
- [ ] Milestone 2: `TypeInfo` + slots
  - Add `TypeInfo` and attach it to runtime objects.
  - Route builtin object behavior (`int`, `bool`, `str`, `list`, function) through slots.
- [ ] Milestone 3: Core operator dispatch
  - Route arithmetic/comparison/truthiness/indexing through shared slot helpers.
  - Implement `NotImplemented` and reflected-op ordering per CPython.
- [ ] Milestone 4: Classes/instances (initial)
  - Add `ClassObject`/`InstanceObject`, class call flow, and method binding.
  - Keep inheritance initially single-inheritance only.
- [ ] Milestone 5: Attribute + descriptor parity
  - Implement CPython attribute order (`__getattribute__`, descriptors, instance dict, `__getattr__`).
  - Implement data-descriptor precedence for set/delete too.
- [ ] Milestone 6: Inheritance + construction parity
  - Add multiple inheritance with C3 MRO.
  - Align `__new__`/`__init__` and type-call behavior with CPython.
- [ ] Milestone 7: Backend cleanup
  - Remove shims and ensure interpreter/VM/JIT all delegate to shared dispatch helpers.

## Verification
- Per milestone:
  - Run `just test`.
  - Add targeted fixtures in `tests/programs/`.
  - Check backend consistency for call/attribute/operator semantics.

## Clarifying Questions (with defaults)
1. Should Milestone 1 be interpreter-first?
- Default: yes, with VM/JIT bridged through shared helpers.
2. Is single inheritance acceptable in Milestone 4?
- Default: yes, only as an intermediate step.
3. Is descriptor parity mandatory before claiming CPython compatibility?
- Default: yes.

## Approval Gate
- Do not implement until explicit approval (for example: "approved", "go ahead", or "implement Milestone 1").
