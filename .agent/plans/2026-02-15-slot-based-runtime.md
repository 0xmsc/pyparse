# Slot-Based Runtime Migration Plan

## Objective
- Migrate from attribute-driven operation dispatch to a slot-based object model with CPython-compatible semantics as the final target.
- Reach parity without a high-risk rewrite by moving behavior in small, testable slices.

## Compatibility Target
- End-state target is CPython behavior for runtime dispatch and object model semantics.
- During migration, temporary gaps are allowed only if they are documented and covered by explicit follow-up milestones.
- Special method dispatch must follow CPython rules: resolve on type/MRO via slots, not instance-level monkeypatching.

## Approach
- Use incremental milestones:
  - First, make call dispatch slot-like with minimal API churn.
  - Then introduce type metadata and slot tables.
  - Then route operations through shared slot dispatch helpers.
  - Then add class/instance model and attribute protocol pieces.
  - Finally complete CPython parity (descriptors, MRO, metaclass-sensitive paths).
- Keep behavior parity validated by fixture tests (`just test`) at each milestone.
- Ensure interpreter, VM, and JIT consume the same runtime dispatch helpers as milestones land.

## Plan Steps
- [ ] Milestone 1: Call dispatch migration (`obj()` uses call slot, not attribute lookup)
  - Replace `Value::call` path that currently resolves `"__call__"` first.
  - Dispatch directly to runtime call entrypoint (`invoke`) on the receiver object.
  - Update callable objects (`BuiltinFunctionObject`, `FunctionObject`, bound wrappers) so `invoke` performs actual calls.
  - Keep `obj.__call__` attribute access for introspection, but decouple from `obj()` semantics.
  - Add regression tests for non-callables and callable objects.
  - Done criteria:
    - `obj()` does not consult instance attribute lookup for `"__call__"`.
    - Non-callable invocation raises the expected runtime error type.

- [ ] Milestone 2: Introduce explicit type metadata (`TypeInfo`) and slot table
  - Add a `TypeInfo` structure containing slot functions (`call`, `getattr`, `setattr`, numeric/container slots, etc.) and type name.
  - Make each runtime object expose/own a reference to `TypeInfo`.
  - Keep compatibility shims so current `RuntimeObject` users compile while migration is in progress.
  - Ensure existing error mapping remains stable.
  - Done criteria:
    - Core builtins (`int`, `bool`, `str`, `list`, functions) dispatch through `TypeInfo` slots.
    - No new direct `"__dunder__"` probing is added in call/operator hot paths.

- [ ] Milestone 3: Move core operations to slot dispatch helpers
  - Route arithmetic (`add`, `sub`, ...), rich comparison, truthiness (`bool`, `len`), and indexing (`getitem`, `setitem`) through slot helpers.
  - Implement explicit protocol for `NotImplemented` and reflected operations (`__r*__`) aligned with CPython ordering.
  - Centralize operation dispatch helpers so interpreter, VM, and JIT all delegate.
  - Add fixture coverage for unsupported-operation and wrong-type error cases.
  - Done criteria:
    - Operator behavior is backend-consistent for interpreter/VM/JIT.
    - Binary-op and comparison fallback ordering is test-covered.

- [ ] Milestone 4: Class and instance runtime objects (initial slice)
  - Add `ClassObject` and `InstanceObject` backed by slot model.
  - Implement class invocation flow (`ClassObject` call creates instance, then runs `__init__`).
  - Implement initial attribute resolution with method binding and explicit TODOs for full descriptor precedence.
  - Keep initial inheritance scope intentionally narrow (single inheritance) but encode MRO plumbing in APIs.
  - Done criteria:
    - Instance method binding works for normal functions stored on class.
    - Instance callability comes from type slots, not instance-level `"__call__"` attributes.

- [ ] Milestone 5: Attribute protocol and descriptor parity
  - Implement CPython-order attribute access rules: `__getattribute__`, descriptor precedence, instance dict, `__getattr__`.
  - Implement set/delete semantics for data descriptors and instance attributes.
  - Validate method/function binding behavior through fixtures.
  - Done criteria:
    - Data-descriptor vs instance-dict precedence matches CPython for covered cases.
    - `__getattr__` only runs after normal resolution failure.

- [ ] Milestone 6: Inheritance and type construction parity
  - Implement multiple inheritance with C3 MRO.
  - Align class/type call flow semantics (`__new__`, `__init__`, return constraints) with CPython.
  - Add metaclass-sensitive behavior needed for parity goals.
  - Done criteria:
    - MRO order in representative multi-inheritance fixtures matches CPython.
    - `__new__`/`__init__` edge-case behavior is fixture-covered and aligned.

- [ ] Milestone 7: Backend alignment and cleanup
  - Remove temporary compatibility shims and dead code paths.
  - Ensure interpreter, VM, and JIT paths all rely on shared runtime semantics for slot dispatch.
  - Document slot architecture, dispatch invariants, and extension points.
  - Done criteria:
    - No backend-specific dispatch logic remains for call/attribute/operator semantics.
    - Migration TODOs are either resolved or moved to explicitly non-goal backlog.

## Verification Strategy
- At every milestone:
  - Run `just test`.
  - Add targeted runtime fixture tests under `tests/programs/` for newly introduced behavior.
  - Verify unchanged behavior for existing programs.
- At transition milestones (2, 3, 4, 5, 6):
  - Add focused unit tests for slot dispatch helpers, protocol ordering, and error mapping.
  - Confirm no backend-specific divergence in call/attribute/operator behavior.
- Add a CPython parity matrix for tracked semantics:
  - Callability (`obj()` vs `obj.__call__`)
  - Binary op and reflected op fallback
  - Rich comparison fallback
  - Attribute protocol and descriptor precedence
  - Class construction (`__new__`/`__init__`) and MRO behavior

## Risks and Mitigations
- Risk: Semantic drift from CPython protocol ordering.
  - Mitigation: Encode protocol ordering in shared helpers and assert with dedicated fixtures.
- Risk: Large trait/API churn causing broad breakage.
  - Mitigation: Introduce compatibility shims and migrate in small, testable slices.
- Risk: Backend divergence (interpreter vs VM/JIT).
  - Mitigation: Require backend delegation to shared helpers in each milestone, not only at the end.
- Risk: Performance regressions from extra indirection.
  - Mitigation: Benchmark critical operations after each slot-dispatch migration step.

## Clarifying Defaults
1. Milestone 1 can be interpreter-first with VM/JIT bridged temporarily.
- Default: yes, but bridge must call shared helpers and not fork semantics.

2. Single inheritance is acceptable only as an intermediate milestone.
- Default: yes for Milestone 4 only; full plan includes C3 MRO and multiple inheritance.

3. Descriptor protocol is required before declaring CPython compatibility.
- Default: yes; Milestone 5 is mandatory for parity.

4. Generic magic-method attribute fallback is not a compatibility strategy.
- Default: no; use explicit CPython slot/protocol rules instead.

5. Metaclass behavior can be scoped, but class/type call semantics cannot be skipped.
- Default: include required metaclass/type-call behavior for parity targets in Milestone 6.

## Approval Gate
- Do not start implementation until explicit approval (for example: "approved", "go ahead", or "implement Milestone 1").
