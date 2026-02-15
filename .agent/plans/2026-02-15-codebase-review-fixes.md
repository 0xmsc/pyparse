# Codebase Review Remediation Plan

## Objective
Address the issues found in the codebase review: eliminate user-reachable panics, improve CPython compatibility, reduce backend drift, and simplify duplicated logic.

## Scope
In scope:
- Runtime panic fixes for magic methods.
- Function/class redefinition semantics alignment.
- Frontend validation for `return` outside functions.
- Interpreter/VM duplication reduction for binary-op dispatch.
- `just` workflow cleanup for lint check vs auto-fix.
- Regression fixtures for the above.

Out of scope:
- New language features beyond current overlap target.
- JIT feature expansion beyond currently documented unsupported areas.

## Success Criteria
- No process panics for validly parsed user programs in interpreter/VM paths.
- Interpreter and VM agree on targeted semantics.
- Behavior matches CPython/PyPy/MicroPython overlap for covered cases.
- `just test` and `just lint` pass.

## Approach
Fix correctness first (panic and semantic mismatches), then centralize shared behavior, then lock in with fixtures and validation command updates.

## Steps
- [ ] Fix `src/runtime/value.rs` to avoid `NoopCallContext` panics for magic-method execution (`__str__`, `__repr__`, `__bool__`, `__len__`) by using real runtime call context and returning `RuntimeError` instead of panic.
- [ ] Update interpreter and VM call sites so operations that may trigger magic methods (truthiness, output rendering, indexing, length, operator dispatch) use context-aware APIs.
- [ ] Align top-level function redefinition semantics with CPython: last definition wins (remove duplicate-definition hard errors in interpreter/VM preparation paths).
- [ ] Align duplicate class-method semantics across backends with CPython behavior (last method wins in VM path, matching interpreter behavior).
- [ ] Add frontend validation pass for `return` outside functions so this is reported as frontend error independent of backend.
- [ ] Keep backend guards for `return` outside function as defensive checks only (should be unreachable via normal frontend pipeline).
- [ ] Extract shared binary-op helper(s) in runtime layer to remove duplicated interpreter/VM attribute lookup and error-mapping code.
- [ ] Split `just` lint workflow into check-only and fix variants (`lint` vs `lint-fix`) to avoid unexpected mutation during checks.
- [ ] Add/adjust `tests/programs` fixtures for:
- [ ] custom `__str__`, `__bool__`, `__len__` methods on user classes,
- [ ] top-level function redefinition,
- [ ] duplicate class method redefinition,
- [ ] frontend `return`-outside-function error case.
- [ ] Run `just test` and `just lint`, then fix any regressions.

## Verification
- Functional:
  - New fixtures pass for interpreter and VM.
  - Parity backends (`cpython`, `pypy`, `micropython`) match where enabled.
- Safety:
  - No `panic!` for reviewed runtime paths on user input.
- Tooling:
  - `just lint` is non-mutating.
  - `just lint-fix` performs autofixes.

## Risks and Mitigations
- Risk: changing magic-method call plumbing introduces borrow/lifetime issues.
  - Mitigation: land minimal API changes first, then incrementally switch call sites.
- Risk: redefinition semantic changes can mask accidental duplicates.
  - Mitigation: add explicit fixtures documenting intended overwrite behavior.
- Risk: parser/validation changes alter existing error messages.
  - Mitigation: stabilize error wording in fixtures and avoid backend-specific phrasing.

## Clarifying Questions (with proposed answers)
1. Should duplicate top-level `def` be an error or overwrite?
   - Proposed answer: overwrite (last definition wins) for CPython compatibility.
2. Should duplicate class methods be handled the same as duplicate functions?
   - Proposed answer: yes, overwrite in declaration order (last method wins).
3. Should we enforce `return`-outside-function at parse time or post-parse validation?
   - Proposed answer: post-parse AST validation pass to keep parser simple and make rule reusable.
4. Should backend defensive checks for invalid `return` remain after frontend validation?
   - Proposed answer: yes, retain as safety assertions with clear errors.

