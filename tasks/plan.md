# Implementation plan: v1.7 maintainability refactor

## Scope

Simplify ChronoKit-FP's internals without changing the frozen v1.7 public API,
runtime behavior, version metadata, or documented user model. Work proceeded
in small, independently verified slices. The maintainability work originally
listed under v1.8 was folded into the v1.7.0 release rather than shipped as a
separate milestone.

## Architecture decisions

- `ChronoKit.pas` remains the only taught public facade.
- Shared domain values use `ChronoKitInternalTypes.pas`; the facade maps them
  to unchanged public records because FPC 3.2.2 enum aliases do not re-export
  enum identifiers.
- Domain units depend on `ChronoKitInternalTypes` and the RTL, never on the facade.
- Equivalent deprecated aliases remain one-line facade delegates. Historical
  algorithms with incompatible semantics belong in `ChronoKitLegacy.pas` and
  are never called by preferred implementations.
- The existing timezone engine remains intact until every other extraction is
  complete. Its platform sections are split only if that removes coupling
  without duplicating shared TZif or conversion logic.

## Task list

### Phase 1: Refactoring guardrails

1. Remove unused and stale test scaffolding without changing assertions.
2. Split the monolithic FPCUnit class into domain suites and retain all 178
   registered tests.
3. Add checked Windows and Linux v1.7 API manifests covering public constants,
   types, methods, directives, visibility, and platform-specific declarations.
4. Record the internal dependency direction and contributor placement rules.

### Checkpoint: guardrails

- The same 178 tests pass.
- Legacy compatibility, examples, package, consumers, and documentation pass.
- API manifests match on the platform that generated them.

### Phase 2: Foundational seams

5. Introduce `ChronoKitInternalTypes.pas`, add explicit facade mappings, and
   verify preferred and legacy consumers compile unchanged.
6. Extract exact duration and half-open range implementations.
7. Extract business-calendar implementations.

### Checkpoint: foundational domains

- Public API manifests are unchanged.
- Domain suites and the full verification matrix pass.
- Preferred units do not depend on legacy code.

### Phase 3: Calendar and text domains

8. Extract calendar arithmetic, boundaries, comparisons, calendar systems,
   decimal years, and rounding.
9. Extract parsing and formatting.
10. Isolate incompatible deprecated algorithms in `ChronoKitLegacy.pas` while
    leaving equivalent aliases as direct facade delegates.

### Checkpoint: facade

- `ChronoKit.pas` contains declarations and thin orchestration rather than
  domain algorithms.
- API manifests and all behavior checks remain unchanged.

### Phase 4: Timezones and completion

11. Collapse duplicated facade timezone conversion orchestration around one
    named-source-to-named-target path.
12. Review Windows and Linux backend cohesion and either split them with full
    matrix evidence or record why the existing conditional unit is clearer.
13. Run the complete release verification and conduct final code review.

## Risks and mitigations

| Risk | Mitigation |
|---|---|
| Shared types change compiler-visible identity | Keep public declarations in the facade and map internal records explicitly |
| Moving tests silently drops registration | Compare the registered test count and test-name inventory before and after |
| Refactoring changes deprecated behavior | Keep legacy behavior tests and compile fixture separate and green |
| Unit dependencies become circular | Enforce `facade -> domains -> types/RTL` and check compiler unit order |
| Platform extraction duplicates timezone logic | Split backends only when shared logic remains single-owned |
| Large moves hide behavior edits | One domain per commit; review moved bodies against the original |

## Open questions

None. The user approved the complete audit sequence on 2026-08-13.
