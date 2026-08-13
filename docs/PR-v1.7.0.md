# release(v1.7.0): deliver executable workflows and maintainable internals

Release date: 2026-08-14

## Scope

Implements the complete v1.7.0 milestone in `ROADMAP.md`: executable learning,
three contract-first workflow additions, generated reference coverage, clean
consumer verification, and the maintainability work originally scheduled for
v1.8. The internal work was folded into v1.7 rather than shipped as a separate
later release. This release still excludes release-rehearsal automation, new
aliases, recurrence, and new instant/zoned value types.

## Changes

- adds direct named-zone conversion, value quarter boundaries, and inclusive
  business-day counting with focused FPCUnit coverage;
- publishes contracts, decision guides, audit results, and five runnable
  learning programs;
- generates the preferred public reference from declaration comments and
  checks it for staleness and missing useful documentation; and
- verifies clean source and Lazarus-package consumer fixtures alongside the
  Windows/Linux test and executable-example workflow;
- splits all 178 tests into nine domain suites without changing their
  assertions;
- freezes the complete Windows and Linux v1.7 façade in checked API manifests;
- moves non-trivial implementations into focused calendar, duration, range,
  business-calendar, parsing, and legacy units behind `ChronoKit`; and
- unifies timezone conversion orchestration while retaining the existing
  platform engine and its established DST behavior.

## Compatibility and risk

No 1.x declaration is removed or newly deprecated. Timezone conversion resolves
the named source directly to UTC before rendering in the target, so it cannot
be affected by a system-timezone DST overlap. The preferred v1.7 public surface
is closed through v1.9. Both platform API manifests remain unchanged after the
internal refactor, and preferred domain units do not depend on legacy code.

## Verification

- 178 FPCUnit tests pass on the Windows release fixture, with the same
  cross-platform timezone matrix used by CI.
- The Windows and Linux API manifests and generated 95-method preferred
  reference are current.
- The Lazarus package, legacy compatibility fixture, eight examples, and both
  clean consumer fixtures compile successfully.
