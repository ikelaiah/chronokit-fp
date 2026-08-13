# feat(learning): deliver executable v1.7.0 workflows

Release date: 2026-08-13

## Scope

Implements only the v1.7.0 milestone in `ROADMAP.md`: executable learning,
three contract-first workflow additions, generated reference coverage, and
clean consumer verification. It deliberately excludes the v1.8 internal
refactor, API-manifest work, release-rehearsal work, new aliases, recurrence,
and new instant/zoned value types.

## Changes

- adds direct named-zone conversion, value quarter boundaries, and inclusive
  business-day counting with focused FPCUnit coverage;
- publishes contracts, decision guides, audit results, and five runnable
  learning programs;
- generates the preferred public reference from declaration comments and
  checks it for staleness and missing useful documentation; and
- verifies clean source and Lazarus-package consumer fixtures alongside the
  Windows/Linux test and executable-example workflow.

## Compatibility and risk

No 1.x declaration is removed or newly deprecated. Timezone conversion resolves
the named source directly to UTC before rendering in the target, so it cannot
be affected by a system-timezone DST overlap. The preferred v1.7 public surface
is closed through v1.9.
