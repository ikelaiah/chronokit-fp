# Implementation Plan: v1.7.0 executable learning path and focused API gaps

## Scope

Deliver only the v1.7.0 roadmap milestone: three reviewed additive workflows,
an executable progressive learning path, decision guidance and audit records,
a generated API reference with a coverage check, and clean-consumer checks for
the documented source and Lazarus installation paths. The preferred v1.7 API
is frozen by this release; no v1.8, v1.9, or 2.0 work is included.

## Architecture decisions

- `BusinessDaysBetween` counts both calendar-date endpoints, ignores their
  time portions, returns a negative count for reverse order, and applies the
  same calendar validation and holiday/working-week rules as existing helpers.
- `ConvertBetweenTimeZones` resolves the named source wall clock directly to
  UTC and then converts UTC to the named target. It does not chain through the
  system zone, so a system DST overlap cannot change a named-zone conversion.
- Learning examples are standalone `.lpr` programs. Their source is the
  canonical documentation listing, and CI compiles and runs them on both
  supported platforms.
- The API-reference generator derives Markdown from `TChronoKit` public
  declaration comments and fails when a non-deprecated declaration has no
  useful contract, error rule, or example reference.

## Task list

### Phase 1: Contract and API behavior

1. [x] Publish the v1.7 workflow contract, including endpoint, direction,
       time-component, timezone-error, and boundary behavior.
2. [x] Add failing focused FPCUnit coverage for named-zone conversion, quarter
       value boundaries, and business-day counting.
3. [x] Implement only those three public additions with declaration comments.

### Checkpoint: API additions

- [x] New focused tests pass.
- [x] Existing Windows/Linux timezone matrix remains meaningful and green.

### Phase 2: Learning and discoverability

4. [x] Add five progressive, runnable learning programs for dates/wall clocks,
       periods/durations, half-open ranges, business calendars, and named
       timezones; include the new workflows where they belong.
5. [x] Publish the progressive guide, decision guides, updated task references,
       and the v1.7 beginner audit. Record out-of-scope observations as
       post-2.0 design input only.
6. [x] Generate and check a searchable API reference from public declaration
       comments, with a committed generated reference and a stale/coverage
       failure mode.

### Phase 3: Reproducible verification and release records

7. [x] Add clean source and Lazarus consumer fixtures/checks; run the
       executable examples and documentation/reference checks in CI.
8. [x] Update version metadata, README, changelog, roadmap, release notes,
       PR summary, and task checklist for v1.7.0.

### Checkpoint: complete

- [x] FPCUnit, examples, package/consumer checks, generated-reference check,
       and documentation links pass.
- [x] Windows and Linux CI execute the same v1.7 behavior and examples.
- [x] No public addition beyond the three v1.7 workflows is present.

## Risks and mitigations

| Risk | Mitigation |
|---|---|
| Named conversion crosses a system-zone overlap | Resolve the source directly to UTC before converting to target. |
| `TDateTime` time components make counts surprising | State and test that counting uses calendar dates only. |
| Examples drift from prose | Link prose to standalone sources and compile/run them in CI. |
| Generated docs become stale | Regenerate in check mode and compare with the committed reference. |
| Lazarus availability differs by platform | Build a clean consumer fixture with the installed Lazarus tool in CI. |
