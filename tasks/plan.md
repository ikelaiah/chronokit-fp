# Implementation Plan: v1.6.0 API consolidation and deprecations

**Status:** Completed on 2026-08-12. See `tasks/todo.md` for the verified
completion checklist.

## Overview

Implement the accepted `docs/API-Deprecations-v1.6.0.md` transition contract:
add explicit period/duration and half-open range APIs, make preferred names the
canonical paths, correct the seven listed defects, annotate every superseded
declaration that Free Pascal 3.2.2 can annotate, and publish a complete
1.6-to-2.0 migration index. All existing 1.x declarations remain present and
source-compatible. No 2.0 removal or unrelated API change is in scope.

## Architecture decisions

- Keep all public compatibility types and methods in `src/ChronoKit.pas`.
- Represent calendar-relative values with `TCalendarPeriod` and exact elapsed
  values with a checked `Int64` millisecond `TDuration`.
- Represent intervals canonically as validated half-open `TDateTimeRange`
  values, using arrays and Boolean `Try*` results instead of sentinel dates.
- Implement preferred methods directly. Equivalent deprecated methods delegate
  toward them; incompatible legacy behavior stays isolated.
- Use Free Pascal 3.2.2 deprecation directives only where the compiler supports
  the declaration kind, with documentation markers for unsupported enum values.
- Preserve timezone rules and conversion semantics; only introduce clearer
  directional entry points and delegate legacy names to them.
- Update current documentation to teach preferred APIs and keep one explicit
  migration guide for deprecated names.

## Task list

### Phase 1: Compiler contract and correctness regressions

#### Task 1: Verify deprecation syntax and add regression tests

**Description:** Verify the Free Pascal 3.2.2 directive forms for methods,
records, enums, and enum values, then add tests that expose the seven listed
v1.5 correctness defects before production changes.

**Acceptance criteria:**

- [ ] The supported deprecation directive forms and enum-value limitation are
      established with compiler evidence.
- [ ] Regression tests cover ceiling rollovers, end boundaries, fractional
      legacy durations, sub-day legacy gaps, decimal round trips, reversed
      legacy intervals, and `duSeason` errors.
- [ ] Each new regression fails against the current implementation for the
      intended reason.

**Verification:**

- [ ] `fpc "-FU." "-Fu..\src" TestRunner.lpr` fails at the expected tests.

**Dependencies:** None

**Files likely touched:**

- `tests/ChronoKit.Test.pas`
- `build-temp/deprecation-probe.pas` (temporary, removed after the probe)

**Estimated scope:** Small

#### Task 2: Correct retained and legacy behavior

**Description:** Fix only the defects enumerated in the v1.6 transition spec,
without changing unrelated rounding, interval, or timezone contracts.

**Acceptance criteria:**

- [ ] All seven regression categories pass.
- [ ] Month-end rolling aliases are proven equivalent to `AddMonths` before
      annotation.
- [ ] No canonical replacement depends on incompatible legacy code.

**Verification:**

- [ ] Focused regression tests pass.
- [ ] Existing tests continue to pass except fixture-gated timezone tests.

**Dependencies:** Task 1

**Files likely touched:**

- `src/ChronoKit.pas`
- `tests/ChronoKit.Test.pas`

**Estimated scope:** Medium

### Checkpoint: Correctness baseline

- [ ] The seven specified defects are covered and fixed.
- [ ] No API has been removed or had its signature changed.

### Phase 2: Explicit replacement APIs

#### Task 3: Add calendar-period and exact-duration APIs

**Description:** Add the replacement value types, checked constructors,
normalization, arithmetic, and elapsed-difference operations with tests written
first.

**Acceptance criteria:**

- [ ] Calendar periods apply components in the specified order and support
      month-end/leap behavior without converting months or years to seconds.
- [ ] Durations store exact milliseconds and checked construction raises
      `ERangeError` on overflow.
- [ ] Negative values and `DurationBetween` one-millisecond accuracy are tested.

**Verification:**

- [ ] New focused period/duration tests pass.
- [ ] Full suite remains green apart from documented missing fixtures.

**Dependencies:** Task 2

**Files likely touched:**

- `src/ChronoKit.pas`
- `tests/ChronoKit.Test.pas`

**Estimated scope:** Medium

#### Task 4: Add validated half-open range APIs

**Description:** Add the new range types and operations for containment,
overlap, touch, exact duration/gap, subtraction, merge, and intersection.

**Acceptance criteria:**

- [ ] Invalid ordering raises `EArgumentException`; equal endpoints represent
      an empty range.
- [ ] Empty, disjoint, intersecting, touching, and split cases have exact,
      sentinel-free results.
- [ ] Sub-day and millisecond precision are retained.

**Verification:**

- [ ] New focused range tests pass.
- [ ] Full suite remains green apart from documented missing fixtures.

**Dependencies:** Task 3

**Files likely touched:**

- `src/ChronoKit.pas`
- `tests/ChronoKit.Test.pas`

**Estimated scope:** Medium

#### Task 5: Add explicit parsing, decimal-year, and timezone names

**Description:** Add `StartOfQuarter`, corrected bidirectional decimal-year
conversion, and explicit timezone-direction methods, then redirect equivalent
legacy entry points to the canonical implementations.

**Acceptance criteria:**

- [ ] Quarter validation covers year `1..9999` and quarter `1..4`.
- [ ] Decimal conversions include time of day and round-trip within one
      millisecond across common and leap years.
- [ ] Timezone replacements retain all v1.3/v1.4 DST and validation behavior.

**Verification:**

- [ ] Focused tests for each replacement pass.
- [ ] The named-timezone regression matrix passes with local fixtures.

**Dependencies:** Task 4

**Files likely touched:**

- `src/ChronoKit.pas`
- `tests/ChronoKit.Test.pas`

**Estimated scope:** Medium

### Checkpoint: Replacement surface

- [ ] Every replacement in the transition specification is public and tested.
- [ ] Canonical implementations do not call deprecated methods.
- [ ] Existing callers remain source-compatible.

### Phase 3: Deprecations, migration, and release records

#### Task 6: Annotate the complete deprecation matrix

**Description:** Add supported compiler deprecation directives with actionable
messages, document unsupported enum-value annotations, and compile a legacy
compatibility fixture that exercises every retained declaration.

**Acceptance criteria:**

- [ ] Every matrix declaration has a compiler annotation or documented marker.
- [ ] Warning messages identify the replacement or migration direction.
- [ ] A compatibility fixture compiles under Free Pascal 3.2.2.

**Verification:**

- [ ] Compiler output contains expected deprecation diagnostics.
- [ ] Unit tests and examples still compile with warnings enabled.

**Dependencies:** Task 5

**Files likely touched:**

- `src/ChronoKit.pas`
- `tests/LegacyCompatibility.pas`
- `tests/ChronoKit.Test.pas`

**Estimated scope:** Medium

#### Task 7: Publish migration guidance and v1.6 release records

**Description:** Update user-facing API guidance to preferred v1.6 paths,
publish the complete migration guide/removal list, and update version metadata,
changelog, roadmap status, release notes, and PR notes after verification.

**Acceptance criteria:**

- [ ] Every deprecated declaration has an actionable migration example.
- [ ] Current guides and shipped examples teach only preferred APIs.
- [ ] Metadata and release records consistently report v1.6.0 and explicitly
      defer removals to 2.0.

**Verification:**

- [ ] Documentation links and public-method coverage checks pass.
- [ ] Every shipped example compiles and the Lazarus package builds.
- [ ] Full fixture-backed FPCUnit suite passes.
- [ ] `git diff --check` and final code review pass.

**Dependencies:** Task 6

**Files likely touched:**

- `README.md`
- `docs/Getting-Started.md`
- `docs/ChronoKit-FP.md`
- `docs/Cheat-Sheet.md`
- `docs/MIGRATION-v1.6-to-v2.0.md`
- `docs/RELEASE-NOTES-v1.6.0.md`
- `docs/PR-v1.6.0.md`
- `CHANGELOG.md`
- `ROADMAP.md`
- `packages/lazarus/chronokit_fp.lpk`
- shipped examples where legacy names appear

**Estimated scope:** Medium

### Checkpoint: Complete

- [ ] Every v1.6.0 roadmap goal and success criterion is met.
- [ ] Windows-local tests, examples, package, links, and API coverage pass.
- [ ] CI retains the equivalent Windows/Linux release matrix.
- [ ] No declaration slated for 2.0 removal has been removed in v1.6.

## Risks and mitigations

| Risk | Impact | Mitigation |
|---|---|---|
| Checked `Int64` arithmetic is compiler-mode dependent | High | Use explicit precondition checks and test every multiplication/addition boundary. |
| `TDateTime` floating-point rounding loses a millisecond | High | Convert once at API boundaries and assert a one-millisecond round-trip tolerance. |
| Half-open empty ranges create ambiguous touch/merge cases | Medium | Treat empty ranges as containing no values and cover each relation explicitly. |
| Deprecating enum values is unsupported in FPC 3.2.2 | Medium | Verify with a compiler probe and use the required documentation marker when unsupported. |
| Legacy warnings obscure internal builds | Medium | Ensure new implementations never reference deprecated declarations and isolate compatibility tests. |
| Timezone aliases accidentally change conversion semantics | High | Move existing implementations under explicit names and rerun the complete named-zone matrix. |

## Open questions

None at scope level. The compiler probe in Task 1 determines only the supported
annotation syntax; it does not change the transition contract.
