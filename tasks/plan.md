# Implementation Plan: v1.4.0 trustworthy timezones

## Overview

Deliver the v1.4.0 roadmap milestone by making the v1.3.0 timezone contract
executable on Windows and Linux. Named-zone conversion will use the requested
zone and the supplied date, platform-native rules will replace hard-coded US
transitions, and every local wall clock will be classified as valid,
ambiguous, or nonexistent before it is converted.

The public timezone signatures remain source-compatible. The implementation
will move platform-specific rule lookup into a small internal unit so the
public `ChronoKit` unit only owns contract-level orchestration and errors.

## Architecture decisions

- Resolve local clocks by testing the platform engine's possible UTC offsets.
  Zero matching instants means nonexistent, one means valid, and two means
  ambiguous. This makes gaps and overlaps visible without adding a caller
  policy that the 1.x API cannot represent.
- On Windows, read the registered timezone catalog and its per-year dynamic
  rules, then use the OS conversion API for the requested identifier/date.
- On Linux, read the installed IANA TZif file directly. This avoids changing
  the process-wide `TZ` variable during conversion and uses the host's own
  timezone database, including historical transitions.
- Keep `UTC` as the sole portable identifier and retain the documented Linux
  `Etc/UTC` aliases. Return canonical platform identifiers from discovery and
  system-zone queries.
- Add no dependency and no new public type or function. Continue reporting all
  lookup, conversion, ambiguity, and gap failures through `ETimeZoneError`.

## Task list

### Phase 1: Executable contract

#### Task 1: Add failing named-zone and DST classification regressions

**Description:** Extend the shared matrix with logical Windows/Linux fixture
names supplied by CI. Prove that target names affect conversion, northern and
southern hemisphere rules differ, and gaps/overlaps raise descriptive errors.

**Acceptance criteria:**

- [x] A non-system target conversion produces the target-zone wall clock.
- [x] Sydney and New York seasonal offsets follow their own platform rules.
- [x] System-local and named-source gaps/overlaps raise `ETimeZoneError` with
      the value, zone, and classification in the message.

**Verification:**

- [x] The new tests fail against the v1.3.0 implementation for the expected
      contract gaps.
- [x] `fpc "-FU." "-Fu..\src" TestRunner.lpr` compiles the expanded suite.

**Dependencies:** None

**Files likely touched:**

- `tests/ChronoKit.Test.pas`
- `.github/workflows/test.yml`

**Estimated scope:** Medium

#### Task 2: Implement the platform-native timezone engine

**Description:** Add one internal unit that discovers platform identifiers,
maps UTC instants with native rules, and classifies local wall clocks without
guessing.

**Acceptance criteria:**

- [x] Windows uses dynamic timezone data for the requested identifier/date.
- [x] Linux parses installed TZif transition/type data for the requested IANA
      identifier/date without mutating process-global timezone state.
- [x] UTC and documented aliases remain deterministic and dependency-free.

**Verification:**

- [x] The engine compiles with Free Pascal 3.2.2 on the local Windows target.
- [x] Focused engine-facing regressions pass after implementation.

**Dependencies:** Task 1

**Files likely touched:**

- `src/ChronoKitTimeZones.pas`
- `packages/lazarus/chronokit_fp.lpk`

**Estimated scope:** Medium

### Checkpoint: Platform engine

- [x] Named target conversion fails before implementation and passes after it.
- [x] No hard-coded regional DST transition calculation remains in use.
- [x] The source tree builds without a new dependency.

### Phase 2: Public contract conformance

#### Task 3: Route public timezone operations through classified conversions

**Description:** Make `GetTimeZone`, `WithTimeZone`, `ForceTimeZone`, discovery,
and system-zone lookup delegate to the engine while preserving public
signatures and translating backend failures to `ETimeZoneError`.

**Acceptance criteria:**

- [x] Every conversion uses the supplied wall clock and requested zone.
- [x] Ambiguous/nonexistent local inputs never return a guessed instant.
- [x] Lookup failures never silently fall back to UTC.

**Verification:**

- [x] The full FPCUnit suite passes locally.
- [x] Public declarations have no breaking signature or type change.

**Dependencies:** Task 2

**Files likely touched:**

- `src/ChronoKit.pas`
- `tests/ChronoKit.Test.pas`

**Estimated scope:** Medium

#### Task 4: Complete the cross-platform release matrix

**Description:** Configure equivalent named-zone fixtures in both CI jobs and
ensure all contract assertions run unchanged on Windows and Linux.

**Acceptance criteria:**

- [x] CI supplies native identifiers for New York, London, Sydney, Tokyo, and
      Auckland on both operating systems.
- [x] Assertion bodies contain no platform-specific skip, tolerance, or pass.
- [x] Tests cover UTC, named conversion, seasonal rules, gaps, overlaps, date
      boundaries, round trips, and invalid names.

**Verification:**

- [x] Workflow syntax and commands match the repository's established jobs.
- [x] Local Windows run passes with the same logical fixture variables.

**Dependencies:** Task 3

**Files likely touched:**

- `.github/workflows/test.yml`
- `tests/ChronoKit.Test.pas`

**Estimated scope:** Small

### Checkpoint: Contract conformance

- [x] The complete v1.3.0 matrix is executable without skipped behavior.
- [x] The local Free Pascal 3.2.2 suite passes.
- [x] CI is ready to enforce identical Windows/Linux semantics.

### Phase 3: Documentation and release material

#### Task 5: Update user and API documentation

**Description:** Replace v1.3.0 implementation caveats with copyable guidance
for choosing conversion operations, native identifiers, and handling DST
boundary exceptions.

**Acceptance criteria:**

- [x] README, getting started, API guide, cheat sheet, troubleshooting, and
      timezone contract describe shipped v1.4.0 behavior consistently.
- [x] Each public timezone operation has a copyable example and clear wall
      clock/instant semantics.
- [x] DST gap and overlap examples show `ETimeZoneError` handling.

**Verification:**

- [x] Documentation links resolve within the repository.
- [x] Examples use only public API available in v1.4.0.

**Dependencies:** Task 4

**Files likely touched:**

- `README.md`
- `docs/Getting-Started.md`
- `docs/ChronoKit-FP.md`
- `docs/Cheat-Sheet.md`
- `docs/Timezone-Contract.md`

**Estimated scope:** Medium

#### Task 6: Prepare v1.4.0 release records

**Description:** Record completion in version metadata, roadmap, changelog,
release notes, and PR notes.

**Acceptance criteria:**

- [x] Source and Lazarus package metadata report v1.4.0.
- [x] Roadmap marks the milestone released only after verification succeeds.
- [x] Changelog, release notes, and PR notes accurately state behavior and
      compatibility.

**Verification:**

- [x] Version search finds no current-release metadata left at v1.3.0.
- [x] Release records include the exact verification commands and outcomes.

**Dependencies:** Task 5

**Files likely touched:**

- `CHANGELOG.md`
- `ROADMAP.md`
- `docs/RELEASE-NOTES-v1.4.0.md`
- `docs/PR-v1.4.0.md`
- `packages/lazarus/chronokit_fp.lpk`

**Estimated scope:** Medium

### Checkpoint: Complete

- [x] All v1.4.0 roadmap goals and done criteria are met.
- [x] Full FPCUnit suite passes with Free Pascal 3.2.2 (154 tests per OS).
- [x] Every shipped example compiles and the Lazarus package builds.
- [x] `git diff --check` and the five-axis review find no required issue.
- [x] Windows/Linux CI configuration runs the same meaningful assertions.

## Risks and mitigations

| Risk | Impact | Mitigation |
|---|---|---|
| Windows registry/API declarations differ across FPC targets | High | Use the standard FCL registry unit plus one stable Win32 conversion entry point, and compile on FPC 3.2.2. |
| TZif variants or corrupt files produce unsafe reads | High | Validate magic, counts, indices, sizes, and every stream boundary before parsing. |
| DST classification accidentally chooses an occurrence | High | Accept a local clock only when exactly one candidate instant round-trips through native rules. |
| Target offset changes between source and destination dates | High | Resolve the source to an instant first, then query the target rule at that instant. |
| Documentation overstates unverified Linux behavior | Medium | Keep the shared CI matrix as the release gate and distinguish local verification from CI readiness. |

## Open questions

None. The v1.3.0 contract specifies the observable behavior and preserves the
public API, so implementation choices do not require a new product decision.
