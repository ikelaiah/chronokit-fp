# Implementation Plan: v1.3.0 timezone contract and regression suite

## Overview

Deliver the v1.3.0 roadmap milestone as a contract-first correctness release.
The public timezone signatures remain unchanged. The release will define the
portable and platform-native identifier sets, state the wall-clock/instant
semantics of every timezone operation, define DST-boundary failure behavior,
and replace platform-specific skips with one shared set of meaningful
regression assertions on Windows and Linux.

## Architecture decisions

- Treat `UTC` as the canonical portable identifier. Document platform-native
  mappings separately: IANA identifiers on Linux and Windows identifiers on
  Windows. Aliases are compatibility inputs, not portable storage values.
- Treat `TDateTime` as an unzoned wall-clock representation. Each operation's
  source-zone assumption and whether it preserves an instant or clock fields
  must therefore be explicit.
- Keep every existing public type and signature unchanged. v1.3.0 may tighten
  tests, diagnostics, and internal correctness only where required to make the
  established contract executable; broader named-zone implementation remains
  v1.4.0 work.
- Use a single data-driven regression matrix and identical assertions on both
  operating systems. CI supplies platform-specific names for the same logical
  zones rather than branching assertions in Pascal.
- Reject ambiguous and nonexistent local times with `ETimeZoneError` under the
  contract. The documentation will distinguish this required behavior from
  legacy behavior that v1.4.0 must finish implementing across named zones.

## Task list

### Phase 1: Contract and baseline

- [x] Task 1: Publish the normative v1.3.0 timezone contract, including
  identifier mappings and operation semantics.
- [x] Task 2: Capture the existing 145-test baseline and identify every
  unconditional pass, tolerance, and platform-specific skipped assertion.

### Checkpoint: Contract

- [x] Supported identifiers, result semantics, and errors are explicit.
- [x] Ambiguous and nonexistent local-time policies are explicit.
- [x] No public API addition or signature change is proposed.

### Phase 2: Shared regression matrix

- [x] Task 3: Replace skipped timezone checks with shared assertions for UTC
  offsets, DST boundaries, conversions, and invalid inputs.
- [x] Task 4: Configure Windows and Linux CI with equivalent logical timezone
  fixtures and run the same test runner on both.
- [x] Task 5: Make only the minimal internal corrections required for the
  shared v1.3.0 assertions, using failing tests first.

### Checkpoint: Cross-platform behavior

- [x] No timezone assertion is bypassed with `IFDEF`, unconditional success,
  diagnostic-only output, or platform-specific tolerance.
- [x] Focused timezone tests pass locally on Windows.
- [x] The complete test runner remains compilable with Free Pascal 3.2.2.

### Phase 3: Documentation and release material

- [x] Task 6: Align the README, getting-started guide, troubleshooting guide,
  API guide, and cheat sheet with the contract.
- [x] Task 7: Update version metadata, roadmap status, changelog, release
  notes, and PR notes for v1.3.0.
- [x] Task 8: Compile the full suite, shipped examples, and Lazarus package,
  then complete the five-axis review.

### Checkpoint: Complete

- [x] All v1.3.0 roadmap goals and done criteria are met.
- [x] Full FPCUnit suite and every shipped example compile.
- [x] Public API diff confirms no signature or type changes.
- [x] Diff passes correctness, readability, architecture, security, and
  performance review.

## Risks and mitigations

| Risk | Impact | Mitigation |
|---|---|---|
| Contract work accidentally implements v1.4.0 | High | Limit behavior changes to what is needed by the shared v1.3.0 matrix and record named-zone engine work as excluded scope. |
| Platform identifier names are treated as interchangeable | High | Publish a logical-zone mapping table and designate only `UTC` as portable. |
| Tests pass without proving behavior | High | Remove unconditional passes, skipped assertions, broad tolerances, and OS-specific expectations from timezone tests. |
| Host timezone makes tests nondeterministic | High | Set an explicit equivalent timezone in both CI jobs and restore process environment in tests. |
| Documentation overstates current guarantees | High | Make the normative contract and remaining v1.4.0 conformance gap explicit. |

## Open questions

None. The roadmap explicitly separates the v1.3.0 contract and regression
suite from the broader v1.4.0 named-zone implementation.
