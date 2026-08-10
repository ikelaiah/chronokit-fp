# Implementation Plan: v1.2.0 business calendars

## Overview

Deliver the v1.2.0 roadmap milestone with an additive business-calendar API.
Existing one-argument business-day calls remain Monday-to-Friday, while new
overloads accept explicit working days and holidays. The release also defines
calendar boundaries, improves invalid-date messages, and adds task-focused
recipes and release documentation.

## Architecture decisions

- Represent a calendar as a value record containing a set of working weekdays
  and an array of holiday dates. This keeps rules explicit and requires no
  global or mutable library state.
- Construct calendars through `CreateBusinessCalendar`; validate the calendar
  again at operation boundaries because record fields remain directly
  assignable in Free Pascal 3.2.2.
- Add overloads instead of changing existing signatures. The original helpers
  continue to implement the current Monday-to-Friday behavior by default.
- Compare holidays by calendar date while preserving the input time in all
  returned `TDateTime` values.
- Continue raising `EConvertError` for invalid text input, but include the
  rejected value, expected shape, and valid-calendar-date requirement.

## Task list

### Phase 1: Contract and compatibility

- [x] Task 1: Publish and review the v1.2.0 business-calendar API design.
- [x] Task 2: Add failing compatibility, holiday, alternative-week, and
  invalid-calendar tests.
- [x] Task 3: Implement calendar types, factories, and business-day overloads.

### Checkpoint: Configurable calculations

- [x] Focused business-calendar tests pass.
- [x] Existing business-day tests still pass without call-site changes.
- [x] Test runner compiles cleanly with Free Pascal 3.2.2.

### Phase 2: Boundaries and validation

- [x] Task 4: Add leap-year, month-end, week-start, zero-day, and
  time-preservation contract tests.
- [x] Task 5: Add failing assertions for actionable invalid-date messages,
  then update the parsing errors.

### Checkpoint: Behavior contract

- [x] Boundary behavior is covered by deterministic tests.
- [x] Invalid inputs identify the rejected value and accepted format.
- [x] Full FPCUnit suite passes.

### Phase 3: Recipes and release material

- [x] Task 6: Add reporting-period, deadline, and date-range recipes and update
  the README, cheat sheet, and API guide.
- [ ] Task 7: Update version metadata, roadmap status, changelog, release notes,
  and PR notes for v1.2.0.
- [ ] Task 8: Compile every shipped example and complete the final review.

### Checkpoint: Complete

- [ ] All v1.2.0 roadmap goals and done criteria are met.
- [ ] Full test suite and all examples compile.
- [ ] Diff passes correctness, readability, architecture, security, and
  performance review.

## Risks and mitigations

| Risk | Impact | Mitigation |
|---|---|---|
| New rules change legacy behavior | High | Preserve original signatures and test their exact defaults. |
| Empty working weeks cause unbounded searches | High | Reject calendars with no working day at every public operation boundary. |
| Holiday timestamps compare incorrectly | Medium | Compare only calendar dates and test non-midnight holidays. |
| Cross-platform date parsing differs | Medium | Keep locale-independent helper formats and assert messages, not RTL wording. |
| Large public unit becomes harder to navigate | Medium | Keep the additive model small and place helpers beside existing business-day logic. |

## Open questions

None. The roadmap and current API provide enough constraints for an additive,
backwards-compatible implementation.
