# Roadmap

ChronoKit-FP's priority through 2.0.0 is a library that is easy to discover,
install, and use correctly. This roadmap describes the intended direction, not
a release-date commitment. Features may move between releases as maintainer
priorities, implementation evidence, and cross-platform testing reveal what
needs the most attention; user feedback is useful input but is not a gate.

## Primary user

ChronoKit-FP is for both experienced Pascal developers and developers who are
new to Free Pascal. The latter is the design baseline: they should be able to
recognise the right operation, copy a small working example, and understand
the result without first learning the library's internal concepts.

For the common path, users should not need to think about time zones, DST, or
calendar edge cases until their application genuinely needs them. When those
concepts do matter, the API and documentation must make the choice explicit.

## Principles

- Keep the common path small: a new user should be able to install the library
  and complete a basic date task in a few minutes.
- Prefer clear, predictable behaviour over adding overlapping helpers.
- Document every public feature with a copy-and-pasteable example.
- Teach one concept at a time, starting with dates before introducing business
  calendars, time zones, or DST.
- Use terminology that is familiar to a developer new to Free Pascal; explain
  Free Pascal-specific setup at the point it is needed.
- Keep Windows and Linux behaviour covered by automated tests.
- Preserve backwards compatibility throughout 1.x; announce and document any
  2.0 migration before making breaking changes.
- Treat user feedback as useful prioritisation input, not as a gate for
  progress or release decisions. Maintainer experience, implementation
  evidence, API coherence, and verified correctness are sufficient grounds to
  advance the roadmap.

## 1.1.0 — First five minutes

**Status:** Released 2026-08-10

Make it straightforward for a new Free Pascal or Lazarus user to get started.

- Publish one verified installation path for Lazarus and one for source-based
  projects.
- Refresh the quick start around the most common operations: creating,
  formatting, parsing, and adding dates.
- Add a concise troubleshooting guide for compiler search paths, formats, and
  platform requirements.
- Audit examples so they compile and use one consistent style.
- Use the Windows and Linux pull-request test suite as the release gate.

**Done when:** a developer new to Free Pascal can follow a single short guide,
run an example, and understand the difference between a date, a local
date/time, and a timezone conversion without inferring missing setup steps.

## 1.2.0 — Business calendars

**Status:** Released 2026-08-10

Extend the existing Monday-to-Friday business-day helpers without surprising
current users.

- Publish and review a small API design for holidays and alternative working
  weeks before implementing it.
- Make business-day calculations configurable for those rules while preserving
  the current Monday-to-Friday behaviour by default.
- Add focused recipes for reporting periods, deadlines, and date ranges.
- Define and test boundary behaviour for leap years, month ends, and week
  starts.
- Improve validation messages for invalid date input.

**Done when:** common calendar rules are expressed directly, existing code
keeps its current behaviour by default, and edge cases are documented.

## 1.3.0 — Timezone contract and regression suite

**Status:** Released 2026-08-11

Define what the existing timezone API must mean before changing its
implementation. This is a correctness release, not a feature-expansion
release.

- Standardise supported timezone identifiers and document platform mappings.
- Specify whether each operation preserves an instant or local clock time.
- Specify the result or error for ambiguous and nonexistent DST local times.
- Build a shared Windows/Linux regression matrix for UTC offsets, DST
  transitions, and conversions; remove platform-specific skipped assertions.
- Keep the current public API unchanged while this contract is established.

**Done when:** the supported input, result, and failure behaviour is written
down and covered by the same meaningful assertions on both platforms.

## 1.4.0 — Time zones users can trust

**Status:** Released 2026-08-11

Implement the 1.3.0 timezone contract across Windows and Linux.

- Ensure conversions use both the supplied date/time and the requested target
  timezone.
- Make timezone data and DST rules platform-appropriate rather than relying on
  one region's transition rules.
- Make ambiguous and nonexistent local times visible to callers instead of
  silently guessing.
- Release only when the 1.3.0 regression matrix passes on both platforms.

**Done when:** a developer can choose the right operation from the
documentation and predict its result at a DST boundary on either platform.

## 1.5.0 — Discoverable API and 2.0 decision

**Status:** Completed 2026-08-11

Reduce friction in the public surface while deciding, from evidence, whether a
major-version change is worthwhile.

- Group the API documentation by task and add a searchable cheat sheet.
- Run a beginner-focused API and documentation audit using common date/time
  tasks.
- Introduce clearer additive helpers only where the audit identifies a real
  discovery problem; retain compatibility wrappers through 1.x.
- Add examples and tests for every public API addition.
- Publish a proposed 2.0 change list only if 1.x usage shows that deprecation
  is justified.

**Done when:** users can find a documented, tested answer to common date/time
questions without reading the implementation, and the project has an
evidence-based decision about 2.0.0.

The completed [beginner API audit](docs/API-Audit-v1.5.0.md) justified two
additive discovery names and a task-oriented documentation rewrite. The
[2.0 decision](docs/V2-DECISION.md) is to continue compatible 1.x releases;
current evidence does not justify a breaking change list or deprecation.

## 1.6.0 — API consolidation and deprecations

**Status:** Completed on 2026-08-12

Make the public surface smaller and more coherent without removing APIs in
the 1.x line. Maintainer cost, duplicated entry points, misleading behaviour,
and a clear replacement are sufficient evidence for deprecation; this
milestone does not wait for external usage feedback.

- Publish an exact deprecation matrix containing the replacement and migration
  recipe for every affected API. The accepted
  [v1.6 API transition specification](docs/API-Deprecations-v1.6.0.md)
  contains that matrix and is the implementation contract for this milestone.
- Consolidate redundant aliases, fixed-format parsers, decimal-year names, and
  timezone conversions whose direction is hidden by their current names.
- Replace the tagged `TDateSpan` model with separate calendar-period and exact
  duration types; no elapsed-time API may approximate months or years as
  seconds.
- Replace inclusive `TInterval` algebra with validated half-open ranges that
  can represent empty, disjoint, intersecting, touching, and split results
  without sentinel dates or discarded ranges.
- Deprecate `duSeason`; its current type cannot express a hemisphere or season
  definition, and rounding must no longer silently return the input.
- Make preferred APIs the canonical implementations where contracts are
  equivalent. Keep incompatible deprecated behaviour isolated from all new
  implementation paths until removal in 2.0.
- Correct the separately identified rounding, end-boundary, fractional-span,
  interval-gap, decimal-round-trip, and interval-validation defects.
- Add compiler deprecation annotations, tests for every replacement, and a
  complete 1.6-to-2.0 migration guide.
- Treat v1.6.0 as the final planned 1.x API-consolidation release. Fix-only
  1.6.x releases may follow, but no deprecated API is removed before 2.0.0.

**Done when:** every deprecation has a tested replacement and actionable
migration example, existing 1.x callers still compile, the Windows and Linux
release matrix passes, and the 2.0 removal list is explicit.

The v1.5.0 decision remains the record of what was justified by that release's
beginner audit. This milestone advances the project using maintainability and
API coherence as additional evidence, without requiring a waiting period for
external feedback.

## 2.0.0 — A focused, predictable ChronoKit

2.0.0 follows the v1.6.0 deprecation release. It is not a release-date
commitment, but it does not require an additional time-based or user-feedback
waiting period once the v1.6.0 acceptance criteria are met.

- Remove the APIs deprecated in v1.6.0.
- Apply consistent naming, validation, and error behaviour across the library.
- Ship a complete migration guide and updated first-five-minutes tutorial.
- Verify the supported Windows and Linux environments in CI before release.

**Done when:** ChronoKit-FP presents one clear path for common date/time work,
with predictable cross-platform behaviour and an actionable upgrade path, and
all v1.6.0 deprecations have been either removed or deliberately retained with
a documented reason.

## Feedback

Please open an issue for a workflow or API that feels harder than it should.
Usability feedback and reproducible cross-platform differences help prioritise
work, but user feedback is not a prerequisite or gate for any milestone. The
project will continue making and shipping evidence-based maintainability,
correctness, and API-design decisions without waiting for external validation.
