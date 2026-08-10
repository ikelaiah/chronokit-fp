# Roadmap

ChronoKit-FP's priority through 2.0.0 is a library that is easy to discover,
install, and use correctly. This roadmap describes the intended direction, not
a release-date commitment. Features may move between releases as feedback and
cross-platform testing reveal what needs the most attention.

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

## 2.0.0 — Conditional: a focused, predictable ChronoKit

2.0.0 is not a release-date commitment. It will be cut only when 1.x contains
documented deprecations with proven replacements and a migration is genuinely
valuable to users.

- Remove deprecated APIs only where a documented replacement has existed in
  at least one 1.x release.
- Apply consistent naming, validation, and error behaviour across the library.
- Ship a complete migration guide and updated first-five-minutes tutorial.
- Verify the supported Windows and Linux environments in CI before release.

**Done when:** ChronoKit-FP presents one clear path for common date/time work,
with predictable cross-platform behaviour and an actionable upgrade path. If
these conditions are not met, the project will continue releasing compatible
1.x versions instead.

## Feedback

Please open an issue for a workflow or API that feels harder than it should.
Usability feedback and reproducible cross-platform differences will be used to
prioritise this roadmap.
