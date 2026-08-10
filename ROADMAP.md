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

Make it straightforward for a new Free Pascal or Lazarus user to get started.

- Publish one verified installation path for Lazarus and one for source-based
  projects.
- Refresh the quick start around the most common operations: creating,
  formatting, parsing, and adding dates.
- Add a concise troubleshooting guide for compiler search paths, formats, and
  platform requirements.
- Audit examples so they compile and use one consistent style.

**Done when:** a developer new to Free Pascal can follow a single short guide,
run an example, and understand the difference between a date, a local
date/time, and a timezone conversion without inferring missing setup steps.

## 1.2.0 — Everyday calendar work

Cover common real-world tasks without requiring callers to assemble several
low-level helpers.

- Make business-day calculations configurable for holidays and alternative
  working weeks.
- Add focused recipes for reporting periods, deadlines, and date ranges.
- Define and test boundary behaviour for leap years, month ends, and week
  starts.
- Improve validation messages for invalid date input.

**Done when:** common calendar rules are expressed directly and their edge
cases are documented.

## 1.3.0 — Time zones users can trust

Make timezone behaviour explicit and consistent across supported platforms.

- Standardise supported timezone identifiers and document platform mappings.
- Make ambiguous and nonexistent DST local times visible to callers instead of
  silently guessing.
- Expand test coverage for UTC offsets, DST transitions, and conversions on
  Windows and Linux.
- Document when an operation preserves an instant versus preserves local clock
  time.

**Done when:** a developer can choose the right timezone operation from the
documentation and predict its result at a DST boundary.

## 1.4.0 — Discoverable, dependable API

Reduce friction in the public surface while keeping 1.x code working.

- Group the API documentation by task and add a searchable cheat sheet.
- Introduce clearer names or convenience helpers where existing names are hard
  to discover; retain deprecated compatibility wrappers through 1.x.
- Add examples and tests for every public API addition.
- Keep pull requests gated by the Windows and Linux test suite.

**Done when:** users can find a documented, tested answer to common date/time
questions without reading the implementation.

## 1.5.0 — Prepare the 2.0 upgrade

Turn the lessons from 1.x into a low-risk migration path.

- Publish the proposed 2.0 API changes and migration guide.
- Mark superseded APIs as deprecated with replacements and examples.
- Provide an upgrade checklist and compatibility notes for Lazarus and Free
  Pascal versions.
- Resolve outstanding cross-platform consistency issues before the major
  release.

**Done when:** an existing 1.x project can assess and plan its upgrade without
guesswork.

## 2.0.0 — A focused, predictable ChronoKit

Deliver the simplified public API announced in 1.5.0.

- Remove deprecated APIs only where a documented replacement has existed in
  1.x.
- Apply consistent naming, validation, and error behaviour across the library.
- Ship a complete migration guide and updated first-five-minutes tutorial.
- Verify the supported Windows and Linux environments in CI before release.

**Done when:** ChronoKit-FP presents one clear path for common date/time work,
with predictable cross-platform behaviour and an actionable upgrade path.

## Feedback

Please open an issue for a workflow or API that feels harder than it should.
Usability feedback and reproducible cross-platform differences will be used to
prioritise this roadmap.
