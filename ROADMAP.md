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

## 2.0 quality bar

The remaining 1.x milestones prepare one stable preferred surface for 2.0.
They do not add another round of aliases, deprecations, or competing concepts.
Each milestone must improve at least one of these outcomes without regressing
the others:

- **Easy to use:** a common task has one preferred operation, important
  semantic choices are visible in names and types, and invalid or ambiguous
  input has a predictable result or error.
- **Easy to learn:** documentation introduces concepts progressively, every
  taught path has a small executable example, and task-oriented guidance leads
  to a complete reference without requiring implementation reading.
- **Easy to maintain:** domain boundaries are explicit, tests follow those
  boundaries, the public surface is mechanically tracked, and release checks
  are reproducible rather than dependent on undocumented manual steps.

The preferred v1.6 API is the compatibility baseline through v1.9. Version
1.7 may make one bounded additive pass for the three workflows named in that
milestone, after their contracts are reviewed against executable examples.
The resulting preferred v1.7 surface is then frozen through v1.9. Other gaps
found by later usability work are recorded as post-2.0 design input; they do
not justify expanding the 1.x surface by default.

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
- Treat v1.6.0 as the final planned 1.x API-consolidation and deprecation
  release. Later 1.x milestones improve learning, internal maintainability,
  and release confidence without removing deprecated APIs. The only planned
  additive exception is the small, contract-first v1.7 workflow set below;
  it does not introduce aliases or new deprecations.

**Done when:** every deprecation has a tested replacement and actionable
migration example, existing 1.x callers still compile, the Windows and Linux
release matrix passes, and the 2.0 removal list is explicit.

The v1.5.0 decision remains the record of what was justified by that release's
beginner audit. This milestone advances the project using maintainability and
API coherence as additional evidence, without requiring a waiting period for
external feedback.

## 1.7.0 — Executable learning path and focused API gaps

**Status:** Released 2026-08-14

Make the preferred v1.6 API understandable as a small set of concepts rather
than a long list of methods, close three concrete workflow gaps, and complete
the internal refactor and API freeze. Its additive runtime scope is limited to
the operations explicitly listed below; the maintainability work changes
structure without expanding the user-facing model.

- Publish one progressive path covering `TDateTime` dates and system-local
  wall clocks, calendar periods versus exact durations, half-open ranges,
  business calendars, and named timezones with DST.
- Back every taught workflow with a shipped program or fixture that compiles
  on Windows and Linux. Documentation snippets must come from, or be checked
  against, those executable sources so examples cannot drift silently.
- Add concise decision guides for choosing a value type, choosing an operation,
  and understanding validation and timezone errors.
- Repeat the task-oriented beginner audit using only preferred v1.6 APIs and
  record every point where the user must inspect source code or legacy docs.
  Resolve documentation gaps here. Use executable tasks to review the
  contracts below before implementation, and record any other genuine API gap
  as post-2.0 design input rather than expanding this milestone.
- Add a direct named-source-to-named-target timezone conversion, provisionally
  `ConvertBetweenTimeZones(Value, SourceTimeZone, TargetTimeZone)`. Its
  contract must state that the input is a wall clock in the source zone, the
  output is the target-zone wall clock for the same instant, the returned
  `TDateTime` retains no zone identity, and ambiguous or nonexistent source
  clocks raise the established timezone error.
- Add `StartOfQuarter(Value)` and `EndOfQuarter(Value)` boundary operations,
  while retaining `StartOfQuarter(Year, Quarter)`. Define them consistently
  with the existing start/end boundary conventions and cover leap years,
  year transitions, fractional input times, and the fourth quarter.
- Add `BusinessDaysBetween(StartDate, EndDate)` and its custom-calendar
  overload. Specify signed direction, endpoint inclusion, same-day behaviour,
  preservation or rejection of time components, holidays, alternative work
  weeks, and invalid calendars before implementation.
- Add focused Windows and Linux tests, declaration comments, searchable
  reference entries, and copyable examples for each new operation. Do not add
  convenience aliases, unrelated RTL wrappers, recurrence APIs, or new
  instant/zoned value types in this release.
- Generate a searchable API reference from public declaration comments and
  fail its documentation check when a preferred declaration lacks a useful
  contract, error rule, or example link.
- Verify both documented installation paths from clean consumer projects, not
  from a checkout that already contains compiled units.
- Split the 178-test monolith into domain suites without changing assertions,
  then move non-trivial implementations into focused internal units behind the
  unchanged `ChronoKit` facade.
- Freeze complete Windows and Linux API manifests and check them with the
  generated reference and documentation workflow.
- Isolate incompatible deprecated algorithms from preferred code paths,
  centralise timezone facade conversion orchestration, and publish contributor
  placement rules plus the internal architecture decision.

**Done when:** a developer starting from the README can choose the correct
value type, complete the common date, duration, range, business-calendar, and
timezone workflows without using a deprecated name; direct named-zone
conversion, quarter boundaries, and business-day counting each have one
reviewed and tested path; and every taught example runs on either supported
platform. The preferred public surface is then closed to further additions
through v1.9. A contributor can change one domain without navigating unrelated
implementations, and both platform manifests prove that the refactor preserves
the complete v1.7 facade.

The completed release includes the executable [learning path](docs/Learning-Path.md),
[decision guides](docs/Decision-Guides.md), [beginner audit](docs/API-Audit-v1.7.0.md),
and generated [API reference](docs/API-Reference.md). `ConvertBetweenTimeZones`,
value `StartOfQuarter`/`EndOfQuarter`, and `BusinessDaysBetween` are the
milestone's only runtime additions. Their contracts are recorded in
[API additions v1.7.0](docs/API-Additions-v1.7.0.md). The accepted
[domain-internals ADR](docs/decisions/0001-domain-internals.md), nine domain
test suites, and checked files in `api/` record the completed structural work.

## 1.8.0 — Maintainable internals

**Status:** Folded into 1.7.0 on 2026-08-14

This work shipped early as part of v1.7.0 rather than as a separate release.
It made changes safer by separating domain logic while preserving the complete
v1.7 public and behavioural contract. It changed structure, not the
user-facing model.

- Publish a short internal architecture design that defines ownership and
  dependency direction for calendar arithmetic, duration and range algebra,
  business calendars, timezone conversion, parsing and formatting, and the
  public `ChronoKit` facade.
- Split the monolithic test unit into domain suites before moving production
  logic, retaining the same assertions and cross-platform fixtures.
- Move implementations into domain-focused internal units behind the existing
  public `ChronoKit` unit. Avoid circular dependencies and keep deprecated
  compatibility paths out of preferred implementations.
- Centralise only genuinely shared validation, checked arithmetic, and error
  construction; do not replace domain boundaries with a new catch-all helper
  unit.
- Capture the preferred and deprecated public declarations in a versioned API
  manifest and compare it in CI so an internal refactor cannot silently change
  a signature, directive, or visibility.
- Add contributor guidance showing where a domain change, regression test,
  public contract comment, and executable example belong.

**Done when:** a change to one date/time domain can be implemented and tested
without editing unrelated domain implementations, the public API manifest is
unchanged, and the full Windows/Linux behaviour matrix passes before and after
the refactor.

Completed evidence is recorded in the
[domain-internals ADR](docs/decisions/0001-domain-internals.md), the contributor
architecture table in [CONTRIBUTING.md](CONTRIBUTING.md), the nine domain test
suites under `tests/`, and the checked Windows/Linux manifests under `api/`.

## 1.9.0 — Reproducible release and 2.0 freeze

**Status:** Planned

Turn every release claim into an automated or explicitly recorded check, then
freeze the contract that 2.0 will retain or remove. No new public declaration
or deprecation is planned for this milestone.

- Run unit tests, named-timezone fixtures, example compilation, the legacy
  compatibility fixture, Lazarus package compilation, documentation links,
  generated-reference coverage, API-manifest comparison, and version-metadata
  consistency from the release workflow.
- Keep compiler outputs outside source and example directories and fail when a
  release build leaves untracked binaries or units in the checkout.
- Maintain two consumer fixtures: a complete v1.6 legacy client that compiles
  with expected deprecation diagnostics and a preferred client that compiles
  without deprecation warnings and is intended to survive 2.0 unchanged.
- Audit the migration guide against the compiler-visible deprecation inventory
  and the preferred API manifest so every removal has exactly one actionable
  destination or an explicit domain-specific decision.
- Publish the 2.0 implementation specification with the exact removal list,
  retained API snapshot, validation and error conventions, migration tests,
  and rollback criteria before changing the public surface.
- Perform a clean-checkout release rehearsal on Windows and Linux, including
  package metadata, changelog, release notes, and generated artifacts.

**Done when:** the complete release gate is reproducible, the preferred client
is warning-free, the legacy client proves the promised 1.x compatibility, and
2.0 implementation is a reviewed mechanical change rather than a new design
exercise.

## 2.0.0 — A focused, predictable ChronoKit

2.0.0 follows the v1.9 contract freeze. It is a deliberately narrow breaking
release: remove the superseded surface, retain the proven preferred model, and
ship the learning and maintenance system established in v1.7 through v1.9.
It does not require an additional time-based or user-feedback waiting period
once those acceptance criteria are met.

- Remove the APIs deprecated in v1.6.0.
- Remove `duSeason` and every compatibility-only implementation that no longer
  has a public caller.
- Preserve the preferred API manifest frozen in v1.9; do not combine removals
  with unrelated feature additions or another naming redesign.
- Apply the reviewed validation and error conventions consistently across the
  retained library.
- Ship the audited migration guide, progressive learning path, generated API
  reference, and updated first-five-minutes tutorial.
- Replace legacy compatibility checks with migration and preferred-client
  checks that prove the documented 2.0 surface.
- Verify the supported Windows and Linux environments in CI before release.

Deliver 2.0 through pre-releases: alpha proves the exact removals and retained
surface, beta proves clean installation and real migration fixtures, and the
release candidate freezes code except for release-blocking correctness,
portability, or documentation defects.

**Done when:** ChronoKit-FP presents one clear path for common date/time work,
with predictable cross-platform behaviour and an actionable upgrade path;
every taught example is executable; domain changes are isolated and covered by
focused tests; the full release gate is reproducible; and all v1.6.0
deprecations have been either removed or deliberately retained with a
documented reason.

## Feedback

Please open an issue for a workflow or API that feels harder than it should.
Usability feedback and reproducible cross-platform differences help prioritise
work, but user feedback is not a prerequisite or gate for any milestone. The
project will continue making and shipping evidence-based maintainability,
correctness, and API-design decisions without waiting for external validation.
