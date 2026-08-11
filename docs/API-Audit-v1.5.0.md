# v1.5.0 beginner API and documentation audit

## Purpose and evidence limits

This audit tests whether a developer new to Free Pascal can start with a
common date/time question, find the intended operation in the repository's
user documentation, and copy a valid example without reading
`src/ChronoKit.pas`.

The evidence is deliberately limited to reproducible repository facts:

- the 92 distinct public `TChronoKit` method names in v1.4.0;
- the README, Getting Started guide, complete guide, cheat sheet, specialist
  guides, examples, and tests at the v1.4.0 tag; and
- the roadmap's primary-user and compatibility principles.

This is not telemetry or a usability study with external participants. It
does not claim that a method is widely disliked or unused. Those claims would
need issue, discussion, package-usage, or user-study evidence.

## Audit method

For each task below, start with the wording in the **Beginner question** column
and use only headings, link text, and in-page search in the user docs. A task
passes when the docs identify one preferred operation, state the important
behavior or failure rule, and contain a copyable call.

| Beginner question | v1.4.0 starting point | Finding | v1.5.0 action |
|---|---|---|---|
| How do I create a date? | Getting Started uses `EncodeDate` | Pass. ChronoKit deliberately uses Free Pascal's `TDateTime`; another constructor would duplicate the RTL. | Keep `EncodeDate` as the taught path; add no helper. |
| How do I get today or the current time? | `GetToday`, `GetNow` | Pass. Both are in the quick start and cheat sheet. | Keep names and add task-index entries. |
| How do I parse text as a date? | `FromString` | Discovery gap. “Parse” is used in prose and the roadmap, but no public method name contains `Parse`; a reader searching the API names cannot find it directly. | Add `ParseDateTime` as an alias of `FromString`; retain and document `FromString` through 1.x. |
| How do I format a date? | `GetAsString` | Discovery gap. “Format” is used throughout the docs, but no public method name contains `Format`; `GetAsString` does not advertise formatting or its `AFormat` argument. | Add `FormatDateTime` as an alias of `GetAsString`; retain and document `GetAsString` through 1.x. |
| How do I add or subtract time? | `AddYears` through `AddSeconds`, `AddSpan`, `SubtractSpan` | Pass after terminology is known, but negative values and period-versus-duration choice are not indexed by question. | Improve task navigation; add no overlapping helper. |
| How do I find a period boundary or round a value? | `StartOf*`, `EndOf*`, `FloorDate`, `CeilingDate`, `RoundDate` | Documentation defect. The cheat sheet's additional rounding example uses nonexistent `drsDay`, `drsWeek`, `drsMonth`, and `drsYear` values instead of `du*`. It also omits the hour boundary methods. | Correct the enum values and cover all boundary methods. |
| How do I compare dates or measure a difference? | `IsBefore`, `IsAfter`, `IsSame*`, `SpanBetween` | `SpanBetween` exists but is easier to find under “span” than “difference” or “between.” Its period/duration result needs explanation, so a scalar alias would hide a semantic choice. | Index “difference,” “between,” and “elapsed”; add no helper. |
| How do I calculate working days and holidays? | Business Calendars guide and overloads | Pass. The specialist guide states defaults, boundaries, errors, and recipes. | Link it from the task index; keep existing API. |
| How do I test or combine date ranges? | `TInterval` and interval methods | Partial. Basic interval calls are shown, but alignment, gap, set difference, union, and intersection are absent from the cheat sheet. | Add task recipes and a complete interval index. |
| How do I get ISO, epidemiological, quarter, semester, or decimal-date values? | Scattered cheat-sheet sections | Partial. Semester, reverse decimal conversion, and several specialist names are missing from the cheat sheet. | Group them under calendar reporting and cover every method. |
| How do I convert a timezone safely? | Getting Started and Timezone Contract | Pass. The source/target distinction, platform-native identifiers, and DST errors are explicit. | Preserve the specialist contract and add direct task links. |
| Where is the complete public API list? | Cheat Sheet and Complete Documentation | Fail. The complete guide documents only a subset, and the cheat sheet omits public operations including `StartOfHour`, `EndOfHour`, `GetSemester`, the advanced interval methods, period conversions, and timezone validation. | Add a compact exhaustive public-method index, grouped by task. |

## Additive API decision

Only two additions meet all of the milestone's tests for a new helper:

1. The task is on the first-five-minutes path.
2. The term is already used consistently in the roadmap and user docs.
3. The term is absent from the existing public method names.
4. The new method can delegate exactly to established behavior, without a
   second semantic path.

The resulting preferred names are:

```pascal
class function FormatDateTime(const AValue: TDateTime;
  const AFormat: string = ''): string; static;
class function ParseDateTime(const AValue: string;
  const AFormat: string = ''): TDateTime; static;
```

`GetAsString` and `FromString` remain supported compatibility names. No other
audited task justifies an additive helper: the existing operation is either
already direct, is an RTL operation intentionally taught by the library, or
requires a semantic choice that a convenience alias would obscure.

## Documentation acceptance matrix

v1.5.0 documentation is complete when:

- the cheat sheet starts with question and synonym vocabulary;
- all public method names appear in a task group;
- the quick-start program compiles with `ParseDateTime` and
  `FormatDateTime`;
- examples use `TDateUnit` values (`du*`) that exist in the public API;
- specialist business-calendar and timezone rules remain linked at the point
  where they become relevant; and
- compatibility names are visible but do not compete with the preferred
  first path.

## What this audit does not justify

The audit identifies two discoverability aliases and documentation defects. It
does not prove that any existing API should be removed, that migration cost is
acceptable, or that a 2.0 release would benefit current users. The separate
[2.0 decision](V2-DECISION.md) applies the roadmap's higher evidence bar to
that question.
