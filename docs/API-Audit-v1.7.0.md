# Beginner API audit: v1.7.0

**Status:** Completed 2026-08-13

## Method

The audit follows a developer who starts from the README and uses only the
preferred v1.6 API and this v1.7 learning path. Each task had to lead to a
copyable program or a documented reference without reading `src/ChronoKit.pas`
or the v1.6 migration guide.

| Task | Evidence reviewed | Result |
|---|---|---|
| Create, parse, format, and add dates | README, Getting Started, learning step 1 | Clear preferred path; no source reading required |
| Choose a month versus exact elapsed time | Learning step 2 and decision guide | Clear after documenting the type choice |
| Represent bookings and touching windows | Learning step 3 and cheat sheet | Clear after linking the half-open example |
| Include a holiday and count workdays | Learning step 4 and business-calendar guide | Gap resolved by `BusinessDaysBetween` and its contract |
| Convert a named source clock directly to another named zone | Learning step 5 and timezone contract | Gap resolved by `ConvertBetweenTimeZones` and direct source-to-UTC resolution |
| Find the quarter containing a value | Learning step 1 and API reference | Gap resolved by value `StartOfQuarter` and `EndOfQuarter` |
| Recover from bad input or a DST discontinuity | Decision guide and timezone contract | Clear error class and recovery guidance |

## Findings resolved in v1.7.0

The audit identified only the three workflow gaps accepted in
[the v1.7 contract](API-Additions-v1.7.0.md): value quarter boundaries,
inclusive business-day counting, and direct named-source-to-named-target
conversion. Each has focused tests, declaration comments, a generated
reference entry, and an executable program.

## Post-2.0 design input

No additional API gap was added to v1.7.0. A future zoned/instant value model
could let callers select one occurrence in a DST overlap, but that would be a
new value-type design and is intentionally post-2.0 input. The v1.7 preferred
surface is closed through v1.9.
