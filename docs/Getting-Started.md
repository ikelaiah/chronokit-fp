# Getting Started with ChronoKit-FP

This guide gets a Free Pascal or Lazarus project to its first useful date
calculation. It deliberately starts with calendar dates; business calendars,
timezones, and DST are separate topics.

## Install ChronoKit

Choose one path:

- **Lazarus:** Open `packages/lazarus/chronokit_fp.lpk`, select `Compile`,
  then select `Use` → `Add to Project`.
- **Source-based project:** Compile with the repository's `src/` directory in
  the unit search path. For example, on Windows:

  ```powershell
  fpc "-FuC:\path\to\chronokit-fp\src" MyProgram.lpr
  ```

Both paths make the `ChronoKit` unit available to the project.

## Your first program

Create `FirstChronoKitProgram.lpr` with the following content:

```pascal
program FirstChronoKitProgram;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils,
  ChronoKit;

var
  CreatedDate, ParsedDate, NextWeek: TDateTime;
begin
  CreatedDate := EncodeDate(2026, 8, 10);
  WriteLn('Created: ', TChronoKit.FormatDateTime(CreatedDate, 'yyyy-mm-dd'));

  ParsedDate := TChronoKit.ParseDateTime('2026-08-10', 'yyyy-mm-dd');
  NextWeek := TChronoKit.AddDays(ParsedDate, 7);
  WriteLn('One week later: ',
    TChronoKit.FormatDateTime(NextWeek, 'yyyy-mm-dd'));
end.
```

Compile it using the source-based command above, substituting the program file
name. Its output is:

```text
Created: 2026-08-10
One week later: 2026-08-17
```

## Choose the right starting point

| Need | Start with |
|---|---|
| A calendar date with no time | `EncodeDate` or `TChronoKit.GetToday` |
| The computer's current local date and time | `TChronoKit.GetNow` |
| A date/time entered as text | `TChronoKit.ParseDateTime` with an explicit format |
| Text formatted for display or output | `TChronoKit.FormatDateTime` with an explicit format |
| A date a fixed number of days away | `TChronoKit.AddDays` |
| Workdays in an inclusive date period | `TChronoKit.BusinessDaysBetween` |
| The same instant represented in a named timezone | `TChronoKit.SystemLocalToTimeZone` |
| A wall clock that should be interpreted in a named timezone | `TChronoKit.TimeZoneToSystemLocal` |
| A named source clock shown in another named timezone | `TChronoKit.ConvertBetweenTimeZones` |

ChronoKit uses Free Pascal's `TDateTime` type. A **date** is conventionally a
`TDateTime` at midnight. A **local date/time** is a wall-clock value such as
the value returned by `GetNow`. A **timezone conversion** is an explicit
operation—use `SystemLocalToTimeZone` only when the target timezone matters. The result
is still a `TDateTime`; keep the intended timezone alongside the value in your
application when it must be known later. `UTC` is the only portable timezone
identifier. Before converting named zones, read the
[timezone contract](Timezone-Contract.md) for Windows/Linux identifier
mappings and DST-boundary errors.

## Convert a timezone value

`SystemLocalToTimeZone` starts with a system-local wall clock and returns the same
instant displayed in the target zone. This portable example converts the
current local time to UTC:

```pascal
var
  LocalValue, UTCValue: TDateTime;
begin
  LocalValue := TChronoKit.GetNow;
  UTCValue := TChronoKit.SystemLocalToTimeZone(LocalValue, 'UTC');
  WriteLn(TChronoKit.FormatDateTime(UTCValue, 'yyyy-mm-dd hh:nn:ss'));
end;
```

`TimeZoneToSystemLocal` starts with clock fields that belong to the named source zone.
It returns the equivalent clock in the computer's system zone. Names are
platform-native: use `America/New_York` on Linux or `Eastern Standard Time` on
Windows for New York.

When both zones are named, use `ConvertBetweenTimeZones` instead of routing
through the computer's system timezone. The source wall clock is resolved once
and the result is the target-zone wall clock for the same instant. See the
[named-timezone learning program](../examples/LearningPath/05-NamedTimeZones.lpr).

Both operations raise `ETimeZoneError` when a source wall clock falls in a DST
gap or overlap. Catch the exception rather than letting the library select an
occurrence silently:

```pascal
try
  SystemValue := TChronoKit.TimeZoneToSystemLocal(InputValue, SourceTimeZone);
except
  on E: ETimeZoneError do
    WriteLn('Choose another local time: ', E.Message);
end;
```

## Formats

Always pass the format when parsing user or file input. The common tokens are:

| Token | Meaning |
|---|---|
| `yyyy` | Four-digit year |
| `mm` | Month |
| `dd` | Day of month |
| `hh` | Hour |
| `nn` | Minute |
| `ss` | Second |

For example, parse `2026-08-10 09:30` with
`'yyyy-mm-dd hh:nn'`. Notice that minutes use `nn`, not `mm`.

The original 1.x formatting and parsing names remain source compatible but are
deprecated. New code should use `FormatDateTime` and `ParseDateTime`, which use
the same behavior and errors. See the [v1.6 migration guide](MIGRATION-v1.6-to-v2.0.md).

## Next steps

- [Business calendars](Business-Calendars.md) for holidays, alternative working
  weeks, deadlines, reporting periods, and date ranges.
- [Executable learning path](Learning-Path.md) for the preferred concepts in order.
- [Decision guides](Decision-Guides.md) for choosing types, operations, and errors.
- [Troubleshooting](Troubleshooting.md) for search-path, format, and platform
  issues.
- [Timezone contract](Timezone-Contract.md) for identifier, conversion, and
  DST rules.
- [Searchable API cheat sheet](Cheat-Sheet.md) to find an operation by question
  or keyword.
- [Generated API reference](API-Reference.md) for declarations and contracts.
- [Task guide](ChronoKit-FP.md) for the wider API.
