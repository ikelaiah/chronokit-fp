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
  WriteLn('Created: ', TChronoKit.GetAsString(CreatedDate, 'yyyy-mm-dd'));

  ParsedDate := TChronoKit.FromString('2026-08-10', 'yyyy-mm-dd');
  NextWeek := TChronoKit.AddDays(ParsedDate, 7);
  WriteLn('One week later: ', TChronoKit.GetAsString(NextWeek, 'yyyy-mm-dd'));
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
| A date/time entered as text | `TChronoKit.FromString` with an explicit format |
| A date a fixed number of days away | `TChronoKit.AddDays` |
| The same instant represented in a named timezone | `TChronoKit.WithTimeZone` |

ChronoKit uses Free Pascal's `TDateTime` type. A **date** is conventionally a
`TDateTime` at midnight. A **local date/time** is a wall-clock value such as
the value returned by `GetNow`. A **timezone conversion** is an explicit
operation—use `WithTimeZone` only when the target timezone matters. The result
is still a `TDateTime`; keep the intended timezone alongside the value in your
application when it must be known later.

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

## Next steps

- [Troubleshooting](Troubleshooting.md) for search-path, format, and platform
  issues.
- [API Cheat Sheet](Cheat-Sheet.md) for a compact function reference.
- [Complete documentation](ChronoKit-FP.md) for the wider API.
