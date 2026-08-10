# ChronoKit-FP v1.3.0

The `ChronoKit` module provides comprehensive date and time manipulation utilities for Free Pascal applications. It offers a wide range of functionality for working with dates, times, timezones, and daylight saving time (DST), with full cross-platform support for Windows and Linux.

New users should begin with the [Getting Started guide](Getting-Started.md),
then use [Troubleshooting](Troubleshooting.md) if a compiler search path,
format, or platform setup needs attention. For working-week and holiday rules,
continue with [Business calendars](Business-Calendars.md).

## Features

- **Basic Date/Time Operations**: Create, manipulate, and format dates and times
- **Date Parts**: Extract and modify individual components of dates and times
- **Date Manipulations**: Add or subtract time periods from dates
- **Date Truncations**: Get the start or end of various time periods (day, month, year, etc.)
- **Date Comparisons**: Compare dates using various criteria
- **Business Calendars**: Work with configurable weekdays and holidays
- **Date Unit Operations**: Floor, ceiling, and round dates to various units
- **Timezone Contract**: Explicit wall-clock, instant, identifier, and error semantics
- **Cross-Platform Regression Matrix**: Shared UTC, offset, conversion, and DST assertions

## Business-calendar operations

Calls without a calendar keep the original Monday-to-Friday behavior:

```pascal
DueDate := TChronoKit.AddBusinessDays(StartDate, 5);
```

Use `CreateBusinessCalendar` to exclude holidays or select a different working
week. Holiday time portions are ignored.

```pascal
type
  TBusinessWeekday = (
    bwdSunday, bwdMonday, bwdTuesday, bwdWednesday,
    bwdThursday, bwdFriday, bwdSaturday
  );
  TBusinessWeek = set of TBusinessWeekday;

Calendar := TChronoKit.CreateBusinessCalendar([
  EncodeDate(2026, 1, 1), EncodeDate(2026, 12, 25)
]);

Calendar := TChronoKit.CreateBusinessCalendar(
  [bwdSunday, bwdMonday, bwdTuesday, bwdWednesday, bwdThursday],
  [EncodeDate(2026, 1, 1)]
);
```

The calendar-aware overloads are:

```pascal
class function IsBusinessDay(const AValue: TDateTime;
  const ACalendar: TBusinessCalendar): Boolean; static;
class function NextBusinessDay(const AValue: TDateTime;
  const ACalendar: TBusinessCalendar): TDateTime; static;
class function PreviousBusinessDay(const AValue: TDateTime;
  const ACalendar: TBusinessCalendar): TDateTime; static;
class function AddBusinessDays(const AValue: TDateTime; const ADays: Integer;
  const ACalendar: TBusinessCalendar): TDateTime; static;
```

`NextBusinessDay` and `PreviousBusinessDay` are strict. `AddBusinessDays` does
not count the starting date, and zero returns it unchanged. Invalid calendars
with no working days raise `EBusinessCalendarError`. See
[Business calendars](Business-Calendars.md) for complete deadline,
reporting-period, and date-range recipes.

## Timezone Operations

ChronoKit's timezone functions operate on unzoned `TDateTime` wall clocks.
`GetTimeZone` reads the system-zone offset for the supplied local value;
`WithTimeZone` treats its input as system-local and preserves the instant while
producing a target-zone wall clock; `ForceTimeZone` interprets the input clock
in the named zone and returns the equivalent system-local wall clock.

`UTC` is the only portable identifier. Linux uses IANA identifiers and Windows
uses Windows identifiers; call `GetTimeZoneNames` for exact values accepted on
the running platform. Offsets are minutes east of UTC, so
`local = UTC + offset`.

A plain `TDateTime` cannot select between repeated clock values. The contract
requires `ETimeZoneError` for ambiguous and nonexistent local inputs instead of
silently guessing. The [timezone contract](Timezone-Contract.md) defines the
identifier mappings, operation table, failure rules, regression matrix, and
the named-zone conformance work gated for v1.4.0.

```pascal
var
  LocalValue, UTCValue: TDateTime;
  TZInfo: TTimeZoneInfo;
begin
  LocalValue := EncodeDateTime(2026, 8, 11, 9, 30, 0, 0);
  TZInfo := TChronoKit.GetTimeZone(LocalValue);
  UTCValue := TChronoKit.WithTimeZone(LocalValue, 'UTC');

  WriteLn(TZInfo.Name, ' offset: ', TZInfo.Offset, ' minutes');
  WriteLn('UTC: ', TChronoKit.GetAsString(UTCValue, 'yyyy-mm-dd hh:nn:ss'));
end;
```

## Examples

### Basic Date/Time Operations

```pascal
var
  CurrentTime: TDateTime;
  NextWorkday: TDateTime;
begin
  // Get current time
  CurrentTime := TChronoKit.GetNow;
  
  // Get next business day
  NextWorkday := TChronoKit.NextBusinessDay(CurrentTime);
  
  // Format for display
  WriteLn(TChronoKit.GetAsString(NextWorkday, 'yyyy-mm-dd'));
end;
```

### Timezone and DST Operations

```pascal
var
  CurrentTime: TDateTime;
  TZInfo: TTimeZoneInfo;
begin
  // Get current time
  CurrentTime := TChronoKit.GetNow;
  
  // Get timezone information
  TZInfo := TChronoKit.GetTimeZone(CurrentTime);
  WriteLn('Timezone: ', TZInfo.Name);
  WriteLn('Offset: ', TZInfo.Offset, ' minutes');
  WriteLn('DST: ', BoolToStr(TZInfo.IsDST, True));
  
  // List available timezones
  TZNames := TChronoKit.GetTimeZoneNames;
  for I := Low(TZNames) to High(TZNames) do
    WriteLn('Available timezone: ', TZNames[I]);
end;
```

## API Reference

### TTimeZoneInfo

```pascal
TTimeZoneInfo = record
  Name: string;           // Timezone name (e.g., 'UTC', 'America/New_York')
  Offset: Integer;        // Minutes east of UTC: local = UTC + Offset
  IsDST: Boolean;        // Whether daylight savings is in effect
end;
```

### TDSTRule

```pascal
TDSTRule = record
  Region: string;           // Region identifier (e.g., 'US', 'EU', 'AU')
  StartMonth: Integer;      // Month when DST starts (1-12)
  StartWeek: Integer;       // Week of the month (1-5, where 5 means last)
  StartDayOfWeek: Integer;  // Day of week (1-7, where 1=Sunday)
  StartHour: Integer;       // Hour when DST starts (0-23)
  EndMonth: Integer;        // Month when DST ends (1-12)
  EndWeek: Integer;         // Week of the month (1-5, where 5 means last)
  EndDayOfWeek: Integer;    // Day of week (1-7, where 1=Sunday)
  EndHour: Integer;         // Hour when DST ends (0-23)
  Offset: Integer;          // DST offset in minutes (typically 60)
end;
```

### Timezone functions

```pascal
class function GetTimeZone(const AValue: TDateTime): TTimeZoneInfo; static;
class function GetTimeZoneNames: TStringArray; static;
class function GetSystemTimeZone: string; static;
class function WithTimeZone(const AValue: TDateTime;
  const ATimeZone: string): TDateTime; static;
class function ForceTimeZone(const AValue: TDateTime;
  const ATimeZone: string): TDateTime; static;
```

- `GetTimeZone` interprets `AValue` as a system-local wall clock and returns
  the system-zone information for that value.
- `GetTimeZoneNames` returns exact platform-native inputs and always includes
  `UTC`.
- `GetSystemTimeZone` returns the current platform-native system-zone name.
- `WithTimeZone` preserves an instant while changing its wall-clock
  representation.
- `ForceTimeZone` assigns the named source-zone meaning to the input clock and
  returns the equivalent system-local clock.

All failures use `ETimeZoneError`. See the
[timezone contract](Timezone-Contract.md) before using non-UTC identifiers or
values at a DST transition.

## Cross-platform considerations

- Windows uses Windows timezone identifiers, such as
  `Eastern Standard Time`.
- Linux uses IANA identifiers, such as `America/New_York`, and requires an
  installed timezone database (commonly `tzdata`).
- Windows and IANA names are mappings, not cross-platform aliases.
- The pull-request workflow sets equivalent New York fixtures and runs the
  same assertions on Windows and Linux.
