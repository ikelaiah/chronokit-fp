# Common recipes

These programs are the same checked source files used by the executable learning path. Copy one whole program, compile it with the repository's `src/` directory on the unit search path, then adapt its fixed input values.

## Dates and wall clocks

Create an ordinary date and a wall-clock value, then inspect the boundaries of the value's calendar quarter.

```pascal
program LearningDatesAndWallClocks;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils,
  DateUtils,
  ChronoKit;

var
  DateOnly, WallClock: TDateTime;
begin
  DateOnly := EncodeDate(2026, 8, 17);
  WallClock := EncodeDateTime(2026, 8, 17, 9, 30, 0, 0);

  WriteLn('Date: ', TChronoKit.FormatDateTime(DateOnly, 'yyyy-mm-dd'));
  WriteLn('Wall clock: ',
    TChronoKit.FormatDateTime(WallClock, 'yyyy-mm-dd hh:nn'));
  WriteLn('Quarter starts: ', TChronoKit.FormatDateTime(
    TChronoKit.StartOfQuarter(WallClock), 'yyyy-mm-dd'));
  WriteLn('Quarter ends: ', TChronoKit.FormatDateTime(
    TChronoKit.EndOfQuarter(WallClock), 'yyyy-mm-dd hh:nn:ss.zzz'));
end.
```

[Source program](../../examples/LearningPath/01-DatesAndWallClocks.lpr)

## Calendar periods and exact durations

Compare one calendar month with exactly 24 hours using fixed input on a month boundary.

```pascal
program LearningPeriodsAndDurations;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils,
  DateUtils,
  ChronoKit;

var
  OneMonth: TCalendarPeriod;
  TwentyFourHours: TDuration;
  StartValue: TDateTime;
begin
  StartValue := EncodeDateTime(2024, 1, 31, 10, 0, 0, 0);
  OneMonth := TChronoKit.CreateCalendarPeriod(0, 1);
  TwentyFourHours := TChronoKit.DurationFromParts(0, 24);

  WriteLn('Start: ', TChronoKit.FormatDateTime(StartValue, 'yyyy-mm-dd hh:nn'));
  WriteLn('One calendar month: ', TChronoKit.FormatDateTime(
    TChronoKit.AddPeriod(StartValue, OneMonth), 'yyyy-mm-dd hh:nn'));
  WriteLn('Exactly 24 hours: ', TChronoKit.FormatDateTime(
    TChronoKit.AddDuration(StartValue, TwentyFourHours), 'yyyy-mm-dd hh:nn'));
end.
```

[Source program](../../examples/LearningPath/02-PeriodsAndDurations.lpr)

## Half-open scheduling ranges

Create a workday range and verify that the start is included while the end is excluded.

```pascal
program LearningHalfOpenRanges;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils,
  DateUtils,
  ChronoKit;

var
  Workday: TDateTimeRange;
begin
  Workday := TChronoKit.CreateRange(
    EncodeDateTime(2026, 8, 17, 9, 0, 0, 0),
    EncodeDateTime(2026, 8, 17, 17, 0, 0, 0));

  WriteLn('09:00 included: ', TChronoKit.RangeContains(Workday,
    EncodeDateTime(2026, 8, 17, 9, 0, 0, 0)));
  WriteLn('17:00 included: ', TChronoKit.RangeContains(Workday,
    EncodeDateTime(2026, 8, 17, 17, 0, 0, 0)));
end.
```

[Source program](../../examples/LearningPath/03-HalfOpenRanges.lpr)

## Business-calendar deadlines

Exclude a configured holiday while counting inclusive business dates and calculating a deadline.

```pascal
program LearningBusinessCalendars;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils,
  DateUtils,
  ChronoKit;

var
  Calendar: TBusinessCalendar;
  Days: Integer;
begin
  Calendar := TChronoKit.CreateBusinessCalendar([
    EncodeDate(2026, 8, 10)
  ]);
  Days := TChronoKit.BusinessDaysBetween(
    EncodeDateTime(2026, 8, 7, 16, 0, 0, 0),
    EncodeDateTime(2026, 8, 14, 9, 0, 0, 0), Calendar);

  WriteLn('Business dates, inclusive: ', Days);
  WriteLn('Five-business-day deadline: ', TChronoKit.FormatDateTime(
    TChronoKit.AddBusinessDays(EncodeDate(2026, 8, 7), 5, Calendar),
    'yyyy-mm-dd'));
end.
```

[Source program](../../examples/LearningPath/04-BusinessCalendars.lpr)

## Named timezone conversion

Represent a New York wall clock in UTC while selecting the correct identifier for Windows or Linux.

```pascal
program LearningNamedTimeZones;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils,
  DateUtils,
  ChronoKit;

var
  NewYorkClock, UTCClock: TDateTime;
  NewYorkTimeZone: string;
begin
  {$IFDEF WINDOWS}
  NewYorkTimeZone := 'Eastern Standard Time';
  {$ELSE}
  NewYorkTimeZone := 'America/New_York';
  {$ENDIF}

  NewYorkClock := EncodeDateTime(2024, 1, 15, 9, 30, 0, 0);
  try
    UTCClock := TChronoKit.ConvertBetweenTimeZones(NewYorkClock,
      NewYorkTimeZone, 'UTC');
    WriteLn('New York clock: ', TChronoKit.FormatDateTime(
      NewYorkClock, 'yyyy-mm-dd hh:nn'));
    WriteLn('Same instant in UTC: ', TChronoKit.FormatDateTime(
      UTCClock, 'yyyy-mm-dd hh:nn'));
  except
    on E: ETimeZoneError do
      WriteLn('Choose another source clock: ', E.Message);
  end;
end.
```

[Source program](../../examples/LearningPath/05-NamedTimeZones.lpr)
