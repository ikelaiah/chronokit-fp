unit ChronoKit.PeriodsDurations.Tests;

{$mode objfpc}{$H+}{$J-}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  Classes, SysUtils, DateUtils, fpcunit, testregistry,
  ChronoKit;

type
  TPeriodsDurationsTests = class(TTestCase)
  published
    procedure Test158_CalendarPeriodArithmetic;
    procedure Test159_ExactDurationArithmetic;
    procedure Test160_DurationConstructionRejectsOverflow;
  end;

implementation

procedure TPeriodsDurationsTests.Test158_CalendarPeriodArithmetic;
var
  Period, Normalized: TCalendarPeriod;
  StartValue: TDateTime;
  Raised: Boolean;
begin
  Period := TChronoKit.CreateCalendarPeriod(1, 13, 0, 25, 61, 61, 1001);
  Normalized := TChronoKit.NormalizeCalendarPeriod(Period);
  AssertEquals('Months must carry into years', 2, Normalized.Years);
  AssertEquals('Normalized month remainder', 1, Normalized.Months);
  AssertEquals('Hours must carry into days', 1, Normalized.Days);
  AssertEquals('Normalized hour remainder', 2, Normalized.Hours);
  AssertEquals('Normalized minute remainder', 2, Normalized.Minutes);
  AssertEquals('Normalized second remainder', 2, Normalized.Seconds);
  AssertEquals('Normalized millisecond remainder',
    1, Normalized.Milliseconds);

  StartValue := EncodeDateTime(2024, 1, 31, 10, 0, 0, 0);
  Period := TChronoKit.CreateCalendarPeriod(0, 1, 1, 2);
  AssertEquals('Periods apply months before days and time',
    EncodeDateTime(2024, 3, 1, 12, 0, 0, 0),
    TChronoKit.AddPeriod(StartValue, Period), OneMillisecond);
  AssertEquals('SubtractPeriod uses calendar month-end rules',
    EncodeDateTime(2024, 2, 29, 10, 0, 0, 0),
    TChronoKit.SubtractPeriod(
      EncodeDateTime(2024, 3, 31, 10, 0, 0, 0),
      TChronoKit.CreateCalendarPeriod(0, 1)), OneMillisecond);

  Raised := False;
  try
    TChronoKit.SubtractPeriod(StartValue,
      TChronoKit.CreateCalendarPeriod(Low(Integer)));
  except
    on E: ERangeError do
      Raised := True;
  end;
  AssertTrue('SubtractPeriod must reject an unrepresentable negation', Raised);
end;

procedure TPeriodsDurationsTests.Test159_ExactDurationArithmetic;
var
  Duration, Difference: TDuration;
  StartValue, EndValue: TDateTime;
begin
  Duration := TChronoKit.DurationFromParts(1, 2, 3, 4, 5);
  AssertEquals('Duration parts must produce exact elapsed milliseconds',
    Int64(93784005), Duration.Milliseconds);
  AssertEquals('Negative durations must remain exact',
    Int64(-3600000),
    TChronoKit.DurationFromParts(0, -1).Milliseconds);

  StartValue := EncodeDateTime(2024, 1, 1, 12, 0, 0, 0);
  Duration := TChronoKit.DurationFromSeconds(1);
  Duration.Milliseconds := Duration.Milliseconds + 500;
  EndValue := TChronoKit.AddDuration(StartValue, Duration);
  AssertEquals('AddDuration must add exact milliseconds',
    IncMilliSecond(StartValue, 1500), EndValue, OneMillisecond);
  AssertEquals('SubtractDuration must reverse exact elapsed addition',
    StartValue, TChronoKit.SubtractDuration(EndValue, Duration),
    OneMillisecond);

  Difference := TChronoKit.DurationBetween(StartValue, EndValue);
  AssertEquals('DurationBetween must round once to milliseconds',
    Int64(1500), Difference.Milliseconds);
  Difference := TChronoKit.DurationBetween(EndValue, StartValue);
  AssertEquals('DurationBetween must preserve direction',
    Int64(-1500), Difference.Milliseconds);
end;

procedure TPeriodsDurationsTests.Test160_DurationConstructionRejectsOverflow;
var
  Raised: Boolean;
begin
  Raised := False;
  try
    TChronoKit.DurationFromParts(High(Int64));
  except
    on E: ERangeError do
      Raised := True;
  end;
  AssertTrue('Day conversion overflow must raise ERangeError', Raised);

  Raised := False;
  try
    TChronoKit.DurationFromSeconds(High(Int64));
  except
    on E: ERangeError do
      Raised := True;
  end;
  AssertTrue('Second conversion overflow must raise ERangeError', Raised);
end;

initialization
  RegisterTest(TPeriodsDurationsTests);

end.
