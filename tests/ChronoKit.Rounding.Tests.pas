unit ChronoKit.Rounding.Tests;

{$mode objfpc}{$H+}{$J-}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  Classes, SysUtils, DateUtils, fpcunit, testregistry,
  ChronoKit;

type
  TRoundingTests = class(TTestCase)
  published
    procedure Test46_FloorDateSecond;
    procedure Test47_FloorDateMinute;
    procedure Test48_FloorDateHour;
    procedure Test49_FloorDateDay;
    procedure Test50_FloorDateWeek;
    procedure Test51_FloorDateMonth;
    procedure Test52_FloorDateBiMonth;
    procedure Test53_FloorDateQuarter;
    procedure Test54_FloorDateHalfYear;
    procedure Test55_FloorDateYear;
    procedure Test56_CeilingDateSecond;
    procedure Test57_CeilingDateMinute;
    procedure Test58_CeilingDateHour;
    procedure Test59_CeilingDateDay;
    procedure Test60_CeilingDateWeek;
    procedure Test61_CeilingDateMonth;
    procedure Test62_CeilingDateBiMonth;
    procedure Test63_CeilingDateQuarter;
    procedure Test64_CeilingDateHalfYear;
    procedure Test65_CeilingDateYear;
    procedure Test66_RoundDateSecond;
    procedure Test67_RoundDateMinute;
    procedure Test68_RoundDateHour;
    procedure Test69_RoundDateDay;
    procedure Test70_RoundDateWeek;
    procedure Test71_RoundDateMonth;
    procedure Test72_RoundDateBiMonth;
    procedure Test73_RoundDateQuarter;
    procedure Test74_RoundDateHalfYear;
    procedure Test75_RoundDateYear;
    procedure Test149_CeilingDateRollsAcrossTimeBoundaries;
    procedure Test150_EndBoundariesContainTheirStartingBoundary;
  end;

implementation

procedure TRoundingTests.Test46_FloorDateSecond;
var
  TestDate: TDateTime;
begin
  WriteLn('Test46_FloorDateSecond:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to second should clear milliseconds',
    EncodeDateTime(2024, 3, 15, 14, 30, 45, 0),
    TChronoKit.FloorDate(TestDate, duSecond));
  WriteLn('Test46_FloorDateSecond:Finished');
end;

procedure TRoundingTests.Test47_FloorDateMinute;
var
  TestDate: TDateTime;
begin
  WriteLn('Test47_FloorDateMinute:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to minute should clear seconds and milliseconds',
    EncodeDateTime(2024, 3, 15, 14, 30, 0, 0),
    TChronoKit.FloorDate(TestDate, duMinute));
  WriteLn('Test47_FloorDateMinute:Finished');
end;

procedure TRoundingTests.Test48_FloorDateHour;
var
  TestDate: TDateTime;
begin
  WriteLn('Test48_FloorDateHour:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to hour should clear minutes, seconds and milliseconds',
    EncodeDateTime(2024, 3, 15, 14, 0, 0, 0),
    TChronoKit.FloorDate(TestDate, duHour));
  WriteLn('Test48_FloorDateHour:Finished');
end;

procedure TRoundingTests.Test49_FloorDateDay;
var
  TestDate: TDateTime;
begin
  WriteLn('Test49_FloorDateDay:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to day should clear time portion',
    EncodeDate(2024, 3, 15),
    TChronoKit.FloorDate(TestDate, duDay));
  WriteLn('Test49_FloorDateDay:Finished');
end;

procedure TRoundingTests.Test50_FloorDateWeek;
var
  TestDate: TDateTime;
begin
  WriteLn('Test50_FloorDateWeek:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500); // March 15, 2024 is a Friday
  AssertEquals('Floor to week should go to Sunday',
    EncodeDate(2024, 3, 10), // Should go to Sunday, March 10
    TChronoKit.FloorDate(TestDate, duWeek));
  WriteLn('Test50_FloorDateWeek:Finished');
end;

procedure TRoundingTests.Test51_FloorDateMonth;
var
  TestDate: TDateTime;
begin
  WriteLn('Test51_FloorDateMonth:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to month should go to first day of month',
    EncodeDate(2024, 3, 1),
    TChronoKit.FloorDate(TestDate, duMonth));
  WriteLn('Test51_FloorDateMonth:Finished');
end;

procedure TRoundingTests.Test52_FloorDateBiMonth;
var
  TestDate: TDateTime;
begin
  WriteLn('Test52_FloorDateBiMonth:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to bi-month should go to first day of even month',
    EncodeDate(2024, 3, 1),
    TChronoKit.FloorDate(TestDate, duBiMonth));
  WriteLn('Test52_FloorDateBiMonth:Finished');
end;

procedure TRoundingTests.Test53_FloorDateQuarter;
var
  TestDate: TDateTime;
begin
  WriteLn('Test53_FloorDateQuarter:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to quarter should go to first day of quarter',
    EncodeDate(2024, 1, 1),
    TChronoKit.FloorDate(TestDate, duQuarter));
  WriteLn('Test53_FloorDateQuarter:Finished');
end;

procedure TRoundingTests.Test54_FloorDateHalfYear;
var
  TestDate: TDateTime;
begin
  WriteLn('Test54_FloorDateHalfYear:Starting');
  TestDate := EncodeDateTime(2024, 8, 15, 14, 30, 45, 500);
  AssertEquals('Floor to half year should go to July 1 or January 1',
    EncodeDate(2024, 7, 1),
    TChronoKit.FloorDate(TestDate, duHalfYear));
  WriteLn('Test54_FloorDateHalfYear:Finished');
end;

procedure TRoundingTests.Test55_FloorDateYear;
var
  TestDate: TDateTime;
begin
  WriteLn('Test55_FloorDateYear:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to year should go to January 1',
    EncodeDate(2024, 1, 1),
    TChronoKit.FloorDate(TestDate, duYear));
  WriteLn('Test55_FloorDateYear:Finished');
end;

procedure TRoundingTests.Test56_CeilingDateSecond;
var
  TestDate: TDateTime;
begin
  WriteLn('Test56_CeilingDateSecond:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to second should round up to next second',
    EncodeDateTime(2024, 3, 15, 14, 30, 46, 0),
    TChronoKit.CeilingDate(TestDate, duSecond));
  WriteLn('Test56_CeilingDateSecond:Finished');
end;

procedure TRoundingTests.Test57_CeilingDateMinute;
var
  TestDate: TDateTime;
begin
  WriteLn('Test57_CeilingDateMinute:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to minute should round up to next minute',
    EncodeDateTime(2024, 3, 15, 14, 31, 0, 0),
    TChronoKit.CeilingDate(TestDate, duMinute));
  WriteLn('Test57_CeilingDateMinute:Finished');
end;

procedure TRoundingTests.Test58_CeilingDateHour;
var
  TestDate: TDateTime;
begin
  WriteLn('Test58_CeilingDateHour:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to hour should round up to next hour',
    EncodeDateTime(2024, 3, 15, 15, 0, 0, 0),
    TChronoKit.CeilingDate(TestDate, duHour));
  WriteLn('Test58_CeilingDateHour:Finished');
end;

procedure TRoundingTests.Test59_CeilingDateDay;
var
  TestDate: TDateTime;
begin
  WriteLn('Test59_CeilingDateDay:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to day should round up to next day',
    EncodeDate(2024, 3, 16),
    TChronoKit.CeilingDate(TestDate, duDay));
  WriteLn('Test59_CeilingDateDay:Finished');
end;

procedure TRoundingTests.Test60_CeilingDateWeek;
var
  TestDate: TDateTime;
begin
  WriteLn('Test60_CeilingDateWeek:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500); // March 15, 2024 is a Friday
  AssertEquals('Ceiling to week should go to next Sunday',
    EncodeDate(2024, 3, 17), // Should go to next Sunday, March 17
    TChronoKit.CeilingDate(TestDate, duWeek));
  WriteLn('Test60_CeilingDateWeek:Finished');
end;

procedure TRoundingTests.Test61_CeilingDateMonth;
var
  TestDate: TDateTime;
begin
  WriteLn('Test61_CeilingDateMonth:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to month should go to first day of next month',
    EncodeDate(2024, 4, 1),
    TChronoKit.CeilingDate(TestDate, duMonth));
  WriteLn('Test61_CeilingDateMonth:Finished');
end;

procedure TRoundingTests.Test62_CeilingDateBiMonth;
var
  TestDate: TDateTime;
begin
  WriteLn('Test62_CeilingDateBiMonth:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to bi-month should go to first day of next even month',
    EncodeDate(2024, 5, 1),
    TChronoKit.CeilingDate(TestDate, duBiMonth));
  WriteLn('Test62_CeilingDateBiMonth:Finished');
end;

procedure TRoundingTests.Test63_CeilingDateQuarter;
var
  TestDate: TDateTime;
begin
  WriteLn('Test63_CeilingDateQuarter:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to quarter should go to first day of next quarter',
    EncodeDate(2024, 4, 1),
    TChronoKit.CeilingDate(TestDate, duQuarter));
  WriteLn('Test63_CeilingDateQuarter:Finished');
end;

procedure TRoundingTests.Test64_CeilingDateHalfYear;
var
  TestDate: TDateTime;
begin
  WriteLn('Test64_CeilingDateHalfYear:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to half year should go to July 1',
    EncodeDate(2024, 7, 1),
    TChronoKit.CeilingDate(TestDate, duHalfYear));
  WriteLn('Test64_CeilingDateHalfYear:Finished');
end;

procedure TRoundingTests.Test65_CeilingDateYear;
var
  TestDate: TDateTime;
begin
  WriteLn('Test65_CeilingDateYear:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to year should go to January 1 of next year',
    EncodeDate(2025, 1, 1),
    TChronoKit.CeilingDate(TestDate, duYear));
  WriteLn('Test65_CeilingDateYear:Finished');
end;

procedure TRoundingTests.Test66_RoundDateSecond;
var
  TestDate: TDateTime;
begin
  WriteLn('Test66_RoundDateSecond:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to second should round to nearest second',
    EncodeDateTime(2024, 3, 15, 14, 30, 46, 0),
    TChronoKit.RoundDate(TestDate, duSecond));
  WriteLn('Test66_RoundDateSecond:Finished');
end;

procedure TRoundingTests.Test67_RoundDateMinute;
var
  TestDate: TDateTime;
begin
  WriteLn('Test67_RoundDateMinute:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to minute should round to nearest minute',
    EncodeDateTime(2024, 3, 15, 14, 31, 0, 0),
    TChronoKit.RoundDate(TestDate, duMinute));
  WriteLn('Test67_RoundDateMinute:Finished');
end;

procedure TRoundingTests.Test68_RoundDateHour;
var
  TestDate: TDateTime;
begin
  WriteLn('Test68_RoundDateHour:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to hour should round to nearest hour',
    EncodeDateTime(2024, 3, 15, 15, 0, 0, 0),
    TChronoKit.RoundDate(TestDate, duHour));
  WriteLn('Test68_RoundDateHour:Finished');
end;

procedure TRoundingTests.Test69_RoundDateDay;
var
  TestDate: TDateTime;
begin
  WriteLn('Test69_RoundDateDay:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to day should round to nearest day',
    EncodeDate(2024, 3, 16),
    TChronoKit.RoundDate(TestDate, duDay));
  WriteLn('Test69_RoundDateDay:Finished');
end;

procedure TRoundingTests.Test70_RoundDateWeek;
var
  TestDate: TDateTime;
begin
  WriteLn('Test70_RoundDateWeek:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500); // March 15, 2024 is a Friday
  AssertEquals('Round to week should round to nearest Sunday',
    EncodeDate(2024, 3, 17), // Should round to next Sunday, March 17
    TChronoKit.RoundDate(TestDate, duWeek));
  WriteLn('Test70_RoundDateWeek:Finished');
end;

procedure TRoundingTests.Test71_RoundDateMonth;
var
  TestDate: TDateTime;
begin
  WriteLn('Test71_RoundDateMonth:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to month should round to nearest month start',
    EncodeDate(2024, 4, 1),
    TChronoKit.RoundDate(TestDate, duMonth));
  WriteLn('Test71_RoundDateMonth:Finished');
end;

procedure TRoundingTests.Test72_RoundDateBiMonth;
var
  TestDate: TDateTime;
begin
  WriteLn('Test72_RoundDateBiMonth:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to bi-month should round to nearest even month start',
    EncodeDate(2024, 3, 1),
    TChronoKit.RoundDate(TestDate, duBiMonth));
  WriteLn('Test72_RoundDateBiMonth:Finished');
end;

procedure TRoundingTests.Test73_RoundDateQuarter;
var
  TestDate: TDateTime;
begin
  WriteLn('Test73_RoundDateQuarter:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to quarter should round to nearest quarter start',
    EncodeDate(2024, 4, 1),
    TChronoKit.RoundDate(TestDate, duQuarter));
  WriteLn('Test73_RoundDateQuarter:Finished');
end;

procedure TRoundingTests.Test74_RoundDateHalfYear;
var
  TestDate: TDateTime;
begin
  WriteLn('Test74_RoundDateHalfYear:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to half year should round to nearest half year start',
    EncodeDate(2024, 7, 1),
    TChronoKit.RoundDate(TestDate, duHalfYear));
  WriteLn('Test74_RoundDateHalfYear:Finished');
end;

procedure TRoundingTests.Test75_RoundDateYear;
var
  TestDate: TDateTime;
begin
  WriteLn('Test75_RoundDateYear:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to year should round to nearest year start',
    EncodeDate(2024, 1, 1),
    TChronoKit.RoundDate(TestDate, duYear));
  WriteLn('Test75_RoundDateYear:Finished');
end;

procedure TRoundingTests.Test149_CeilingDateRollsAcrossTimeBoundaries;
begin
  AssertEquals('Second ceiling must carry into the next minute',
    EncodeDateTime(2024, 3, 15, 14, 31, 0, 0),
    TChronoKit.CeilingDate(
      EncodeDateTime(2024, 3, 15, 14, 30, 59, 500), duSecond));
  AssertEquals('Minute ceiling must carry into the next hour',
    EncodeDateTime(2024, 3, 15, 15, 0, 0, 0),
    TChronoKit.CeilingDate(
      EncodeDateTime(2024, 3, 15, 14, 59, 30, 0), duMinute));
  AssertEquals('Hour ceiling must carry into the next day',
    EncodeDate(2024, 3, 16),
    TChronoKit.CeilingDate(
      EncodeDateTime(2024, 3, 15, 23, 30, 0, 0), duHour));
end;

procedure TRoundingTests.Test150_EndBoundariesContainTheirStartingBoundary;
begin
  AssertEquals('EndOfYear at January 1 must end the containing year',
    EncodeDateTime(2024, 12, 31, 23, 59, 59, 999),
    TChronoKit.EndOfYear(EncodeDate(2024, 1, 1)), OneMillisecond);
  AssertEquals('EndOfWeek at Sunday midnight must end the containing week',
    EncodeDateTime(2024, 3, 9, 23, 59, 59, 999),
    TChronoKit.EndOfWeek(EncodeDate(2024, 3, 3)), OneMillisecond);
end;

initialization
  RegisterTest(TRoundingTests);

end.
