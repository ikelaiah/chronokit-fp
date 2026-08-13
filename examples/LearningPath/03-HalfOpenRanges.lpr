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
