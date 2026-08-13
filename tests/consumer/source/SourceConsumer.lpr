program SourceConsumer;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils,
  ChronoKit;

var
  Calendar: TBusinessCalendar;
  QuarterEnd: TDateTime;
begin
  Calendar := TChronoKit.CreateBusinessCalendar([]);
  QuarterEnd := TChronoKit.EndOfQuarter(EncodeDate(2026, 8, 17));
  WriteLn(TChronoKit.FormatDateTime(QuarterEnd, 'yyyy-mm-dd'));
  WriteLn(TChronoKit.BusinessDaysBetween(EncodeDate(2026, 8, 3),
    EncodeDate(2026, 8, 7), Calendar));
end.
