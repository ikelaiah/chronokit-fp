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
