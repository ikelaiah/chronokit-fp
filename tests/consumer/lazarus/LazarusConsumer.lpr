program LazarusConsumer;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils,
  ChronoKit;

var
  Calendar: TBusinessCalendar;
begin
  Calendar := TChronoKit.CreateBusinessCalendar([]);
  WriteLn(TChronoKit.BusinessDaysBetween(EncodeDate(2026, 8, 3),
    EncodeDate(2026, 8, 7), Calendar));
end.
