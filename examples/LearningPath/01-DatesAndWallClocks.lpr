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
