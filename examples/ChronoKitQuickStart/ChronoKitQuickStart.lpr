program ChronoKitQuickStart;

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

