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
