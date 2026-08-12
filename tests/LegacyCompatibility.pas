program LegacyCompatibility;

{$mode objfpc}{$H+}
{$WARN SYMBOL_DEPRECATED ON}

uses
  SysUtils, DateUtils, ChronoKit;

procedure CompileLegacySurface;
var
  Value, Other: TDateTime;
  Text: string;
  Kind: TDateSpanKind;
  Span: TDateSpan;
  Interval, OtherInterval: TInterval;
  ZoneInfo: TTimeZoneInfo;
  UnitValue: TDateUnit;
  IntegerValue: Integer;
  Int64Value: Int64;
  BooleanValue: Boolean;
begin
  Value := EncodeDateTime(2024, 1, 15, 12, 0, 0, 0);
  Other := EncodeDateTime(2024, 1, 16, 12, 0, 0, 0);

  Value := TChronoKit.GetDateTime(Value);
  Text := TChronoKit.GetAsString(Value, 'yyyy-mm-dd');
  Value := TChronoKit.FromString(Text, 'yyyy-mm-dd');
  Value := TChronoKit.RollbackMonth(Value);
  Value := TChronoKit.RollForwardMonth(Value);

  Value := TChronoKit.YMD('2024-01-15');
  Value := TChronoKit.MDY('01-15-2024');
  Value := TChronoKit.DMY('15-01-2024');
  Value := TChronoKit.YQ('2024-1');

  Kind := dskPeriod;
  Span := TChronoKit.CreatePeriod(0, 1);
  Span := TChronoKit.CreateDuration(0, 0, 1);
  Value := TChronoKit.AddSpan(Value, Span);
  Value := TChronoKit.SubtractSpan(Value, Span);
  Span := TChronoKit.SpanBetween(Value, Other, Kind);
  Int64Value := TChronoKit.PeriodToSeconds(Span);
  Span := TChronoKit.SecondsToPeriod(Int64Value);
  Span := TChronoKit.StandardizePeriod(Span);

  Interval := TChronoKit.CreateInterval(Value, Other);
  OtherInterval := TChronoKit.CreateInterval(Other, Other + 1);
  BooleanValue := TChronoKit.IsWithinInterval(Value, Interval);
  BooleanValue := TChronoKit.IntervalsOverlap(Interval, OtherInterval);
  Span := TChronoKit.IntervalLength(Interval, dskDuration);
  BooleanValue := TChronoKit.IntervalAlign(Interval, OtherInterval);
  Span := TChronoKit.IntervalGap(Interval, OtherInterval);
  Interval := TChronoKit.IntervalSetdiff(Interval, OtherInterval);
  Interval := TChronoKit.IntervalUnion(Interval, OtherInterval);
  Interval := TChronoKit.IntervalIntersection(Interval, OtherInterval);

  Value := TChronoKit.DateDecimal(2024.5);
  Int64Value := Round(TChronoKit.GetDecimalDate(Value));
  ZoneInfo := TChronoKit.GetTimeZone(Value);
  Value := TChronoKit.WithTimeZone(Value, 'UTC');
  Value := TChronoKit.ForceTimeZone(Value, 'UTC');
  IntegerValue := TChronoKit.GetEpiYear(Value);
  IntegerValue := TChronoKit.GetEpiWeek(Value);

  { FPC 3.2.2 cannot annotate one enum value. This use proves duSeason still
    compiles; its v1.6 documentation marker supplies the deprecation notice. }
  UnitValue := duSeason;

  if BooleanValue and (IntegerValue = Int64Value) and ZoneInfo.IsDST and
     (UnitValue = duSeason) then
    WriteLn(Text, Span.Milliseconds, Interval.StartDate);
end;

begin
  if False then
    CompileLegacySurface;
end.
