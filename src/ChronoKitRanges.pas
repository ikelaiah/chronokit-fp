unit ChronoKitRanges;

{$mode objfpc}{$H+}{$J-}

interface

uses
  ChronoKitInternalTypes;

function CKCreateRange(const AStart, AEnd: TDateTime): TCKDateTimeRange;
function CKRangeContains(const ARange: TCKDateTimeRange;
  const AValue: TDateTime): Boolean;
function CKRangesOverlap(const AFirst, ASecond: TCKDateTimeRange): Boolean;
function CKRangeDuration(const ARange: TCKDateTimeRange): TCKDuration;
function CKRangesTouch(const AFirst, ASecond: TCKDateTimeRange): Boolean;
function CKRangeGap(const AFirst, ASecond: TCKDateTimeRange): TCKDuration;
function CKSubtractRange(const AValue,
  ARemove: TCKDateTimeRange): TCKDateTimeRangeArray;
function CKTryMergeRanges(const AFirst, ASecond: TCKDateTimeRange;
  out AMerged: TCKDateTimeRange): Boolean;
function CKTryIntersectRanges(const AFirst, ASecond: TCKDateTimeRange;
  out AIntersection: TCKDateTimeRange): Boolean;

implementation

uses
  SysUtils, DateUtils, ChronoKitDurations;

procedure ValidateRange(const ARange: TCKDateTimeRange);
begin
  if CompareDateTime(ARange.StartValue, ARange.EndValue) > 0 then
    raise EArgumentException.Create(
      'Range start must not be later than range end');
end;

function RangeIsEmpty(const ARange: TCKDateTimeRange): Boolean;
begin
  Result := CompareDateTime(ARange.StartValue, ARange.EndValue) = 0;
end;

function CKCreateRange(const AStart, AEnd: TDateTime): TCKDateTimeRange;
begin
  Result.StartValue := AStart;
  Result.EndValue := AEnd;
  ValidateRange(Result);
end;

function CKRangeContains(const ARange: TCKDateTimeRange;
  const AValue: TDateTime): Boolean;
begin
  ValidateRange(ARange);
  Result := (CompareDateTime(AValue, ARange.StartValue) >= 0) and
    (CompareDateTime(AValue, ARange.EndValue) < 0);
end;

function CKRangesOverlap(const AFirst, ASecond: TCKDateTimeRange): Boolean;
begin
  ValidateRange(AFirst);
  ValidateRange(ASecond);
  Result := not RangeIsEmpty(AFirst) and not RangeIsEmpty(ASecond) and
    (CompareDateTime(AFirst.StartValue, ASecond.EndValue) < 0) and
    (CompareDateTime(ASecond.StartValue, AFirst.EndValue) < 0);
end;

function CKRangeDuration(const ARange: TCKDateTimeRange): TCKDuration;
begin
  ValidateRange(ARange);
  Result := CKDurationBetween(ARange.StartValue, ARange.EndValue);
end;

function CKRangesTouch(const AFirst, ASecond: TCKDateTimeRange): Boolean;
begin
  ValidateRange(AFirst);
  ValidateRange(ASecond);
  Result := not RangeIsEmpty(AFirst) and not RangeIsEmpty(ASecond) and
    ((CompareDateTime(AFirst.EndValue, ASecond.StartValue) = 0) or
     (CompareDateTime(ASecond.EndValue, AFirst.StartValue) = 0));
end;

function CKRangeGap(const AFirst, ASecond: TCKDateTimeRange): TCKDuration;
begin
  ValidateRange(AFirst);
  ValidateRange(ASecond);
  Result.Milliseconds := 0;
  if CompareDateTime(AFirst.EndValue, ASecond.StartValue) < 0 then
    Result := CKDurationBetween(AFirst.EndValue, ASecond.StartValue)
  else if CompareDateTime(ASecond.EndValue, AFirst.StartValue) < 0 then
    Result := CKDurationBetween(ASecond.EndValue, AFirst.StartValue);
end;

function CKSubtractRange(const AValue,
  ARemove: TCKDateTimeRange): TCKDateTimeRangeArray;
begin
  ValidateRange(AValue);
  ValidateRange(ARemove);
  Result := nil;
  if RangeIsEmpty(AValue) then
    Exit;

  if RangeIsEmpty(ARemove) or not CKRangesOverlap(AValue, ARemove) then
  begin
    SetLength(Result, 1);
    Result[0] := AValue;
    Exit;
  end;

  if (CompareDateTime(ARemove.StartValue, AValue.StartValue) <= 0) and
     (CompareDateTime(ARemove.EndValue, AValue.EndValue) >= 0) then
    Exit;

  if CompareDateTime(ARemove.StartValue, AValue.StartValue) <= 0 then
  begin
    SetLength(Result, 1);
    Result[0] := CKCreateRange(ARemove.EndValue, AValue.EndValue);
    Exit;
  end;

  if CompareDateTime(ARemove.EndValue, AValue.EndValue) >= 0 then
  begin
    SetLength(Result, 1);
    Result[0] := CKCreateRange(AValue.StartValue, ARemove.StartValue);
    Exit;
  end;

  SetLength(Result, 2);
  Result[0] := CKCreateRange(AValue.StartValue, ARemove.StartValue);
  Result[1] := CKCreateRange(ARemove.EndValue, AValue.EndValue);
end;

function CKTryMergeRanges(const AFirst, ASecond: TCKDateTimeRange;
  out AMerged: TCKDateTimeRange): Boolean;
begin
  ValidateRange(AFirst);
  ValidateRange(ASecond);

  if RangeIsEmpty(AFirst) then
  begin
    AMerged := ASecond;
    Exit(True);
  end;
  if RangeIsEmpty(ASecond) then
  begin
    AMerged := AFirst;
    Exit(True);
  end;
  if not CKRangesOverlap(AFirst, ASecond) and
     not CKRangesTouch(AFirst, ASecond) then
    Exit(False);

  if CompareDateTime(AFirst.StartValue, ASecond.StartValue) <= 0 then
    AMerged.StartValue := AFirst.StartValue
  else
    AMerged.StartValue := ASecond.StartValue;
  if CompareDateTime(AFirst.EndValue, ASecond.EndValue) >= 0 then
    AMerged.EndValue := AFirst.EndValue
  else
    AMerged.EndValue := ASecond.EndValue;
  Result := True;
end;

function CKTryIntersectRanges(const AFirst, ASecond: TCKDateTimeRange;
  out AIntersection: TCKDateTimeRange): Boolean;
begin
  ValidateRange(AFirst);
  ValidateRange(ASecond);
  if not CKRangesOverlap(AFirst, ASecond) then
    Exit(False);

  if CompareDateTime(AFirst.StartValue, ASecond.StartValue) >= 0 then
    AIntersection.StartValue := AFirst.StartValue
  else
    AIntersection.StartValue := ASecond.StartValue;
  if CompareDateTime(AFirst.EndValue, ASecond.EndValue) <= 0 then
    AIntersection.EndValue := AFirst.EndValue
  else
    AIntersection.EndValue := ASecond.EndValue;
  Result := True;
end;

end.
