unit ChronoKit.Ranges.Tests;

{$mode objfpc}{$H+}{$J-}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  Classes, SysUtils, DateUtils, fpcunit, testregistry,
  ChronoKit;

type
  TRangeTests = class(TTestCase)
  published
    procedure Test161_HalfOpenRangeValidationAndContainment;
    procedure Test162_HalfOpenRangeRelationsAndGap;
    procedure Test163_SubtractRangeReturnsEveryRemainder;
    procedure Test164_RangeTryOperationsAvoidSentinels;
  end;

implementation

procedure TRangeTests.Test161_HalfOpenRangeValidationAndContainment;
var
  RangeValue, EmptyRange: TDateTimeRange;
  Raised: Boolean;
begin
  RangeValue := TChronoKit.CreateRange(EncodeDate(2024, 1, 1),
    EncodeDate(2024, 1, 2));
  AssertTrue('A half-open range contains its start',
    TChronoKit.RangeContains(RangeValue, EncodeDate(2024, 1, 1)));
  AssertFalse('A half-open range excludes its end',
    TChronoKit.RangeContains(RangeValue, EncodeDate(2024, 1, 2)));

  EmptyRange := TChronoKit.CreateRange(EncodeDate(2024, 1, 1),
    EncodeDate(2024, 1, 1));
  AssertFalse('An empty range contains no values',
    TChronoKit.RangeContains(EmptyRange, EncodeDate(2024, 1, 1)));

  Raised := False;
  try
    TChronoKit.CreateRange(EncodeDate(2024, 1, 2),
      EncodeDate(2024, 1, 1));
  except
    on E: EArgumentException do
      Raised := Pos('start', LowerCase(E.Message)) > 0;
  end;
  AssertTrue('CreateRange must reject reversed endpoints', Raised);
end;

procedure TRangeTests.Test162_HalfOpenRangeRelationsAndGap;
var
  FirstRange, TouchingRange, OverlappingRange, DistantRange: TDateTimeRange;
  Gap: TDuration;
begin
  FirstRange := TChronoKit.CreateRange(
    EncodeDateTime(2024, 1, 1, 9, 0, 0, 0),
    EncodeDateTime(2024, 1, 1, 10, 0, 0, 0));
  TouchingRange := TChronoKit.CreateRange(
    EncodeDateTime(2024, 1, 1, 10, 0, 0, 0),
    EncodeDateTime(2024, 1, 1, 11, 0, 0, 0));
  OverlappingRange := TChronoKit.CreateRange(
    EncodeDateTime(2024, 1, 1, 9, 30, 0, 0),
    EncodeDateTime(2024, 1, 1, 10, 30, 0, 0));
  DistantRange := TChronoKit.CreateRange(
    EncodeDateTime(2024, 1, 1, 10, 0, 0, 1),
    EncodeDateTime(2024, 1, 1, 11, 0, 0, 0));

  AssertTrue('Ranges with equal adjacent endpoints touch',
    TChronoKit.RangesTouch(FirstRange, TouchingRange));
  AssertFalse('Touching half-open ranges do not overlap',
    TChronoKit.RangesOverlap(FirstRange, TouchingRange));
  AssertTrue('Ranges sharing included values overlap',
    TChronoKit.RangesOverlap(FirstRange, OverlappingRange));
  AssertEquals('RangeDuration returns exact elapsed milliseconds',
    Int64(60 * 60 * 1000),
    TChronoKit.RangeDuration(FirstRange).Milliseconds);

  Gap := TChronoKit.RangeGap(FirstRange, DistantRange);
  AssertEquals('RangeGap preserves a one-millisecond gap',
    Int64(1), Gap.Milliseconds);
  AssertEquals('Touching ranges have a zero gap', Int64(0),
    TChronoKit.RangeGap(FirstRange, TouchingRange).Milliseconds);
end;

procedure TRangeTests.Test163_SubtractRangeReturnsEveryRemainder;
var
  ValueRange, RemoveRange: TDateTimeRange;
  Results: TDateTimeRangeArray;
begin
  ValueRange := TChronoKit.CreateRange(EncodeDate(2024, 1, 1),
    EncodeDate(2024, 1, 10));
  RemoveRange := TChronoKit.CreateRange(EncodeDate(2024, 1, 4),
    EncodeDate(2024, 1, 6));
  Results := TChronoKit.SubtractRange(ValueRange, RemoveRange);
  AssertEquals('A middle removal must produce two ranges', 2, Length(Results));
  AssertEquals('Left remainder starts at original start',
    EncodeDate(2024, 1, 1), Results[0].StartValue);
  AssertEquals('Left remainder ends at removal start',
    EncodeDate(2024, 1, 4), Results[0].EndValue);
  AssertEquals('Right remainder starts at removal end',
    EncodeDate(2024, 1, 6), Results[1].StartValue);
  AssertEquals('Right remainder ends at original end',
    EncodeDate(2024, 1, 10), Results[1].EndValue);

  Results := TChronoKit.SubtractRange(ValueRange,
    TChronoKit.CreateRange(EncodeDate(2023, 12, 1),
      EncodeDate(2024, 2, 1)));
  AssertEquals('Complete removal returns no ranges', 0, Length(Results));

  Results := TChronoKit.SubtractRange(ValueRange,
    TChronoKit.CreateRange(EncodeDate(2024, 1, 10),
      EncodeDate(2024, 1, 12)));
  AssertEquals('A touching removal leaves the value unchanged',
    1, Length(Results));
  AssertEquals('Unchanged result preserves the end',
    ValueRange.EndValue, Results[0].EndValue);
end;

procedure TRangeTests.Test164_RangeTryOperationsAvoidSentinels;
var
  FirstRange, SecondRange, ResultRange: TDateTimeRange;
begin
  FirstRange := TChronoKit.CreateRange(0, 1);
  SecondRange := TChronoKit.CreateRange(0, 0.5);
  AssertTrue('A valid intersection may start at TDateTime zero',
    TChronoKit.TryIntersectRanges(FirstRange, SecondRange, ResultRange));
  AssertEquals('Intersection preserves the valid zero start',
    TDateTime(0), ResultRange.StartValue);
  AssertEquals('Intersection chooses the earlier end',
    TDateTime(0.5), ResultRange.EndValue);

  SecondRange := TChronoKit.CreateRange(1, 2);
  AssertTrue('Touching ranges can be merged',
    TChronoKit.TryMergeRanges(FirstRange, SecondRange, ResultRange));
  AssertEquals('Merged range spans both inputs', TDateTime(2),
    ResultRange.EndValue);

  SecondRange := TChronoKit.CreateRange(2, 3);
  AssertFalse('Disjoint ranges cannot be represented by one merge result',
    TChronoKit.TryMergeRanges(FirstRange, SecondRange, ResultRange));
  AssertFalse('Disjoint ranges have no intersection',
    TChronoKit.TryIntersectRanges(FirstRange, SecondRange, ResultRange));
end;

initialization
  RegisterTest(TRangeTests);

end.
