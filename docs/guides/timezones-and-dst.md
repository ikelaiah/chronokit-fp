# Timezones and DST

ChronoKit's timezone functions operate on wall-clock `TDateTime` values. A
value does not store its timezone, so choose the operation from what the input
clock means and retain the zone name in your own model when it matters later.

## Choose the conversion direction

| Input meaning | Preferred operation |
|---|---|
| A clock in the computer's system timezone | `SystemLocalToTimeZone(Value, TargetZone)` |
| A clock in a named source zone, shown in the system zone | `TimeZoneToSystemLocal(Value, SourceZone)` |
| A clock in a named source zone, shown in another named zone | `ConvertBetweenTimeZones(Value, SourceZone, TargetZone)` |

The target value represents the same instant, but remains an unzoned
`TDateTime`. `GetSystemTimeZone`, `GetSystemTimeZoneInfo`, and
`GetTimeZoneNames` expose platform information when you need it.

## Do not guess at DST discontinuities

An ambiguous clock during a fall-back overlap and a nonexistent clock during a
spring-forward gap both raise `ETimeZoneError` when used as a source value.
Catch the error and ask the caller to choose another clock rather than silently
selecting an occurrence.

```pascal
try
  UTCValue := TChronoKit.ConvertBetweenTimeZones(
    SourceValue, SourceTimeZone, 'UTC');
except
  on E: ETimeZoneError do
    WriteLn('The source clock cannot identify one instant: ', E.Message);
end;
```

`UTC` is portable on Windows and Linux. Other names are platform-native:
Linux uses IANA identifiers such as `America/New_York`, while Windows uses
names such as `Eastern Standard Time`. Read the authoritative
[Timezone Contract](../Timezone-Contract.md) before accepting or persisting
named identifiers. The [named-timezone example](../../examples/LearningPath/05-NamedTimeZones.lpr)
uses the correct New York identifier for each platform.
