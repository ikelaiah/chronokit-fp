# Troubleshooting

## The compiler cannot find `ChronoKit`

For a source-based project, the compiler must be given the `src/` directory,
not the repository root. For example:

```powershell
fpc "-FuC:\path\to\chronokit-fp\src" MyProgram.lpr
```

In Lazarus, open `packages/lazarus/chronokit_fp.lpk`, compile it, and select
`Use` → `Add to Project`. Then confirm that `ChronoKit` appears in the
project's `uses` clause.

## Parsing does not match the input

Pass the format that matches the input exactly. ChronoKit uses Free Pascal
format tokens: `mm` is the month and `nn` is the minute.

```pascal
Value := TChronoKit.FromString('2026-08-10 09:30', 'yyyy-mm-dd hh:nn');
```

Avoid relying on the computer's regional settings for values shared between
machines. Prefer an unambiguous form such as `yyyy-mm-dd`.

## The formatted value is unexpected

`TDateTime` combines a calendar date and a time. If a value has a time
component, include it in the format when diagnosing the result:

```pascal
WriteLn(TChronoKit.GetAsString(Value, 'yyyy-mm-dd hh:nn:ss'));
```

Use `EncodeDate` or `TChronoKit.GetToday` when the value should be a date at
midnight.

## Timezone results differ from a local time

`GetNow` returns the computer's local wall-clock time. A conversion with
`WithTimeZone` is a separate operation that depends on the supplied target
timezone and the platform's timezone data. Keep the timezone name with values
whose intended timezone must be preserved by your application.

`UTC` is the only identifier guaranteed on both platforms. On Linux, install
the system timezone database (commonly the `tzdata` package) and use supported
IANA names such as `Australia/Sydney`. Windows uses Windows identifiers such as
`AUS Eastern Standard Time`; the IANA name is not a portable alias.

An ambiguous local value occurs when clocks move backward, and a nonexistent
value occurs when clocks move forward. The timezone contract requires
`ETimeZoneError` instead of silently selecting an occurrence. Catch the
exception rather than matching its message, and see the
[timezone contract](Timezone-Contract.md) for the exact operation semantics
and v1.4.0 conformance scope.

## The project does not compile

ChronoKit-FP supports Free Pascal 3.2.2 or later. Lazarus is optional for
source-based projects, but Lazarus 4.0 or later is the supported IDE path.

Run the repository's complete test suite to check a local build:

```powershell
cd tests
fpc "-Fu..\src" TestRunner.lpr
.\TestRunner.exe -a --format=plain
```

Pull requests run this suite and compile the shipped examples on Windows and
Linux.
