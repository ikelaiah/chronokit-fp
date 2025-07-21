# 📅 ChronoKit-FP: Toolkit for Dates & Times in Free Pascal

[![License: MIT](https://img.shields.io/badge/License-MIT-1E3A8A.svg)](https://opensource.org/licenses/MIT)
[![Free Pascal](https://img.shields.io/badge/Free%20Pascal-3.2.2+-3B82F6.svg)](https://www.freepascal.org/)
[![Lazarus](https://img.shields.io/badge/Lazarus-4.0+-60A5FA.svg)](https://www.lazarus-ide.org/)
![Supports Windows](https://img.shields.io/badge/support-Windows-F59E0B?logo=Windows)
![Supports Linux](https://img.shields.io/badge/support-Linux-F59E0B?logo=Linux)
[![Version](https://img.shields.io/badge/version-1.0.0-8B5CF6.svg)](CHANGELOG.md)
![No Dependencies](https://img.shields.io/badge/dependencies-none-10B981.svg)
[![Documentation](https://img.shields.io/badge/Docs-Available-brightgreen.svg)](docs/)
[![Status](https://img.shields.io/badge/Status-Stable-brightgreen.svg)]()

ChronoKit-FP is a lightweight toolkit designed to make date and time handling in Free Pascal easier for everyone. Whether you're calculating business days, handling timezones, or formatting dates, ChronoKit-FP offers practical tools to simplify your work.

```pascal
// Get today's date and add 5 business days
var
  Today, NextWeek: TDateTime;
begin
  Today := TChronoKit.GetToday;
  NextWeek := TChronoKit.AddBusinessDays(Today, 5);
  WriteLn('Next workday: ', TChronoKit.GetAsString(NextWeek, 'yyyy-mm-dd'));
end;
```

## 🌟 Why ChronoKit-FP?

ChronoKit-FP is a cross-platform date and time library for Free Pascal developers. If you've ever struggled with timezone handling or needed better date manipulation tools, ChronoKit-FP has you covered. It gives you everything you need to work with dates, times, and timezones in your Free Pascal applications.

**Key Features:**

- 🌍 **Cross-Platform Timezone Support** - Works on Windows and Linux
- ⏰ **50+ DateTime Functions** - Everything you need for date/time work
- 💼 **Business Day Calculations** - Built-in workday calculations
- 🎯 **Simple API** - Clean, easy-to-use function names
- 🧪 **Well Tested** - 130+ tests ensure everything works
- 📚 **Good Documentation** - Complete API reference with examples

## 📑 Table of Contents 

- [📅 ChronoKit-FP: Toolkit for Dates \& Times in Free Pascal](#-chronokit-fp-toolkit-for-dates--times-in-free-pascal)
  - [🌟 Why ChronoKit-FP?](#-why-chronokit-fp)
  - [📑 Table of Contents](#-table-of-contents)
  - [💻 Installation (Lazarus IDE)](#-installation-lazarus-ide)
  - [💻 Installation (General)](#-installation-general)
  - [📝 Library Usage](#-library-usage)
  - [🚀 Quick Start](#-quick-start)
    - [📅 DateTime Operations](#-datetime-operations)
    - [Dependencies](#dependencies)
    - [Build Requirements](#build-requirements)
  - [📚 Documentation](#-documentation)
  - [📊 Real-World Examples](#-real-world-examples)
  - [⚠️ Known Limitations](#️-known-limitations)
  - [✅ Testing](#-testing)
  - [🤝 Contributing](#-contributing)
  - [⚖️ License](#️-license)
  - [🙏 Acknowledgments](#-acknowledgments)

## 💻 Installation (Lazarus IDE)

1. Clone the repository:

```bash
git clone https://github.com/ikelaiah/chronokit-fp
```

2. Open / start a new project in Lazarus IDE

3. Go to `Package` → `Open Package File (.lpk)...`

4. Navigate to the ChronoKit packages in the `packages/lazarus/` folder and select `chronokit_fp.lpk`

5. In the package window that opens, click `Compile`

6. Click `Use → Add to Project` to install the package

The ChronoKit-FP package is now ready to use in your Lazarus project.

## 💻 Installation (General)

1. Clone the repository:

```bash
git clone https://github.com/ikelaiah/chronokit-fp
```

2. Add the source directory to your project's search path.


## 📝 Library Usage

```pascal
uses
  // Date Time manipulation unit
  ChronoKit;          // Date Time operations
```

## 🚀 Quick Start


### 📅 DateTime Operations

```pascal
program ChronoKitDemo;
uses
  ChronoKit, SysUtils;

var
  CurrentTime, NextWorkday: TDateTime;
  TZInfo: TTimeZoneInfo;
  BusinessHours: TInterval;
  OffsetSign: string;
begin
  // Basic date/time operations
  CurrentTime := TChronoKit.GetNow;
  NextWorkday := TChronoKit.NextBusinessDay(CurrentTime);
  
  WriteLn('Current time: ', TChronoKit.GetAsString(CurrentTime, 'yyyy-mm-dd hh:nn:ss'));
  WriteLn('Next workday: ', TChronoKit.GetAsString(NextWorkday, 'yyyy-mm-dd'));
  
  // Business hours check (9 AM - 5 PM)
  BusinessHours := TChronoKit.CreateInterval(
    TChronoKit.StartOfDay(CurrentTime) + EncodeTime(9, 0, 0, 0),
    TChronoKit.StartOfDay(CurrentTime) + EncodeTime(17, 0, 0, 0)
  );
  
  if TChronoKit.IsWithinInterval(CurrentTime, BusinessHours) then
    WriteLn('✅ Within business hours')
  else
    WriteLn('❌ Outside business hours');
    
  // Timezone information
  TZInfo := TChronoKit.GetTimeZone(CurrentTime);
  if TZInfo.Offset >= 0 then
    OffsetSign := '+'
  else
    OffsetSign := '-';
    
  WriteLn('Timezone: ', TZInfo.Name, ' (UTC', OffsetSign, IntToStr(Abs(TZInfo.Offset) div 60), ')');
  WriteLn('DST active: ', BoolToStr(TZInfo.IsDST, 'Yes', 'No'));
end.
```

### Dependencies

- Windows / Linux
  - No external dependencies required
- Uses only standard Free Pascal RTL units

### Build Requirements

- Free Pascal Compiler (FPC) 3.2.2+
- Lazarus 4.0+ (for compiling example projects and test suites)
- Git for version control

## 📚 Documentation

For detailed documentation, check out:

- 📋 [API Cheat Sheet](docs/cheat-sheet.md) - Quick reference for all functions
- 📖 [Complete Documentation](docs/ChronoKit-FP.md) - Full guide and examples

## 📊 Real-World Examples

You can use ChronoKit-FP to build all kinds of applications:

| Example Project | Description | Source Code |
|-----------------|-------------|-------------|
| ChronoKit Example | Demonstrates the library's capabilities with practical use cases | [View Example](examples/ChronoKitExample/) |
| Add Business Days | Calculate next business day, accounting for weekends and holidays | [View Example](examples/AddBusinessDays/) |
| Quick Start Demo | Shows off what the library can do | [View Example](examples/ChronoKitQuickStart/) |

## ⚠️ Known Limitations

- **Platform Support**: Currently works on Windows 11 and Ubuntu 24.04.
- **Timezone Database**: Linux systems need timezone data installed (usually comes with most distributions).
- **DST Detection**:
  - Windows: Has hardcoded US DST rules
  - Linux: Uses the system's timezone database
  - The library detects whether a given date/time falls within DST period

## ✅ Testing

1. Open the `TestRunner.lpi` using Lazarus IDE
2. Compile the project
3. Run the Test Runner:

```bash
cd tests
./TestRunner.exe -a --format=plain
```

## 🤝 Contributing

Want to help out? Great! Feel free to submit a Pull Request. For big changes, please open an issue first so we can chat about it.

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## ⚖️ License

This project is licensed under the MIT License - see the [LICENSE](LICENSE.md) file for details.

## 🙏 Acknowledgments

- [Free Pascal Dev Team](https://www.freepascal.org/) for the Free Pascal compiler
- [Lazarus IDE Team](https://www.lazarus-ide.org/) for such an amazing IDE
- The helpful folks on various online communities:
    - [Unofficial Free Pascal Discord server](https://discord.com/channels/570025060312547359/570091337173696513)
    - [Free Pascal & Lazarus forum](https://forum.lazarus.freepascal.org/index.php)
    - [Tweaking4All Delphi, Lazarus, Free Pascal forum](https://www.tweaking4all.com/forum/delphi-lazarus-free-pascal/)
    - [Laz Planet - Blogspot](https://lazplanet.blogspot.com/) / [Laz Planet - GitLab](https://lazplanet.gitlab.io/)
    - [Delphi Basics](https://www.delphibasics.co.uk/index.html)
- Everyone who has helped make this project better
