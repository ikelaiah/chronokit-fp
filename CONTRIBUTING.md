# Contributing to ChronoKit-FP

Thank you for your interest in contributing to ChronoKit-FP! We want to make
contributing to this project as easy and transparent as possible.

## 📝 Code of Conduct

- Be respectful and inclusive
- Use welcoming and inclusive language
- Be collaborative
- Focus on what is best for the community
- Show empathy towards other community members

## 🚀 Getting Started

1. Fork the repository
2. Clone your fork:
   ```bash
   git clone https://github.com/your-username/chronokit-fp.git
   ```
3. Create a new branch:
   ```bash
   git checkout -b feature/your-feature-name
   ```

## 💻 Development Guidelines

### Code Style

#### Naming Conventions
- `T` prefix for types (e.g., `TChronoKit`, `TDateSpan`)
- `I` prefix for interfaces
- `F` prefix for private fields (e.g., `FTimeZone`)
- `A` prefix for parameters in documentation (e.g., `AValue`, `ATimeZone`)
- PascalCase for types, methods, and variables
- UPPERCASE for constants

#### Formatting
- 2 spaces for indentation (no tabs)
- No space before opening parenthesis in method calls
- Space after commas in parameter lists
- Operators surrounded by spaces (`a := b + c`)
- Begin/end on new lines for procedures/functions
- Begin/end on same line for control structures

#### Documentation
- Block comments for class/interface documentation
- Line comments for implementation details
- Document public methods using:
  ```pascal
  { @description Detailed description
    @param ParamName Description
    @return Description of return value }
  ```
- Comments should explain why, not what (the code should be self-documenting)

#### Code Organization
- Public methods first, then protected, then private
- Group related methods together
- Implementation details after interface
- Local variables at the beginning of methods
- Keep methods focused and small (ideally < 50 lines)

### Internal Architecture

Users learn one unit: `ChronoKit`. Keep its public declarations stable and its
implementations as thin delegates or orchestration. Put non-trivial logic in
the internal unit that owns the domain:

| Change | Implementation unit | Test suite |
|---|---|---|
| Components, arithmetic, boundaries, rounding, quarters, ISO dates | `ChronoKitCalendar` | `ChronoKit.DateBasics.Tests`, `ChronoKit.Rounding.Tests`, `ChronoKit.CalendarSystems.Tests` |
| Exact durations and calendar periods | `ChronoKitDurations` | `ChronoKit.PeriodsDurations.Tests` |
| Half-open ranges | `ChronoKitRanges` | `ChronoKit.Ranges.Tests` |
| Working weeks and holidays | `ChronoKitBusinessCalendars` | `ChronoKit.BusinessCalendars.Tests` |
| Preferred parsing and formatting | `ChronoKitParsing` | `ChronoKit.Parsing.Tests` |
| Incompatible deprecated behavior | `ChronoKitLegacy` | `ChronoKit.LegacyBehavior.Tests` |
| Named timezones and platform rules | `ChronoKitTimeZones` | `ChronoKit.TimeZones.Tests` |

Shared internal records belong in `ChronoKitInternalTypes`. Public records and
enum values stay declared in `ChronoKit`; map them explicitly at the façade
boundary. Preferred domain units must never depend on `ChronoKitLegacy`.

When a public declaration intentionally changes, regenerate both manifests:

```powershell
pwsh -NoProfile -File tools/GenerateApiManifest.ps1 -Write
```

Review both files in `api/` as part of the same change. For internal-only work,
`tools/TestApiManifest.ps1` must remain green without regeneration.

#### Error Handling
- Use exceptions for error conditions
- Clean up resources in `finally` blocks
- Provide meaningful error messages
- Use custom exception types for specific error cases

### Commit Messages

- Use clear and meaningful commit messages
- Start with a verb (Add, Fix, Update, etc.)
- Reference issues when relevant

Example:
```
Improve timezone validation errors

- Return a clear error for unsupported timezone names
- Add unit tests
- Update documentation
Fixes #123
```

### Testing

- Add unit tests for new functionality
- Ensure all tests pass before submitting PR
- Ensure the Windows and Linux pull-request checks pass
- Keep tests in the domain suite that owns the behavior; do not rebuild a
  monolithic test fixture
- Run `pwsh -NoProfile -File tools/TestDocumentation.ps1` to check both API
  manifests and generated reference documentation
- Run `pwsh -NoProfile -File tools/TestCleanConsumers.ps1` after adding or
  moving an internal unit

### Documentation

- Update README.md if needed
- Add/update API documentation
- Include examples for new features
- Update changelog

## 📋 Pull Request Process

1. Update the README.md with details of changes if needed
2. Update the documentation
3. Add tests for new functionality
4. Ensure the test suite passes
5. Update the CHANGELOG.md
6. Submit a pull request

### Pull Request Title Format

```text
type(scope): short description
```

Common types:

- `feat` - New functionality
- `fix` - Bug fixes
- `docs` - Documentation only
- `test` - Test-related changes
- `refactor` - Code refactoring
- `release` - Release preparation

## 🚢 Releasing

Follow [RELEASING.md](RELEASING.md) when preparing or publishing a release.
The release checks run in CI and refuse to merge a release PR that forgets any
publication metadata.

When preparing a release, never replace `docs/versions.json` with a
single-version catalogue. Add the new release at the TOP, update `current`,
retain all historical entries, and use the immutable `vX.Y.Z` tag as
`source_ref`. Run the repository release checker
(`tools/check_release.py --pre-tag`) before opening the PR. These rules apply
equally to human maintainers and coding agents.

## 🐛 Reporting Issues

- Use the issue tracker
- Describe the bug or feature request clearly
- Include code examples if relevant
- Provide system information (OS, FPC version)
- Follow the issue template

## 📚 Documentation Contributions

We especially welcome documentation improvements:
- Fix typos
- Add examples
- Clarify confusing sections
- Add missing documentation
- Translate documentation

## ⭐ Recognition

Contributors will be recognized in:
- CONTRIBUTORS.md file
- Release notes
- Project documentation

## 📄 License

By contributing, you agree that your contributions will be licensed under the MIT License.
