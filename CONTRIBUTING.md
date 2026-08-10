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
