# Description

Summarise the change, its motivation, and any relevant context.

Closes # (if applicable)

## Type of change

Select all that apply.

- [ ] Bug fix
- [ ] New feature
- [ ] Breaking change
- [ ] Documentation update
- [ ] Tests or CI update
- [ ] Release or maintenance

## Checklist

- [ ] I have self-reviewed this change.
- [ ] The change follows this project's style and naming conventions.
- [ ] I updated documentation where needed.
- [ ] The change introduces no new compiler warnings.
- [ ] New and existing tests pass locally, where applicable.
- [ ] Updated or added examples compile, where applicable.
- [ ] I updated the changelog, where applicable.

## Testing

Describe the checks you ran and their result. Include the platform and Free
Pascal version when it matters.

- [ ] FPCUnit suite passed
- [ ] Shipped examples compiled
- [ ] Lazarus package compiled
- [ ] Other: describe below

### Windows (PowerShell)

```powershell
cd tests
fpc "-FU." "-Fu..\src" TestRunner.lpr
.\TestRunner.exe -a --format=plain
```

### Linux

```bash
cd tests
fpc "-FU." "-Fu../src" TestRunner.lpr
./TestRunner -a --format=plain
```

## Notes for reviewers

Call out compatibility implications, follow-up work, or intentionally excluded
scope.
