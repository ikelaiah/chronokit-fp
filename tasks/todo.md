# v1.7 maintainability checklist

- [x] Remove dead test scaffolding
- [x] Split 178 tests into domain suites without assertion changes
- [x] Add and check Windows/Linux v1.7 API manifests
- [ ] Record internal architecture and contributor placement rules
- [ ] Introduce and verify the shared-types seam
- [ ] Extract durations and ranges
- [ ] Extract business calendars
- [ ] Extract calendar arithmetic and rounding
- [ ] Extract parsing and formatting
- [ ] Isolate incompatible legacy implementations
- [ ] Simplify timezone facade duplication
- [ ] Decide whether platform timezone backends should be split
- [ ] Run full Windows-oriented local verification
- [ ] Run documentation, package, example, and consumer checks
- [ ] Complete final code review
