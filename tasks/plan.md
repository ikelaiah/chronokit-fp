# Implementation Plan: v1.5.0 discoverable API and 2.0 decision

## Overview

Deliver the v1.5.0 roadmap milestone by auditing common date/time tasks from a
beginner's point of view, making the documentation searchable by intent, and
adding only the small compatibility-preserving aliases justified by that
audit. The milestone will also record whether current evidence supports a 2.0
change list.

The public surface remains backwards compatible. `GetAsString` and
`FromString` continue to work throughout 1.x; the clearer `FormatDateTime` and
`ParseDateTime` names delegate to those established implementations.

## Architecture decisions

- Treat task questions (for example, “How do I parse a date?”) as the primary
  navigation model, with type/function taxonomy as a secondary reference.
- Add `TChronoKit.FormatDateTime` and `TChronoKit.ParseDateTime` as thin public
  aliases. The audit shows that text conversion is a first-five-minutes task,
  while the existing names do not contain the terms beginners search for.
- Keep `GetAsString` and `FromString` unchanged and documented as compatibility
  names. Do not introduce deprecation warnings during 1.x.
- Do not propose 2.0 removals without external usage/deprecation evidence. The
  decision record will distinguish audit findings from proof that migration is
  valuable.
- Add no dependency and make no timezone or calendar behavior change.

## Task list

### Phase 1: Audit and executable API additions

#### Task 1: Publish the beginner-focused API audit

**Description:** Test common date/time questions against the current docs and
public surface, recording the expected starting point, findability, example
coverage, and action for every observed gap.

**Acceptance criteria:**

- [x] The audit covers creation, parsing, formatting, arithmetic, boundaries,
      comparisons, spans, business calendars, intervals, week systems, and
      timezone conversion.
- [x] Every proposed API addition is tied to an observed discovery problem.
- [x] Documentation-only gaps and copy/paste errors are explicitly identified.

**Verification:**

- [x] Every audit action maps to a later task or a documented no-change result.
- [x] No unsupported claim about external user behavior is made.

**Dependencies:** None

**Files likely touched:**

- `docs/API-Audit-v1.5.0.md`

**Estimated scope:** Small

#### Task 2: Add failing tests for discoverable text helpers

**Description:** Specify the clearer formatting and parsing aliases before
implementation, including explicit-format behavior and established error
behavior.

**Acceptance criteria:**

- [x] Tests require `FormatDateTime` to match `GetAsString`.
- [x] Tests require `ParseDateTime` to match `FromString`.
- [x] Invalid input still raises the established descriptive `EConvertError`.

**Verification:**

- [x] The focused compile fails before implementation because the methods do
      not exist.
- [x] The focused tests pass after implementation.

**Dependencies:** Task 1

**Files likely touched:**

- `tests/ChronoKit.Test.pas`

**Estimated scope:** Small

#### Task 3: Implement and demonstrate the additive aliases

**Description:** Add the two thin public aliases, document their relationship
to the compatibility names, and update the copyable quick-start example.

**Acceptance criteria:**

- [x] Both aliases delegate to the existing behavior without duplicating
      parsing or formatting logic.
- [x] Existing 1.x calls remain source-compatible.
- [x] A shipped example compiles using both additions.

**Verification:**

- [x] The focused alias tests pass.
- [x] `examples/ChronoKitQuickStart/ChronoKitQuickStart.lpr` compiles.

**Dependencies:** Task 2

**Files likely touched:**

- `src/ChronoKit.pas`
- `tests/ChronoKit.Test.pas`
- `examples/ChronoKitQuickStart/ChronoKitQuickStart.lpr`

**Estimated scope:** Small

### Checkpoint: Audited additions

- [x] Each new method is justified, tested, documented inline, and executable.
- [x] No existing signature or behavior changed.
- [x] The source tree remains dependency-free.

### Phase 2: Task-oriented discovery

#### Task 4: Rebuild the cheat sheet around searchable questions

**Description:** Replace the long category-only reference with a searchable
task index, copyable recipes, and a complete compact public-method index.

**Acceptance criteria:**

- [x] Common search terms lead directly to the appropriate API and example.
- [x] Every public `TChronoKit` method appears in the compact index.
- [x] Incorrect `drs*` examples are replaced with the public `du*` enum.

**Verification:**

- [x] Method names in the index match the public declarations.
- [x] Every code sample uses the v1.5.0 public API.

**Dependencies:** Task 3

**Files likely touched:**

- `docs/Cheat-Sheet.md`

**Estimated scope:** Medium

#### Task 5: Group guides and API documentation by task

**Description:** Make the README, getting-started guide, and complete guide
lead from user intent to copyable answers, while preserving links to advanced
calendar and timezone contracts.

**Acceptance criteria:**

- [x] The main documentation paths use the same task vocabulary and preferred
      v1.5.0 names.
- [x] Parsing, formatting, arithmetic, business-day, interval, and timezone
      tasks each have a documented answer.
- [x] Compatibility aliases are explained without prematurely deprecating them.

**Verification:**

- [x] Repository-local documentation links resolve.
- [x] Examples compile or correspond to compiled public calls.

**Dependencies:** Task 4

**Files likely touched:**

- `README.md`
- `docs/Getting-Started.md`
- `docs/ChronoKit-FP.md`
- `docs/Troubleshooting.md`

**Estimated scope:** Medium

### Checkpoint: Discovery paths

- [x] Common questions can be answered without reading implementation code.
- [x] Searchable index and detailed guide agree on preferred operations.
- [x] Public additions have tests and copyable examples.

### Phase 3: Version decision and release records

#### Task 6: Publish the evidence-based 2.0 decision

**Description:** Evaluate the audit, existing compatibility promises, and the
absence or presence of proven deprecations, then publish the resulting 2.0
decision and its reconsideration criteria.

**Acceptance criteria:**

- [x] The record separates repository evidence from assumptions about users.
- [x] A proposed 2.0 change list is published only if justified by evidence.
- [x] The decision defines what future evidence would trigger reconsideration.

**Verification:**

- [x] The decision is consistent with the roadmap's conditional 2.0 policy.
- [x] README and roadmap link to the decision where appropriate.

**Dependencies:** Task 5

**Files likely touched:**

- `docs/V2-DECISION.md`
- `ROADMAP.md`
- `README.md`

**Estimated scope:** Small

#### Task 7: Prepare v1.5.0 release documentation and metadata

**Description:** Update version metadata, changelog, release notes, PR notes,
and milestone status after verification.

**Acceptance criteria:**

- [x] Source and Lazarus package metadata report v1.5.0.
- [x] Changelog and release notes describe additions and compatibility.
- [x] Roadmap status reflects the verified milestone outcome.

**Verification:**

- [x] Version search finds no current-release metadata left at v1.4.0.
- [x] Release records state the exact verification commands and outcomes.

**Dependencies:** Task 6

**Files likely touched:**

- `CHANGELOG.md`
- `ROADMAP.md`
- `docs/RELEASE-NOTES-v1.5.0.md`
- `docs/PR-v1.5.0.md`
- `packages/lazarus/chronokit_fp.lpk`

**Estimated scope:** Medium

### Checkpoint: Complete

- [ ] All v1.5.0 roadmap goals and done criteria are met.
- [ ] The full FPCUnit suite passes with the required local timezone fixtures.
- [ ] Every shipped example compiles and the Lazarus package builds.
- [ ] Documentation links and public-method coverage are verified.
- [ ] `git diff --check` and the five-axis review find no required issue.

## Risks and mitigations

| Risk | Impact | Mitigation |
|---|---|---|
| New names conflict with `SysUtils` routines | Medium | Keep calls class-qualified for users and unit-qualify RTL calls inside the implementation. |
| A documentation audit overstates user evidence | High | Record only reproducible repository findings and explicitly label missing external usage data. |
| The cheat sheet becomes another long reference | Medium | Lead with question/search vocabulary and keep the exhaustive method list compact. |
| Release docs drift from executable examples | Medium | Compile every shipped example and reuse the preferred names consistently. |
| Local timezone tests fail without CI variables | Low | Run the documented Windows fixture matrix with system-local Sydney DST boundary values. |

## Open questions

None. The roadmap explicitly permits additive helpers only where the audit
finds a discovery problem, and it makes the 2.0 change list conditional on
evidence. Both decisions can be made from documented repository evidence
without breaking compatibility.
