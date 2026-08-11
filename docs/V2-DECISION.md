# 2.0 decision after v1.5.0

**Status:** Historical. Superseded for future planning on 2026-08-12 by the
[v1.6.0 API transition specification](API-Deprecations-v1.6.0.md). This record
is retained to explain the decision made from the narrower v1.5.0 beginner
audit; it is no longer the active 2.0 plan.

**Decision:** Continue with compatible 1.x releases. Do not publish a proposed
2.0 change list yet.

**Date:** 2026-08-11

## Why this decision is evidence-based

The roadmap makes 2.0 conditional: removal is justified only after a
documented replacement has existed in a 1.x release and usage evidence shows
that migration is valuable.

The [v1.5.0 API audit](API-Audit-v1.5.0.md) found two first-five-minutes
discovery problems:

- developers searching method names for “format” could not find
  `GetAsString`; and
- developers searching method names for “parse” could not find `FromString`.

Both problems have an additive solution in v1.5.0. `FormatDateTime` and
`ParseDateTime` use the established implementations, while `GetAsString` and
`FromString` remain source-compatible. The documentation gaps and invalid
cheat-sheet examples identified by the audit are also correctable without a
breaking release.

Repository evidence does not currently show:

- a public API with a documented deprecation and proven replacement in an
  earlier 1.x release;
- issue, discussion, telemetry, or user-study evidence that removal would
  deliver more value than its migration cost; or
- a required behavior correction that cannot be delivered compatibly.

The audit therefore supports a discoverability release, not a major-version
migration. Publishing a speculative removal list would turn naming preference
into a compatibility commitment without the evidence required by the roadmap.

## Consequences

- v1.5.0 adds preferred discovery names but deprecates nothing.
- Existing 1.x source continues to compile with `GetAsString` and
  `FromString`.
- The roadmap's 2.0 milestone remains conditional, not scheduled.
- Documentation teaches one preferred path while clearly identifying
  compatibility names.
- No 2.0 migration guide is needed for v1.5.0 because there is no proposed
  breaking change list.

## Evidence required to reconsider

Reconsider a 2.0 proposal only when all applicable evidence is available:

1. A repeated usability or correctness problem is documented through issues,
   discussions, support reports, or a recorded user study.
2. A compatible replacement ships in a 1.x release with examples and tests.
3. The original API is formally deprecated in 1.x and the deprecation period
   covers at least one release.
4. The proposed removal or behavior change has a concrete migration recipe.
5. The benefit of one clearer path outweighs source-migration cost.
6. The complete Windows and Linux release matrix passes before the major
   release.

When those conditions are met, publish a proposed change list for review
before implementation. Until then, continue improving discovery and behavior
compatibly in 1.x.

## Observations that are not proposals

The audit encountered historical names and specialized behavior that may
deserve future usage research. They are intentionally not listed as 2.0
changes because repository inspection alone does not prove that changing or
removing them would help users. Recording that distinction prevents this
decision from being treated as an undeclared deprecation plan.
