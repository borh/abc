# Custom parser quarterly maintenance evidence template

Copy the machine-readable record from `docs/evidence/external/custom-parser-maintenance-2026-q3.json`, update every identity and observation, and validate it against `schemas/custom-parser-maintenance-evidence.schema.json` plus `abc.tools.parser-maintenance-evidence/problems`. Never carry an unreviewed zero into a new quarter.

Declare the quarter, measurement period, recording date, `review_after`, and `expires_on`. Set status to `initial`, `observed`, or `quarter_closed`; an initial record is protocol scaffolding rather than a completed zero observation. A record is decision evidence only through its expiry date, and validation uses the explicit governance `as-of` date rather than wall-clock time. Declare the fixed focused-session duration; the protocol baseline is 120 minutes, while partial sessions use exact elapsed minutes.

## Quarterly categories

Record elapsed minutes and factual notes for every category, including zero observations:

- Upstream review
- Selective ports
- Security review
- Missed fixes
- Fork-only defects
- Planned divergence
- Remeasurement
- Maintainer availability

Planned divergence stays separate from incurred defect or port work, but remains part of total ownership cost. Availability notes state capacity changes and continuity risk; they are not inferred from a zero-minute entry.

## Disposable trial merge

Pin the upstream repository and 40-character upstream and fork revisions. Create a disposable worktree, perform a no-commit merge, record its merge-base and conflicts, run focused tests, capture reproducible outputs, then delete the worktree. The maintained branch must not change.

The trial-merge estimate is an `expert-assessment`. List additive minute components and explicit assumptions so another reviewer can re-derive the total. It must not claim observed conflicts or timings. Put reproducible observations in `benchmarks`, each typed `benchmark` with an integer value/unit, exact command, environment, and SHA-256 result hash. Every benchmark also names a workspace-relative raw artifact and its SHA-256 hash; a summary value without hashed raw output is not admissible.

## Decision-revisit predicates

Record the complete typed observation set in the same quarterly record: rolling-quarter total minutes, the selective-port minute sample and derived p90, maximum missed-fix severity on the 0–5 scale, and maintainer availability. Declare executable predicates over those observations for rolling total maintenance time, selective-port cost distribution, maximum missed-fix severity, and maintainer availability. Numeric predicates use numeric operators and integer thresholds; availability uses equality and a boolean threshold. Triggered predicates initiate review; they do not silently reverse ownership.
