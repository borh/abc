# Custom parser quarterly maintenance evidence template

Copy the machine-readable record from `docs/evidence/external/custom-parser-maintenance-2026-q3.json`, update every identity and observation, and validate it against `schemas/custom-parser-maintenance-evidence.schema.json` plus `abc.tools.parser-maintenance-evidence/problems`. Never carry an unreviewed zero into a new quarter.

Declare the quarter, measurement period, recording date, `review_after`, and `expires_on`. A record is decision evidence only through its expiry date. Declare the fixed focused-session duration; the protocol baseline is 120 minutes, while partial sessions use exact elapsed minutes.

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

The trial-merge estimate is an `expert-assessment`. List additive minute components and explicit assumptions so another reviewer can re-derive the total. It must not claim observed conflicts or timings. Put reproducible observations in `benchmarks`, each typed `benchmark` with an integer value/unit, exact command, environment, and SHA-256 result hash.

## Decision-revisit predicates

Declare executable predicates for rolling-quarter total maintenance time, selective-port cost distribution, maximum missed-fix severity, and maintainer availability. Record the operator, threshold, and review action. Triggered predicates initiate review; they do not silently reverse ownership.
