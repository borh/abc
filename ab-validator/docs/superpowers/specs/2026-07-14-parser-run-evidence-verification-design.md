# Parser Run Evidence Verification Design

## Outcome

The checked parser-study summary becomes a deterministic projection of external
raw-run evidence rather than a hand-maintained claim. A local resolver supplied
at verification time maps checked logical artifact roots to external directories;
no machine-local path is stored in source.

## Evidence contract

Every lane binds candidate, mode, parser and adapter revisions, corpus and
protocol hashes, timeout, exact command vector, declared environment, execution
hash, external manifest hash, and exact outcome counts. The verifier accepts
exactly five candidates, two modes, and two inventories. It resolves each root,
runs the neutral executor's full per-item verifier, confirms manifest and
execution hashes, proves command/environment correspondence to the declared
candidate and mode, and regenerates byte-identical canonical JSON.

Host capture is closed data with either all preregistered fields or an explicit
unavailable classification. Existing runs use `not_captured_immediately_before_run`;
their behavioral integrity remains usable, while performance host comparability
is unavailable under the preregistered missing-data rule. No post-hoc host value
is substituted.

## Repair audit

The aozora2html audit records recoverable facts only: both lane identities,
rejected-manifest status, unavailable rejected bytes where they were not
preserved, the overlap cause, the path-neutral repair command template, and the
final verified hashes. Mismatched IDs are recorded only if recoverable from
preserved evidence; otherwise their unavailable reason is explicit.

## Testing

Focused tests build small synthetic external roots and require byte-identical
regeneration, reject lane/count/command/environment mismatches, and require the
closed missing-host classification. A real-data verification uses an explicit
temporary resolver and all 20 completed external lanes.
