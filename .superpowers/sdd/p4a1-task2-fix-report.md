# P4A1 Task 2 correction report

Status: **DONE_WITH_CONCERNS**.

## Empirical commands

```text
cd ab-validator
CLASSIFIED_SOURCE_OUT=/tmp/classified-source-1.json \
  cargo test -p ab-aozora-pipeline --test classified_source_characterization -- --ignored
result: 1 passed; 0 failed

CLASSIFIED_SOURCE_OUT=/tmp/classified-source-2.json \
  cargo test -p ab-aozora-pipeline --test classified_source_characterization -- --ignored
result: 1 passed; 0 failed

cmp /tmp/classified-source-1.json /tmp/classified-source-2.json
result: exit 0; files identical
```

The disposable classifier-local observer ran at the decline/recovery branches
before `pending_plain_start` / `pending_refmark` coalescing. It did not inspect
AAT or Parser-IR. The generated JSON contains 36 cases with input bytes, pair
events, branch observations, `ClassifiedSpan` output, diagnostics, normalized
and verbatim SHA-256, and live `NodeKind` coverage.

## Decision

The empirical matrix confirms branch identity exists locally and disappears
from the existing emitted span seam. It does not justify freezing a closed
reason enum. Task 3 remains blocked, and no Task 3 code or `Other` reason was
introduced.

## Final covering verification

After removing the observer, probe, public helper, and temporary dependencies:

```text
cd ab-validator
cargo test -p ab-aozora-pipeline
result: pass; 143 unit tests, 74 integration/property tests, and 1 doc test;
        0 failed

cargo clippy -p ab-aozora-pipeline --all-targets -- -D warnings
result: pass; exit 0

cargo fmt --all -- --check
first result: failed only because the temporary edit had left import grouping
              and one blank line in classify/mod.rs unformatted
correction: applied rustfmt's exact import/blank-line layout
```

```text
cargo fmt --all -- --check
result: pass; exit 0

cargo test -p ab-aozora-pipeline --lib lexer::classify::tests
result: 14 passed; 0 failed; 129 filtered out

jq empty docs/superpowers/reports/2026-07-16-classified-source-provenance.json
result: pass; exit 0

! rg -n 'CHARACTERIZATION|characterize\(|classified_source_characterization|take_characterization' \
  crates/ab-aozora-pipeline
result: pass; no disposable observer/probe symbols remain

git diff --check
result: pass; exit 0
```

## Concern

The declared `NodeKind` universe has 26 tags, but this live-pipeline case set
observed 23. `warichu`, `framed`, and `container` were not fabricated: attempted
surface shapes resolve to `Directive` or block-container sentinel kinds in this
classifier. The JSON records those tags explicitly under `not_observed`.
