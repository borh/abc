# P4A1 Task 5 implementation report

## Result

Projected immutable `ClassifiedSourceFact` values in the existing
`Normalizer::emit` fold. The projection shares the classified-span traversal
that already produces normalized text and registries; it adds no second pass,
callback, or event bus. Facts retain sanitized UTF-8 coordinates for the later
capture/rebase work.

The Rust vocabulary is closed over the approved ABC policy roles,
dispositions, evidence classes, and construct identifiers. Parser variants
without an approved policy entry produce no invented fact. Canonicalization
orders complete fact values while deliberately preserving duplicates so later
validation can fail closed instead of silently repairing them.

## RED evidence

Command:

```text
cargo test -p ab-aozora-pipeline --test classified_source_facts
```

The new test target failed to compile with unresolved imports for the fact
vocabulary/canonicalizer and ten `LexOutput` missing-field errors. This was the
expected failure: no classified-source fact API or output existed.

## GREEN evidence

- Focused fact suite: 7 passed, including plain text, recovery, CRLF-normalized
  newline coordinates, unknown directives, ruby, balanced block markers,
  overlap/lowering, complete-value ordering, and duplicate preservation.
- Hegel properties: permutation-invariant canonicalization, semantic intervals
  contained in accounted intervals, and opaque facts never semantic (100 cases
  each).
- `cargo test -p ab-aozora-syntax`: 104 unit tests and 2 doc tests passed.
- `cargo test -p ab-aozora-pipeline`: 143 unit tests plus every integration and
  doc-test target passed, including byte-pinned recovery output checks.
- `cargo test -p ab-aozora-facade -p ab-aozora-aat`: 64 AAT unit tests, AAT
  goldens/properties, 93 facade unit tests, facade integration/property suites,
  and doc tests passed.
- `cargo clippy -p ab-aozora-syntax -p ab-aozora-pipeline --lib -- -D warnings`
  passed.
- `cargo fmt --all -- --check`, `git diff --check`, and
  `scripts/comment-hygiene-check.sh` passed.

`cargo clippy ... --all-targets -- -D warnings` reaches two pre-existing
warnings in `tests/plain_provenance.rs` (an item-after-statements import and a
`usize as u32` cast). The library targets changed here are warning-free; the
earlier test was not modified as part of this task.

## Boundary notes

Conditional target identities, structural witness serialization, sanitizer
normalization proofs, and decoded-coordinate rebasing remain downstream
capture responsibilities. Unsupported live parser variants remain visible as
missing policy facts rather than being coerced into a catch-all authorization.
