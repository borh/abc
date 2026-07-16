# P4A1 Task 7 composition fix

## Result

Removed the pipeline-local accent reconciliation introduced by `3fc51cec` and
its direct-`lex(raw_source)` tests. That reconciliation observed only the
pipeline's own sanitizer diagnostics. The live adapter sanitizes first and then
parses `DecodedSource.span_text`, so the second sanitizer cannot rediscover the
already-applied rewrite and the code was inert at the publication boundary.

The replacement is exercised at the live composition boundary in the pending
AAT capture work:

```text
decode_source_bytes
  -> sanitize_for_aat (rewrite identity + composed map)
  -> lex(decoded.span_text) (classified fragments)
  -> reconcile_accent_edit_facts
  -> fact_entry (decoded-coordinate rebase)
```

The correlation key is not coordinate containment alone. One authenticated
`AccentDecompositionApplied` edit owns a classifier stream only when there is
exactly one recovered opening-delimiter fact and exactly one recovered
closing-delimiter fact at that edit's body-relative boundaries. With that
unique pair established, only those two delimiters and the pair's contained
`PlainText`/`Newline` payload fragments are replaced by one whole-form
`RecoveredVerbatim` fact. Typed child constructs and additional recovery facts
remain independent evidence. Ambiguous duplicate boundary facts decline
reconciliation and therefore remain available to the ledger's duplicate
fail-stop.

This is the smallest identity required by the current two-stage composition:
the sanitizer edit is the source-form identity, and the unique delimiter pair
is its classifier-side correlation witness. No global deduplication and no
containment-only erasure is performed.

## Live characterizations

The pending AAT module contains tests that execute the real decode/sanitize,
parse, correlation, and decoded-coordinate projection chain for:

- a plain accent rewrite;
- a rewrite split by a newline;
- a rewrite containing a recognized ruby child;
- an unrelated recovery candidate inside the same edit interval;
- a literal tortoise form with no rewrite identity; and
- an independently duplicated boundary fact, which remains ambiguous rather
  than being laundered into one claim.

The unrelated inner recovery survives. Because the current coarse `MapEdit`
rebases both it and the whole-form recovery to the same decoded interval, the
closed ledger correctly rejects that ambiguity as a duplicate instead of
silently deleting evidence.

## Task 7 resume contract

The uncommitted AAT capture implementation must retain the following call
order in `build_ledger`:

```rust
let output = lex(&decoded.span_text);
let facts = reconcile_accent_edit_facts(output.classified_source_facts, decoded);
let entries = facts.into_iter().filter_map(|fact| fact_entry(fact, decoded, &parser));
```

Do not move this reconciliation back into `ab-aozora-pipeline`: the first
sanitizer's edit identity does not exist there on the live pre-sanitized parse.
Do not weaken the unique delimiter-pair requirement, suppress typed or inner
recovery facts, or deduplicate after decoded-coordinate projection. Task 7 may
commit the AAT implementation with the rest of its coherent capture boundary.

## Verification

- `cargo test -p ab-aozora-pipeline`: passed (145 unit tests plus all
  integration/property/doc suites).
- `cargo test -p ab-aozora-aat --lib`: passed (69 tests, including all five
  live composition characterizations).
- `cargo clippy -p ab-aozora-pipeline --lib -- -D warnings`: passed.
- `cargo fmt --all -- --check`: passed after formatting.
- `scripts/comment-hygiene-check.sh`: passed.

`cargo clippy -p ab-aozora-pipeline --all-targets -- -D warnings` still stops
on the two pre-existing Task 5 test-target findings in `plain_provenance.rs`
(`items_after_statements` and `usize` to `u32`). This fix neither introduced
nor modified those lines.
