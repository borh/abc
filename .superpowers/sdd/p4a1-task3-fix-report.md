# P4A1 Task 3 fix report

## Outcome

Fixed both Important findings from `p4a1-task3-review.md`.

- `splice_plain_around` now removes queued plain coverage through the emitted
  decoration's end. If the decoration splits a queued segment, its tail is
  re-seeded at `deco_span.end` with the original provenance.
- The classifier regression checks gap-free, non-overlapping tiling for an
  interior forward decoration and for a mixed `RecoveredVerbatim`/`Text`
  queue. It also pins the surviving mixed-provenance segments.
- A new `ab-aozora-aat` integration test pins serialized diagnostics JSON and
  the relevant serialized AAT JSON projection for the accepted recovery
  matrix. This is the non-cyclic boundary: AAT already depends on facade and
  pipeline, whereas adding facade/AAT beneath pipeline would cycle.

## TDD evidence

The classifier regression was written first. Before the fix:

```text
$ cargo test -p ab-aozora-pipeline --test plain_provenance interior_forward_decoration_does_not_overlap_queued_plain -- --nocapture
overlapping classifier spans for "前に青空の後［＃「青空」に傍点］末尾": [(0, 6, "plain"), (6, 12, "aozora"), (6, 18, "plain"), (18, 48, "aozora"), (48, 54, "plain")]
test result: FAILED. 0 passed; 1 failed
```

After the segment-aware splice fix:

```text
$ cargo test -p ab-aozora-pipeline --test plain_provenance interior_forward_decoration_does_not_overlap_queued_plain -- --nocapture
test result: ok. 1 passed; 0 failed
```

The AAT characterization was also observed failing before its expected JSON
was installed (`capture characterization`, 0 passed; 1 failed), then green
with the byte-exact expectations.

## Fresh verification

All commands exited zero.

```text
$ cargo test -p ab-aozora-aat --test plain_provenance_projection -- --nocapture
test result: ok. 1 passed; 0 failed

$ cargo test -p ab-aozora-pipeline --test plain_provenance -- --nocapture
test result: ok. 3 passed; 0 failed

$ cargo test -p ab-aozora-pipeline
143 unit tests passed; all integration, property, regression, and doc tests passed

$ cargo test -p ab-aozora-pipeline --features classify-instrument --lib
test result: ok. 145 passed; 0 failed

$ cargo test -p ab-aozora-facade
88 unit tests passed; all integration, property, splice, corpus, and doc tests passed

$ cargo test -p ab-aozora-aat
64 unit tests, 5 model tests, 1 golden test, and 1 provenance projection test passed

$ cargo fmt --all -- --check
(no output)

$ git diff --check
(no output)

$ ../scripts/comment-hygiene-check.sh
comment-hygiene: all criteria pass
```

## Concerns

None. The fix is local to queued segment consumption and preserves provenance
on the surviving tail; the JSON characterization lives at the existing
downstream AAT boundary without changing crate dependencies.
