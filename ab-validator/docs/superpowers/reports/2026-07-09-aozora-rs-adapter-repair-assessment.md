# aozora-rs adapter repair — assessment (§4)

**Date:** 2026-07-09
**Item:** §4 of `2026-07-09-parser-comparison-followups-handoff.md` — repair the
`adapters/aozora-rs` typed AAT projection (fallback-dominant) + the `retokenized` gaiji
blind spot. Memory: `aozora-rs-adapter-repair-deferred`.
**Verdict of this assessment: keep DEFERRED.** The condition that makes it matter
("aozora-rs becomes a serious throughput-first candidate") is not met — the study
recommends `aozora-pipeline`, and aozora-rs is "a candidate *only* if raw throughput
dominates." This note records the concrete state so the repair can start cold if that
changes.

## Measured current state

- **The adapter builds** — `cargo build --manifest-path adapters/aozora-rs/Cargo.toml
  --release` exits 0 and the binary produces AAT. **The README is stale**: it declares
  "Status: not building (2026-07-03)" with "three `E0425` errors at
  `src/aat.rs:464,642,753`" — those no longer occur (the `ab-ir` drift it cites has
  since been resolved). *Recommend correcting the README status line.*
- **It is fallback-dominant, quantified.** On a 40-work random sample (seed 7):
  - **32/40 works (80%)** are fallback-dominant (>50% of nodes `x-provenance:
    source_fallback`).
  - At node level: **`source_fallback` 24,489 vs `parser_normalized` 815 — ~97% of
    emitted nodes are source-lexer fallback, ~3% are aozora-rs-core's typed parse.**

  So the production adapter today reflects the *source lexer*, not aozora-rs-core. This
  is why the study measured aozora-rs-core's real capability via the bypass path
  (`--mode retokenized` + `measure-aozora-rs-core.py`), not the adapter.

## The repair (confirmed code sites)

1. **`retokenized_to_aat_blocks` (`src/aat.rs:234`)** drops content:
   - `Retokenized::Kunten(_) | Retokenized::Okurigana(_) => {}` (`aat.rs:241`) — no-op,
     so kunten/okurigana visible text is lost from the projection.
   - block-level `Deco` (Indent / Hanging / Grounded / LowFlying / HorizontalLayout /
     VHCentre) is under-mapped, so its content doesn't round-trip.
2. **Fallback gate (`src/lib.rs:227–232`)** uses the typed projection only when
   `projection.in_source_order` **and** `source_visible_chars == projected_visible_chars`.
   Because (1) drops visible characters, that equality fails on most works → the gate
   trips `FallbackReason::ProjectionMismatch` → the whole work falls back to the lexer.
   The fix is: map (1) so visible text round-trips, then the existing gate passes on
   parser-engaged works (accept a residue of genuine gaps, e.g. `align_end_container`
   → retokenized 0).
3. **`retokenized` gaiji blind spot** (study §5 threat #4): the retokenized token stream
   omits gaiji (aozora-rs resolves gaiji in a separate layer), so even a fixed
   projection under-counts gaiji until that layer is wired into the projection —
   the deeper part of the repair.

## Effort / risk

Substantive Rust engineering under a round-trip-correctness constraint, plus keeping the
adapter's existing tests green (66 unit + 1 golden, per the merge record). The gaiji
blind spot likely needs the separate gaiji layer threaded into the projection, not just
a mapping tweak. Estimate ~1 focused session with real regression risk — disproportionate
to a parser the verdict does not select.

## Recommendation

- **Keep deferred.** Revisit only if aozora-rs is reconsidered on throughput grounds
  (its one distinguishing strength, §4.6: ~90× faster than aozora-core).
- **Do now (cheap, safe, independent of the repair):** fix the stale README status line
  so it no longer claims the adapter fails to build.
- If/when undertaken, do it in a worktree; start from the two code sites above; validate
  by re-measuring the fallback ratio (target: parser_normalized-dominant on
  parser-engaged works) and by the adapter's golden test.
