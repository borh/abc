# Adapter preservation: live verification vs the static coverage matrix (2026-07-08)

## Summary

Running the AAT adapters **live** against the `ab-oracle` fixtures shows the
per-adapter `aat_fidelity` cells in `data/aozora-syntax-coverage.toml` are
substantially **out of date**. An earlier gap ledger built from the static
matrix reported ~14 "typed but unpreserved" constructs as a parser target;
live verification overturns that:

- **45 / 56 constructs are handled (oracle-pass) by ≥ 1 native adapter**
  (aozora2 and/or aozora-rs).
- **0 corpus-occurring constructs lack a handling adapter.** The 11 constructs
  with no oracle case are all **0-occurrence** theoretical rows
  (`structure.table`, `layout.multicolumn`, `source.*`, …).
- The matrix marks many of these constructs `dropped`/`lossy`/`not_applicable`
  where the adapter in fact emits a faithful typed node — e.g. `gaiji.jis_code`
  is marked `lossy` for aozora2, but aozora2 emits the fully-resolved
  `{gaiji, resolved:撑, jis_code:2-13-47}` that matches the oracle exactly.

## The staleness, read through the schema coupling

`ab-coverage` couples `aat_fidelity` to the parser `recognition`
(`crates/ab-coverage/src/schema.rs`): `unrecognised ⇒ synthesised|not_applicable`,
`parsed ⇒ not synthesised`, `aborts ⇒ not_applicable`. Read through that lens,
the cells contradicted by a live faithful node break down as:

| matrix cell | parser recognition | count | correct value (coupling-consistent) |
|---|---|--:|---|
| dropped | parsed | 32 | preserved |
| dropped | normalised | 3 | preserved |
| lossy | parsed | 13 | preserved |
| lossy | normalised | 2 | preserved |
| not_applicable | normalised | 2 | preserved |
| not_applicable | unrecognised | 23 | synthesised |
| synthesised | unrecognised | 6 | **already correct — not stale** |
| (absent) | — (aozora, untracked) | 6 | out of scope (adapter not in matrix) |

So ~75 cells are genuinely contradicted; 6 `synthesised` cells legitimately
pass by reconstruction and are **not** stale (oracle-pass ≠ preserved).

## Why the matrix is NOT rewritten in this change

`dropped` is factually falsified by a live faithful node, but promoting the
contradicted cells to `preserved` would over-correct on an axis the oracle
cannot settle: the oracle passes a **single canonical fixture** per construct,
which does not establish full-corpus fidelity (`preserved`) vs degradation on
harder real-world instances (`lossy`). The human curation may reflect corpus
behavior, not error. **The matrix regeneration should therefore be driven by a
full-corpus fidelity pass** (adapters over real works, not fixtures), which is
the tracked follow-up. This report records the contradiction; it does not
guess the corrected values.

## Method / reproduction

```
AB_AOZORA_BIN=$(nix build --no-link --print-out-paths .#upstream-parser-aozora)/bin/aozora \
cargo run --manifest-path crates/ab-oracle/Cargo.toml -- \
  --oracle data/aat-oracle-cases.toml --upstream data/aat-upstream-observations.toml \
  --adapter aozora2=adapters/aozora2/target/release/aozora2-adapter \
  --adapter aozora-rs=adapters/aozora-rs/target/release/aozora-rs-adapter \
  --adapter aozora=adapters/aozora/target/release/aozora-adapter \
  --report-json <out>/oracle-report.json
python reports/aat-fidelity/build-adapter-preservation-ledger.py <out>
```

`aozora2html` (Ruby gem) and `aozora-epub3` (Java; requires `--source`, no
stdin path) were not run; aozora2 + aozora-rs already handle every covered
construct, so adding them can only strengthen the result.

Data: `2026-07-08-adapter-preservation-live-verification.summary.json`,
`2026-07-08-adapter-preservation-live-oracle-report.json`.
