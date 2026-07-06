# Interestingness Ranking for Morphological Analyzer Comparison

**Date:** 2026-07-05  
**Status:** Proposed  
**Scope:** Design for a pattern-level ranking feature over the ab-validator morph warehouse, plus v2 enrichment with Aozora Bunko oracles, lattice cause-classification, and a review feedback ledger.

## Goal

When comparing N Japanese morphological analyzers over the full Aozora Bunko corpus, the system produces ~1.5M disagreement regions and ~500K recurring patterns. The user cannot triage this volume manually. The goal is a ranked view — "which disagreements should I look at first?" — with transparent scoring, graceful degradation across warehouse versions, and a feedback loop so human review effort accumulates rather than evaporating.

## Scope

This design covers:

- **MVP v1:** A `summarize-warehouse-interesting` CLI command that reads existing Parquet warehouse tables (no schema migration), computes per-pattern Reciprocal Rank Fusion scores over 4 signals, and emits deterministic ranked output with explainability.
- **MVP v2:** Additive sidecar tables (`projection_spans`, `nway_region_oracle_evidence`, `nway_region_causes`, `boundary_contexts`, `boundary_consensus`, `aozora_works`, `review_events`) that enrich ranking with oracle signals, cause classification, boundary entropy, era stratification, and a review feedback ledger. No existing table column is modified.
- **Aozora oracles:** Ruby annotations as zero-annotation-cost reading-oracle evidence; paired 旧字旧仮名/新字新仮名 editions as metamorphic tests; consensus silver standard as pseudo-gold.
- **Lattice cause-classification:** For each disagreement region, classify whether it's a coverage gap (missing dictionary entry), a ranking difference (cost/weight tuning), a normalization difference, or a granularity-policy difference.

This design does **not** cover: per-region concordance ranking (beyond the anomaly channel), multi-field feature deltas (Gap 3, deferred), full AAT structural-context ranking (Gap 7, deferred), automatic weight tuning, LLM-assisted explanations, or cross-run pattern trajectory tracking (designed, post-MVP).

## Architecture

The system has three layers:

1. **Analysis pass** (`ab-morph-run analyze-aat`): Runs analyzers over AAT plaintext projections, writes Parquet fact tables to the warehouse. In v2, also generates sidecar tables (oracle evidence, cause classification, projection spans, boundary consensus/context) during this pass, where analyzer lattice access is available.

2. **Summarization** (`ab-morph-run summarize-warehouse-interesting`): Reads warehouse tables (v1: existing facts; v2: optional sidecars), computes per-signal ranks, fuses with normalized RRF, emits ranked output. Degrades gracefully when sidecar tables are absent.

3. **Review** (human-in-the-loop, assisted by the review ledger): Inspects ranked patterns, records verdicts in `review_events.parquet`. Suppressed patterns are excluded from future rankings.

The v1/v2 boundary is enforced by a **sidecar-only** rule: v2 adds new tables that join with v1 tables on shared keys. No existing Parquet column is modified, preserving backward compatibility for all v1 readers.

### Source Code References

The morph comparison pipeline spans these crates and files. References are **symbol-anchored** rather than line-numbered: line numbers in a design doc rot fast and mislead reviewers, so look up the named symbol in the current source. (If a symbol has moved, `rg '<symbol>'` finds it.)

| Component | Crate | Symbols to look up |
|---|---|---|
| Core data model | `ab-morph-diff` | `model.rs` — `Morpheme`, `Analysis`, `Region`, `FeatureDiff`, `Comparison`, `NwayComparison`, `NwayRegion`, `NwayStats`; `nway.rs` — `visit_nway_regions*`, `NwayStatsAccumulator` (shared region alignment, feature group construction) |
| Feature comparison | `ab-morph-diff` | `features.rs` — `compare_feature_diffs`, `visit_feature_pairs` |
| Stats | `ab-morph-diff` | `stats.rs` — `derive_stats`, `derive_boundary_metrics`, `char_span_is_whitespace_only` |
| Validation | `ab-morph-diff` | `validate.rs` — `validate_analysis`, `validate_analysis_against_source` |
| Streaming/compact | `ab-morph-diff` | `streaming.rs` — `CompactAccumulator`, `compare_pair_compact_with_source_text` |
| Char map | `ab-morph-diff` | `char_map.rs` — `CharByteMap` (O(1) char↔byte offset) |
| Error types | `ab-morph-diff` | `error.rs` — `MorphDiffError` variants |
| Public API | `ab-morph-diff` | `lib.rs` — `compare_pair`, `compare_nway_with_source_text`, `visit_nway_regions_with_source_text` |
| Analyzer adapters | `ab-morph-analyzers` | `lib.rs` — `MorphAnalyzer` trait; `vibrato.rs` — `VibratoAnalyzer` (UniDic); `sudachi.rs` — `SudachiAnalyzer`, `SudachiMode::{A,B,C}`; `vaporetto.rs` — `VaporettoAnalyzer`; `features.rs` (feature parsing); `span_builder.rs` (RawToken → Morpheme) |
| Plaintext projection | `ab-plaintext` | `aat.rs` — `visible_text_projection` (strips structural context; the gap for v2 `projection_spans`), `from_aat_value`; `aozora.rs` (honbun extraction); `lib.rs` — `PlainTextDocument` |
| Warehouse schema | `ab-warehouse` | `schema.rs` — `SCHEMA_VERSION`, `WarehouseTable` enum, row structs `NwayRegionRow` (carries `char_start`, `char_end`, `has_coverage_mismatch`), `NwayFeatureDiffRow` (carries `feature_key`), `FeaturePatternCountRow` (carries `source_count`), `ErrorRow`; `writer.rs` — `WarehouseWriter`, two-phase commit; `sql.rs` — DuckDB view generation (asserts "Readers must reject runs.schema_version values other than 1" — see §v2 schema bump) |
| Pipeline orchestration | `ab-morph-run` | `pipeline.rs` — `run_analyze_aat` (warehouse mode, parallel execution with shard merging); `main.rs` — CLI subcommands (`analyze-aat`, `summarize-warehouse-*`); `options.rs` — `WarehouseProfile`, `OutputProfile`, analyzer spec parsing |
| Warehouse fact rows | `ab-morph-run` | `warehouse/rows.rs` — `source_row`, `analysis_row`, `morpheme_rows`, `nway_fact_rows`, `push_region_rows` (where `NwayRegion` is mapped to Parquet rows) |
| Summarization | `ab-morph-run` | `summary/summary_body.rs` — `summarize_warehouse_nway`, `summarize_warehouse_pairwise`, `summarize_warehouse_nway_patterns`, `materialize_warehouse_core_feature_pattern_counts`; `summary/types.rs` — `NwaySummarySort`, `WarehousePairwiseSummaryOptions`, `WarehousePatternOptions`; `summary/warehouse.rs` — public re-exports |
| Compact/N-way output | `ab-morph-run` | `compact.rs`, `nway.rs` — `ComparisonSummaryRow`, `NwayComparisonRow`, `NwayPatternCountRow` |
| Diff utilities | `ab-diff-utils` | `lib.rs` — `first_diff`, frequency tables, hashing |
| AAT schema | `ab-ir` | `aat_view.rs` — `AatDocument`, selectors; `semantic_summary.rs` — `SemanticSummary`, feature counting |
| AAT-to-Parser-IR | `ab-aat-to-parser-ir` | `convert.rs`, `structural_probe.rs` (structural information that could intersect with morph diff) |
| Schema definitions | Data files | `data/aat-schema.json` (AAT v1 block/inline types), `data/aat-to-parser-ir-mapping-v1.json` (IR conversion mapping) |
| Spec reference | Documentation | `docs/morpheme-diff-algorithm-spec.md` (full algorithm spec, especially §6.10 Interestingness Ranking, §7 Aggregation Model, §10 Recommendation), `docs/morph-corpus-workflow.md` (canonical warehouse workflow) |
| Build/config | Root | `Cargo.toml` (workspace with 14 crates, vibrato-rkyv and sudachi dependencies), `justfile` (morph-warehouse-* recipes, dictionary builds), `flake.nix` (Nix development environment with Sudachi dictionary) |
| Sibling repo: Aozora catalog & parser-IR boundary | `../abc` | See §Cross-Repo Dependency on `abc` below — ABC owns `work_id`, `orthographic_style`, person records, and the parser-IR divergence bundle |

## Cross-Repo Dependency on `abc` (Aozora Bunko corpus Converter)

The sibling repository `../abc` is the authoritative producer of two things this design consumes:

1. **Work metadata & era stratification.** ABC's `schemas/metadata-record.schema.json` is the canonical, RFC-8785-JCS content-hashed record for one Aozora work. It already models everything `aozora_works.parquet` needs — *as identity-bearing fields* with provenance — so this design treats `aozora_works` as an **imported projection**, not a new schema. Field mapping:

   | `aozora_works` column (ab-validator) | `metadata-record` source field (abc) | Notes |
   |---|---|---|
   | `work_id` | `work.work_id` (`^[0-9]{6}$`) | Same fact; ABC enforces the regex, we re-validate on import. |
   | `title` | `work.title` | Verbatim. |
   | `author_name` | (derived) `contributors[role=著者] → person-record.person_id` | We store the **person_id**, not a free-text name, so reviewer "森鴎外" / "森鴎外" reconciliation is impossible to lose. The person-record registry (with split/merge drift tracking) lives in ABC and is not duplicated here. |
   | `publication_year` | `work.first_published` or `work.source_editions[].first_edition_year` | Prefer `first_published`; fall back to the first `first_edition_year` across `source_editions`. |
   | `orthographic_style` | `work.orthographic_style` | **Name aligned to ABC** (not `orthography_type` — that earlier draft name diverged and is dropped). Takes the enum `[新字新仮名, 新字旧仮名, 旧字新仮名, 旧字旧仮名, その他]` verbatim. |
   | `genre` | — (not in `metadata-record`) | Nullable; populated opportunistically from other ABC exports when available, otherwise null. |
   | `metadata_record_schema_hash` | `metadata_record_schema_hash` | Stored verbatim (with `metadata_record_retrieved_at` alongside). The earlier `catalog_version` draft name is dropped; `source_csv_provenance` is stripped from `out/corpus` exports and is not imported. |

   ABC field names (`orthographic_style`, `work_id`) and enum values are adopted verbatim. We do **not** invent parallel names. The `aozora_works` rows are non-authoritative projections: re-derivable from ABC inputs; if ABC changes, the projection is regenerated.

2. **Parser-IR divergence bundle.** ABC's `schemas/aat-parser-ir-divergence-bundle.schema.json` is the actual artifact the post-MVP morph⇄parser join targets (see §Cross-Subsystem Join: Morph ⇄ Parser, scoped to the bundle's *real* shape, not a hypothetical per-span parser-error table).

**Boundary rules** (cf. `abc/docs/v0-design-bundle/ab-validator-boundary.md`):

- `work_id` is a stable join key both repos already use (`ab-aat-to-parser-ir/tests/integration.rs` keys on `work_id` "1567", "236"; ABC derives it from 作品ID as `w<図書カードID>`). The shared key is a fact, not a new contract.
- `orthographic_style` enum values must match ABC exactly. Any change on either side is a coordinated release.
- `person_id` and person-record drift are owned by ABC. ab-validator does **not** store `author_name` as a free string in any identity-bearing role. Display strings may be computed from the latest ABC person registry at query time, but verdict identity is `person_id`-keyed.
- The `aozora_works` projection is regenerated by `ab-morph-run import-aozora-metadata --run-dir <run> --from <abc export dir>`, where the export dir is ABC's `out/corpus` layout (`works/<work_id>.json`); it is **never hand-edited** in the warehouse. Producer design: `docs/superpowers/specs/2026-07-06-aozora-works-import-design.md`. A run that ingested a non-Aozora corpus and lacks the projection degrades the era-novelty and rarity dedup signals honestly (see §Error Behavior).

This is the only cross-repo dependency in the design. It is called out here rather than buried in the sidecar table definition so reviewers can check it first.

## MVP v1: Warehouse-Compatible Ranking (No Schema Migration)

### Contract

`ab-morph-run summarize-warehouse-interesting` runs on **v1 warehouses without schema migration**. It reads only existing tables:

- `nway_regions`, `nway_region_analyzers`, `nway_feature_diffs`
- `feature_pattern_counts` (if materialized; otherwise computed from raw diffs)
- `sources`, `run_analyzers`

### Scoring: Reciprocal Rank Fusion with Explicit Missing-Data Handling

For each pattern `p` and each **applicable** signal `s`:

1. Compute raw signal value `v_s(p)`.
2. Rank all patterns *of the same kind* by `v_s(p)` descending → `rank_s(p)` (1 = best). Within-kind ranking prevents impact signals from structurally advantaging feature patterns over segmentation patterns. (Open Question 4 — see §Calibration Plan: an A/B against global RRF will confirm or refute that within-kind ranking is empirically worth the comparability constraint.)
3. Fuse:

```
RRF(p) = Σ_s [ applicable_s(p) ? 1/(60 + rank_s(p)) : λ_missing ]  /  |S_applicable(p)|
```

- `applicable_s(p)` is true iff signal `s` is applicable to pattern `p`'s *kind* (e.g., `impact` is not applicable to segmentation patterns). Applicability is a property of the kind, not the data — it does not change per pattern.
- `|S_applicable(p)|` is the count of applicable signals for `p`'s kind — a constant across patterns of the same kind.
- `λ_missing` is the term contributed by a signal that is **applicable to the kind but whose data is missing for this pattern** (e.g., v2 oracle/cause-classification sidecars absent in a v1 run, or a feature pattern with `feature_key = null` so `impact` cannot fire). `λ_missing` is a small positive constant (default `1/(60 + N+1)` where `N` is the kind's pattern count — effectively "ranked just below the worst-ranked observed pattern"). Default `λ_missing = 0.005` is a calibration knob recorded in the `score_version` block.

**Why this form and not bare `1/|S| × Σ 1/(60+rank)`:** bare per-pattern normalization by `|S_applicable|` produces a counter-intuitive inversion — a pattern that *drops* a data-available signal (because the sidecar is absent) sees its top-rank score *rise* by ~34% (from `1/4 × 1/61 ≈ 0.00410` to `1/3 × 1/61 ≈ 0.00546`) since the divisor shrinks. The explicit `λ_missing` term preserves the intuition "more data = more confident = higher score": missing data contributes a small floor instead of resetting the normalization. The divisor `|S_applicable|` stays constant per kind (it counts applicability, which is kind-level), so missing data no longer boosts the normalized score.

**Signals not applicable to a pattern kind** (e.g., impact for segmentation patterns) are omitted entirely — they are neither ranked nor counted in `|S_applicable|`, and they contribute no `λ_missing` term. This is distinct from a signal that is applicable but has missing data for a specific pattern, which contributes `λ_missing`.

**Property-based invariant (corrected):** the §Test Plan "Missing-signal consistency" invariant is rewritten as: *removing signal s from a pattern's data changes only the s-contribution (turning `1/(60+rank_s)` into `λ_missing`); per-signal ranks of other signals are unchanged; the fused score changes monotonically downward by a bounded, documented amount.* The previous draft's "other ranks unchanged" was correct for per-signal ranks but the property did *not* hold for the fused score under bare normalization — that bug is fixed by `λ_missing`.

**Calibration:** before locking v1 defaults, the §Calibration Plan sweeps `λ_missing ∈ {0, 0.005, 0.010}` and confirms top-50 ordering is not artifactually reshuffled by missing data; if the sweep is unstable, revisit signal definitions.

### Per-Signal Definitions (v1)

The four v1 signals. **Source-table column names refer to the current symbol** (see §Source Code References); verify against the live struct rather than a line number.

| Signal | Raw Value | Applicable To | Source |
|---|---|---|---|
| **Coverage** | `log2(1 + count of regions where has_coverage_mismatch = true)` | All kinds | `nway_regions.has_coverage_mismatch` (`NwayRegionRow`) |
| **Rarity** | IDF: `log((total_work_count + 1) / (work_count + 1))` | All kinds | `feature_pattern_counts.source_count` for feature patterns (`FeaturePatternCountRow`); on-the-fly aggregation over `nway_regions` for segmentation/coverage kinds (see "Per-kind materialization" below). `total_work_count` = distinct `work_id` from `aozora_works` (joined via `sources.source_id → aozora_works.source_id`) plus the count of run sources absent from `aozora_works`, which coalesce to per-source rarity keys. Falls back to distinct `sources` count when `aozora_works` is absent — the run then carries `rarity_basis = "source"` in its `signal_profile`, marking its scores non-comparable to work-based runs. |
| **Impact** | Feature-key-level lookup table: POS1=4.0, lemma=3.5, conj-type=2.5, conj-form=2.0, reading=1.5, other=1.0 | Feature patterns only | `nway_feature_diffs.feature_key` (`NwayFeatureDiffRow`); pattern materialization in `materialize_warehouse_core_feature_pattern_counts` |
| **Span** | 90th percentile `(char_end - char_start)` across pattern regions | All kinds | `nway_regions.char_start`, `nway_regions.char_end` (`NwayRegionRow`) |

**Rarity counting is work-based, not file-based.** An earlier draft conflated the column name `source_count` (inherited from feature-pattern materialization, which counts source files) with the semantic value. The IDF denominator is **distinct `work_id`** (the Aozora card id, imported through `aozora_works`), so a pattern appearing in multiple files of one card counts once — measured 2026-07-06: 287 multi-file-card sources, 1.6% of the corpus, a 2× inflation *per affected pattern*. Paired 旧字/新字 editions are separate cards = separate work_ids and **deliberately count as distinct works** (owner decision 2026-07-06: they are distinct analysis targets that may pair better with different tokenizers, and they feed the paired-edition metamorphic tests; any future merge is a TEI-edition/token-variant concern, and an edition-cluster identity would arrive as a new `rarity_basis` value, never silently under `"work"`). When `aozora_works` is absent, the ranker falls back to `sources` count and records `rarity_basis = "source"` so consumers don't cross-compare scores across different rarity bases.

**Per-kind materialization (Open Question 1, resolved):** only `feature_pattern_counts` is materialized today, and it is feature-keyed. Rarity/coverage/span for `segmentation` and `coverage` kinds are computed at query time by aggregating `nway_regions` (and `nway_region_analyzers` for the analyzer set). These kinds have far fewer patterns (~100K total) and aggregation is a single DuckDB GROUP BY. The v1 contract does **not** require a new materialization step for non-feature kinds; the ranker tracks where each signal's value comes from per kind, and `signal_profile` records the kind.

**Ties in raw signal values** are broken by `pattern_id` lexicographic order (see §Tie-Breaking and Determinism). This is what makes per-signal ranks deterministic.

### Output Contract

Every ranked row carries:

- `pattern_id`: content hash with canonical serialization (see §Pattern Identity below)
- `pattern`: human-readable display string
- `kind`: `feature`, `segmentation`, or `coverage`
- `rrf_score`: normalized RRF value (6 decimal places)
- `signal_profile`: list of active signal names — scores are comparable only within the same profile
- Per-signal ranks and raw values for `--explain` decomposition
- `examples`, `source_count`, `text_count`, `sample_source_ids`, `sample_text_ids`
- `region_examples`: bounded list of `(source_id, text_id, region_index, char_start, char_end)`

### Anomaly Channel

A small top-k of individual regions whose per-region score exceeds a threshold but whose patterns fall below the RRF cutoff. Region score:

```
anomaly_score(r) = has_coverage_mismatch(r) × W_COV + log2(1 + char_length(r))
```

with `W_COV = 5.0`. The magic constant is a **calibration knob**, recorded in the `score_version` block as `anomaly_w_cov`; it is tuned in §Calibration Plan, not chosen by intuition. The `log2(1 + char_length)` term is bounded and monotone in disagreement span; coverage-mismatch is binary so the multiplier sets the relative weight of a coverage finding vs. a long-segmentation disagreement. Simple, deterministic, requires no new data.

**Why a separate score, not lifted into RRF (Open Question 5, resolved):** RRF as defined operates on patterns (recurring, materialized). Regions are ~1.5M raw rows with no materialized "pattern" entity behind them — the anomaly channel exists precisely to surface high-signal regions whose *patterns* did not make the cutoff. Lifting RRF to per-region (a) doubles the ranking surface, (b) would re-rank the same ~1.5M rows the pattern ranker just compressed, and (c) the per-region signals available today (`has_coverage_mismatch`, `char_length`) are too few to justify a 4-signal RRF. The hand-picked linear combination is cheaper and honest. If future per-region signals (oracle resolution at region level, cause-class availability) make per-region RRF worthwhile, the unified form is the natural follow-up; `anomaly_score` becomes one term, with a documented promotion path.

### `--filter lexical-only` Definition

The filter excludes regions/patterns where the entire span is whitespace or punctuation. The predicate is:

- All characters in `[char_start, char_end)` belong to Unicode general categories `P*` (punctuation) or `Z*` (separators), AND
- The region has no analyzer producing a morpheme whose POS major category is non-punctuation.

Implemented by reading `morphemes` joined to the region. This mirrors the existing `char_span_is_whitespace_only` helper in `ab-morph-diff/src/stats.rs`, extended to punctuation categories. Punctuation-only corpora are real (quote anthologies) and inflate pattern counts without insight; the filter defaults to `all` and is a display control, not a ranking input.

### CLI Contract

```text
ab-morph-run summarize-warehouse-interesting
  --run-dir PATH
  --limit 50
  --format table|json
  --filter lexical-only|all
  --explain PATTERN_ID
  --anomalies 10
  --output PATH
```

Mirrors the existing summary command interface (`ab-morph-run/src/main.rs` — see the `summarize-warehouse-*` subcommands). `--format json --output PATH` writes one JSON object with `rows` array. `--format table --output PATH` writes TSV. `--explain` ignores `--limit` and `--anomalies`.

### Pattern Identity (`pattern_id`)

`pattern_id` is a content hash. It is a contract that two engineers implementing the ranker must produce identically byte-for-byte for the same pattern, otherwise verdicts orphan. Therefore the canonicalization is fully specified here and pinned by a golden test before Phase 1 ships.

**Canonical serialization (pseudocode — implement exactly):**

```
fn canonical(p: Pattern) -> String {
    let scope = canonical_scope(p.scope);            // see below
    let pair   = p.analyzer_pair.iter().sorted().join(","); // direction-independent
    let from   = p.from_value.map(nfc_collapse_ws).unwrap_or("");
    let to     = p.to_value  .map(nfc_collapse_ws).unwrap_or("");
    let fkey   = p.feature_key.unwrap_or("");
    format!("pattern-v{VER}:{kind}:{fkey}:{from}:{to}:{scope}:{pair}")
}
pattern_id = "sha256:" + hex(sha256(canonical(p)))
```

- `VER` (currently `1`) is the `pattern_id` schema version, independent of warehouse `SCHEMA_VERSION`. Bump when normalization, separator, field order, or algorithm changes. A bump orphans existing verdicts; the review ledger's fuzzy matching (§review_events) is the safety net.
- `nfc_collapse_ws`: Unicode NFC normalization, then collapse runs of ASCII whitespace to a single U+0020, then trim leading/trailing. Reuses the existing hashing discipline (`ab-warehouse/src/writer.rs` already hashes via RFC-8785 JCS elsewhere; one canonicalization function is added there and reused).
- The value pair `(from, to)` is **not** lexicographically ordered — direction matters semantically (which analyzer produces `from` vs `to` is encoded by `pair` ordering separately). Only the analyzer pair is sorted, so "A says X, B says Y" and "A says Y, B says X" are distinct patterns (they are different findings) but the analyzer-pair signature is stable.
- `scope` is derived **deterministically** from the `(scope_type, scope_position, scope_surface)` tuple stored on `NwayFeatureScopeRow` (see `summary_body.rs` — existing fields). The example `whole_region` is **not** a free string — it is the canonical token printed for the scope whose `scope_type = "whole_region"`; every `(scope_type, scope_position, scope_surface)` maps to exactly one canonical token via a closed table:

  | scope tuple | canonical token |
  |---|---|
  | `("whole_region", None, None)` | `whole_region` |
  | `("position", Some(n), None)` | `pos:{n}` |
  | `("surface", None, Some(s))` | `surf:{nfc_collapse_ws(s)}` |

  Extending this table bumps `VER`.

**Stability rules (restated as invariants, pinned by property-based tests):**

1. `pattern_id(p)` is a bijection over the canonical-serialized form (same canonical string ⇒ same hash; different canonical strings ⇒ different hashes).
2. Reordering the analyzer pair does not change `pattern_id` (sort before hashing).
3. Adding a new scope tuple without extending the table is a hard error in `canonical_scope`, not a silent fallback to `unknown`.
4. NFC normalization is applied identically to `from_value`, `to_value`, and any `surface` value.

**Why the spec is this explicit:** the design's own fuzzy-matching fallback implies `pattern_id` *will* drift across reviewer sessions and dictionary upgrades. If the canonical form is not pinned by tests, two implementers will produce different bytes for the same pattern and the fuzzy matcher becomes the primary path rather than a safety net. The golden test (§Test Plan) catches this before any verdict is written.

## MVP v2: Enriched Ranking (Additive Sidecar Tables)

### Principle

v2 adds new Parquet tables that **join with existing v1 tables via shared keys** (`run_id, source_id, text_id, region_index`). No existing table column is modified. No existing reader is broken. The `SCHEMA_VERSION` in `runs.parquet` is bumped to 2; v2 readers degrade gracefully on v1 runs by omitting signals that depend on absent sidecar tables.

**Note on the existing schema-version assertion:** `ab-warehouse/src/sql.rs` currently asserts `"Readers must reject runs.schema_version values other than 1"`. Bumping to 2 requires relaxing this assertion to "readers must reject `schema_version` values greater than the reader's supported maximum" (the supported maximum becomes a reader constant, currently 2). The bump is itself a v2-sidecar deliverable and is versioned independently of the `pattern_id` schema version above. A v1 reader reading a v2 run sees `schema_version > 1` and hard-errors per the existing contract — this is the documented compatibility gate, not a regression.

### v2 Sidecar Tables

#### `projection_spans.parquet`

Bridges the structural-context gap by recording the mapping from projected-plaintext character offsets back to AAT inline nodes. Generated during the plaintext projection step (`ab-plaintext/src/aat.rs` — `visible_text_projection` currently discards this mapping; the implementation extends it to optionally emit span mappings). This is NOT full structural context (Gap 7); it's a minimal bridge enabling ruby oracle and artifact flagging.

| Column | Type | Purpose |
|---|---|---|
| `run_id`, `source_id`, `text_id` | `Utf8` | Join keys |
| `projected_char_start`, `projected_char_end` | `UInt64` | Span in the plaintext string that morph analysis uses |
| `aat_pointer` | `Utf8` | JSON pointer path into the AAT document |
| `inline_kind` | `Utf8` | `text`, `ruby`, `gaiji`, `accent`, `figure`, `raw`, `style`, `warigaki`, `note` |
| `is_ruby_base` | `Boolean` | True if this span is a ruby base text |
| `is_gaiji` | `Boolean` | True if this span is a gaiji reference |
| `is_note` | `Boolean` | True if editor annotation |

#### `nway_region_oracle_evidence.parquet`

Records oracle resolution per disagreement region. Populated by the ruby oracle (and later paired-edition and gold oracles).

| Column | Type | Purpose |
|---|---|---|
| `run_id`, `source_id`, `text_id` | `Utf8` | Join keys |
| `region_index` | `UInt64` | |
| `oracle_source` | `Utf8` | `ruby`, `paired_edition`, `gold`, `silver_consensus` |
| `winning_analyzer` | `Utf8` (nullable) | Which analyzer's analysis matches the oracle |
| `losing_analyzers` | `List<Utf8>` | Which analyzers disagree with the oracle |
| `evidence_detail` | `Utf8` | JSON with source-specific fields (e.g., ruby base/reading strings) |

#### `nway_region_causes.parquet`

Records the cause classification for each disagreement region. Generated during the analysis pass where analyzer lattice access is available. For Vibrato: `vibrato-rkyv` tokenizer n-best mode; for Sudachi: `sudachi::analysis::stateful_tokenizer::StatefulTokenizer` lattice access.

| Column | Type | Purpose |
|---|---|---|
| `run_id`, `source_id`, `text_id` | `Utf8` | Join keys |
| `region_index` | `UInt64` | |
| `cause_class` | `Utf8` | `coverage-gap`, `ranking-difference`, `normalization-difference`, `policy-difference`, `unknown` |
| `cause_detail` | `Utf8` (nullable) | Analyzer-specific lattice evidence |

Classification logic:
- **Coverage gap:** B's chosen segmentation contains a word absent from A's lattice entirely → actionable dictionary gap.
- **Ranking difference:** B's segmentation exists in A's lattice but scored worse → expected cost/weight tuning.
- **Normalization difference:** Same lattice path, different surface normalization → policy, not error.
- **Policy difference:** Both correct at different granularity (短単位 vs 中単位; Sudachi split modes A/B/C) → not a real disagreement.

#### `aozora_works.parquet`

**Scope:** an *imported projection* of ABC's `metadata-record.schema.json` (see §Cross-Repo Dependency on `abc`). Do not invent fields here; if a needed field is absent in ABC, the change is landed in ABC first and then mirrored.

| Column | Type | Purpose |
|---|---|---|
| `work_id` | `Utf8` | Aozora 作品ID — join key for dedup. Matches ABC `work.work_id` (`^[0-9]{6}$`). |
| `source_id` | `Utf8` | Joins to warehouse `sources.source_id`. One work may have multiple `source_id` rows. |
| `title` | `Utf8` | From ABC `work.title`. |
| `author_person_id` | `Utf8` (nullable) | ABC contributor `person_id` for `relation_to_work = 著者`, **not** a free-text author name. The person registry (with split/merge drift) lives in ABC; ab-validator never stores `author_name` as identity-bearing. |
| `publication_year` | `Int32` (nullable) | Derived: prefer ABC `work.first_published`; fall back to `work.source_editions[0].first_edition_year`. Nullable when neither is present. |
| `orthographic_style` | `Utf8` | ABC enum verbatim: `新字新仮名`, `新字旧仮名`, `旧字新仮名`, `旧字旧仮名`, `その他`. (Earlier draft used `orthography_type`; renamed to match ABC exactly.) |
| `genre` | `Utf8` (nullable) | Not in ABC `metadata-record`; populated opportunistically when future ABC exports supply it, otherwise `null`. |
| `metadata_record_schema_hash` | `Utf8` | ABC content hash of the source record, stored verbatim. Provenance: this row is reproducible from the ABC export identified by this hash. (Earlier draft used a vague `catalog_version`; this is the real, hashable identity.) |
| `metadata_record_retrieved_at` | `Utf8` | ISO 8601 timestamp of the ABC export import. For reproducibility. |

**Rarity counting uses `work_id`, not `source_id`** (see §Per-Signal Definitions / Rarity). Without this, pattern frequencies are inflated ~2× and the rarity signal is corrupted.

#### `boundary_contexts.parquet` and `boundary_consensus.parquet`

Per-boundary records with character context and analyzer vote.

**Literal-context storage policy (Decision 4, scoped):** the rationale "Aozora Bunko is public domain; no privacy constraint" applies **only to Aozora-sourced runs**. Warehouse runs may include non-Aozora corpora (BCCWJ is research-licensed and not freely redistributable; multi-corpus warehouses are an explicit goal). Therefore literal-context storage is gated, not assumed:

- `runs.parquet` carries a `literal_context_policy` field (enum `literal`, `hash-only`). The analysis pass sets it from a corpus-permission precondition (an Aozora import ⇒ `literal`; an unknown or non-Aozora import ⇒ `hash-only`).
- When `hash-only`, `boundary_contexts` stores `char_class_*` columns plus a rolling `source_text_hash` over the (bounded) context window — sufficient for artifact clustering and 踊り字-class analytics — but **not** the literal `char_m1`/`char_p1`. Per-query source lookups are required to display actual characters.
- When `literal` (Aozora runs), the table additionally stores `char_m1`, `char_p1` (±1 literal) for ergonomic display, plus `class_m2`/`class_m1`/`class_p1`/`class_p2` (character classes at radius 2) and `source_text_hash` regardless. Aozora literal storage enables the "show me the 踊り字" display without a source join.

`boundary_contexts`: one row per `(run_id, source_id, text_id, boundary_offset_char)`.

`boundary_consensus`: one row per `(run_id, source_id, text_id, boundary_offset_char, analyzer_id)` with `has_boundary` boolean.

A downstream consumer can tell from `literal_context_policy` whether to expect `char_m1`/`char_p1`; this is the missing boundary between licensing fact and warehouse artifact policy.

#### `review_events.parquet`

Append-only review ledger. Each row is one verdict by one reviewer on one pattern in one run. Current state for a reviewer = latest event per `(pattern_id, reviewer)`. For suppression decisions across reviewers, maintainer-designated verdicts override non-maintainer verdicts.

| Column | Type | Purpose |
|---|---|---|
| `event_id` | `Utf8` | UUID for monotonic ordering |
| `pattern_id` | `Utf8` | Joins to ranked output |
| `run_id` | `Utf8` | Run where verdict was made |
| `reviewer` | `Utf8` | |
| `verdict` | `Utf8` | `expected-policy`, `expected-dictionary`, `bug`, `corpus-artifact`, `noise`, `unclear` |
| `note` | `Utf8` (nullable) | Human-readable explanation |
| `suppression_scope` | `Utf8` (nullable) | JSON: `{"analyzer_pair": "...", "feature_key": "..."}` |
| `timestamp` | `Utf8` | ISO 8601 |

Verdicts double as calibration labels for future learning-to-rank model fitting. `bug` → positive class; `expected-policy` → negative; `unclear` → excluded from training.

**Verdict inheritance across `pattern_id` drift (Q3, resolved):** On `pattern_id` schema evolution (or dictionary upgrade), verdicts from prior runs must still apply. Two designs are weighed; **adopt read-time graph traversal, with write-time cache:**

- *Read-time* is authoritative: at query, the summarizer walks a Jaccard-similarity graph over `(kind, feature_key, value-pair, analyzer-set)` signatures, attaches inherited verdicts with `verdict_inherited = true` and a confidence score, and applies maintainer-override rules. Jaccard ≥ 0.85 inherits; the threshold is a `score_version` field (`inheritance_jaccard_threshold`) and is part of calibration.
- *Write-time cache* is an optimization: a `review_events_inheritance` view (Parquet or DuckDB view over `review_events`) materializes inherited verdicts per current-run `pattern_id` so the `latest event per (pattern_id, reviewer)` query is O(1) at read time. The cache is invalidated on any new verdict in the affected signature neighborhood; invalidation is batched per run.
- Choosing read-time as the source of truth keeps suppression correctness single-path: there is no risk of a stale inheritance write surviving a re-verdict. The cache is purely performance. The 45M-row `boundary_consensus` perf concern has a quieter sibling here that the cache resolves.

**Suppression auto-expiry** is deferred to post-MVP (requires cross-run frequency tracking). In v2, suppression is manual (re-evaluate on each new run).

### v2 Scoring Additions

When v2 sidecar tables are present, additional signals join the RRF:

| Signal | Raw Value | Requires |
|---|---|---|
| **Oracle** | `oracle_resolved_region_count` | `nway_region_oracle_evidence` |
| **Cause novelty** | `1.0` if `cause_class = coverage-gap`, `0.5` if `ranking-difference`, `0.0` otherwise | `nway_region_causes` |
| **Entropy** | 90th percentile Bernoulli entropy: `-p*log2(p) - (1-p)*log2(1-p)` | `boundary_consensus` + `boundary_contexts` |
| **Surprise** | Median `-log2(max(ε, 1 - smoothed_agreement))`; ε=0.001; Laplace smoothing α=0.5 | `analyzer_bias_summary` (see §Dependent Design: Analyzer Bias Summary, with a fully-specified contract below) |
| **Era novelty** | Lift of pattern frequency within an `orthographic_style` stratum vs. global baseline | `aozora_works`. Stratum key is `aozora_works.orthographic_style` (ABC's name, see §Cross-Repo Dependency). |

**Surprise signal lives behind a real contract, not a vague interface.** Earlier drafts hand-waved "a `pairwise_agreement_rate` value per analyzer pair, queryable by the summarizer" while the symbol appears nowhere in the codebase and no producer exists. That is the fake-seam failure mode: the signal ships, the producer is never built, the signal silently never activates. The bias summary is therefore fully specified as a sidecar table here so the contract is honest (§Dependent Design: Analyzer Bias Summary). If the producer is not finished before v2 ships, the surprise signal is excluded from the v2 RRF and `signal_profile` records `surprise: absent` — the degradation is observable, not silent.

### Granularity Harmonization (Preprocessing)

Before comparison, Sudachi is run in the mode matching UniDic short units (or all modes are compared simultaneously). A disagreement persisting at every granularity level is real; one vanishing at mode C is policy. This preprocessing step (`ab-morph-analyzers/src/sudachi.rs` — `SudachiMode` enum controls split level) runs during the analysis pass and reduces policy-noise disagreements before ranking.

**Cross-version scoring note (F1):** harmonization changes *which disagreements exist*, so v1-without-harmonization and v2-with-harmonization produce different RRF scores on the same patterns even with identical signal sets. The `signal_profile` field does **not** catch this (same signals, different underlying data). To preserve cross-version comparability, the `score_version` block carries `granularity_profile` (e.g., `"suw+luw"` for a mixed-granularity comparison, `"suw"` after harmonizing to a single class). Consumers comparing two runs MUST check `granularity_profile` matches, same as `signal_profile`. The cleanest path is to land granularity harmonization in **Phase 1**, before any v1 score is locked — see §Implementation Phases, where harmonization is moved to the v1 boundary accordingly.

## Aozora Oracles

### Ruby Oracle

Aozora ruby annotations (`東京《とうきょう》`) provide editor-sanctioned readings over known character spans. This is ground truth with **zero annotation cost** (the machinery — kana normalization, old-kana folding, span mapping — is implementation work).

**Empirical motivation (cross-repo):** ABC's full-corpus probe (see `../abc/docs/adr/0024-parser-ir-span-and-ruby-direction.md`) measured `ruby.direction` LOSS as the single largest AAT-divergence category — ~1.76M occurrences across the corpus. Ruby is where divergences cluster; this oracle is therefore the highest-leverage v2 enrichment, not a speculative signal.

**Mechanism:** Requires `projection_spans.parquet` to map projected-text offsets to AAT ruby base spans. For each ruby span, concatenate each analyzer's reading output over the covered morphemes, normalize kana, compare against the ruby reading string. Per region: record `winning_analyzer` and `losing_analyzers` in `nway_region_oracle_evidence.parquet`.

**Artistic ruby exclusion:** Spans where no analyzer matches the ruby reading are flagged as `nonstandard_ruby` (e.g., 本気《マジ》-style artistic readings). These are excluded from oracle resolution but retained for manual review.

**Coverage:** ~30-60% of Aozora corpus text is ruby-annotated, varying by era (pre-war texts use extensive ruby).

### Paired-Edition Metamorphic Tests

Many Aozora works exist in both 旧字旧仮名 and 新字新仮名 editions under the same 作品ID. The metamorphic property: morphological analysis (lemma sequence, POS sequence) should be approximately stable under orthography modernization. **This is a hypothesis, not a guarantee** — modernization can legitimately alter inflection forms, punctuation, and editorial corrections.

**Mechanism:** Separate CLI command: `ab-morph-run evaluate-metamorphic --paired-editions MANIFEST.json`. Runs analyzers on both editions, aligns by character-offset mapping, reports lemma/POS sequence differences as **suspect violations** for review, not absolute errors.

### Consensus Silver Standard

Where all N analyzers agree exactly on segmentation + POS + lemma, treat as pseudo-gold. High precision by construction. Caveat: consensus can be collectively wrong where analyzers share a dictionary ancestor — the ruby and paired-edition oracles detect this failure mode.

### Gold Annotation Strategy (When Needed)

Select sentences by **set-cover over top-ranked pattern_ids**: greedily pick sentences covering the most distinct high-ranked patterns not yet covered. 100 sentences chosen this way adjudicate far more patterns than 100 random sentences. Annotate to UniDic 短単位 standard; store as `gold_morphemes.parquet` with the same span schema as analyzer morphemes (`ab-morph-diff` `Morpheme` / `model.rs`). This is a separate dependent design (see §Dependent Designs: Gold Morphemes) called out here because the span-schema alignment is the load-bearing constraint the design must respect.

## Implementation Phases

### Phase 1: v1 RRF Ranker

- New CLI subcommand in `ab-morph-run/src/main.rs`
- New summarization logic in `ab-morph-run/src/summary/` (following patterns in `summary_body.rs`)
- Reads existing v1 tables only; no schema migration
- RRF computation: per-signal ranking within-kind, signal-profile tracking (**fusion policy per §Scoring: Normalized RRF Fusion**, including the missing-data behavior pinned by the property-based test)
- `--explain` decomposition mode
- Anomaly channel with simple deterministic per-region score (W_COV defaults to 5.0; tuned in §Calibration Plan)
- `--filter lexical-only` defined as a Unicode GC predicate over `char_start..char_end` (§`--filter lexical-only` Definition)
- `pattern_id` canonicalization function added to `ab-warehouse` (reuses the existing JCS hashing discipline) with a golden test covering bijection, analyzer-pair sorting, and NFC normalization
- Granularity harmonization *tagging* (not the harmonization itself): the run records `granularity_profile = "none"` in the `score_version` block so v1-without-harmonization scores are honestly labeled *(historical: Phase 2 replaced the fixed tag with a derived granularity-class composition — see the Phase 2 status note)*
- Golden tests for output ordering and JSON shape

### Phase 2: Granularity Harmonization (within-v1, scores stabilizing)

Moved here from its previous "Phase 2 after v1" position because harmonization changes *which disagreements exist* and therefore changes RRF scores even with the same signal set. To keep v1 scores stable across the v1-without→v1-with transition, harmonization must land before any v1 score is locked. If harmonization slips past v1 locking, v1 scores are explicitly labeled with the multi-class `granularity_profile` (e.g. `"suw+luw"`) they were actually computed on, and a later harmonized set carries the single-class `"suw"` — the two are non-comparable and consumers are warned.

- Preprocessing step in the analysis pass (`ab-morph-run/src/pipeline.rs`)
- Sudachi mode-matching configuration
- Stored as run metadata, exposed in `score_version` block as `granularity_profile`

**Status (2026-07-05): implemented**, with two deviations from the letter above:

1. `granularity_profile` is *derived at summarize time* from `run_analyzers.parquet` (`analyzer_family`/`analyzer_arg`), not stored as a new run-metadata column. This keeps `run_analyzers.parquet` the single source of truth, avoids a schema change within v1, and is retroactively correct for every existing warehouse — including ones run with the mixed `sudachi-a sudachi-c` set, which correctly derive to `"suw+luw"`.
2. There is no new pipeline preprocessing step. The Sudachi mode-matching configuration this phase called for already exists as the `sudachi-a` analyzer spec; harmonization is an analyzer-set choice at run time, not a new code path. That choice is now canonicalized as the `morph-warehouse-run-suw` justfile recipe (vibrato + the novel-register vibrato dictionary + sudachi-a only, all 短単位/SUW).

**Vocabulary revised 2026-07-05 after owner review:** the original three-token enumeration described just above (one token per harmonization state) was judged very confusing and replaced with the NINJAL granularity-class composition below; no persisted artifact ever used the original tokens.

The derivation (see `granularity_profile_token` in `ab-morph-run/src/summary/interesting.rs`) classifies each `run_analyzers` row into a segmentation granularity class — `suw` (短単位 / Short Unit Word), `muw` (中単位 / Middle Unit Word), or `luw` (長単位 / Long Unit Word) — and joins the deduped classes present, in canonical order `suw < muw < luw`, with `"+"`:

| Analyzers present (by class) | `granularity_profile` |
| --- | --- |
| vibrato (any dictionary) and/or vaporetto only, or Sudachi mode A only | `"suw"` |
| Sudachi mode B only | `"muw"` |
| Sudachi mode C only, or Sudachi mode A + mode C | `"suw+luw"` |
| Sudachi modes A + B + C | `"suw+muw+luw"` |

A single-class profile (`"suw"`) means no granularity-policy noise is possible; a multi-class profile means granularity-policy disagreements are present in the run by design.

### Phase 3: `projection_spans.parquet`

- Extend `ab-plaintext/src/aat.rs` visible_text_projection to optionally emit span mappings
- Write `projection_spans.parquet` during analysis pass
- Schema definition in `ab-warehouse/src/schema.rs`

### Phase 4: v2 Sidecar Tables

- `nway_region_oracle_evidence.parquet`: ruby oracle during analysis pass
- `nway_region_causes.parquet`: lattice interrogation during analysis pass — **with an analyzer asymmetry (§Known Limitations: cause-classification analyzer asymmetry)**. Sudachi `StatefulTokenizer` exposes lattice today; Vibrato `vibrato-rkyv`'s `Lattice`/`LatticeNBest` are `pub(crate)` in the pinned vendor checkout, so the Vibrato path either (a) lands an upstream/fork API change exposing an `Worker::lattice()` accessor, or (b) ships Phase 4 with Vibrato regions' `cause_class = unknown` and degrades the cause-novelty signal accordingly. The choice is recorded in `cause_classification_profile` in the `score_version` block (`sudachi-only` vs `all-analyzers`).
- `boundary_contexts.parquet`, `boundary_consensus.parquet`: boundary data during analysis pass; `literal_context_policy` set per-run (Aozora ⇒ literal; else ⇒ hash-only)
- `aozora_works.parquet`: an *imported projection* of ABC's `metadata-record` (see §Cross-Repo Dependency on `abc`); populated by `ab-morph-run import-aozora-metadata`, not hand-edited
- `analyzer_bias_summary.parquet`: the surprise-signal producer (see §Dependent Design: Analyzer Bias Summary); if not implemented by Phase 5, surprise is excluded from RRF with `signal_profile = surprise: absent`
- Bump `SCHEMA_VERSION` to 2 in `ab-warehouse/src/schema.rs`; relax the `sql.rs` "reject != 1" assertion to "reject > reader maximum" as documented in §MVP v2 Principle

### Phase 5: v2 Enriched RRF

- Extend summarizer to join sidecar tables when present
- Add oracle, cause-novelty, entropy, surprise, era-novelty signals
- Degraded mode when sidecars absent

### Phase 6: Review Ledger

- `review_events.parquet` table + append-only write path
- Verdict → calibration label mapping
- Fuzzy pattern matching for verdict inheritance
- Escalation artifact generation (`verdict = bug` → repro bundle)
- Paired-edition metamorphic test command (`ab-morph-run evaluate-metamorphic`)

### Phase 7: Post-MVP

- Cross-run pattern trajectory tracking (`pattern_trajectory.parquet`)
- Suppression auto-expiry (binomial test on frequency change)
- Learning-to-rank model on accumulated labels
- Cross-subsystem morph ⇄ parser join (`text_quality.parquet`)
- Gold annotation tooling (set-cover selection)
- LLM-assisted explanation drafts (hypothesis generation with provenance)

## Decisions

| # | Decision | Status | Rationale |
|---|---|---|---|
| 1 | Pattern-level ranking for MVP | Proposed | Existing pattern-count infrastructure; anomaly channel catches one-off regions |
| 2 | Sidecar tables, not column modifications | Proposed | Preserves v1 reader compatibility; avoids Parquet struct deserialization risks |
| 3 | `SCHEMA_VERSION` bumped at the first *analysis-pass-produced* sidecar table (Phase 3 `projection_spans`); reader rule then relaxed to "reject > reader max". Post-hoc imported, presence-probed sidecars (`aozora_works`) are version-neutral | Proposed | v2 readers degrade on v1; existing "reject != 1" assertion is generalized, not bypassed; `aozora_works` ships under v1 via the reader's presence probe |
| 4 | Literal boundary context gated by `literal_context_policy` per run, not assumed | Proposed | Aozora is public domain and may store literal chars; BCCWJ/research-licensed corpora are not. Licensing fact is enforced at the run level, not embedded in one table's justification |
| 5 | Oracle and cause-classification in v2, not v1 | Proposed | Requires projection_spans and lattice access not available in v1 warehouses |
| 6 | Within-kind ranking for RRF; missing-signal handling made explicit (see §Scoring) | Proposed | Prevents impact-signal dominance over segmentation patterns; missing-data behavior is unambiguous: an absent applicable signal contributes a `λ_missing` term, not a renormalization-induced boost (see §Scoring) |
| 7 | `projection_spans.parquet` as minimal structural bridge | Proposed | Narrower than full Gap 7; enables oracle/artifact without committing to block-type ranking |
| 8 | Paired-edition tests as separate command | Proposed | Independent of warehouse version; produces suspect violations, not absolute errors |
| 9 | Review ledger append-only with maintainer override; verdict inheritance is read-time graph traversal with write-time cache | Proposed | Audit trail; inter-reviewer disagreement measurable; maintainer resolves suppression conflicts; inheritance single-path at read time avoids stale-write risk |
| 10 | `aozora_works` is an imported projection of ABC's `metadata-record`, not a new schema | Proposed | ABC already models `work_id`, `orthographic_style`, person identity as content-hashed authoritative fields. Re-inventing them with divergent names (`orthography_type`, `author_name`) hard-forks a dimension the codebase has committed to importing |
| 11 | `pattern_id` canonicalization fully specified, golden-tested before Phase 1 verdicts | Proposed | The fuzzy-matching fallback in review_events implies pattern_id *will* drift; if the canonical form is not pinned, fuzzy matching becomes the primary path, not the safety net |
| 12 | Surprise signal either has a real producer-side contract (`analyzer_bias_summary.parquet`) or is excluded from v2 with `signal_profile = surprise: absent` | Proposed | Avoids the fake-seam failure: a half-defined interface that the producer never builds, silently disabling a signal |
| 13 | Granularity harmonization lands within v1 (Phase 2 before score lock) or is tagged `granularity_profile = "none"` | Proposed | Harmonization changes which disagreements exist, so v1-without and v1-with produce non-comparable RRF on the same signals. Either land it before locking or label scores honestly |
| 14 | Morph⇄parser cross-subsystem join scoped to text-level (Spearman + era covariate) for v-post-MVP; region-level join requires a new per-span parser-error sidecar in ABC | Proposed | ABC's `aat-parser-ir-divergence-bundle` is per-work, rule-aggregated — there is no per-char-span parser-error table to range-join against today. Text-level is honest; region-level needs a dependent design |
| 15 | `cause_classification_profile` records analyzer asymmetry (Sudachi-only vs all-analyzers) | Proposed | Vibrato `vibrato-rkyv` lattice is `pub(crate)`; the asymmetry is real and must be visible in the score profile, not hidden behind "available" |

## Open Questions

1. **(Resolved)** v1 scope is RRF over 4 signals with no migration; oracle/cause-classification stays in v2 because they require `projection_spans` and lattice access not available in v1 warehouses. Confirmed by Decision 5.
2. **(Open)** Should `projection_spans.parquet` be generated during `ab-morph-run analyze-aat` (analysis pass) or as a separate preprocessing step? Lean: analysis pass, because ruby-oracle resolution needs the spans at the same time as morph analysis; deferring it adds a second pass over the same AAT.
3. **(Open)** Should suppression auto-expiry be included in v2 (minimal cross-run frequency check) rather than deferred to post-MVP? Default: deferred — cross-run frequency tracking is nontrivial and v2's manual re-evaluation is the documented contract.
4. **(Resolved via calibration)** `pattern_id` stability is now fully specified (§Pattern Identity) with a closed scope-token table and golden bijection test. Cross-run identity is achieved by `pattern_id` schema versioning + read-time fuzzy inheritance. The question of "should it incorporate additional fields" is settled: the canonical form includes `analyzer_pair` and `scope`.
5. **(Resolved)** Anomaly channel keeps the simpler deterministic score; lifting to per-region RRF is a documented future promotion path, not the v1 default (see §Anomaly Channel).
6. **(Open)** Should the era-stratification signal use subgroup-discovery lift or simple within-stratum rank? Default: subgroup-discovery lift (Decision-era pattern), benchmarked in calibration.

## Dependent Designs (Referenced, Not Repeated)

### Analyzer Bias Summary (Gap 2) — fully specified sidecar

A prerequisite for the v2 **surprise** signal. Earlier drafts left this as "tracked separately; only the interface defined here," but the interface itself was undefined — a fake seam. To prevent that failure mode, the producer contract is fully specified here as a sidecar table; if the producer is not implemented by Phase 5, the surprise signal is excluded with `surprise: absent` and the failure is observable rather than silent.

**Sidecar table: `analyzer_bias_summary.parquet`**

| Column | Type | Purpose |
|---|---|---|
| `run_id` | `Utf8` | Join key |
| `analyzer_id` | `Utf8` | One row per analyzer |
| `pair_analyzer_id` | `Utf8` (nullable) | When non-null, this row is a *pair* bias record (see `cardinality`) |
| `cardinality` | `Utf8` | `single` or `pair` |
| `morphemes_per_char` | `Float64` | Average morphemes per character (single only) |
| `split_merge_tendency` | `Float64` | (splits − merges) / total_regions (single only) |
| `one_to_one_region_pct` | `Float64` | Percentage of one-to-one regions (single only) |
| `coverage_error_rate` | `Float64` | Coverage mismatch rate (single only) |
| `pairwise_agreement_rate` | `Float64` | For `pair` rows: fraction of regions where the two analyzers agree exactly on segmentation + POS + lemma. **This is the value the surprise signal consumes.** |
| `total_regions` | `UInt64` | Denominator for rate columns; recorded for confidence intervals |
| `computed_at` | `Utf8` | ISO 8601 |

The surprise signal queries `(run_id, analyzer_id, pair_analyzer_id)` for `pair` rows and reads `pairwise_agreement_rate`. If any pair has no row (the producer did not run for that pair), the surprise contribution for patterns involving that pair is `λ_missing` and `surprise: partial` is recorded in the score block (a third state beyond `present`/`absent`).

**Producer responsibilities:** written by the analysis pass after `nway_regions` is materialized. The producer is a single-pass aggregation over `nway_regions` joined to `nway_region_analyzers`; it does not require analyzer lattice access and is implementable in pure DuckDB SQL. This is why it ships as a v2 sidecar rather than being deferred — there is no upstream blocker.

### Per-Span Parser-Error Sidecar (Dependency for Region-Level Morph ⇄ Parser Join)

Required for the **region-level** morph⇄parser join described in §Cross-Subsystem Join (post-MVP). ABC's current `aat-parser-ir-divergence-bundle` is per-work, rule-aggregated (`category ∈ {LOSS, INVENTION, AMBIGUITY, UNSUPPORTED, STRUCTURAL}`, with only `first_path` pointing at the first occurrence). It does **not** carry per-span `(start, end)` records for every parser error, so the post-MVP "odds ratio of a parser error within ±k characters" cannot be computed against it as a one-shot range join.

This dependent design specifies (in its own doc, not here): a new ABC-side producer that materializes `parser_error_spans.parquet` keyed by `(work_id, source_id, char_start, char_end, rule_id, category)`, populating *all* occurrences (not just `first_path`), at the `decoded_utf8` byte coordinate system (ABC ADR 0024). The morph warehouse's `char_start`/`char_end` are character offsets, so the join additionally requires a coordinate-conversion step (the morph side's `CharByteMap` already provides O(1) char↔byte offsets). Until this sidecar lands, the morph⇄parser join is scoped to **text-level Spearman with era covariate only** (which the existing bundle's per-work `summary` block already supports). The region-level odds-ratio is described in §Cross-Subsystem Join as a post-MVP *plan*, not a v2 deliverable.

### Token/Character Diff Fallbacks (Gap 6)

Explicitly out of scope for this ranking feature. The spec (§4.1, §4.2) designates token-sequence diff and character diff as optional diagnostics, not foundational to the ranking pipeline. They are useful for coverage/normalization validation and may be implemented as separate diagnostic subcommands, but they are not inputs to the interestingness ranker.

### Gold Morphemes (Dependency for LTR)

Select sentences by **set-cover over top-ranked pattern_ids**: greedily pick sentences covering the most distinct high-ranked patterns not yet covered. 100 sentences chosen this way adjudicate far more patterns than 100 random sentences. Annotate to UniDic 短単位 standard; store as `gold_morphemes.parquet` with the same span schema as analyzer morphemes (`ab-morph-diff` `Morpheme` / `model.rs`). This is a separate dependent design tracked in its own doc — called out here because (a) the LTR calibration in §Calibration Plan step 10 implicitly requires it, and (b) its span schema must match the analyzer `Morpheme` exactly for the LTR labels to join cleanly. Not a v1 or v2 deliverable.

## Error Behavior

| Situation | Behavior |
|---|---|
| `--run-dir` missing or not a warehouse run directory | Hard error, exit 1 |
| Run has `schema_version` greater than the reader's supported maximum (v1 reader max = 1; v2 reader max = 2) | Hard error, exit 1. This replaces the prior "reject != 1" rule (§MVP v2 Principle). |
| v1 run, no sidecar tables present | v1 degraded mode: 4 signals; oracle, entropy, surprise, era-novelty excluded; `signal_profile` records the available set; `rarity_basis`, `granularity_profile`, etc. populated honestly |
| v2 run missing some sidecars (e.g., `aozora_works` absent because the corpus isn't Aozora, or `analyzer_bias_summary` absent because the producer didn't run) | Partial degradation: affected signals excluded; `rarity_basis = "source"`, `surprise = "absent"`, etc. recorded in the score block. No hard error — degradation is observable, not fatal. |
| `aozora_works` import out of sync with ABC schema (e.g., ABC renamed a field, importer not updated) | Hard error at import: `import-aozora-metadata` validates against the ABC schema hash; mismatch aborts with a message naming the expected and observed hashes |
| `import-aozora-metadata` maps zero sources to ABC works (wrong export root, empty `works/`) | Hard error at import; no file written. A present-but-empty `aozora_works.parquet` is likewise treated as absent by readers |
| Non-Aozora corpus run with `literal_context_policy = hash-only` forced but `--store-literal-context` flag passed | Hard error: licensing gate refuses; the policy is set from corpus provenance, not the flag |
| Single analyzer in run | Hard error: "ranking requires ≥ 2 analyzers" |
| Missing required `feature_pattern_counts` (not materialized and raw diffs unavailable for feature-kind patterns) | Hard error |
| `--explain PATTERN_ID` with unknown `pattern_id` | Hard error |
| Empty run (zero sources) | No ranked rows, exit 0 |
| `--format json --output PATH` where file exists | Refuse to overwrite; exit 1 unless `--force` |
| `--format table` with `--output PATH` | Write TSV to file |
| `--format table` without `--output` | Write TSV to stdout |

## Test Plan

### Functional Tests

| Case | Expected Behavior |
|---|---|
| Empty run (zero sources) | No ranked rows; exit 0 |
| Single analyzer | Hard error: "requires ≥ 2 analyzers" |
| Punctuation-only corpus | Rows present; `--filter lexical-only` excludes them per the Unicode-GC predicate |
| Multi-byte Japanese text | Char/byte spans consistent in output |
| Normalized surfaces (surface ≠ source substring) | Coverage mismatch rows generated and RRF-ranked |
| Analyzer emits zero morphemes for a non-empty source | Coverage mismatch row; ranked normally |
| Partial boundary agreement (2 of 3 analyzers agree) | Entropy computed from Bernoulli formula when boundary tables exist |
| v1 warehouse (no sidecar tables) | RRF on 4 signals; missing signals excluded; `signal_profile` reflects available set; `rarity_basis = "source"` when `aozora_works` absent |
| v2 warehouse with all sidecars | RRF on up to 9 signals; all signal-profile fields populated (`surprise: present`, `cause_classification_profile`, `granularity_profile`, etc.) |
| v2 warehouse missing `analyzer_bias_summary` | Surprise excluded; `surprise: absent` recorded in score block; other signals unaffected; top-50 not reshuffled artifactually (missing-data test) |
| Non-Aozora corpus run | `rarity_basis = "source"`; `literal_context_policy = "hash-only"`; `aozora_works` rows absent; era-novelty and surprise signals excluded |
| `--explain PATTERN_ID` | Per-signal rank decomposition with raw values and RRF terms, including any `λ_missing` terms for missing applicable signals |
| `--anomalies 10` | Top-10 individual regions whose patterns fall below cutoff emitted; `anomaly_w_cov` default 5.0 |
| Pattern-id bijection (golden) | Property test: canonical string → `pattern_id` is injective over the generator's pattern space; analyzer-pair reordering produces identical `pattern_id` |
| Reproducibility | Same input → byte-stable JSON (deterministic ordering, no "modulo"); score block fields stable |

### Property-Based Tests

| Invariant | Description |
|---|---|
| Monotonicity (coverage) | Increasing `coverage_mismatch_count` never worsens coverage rank |
| Monotonicity (rarity) | Decreasing `work_count` never worsens rarity rank |
| Permutation invariance | Input row order never changes output ranking |
| Missing-signal consistency (corrected) | Removing signal s's data for a pattern changes only the s-contribution (turning `1/(60+rank_s)` into `λ_missing`); per-signal ranks of other signals are unchanged; fused score changes monotonically downward by a bounded amount. Does not silently *boost* the normalized score via divisor shrinkage |
| Missing-signal monotonicity | Adding an applicable signal's data (transitioning `λ_missing → 1/(60+rank_s)`) never decreases the fused score; removing it never increases it |
| Applicability is kind-level | Two patterns of the same kind have the same `|S_applicable|`; missing-data terms are the only per-pattern variation |
| `pattern_id` bijection | Same canonical string ⇒ same `pattern_id`; different canonical strings ⇒ different `pattern_id` (no collisions over the property-test generator's pattern space) |
| `pattern_id` analyzer-pair symmetry | Reordering the analyzer pair does not change `pattern_id` |
| `pattern_id` NFC stability | NFC-equivalent value strings produce identical `pattern_id` |
| Score bounds | RRF score ∈ `[0, 1/|S_applicable|]` per kind, with upper bound `1/61 ≈ 0.0164` for a single-signal kind at rank 1; `λ_missing` bounds the lower end |
| Golden output | Fixed input produces byte-identical JSON across platforms |
| Score-profile labeling | A run's `signal_profile`, `rarity_basis`, `granularity_profile`, `cause_classification_profile`, `literal_context_policy`, and `surprise` availability are all present in output and consistent with the input tables available |

## Calibration Plan

Before locking v1 defaults:

1. Run the RRF ranker on three warehouse profiles: small smoke corpus (~100 sources), representative triage corpus (~1000 sources), full corpus (~15000 sources).
2. Compare top-50 RRF output against two baselines:
   - **Frequency sort** (most common patterns first)
   - **Random sort**
3. **A/B within-kind vs global RRF (Open Question 4):** also produce a top-50 with signals ranked globally (across kinds), not within-kind. If within-kind ranking does not measurably beat global on p@50 / nDCG@50, drop the within-kind split — it adds the `signal_profile` comparability constraint for a benefit that may not exist. Decision 6 is conditional on this measurement.
4. **Missing-data sweep (S5):** sweep `λ_missing ∈ {0, 0.005, 0.010}` on the representative triage corpus. Confirm top-50 ordering is stable (Kendall τ within threshold); if the sweep reshuffles top-50, revisit signal definitions before locking.
5. **Anomaly-weight sweep:** sweep `anomaly_w_cov ∈ {2.0, 5.0, 10.0}`; confirm the top-10 anomaly regions surface coverage gaps and long-span disagreements rather than noise.
6. Label each ranked row from the top-50 with verdicts (`bug`, `expected-policy`, `expected-dictionary`, `corpus-artifact`, `noise`, `unclear`).
7. Report **precision@50** and **nDCG@50** vs. baselines.
8. If RRF doesn't beat frequency sort at p@50, the feature isn't earning its complexity — revisit signal definitions before adding more.
9. After calibration, lock the v1 signal definitions, default RRF k=60, `λ_missing`, `anomaly_w_cov`, and `inheritance_jaccard_threshold` constants.
10. Post-MVP: when `review_events` accumulates sufficient labels from the review ledger, fit a learning-to-rank model on `(per-signal-rank, verdict)` tuples. The RRF scaffold is compatible — replace fusion with learned model while keeping per-signal rank computation unchanged.

## Tie-Breaking and Determinism

Within RRF, ties in raw signal values are broken by `pattern_id` lexicographic order to ensure deterministic rank assignment. Ties in final RRF score are broken by:

1. Higher `source_count`
2. Lexicographic by `pattern_id`

Sample ordering in output MUST be deterministic: `source_id`, then `text_id`, then `region_index`, then `analyzer_id`. Any future random sampling must require `--sample-seed`.

RRF scores are computed in double precision and rounded to **6 decimal places** (half-up) before serialization. Golden tests compare against these rounded values. Since RRF uses only `+` and `/`, cross-platform IEEE 754 drift is negligible.

## Score Versioning

Every output document carries a `score_version` block recording every knob that affects comparability. Two runs are comparable only if **all** of these fields match:

```json
{
  "score_version": 1,
  "pattern_id_version": 1,
  "rrf_k": 60,
  "lambda_missing": 0.005,
  "anomaly_w_cov": 5.0,
  "inheritance_jaccard_threshold": 0.85,
  "signal_profile": ["coverage", "rarity", "impact", "span"],
  "rarity_basis": "work",
  "granularity_profile": "none",
  "cause_classification_profile": "sudachi-only",
  "literal_context_policy": "literal",
  "surprise": "absent"
}
```

Any change to default weights, normalization, tie-breaking, RRF k-constant, `λ_missing`, anomaly weight, inheritance threshold, or component semantics increments `score_version`. The profile fields record the *data context* that the score was computed in:

- `signal_profile`: which signals were active. Scores are comparable only within the same profile.
- `rarity_basis`: `"work"` (dedup via `aozora_works`) or `"source"` (fallback when `aozora_works` absent). Cross-basis comparison is forbidden.
- `granularity_profile`: NINJAL granularity-class composition — deduped segmentation granularity classes (`"suw"`, `"muw"`, `"luw"`) present in the run's analyzers, sorted `suw < muw < luw` and joined with `"+"` (e.g. `"suw"`, `"suw+luw"`, `"suw+muw+luw"`). A run scored on a multi-class comparison is not comparable to one scored on a single-class comparison even with the same `signal_profile`.
- `cause_classification_profile`: `"sudachi-only"`, `"all-analyzers"`, or `"absent"`. Records the vibrato-lattice asymmetry (Decision 15).
- `literal_context_policy`: `"literal"` or `"hash-only"`. Records the per-run licensing gate (Decision 4).
- `surprise`: `"present"` or `"absent"`. Records whether the surprise signal's producer was available (Decision 12); avoids the silent-degradation failure mode.

## Known Limitations

- **Oracle coverage:** Ruby oracle only covers ruby-annotated spans (~30-60% of corpus depending on era). Consensus silver covers the rest. Gold annotation covers selected ambiguities.
- **Cross-kind ranking:** Within-kind ranking with `λ_missing` handling mitigates but does not eliminate the structural difference between feature patterns (4 signals) and segmentation patterns (3 signals). Signal profiles make this explicit.
- **Cross-run comparability:** RRF scores are comparable only within the same full `score_version` profile (all fields in §Score Versioning must match). Tooling can compute intersection-based comparison but this is not automatic.
- **Cause classification requires analyzer lattice access during the analysis pass; unavailable for v1 warehouses.** Classification is heuristic and may misclassify edge cases. **Analyzer asymmetry (Decision 15):** Sudachi exposes lattice today; Vibrato `vibrato-rkyv`'s lattice and n-best structures are `pub(crate)` in the pinned vendor checkout, so the Vibrato path either lands an upstream/fork API change or returns `cause_class = unknown` for Vibrato regions. `cause_classification_profile` records which path shipped.
- **Pattern ID stability** depends on the canonical serialization rules in §Pattern Identity; changes to Unicode normalization, separator characters, field order, or the scope-token table require a `pattern_id` version bump and orphan existing verdicts (mitigated by fuzzy matching in the review ledger; the matching is read-time authoritative with a write-time cache).
- **Aozora catalog** availability and accuracy depend on external metadata maintenance — specifically, ABC's `metadata-record` exports. `orthographic_style` and `publication_year` may be `null` for some works; the ranker degrades gracefully (era-novelty signal is excluded when stratification data is missing).
- **ABC projection dependency (Decision 10):** `aozora_works` is non-authoritative — it is regenerated from ABC exports by `ab-morph-run import-aozora-metadata`. If ABC changes field names or enum values, the importer must change in lockstep; this is a coordinated cross-repo release, not a unilateral ab-validator edit.
- **Boundary entropy and surprise** require v2 sidecar tables; unavailable in v1. Surprise additionally requires `analyzer_bias_summary.parquet` (Decision 12); if the producer slips, surprise is excluded with `surprise: absent` rather than silently disabled.
- **Morph ⇄ parser region-level join** is not implementable against today's ABC divergence artifact (the bundle is per-work, rule-aggregated with only `first_path` for the first occurrence). Region-level odds-ratio join requires a new per-span parser-error sidecar in ABC — a dependent design (Decision 14). Text-level Spearman with era covariate is the honest post-MVP scope.
- **Suppression auto-expiry** requires cross-run frequency tracking (post-MVP). In v2, suppressed patterns remain suppressed until manually reviewed.
- **All performance claims are design targets**, not benchmarks. Benchmarking is part of the calibration plan. The "~45M rows is within DuckDB's range" claim in §Performance Notes is explicitly unverified and is a calibration-plan measurement target.

## Cross-Subsystem Join: Morph ⇄ Parser (Post-MVP)

Connecting the morph diff warehouse to the parser evaluation (AAT comparison) subsystem owned by `abc`. **Scope correction:** earlier drafts described a region-level "±k character interval join implemented as one DuckDB range join over the two warehouse schemas." The actual parser-IR divergence artifact (`../abc`'s `aat-parser-ir-divergence-bundle.schema.json`) is **per-work, rule-aggregated** with only `first_path` for the first occurrence — it does not carry per-span parser-error records. The region-level join as described is therefore not implementable against today's data.

### Two Analysis Levels (corrected scope)

**Text level (the honest post-MVP deliverable):** Spearman correlation of morph-disagreement density vs. parser structural error rate at the per-work level, **controlling for `orthographic_style` as a covariate** — old-orthography texts are harder at both, so era is a confounder that would produce spurious correlation if uncontrolled. This join is implementable today: morph density comes from `nway_regions` aggregated per `source_id`; parser divergence comes from the bundle's per-work `summary` block; era comes from `aozora_works.orthographic_style`. One DuckDB join on `work_id`.

**Region level (requires the Per-Span Parser-Error Sidecar dependent design):** odds ratio of a parser error occurring within ±k characters of a morph disagreement vs. elsewhere in the same text. *Not* a one-shot range join: requires (a) ABC to materialize per-span parser-error records in a new sidecar (the bundle currently records only `first_path`), (b) a coordinate conversion from the morph warehouse's `char_*` offsets to parser-IR's `decoded_utf8` byte offsets (the morph side's `CharByteMap` provides O(1) char↔byte). Until (a) lands, the region-level join is described here as the *target shape*, not a deliverable.

### Conformed Dimensions

The eventual join key is `(work_id, source_id, char_span_or_byte_span)`. `work_id` is shared today; `source_id` is shared today; the span-coordinate alignment is the dependent design above.

### `text_quality.parquet` (post-MVP fact table)

When both levels are operational, a `text_quality.parquet` fact table records per-text: morph disagreement density, oracle failure rate, parser structural error rate, plus covariates (gaiji density, ruby density, `orthographic_style`, `publication_year` from `aozora_works`).

### Feedback to Ranking

When the morph ⇄ parser join is operational at the text level, patterns enriched in works with high parser error rate receive a `downstream_impact` signal in RRF — an empirical impact measure replacing the hand-tuned impact lookup table with observed consequence. This is post-MVP.

## Known-Phenomena Knowledge Base (Post-MVP)

A curated store of signature → explanation rules, keyed by `(feature_key, from_value, to_value, surface_pattern_regex, analyzer_pair_pattern)`. Each rule carries:

- `explanation`: Human-readable description (e.g., "Classic auxiliary/adjective ambiguity on ある/ない forms")
- `verdict`: Expected classification for matching patterns
- `auto_suppress`: Whether to automatically exclude matching patterns from future rankings
- `scope`: `(analyzer_pair, dictionary_version, feature_key)` — limits where the rule applies
- `source`: `curator`, `llm-draft:confirmed`, `publication`

Matched against patterns at rank time. Matching patterns receive an `explanation` field in output. Extended by every review session that identifies a new class of expected difference.

LLM-assisted explanation drafts (hypothesis generation from `--explain` output + region examples, stored as `unverified_annotation` with provenance, promoted to knowledge base on reviewer confirmation) are post-MVP.

## Aozora-Specific Corpus Handling

### Deduplication

Aozora works may appear as multiple files under one card (different encodings, anthologized reprints). Pattern frequency counting uses distinct `work_id` from `aozora_works` (an ABC projection of `work.work_id`, i.e. the Aozora card id), not raw file count — without this, affected patterns are counted 2× (287 multi-file-card sources measured 2026-07-06). Paired 旧字/新字 editions are separate cards and deliberately remain distinct works (see §Per-Signal Definitions); serials sharing a title (e.g. 銭形平次捕物控, 438 cards) are correctly distinct works. Enforced at the rarity-signal level and recorded as `rarity_basis = "work"` in the score block.

### Era/Orthography Heterogeneity

The corpus spans 1870s 文語 to 1950s modern prose — effectively two different languages when analyzed with modern dictionaries. Global pattern statistics average over this heterogeneity. The era-novelty signal (v2) computes pattern lift within an `orthographic_style` stratum (the `aozora_works.orthographic_style` enum, verbatim from ABC) vs. the global baseline, surfacing findings like "this disagreement is 12× enriched in pre-1946 texts."

### Era-Appropriate Dictionary Variants

For pre-modern texts, include era-appropriate dictionaries (近代文語 UniDic, 旧仮名 UniDic lineages) as additional analyzers. The paired-edition metamorphic tests tell you empirically where the modern-dictionary cutover matters — texts where the modern analyzer produces significantly different lemma sequences from the era-appropriate one.

### Artifact Flagging

Regions overlapping gaiji notation (※［＃...］), editor notes, or preprocessing artifacts may produce spurious disagreements. The `projection_spans.parquet` sidecar flags these (`is_gaiji`, `is_note`). In v2, patterns where a significant fraction of regions are flagged receive reduced ranking priority or are routed to a separate artifact channel.

### Repetition Marks (踊り字)

Characters like 々, ゝ, ゞ, 〳, 〵 can cause systematic segmentation chaos. The character-class columns in `boundary_contexts.parquet` capture these as a distinct class for boundary analytics. The knowledge base can pre-classify known 踊り字-related patterns.

## Performance Notes

### Redundancy Control (MMR Diversification)

Without diversity control, the top 50 can be 50 near-duplicate manifestations of one underlying phenomenon (e.g., one analyzer's dictionary lacking a common compound producing patterns across pos1, lemma, and reading simultaneously). After scoring, select output rows via **Maximal Marginal Relevance**: greedily pick the highest-scoring pattern whose similarity (shared `feature_key`, overlapping `sample_source_ids`, same analyzer-pair signature) to already-selected rows is below a threshold, or penalized by λ·max-similarity. Ensures the top-k covers k *phenomena*, not one phenomenon k times. Deterministic given deterministic tie-breaks.

### `boundary_consensus` Row Count

The proposed long-format table stores one row per `(source_id, text_id, boundary_offset_char, analyzer_id)`. For N analyzers, this produces N × B rows where B is the union of all boundary offsets. On the full corpus (~15K sources, ~1.5M regions), this approaches ~45M rows. If query performance is a concern, a bitvector encoding alternative stores one row per offset with an `analyzer_votes: UInt64` bitmask over an ordered analyzer registry, reducing row count by the analyzer count (~6-10×). The DuckDB view rendering this as long format for entropy queries remains a view-time option.

### Pattern-Level vs. Per-Region Query Cost

Pattern-level RRF operates on materialized `feature_pattern_counts` (~100K segmentation + ~500K feature rows). DuckDB can rank-sort this in seconds. Per-region scoring (anomaly channel) joins `nway_regions` with region analyzers and feature diffs (~1.5M × 3 × 10 = ~45M rows). This is within DuckDB's range but requires benchmark validation against target latency.
