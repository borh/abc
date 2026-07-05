# Interestingness Ranking v1 (RRF Ranker) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Implement `ab-morph-run summarize-warehouse-interesting` — a deterministic, explainable RRF ranking over v1 warehouse pattern data (spec: `docs/superpowers/specs/2026-07-05-interestingness-ranking-design.md`, MVP v1 / Phase 1).

**Architecture:** One streaming collection pass over the existing v1 Parquet fact tables (`nway_regions`, `nway_region_analyzers`, `nway_feature_diffs`, `runs`, `run_analyzers`, `sources`) builds patterns of three kinds (feature / segmentation / coverage) with per-pattern statistics. Four signals are ranked within-kind, fused with normalized RRF + `λ_missing`, and emitted with a `score_version` block, an anomaly channel, and `--explain` decomposition. No schema migration; no existing table or reader changes.

**Tech Stack:** Rust (existing `ab-morph-run` crate patterns: arrow-rs Parquet readers, clap subcommands, BTreeMap accumulators), `ab-diff-utils` sha256 hashing, `unicode-normalization` (NFC), `unicode-properties` (general categories), `proptest` (property tests).

## Global Constraints

- Deterministic byte-stable output: raw-signal ties break by `pattern_id` lex order; final RRF ties break by higher `source_count` then `pattern_id` lex; sample ordering by `(source_id, text_id, region_index, analyzer_id)`.
- RRF: `k = 60`, `λ_missing = 0.005`, `anomaly_w_cov = 5.0`, scores rounded to 6 decimal places (half-up) before serialization.
- `pattern_id` = `sha256:<hex>` over a versioned canonical serialization (`pattern-v1`); the canonical form is pinned by golden tests before anything consumes it.
- Signal applicability is kind-level: feature kind = {coverage, rarity, impact, span} (|S|=4); segmentation and coverage kinds = {coverage, rarity, span} (|S|=3).
- `score_version` block fields: `score_version: 1`, `pattern_id_version: 1`, `rrf_k: 60`, `lambda_missing: 0.005`, `anomaly_w_cov: 5.0`, `signal_profile`, `rarity_basis` (`"work"`/`"source"`), `granularity_profile: "none"`, `cause_classification_profile: "absent"`, `literal_context_policy: null`, `surprise: "absent"`.
- Error behavior per spec §Error Behavior: bad run dir → exit 1; `schema_version > 1` → exit 1; single analyzer → exit 1; empty run → no rows, exit 0; unknown `--explain` id → exit 1; existing `--output` file → exit 1 unless `--force`.
- v1 reads only existing v1 tables. `aozora_works.parquet` is probed opportunistically for work-based rarity; absent → `rarity_basis = "source"`.

## Deviations from the spec (decided, with reasons)

1. **`pattern_id` module home is `ab-morph-run/src/summary/pattern_id.rs`, not `ab-warehouse`.** The spec claims `ab-warehouse/src/writer.rs` "already hashes via RFC-8785 JCS" — it does not (no hashing dep there). The real hashing home is `ab-diff-utils/src/hashing.rs` (`hash_string_sequence`, length-prefixed sha256). Pattern identity is a summarizer concept; it consumes `NwayPatternKey` types private to `ab-morph-run`.
2. **Canonical form covers the real N-way pattern shape.** The spec's `canonical()` sketch is pairwise (`from_value`, `to_value`, `analyzer_pair`). The warehouse pattern entity is N-way: `NwayPatternKey { kind, segmentation_groups: [(surfaces, analyzers)], feature_key, feature_scope, feature_values: [(value, analyzers)] }`. The canonical serialization encodes the full value→analyzer-set group structure. All spec invariants carry over in strengthened form: analyzer ids sorted within groups, groups canonically ordered, NFC + whitespace-collapse on all value/surface strings, closed scope-token table (extension = hard error), `pattern-v1` version prefix. Analyzer-order invariance holds because analyzer lists are sorted and group order is a deterministic function of content. Direction still matters: value→analyzer assignment is preserved (stronger than pair-sorting).
3. **Length-prefixed part hashing instead of `format!` string joining.** Values may contain any separator character; `hash_string_sequence` (4-byte LE length prefix per part) removes injection ambiguity without inventing an escaping scheme. The golden test pins exact digests.
4. **All four signals computed from raw fact tables in one pass.** The materialized `feature_pattern_counts` table carries no span distribution, no coverage-mismatch counts, and no region references — 3 of 4 signals can't be computed from it. Deriving everything from `nway_regions` + `nway_region_analyzers` + `nway_feature_diffs` (the same tables the existing query-time summarizers already load whole) keeps one code path and exact `source_count`s.
5. **Coverage-kind pattern construction defined** (the spec names the kind but never defines its key): coverage patterns use the segmentation-style surface-group key computed over regions with `has_coverage_mismatch = true`, with `kind = "coverage"`, and — unlike segmentation — a single surface group is allowed (the mismatch itself is the finding; all analyzers may agree on surfaces while none covers the source span exactly).
6. **`--filter lexical-only` predicate approximated from warehouse data.** Spec asks for Unicode `P*`/`Z*` over `[char_start, char_end)` of the *source text* plus a morpheme-POS clause; source text is not stored in the warehouse and the POS clause needs a `morpheme_features` scan. Implemented predicate: region excluded when `is_nonempty_whitespace` OR every char of every analyzer's surfaces is in Unicode `P*`/`Z*` (via `unicode-properties`). Surfaces are the analyzers' view of the span, which subsumes the POS clause for the punctuation-anthology case the filter targets.
7. **Rarity uses `log2`** (spec writes `log` without a base; rank order is base-invariant; `log2` matches the other signals' units in `--explain`).
8. **Impact lookup keys mapped to this codebase's canonical feature keys**: `pos1`→4.0, `lemma`→3.5, `ctype`→2.5, `cform`→2.0, readings {`kana`, `kana_base`, `pron`, `pron_base`, `lform`}→1.5, everything else→1.0.
9. **`--output` refuses to overwrite without `--force` for both formats** (spec pins this for JSON only; symmetric behavior is strictly safer).
10. **Two collection engines; DuckDB required at real scale.** The spec sized the problem at ~1.5M disagreement regions; the actual full-corpus warehouse (`morph-warehouse-full-2026-05-02`) holds **165M** regions, 495M region-analyzer rows, and **17.8B** feature-diff rows. Deviation 4's single in-memory pass cannot hold that, so collection is engine-split: a DuckDB CLI aggregation (same binary discovery and settings as the existing materializers; canonical `list_sort` signatures grouped in SQL, re-canonicalized and hashed in Rust) and the in-memory path for small runs/tests/no-duckdb environments. `--engine auto|in-memory|duckdb`; a path-equality test pins byte-identical summaries. The anomaly channel's "patterns below cutoff" exclusion is an SQL anti-join on the top patterns' aggregation signatures.
11. **`λ_missing` is a policy, not a constant.** The spec's fixed default 0.005 contradicts its own property test: `1/(60+rank) < 0.005` for rank > 140, so gaining data would *lower* a score. Implemented the spec's parenthetical rank-floor default `λ = 1/(k + N_kind + 1)` (monotonicity holds unconditionally); the score block records `lambda_missing_policy: "rank-floor"` instead of a numeric knob.
12. **`--feature-profile raw|core` added, defaulting to `core`** (pos1–pos4). Measured on the full corpus, the raw profile is ~2.5B pattern occurrences over tens of millions of near-singleton patterns (reading keys make nearly every region its own pattern); even a digest-keyed, interned, sorted-vec accumulator holds tens of GB, and singleton floods drown the rarity signal. The spec's own sizing ("~500K recurring patterns") and its `feature_pattern_counts` fast path are core-scoped. Raw remains opt-in for hosts with the memory. Recorded in the `score_version` block as `feature_profile` since it changes which patterns exist.
13. **DuckDB engine final architecture** (what survived the full-corpus campaign; each step is a commit with the failure it fixes):
    - Grouped `list(...)`/ordered-aggregate states cannot spill in DuckDB → the feature path COPYs a deduplicated per-`(region, key, scope, value)` stage with fixed-state `bool_or` analyzer pivots, externally sorted; Rust streams contiguous groups into the same `PatternAccumulator` as the in-memory engine.
    - One feature key per DuckDB invocation by default (`AB_INTERESTING_FEATURE_KEY_BATCH`): a 6-key stage sort overflows a ~110 GiB temp volume.
    - Memory budget from `MemAvailable`, not total RAM (`AB_DUCKDB_MEMORY_LIMIT`): `earlyoom -m10` SIGTERMs at the 10% watermark.
    - Anomaly exclusion for top feature patterns re-runs the stage for just their feature keys and anti-joins an exclusion parquet written from Rust; seg/coverage exclusion stays signature-based in SQL.
    - Full-corpus result (165M regions / 495M analyzer rows / 17.8B feature diffs): **~8 minutes end-to-end** under the core profile.

---

### Task 1: `pattern_id` canonicalization module

**Files:**
- Modify: `crates/ab-morph-run/Cargo.toml` (add `ab-diff-utils.workspace`, `unicode-normalization.workspace`; dev-dep `proptest.workspace`)
- Modify: `Cargo.toml` (workspace: add `ab-diff-utils` path dep entry if absent; add `proptest = "1.5"` and `unicode-properties = "0.1"` workspace entries)
- Create: `crates/ab-morph-run/src/summary/pattern_id.rs`
- Modify: `crates/ab-morph-run/src/summary/mod.rs` (declare module)

**Interfaces:**
- Produces: `pub(crate) const PATTERN_ID_VERSION: u32 = 1;`
- Produces: `pub(crate) fn pattern_id(key: &NwayPatternKey) -> String` — returns `"sha256:<hex>"`.
- Produces: `pub(crate) fn nfc_collapse_ws(value: &str) -> String` — NFC, collapse ASCII-whitespace runs to one U+0020, trim.
- Consumes: `NwayPatternKey`, `NwayFeatureScopeRow`, `NwaySegmentationGroupRow`, `NwayFeatureValueGroupRow` from `summary_body.rs` / `nway.rs`; `ab_diff_utils::hash_string_sequence`.

**Canonical part sequence (pinned):**

```
parts = ["pattern-v1", kind]
parts += ["fkey", feature_key or ""]            // feature kind only ("" for others)
parts += ["scope", scope_token]                  // closed table below; "" when scope is None
for each segmentation/coverage surface group (canonical order):
    parts += ["group", surfaces.len() as string] + surfaces.map(nfc_collapse_ws)
    parts += ["analyzers", analyzers.sorted().join(",")]
for each feature value group (canonical order):
    parts += [value.is_none() ? "null" : "value", nfc_collapse_ws(value or "")]
    parts += ["analyzers", analyzers.sorted().join(",")]
pattern_id = hash_string_sequence(parts)         // ab-diff-utils, length-prefixed sha256
```

Scope tokens (closed table; extending it bumps `PATTERN_ID_VERSION`):
`WholeRegion` → `whole_region`; `TokenPosition{position}` → `pos:{position}`; `Surface{surface}` → `surf:{nfc_collapse_ws(surface)}`. (Analyzer ids are ASCII CLI names; `,`-join is unambiguous.)

Group canonical order: groups are re-sorted AFTER value normalization by `(normalized surfaces/value, sorted analyzers)` so NFC-equivalent inputs produce identical order and hash.

- [x] Step 1: Add deps; write failing unit tests (module with `pattern_id` calls that don't compile yet counts): NFC stability, analyzer-order invariance, kind separation, null-vs-empty value distinction, golden digest for one fixed key.
- [x] Step 2: `cargo test -p ab-morph-run pattern_id` → fails.
- [x] Step 3: Implement `pattern_id.rs` exactly per the pinned sequence.
- [x] Step 4: `cargo test -p ab-morph-run pattern_id` → passes.
- [x] Step 5: Property tests (proptest): NFC-equivalent values ⇒ equal ids; analyzer permutation ⇒ equal ids; distinct canonical parts ⇒ distinct ids (injectivity over generator space); value↔analyzer reassignment ⇒ different ids.
- [x] Step 6: `cargo test -p ab-morph-run` green; commit.

### Task 2: pattern collection from raw warehouse tables

**Files:**
- Create: `crates/ab-morph-run/src/summary/interesting.rs`
- Modify: `crates/ab-morph-run/src/summary/mod.rs`, `summary_body.rs` (make needed helpers `pub(super)`)

**Interfaces:**
- Produces: `pub struct WarehouseInterestingOptions { pub limit: usize, pub filter: InterestingTextFilter, pub anomalies: usize, pub explain: Option<String>, pub max_region_examples: usize /* default 5 */ }`
- Produces: `pub enum InterestingTextFilter { All, LexicalOnly }` (ValueEnum)
- Produces (internal): `struct PatternStats { key: NwayPatternKey, pattern_id: String, examples: usize, source_ids: BTreeSet<String>, text_ids: BTreeSet<String>, rarity_count: usize /* distinct works or sources */, coverage_region_count: usize, span_lengths: Vec<u64>, region_examples: Vec<RegionExampleRef> }`
- Produces (internal): `fn collect_patterns(run_dir, options) -> Result<CollectedPatterns>` where `CollectedPatterns` also carries `region_pattern_membership: BTreeMap<WarehouseRegionKey, Vec<usize>>`, region flags map, run metadata (`run_id`, distinct source count, work map presence).
- Consumes: `read_warehouse_region_flags`, `read_warehouse_region_analyzers`, `read_warehouse_feature_diffs`, `warehouse_segmentation_pattern_key`, `warehouse_feature_pattern_key`, `warehouse_feature_facts_for_profile`, `nway_pattern_display` (make `pub(super)` where needed), `pattern_id::pattern_id`.

Collection rules:
- Feature patterns: same grouping as `summarize_warehouse_feature_patterns` with `feature_profile = Raw`, no feature-key filter, no exclusions; only keys with `feature_values.len() > 1`.
- Segmentation patterns: same as `summarize_warehouse_segmentation_patterns` (requires `has_segmentation_disagreement` and >1 surface group).
- Coverage patterns: surface-group key over regions with `has_coverage_mismatch`, `kind = "coverage"`, single group allowed.
- `lexical-only` filter: skip regions where `is_nonempty_whitespace` OR all surface chars ∈ `P*`/`Z*` (helper `fn region_is_punctuation_only(facts: &[WarehouseRegionAnalyzerFact]) -> bool` using `unicode_properties::GeneralCategoryGroup`).
- Rarity basis: if `run_dir/aozora_works.parquet` exists, read `(work_id, source_id)` and count distinct works per pattern and total distinct works; else distinct sources. Record basis.
- Region examples: first `max_region_examples` in `(source_id, text_id, region_index)` order.

- [x] Steps: failing unit tests on a `WarehouseWriter` fixture (mirroring `summarize_warehouse_patterns_reads_segmentation_disagreement_facts`) asserting pattern counts per kind, coverage single-group admission, filter exclusion, work-based rarity when a synthetic `aozora_works.parquet` is present → implement → green → commit.

### Task 3: signals, within-kind ranking, RRF fusion

**Files:**
- Modify: `crates/ab-morph-run/src/summary/interesting.rs`

**Interfaces:**
- Produces: `pub(crate) enum Signal { Coverage, Rarity, Impact, Span }` with `fn applicable(kind) -> &'static [Signal]`.
- Produces: `fn raw_signal_value(signal, stats, totals) -> Option<f64>` (None = applicable-but-missing → `λ_missing`).
- Produces: `fn fuse(ranked: &mut [PatternScore])` implementing `RRF(p) = Σ_s [present ? 1/(60+rank_s) : λ_missing] / |S_applicable(kind)|`.
- Produces: `pub(crate) fn round6(x: f64) -> f64 { (x * 1e6).round() / 1e6 }`.

Signal definitions:
- coverage: `log2(1 + coverage_region_count)`
- rarity: `log2((rarity_total + 1) / (rarity_count + 1))` — note inverted: fewer works = rarer = larger
- impact (feature kind only): lookup table from Deviation 8
- span: 90th percentile of `span_lengths` by nearest-rank (`sorted[ceil(0.9 n) - 1]`)

Ranking: per kind, per signal, sort patterns by raw desc then `pattern_id` asc; assign dense ranks 1..n (total order after tiebreak). Final ordering: `rrf_score` desc (unrounded compare on rounded value), `source_count` desc, `pattern_id` asc.

- [x] Steps: TDD unit tests (hand-computed RRF for a 3-pattern fixture; λ_missing path via direct `fuse` call), then property tests (coverage/rarity monotonicity, permutation invariance of input row order, missing-signal consistency & monotonicity, kind-level |S| constancy, score bounds `[0, Σ 1/(60+i) / |S|] ⊂ [0, 1/61]`-per-signal-at-rank-1 check) → implement → green → commit.

### Task 4: anomaly channel

**Files:**
- Modify: `crates/ab-morph-run/src/summary/interesting.rs`

**Interfaces:**
- Produces: `pub struct AnomalyRow { pub source_id, pub text_id, pub region_index, pub char_start, pub char_end, pub has_coverage_mismatch, pub anomaly_score: f64 }`
- Produces: `fn anomaly_channel(collected, top_pattern_indices: &BTreeSet<usize>, k: usize) -> Vec<AnomalyRow>`

Rules: candidate regions = non-agreement regions passing the text filter whose owning patterns (from `region_pattern_membership`) are all outside the emitted top-`limit` set (regions owning no pattern qualify). `anomaly_score = (has_coverage_mismatch as f64) * 5.0 + log2(1 + (char_end - char_start))`, rounded 6dp. Order: score desc, then `(source_id, text_id, region_index)`. Take `k`.

- [x] Steps: TDD → implement → green → commit (folded into Task 3's commit cadence if small).

### Task 5: output contract, score_version block, explain

**Files:**
- Modify: `crates/ab-morph-run/src/summary/interesting.rs`, `summary/mod.rs`, `crates/ab-morph-run/src/lib.rs` (re-exports)

**Interfaces:**
- Produces: `pub struct InterestingSummary { pub score_version: ScoreVersionBlock, pub run_id: String, pub rows: Vec<InterestingRow>, pub anomalies: Vec<AnomalyRow> }` (Serialize)
- Produces: `pub struct ScoreVersionBlock` with the Global-Constraints fields exactly (serde field order = struct order, pinned by golden test).
- Produces: `pub struct InterestingRow { pattern_id, pattern (via nway_pattern_display), kind, rrf_score, signal_profile: Vec<&'static str>, signals: Vec<SignalExplain>, examples, source_count, text_count, sample_source_ids: Vec<String> (≤5), sample_text_ids: Vec<String> (≤5), region_examples: Vec<RegionExampleOut> }`
- Produces: `pub struct SignalExplain { signal: &'static str, status: "present"|"missing", raw_value: Option<f64>, rank: Option<u64>, rrf_term: f64 }`
- Produces: `pub fn summarize_warehouse_interesting(run_dir: &Path, options: WarehouseInterestingOptions) -> Result<InterestingSummary>` — the single public entry; `--explain` mode returns a summary whose `rows` contain exactly the matched pattern (full decomposition), `anomalies` empty; unknown id → `Err`.
- Produces: `pub fn write_interesting_tsv(summary: &InterestingSummary, out: impl Write) -> Result<()>` — header + rows: `rank, kind, rrf_score (6dp fixed), examples, source_count, pattern_id, pattern`; anomaly section appended with `# anomalies` header when non-empty.

- [x] Steps: TDD on fixture warehouse (JSON shape asserted via serde_json::Value, TSV golden string) → implement → green → commit.

### Task 6: CLI subcommand, error gates, golden byte-stability

**Files:**
- Modify: `crates/ab-morph-run/src/main.rs` (subcommand `SummarizeWarehouseInteresting`, dispatch, `--format table|json`, `--output`, `--force`)
- Modify: `crates/ab-morph-run/src/summary/interesting.rs` (run gates)

Gates inside `summarize_warehouse_interesting`: missing `runs.parquet` → hard error; `schema_version > 1` → hard error naming the version; `run_analyzers` distinct count < 2 → hard error "ranking requires >= 2 analyzers"; zero sources → empty summary Ok.

CLI: `--run-dir PATH`, `--limit 50`, `--format table|json` (default table), `--filter lexical-only|all` (default all), `--explain PATTERN_ID`, `--anomalies 10`, `--output PATH`, `--force`. `--explain` ignores `--limit`/`--anomalies`. JSON = `serde_json::to_writer_pretty` of `InterestingSummary` + trailing newline; table = TSV.

- [x] Steps: failing integration test in `summary/interesting.rs` tests covering: v2-version run rejected, single-analyzer rejected, empty run exit-ok, byte-identical JSON across two invocations (golden fixture), then main.rs wiring compiled + `cargo test -p ab-morph-run` + `cargo clippy` → commit.

## Self-Review Notes

- Spec coverage: v1 contract (tables, signals, RRF form, λ_missing, output contract, anomaly channel, filter, CLI, pattern identity, tie-breaking, score_version, error behavior, functional+property test plan rows relevant to v1) → Tasks 1–6. v2 sidecars/oracles/ledger/harmonization: out of scope (separate plans; `granularity_profile: "none"` tag emitted per Decision 13).
- Type consistency: `NwayPatternKey`/`WarehouseRegionKey` reused from `summary_body.rs`; new public types only in `interesting.rs`.
