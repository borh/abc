# Phase 3: `projection_spans.parquet` + Warehouse Run Memory Reduction — Design

Date: 2026-07-06. Owner-approved scope (this session): Phase 3 structural bridge
as specified below, plus perf items P1–P4. Governing spec:
`docs/superpowers/specs/2026-07-05-interestingness-ranking-design.md`
(§v2 Sidecar Tables / `projection_spans.parquet`, §Aozora Oracles, §MVP v2
Principle, Implementation Phases §Phase 3, Decision 3, Decision 7, Open
Question 2). This document specifies the *how*; column semantics and the
version-bump policy are the governing spec's and are not re-decided here.

## Goal

1. The analysis pass (`ab-morph-run analyze-aat --warehouse-dir`, Full profile)
   writes `projection_spans.parquet`: the mapping from projected-plaintext char
   offsets back to the AAT inline nodes that produced them. This is the minimal
   structural bridge that unlocks the Phase 4 ruby oracle (ABC measured
   `ruby.direction` as the largest divergence category, ~1.76M occurrences) and
   gaiji/note artifact flagging.
2. The first analysis-pass-produced sidecar triggers the warehouse
   `SCHEMA_VERSION` 1→2 bump with the reader rule generalized to "reject
   `schema_version` greater than the reader's supported maximum" (Decision 3).
3. Warehouse-run worst-case memory drops on the large-document tail — the
   measured cause of the earlyoom restarts (attempt 1 killed at 57.5 GB RSS;
   see `docs/superpowers/plans/2026-07-05-warehouse-run-perf.md` §Task 5
   validation record) — via three targeted, behavior-preserving changes, with
   the mandatory full-corpus regeneration doubling as the measurement run.

**Resolves Open Question 2:** spans are generated during the analysis pass, not
a separate preprocessing step (the pass already holds the parsed AAT; a second
pass over 17,885 AAT files would be pure waste).

## Non-Goals

- The ruby-oracle consumer (`nway_region_oracle_evidence.parquet`) — Phase 4.
- Full structural context (Gap 7 / block-type ranking) — Decision 7 scopes the
  bridge deliberately narrower.
- mmap-backed dictionary loading (~13.1 GB fixed dictionary residency),
  §3.15 intra-worker parallelism, and batch-size sweeps — each needs its own
  measurement window; recorded in §Deferred.
- Any change to which disagreements exist or how they score. `score_version`
  is untouched; the warehouse `SCHEMA_VERSION` bump is independent of it.

## Part 1 — Span-Emitting Projection (`ab-plaintext`)

### API

`crates/ab-plaintext/src/aat.rs` gains:

```rust
pub struct ProjectionSpan {
    /// Char offsets into the projected (canonicalized) text; end exclusive.
    pub projected_char_start: u64,
    pub projected_char_end: u64,
    /// RFC 6901 JSON pointer to the AAT node that contributed the chars,
    /// e.g. "/blocks/12/content/3" or "/blocks/4/content/1/upper/0".
    pub aat_pointer: String,
    /// The contributing node's own `kind` (see §Emission Semantics).
    pub inline_kind: String,
    pub is_ruby_base: bool,
    pub is_gaiji: bool,
    pub is_note: bool,
}

pub fn visible_text_projection_with_spans(aat: &Value) -> (String, Vec<ProjectionSpan>);
pub fn from_aat_value_with_spans(aat: &Value)
    -> Result<(PlainTextDocument, Vec<ProjectionSpan>), PlainTextError>;
```

`visible_text_projection` becomes a delegating wrapper that discards the spans
(or shares a common internal walker with a no-op collector — implementer's
choice, but there must be exactly **one** tree walker; a second spans-only
traversal was rejected because two walkers of the same tree drift silently).

**Identity invariant (load-bearing):** `visible_text_projection_with_spans(aat).0`
is byte-identical to today's `visible_text_projection(aat)` for every input.
The projection feeds morph analysis; any text drift invalidates row-count
parity with the canonical warehouse. Pinned by tests (§Test Plan).

### Emission Semantics

One span per AAT node that contributes ≥1 projected char, in document order,
non-overlapping, and jointly exhaustive: the concatenation of span slices
equals the full projected string (every projected char comes from exactly one
contributing node).

| Contributing node | `inline_kind` | Flags | Notes |
|---|---|---|---|
| `text` (`value`) | `text` | — | Includes text children inside `warigaki` `upper`/`lower`, `figure` `caption`, and style-family containers (`style`, `font_size`, `tcy`, `keigakomi`, `yokogumi`, `caption`); the container is recoverable from `aat_pointer`. |
| `ruby` (`base`) | `ruby` | `is_ruby_base = true` | The reading is NOT duplicated into the table; Phase 4 fetches it via `aat_pointer`. |
| `gaiji` (`resolved` or `description` fallback) | `gaiji` | `is_gaiji = true` | Unresolved-with-reason gaiji project nothing → no span. |
| `accent` (`resolved` or `name` fallback) | `accent` | — | |
| `raw` (`source`) | `raw` | — | |

Zero-width contributions emit no span: excluded `style_type = "notes"` nodes,
empty-`resolved` gaiji, adapter extension kinds without `content` (e.g.
`gaiji_ruby`, `Image` observed in the corpus). Consequence: **`is_note` is
structurally always false in v1 emission** (notes are excluded from projection,
so no note node ever contributes a char). The column ships anyway, per the
governing spec's column table — same forward-infrastructure precedent as the
dormant `lambda_missing` rank-floor policy — and this dormancy is documented in
`schema.sql`.

Newline chars synthesized by nothing (there are none today — the projection
emits only node text) need no rule; if a future projection change introduces
synthesized separators, they must be assigned to a source node or the identity
test will force the decision explicitly.

### Line-Ending Canonicalization Remap

`visible_text_projection` canonicalizes `\r\n`/`\r` → `\n` **after**
collection (`canonicalize_line_endings`, `ab-plaintext/src/lib.rs:102`), which
shifts char offsets. Design:

- Collect raw text + spans with raw char offsets during the single walk.
- If the raw text contains no `\r` (the corpus-dominant fast path), the raw
  offsets are already final — return as-is, zero extra cost.
- Otherwise run a canonicalization pass that, in one scan, builds the output
  string and remaps span boundaries: `\r\n` → `\n` drops one char (offsets
  after it shift by −1); lone `\r` → `\n` is 1:1. The map is monotonic, so
  remapping is a linear merge over the (sorted) span boundaries — no
  per-span search.

The existing whole-string `canonicalize_line_endings` remains the semantic
reference; a property test asserts the remapping pass produces the identical
string (§Test Plan).

### Ortho Interplay

Span offsets index the **projected original text** (`document.text`) — the
same coordinate space as `nway_regions.char_start/char_end` and post-remap
morpheme spans (spec invariant: warehouse coordinates reference the original
document). Warehouse runs currently force `ortho_detect = Off`
(`pipeline.rs:1017` TODO), so projected == analyzed text today; if ortho is
ever threaded through, morpheme remap already restores original coords and the
join stays valid. No new coordinate space is introduced.

## Part 2 — Warehouse Integration (`ab-warehouse`, `ab-morph-run`)

### Schema

- `WarehouseTable::ProjectionSpans`, file `projection_spans.parquet`, appended
  to `WarehouseTable::ALL` and `MERGED_DATA` (`ab-warehouse/src/schema.rs`).
- Row struct `ProjectionSpanRow` and Arrow schema, matching the governing
  spec's column table exactly:
  `run_id Utf8, source_id Utf8, text_id Utf8, projected_char_start UInt64,
  projected_char_end UInt64, aat_pointer Utf8, inline_kind Utf8,
  is_ruby_base Boolean, is_gaiji Boolean, is_note Boolean`.
  Id columns use the same shared-`Arc<str>` discipline as `MorphemeRow`.
- `WarehouseWriter`: new optional `ArrowWriter` field + `append_projection_spans`
  + `append_record_batch`/`finalize`/`writes_table` arms (mechanical; the
  compiler enumerates the match sites).
- `schema.sql`: `CREATE TABLE projection_spans (...)` with column comments,
  including the `is_note` dormancy note; header comment becomes
  "Morph warehouse schema version 2." and the policy line becomes
  "Readers must reject runs.schema_version values greater than the reader's
  supported maximum (currently 2)."
- `morph_views.sql`: a `warehouse_projection_spans` view guarded by
  begin/end markers (same mechanism as `__RAW_FEATURE_DIFFS_*`), stripped by
  `write_run_views_sql` when the table file is absent (triage runs, pre-v2
  runs recompacted, etc.). The runs-view filter `WHERE schema_version = 1`
  becomes `WHERE schema_version <= 2`.

### Version Bump (Decision 3 executed)

- `SCHEMA_VERSION: u32 = 2` (`ab-warehouse/src/schema.rs:4`).
- `READER_MAX_SCHEMA_VERSION: u32 = 2`
  (`ab-morph-run/src/summary/interesting.rs:51`) — the "reject > max" check at
  `interesting.rs:1430` already implements the generalized rule; only the
  constant and its tests move.
- `sql.rs` test `sql_mentions_parquet_not_jsonl_and_has_version_policy`
  updated to assert the new policy string.
- v1 runs (`schema_version = 1`) remain readable by the v2 reader; sidecar
  absence is handled by the existing presence-probe contract. A v1 reader on a
  v2 run hard-errors — the documented compatibility gate, not a regression.
- `aozora_works.parquet` stays version-neutral (post-hoc import, presence-
  probed) exactly as Decision 3 states; `import-aozora-metadata` is untouched.

### Profile Wiring and Emission Point

- **Full profile only.** `WarehouseProfile::Full` (= `ALL`) gains the table
  automatically; the triage table lists (`options.rs:34-54`) do NOT include it.
  Triage's contract is "cheap subset scoring"; sidecar consumers already
  presence-probe.
- Emission: in `run_analyze_aat_serial`, project via `from_aat_value_with_spans`
  when (and only when) the warehouse writer writes the table (else the existing
  span-free path). Rows are appended right after `append_sources`, chunked by
  `WAREHOUSE_MORPHEME_ROW_BATCH_SIZE` like morpheme rows. The shard merge and
  two-phase commit need no new logic beyond the `MERGED_DATA` membership
  (`stage_parquet_table_part` + `compact_staged_table` iterate
  `merged_data_tables()`); expected volume ~10–30M rows corpus-wide (sampled:
  ~500 emitting nodes per 100 KB source), so merge compaction thresholds are
  unaffected.

## Part 3 — Memory Reduction (P1–P4)

Evidence base: the 2026-07-06 hotspot sweep of this session. Worst-case RSS is
driven by per-document transients on the large-document tail (corpus max file
4.6× the calibration subset max), stacked per worker: serde_json DOM held for
the whole loop iteration + a redundant full-text clone + 4 analyzers' morpheme
sets + `CharByteMap` (~6.6 MB per source-MB) + per-analyzer `BTreeSet`
boundary sets. All three code changes are behavior-preserving; none changes
any output byte.

- **P1 — Drop the AAT DOM after projection.** `read_aat_value` parses the full
  file into a `serde_json::Value` (`lib.rs:651`) that is unused after
  `from_aat_value*` yet lives to the end of the loop iteration
  (`pipeline.rs:555-917`). Drop it immediately after projection (span
  extraction happens at projection time, so Phase 3 composes). Frees several×
  file size per in-flight document before analysis begins.
- **P2 — Eliminate the ortho-off text clone.** The detector-off arm clones the
  full document text (`pipeline.rs:666`: `document.text.clone()`; the
  detector-on empty-annotations arm at `pipeline.rs:659` clones too); restructure
  so the no-detector path reuses the existing allocation (e.g. build
  `shared_normalized: Arc<str>` directly from `document.text` and construct
  `norm_doc` without an intermediate copy). One full-document copy per doc,
  pure waste on warehouse runs (ortho is always Off there).
- **P3 — Boundary sets as sorted vectors.** `NwayStatsAccumulator`'s
  `boundary_counts` (`nway.rs:463-484`) builds one `BTreeSet<usize>` per
  analyzer plus a merged set over every morpheme boundary. Morpheme end
  offsets are already emitted in ascending order per analysis, so per-analyzer
  sorted `Vec<usize>` + k-way merge (or `Vec` + sort + dedup where order isn't
  guaranteed) replaces BTree node overhead (~3-4× per element) with flat
  storage. Tens of MB per large document at 4 analyzers. Must be pinned by the
  existing n-way golden/unit tests — identical regions out.
- **P4 — Measure the regeneration run.** The Phase 3 full-corpus run (§
  Validation) runs under `/usr/bin/time -v` at the canonical `jobs=8`; record
  wall and peak RSS against the jobs=19 baseline (59:43, 65.6 GB) and the
  jobs=10 pre-Arc reference (42 min) in the plan file. This is the
  before/after evidence for P1–P3 and the input for deciding whether the
  deferred dictionary/§3.15 work is next.

### Deferred (recorded for the next handoff)

- **Dictionary residency (~13.1 GB fixed):** mmap-backed loading for
  vibrato-rkyv (`Dictionary::from_zstd` → heap today) and Sudachi. The largest
  single number in the profile and job-count-independent — a force multiplier
  for the auto-jobs budget — but it touches the vendored vibrato-rkyv loader
  and needs a dedicated spike.
- **§3.15 intra-worker parallelism:** the established wall-clock lever
  (~22/32 cores busy at jobs=32).
- **`WAREHOUSE_REGULAR_BATCH_SIZE` / row-group sweeps** (§3.5): only with
  RSS-swept measurement.
- `auto_jobs` recalibration: P1–P3 shrink the per-job transient term; the
  constants (`PER_ANALYZER_BYTES = 384 MiB`) stay as-is this cycle
  (over-conservative beats OOM) and are re-fitted only in a future calibration
  window with P4's numbers in hand.

## Error Behavior

| Condition | Behavior |
|---|---|
| Run `schema_version` > reader max (now 2) | Hard error, exit 1 (existing generalized rule; constant bumped). |
| `projection_spans.parquet` absent (v1 run, triage run) | Readers degrade via presence probe; no error. |
| Span emission internal inconsistency (span end beyond projected length, overlapping spans) | Debug-assert + property-test territory; never a silent bad row. The emitter is total for schema-valid AAT — unknown kinds recurse into `content` exactly as the projection does and emit spans only for chars actually pushed. |
| AAT that fails projection today | Unchanged error path (`project_aat` error row); spans never partially emitted for a failed source. |

## Test Plan

1. **Projection identity:** for every existing aat.rs test fixture plus a
   generated corpus sample, `visible_text_projection_with_spans(aat).0 ==
   visible_text_projection(aat)` (and the wrapper delegates, making this
   structural). Property test with arbitrary nested AAT-shaped JSON including
   `\r`/`\r\n` content.
2. **Span coverage invariant (property):** spans are in-order, non-overlapping,
   within bounds, and their concatenated slices reconstruct the projected text
   exactly.
3. **Golden spans:** the existing nested-projection fixture (`aat.rs` tests:
   ruby, gaiji variants, accent, raw, warigaki, style, figure caption, notes
   exclusion) with exact expected span tuples, including pointers into
   `upper`/`lower`/`caption` and the `\r\n` remap case.
4. **Warehouse round-trip:** `create → append_projection_spans → finalize`,
   schema column-name parity tests (`schema_sql_columns_match_documented_parquet_columns`
   picks the new table up automatically once `column_names()` is filled in),
   empty-table validity, shard merge staging of the new table, views.sql
   marker stripping when absent.
5. **Version gate:** summarizer accepts `schema_version = 2` and still accepts
   1; rejects 3 with the existing message (test at `interesting.rs:2187`
   updated).
6. **P1–P3 regression:** existing suites pin behavior (`cargo test -p
   ab-morph-run --features test-analyzer`, ab-morph-diff, ab-plaintext); P3
   additionally re-runs the n-way unit tests unchanged.
7. **End-to-end:** test-analyzer warehouse run (2 analyzers, few AATs, Full
   profile) asserts `projection_spans.parquet` exists in the published run with
   plausible rows joined against `sources`.

## Validation (operational, after implementation)

1. Full-corpus regeneration from the monorepo layout at `jobs=8`, Full
   profile, canonical analyzer set, under `/usr/bin/time -v` (P4).
2. Row-count parity gate: all 11 v1 tables row-count-exact vs canonical
   `full-2026-07-05_164518-jobs0` (analyses 71,540; morphemes 662,984,226;
   nway_regions 161,142,784; nway_feature_diffs 23,356,986,673; sources
   17,885; per-analyzer counts identical). `projection_spans` present with
   row count recorded.
3. Swap canonical ONLY on exact match, same discipline as the perf plan's
   Task 5; on any mismatch the old run stays canonical and the discrepancy is
   diagnosed first.
4. Record wall/RSS + the auto-jobs line in the implementation plan; append the
   deferred-items handoff note.

## Decisions

| # | Decision | Rationale |
|---|---|---|
| 1 | Spans generated in the analysis pass (resolves governing Open Question 2) | AAT already parsed there; Phase 4 needs spans at analysis time; avoids a second corpus pass |
| 2 | Single shared tree walker; identity pinned by tests | Two traversals drift silently; the projection string is the warehouse's coordinate ground truth |
| 3 | Span per contributing node, ≥1 char, `inline_kind` = node's own kind | Containers recoverable from `aat_pointer`; zero-width rows carry no join value for oracle/artifact use |
| 4 | `is_note` ships dormant (structurally false in v1 emission) | Governing spec's column table is the contract; documented dormancy over spec churn (rank-floor precedent) |
| 5 | Full profile only; triage excluded | Triage = cheap subset scoring; presence probe is the sidecar contract |
| 6 | `SCHEMA_VERSION` 1→2 with this table (executes governing Decision 3) | First analysis-pass-produced sidecar |
| 7 | Perf scope = P1–P4; dictionary mmap, §3.15, batch sweeps deferred | Owner-approved; P1–P3 are behavior-preserving and target the measured OOM cause; deferred items need their own windows |
| 8 | `auto_jobs` constants unchanged this cycle | Over-conservative beats OOM; re-fit needs a calibration window with P4 data |
