# Workspace crates — optimization audit

**Scope:** read-only optimization audit (performance + simplification) of all 13
workspace crates in `crates/`. Proposal-only — no crate was edited.
**Method:** three parallel deep-dive passes (morphology pipeline, IR/text
foundation, CLI/harness) under the `rich-hickey-review` (design lens) and
`codebase-simplification` (audit mode) skills. Every finding carries `file:line`
evidence.
**Pre-existing context:** `docs/handoffs/crate-classification.md` (design
classification). This audit extends it on the *optimization* axis and corrects
two of its premises (see §0).

---

## 0. Corrections to the existing classification doc

Two claims in `docs/handoffs/crate-classification.md` do **not** hold against the
current source:

| Claim in classification doc | Verified reality | Evidence |
| --- | --- | --- |
| `ab-oracle`'s `ab-check` dep is "currently unused in source" (§1a) | **LIVE, not stale.** `ab-oracle` uses `ab_check::check::validate_aat_value` for the `schema_status` oracle axis. | `crates/ab-oracle/src/evaluate.rs:19` (+ test `:285`) |
| `ab-morph-diff` duplicates `ab-diff-utils` hashing/`first_difference` (implied cleanup candidate) | **No duplication.** `ab-morph-diff` keeps its own diff model; the actually-duplicated logic is the **zip+deflate+shift-jis+sha256 source loader**, copy-pasted across `ab-check`/`ab-coverage`/`ab-index`. | `rg "first_difference|Sha256|hash_string_sequence|FrequencyTable" crates/ab-morph-diff/src/` → 0 hits; cf. §3.3 |

Additionally, a full dead-dependency sweep across all 13 `Cargo.toml`s returned
**zero unused deps**. The "heavy unused deps" premise is not borne out. Minor
cleanup levers exist (see §4) but no dead-weight to delete.

---

## 1. The headline: one O(n²) bug duplicated across 8+ sites

This is the single highest-impact finding in the workspace. It is one logical
defect — converting a single char↔byte offset by scanning the text prefix —
copy-pasted across **three crates** and invoked on every hot path.

**Primitive:** `source[..byte_offset].chars().count()` or
`text.char_indices().nth(i)` to convert *one* offset, each call O(prefix length).

### 1.1 Morphology: validation re-derives char spans per morpheme → O(K·M·N)
- **`crates/ab-morph-diff/src/validate.rs:89-90`** (`byte_span_to_char_span`),
  called from `:56` inside the per-morpheme loop; entry points
  `crates/ab-morph-diff/src/lib.rs:35` (`compare_pair` → `validate_analysis`)
  and `crates/ab-morph-diff/src/nway.rs:43-50` (validates *every* analysis
  before alignment). Warehouse path: `crates/ab-morph-run/src/warehouse/rows.rs:131,155`.
- For a document with M morphemes over N chars, `validate.rs:101-102` does two
  `.chars().count()` per morpheme → **O(M·N)** per analysis; n-way/warehouse
  validate all K analyses → **O(K·M·N)**. On multi-MB honbun this is
  seconds→minutes per work. Tokens already carry correct `char_span` from the
  adapters — the recomputation re-derives them by scanning raw bytes.

### 1.2 Same primitive, 8 more call sites (construction time too)
- **`crates/ab-morph-analyzers/src/span_builder.rs:106-107,131`**
  (`byte_to_char_offset`, called **twice per token**).
- **`crates/ab-morph-analyzers/src/vaporetto.rs:246-270`**
  (`char_range_to_byte_range` → `char_offset_to_byte_index`, O(M²)).
- **`crates/ab-morph-run/src/warehouse/rows.rs:269-276`** (`byte_offset_for_char`).
- `crates/ab-morph-run/src/compact.rs:427,770,775`; **`crates/ab-morph-run/src/nway.rs:419`**;
  `crates/ab-morph-diff/src/lib.rs:169-174`, **`align.rs:152`**, `stats.rs:192-197`.
- The adapters emit the very morphemes that become the data validated in §1.1 —
  so the O(n²) cost is paid at construction time too.

### 1.3 Redundant `source_text.chars().count()` re-passes
- Independently recomputed ≥3–5× per document: `ab-morph-diff/src/align.rs:7`,
  `nway.rs:58,418`, `streaming.rs:25`, `stats.rs:24`, `lib.rs:81`, and again at
  `ab-morph-run/src/warehouse/rows.rs:31`. The char length of a `&str` is fixed
  once known.

### Fix (one change kills §1.1–§1.3)
Build `char_to_byte: Vec<usize>` (length = char count + 1) **once per document**
in the analyzer adapter, thread it through a small `SourceText` newtype (holds
`&str` + the table + precomputed `source_len`) into
`build_morphemes_from_tokens` / `visit_nway_regions_with_source_text` /
`validate_analysis_against_source` / `align_*` / `stats` / row builders. All
conversions become O(1) indexing and the 8 duplicated `char↔byte` helpers
collapse to two functions on `SourceText`. **No on-disk format change** —
`char_span`/`byte_span` values are unchanged, only computed faster.
- **Severity: Blocker [perf] + [simplification]** — start here. Pin with the
  existing `build_analysis_from_tokens`, `validate_analysis`, and
  `compact_streaming_stats_match_full_comparison`
  (`ab-morph-diff/src/streaming.rs:223`) tests first.

---

## 2. IR/text foundation (ab-ir · ab-plaintext · ab-source-syntax)

### 2.1 Four independent full-tree walks per document in the adapter hot path [perf] — Strong
- `crates/ab-ir/src/lib.rs:637` (`blocks_to_aat_projection`), `:703`
  (`visible_projection`), `:1048` (`provenance_counts`);
  `crates/ab-ir/src/semantic_summary.rs:52` (`semantic_summary`). Adapter calls
  all four in `adapters/aozora-rs/src/lib.rs:48,93,110-111,400,451,506`.
- `semantic_summary` re-runs `inline_visible_text` (fresh `String`) per
  ruby/style/figure (`semantic_summary.rs:73,87,107,162,177`); projection calls
  it **twice per ruby** (`lib.rs:749,805`) plus a `contains_unresolved_gaiji`
  sub-walk per ruby (`lib.rs:831-859`). Tree is walked ≥4× plus N sub-walks.
- **Fix:** one fused `InlineVisitor` that emits AAT JSON, accumulates
  `ProvenanceCounts`, builds `SemanticSummary`, and collects `visible_text` in a
  single traversal. The visitor trait (`lib.rs:114-145`) already abstracts the
  walk. Subsumes §2.2 and §2.5.

### 2.2 `inline_visible_text` allocs a fresh `String` per ruby/gaiji [perf] — Strong
- `crates/ab-ir/src/lib.rs:1142-1146` (`VisibleCollector { out: String::new() }`);
  no `with_capacity`, no reuse. O(R+figures+jisage) transient allocations, each
  recomputed again later by `visible_projection`.
- **Fix:** `String::with_capacity` from a heuristic; better, thread one reusable
  buffer through the visitor (`clear()`/`truncate`). Part of §2.1's fused visitor.

### 2.3 Hand-rolled byte scan, no fast-skip in tokenizer hot loop [perf] — Strong
- `crates/ab-source-syntax/src/lib.rs:97-260` (`source_events`) advances
  `offset += ch.len_utf8()` one char at a time, branching on ~8
  `rest.starts_with(...)` per iteration even on long plain-text runs.
  `marker_end_on_same_line` (`:540-558`) and friends are `chars()` linear scans.
  `needs_lossy_projection` (`:303-305`) already proves the fast-skip pattern works
  but the main loop doesn't use it. `memchr` is not a workspace dep.
- `ab-source-syntax` is the zero-dep foundation crate (a deliberate, documented
  property).
- **Fix (no new dep):** `str::find` on the marker-lead bytes (`※`,`｜`,`《`,`［`,`[`,`」`)
  to jump to the next candidate; std `str::find` already uses memchr internally
  for single-char patterns. Large constant-factor win, no dep added.
- Demote to Follow-up if the zero-dep property is a hard contract the team wants
  to protect by keeping the loop literally dependency-free (the `str::find` route
  respects it — only reject a `memchr`-crate route).

### 2.4 Recursive re-tokenization + unbounded recursion [perf + simplification + correctness] — Strong
- `crates/ab-source-syntax/src/lib.rs:90`: `comparison_lossy_body(base)`
  re-enters the full `source_events` state machine over a base substring that
  the enclosing pass already scanned. `command_end_on_same_line` (`:560-597`)
  recurses for nested `［＃…］` (`:573,580,586`) with no depth bound.
- Two costs: (a) re-scan waste; (b) stack-overflow risk on adversarial /
  pathological nesting (relevant if input is ever untrusted).
- **Fix:** project the ruby base from the already-emitted events rather than
  re-tokenizing; convert `command_end_on_same_line`'s recursion to an explicit
  stack / depth counter. Tests at `:714,723,747` pin current output.

### 2.5 Duplicate visible-text implementations [simplification] — Strong (delete) / Follow-up (unify)
- `crates/ab-ir/src/lib.rs:1003-1046`: `VisibleCollector` **and** a dead
  parallel `collect_visible`/`Vis` doing the identical thing. Plus
  `crates/ab-plaintext/src/aat.rs:130-205` is a *third* hand-written visible-text
  walk over the JSON form.
- **Fix (cheap):** delete `collect_visible` + `Vis`, route `visible_projection`
  through `VisibleCollector`. **Defer** the IR-vs-JSON unification — it inverts
  the perf direction of §2.1, so it depends on the §2.8 boundary decision.

### 2.6 Honbun path forces `.into_owned()` even when `Cow::Borrowed` possible [perf] — Nit
- `crates/ab-plaintext/src/aozora.rs:11` calls
  `comparison_lossy_body(&body).into_owned()` unconditionally; marker-free honbun
  (rare) still copies. `canonicalize_line_endings` (`lib.rs:48`) also loses the
  borrow at the boundary. Niche; only fix if §2.3 is being touched.

### 2.7 Three copies of gaiji-marker scanning [simplification] — Follow-up
- `crates/ab-source-syntax/src/lib.rs:135-170` (`source_events`),
  `:347-403` (`collect_gaiji_markers`, `gaiji_marker_count`). Re-implement the
  same `※［＃`/`※[#` + `marker_end_on_same_line` state machine thrice.
- **Fix:** `gaiji_marker_count` →
  `source_events(body).iter().filter(|e| matches!(e.kind, SourceEventKind::Gaiji{..})).count()`;
  verify it's not hot first.

### 2.8 Adapter-contract drift: `block_content_mut` called by adapter, absent from crate [simplification + protocol] — Strong
- `adapters/aozora-rs/src/aat.rs:464,642,753` calls `ab_ir::block_content_mut`;
  **`grep "fn block_content_mut"` over the whole repo → zero matches** (only
  `block_content(&Block) -> &[Inline]` exists at `lib.rs:1180`). The adapter is
  excluded from the workspace (`Cargo.toml:18-23`) so this doesn't break the
  build, but the Rust-typed adapter contract is **already broken in one
  direction** — the concrete, file:line instance of the classification doc's
  "versioning gap" (§3).
- **Decision required before §2.9:** the adapter boundary is JSON-only (per
  classification §3; `aozora2`/`aozora2html` already emit JSON by hand) **or**
  Rust-typed. If JSON-only, `visible_projection`/`provenance_counts`/
  `block_content_mut` become internal helpers and §2.9's API shrink is a real
  win. Route to `hammock-driven-design` / `rich-hickey-review` before any edit.

### 2.9 Single-use / dead builders [simplification] — Strong (delete `dakuten_gaiji`) / Follow-up (ruby ladder)
- `crates/ab-ir/src/lib.rs:513` `Inline::dakuten_gaiji` has **zero production
  callers** (only `:1276` test). `Inline::gaiji` is test-only too. The ruby
  constructor ladder `:402-464` has 5 entry points for one variant.
- **Fix:** delete `dakuten_gaiji` (rung 2 of the simplification ladder). Collapse
  the ruby ladder only if §2.8 keeps `ab-ir` as the contract — gate on §2.8.

### 2.10 `select` per-segment alloc; `**` materializes all objects [perf] — Follow-up
- `crates/ab-ir/src/aat_view.rs:62-126`: `current`/`next` reallocated per segment;
  `**` eagerly collects every object into a `Vec<&Value>`. Fine for typical AAT
  sizes; only worth a buffer-reuse/lazy-iterator if the oracle's call pattern
  proves hot. Verify oracle call counts first.

---

## 3. Morphology pipeline extras (ab-morph-run · ab-morph-diff · ab-morph-analyzers · ab-warehouse)

(Findings §3.1–§3.3 assume §1's `SourceText` fix is the prerequisite perf change.)

### 3.1 `FeatureMap` O(K²) per morpheme + 28 redundant `Arc<str>` allocations [perf] — Strong
- `crates/ab-morph-diff/src/model.rs:35-99` (`insert` → `binary_search` +
  `Vec::insert`, O(K) shift per call); `crates/ab-morph-analyzers/src/features.rs:34-67`
  inserts 28 keys/morpheme (fixed unidic set) yet re-allocates each key as
  `Arc<str>` via `(*key).into()` (`features.rs:38`; keys are `&'static [&str]`).
- **Fix:** intern the unidic keys into `FeatureKey`/`&'static str` once; bypass
  `binary_search`/`insert` for the fixed-profile fast path (keys are a fixed
  ordered list), keep the generic path for `field_N` extras; consider `SmallVec`.

### 3.2 Warehouse rows clone 4 constant strings per morpheme (per feature) [perf] — Strong
- `crates/ab-morph-run/src/warehouse/rows.rs:55-66` (4 `.to_owned()`/`.clone()`
  × every morpheme) and `rows.rs:84-99` (~36–112 clones × every morpheme via
  features). `run_id`/`source_id`/`text_id`/`analyzer_id` are constant across
  the whole batch.
- **Fix:** build the Arrow `StringArray` from `&str` borrowed from a single
  `Arc<str>` per column (writer's `string_array` already takes
  `Iterator<Item=&str>`, `writer.rs:648`); or hold `Arc<str>` in the row structs.
  Parquet bytes unchanged. `WAREHOUSE_MORPHEME_ROW_BATCH_SIZE = 50_000` (`lib.rs:37`)
  — the strings are constant across all batches of one analysis.

### 3.3 Copy-pasted zip+deflate+shift-jis+sha256 source loader [simplification + correctness] — Strong
- `crates/ab-check/src/check.rs:430-497` (`read_indexed_source_bytes`,
  `read_zip_entry_bytes`) + `crates/ab-check/src/encoding.rs:16-55`; the same in
  `crates/ab-index/src/index.rs:330-383` + `encoding.rs:18-58` (byte-for-byte
  identical except tests); `crates/ab-coverage/src/prevalence.rs:185,209-253` has
  a *fourth* `decode_source` that has **already drifted** (drops the lossy-tag
  and `raw_sha256` that `ab-check` computes → corpus-side decoding silently
  diverges).
- **Fix:** extract a small `ab-source-io` (or extend `ab-plaintext`, which
  already owns `from_aozora_honbun_bytes`) exposing `decode_source_bytes`,
  `hex_sha256`, `read_indexed_source_bytes(corpus_root, path) -> Vec<u8>`.
  Characterize the 4 current behaviors first — the drift is a latent bug, not
  just duplication.

### 3.4 Braided `run_analyze_aat_serial` (~250 lines, 6 writers, 3 duplicated error branches) [simplification] — Strong
- `crates/ab-morph-run/src/pipeline.rs:388-680`. Holds 6 writer states + the
  warehouse writer; three copy-pasted error-routing branches
  (`read_aat` fail `:481-503`, `project_aat` fail `:505-523`, `analyze` fail
  `:528-560`) — the highest-risk copy-paste (the blocks have already drifted:
  `read_aat` passes `None` for analyzer/text, `analyze` passes them).
- This is the braided-concern candidate the classification doc flagged (§5),
  here quantified. It is rung 5–6 of the simplification ladder.
- **Fix (behavior-preserving):** extract an `ErrorRouter`/`SinkSet` value holding
  the optional writers + warehouse option once, with one
  `record_error(source_id, text_id, analyzer, stage, code, error)` method;
  inline the three call sites to one call. Do **not** restructure the analysis
  loop in the same commit.
- Unblocks safely raising `WAREHOUSE_REGULAR_BATCH_SIZE` (§3.7) and the
  `SourceText` threading (§1).

### 3.5 `WAREHOUSE_REGULAR_BATCH_SIZE = 32` → excessive shards on full corpus [perf] — Strong
- `crates/ab-morph-run/src/lib.rs:38`; `take_batch` at `pipeline.rs:910`. Each
  shard opens **11 separate Parquet files** (`ab-warehouse/src/writer.rs:55-66`).
  On a ~17k-work corpus: ~530 shards × 11 tables = ~5,800 Parquet parts to
  create/rename/merge. Most regular shards produce *less than one row group* of
  morphemes each (row-group size is 50k).
- **Fix:** raise batch size to ~1k–5k (or a byte-budget like 200 MB/shard); the
  memory ceiling is the per-analysis `source_text.clone()` (§3.10), not the
  batch size. Keep the large-input lane (`pipeline.rs:945`) as-is. Verify with
  `benchmarks/run-full-corpus.sh` + peak RSS.

### 3.6 Repeated SHA-256 of unchanged bytes + double-digest in `compute_adapter_sha` [perf] — Strong
- `crates/ab-coverage/src/cache.rs:51-95`: hashes path *and* content per file
  (`:88-91`), with a `to_string_lossy()` alloc per file. The length-prefixing
  technique to fold both into one hasher already exists in
  `ab-diff-utils/src/hashing.rs:38-47` (`hash_string_sequence`) but `ab-coverage`
  doesn't depend on `ab-diff-utils` (see §4.4).
- **Fix:** length-prefix path bytes + content bytes into a single `Sha256`;
  use `rel.as_os_str().as_encoded_bytes()` to skip the UTF-8 conversion + `String`
  alloc. **Caveat:** `adapter_sha` is the on-disk cache namespace (`cache.rs:23`),
  so this invalidates pre-existing cache entries (regenerable) — rotate the
  layout or bump a version prefix.

### 3.7 `ab-compare` serial `WalkDir`; no `rayon` at all [perf] — Strong
- `crates/ab-compare/src/aat_diff.rs:152-176` and `lib.rs:96-127` walk
  single-threaded; `rayon` not even in `ab-compare/Cargo.toml`. `ab-index` and
  `ab-coverage` parallelize the identical pattern. `ab-compare` owns the heaviest
  per-file work (SHA of normalized visible strings, semantic sequences, BTreeMap
  counting).
- **Fix:** add `rayon`, `par_iter()` over collected paths, merge into the
  `BTreeMap<String,AatSummary>`. The `difference_limit` short-circuit
  (`lib.rs:131-134`) stays — limit only the difference-emission loop.

### 3.8 `ab-compare/metrics.rs` rebuilds 3 BTreeMaps + 3 clones per AAT file [perf] — Strong (cheapest win)
- `crates/ab-compare/src/metrics.rs:128-176`: per file, `stages()` allocs a
  `BTreeMap<String,f64>` with 8 `String` keys, `node_counts()` another with 4,
  then `stage_map.clone()` ×3 (`:153,160,167`). Constants like
  `"decode".to_owned()` re-allocated per file.
- **Fix:** key the maps on `&'static str` (or a fixed `[(name,val);8]` slice);
  pass `&stage_map` to the three hotspot structs, `clone()` only at the final
  owned boundary. Pair with §3.7's parallelism. Pure local refactor, no
  protocol/dep change — the recommended **start-here** for the CLI cluster.

### 3.9 `detectors.rs` re-walks the AAT tree per detector ⇒ W×R walks [perf] — Strong
- `crates/ab-coverage/src/detectors.rs:160-176` (`walk`), called from `count_aat_kinds`/`count_kind`/`count_with` per detector; invoked W works × R rows (~60) from `prevalence.rs:90-99`.
- **Fix:** single-pass visitor that emits each node's `kind`/`style_type` to a `HashMap<kind, &[&Detector]>` fan-out; collapses W×R walks to W×1 + small per-node dispatch. Source-side regexes stay separate (they operate on the source string). Characterization tests at `detectors.rs:594-720`.

### 3.10 `analysis.source_text` cloned per analyzer; pair comparison clones the tree [perf + simplification] — Follow-up
- Each adapter does `source_text: document.text.clone()` (`ab-morph-analyzers` vibrato `:250`, sudachi `:152`, vaporetto `:135`) → K copies for K analyzers. The no-source-text `compare_pair` (`ab-morph-diff/src/lib.rs:31`) is the only reason `Analysis` owns it; `compare_pair_with_source_text` (`lib.rs:74`) already takes it separately. Memory ceiling that gates §3.5.
- **Fix:** `Analysis.source_text: Arc<str>` (shared with `PlainTextDocument`); keep `Serialize` identical. Consider deprecating the no-source-text `compare_pair`.

### 3.11 Warehouse merge re-reads Parquet footers for row counts it already knew [perf] — Follow-up
- `crates/ab-morph-run/src/pipeline.rs:713-723` (`parquet_table_row_count` per shard × `Errors`/`Sources`); `ab-warehouse/src/writer.rs:581-589` opens the file + builds a reader just to read `num_rows()`. ~2000 file opens on a 16-lane run, purely for two integers.
- **Fix:** carry `source_count`/`error_count` (already tracked in `run_analyze_aat_serial`) up to `merge_warehouse_shard_runs` via `WarehouseShardOutput` (`pipeline.rs:797`); sum from memory. Keep the footer-parse path as a test fallback.

### 3.12 No Parquet compaction on merge; many small parts hurt DuckDB scans [perf — needs measurement] — Follow-up
- `stage_parquet_table_part` (`ab-warehouse/src/writer.rs:601`) only *moves* parts; the coalescing machinery (`append_parquet_table_file`, tested at `writer.rs:823`) is unused in the prod merge. DuckDB is the warehouse's real consumer. zstd-3 + 50k row groups are fine defaults (don't change without measurement).
- **Fix:** add a coalescing pass in `merge_warehouse_shard_runs` for high-row tables, gated on shard/row count. Measure with `benchmarks/run-full-corpus.sh` first.

### 3.13 n-way `feature_groups` re-sort an already-sorted `BTreeMap`; surfaces re-cloned [perf + simplification] — Follow-up
- `crates/ab-morph-diff/src/nway.rs:242-243` (`sort().dedup()` after a `BTreeMap`); `:188` keys a `BTreeMap<Vec<String>,…>` on `surfaces.clone()`. Both go away once surfaces move to `Arc<str>`. Consolidate the three subgroupers into one `BTreeMap<(FeatureKey, Scope), …>` and delete the global sort.

### 3.14 `CompactComparison` summary row builders + 5 group-by closures duplicated [simplification] — Follow-up
- `crates/ab-morph-run/src/compact.rs:49-90`: `from_comparison` (hardcodes `ScriptCategory::Other`) vs `from_compact_comparison` (calls `classify_text`) — ~40-line field-by-field duplicates. Five `match group_by {…}` blocks across `summary/summary_body.rs` (`:271-272,318-319,468-469,645-646`).
- **Fix:** `ComparisonSummaryRow::from_stats(...)` collapses both builders; a `summary_key(group_by, &row)` helper kills the 5 `match` blocks. Trivial, behavior-preserving, test-covered (`compact.rs:583`).

### 3.15 Under-parallelism: analyzers run strictly sequentially within one worker [perf — measure first] — Follow-up
- `crates/ab-morph-run/src/pipeline.rs:527` (`for analyzer in analyzers`) inside each `std::thread::scope` worker; the K analyzers (vibrato/sudachi/vaporetto, all `&self`-shareable) serialize on a single large work. `jobs=16` leaves 15 cores idle on the large-work tail.
- **Fix (low-risk):** within a worker, run the K analyzers concurrently via `rayon::scope`/`thread::scope`. Ceiling = 3× on a single large work; only worth it if the large-work tail is a measurable corpus fraction. Measure first.

### 3.16 `#[cfg(test)] pub` on test-only builders; pub leaked into test API [simplification] — Nit
- `ab-morph-run/src/warehouse/rows.rs:44-48,82-86,122-134`, `ab-warehouse/src/writer.rs:395-435,553`. Tighten `pub` → `pub(crate)`.

---

## 4. CLI/harness extras (ab-check · ab-compare · ab-coverage · ab-index · ab-oracle · ab-diff-utils)

(Findings §3.6–§3.9 are the CLI-cluster performance cluster; this section holds the remainder.)

### 4.1 `validate_aat_value` is cached (good) — no validator-in-loop bug [perf — non-issue]
- `crates/ab-check/src/check.rs:24-29` uses `LazyLock<Validator>`. The task's
  "validator built in a loop" hypothesis is **false**. `validate_aat_value`
  (`:30-42`) does run the full meta-schema per work — inherent to the correctness
  floor, not a defect. No double-validation in `invoke_and_check:364-375` (verified:
  `aat` is mutated before `check_value`, single validate is correct).

### 4.2 `generate_taxonomy` compiles constant regexes inside per-call fns [perf] — Nit
- `crates/ab-coverage/src/bin/generate_taxonomy.rs:144,259,273,404,615-617`:
  `Regex::new(...).expect("constant regex")` per invocation. Hoist to
  `static RE: LazyLock<Regex>`. Offline tool, low impact. (Contrast:
  `ab-index/src/features.rs:50-53` and `detectors.rs:115` already do it right —
  compile once at registry construction.)

### 4.3 `aat_diff.rs` retains full `normalized_visible: String` per work [perf] — Follow-up
- `crates/ab-compare/src/aat_diff.rs:62,218-241`: kept for the optional
  `first_difference` snippet (`:148-151`, only when hashes differ). On a long-novel
  corpus this holds ~1.5k full books resident. Snippets are bounded to 48 chars
  (`first_diff.rs:42-49`), so retain only the first N KB, or re-derive lazily for
  the rare differing pair.

### 4.4 `ab-diff-utils` still a 5-export crate for one consumer [simplification] — Follow-up
- `crates/ab-diff-utils/src/lib.rs:1-7`; only consumers
  `crates/ab-compare/src/aat_diff.rs:7` and `triage.rs:3`. The classification doc
  already walked this back from "fake seam" to "optional inline" (§1b). The
  actionable lever: `hash_string_sequence` (`hashing.rs:38-47`) is exactly the
  technique §3.6 should reuse; either **inline the crate into `ab-compare`** (drop
  the workspace dep) **or** have `ab-coverage` adopt `hash_string_sequence` for
  `compute_adapter_sha` so the boundary earns a second consumer. Don't leave it
  half-shared.

### 4.5 Copy-pasted `mkdir -p + File::create + to_writer_pretty` [simplification] — Nit
- `crates/ab-compare/src/main.rs:39-43,53-58,67-72,82-87`;
  `crates/ab-coverage/src/bin/coverage.rs:172-176`;
  `ab-check/src/check.rs:243-256`; `ab-oracle/src/report.rs:13-24`,
  `audit.rs`. ~6 copies; `coverage.rs:173` already drifts (`.ok()` vs `?`).
- **Fix:** one `fn write_json_pretty<T: Serialize>(path, &T) -> Result<()>` per
  crate (a shared tiny crate for one 4-liner is over-engineering).

### 4.6 No unused Cargo dependencies anywhere [dead-dep — non-issue]
- Full sweep of all 13 `Cargo.toml`s: every declared dep has ≥1 production
  reference. `ab-oracle`'s `ab-check` dep is live (§0). `ab-coverage` needs both
  `toml` (matrix deserialize) and `toml_edit` (preserve comments in merge) — both
  genuine. `ab-compare`'s `rustc-hash` is used (`aat_diff.rs:30` `FxHashMap`).
  No `cargo-machete`-style cleanups available.

---

## 5. Ranked start-here list (decoupled ordering)

These are the changes worth doing first, ordered by impact-per-effort and with
their dependencies on each other / on the §2.8 boundary decision called out.

| # | Finding | Sev | Tags | Prereq |
| --- | --- | --- | --- | --- |
| **A** | §1.1–§1.3 char↔byte O(n²): precompute `SourceText` once per document, thread through morphology | Blocker | perf+simplification | characterize first |
| **B** | §2.1 fused single-pass IR visitor (subsumes §2.2, §2.5-partial, §2.6 of IR) | Strong | perf | pin 4 outputs first |
| **C** | §3.8 metrics.rs `&'static str` keys + `&stage_map` (cheapest local win) | Strong | perf | none |
| **D** | §3.7 ab-compare parallelize `read_aat_summaries` (+ add `rayon`) | Strong | perf | none |
| **E** | §3.3 extract zip+decode+sha256 loader (kills a latent drift bug) | Strong | simplification+correctness | characterize 4 behaviors |
| **F** | §3.1 intern unidic feature keys + fast-path FeatureMap build | Strong | perf | none |
| **G** | §3.2 warehouse rows: borrow constant strings into Arrow arrays | Strong | perf | none |
| **H** | §3.6 adapter_sha single-digest + `as_encoded_bytes` | Strong | perf | cache-layout rotation |
| **I** | §3.9 detectors single-pass visitor (W×R → W×1) | Strong | perf | existing tests cover |
| **J** | §3.5 raise `WAREHOUSE_REGULAR_BATCH_SIZE` (gated by §3.10 memory ceiling) | Strong | perf | measure RSS |
| **K** | §2.3 `str::find` fast-skip in `source_events` (no new dep) | Strong | perf | respect zero-dep property |
| **L** | §2.4 convert `command_end_on_same_line` recursion to explicit stack | Strong | perf+correctness | tests pin output |
| **M** | §3.4 extract `ErrorRouter`/`SinkSet` from `run_analyze_aat_serial` | Strong | simplification | pin 3 error paths first; unblocks J |
| **N** | §2.8 adapter-contract decision (JSON-only vs Rust-typed) | Strong | protocol | route to hammock/rich-hickey before §2.9 |

Follow-ups (lower blast radius, do after the above): §2.7, §2.9, §2.10, §3.10,
§3.11, §3.12, §3.13, §3.14, §3.15, §4.3, §4.4. Nits: §2.6, §3.16, §4.2, §4.5.

---

## 6. How this connects to the existing classification

- **`ab-morph-run` braided concerns** (classification §5) → quantified here as
  §3.4 (`run_analyze_aat_serial` + `ErrorRouter`); pairs with §3.5 (batch size)
  and §1 (`SourceText` threading).
- **`ab-ir` protocol/versioning gap** (classification §3) → concretized as §2.8
  (`block_content_mut` already absent); gates §2.9 (API shrink). Route to
  `hammock-driven-design` for the boundary decision.
- **`ab-diff-utils` fake-seam** (classification §1b, already relaxed) → §4.4;
  the actionable lever is sharing `hash_string_sequence` with §3.6, not deletion.
- **`ab-plaintext` two-format boundary** (classification §2) → re-confirmed
  justified; only the `Cow` nit §2.6 found.
- **`ab-oracle` stale `ab-check` dep** (classification §1a) → **corrected in §0**:
  the dep is live.

---

## 7. Verification discipline (per `codebase-simplification` / TDD skills)

Every perf/refactor change above is behavior-preserving and must be gated by
characterization tests *before* structure changes, then verified by:
- `cargo test -p <crate>` green;
- `benchmarks/run-full-corpus.sh` (`AB_CORPUS=references/aozorabunko`)
  before/after for `summary.json` work/second + peak RSS, for §1, §3.1, §3.2,
  §3.5, §3.7, §3.8, §3.9;
- `cargo bench -p ab-check --bench aat_json_io` / `-p ab-check --bench check_properties`
  / `-p ab-index --bench index_build` for §2.1, §2.3, §3.8.
Do **not** fold any behavior/bug (e.g., the §3.3 decode drift, the §2.4 recursion
bound) into the same commit as a perf refactor — file it separately per the
`codebase-simplification` "TRANSFORM vs VERIFY" rule.

---

## 8. Execution status (branch `crates-optimization-wt`)

Work executed in an isolated git worktree at `.worktrees/crates-optimization`
on branch `crates-optimization-wt` (off `main`). Each task is a separate
behavior-preserving commit, verified by the existing characterization tests +
full `cargo test --workspace` (0 failures) and `cargo clippy --workspace
--all-targets` (clean) after each stage.

### Done (6 commits)

| Task | Audit ref | Summary | Verification |
| --- | --- | --- | --- |
| `f4324b6` | §3.8 | `ab-compare/metrics.rs`: fixed stage/node `BTreeMap` keys → `&'static str`; per-file String-key allocations removed; the 3× `stage_map.clone()` now copies pointer values. | 18/18 `ab-compare` tests |
| `e9a8e4d` | §3.7 | `ab-compare`: parallelize `read_reports` + `read_aat_summaries` read/parse/summarize with `rayon` (the SHA-256-heavy step was serial). | 18/18 `ab-compare` tests |
| `ba84d4a` | §1.1–§1.2 | **Headline.** Add `ab_morph_diff::CharByteMap` (O(N) build, O(1) lookup); thread through `validate_analysis_against_source`, `build_morphemes_from_tokens`, `VaporettoAnalyzer::analyze`, `warehouse::rows::push_region_rows`. Eliminates the O(K·M·N) char↔byte scan duplicated across 3 crates. Two dead single-offset helpers deleted. | full workspace test green |
| `564799f` | §2.9 (partial) | `ab-ir`: delete dead `Inline::dakuten_gaiji` builder (zero prod callers); test rewritten via `gaiji_ref`. | 22/22 `ab-ir` tests |
| `1a27b4a` | §3.2 | `ab-warehouse` schema: `MorphemeRow` / `MorphemeFeatureRow` id columns → `Arc<str>`; builders share one `Arc` per column per analysis (refcount bump vs fresh String clone per morpheme/per feature). Parquet bytes unchanged. | full workspace test green |
| `86ef3fb` | §3.14 (partial) | `ab-morph-run/compact.rs`: collapse the two ~40-line duplicate `ComparisonSummaryRow` builders into a private `from_parts` helper. | 87/87 `ab-morph-run` tests |

### Deliberately deferred (with rationale)

These were **not** done unilaterally because the skills forbid folding a
behavior/contract/measurement decision into a behavior-preserving pass, or
because they require a product/architecture decision upfront.

| Audit ref | Why deferred |
| --- | --- |
| §2.8 (adapter contract: JSON-only vs Rust-typed) + §2.9 ruby-ladder collapse dependent on it | A **decision** is required first — route to `hammock-driven-design`. (The §2.8-independent `dakuten_gaiji` delete above was done.) |
| §3.3 (extract zip+decode+sha256 loader) | The dedup cannot be behavior-preserving: `ab-coverage`'s `decode_source` has **already drifted** from `ab-check`'s (drops the lossy-tag + `raw_sha256`). Unifying fixes a latent decode bug — per `codebase-simplification` "a behavior-preserving refactor and a bug fix are different commits." File the drift as a separate finding with a failing test + canonical-decode decision first. |
| §3.6 (single-digest `adapter_sha` + `as_encoded_bytes`) | Changes the on-disk **cache namespace** (`adapter_sha` is the cache key) → semantic change to the cache layer, not a refactor. Needs a cache-version bump + invalidation plan. |
| §3.5 / §3.12 / §3.15 | Explicitly "needs measurement" of the full-corpus bench (`benchmarks/run-full-corpus.sh`). Tuning `WAREHOUSE_REGULAR_BATCH_SIZE`, Parquet compaction, or per-document analyzer parallelism without measurement data would be guessing. |
| §3.1 (intern unidic feature keys) | Real per-morpheme alloc win, but wider surface (`FeatureMap` API + `features.rs` parsing + downstream). Tracked for a focused follow-up. |
| §3.9 (detectors single-pass visitor) | Real refactor (W×R → W×1 walks) needing the existing detector characterization tests; larger blast radius, deferred for a dedicated task. |
| §3.4 (`ErrorRouter` extraction from `run_analyze_aat_serial`) | Contained but a structural restructure of the braided 250-line runner; needs its three error-path characterization tests pinned first. Deferred for a focused simplification task. |
| §2.1 (fused single-pass IR visitor) | Single-file deep change touching projection + summary + provenance + visible; needs the four-output characterization tests reviewed before fusing. Deferred. |
| §2.3 (`str::find` fast-skip) / §2.4 (recursion→iterative) / §2.7 (gaiji scan dedup) | Foundation crate (`ab-source-syntax`) changes; §2.4 also has a correctness angle (unbounded recursion on adversarial input) that must be filed as a separate bug. Deferred. |
| §3.10 (`Analysis.source_text: Arc<str>`) / §3.11 (warehouse merge row counts) / §3.13 (nway feature_groups sort cleanup) / §1.3 (`chars().count()` single re-passes) / §2.6 / §3.16 / §4.2 / §4.3 / §4.4 / §4.5 | Smaller follow-ups; tracked in this audit, not blockers. |

### Worktree-only workaround (NOT committed)

The repo-root `dictionary` symlink is relative (`../vibrato-pipe/dictionary`),
which is correct for the canonical checkout but resolves to a non-existent
path inside `.worktrees/`. It was replaced with an **absolute** symlink in the
worktree only so the vibrato/sudachi dictionary-dependent tests resolve. This
change is intentionally **not committed** (host-specific absolute path); the
main checkout's relative symlink remains canonical.

### How to verify

```bash
cd .worktrees/crates-optimization   # or merge crates-optimization-wt
git log --oneline main..HEAD
cargo test --workspace          # 0 failures
cargo clippy --workspace --all-targets   # clean
```
