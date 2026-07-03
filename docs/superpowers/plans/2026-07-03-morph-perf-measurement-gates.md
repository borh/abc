# Morph-Pipeline Measurement Gates Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give the three measurement-gated perf findings (§3.5 warehouse batch sharding, §3.12 Parquet compaction, §3.15 per-worker analyzer parallelism) a real measurement harness and a baseline, then make a TUNE / DEFER decision for each from evidence — without writing any tuning blind.

**Architecture:** Add a `benchmarks/run-morph-corpus.sh` that runs `ab-morph-run analyze-aat` over a *stable* persisted AAT corpus (so we measure only the morph + warehouse pipeline, not adapter variance), capturing wall time, peak RSS, Parquet part count per table, row-group counts, and a jobs-scaling sweep. Capture a same-machine baseline into `benchmarks/baselines/`, then write a decision doc that classifies each finding. Tuning itself is a follow-up plan triggered by the decision, not a step here.

**Tech Stack:** Bash, `command time -v` (peak RSS), `duckdb` (Parquet metadata via `parquet_metadata()`), `jq`, release `ab-morph-run`, the persisted aozora2html AAT corpus.

## Global Constraints

- **Measure, do not tune, in this plan.** No change to `WAREHOUSE_REGULAR_BATCH_SIZE`, the merge path, or analyzer scheduling. Those are follow-up plans unlocked by Task 3.
- **Reuse the stable AAT corpus.** Default input is `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter` (17,689 AAT JSON files, already verified present). Do not regenerate adapters in the bench — that would couple morph measurement to adapter variance.
- **Same-machine baselines only.** Write baselines into `benchmarks/baselines/` and treat them as comparison points for this machine, not portable promises (matches the existing `benchmarks/README.md` convention).
- **Do not commit generated warehouse output.** The bench writes the warehouse under `${AB_BENCH_OUT:-/tmp/ab-validator-morph-bench-...}` and cleans it up; only `summary.json` and the Markdown baseline may be committed.
- **Dictionaries are external.** Vibrato uses the repo-root `dictionary` symlink; sudachi needs `AB_SUDACHI_DICT`. Both are read-only inputs, never committed.
- **Behavior-preserving for existing code.** Tasks 1–2 add a new bench script and a baseline doc; they touch no production crate. Task 3 is a doc.
- **Edition 2024, resolver v2.** No new crate deps in this plan.

---

## Hammock Synthesis

Current settled facts (verified 2026-07-03):

- `benchmarks/run-full-corpus.sh` measures only `ab-index` + `ab-check` (the aozora2 adapter + validation pipeline). It never invokes `ab-morph-run`. So §3.5 / §3.12 / §3.15 — all `ab-morph-run` / `ab-warehouse` concerns — have **no measurement harness today**. The audit's "Measure with `benchmarks/run-full-corpus.sh`" advice was correct in spirit but the harness doesn't cover the morph pipeline.
- The morph run is invoked `cargo run --release -p ab-morph-run -- analyze-aat --aat-dir <dir> --analyzer vibrato --analyzer sudachi-a --analyzer sudachi-c --warehouse-dir <dir> --run-id <id> --warehouse-profile full --jobs <n>` (verified `justfile:300`, `crates/ab-morph-run/src/main.rs:17–75`).
- A stable AAT corpus exists on disk: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter` with 17,689 AAT JSON files (verified `find … -name '*.json' | wc -l` = 17689).
- Warehouse on-disk layout is `<warehouse-dir>/runs/<run-id>/<table-dir>/part-XXXXX-YYYYY.parquet` or `part-XXXXX.parquet` (verified `crates/ab-warehouse/src/writer.rs:506,512`). So part counting is `find … -name '*.parquet'` and per-table row/row-group stats come from duckdb `parquet_metadata`.
- Peak RSS is reachable via `command time -v` (verified: prints `Maximum resident set size (kbytes)`).

Crux: the three findings' evidence signals are different and each has a clean threshold:

- **§3.5** (`WAREHOUSE_REGULAR_BATCH_SIZE = 32`, `crates/ab-morph-run/src/lib.rs:38`, `take_batch` at `pipeline.rs:990`): the signal is *most shards produce less than one row group*. Row-group size is 50k. If shard_count ≫ total_morpheme_rows / 50k, most shards are sub-row-group and sharding is pure overhead. TUNE iff shard_count > 4 × max(1, total_morpheme_rows / 50k).
- **§3.12** (no compaction in `merge_warehouse_shard_runs`, `pipeline.rs:1054`; coalescing machinery `append_parquet_table_file` at `ab-warehouse/src/writer.rs:490+` unused in prod): the signal is *many small parts per table after merge*. TUNE iff any high-row table has > 64 parts and median part byte size < 1 MiB.
- **§3.15** (analyzers serialize within a worker, `pipeline.rs:582` `for analyzer in analyzers` inside `std::thread::scope`): the signal is *jobs scaling plateaus*. Run jobs=1 vs jobs=N. TUNE iff speedup < 0.6 × N on a corpus with ≥2 analyzers.

Chosen direction: build the harness that produces exactly these three signals, capture one baseline run, write the decision. If a finding is TUNE, its tuning becomes a separate plan (so the tuning diff is reviewed on its own, with a before/after measurement in the same harness).

---

## Task 1: Add The Morph-Pipeline Measurement Harness

**Files:**
- Create: `benchmarks/run-morph-corpus.sh`
- Create: `tests/morph-corpus-bench-smoke.sh`
- Read: `justfile` (analyzers + env pattern at lines 295–311)
- Read: `crates/ab-morph-run/src/main.rs:17–75` (CLI surface)
- Read: `crates/ab-warehouse/src/writer.rs:490–520` (part file naming)

**Interfaces:**
- Consumes: the release `ab-morph-run` binary; the stable AAT corpus dir; the `duckdb`, `jq`, and `command time -v` tools (all verified present in the nix dev shell).
- Produces: `benchmarks/run-morph-corpus.sh` (executable) that writes a `summary.json` with the fields listed in Step 4, and a `parts.json` with per-table Parquet part counts.

- [ ] **Step 1:** Write the bench harness.

```bash
#!/usr/bin/env bash
# Measure the ab-morph-run analyze-aat pipeline over a stable persisted AAT
# corpus. Captures wall time, peak RSS, Parquet part counts, row/row-group
# stats, and (optionally) a jobs scaling sweep for §3.15.
#
# Does NOT tune anything. Output feeds the §3.5/§3.12/§3.15 decision doc.
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
aat_dir="${AB_MORPH_AAT_DIR:-/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter}"
warehouse_dir="${AB_MORPH_WAREHOUSE_DIR:-/db/ab-validator/morph-warehouse-bench}"
jobs_list="${AB_MORPH_JOBS:-1 $(nproc)}"
analyzers="${AB_MORPH_ANALYZERS:-vibrato sudachi-a sudachi-c}"
out_dir="${AB_BENCH_OUT:-/tmp/ab-validator-morph-bench-$(date -u +%Y%m%dT%H%M%SZ)}"
cleanup_tmp=false
if [[ -z "${AB_BENCH_OUT+x}" ]]; then cleanup_tmp=true; fi

if [[ ! -d "$aat_dir" ]]; then
  echo "AAT corpus not found: $aat_dir" >&2
  exit 2
fi

mkdir -p "$out_dir" "$warehouse_dir"
if "$cleanup_tmp"; then trap 'rm -rf -- "$out_dir" "$warehouse_dir/runs/bench-*"' EXIT; fi
cd "$repo_root"

# Resolve the time binary (avoid the bash `time` keyword, which ignores -v).
time_bin="$(type -P time || true)"
if [[ -z "$time_bin" ]]; then
  echo "GNU time not found; peak RSS will be unavailable" >&2
  time_bin="command time"   # best-effort fallback
fi

echo "building release ab-morph-run"
cargo build --release -p ab-morph-run

sudachi_dict="${AB_SUDACHI_DICT:-}"
if [[ -z "$sudachi_dict" ]]; then
  sudachi_dict="$(nix path-info .#sudachi-dictionary-full 2>/dev/null)/share/sudachi/system.dic"
fi
export TMPDIR="${AB_DB_ROOT:-/db/ab-validator}/tmp"
export TMP="$TMPDIR"; export TEMP="$TMPDIR"

run_one() {
  local j="$1"
  local run_id="bench-j${j}"
  rm -rf "$warehouse_dir/runs/$run_id"
  local args=()
  for a in $analyzers; do args+=(--analyzer "$a"); done
  local timing="$out_dir/time-j${j}.txt"
  AB_SUDACHI_DICT="$sudachi_dict" \
    "$time_bin" -v \
    target/release/ab-morph-run analyze-aat \
      --aat-dir "$aat_dir" \
      "${args[@]}" \
      --warehouse-dir "$warehouse_dir" \
      --run-id "$run_id" \
      --warehouse-profile full \
      --jobs "$j" \
    >"$out_dir/j${j}.stdout" 2>"$timing"
  # wall = the time -v "Elapsed (wall clock) time" line, in seconds
  local wall
  wall="$(awk -F': ' '/Elapsed \(wall clock\) time/ {print $2}' "$timing" \
          | awk '{printf "%.3f", $1*60+$2}')"
  local peak_kb
  peak_kb="$(awk -F': ' '/Maximum resident set size/ {print $2}' "$timing" | tr -d ' ')"
  echo "{\"jobs\":$j,\"wall_seconds\":$wall,\"peak_rss_kb\":$peak_kb}"
}

# §3.5 / §3.12 signals: part counts + per-table row and row-group stats from the
# jobs=$(nproc) run (representative full run).
primary_j="$(echo "$jobs_list" | awk '{print $NF}')"
echo "running primary measurement at jobs=$primary_j"
primary_json="$(run_one "$primary_j")"

warehouse_run_dir="$warehouse_dir/runs/bench-j${primary_j}"
parts_json="$out_dir/parts.json"
{
  echo "{"
  echo "\"total_parquet_parts\":$(find "$warehouse_run_dir" -name '*.parquet' | wc -l | tr -d ' '),"
  echo "\"total_parquet_bytes\":$(du -sb "$warehouse_run_dir" 2>/dev/null | awk '{print $1}'),"
  echo "\"tables\":["
  first=1
  for d in "$warehouse_run_dir"/*/; do
    [[ -d "$d" ]] || continue
    name="$(basename "$d")"
    part_count="$(find "$d" -name '*.parquet' | wc -l | tr -d ' ')"
    # duckdb parquet_metadata gives row_count + row_groups per file; aggregate.
    stats="$(duckdb -noheader -list -c "
      SELECT
        COUNT(*) AS files,
        COALESCE(SUM(row_groups),0) AS row_groups,
        COALESCE(SUM(total_compressed_size),0) AS compressed_bytes
      FROM parquet_metadata('$d/**/*.parquet');" 2>/dev/null || echo "0|0|0")"
    files="$(echo "$stats" | cut -d'|' -f1)"
    row_groups="$(echo "$stats" | cut -d'|' -f2)"
    bytes="$(echo "$stats" | cut -d'|' -f3)"
    [[ $first -eq 1 ]] || echo ","
    printf '  {"table":"%s","parts":%s,"files":%s,"row_groups":%s,"compressed_bytes":%s}' \
      "$name" "$part_count" "$files" "$row_groups" "$bytes"
    first=0
  done
  echo ""
  echo "]"
  echo "}"
} >"$parts_json"

# §3.15 signal: jobs scaling sweep (jobs=1 vs jobs=primary_j).
echo "running jobs scaling sweep over: $jobs_list"
scaling_json="["
first=1
for j in $jobs_list; do
  [[ "$j" -eq "$primary_j" ]] && sample="$primary_json" || sample="$(run_one "$j")"
  [[ $first -eq 1 ]] || scaling_json+=","
  scaling_json+="$sample"
  first=0
done
scaling_json+="]"

speedup="$(jq -r --argjson one "$(echo "$scaling_json" | jq '.[]|select(.jobs==1)|.wall_seconds')" \
  --argjson peak "$(echo "$scaling_json" | jq '.[]|select(.jobs=='$primary_j')|.wall_seconds')" \
  -n "(\$one / \$peak)")"

jq -n \
  --arg generated_at "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
  --arg aat_dir "$aat_dir" \
  --arg analyzers "$analyzers" \
  --argjson primary_j "$primary_j" \
  --argjson primary "$primary_json" \
  --slurpfile parts "$parts_json" \
  --argjson scaling "$scaling_json" \
  --argjson speedup "$speedup" \
  '{
    generated_at: $generated_at,
    aat_dir: $aat_dir,
    analyzers: $analyzers,
    primary_jobs: $primary_j,
    primary: $primary,
    parts: $parts[0],
    scaling: $scaling,
    scaling_speedup_primary_over_single: $speedup
  }' | tee "$out_dir/summary.json"

echo "summary: $out_dir/summary.json"
echo "parts:   $out_dir/parts.json"
```

  Save it as `benchmarks/run-morph-corpus.sh` and `chmod +x`.

- [ ] **Step 2:** Write a smoke test that runs the harness against a tiny synthetic AAT corpus.

  The smoke test must prove the harness wires up correctly (build, run, parse timing, count parts, emit summary.json) without needing the 17k-file corpus. Use the existing `tests/` fixture pattern: create 2 tiny AAT JSON files under a temp dir.

```bash
#!/usr/bin/env bash
# Smoke test for benchmarks/run-morph-corpus.sh: runs it against a tiny
# synthetic AAT corpus and asserts the shape of summary.json + parts.json.
set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf -- "$tmp"' EXIT
aat_dir="$tmp/aat"
mkdir -p "$aat_dir"
# Two minimal AAT JSON documents (shape must match ab-morph-run's parser).
cat >"$aat_dir/000001-aaa.json" <<'JSON'
{"version":1,"text_id":"000001","nodes":[{"kind":"text","value":"私は猫である。"}]}
JSON
cat >"$aat_dir/000002-bbb.json" <<'JSON'
{"version":1,"text_id":"000002","nodes":[{"kind":"text","value":"今日は良い天気だ。"}]}
JSON

out_dir="$tmp/out"
AB_MORPH_AAT_DIR="$aat_dir" \
AB_MORPH_WAREHOUSE_DIR="$tmp/warehouse" \
AB_MORPH_JOBS="1 1" \
AB_MORPH_ANALYZERS="vibrato" \
AB_BENCH_OUT="$out_dir" \
bash "$repo_root/benchmarks/run-morph-corpus.sh"

# Assert the harness produced both outputs with the required top-level keys.
jq -e '.primary.jobs == 1' "$out_dir/summary.json"
jq -e '.primary.wall_seconds | type == "number"' "$out_dir/summary.json"
jq -e '.parts.total_parquet_parts | type == "number"' "$out_dir/summary.json"
jq -e '(.scaling | length) == 2' "$out_dir/summary.json"
jq -e '.scaling_speedup_primary_over_single | type == "number"' "$out_dir/summary.json"
echo "smoke ok"
```

  Save as `tests/morph-corpus-bench-smoke.sh` and `chmod +x`.

- [ ] **Step 3:** Run the smoke test.

  ```bash
  bash tests/morph-corpus-bench-smoke.sh
  ```
  Expected: prints `smoke ok`. If it fails on the AAT JSON shape, fix the fixture in Step 2 to match the real AAT schema by copying one real document from `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter/` (use `head -c 400 <real-file>` to preview) — the harness itself is what's under test, so a real-shape fixture is acceptable.

- [ ] **Step 4:** Verify the primary-audit-signal fields are present.

  ```bash
  # Confirm summary.json carries the three decision inputs (§3.5/§3.12/§3.15).
  jq -e '.parts.total_parquet_parts' tests/../../tmp/*/out/summary.json 2>/dev/null || \
    jq -e '.parts.total_parquet_parts + .parts.tables[].row_groups + .scaling_speedup_primary_over_single' \
      "$(find /tmp -maxdepth 1 -name 'ab-validator-morph-bench-*' | tail -n 1)/summary.json"
  ```
  Expected: a number (the three signal fields add to a finite number). This step only checks shape; the real numbers come in Task 2.

- [ ] **Step 5:** Commit Task 1.

  ```bash
  git add benchmarks/run-morph-corpus.sh tests/morph-corpus-bench-smoke.sh
  git commit -m "test: add morph-pipeline measurement harness
  No tuning. Feeds §3.5/§3.12/§3.15 decision."
  ```

---

## Task 2: Capture The Baseline

**Files:**
- Create: `benchmarks/baselines/morph-2026-07-03.md`
- Generated (NOT committed): the warehouse output under `/db/ab-validator/morph-warehouse-bench/runs/bench-*`
- Read: `benchmarks/run-morph-corpus.sh` (from Task 1)
- Read: `benchmarks/README.md` (baseline convention)

**Interfaces:**
- Consumes: the stable AAT corpus and the harness from Task 1.
- Produces: a committed same-machine baseline Markdown doc with the raw `summary.json` + `parts.json` contents and a human-readable reading.

- [ ] **Step 1:** Run the harness on the full stable corpus with the default sweep (`1 <nproc>`).

  ```bash
  AB_MORPH_AAT_DIR=/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter \
  AB_MORPH_WAREHOUSE_DIR=/db/ab-validator/morph-warehouse-bench \
  AB_MORPH_JOBS="1 $(nproc)" \
  AB_BENCH_OUT="$(pwd)/benchmarks/baselines/morph-run-2026-07-03" \
  bash benchmarks/run-morph-corpus.sh
  ```
  Expected: prints a `summary.json` with `primary.jobs == $(nproc)`, non-zero `wall_seconds`, a `parts` block with per-table `row_groups` and `parts`, and a `scaling` array of length 2. This run takes roughly as long as a real morph corpus pass — plan for several minutes.

- [ ] **Step 2:** Capture the baseline doc.

  Create `benchmarks/baselines/morph-2026-07-03.md` with this exact structure, filling the numbers from the `summary.json` the harness just wrote:

  ```markdown
  # Morph-Pipeline Baseline — 2026-07-03

  Same-machine baseline (NOT a portable performance promise). Machine: <fill
  from `uname -srm` + `nproc`>. Input: 17,689 AAT JSON files from
  `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter`.
  Analyzers: vibrato, sudachi-a, sudachi-c. Warehouse profile: full.

  ## Raw measurement

  <!-- paste the contents of benchmarks/baselines/morph-run-2026-07-03/summary.json here -->

  ## Parquet part breakdown

  <!-- paste the contents of benchmarks/baselines/morph-run-2026-07-03/parts.json here -->

  ## Signal classification (one line each; detail in the decision doc)

  - §3.5 shard-size: shard_count = <N>, total_morpheme_row_groups ≈ <M> → TUNE / DEFER
  - §3.12 part compaction: max parts per table = <P>, median part bytes = <B> → TUNE / DEFER
  - §3.15 analyzer parallelism: jobs=1 → <T1>s, jobs=<N> → <Tn>s, speedup = <S> → TUNE / DEFER
  ```

- [ ] **Step 3:** Verify no generated warehouse output is staged.

  ```bash
  git status --short
  git diff --stat --staged
  ```
  Expected: only `benchmarks/baselines/morph-2026-07-03.md` (and optionally the raw `summary.json` / `parts.json` if you commit them alongside — keep them under `benchmarks/baselines/morph-run-2026-07-03/`). No files under `/db/ab-validator/morph-warehouse-bench/` should appear (that path is outside the repo).

- [ ] **Step 4:** Commit Task 2.

  ```bash
  git add benchmarks/baselines/morph-2026-07-03.md \
          benchmarks/baselines/morph-run-2026-07-03/summary.json \
          benchmarks/baselines/morph-run-2026-07-03/parts.json
  git commit -m "docs: morph-pipeline measurement baseline (same-machine)"
  ```

---

## Task 3: Publish The §3.5 / §3.12 / §3.15 Decision

**Files:**
- Create: `docs/superpowers/reports/2026-07-03-morph-perf-decision.md`
- Read: `benchmarks/baselines/morph-2026-07-03.md`
- Read: `docs/handoffs/crates-optimization-audit.md` §3.5, §3.12, §3.15

**Decision values per finding:**
- `TUNE`: the baseline crosses the finding's threshold (below). A tuning follow-up plan is unlocked.
- `DEFER`: the threshold is not crossed. Record the threshold and the measured value; revisit only if corpus shape changes.

**Thresholds (from the Hammock Synthesis; each is the audit's "measure it first" made explicit):**

| Finding | Site | TUNE iff |
| --- | --- | --- |
| §3.5 | `crates/ab-morph-run/src/lib.rs:38` (`WAREHOUSE_REGULAR_BATCH_SIZE = 32`), `pipeline.rs:990` `take_batch` | `shard_count > 4 × max(1, total_morpheme_rows / 50_000)` — i.e. most shards produce less than one row group of morphemes. Use `total_morpheme_rows` = sum of `row_groups × 50_000` upper bound, or read exact row count from duckdb if feasible. |
| §3.12 | `crates/ab-morph-run/src/pipeline.rs:1054` `merge_warehouse_shard_runs` + unused `append_parquet_table_file` (`ab-warehouse/src/writer.rs:490+`) | Any high-row table has `> 64` parts AND median part `compressed_bytes < 1 MiB`. |
| §3.15 | `crates/ab-morph-run/src/pipeline.rs:582` (`for analyzer in analyzers` inside `std::thread::scope`) | `scaling_speedup_primary_over_single < 0.6 × primary_jobs` on a corpus run with ≥2 analyzers. |

- [ ] **Step 1:** Compute the three verdicts from the baseline.

  ```bash
  summary="benchmarks/baselines/morph-run-2026-07-03/summary.json"
  echo "--- §3.5 ---"
  shard_count="$(jq '.parts.total_parquet_parts' "$summary")"
  # total morpheme rows ≈ sum of (row_groups × 50k) upper bound; compressed bytes is the size signal
  echo "shard_count (total parts) = $shard_count"
  echo "threshold = 4 × max(1, total_morpheme_rows/50000) — compute total_morpheme_rows from duckdb below"
  echo "--- §3.12 ---"
  jq '.parts.tables[] | {table, parts, compressed_bytes, median_part_bytes: (if .parts>0 then (.compressed_bytes/.parts) else 0 end)}' "$summary"
  echo "TUNE iff any table has parts > 64 AND median part bytes < 1048576"
  echo "--- §3.15 ---"
  jq '.scaling' "$summary"
  jq '.scaling_speedup_primary_over_single' "$summary"
  ```

  If `total_parquet_parts` over-counts (parts include both `part-XXXXX-YYYYY.parquet` and `part-XXXXX.parquet` variants), refine `shard_count` for §3.5 by counting distinct `(shard_index)` prefixes per table from the file names:

  ```bash
  # Distinct shard indices (the first 5-digit number in part-NNNNN-...)
  duckdb -noheader -list -c "
    SELECT COUNT(DISTINCT regexp_extract(filename, 'part-(\d{5})', 1))
    FROM parquet_metadata('/db/ab-validator/morph-warehouse-bench/runs/bench-$(nproc)/**/*.parquet');"
  ```

- [ ] **Step 2:** Write the decision doc.

  Create `docs/superpowers/reports/2026-07-03-morph-perf-decision.md` with this structure, filling each verdict from Step 1:

  ```markdown
  # Morph-Pipeline Perf Decision — 2026-07-03

  Decision-quality measurement for the three measurement-gated findings in
  `docs/handoffs/crates-optimization-audit.md`. Baseline:
  `benchmarks/baselines/morph-2026-07-03.md`.

  ## §3.5 — WAREHOUSE_REGULAR_BATCH_SIZE = 32

  - Site: `crates/ab-morph-run/src/lib.rs:38`; `take_batch` at `pipeline.rs:990`.
  - Measured: shard_count = <N>, total_morpheme_rows ≈ <M>.
  - Threshold: shard_count > 4 × max(1, total_morpheme_rows / 50_000).
  - Verdict: <TUNE | DEFER>.
  - If TUNE: follow-up plan will make `WAREHOUSE_REGULAR_BATCH_SIZE` a CLI/env
    knob (default unchanged), sweep 32 / 256 / 1024 / 2048, re-measure, and pick
    the new default is a separate perf
    commit, not part of this plan.

  ## §3.12 — No Parquet compaction on merge

  - Site: `crates/ab-morph-run/src/pipeline.rs:1054` `merge_warehouse_shard_runs`;
    unused coalescing at `ab-warehouse/src/writer.rs:490+`.
  - Measured: <per-table parts + median part bytes, copied from Step 1>.
  - Threshold: any high-row table has > 64 parts AND median part < 1 MiB.
  - Verdict: <TUNE | DEFER>.
  - If TUNE: follow-up plan will add an optional coalescing pass in
    `merge_warehouse_shard_runs` for high-row tables, gated on shard/row count,
    using the existing tested `append_parquet_table_file`. zstd-3 + 50k row
    groups stay as defaults.

  ## §3.15 — Analyzers serialize within a worker

  - Site: `crates/ab-morph-run/src/pipeline.rs:582` (`for analyzer in analyzers`
    inside `std::thread::scope`).
  - Measured: jobs=1 → <T1>s, jobs=<N> → <Tn>s, speedup = <S>.
  - Threshold: speedup < 0.6 × N on a ≥2-analyzer corpus.
  - Verdict: <TUNE | DEFER>.
  - If TUNE: follow-up plan will run the K analyzers concurrently within a
    worker via `rayon::scope`/`thread::scope` (all analyzers are `&self`-
    shareable). Ceiling = 3× on a single large work. Re-measure with this same
    harness; only commit if the large-work tail shrinks measurably.

  ## Next-allowed plans

  For each finding marked TUNE, a separate implementation plan may be written.
  No tuning diff lands in this plan.
  ```

- [ ] **Step 3:** Verify Task 3.

  ```bash
  rg -n "§3\.5|§3\.12|§3\.15|TUNE|DEFER" docs/superpowers/reports/2026-07-03-morph-perf-decision.md
  jq -e '.parts.total_parquet_parts' benchmarks/baselines/morph-run-2026-07-03/summary.json
  ```
  Expected: each finding has a TUNE or DEFER line; the baseline summary.json still parses.

- [ ] **Step 4:** Commit Task 3.

  ```bash
  git add docs/superpowers/reports/2026-07-03-morph-perf-decision.md
  git commit -m "docs: §3.5/§3.12/§3.15 morph-perf decision (TUNE/DEFER per baseline)"
  ```

---

## Task 4: Keep Existing Gates Green

**Files:**
- Read: `benchmarks/run-morph-corpus.sh`
- Read: `tests/morph-corpus-bench-smoke.sh`

- [ ] **Step 1:** Run the new harness smoke test.

  ```bash
  bash tests/morph-corpus-bench-smoke.sh
  ```
  Expected: `smoke ok`.

- [ ] **Step 2:** Run the pre-existing measurement/gate smoke tests (no regression).

  ```bash
  bash tests/aat-parser-ir-mapping-smoke.sh
  bash tests/aozora2html-aat-full-smoke.sh
  cargo test --workspace
  cargo fmt --all -- --check
  ```
  Expected: all pass; the new bench script is bash-only and adds no Rust deps, so the workspace test count is unchanged.

- [ ] **Step 3:** Confirm no generated artifacts are staged.

  ```bash
  git status --short
  ```
  Expected: clean (only the committed bench script, baseline doc, and decision doc are on the branch).

- [ ] **Step 4:** This task has no required commit when the gate commands are clean. If Step 2 surfaces a regression, stop and inspect before committing.

---

## Follow-Up Plan Trigger

For each finding that Task 3 marked `TUNE`, write a separate implementation plan (under `docs/superpowers/plans/`) that:

1. Starts from the failing measurement (re-run `benchmarks/run-morph-corpus.sh` and quote the before-number).
2. Makes the smallest change at the exact site named in Task 3.
3. Re-runs the harness and commits only if the after-number improves without regressing the other two signals or peak RSS.
4. Keeps the change in a standalone behavior-preserving-or-perf commit (no semantic change folded in).

Do not start any TUNE follow-up before Task 3 records its verdicts. DEFER findings stay recorded with their threshold so a future corpus-shape change can re-trigger measurement.
