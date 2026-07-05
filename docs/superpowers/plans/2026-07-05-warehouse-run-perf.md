# Reliable and Faster Warehouse Runs Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Full-corpus `analyze-aat` warehouse runs complete unattended: shared `Arc<str>` source text removes the per-analysis memory multiplier, `--jobs 0` computes a memory-aware parallelism from `/proc/meminfo`, and shard staging moves to a run-owned `.staging/shards-<run_id>/` directory with PID+cmdline orphan cleanup.

**Architecture:** Three independent changes to the existing pipeline (spec: `docs/superpowers/specs/2026-07-05-warehouse-run-perf-design.md`). Task 1 is a model-type change in `ab-morph-diff` rippling mechanically through ~25 construction sites. Task 2 adds a pure-function auto-jobs module in `ab-morph-run`. Task 3 relocates the shard temp root and adds startup orphan cleanup. Tasks 4-5 are operational: constant calibration on a subset sweep, then the full-corpus validation run.

**Tech Stack:** Rust workspace; serde (needs `rc` feature for `Arc<str>`); `/proc/meminfo` + `/proc/<pid>/cmdline`; justfile recipes.

## Global Constraints

- Test command is ALWAYS `cargo test -p <crate> --features test-analyzer` for `ab-morph-run` (bare `cargo test -p ab-morph-run` false-fails 2 bin tests); plain `cargo test -p <crate>` for other crates.
- `cargo clippy -p <crate> --all-targets` (plus `--features test-analyzer` for ab-morph-run) must stay clean after every task.
- Exact tokens/formulas from the spec: `auto_jobs = clamp(1, nproc, floor(MemAvailable × 0.7 / per_job_bytes))`; `per_job_bytes = base_per_job + analyzer_count × per_analyzer_increment`; fallback `max(1, min(nproc / 4, 8))`; shard staging root `<warehouse_dir>/.staging/shards-<run_id>/`; liveness = `/proc/<pid>` exists AND its cmdline contains `ab-morph-run`.
- Placeholder constants until Task 4 calibrates: `base_per_job = 100 MiB`, `per_analyzer_increment = 512 MiB` (named constants, one definition site).
- The merge path's own staging (`.staging/{run_id}.{pid}`, no `shards-` prefix) is owned by `WarehousePaths`/`cleanup_stale_staging` and must NOT be touched by the new orphan scanner — the scanner only considers entries whose name starts with `shards-`.
- Worktree gotcha: repo-root `dictionary` is a tracked symlink to `../vibrato-pipe/dictionary` and dangles in worktrees. Tasks 4-5 run real analyses: first run `ln -sfn /home/bor/Projects/ab-validator/dictionary dictionary` in the worktree, and `git checkout -- dictionary` before committing (never commit the symlink modified or deleted).
- Commits end with the trailer: `🤖 Generated with [Claude Code](https://claude.com/claude-code)`

---

### Task 1: `Analysis.source_text` becomes `Arc<str>`

**Files:**
- Modify: `crates/ab-morph-diff/src/model.rs` (struct field, ~line 124)
- Modify: `crates/ab-morph-diff/Cargo.toml` and any crate whose serde derive now touches `Arc<str>` (add `rc` to serde features)
- Modify: `crates/ab-morph-run/src/pipeline.rs` (~line 737: share one Arc per document)
- Modify (compiler-enumerated, ~25 sites): construction sites in `ab-morph-diff` (lib.rs, streaming.rs, tests), `ab-morph-analyzers` (span_builder.rs and adapters vibrato/sudachi/vaporetto), `ab-morph-run` (compact.rs, nway.rs, warehouse/rows.rs tests), `adapters/aozora2html/src/source_derived.rs`, `adapters/aozora-epub3/src/source_derived.rs`, benches
- Test: existing suites (they exercise `source_text` heavily); no new test file

**Interfaces:**
- Consumes: nothing from other tasks.
- Produces: `pub struct Analysis { ..., pub source_text: Arc<str>, ... }` (`std::sync::Arc`). All later tasks and all consumers read it as `&str` via deref, unchanged.

- [ ] **Step 1: Change the model field**

In `crates/ab-morph-diff/src/model.rs` (imports at top of file):

```rust
use std::sync::Arc;
```

and in the struct at ~line 121:

```rust
pub struct Analysis {
    pub analyzer: AnalyzerId,
    pub text_id: TextId,
    pub source_text: Arc<str>,
    pub morphemes: Vec<Morpheme>,
    pub warnings: Vec<AnalyzerWarning>,
    /// Orthographic normalizations applied before tokenization.
    pub ortho_annotations: Option<Vec<ab_ortho_detect::OrthoAnnotation>>,
    /// Maps normalized-text byte ranges to original-text byte ranges.
    pub ortho_offset_map: Option<ab_ortho_detect::OffsetMap>,
}
```

Serde note: the workspace `Cargo.toml` (line 65) already declares `serde = { version = "1.0", features = ["derive", "rc"] }`, so `Arc<str>` serialization works for every crate inheriting via `serde = { workspace = true }`. Only if a member crate pins its own serde features without `rc` (the compiler will say so) add `rc` there.

- [ ] **Step 2: Let the compiler enumerate every construction site**

Run: `cargo check --workspace --all-targets 2>&1 | grep -c "expected .Arc<str>." ` — expect ~25 errors.

Fix each mechanically, by pattern:

```rust
// before                                   // after
source_text: s.to_owned(),                  source_text: Arc::from(s),
source_text: document.text.clone(),         source_text: Arc::from(document.text.as_str()),
source_text: "今日".to_owned(),             source_text: Arc::from("今日"),
```

Comparisons and reads (`&analysis.source_text`, `.len()`, `.chars()`, slicing) compile unchanged via deref; where a `&String` was expected, `analysis.source_text.as_ref()` yields `&str`. Do NOT restructure any call site beyond the minimal conversion.

- [ ] **Step 3: Share one Arc per document in the pipeline — BOTH texts**

Critical context (this is THE step that delivers the memory win; everything else in this task is mechanical): the existing `analysis.source_text = document.text.clone()` at ~line 737 fires **only in the ortho-remap-success arm** of `if let Some(ref map) = offset_map_opt`. Warehouse runs have ortho detection off, so that arm never runs and each analyzer-constructed `Analysis` keeps its own copy of the normalized text. Sharing must therefore cover the normalized text on every analysis, not just the original text on remap.

In `crates/ab-morph-run/src/pipeline.rs`, in `run_analyze_aat_serial`, immediately **after** the `norm_doc` construction (`let norm_doc = ab_plaintext::PlainTextDocument { ... }`, ~line 674 pre-change — `normalized_text` has been moved into `norm_doc.text` by then, so build the Arc from the struct field):

```rust
// One allocation per document, shared by every per-analyzer Analysis.
let shared_normalized: Arc<str> = Arc::from(norm_doc.text.as_str());
// The original text is only needed when ortho remap can fire.
let shared_original: Option<Arc<str>> =
    offset_map_opt.is_some().then(|| Arc::from(document.text.as_str()));
```

Inside the `for analyzer in analyzers` loop, immediately after the successful `analyzer.analyze(&norm_doc)` match arm binds `let mut analysis = ...`, add:

```rust
// Drop the analyzer's own text copy; share the per-document allocation.
analysis.source_text = Arc::clone(&shared_normalized);
```

And in the remap-success arm, change `analysis.source_text = document.text.clone();` to:

```rust
analysis.source_text = Arc::clone(
    shared_original.as_ref().expect("offset_map_opt is Some in this arm"),
);
```

(`Arc` is already imported in pipeline.rs.) Net effect per document: one `shared_normalized` allocation (plus one `shared_original` when ortho fired) regardless of analyzer count; each analyzer's internal copy is dropped the moment its analysis returns.

- [ ] **Step 4: Workspace green**

Run: `cargo test -p ab-morph-diff && cargo test -p ab-morph-analyzers && cargo test -p ab-morph-run --features test-analyzer && cargo test -p aozora2html-adapter -p aozora-epub3-adapter && cargo clippy --workspace --all-targets --features ab-morph-run/test-analyzer 2>&1 | tail -1`

Expected: all suites pass (ab-morph-run baseline: 127 lib + 28 bin; the trailing `Doc-tests ... 0 passed` line is the empty doctest target, not a failed run), clippy clean. If clippy flags `.as_str()` redundancy or similar in your conversions, fix as suggested.

- [ ] **Step 5: Commit**

```bash
git add -A && git commit -m "perf(morph-diff): share source_text via Arc<str> across analyses"
```

---

### Task 2: Memory-aware auto-jobs (`--jobs 0`)

**Files:**
- Create: `crates/ab-morph-run/src/auto_jobs.rs`
- Modify: `crates/ab-morph-run/src/lib.rs` (add `mod auto_jobs;`)
- Modify: `crates/ab-morph-run/src/pipeline.rs` (~line 112: replace the `jobs == 0` bail with resolution)
- Modify: `justfile` (`morph-warehouse-run-with-analyzers`: drop the `$(nproc)` substitution, pass 0 through)
- Test: unit tests inside `auto_jobs.rs`

**Interfaces:**
- Consumes: nothing from Task 1.
- Produces: `pub(crate) fn resolve_jobs(requested: usize, analyzer_count: usize) -> usize` (logs its decision via `eprintln!`), built on the pure `fn auto_jobs(nproc: usize, mem_available_bytes: Option<u64>, analyzer_count: usize) -> usize`.

- [ ] **Step 1: Write the failing tests**

Create `crates/ab-morph-run/src/auto_jobs.rs` with the test module first:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn budget_scales_with_memory_and_analyzers() {
        // 64 GiB available, 4 analyzers → per_job = 0.1 + 4×0.5 = 2.1 GiB;
        // 0.7 × 64 / 2.1 = 21.3 → 21 jobs, clamped to nproc 32 → 21.
        let mem = 64 * GIB;
        assert_eq!(auto_jobs(32, Some(mem), 4), 21);
    }

    #[test]
    fn budget_clamps_to_nproc() {
        // Plenty of memory → nproc wins.
        assert_eq!(auto_jobs(8, Some(512 * GIB), 4), 8);
    }

    #[test]
    fn budget_clamps_to_one() {
        // Tiny memory → never below 1.
        assert_eq!(auto_jobs(32, Some(1 * GIB), 4), 1);
    }

    #[test]
    fn missing_meminfo_uses_fallback() {
        // max(1, min(nproc/4, 8))
        assert_eq!(auto_jobs(32, None, 4), 8);
        assert_eq!(auto_jobs(64, None, 4), 8);
        assert_eq!(auto_jobs(4, None, 4), 1);
        assert_eq!(auto_jobs(2, None, 4), 1);
    }

    #[test]
    fn zero_mem_available_uses_fallback() {
        assert_eq!(auto_jobs(32, Some(0), 4), 8);
    }

    #[test]
    fn explicit_request_is_honored() {
        assert_eq!(resolve_requested(10, 32, Some(64 * GIB), 4), 10);
        // Over budget: still honored (warning is logged by the caller wrapper).
        assert_eq!(resolve_requested(32, 32, Some(8 * GIB), 4), 32);
    }
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p ab-morph-run --features test-analyzer auto_jobs -- --nocapture`
Expected: FAIL — `auto_jobs`/`resolve_requested`/`GIB` not found.

- [ ] **Step 3: Implement**

Above the test module in `auto_jobs.rs`:

```rust
//! Memory-aware default parallelism for warehouse analyze runs.
//!
//! `--jobs 0` derives a job count from `MemAvailable` so a full-corpus run
//! never trips earlyoom (observed: jobs = nproc = 32 with 4 analyzers reached
//! 68.5 GiB RSS and was SIGTERM'd at the 10% MemAvailable watermark).
//! Constants are calibrated by the subset sweep in the 2026-07-05
//! warehouse-run-perf plan (Task 4); the budget mirrors the summarizer's
//! read-once /proc/meminfo discipline.

pub(crate) const GIB: u64 = 1024 * 1024 * 1024;
/// Fraction of MemAvailable the run may budget (30% headroom for the OS,
/// page cache churn, and concurrent processes).
const MEM_FRACTION_NUM: u64 = 7;
const MEM_FRACTION_DEN: u64 = 10;
/// Placeholder until Task 4 calibration (see plan): fixed per-job overhead.
const BASE_PER_JOB_BYTES: u64 = 100 * 1024 * 1024;
/// Placeholder until Task 4 calibration: additional bytes per analyzer per job.
const PER_ANALYZER_BYTES: u64 = 512 * 1024 * 1024;

fn per_job_bytes(analyzer_count: usize) -> u64 {
    BASE_PER_JOB_BYTES + analyzer_count as u64 * PER_ANALYZER_BYTES
}

fn fallback_jobs(nproc: usize) -> usize {
    (nproc / 4).clamp(1, 8)
}

pub(crate) fn auto_jobs(nproc: usize, mem_available_bytes: Option<u64>, analyzer_count: usize) -> usize {
    match mem_available_bytes {
        Some(mem) if mem > 0 => {
            let budget = mem / MEM_FRACTION_DEN * MEM_FRACTION_NUM;
            let by_memory = (budget / per_job_bytes(analyzer_count)) as usize;
            by_memory.clamp(1, nproc)
        }
        _ => fallback_jobs(nproc),
    }
}

pub(crate) fn resolve_requested(requested: usize, nproc: usize, mem_available_bytes: Option<u64>, analyzer_count: usize) -> usize {
    if requested == 0 {
        auto_jobs(nproc, mem_available_bytes, analyzer_count)
    } else {
        requested
    }
}

fn read_mem_available_bytes() -> Option<u64> {
    let meminfo = std::fs::read_to_string("/proc/meminfo").ok()?;
    let kb = meminfo.lines().find_map(|line| {
        line.strip_prefix("MemAvailable:")?
            .trim()
            .split(' ')
            .next()?
            .parse::<u64>()
            .ok()
    })?;
    Some(kb * 1024)
}

/// Resolve the effective job count for an analyze run, logging the decision.
pub(crate) fn resolve_jobs(requested: usize, analyzer_count: usize) -> usize {
    let nproc = std::thread::available_parallelism()
        .map(std::num::NonZeroUsize::get)
        .unwrap_or(1);
    let mem = read_mem_available_bytes();
    let jobs = resolve_requested(requested, nproc, mem, analyzer_count);
    match (requested, mem) {
        (0, Some(m)) => eprintln!(
            "auto-jobs: {jobs} (nproc={nproc}, MemAvailable={:.1} GiB, {analyzer_count} analyzers, per-job={:.1} GiB, 70% budget)",
            m as f64 / GIB as f64,
            per_job_bytes(analyzer_count) as f64 / GIB as f64,
        ),
        (0, None) => eprintln!("auto-jobs: {jobs} (MemAvailable unreadable; fallback max(1, min(nproc/4, 8)) with nproc={nproc})"),
        (n, Some(m)) => {
            let budget_jobs = auto_jobs(nproc, Some(m), analyzer_count);
            if n > budget_jobs {
                eprintln!(
                    "warning: --jobs {n} exceeds the memory budget ({budget_jobs} jobs for MemAvailable={:.1} GiB, {analyzer_count} analyzers); honoring the explicit request",
                    m as f64 / GIB as f64,
                );
            }
        }
        _ => {}
    }
    jobs
}
```

Note the arithmetic order `mem / DEN * NUM` (divide first) avoids u64 overflow on huge machines. Add `mod auto_jobs;` to `crates/ab-morph-run/src/lib.rs` next to the other module declarations.

- [ ] **Step 4: Wire into the analyze entry point**

In `crates/ab-morph-run/src/pipeline.rs` at ~line 112, replace:

```rust
    if jobs == 0 {
        bail!("--jobs must be at least 1");
    }
```

with:

```rust
    let jobs = crate::auto_jobs::resolve_jobs(jobs, analyzer_ids.len());
```

(`jobs` is rebound; the rest of the function is unchanged. If a later shadowing or a `jobs: usize` struct-field initialization uses the old binding, the compiler will point at it.) Check `crates/ab-morph-run/src/main.rs` for a CLI-level `jobs == 0` rejection or a clap `default_value` that blocks 0 — if `validate_warehouse_cli` (main.rs:351) rejects 0, relax it to accept 0 with the meaning "auto".

- [ ] **Step 5: Run tests**

Run: `cargo test -p ab-morph-run --features test-analyzer && cargo clippy -p ab-morph-run --features test-analyzer --all-targets 2>&1 | tail -1`
Expected: all pass (baseline 127 lib + 28 bin plus the 6 new auto_jobs tests), clippy clean. The trailing Doc-tests line reporting 0 tests is the empty doctest target, not a silent failure.

- [ ] **Step 6: justfile passes 0 through**

In `justfile` recipe `morph-warehouse-run-with-analyzers` (~line 534), delete the two lines:

```
	@jobs="{{jobs}}"; \
	if [ "$jobs" = "0" ]; then jobs="$(nproc)"; fi; \
```

and replace with:

```
	@jobs="{{jobs}}"; \
```

Verify: `just -n morph-warehouse-run-with-analyzers | grep -c nproc` prints `0` (scoped to this recipe's expansion — `nproc` legitimately appears in unrelated recipes elsewhere in the justfile).

- [ ] **Step 7: Commit**

```bash
git add -A && git commit -m "feat(morph-run): memory-aware auto-jobs for --jobs 0"
```

---

### Task 3: Run-owned shard staging with orphan cleanup

**Files:**
- Modify: `crates/ab-morph-run/src/pipeline.rs` (~lines 951-960: temp_root; new cleanup + marker functions; call cleanup at parallel-run start)
- Modify: `justfile` (drop `TMPDIR=`/`TMP=`/`TEMP=` exports from `morph-warehouse-run-with-analyzers`)
- Test: unit tests in `pipeline.rs`'s existing `#[cfg(test)]` module (or a small new `staging_guard` module with its own tests)

**Interfaces:**
- Consumes: `WarehouseParallelOptions { warehouse_dir, run_id, .. }` (`crates/ab-morph-run/src/options.rs:118`).
- Produces: `fn shard_staging_root(warehouse_dir: &Path, run_id: &str) -> PathBuf`; `fn claim_shard_staging(root: &Path) -> Result<()>` (writes `pid` marker, errors on live collision); `fn cleanup_orphaned_shard_staging(warehouse_dir: &Path) -> Result<()>`; `fn staging_owner_alive(pid: u32, cmdline_needle: &str) -> bool`.

- [ ] **Step 1: Write the failing tests**

In `crates/ab-morph-run/src/pipeline.rs` test module (use `tempfile::TempDir` if already a dev-dependency; otherwise create dirs under `std::env::temp_dir()` with a unique suffix and clean up):

```rust
#[test]
fn staging_owner_liveness_checks_pid_and_cmdline() {
    // Dead PID: far beyond pid_max.
    assert!(!staging_owner_alive(999_999_999, "ab-morph-run"));
    // Live PID, foreign cmdline: PID 1 is init/systemd, never ab-morph-run.
    assert!(!staging_owner_alive(1, "ab-morph-run"));
    // Live PID, matching cmdline: this very test process, matched against
    // its own binary name.
    let me = std::process::id();
    let exe = std::env::current_exe().unwrap();
    let needle = exe.file_name().unwrap().to_str().unwrap().to_owned();
    assert!(staging_owner_alive(me, &needle));
}

#[test]
fn orphaned_shard_staging_is_removed_and_live_is_kept() {
    let root = std::env::temp_dir().join(format!("staging-test-{}", std::process::id()));
    let staging = root.join(".staging");
    // Orphan: dead PID marker.
    let dead = staging.join("shards-old-run");
    std::fs::create_dir_all(&dead).unwrap();
    std::fs::write(dead.join("pid"), "999999999").unwrap();
    // Live: this process's PID (cmdline needle in production is "ab-morph-run";
    // the cleanup fn takes the needle as a parameter so this test can pass its
    // own binary name).
    let live = staging.join("shards-live-run");
    std::fs::create_dir_all(&live).unwrap();
    std::fs::write(live.join("pid"), std::process::id().to_string()).unwrap();
    // Merge-owned staging entry (no shards- prefix): must never be touched.
    let merge = staging.join("some-run.12345");
    std::fs::create_dir_all(&merge).unwrap();

    let exe = std::env::current_exe().unwrap();
    let needle = exe.file_name().unwrap().to_str().unwrap().to_owned();
    cleanup_orphaned_shard_staging_with_needle(&root, &needle).unwrap();

    assert!(!dead.exists(), "dead-PID orphan must be removed");
    assert!(live.exists(), "live staging must be kept");
    assert!(merge.exists(), "merge-owned staging must not be touched");
    std::fs::remove_dir_all(&root).unwrap();
}

#[test]
fn claim_errors_on_live_collision_and_replaces_dead() {
    let root = std::env::temp_dir().join(format!("claim-test-{}", std::process::id()));
    let shard_root = root.join(".staging").join("shards-run-x");
    // Dead prior claim → replaced silently.
    std::fs::create_dir_all(&shard_root).unwrap();
    std::fs::write(shard_root.join("pid"), "999999999").unwrap();
    claim_shard_staging_with_needle(&shard_root, "no-such-needle").unwrap();
    assert_eq!(
        std::fs::read_to_string(shard_root.join("pid")).unwrap(),
        std::process::id().to_string()
    );
    // Live claim (our own PID, matched by our own binary name) → collision error.
    let exe = std::env::current_exe().unwrap();
    let needle = exe.file_name().unwrap().to_str().unwrap().to_owned();
    let err = claim_shard_staging_with_needle(&shard_root, &needle).unwrap_err();
    assert!(err.to_string().contains("already in progress"), "{err}");
    std::fs::remove_dir_all(&root).unwrap();
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p ab-morph-run --features test-analyzer staging -- --nocapture`
Expected: FAIL — functions not defined.

- [ ] **Step 3: Implement**

In `pipeline.rs` (near the other free functions):

```rust
const STAGING_SHARD_PREFIX: &str = "shards-";
const STAGING_OWNER_NEEDLE: &str = "ab-morph-run";

fn shard_staging_root(warehouse_dir: &Path, run_id: &str) -> PathBuf {
    warehouse_dir
        .join(".staging")
        .join(format!("{STAGING_SHARD_PREFIX}{run_id}"))
}

/// A staging owner is alive iff its PID exists and its cmdline names this
/// binary — the cmdline check closes the PID-reuse hole (an unrelated process
/// that recycled the PID does not block cleanup).
fn staging_owner_alive(pid: u32, cmdline_needle: &str) -> bool {
    match std::fs::read(format!("/proc/{pid}/cmdline")) {
        Ok(bytes) => String::from_utf8_lossy(&bytes).contains(cmdline_needle),
        Err(_) => false,
    }
}

fn staging_entry_owner(entry: &Path) -> Option<u32> {
    std::fs::read_to_string(entry.join("pid"))
        .ok()?
        .trim()
        .parse()
        .ok()
}

fn cleanup_orphaned_shard_staging_with_needle(warehouse_dir: &Path, needle: &str) -> Result<()> {
    let staging_root = warehouse_dir.join(".staging");
    if !staging_root.exists() {
        return Ok(());
    }
    for entry in fs::read_dir(&staging_root)? {
        let entry = entry?;
        let name = entry.file_name();
        let Some(name) = name.to_str() else { continue };
        if !name.starts_with(STAGING_SHARD_PREFIX) {
            continue; // merge-owned staging ({run_id}.{pid}) has its own cleanup
        }
        let alive = staging_entry_owner(&entry.path())
            .is_some_and(|pid| staging_owner_alive(pid, needle));
        if !alive {
            eprintln!(
                "removing orphaned shard staging {} (owner dead or marker missing)",
                entry.path().display()
            );
            fs::remove_dir_all(entry.path()).with_context(|| {
                format!("failed to remove orphaned shard staging {}", entry.path().display())
            })?;
        }
    }
    Ok(())
}

fn cleanup_orphaned_shard_staging(warehouse_dir: &Path) -> Result<()> {
    cleanup_orphaned_shard_staging_with_needle(warehouse_dir, STAGING_OWNER_NEEDLE)
}

fn claim_shard_staging_with_needle(shard_root: &Path, needle: &str) -> Result<()> {
    if shard_root.exists() {
        if staging_entry_owner(shard_root).is_some_and(|pid| staging_owner_alive(pid, needle)) {
            bail!(
                "warehouse run already in progress: {} is claimed by a live process",
                shard_root.display()
            );
        }
        fs::remove_dir_all(shard_root)
            .with_context(|| format!("failed to replace dead staging {}", shard_root.display()))?;
    }
    fs::create_dir_all(shard_root)
        .with_context(|| format!("failed to create {}", shard_root.display()))?;
    fs::write(shard_root.join("pid"), std::process::id().to_string())
        .with_context(|| format!("failed to write pid marker in {}", shard_root.display()))?;
    Ok(())
}

fn claim_shard_staging(shard_root: &Path) -> Result<()> {
    claim_shard_staging_with_needle(shard_root, STAGING_OWNER_NEEDLE)
}
```

- [ ] **Step 4: Rewire `run_analyze_aat_warehouse_parallel`**

At ~line 951, replace:

```rust
    let temp_root = std::env::temp_dir().join(format!(
        "ab-morph-run-warehouse-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos()
    ));
    let shard_warehouse_dir = temp_root.join("warehouse");
    fs::create_dir_all(&shard_warehouse_dir)
        .with_context(|| format!("failed to create {}", shard_warehouse_dir.display()))?;
```

with:

```rust
    cleanup_orphaned_shard_staging(&options.warehouse_dir)?;
    let temp_root = shard_staging_root(&options.warehouse_dir, &options.run_id);
    claim_shard_staging(&temp_root)?;
    let shard_warehouse_dir = temp_root.join("warehouse");
    fs::create_dir_all(&shard_warehouse_dir)
        .with_context(|| format!("failed to create {}", shard_warehouse_dir.display()))?;
```

The existing success cleanup (`fs::remove_dir_all(&temp_root)` at ~line 1068) and error cleanup (~line 1037) work unchanged on the new path. Leave the non-warehouse JSONL path's temp_root (~line 1266) untouched — it is out of the spec's scope.

- [ ] **Step 5: Run tests, plus tiny end-to-end**

Run: `cargo test -p ab-morph-run --features test-analyzer && cargo clippy -p ab-morph-run --features test-analyzer --all-targets 2>&1 | tail -1`
Expected: all pass, clippy clean.

End-to-end (test analyzers need no dictionaries):

```bash
cargo run --release --features test-analyzer -p ab-morph-run -- analyze-aat \
  --aat-dir <any 2-3 AAT files copied to a scratch dir> \
  --analyzer test:single --analyzer test:split \
  --warehouse-dir /tmp/staging-e2e-wh --run-id e2e --warehouse-profile full --jobs 2
ls /tmp/staging-e2e-wh/.staging/   # while running would show shards-e2e; after success: empty
ls /tmp/staging-e2e-wh/runs/e2e    # all tables present
```

Expected: run succeeds; `.staging/` contains no `shards-e2e` afterwards; `runs/e2e` has the parquet tables.

- [ ] **Step 6: justfile drops the TMPDIR exports**

In `morph-warehouse-run-with-analyzers`, delete the three lines:

```
	TMPDIR="{{ab_db_root}}/tmp" \
	TMP="{{ab_db_root}}/tmp" \
	TEMP="{{ab_db_root}}/tmp" \
```

Verify `just -n morph-warehouse-run-with-analyzers` no longer mentions TMPDIR.

- [ ] **Step 7: Commit**

```bash
git add -A && git commit -m "feat(morph-run): run-owned shard staging with PID+cmdline orphan cleanup"
```

---

### Task 4: Calibrate auto-jobs constants (subset sweep)

**Files:**
- Modify: `crates/ab-morph-run/src/auto_jobs.rs` (the two constants + doc comment recording the fit)
- Create: nothing committed besides the constant change; sweep artifacts stay in scratch

**Interfaces:**
- Consumes: Tasks 1-3 merged into the working branch; real dictionaries (worktree symlink gotcha in Global Constraints).
- Produces: calibrated `BASE_PER_JOB_BYTES` / `PER_ANALYZER_BYTES` with the fit recorded in the constant doc comments and in this plan file (fill in the table below).

- [ ] **Step 1: Build a ~2,000-source subset and check representativeness**

Random sample (not `head` — lexical prefix correlates with catalog order, not size), then verify the size distribution matches the corpus, since large documents dominate per-job memory:

```bash
CORPUS=/db/ab-validator/aat-corpus/aozora2html-aat/aozora2html-adapter
SUBSET=/db/ab-validator/tmp/calib-aat && mkdir -p "$SUBSET"
ls "$CORPUS" | shuf --random-source=<(yes 42) -n 2000 \
  | xargs -I{} ln -s "$CORPUS"/{} "$SUBSET"/
# Representativeness: mean and p95 file size, subset vs corpus — must agree within ~15%.
for d in "$SUBSET" "$CORPUS"; do
  find -L "$d" -name '*.json' -printf '%s\n' | sort -n \
    | awk -v d="$d" '{a[NR]=$1; s+=$1} END {printf "%s mean=%.0f p95=%d max=%d\n", d, s/NR, a[int(NR*0.95)], a[NR]}'
done
```

If the subset's mean or p95 is >15% below the corpus's, re-draw with a different seed or stratify (append the corpus's 50 largest files to the subset) — under-sampling large documents under-calibrates the budget.

- [ ] **Step 2: Sweep jobs 4 / 16 / 32 with peak-RSS capture**

For J in 4 16 32 (each run ~minutes; use a scratch warehouse dir, delete between runs):

```bash
/usr/bin/time -v just morph-warehouse-run-with-analyzers full "$SUBSET" \
  "vibrato vibrato:unidic-novel-202512 sudachi-a sudachi-c" calib-j$J $J \
  2>&1 | grep -E "Maximum resident|Elapsed"
```

(If `just` indirection muddies `time -v`, invoke the `cargo run --release -p ab-morph-run -- analyze-aat ...` command directly under `/usr/bin/time -v` with the same analyzer flags and `--warehouse-dir /db/ab-validator/tmp/calib-wh-j$J`.) Record the three (jobs, peak_rss_kb, wall) rows in the table below.

- [ ] **Step 3: Fit and update constants**

Linear fit `peak_rss = intercept + slope × jobs` over the three points; `per_job_bytes(4 analyzers) = slope`. Split: `PER_ANALYZER_BYTES = slope / 4` (attribute the per-job cost to analyzers; keep `BASE_PER_JOB_BYTES` at the residual, minimum 64 MiB). If the three points are visibly non-linear (middle point off the line by >20%), set `per_job_bytes` from the worst measured `peak_rss / jobs` instead — over-conservative beats OOM (spec §2).

Record here:

| jobs | peak RSS (kB) | wall | fitted slope | residual |
|---|---|---|---|---|
| 4 | 15,397,980 | 14:29.28 | 859,500 kB/job | −7.2% |
| 16 | 28,978,108 | 8:53.63 | (least squares over all 3 points) | +7.7% |
| 32 | 39,760,908 | 8:40.32 | intercept 13,147,661 kB ≈ 12.5 GiB (shared dicts) | −2.2% |

Measured 2026-07-06 on the 2,000-source random subset (seed 42; subset mean 163,545 B / p95 751,488 B vs corpus mean 169,475 B / p95 674,536 B — within 15%, no re-draw). Middle point +7.7% off the line (< 20%) → linear fit used. Split per the rule: `PER_ANALYZER_BYTES` = 859,500 kB ÷ 4 ≈ 209.8 MiB → 210 MiB; residual 0 → `BASE_PER_JOB_BYTES` = 64 MiB floor. per_job(4 analyzers) = 904 MiB; on this box (MemAvailable ≈ 72 GiB) auto-jobs = 57 by memory → clamped to nproc 32, predicted peak ≈ 12.5 + 32 × 0.82 ≈ 38.8 GiB (measured 37.9 GiB at J=32).

Update the two constants in `auto_jobs.rs`, extend their doc comments with the fit numbers and date, and adjust the `budget_scales_with_memory_and_analyzers` test's expected value to the new constants (recompute the expected jobs by hand in the test comment).

Follow-up (2026-07-06, task 4b): the 13,147,661 kB intercept above is large enough (~12.5 GiB) that hosts with under ~20 GiB MemAvailable could have their whole 70% budget consumed by it, so the formula was amended to budget it explicitly — see the design spec §2 update — rather than leaving it unbudgeted as this step originally did.

- [ ] **Step 4: Tests still green; commit**

```bash
cargo test -p ab-morph-run --features test-analyzer auto_jobs
rm -rf /db/ab-validator/tmp/calib-* 
git add -A && git commit -m "feat(morph-run): calibrate auto-jobs constants from subset sweep"
```

---

### Task 5: Full-corpus validation run and canonical swap

**Files:** none (operational; results recorded in the merge/PR message and the plan)

**Interfaces:**
- Consumes: Tasks 1-4 complete; branch merged to main (run from the main checkout so `dictionary` resolves).
- Produces: the new canonical warehouse run.

- [ ] **Step 1: Full run at auto-jobs**

```bash
just morph-warehouse-run   # profile full, default aat_dir, jobs 0 → auto
```

Record: the auto-jobs line it logs, wall-clock, and peak RSS (`/usr/bin/time -v` wrapper or monitor `ps -o rss= -C ab-morph-run` during the run). Expected: auto-jobs well above 10; wall-clock ≈ 20-25 min; no earlyoom event (`journalctl -u earlyoom --since "1 hour ago"` quiet).

- [ ] **Step 2: Row-count verification against the prior run**

```bash
OLD=/db/ab-validator/morph-warehouse/runs/full-2026-07-05_114245-jobs10
NEW=/db/ab-validator/morph-warehouse/runs/<new run id>
for t in analyses morphemes nway_regions; do
  duckdb -c "SELECT '$t', (SELECT count(*) FROM read_parquet('$OLD/$t.parquet')) AS old,
                     (SELECT count(*) FROM read_parquet('$NEW/$t.parquet')) AS new;"
done
duckdb -c "SELECT analyzer_id, count(*) FROM read_parquet('$NEW/analyses.parquet') GROUP BY 1 ORDER BY 1;"
duckdb -c "SELECT analyzer_id, count(*) FROM read_parquet('$OLD/analyses.parquet') GROUP BY 1 ORDER BY 1;"
```

Expected: identical counts, total and per-analyzer.

- [ ] **Step 3: Swap canonical — ONLY on exact match**

If all counts match: `rm -rf "$OLD"` and note the new run id as canonical.
If ANY count differs: **do not delete anything** — the prior run stays canonical; diff per-analyzer/per-table counts to localize the discrepancy, diagnose before proceeding (spec Validation §4).

- [ ] **Step 4: Record outcomes**

Append the measured auto-jobs value, wall-clock, peak RSS, and the verification verdict to this plan file under this task; commit as `docs: record warehouse-run-perf validation results`.

---

## Self-Review Notes

- Spec coverage: §1 Arc → Task 1; §2 auto-jobs + calibration procedure → Tasks 2+4; §3 staging/orphans/collision/atomicity (publish path untouched — atomicity is preserved by construction since `.staging` and `runs/` share the warehouse dir) → Task 3; Validation incl. failure procedure → Task 5; Error-behavior table rows → Task 2 step 3 (fallback), Task 3 steps 1-3 (collision, orphan), warning path → Task 2.
- Deliberate scope notes: the non-warehouse JSONL parallel path keeps `std::env::temp_dir()` (out of spec scope, stated in Task 3 step 4); `PlainTextDocument.text` stays `String` (one copy per document is not the multiplier; spec targets the per-analysis clone).
- Type consistency: `resolve_jobs(requested, analyzer_count)` used in Task 2 steps 3-4; `claim_shard_staging`/`cleanup_orphaned_shard_staging` names match between Task 3 steps 1, 3, and 4.
