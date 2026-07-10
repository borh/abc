# Rust Quality, Performance, and Hermetic Tests Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make adapter validation hermetic and improve the highest-impact quality, memory, and corpus-performance paths in `ab-validator` without changing published data contracts.

**Architecture:** Nix checks receive pinned tools through derivation references, never literal store paths. Performance work introduces prepared run resources and thread-local accumulation before local allocation cleanup. Structural work is isolated into behavior-preserving commits after output characterization.

**Tech Stack:** Rust 2024, Cargo, Criterion, Rayon, Serde JSON, Nix flakes, `rustPlatform`, `just`.

## Global Constraints

- Run commands from `/home/bor/Projects/soranoha` unless a task names another directory.
- Use the root flake and root `justfile` as the primary entry points.
- Do not hardcode `/nix/store`, `/db`, `result`, or host-specific executable paths.
- In Nix, bind Aozora as `AB_AOZORA_BIN = "${upstreamParserAozora}/bin/aozora"`.
- Do not update `flake.lock`; this plan adds no flake input.
- Preserve AAT, parser-IR, warehouse, hash, report, ordering, and error semantics.
- Preserve both `FeatureDiffsShape::ScalarAnalyzerId` and `FeatureDiffsShape::CollapsedAnalyzers`.
- Keep Parquet-footer shape detection authoritative; do not infer shape from run metadata.
- Preserve the distinction between required shape detection in execution paths and collapsed-shape fallback in SQL-preview wrappers when source files are absent.
- Keep performance changes separate from structural file moves.
- Preserve unrelated worktree changes.

---

### Task 1: Hermetic Aozora integration tests

**Files:**
- Modify: `ab-validator/adapters/aozora/tests/integration.rs`
- Create: `ab-validator/tests/aozora-adapter-integration.sh`
- Modify: `ab-validator/flake.nix`
- Modify: `ab-validator/justfile`

**Interfaces:**
- Consumes: `upstreamParserAozora`, `aozoraCargoDeps`, and `mkSmokeCheck` from `ab-validator/flake.nix`.
- Produces: `checks.<system>.aozora-integration`, a default-shell `AB_AOZORA_BIN`, and a hermetic `just aozora-test` entry point.

- [ ] **Step 1: Record the failing check contract**

```sh
cd ab-validator
system="$(nix eval --impure --raw --expr builtins.currentSystem)"
nix build ".#checks.${system}.aozora-integration" --no-link
```

Expected: failure because `aozora-integration` is not yet an output.

- [ ] **Step 2: Make the test use Cargo's binary contract**

Replace `adapter_bin` in `adapters/aozora/tests/integration.rs` with:

```rust
fn adapter_bin() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_aozora-adapter"))
}
```

- [ ] **Step 3: Add the integration driver**

Create `tests/aozora-adapter-integration.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
: "${AB_AOZORA_BIN:?AB_AOZORA_BIN must be provided by the Nix check or dev shell}"
test -x "$AB_AOZORA_BIN"

cargo test \
  --manifest-path "$repo_root/adapters/aozora/Cargo.toml" \
  --test integration \
  --offline \
  --locked
```

- [ ] **Step 4: Add the package-derived Nix check**

Add beside `aozoraAdapterSmokeCheck` in `ab-validator/flake.nix`:

```nix
aozoraAdapterIntegrationCheck = mkSmokeCheck {
  name = "aozora-adapter-integration-check";
  testScript = "tests/aozora-adapter-integration.sh";
  nativeBuildInputs = [ rustToolchain ];
  extraEnv = {
    AB_AOZORA_BIN = "${upstreamParserAozora}/bin/aozora";
  };
  extraPreScript = ''
    export CARGO_HOME="$work_dir/cargo-home"
    mkdir -p "$CARGO_HOME"
    cat > "$CARGO_HOME/config.toml" <<'EOF'
    [source.crates-io]
    replace-with = "vendored-sources"

    [source.vendored-sources]
    directory = "${aozoraCargoDeps}"
    EOF
  '';
};
```

Expose it as:

```nix
aozora-integration = aozoraAdapterIntegrationCheck;
```

Add to `devShells.default`:

```nix
AB_AOZORA_BIN = "${upstreamParserAozora}/bin/aozora";
```

The interpolation is the package output path tracked by Nix. Do not replace it with a literal store path, `result`, or command substitution.

- [ ] **Step 5: Route the Just recipe through the flake shell**

```just
aozora-test:
	@nix develop "{{repo_root}}" --command bash "{{repo_root}}/tests/aozora-adapter-integration.sh"
```

- [ ] **Step 6: Format and verify**

```sh
nix fmt ab-validator/flake.nix
cd ab-validator
system="$(nix eval --impure --raw --expr builtins.currentSystem)"
nix build ".#checks.${system}.aozora-integration" --no-link --print-build-logs
just aozora-test
```

Expected: all 15 integration tests pass without a host-installed parser.

- [ ] **Step 7: Commit**

```sh
git add ab-validator/adapters/aozora/tests/integration.rs \
  ab-validator/tests/aozora-adapter-integration.sh \
  ab-validator/flake.nix ab-validator/justfile
git commit -m "test(ab-validator): run aozora integration tests hermetically"
```

---

### Task 2: Strict quality checks for excluded adapters

**Files:**
- Modify: `ab-validator/flake.nix`
- Modify: Rust files reported by Clippy under `ab-validator/adapters/{aozora,aozora2,aozora2html,aozora-rs}`

**Interfaces:**
- Consumes: each adapter's existing `importCargoLock` result.
- Produces: `checks.<system>.adapters-cargo-quality`.

- [ ] **Step 1: Capture current failures**

```sh
for adapter in aozora aozora2 aozora2html aozora-rs aozora-epub3; do
  cargo clippy --manifest-path "ab-validator/adapters/$adapter/Cargo.toml" \
    --all-targets -- -D warnings || true
done
```

Expected: four adapters fail; `aozora-epub3` passes.

- [ ] **Step 2: Add a reusable quality derivation**

Add after the adapter Cargo dependency bindings:

```nix
mkAdapterCargoQualityCheck =
  { name, manifestPath, cargoDeps }:
  pkgs.runCommand "${name}-cargo-quality-check"
    { nativeBuildInputs = [ rustToolchain ]; }
    ''
      work_dir="$(mktemp -d)"
      cp -R ${source} "$work_dir/source"
      chmod -R +w "$work_dir/source"
      export CARGO_HOME="$work_dir/cargo-home"
      mkdir -p "$CARGO_HOME"
      cat > "$CARGO_HOME/config.toml" <<'EOF'
      [source.crates-io]
      replace-with = "vendored-sources"

      [source.vendored-sources]
      directory = "${cargoDeps}"
      EOF
      manifest="$work_dir/source/${manifestPath}"
      cargo fmt --manifest-path "$manifest" -- --check
      cargo clippy --manifest-path "$manifest" \
        --all-targets --offline --locked -- -D warnings
      touch "$out"
    '';
```

Instantiate and aggregate it as follows:

```nix
adapterCargoQualityChecks = [
  (mkAdapterCargoQualityCheck {
    name = "aozora";
    manifestPath = "adapters/aozora/Cargo.toml";
    cargoDeps = aozoraCargoDeps;
  })
  (mkAdapterCargoQualityCheck {
    name = "aozora2";
    manifestPath = "adapters/aozora2/Cargo.toml";
    cargoDeps = aozora2AdapterCargoDeps;
  })
  (mkAdapterCargoQualityCheck {
    name = "aozora2html";
    manifestPath = "adapters/aozora2html/Cargo.toml";
    cargoDeps = aozora2htmlCargoDeps;
  })
  (mkAdapterCargoQualityCheck {
    name = "aozora-rs";
    manifestPath = "adapters/aozora-rs/Cargo.toml";
    cargoDeps = aozoraRsAdapterCargoDeps;
  })
  (mkAdapterCargoQualityCheck {
    name = "aozora-epub3";
    manifestPath = "adapters/aozora-epub3/Cargo.toml";
    cargoDeps = aozoraEpub3CargoDeps;
  })
];

adapterCargoQualityCheck = pkgs.runCommand "adapter-cargo-quality-check" { } ''
  ${pkgs.lib.concatMapStringsSep "\n" (check: "test -e ${check}") adapterCargoQualityChecks}
  touch "$out"
'';
```

Expose `adapters-cargo-quality = adapterCargoQualityCheck;` in `checks`.

- [ ] **Step 3: Apply mechanical corrections**

```sh
for adapter in aozora aozora2 aozora2html aozora-rs; do
  cargo clippy --manifest-path "ab-validator/adapters/$adapter/Cargo.toml" \
    --all-targets --fix --allow-dirty --allow-staged
done
```

For `MappingErrorKind`, preserve protocol names with:

```rust
#[allow(clippy::enum_variant_names)]
pub enum MappingErrorKind {
```

- [ ] **Step 4: Verify and commit**

```sh
nix fmt ab-validator/flake.nix
cd ab-validator
system="$(nix eval --impure --raw --expr builtins.currentSystem)"
nix build ".#checks.${system}.adapters-cargo-quality" --no-link --print-build-logs
cd ..
git add ab-validator/flake.nix ab-validator/adapters
git commit -m "chore(ab-validator): gate excluded adapters with clippy"
```

---

### Task 3: Characterize and reuse orthographic detectors

**Files:**
- Modify: `ab-validator/crates/ab-morph-run/src/options.rs`
- Modify: `ab-validator/crates/ab-morph-run/src/pipeline.rs`
- Create: `ab-validator/crates/ab-morph-run/benches/warehouse_detector_lifetime.rs`
- Modify: `ab-validator/crates/ab-morph-run/Cargo.toml`

**Interfaces:**
- Produces: `PreparedOrthoDetector` and `prepare_ortho_detector`.
- Preserves: detector identity, normalization policy hash, warnings, errors, and warehouse output.

- [ ] **Step 1: Add a construction-count regression test**

Under `#[cfg(test)]`, add an `AtomicUsize` increment in the extracted constructor and a 65-input warehouse test. Assert:

```rust
assert_eq!(detector_build_count(), 1);
```

Expected before extraction: compilation failure because the hook is absent.

- [ ] **Step 2: Define the prepared resource**

Add to `options.rs`:

```rust
#[derive(Clone)]
pub(crate) struct PreparedOrthoDetector(
    pub(crate) Option<std::sync::Arc<dyn ab_ortho_detect::OrthoDetector>>,
);

impl std::fmt::Debug for PreparedOrthoDetector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("PreparedOrthoDetector")
            .field(&self.0.as_ref().map(|detector| detector.detector_id()))
            .finish()
    }
}
```

Add `prepared_ortho_detector: Option<PreparedOrthoDetector>` to `SerialRunOptions` and set it to `None` at non-warehouse call sites.

- [ ] **Step 3: Extract existing construction logic**

Create:

```rust
fn prepare_ortho_detector(
    analyzers: &[Arc<LoadedAnalyzer>],
    mode: OrthoDetectMode,
    ml_model: Option<&Path>,
) -> Result<PreparedOrthoDetector> {
    let detector: Option<Arc<dyn OrthoDetector>> = match mode {
        OrthoDetectMode::Off => None,
        OrthoDetectMode::Heuristic => build_heuristic_detector(analyzers)?,
        OrthoDetectMode::Ml => build_ml_detector(ml_model)?,
        OrthoDetectMode::Historical => build_historical_detector()?,
    };
    Ok(PreparedOrthoDetector(detector))
}
```

Extract the current three construction bodies into the named helpers without altering parser selection, model loading, dictionary selection, or error text.

- [ ] **Step 4: Prepare once before worker dispatch**

Before `std::thread::scope`:

```rust
let prepared_ortho_detector = prepare_ortho_detector(
    &analyzers,
    options.ortho_detect,
    options.ortho_ml_model.as_deref(),
)?;
```

Clone it into every warehouse batch. In serial execution, use the supplied resource or construct once when it is absent.

- [ ] **Step 5: Cross the batch boundary in a warehouse benchmark**

Create `benches/warehouse_detector_lifetime.rs`. Reuse the AAT fixture shape and analyzer environment selection from `analyze_aat.rs`, generate 65 inputs, and call the public `run_analyze_aat_warehouse` with `jobs = 2`. Benchmark `OrthoDetectMode::Ml` when `AB_ORTHO_ML_MODEL` is set and `OrthoDetectMode::Heuristic` when the default Vibrato dictionary is available. Register it as:

```toml
[[bench]]
name = "warehouse_detector_lifetime"
harness = false
```

- [ ] **Step 6: Verify and commit**

```sh
cd ab-validator
cargo test -p ab-morph-run
cargo bench -p ab-morph-run --bench warehouse_detector_lifetime --no-run
cargo clippy -p ab-morph-run --all-targets -- -D warnings
cd ..
git add ab-validator/crates/ab-morph-run
git commit -m "perf(ab-morph-run): reuse ortho detectors across shards"
```

---

### Task 4: Consolidate prevalence traversal and remove shared locks

**Files:**
- Modify: `ab-validator/crates/ab-coverage/src/detectors.rs`
- Modify: `ab-validator/crates/ab-coverage/src/prevalence.rs`
- Create: `ab-validator/crates/ab-coverage/benches/prevalence_detectors.rs`
- Modify: `ab-validator/crates/ab-coverage/Cargo.toml`

**Interfaces:**
- Produces: `DetectorRegistry::detect_all(&DetectorContext) -> BTreeMap<&str, u64>`.
- Preserves: every row count, sample ordering, processed/failed totals, and report shape.

- [ ] **Step 1: Add all-row equivalence tests**

Build one fixture containing ruby, gaiji, styles, headings, figures, captions, and source markers. For every registry row, assert:

```rust
let all = registry.detect_all(&ctx);
for row_id in registry.rows() {
    assert_eq!(registry.detect(row_id, &ctx), all[row_id]);
}
```

Expected initially: compilation failure because `detect_all` is absent.

- [ ] **Step 2: Separate node-local and structural rules**

Replace `Rule::Named` with:

```rust
type NodePredicate = fn(&Value) -> bool;
type WholeAatFn = fn(&Value) -> u64;

enum Rule {
    AatKindCount(Vec<String>),
    SourceRegex(Regex),
    AatNode(NodePredicate),
    WholeAat(WholeAatFn),
}
```

Use `AatNode` for gaiji attributes, ruby direction, style type, headings, captions, figures, `tcy`, and `warichu`. Keep relationship-sensitive gaiji-in-ruby and figure-with-caption rules as `WholeAat`.

- [ ] **Step 3: Implement the shared traversal**

Implement `detect_all` with a stable `BTreeMap`: initialize all row counters, walk the AAT once, update `AatKindCount` and `AatNode` rules per node, then run each `WholeAat` and source regex rule once. Keep `detect` temporarily as the equivalence oracle and remove it only after all callers migrate.

- [ ] **Step 4: Replace shared mutexes with fold/reduce**

Add:

```rust
#[derive(Debug)]
struct LocalPrevalence {
    rows: Vec<RowCounter>,
    processed: u64,
    failed: u64,
}
```

Implement `RowCounter::merge` by summing counts, combining both top-five lists, sorting with the existing comparator, and truncating to five. Replace `par_iter().for_each` with Rayon `fold` and `reduce`; log individual failures inside the fold but mutate only worker-local state.

- [ ] **Step 5: Add the detector benchmark**

Register:

```toml
[dev-dependencies]
criterion.workspace = true

[[bench]]
name = "prevalence_detectors"
harness = false
```

Benchmark repeated `detect` calls against `detect_all` using all 56 matrix rows and a synthetic 10,000-node AAT.

- [ ] **Step 6: Verify canonical equality and commit**

```sh
cd ab-validator
cargo test -p ab-coverage
cargo bench -p ab-coverage --bench prevalence_detectors --no-run
cargo clippy -p ab-coverage --all-targets -- -D warnings
cd ..
git add ab-validator/crates/ab-coverage
git commit -m "perf(ab-coverage): share detector traversal and reduce locally"
```

For the existing small prevalence fixture, canonicalize before/after JSON with `jq -S`; expected diff is empty.

---

### Task 5: Bound AAT comparison memory

**Files:**
- Modify: `ab-validator/crates/ab-compare/src/aat_diff.rs`
- Modify: `ab-validator/crates/ab-compare/benches/aat_diff.rs`

**Interfaces:**
- Preserves duplicate work-ID keys, limits, summary counts, and serialized output.

- [ ] **Step 1: Add lifetime and duplicate-ID tests**

Add a test-only live-root counter around parsing and assert a 200-file comparison does not retain 200 complete roots after the change. Retain the existing duplicate-work-ID tests as key-contract coverage.

- [ ] **Step 2: Summarize in the parallel read**

Add:

```rust
struct PathSummary {
    path: PathBuf,
    work_id: String,
    summary: AatSummary,
}
```

For each path, read and parse `AatRoot`, clone its `work_id`, immediately call `summarize(root)`, and return `PathSummary`. Count duplicate IDs from these compact values and construct final keys without a corpus-wide `Vec<AatRoot>`.

- [ ] **Step 3: Extend the benchmark**

Add a 2,000-file Criterion fixture with moderate block arrays. Keep generated inputs in `TempDir` and out of source.

- [ ] **Step 4: Verify and commit**

```sh
cd ab-validator
cargo test -p ab-compare
cargo bench -p ab-compare --bench aat_diff --no-run
cargo clippy -p ab-compare --all-targets -- -D warnings
cd ..
git add ab-validator/crates/ab-compare
git commit -m "perf(ab-compare): summarize aat inputs during loading"
```

---

### Task 6: Remove measured local allocations

**Files:**
- Modify: `ab-validator/adapters/aozora/src/lib.rs`
- Modify: `ab-validator/adapters/aozora-epub3/src/source_derived.rs`
- Modify: `ab-validator/crates/ab-ortho-detect/src/features.rs`
- Modify: `ab-validator/crates/ab-ortho-detect/src/ml.rs`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/convert.rs`

**Interfaces:**
- Preserves regex patterns, ML feature order and scores, and emitted JSON.

- [ ] **Step 1: Add exact equivalence tests**

Add tests for existing ruby, gaiji, figure output and a fixed ML score before changing representations.

- [ ] **Step 2: Cache constant regexes**

Use `std::sync::LazyLock<Regex>` for the ruby regex and both EPUB3 recovery regexes. Keep pattern strings unchanged.

- [ ] **Step 3: Use a fixed feature array**

Change the signature and return value to:

```rust
pub fn features_to_vector(f: &CharFeatures) -> [f64; 9] {
    [
        f.total_chars as f64,
        f.hiragana_ratio,
        f.katakana_ratio,
        f.kanji_ratio,
        f.unique_char_ratio,
        f.max_bigram_repeat_ratio,
        f.char_run_repeat_ratio,
        f.repeated_bigram_pattern_ratio,
        if f.katakana_at_sentence_end { 1.0 } else { 0.0 },
    ]
}
```

- [ ] **Step 4: Remove quote-marker length allocation**

Replace `utf8_len(ch.to_string().as_str())` with `ch.len_utf8() as u64`; retain `ch.to_string()` only for JSON emission.

- [ ] **Step 5: Verify and commit**

```sh
cd ab-validator
cargo test -p ab-ortho-detect -p ab-aat-to-parser-ir
cargo clippy -p ab-ortho-detect -p ab-aat-to-parser-ir --all-targets -- -D warnings
nix develop --command cargo test --manifest-path adapters/aozora/Cargo.toml
cargo test --manifest-path adapters/aozora-epub3/Cargo.toml
cd ..
git add ab-validator/adapters/aozora ab-validator/adapters/aozora-epub3 \
  ab-validator/crates/ab-ortho-detect ab-validator/crates/ab-aat-to-parser-ir
git commit -m "perf(ab-validator): remove repeated regex and feature allocations"
```

---

### Task 7: Protect the canonical decoding contract

**Files:**
- Create: `ab-validator/data/fixtures/source-decoding-contract.json`
- Modify: decoder unit tests in all five Rust adapters
- Modify: `ab-validator/flake.nix`

**Interfaces:**
- Canonical behavior: UTF-8 BOM stripping, strict UTF-8, Windows-31J fallback, lossy label, and SHA-256 of original bytes.

- [ ] **Step 1: Add shared vectors**

Create `data/fixtures/source-decoding-contract.json`:

```json
[
  {
    "name": "utf8",
    "bytes": [97, 98, 99],
    "text": "abc",
    "encoding": "utf-8",
    "sha256": "sha256:ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
  },
  {
    "name": "utf8-bom",
    "bytes": [239, 187, 191, 97, 98, 99],
    "text": "abc",
    "encoding": "utf-8-bom",
    "sha256": "sha256:1c28dc3f1f804a1ad9c9b4b4cf5e2658d16ad4ed08e3020d04a8d2865018947c"
  },
  {
    "name": "windows-31j",
    "bytes": [140, 225, 148, 121],
    "text": "吾輩",
    "encoding": "windows-31j",
    "sha256": "sha256:75b97809c3a81e813199db4c45e929f442f7660105a23fb8fe8da6555a2c161b"
  },
  {
    "name": "windows-31j-lossy",
    "bytes": [130, 160, 255],
    "text": "あ�",
    "encoding": "windows-31j-lossy",
    "sha256": "sha256:81e29c40b6c9dd61f2d6f80a5ade5851d78ff1965fbd6ad9927cff5464c3aca0"
  }
]
```

- [ ] **Step 2: Consume the same vectors in each adapter**

Deserialize the fixture with `include_str!` from each adapter decoder test module. Assert all four fields; keep sanitized-span behavior in adapter-specific tests.

- [ ] **Step 3: Add a Nix contract check**

Reuse the per-adapter Cargo quality derivation's source and vendor configuration, but run each decoder test filter. Expose the combined derivation as `checks.<system>.adapter-decoding-contract`. Every vendor directory must enter through its Nix binding, not a discovered store path.

- [ ] **Step 4: Verify and commit**

```sh
nix fmt ab-validator/flake.nix
cd ab-validator
system="$(nix eval --impure --raw --expr builtins.currentSystem)"
nix build ".#checks.${system}.adapter-decoding-contract" --no-link --print-build-logs
cd ..
git add ab-validator/data/fixtures/source-decoding-contract.json \
  ab-validator/adapters ab-validator/flake.nix
git commit -m "test(ab-validator): share adapter decoding contract vectors"
```

---

### Task 8: Full validation and DuckDB compatibility evidence

**Files:**
- Modify only files needed to correct failures introduced by Tasks 1-8.

- [ ] **Step 1: Validate the merged DuckDB compatibility matrix**

Run through the flake dev shell so `AB_DUCKDB_BIN` comes from the pinned `pkgs.duckdb` package:

```sh
cd ab-validator
nix develop --command cargo test -p ab-morph-run summary::
```

Expected: the summary suite passes, including `engines_agree_across_feature_diff_shapes`, `duckdb_engine_matches_in_memory_engine`, `materialized_feature_iter_reads_pre_v3_scalar_analyzer_id_column`, and `feature_diffs_shape_detects_scalar_and_collapsed`.

- [ ] **Step 2: Validate the Rust workspace**

```sh
cd ab-validator
cargo fmt --all -- --check
cargo clippy --workspace --all-targets --all-features -- -D warnings
cargo test --workspace
```

- [ ] **Step 3: Validate adapters and Nix checks**

```sh
cd ab-validator
system="$(nix eval --impure --raw --expr builtins.currentSystem)"
nix build ".#checks.${system}.aozora-integration" --no-link --print-build-logs
nix build ".#checks.${system}.adapters-cargo-quality" --no-link --print-build-logs
nix build ".#checks.${system}.adapter-decoding-contract" --no-link --print-build-logs
```

- [ ] **Step 4: Run monorepo gates**

```sh
cd /home/bor/Projects/soranoha
just nix-format-check
just validate-migration
```

- [ ] **Step 5: Review performance evidence**

Run the three Criterion benchmarks before and after their corresponding commits using the same release toolchain. Report medians and confidence intervals for detector lifecycle, prevalence detection, and AAT comparison. Do not claim improvements from source inspection alone.

- [ ] **Step 6: Confirm repository hygiene**

```sh
git status --short
rg -n '/nix/store|/db/' ab-validator/flake.nix ab-validator/justfile \
  ab-validator/tests ab-validator/crates ab-validator/adapters
```

Expected: no newly introduced literal store/database paths, no generated benchmark artifacts, and only intended source changes.
