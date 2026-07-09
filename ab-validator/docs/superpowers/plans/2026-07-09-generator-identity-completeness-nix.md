# Generator Identity Completeness + Nix Packaging Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close the AAT-generator staleness hole — pin every binary that produces the `aat/` tree in that tree's input identity — by building those binaries through nix, then wire the generator's active skip so identical inputs are not recomputed.

**Architecture:** Extends the merged batch-run-staleness work (spec `2026-07-09-batch-run-staleness-skip-recompute-design.md`). Today the morph-warehouse path is complete-and-safe (skip wired + engine pinned via `.#ab-morph-run`); the AAT generator is unsafe-if-wired: `generator_identity.build_identity_object` pins the corpus, adapter binary, and feature-patterns, but **not** `ab-index` or `ab-check`, even though `run-aat-full.sh` runs both from live source (`run_cargo run -p ab-index`, `-p ab-check`) and `ab-index`'s `index.json` feeds `ab-check`. Both binaries determine `aat/`; neither is in `aat/`'s identity — the same engine-hole class the whole-branch review fixed for the warehouse. We package the two crates through nix (content-id store paths), pin their hashes in the generator identity, switch the generator to the flake's existing nix adapters, resolve all binaries up front so identity is computable before any output is written, then wire the active skip.

**Tech Stack:** Nix (`rustPlatform.buildRustPackage`), Python 3 (identity/skip modules, `unittest`), Bash (`run-aat-full.sh`).

## Global Constraints

- **Fail toward correctness / recompute.** Every output-determining input must be in the identity; omitting one causes silent staleness. Any anomaly (missing/broken metadata, unreadable binary, changed inputs) → not fresh → recompute. Never serve stale as fresh.
- **Content-based identity, never mtime.** Binaries pinned by content hash of the resolved (nix store-path) binary; files by content.
- **Pinned bytes must not move.** `aat_hash.hash_aat_dir` output is pinned as `expected.content_hash` across ~25 GB of fidelity dumps — do NOT refactor or alter its bytes. `tree_hash.tree_hash` is the separate general fold.
- **`input_set_hash` format:** `"sha256:" + sha256(canonical_json({**identity_object, "identity_version": INPUT_SET_IDENTITY_VERSION}))` via `run_identity.input_set_hash`. Do not change `INPUT_SET_IDENTITY_VERSION` unless the identity *construction* changes in a way that must invalidate all prior hashes — and note that adding keys to an identity object already changes its hash (existing recorded hashes become stale → recompute, which is correct).
- **Nix packages mirror the existing `abMorphRun`/`abAatToParserIr` shape:** `pname`, `version = "0.1.0"`, `src = source`, `cargoDeps = abCargoDeps`, `cargoBuildFlags = ["--package" "<crate>"]`, `doCheck = false`, wrapped in the `if hasCargoManifest && hasCargoLock then … else writeShellApplication` fallback.
- **`--jobs` is parallelism only** and stays OUT of identity. `--report-id`, `--out-dir` are addressing, not identity.
- Tests run via `python -m unittest discover` (pytest is not in the default devShell); the identity/skip unit suites are what the `monorepo-batch-run-staleness` flake check exercises.

---

### Task 1: Nix-package `ab-index` and `ab-check`

Package the two AAT-generator binaries through nix so their store paths are content ids that the identity can pin — exactly as `.#ab-morph-run` did for the warehouse engine. Both crates are light (`ab-check` depends only on `ab-plaintext`; `ab-index` has no heavy native deps), so their packages need none of `abMorphRun`'s pdfium/gaiji/zstd inputs — start minimal.

**Files:**
- Modify: `ab-validator/flake.nix` — add `abIndex` and `abCheck` derivations next to `abMorphRun` (after it, ~line 1393); export `ab-index`/`ab-check` in the `packages` block (~line 1500).

**Interfaces:**
- Produces: flake outputs `packages.ab-index` (→ `bin/ab-index`) and `packages.ab-check` (→ `bin/ab-check`), consumed by Task 3's recipe wiring.

- [ ] **Step 1: Add the `abIndex` derivation** immediately after `abMorphRun` (after its `else … writeShellApplication` block, before `abAatToParserIrCheck`). Mirror `abAatToParserIr` (the light package), NOT `abMorphRun` (which carries pdfium). Minimal inputs:

```nix
        # ab-index: builds the corpus feature index (index.json) consumed by
        # ab-check. Packaged through nix so run-aat-full.sh can pin it by content
        # (store path) in the AAT dump's input identity — running it via cargo from
        # live source would leave the indexer's version out of the dump identity.
        abIndex =
          if hasCargoManifest && hasCargoLock then
            rustPlatform.buildRustPackage {
              pname = "ab-index";
              version = "0.1.0";
              src = source;
              cargoDeps = abCargoDeps;
              nativeBuildInputs = [ pkgs.pkg-config ];
              AB_ABC_ROOT = "${abcSchemaRootForNix}";
              cargoBuildFlags = [ "--package" "ab-index" ];
              doCheck = false;
            }
          else
            pkgs.writeShellApplication {
              name = "ab-index";
              text = ''
                echo 'Rust workspace not scaffolded' >&2
                exit 1
              '';
            };
```

- [ ] **Step 2: Add the `abCheck` derivation** right after `abIndex`, same shape, `pname = "ab-check"`, `cargoBuildFlags = ["--package" "ab-check"]`:

```nix
        # ab-check: the fidelity engine that produces the aat/ tree (runs the
        # adapter per work, emits AAT JSON). Packaged through nix so its version is
        # pinnable by content in the dump identity — it is the primary output
        # producer, so leaving it unpinned is the worst engine-hole.
        abCheck =
          if hasCargoManifest && hasCargoLock then
            rustPlatform.buildRustPackage {
              pname = "ab-check";
              version = "0.1.0";
              src = source;
              cargoDeps = abCargoDeps;
              nativeBuildInputs = [ pkgs.pkg-config ];
              AB_ABC_ROOT = "${abcSchemaRootForNix}";
              cargoBuildFlags = [ "--package" "ab-check" ];
              doCheck = false;
            }
          else
            pkgs.writeShellApplication {
              name = "ab-check";
              text = ''
                echo 'Rust workspace not scaffolded' >&2
                exit 1
              '';
            };
```

- [ ] **Step 3: Export both packages** in the `packages` block (after `ab-morph-run = abMorphRun;`, ~line 1500):

```nix
          ab-index = abIndex;
          ab-check = abCheck;
```

- [ ] **Step 4: Build both and verify the binaries exist**

Run: `cd ab-validator && nix build .#ab-index --no-link --print-out-paths && nix build .#ab-check --no-link --print-out-paths`
Expected: two store paths print; `test -x "$(nix build .#ab-index --no-link --print-out-paths)/bin/ab-index"` and the `ab-check` equivalent both succeed.

If the build fails for a missing native input, add the minimal `buildInputs`/`nativeBuildInputs` the error names (e.g. `pkgs.zstd`) — do NOT copy `abMorphRun`'s full input set wholesale; add only what the compiler actually requires, and note what you added in the report.

- [ ] **Step 5: Commit**

```bash
git add ab-validator/flake.nix
git commit -m "feat(nix): package ab-index and ab-check as focused flake outputs"
```

---

### Task 2: Pin `ab-index` and `ab-check` in the generator identity

The correctness fix. Add both binaries' content hashes to the AAT-dump identity so a logic change in either invalidates the dump — closing the engine-hole. Pure Python + unittest; no shell, fully sandbox-testable.

**Files:**
- Modify: `ab-validator/reports/aat-fidelity/generator_identity.py`
- Modify: `ab-validator/reports/aat-fidelity/tests/test_generator_identity.py`

**Interfaces:**
- Consumes: `hashing.file_sha256(Path) -> str` (already imported), `run_identity.input_set_hash`.
- Produces: `build_identity_object` gains required kwargs `ab_index_binary: str | Path` and `ab_check_binary: str | Path`, adding keys `ab_index_binary_hash` and `ab_check_binary_hash`. `generator_input_set_hash`, `provenance_fields`, and `main()` thread them through. Task 3 supplies the two nix store-path binaries.

- [ ] **Step 1: Write the failing tests.** Add to `test_generator_identity.py` (mirror the existing per-input change tests; the suite already builds a corpus tree, an adapter-binary fixture, and a feature-patterns fixture in `setUp` — add two binary fixtures and pass them through the helper that calls `build_identity_object`). Add fixtures in `setUp`:

```python
        self.ab_index = self.dir / "ab-index"
        self.ab_index.write_bytes(b"INDEXv1")
        self.ab_check = self.dir / "ab-check"
        self.ab_check.write_bytes(b"CHECKv1")
```

Thread `ab_index_binary=self.ab_index, ab_check_binary=self.ab_check` into the `setUp` `self.base` dict (or whatever the existing helper passes to `build_identity_object`). Then add:

```python
    def test_ab_index_binary_change_changes_hash(self) -> None:
        before = self.h()
        self.ab_index.write_bytes(b"INDEXv2")  # indexer rebuilt with a logic change
        self.assertNotEqual(before, self.h())

    def test_ab_check_binary_change_changes_hash(self) -> None:
        before = self.h()
        self.ab_check.write_bytes(b"CHECKv2")  # fidelity engine rebuilt
        self.assertNotEqual(before, self.h())
```

And extend the keyset assertion test to expect the two new keys:

```python
        self.assertEqual(
            set(obj),
            {"corpus_content_hash", "adapter_version", "adapter_binary_hash",
             "ab_index_binary_hash", "ab_check_binary_hash",
             "feature_patterns_hash", "timeout", "features", "work_ids_hash"},
        )
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cd ab-validator && python -m unittest discover -s reports/aat-fidelity/tests -p 'test_generator_identity*.py' -v`
Expected: FAIL — `build_identity_object() got an unexpected keyword argument 'ab_index_binary'` (and keyset mismatch).

- [ ] **Step 3: Add the two keys to `build_identity_object`.** New signature and body (keep the existing docstring, extend it to name the two binaries as content-pinned output producers):

```python
def build_identity_object(
    *,
    corpus_dir: str | Path,
    adapter_version: str,
    adapter_binary: str | Path,
    ab_index_binary: str | Path,
    ab_check_binary: str | Path,
    feature_patterns_file: str | Path,
    timeout: str | None = None,
    features: str | None = None,
    work_ids: str | None = None,
) -> dict[str, Any]:
    return {
        "corpus_content_hash": tree_hash.tree_hash(corpus_dir),
        "adapter_version": adapter_version,
        "adapter_binary_hash": hashing.file_sha256(Path(adapter_binary)),
        "ab_index_binary_hash": hashing.file_sha256(Path(ab_index_binary)),
        "ab_check_binary_hash": hashing.file_sha256(Path(ab_check_binary)),
        "feature_patterns_hash": hashing.file_sha256(Path(feature_patterns_file)),
        "timeout": timeout,
        "features": features,
        "work_ids_hash": (
            hashing.file_sha256(Path(work_ids)) if work_ids is not None else None
        ),
    }
```

- [ ] **Step 4: Thread the two kwargs through `main()`'s CLI.** Add two required args and pass them into `generator_input_set_hash`:

```python
    ap.add_argument("--ab-index-binary", required=True)
    ap.add_argument("--ab-check-binary", required=True)
```

and in the `print(generator_input_set_hash(...))` call add `ab_index_binary=a.ab_index_binary, ab_check_binary=a.ab_check_binary,`. (`generator_input_set_hash` and `provenance_fields` use `**kwargs`, so they need no change beyond receiving the new keys.)

- [ ] **Step 5: Run tests to verify they pass**

Run: `cd ab-validator && python -m unittest discover -s reports/aat-fidelity/tests -p 'test_generator_identity*.py' -v`
Expected: PASS (all, including the two new change tests and the keyset test).

- [ ] **Step 6: Commit**

```bash
git add ab-validator/reports/aat-fidelity/generator_identity.py ab-validator/reports/aat-fidelity/tests/test_generator_identity.py
git commit -m "feat(fidelity): pin ab-index and ab-check binaries in AAT-dump identity"
```

---

### Task 3: Run `ab-index`, `ab-check`, and the adapter from nix in `run-aat-full.sh`; feed identity up front

Replace the three live-source cargo/just builds with nix store-path binaries, resolve them BEFORE any output is written, and pass all three into the identity computation. This closes the "all code through nix" goal for the generator AND supplies Task 2's new identity inputs. Unsandboxable (needs a real nix build + adapter); verify with `--print-plan` and targeted dry checks, not a full 45 GB run.

**Files:**
- Modify: `ab-validator/reports/aat-fidelity/run-aat-full.sh`

**Interfaces:**
- Consumes: `packages.ab-index`, `packages.ab-check` (Task 1); existing `aozora-adapter`/`aozora2html-adapter`/`aozora-epub3-adapter` nix packages; `generator_identity.build_identity_object`'s new kwargs (Task 2).
- Produces: `metadata.json` whose `input_set_hash` now covers the two engine binaries; the resolved binary paths available before the compute region.

- [ ] **Step 1: Resolve the three binaries from nix, up front.** In the per-adapter `case` (~line 91-121), replace the cargo-built `adapter=…` + `build_step=(run_just …)` pairs so `adapter` is a nix store-path binary. Map each adapter id to its flake attr (`aozora`→`aozora-adapter`, `aozora2html`→`aozora2html-adapter`, `aozora-epub3`→`aozora-epub3-adapter`) and set, per case:

```bash
    adapter_attr="aozora2html-adapter"   # (the matching attr per case)
```

Then after the `case` (after line 127, where addressing vars are finalized), resolve all three binaries once:

```bash
adapter="$(nix build ".#$adapter_attr" --no-link --print-out-paths)/bin/${adapter_attr%-adapter}-adapter"
ab_index_bin="$(nix build .#ab-index --no-link --print-out-paths)/bin/ab-index"
ab_check_bin="$(nix build .#ab-check --no-link --print-out-paths)/bin/ab-check"
```

Confirm each adapter package's actual binary name inside `bin/` first (check `nix build .#aozora2html-adapter --no-link --print-out-paths` then `ls bin/`); adjust the `${…%-adapter}-adapter` expansion if the naming differs. Delete the now-unused `build_step` array and the `run_step build-adapter …` line (line 221) — the adapter is already built by nix. Keep the epub3 `AB_AOZORAEPUB3_JAR` block (lines 223-226); it is orthogonal.

- [ ] **Step 2: Point the index and check steps at the nix binaries.** Replace `run_cargo run -p ab-index --` (line 228) with a direct call to `"$ab_index_bin"`, and `run_cargo "${check_args[@]}"` (line 250) where `check_args` starts `run -p ab-check --` with `"$ab_check_bin"` (drop the `run -p ab-check --` prefix; keep every real flag). So:

```bash
run_step build-index "$index_path" "$ab_index_bin" \
  --corpus "$corpus" \
  --patterns "$repo_root/data/feature-patterns.toml" \
  --output "$index_path"

check_args=(
  --index "$index_path"
  --corpus "$corpus"
  --adapter "$adapter"
  --output "$reports_dir"
  --aat-output "$aat_dir"
  --jobs "$jobs"
  --per-work-timeout "$timeout"
)
# … existing --work-ids / --features appends …
run_step check-corpus "$reports_dir" "$ab_check_bin" "${check_args[@]}"
```

- [ ] **Step 3: Pass the two binaries into the metadata identity call.** In the final metadata `python - … <<'PY'` heredoc, extend the argv list to carry `ab_index_bin` and `ab_check_bin`, unpack them, record them in `metadata` (as `ab_index`/`ab_check` provenance paths), and add them to the `generator_identity.provenance_fields(...)` call:

```python
metadata.update(generator_identity.provenance_fields(
    aat_dir=out / "aat",
    corpus_dir=pathlib.Path(corpus) / "cards",
    adapter_version=metadata["adapter_version"],
    adapter_binary=adapter,
    ab_index_binary=ab_index_bin,
    ab_check_binary=ab_check_bin,
    feature_patterns_file=repo / "data" / "feature-patterns.toml",
    timeout=timeout or None,
    features=features or None,
    work_ids=work_ids or None,
))
```

Add `"$ab_index_bin" "$ab_check_bin"` to the heredoc's positional args and to the `= sys.argv[1:]` unpacking line.

- [ ] **Step 4: Verify the plan still emits and the script parses.**

Run: `cd ab-validator && bash -n reports/aat-fidelity/run-aat-full.sh && reports/aat-fidelity/run-aat-full.sh --adapter aozora2html --print-plan`
Expected: `bash -n` clean; `--print-plan` prints the JSON plan without error. (`--print-plan` returns before any nix build, so it needs no network; if the resolve lines run before the `--print-plan` early-exit, move the early-exit above them or guard the resolves.)

Also grep-verify no live-source builds remain:

Run: `grep -nE 'run_cargo|run_just|cargo (run|build)' reports/aat-fidelity/run-aat-full.sh`
Expected: no matches for the ab-index/ab-check/adapter build paths (the duckdb `uv run` and any unrelated lines are out of scope for this task — note them if present).

- [ ] **Step 5: Commit**

```bash
git add ab-validator/reports/aat-fidelity/run-aat-full.sh
git commit -m "feat(fidelity): run ab-index/ab-check/adapter from nix; pin them in dump identity"
```

---

### Task 4: Wire the generator's active skip

With identity now complete (Tasks 2-3), it is safe to skip recompute when a prior dump at `out_dir` is fresh for the current inputs. Reuses the already-built-and-tested `generator_skip.is_fresh`.

**Files:**
- Modify: `ab-validator/reports/aat-fidelity/run-aat-full.sh`

**Interfaces:**
- Consumes: `generator_skip.is_fresh(out_dir, current_input_set_hash) -> {"fresh": bool, "reason": str}` and `generator_identity.generator_input_set_hash(**kwargs)` — both exist.

- [ ] **Step 1: Compute the current `input_set_hash` before the destructive `rm -rf`.** After the binaries are resolved (Task 3, Step 1) and after the existence/validation checks but BEFORE `rm -rf "$out_dir"` (line 203), compute the hash via the `generator_identity` CLI (Task 2's `main()`):

```bash
current_hash="$(python "$repo_root/reports/aat-fidelity/generator_identity.py" \
  --corpus-dir "$corpus/cards" \
  --adapter-version "$("$adapter" --version)" \
  --adapter-binary "$adapter" \
  --ab-index-binary "$ab_index_bin" \
  --ab-check-binary "$ab_check_bin" \
  --feature-patterns "$repo_root/data/feature-patterns.toml" \
  ${timeout:+--timeout "$timeout"} \
  ${features:+--features "$features"} \
  ${work_ids:+--work-ids "$work_ids"})"
```

- [ ] **Step 2: Replace the "exists → require --force" guard with a fresh-aware gate.** The current block (lines 197-201) errors when `out_dir` exists without `--force`. Change it so that, when `out_dir` exists and `--force` was NOT passed, it consults freshness:

```bash
if [[ -e "$out_dir" && "$force" != "1" ]]; then
  if python "$repo_root/reports/aat-fidelity/generator_skip.py" \
       --out-dir "$out_dir" --input-set-hash "$current_hash"; then
    printf '%s AAT dump already fresh, skipping: %s\n' "$adapter_id" "$out_dir"
    exit 0
  fi
  printf 'output directory exists and is stale (or unverifiable): %s\n' "$out_dir" >&2
  printf 'pass --force to replace it\n' >&2
  exit 2
fi
```

`generator_skip.py` exits 0 on fresh, 1 otherwise, and prints its reason to stderr — so a fresh dump short-circuits with exit 0, a stale one keeps the fail-closed "pass --force" behavior. `--force` still bypasses entirely (recompute).

- [ ] **Step 3: Verify the gate logic parses and the skip path triggers on a hand-built fixture.**

Run: `cd ab-validator && bash -n reports/aat-fidelity/run-aat-full.sh`
Expected: clean.

Then a focused unit check that the skip decision is wired to the same hash the metadata records — construct a tiny fake `out_dir` with a `metadata.json` whose `input_set_hash` matches a known-inputs hash and a matching `aat/` tree, and assert `generator_skip.py` exits 0. (This is covered by the existing `test_generator_skip*.py` suite; run it to confirm no regression:)

Run: `python -m unittest discover -s reports/aat-fidelity/tests -p 'test_generator_skip*.py' -v`
Expected: PASS.

- [ ] **Step 4: Commit**

```bash
git add ab-validator/reports/aat-fidelity/run-aat-full.sh
git commit -m "feat(fidelity): skip AAT dump recompute when a fresh dump exists for current inputs"
```

---

### Task 5: Quality — dedup the tree fold; record the full identity object

Two review-carried cleanups (#5). Behavior-preserving. `warehouse_index.hash_run_dir` is a third copy of the sorted-path/content fold that `tree_hash.tree_hash` already generalizes; collapse it. And record the generator's whole `identity_object` (not only the two derived hashes) in `metadata.json` so a future audit can see *which* input changed, not just *that* the hash moved.

**Files:**
- Modify: `ab-validator/reports/morph-warehouse/warehouse_index.py`
- Modify: `ab-validator/reports/morph-warehouse/tests/test_warehouse_index.py`
- Modify: `ab-validator/reports/aat-fidelity/generator_identity.py` (add an identity-object accessor for metadata)
- Modify: `ab-validator/reports/aat-fidelity/run-aat-full.sh` (record it)

**Interfaces:**
- Consumes: `tree_hash.tree_hash(root, *, pattern, exclude_names)`.
- Produces: `warehouse_index.hash_run_dir` delegates to `tree_hash.tree_hash`; `metadata.json` gains an `input_identity` object.

- [ ] **Step 1: Confirm the two folds are byte-identical before collapsing.** Read `warehouse_index.hash_run_dir` and `tree_hash.tree_hash`. They must produce the same digest for the same tree (same sorted-relative-POSIX-path + `\0` + file-digest + `\n` construction, same `HASH_PREFIX`). If and only if they match, proceed. If they differ in any byte of construction, STOP and report — do not silently change recorded warehouse hashes.

- [ ] **Step 2: Add a characterization test** pinning the current `hash_run_dir` output on a fixture tree, so the refactor is proven behavior-preserving:

```python
    def test_hash_run_dir_matches_tree_hash(self) -> None:
        import tree_hash
        # same exclude the runner uses for the manifest
        self.assertEqual(
            wix.hash_run_dir(self.run_dir, exclude_names=("run-manifest.json",)),
            tree_hash.tree_hash(self.run_dir, exclude_names=("run-manifest.json",)),
        )
```

Run it against the CURRENT implementation first: it must PASS before you change `hash_run_dir` (that is what makes it a characterization test).

- [ ] **Step 3: Collapse `hash_run_dir` to delegate to `tree_hash.tree_hash`**, preserving its signature and default `exclude_names=("run-manifest.json",)`. Keep the docstring noting it is the warehouse-run view of the general fold.

- [ ] **Step 4: Run the warehouse suite**

Run: `cd ab-validator && python -m unittest discover -s reports/morph-warehouse/tests -p 'test_warehouse_*.py' -v`
Expected: PASS (all, including the new characterization test and the existing index/runner tests).

- [ ] **Step 5: Record the full identity object in the AAT metadata.** Add to `generator_identity.py` a thin helper returning the identity object for metadata (so the script need not re-list kwargs):

```python
def identity_fields(**identity_kwargs: Any) -> dict[str, Any]:
    """The full identity object, for recording in metadata.json alongside the
    derived input_set_hash — so an audit can see WHICH input changed."""
    return build_identity_object(**identity_kwargs)
```

In `run-aat-full.sh`'s metadata heredoc, after the `provenance_fields` update, add `metadata["input_identity"] = generator_identity.identity_fields(**same_kwargs)` (factor the shared kwargs dict so `provenance_fields` and `identity_fields` receive identical inputs — one dict, two calls). Confirm `bash -n` clean and `--print-plan` still works.

- [ ] **Step 6: Commit**

```bash
git add ab-validator/reports/morph-warehouse/warehouse_index.py ab-validator/reports/morph-warehouse/tests/test_warehouse_index.py ab-validator/reports/aat-fidelity/generator_identity.py ab-validator/reports/aat-fidelity/run-aat-full.sh
git commit -m "refactor: collapse hash_run_dir into tree_hash; record full input identity in AAT metadata"
```

---

## Self-Review

- **Spec coverage:** #6 (nix-package ab-index/ab-check + nix adapters) = Tasks 1, 3. #4 (pin the two binaries in identity) = Task 2. #1 (wire active skip) = Task 4. #5 (dedup fold + record identity object) = Task 5. All four follow-ups covered.
- **Safety-ordering:** Task 4 (skip) strictly follows Tasks 2-3 (complete identity) — the plan cannot wire skip before the identity pins the two producers. Preserved by task order.
- **Type consistency:** `build_identity_object` gains `ab_index_binary`/`ab_check_binary`; every caller (`generator_input_set_hash`, `provenance_fields`, `identity_fields`, `main()`, the script) passes both. `generator_skip.is_fresh` and `generator_identity.main()` signatures already accept what Task 4 sends.
- **Pinned-bytes guard:** Task 5 collapses only `hash_run_dir` (not the pinned `aat_hash.hash_aat_dir`) and gates the change behind a characterization test proving byte-equality first.
- **Fail-closed:** Task 4's gate skips on fresh ONLY; stale/unverifiable → the existing fail-closed `--force` path.
