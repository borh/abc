# Warehouse Variant Gating + Nix Completions Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Two independent completions of the merged staleness/nix work: (A) route the two bespoke morph-warehouse recipes (`-historical`, `-lane-b`) through the existing skip gate so they stop always-recomputing; (B) finish "all through nix" for the AAT generator — nixify the duckdb triage step and build the wrapper adapters' Rust mappers via nix instead of in-tree cargo.

**Architecture:** Extends the merged `2026-07-09-generator-identity-completeness-nix.md` and `2026-07-09-wrapper-adapter-identity-completeness.md`. Part A is a pure justfile recipe rewrite — the `warehouse_runner.py` CLI and `warehouse_identity.build_identity_object` already accept and hash every input the two variants vary (`--ortho-detect`, `--works-parquet`, dict-variant `--analyzer`), so there is NO Python change and NO stale-as-fresh hole; the variants just never adopted the wired `-with-analyzers` shape. Part B has two pieces: B1 replaces `uv run --with duckdb` with a nix `python3.withPackages([duckdb])`; B2 resolves the wrapper adapters' Rust mappers from the existing flake packages (`aozora2html-adapter`, `aozora-epub3-adapter`) and pins the store-path binary in identity, which REQUIRES adding a mapper-path env override to the two wrapper scripts (they currently hardcode the in-tree `target/release` path).

**Tech Stack:** Bash (justfile recipes, `run-aat-full.sh`, wrapper adapter scripts), Nix (`rustPlatform.buildRustPackage` packages already exist; add one `python3.withPackages`), Python 3 (unchanged).

## Global Constraints

- **No stale-as-fresh, ever.** Every output-determining input must be in the identity. Part A adds no new inputs (all already hashed). Part B2's mapper switch must keep `adapter_hash_target` (the binary identity hashes) byte-identical to the binary the wrapper actually execs — otherwise recorded≠checked and skip silently breaks or serves stale.
- **Recorded == checked.** In B2, the wrapper's `--version` path AND its `exec` path AND `run-aat-full.sh`'s `adapter_hash_target` must all point at the SAME nix mapper binary. The gate runs `"$adapter" --version` from `$repo_root` (`run-aat-full.sh`) to build the identity string; the wrapper's `--version` execs the mapper, so the mapper env override must govern `--version` too.
- **Byte-identical compute.** In Part A, the flags the recipe passes to `warehouse_runner.py` (which build identity) must match the flags in the post-`--` compute command (`"$engine_bin" analyze-aat ...`) — same `--analyzer`/`--ortho-detect`/`--works-parquet`/`--warehouse-profile`. Mirror the `-with-analyzers` recipe exactly.
- **`--print-plan` stays build-free** in `run-aat-full.sh`: all nix/cargo resolution happens past the early-exit.
- **Repo-anchored flake refs:** `nix build "$repo_root#<attr>"` (bare `.#` resolves against caller PWD and breaks from the monorepo root — established bug).
- **Do not delete the `aat_duckdb_bin`/`aat_setup_duckdb_runtime` helpers** in `tests/lib/aat-fidelity-env.sh` — ~10 other test scripts call them. B1 only drops the two CALL SITES in `run-aat-full.sh`.
- Tests run via `python -m unittest discover` (pytest not in the default devShell). No Python is changed in this plan; run the existing suites to confirm no regression.

---

## PART A — Gate the bespoke morph-warehouse variants

### Task 1: Route `-historical` and `-lane-b` through `warehouse_runner.py`

Rewire both recipes to the wired `-with-analyzers` shape: resolve `ab-morph-run` from nix, pass all inputs to `warehouse_runner.py`, and run the byte-identical compute command after `--`. No Python change.

**Files:**
- Modify: `ab-validator/justfile` (recipes `morph-warehouse-run-historical`, `morph-warehouse-run-lane-b`)

**Interfaces:**
- Consumes: `reports/morph-warehouse/warehouse_runner.py` `main()` — already accepts `--aat-dir --engine-binary --warehouse-dir --run-id --warehouse-profile --analyzer(repeat) --ortho-detect --works-parquet --dict(repeat) --schema-file(repeat) --force`, splits argv on the first `--`.
- Reference: the WIRED `morph-warehouse-run-with-analyzers` recipe in the same justfile is the exact structural template.

- [ ] **Step 1: Read the three recipes.** Open `ab-validator/justfile` and read `morph-warehouse-run-with-analyzers` (the wired template), `morph-warehouse-run-historical`, and `morph-warehouse-run-lane-b` in full. Note the template's structure: `force=""` param; a `jobs`/`run_id` default block; a `force_flag=()` array; `vibrato_dir`/`sudachi_dic`/`engine_bin` resolved via `nix build .#<attr> --no-link --print-out-paths`; a `python3 reports/morph-warehouse/warehouse_runner.py` call with wrapper args; then `--` and the byte-identical `"$engine_bin" analyze-aat ...` compute command.

- [ ] **Step 2: Rewire `morph-warehouse-run-historical`.** Add a `force=""` parameter to its signature (after `jobs="0"`). Replace its body so it (a) resolves `vibrato_dir` (it uses vibrato only — no sudachi) and `engine_bin` via `nix build .#vibrato-dictionaries` / `.#ab-morph-run` exactly as `-with-analyzers` does; (b) builds a `force_flag=()` array set to `(--force)` when `{{force}}` is non-empty; (c) calls `python3 reports/morph-warehouse/warehouse_runner.py` with: `--aat-dir "{{aat_dir}}"`, `--engine-binary "$engine_bin"`, `--analyzer "vibrato:{{dict}}"`, `--warehouse-dir "{{morph_warehouse_dir}}"`, `--run-id "$run_id"`, `--warehouse-profile "{{profile}}"`, `--dict "vibrato=$vibrato_dir"`, `--schema-file crates/ab-morph-run/sql/schema.sql`, `--schema-file crates/ab-warehouse/sql/schema.sql`, `--works-parquet "{{works_parquet}}"`, `"${force_flag[@]}"`; then `--`; then the compute command `"$engine_bin" analyze-aat --aat-dir "{{aat_dir}}" --analyzer "vibrato:{{dict}}" --warehouse-dir "{{morph_warehouse_dir}}" --run-id "$run_id" --warehouse-profile "{{profile}}" --works-parquet "{{works_parquet}}" --jobs "$jobs"`. Set `AB_VIBRATO_DICT_DIR="$vibrato_dir"` on the invocation (the compute command needs it at runtime), mirroring how `-with-analyzers` exports `AB_VIBRATO_DICT_DIR`/`AB_SUDACHI_DICT` before the `python3` call. The `run_id` default block (`if [ -z "$run_id" ]; then run_id="historical-{{dict}}-$(date ...)"; fi`) is preserved.

- [ ] **Step 3: Rewire `morph-warehouse-run-lane-b`.** Same transformation. Add `force=""` param. It uses BOTH vibrato and sudachi (its analyzer loop already builds `--analyzer` args from `{{analyzers}}`), so resolve `vibrato_dir` AND `sudachi_dic` AND `engine_bin` via nix, and export `AB_VIBRATO_DICT_DIR`/`AB_SUDACHI_DICT`. Build the `args=(--analyzer ...)` array from `{{analyzers}}` exactly as the current recipe and `-with-analyzers` do. Call `warehouse_runner.py` with `--aat-dir`, `--engine-binary "$engine_bin"`, `"${args[@]}"`, `--warehouse-dir`, `--run-id`, `--warehouse-profile`, `--dict "vibrato=$vibrato_dir"`, `--dict "sudachi=$sudachi_dic"`, `--schema-file crates/ab-morph-run/sql/schema.sql`, `--schema-file crates/ab-warehouse/sql/schema.sql`, `--ortho-detect historical`, `--works-parquet "{{works_parquet}}"`, `"${force_flag[@]}"`; then `--`; then `"$engine_bin" analyze-aat --aat-dir "{{aat_dir}}" "${args[@]}" --warehouse-dir "{{morph_warehouse_dir}}" --run-id "$run_id" --warehouse-profile "{{profile}}" --ortho-detect historical --works-parquet "{{works_parquet}}" --jobs "$jobs"`. Preserve the lane-b `run_id` default block.

- [ ] **Step 4: Verify the recipes parse and expand correctly (no batch run).**

Run: `cd ab-validator && just --list 2>/dev/null | grep -E 'morph-warehouse-run-(historical|lane-b)'`
Expected: both recipes still listed (parse OK).

Run: `cd ab-validator && just -n morph-warehouse-run-historical /tmp/fake.parquet 2>&1 | head -40` and the same for `just -n morph-warehouse-run-lane-b /tmp/fake.parquet`.
Expected: the dry-run expansion shows the `warehouse_runner.py` invocation followed by `--` and the `analyze-aat` compute command, with the SAME `--analyzer`/`--ortho-detect`/`--works-parquet`/`--warehouse-profile` on both sides. (nix builds may run during `just -n`? No — `just -n` prints without executing, so the `$(nix build ...)` substitutions appear literally. That is fine for verifying structure.) Manually confirm the post-`--` flags match the pre-`--` identity-determining flags.

- [ ] **Step 5: Confirm no identity gap (read-only cross-check).** Confirm `warehouse_identity.build_identity_object` hashes every input these recipes vary: `ortho_detect` (lane-b), `works_parquet` (both, by content hash), dict-variant `analyzers` (historical's `vibrato:{{dict}}`). This is already true (the identity module was built as a superset) — this step is a read-only sanity check, not a change. State the confirmation in the report.

- [ ] **Step 6: Commit**

```bash
git add ab-validator/justfile
git commit -m "feat(warehouse): gate morph-warehouse-run-historical and -lane-b through the skip runner"
```

---

## PART B — Nix completions for the AAT generator

### Task 2: Nixify the duckdb triage step

Replace `uv run --isolated --no-project --with 'duckdb>=1.1'` with a nix `python3.withPackages([duckdb])` interpreter, and drop the now-unnecessary `libstdc++` helper calls at the triage call site.

**Files:**
- Modify: `ab-validator/flake.nix` (add a `pythonWithAatDuckdb` binding + expose it as a package)
- Modify: `ab-validator/reports/aat-fidelity/run-aat-full.sh` (triage step)

**Interfaces:**
- Produces: flake output `packages.aat-triage-python` (a `python3.withPackages(ps: [ps.duckdb])`), consumed by `run-aat-full.sh` via `nix build "$repo_root#aat-triage-python" --print-out-paths`.
- `build-aat-batch-triage.py` does `import duckdb` (module) and never shells a CLI — so a duckdb-enabled interpreter is the correct and sufficient dependency.

- [ ] **Step 1: Add the interpreter binding in `flake.nix`.** Near the existing `pythonWithAatSchemaDeps = pkgs.python3.withPackages (ps: [ ps.jsonschema ps.tomli ps.pytest ]);` binding, add:

```nix
        pythonWithAatDuckdb = pkgs.python3.withPackages (ps: [ ps.duckdb ]);
```

Expose it in the `packages` block (near `ab-index = abIndex;` / `ab-check = abCheck;`):

```nix
          aat-triage-python = pythonWithAatDuckdb;
```

- [ ] **Step 2: Build it and confirm the interpreter imports duckdb.**

Run: `cd ab-validator && p="$(nix build ".#aat-triage-python" --no-link --print-out-paths)" && "$p/bin/python3" -c 'import duckdb; print(duckdb.__version__)'`
Expected: prints a version ≥ 1.1 (pinned nixpkgs has 1.5.2).

- [ ] **Step 3: Swap the triage step in `run-aat-full.sh`.** The current block is:

```bash
duckdb_bin="$(aat_duckdb_bin)"
aat_setup_duckdb_runtime "$duckdb_bin"

run_step build-triage "$triage_dir" uv run --isolated --no-project --with 'duckdb>=1.1' \
  "$repo_root/reports/aat-fidelity/build-aat-batch-triage.py" \
  ...
```

Replace it with (resolve the nix interpreter up front, past the `--print-plan` early-exit — put the resolution next to the `ab_index_bin`/`ab_check_bin` resolution, or immediately before this step; do NOT resolve it before the early-exit):

```bash
triage_python="$(nix build "$repo_root#aat-triage-python" --no-link --print-out-paths)/bin/python3"

run_step build-triage "$triage_dir" "$triage_python" \
  "$repo_root/reports/aat-fidelity/build-aat-batch-triage.py" \
  --reports-dir "$reports_dir" \
  --aat-dir "$aat_dir" \
  --db "$db_path" \
  --report-id "$report_id" \
  --out-dir "$triage_dir"
```

Delete the `duckdb_bin=...` and `aat_setup_duckdb_runtime "$duckdb_bin"` lines at this call site (the nix interpreter carries its own correct `libstdc++` via RPATH; the manual `LD_LIBRARY_PATH` patch is not needed). Do NOT remove the helper definitions in `tests/lib/aat-fidelity-env.sh`.

- [ ] **Step 4: Verify.**

Run: `cd ab-validator && bash -n reports/aat-fidelity/run-aat-full.sh` → clean.
Run: `reports/aat-fidelity/run-aat-full.sh --adapter aozora2html --corpus <dir-with-cards/> --print-plan` → fast, ZERO builds (the `nix build` for the interpreter must be past the early-exit).

**Runtime smoke of the triage script under the nix interpreter** (this is the real proof the interpreter suffices without the `libstdc++` helper): build a tiny fixture that exercises `build-aat-batch-triage.py` end-to-end under `"$triage_python"`, OR — if a full fixture is heavy — at minimum run `"$triage_python" "$repo_root/reports/aat-fidelity/build-aat-batch-triage.py" --help` and confirm it imports duckdb without an `LD_LIBRARY_PATH` error. If the script genuinely needs the `libstdc++` shim even under the nix interpreter (it should not), STOP and report — do not silently drop `aat_setup_duckdb_runtime` if it turns out load-bearing.

- [ ] **Step 5: Commit**

```bash
git add ab-validator/flake.nix ab-validator/reports/aat-fidelity/run-aat-full.sh
git commit -m "feat(nix): run AAT triage via a nix python3+duckdb instead of uv run"
```

---

### Task 3: Build the wrapper Rust mappers via nix; pin the store path

Resolve the two wrapper adapters' Rust mappers from the existing flake packages, inject them into the wrapper via a new env override, and pin the nix store binary in identity. SAFETY-CRITICAL: the hashed binary and the exec'd binary must be identical.

**Files:**
- Modify: `ab-validator/adapters/aozora2html/aozora2html-adapter` (mapper env override)
- Modify: `ab-validator/adapters/aozora-epub3/aozora-epub3-adapter` (mapper env override)
- Modify: `ab-validator/reports/aat-fidelity/run-aat-full.sh` (resolve mapper from nix; drop `build_step`)

**Interfaces:**
- Consumes: flake packages `aozora2html-adapter` (→ `bin/aozora2html-adapter`) and `aozora-epub3-adapter` (→ `bin/aozora-epub3-adapter`) — already build the identical mapper crate.
- The wrappers gain env overrides `AB_AOZORA2HTML_MAPPER_BIN` / `AB_AOZORAEPUB3_MAPPER_BIN` (default = current in-tree path, so non-gated callers are unaffected).

- [ ] **Step 1: Add a mapper env override to `adapters/aozora2html/aozora2html-adapter`.** The current line is:

```bash
RUST_MAPPER_BIN="$REPO_ROOT/adapters/aozora2html/target/release/aozora2html-adapter"
```

Change it to honor an env var, defaulting to the existing in-tree path (so other callers are unchanged):

```bash
RUST_MAPPER_BIN="${AB_AOZORA2HTML_MAPPER_BIN:-$REPO_ROOT/adapters/aozora2html/target/release/aozora2html-adapter}"
```

Confirm `RUST_MAPPER_BIN` is used for BOTH the `--version` path and the final `exec` (it is — the existence check, the `--version` exec, and the mapping exec all reference `$RUST_MAPPER_BIN`), so the override governs every invocation.

- [ ] **Step 2: Add the same override to `adapters/aozora-epub3/aozora-epub3-adapter`.** The current line:

```bash
MAPPER_BIN="${REPO_ROOT}/adapters/aozora-epub3/target/release/aozora-epub3-adapter"
```

becomes:

```bash
MAPPER_BIN="${AB_AOZORAEPUB3_MAPPER_BIN:-${REPO_ROOT}/adapters/aozora-epub3/target/release/aozora-epub3-adapter}"
```

- [ ] **Step 3: Resolve the mapper from nix in `run-aat-full.sh` and export the override.** In the `case` block, the wrapper adapters currently set `build_step=(run_just <x>-rust-build)` and `adapter_hash_target="$repo_root/adapters/<x>/target/release/<x>-adapter"`. Introduce a per-adapter `mapper_attr` (`aozora2html` → `aozora2html-adapter`; `aozora-epub3` → `aozora-epub3-adapter`; `aozora` → `""`), and REMOVE the `build_step=(run_just ...)` assignments (set `build_step=()` for the wrappers too, or drop the machinery — see Step 5). Then, in the post-`--print-plan` resolution region (next to `ab_index_bin`/`ab_check_bin`), for wrapper adapters resolve the mapper from nix and export the override, mirroring the renderer resolution:

```bash
if [[ -n "$mapper_attr" ]]; then
  mapper_bin="$(nix build "$repo_root#$mapper_attr" --no-link --print-out-paths)/bin/$mapper_attr"
  adapter_hash_target="$mapper_bin"
  if [[ "$adapter_id" == "aozora2html" ]]; then
    export AB_AOZORA2HTML_MAPPER_BIN="$mapper_bin"
  elif [[ "$adapter_id" == "aozora-epub3" ]]; then
    export AB_AOZORAEPUB3_MAPPER_BIN="$mapper_bin"
  fi
fi
```

(The binary name inside `bin/` equals the attr name: `aozora2html-adapter` / `aozora-epub3-adapter` — verify with `ls` during implementation.) Now `adapter_hash_target` (hashed into identity) IS the nix mapper, AND the wrapper execs that same nix mapper via the exported override — recorded == checked.

- [ ] **Step 4: Confirm the gate/metadata still align.** `run-aat-full.sh` already computes the gate hash with `--adapter-binary "$adapter_hash_target"` and `--adapter-version "$(cd "$repo_root" && "$adapter" --version)"`; the metadata heredoc uses the same `adapter_hash_target` and `run([adapter,"--version"], cwd=repo)`. Since the wrapper's `--version` now execs the nix mapper (via the override exported in Step 3, which is set before the gate), both `--version` calls return the nix mapper's version and `adapter_hash_target` is the nix mapper — no change needed to the gate/heredoc code, but VERIFY the override is exported BEFORE the gate runs (Step 3's resolution must precede the `if [[ -e "$out_dir" ... ]]` gate).

- [ ] **Step 5: Remove the now-dead `build_step` machinery.** After Step 3, no adapter uses `build_step` (aozora was already empty; wrappers now resolve from nix). Remove: the `build_step=(...)` lines from the wrapper `case` branches, the pre-gate `if [[ ${#build_step[@]} -gt 0 ]]; then "${build_step[@]}"; fi` block, and the post-`workflow_init` `if [[ ${#build_step[@]} -gt 0 ]]; then run_step build-adapter ...; fi` block. If any residual reference to `build_step` remains, the script would error under `set -u` — grep to confirm none remain. (The justfile recipes `aozora2html-rust-build`/`aozora-epub3-build` themselves are NOT removed — other callers may use them.)

- [ ] **Step 6: Verify (unsandboxable).**

Run: `cd ab-validator && bash -n reports/aat-fidelity/run-aat-full.sh` → clean; `grep -n build_step reports/aat-fidelity/run-aat-full.sh` → no matches.
Run: `reports/aat-fidelity/run-aat-full.sh --adapter aozora2html --corpus <dir-with-cards/> --print-plan` → fast, build-free; same for `aozora-epub3` and `aozora`.

**Recorded==checked proof (the key safety check):** for `aozora2html`, resolve `MB="$(nix build "$(pwd)#aozora2html-adapter" --no-link --print-out-paths)/bin/aozora2html-adapter"`, then run the wrapper with the override and confirm it execs THAT binary: `AB_AOZORA2HTML_MAPPER_BIN="$MB" adapters/aozora2html/aozora2html-adapter --version` returns the same string as `"$MB" --version`. Confirm `file_sha256("$MB")` equals what `generator_identity` would hash for `adapter_hash_target`. Capture in the report. Repeat the `--version` equality for `aozora-epub3`.

- [ ] **Step 7: Commit**

```bash
git add ab-validator/adapters/aozora2html/aozora2html-adapter ab-validator/adapters/aozora-epub3/aozora-epub3-adapter ab-validator/reports/aat-fidelity/run-aat-full.sh
git commit -m "feat(nix): build wrapper-adapter Rust mappers via nix; pin store-path binary in identity"
```

---

## Self-Review

- **Spec coverage:** Follow-up #2 = Task A1 (both variant recipes gated). Follow-up #4 = Task B1 (duckdb nix) + Task B2 (mapper nix). Covered.
- **No stale-as-fresh:** Part A adds no new output-determinant (identity already hashes ortho_detect/works_parquet/analyzer-variants — confirmed by scope, re-confirmed in A1 Step 5). B2 keeps `adapter_hash_target` == the exec'd mapper by resolving both from the same nix store path and governing the wrapper's `--version`+exec via the override; the proof in B2 Step 6 verifies it.
- **Recorded == checked:** A1 mirrors the wired recipe's pre-`--`/post-`--` flag symmetry (byte-identical compute). B2 exports the mapper override before the gate so `--version` (identity) and exec use the same binary.
- **Type consistency:** B2 keeps `file_sha256` for the mapper (a single file, works on nix store paths); no `tree_hash` switch (that is only for the renderer directory). B1's interpreter is a `python3.withPackages` mirroring the existing `pythonWithAatSchemaDeps`.
- **Independence:** Part A touches only `justfile`; Part B touches `flake.nix`, `run-aat-full.sh`, and the two wrapper scripts. No cross-part file conflict; each part is independently testable and mergeable.
- **Fail safe:** B1's smoke step stops if the nix interpreter can't import duckdb without the shim; B2 defaults the wrapper override to the in-tree path so non-gated callers are unaffected.
