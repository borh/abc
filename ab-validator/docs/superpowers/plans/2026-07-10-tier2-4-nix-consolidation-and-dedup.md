# Tier 2–4: Nix Consolidation, Dedup, and Local Cleanups Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Eliminate remaining hardcoded toolchain invocations and duplicated work in the ab-validator batch/fidelity pipeline by routing binaries through nix flake outputs (Tier 2), collapsing byte-identical duplicated code behind a golden-pinned shared module (Tier 3), and tidying two local hazards (Tier 4).

**Architecture:** The repo already content-addresses its toolchain through `ab-validator/flake.nix` (Rust bins via `rustPlatform.buildRustPackage`, a duckdb-bearing `python3.withPackages` as `aat-triage-python`) and injects them into scripts via `AB_*_BIN` env overrides with a build-from-source fallback (the `sourceInventoryBin` / `AB_SOURCE_INVENTORY_BIN` pattern). This plan extends that established pattern to the last few sites that still shell out to `cargo run` / `uv run` directly, then removes duplicated canonicalization code (PINNED — guarded by a byte-equality golden test) and two small local smells.

**Tech Stack:** Nix flakes, Bash smoke scripts, Rust (cargo), Python 3 (stdlib + duckdb), `uv`.

## Global Constraints

- **PINNED bytes must not change.** `canonical_json` (abc-legacy-json-c14n-v0) output feeds schema/document hashes that are compared across adapters and against the `abc` side. Any refactor of it MUST be byte-for-byte behavior-preserving, proven by a golden test. Never "clean up" its formatting or escaping.
- **Fallback preservation.** Every `AB_*_BIN` / env-override introduced MUST keep the pre-existing `cargo run` / `uv run` path as the fallback when the env var is unset/empty. Unset env ⇒ identical behavior to today.
- **License safety.** Do not link against or vendor GPL renderers; only content-address unmodified subprocess binaries (not touched by this plan, but hold the line).
- **Do not converge distinct contracts.** Two intentionally-separate designs are OUT OF SCOPE and must not be "unified": the two skip-gate wrappers (`generator_skip` vs `warehouse_index`) and the two identity builders. See "Decisions — considered and rejected" below.
- Match surrounding code style (bash `set -euo pipefail`, existing quoting/array idioms; Python `from __future__ import annotations`, existing import ordering).
- Verify shell edits with `bash -n`; verify nix edits with `nix build`/`nix eval` on the touched attribute (repo-anchored refs, never bare `.#` in non-interactive code).

## Decisions — considered and rejected (do NOT implement)

- **Shared `freshness.verify()` core (rejected):** `generator_skip.is_fresh` returns binary `{fresh, reason}` over a `metadata.json`; `warehouse_index.check` returns a 4-state `{status: missing|invalid|stale|fresh, run_dir, recorded, actual}` after resolving a by-input symlink. The shared substance (input-hash compare, output-recompute compare) is ~6 lines and already sits atop the shared `run_identity.input_set_hash`. A common helper would either flatten warehouse's status taxonomy (a behavior change on a skip gate) or thread callbacks to save a few lines (net complexity). Leave both as-is.
- **Deleting `abc/tools/schema_contracts.py` (rejected here):** redundant with `scripts/abc_schema_contracts.py` but NOT dead — it is executed by `abc/flake.nix:464`. Removing it requires rewiring the separate `abc` flake to call the root `scripts/` version, coupling two deployment units. Track as a follow-up, not this plan.
- **Consolidating the `abc/`, `scripts/`, and `abc/prototypes/` copies of `canonical_json` (out of scope):** they live in separate flakes / deployment units (and a throwaway prototype). A shared import would couple them across the `abc`↔`ab-validator` boundary. Task 5 consolidates only the three copies inside `ab-validator/reports/`, which already share a `sys.path` convention.

---

## PART A — Tier 2: Nix Consolidation

### Task 1: Expose `ab-source-inventory` as a flake package output

The `sourceInventoryBin` derivation already exists (`ab-validator/flake.nix:1243`) and is injected into two checks via `AB_SOURCE_INVENTORY_BIN`, but it is not in the `packages` output set, so `nix build .#ab-source-inventory` fails. Expose it. The two test scripts already prefer `AB_SOURCE_INVENTORY_BIN` with a `cargo run` fallback — no script change needed.

**Files:**
- Modify: `ab-validator/flake.nix` (packages block, near line 1674)

**Interfaces:**
- Consumes: existing `sourceInventoryBin` derivation.
- Produces: `packages.ab-source-inventory` flake output.

- [ ] **Step 1: Verify the output does not yet exist (expected failure)**

Run (from `ab-validator/`):
```bash
nix build "$(pwd)#ab-source-inventory" --no-link 2>&1 | tail -3
```
Expected: an error that the attribute `ab-source-inventory` is missing / not found.

- [ ] **Step 2: Add the package output**

In `ab-validator/flake.nix`, in the `packages = { ... }` set, immediately after the `aat-triage-python = pythonWithAatDuckdb;` line, add:
```nix
          ab-source-inventory = sourceInventoryBin;
```

- [ ] **Step 3: Verify it builds**

Run (from `ab-validator/`):
```bash
nix build "$(pwd)#ab-source-inventory" --no-link --print-out-paths
test -x "$(nix build "$(pwd)#ab-source-inventory" --no-link --print-out-paths)/bin/ab-source-inventory" && echo OK
```
Expected: prints a store path and `OK`.

- [ ] **Step 4: Confirm flake still evaluates**

Run: `nix flake check --no-build 2>&1 | tail -5` (from `ab-validator/`). Expected: no evaluation errors introduced by this change.

- [ ] **Step 5: Commit**
```bash
git add ab-validator/flake.nix
git commit -m "feat(flake): expose ab-source-inventory as a package output"
```

---

### Task 2: Package `ab-oracle` as a flake output and route its call sites through `AB_ORACLE_BIN`

`ab-oracle` (crate `ab-validator/crates/ab-oracle`) has no flake derivation; every caller rebuilds it via `cargo run --manifest-path .../ab-oracle/Cargo.toml`. Add a `buildRustPackage` derivation mirroring `sourceInventoryBin`, expose it as a package + app, and switch call sites to the `AB_ORACLE_BIN` env-override pattern (cargo fallback preserved). Wire the env var into the checks that run these scripts.

**Files:**
- Modify: `ab-validator/flake.nix` (add `abOracleBin` derivation near `sourceInventoryBin` ~line 1243; add package output near line 1674; add app near the `apps.ab-aat-to-parser-ir` block; add `AB_ORACLE_BIN` to relevant `mkSmokeCheck` `extraEnv`)
- Modify: `ab-validator/reports/aat-fidelity/run-cross-adapter-report.sh` (2 invocations, lines ~31 and ~45)
- Modify: `ab-validator/tests/aat-fidelity-cross-summary-xhtml-smoke.sh` (line ~30)
- Modify: `ab-validator/tests/adapter-fidelity-smoke.sh` (line ~14)
- Modify: `ab-validator/tests/adapter-oracle-report-smoke.sh` (line ~14)
- Modify: `ab-validator/tests/aat-oracle-audit-smoke.sh` (lines ~12 and ~20)
- Modify: `ab-validator/tests/aozora-rs-oracle-smoke.sh` (line ~15)

**Interfaces:**
- Consumes: `rustPlatform`, `source`, `abCargoDeps` (all already in scope for `sourceInventoryBin`).
- Produces: `packages.ab-oracle`, `apps.ab-oracle`, and an `AB_ORACLE_BIN` contract read by the six scripts (`"$AB_ORACLE_BIN"` when set, else the existing `run_cargo run --manifest-path .../ab-oracle/Cargo.toml --target-dir "$oracle_target" --` form).

- [ ] **Step 1: Add the derivation**

In `ab-validator/flake.nix`, immediately after the `sourceInventoryBin = rustPlatform.buildRustPackage { ... };` block (ends ~line 1257), add:
```nix
        abOracleBin = rustPlatform.buildRustPackage {
          pname = "ab-oracle";
          version = "0.1.0";

          src = source;
          cargoDeps = abCargoDeps;

          cargoBuildFlags = [
            "--package"
            "ab-oracle"
          ];

          doCheck = false;
        };
```

- [ ] **Step 2: Verify the derivation builds**

Run (from `ab-validator/`): `nix build "$(pwd)#ab-oracle" --no-link 2>&1 | tail -3`

Expected at this step: still fails (`ab-oracle` not yet in `packages`). This confirms the attribute isn't accidentally pre-existing. Proceed.

- [ ] **Step 3: Expose package + app**

In the `packages = { ... }` set (after the `ab-source-inventory` line added in Task 1), add:
```nix
          ab-oracle = abOracleBin;
```
And after the `apps.ab-aat-to-parser-ir = ...;` block, add:
```nix
        apps.ab-oracle =
          flake-utils.lib.mkApp {
            drv = abOracleBin;
          }
          // {
            meta.description = "Run the ab-oracle cross-adapter fidelity oracle";
          };
```

- [ ] **Step 4: Verify build + binary**

Run (from `ab-validator/`):
```bash
out=$(nix build "$(pwd)#ab-oracle" --no-link --print-out-paths) && ls "$out/bin" && echo OK
```
Expected: lists an `ab-oracle` binary and prints `OK`. (If the binary name differs, note it — the `[[bin]]` name in `crates/ab-oracle/Cargo.toml` governs the `AB_ORACLE_BIN` target in Step 5.)

- [ ] **Step 5: Switch call sites to the env-override pattern**

In each of the six scripts, replace the `run_cargo run --manifest-path .../ab-oracle/Cargo.toml --target-dir "$oracle_target" --` invocation(s) with a pre-computed command array, defined once per script before its first use:
```bash
if [[ -n "${AB_ORACLE_BIN:-}" ]]; then
  oracle_cmd=("$AB_ORACLE_BIN")
else
  oracle_cmd=(run_cargo run --manifest-path "$AB_VALIDATOR_ROOT/crates/ab-oracle/Cargo.toml" --target-dir "$oracle_target" --)
fi
```
Then change each invocation from `run_cargo run --manifest-path ... --target-dir ... -- <args>` to `"${oracle_cmd[@]}" <args>`.

Notes for the implementer:
- In `run-cross-adapter-report.sh` and `aat-oracle-audit-smoke.sh` there are TWO invocations — define `oracle_cmd` once, reuse for both.
- Scripts that use `"$AB_VALIDATOR_ROOT"` already have it in scope; scripts that use `"$repo_root"` for the manifest path should keep whichever variable they already use for that path — do not introduce a new one. Verify the manifest path variable each script actually uses before editing.
- Preserve every existing `--` separator and argument line verbatim.

- [ ] **Step 6: Verify each script parses**

Run (from repo root):
```bash
for f in ab-validator/reports/aat-fidelity/run-cross-adapter-report.sh \
         ab-validator/tests/aat-fidelity-cross-summary-xhtml-smoke.sh \
         ab-validator/tests/adapter-fidelity-smoke.sh \
         ab-validator/tests/adapter-oracle-report-smoke.sh \
         ab-validator/tests/aat-oracle-audit-smoke.sh \
         ab-validator/tests/aozora-rs-oracle-smoke.sh; do bash -n "$f" && echo "ok $f"; done
```
Expected: `ok` for all six.

- [ ] **Step 7: Wire `AB_ORACLE_BIN` into the checks that run these scripts**

For each `mkSmokeCheck` in `flake.nix` whose `testScript` is one of the six scripts above, add `abOracleBin` to `nativeBuildInputs` and `AB_ORACLE_BIN = "${abOracleBin}/bin/ab-oracle";` to `extraEnv` (use the actual binary name confirmed in Step 4). Grep first: `grep -n "adapter-fidelity-smoke\|adapter-oracle-report-smoke\|aat-oracle-audit-smoke\|aozora-rs-oracle-smoke\|aat-fidelity-cross-summary-xhtml-smoke" flake.nix`. Only edit checks that exist; if a script has no corresponding check, note it in the report (its `cargo` fallback still works locally).

- [ ] **Step 8: Verify one wired check evaluates**

Run (from `ab-validator/`): `nix eval "$(pwd)#checks.$(nix eval --impure --raw --expr builtins.currentSystem).adapter-fidelity-smoke-check.drvPath" 2>&1 | tail -3` (substitute the real check name from Step 7). Expected: prints a `.drv` path (evaluation succeeds). Do NOT run the full check build unless quick.

- [ ] **Step 9: Commit**
```bash
git add ab-validator/flake.nix ab-validator/reports/aat-fidelity/run-cross-adapter-report.sh ab-validator/tests/aat-fidelity-cross-summary-xhtml-smoke.sh ab-validator/tests/adapter-fidelity-smoke.sh ab-validator/tests/adapter-oracle-report-smoke.sh ab-validator/tests/aat-oracle-audit-smoke.sh ab-validator/tests/aozora-rs-oracle-smoke.sh
git commit -m "feat(flake): package ab-oracle; route call sites through AB_ORACLE_BIN"
```

---

### Task 3: Route the 8 duckdb-only `uv run` sites through `AB_AAT_TRIAGE_PYTHON`

Eight `uv run --isolated --no-project --with 'duckdb>=1.1' ...` invocations network-fetch duckdb per run. The flake already exposes `aat-triage-python` (`python3.withPackages [duckdb]`, duckdb 1.5.2 ≥ 1.1). Add an `AB_AAT_TRIAGE_PYTHON` env override (pointing at that python) with the existing `uv run` form preserved as the fallback. Unset env ⇒ identical behavior to today.

**Files:**
- Modify: `ab-validator/tests/aat-batch-triage-smoke.sh` (line ~89)
- Modify: `ab-validator/tests/aat-fidelity-duckdb-smoke.sh` (lines ~19 and ~29)
- Modify: `ab-validator/tests/aozora2html-policy-samples-smoke.sh` (lines ~247 and ~255)
- Modify: `ab-validator/tests/aozora2html-measurement-audit-smoke.sh` (lines ~298, ~306, ~314)

**Interfaces:**
- Consumes: existing `aat-triage-python` flake output.
- Produces: an `AB_AAT_TRIAGE_PYTHON` contract: when set, it is the path to a `python` interpreter with `duckdb` importable; scripts run `"$AB_AAT_TRIAGE_PYTHON" <script.py> <args>` (and `"$AB_AAT_TRIAGE_PYTHON" - <args> <<'PY'` for the heredoc) instead of `uv run --with duckdb`.

- [ ] **Step 1: Define a helper resolver once per script**

At the top of each of the four scripts (after `set -euo pipefail` and `repo_root`/env setup, before first use), add a command array:
```bash
if [[ -n "${AB_AAT_TRIAGE_PYTHON:-}" ]]; then
  triage_py=("$AB_AAT_TRIAGE_PYTHON")
else
  triage_py=(uv run --isolated --no-project --with 'duckdb>=1.1')
fi
```
Then replace each `uv run --isolated --no-project --with 'duckdb>=1.1'` prefix with `"${triage_py[@]}"`, leaving the trailing `python`/script path and all args unchanged.

Concrete rules:
- Script-path form: `uv run ... "$repo_root/reports/aat-fidelity/build-aat-batch-triage.py" <args>` → `"${triage_py[@]}" "$repo_root/reports/aat-fidelity/build-aat-batch-triage.py" <args>`. Both `uv run <script.py>` and `python <script.py>` execute the script, so this is correct for either branch.
- Heredoc `python -` form (`aat-fidelity-duckdb-smoke.sh:29`): `uv run ... python - "$db_path" <<'PY'` → `"${triage_py[@]}" python - "$db_path" <<'PY'`. When the fallback array ends in `uv run ...`, the literal `python` token that follows is required (`uv run python -`); when `triage_py` is the nix interpreter, `"$AB_AAT_TRIAGE_PYTHON" python - ...` would wrongly pass `python` as `sys.argv`. Handle this ONE site by branching the interpreter token:
```bash
if [[ -n "${AB_AAT_TRIAGE_PYTHON:-}" ]]; then
  "$AB_AAT_TRIAGE_PYTHON" - "$db_path" <<'PY'
  ...
PY
else
  uv run --isolated --no-project --with 'duckdb>=1.1' python - "$db_path" <<'PY'
  ...
PY
fi
```
To avoid duplicating the heredoc body, extract the Python to a temp file once and run `"${triage_py[@]}" "$tmp_py" "$db_path"` — but only if the `uv run` fallback still resolves `python` implicitly. Since `uv run <file.py>` runs the file, writing the heredoc to a temp `.py` and invoking `"${triage_py[@]}" "$tmp_py" "$db_path"` works uniformly for BOTH branches and avoids the `python` token problem. Prefer this: write the heredoc body to `"$(mktemp --suffix=.py)"` (with a `trap 'rm -f "$tmp_py"' EXIT`-style cleanup consistent with the script), then `"${triage_py[@]}" "$tmp_py" "$db_path"`.

- [ ] **Step 2: The libstdc++ hack in `aat-fidelity-duckdb-smoke.sh` (lines 12–17)**

That `LD_LIBRARY_PATH` dance exists so the `uv`-installed duckdb wheel finds libstdc++. It is only needed on the `uv` path. Leave it in place unconditionally (it is harmless when `AB_AAT_TRIAGE_PYTHON` is set — it only prepends a lib dir). Do NOT remove it; removing it would break the `uv` fallback. (Optional, only if trivial: guard it with `if [[ -z "${AB_AAT_TRIAGE_PYTHON:-}" ]]; then ... fi` — but the default is to leave it untouched to minimize risk.)

- [ ] **Step 3: Verify all four scripts parse**

Run (from repo root):
```bash
for f in ab-validator/tests/aat-batch-triage-smoke.sh ab-validator/tests/aat-fidelity-duckdb-smoke.sh ab-validator/tests/aozora2html-policy-samples-smoke.sh ab-validator/tests/aozora2html-measurement-audit-smoke.sh; do bash -n "$f" && echo "ok $f"; done
```
Expected: `ok` for all four.

- [ ] **Step 4: Behavior check — nix-python path**

Run (from `ab-validator/`):
```bash
py=$(nix build "$(pwd)#aat-triage-python" --no-link --print-out-paths)/bin/python
AB_AAT_TRIAGE_PYTHON="$py" AB_DB_ROOT="${TMPDIR:-/tmp}/tier3-check" bash tests/aat-fidelity-duckdb-smoke.sh && echo "SMOKE OK"
```
Expected: `aat fidelity duckdb smoke ok` and `SMOKE OK`. (This exercises both the script-path invocation and the heredoc/temp-file invocation through the nix interpreter.)

- [ ] **Step 5: Commit**
```bash
git add ab-validator/tests/aat-batch-triage-smoke.sh ab-validator/tests/aat-fidelity-duckdb-smoke.sh ab-validator/tests/aozora2html-policy-samples-smoke.sh ab-validator/tests/aozora2html-measurement-audit-smoke.sh
git commit -m "feat(tests): route duckdb triage through AB_AAT_TRIAGE_PYTHON (uv fallback)"
```

---

### Task 4: Guard the unconditional `cargo build -p ab-aat-to-parser-ir`

`parser-ir-level3-generated-workset-audit-smoke.sh:262` runs `cargo build -p ab-aat-to-parser-ir` on every invocation, but line ~268 already prefers `${AB_AAT_TO_PARSER_IR_BIN:-"$repo_root/target/debug/ab-aat-to-parser-ir"}`. When the flake binary is injected (the check sets `AB_AAT_TO_PARSER_IR_BIN`), the build is wasted work. Guard it.

**Files:**
- Modify: `ab-validator/tests/parser-ir-level3-generated-workset-audit-smoke.sh:262`

**Interfaces:**
- Consumes: `AB_AAT_TO_PARSER_IR_BIN` (already read at line ~268).
- Produces: nothing new; only skips a redundant build when the binary is injected.

- [ ] **Step 1: Wrap the build in a guard**

Replace line 262:
```bash
(cd "$repo_root" && cargo build -p ab-aat-to-parser-ir >/dev/null)
```
with:
```bash
if [[ -z "${AB_AAT_TO_PARSER_IR_BIN:-}" ]]; then
  (cd "$repo_root" && cargo build -p ab-aat-to-parser-ir >/dev/null)
fi
```

- [ ] **Step 2: Verify parse + guard logic**

Run (from repo root):
```bash
bash -n ab-validator/tests/parser-ir-level3-generated-workset-audit-smoke.sh && echo "parse ok"
```
Expected: `parse ok`. Then eyeball: when `AB_AAT_TO_PARSER_IR_BIN` is set, the `cargo build` is skipped and line ~268's converter-bin resolves to the injected binary; when unset, the build runs and line ~268 falls back to `target/debug/...` exactly as before.

- [ ] **Step 3: Commit**
```bash
git add ab-validator/tests/parser-ir-level3-generated-workset-audit-smoke.sh
git commit -m "perf(tests): skip ab-aat-to-parser-ir build when AB_AAT_TO_PARSER_IR_BIN is injected"
```

---

## PART B — Tier 3: Dedup (PINNED-careful)

### Task 5: Consolidate the three `ab-validator/reports` `canonical_json` copies behind a golden-pinned shared module

Three byte-equivalent `canonical_json` implementations of abc-legacy-json-c14n-v0 live in `ab-validator/reports/`. Extract one shared module in `reports/lib/`, add a golden byte-equality test FIRST (characterization), then switch the three copies to import it. Each file keeps its own hash wrapper (`schema_hash` / `document_hash`) — only `canonical_json` moves. The `run_identity.py` `canonical_json` is a DIFFERENT function (no `/`-escape) and MUST NOT be touched.

**Files:**
- Create: `ab-validator/reports/lib/legacy_json_c14n.py`
- Create: `ab-validator/reports/lib/tests/test_legacy_json_c14n.py` (or the repo's existing reports test location — verify where `reports/lib` tests live before creating; if none, place beside other `reports/**/tests/` suites and note it)
- Modify: `ab-validator/reports/aat-fidelity/aat_parser_ir_mapping/c14n.py` (replace local `canonical_json`, keep `schema_hash`)
- Modify: `ab-validator/reports/parser-ir/level3-admission.py` (replace local `canonical_json`, keep `document_hash`)
- Modify: `ab-validator/reports/parser-ir/publication-coverage.py` (replace local `canonical_json`, keep `document_hash`)

**Interfaces:**
- Produces: `legacy_json_c14n.canonical_json(value: object) -> str` — returns `json.dumps(value, ensure_ascii=False, sort_keys=True, separators=(",", ":")).replace("/", "\\/")`, byte-for-byte identical to the three current copies.
- Consumes: nothing new (stdlib `json` only).

- [ ] **Step 1: Write the golden test FIRST, against the CURRENT (un-refactored) code**

Create the test asserting the exact canonical bytes for representative inputs, importing each of the three current implementations and the (soon-to-exist) shared one. Because the shared module doesn't exist yet, this test will fail to import — that is the expected initial failure. Test content:
```python
"""Golden byte-equality pin for abc-legacy-json-c14n-v0 canonical JSON.

PINNED: these exact strings feed schema/document hashes compared across
adapters and against the abc side. If this test changes, hashes change and
recorded fidelity/skip metadata silently invalidates. Do not "fix" expected
values to match new output — investigate the canonicalization change instead.
"""
from __future__ import annotations

import sys
from pathlib import Path

_LIB = Path(__file__).resolve().parents[1]  # reports/lib
sys.path.insert(0, str(_LIB))

import legacy_json_c14n  # noqa: E402

# Golden cases: input value -> exact canonical JSON text.
GOLDEN = [
    ({"b": 1, "a": 2}, '{"a":2,"b":1}'),
    ({"path": "a/b/c"}, '{"path":"a\\/b\\/c"}'),
    ({"z": [3, 1, 2], "y": "x/y"}, '{"y":"x\\/y","z":[3,1,2]}'),
    ({"u": "café"}, '{"u":"café"}'),
    ({"nested": {"d/e": {"f": "/root"}}}, '{"nested":{"d\\/e":{"f":"\\/root"}}}'),
    ([], "[]"),
    ({}, "{}"),
]


def test_canonical_json_matches_golden():
    for value, expected in GOLDEN:
        assert legacy_json_c14n.canonical_json(value) == expected


def test_shared_matches_all_three_legacy_copies():
    # Import each legacy site's implementation and assert byte-identical output
    # to the shared one across the golden inputs, proving the extraction is
    # behavior-preserving for every current caller.
    import importlib.util

    def _load(path: Path, name: str):
        spec = importlib.util.spec_from_file_location(name, path)
        mod = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(mod)
        return mod

    reports = Path(__file__).resolve().parents[2]  # ab-validator/reports
    c14n = _load(reports / "aat-fidelity/aat_parser_ir_mapping/c14n.py", "c14n_legacy")
    level3 = _load(reports / "parser-ir/level3-admission.py", "level3_legacy")
    pub = _load(reports / "parser-ir/publication-coverage.py", "pub_legacy")
    for value, _ in GOLDEN:
        want = legacy_json_c14n.canonical_json(value)
        assert c14n.canonical_json(value) == want
        assert level3.canonical_json(value) == want
        assert pub.canonical_json(value) == want
```
Note for implementer: `level3-admission.py` and `publication-coverage.py` have module-level code/imports; loading them via `spec.loader.exec_module` runs top-level statements. If either module has import-time side effects or heavy imports that make `_load` impractical, fall back to asserting the shared output against the inlined literal bodies (they are known: the `json.dumps(...).replace("/", "\\/")` form) and record the deviation in the report. The `test_canonical_json_matches_golden` test is the mandatory pin; `test_shared_matches_all_three_legacy_copies` is best-effort proof.

- [ ] **Step 2: Run the test — expect import failure**

Run (from `ab-validator/`): `python -m pytest reports/lib/tests/test_legacy_json_c14n.py -x -q` (adjust path if placed elsewhere).
Expected: collection/import error (`No module named 'legacy_json_c14n'`).

- [ ] **Step 3: Create the shared module**

Create `ab-validator/reports/lib/legacy_json_c14n.py`:
```python
"""abc-legacy-json-c14n-v0 canonical JSON — single source for the ab-validator
reports tree.

PINNED: the returned bytes feed schema/document SHA-256 hashes that are compared
across adapters and against the abc side. Output MUST stay byte-for-byte stable;
changing it silently invalidates recorded fidelity and skip metadata. Guarded by
reports/lib/tests/test_legacy_json_c14n.py.

Algorithm:
1. serialize UTF-8 JSON with sorted object keys and compact separators;
2. escape every "/" as "\\/", including slashes inside string values.
(Callers SHA-256 the result and prefix with "sha256:".)
"""

from __future__ import annotations

import json


def canonical_json(value: object) -> str:
    return json.dumps(
        value,
        ensure_ascii=False,
        sort_keys=True,
        separators=(",", ":"),
    ).replace("/", "\\/")
```

- [ ] **Step 4: Run the test — expect pass**

Run (from `ab-validator/`): `python -m pytest reports/lib/tests/test_legacy_json_c14n.py -x -q`
Expected: PASS (both tests, or the mandatory one plus the documented best-effort one).

- [ ] **Step 5: Switch the three copies to import the shared module**

In each file, remove the local `def canonical_json(...)` and add an import of the shared module. Follow the file's existing `sys.path`/import convention:
- `c14n.py` (at `reports/aat-fidelity/aat_parser_ir_mapping/`): add near the top, after stdlib imports:
```python
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "lib"))
from legacy_json_c14n import canonical_json  # noqa: E402
```
  (parents[2] = `reports/`; `reports/lib` holds the module. Verify the relative depth: `c14n.py` → `aat_parser_ir_mapping` → `aat-fidelity` → `reports`, so `reports/lib` is `parents[2] / "lib"`.)
- `level3-admission.py` and `publication-coverage.py` (at `reports/parser-ir/`): `parents[1] / "lib"` (`parser-ir` → `reports`). Add the same `sys.path.insert` + `from legacy_json_c14n import canonical_json` idiom, matching each file's existing import style.
- Keep each file's `schema_hash` / `document_hash` wrapper unchanged — they now call the imported `canonical_json`.

- [ ] **Step 6: Verify callers still work**

Run (from `ab-validator/`):
```bash
python -c "import sys; sys.path.insert(0,'reports/aat-fidelity/aat_parser_ir_mapping'); import c14n; print(c14n.canonical_json({'a/b':1}))"
python -m pytest reports/lib/tests/test_legacy_json_c14n.py -x -q
bash tests/aat-parser-ir-schema-hash-smoke.sh 2>&1 | tail -3 || echo "(smoke needs env; note result)"
```
Expected: first prints `{"a\/b":1}`; pytest PASS; the schema-hash smoke either passes or its failure is unrelated to canonicalization (record which).

- [ ] **Step 7: Commit**
```bash
git add ab-validator/reports/lib/legacy_json_c14n.py ab-validator/reports/lib/tests/test_legacy_json_c14n.py ab-validator/reports/aat-fidelity/aat_parser_ir_mapping/c14n.py ab-validator/reports/parser-ir/level3-admission.py ab-validator/reports/parser-ir/publication-coverage.py
git commit -m "refactor(reports): single-source abc-legacy-json-c14n canonical_json + golden pin"
```

---

## PART C — Tier 4: Local Cleanups

### Task 6: Collapse the aozora2html adapter triple-`mktemp`/triple-`trap` into one temp dir + one trap

`adapters/aozora2html/aozora2html-adapter` creates six temp files across three `mktemp` batches, each followed by its own `trap ... EXIT` that supersedes the prior (accumulating the file list by hand). Replace with a single `mktemp -d` + one `trap 'rm -rf "$dir"' EXIT`, and point the six paths inside it. Behavior-preserving (the temp paths are internal; only suffixes matter for the tools, which are preserved as filenames).

**Files:**
- Modify: `ab-validator/adapters/aozora2html/aozora2html-adapter` (lines ~27–61)

**Interfaces:** none external — internal temp-file handling only.

- [ ] **Step 1: Replace the three trap groups**

Replace the first `mktemp` block + trap (lines ~27–31):
```bash
stdin_raw="$(mktemp --suffix=.raw)"
parser_sjis_src="$(mktemp --suffix=.parser.sjis)"
crlf_src="$(mktemp --suffix=.txt)"
xhtml="$(mktemp --suffix=.html)"
trap 'rm -f "$stdin_raw" "$parser_sjis_src" "$crlf_src" "$xhtml"' EXIT
```
with a single temp dir and one trap:
```bash
tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT
stdin_raw="$tmp_dir/stdin.raw"
parser_sjis_src="$tmp_dir/parser.sjis"
crlf_src="$tmp_dir/crlf.txt"
xhtml="$tmp_dir/out.html"
```
Then delete the second trap line (~39) and its `sjis_src="$(mktemp --suffix=.sjis)"` becomes `sjis_src="$tmp_dir/sjis.sjis"`; delete the third trap line (~61) and its `parser_err="$(mktemp --suffix=.err)"` becomes `parser_err="$tmp_dir/parser.err"`.

Implementer notes:
- Preserve the ORDER of variable assignments relative to the code that first writes each file (keep `sjis_src` defined at its current point ~line 38, `parser_err` at ~line 60) — only change the RHS from `mktemp --suffix=...` to `"$tmp_dir/<name>.<ext>"`, and remove the now-redundant 2nd and 3rd `trap` lines.
- Keep the file extensions in the basenames (`.raw`, `.sjis`, `.txt`, `.html`, `.err`) in case any downstream tool inspects them.
- Confirm no code reads `mktemp`'s exit status or the specific `/tmp/tmp.XXXX` path shape.

- [ ] **Step 2: Verify parse**

Run (from repo root): `bash -n ab-validator/adapters/aozora2html/aozora2html-adapter && echo "parse ok"`
Expected: `parse ok`.

- [ ] **Step 3: Behavior smoke — adapter still runs end-to-end**

Run (from repo root), feeding the adapter minimal Aozora text via its `--mode aat` path (mirror the flake's inline invocation, or run the existing aozora2html smoke if lightweight):
```bash
printf 'テスト作品\nテスト著者\n\n-------------------------------------------------------\n凡例\n-------------------------------------------------------\n\n吾輩《わがはい》は猫である。\n\n底本：テスト出版\n' \
  | bash ab-validator/adapters/aozora2html/aozora2html-adapter --mode aat 2>&1 | tail -5
```
Expected: emits AAT JSON (or the same output as before the change); no leftover temp files (`ls "${TMPDIR:-/tmp}"/tmp.* 2>/dev/null` shows none newly orphaned). If the adapter needs the aozora2html gem/renderer on PATH and it is absent locally, record that the parse check passed and defer the full run to CI, noting it in the report.

- [ ] **Step 4: Commit**
```bash
git add ab-validator/adapters/aozora2html/aozora2html-adapter
git commit -m "refactor(aozora2html): one temp dir + one trap instead of three"
```

---

### Task 7: Anchor the bare `.#` flake refs in the `vibrato-dict-link` dev-shell helper

The `vibrato-dict-link` function in the dev shell (`flake.nix:1836` and `:1840`) runs `nix eval ".#packages..."` and `nix build ".#$attr"`, which resolve against the caller's CWD and break on directory drift. Anchor them to the ab-validator flake directory computed once at function entry.

**Files:**
- Modify: `ab-validator/flake.nix` (the `vibrato-dict-link` shell function, ~lines 1830–1845)

**Interfaces:** dev-shell convenience function only; no runtime/CI contract.

- [ ] **Step 1: Compute a flake root once and use it for both refs**

At the top of the `vibrato-dict-link()` function body, add:
```bash
                local flake_dir
                flake_dir="$(git rev-parse --show-toplevel 2>/dev/null)/ab-validator"
                if [ ! -e "$flake_dir/flake.nix" ]; then flake_dir="."; fi
```
Then change `nix eval ".#packages.$(...)..."` → `nix eval "$flake_dir#packages.$(...)..."` and `nix build ".#$attr"` → `nix build "$flake_dir#$attr"`.

Implementer notes:
- The dev shell is entered from within the repo, so `git rev-parse --show-toplevel` resolves the working tree; appending `/ab-validator` targets this flake. The `flake.nix` existence guard falls back to `.` (today's behavior) if the layout is unexpected, so this never regresses.
- Leave the `nix build .#ab-validator` string inside the help/message heredoc (~line 913) UNCHANGED — it is user-facing guidance to run from the repo root, not a CWD-sensitive execution.
- Do not alter the two-attr fallback logic (`attr="ab-validator-$pkg"`), only the leading `.#` → `"$flake_dir#"`.

- [ ] **Step 2: Verify the flake still evaluates**

Run (from `ab-validator/`): `nix flake check --no-build 2>&1 | tail -5`
Expected: no new evaluation errors (the shell hook is a string; this confirms no syntax breakage in the surrounding Nix).

- [ ] **Step 3: Verify the shell-hook string parses as bash**

Extract and lint the function body if practical, or eyeball that quoting is balanced. Minimum: `nix develop "$(pwd)" --command bash -c 'type vibrato-dict-link' 2>&1 | tail -3` (from `ab-validator/`) — expected: reports `vibrato-dict-link is a function` (confirms the hook sourced without a bash syntax error). If entering the dev shell is too heavy, note it and rely on Step 2 plus manual review.

- [ ] **Step 4: Commit**
```bash
git add ab-validator/flake.nix
git commit -m "fix(flake): anchor vibrato-dict-link nix refs to the flake dir, not CWD"
```

---

## Self-Review Checklist (planner)

- **Coverage:** Tier 2 = Tasks 1–4 (ab-source-inventory expose, ab-oracle package+route, duckdb→nix-python, guard redundant cargo build). Tier 3 = Task 5 (canonical_json dedup + golden pin; freshness.verify + schema_contracts deletion explicitly rejected with rationale). Tier 4 = Tasks 6–7 (triple-trap, bare `.#`). All items from the user's Tier 2–4 list are either implemented or have a documented rejection.
- **PINNED safety:** Task 5 writes the golden byte-equality test before touching any copy and re-proves the three legacy copies match the shared output; `run_identity.py`'s distinct `canonical_json` is explicitly out of scope.
- **Fallback preservation:** Tasks 2 and 3 keep the `cargo run` / `uv run` fallbacks intact when env vars are unset; Task 4 only skips work when the binary is injected.
- **No placeholders:** every code/command step contains concrete content.
- **Type/name consistency:** `AB_ORACLE_BIN`, `AB_AAT_TRIAGE_PYTHON`, `AB_AAT_TO_PARSER_IR_BIN` used consistently; `abOracleBin`/`ab-oracle`, `sourceInventoryBin`/`ab-source-inventory` mirror the existing derivation→output naming.
