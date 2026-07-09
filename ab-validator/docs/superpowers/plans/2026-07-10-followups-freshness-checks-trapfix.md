# Follow-ups: shared freshness core, aozora2html trap-safety, nix-path CI checks

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land the three actionable follow-ups from the Tier 2–4 branch review: (1) centralize the skip-gate fresh/stale *decision rule* behind one tested pure function shared by both gate sites; (2) make the aozora2html adapter clean up its temp dir on the success path; (3) add two cheap hermetic nix checks so CI exercises the new `AB_AAT_TRIAGE_PYTHON` / `AB_ORACLE_BIN` nix paths instead of only the `uv`/`cargo` fallbacks.

**Architecture:** The two skip gates (`generator_skip.is_fresh` for AAT dumps, `warehouse_index.check` for morph-warehouse runs) independently reimplement the same rule — *fresh iff the recorded input-set hash matches the current one AND the outputs re-hash to their recorded value (an unreadable/absent output ⇒ not fresh)*. They keep genuinely different result vocabularies (binary `{fresh,reason}` vs 4-state `{status,...}`) and different I/O (a `metadata.json` vs a manifest reached through a by-input symlink). This plan extracts ONLY the decision rule into `reports/lib/freshness.py` as a pure function returning a tagged verdict; each caller keeps its own loading and maps the verdict to its exact existing return dict. Behavior is preserved byte-for-byte, proven by characterization tests over every branch.

**Tech Stack:** Python 3 stdlib (`enum`, `typing.NamedTuple`, `unittest`), Bash, Nix flakes.

## Global Constraints

- **Behavior preservation (Task 1):** every existing return value of `generator_skip.is_fresh` and `warehouse_index.check` — the exact `fresh`/`status`/`reason` strings and the `recorded`/`actual`/`run_dir`/`run_id` fields — MUST be unchanged for every input branch. This is a safety-critical skip gate about to be validated in production. Prove it with characterization tests that assert the exact dicts, and keep the pre-refactor behavior in the test as the oracle.
- **No change to what gets read/written:** Task 1 must NOT touch identity computation, metadata/manifest *writing*, `aat_hash.hash_aat_dir`, `tree_hash`, or `run_identity`. Only the in-memory decision logic inside the two reader functions changes.
- **Fallback preservation (Task 3):** the two scripts wired as checks already prefer their `AB_*` env var with a `uv`/`cargo` fallback; do not alter the scripts. Only add flake checks that set the env var to the nix output.
- **Hermetic checks only (Task 3):** the two chosen scripts must run in the sealed `mkSmokeCheck` sandbox (no network, no corpus, no adapter builds). Do not wire the network/corpus/adapter-dependent scripts.
- Match surrounding style. Verify Python with the repo's unittest harness; shell with `bash -n`; nix with `nix build`/`nix flake check`.

## Decisions — documented non-goals (do NOT implement)

- **Deduping the `abc/tools/schema_contracts.py` ↔ `scripts/abc_schema_contracts.py` canonical_json copies (rejected):** `abc/` is a self-contained sub-flake (`ab-validator/flake.nix` consumes it as `path:../abc`), and its `schema-contract-drift` check (`abc/flake.nix:464`) builds from `${./.}` — the abc dir only. A flake cannot reference `../scripts/` outside its own root under pure eval, so the two copies are a *deliberate* consequence of abc's flake self-containment, not accidental duplication. Furthermore both copies are already implicitly drift-protected: `abc/flake.nix`'s `schema-contract-drift` recomputes schema hashes against the committed manifest (any canonicalization drift fails that check), and `ab-validator/scripts/compare_abc_schema_contracts.py` diffs the generated manifests. No extra pin is needed and no clean dedup exists.
- **Wiring the corpus/adapter/network-dependent smoke scripts as checks (out of scope):** `run-cross-adapter-report.sh`, `aat-fidelity-cross-summary-xhtml-smoke.sh` (uses `uv run --with duckdb,lxml`, no override), `adapter-oracle-report-smoke.sh` (needs a prior run's artifact), and the adapter-building oracle/duckdb scripts are not cheaply hermetic. Task 3 wires only the two cheapest; the rest are a documented deferral.

---

### Task 1: Extract the shared fresh/stale decision rule into `reports/lib/freshness.py`

**Files:**
- Create: `ab-validator/reports/lib/freshness.py`
- Create: `ab-validator/reports/lib/tests/test_freshness.py`
- Modify: `ab-validator/reports/aat-fidelity/generator_skip.py` (replace inline compare in `is_fresh`)
- Modify: `ab-validator/reports/morph-warehouse/warehouse_index.py` (replace inline compare in `check`)

**Interfaces:**
- Produces: `freshness.classify(recorded_input_hash: str | None, current_input_hash: str, recorded_output_hash: str | None, recompute_output: Callable[[], str]) -> freshness.Decision`, where `Decision` is a `NamedTuple(verdict: Verdict, recorded: str | None, actual: str | None)` and `Verdict` is an `enum.Enum` with members `FRESH`, `INPUT_MISMATCH`, `OUTPUT_UNREADABLE`, `OUTPUT_MISMATCH`. `classify` compares the input hashes; if equal, calls `recompute_output()` inside a `try/except (ValueError, OSError)` (→ `OUTPUT_UNREADABLE`); compares the result to `recorded_output_hash` (→ `OUTPUT_MISMATCH` carrying both, else `FRESH` carrying both).
- Consumes (unchanged): `aat_hash.hash_aat_dir` (generator_skip) and `hash_run_dir` (warehouse_index) are passed in as the `recompute_output` thunk.

- [ ] **Step 1: Write the characterization test FIRST**

Create `ab-validator/reports/lib/tests/test_freshness.py` as a `unittest.TestCase` (match sibling tests: `sys.path.insert(0, str(Path(__file__).resolve().parents[1]))`). It must:
1. Directly test `freshness.classify` for all four verdicts:
```python
from __future__ import annotations
import sys
import unittest
from pathlib import Path

_LIB = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(_LIB))

import freshness  # noqa: E402


class Classify(unittest.TestCase):
    def test_input_mismatch(self):
        d = freshness.classify("sha256:a", "sha256:b", "sha256:o", lambda: "unused")
        self.assertEqual(d.verdict, freshness.Verdict.INPUT_MISMATCH)

    def test_output_unreadable(self):
        def boom():
            raise OSError("gone")
        d = freshness.classify("sha256:a", "sha256:a", "sha256:o", boom)
        self.assertEqual(d.verdict, freshness.Verdict.OUTPUT_UNREADABLE)

    def test_output_unreadable_valueerror(self):
        def boom():
            raise ValueError("bad")
        d = freshness.classify("sha256:a", "sha256:a", "sha256:o", boom)
        self.assertEqual(d.verdict, freshness.Verdict.OUTPUT_UNREADABLE)

    def test_output_mismatch_carries_hashes(self):
        d = freshness.classify("sha256:a", "sha256:a", "sha256:o", lambda: "sha256:x")
        self.assertEqual(d.verdict, freshness.Verdict.OUTPUT_MISMATCH)
        self.assertEqual(d.recorded, "sha256:o")
        self.assertEqual(d.actual, "sha256:x")

    def test_fresh(self):
        d = freshness.classify("sha256:a", "sha256:a", "sha256:o", lambda: "sha256:o")
        self.assertEqual(d.verdict, freshness.Verdict.FRESH)
```
2. Characterize BOTH callers end-to-end against fixtures on disk, asserting the EXACT return dicts (these are the oracle — copy the expected dicts from the CURRENT behavior). For `generator_skip.is_fresh`, build a temp dir with a `metadata.json` and an `aat/` subtree and assert:
   - missing metadata → `{"fresh": False, "reason": "no metadata.json"}`
   - input mismatch → `{"fresh": False, "reason": "input_set_hash differs (inputs changed)"}`
   - matching input, missing `aat/` → `{"fresh": False, "reason": "aat outputs missing or unreadable"}`
   - matching input, tampered output → `{"fresh": False, "reason": "aat content hash mismatch"}`
   - all good → `{"fresh": True, "reason": "inputs unchanged and outputs verify"}`
   Use `aat_hash.hash_aat_dir` to compute the correct `output_content_hash` for the "all good" fixture so it verifies. For `warehouse_index.check`, build a `<warehouse>/by-input/<hex>` symlink → `runs/<id>/` with a `run-manifest.json`, and assert the exact 4-state dicts including `status`, `reason`, `run_dir`, `run_id`, and (for output mismatch) `recorded`/`actual`. Look at `reports/morph-warehouse/tests/` for existing fixtures/patterns to reuse.

- [ ] **Step 2: Run the test — expect failure on the direct-classify cases**

Run: `cd ab-validator && python3 -m unittest discover -s reports/lib/tests -p 'test_freshness.py' -t reports/lib/tests`
Expected: the `Classify` cases error with `ModuleNotFoundError: freshness` (module not created yet). The caller-characterization cases (importing the current generator_skip/warehouse_index) should PASS against current code — confirming the oracle encodes today's behavior before any refactor.

(If you place caller-characterization in a separate discover path, run both; the point is: characterization passes now, classify tests fail only for the missing module.)

- [ ] **Step 3: Create `reports/lib/freshness.py`**
```python
"""Shared fresh/stale decision rule for the batch-run skip gates.

Both skip gates — AAT dumps (reports/aat-fidelity/generator_skip.py) and
morph-warehouse runs (reports/morph-warehouse/warehouse_index.py) — reuse the
same rule: an existing output is FRESH iff its recorded input-set hash equals the
current inputs' hash AND its outputs still re-hash to the recorded output hash. An
absent/unreadable output is never fresh (fail toward recompute). This module holds
ONLY that rule as a pure function; each caller keeps its own metadata loading and
maps the returned verdict to its own result vocabulary. See
docs/superpowers/specs/2026-07-09-batch-run-staleness-skip-recompute-design.md.
"""

from __future__ import annotations

import enum
from typing import Callable, NamedTuple


class Verdict(enum.Enum):
    FRESH = "fresh"
    INPUT_MISMATCH = "input_mismatch"
    OUTPUT_UNREADABLE = "output_unreadable"
    OUTPUT_MISMATCH = "output_mismatch"


class Decision(NamedTuple):
    verdict: Verdict
    recorded: str | None
    actual: str | None


def classify(
    recorded_input_hash: str | None,
    current_input_hash: str,
    recorded_output_hash: str | None,
    recompute_output: Callable[[], str],
) -> Decision:
    """Decide freshness. ``recompute_output`` is called only when the input
    hashes match; a ``ValueError``/``OSError`` from it means the outputs are
    unreadable/absent (not fresh). Returns the verdict plus the recorded and
    (when computed) actual output hashes for the caller to report."""
    if recorded_input_hash != current_input_hash:
        return Decision(Verdict.INPUT_MISMATCH, recorded_output_hash, None)
    try:
        actual = recompute_output()
    except (ValueError, OSError):
        return Decision(Verdict.OUTPUT_UNREADABLE, recorded_output_hash, None)
    if actual != recorded_output_hash:
        return Decision(Verdict.OUTPUT_MISMATCH, recorded_output_hash, actual)
    return Decision(Verdict.FRESH, recorded_output_hash, actual)
```

- [ ] **Step 4: Refactor `generator_skip.is_fresh`**

Keep the metadata loading (missing/malformed/non-dict) exactly as-is. Replace the input-compare + recompute + output-compare tail with a `freshness.classify` call and a verdict→dict mapping that reproduces the EXACT current strings:
```python
_LIB = Path(__file__).resolve().parents[1] / "lib"
sys.path.insert(0, str(_LIB))

import aat_hash  # noqa: E402
import freshness  # noqa: E402
...
    decision = freshness.classify(
        meta.get("input_set_hash"),
        current_input_set_hash,
        meta.get("output_content_hash"),
        lambda: aat_hash.hash_aat_dir(out / "aat"),
    )
    if decision.verdict is freshness.Verdict.INPUT_MISMATCH:
        return {"fresh": False, "reason": "input_set_hash differs (inputs changed)"}
    if decision.verdict is freshness.Verdict.OUTPUT_UNREADABLE:
        return {"fresh": False, "reason": "aat outputs missing or unreadable"}
    if decision.verdict is freshness.Verdict.OUTPUT_MISMATCH:
        return {"fresh": False, "reason": "aat content hash mismatch"}
    return {"fresh": True, "reason": "inputs unchanged and outputs verify"}
```
(Add `import freshness` alongside the existing `import aat_hash`; both resolve via the existing `_LIB` sys.path insert.)

- [ ] **Step 5: Refactor `warehouse_index.check`**

Keep the symlink resolution + manifest loading (missing/dangling/malformed/non-dict) exactly as-is. Replace the input-compare + recompute + output-compare tail:
```python
import freshness  # noqa: E402   (alongside the existing `import tree_hash`)
...
    decision = freshness.classify(
        manifest.get("input_set_hash"),
        input_set_hash,
        manifest.get("output_content_hash"),
        lambda: hash_run_dir(run_dir),
    )
    if decision.verdict is freshness.Verdict.INPUT_MISMATCH:
        return {"status": "invalid", "reason": "manifest input_set_hash mismatch",
                "run_dir": str(run_dir)}
    if decision.verdict is freshness.Verdict.OUTPUT_UNREADABLE:
        return {"status": "stale", "reason": "run outputs missing or unreadable",
                "run_dir": str(run_dir)}
    if decision.verdict is freshness.Verdict.OUTPUT_MISMATCH:
        return {"status": "stale", "reason": "output content hash mismatch",
                "recorded": decision.recorded, "actual": decision.actual,
                "run_dir": str(run_dir)}
    return {"status": "fresh", "run_dir": str(run_dir), "run_id": run_dir.name}
```

- [ ] **Step 6: Run all three suites — expect PASS**

Run:
```bash
cd ab-validator && python3 -m unittest discover -s reports/lib/tests -p 'test_*.py' -t reports/lib/tests
python3 -m unittest discover -s reports/morph-warehouse/tests -p 'test_*.py' -t reports/morph-warehouse/tests
python3 -m unittest discover -s reports/aat-fidelity/tests -p 'test_*.py' -t reports/aat-fidelity/tests
```
Expected: all PASS (the pre-existing gate/warehouse suites now exercise the refactored code and must be green; the new freshness suite passes). Also run the aggregate gate the CI uses: `bash tests/batch-run-staleness-smoke.sh` — expect it to pass.

- [ ] **Step 7: Commit**
```bash
git add ab-validator/reports/lib/freshness.py ab-validator/reports/lib/tests/test_freshness.py ab-validator/reports/aat-fidelity/generator_skip.py ab-validator/reports/morph-warehouse/warehouse_index.py
git commit -m "refactor(skip-gate): share fresh/stale decision rule via reports/lib/freshness"
```

---

### Task 2: Make the aozora2html adapter clean up its temp dir on the success path

The adapter's final line is `exec "$RUST_MAPPER_BIN" "${common_args[@]}"`, which replaces the shell process, so the `EXIT` trap that removes `$tmp_dir` never fires on success (the temp dir leaks every successful run). Drop `exec` so the wrapper shell waits on the mapper, exits with its status, and runs the cleanup trap. The mapper reads `$tmp_dir` contents (e.g. `$xhtml`), so cleanup must stay AFTER it, not before.

**Files:**
- Modify: `ab-validator/adapters/aozora2html/aozora2html-adapter` (final dispatch line, ~97)

**Interfaces:** none external.

- [ ] **Step 1: Confirm the current tail**

Read the last ~15 lines. Confirm the final statement is `exec "$RUST_MAPPER_BIN" "${common_args[@]}"` and that nothing follows it, and that `$tmp_dir` (from Task 6 of the prior branch) is created near the top with `trap 'rm -rf "$tmp_dir"' EXIT`.

- [ ] **Step 2: Replace `exec` with a plain foreground call**

Change:
```bash
exec "$RUST_MAPPER_BIN" "${common_args[@]}"
```
to:
```bash
"$RUST_MAPPER_BIN" "${common_args[@]}"
```
Rationale: as the script's last command, the shell exits with the mapper's exit status (behavior-preserving for callers reading `$?`), and the `EXIT` trap now fires, removing `$tmp_dir`. Also broaden the trap to cover interrupt/termination so kills clean up too: change the trap line near the top from `trap 'rm -rf "$tmp_dir"' EXIT` to `trap 'rm -rf "$tmp_dir"' EXIT INT TERM`.

- [ ] **Step 3: Verify parse + behavior**

Run: `bash -n ab-validator/adapters/aozora2html/aozora2html-adapter && echo "parse ok"`.
Then, if the aozora2html renderer is available (build the Rust mapper + `nix develop` for the gem, as the prior task did — or run the existing `tests/` smoke if lightweight), pipe the sample Aozora text through `--mode aat` and confirm: (a) valid AAT JSON with `parse_complete: true`, (b) exit code preserved, (c) NO leftover `tmp.XXXX` dir remains after a successful run (`ls "${TMPDIR:-/tmp}"/tmp.* 2>/dev/null` shows none newly orphaned). If the renderer is unavailable locally, record that `bash -n` passed and defer the run to CI, noting it.

- [ ] **Step 4: Commit**
```bash
git add ab-validator/adapters/aozora2html/aozora2html-adapter
git commit -m "fix(aozora2html): drop exec so temp dir is cleaned on success"
```

---

### Task 3: Add two hermetic nix checks exercising the `AB_AAT_TRIAGE_PYTHON` and `AB_ORACLE_BIN` paths

Currently no check runs the scripts that use these env contracts, so CI only exercises their `uv`/`cargo` fallbacks. Add two cheap, sealed `mkSmokeCheck`s: one for `aat-fidelity-duckdb-smoke.sh` (sets `AB_AAT_TRIAGE_PYTHON` to the flake's `aat-triage-python` python) and one for `aat-oracle-audit-smoke.sh` (sets `AB_ORACLE_BIN` to the `ab-oracle` package — no adapter builds needed).

**Files:**
- Modify: `ab-validator/flake.nix` (define two `mkSmokeCheck`s near the existing ones ~line 1275; register both in the `checks` attrset ~lines 1787–1824)

**Interfaces:**
- Consumes: `mkSmokeCheck`, `pythonWithAatDuckdb` (line ~1073), `abOracleBin` (line ~1260), `pkgs.duckdb`, `pkgs.ripgrep`.
- Produces: `checks.aat-fidelity-duckdb-smoke` and `checks.aat-oracle-audit-smoke`.

- [ ] **Step 1: Define the two checks**

Near the other `*SmokeCheck` definitions (after `sourceInventorySmokeCheck`, ~line 1286), add:
```nix
        aatFidelityDuckdbSmokeCheck = mkSmokeCheck {
          name = "aat-fidelity-duckdb-smoke-check";
          testScript = "tests/aat-fidelity-duckdb-smoke.sh";
          nativeBuildInputs = [
            pythonWithAatDuckdb
            pkgs.duckdb
            pkgs.glibc.bin
          ];
          extraEnv = {
            AB_AAT_TRIAGE_PYTHON = "${pythonWithAatDuckdb}/bin/python3";
            AB_DUCKDB_BIN = "${pkgs.duckdb}/bin/duckdb";
          };
          extraPreScript = ''
            export AB_DB_ROOT="$TMPDIR/ab-validator"
          '';
        };

        aatOracleAuditSmokeCheck = mkSmokeCheck {
          name = "aat-oracle-audit-smoke-check";
          testScript = "tests/aat-oracle-audit-smoke.sh";
          nativeBuildInputs = [
            abOracleBin
            pkgs.ripgrep
          ];
          extraEnv = {
            AB_ORACLE_BIN = "${abOracleBin}/bin/ab-oracle";
          };
          extraPreScript = ''
            export AB_DB_ROOT="$TMPDIR/ab-validator"
          '';
        };
```
(`pkgs.glibc.bin` provides `ldd`, which `aat-fidelity-duckdb-smoke.sh` calls on the duckdb binary; `AB_DUCKDB_BIN` short-circuits its `command -v duckdb` fallback. If a `ldd`/`awk` issue arises, note it — `pkgs.gawk` may be added, though coreutils/stdenv usually provide `awk`.)

- [ ] **Step 2: Register both in the `checks` attrset**

In the `checks = { ... }` set (~lines 1787–1824, alongside `source-inventory-smoke = sourceInventorySmokeCheck;`), add:
```nix
          aat-fidelity-duckdb-smoke = aatFidelityDuckdbSmokeCheck;
          aat-oracle-audit-smoke = aatOracleAuditSmokeCheck;
```

- [ ] **Step 3: Build both checks (this actually runs the sealed scripts)**

Run (from `ab-validator/`):
```bash
nix build "$(pwd)#checks.$(nix eval --impure --raw --expr builtins.currentSystem).aat-oracle-audit-smoke" --no-link -L 2>&1 | tail -20
nix build "$(pwd)#checks.$(nix eval --impure --raw --expr builtins.currentSystem).aat-fidelity-duckdb-smoke" --no-link -L 2>&1 | tail -30
```
Expected: both succeed (exit 0). The build log should show the scripts' success markers (`aat oracle audit smoke ok`, `aat fidelity duckdb smoke ok`). If the duckdb check fails on a missing tool in the sandbox (e.g. `ldd`/`awk`), add the corresponding nix input to `nativeBuildInputs` and rebuild; capture the exact failing line in the report before fixing.

- [ ] **Step 4: Confirm the flake still evaluates cleanly**

Run (from `ab-validator/`): `nix flake check --no-build 2>&1 | tail -5`. Expected: no evaluation errors; the two new checks are recognized.

- [ ] **Step 5: Commit**
```bash
git add ab-validator/flake.nix
git commit -m "test(flake): hermetic checks exercising AB_AAT_TRIAGE_PYTHON and AB_ORACLE_BIN nix paths"
```

---

## Self-Review Checklist (planner)

- **Coverage:** the three actionable follow-ups (freshness core, aozora2html trap-safety, nix-path checks) each have a task; the two non-goals (abc schema_contracts dedup, heavier check wiring) are documented with concrete reasons.
- **Behavior preservation (Task 1):** characterization tests assert the exact pre-refactor dicts for every branch of both callers; the shared function is pure and the callers keep their vocabularies. `run_identity`, identity/metadata writing, and the hashers are untouched.
- **Hermeticity (Task 3):** both wired scripts need only in-repo fixtures + the injected nix binaries; Step 3 actually builds (runs) them in the sandbox to prove it.
- **No placeholders:** every code/command step is concrete.
- **Naming consistency:** `AB_AAT_TRIAGE_PYTHON`, `AB_ORACLE_BIN`, `pythonWithAatDuckdb`, `abOracleBin`, `Verdict`/`Decision`/`classify` used consistently.
