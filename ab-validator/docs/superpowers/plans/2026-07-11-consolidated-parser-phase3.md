# Consolidated Parser Phase 3 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land Phase 3 of the consolidated Aozora parser — sanitize swap
(stage 0), capability fixes (rotation A: diagnostics codes +
keigakomi/yokogumi classifiers), and decoded-source span semantics
(rotation B) — per
`ab-validator/docs/superpowers/specs/2026-07-11-consolidated-parser-phase3-capability-spans-design.md`.

**Architecture:** Three strictly ordered stages on one branch. Stage 0
swaps the last live crates.io upstream dependency for the fork's sanitize
and proves byte-identity against the Phase 2 corpus dump. Rotation A adds
kebab diagnostic `code`s (wire schema 3, new `--mode diagnostics`),
classifies `keigakomi_block`/`yokogumi_block`, and proves confinement with
a forward-rewrite delta audit. Rotation B rebases every span to
decoded-source offsets with real line numbers via a sanitize offset map,
proved by a span-confinement audit and hand-verified goldens. Each
rotation ends with a fresh conversion audit and a new **unadmitted**
registry row.

**Tech Stack:** Rust (workspace crates `ab-aozora-aat`, `ab-aozora`,
`ab-aozora-facade`, `ab-aozora-pipeline`), Python 3 instruments under
`reports/`, Clojure/EDN on the abc side, hinoki for heavy runs.

## Global Constraints

Copied from the spec; every task implicitly includes these.

- Working root: the `ab-validator/` directory of the phase worktree
  (`.worktrees/parser-fork-phase3`, branch `feat/parser-fork-phase3`,
  created at execution start via superpowers:using-git-worktrees), except
  the two abc-side registry tasks (11, 18) which edit
  `abc/data/aat-parser-ir-compatibility.edn` in the same worktree.
- **Never** enable the facade `json` feature in any workspace member:
  it pulls `serde_json/preserve_order` into the workspace graph.
  `bash tests/workspace-no-preserve-order.sh` must stay green after every
  task ("OK: no preserve_order in the root workspace feature graph").
- **Never** edit frozen evidence reports (anything already under
  `docs/superpowers/reports/` dated 2026-07-10 or earlier). Phase 3 gate
  evidence files are frozen the moment their task completes.
- **Never** touch `adapters/aozora`, the pinned upstream lane, or the
  measurement default (`--adapter aozora`). No registry row is admitted;
  activation is Phase 4.
- Exit codes of `ab-aozora`: 0 success, 1 failure, 2 reserved-never-emitted.
  New `--mode diagnostics` follows the same contract.
- Version strings (exact, per stage — the cross-gate join keys):
  - Stage 0 (C0): `ab-aozora 0.1.0 aat-schema 1 facade 0.1.0 wire-schema 2 (git <C0>)`
  - Rotation A (C1): `ab-aozora 0.2.0 aat-schema 1 facade 0.2.0 wire-schema 3 (git <C1>)`
  - Rotation B (C2): `ab-aozora 0.3.0 aat-schema 1 facade 0.2.0 wire-schema 3 (git <C2>)`
- **Candidate identity (binding for tasks 3, 10, 11, 17, 18):** each
  stage's candidate commit (`C0`, `C1`, `C2`) is the ledger-recorded HEAD
  after that stage's last code task. Gate builds happen on hinoki in a
  **detached clean checkout** of the candidate commit with
  `AB_AOZORA_GIT_REV=$CANDIDATE` exported; every gate asserts
  `./target/release/ab-aozora --version | grep -F $CANDIDATE` before
  running. A `(git unknown)` binary is never gate evidence. If any commit
  lands after a stage's gate started, that stage's candidate moves forward
  and ALL of that stage's gates re-run.
- Gate summary JSON schema (one file per gate, frozen once written):

  ```json
  {
    "stage": "stage0 | rotation-a | rotation-b",
    "gate": "<gate name>",
    "candidate": {
      "commit": "<full 40-hex>",
      "bin_sha256": "<sha256 of target/release/ab-aozora>",
      "version": "<verbatim --version line>"
    },
    "verdict": "PASS",
    "details": { }
  }
  ```

- hinoki: `hinoki.hyakutake-barbel.ts.net`, passwordless ssh, 32 cores,
  `AB_DB_ROOT=/db/ab-validator`. PrivateTmp — never use `/tmp` there.
  ~10-minute ssh command cap: long runs are `nohup … &` detached with a
  log file under `~`, then polled.
- Never delete `/db/ab-validator/aat-corpus/aozora-full-repin-1a4f864`,
  `/db/ab-validator/aat-corpus/ab-aozora-phase2-9cbb7b7b`, or
  `/db/ab-validator/aat-corpus/aozora-fork-parity-2263b92a`. Phase 3
  retains `ab-aozora-phase3-capability-<C1:0:7>` and
  `ab-aozora-phase3-span-<C2:0:7>`; the stage-0 dump may be deleted after
  rotation A passes.
- Builds for gates and perf: `export RUSTC_WRAPPER= SCCACHE_DISABLE=1`.
- Progress ledger: append task-completion lines and the three candidate
  commits to `$(git rev-parse --show-toplevel)/.superpowers/sdd/progress.md`
  under a `## Phase 3` heading.
- The perf gate (rotations and stage 0 alike): committed workset
  `data/perf-workset.json`, corpus `/db/ab-validator/perf-workset-corpus-v1`
  (already extracted on hinoki), `--runs 5` minimum, >10% median wall-time
  regression on the workset blocks the stage, new timeouts block
  unconditionally.

---

## File structure (locked decomposition)

| File | Responsibility |
| --- | --- |
| `crates/ab-aozora-aat/Cargo.toml`, `src/lib.rs` | Stage 0 dep swap; classifiers; jizume recognition; `diagnostics_json_from_bytes`; rotation-B span composition; tripwire |
| `crates/ab-aozora-facade/src/json.rs`, `src/lib.rs`, `Cargo.toml` | Diagnostic `code` field, `SCHEMA_VERSION` 3, `VERSION` const, crate 0.2.0 |
| `crates/ab-aozora/src/main.rs`, `tests/wire.rs`, `Cargo.toml` | `--mode diagnostics` dispatch; version bumps |
| `crates/ab-aozora-pipeline/src/lexer/sanitize.rs` | `sanitize_mapped` + `OffsetMap` (rotation B) |
| `crates/ab-aozora-aat/tests/reference_parity.rs` → `tests/goldens.rs` + `tests/goldens/*.json` | Hand-verified committed goldens (rotation B) |
| `reports/parser-conformance/run-aozora-notation-spec.py` + `tests/` | `--adapter-diagnostics`, diagnostics comparison, `--span-deviation-manifest` |
| `reports/parser-conformance/span-deviation-manifest.json` | Pre-committed rotation-B deviation authorizations |
| `reports/parser-conformance/compare-adapter-rows.py` + test | One adapter's rows equal across two summary files |
| `reports/aat-fidelity/audit-aat-delta.py` + `tests/test_audit_aat_delta.py` | Rotation A forward-rewrite delta audit; rotation B span-confinement audit |
| `reports/aat-fidelity/denominator-attribution.py` | Marker-form decomposition for keigakomi/yokogumi/jizume denominators |
| `reports/aat-fidelity/verify-phase3-checkpoint.py` + test | Fail-closed 9-summary checkpoint |
| `abc/data/aat-parser-ir-compatibility.edn` | Two new unadmitted rows (C1, C2) |
| `docs/handoffs/2026-07-10-parser-fork-provenance.md` | Phase 3 closure notes |
| `justfile` | `--adapter-diagnostics` wiring for the ab-aozora lane |

---

### Task 1: Stage 0 — sanitize swap

**Files:**
- Modify: `crates/ab-aozora-aat/Cargo.toml` (remove `aozora-pipeline`, add `ab-aozora-pipeline`)
- Modify: `crates/ab-aozora-aat/src/lib.rs:6` (import swap)

**Interfaces:**
- Consumes: `ab_aozora_pipeline::lexer::sanitize::sanitize(&str) -> SanitizeOutput<'_>` (fork crate, same `{ text: Cow<str>, diagnostics }` shape as upstream 0.4.1's).
- Produces: `ab-aozora-aat` with zero crates.io parser deps. Behavior must be byte-identical — the tripwire proves it locally, Task 3 proves it corpus-wide.

- [ ] **Step 1: Confirm the tripwire is green before touching anything**

Run: `cd ab-validator && export RUSTC_WRAPPER= SCCACHE_DISABLE=1 && cargo test -p ab-aozora-aat aat_json_from_bytes_is_byte_exact_under_default_map_ordering`
Expected: PASS (this exact test must pass **unmodified** after the swap too).

- [ ] **Step 2: Swap the dependency**

In `crates/ab-aozora-aat/Cargo.toml`, delete the `aozora-pipeline = "=0.4.1"` line **and** its whole preceding comment block ("Ported verbatim from the frozen adapter's dependency surface … Phase 3 item …"), replacing them with:

```toml
# Fork-owned sanitize (Phase 3 stage 0): the crates.io aozora-pipeline
# =0.4.1 dependency is retired; ab_aozora_pipeline::lexer::sanitize is the
# 1a4f864-lifted implementation, byte-equivalence proven corpus-wide in
# docs/superpowers/reports/2026-07-11-phase3-stage0-sanitize-parity.md.
ab-aozora-pipeline = { path = "../ab-aozora-pipeline" }
```

In `crates/ab-aozora-aat/src/lib.rs` line 6, replace:

```rust
use aozora_pipeline::lexer::sanitize as sanitize_aozora_source;
```

with:

```rust
use ab_aozora_pipeline::lexer::sanitize::sanitize as sanitize_aozora_source;
```

- [ ] **Step 3: Verify no crates.io parser dep remains**

Run: `cargo tree -p ab-aozora-aat | grep -c "aozora-pipeline v0.4.1"`
Expected: `0` (grep exits 1). Also run `grep -n "aozora-pipeline" Cargo.lock | grep -v "ab-aozora-pipeline"` — the only acceptable remaining hits are other crates' deps if any exist (expected: none).

- [ ] **Step 4: Full local verification**

Run, all from `ab-validator/`:
```bash
cargo test --workspace
bash tests/workspace-no-preserve-order.sh
just preserve-order-hazard-check
```
Expected: workspace all green **including the unmodified tripwire**; guard prints `OK: no preserve_order in the root workspace feature graph`; canary 2/2.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-aozora-aat/Cargo.toml Cargo.lock crates/ab-aozora-aat/src/lib.rs
git commit -m "feat(ab-aozora-aat): swap crates.io sanitize for fork sanitize (phase 3 stage 0)"
```

Record this commit in the ledger as the provisional `C0` (final `C0` is HEAD after Task 2).

---

### Task 2: `compare-adapter-rows.py` — conformance row comparator

**Files:**
- Create: `reports/parser-conformance/compare-adapter-rows.py`
- Test: `reports/parser-conformance/tests/test_compare_adapter_rows.py`

**Interfaces:**
- Consumes: two `run-aozora-notation-spec.py` summary JSONs (shape: `{"rows": [{"vector", "feature", "level", "adapter", "status", "failures", "warnings", "skips"}, …]}`).
- Produces: CLI `compare-adapter-rows.py OLD NEW --adapter LABEL`; exit 0 when the adapter's rows are identical in both files, 1 with a diff listing otherwise, 2 on usage/reference errors. Tasks 3, 10, 17 gate on it.

- [ ] **Step 1: Write the failing tests**

```python
# reports/parser-conformance/tests/test_compare_adapter_rows.py
import json
import subprocess
import sys
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[1] / "compare-adapter-rows.py"


def summary(rows):
    return {"rows": rows}


def row(vector="v1", status="pass", adapter="ab-aozora", **kw):
    base = {"vector": vector, "feature": "f", "level": "must", "adapter": adapter,
            "status": status, "failures": [], "warnings": [], "skips": []}
    base.update(kw)
    return base


def run(tmp_path, old_rows, new_rows, adapter="ab-aozora"):
    old = tmp_path / "old.json"
    new = tmp_path / "new.json"
    old.write_text(json.dumps(summary(old_rows)))
    new.write_text(json.dumps(summary(new_rows)))
    return subprocess.run([sys.executable, str(SCRIPT), str(old), str(new),
                           "--adapter", adapter], capture_output=True, text=True)


def test_identical_rows_pass(tmp_path):
    rows = [row(), row(vector="v2", status="warning")]
    assert run(tmp_path, rows, rows).returncode == 0


def test_status_change_fails(tmp_path):
    p = run(tmp_path, [row()], [row(status="fail")])
    assert p.returncode == 1
    assert "v1" in p.stdout


def test_other_adapters_ignored(tmp_path):
    assert run(tmp_path, [row(), row(adapter="aozora", status="fail")],
               [row(), row(adapter="aozora", status="pass")]).returncode == 0


def test_missing_vector_fails(tmp_path):
    assert run(tmp_path, [row(), row(vector="v2")], [row()]).returncode == 1


def test_adapter_absent_is_reference_error(tmp_path):
    assert run(tmp_path, [row(adapter="x")], [row(adapter="x")]).returncode == 2
```

- [ ] **Step 2: Run to verify failure**

Run: `python -m pytest reports/parser-conformance/tests/test_compare_adapter_rows.py -v`
Expected: FAIL (script missing).

- [ ] **Step 3: Implement**

```python
#!/usr/bin/env python3
"""Compare one adapter's conformance rows across two summary JSONs.

Used by the Phase 3 gates: stage 0 requires the ab-aozora lane identical
to the frozen Phase 2 echo summaries; rotations diff against the previous
stage and the differences are reviewed against expectations.

Exit 0 = identical; 1 = any row differs / vector set differs; 2 = usage or
reference error (unreadable file, adapter absent from either file).
"""
import argparse
import json
import sys


def rows_for(path, adapter):
    try:
        doc = json.load(open(path))
    except (OSError, json.JSONDecodeError) as err:
        print(f"ERROR: {path}: {err}", file=sys.stderr)
        raise SystemExit(2)
    rows = {r["vector"]: r for r in doc.get("rows", []) if r.get("adapter") == adapter}
    if not rows:
        print(f"ERROR: {path}: no rows for adapter {adapter!r}", file=sys.stderr)
        raise SystemExit(2)
    return rows


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("old")
    ap.add_argument("new")
    ap.add_argument("--adapter", required=True)
    args = ap.parse_args()
    old = rows_for(args.old, args.adapter)
    new = rows_for(args.new, args.adapter)
    diffs = []
    for vector in sorted(set(old) | set(new)):
        if vector not in old:
            diffs.append(f"{vector}: only in new")
        elif vector not in new:
            diffs.append(f"{vector}: only in old")
        elif old[vector] != new[vector]:
            diffs.append(f"{vector}: old={json.dumps(old[vector], ensure_ascii=False)}\n"
                         f"  new={json.dumps(new[vector], ensure_ascii=False)}")
    for d in diffs:
        print(d)
    print(f"compared={len(set(old) | set(new))} differing={len(diffs)}")
    return 1 if diffs else 0


if __name__ == "__main__":
    raise SystemExit(main())
```

- [ ] **Step 4: Run tests**

Run: `python -m pytest reports/parser-conformance/tests/test_compare_adapter_rows.py -v`
Expected: 5 passed.

- [ ] **Step 5: Commit**

```bash
git add reports/parser-conformance/compare-adapter-rows.py reports/parser-conformance/tests/test_compare_adapter_rows.py
git commit -m "feat(conformance): adapter-row comparator for phase 3 stage gates"
```

---

### Task 3: Stage 0 gate — corpus byte-parity, conformance echo, perf (hinoki)

**Files:**
- Create: `docs/superpowers/reports/2026-07-11-phase3-stage0-sanitize-parity.md`
- Create: `docs/superpowers/reports/2026-07-11-phase3-stage0-sanitize-parity.summary.json`
- Create: `docs/superpowers/reports/2026-07-11-phase3-stage0-conformance.summary.json` (+ `.md` and seed variants via the harness)
- Create: `docs/superpowers/reports/2026-07-11-phase3-stage0-conformance-gate.summary.json` (candidate-bearing gate summary)
- Create: `docs/superpowers/reports/2026-07-11-phase3-stage0-perf.{md,summary.json,runner.json}`

**Interfaces:**
- Consumes: Tasks 1–2 merged; `C0` = ledger-recorded HEAD after Task 2; `compare-aat-dumps.py --bytes`; `compare-adapter-rows.py`; frozen Phase 2 echo summaries `docs/superpowers/reports/2026-07-10-phase2-conformance-echo-p4suta.summary.json` and `…-official-seed.summary.json`; `data/perf-workset.json`.
- Produces: three PASS gate summaries (`stage: "stage0"`, gates `parity` / `conformance` / `perf`) with one candidate identity; the stage-0 dump `/db/ab-validator/aat-corpus/ab-aozora-phase3-stage0-<C0:0:7>` (Task 10's delta baseline).

- [ ] **Step 1: Record C0**

```bash
CANDIDATE=$(git rev-parse HEAD)   # full 40-hex; append to ledger: "Phase 3 C0 = <sha>"
```

- [ ] **Step 2: Detached clean build on hinoki, rev injected**

```bash
ssh hinoki.hyakutake-barbel.ts.net "cd ~/Projects/soranoha && git fetch origin && (git worktree add --detach ~/Projects/soranoha/.worktrees/parser-fork-phase3 $CANDIDATE 2>/dev/null || git -C ~/Projects/soranoha/.worktrees/parser-fork-phase3 checkout --detach $CANDIDATE)"
ssh hinoki.hyakutake-barbel.ts.net "cd ~/Projects/soranoha/.worktrees/parser-fork-phase3 && test -z \"\$(git status --porcelain)\" && git rev-parse HEAD"
ssh hinoki.hyakutake-barbel.ts.net "cd ~/Projects/soranoha/.worktrees/parser-fork-phase3/ab-validator && export RUSTC_WRAPPER= SCCACHE_DISABLE=1 AB_AOZORA_GIT_REV=$CANDIDATE && cargo build --package ab-aozora --release && ./target/release/ab-aozora --version | grep -F $CANDIDATE && sha256sum ./target/release/ab-aozora"
```

Expected: second command prints exactly `$CANDIDATE`; version line reads
`ab-aozora 0.1.0 aat-schema 1 facade 0.1.0 wire-schema 2 (git $CANDIDATE)`.
Record `bin_sha256`. (Requires the branch pushed to origin, or push the
worktree branch to a temp remote ref first: `git push origin HEAD:refs/heads/feat/parser-fork-phase3`.)

- [ ] **Step 3: Full-corpus run, detached**

```bash
ssh hinoki.hyakutake-barbel.ts.net 'cd ~/Projects/soranoha/.worktrees/parser-fork-phase3/ab-validator && AB_DB_ROOT=/db/ab-validator nohup reports/aat-fidelity/run-aat-full.sh --adapter ab-aozora --adapter-bin ./target/release/ab-aozora --jobs 32 --report-id phase3-stage0 --out-dir /db/ab-validator/aat-corpus/ab-aozora-phase3-stage0-<C0:0:7> > ~/phase3-stage0-run.log 2>&1 &'
# poll every few minutes:
ssh hinoki.hyakutake-barbel.ts.net 'tail -3 ~/phase3-stage0-run.log; ls /db/ab-validator/aat-corpus/ab-aozora-phase3-stage0-<C0:0:7>/aat 2>/dev/null | wc -l'
```

Expected on completion: 17886 files.

- [ ] **Step 4: Byte-parity against the Phase 2 dump**

```bash
ssh hinoki.hyakutake-barbel.ts.net 'cd ~/Projects/soranoha/.worktrees/parser-fork-phase3/ab-validator && python3 reports/aat-fidelity/compare-aat-dumps.py --bytes /db/ab-validator/aat-corpus/ab-aozora-phase2-9cbb7b7b/aat /db/ab-validator/aat-corpus/ab-aozora-phase3-stage0-<C0:0:7>/aat > ~/phase3-stage0-parity.json; echo "exit=$?"; cat ~/phase3-stage0-parity.json'
```

Expected: `exit=0`, `"compared": 17886`, `"missing_count": 0`, `"bytes": {"diverged_count": 0, …}`.
**If any work diverges (fail closed):** stop the stage; classify per the
spec's divergence resolution rule (fork-more-correct → escalate to the
human, the swap re-scopes into rotation A; lift defect → fix and re-run
from Step 1 with a new C0). Never widen the comparator.

- [ ] **Step 5: Conformance echo, both suites, locally**

```bash
just aozora-notation-spec-comparison REPORT_MD=docs/superpowers/reports/2026-07-11-phase3-stage0-conformance.md SUMMARY_JSON=docs/superpowers/reports/2026-07-11-phase3-stage0-conformance.summary.json
just official-docs-seed-comparison REPORT_MD=docs/superpowers/reports/2026-07-11-phase3-stage0-conformance-seed.md SUMMARY_JSON=docs/superpowers/reports/2026-07-11-phase3-stage0-conformance-seed.summary.json
python reports/parser-conformance/compare-adapter-rows.py docs/superpowers/reports/2026-07-10-phase2-conformance-echo-p4suta.summary.json docs/superpowers/reports/2026-07-11-phase3-stage0-conformance.summary.json --adapter ab-aozora
python reports/parser-conformance/compare-adapter-rows.py docs/superpowers/reports/2026-07-10-phase2-conformance-echo-official-seed.summary.json docs/superpowers/reports/2026-07-11-phase3-stage0-conformance-seed.summary.json --adapter ab-aozora
```

Expected: both comparators print `differing=0`, exit 0 (baselines 113/9/0/5 and 22/8/0/0 preserved).
Note: the local build has `(git unknown)` — acceptable for conformance
*scoring* only because the row comparator ignores identity; the gate
summary's candidate identity comes from the hinoki build of Step 2.

- [ ] **Step 6: Perf workset on hinoki**

Mirror the Phase 2 lane setup verbatim (`2026-07-10-phase2-perf.md`):
frozen-adapter baseline (`adapters/aozora` release build + pinned upstream
`nix build .#upstream-parser-aozora`), candidate = the Step 2 binary,
corpus `/db/ab-validator/perf-workset-corpus-v1`:

```bash
ssh hinoki.hyakutake-barbel.ts.net 'cd ~/Projects/soranoha/.worktrees/parser-fork-phase3/ab-validator && export RUSTC_WRAPPER= SCCACHE_DISABLE=1 && cargo build --manifest-path adapters/aozora/Cargo.toml --release && AOZORA_PKG=$(nix build .#upstream-parser-aozora --no-link --print-out-paths) && nohup python3 reports/aat-fidelity/run-perf-workset.py --workset data/perf-workset.json --corpus /db/ab-validator/perf-workset-corpus-v1 --baseline-cmd "env AB_AOZORA_BIN=$AOZORA_PKG/bin/aozora $PWD/adapters/aozora/target/release/aozora-adapter --mode aat" --candidate-cmd "$PWD/target/release/ab-aozora --mode aat" --baseline-id-bin adapters/aozora/target/release/aozora-adapter --candidate-id-bin target/release/ab-aozora --runs 5 --out ~/phase3-stage0-perf.json > ~/phase3-stage0-perf.log 2>&1 &'
```

Expected on completion: 0 timeouts; median regression ≤ 10% (stage 0 should be ~0%).

- [ ] **Step 7: Write the three gate summaries + parity report, verify, commit, freeze**

Write `2026-07-11-phase3-stage0-sanitize-parity.summary.json` (gate
`parity`, details = the compare-aat-dumps output), the conformance gate
summary `…-stage0-conformance-gate.summary.json` (gate `conformance`,
details `{p4suta_differing: 0, seed_differing: 0}`), and the perf gate
summary (gate `perf`, details from the runner JSON: per-work medians,
`new_timeouts: 0`, `median_delta_pct`). All three carry `stage: "stage0"`
and the identical Step 2 candidate triple. Copy
`~/phase3-stage0-perf.json` down as `…-stage0-perf.runner.json`. Write the
markdown report narrating identity, lanes, and results (model:
`2026-07-10-phase2-absorption-parity.md`).

```bash
git add docs/superpowers/reports/2026-07-11-phase3-stage0-*
git commit -m "docs(reports): phase 3 stage 0 gate evidence — sanitize swap byte-parity PASS"
```

Append to ledger: `Stage 0 gates PASS (C0=<sha>, bin=<sha256>)`.

---

### Task 4: Rotation A identity — diagnostic `code`, wire schema 3, version bumps, tripwire re-baseline

**Files:**
- Modify: `crates/ab-aozora-facade/src/json.rs` (SCHEMA_VERSION, `Diagnostic` struct, its `From` impl, the `assert_eq!(SCHEMA_VERSION, 2)` test at ~line 611)
- Modify: `crates/ab-aozora-facade/src/lib.rs` (add `VERSION` const)
- Modify: `crates/ab-aozora-facade/Cargo.toml` (`version = "0.2.0"`)
- Modify: `crates/ab-aozora-aat/Cargo.toml` (`version = "0.2.0"`), `src/lib.rs` (const assert, `ab_aozora_facade_version`, tripwire literal)
- Modify: `crates/ab-aozora/Cargo.toml` (`version = "0.2.0"`), `tests/wire.rs` (version-field literals)

**Interfaces:**
- Consumes: facade `crate::Diagnostic::code() -> &str` (namespaced, e.g. `aozora::lex::unclosed_bracket`) — already exists.
- Produces: wire `json::Diagnostic` entries serialize with a `code` field = kebab of the trailing token (`"unclosed-bracket"`); `json::SCHEMA_VERSION == 3`; `ab_aozora_facade::VERSION == "0.2.0"`; `adapter_version()` = `ab-aozora 0.2.0 aat-schema 1 facade 0.2.0 wire-schema 3 (git …)`. **AAT output bytes change ONLY in `/meta/adapter_version`** (the local `AozoraDiagnostic` deserializer ignores the new field).

- [ ] **Step 1: Write the failing facade test**

Add to the tests in `crates/ab-aozora-facade/src/json.rs` (an
entries-level test, `#[cfg(feature = "entries")]`-compatible module):

```rust
#[test]
fn diagnostic_entries_carry_kebab_code() {
    // Any source producing an unclosed_bracket diagnostic:
    let doc = crate::Document::new("あ［＃ここから".to_owned());
    let tree = doc.parse();
    let entries = diagnostic_entries(tree.diagnostics());
    assert!(!entries.is_empty());
    let value = serde_json::to_value(&entries).unwrap();
    let entry = &value[0];
    let code = entry["code"].as_str().unwrap();
    assert_eq!(code, entry["kind"].as_str().unwrap().replace('_', "-"));
    assert!(!code.contains("::"));
}
```

(If this exact source does not produce a diagnostic, pick one from the
existing json.rs diagnostics tests — reuse their input literal.)

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p ab-aozora-facade --features json diagnostic_entries_carry_kebab_code`
Expected: FAIL (no `code` field).

- [ ] **Step 3: Implement**

In `crates/ab-aozora-facade/src/json.rs`:

1. `pub const SCHEMA_VERSION: u32 = 3;` and extend the doc comment:
   `/// Schema 3 (Phase 3): added the stable kebab-case `code` field to diagnostics entries.`
2. The `Diagnostic` struct loses `Copy` (a `String` field lands) and gains
   `code`:

```rust
#[derive(Debug, Clone, Serialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
pub struct Diagnostic {
    kind: &'static str,
    /// Stable kebab-case diagnostic code — the conformance-contract
    /// identity of this diagnostic (Phase 3 design spec). Always the
    /// 1:1 kebab form of `kind`.
    code: String,
    severity: &'static str,
    source: &'static str,
    span: Span,
    #[serde(skip_serializing_if = "Option::is_none")]
    codepoint: Option<char>,
}
```

3. In `impl From<&crate::Diagnostic>`, after the existing
   `let kind = …` line, add `let code = kind.replace('_', "-");` and set
   `code` in the constructor.
4. Update the `assert_eq!(SCHEMA_VERSION, 2)` test to `3`.
5. Fix any compile fallout from the lost `Copy` (e.g. `.iter().map(Diagnostic::from)` is unaffected; clone where a move now occurs).

In `crates/ab-aozora-facade/src/lib.rs`, add near the top-level consts:

```rust
/// Crate version, re-exported for identity strings (ab-aozora-aat's
/// adapter_version embeds it as the `facade` coordinate).
pub const VERSION: &str = env!("CARGO_PKG_VERSION");
```

`crates/ab-aozora-facade/Cargo.toml`: `version = "0.2.0"`.

In `crates/ab-aozora-aat/src/lib.rs`:
- const assert → `const _: () = assert!(aozora_json::SCHEMA_VERSION == 3, "incompatible wire schema version");`
- `ab_aozora_facade_version()` body → `ab_aozora_facade::VERSION` (keep the existing doc comment).
- Tripwire expected literal: replace the substring
  `ab-aozora 0.1.0 aat-schema 1 facade 0.1.0 wire-schema 2 (git unknown)`
  with
  `ab-aozora 0.2.0 aat-schema 1 facade 0.2.0 wire-schema 3 (git unknown)`
  (nothing else in the literal changes — AAT bytes are otherwise stable).
- `crates/ab-aozora-aat/Cargo.toml`: `version = "0.2.0"`.

In `crates/ab-aozora/Cargo.toml`: `version = "0.2.0"`. In
`tests/wire.rs::version_carries_the_identity_fields`, change the checked
field list to `["ab-aozora 0.2.0", "aat-schema 1", "facade 0.2.0", "wire-schema 3", "git "]`.

- [ ] **Step 4: Run the full check surface**

```bash
cargo test --workspace
bash tests/workspace-no-preserve-order.sh
```
Expected: all green, including the re-baselined tripwire and the new facade test.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-aozora-facade crates/ab-aozora-aat crates/ab-aozora Cargo.lock
git commit -m "feat(facade): kebab diagnostic code, wire schema 3, rotation A version bumps"
```

---

### Task 5: `diagnostics_json_from_bytes` + `--mode diagnostics`

**Files:**
- Modify: `crates/ab-aozora-aat/src/lib.rs` (new pub fn + unit test)
- Modify: `crates/ab-aozora/src/main.rs` (mode dispatch), `tests/wire.rs` (3 new tests)

**Interfaces:**
- Consumes: `decode_source_bytes`, `Document`/`encoding::decode_auto` (the exact `projections()` parse path), `aozora_json::diagnostic_entries`, `aozora_json::SCHEMA_VERSION` (all in scope in lib.rs).
- Produces: `pub fn diagnostics_json_from_bytes(bytes: &[u8]) -> Result<Vec<u8>>` — one JSON line `{"data":[…],"schemaVersion":3}` (serde_json default sorted-key ordering; key order is NOT contract). Binary surface `ab-aozora --mode diagnostics`. Task 6's scorer and Task 14's span rebase consume this function.

- [ ] **Step 1: Write the failing unit test** (in lib.rs `#[cfg(test)] mod tests`)

```rust
#[test]
fn diagnostics_json_from_bytes_emits_schema3_envelope_with_codes() {
    // Unclosed bracket → one error diagnostic.
    let out = diagnostics_json_from_bytes("あ［＃ここから\n".as_bytes()).unwrap();
    assert_eq!(out.last(), Some(&b'\n'));
    let doc: Value = serde_json::from_slice(&out).unwrap();
    assert_eq!(doc["schemaVersion"], 3);
    let data = doc["data"].as_array().unwrap();
    assert!(!data.is_empty());
    for entry in data {
        let code = entry["code"].as_str().unwrap();
        assert!(!code.contains('_') && !code.contains("::"), "not kebab: {code}");
        assert!(entry["severity"].is_string());
        assert!(entry["span"]["start"].is_u64() && entry["span"]["end"].is_u64());
    }
}

#[test]
fn diagnostics_json_from_bytes_clean_source_is_empty_data() {
    let doc: Value = serde_json::from_slice(
        &diagnostics_json_from_bytes("あ\n".as_bytes()).unwrap()).unwrap();
    assert_eq!(doc["data"], json!([]));
}
```

(If `あ［＃ここから\n` yields no diagnostic, use the input from Task 4's
facade test — the two tests must share the diagnostic-producing literal.)

- [ ] **Step 2: Run to verify failure** — `cargo test -p ab-aozora-aat diagnostics_json_from_bytes` → FAIL (fn missing).

- [ ] **Step 3: Implement in lib.rs** (next to `aat_json_from_bytes`)

```rust
/// One wire diagnostics envelope (`{"data": […], "schemaVersion": 3}`)
/// per input — the `--mode diagnostics` payload. Single owner of the
/// diagnostics path (Phase 3 design spec): decoding, sanitization, and
/// body selection are the EXACT same `decode_source_bytes` path as
/// `aat_json_from_bytes`; the parse mirrors `projections()`.
///
/// # Errors
///
/// Returns an error if source decoding, projection parsing, or JSON
/// serialization fails.
pub fn diagnostics_json_from_bytes(bytes: &[u8]) -> Result<Vec<u8>> {
    let decoded = decode_source_bytes(bytes)?;
    let source = encoding::decode_auto(decoded.span_text.as_bytes())
        .map_err(|err| anyhow::anyhow!("decode_auto: {err:?}"))?;
    let doc = Document::new(source);
    let tree = doc.parse();
    let entries = aozora_json::diagnostic_entries(tree.diagnostics());
    let envelope = json!({
        "schemaVersion": aozora_json::SCHEMA_VERSION,
        "data": serde_json::to_value(entries)?,
    });
    let mut out = Vec::new();
    serde_json::to_writer(&mut out, &envelope)?;
    out.push(b'\n');
    Ok(out)
}
```

- [ ] **Step 4: Run** — `cargo test -p ab-aozora-aat diagnostics_json_from_bytes` → 2 passed.

- [ ] **Step 5: Write the failing wire tests** (append to `crates/ab-aozora/tests/wire.rs`)

```rust
#[test]
fn mode_diagnostics_emits_schema3_envelope() {
    let mut child = Command::new(bin())
        .args(["--mode", "diagnostics"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    child.stdin.take().unwrap()
        .write_all("あ［＃ここから\n".as_bytes()).unwrap();
    let out = child.wait_with_output().unwrap();
    assert!(out.status.success());
    let doc: serde_json::Value = serde_json::from_slice(&out.stdout).unwrap();
    assert_eq!(doc["schemaVersion"], 3);
    assert!(doc["data"].as_array().unwrap().iter()
        .all(|e| e["code"].is_string()));
}

#[test]
fn mode_diagnostics_no_partial_stdout_contract_holds() {
    // Unknown mode still exits 1 with empty stdout (contract unchanged).
    let out = Command::new(bin()).args(["--mode", "nodes"]).output().unwrap();
    assert_eq!(out.status.code(), Some(1));
    assert!(out.stdout.is_empty());
}
```

- [ ] **Step 6: Run to verify failure** — `cargo test -p ab-aozora --test wire mode_diagnostics` → FAIL.

- [ ] **Step 7: Implement the dispatch in `crates/ab-aozora/src/main.rs`**

Replace the `--mode` match arm and the fixed `aat_json_from_bytes` call
with a mode variable:

```rust
const USAGE: &str = "usage: ab-aozora [--mode aat|diagnostics] [--version]  \
(source bytes on stdin; one JSON document on stdout)";
```

```rust
    let mut mode = Mode::Aat;
    // in the arg loop:
            "--mode" => match args.next().as_deref() {
                Some("aat") => mode = Mode::Aat,
                Some("diagnostics") => mode = Mode::Diagnostics,
                other => {
                    eprintln!(
                        "ab-aozora: unsupported --mode {:?} (aat | diagnostics)\n{USAGE}",
                        other.unwrap_or("<missing>")
                    );
                    return ExitCode::FAILURE;
                }
            },
```

```rust
#[derive(Clone, Copy)]
enum Mode { Aat, Diagnostics }
```

and at the emission site:

```rust
    let result = match mode {
        Mode::Aat => ab_aozora_aat::aat_json_from_bytes(&bytes),
        Mode::Diagnostics => ab_aozora_aat::diagnostics_json_from_bytes(&bytes),
    };
    match result { /* unchanged Ok/Err arms */ }
```

Keep the no-partial-output comment: both functions build the full
document in memory before any stdout write.

- [ ] **Step 8: Run** — `cargo test -p ab-aozora --test wire` → all pass (old + new).

- [ ] **Step 9: Commit**

```bash
git add crates/ab-aozora-aat/src/lib.rs crates/ab-aozora/src/main.rs crates/ab-aozora/tests/wire.rs
git commit -m "feat(ab-aozora): --mode diagnostics via single-owner diagnostics_json_from_bytes"
```

---

### Task 6: Scorer — AAT-lane diagnostics comparison

**Files:**
- Modify: `reports/parser-conformance/run-aozora-notation-spec.py` (Adapter dataclass, `parse_adapter`-adjacent flag, the AAT-branch skip loop at lines ~245–248, `main()`)
- Test: `reports/parser-conformance/tests/test_diagnostics_scoring.py`
- Modify: `justfile` (`aozora-notation-spec-comparison` recipe: add the diagnostics command for the ab-aozora lane)

**Interfaces:**
- Consumes: `ab-aozora --mode diagnostics` (Task 5): envelope `{"schemaVersion": 3, "data": [{"kind", "code", "severity", "source", "span": {"start", "end"}, …}]}`.
- Produces: `--adapter-diagnostics "label=command"` flag; for AAT adapters with a diagnostics command, `expected.diagnostics` is scored by projecting each actual entry to `{"code", "severity", "span": {"start", "end"}}` (key selection only) and exact-comparing. Task 16 extends this same path with the deviation manifest.

- [ ] **Step 1: Write the failing tests**

```python
# reports/parser-conformance/tests/test_diagnostics_scoring.py
import importlib.util
import json
import sys
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[1] / "run-aozora-notation-spec.py"
spec = importlib.util.spec_from_file_location("scorer", SCRIPT)
scorer = importlib.util.module_from_spec(spec)
spec.loader.exec_module(scorer)


def fake_diag_adapter(tmp_path, entries, schema_version=3):
    """A stub diagnostics command: prints a fixed envelope."""
    stub = tmp_path / "diag.sh"
    envelope = json.dumps({"schemaVersion": schema_version, "data": entries})
    stub.write_text("#!/bin/sh\ncat >/dev/null\nprintf '%s\\n' " +
                    json.dumps(envelope).replace("%", "%%") + "\n")
    stub.chmod(0o755)
    aat = tmp_path / "aat.sh"
    aat.write_text("#!/bin/sh\ncat >/dev/null\nprintf '{\"blocks\":[],\"meta\":{}}\\n'\n")
    aat.chmod(0o755)
    return scorer.Adapter(label="x", mode="aat", command=[str(aat)],
                          diagnostics_command=[str(stub)])


def vector(diagnostics):
    return {"name": "v", "meta": {"feature": "f"},
            "source": "s", "level": "must",
            "expected": {"diagnostics": diagnostics}}


FULL = {"kind": "unclosed_bracket", "code": "unclosed-bracket",
        "severity": "error", "source": "source",
        "span": {"start": 1, "end": 4}}
WANT = [{"code": "unclosed-bracket", "severity": "error",
         "span": {"start": 1, "end": 4}}]


def test_matching_diagnostics_pass(tmp_path):
    row = scorer.evaluate(fake_diag_adapter(tmp_path, [FULL]), vector(WANT))
    assert row.status == "pass", (row.failures, row.skips)


def test_mismatching_span_fails_at_must(tmp_path):
    bad = dict(FULL, span={"start": 0, "end": 4})
    row = scorer.evaluate(fake_diag_adapter(tmp_path, [bad]), vector(WANT))
    assert row.status == "fail"


def test_missing_code_key_fails(tmp_path):
    entry = {k: v for k, v in FULL.items() if k != "code"}
    row = scorer.evaluate(fake_diag_adapter(tmp_path, [entry]), vector(WANT))
    assert row.status == "fail"


def test_wrong_schema_version_fails(tmp_path):
    row = scorer.evaluate(fake_diag_adapter(tmp_path, [FULL], schema_version=2),
                          vector(WANT))
    assert row.status == "fail"


def test_without_diagnostics_command_still_skips(tmp_path):
    adapter = fake_diag_adapter(tmp_path, [FULL])
    adapter = scorer.Adapter(label="x", mode="aat", command=adapter.command)
    row = scorer.evaluate(adapter, vector(WANT))
    assert row.status == "skip"
    assert any("not comparable" in s for s in row.skips)
```

- [ ] **Step 2: Run to verify failure** — `python -m pytest reports/parser-conformance/tests/test_diagnostics_scoring.py -v` → FAIL (`diagnostics_command` unknown).

- [ ] **Step 3: Implement**

1. Dataclass:

```python
@dataclass
class Adapter:
    label: str
    mode: str
    command: list[str]
    diagnostics_command: list[str] | None = None
```

2. New parser + wiring in `main()`:

```python
def parse_adapter_diagnostics(spec: str) -> tuple[str, list[str]]:
    label, sep, command = spec.partition("=")
    if not sep or not label or not command:
        raise SystemExit(f"--adapter-diagnostics must be label=command, got {spec!r}")
    return label, shlex.split(command)
```

In `main()` after adapters are parsed:

```python
    ap.add_argument("--adapter-diagnostics", action="append", default=[])
    ...
    for spec in args.adapter_diagnostics:
        label, command = parse_adapter_diagnostics(spec)
        matches = [a for a in adapters if a.label == label]
        if not matches or matches[0].mode != "aat":
            raise SystemExit(f"--adapter-diagnostics {label!r}: no aat adapter with that label")
        matches[0].diagnostics_command = command
```

3. Runner + projection (place after `run_aat`):

```python
def run_diagnostics(adapter: Adapter, source: str) -> tuple[list | None, str | None]:
    proc = subprocess.run(adapter.diagnostics_command, input=source, text=True,
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=False)
    if proc.returncode != 0:
        return None, proc.stderr.strip() or f"exit {proc.returncode}"
    try:
        value = json.loads(proc.stdout)
    except json.JSONDecodeError as error:
        return None, f"invalid JSON: {error}"
    if value.get("schemaVersion") != 3 or not isinstance(value.get("data"), list):
        return None, "unsupported diagnostics envelope"
    projected = []
    for entry in value["data"]:
        try:
            projected.append({"code": entry["code"], "severity": entry["severity"],
                              "span": {"start": entry["span"]["start"],
                                       "end": entry["span"]["end"]}})
        except (KeyError, TypeError):
            return None, f"entry missing code/severity/span: {entry!r}"
    return projected, None
```

4. In `evaluate()`'s AAT branch, replace the four-projection skip loop:

```python
        for projection in ("pairs", "serialize", "html"):
            if expected.get(projection) is not None:
                skips.append(f"{projection}: not comparable for AAT adapter (kind-sequence only)")
        want_diag = expected.get("diagnostics")
        if want_diag is not None:
            if adapter.diagnostics_command is None:
                skips.append("diagnostics: not comparable for AAT adapter (kind-sequence only)")
            else:
                scored += 1
                actual_diag, error = run_diagnostics(adapter, vector["source"])
                if error:
                    failures.append(f"diagnostics: {error}")
                elif actual_diag != want_diag:
                    failures.append(f"diagnostics: expected {want_diag!r}, got {actual_diag!r}")
```

5. justfile: in the `aozora-notation-spec-comparison` recipe, directly
   after the `--adapter "ab-aozora=aat:…"` line, add:

```
		--adapter-diagnostics "ab-aozora={{repo_root}}/target/release/ab-aozora --mode diagnostics" \
```

- [ ] **Step 4: Run** — the new pytest file (6 passed) and the pre-existing conformance tests (`python -m pytest reports/parser-conformance/tests/ -v`) all green.

- [ ] **Step 5: Commit**

```bash
git add reports/parser-conformance/run-aozora-notation-spec.py reports/parser-conformance/tests/test_diagnostics_scoring.py justfile
git commit -m "feat(conformance): score expected.diagnostics on AAT adapters with a diagnostics command"
```

---

### Task 7: keigakomi / yokogumi block classifiers

**Files:**
- Modify: `crates/ab-aozora-aat/src/lib.rs` (`blocks_from_inline_content` branch chain at ~lines 291–337; `find_matching_jisage_close` generalization at ~line 472; new recognizers; unit tests)

**Interfaces:**
- Consumes: raw marker node shape `{"kind":"raw","source":"［＃ここから罫囲み］","x-source-marker-kind":"containerOpen", …}`; helpers `push_paragraph_if_not_empty`, `strip_boundary_newlines`, `is_container_open_raw`.
- Produces: AAT blocks `{"kind":"keigakomi_block","children":[…]}` / `{"kind":"yokogumi_block","children":[…]}` (schema-v1 `block_container`; no span, no x- attrs — jisage precedent). **Well-paired markers only**: any intervening `containerOpen` aborts pairing and both markers stay raw (spec rule; also what makes Task 9's grammar deterministic). Task 9 mirrors this emission EXACTLY.

- [ ] **Step 1: Write the failing tests** (lib.rs tests module)

```rust
fn block_kinds(doc: &Value) -> Vec<String> {
    doc["blocks"].as_array().unwrap().iter()
        .map(|b| b["kind"].as_str().unwrap().to_owned()).collect()
}

#[test]
fn keigakomi_container_classifies_as_block() {
    let src = "前文\n［＃ここから罫囲み］\n中身\n［＃ここで罫囲み終わり］\n後文\n";
    let doc: Value = serde_json::from_slice(&aat_json_from_bytes(src.as_bytes()).unwrap()).unwrap();
    assert_eq!(block_kinds(&doc), ["paragraph", "keigakomi_block", "paragraph"]);
    let block = &doc["blocks"][1];
    assert!(block.get("span").is_none() && block.get("x-indent").is_none());
    let children = block["children"].as_array().unwrap();
    assert_eq!(children[0]["kind"], "paragraph");
    let text: String = children.iter().flat_map(|c| c["content"].as_array().unwrap())
        .filter_map(|n| n["value"].as_str()).collect();
    assert!(text.contains("中身"));
    // No raw container markers survive inside or around the block.
    assert!(!serde_json::to_string(&doc).unwrap().contains("罫囲み］"));
}

#[test]
fn yokogumi_container_classifies_as_block() {
    let src = "［＃ここから横組み］\nＡＢＣ\n［＃ここで横組み終わり］\n";
    let doc: Value = serde_json::from_slice(&aat_json_from_bytes(src.as_bytes()).unwrap()).unwrap();
    assert!(block_kinds(&doc).contains(&"yokogumi_block".to_owned()));
}

#[test]
fn unpaired_keigakomi_open_stays_raw() {
    let src = "前\n［＃ここから罫囲み］\n中身\n";
    let doc: Value = serde_json::from_slice(&aat_json_from_bytes(src.as_bytes()).unwrap()).unwrap();
    assert!(!block_kinds(&doc).contains(&"keigakomi_block".to_owned()));
    assert!(serde_json::to_string(&doc).unwrap().contains("containerOpen"));
}

#[test]
fn intervening_container_open_aborts_keigakomi_pairing() {
    let src = "［＃ここから罫囲み］\n［＃ここから２字下げ］\nａ\n［＃ここで字下げ終わり］\n［＃ここで罫囲み終わり］\n";
    let doc: Value = serde_json::from_slice(&aat_json_from_bytes(src.as_bytes()).unwrap()).unwrap();
    let kinds = block_kinds(&doc);
    assert!(!kinds.contains(&"keigakomi_block".to_owned()), "pairing must abort: {kinds:?}");
    assert!(kinds.contains(&"jisage_block".to_owned()));
}
```

- [ ] **Step 2: Run to verify failure** — `cargo test -p ab-aozora-aat keigakomi` → the classify tests FAIL (no keigakomi_block emitted), the abort/unpaired tests may already pass (that is fine — they pin current recovery behavior).

- [ ] **Step 3: Implement**

1. Generalize the close finder (keep the old name as a delegate so the
   jisage call sites don't churn):

```rust
fn find_matching_jisage_close(content: &[Value], start: usize) -> Option<usize> {
    find_matching_container_close(content, start, "字下げ")
}

fn find_matching_container_close(content: &[Value], start: usize, needle: &str) -> Option<usize> {
    for (offset, node) in content[start..].iter().enumerate() {
        if is_container_open_raw(node) {
            return None;
        }
        if is_container_close_with(node, needle) {
            return Some(start + offset);
        }
    }
    None
}

fn is_container_close_with(node: &Value, needle: &str) -> bool {
    node.get("kind").and_then(Value::as_str) == Some("raw")
        && node.get("x-source-marker-kind").and_then(Value::as_str) == Some("containerClose")
        && node
            .get("source")
            .and_then(Value::as_str)
            .is_some_and(|source| source.contains(needle))
}
```

Delete `is_jisage_container_close` and point its one caller
(`find_matching_jisage_close`, now the delegate above) at the generalized
form. Confirm with `grep -n is_jisage_container_close src/lib.rs` → no
remaining references.

2. New recognizer:

```rust
fn block_container_open(node: &Value, marker: &str) -> bool {
    is_container_open_raw(node)
        && node
            .get("source")
            .and_then(Value::as_str)
            .is_some_and(|source| source.trim() == marker)
}
```

3. Extend the branch chain in `blocks_from_inline_content` — after the
   `else if let Some(indent) = jisage_container_indent(&node)` block's
   closing brace, insert two more `else if` arms (paired-only, no
   boundary fallback — spec rule):

```rust
        } else if block_container_open(&node, "［＃ここから罫囲み］") {
            if let Some(close_index) = find_matching_container_close(&content, index + 1, "罫囲み") {
                push_paragraph_if_not_empty(&mut blocks, mem::take(&mut paragraph));
                let mut inner = content[index + 1..close_index].to_vec();
                strip_boundary_newlines(&mut inner);
                blocks.push(json!({
                    "kind": "keigakomi_block",
                    "children": blocks_from_inline_content(inner)
                }));
                strip_next_leading_newline = true;
                index = close_index + 1;
                continue;
            }
        } else if block_container_open(&node, "［＃ここから横組み］") {
            if let Some(close_index) = find_matching_container_close(&content, index + 1, "横組み") {
                push_paragraph_if_not_empty(&mut blocks, mem::take(&mut paragraph));
                let mut inner = content[index + 1..close_index].to_vec();
                strip_boundary_newlines(&mut inner);
                blocks.push(json!({
                    "kind": "yokogumi_block",
                    "children": blocks_from_inline_content(inner)
                }));
                strip_next_leading_newline = true;
                index = close_index + 1;
                continue;
            }
        }
```

- [ ] **Step 4: Run** — `cargo test -p ab-aozora-aat` → all pass including the 4 new tests and the (unchanged-bytes) tripwire — the tripwire input has no containers, so it must NOT need re-baselining here; if it goes red, the change leaked outside the classifiers: stop and fix.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-aozora-aat/src/lib.rs
git commit -m "feat(ab-aozora-aat): classify keigakomi_block / yokogumi_block from paired container markers"
```

---

### Task 8: jizume recognition API (parser-typed, AAT-raw)

**Files:**
- Modify: `crates/ab-aozora-aat/src/lib.rs` (two pub fns + tests)

**Interfaces:**
- Consumes: `parse_aozora_number_before` (private helper, same file).
- Produces: `pub fn jizume_open_chars(source: &str) -> Option<u64>` (standalone `［＃ここからN字詰め］` AND final-clause compound `［＃ここから…、N字詰め］` → `Some(N)`); `pub fn is_jizume_close(source: &str) -> bool`. **NOT wired into block emission** — Phase 4's schema rotation surfaces jizume; jizume corpus AAT stays byte-stable (the spec's decision 4).

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn jizume_open_chars_recognizes_standalone_and_compound() {
    assert_eq!(jizume_open_chars("［＃ここから２３字詰め］"), Some(23));
    assert_eq!(jizume_open_chars("［＃ここから６字下げ、折り返して７字下げ、２１字詰め］"), Some(21));
    assert_eq!(jizume_open_chars("［＃ここから２字下げ］"), None);
    assert_eq!(jizume_open_chars("［＃ここで字詰め終わり］"), None);
    assert!(is_jizume_close("［＃ここで字詰め終わり］"));
    assert!(!is_jizume_close("［＃ここで字下げ終わり］"));
}

#[test]
fn compound_jizume_still_classifies_burasage_and_emits_no_jizume_block() {
    // The compound container already classifies as burasage (6,7) today —
    // the ２１字詰め clause is recognition-only until Phase 4.
    assert_eq!(
        burasage_open_indent("［＃ここから６字下げ、折り返して７字下げ、２１字詰め］"),
        Some((6, 7))
    );
    let src = "［＃ここから６字下げ、折り返して７字下げ、２１字詰め］\nあ\n［＃ここで字下げ終わり］\n";
    let doc: Value = serde_json::from_slice(&aat_json_from_bytes(src.as_bytes()).unwrap()).unwrap();
    let text = serde_json::to_string(&doc).unwrap();
    assert!(!text.contains("jizume_block"));
    assert!(text.contains("burasage"));
}
```

- [ ] **Step 2: Run to verify failure** — `cargo test -p ab-aozora-aat jizume` → FAIL (fns missing).

- [ ] **Step 3: Implement**

```rust
/// Recognize a jizume (字詰め) container-open marker and extract the
/// chars-per-line count — standalone (`［＃ここからN字詰め］`) or as the
/// FINAL clause of a compound container
/// (`［＃ここから６字下げ、折り返して７字下げ、２１字詰め］`).
///
/// Phase 4 wiring point: recognized but deliberately NOT emitted — AAT
/// schema v1 has no `jizume_block` kind (Phase 3 design spec, decision 4),
/// so jizume markers stay raw in AAT output until the Phase 4 schema
/// rotation.
#[must_use]
pub fn jizume_open_chars(source: &str) -> Option<u64> {
    let marker = source.trim();
    if !marker.starts_with("［＃ここから") || !marker.ends_with('］') {
        return None;
    }
    let (_, after) = marker.split_once("字詰め")?;
    if after != "］" {
        return None;
    }
    parse_aozora_number_before(marker, "字詰め")
}

/// Recognize the jizume container-close marker (`［＃ここで字詰め終わり］`).
#[must_use]
pub fn is_jizume_close(source: &str) -> bool {
    source.trim() == "［＃ここで字詰め終わり］"
}
```

- [ ] **Step 4: Run** — `cargo test -p ab-aozora-aat` → all pass (tripwire untouched).

- [ ] **Step 5: Commit**

```bash
git add crates/ab-aozora-aat/src/lib.rs
git commit -m "feat(ab-aozora-aat): jizume recognition API (parser-typed, AAT-raw until phase 4)"
```

---

### Task 9: `audit-aat-delta.py` — forward-rewrite + span-confinement audit

**Files:**
- Create: `reports/aat-fidelity/audit-aat-delta.py`
- Test: `reports/aat-fidelity/tests/test_audit_aat_delta.py`

**Interfaces:**
- Consumes: two dump dirs of per-work `*.json` AAT documents.
- Produces:
  `audit-aat-delta.py container-rewrite BASELINE_DIR CANDIDATE_DIR --summary-json OUT` and
  `audit-aat-delta.py span-confinement BASELINE_DIR CANDIDATE_DIR --summary-json OUT`.
  Exit 0 = PASS; exit 2 = ANY unclassified difference, missing file, or
  parse error (fail-closed, per spec — there is no exit 1). Tasks 10 and
  17 gate on it. The container grammar mirrors Task 7's Rust emission
  EXACTLY; a mismatch on real corpus data is an escalation (spec risk
  rule), never a reason to weaken to invariants.

- [ ] **Step 1: Write the failing tests**

```python
# reports/aat-fidelity/tests/test_audit_aat_delta.py
import json
import subprocess
import sys
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[1] / "audit-aat-delta.py"
LEGACY_WARNING = ("aozora upstream spans are sanitized-source byte offsets; "
                  "line_start and line_end are synthesized as 1")


def meta(version="ab-aozora 0.2.0 aat-schema 1 facade 0.2.0 wire-schema 3 (git aaa)",
         warnings=None):
    return {"adapter": "ab-aozora", "adapter_version": version,
            "parse_complete": True, "source_encoding": "utf-8",
            "source_hash": "sha256:x", "warnings": warnings or []}


def doc(blocks, **meta_kw):
    return {"version": 1, "work_id": "stdin", "blocks": blocks, "meta": meta(**meta_kw)}


def text(value, start=0, end=None):
    end = (start + len(value.encode())) if end is None else end
    return {"kind": "text", "value": value,
            "span": {"byte_start": start, "byte_end": end, "line_start": 1, "line_end": 1}}


def raw_marker(source, marker_kind, start=0):
    return {"kind": "raw", "source": source, "x-source-marker-kind": marker_kind,
            "x-provenance": "parser-derived",
            "span": {"byte_start": start, "byte_end": start + len(source.encode()),
                     "line_start": 1, "line_end": 1}}


def para(*content):
    return {"kind": "paragraph", "content": list(content)}


def write_dump(tmp_path, name, docs):
    d = tmp_path / name
    d.mkdir()
    for work, document in docs.items():
        (d / f"{work}.json").write_bytes(
            json.dumps(document, ensure_ascii=False, sort_keys=True).encode() + b"\n")
    return d


def run(mode, base, cand, tmp_path):
    out = tmp_path / "summary.json"
    proc = subprocess.run([sys.executable, str(SCRIPT), mode, str(base), str(cand),
                           "--summary-json", str(out)], capture_output=True, text=True)
    summary = json.loads(out.read_text()) if out.exists() else None
    return proc.returncode, summary, proc.stderr


OPEN = "［＃ここから罫囲み］"
CLOSE = "［＃ここで罫囲み終わり］"


def test_identical_dumps_pass(tmp_path):
    docs = {"w1": doc([para(text("あ\n"))])}
    base = write_dump(tmp_path, "a", docs)
    cand = write_dump(tmp_path, "b", docs)
    code, summary, _ = run("container-rewrite", base, cand, tmp_path)
    assert code == 0 and summary["verdict"] == "PASS"


def test_identity_pointer_change_is_class3(tmp_path):
    base = write_dump(tmp_path, "a", {"w1": doc([para(text("あ\n"))])})
    cand = write_dump(tmp_path, "b", {"w1": doc([para(text("あ\n"))],
        version="ab-aozora 0.2.0 aat-schema 1 facade 0.2.0 wire-schema 3 (git bbb)")})
    code, summary, _ = run("container-rewrite", base, cand, tmp_path)
    assert code == 0


def test_same_paragraph_pair_rewrites_to_container(tmp_path):
    # Baseline inner node "\n中身\n" (span 10..18); the grammar strips the
    # boundary newlines (value → "中身", span untouched — mirroring the Rust
    # strip helpers, which mutate values only) and strips the leading "\n"
    # of the post-close text.
    inner = text("\n中身\n", 10)          # span 10..18
    base = write_dump(tmp_path, "a", {"w1": doc([
        para(text("前\n"), raw_marker(OPEN, "containerOpen", 4), inner,
             raw_marker(CLOSE, "containerClose", 40), text("\n後\n", 70))])})
    cand = write_dump(tmp_path, "b", {"w1": doc([
        para(text("前\n")),
        {"kind": "keigakomi_block", "children": [para(text("中身", 10, 18))]},
        para(text("後\n", 70, 75))])})
    code, summary, err = run("container-rewrite", base, cand, tmp_path)
    assert code == 0, (summary, err)
    assert summary["classes"]["rewritten"] == 1


def test_unrelated_change_in_marker_work_fails(tmp_path):
    inner = text("\n中身\n", 10)
    base = write_dump(tmp_path, "a", {"w1": doc([
        para(text("前\n"), raw_marker(OPEN, "containerOpen", 4), inner,
             raw_marker(CLOSE, "containerClose", 40))])})
    cand = write_dump(tmp_path, "b", {"w1": doc([
        para(text("変わった\n")),   # unrelated text change smuggled in
        {"kind": "keigakomi_block", "children": [para(text("中身", 10, 18))]}])})
    code, _, _ = run("container-rewrite", base, cand, tmp_path)
    assert code == 2


def test_diff_without_markers_fails(tmp_path):
    base = write_dump(tmp_path, "a", {"w1": doc([para(text("あ\n"))])})
    cand = write_dump(tmp_path, "b", {"w1": doc([para(text("い\n"))])})
    code, _, _ = run("container-rewrite", base, cand, tmp_path)
    assert code == 2


def test_missing_file_fails(tmp_path):
    base = write_dump(tmp_path, "a", {"w1": doc([para(text("あ\n"))])})
    cand = write_dump(tmp_path, "b", {})
    (tmp_path / "b").mkdir(exist_ok=True)
    code, _, _ = run("container-rewrite", base, cand, tmp_path)
    assert code == 2


# --- span-confinement mode ---

def spanned(value, bs, be, ls, le):
    return {"kind": "text", "value": value,
            "span": {"byte_start": bs, "byte_end": be, "line_start": ls, "line_end": le}}


def test_span_only_change_passes(tmp_path):
    base = write_dump(tmp_path, "a", {"w1": doc([para(spanned("あ\n", 0, 4, 1, 1))],
        warnings=[{"message": LEGACY_WARNING, "line": 1}])})
    cand = write_dump(tmp_path, "b", {"w1": doc([para(spanned("あ\n", 100, 104, 3, 3))])})
    code, summary, err = run("span-confinement", base, cand, tmp_path)
    assert code == 0, (summary, err)


def test_value_change_fails_in_span_mode(tmp_path):
    base = write_dump(tmp_path, "a", {"w1": doc([para(spanned("あ\n", 0, 4, 1, 1))])})
    cand = write_dump(tmp_path, "b", {"w1": doc([para(spanned("い\n", 0, 4, 1, 1))])})
    code, _, _ = run("span-confinement", base, cand, tmp_path)
    assert code == 2


def test_invalid_span_fails(tmp_path):
    base = write_dump(tmp_path, "a", {"w1": doc([para(spanned("あ\n", 0, 4, 1, 1))])})
    cand = write_dump(tmp_path, "b", {"w1": doc([para(spanned("あ\n", 4, 0, 1, 1))])})
    code, _, _ = run("span-confinement", base, cand, tmp_path)
    assert code == 2


def test_wholesale_line1_synthesis_trips(tmp_path):
    nodes = [spanned(f"x{i}\n", i * 4, i * 4 + 3, 1, 1) for i in range(12)]
    base = write_dump(tmp_path, "a", {"w1": doc([para(*nodes)])})
    cand = write_dump(tmp_path, "b", {"w1": doc([para(*nodes)])})
    code, _, _ = run("span-confinement", base, cand, tmp_path)
    assert code == 2
```

- [ ] **Step 2: Run to verify failure** — `python -m pytest reports/aat-fidelity/tests/test_audit_aat_delta.py -v` → FAIL (script missing).

- [ ] **Step 3: Implement**

```python
#!/usr/bin/env python3
"""Fail-closed AAT delta audit between two dumps (Phase 3 rotations).

container-rewrite (rotation A): every difference must be explained by the
three-class taxonomy of the Phase 3 design spec —
  1. identity pointers (/meta/adapter_version),
  2. works whose baseline carries well-paired keigakomi/yokogumi container
     markers, checked by FORWARD REWRITE + DEEP EQUALITY: an independent
     reimplementation of ab-aozora-aat's classification over the baseline
     JSON must reproduce the candidate exactly,
  3. everything else byte-identical after identity substitution.

span-confinement (rotation B): after dropping /meta/adapter_version,
masking every span object, masking warning "line" values, and dropping the
legacy synthesized-span warning from the baseline, the documents must be
deeply equal; candidate spans must satisfy field invariants; the
line-synthesis tripline flags wholesale line=1 output.

Exit 0 = PASS. Exit 2 = ANY unclassified difference or reference error
(fail-closed; there is no exit 1). A container-rewrite mismatch on real
corpus data is an ESCALATION per the spec — do not weaken the grammar to
invariants.
"""
import argparse
import copy
import json
import pathlib
import sys

LEGACY_WARNING = ("aozora upstream spans are sanitized-source byte offsets; "
                  "line_start and line_end are synthesized as 1")
CONSTRUCTS = {
    "keigakomi_block": ("［＃ここから罫囲み］", "罫囲み"),
    "yokogumi_block": ("［＃ここから横組み］", "横組み"),
}


def die(msg):
    print(f"AUDIT FAIL: {msg}", file=sys.stderr)
    raise SystemExit(2)


def load_dir(d):
    files = {p.name: p for p in sorted(pathlib.Path(d).glob("*.json"))}
    if not files:
        die(f"no *.json under {d}")
    return files


def strip_identity(doc):
    doc = copy.deepcopy(doc)
    if isinstance(doc.get("meta"), dict):
        doc["meta"].pop("adapter_version", None)
    return doc


# --- container-rewrite grammar (mirror of blocks_from_inline_content) ------

def is_raw(node, marker_kind):
    return (isinstance(node, dict) and node.get("kind") == "raw"
            and node.get("x-source-marker-kind") == marker_kind)


def open_kind(node):
    if is_raw(node, "containerOpen"):
        source = (node.get("source") or "").strip()
        for kind, (marker, _) in CONSTRUCTS.items():
            if source == marker:
                return kind
    return None


def close_matches(node, needle):
    return is_raw(node, "containerClose") and needle in (node.get("source") or "")


def strip_leading_newline(node):
    if isinstance(node, dict) and node.get("kind") == "text":
        value = node.get("value", "")
        if value.startswith("\n"):
            node = dict(node, value=value[1:])
    return node


def strip_trailing_newline(node):
    if isinstance(node, dict) and node.get("kind") == "text":
        value = node.get("value", "")
        if value.endswith("\n"):
            node = dict(node, value=value[:-1])
    return node


def make_para(content):
    # Mirrors push_paragraph_if_not_empty: only an EMPTY content list is
    # dropped. Empty text nodes produced by boundary stripping stay —
    # the Rust strip helpers mutate values without removing nodes.
    return {"kind": "paragraph", "content": list(content)} if content else None


def is_empty_text(node):
    return (isinstance(node, dict) and node.get("kind") == "text"
            and node.get("value", "") == "")


def scan_segment(nodes, start, needle):
    """Mirror find_matching_container_close over one content slice."""
    for k in range(start, len(nodes)):
        node = nodes[k]
        if is_raw(node, "containerOpen"):
            return "abort", k
        if close_matches(node, needle):
            return "close", k
    return None, None


def rewrite_blocks(blocks):
    """One grammar pass over a block list; returns (rewritten, count)."""
    out, count, i = [], 0, 0
    while i < len(blocks):
        block = blocks[i]
        if isinstance(block, dict) and isinstance(block.get("children"), list):
            children, inner_count = rewrite_blocks(block["children"])
            count += inner_count
            block = dict(block, children=children)
        if not (isinstance(block, dict) and block.get("kind") == "paragraph"):
            out.append(block)
            i += 1
            continue
        content = block.get("content", [])
        oi, kind = next(((j, open_kind(n)) for j, n in enumerate(content)
                         if open_kind(n)), (None, None))
        if oi is None:
            out.append(block)
            i += 1
            continue
        needle = CONSTRUCTS[kind][1]
        # Scan forward through the flat stream for the matching close;
        # any containerOpen aborts. First the open paragraph's remainder,
        # then each following paragraph block (non-paragraph blocks carry
        # no container markers by construction — see Task 7's abort rule).
        close_block_index = ci = None
        state, k = scan_segment(content, oi + 1, needle)
        if state == "close":
            close_block_index, ci = i, k
        elif state is None:
            j = i + 1
            while j < len(blocks):
                nxt = blocks[j]
                if isinstance(nxt, dict) and nxt.get("kind") == "paragraph":
                    state, k = scan_segment(nxt.get("content", []), 0, needle)
                    if state == "close":
                        close_block_index, ci = j, k
                    if state is not None:
                        break
                j += 1
        if close_block_index is None:
            out.append(block)  # unpaired/aborted: candidate must equal baseline
            i += 1
            continue
        pre = content[:oi]
        if close_block_index == i:
            inner = content[oi + 1:ci]
            post = content[ci + 1:]
            if inner:
                inner = [strip_leading_newline(inner[0])] + inner[1:]
                inner = inner[:-1] + [strip_trailing_newline(inner[-1])]
            inner_para = make_para(inner)
            children = [inner_para] if inner_para else []
        else:
            head = content[oi + 1:]
            close_content = blocks[close_block_index].get("content", [])
            tail = close_content[:ci]
            post = close_content[ci + 1:]
            middle = blocks[i + 1:close_block_index]
            if head:
                head = [strip_leading_newline(head[0])] + head[1:]
            if tail:
                tail = tail[:-1] + [strip_trailing_newline(tail[-1])]
            elif head:
                head = head[:-1] + [strip_trailing_newline(head[-1])]
            children = []
            head_para = make_para(head)
            if head_para:
                children.append(head_para)
            children.extend(middle)
            tail_para = make_para(tail)
            if tail_para:
                children.append(tail_para)
        if not children:
            children = [{"kind": "paragraph", "content": []}]
        pre_para = make_para(pre)
        if pre_para:
            out.append(pre_para)
        out.append({"kind": kind, "children": children})
        count += 1
        # strip_next_leading_newline after close: strip post[0]'s leading
        # newline; if that empties the text node, DROP it (the Rust
        # post-close path removes emptied nodes — asymmetric with
        # strip_boundary_newlines, which keeps them).
        if post:
            first = strip_leading_newline(post[0])
            post = ([] if is_empty_text(first) else [first]) + post[1:]
        post_para = make_para(post)
        rest = blocks[close_block_index + 1:]
        rewritten_rest, rest_count = rewrite_blocks(
            ([post_para] if post_para else []) + rest)
        out.extend(rewritten_rest)
        return out, count + rest_count
    return out, count


def container_rewrite_mode(base_doc, cand_doc, name, summary):
    base = strip_identity(base_doc)
    cand = strip_identity(cand_doc)
    if base == cand:
        summary["classes"]["identical"] += 1
        return
    rewritten_blocks, count = rewrite_blocks(base.get("blocks", []))
    rewritten = dict(base, blocks=rewritten_blocks)
    if count == 0:
        die(f"{name}: differs but baseline has no well-paired "
            f"keigakomi/yokogumi markers (unclassified)")
    if rewritten != cand:
        die(f"{name}: candidate is not exactly the grammar's rewrite "
            f"({count} container(s) rewritten) — escalate per spec")
    summary["classes"]["rewritten"] += 1


# --- span-confinement -------------------------------------------------------

def mask_spans(node, spans_out):
    if isinstance(node, dict):
        return {k: (spans_out.append(v) or None) if k == "span" and isinstance(v, dict)
                else mask_spans(v, spans_out) for k, v in node.items()}
    if isinstance(node, list):
        return [mask_spans(v, spans_out) for v in node]
    return node


def span_confinement_mode(base_doc, cand_doc, name, summary):
    base = strip_identity(base_doc)
    cand = strip_identity(cand_doc)
    meta = base.get("meta", {})
    meta["warnings"] = [w for w in meta.get("warnings", [])
                        if w.get("message") != LEGACY_WARNING]
    for doc in (base, cand):
        for w in doc.get("meta", {}).get("warnings", []):
            if "line" in w:
                w["line"] = None
    cand_spans = []
    base_masked = mask_spans(base, [])
    cand_masked = mask_spans(cand, cand_spans)
    if base_masked != cand_masked:
        die(f"{name}: non-span difference in span-confinement mode")
    all_line1 = True
    for span in cand_spans:
        keys = {"byte_start", "byte_end", "line_start", "line_end"}
        if not keys <= set(span):
            # wire-shaped {start,end} spans (none expected in AAT) fail too
            die(f"{name}: span missing AAT fields: {span}")
        if not (isinstance(span["byte_start"], int) and isinstance(span["byte_end"], int)
                and span["byte_end"] >= span["byte_start"]
                and span["line_end"] >= span["line_start"] >= 1):
            die(f"{name}: invalid span {span}")
        if span["line_start"] != 1 or span["line_end"] != 1:
            all_line1 = False
    if len(cand_spans) >= 10 and all_line1:
        die(f"{name}: {len(cand_spans)} spans all line 1/1 — synthesis tripline")
    summary["classes"]["span_confined"] += 1


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("mode", choices=["container-rewrite", "span-confinement"])
    ap.add_argument("baseline_dir")
    ap.add_argument("candidate_dir")
    ap.add_argument("--summary-json", required=True)
    args = ap.parse_args()
    base_files = load_dir(args.baseline_dir)
    cand_files = load_dir(args.candidate_dir)
    missing = sorted(set(base_files) ^ set(cand_files))
    if missing:
        die(f"file sets differ: {missing[:10]}")
    summary = {"mode": args.mode, "compared": len(base_files),
               "classes": {"identical": 0, "rewritten": 0, "span_confined": 0},
               "verdict": "PASS"}
    handler = (container_rewrite_mode if args.mode == "container-rewrite"
               else span_confinement_mode)
    for name in sorted(base_files):
        try:
            base_doc = json.loads(base_files[name].read_bytes())
            cand_doc = json.loads(cand_files[name].read_bytes())
        except json.JSONDecodeError as err:
            die(f"{name}: unreadable ({err})")
        handler(base_doc, cand_doc, name, summary)
    pathlib.Path(args.summary_json).write_text(json.dumps(summary, indent=2) + "\n")
    print(json.dumps(summary, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
```

Implementation notes the engineer must honor:
- `container-rewrite` class 3 uses full-document equality after
  `adapter_version` removal (parsed-JSON equality is sufficient here
  because both dumps are emitted by serde_json under identical ordering
  rules; the *stage-0* gate is where byte-level identity is proven).
- The grammar intentionally handles ONLY well-paired markers with no
  intervening `containerOpen` — exactly Task 7's Rust rule. If real
  corpus works fail the deep-equality check, collect up to 5 example
  work IDs, stop, and escalate with the examples (spec risk rule).
- The `test_identity_pointer_change_is_class3` fixture also implicitly
  tests that `strip_identity` does not mutate the input (deepcopy).

- [ ] **Step 4: Run** — `python -m pytest reports/aat-fidelity/tests/test_audit_aat_delta.py -v` → 11 passed. Fix the grammar (not the tests) until the hand-built same-paragraph fixture passes.

- [ ] **Step 5: Commit**

```bash
git add reports/aat-fidelity/audit-aat-delta.py reports/aat-fidelity/tests/test_audit_aat_delta.py
git commit -m "feat(aat-fidelity): fail-closed delta audit (forward container rewrite + span confinement)"
```

---

### Task 10: Rotation A gate — delta, conformance, perf, conversion audit (hinoki)

**Files:**
- Create: `docs/superpowers/reports/2026-07-11-phase3-capability-delta.{md,summary.json}`
- Create: `docs/superpowers/reports/2026-07-11-phase3-capability-conformance.{md,summary.json}` (+ seed variants) and `…-capability-conformance-gate.summary.json`
- Create: `docs/superpowers/reports/2026-07-11-phase3-capability-perf.{md,summary.json,runner.json}`
- Create: `docs/superpowers/reports/2026-07-11-ab-aozora-phase3-capability-conversion-audit.{md,summary.json}`

**Interfaces:**
- Consumes: Tasks 4–9 merged; `C1` = ledger-recorded HEAD after Task 9; stage-0 dump; `audit-aat-delta.py container-rewrite`; scorer with `--adapter-diagnostics`; `compare-adapter-rows.py`; `ab-aat-to-parser-ir audit-corpus`.
- Produces: three PASS gate summaries (`stage: "rotation-a"`, gates `delta` / `conformance` / `perf`) + the conversion-audit evidence pair Task 11's registry row cites; dump `/db/ab-validator/aat-corpus/ab-aozora-phase3-capability-<C1:0:7>` (retained; Task 17's baseline).

- [ ] **Step 1: Record C1** — `CANDIDATE=$(git rev-parse HEAD)`; ledger: `Phase 3 C1 = <sha>`. Push the branch.

- [ ] **Step 2: Detached clean build on hinoki** — identical commands to Task 3 Step 2 with the new `$CANDIDATE`. Expected version line: `ab-aozora 0.2.0 aat-schema 1 facade 0.2.0 wire-schema 3 (git $CANDIDATE)`. Record `bin_sha256`.

- [ ] **Step 3: Full-corpus run** — identical to Task 3 Step 3 with `--report-id phase3-capability --out-dir /db/ab-validator/aat-corpus/ab-aozora-phase3-capability-<C1:0:7>`. Expected: 17886 files.

- [ ] **Step 4: Delta audit vs stage 0**

```bash
ssh hinoki.hyakutake-barbel.ts.net 'cd ~/Projects/soranoha/.worktrees/parser-fork-phase3/ab-validator && python3 reports/aat-fidelity/audit-aat-delta.py container-rewrite /db/ab-validator/aat-corpus/ab-aozora-phase3-stage0-<C0:0:7>/aat /db/ab-validator/aat-corpus/ab-aozora-phase3-capability-<C1:0:7>/aat --summary-json ~/phase3-capability-delta.json; echo "exit=$?"'
```

Expected: `exit=0`, `verdict: PASS`. Record class counts and check
magnitudes against the backlog references (`rewritten` should cover on
the order of the keigakomi(106)+yokogumi(88) work sets — record the exact
number; jizume-only works must land in `identical`). **On exit 2:**
collect the example work IDs from stderr and STOP — escalate to the human
with the examples (grammar-vs-parser divergence is a design finding).

- [ ] **Step 5: Conformance, both suites — the 25/25 must gate**

Locally (justfile lane now passes `--adapter-diagnostics` from Task 6):

```bash
just aozora-notation-spec-comparison REPORT_MD=docs/superpowers/reports/2026-07-11-phase3-capability-conformance.md SUMMARY_JSON=docs/superpowers/reports/2026-07-11-phase3-capability-conformance.summary.json
just official-docs-seed-comparison REPORT_MD=docs/superpowers/reports/2026-07-11-phase3-capability-conformance-seed.md SUMMARY_JSON=docs/superpowers/reports/2026-07-11-phase3-capability-conformance-seed.summary.json
python3 - <<'EOF'
import json
d = json.load(open('docs/superpowers/reports/2026-07-11-phase3-capability-conformance.summary.json'))
must = [r for r in d['rows'] if r['adapter'] == 'ab-aozora' and r['level'] == 'must']
fails = [r['vector'] for r in must if r['status'] == 'fail']
skips = [r['vector'] for r in must if r['status'] == 'skip']
print(f"must={len(must)} fail={fails} skip={skips}")
assert len(must) == 25 and not fails and not skips, "25/25 must gate FAILED"
EOF
python reports/parser-conformance/compare-adapter-rows.py docs/superpowers/reports/2026-07-11-phase3-stage0-conformance.summary.json docs/superpowers/reports/2026-07-11-phase3-capability-conformance.summary.json --adapter ab-aozora || true
```

Expected: the assert passes (`pua_collision`, `unclosed_bracket`,
`tate_chu_yoko` now scored-and-passing via diagnostics). The row
comparator WILL report differences — review each: acceptable diffs are
skip→pass/warning movements from diagnostics scoring and
keigakomi/yokogumi vector improvements; anything else must be explained
in the report or fixed. Record the full diff list in the gate report.
**If any newly-comparable must-level diagnostics vector fails:** fix the
fork (never the vector), then C1 moves and the stage re-runs.

- [ ] **Step 6: Perf workset** — identical to Task 3 Step 6 with log names `phase3-capability-perf.*`. Expected: 0 timeouts, ≤10% regression (the classifier branches are per-marker string compares; expect noise-level).

- [ ] **Step 7: Conversion audit against mapping 0.2.8**

```bash
ssh hinoki.hyakutake-barbel.ts.net 'cd ~/Projects/soranoha/.worktrees/parser-fork-phase3/ab-validator && export RUSTC_WRAPPER= SCCACHE_DISABLE=1 && cargo build -p ab-aat-to-parser-ir --release && nohup ./target/release/ab-aat-to-parser-ir audit-corpus --aat-dir /db/ab-validator/aat-corpus/ab-aozora-phase3-capability-<C1:0:7>/aat --mapping data/aat-to-parser-ir-mapping-v1.json --summary-json ~/phase3-capability-audit.summary.json --report-md ~/phase3-capability-audit.md --compat-edn-out ~/phase3-capability-compat.edn --jobs 32 --abc-root data/abc-schemas > ~/phase3-capability-audit.log 2>&1 &'
```

Expected: `files_succeeded == files_attempted == 17886`, `files_failed == 0`.
The `ab-aat-to-parser-ir` integration suite must be green at this commit
(`cargo test -p ab-aat-to-parser-ir` locally — 57/57; keigakomi/yokogumi
block kinds are schema-v1, so mapping 0.2.8 already handles them; ANY
converter change needed here is out of contract → escalate).

- [ ] **Step 8: Evidence + freeze**

Copy down the audit outputs as
`2026-07-11-ab-aozora-phase3-capability-conversion-audit.{md,summary.json}`
and the delta summary; write the three gate summaries — filenames the
Task 18 checkpoint reads verbatim:
`2026-07-11-phase3-capability-delta.summary.json` (gate `delta`, details =
the audit summary incl. `verdict`),
`2026-07-11-phase3-capability-conformance-gate.summary.json` (gate
`conformance`, details MUST include `must_fail: 0`, `must_skip: 0`, plus
the row-diff list), and
`2026-07-11-phase3-capability-perf.summary.json` (gate `perf`, details
MUST include `new_timeouts: 0` and `median_delta_pct`). All carry
`stage: "rotation-a"` and the shared candidate triple. Write the
narrative delta/perf reports. Commit:

```bash
git add docs/superpowers/reports/2026-07-11-phase3-capability-* docs/superpowers/reports/2026-07-11-ab-aozora-phase3-capability-*
git commit -m "docs(reports): phase 3 rotation A gate evidence — capability PASS"
```

Ledger: `Rotation A gates PASS (C1=<sha>, bin=<sha256>, rewritten=<n>)`.

---

### Task 11: Rotation A registry row (abc, unadmitted)

**Files:**
- Modify: `abc/data/aat-parser-ir-compatibility.edn` (append one row after the Phase 2 `ab-aozora` row at ~line 739)

**Interfaces:**
- Consumes: Task 10's conversion-audit summary + compat EDN candidate (the audit's own emitted pairing — the Phase 2 rule: fresh evidence, NO carried-over or template coordinates).
- Produces: an unadmitted row for the 0.2.0/C1 identity.

- [ ] **Step 1: Append the row**

Copy the exact shape of the Phase 2 `ab-aozora` row (lines ~735–761), with
EVERY value taken from Task 10's audit artifacts, not from the old row:
`:aat_adapter_version` = the verbatim C1 `--version` line; mapping
coordinates (`:mapping_id/:mapping_version/:mapping_hash/:mapping_schema_hash/:parser_ir_schema_hash`)
= exactly what `~/phase3-capability-compat.edn` / the audit summary emit;
`:evidence_scope` numbers (`:files_scanned/:files_succeeded/:files_failed/:parser_ir_nodes/:divergence_records/:divergence_occurrences/:rules_total/:rules_emitted/:rules_missing/:unsupported_occurrences`)
= the audit summary's numbers; `:corpus` = `"ab-aozora Phase 3 rotation A dump (aozora-full corpus, 17886 works)"`
(prose convention matching the Phase 2 row); `:compatibility` = what the
audit emits (expect `"lossy"`).

- [ ] **Step 2: Validate**

```bash
cd abc
clojure -M -m abc.tools.adr-governance      # expect: ADR governance valid
clojure -M:test:kaocha -m kaocha.runner     # abc suite green (admission tests prove no bypass)
```

- [ ] **Step 3: Commit**

```bash
git add abc/data/aat-parser-ir-compatibility.edn
git commit -m "feat(abc): unadmitted registry row for ab-aozora 0.2.0 rotation A identity"
```

---

### Task 12: Denominator attribution audit

**Files:**
- Create: `reports/aat-fidelity/denominator-attribution.py`
- Create: `docs/superpowers/reports/2026-07-11-keigakomi-yokogumi-denominator-attribution.{md,summary.json}` (script output + narrative)

**Interfaces:**
- Consumes: the pinned nix corpus (`nix build --no-link --print-out-paths .#aozorabunko-corpus`), Shift_JIS work files; the frozen figures: keigakomi 717 / yokogumi 3,690 / jizume 3,239 (2026-07-09 recompute) vs block-start counts 200 / 183 / 1,373 (backlog).
- Produces: a dated report decomposing each denominator by marker form and stating which subset denominates the new block classifiers' rates. **No frozen report is edited.**

- [ ] **Step 1: Implement the decomposition script**

```python
#!/usr/bin/env python3
"""Decompose keigakomi/yokogumi/jizume marker counts by form on the pinned corpus.

Forms counted per construct token (罫囲み / 横組み / 字詰め):
  block_start   ［＃ここから…<token>…］
  block_end     ［＃ここで…<token>…終わり…］ (or the token inside a ここで marker)
  inline_attr   ［＃「…」は…<token>…］  (per-part attribution form)
  other         any other ［＃…<token>…］ marker

Usage: denominator-attribution.py CORPUS_ROOT --summary-json OUT
Reads every *.txt under CORPUS_ROOT as Shift_JIS (errors=replace).
"""
import argparse
import json
import pathlib
import re
import sys
from collections import Counter

TOKENS = {"keigakomi": "罫囲み", "yokogumi": "横組み", "jizume": "字詰め"}
MARKER_RE = re.compile(r"［＃[^］]*］")


def classify(marker, token):
    if token not in marker:
        return None
    if marker.startswith("［＃ここから"):
        return "block_start"
    if marker.startswith("［＃ここで"):
        return "block_end"
    if marker.startswith("［＃「"):
        return "inline_attr"
    return "other"


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("corpus_root")
    ap.add_argument("--summary-json", required=True)
    args = ap.parse_args()
    counts = {name: Counter() for name in TOKENS}
    works = {name: set() for name in TOKENS}
    files = sorted(pathlib.Path(args.corpus_root).rglob("*.txt"))
    if not files:
        print(f"ERROR: no *.txt under {args.corpus_root}", file=sys.stderr)
        return 2
    for path in files:
        text = path.read_bytes().decode("shift_jis", errors="replace")
        for marker in MARKER_RE.findall(text):
            for name, token in TOKENS.items():
                form = classify(marker, token)
                if form:
                    counts[name][form] += 1
                    works[name].add(path.name)
    summary = {"files_scanned": len(files),
               "constructs": {name: {"forms": dict(counts[name]),
                                     "total": sum(counts[name].values()),
                                     "works": len(works[name])}
                              for name in TOKENS}}
    pathlib.Path(args.summary_json).write_text(json.dumps(summary, indent=2,
                                                          ensure_ascii=False) + "\n")
    print(json.dumps(summary, indent=2, ensure_ascii=False))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
```

- [ ] **Step 2: Run on the pinned corpus**

```bash
CORPUS=$(nix build --no-link --print-out-paths .#aozorabunko-corpus)
python3 reports/aat-fidelity/denominator-attribution.py "$CORPUS" --summary-json docs/superpowers/reports/2026-07-11-keigakomi-yokogumi-denominator-attribution.summary.json
```

Expected: `block_start` counts near 200 (keigakomi) / 183 (yokogumi) /
1,373 (jizume) — cross-validating the backlog grep — with the remainder
attributed to `block_end`/`inline_attr`/`other`. If totals do not
reconcile with 717/3,690/3,239 to within the forms counted, say so
honestly in the report and name the residual (the inventory may count
additional units); do NOT force agreement.

- [ ] **Step 3: Write the narrative report**

`…-attribution.md`: the decomposition table, the reconciliation against
717/3,690/3,239 and against the block-start figures, and the explicit
statement the spec requires: *the all-form inventory figures denominate
the fidelity instrument's construct rates (consistent with every other
construct); the block-start subset is the ceiling for `keigakomi_block`/
`yokogumi_block` numerators, so block-classifier rates must be read
against the attribution table, and this report is the citation for that
reading.* Cite the backlog report and the 2026-07-09 recompute.

- [ ] **Step 4: Commit**

```bash
git add reports/aat-fidelity/denominator-attribution.py docs/superpowers/reports/2026-07-11-keigakomi-yokogumi-denominator-attribution.*
git commit -m "docs(reports): keigakomi/yokogumi/jizume denominator attribution audit"
```

---

### Task 13: Sanitize offset map (`sanitize_mapped`)

**Files:**
- Modify: `crates/ab-aozora-pipeline/src/lexer/sanitize.rs`
- Test: same file's `mod tests` + property tests

**Interfaces:**
- Consumes: the five existing transforms (BOM strip; `normalize_line_endings`; `isolate_decorative_rules`; `rewrite_accent_spans_collecting`; `neutralize_sentinel_collisions` — the last is byte-length-preserving, no edits).
- Produces:

```rust
pub struct MapEdit { pub src_start: usize, pub src_end: usize, pub dst_start: usize, pub dst_end: usize }
pub struct OffsetMap { /* sorted, non-overlapping edits + src/dst lens */ }
impl OffsetMap {
    pub fn to_source_offset(&self, dst: usize) -> usize;  // span-start semantics: inside an edit → src_start
    pub fn to_source_end(&self, dst: usize) -> usize;     // span-end semantics: inside an edit → src_end
}
pub struct SanitizeMaps { steps: Vec<OffsetMap> }         // query walks steps in reverse
impl SanitizeMaps { pub fn to_source_offset(&self, dst: usize) -> usize; pub fn to_source_end(&self, dst: usize) -> usize; }
pub struct SanitizeMappedOutput<'s> { pub text: Cow<'s, str>, pub diagnostics: Vec<Diagnostic>, pub maps: SanitizeMaps }
pub fn sanitize_mapped(source: &str) -> SanitizeMappedOutput<'_>;
```

Task 14 consumes `sanitize_mapped` and the two query methods. Existing
`sanitize()` behavior must be bit-identical (delegate to the same cores).

- [ ] **Step 1: Write the failing tests first**

```rust
#[test]
fn sanitize_mapped_text_equals_sanitize() {
    for src in ["\u{feff}あ\r\nい\r う", "plain", "〔fune`bre〕\r\n----------\nx",
                "あ\u{e001}い", ""] {
        let plain = sanitize(src);
        let mapped = sanitize_mapped(src);
        assert_eq!(plain.text, mapped.text, "text drift on {src:?}");
        assert_eq!(plain.diagnostics.len(), mapped.diagnostics.len());
    }
}

#[test]
fn offset_map_translates_through_bom_and_crlf() {
    // source: BOM(3) + "あ\r\nい"  → sanitized: "あ\nい"
    let src = "\u{feff}あ\r\nい";
    let m = sanitize_mapped(src).maps;
    assert_eq!(m.to_source_offset(0), 3);        // あ starts after BOM
    assert_eq!(m.to_source_offset(3), 6);        // \n ← \r\n start
    assert_eq!(m.to_source_end(4), 8);           // end of \n ← end of \r\n
    assert_eq!(m.to_source_offset(4), 8);        // い
}

#[test]
fn offset_map_is_monotone_and_content_preserving() {
    let src = "\u{feff}前〔e'te'〕中\r\n==========\n後\r尾";
    let out = sanitize_mapped(src);
    let dst = out.text.as_ref();
    let mut prev = 0usize;
    for i in (0..=dst.len()).filter(|i| dst.is_char_boundary(*i)) {
        let s = out.maps.to_source_offset(i);
        assert!(s >= prev && s <= src.len(), "monotonicity at {i}");
        prev = s;
    }
    // Unedited runs must slice identically: check every char that maps to a
    // width-equal source region.
    for (i, ch) in dst.char_indices() {
        let (s, e) = (out.maps.to_source_offset(i), out.maps.to_source_end(i + ch.len_utf8()));
        if e - s == ch.len_utf8() {
            assert_eq!(&src[s..e], &dst[i..i + ch.len_utf8()], "content drift at {i}");
        }
    }
}
```

Add a proptest (the crate's dev-deps already include proptest via the
facade family; if not present in this crate add `proptest = "1.11"` to
`[dev-dependencies]`):

```rust
proptest::proptest! {
    #[test]
    fn mapped_output_always_matches_unmapped(parts in proptest::collection::vec(
        proptest::prop_oneof![
            proptest::strategy::Just("あいう".to_string()),
            proptest::strategy::Just("\r\n".to_string()),
            proptest::strategy::Just("\r".to_string()),
            proptest::strategy::Just("〔cafe'〕".to_string()),
            proptest::strategy::Just("----------\n".to_string()),
            proptest::strategy::Just("\u{feff}".to_string()),
            proptest::strategy::Just("\u{e001}".to_string()),
        ], 0..12)) {
        let src: String = parts.concat();
        let plain = sanitize(&src);
        let mapped = sanitize_mapped(&src);
        proptest::prop_assert_eq!(plain.text.as_ref(), mapped.text.as_ref());
        let dst_len = mapped.text.len();
        let mut prev = 0usize;
        for i in (0..=dst_len).filter(|i| mapped.text.is_char_boundary(*i)) {
            let s = mapped.maps.to_source_offset(i);
            proptest::prop_assert!(s >= prev && s <= src.len());
            prev = s;
        }
    }
}
```

- [ ] **Step 2: Run to verify failure** — `cargo test -p ab-aozora-pipeline sanitize_mapped` → FAIL (types missing).

- [ ] **Step 3: Implement**

Core types + query (complete, add near `SanitizeOutput`):

```rust
/// One non-identity rewrite: source bytes `src_start..src_end` became
/// output bytes `dst_start..dst_end`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MapEdit {
    pub src_start: usize,
    pub src_end: usize,
    pub dst_start: usize,
    pub dst_end: usize,
}

/// Offset map for ONE transform pass: sorted, non-overlapping edits;
/// offsets between edits translate by the accumulated length delta.
#[derive(Debug, Clone, Default)]
pub struct OffsetMap {
    edits: Vec<MapEdit>,
}

impl OffsetMap {
    fn locate(&self, dst: usize) -> Result<usize, isize> {
        // Returns Ok(edit index) when dst falls inside an edit's dst
        // range, Err(delta) with the accumulated (src - dst) delta of all
        // edits ending at or before dst otherwise.
        let mut delta: isize = 0;
        for (i, e) in self.edits.iter().enumerate() {
            if dst < e.dst_start {
                return Err(delta);
            }
            if dst < e.dst_end {
                return Ok(i);
            }
            delta += (e.src_end - e.src_start) as isize - (e.dst_end - e.dst_start) as isize;
        }
        Err(delta)
    }

    #[must_use]
    pub fn to_source_offset(&self, dst: usize) -> usize {
        match self.locate(dst) {
            Ok(i) => self.edits[i].src_start,
            Err(delta) => (dst as isize + delta) as usize,
        }
    }

    #[must_use]
    pub fn to_source_end(&self, dst: usize) -> usize {
        // End-exclusive semantics: an end whose last covered byte
        // (dst - 1) lands inside an edit resolves to the edit's src_end;
        // an end at an edit's dst_start resolves BEFORE the edit. An
        // empty span's end behaves like its start.
        if dst == 0 {
            return self.to_source_offset(0);
        }
        match self.locate(dst - 1) {
            Ok(i) => self.edits[i].src_end,
            Err(delta) => (dst as isize + delta) as usize,
        }
    }
}

/// Composed per-step maps; queries walk the steps in reverse.
#[derive(Debug, Clone, Default)]
pub struct SanitizeMaps {
    steps: Vec<OffsetMap>,
}

impl SanitizeMaps {
    #[must_use]
    pub fn to_source_offset(&self, dst: usize) -> usize {
        self.steps.iter().rev().fold(dst, |o, m| m.to_source_offset(o))
    }

    #[must_use]
    pub fn to_source_end(&self, dst: usize) -> usize {
        self.steps.iter().rev().fold(dst, |o, m| m.to_source_end(o))
    }
}

/// [`sanitize`] plus the composed offset map (Phase 3 span semantics).
#[derive(Debug)]
pub struct SanitizeMappedOutput<'s> {
    pub text: Cow<'s, str>,
    pub diagnostics: Vec<Diagnostic>,
    pub maps: SanitizeMaps,
}
```

`sanitize_mapped` mirrors `sanitize`'s exact step sequence; each step
appends one `OffsetMap` (empty = identity):

1. **BOM strip:** count stripped bytes `k`; if `k > 0`, one edit
   `{src: 0..k, dst: 0..0}`.
2. **CR/LF:** refactor `normalize_line_endings` into
   `fn normalize_line_endings_core(input: &str, edits: Option<&mut Vec<MapEdit>>) -> String`
   preserving the existing scan logic verbatim; at each `\r\n`→`\n`
   substitution record `{src: i..i+2, dst: j..j+1}` and at each lone
   `\r`→`\n` record `{src: i..i+1, dst: j..j+1}` (where `i` = input byte
   position, `j` = output length before the push). The existing
   `pub fn normalize_line_endings` becomes a `None`-edits wrapper.
3. **Rule isolation:** same pattern for `isolate_decorative_rules` — at
   each inserted blank line record `{src: i..i, dst: j..j+1}`.
4. **Accent rewrite:** same pattern for
   `rewrite_accent_spans_collecting` — at each digraph replacement
   record `{src: r.start..r.end, dst: out_start..out_len_after}`.
5. **PUA neutralization:** byte-length-preserving — push
   `OffsetMap::default()`.

Steps that take the borrowed fast path (no `\r`, no rule line, no `〔`)
contribute `OffsetMap::default()`. Keep each core private; the public
unmapped functions delegate with `None` so mapped/unmapped can never
drift.

- [ ] **Step 4: Run** — `cargo test -p ab-aozora-pipeline` → all green (existing sanitize tests + the new map tests + proptest).

- [ ] **Step 5: Commit**

```bash
git add crates/ab-aozora-pipeline/src/lexer/sanitize.rs crates/ab-aozora-pipeline/Cargo.toml
git commit -m "feat(ab-aozora-pipeline): sanitize_mapped with composed offset map (span semantics groundwork)"
```

---### Task 14: Rotation B — span composition, real lines, version 0.3.0, tripwire

**Files:**
- Modify: `crates/ab-aozora-aat/src/lib.rs` (DecodedSource, `sanitize_for_aat`, `aozora_body_text` range refactor, `span_json`, `diagnostic_warning`, synthesized-warning removal, `diagnostics_json_from_bytes` rebase, tripwire literal, new unit tests)
- Modify: `crates/ab-aozora-aat/Cargo.toml`, `crates/ab-aozora/Cargo.toml` (`0.3.0`), `crates/ab-aozora/tests/wire.rs` (`ab-aozora 0.3.0`)

**Interfaces:**
- Consumes: `ab_aozora_pipeline::lexer::sanitize::{sanitize_mapped, SanitizeMaps}` (Task 13).
- Produces: every AAT span = `{byte_start, byte_end}` into `DecodedSource.text` (the full decoded source) with real `line_start`/`line_end` (1-based; `line_end` = line of `byte_end - 1`, or of `byte_start` for empty spans); warning `line` = real line of the diagnostic span start; the legacy synthesized warning GONE; `--mode diagnostics` spans rebased identically. Env-gated `reference_parity` tests will go red — expected, Task 15 replaces them; workspace tests must be green.

- [ ] **Step 1: Write the failing unit test**

```rust
#[test]
fn spans_are_decoded_source_offsets_with_real_lines() {
    // BOM + CRLF: decoded text (post-BOM-strip, per decode_source_bytes)
    // is "あ\r\nい\n"; sanitized body is "あ\nい\n".
    let bytes = [b"\xef\xbb\xbf".as_ref(), "あ\r\nい\n".as_bytes()].concat();
    let doc: Value = serde_json::from_slice(&aat_json_from_bytes(&bytes).unwrap()).unwrap();
    let spans: Vec<&Value> = collect_spans(&doc["blocks"]);
    assert!(!spans.is_empty());
    // First text node "あ\n" ← decoded "あ\r\n" = bytes 0..5, line 1.
    assert_eq!(spans[0]["byte_start"], 0);
    assert_eq!(spans[0]["byte_end"], 5);
    assert_eq!(spans[0]["line_start"], 1);
    // Some span must sit on line 2 (the "い" line).
    assert!(spans.iter().any(|s| s["line_start"] == 2), "no real line >1: {spans:?}");
    // The legacy synthesized warning is gone.
    let warnings = doc["meta"]["warnings"].as_array().unwrap();
    assert!(warnings.iter().all(|w| w["message"].as_str()
        != Some("aozora upstream spans are sanitized-source byte offsets; line_start and line_end are synthesized as 1")));
}

fn collect_spans(v: &Value) -> Vec<&Value> {
    let mut out = Vec::new();
    match v {
        Value::Object(map) => {
            if let Some(span) = map.get("span") { out.push(span); }
            for val in map.values() { out.extend(collect_spans(val)); }
        }
        Value::Array(items) => for val in items { out.extend(collect_spans(val)); },
        _ => {}
    }
    out
}
```

(The exact expected offsets in the first assertions are the engineer's to
verify by hand against the input literal before accepting them — if the
parser splits text nodes differently, adjust the assertion to the first
span whose value is known, keeping the line-2 and no-warning assertions.)

- [ ] **Step 2: Run to verify failure** — `cargo test -p ab-aozora-aat spans_are_decoded` → FAIL (all lines 1, sanitized offsets).

- [ ] **Step 3: Implement**

1. `DecodedSource` gains a span context:

```rust
pub struct DecodedSource {
    pub text: String,
    pub span_text: String,
    pub encoding: &'static str,
    pub source_hash: String,
    /// Sanitize offset maps + body offset + line index for span rebasing
    /// (ADR 0024: offsets against the full decoded source, real lines).
    span_ctx: SpanContext,
}

#[derive(Debug)]
struct SpanContext {
    maps: SanitizeMaps,
    /// Byte offset of the body slice within the SANITIZED text.
    body_offset: usize,
    /// Byte offsets of line starts in the DECODED text (`text`).
    line_starts: Vec<usize>,
}

impl SpanContext {
    fn to_decoded(&self, body_offset: usize) -> usize {
        self.maps.to_source_offset(body_offset + self.body_offset)
    }
    fn to_decoded_end(&self, body_end: usize) -> usize {
        self.maps.to_source_end(body_end + self.body_offset)
    }
    fn line_of(&self, decoded_offset: usize) -> u64 {
        (self.line_starts.partition_point(|&s| s <= decoded_offset)) as u64
    }
}
```

2. `sanitize_for_aat` → returns the context too:

```rust
fn sanitize_for_aat(text: &str) -> (String, SanitizeMaps, usize) {
    let mapped = sanitize_mapped(text);
    let sanitized = mapped.text.into_owned();
    let body = aozora_body_range(&sanitized);
    (sanitized[body.clone()].to_owned(), mapped.maps, body.start)
}
```

Refactor `aozora_body_text(source: &str) -> &str` into
`aozora_body_range(source: &str) -> std::ops::Range<usize>` (it already
computes `body_start`/`body_end` internally — return them instead of
slicing; keep a thin `aozora_body_text` wrapper only if other callers
remain). Build `line_starts` in `decode_source_bytes` after `text` is
final:

```rust
fn line_starts(text: &str) -> Vec<usize> {
    std::iter::once(0)
        .chain(text.match_indices('\n').map(|(i, _)| i + 1))
        .collect()
}
```

Update the three `decode_source_bytes` arms to build the full
`DecodedSource` with `span_ctx`.

3. `span_json` takes the context (update its ~6 call sites to pass
   `&decoded.span_ctx`, threading `decoded` where a call site lacks it —
   `inline_content` already has it):

```rust
fn span_json(span: &Span, ctx: &SpanContext) -> Value {
    let byte_start = ctx.to_decoded(span.start);
    let byte_end = ctx.to_decoded_end(span.end);
    let line_start = ctx.line_of(byte_start);
    let line_end = ctx.line_of(if byte_end > byte_start { byte_end - 1 } else { byte_start });
    json!({
        "line_start": line_start,
        "line_end": line_end,
        "byte_start": byte_start,
        "byte_end": byte_end
    })
}
```

4. `diagnostic_warning(diagnostic, ctx)`: replace the synthesized
   `1_u64` with
   `ctx.line_of(ctx.to_decoded(span.start))` when a span exists.
5. Delete the `warnings.push(json!({ "message": "aozora upstream spans …", "line": 1 }))`
   block in `build_aat` (lines ~232–237) entirely.
6. `diagnostics_json_from_bytes`: after building the envelope data,
   rebase each entry's wire span in place:

```rust
    let mut data = serde_json::to_value(entries)?;
    if let Some(items) = data.as_array_mut() {
        for entry in items {
            let (Some(start), Some(end)) = (entry["span"]["start"].as_u64(), entry["span"]["end"].as_u64()) else { continue };
            entry["span"]["start"] = json!(decoded.span_ctx.to_decoded(start as usize));
            entry["span"]["end"] = json!(decoded.span_ctx.to_decoded_end(end as usize));
        }
    }
```

7. Versions: `ab-aozora-aat` and `ab-aozora` → `0.3.0`; wire.rs field
   list → `"ab-aozora 0.3.0"`. Tripwire literal: version substring →
   `ab-aozora 0.3.0 aat-schema 1 facade 0.2.0 wire-schema 3 (git unknown)`;
   the `あ\n` span stays `{"byte_end":4,"byte_start":0,"line_end":1,"line_start":1}`
   (single-line input — verify, don't assume: run the test, read the
   panic output, and confirm the span bytes are UNCHANGED before pasting;
   if they changed, the composition has a bug).

- [ ] **Step 4: Run**

```bash
cargo test --workspace
bash tests/workspace-no-preserve-order.sh
```
Expected: green (env-gated reference_parity skips without env vars — do
NOT set them; the frozen-adapter comparison is retired next task).

- [ ] **Step 5: Commit**

```bash
git add crates/ab-aozora-aat crates/ab-aozora Cargo.lock
git commit -m "feat(ab-aozora-aat): decoded-source spans with real line coordinates (rotation B)"
```

---

### Task 15: Hand-verified goldens replace reference_parity

**Files:**
- Delete: `crates/ab-aozora-aat/tests/reference_parity.rs`
- Create: `crates/ab-aozora-aat/tests/goldens.rs`
- Create: `crates/ab-aozora-aat/tests/goldens/<sample>.expected.json` (one per existing `tests/data/` sample, 4 files)
- Create: `reports/aat-fidelity/verify-golden-spans.py` (verification helper, committed)

**Interfaces:**
- Consumes: `tests/data/*` (4 samples incl. real SJIS), `aat_json_from_bytes`.
- Produces: an always-on, hermetic golden test (`cargo test` — no env
  gating; goldens embed `(git unknown)`); the byte-parity-vs-frozen-adapter
  claim formally ends here (spans diverged by design at rotation B).

- [ ] **Step 1: Write the verification helper**

```python
#!/usr/bin/env python3
"""Check golden AAT spans against the decoded source (independent oracle).

For every node with both `value` and `span`, assert
decoded[byte_start:byte_end] == value, and that line_start equals
1 + count of '\\n' in decoded[:byte_start]. Prints one line per checked
span; exit 1 on any mismatch.

Usage: verify-golden-spans.py SOURCE_FILE GOLDEN_JSON
"""
import json
import sys

src = open(sys.argv[1], "rb").read()
try:
    decoded = src.decode("utf-8-sig") if src.startswith(b"\xef\xbb\xbf") else src.decode("utf-8")
except UnicodeDecodeError:
    decoded = src.decode("shift_jis", errors="replace")
data = decoded.encode("utf-8")
doc = json.load(open(sys.argv[2]))
failures = 0


def walk(node):
    global failures
    if isinstance(node, dict):
        span, value = node.get("span"), node.get("value")
        if isinstance(span, dict) and isinstance(value, str):
            got = data[span["byte_start"]:span["byte_end"]].decode("utf-8", "replace")
            line = 1 + data[:span["byte_start"]].decode("utf-8", "replace").count("\n")
            ok_v = got == value
            ok_l = line == span["line_start"]
            print(f"{'OK ' if ok_v and ok_l else 'FAIL'} span={span} value={value!r} slice={got!r} line={line}")
            failures += 0 if ok_v and ok_l else 1
        for v in node.values():
            walk(v)
    elif isinstance(node, list):
        for v in node:
            walk(v)


walk(doc["blocks"])
sys.exit(1 if failures else 0)
```

Note the deliberate limitation: value-bearing nodes whose `value` differs
from the raw source slice (e.g. ruby projections) will print FAIL — for
those, the human verifies by eye and records the exceptions in the test
file's doc comment. The helper is an aid, not the authority; the HUMAN
check is the acceptance step.

- [ ] **Step 2: Generate candidates, verify by hand**

```bash
for f in crates/ab-aozora-aat/tests/data/*; do
  cargo run -p ab-aozora --release -- --mode aat < "$f" > "crates/ab-aozora-aat/tests/goldens/$(basename "$f").expected.json"
  python3 reports/aat-fidelity/verify-golden-spans.py "$f" "crates/ab-aozora-aat/tests/goldens/$(basename "$f").expected.json"
done
```

For EACH sample: read the helper output; for every FAIL line decide
whether it is a projection (value ≠ slice by design — document it) or a
span bug (stop, fix Task 14, regenerate). Additionally spot-check ≥3
line numbers per sample against the actual file (`awk 'NR==<n>'`), incl.
a line >1 in the SJIS sample. This is the spec's hand-verification gate —
do not skip it.

- [ ] **Step 3: Write the golden test**

```rust
// crates/ab-aozora-aat/tests/goldens.rs
//! Committed, HAND-VERIFIED goldens (Phase 3 rotation B). These replaced
//! tests/reference_parity.rs: the frozen-adapter byte-parity claim ended
//! at the span rotation by design (legacy adapter emits sanitized-offset
//! spans with synthesized lines). Spans in these files were verified
//! against the decoded sources with reports/aat-fidelity/
//! verify-golden-spans.py plus manual line-number checks — see the
//! rotation B confinement report. Hermetic: plain `cargo test` builds
//! carry `(git unknown)`, which these goldens embed.
use std::fs;

#[test]
fn aat_output_matches_hand_verified_goldens() {
    let data = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/data");
    for entry in fs::read_dir(data).unwrap() {
        let path = entry.unwrap().path();
        let name = path.file_name().unwrap().to_str().unwrap();
        let golden = format!("{}/tests/goldens/{name}.expected.json",
                             env!("CARGO_MANIFEST_DIR"));
        let expected = fs::read(&golden).unwrap();
        let actual = ab_aozora_aat::aat_json_from_bytes(&fs::read(&path).unwrap()).unwrap();
        assert_eq!(actual, expected, "golden drift: {name}");
    }
}
```

Delete `tests/reference_parity.rs`.

- [ ] **Step 4: Run** — `cargo test -p ab-aozora-aat --test goldens` → PASS; `cargo test --workspace` green.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-aozora-aat/tests reports/aat-fidelity/verify-golden-spans.py
git rm crates/ab-aozora-aat/tests/reference_parity.rs 2>/dev/null || true
git commit -m "test(ab-aozora-aat): hand-verified span goldens replace frozen-adapter parity"
```

---

### Task 16: Span-deviation manifest + scorer support

**Files:**
- Create: `reports/parser-conformance/span-deviation-manifest.json`
- Modify: `reports/parser-conformance/run-aozora-notation-spec.py` (`--span-deviation-manifest`)
- Test: extend `reports/parser-conformance/tests/test_diagnostics_scoring.py`

**Interfaces:**
- Consumes: rotation-B local build (`cargo build -p ab-aozora --release`), both vector suites.
- Produces: manifest schema (list of entries):

```json
{
  "vector": "<name>",
  "reason": "<which sanitize step shifts the offsets>",
  "original_expected": [ {"code": "…", "severity": "…", "span": {"start": 0, "end": 0}} ],
  "expected": [ {"code": "…", "severity": "…", "span": {"start": 0, "end": 0}} ],
  "source_sha256": "<hex sha256 of the vector source utf-8 bytes>"
}
```

`expected` spans are decoded-source offsets **derived by hand** from the
vector source and ADR 0024 — NEVER computed by fork code. The scorer uses
`expected` verbatim for listed vectors; unlisted divergence, hash
mismatch, or span mismatch fails. This file must be committed and
reviewed BEFORE Task 17's gate scoring run.

- [ ] **Step 1: Write the failing scorer tests** (append to the Task 6 test file)

```python
def manifest_file(tmp_path, entries):
    p = tmp_path / "manifest.json"
    p.write_text(json.dumps(entries))
    return p


def test_manifest_overrides_expected_for_listed_vector(tmp_path):
    import hashlib
    sha = hashlib.sha256(b"s").hexdigest()
    decoded_want = [{"code": "unclosed-bracket", "severity": "error",
                     "span": {"start": 3, "end": 6}}]
    entry = dict(FULL, span={"start": 3, "end": 6})
    adapter = fake_diag_adapter(tmp_path, [entry])
    manifest = scorer.load_span_deviation_manifest(manifest_file(tmp_path, [
        {"vector": "v", "reason": "crlf", "original_expected": WANT,
         "expected": decoded_want, "source_sha256": sha}]))
    row = scorer.evaluate(adapter, vector(WANT), manifest=manifest)
    assert row.status == "pass", (row.failures,)


def test_manifest_hash_mismatch_fails(tmp_path):
    manifest = scorer.load_span_deviation_manifest(manifest_file(tmp_path, [
        {"vector": "v", "reason": "crlf", "original_expected": WANT,
         "expected": WANT, "source_sha256": "0" * 64}]))
    row = scorer.evaluate(fake_diag_adapter(tmp_path, [FULL]), vector(WANT),
                          manifest=manifest)
    assert row.status == "fail"
    assert any("manifest" in f for f in row.failures)


def test_unlisted_divergence_still_fails(tmp_path):
    shifted = dict(FULL, span={"start": 3, "end": 6})
    row = scorer.evaluate(fake_diag_adapter(tmp_path, [shifted]), vector(WANT),
                          manifest={})
    assert row.status == "fail"
```

- [ ] **Step 2: Run to verify failure** — the three new tests FAIL (`load_span_deviation_manifest` / `manifest=` unknown).

- [ ] **Step 3: Implement in the scorer**

```python
def load_span_deviation_manifest(path) -> dict:
    entries = json.loads(Path(path).read_text(encoding="utf-8"))
    manifest = {}
    for e in entries:
        for key in ("vector", "reason", "original_expected", "expected", "source_sha256"):
            if key not in e:
                raise SystemExit(f"span-deviation-manifest entry missing {key!r}: {e}")
        manifest[e["vector"]] = e
    return manifest
```

`evaluate` grows a keyword arg (`def evaluate(adapter, vector, manifest=None)`;
add `import hashlib` at the top of the file). The diagnostics-command
branch becomes:

```python
            else:
                scored += 1
                entry = (manifest or {}).get(vector["name"])
                bad_manifest = False
                if entry is not None:
                    digest = hashlib.sha256(vector["source"].encode("utf-8")).hexdigest()
                    if digest != entry["source_sha256"]:
                        failures.append("diagnostics: manifest source hash mismatch "
                                        "(vector changed since authorization)")
                        bad_manifest = True
                    else:
                        want_diag = entry["expected"]
                if not bad_manifest:
                    actual_diag, error = run_diagnostics(adapter, vector["source"])
                    if error:
                        failures.append(f"diagnostics: {error}")
                    elif actual_diag != want_diag:
                        failures.append(f"diagnostics: expected {want_diag!r}, got {actual_diag!r}")
```

`main()` gains
`ap.add_argument("--span-deviation-manifest")`, loads it once, and passes
it into every `evaluate` call; the justfile ab-aozora lane gains
`--span-deviation-manifest "{{repo_root}}/reports/parser-conformance/span-deviation-manifest.json"`.

- [ ] **Step 4: Run** — full conformance pytest dir green.

- [ ] **Step 5: Identify deviating vectors and author the manifest**

```bash
export RUSTC_WRAPPER= SCCACHE_DISABLE=1 && cargo build -p ab-aozora --release
printf '[]' > reports/parser-conformance/span-deviation-manifest.json
just aozora-notation-spec-comparison REPORT_MD=scratch/rotb-dryrun.md SUMMARY_JSON=scratch/rotb-dryrun.summary.json
just official-docs-seed-comparison REPORT_MD=scratch/rotb-dryrun-seed.md SUMMARY_JSON=scratch/rotb-dryrun-seed.summary.json
python3 - <<'EOF'
import json
for f in ("scratch/rotb-dryrun.summary.json", "scratch/rotb-dryrun-seed.summary.json"):
    for r in json.load(open(f))["rows"]:
        if r["adapter"] == "ab-aozora" and any("diagnostics" in x for x in r["failures"] + r["warnings"]):
            print(f, r["vector"], r["level"], r["status"])
EOF
```

For EACH listed vector: open its `vector.json`, hand-derive the
decoded-source span (count bytes in the source string per ADR 0024;
BOM/CRLF/accent content is what shifts offsets), write the manifest entry
with `sha256(source)`. If the dry run lists NO deviating vectors, the
manifest stays `[]` — that is a valid, committed result. Do NOT use
`ab-aozora` output to fill in `expected`.

- [ ] **Step 6: Commit (manifest authorization commit — before the gate)**

```bash
git add reports/parser-conformance/span-deviation-manifest.json reports/parser-conformance/run-aozora-notation-spec.py reports/parser-conformance/tests/test_diagnostics_scoring.py justfile
git commit -m "feat(conformance): pre-committed span-deviation manifest with hand-verified decoded spans"
```

---

### Task 17: Rotation B gate — span confinement, conformance, perf, conversion audit (hinoki)

**Files:**
- Create: `docs/superpowers/reports/2026-07-11-phase3-span-confinement.{md,summary.json}`
- Create: `docs/superpowers/reports/2026-07-11-phase3-span-conformance.{md,summary.json}` (+ seed variants) and `…-span-conformance-gate.summary.json`
- Create: `docs/superpowers/reports/2026-07-11-phase3-span-perf.{md,summary.json,runner.json}`
- Create: `docs/superpowers/reports/2026-07-11-ab-aozora-phase3-span-conversion-audit.{md,summary.json}`

**Interfaces:**
- Consumes: Tasks 13–16 merged; `C2` = ledger-recorded HEAD after Task 16; rotation-A dump; `audit-aat-delta.py span-confinement`; the committed manifest.
- Produces: three PASS gate summaries (`stage: "rotation-b"`, gates `confinement` / `conformance` / `perf`) + conversion-audit evidence for Task 18's row; dump `/db/ab-validator/aat-corpus/ab-aozora-phase3-span-<C2:0:7>` (retained — the expected Phase 4 baseline).

- [ ] **Step 1: Record C2, push** — `CANDIDATE=$(git rev-parse HEAD)`; ledger: `Phase 3 C2 = <sha>`.

- [ ] **Step 2: Detached build on hinoki** — Task 3 Step 2 commands. Expected version: `ab-aozora 0.3.0 aat-schema 1 facade 0.2.0 wire-schema 3 (git $CANDIDATE)`. Record `bin_sha256`.

- [ ] **Step 3: Full-corpus run** — `--report-id phase3-span --out-dir /db/ab-validator/aat-corpus/ab-aozora-phase3-span-<C2:0:7>`. Expected: 17886 files.

- [ ] **Step 4: Span-confinement audit vs rotation A**

```bash
ssh hinoki.hyakutake-barbel.ts.net 'cd ~/Projects/soranoha/.worktrees/parser-fork-phase3/ab-validator && python3 reports/aat-fidelity/audit-aat-delta.py span-confinement /db/ab-validator/aat-corpus/ab-aozora-phase3-capability-<C1:0:7>/aat /db/ab-validator/aat-corpus/ab-aozora-phase3-span-<C2:0:7>/aat --summary-json ~/phase3-span-confinement.json; echo "exit=$?"'
```

Expected: `exit=0`, all 17886 in `span_confined`/`identical`. Any exit 2
is a real leak (node structure, values, or invalid spans moved) — fix,
new C2, re-run the stage.

- [ ] **Step 5: Conformance with the manifest**

Same two `just` invocations as Task 10 Step 5 with `phase3-span-…` report
names (the justfile lane now passes the manifest). Re-run the 25/25 must
assert (same inline python, pointed at the span conformance summary).
Then `compare-adapter-rows.py` against the rotation-A summaries — expected
differences: ONLY vectors listed in the manifest (cross-check
programmatically: the differing vector set must be a subset of the
manifest's vector set; empty manifest → `differing=0`).

- [ ] **Step 6: Perf** — Task 3 Step 6 with `phase3-span-perf.*` names. The line index and map queries run per span — watch the median; >10% blocks.

- [ ] **Step 7: Conversion audit** — Task 10 Step 7 with the span dump and `phase3-span-audit.*` names. Expected 17886/17886/0. (Real spans may shift divergence-record details; `files_failed` must stay 0.)

- [ ] **Step 8: Evidence + freeze** — copy down artifacts, write the three gate summaries with the filenames Task 18 reads verbatim: `2026-07-11-phase3-span-confinement.summary.json` (gate `confinement`), `2026-07-11-phase3-span-conformance-gate.summary.json` (gate `conformance`, details incl. `must_fail: 0`, `must_skip: 0`, manifest vector list), `2026-07-11-phase3-span-perf.summary.json` (gate `perf`, details incl. `new_timeouts: 0`); all `stage: "rotation-b"`, shared candidate triple. The confinement narrative report lists every manifest vector with before/after offsets ("the report documents, the manifest authorizes").

```bash
git add docs/superpowers/reports/2026-07-11-phase3-span-* docs/superpowers/reports/2026-07-11-ab-aozora-phase3-span-*
git commit -m "docs(reports): phase 3 rotation B gate evidence — span semantics PASS"
```

Ledger: `Rotation B gates PASS (C2=<sha>, bin=<sha256>)`. The stage-0 dump
may now be scheduled for deletion (after Task 18's checkpoint passes).

---

### Task 18: Rotation B registry row, phase checkpoint verifier, closure docs

**Files:**
- Modify: `abc/data/aat-parser-ir-compatibility.edn` (append the 0.3.0 row)
- Create: `reports/aat-fidelity/verify-phase3-checkpoint.py`
- Test: `reports/aat-fidelity/tests/test_verify_phase3_checkpoint.py`
- Modify: `docs/handoffs/2026-07-10-parser-fork-provenance.md` (close the "Phase 3 follow-ups" section)

**Interfaces:**
- Consumes: the nine frozen gate summaries (3 stages × 3 gates), C0/C1/C2 from the ledger, Task 17's audit artifacts.
- Produces: `verify-phase3-checkpoint.py --stage0 P C E --rotation-a D C P --rotation-b D C P --c0 SHA --c1 SHA --c2 SHA` → `CHECKPOINT OK` / exit 1. The branch merges only after CHECKPOINT OK.

- [ ] **Step 1: Registry row** — exactly Task 11's procedure with Task 17's audit artifacts and the C2 version string; `:corpus` = `"ab-aozora Phase 3 rotation B dump (aozora-full corpus, 17886 works)"`. Validate (`adr-governance` + kaocha) and commit.

- [ ] **Step 2: Write the failing verifier tests**

```python
# reports/aat-fidelity/tests/test_verify_phase3_checkpoint.py
import json
import subprocess
import sys
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[1] / "verify-phase3-checkpoint.py"
C0, C1, C2 = "a" * 40, "b" * 40, "c" * 40
V0 = f"ab-aozora 0.1.0 aat-schema 1 facade 0.1.0 wire-schema 2 (git {C0})"
V1 = f"ab-aozora 0.2.0 aat-schema 1 facade 0.2.0 wire-schema 3 (git {C1})"
V2 = f"ab-aozora 0.3.0 aat-schema 1 facade 0.2.0 wire-schema 3 (git {C2})"


def summary(stage, gate, commit, version, bin_sha="f" * 64, verdict="PASS", details=None):
    return {"stage": stage, "gate": gate, "verdict": verdict,
            "candidate": {"commit": commit, "bin_sha256": bin_sha, "version": version},
            "details": details or {}}


def write_all(tmp_path, mutate=None):
    docs = {
        "s0p": summary("stage0", "parity", C0, V0, bin_sha="0" * 64,
                       details={"compared": 17886, "missing_count": 0,
                                "bytes": {"diverged_count": 0}}),
        "s0c": summary("stage0", "conformance", C0, V0, bin_sha="0" * 64),
        "s0f": summary("stage0", "perf", C0, V0, bin_sha="0" * 64,
                       details={"new_timeouts": 0}),
        "rap": summary("rotation-a", "delta", C1, V1, bin_sha="1" * 64,
                       details={"verdict": "PASS"}),
        "rac": summary("rotation-a", "conformance", C1, V1, bin_sha="1" * 64,
                       details={"must_fail": 0, "must_skip": 0}),
        "raf": summary("rotation-a", "perf", C1, V1, bin_sha="1" * 64,
                       details={"new_timeouts": 0}),
        "rbp": summary("rotation-b", "confinement", C2, V2, bin_sha="2" * 64),
        "rbc": summary("rotation-b", "conformance", C2, V2, bin_sha="2" * 64,
                       details={"must_fail": 0, "must_skip": 0}),
        "rbf": summary("rotation-b", "perf", C2, V2, bin_sha="2" * 64,
                       details={"new_timeouts": 0}),
    }
    if mutate:
        mutate(docs)
    paths = {}
    for key, doc in docs.items():
        p = tmp_path / f"{key}.json"
        p.write_text(json.dumps(doc))
        paths[key] = str(p)
    return paths


def run(paths):
    return subprocess.run(
        [sys.executable, str(SCRIPT),
         "--stage0", paths["s0p"], paths["s0c"], paths["s0f"],
         "--rotation-a", paths["rap"], paths["rac"], paths["raf"],
         "--rotation-b", paths["rbp"], paths["rbc"], paths["rbf"],
         "--c0", C0, "--c1", C1, "--c2", C2],
        capture_output=True, text=True)


def test_all_pass(tmp_path):
    p = run(write_all(tmp_path))
    assert p.returncode == 0 and "CHECKPOINT OK" in p.stdout


def test_verdict_fail_rejected(tmp_path):
    def mutate(d): d["rap"]["verdict"] = "FAIL"
    assert run(write_all(tmp_path, mutate)).returncode == 1


def test_commit_mismatch_within_stage_rejected(tmp_path):
    def mutate(d): d["rac"]["candidate"]["commit"] = C2
    assert run(write_all(tmp_path, mutate)).returncode == 1


def test_bin_mismatch_within_stage_rejected(tmp_path):
    def mutate(d): d["raf"]["candidate"]["bin_sha256"] = "9" * 64
    assert run(write_all(tmp_path, mutate)).returncode == 1


def test_wrong_version_pattern_rejected(tmp_path):
    def mutate(d): d["rbp"]["candidate"]["version"] = V1.replace(C1, C2)
    assert run(write_all(tmp_path, mutate)).returncode == 1


def test_duplicate_candidates_across_stages_rejected(tmp_path):
    def mutate(d):
        for k in ("rbp", "rbc", "rbf"):
            d[k]["candidate"]["commit"] = C1
            d[k]["candidate"]["version"] = V2.replace(C2, C1)
    paths = write_all(tmp_path, mutate)
    proc = subprocess.run(
        [sys.executable, str(SCRIPT),
         "--stage0", paths["s0p"], paths["s0c"], paths["s0f"],
         "--rotation-a", paths["rap"], paths["rac"], paths["raf"],
         "--rotation-b", paths["rbp"], paths["rbc"], paths["rbf"],
         "--c0", C0, "--c1", C1, "--c2", C1],
        capture_output=True, text=True)
    assert proc.returncode == 1
```

- [ ] **Step 3: Implement the verifier**

```python
#!/usr/bin/env python3
"""Fail-closed Phase 3 checkpoint: nine PASS gate summaries, three candidates.

Per stage (stage0 / rotation-a / rotation-b): the three summaries carry
the expected stage + gate names, verdict PASS, ONE candidate commit and
ONE bin_sha256, and a version string matching the stage's exact pattern
and embedding the commit. Across stages: the three candidate commits
equal --c0/--c1/--c2 and are pairwise distinct. Detail minimums: stage0
parity compared>0/missing 0/bytes diverged 0; rotation conformance
must_fail==0 and must_skip==0; every perf new_timeouts==0.

Exit 0 + "CHECKPOINT OK" or exit 1 with the first violation."""
import argparse
import json
import re
import sys

STAGES = {
    "stage0": (("parity", "conformance", "perf"),
               r"^ab-aozora 0\.1\.0 aat-schema 1 facade 0\.1\.0 wire-schema 2 \(git {c}\)$"),
    "rotation-a": (("delta", "conformance", "perf"),
                   r"^ab-aozora 0\.2\.0 aat-schema 1 facade 0\.2\.0 wire-schema 3 \(git {c}\)$"),
    "rotation-b": (("confinement", "conformance", "perf"),
                   r"^ab-aozora 0\.3\.0 aat-schema 1 facade 0\.2\.0 wire-schema 3 \(git {c}\)$"),
}


def die(msg):
    print(f"CHECKPOINT FAIL: {msg}", file=sys.stderr)
    raise SystemExit(1)


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--stage0", nargs=3, required=True)
    ap.add_argument("--rotation-a", dest="rotation_a", nargs=3, required=True)
    ap.add_argument("--rotation-b", dest="rotation_b", nargs=3, required=True)
    ap.add_argument("--c0", required=True)
    ap.add_argument("--c1", required=True)
    ap.add_argument("--c2", required=True)
    args = ap.parse_args()
    expected_commits = {"stage0": args.c0, "rotation-a": args.c1, "rotation-b": args.c2}
    for sha in expected_commits.values():
        if not re.fullmatch(r"[0-9a-f]{40}", sha):
            die(f"candidate commit must be 40-hex: {sha!r}")
    if len(set(expected_commits.values())) != 3:
        die("candidate commits must be pairwise distinct")
    files = {"stage0": args.stage0, "rotation-a": args.rotation_a,
             "rotation-b": args.rotation_b}
    for stage, (gates, version_pat) in STAGES.items():
        commit = expected_commits[stage]
        bins, versions = set(), set()
        for path, gate in zip(files[stage], gates):
            try:
                doc = json.load(open(path))
            except (OSError, json.JSONDecodeError) as err:
                die(f"{path}: unreadable ({err})")
            if doc.get("stage") != stage:
                die(f"{path}: stage is {doc.get('stage')!r}, expected {stage!r}")
            if doc.get("gate") != gate:
                die(f"{path}: gate is {doc.get('gate')!r}, expected {gate!r}")
            if doc.get("verdict") != "PASS":
                die(f"{path}: verdict is {doc.get('verdict')!r}")
            cand = doc.get("candidate") or {}
            if cand.get("commit") != commit:
                die(f"{path}: commit {cand.get('commit')!r} != {commit}")
            if not re.fullmatch(version_pat.format(c=commit), cand.get("version") or ""):
                die(f"{path}: version {cand.get('version')!r} fails the {stage} pattern")
            bins.add(cand.get("bin_sha256"))
            versions.add(cand.get("version"))
            details = doc.get("details") or {}
            if gate == "parity":
                if not (details.get("compared", 0) > 0 and details.get("missing_count") == 0
                        and (details.get("bytes") or {}).get("diverged_count") == 0):
                    die(f"{path}: parity details fail minimums: {details}")
            if gate == "conformance" and stage != "stage0":
                if details.get("must_fail") != 0 or details.get("must_skip") != 0:
                    die(f"{path}: conformance must gate not clean: {details}")
            if gate == "perf" and details.get("new_timeouts") != 0:
                die(f"{path}: perf new_timeouts != 0: {details}")
        if len(bins) != 1 or None in bins:
            die(f"{stage}: bin_sha256 not identical across gates: {bins}")
        if len(versions) != 1:
            die(f"{stage}: version strings differ: {versions}")
    print("CHECKPOINT OK")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
```

- [ ] **Step 4: Run the tests** — `python -m pytest reports/aat-fidelity/tests/test_verify_phase3_checkpoint.py -v` → 6 passed. Ensure the three rotation gate summaries written in Tasks 10/17 include `details.must_fail`/`details.must_skip`/`details.new_timeouts` fields the verifier reads — if a written summary lacks one, that summary is WRONG (not the verifier); fix it before freezing (they are only frozen after checkpoint).

- [ ] **Step 5: Run the real checkpoint**

```bash
python3 reports/aat-fidelity/verify-phase3-checkpoint.py \
  --stage0 docs/superpowers/reports/2026-07-11-phase3-stage0-sanitize-parity.summary.json docs/superpowers/reports/2026-07-11-phase3-stage0-conformance-gate.summary.json docs/superpowers/reports/2026-07-11-phase3-stage0-perf.summary.json \
  --rotation-a docs/superpowers/reports/2026-07-11-phase3-capability-delta.summary.json docs/superpowers/reports/2026-07-11-phase3-capability-conformance-gate.summary.json docs/superpowers/reports/2026-07-11-phase3-capability-perf.summary.json \
  --rotation-b docs/superpowers/reports/2026-07-11-phase3-span-confinement.summary.json docs/superpowers/reports/2026-07-11-phase3-span-conformance-gate.summary.json docs/superpowers/reports/2026-07-11-phase3-span-perf.summary.json \
  --c0 <C0> --c1 <C1> --c2 <C2>
```

Expected: `CHECKPOINT OK`.

- [ ] **Step 6: Close the provenance handoff**

In `docs/handoffs/2026-07-10-parser-fork-provenance.md`, under "Phase 3
follow-ups", append a dated closure note: the `aozora-pipeline =0.4.1`
dependency is retired (stage-0 byte-parity evidence path), diagnostics
codes + classifiers landed (rotation A), span semantics landed
(rotation B, ADR 0024), the upstream-first obligation is closed per
ADR 0032's replacement clause, and jizume AAT surfacing + schema/warning
enrichment remain Phase 4 items. Cite the three evidence report sets and
the two registry rows. Do not rewrite existing sections.

- [ ] **Step 7: Commit + final ledger**

```bash
git add abc/data/aat-parser-ir-compatibility.edn reports/aat-fidelity/verify-phase3-checkpoint.py reports/aat-fidelity/tests/test_verify_phase3_checkpoint.py docs/handoffs/2026-07-10-parser-fork-provenance.md
git commit -m "feat(abc): rotation B registry row; phase 3 checkpoint verifier CHECKPOINT OK"
```

Ledger: `Phase 3 CHECKPOINT OK (C0=…, C1=…, C2=…)`. The plan is complete;
hand off to superpowers:finishing-a-development-branch.
