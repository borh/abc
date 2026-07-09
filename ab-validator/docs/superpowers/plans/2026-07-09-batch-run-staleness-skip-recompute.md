# Batch Run Staleness &amp; Skip-Recompute Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Let the two expensive Soranoha batch runs (AAT dump generation, morph-warehouse) skip recomputation when inputs are unchanged and detect staleness the instant an input changes, via one content-based input-set identity model.

**Architecture:** A shared Python identity library reduces a run's full input set to one `input_set_hash` (content-based, fail-toward-correctness). Each batch's runner resolves that identity, looks for a valid output already carrying it, and skips or computes. Outputs stay content-addressed in place on `/db` (nix-derivation caching was rejected — store full). Design: `../specs/2026-07-09-batch-run-staleness-skip-recompute-design.md`.

**Tech Stack:** Python 3 (`reports/lib/`, stdlib + existing helpers), Rust (`crates/ab-morph-run`), bash (`reports/aat-fidelity/run-aat-full.sh`), pytest (unittest-style), nix flake checks.

## Global Constraints

- **Content, never mtime.** Every identity input is a content hash or a nix store-path/`narHash`; no timestamps or wall-clock in identity.
- **Fail toward correctness.** Include every output-determining input in `input_set_hash`; when unsure, include it (spurious recompute is slow, never wrong). A missing/unhashable input **fails closed** (refuse), never reads as "no change".
- **Additive & behavior-preserving.** Skip logic is purely additive: `--force`, or any identity miss, computes exactly as today. Any bug/semantic fix discovered while wiring identity is **filed separately, not folded in**.
- **Reuse, don't reinvent.** Use `reports/lib/hashing.py` (`sha256_hex`, `file_sha256`) and `reports/lib/aat_hash.py` (`hash_aat_dir`); mirror the `fidelity-lock/v1` resolve→compute→lock conventions. Do not add a parallel identity system.
- **Hash format is `sha256:<hex>`** everywhere (matches `hashing.py` / `aat_hash.py`).
- **Tests are `unittest.TestCase` in `reports/lib/tests/`**, run under `python -m pytest`, importing the module under test via `sys.path.insert(0, str(Path(__file__).resolve().parents[1]))` (match `test_aat_hash.py`).

## File Structure

- `reports/lib/run_identity.py` (NEW) — the shared identity library: `canonical_json`, `input_set_hash`, `INPUT_SET_IDENTITY_VERSION`. Pure.
- `reports/lib/tests/test_run_identity.py` (NEW) — determinism / order-independence / sensitivity / fail-closed.
- *(Slice 2)* `crates/ab-morph-run/…` — warehouse identity object, `run-manifest.json` sidecar, `input_set_hash` column, `by-input/<hash>` index, `--force`, skip-on-match.
- *(Slice 3)* `reports/aat-fidelity/run-aat-full.sh` + `metadata.json` descriptor — record input+output identity; skip regeneration on verifying match.
- *(Slice 4)* a fixture-based nix flake check / smoke asserting skip-on-unchanged and recompute-on-mutated.

---

## Slice 1 — Shared identity library (execution-ready)

### Task 1: `run_identity` — input-set hash

**Files:**
- Create: `ab-validator/reports/lib/run_identity.py`
- Test: `ab-validator/reports/lib/tests/test_run_identity.py`

**Interfaces:**
- Consumes: nothing (stdlib only).
- Produces:
  - `INPUT_SET_IDENTITY_VERSION: str` — identity-format stamp.
  - `canonical_json(value) -> str` — deterministic JSON (sorted keys, compact, UTF-8, no NaN); raises `TypeError`/`ValueError` on non-serializable input.
  - `input_set_hash(identity_object: dict) -> str` — `"sha256:<hex>"` over `canonical_json` of `identity_object` with `identity_version` folded in (stamp wins). Raises `TypeError` if `identity_object` is not a dict or contains a non-serializable value.

- [ ] **Step 1: Write the failing test**

Create `ab-validator/reports/lib/tests/test_run_identity.py`:

```python
"""Tests for the batch-run input-set identity hasher."""

from __future__ import annotations

import sys
import unittest
from pathlib import Path

_LIB = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(_LIB))

import run_identity  # noqa: E402


class InputSetHash(unittest.TestCase):
    def test_deterministic_and_prefixed(self) -> None:
        obj = {"a": 1, "corpus": "sha256:aa", "profile": "full"}
        h1 = run_identity.input_set_hash(obj)
        h2 = run_identity.input_set_hash(dict(obj))
        self.assertEqual(h1, h2)
        self.assertTrue(h1.startswith("sha256:"))
        self.assertEqual(len(h1), len("sha256:") + 64)

    def test_key_order_independent(self) -> None:
        a = run_identity.input_set_hash({"x": 1, "y": 2})
        b = run_identity.input_set_hash({"y": 2, "x": 1})
        self.assertEqual(a, b)

    def test_value_change_changes_hash(self) -> None:
        base = run_identity.input_set_hash({"corpus": "sha256:aa"})
        changed = run_identity.input_set_hash({"corpus": "sha256:bb"})
        self.assertNotEqual(base, changed)

    def test_omitting_an_input_changes_hash(self) -> None:
        # Narrowing identity must be detectable, not silent.
        full = run_identity.input_set_hash({"corpus": "sha256:aa", "dict": "sha256:bb"})
        narrowed = run_identity.input_set_hash({"corpus": "sha256:aa"})
        self.assertNotEqual(full, narrowed)

    def test_identity_version_is_folded_in(self) -> None:
        obj = {"corpus": "sha256:aa"}
        h = run_identity.input_set_hash(obj)
        # A caller-supplied identity_version must not override the module stamp.
        spoofed = run_identity.input_set_hash({**obj, "identity_version": "evil"})
        self.assertEqual(h, spoofed)

    def test_non_serializable_fails_closed(self) -> None:
        with self.assertRaises((TypeError, ValueError)):
            run_identity.input_set_hash({"bad": {1, 2, 3}})  # a set is not JSON

    def test_non_dict_fails_closed(self) -> None:
        with self.assertRaises(TypeError):
            run_identity.input_set_hash(["not", "a", "dict"])


if __name__ == "__main__":
    unittest.main()
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cd ab-validator && python -m pytest reports/lib/tests/test_run_identity.py -v`
Expected: FAIL — `ModuleNotFoundError: No module named 'run_identity'`.

- [ ] **Step 3: Write minimal implementation**

Create `ab-validator/reports/lib/run_identity.py`:

```python
"""Content identity of an expensive batch run's input set.

`input_set_hash(identity_object)` reduces the full set of inputs that determine a
batch run's output to a single `sha256:<hex>`, so a run can be skipped when its
inputs are unchanged and detected as stale the instant any input changes. See
docs/superpowers/specs/2026-07-09-batch-run-staleness-skip-recompute-design.md.

Identity is content-based (never mtime) and fails toward correctness: any input
that can affect the output belongs in `identity_object`; omitting one silently
narrows identity and would serve a stale output as fresh.
"""

from __future__ import annotations

import hashlib
import json
from typing import Any

# Bump when the canonical form or the identity contract changes, so prior hashes
# invalidate cleanly.
INPUT_SET_IDENTITY_VERSION = "soranoha-run-identity-v1"


def canonical_json(value: Any) -> str:
    """Deterministic JSON: sorted keys, compact separators, UTF-8, no NaN.

    Raises on a non-JSON-serializable value (fail closed — a run must never get a
    silent partial identity)."""
    return json.dumps(
        value,
        ensure_ascii=False,
        sort_keys=True,
        separators=(",", ":"),
        allow_nan=False,
    )


def input_set_hash(identity_object: dict[str, Any]) -> str:
    """Return `sha256:<hex>` over the canonical JSON of `identity_object`, with the
    identity-format version folded in authoritatively (a caller cannot override
    `identity_version`)."""
    if not isinstance(identity_object, dict):
        raise TypeError("identity_object must be a dict")
    stamped = {**identity_object, "identity_version": INPUT_SET_IDENTITY_VERSION}
    payload = canonical_json(stamped).encode("utf-8")
    return "sha256:" + hashlib.sha256(payload).hexdigest()
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cd ab-validator && python -m pytest reports/lib/tests/test_run_identity.py -v`
Expected: PASS (7 tests).

- [ ] **Step 5: Run the full lib test suite (no regressions)**

Run: `cd ab-validator && python -m pytest reports/lib/tests/ -v`
Expected: all existing tests + the 7 new ones PASS.

- [ ] **Step 6: Commit**

```bash
git add ab-validator/reports/lib/run_identity.py ab-validator/reports/lib/tests/test_run_identity.py
git commit -m "feat(fidelity): shared input-set identity hasher for batch skip-recompute"
```

---

## Slice 2 — morph-warehouse identity + skip (grounded outline; expand when reached)

**Deliverable:** `ab-morph-run analyze-aat` computes a warehouse `identity_object` = `{aat_content_hash (reuse the AAT dump's existing content_hash), dictionaries (nix store paths of sudachi/vibrato/vaporetto), analyzers (selected set), warehouse_profile (full|triage), schema_version (sha256 of ab-morph-run/sql/schema.sql + ab-warehouse schema)}`, writes a `run-manifest.json` sidecar + an `input_set_hash` column on `runs.parquet`, maintains a `by-input/<input_set_hash> -> runs/<run-id>` index, and **skips** when `by-input/<H>` resolves to a run whose Parquet outputs re-verify — unless `--force`.

**Grounding needed before writing tasks:** `crates/ab-morph-run/src/main.rs` (analyze-aat args, dispatch `:417-473`), `src/lib.rs:180 run_analyze_aat_warehouse`, `src/pipeline.rs` (`.staging` + publish path `:2146-2252`), `sql/schema.sql` (runs table), how dictionary paths/env arrive. Confirm open-question knobs: does `warehouse_profile` change values or only retained tables; are analyzer versions captured.

**Interfaces produced:** `run-manifest.json` schema; `input_set_hash` column; `by-input/` index layout; `--force` flag. **Verification:** fixture warehouse (tiny AAT dir) proving deterministic identity, skip-on-match, recompute-on-mutated-input, `--force` overrides.

## Slice 3 — AAT generator identity + skip / F6 (grounded outline; expand when reached)

**Deliverable:** `run-aat-full.sh` records in `metadata.json` (schema_version bump) both the dump's `input_set_hash` = `{corpus_content_hash, adapter (store path + adapter_version), feature_patterns_hash (sha256 of data/feature-patterns.toml), output-affecting flags, identity_version}` **and** the dump's own output content hash (`hash_aat_dir` of the produced `aat/` tree); skips regeneration when a dump with the current `input_set_hash` already exists on `/db` and its output hash re-verifies — unless forced.

**Grounding needed:** `run-aat-full.sh` steps (`:221-311`), the `metadata.json` writer (`:290-309`), corpus gating (`:193`), how the corpus path/adapter store-path are known at run time.

**Verification:** dry-run/fixture test: unchanged inputs → skip; mutated corpus fixture → recompute; forced → always recompute.

## Slice 4 — CI staleness gate (grounded outline; expand when reached)

**Deliverable:** extend the existing `monorepo-fidelity-lock-idempotency` flake check / smoke pattern with a fixture-based assertion that both batches skip-on-unchanged and recompute-on-mutated. **Grounding needed:** `tests/fidelity-lock-idempotency-smoke.sh`, the root `flake.nix` check wiring.
