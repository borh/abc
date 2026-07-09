"""Characterization + unit tests for the shared fresh/stale decision rule.

Two suites live here:

* ``Classify`` exercises ``freshness.classify`` directly for all four verdicts.
* ``GeneratorSkipCharacterization`` and ``WarehouseCheckCharacterization`` pin the
  EXACT result dicts of the two callers (``generator_skip.is_fresh`` and
  ``warehouse_index.check``) against real on-disk fixtures. They are the oracle:
  written to PASS against the current (un-refactored) callers, they must stay green
  after the decision rule is extracted into ``freshness`` — byte-for-byte identical
  return vocabulary.

``freshness`` is imported lazily inside the ``Classify`` cases so that, before the
module exists, ONLY those cases error with ``ModuleNotFoundError`` while the
caller-characterization cases still import and pass against today's code.
"""

from __future__ import annotations

import json
import shutil
import sys
import tempfile
import unittest
from pathlib import Path

_LIB = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(_LIB))
# Callers live outside lib/; add their package dirs so we can import them here.
_REPORTS = _LIB.parent
sys.path.insert(0, str(_REPORTS / "aat-fidelity"))
sys.path.insert(0, str(_REPORTS / "morph-warehouse"))

import aat_hash  # noqa: E402
import generator_skip as gs  # noqa: E402
import warehouse_index as wix  # noqa: E402


class Classify(unittest.TestCase):
    def test_input_mismatch(self):
        import freshness  # noqa: E402
        d = freshness.classify("sha256:a", "sha256:b", "sha256:o", lambda: "unused")
        self.assertEqual(d.verdict, freshness.Verdict.INPUT_MISMATCH)

    def test_output_unreadable(self):
        import freshness  # noqa: E402

        def boom():
            raise OSError("gone")
        d = freshness.classify("sha256:a", "sha256:a", "sha256:o", boom)
        self.assertEqual(d.verdict, freshness.Verdict.OUTPUT_UNREADABLE)

    def test_output_unreadable_valueerror(self):
        import freshness  # noqa: E402

        def boom():
            raise ValueError("bad")
        d = freshness.classify("sha256:a", "sha256:a", "sha256:o", boom)
        self.assertEqual(d.verdict, freshness.Verdict.OUTPUT_UNREADABLE)

    def test_output_mismatch_carries_hashes(self):
        import freshness  # noqa: E402
        d = freshness.classify("sha256:a", "sha256:a", "sha256:o", lambda: "sha256:x")
        self.assertEqual(d.verdict, freshness.Verdict.OUTPUT_MISMATCH)
        self.assertEqual(d.recorded, "sha256:o")
        self.assertEqual(d.actual, "sha256:x")

    def test_fresh(self):
        import freshness  # noqa: E402
        d = freshness.classify("sha256:a", "sha256:a", "sha256:o", lambda: "sha256:o")
        self.assertEqual(d.verdict, freshness.Verdict.FRESH)


class GeneratorSkipCharacterization(unittest.TestCase):
    """Pin the EXACT dicts returned by generator_skip.is_fresh for each branch."""

    def setUp(self) -> None:
        self.out = Path(tempfile.mkdtemp())
        self.addCleanup(lambda: shutil.rmtree(self.out, ignore_errors=True))
        self.aat = self.out / "aat"
        self.aat.mkdir()
        (self.aat / "000001_1-a.json").write_text('{"b":[1]}', encoding="utf-8")
        self.ish = "sha256:" + "ab" * 32
        # Derive the fresh fixture's output_content_hash by actually hashing.
        self.och = aat_hash.hash_aat_dir(self.aat)
        (self.out / "metadata.json").write_text(
            json.dumps({"input_set_hash": self.ish, "output_content_hash": self.och}),
            encoding="utf-8",
        )

    def test_all_good_fresh(self) -> None:
        self.assertEqual(
            gs.is_fresh(self.out, self.ish),
            {"fresh": True, "reason": "inputs unchanged and outputs verify"},
        )

    def test_missing_metadata(self) -> None:
        (self.out / "metadata.json").unlink()
        self.assertEqual(
            gs.is_fresh(self.out, self.ish),
            {"fresh": False, "reason": "no metadata.json"},
        )

    def test_input_mismatch(self) -> None:
        self.assertEqual(
            gs.is_fresh(self.out, "sha256:" + "cd" * 32),
            {"fresh": False, "reason": "input_set_hash differs (inputs changed)"},
        )

    def test_matching_input_missing_aat(self) -> None:
        shutil.rmtree(self.aat)
        self.assertEqual(
            gs.is_fresh(self.out, self.ish),
            {"fresh": False, "reason": "aat outputs missing or unreadable"},
        )

    def test_matching_input_tampered_output(self) -> None:
        (self.aat / "000001_1-a.json").write_text('{"b":[999]}', encoding="utf-8")
        self.assertEqual(
            gs.is_fresh(self.out, self.ish),
            {"fresh": False, "reason": "aat content hash mismatch"},
        )


class WarehouseCheckCharacterization(unittest.TestCase):
    """Pin the EXACT dicts returned by warehouse_index.check for each branch that
    flows through the freshness decision (input mismatch / output unreadable /
    output mismatch / fresh)."""

    def setUp(self) -> None:
        self.wh = Path(tempfile.mkdtemp())
        self.addCleanup(lambda: shutil.rmtree(self.wh, ignore_errors=True))
        self.run_id = "test-run-1"
        self.run = self.wh / "runs" / self.run_id
        (self.run / "analyses.parquet").mkdir(parents=True)
        (self.run / "analyses.parquet" / "part-0.parquet").write_bytes(b"col-data-0")
        (self.run / "runs.parquet").write_bytes(b"runs-table")
        (self.run / "views.sql").write_text("SELECT 1;", encoding="utf-8")
        self.ident = {"aat_content_hash": "sha256:aa", "warehouse_profile": "full"}
        self.ish = "sha256:" + "ab" * 32
        wix.write_run_manifest(self.run, identity_object=self.ident, input_set_hash=self.ish)
        wix.link_by_input(self.wh, self.ish, self.run_id)
        # link.resolve() is what check() reports as run_dir.
        self.run_dir = (self.wh / "by-input" / self.ish.split(":", 1)[-1]).resolve()

    def test_fresh(self) -> None:
        self.assertEqual(
            wix.check(self.wh, self.ish),
            {"status": "fresh", "run_dir": str(self.run_dir), "run_id": self.run_dir.name},
        )

    def test_input_mismatch_invalid(self) -> None:
        mp = self.run / wix.MANIFEST_NAME
        m = json.loads(mp.read_text(encoding="utf-8"))
        m["input_set_hash"] = "sha256:" + "cd" * 32
        mp.write_text(json.dumps(m), encoding="utf-8")
        self.assertEqual(
            wix.check(self.wh, self.ish),
            {"status": "invalid", "reason": "manifest input_set_hash mismatch",
             "run_dir": str(self.run_dir)},
        )

    def test_output_unreadable_stale(self) -> None:
        (self.run / "runs.parquet").unlink()
        (self.run / "views.sql").unlink()
        shutil.rmtree(self.run / "analyses.parquet")
        self.assertEqual(
            wix.check(self.wh, self.ish),
            {"status": "stale", "reason": "run outputs missing or unreadable",
             "run_dir": str(self.run_dir)},
        )

    def test_output_mismatch_stale_carries_hashes(self) -> None:
        recorded = json.loads(
            (self.run / wix.MANIFEST_NAME).read_text(encoding="utf-8")
        )["output_content_hash"]
        (self.run / "runs.parquet").write_bytes(b"CORRUPT-DIFFERENT-LEN")
        actual = wix.hash_run_dir(self.run)
        self.assertEqual(
            wix.check(self.wh, self.ish),
            {"status": "stale", "reason": "output content hash mismatch",
             "recorded": recorded, "actual": actual, "run_dir": str(self.run_dir)},
        )


if __name__ == "__main__":
    unittest.main()
