"""Tests for the morph-warehouse resolve->compute->record wrapper."""

from __future__ import annotations

import shutil
import sys
import tempfile
import unittest
from pathlib import Path

_PKG = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(_PKG))

import warehouse_runner as wr  # noqa: E402
import warehouse_index as wix  # noqa: E402


class WarehouseRunner(unittest.TestCase):
    def setUp(self) -> None:
        self.wh = Path(tempfile.mkdtemp())
        self.addCleanup(lambda: shutil.rmtree(self.wh, ignore_errors=True))
        self.run_id = "run-A"
        self.ident = {"aat_content_hash": "sha256:aa", "warehouse_profile": "full"}
        self.calls = 0

    def _make_compute(self, payload: bytes = b"table-bytes"):
        """A fake compute that materializes runs/<run_id>/ like analyze-aat would."""

        def compute() -> None:
            self.calls += 1
            run = self.wh / "runs" / self.run_id
            (run / "analyses.parquet").mkdir(parents=True, exist_ok=True)
            (run / "analyses.parquet" / "part-0.parquet").write_bytes(payload)
            (run / "runs.parquet").write_bytes(b"runs-" + payload)

        return compute

    def test_missing_computes_and_records(self) -> None:
        res = wr.resolve_compute_record(
            warehouse_dir=self.wh,
            run_id=self.run_id,
            identity_object=self.ident,
            compute=self._make_compute(),
        )
        self.assertEqual(res["action"], "computed")
        self.assertEqual(res["reason"], "missing")
        self.assertEqual(self.calls, 1)
        # now indexed + fresh
        self.assertEqual(wix.check(self.wh, res["input_set_hash"])["status"], "fresh")

    def test_second_run_unchanged_skips_without_computing(self) -> None:
        wr.resolve_compute_record(
            warehouse_dir=self.wh,
            run_id=self.run_id,
            identity_object=self.ident,
            compute=self._make_compute(),
        )
        self.calls = 0
        res = wr.resolve_compute_record(
            warehouse_dir=self.wh,
            run_id=self.run_id,
            identity_object=self.ident,
            compute=self._make_compute(),
        )
        self.assertEqual(res["action"], "skip")
        self.assertEqual(res["reason"], "fresh")
        self.assertEqual(self.calls, 0)  # compute NOT invoked

    def test_force_recomputes_even_when_fresh(self) -> None:
        wr.resolve_compute_record(
            warehouse_dir=self.wh,
            run_id=self.run_id,
            identity_object=self.ident,
            compute=self._make_compute(),
        )
        self.calls = 0
        res = wr.resolve_compute_record(
            warehouse_dir=self.wh,
            run_id=self.run_id,
            identity_object=self.ident,
            compute=self._make_compute(),
            force=True,
        )
        self.assertEqual(res["action"], "computed")
        self.assertEqual(res["reason"], "forced")
        self.assertEqual(self.calls, 1)

    def test_stale_outputs_recompute(self) -> None:
        first = wr.resolve_compute_record(
            warehouse_dir=self.wh,
            run_id=self.run_id,
            identity_object=self.ident,
            compute=self._make_compute(),
        )
        # corrupt an output after publish
        (self.wh / "runs" / self.run_id / "runs.parquet").write_bytes(b"CORRUPT")
        self.assertEqual(wix.check(self.wh, first["input_set_hash"])["status"], "stale")
        self.calls = 0
        res = wr.resolve_compute_record(
            warehouse_dir=self.wh,
            run_id=self.run_id,
            identity_object=self.ident,
            compute=self._make_compute(),
        )
        self.assertEqual(res["action"], "computed")
        self.assertEqual(res["reason"], "stale")
        self.assertEqual(self.calls, 1)
        self.assertEqual(wix.check(self.wh, res["input_set_hash"])["status"], "fresh")

    def test_changed_identity_computes_new(self) -> None:
        wr.resolve_compute_record(
            warehouse_dir=self.wh,
            run_id=self.run_id,
            identity_object=self.ident,
            compute=self._make_compute(),
        )
        self.calls = 0
        res = wr.resolve_compute_record(
            warehouse_dir=self.wh,
            run_id=self.run_id,
            identity_object={**self.ident, "warehouse_profile": "triage"},
            compute=self._make_compute(),
        )
        self.assertEqual(res["action"], "computed")
        self.assertEqual(res["reason"], "missing")
        self.assertEqual(self.calls, 1)

    def test_compute_failure_records_nothing(self) -> None:
        def boom() -> None:
            self.calls += 1
            raise RuntimeError("analyze-aat failed")

        with self.assertRaises(RuntimeError):
            wr.resolve_compute_record(
                warehouse_dir=self.wh,
                run_id=self.run_id,
                identity_object=self.ident,
                compute=boom,
            )
        # no by-input index entry written on failure
        self.assertFalse((self.wh / "by-input").exists())


if __name__ == "__main__":
    unittest.main()
