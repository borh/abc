"""Tests for the morph-warehouse by-input index + run manifest."""

from __future__ import annotations

import json
import shutil
import sys
import tempfile
import unittest
from pathlib import Path

_PKG = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(_PKG))

import warehouse_index as wix  # noqa: E402


class WarehouseIndex(unittest.TestCase):
    def setUp(self) -> None:
        self.wh = Path(tempfile.mkdtemp())
        self.addCleanup(lambda: shutil.rmtree(self.wh, ignore_errors=True))
        self.run_id = "test-run-1"
        self.run = self.wh / "runs" / self.run_id
        # Mimic the real layout: partitioned parquet dirs + single-file parquet + views.sql.
        (self.run / "analyses.parquet").mkdir(parents=True)
        (self.run / "analyses.parquet" / "part-0.parquet").write_bytes(b"col-data-0")
        (self.run / "runs.parquet").write_bytes(b"runs-table")
        (self.run / "views.sql").write_text("SELECT 1;", encoding="utf-8")
        self.ident = {"aat_content_hash": "sha256:aa", "warehouse_profile": "full"}
        self.ish = "sha256:" + "ab" * 32

    def _publish(self) -> None:
        wix.write_run_manifest(self.run, identity_object=self.ident, input_set_hash=self.ish)
        wix.link_by_input(self.wh, self.ish, self.run_id)

    def test_hash_run_dir_deterministic_prefixed_and_excludes_manifest(self) -> None:
        h1 = wix.hash_run_dir(self.run)
        self.assertTrue(h1.startswith("sha256:"))
        self.assertEqual(h1, wix.hash_run_dir(self.run))
        # writing the manifest into the dir must NOT change the output hash
        (self.run / wix.MANIFEST_NAME).write_text("{}", encoding="utf-8")
        self.assertEqual(h1, wix.hash_run_dir(self.run))

    def test_hash_run_dir_changes_on_output_change(self) -> None:
        before = wix.hash_run_dir(self.run)
        (self.run / "runs.parquet").write_bytes(b"MUTATED")
        self.assertNotEqual(before, wix.hash_run_dir(self.run))

    def test_check_missing_when_not_indexed(self) -> None:
        self.assertEqual(wix.check(self.wh, self.ish)["status"], "missing")

    def test_check_fresh_after_publish(self) -> None:
        self._publish()
        res = wix.check(self.wh, self.ish)
        self.assertEqual(res["status"], "fresh")
        self.assertEqual(res["run_id"], self.run_id)

    def test_check_stale_after_output_mutation(self) -> None:
        self._publish()
        (self.run / "runs.parquet").write_bytes(b"CORRUPT-DIFFERENT-LEN")
        self.assertEqual(wix.check(self.wh, self.ish)["status"], "stale")

    def test_check_invalid_when_manifest_removed(self) -> None:
        self._publish()
        (self.run / wix.MANIFEST_NAME).unlink()
        self.assertEqual(wix.check(self.wh, self.ish)["status"], "invalid")

    def test_check_invalid_on_manifest_hash_mismatch(self) -> None:
        self._publish()
        mp = self.run / wix.MANIFEST_NAME
        m = json.loads(mp.read_text(encoding="utf-8"))
        m["input_set_hash"] = "sha256:" + "cd" * 32
        mp.write_text(json.dumps(m), encoding="utf-8")
        self.assertEqual(wix.check(self.wh, self.ish)["status"], "invalid")

    def test_check_invalid_on_dangling_link(self) -> None:
        self._publish()
        shutil.rmtree(self.run)  # target gone, symlink dangles
        self.assertEqual(wix.check(self.wh, self.ish)["status"], "invalid")

    def test_check_invalid_on_non_object_manifest(self) -> None:
        self._publish()
        (self.run / wix.MANIFEST_NAME).write_text("[1, 2, 3]", encoding="utf-8")
        self.assertEqual(wix.check(self.wh, self.ish)["status"], "invalid")

    def test_check_invalid_on_malformed_json_manifest(self) -> None:
        self._publish()
        (self.run / wix.MANIFEST_NAME).write_text("{not json", encoding="utf-8")
        self.assertEqual(wix.check(self.wh, self.ish)["status"], "invalid")

    def test_check_stale_when_outputs_vanish(self) -> None:
        self._publish()
        # Remove EVERY output file (incl. views.sql) but keep the manifest, so the
        # re-hash finds nothing and hits the new exception guard (not the ordinary
        # hash-mismatch branch).
        (self.run / "runs.parquet").unlink()
        (self.run / "views.sql").unlink()
        shutil.rmtree(self.run / "analyses.parquet")
        res = wix.check(self.wh, self.ish)
        self.assertEqual(res["status"], "stale")
        self.assertEqual(res["reason"], "run outputs missing or unreadable")

    def test_link_by_input_is_symlink_and_replaceable(self) -> None:
        self._publish()
        link = self.wh / "by-input" / self.ish.split(":", 1)[-1]
        self.assertTrue(link.is_symlink())
        # re-publishing the same identity replaces the link without error
        wix.link_by_input(self.wh, self.ish, self.run_id)
        self.assertTrue(link.is_symlink())
        self.assertEqual(wix.check(self.wh, self.ish)["status"], "fresh")

    def test_manifest_records_identity_and_output_hash(self) -> None:
        path = wix.write_run_manifest(
            self.run, identity_object=self.ident, input_set_hash=self.ish
        )
        m = json.loads(path.read_text(encoding="utf-8"))
        self.assertEqual(m["manifest_format"], wix.MANIFEST_FORMAT)
        self.assertEqual(m["input_set_hash"], self.ish)
        self.assertEqual(m["identity_object"], self.ident)
        self.assertTrue(m["output_content_hash"].startswith("sha256:"))


if __name__ == "__main__":
    unittest.main()
