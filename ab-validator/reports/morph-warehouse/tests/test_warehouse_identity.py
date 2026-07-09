"""Tests for the morph-warehouse input-set identity."""

from __future__ import annotations

import shutil
import sys
import tempfile
import unittest
from pathlib import Path

_PKG = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(_PKG))

import warehouse_identity as wi  # noqa: E402


class WarehouseIdentity(unittest.TestCase):
    def setUp(self) -> None:
        self.dir = Path(tempfile.mkdtemp())
        self.addCleanup(lambda: shutil.rmtree(self.dir, ignore_errors=True))
        self.aat = self.dir / "aat"
        self.aat.mkdir()
        (self.aat / "000001_1-aaaa.json").write_text('{"blocks":[1]}', encoding="utf-8")
        self.schema = self.dir / "schema.sql"
        self.schema.write_text("CREATE TABLE runs(id TEXT);", encoding="utf-8")
        self.base = dict(
            aat_dir=self.aat,
            dictionaries={"sudachi": "/nix/store/aaa-sudachi", "vibrato": "/nix/store/bbb-vibrato"},
            analyzers=["vibrato", "sudachi-a", "sudachi-c"],
            warehouse_profile="full",
            schema_files=[self.schema],
        )

    def h(self, **overrides):
        return wi.warehouse_input_set_hash(**{**self.base, **overrides})

    def test_deterministic_and_prefixed(self) -> None:
        self.assertEqual(self.h(), self.h())
        self.assertTrue(self.h().startswith("sha256:"))

    def test_analyzer_order_independent(self) -> None:
        self.assertEqual(
            self.h(analyzers=["vibrato", "sudachi-a", "sudachi-c"]),
            self.h(analyzers=["sudachi-c", "vibrato", "sudachi-a"]),
        )

    def test_analyzer_set_change_changes_hash(self) -> None:
        self.assertNotEqual(self.h(), self.h(analyzers=["vibrato", "sudachi-a"]))

    def test_aat_content_change_changes_hash(self) -> None:
        before = self.h()
        (self.aat / "000001_1-aaaa.json").write_text('{"blocks":[999]}', encoding="utf-8")
        self.assertNotEqual(before, self.h())

    def test_dictionary_path_change_changes_hash(self) -> None:
        self.assertNotEqual(
            self.h(),
            self.h(dictionaries={"sudachi": "/nix/store/ZZZ-sudachi", "vibrato": "/nix/store/bbb-vibrato"}),
        )

    def test_profile_change_changes_hash(self) -> None:
        self.assertNotEqual(self.h(), self.h(warehouse_profile="triage"))

    def test_ortho_detect_absent_vs_present(self) -> None:
        self.assertNotEqual(self.h(), self.h(ortho_detect="historical"))

    def test_works_parquet_absent_vs_present(self) -> None:
        wp = self.dir / "works.parquet"
        wp.write_text("eligible-slice-A", encoding="utf-8")
        with_wp = self.h(works_parquet=wp)
        self.assertNotEqual(self.h(), with_wp)
        wp.write_text("eligible-slice-B", encoding="utf-8")
        self.assertNotEqual(with_wp, self.h(works_parquet=wp))

    def test_schema_change_changes_hash(self) -> None:
        before = self.h()
        self.schema.write_text("CREATE TABLE runs(id TEXT, extra INT);", encoding="utf-8")
        self.assertNotEqual(before, self.h())

    def test_identity_object_keys_all_present(self) -> None:
        obj = wi.build_identity_object(**self.base)
        self.assertEqual(
            set(obj),
            {"aat_content_hash", "dictionaries", "analyzers", "warehouse_profile",
             "ortho_detect", "works_parquet_hash", "schema_version"},
        )
        self.assertIsNone(obj["ortho_detect"])
        self.assertIsNone(obj["works_parquet_hash"])


if __name__ == "__main__":
    unittest.main()
