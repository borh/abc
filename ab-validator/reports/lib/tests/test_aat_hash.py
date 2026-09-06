"""Tests for the AAT dump content hasher."""

from __future__ import annotations

import shutil
import sys
import tempfile
import unittest
from pathlib import Path

_LIB = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(_LIB))

import aat_hash  # noqa: E402


class AatHash(unittest.TestCase):
    def setUp(self) -> None:
        self.dir = Path(tempfile.mkdtemp())
        self.addCleanup(lambda: shutil.rmtree(self.dir, ignore_errors=True))
        (self.dir / "000001_1-aaaa.json").write_text('{"blocks":[1]}', encoding="utf-8")
        (self.dir / "000002_1-bbbb.json").write_text('{"blocks":[2]}', encoding="utf-8")

    def test_deterministic_and_prefixed(self) -> None:
        h1 = aat_hash.hash_aat_dir(self.dir)
        h2 = aat_hash.hash_aat_dir(self.dir)
        self.assertEqual(h1, h2)
        self.assertTrue(h1.startswith("sha256:"))

    def test_content_change_changes_hash(self) -> None:
        before = aat_hash.hash_aat_dir(self.dir)
        (self.dir / "000001_1-aaaa.json").write_text('{"blocks":[999]}', encoding="utf-8")
        self.assertNotEqual(before, aat_hash.hash_aat_dir(self.dir))

    def test_fileset_change_changes_hash(self) -> None:
        before = aat_hash.hash_aat_dir(self.dir)
        (self.dir / "000003_1-cccc.json").write_text('{"blocks":[]}', encoding="utf-8")
        after_add = aat_hash.hash_aat_dir(self.dir)
        self.assertNotEqual(before, after_add)
        (self.dir / "000003_1-cccc.json").unlink()
        self.assertEqual(before, aat_hash.hash_aat_dir(self.dir))  # removal restores

    def test_empty_dir_fails_closed(self) -> None:
        empty = Path(tempfile.mkdtemp())
        self.addCleanup(lambda: shutil.rmtree(empty, ignore_errors=True))
        with self.assertRaises(ValueError):
            aat_hash.hash_aat_dir(empty)


if __name__ == "__main__":
    unittest.main()
