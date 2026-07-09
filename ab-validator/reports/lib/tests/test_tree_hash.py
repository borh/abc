"""Tests for the general directory tree hash."""

from __future__ import annotations

import shutil
import sys
import tempfile
import unittest
from pathlib import Path

_LIB = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(_LIB))

import tree_hash  # noqa: E402


class TreeHash(unittest.TestCase):
    def setUp(self) -> None:
        self.d = Path(tempfile.mkdtemp())
        self.addCleanup(lambda: shutil.rmtree(self.d, ignore_errors=True))
        (self.d / "sub").mkdir()
        (self.d / "a.txt").write_bytes(b"A")
        (self.d / "sub" / "b.json").write_bytes(b'{"x":1}')
        (self.d / "sub" / "c.bin").write_bytes(b"\x00\x01")

    def test_deterministic_prefixed(self) -> None:
        h = tree_hash.tree_hash(self.d)
        self.assertTrue(h.startswith("sha256:"))
        self.assertEqual(h, tree_hash.tree_hash(self.d))

    def test_hashes_non_json_files(self) -> None:
        before = tree_hash.tree_hash(self.d)
        (self.d / "sub" / "c.bin").write_bytes(b"\x00\x02")
        self.assertNotEqual(before, tree_hash.tree_hash(self.d))

    def test_pattern_filters(self) -> None:
        only_json = tree_hash.tree_hash(self.d, pattern="*.json")
        (self.d / "a.txt").write_bytes(b"CHANGED")  # non-json edit
        self.assertEqual(only_json, tree_hash.tree_hash(self.d, pattern="*.json"))

    def test_exclude_names(self) -> None:
        self.assertNotEqual(
            tree_hash.tree_hash(self.d),
            tree_hash.tree_hash(self.d, exclude_names=("a.txt",)),
        )

    def test_content_change(self) -> None:
        before = tree_hash.tree_hash(self.d)
        (self.d / "a.txt").write_bytes(b"A2")
        self.assertNotEqual(before, tree_hash.tree_hash(self.d))

    def test_fileset_change(self) -> None:
        before = tree_hash.tree_hash(self.d)
        (self.d / "d.txt").write_bytes(b"D")
        self.assertNotEqual(before, tree_hash.tree_hash(self.d))

    def test_empty_and_missing_fail_closed(self) -> None:
        empty = Path(tempfile.mkdtemp())
        self.addCleanup(lambda: shutil.rmtree(empty, ignore_errors=True))
        with self.assertRaises(ValueError):
            tree_hash.tree_hash(empty)
        with self.assertRaises(ValueError):
            tree_hash.tree_hash(self.d / "does-not-exist")


if __name__ == "__main__":
    unittest.main()
