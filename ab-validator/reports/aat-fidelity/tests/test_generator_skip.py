"""Tests for the AAT-dump freshness check."""

from __future__ import annotations

import json
import shutil
import sys
import tempfile
import unittest
from pathlib import Path

_PKG = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(_PKG))
_LIB = Path(__file__).resolve().parents[2] / "lib"
sys.path.insert(0, str(_LIB))

import generator_skip as gs  # noqa: E402
import aat_hash  # noqa: E402


class GeneratorSkip(unittest.TestCase):
    def setUp(self) -> None:
        self.out = Path(tempfile.mkdtemp())
        self.addCleanup(lambda: shutil.rmtree(self.out, ignore_errors=True))
        self.aat = self.out / "aat"
        self.aat.mkdir()
        (self.aat / "000001_1-a.json").write_text('{"b":[1]}', encoding="utf-8")
        self.ish = "sha256:" + "ab" * 32
        self._write_meta(self.ish, aat_hash.hash_aat_dir(self.aat))

    def _write_meta(self, ish: str, och: str) -> None:
        (self.out / "metadata.json").write_text(
            json.dumps({"input_set_hash": ish, "output_content_hash": och}),
            encoding="utf-8",
        )

    def test_fresh_when_inputs_unchanged_and_outputs_verify(self) -> None:
        res = gs.is_fresh(self.out, self.ish)
        self.assertTrue(res["fresh"])

    def test_not_fresh_on_input_hash_difference(self) -> None:
        self.assertFalse(gs.is_fresh(self.out, "sha256:" + "cd" * 32)["fresh"])

    def test_not_fresh_when_metadata_missing(self) -> None:
        (self.out / "metadata.json").unlink()
        self.assertFalse(gs.is_fresh(self.out, self.ish)["fresh"])

    def test_not_fresh_on_malformed_metadata(self) -> None:
        (self.out / "metadata.json").write_text("{not json", encoding="utf-8")
        self.assertFalse(gs.is_fresh(self.out, self.ish)["fresh"])

    def test_not_fresh_on_non_object_metadata(self) -> None:
        (self.out / "metadata.json").write_text("[1,2,3]", encoding="utf-8")
        self.assertFalse(gs.is_fresh(self.out, self.ish)["fresh"])

    def test_not_fresh_when_aat_content_changed(self) -> None:
        (self.aat / "000001_1-a.json").write_text('{"b":[999]}', encoding="utf-8")
        self.assertFalse(gs.is_fresh(self.out, self.ish)["fresh"])

    def test_not_fresh_when_aat_outputs_gone(self) -> None:
        shutil.rmtree(self.aat)
        self.assertFalse(gs.is_fresh(self.out, self.ish)["fresh"])


if __name__ == "__main__":
    unittest.main()
