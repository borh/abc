"""Tests for the AAT-dump input-set identity."""

from __future__ import annotations

import shutil
import sys
import tempfile
import unittest
from pathlib import Path

_PKG = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(_PKG))

import generator_identity as gi  # noqa: E402


class GeneratorIdentity(unittest.TestCase):
    def setUp(self) -> None:
        self.d = Path(tempfile.mkdtemp())
        self.addCleanup(lambda: shutil.rmtree(self.d, ignore_errors=True))
        self.corpus = self.d / "corpus"
        (self.corpus / "cards").mkdir(parents=True)
        (self.corpus / "cards" / "w1.txt").write_text("work one", encoding="utf-8")
        self.adapter = self.d / "adapter.bin"
        self.adapter.write_bytes(b"ADAPTERv1")
        self.ab_index = self.d / "ab-index"
        self.ab_index.write_bytes(b"INDEXv1")
        self.ab_check = self.d / "ab-check"
        self.ab_check.write_bytes(b"CHECKv1")
        self.fp = self.d / "feature-patterns.toml"
        self.fp.write_text("[p]\n", encoding="utf-8")
        self.renderer = self.d / "renderer"
        (self.renderer / "lib").mkdir(parents=True)
        (self.renderer / "lib" / "engine.rb").write_text("render v1", encoding="utf-8")
        self.base = dict(
            corpus_dir=self.corpus, adapter_version="1.2.3",
            adapter_binary=self.adapter,
            ab_index_binary=self.ab_index, ab_check_binary=self.ab_check,
            feature_patterns_file=self.fp,
        )

    def h(self, **ov):
        return gi.generator_input_set_hash(**{**self.base, **ov})

    def test_deterministic_prefixed(self) -> None:
        self.assertEqual(self.h(), self.h())
        self.assertTrue(self.h().startswith("sha256:"))

    def test_corpus_change(self) -> None:
        before = self.h()
        (self.corpus / "cards" / "w1.txt").write_text("work one!!", encoding="utf-8")
        self.assertNotEqual(before, self.h())

    def test_adapter_version_change(self) -> None:
        self.assertNotEqual(self.h(), self.h(adapter_version="9.9.9"))

    def test_adapter_binary_change(self) -> None:
        before = self.h()
        self.adapter.write_bytes(b"ADAPTERv2")
        self.assertNotEqual(before, self.h())

    def test_ab_index_binary_change_changes_hash(self) -> None:
        before = self.h()
        self.ab_index.write_bytes(b"INDEXv2")  # indexer rebuilt with a logic change
        self.assertNotEqual(before, self.h())

    def test_ab_check_binary_change_changes_hash(self) -> None:
        before = self.h()
        self.ab_check.write_bytes(b"CHECKv2")  # fidelity engine rebuilt
        self.assertNotEqual(before, self.h())

    def test_feature_patterns_change(self) -> None:
        before = self.h()
        self.fp.write_text("[p]\nx=1\n", encoding="utf-8")
        self.assertNotEqual(before, self.h())

    def test_timeout_and_features_absent_vs_present(self) -> None:
        self.assertNotEqual(self.h(), self.h(timeout="30"))
        self.assertNotEqual(self.h(), self.h(features="ruby"))

    def test_work_ids_hashed_by_content_not_path(self) -> None:
        wid = self.d / "work-ids.json"
        wid.write_text('["1","2"]', encoding="utf-8")
        with_wid = self.h(work_ids=str(wid))
        self.assertNotEqual(self.h(), with_wid)  # absent vs present
        # editing the work-ids file IN PLACE (same path) must change identity
        wid.write_text('["3","4"]', encoding="utf-8")
        self.assertNotEqual(with_wid, self.h(work_ids=str(wid)))

    def test_identity_object_keys(self) -> None:
        obj = gi.build_identity_object(**self.base)
        self.assertEqual(
            set(obj),
            {"corpus_content_hash", "adapter_version", "adapter_binary_hash",
             "ab_index_binary_hash", "ab_check_binary_hash", "renderer_content_hash",
             "feature_patterns_hash", "timeout", "features", "work_ids_hash"},
        )
        self.assertIsNone(obj["timeout"])
        self.assertIsNone(obj["features"])
        self.assertIsNone(obj["work_ids_hash"])

    def test_renderer_absent_by_default_is_none(self) -> None:
        obj = gi.build_identity_object(**self.base)
        self.assertIsNone(obj["renderer_content_hash"])

    def test_renderer_presence_changes_hash(self) -> None:
        self.assertNotEqual(self.h(), self.h(renderer_dir=self.renderer))

    def test_renderer_content_change_changes_hash(self) -> None:
        before = self.h(renderer_dir=self.renderer)
        (self.renderer / "lib" / "engine.rb").write_text("render v2", encoding="utf-8")
        self.assertNotEqual(before, self.h(renderer_dir=self.renderer))

    def test_provenance_fields(self) -> None:
        aat = self.d / "aat"
        aat.mkdir()
        (aat / "000001_1-a.json").write_text('{"b":[1]}', encoding="utf-8")
        pf = gi.provenance_fields(aat_dir=aat, **self.base)
        self.assertEqual(set(pf), {"input_set_hash", "output_content_hash"})
        self.assertTrue(pf["input_set_hash"].startswith("sha256:"))
        self.assertTrue(pf["output_content_hash"].startswith("sha256:"))
        # the input identity equals the standalone hash of the same inputs
        self.assertEqual(pf["input_set_hash"], self.h())


if __name__ == "__main__":
    unittest.main()
