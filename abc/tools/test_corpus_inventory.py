#!/usr/bin/env python3
"""Guards on the exploratory corpus tools' determinism and metric definitions.

These behaviours are not protected by any flake check that builds the real
corpus, and the two properties that matter most -- byte-identical output across
filesystem traversal orders, and metrics that mean what the design says -- are
invisible to a single-host run. So they are pinned here against small synthetic
archives instead.
"""

from __future__ import annotations

import importlib.util
import json
import pathlib
import tempfile
import unittest
import zipfile

TOOLS = pathlib.Path(__file__).resolve().parent


def load(name: str):
    spec = importlib.util.spec_from_file_location(name, TOOLS / f"{name}.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


inventory_tool = load("corpus_inventory")
tail_tool = load("corpus_tail_set")


def write_archive(root: pathlib.Path, card: str, name: str, text: str) -> None:
    path = root / "cards" / card / "files" / name
    path.parent.mkdir(parents=True, exist_ok=True)
    with zipfile.ZipFile(path, "w") as archive:
        archive.writestr("work.txt", text.encode("cp932"))


class NotationVolumeTest(unittest.TestCase):
    def test_gaiji_is_excluded_from_the_non_gaiji_annotation_count(self):
        # `※［＃…］` contains the annotation opener `［＃`, so a bare opener count
        # double-counts every gaiji. Both metrics are emitted precisely so the
        # selector can rank on the one that means "editor note".
        volume = inventory_tool.notation_volume("※［＃「口＋世」、U+546D］と［＃ここから２字下げ］")
        self.assertEqual(1, volume["gaiji"])
        self.assertEqual(2, volume["annot_markers"])
        self.assertEqual(1, volume["annot_non_gaiji"])

    def test_ruby_counts_openers_not_matched_pairs(self):
        # An unterminated delimiter is the input a stress tier exists to carry,
        # so it must not be dropped from the count that would select it.
        self.assertEqual(2, inventory_tool.notation_volume("猫《ねこ》と犬《")["ruby"])

    def test_max_line_and_lines_are_measured_over_split_lines(self):
        volume = inventory_tool.notation_volume("ab\ncdef\ng")
        self.assertEqual(3, volume["lines"])
        self.assertEqual(4, volume["max_line"])


class DeterminismTest(unittest.TestCase):
    def test_works_and_skipped_are_both_path_sorted(self):
        with tempfile.TemporaryDirectory() as raw:
            root = pathlib.Path(raw)
            # Created out of order, and named so that traversal order and sorted
            # order differ.
            for card in ("000300", "000100", "000200"):
                write_archive(root, card, "work.zip", "猫《ねこ》")
            for card in ("000900", "000400"):
                bad = root / "cards" / card / "files" / "broken.zip"
                bad.parent.mkdir(parents=True, exist_ok=True)
                bad.write_bytes(b"not a zip")

            works, skipped = inventory_tool.inventory(str(root))

            archives = [w["archive"] for w in works]
            self.assertEqual(sorted(archives), archives)
            reasons = [s["archive"] for s in skipped]
            self.assertEqual(sorted(reasons), reasons)
            self.assertEqual(3, len(works))
            self.assertEqual(2, len(skipped))

    def test_two_runs_agree_exactly(self):
        with tempfile.TemporaryDirectory() as raw:
            root = pathlib.Path(raw)
            for card in ("000002", "000001"):
                write_archive(root, card, "work.zip", "猫《ねこ》※［＃「口＋世」、U+546D］")
            first = inventory_tool.inventory(str(root))
            second = inventory_tool.inventory(str(root))
            self.assertEqual(
                json.dumps(first, ensure_ascii=False), json.dumps(second, ensure_ascii=False)
            )


class ProjectionTest(unittest.TestCase):
    def test_resource_and_correctness_projections_are_disjoint_metric_sets(self):
        # The prose tier and the generated payload must agree about ownership: a
        # metric belongs to exactly one projection, so an archive selected for a
        # correctness reason is never reported as resource-envelope evidence.
        resource = {m for m, p, _ in tail_tool.DIMENSIONS if p == "resource"}
        correctness = {m for m, p, _ in tail_tool.DIMENSIONS if p == "correctness"}
        self.assertEqual(set(), resource & correctness)
        self.assertEqual({"bytes", "ruby", "lines"}, resource)
        self.assertEqual({"max_line", "gaiji", "annot_non_gaiji"}, correctness)
        self.assertEqual(set(tail_tool.PROJECTIONS), {"resource", "correctness"})

    def test_projection_selects_only_its_own_metrics(self):
        works = [
            {
                "archive": "a",
                "bytes": 100,
                "ruby": 0,
                "lines": 1,
                "max_line": 1,
                "gaiji": 0,
                "annot_non_gaiji": 0,
            },
            {
                "archive": "b",
                "bytes": 1,
                "ruby": 0,
                "lines": 1,
                "max_line": 999,
                "gaiji": 0,
                "annot_non_gaiji": 0,
            },
        ]
        resource = tail_tool.project(works, 1, "resource")
        self.assertIn("bytes", resource["a"])
        self.assertNotIn("b", resource)
        correctness = tail_tool.project(works, 1, "correctness")
        self.assertIn("max_line", correctness["b"])

    def test_rank_breaks_ties_on_archive_path(self):
        works = [
            {"archive": "z", "bytes": 5},
            {"archive": "a", "bytes": 5},
        ]
        self.assertEqual(["a", "z"], [w["archive"] for w in tail_tool.rank(works, "bytes", 2)])


if __name__ == "__main__":
    unittest.main()
