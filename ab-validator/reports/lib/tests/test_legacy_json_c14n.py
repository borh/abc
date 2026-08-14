"""Golden byte-equality pin for abc-legacy-json-c14n-v0 canonical JSON.

PINNED: these exact strings feed schema/document hashes compared across
adapters and against the abc side. If this test changes, hashes change and
recorded fidelity/skip metadata silently invalidates. Do not "fix" expected
values to match new output -- investigate the canonicalization change instead.
"""

from __future__ import annotations

import hashlib
import importlib.util
import json
import sys
import unittest
from pathlib import Path

_LIB = Path(__file__).resolve().parents[1]  # ab-validator/reports/lib
sys.path.insert(0, str(_LIB))

import legacy_json_c14n  # noqa: E402

# Golden cases: input value -> exact canonical JSON text. Covers slash-escaping
# inside string values AND inside keys, non-ASCII (ensure_ascii=False), nested
# objects, key sorting, and empty dict/list. Derived by running the current
# reports/aat-fidelity/.../c14n.py canonical_json, then pasting observed bytes.
GOLDEN = [
    ({"b": 1, "a": 2}, '{"a":2,"b":1}'),
    ({"path": "a/b/c"}, '{"path":"a\\/b\\/c"}'),
    ({"z": [3, 1, 2], "y": "x/y"}, '{"y":"x\\/y","z":[3,1,2]}'),
    ({"u": "café"}, '{"u":"café"}'),
    ({"nested": {"d/e": {"f": "/root"}}}, '{"nested":{"d\\/e":{"f":"\\/root"}}}'),
    ([], "[]"),
    ({}, "{}"),
]


def _load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class LegacyJsonC14n(unittest.TestCase):
    def test_canonical_json_matches_golden(self) -> None:
        for value, expected in GOLDEN:
            self.assertEqual(legacy_json_c14n.canonical_json(value), expected)

    def test_matches_every_shared_cross_language_vector(self) -> None:
        # abc/test/fixtures/canonicalization/abc-legacy-json-c14n-v0-vectors.json,
        # mirrored into this tree at data/abc-fixtures/canonicalization (symlink
        # in a checkout; copied from the abc flake input in the sandbox). Also
        # pinned by abc/tools/test_legacy_json_c14n.py (Python) and
        # abc hash_test.clj (Clojure) — a mismatch means the trees drifted.
        fixture_path = (
            Path(__file__).resolve().parents[3]
            / "data/abc-fixtures/canonicalization/abc-legacy-json-c14n-v0-vectors.json"
        )
        fixture = json.loads(fixture_path.read_text(encoding="utf-8"))
        self.assertEqual(fixture["algorithm_id"], "abc-legacy-json-c14n-v0")
        self.assertTrue(fixture["vectors"])
        for vector in fixture["vectors"]:
            with self.subTest(vector["name"]):
                canon = legacy_json_c14n.canonical_json(vector["input"])
                self.assertEqual(canon, vector["canonical_json"])
                self.assertEqual(
                    hashlib.sha256(canon.encode("utf-8")).hexdigest(),
                    vector["sha256"],
                )

    def test_shared_matches_remaining_legacy_copy(self) -> None:
        # Import the surviving legacy site's implementation and assert
        # byte-identical output to the shared one across the golden inputs.
        # (The level3-admission and publication-coverage copies died with the
        # five-parser gates — ADR third-party-comparison-retirement.)
        reports = Path(__file__).resolve().parents[2]  # ab-validator/reports
        c14n = _load(reports / "aat-fidelity/aat_parser_ir_mapping/c14n.py", "c14n_legacy")
        for value, _ in GOLDEN:
            want = legacy_json_c14n.canonical_json(value)
            self.assertEqual(c14n.canonical_json(value), want)


if __name__ == "__main__":
    unittest.main()
