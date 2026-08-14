"""Cross-language conformance pin for abc/tools/legacy_json_c14n.py.

The shared vector fixture is also pinned by the Clojure implementation
(abc.tools.hash/abc-legacy-json-c14n-v0, via hash_test.clj) and the
ab-validator Python implementation (reports/lib/legacy_json_c14n.py). A
mismatch here means one tree's canonical bytes drifted and recorded hashes
would silently diverge — investigate the change, never repin.
"""

from __future__ import annotations

import hashlib
import json
import sys
import unittest
from pathlib import Path

_TOOLS = Path(__file__).resolve().parent
sys.path.insert(0, str(_TOOLS))

from legacy_json_c14n import canonical_json  # noqa: E402

FIXTURE = _TOOLS.parent / "test/fixtures/canonicalization/abc-legacy-json-c14n-v0-vectors.json"


class LegacyJsonC14n(unittest.TestCase):
    def test_matches_every_shared_vector(self) -> None:
        fixture = json.loads(FIXTURE.read_text(encoding="utf-8"))
        self.assertEqual(fixture["algorithm_id"], "abc-legacy-json-c14n-v0")
        self.assertTrue(fixture["vectors"])
        for vector in fixture["vectors"]:
            with self.subTest(vector["name"]):
                canon = canonical_json(vector["input"])
                self.assertEqual(canon, vector["canonical_json"])
                self.assertEqual(
                    hashlib.sha256(canon.encode("utf-8")).hexdigest(),
                    vector["sha256"],
                )


if __name__ == "__main__":
    unittest.main()
