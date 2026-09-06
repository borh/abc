from __future__ import annotations

import sys
import unittest
from pathlib import Path


PROBE_DIR = Path(__file__).resolve().parents[1] / "aat_parser_ir_mapping"
sys.path.insert(0, str(PROBE_DIR))

import mapper  # noqa: E402


class GaijiProjectionTest(unittest.TestCase):
    def test_resolved_and_unresolved_gaiji_match_production_projection(self) -> None:
        for resolved in ["犍", "葛\U000e0100", "", None]:
            with self.subTest(resolved=resolved):
                ledger: list[dict] = []
                node, end = mapper.map_inline(
                    {
                        "kind": "gaiji",
                        "description": "原注記",
                        "jis_code": "第3水準1-87-71",
                        "resolved": resolved,
                    },
                    7,
                    ledger,
                    "blocks[0].content[0]",
                )
                self.assertEqual(node["gaiji"]["unicode"], resolved)
                self.assertEqual(node["gaiji"]["resolved"], resolved is not None)
                self.assertEqual(node["gaiji"]["raw_marker"], "原注記")
                self.assertEqual(node["gaiji"]["reference"], "第3水準1-87-71")
                visible = resolved if resolved is not None else "原注記"
                self.assertEqual(end, 7 + len(visible.encode("utf-8")))
                self.assertEqual(
                    any(
                        row["category"] == "LOSS" and row["parser_ir"] == "gaiji.unicode"
                        for row in ledger
                    ),
                    resolved is None,
                )


if __name__ == "__main__":
    unittest.main()
