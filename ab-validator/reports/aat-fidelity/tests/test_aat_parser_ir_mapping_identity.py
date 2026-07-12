from __future__ import annotations

import sys
import unittest
from collections import Counter
from pathlib import Path


PROBE_DIR = Path(__file__).resolve().parents[1] / "aat_parser_ir_mapping"
sys.path.insert(0, str(PROBE_DIR))

import mapper  # noqa: E402
import mapping_doc  # noqa: E402


PRIMARY = "sha256:" + "1" * 64
BUNDLE = "sha256:" + "2" * 64


class MappingIdentityTest(unittest.TestCase):
    def test_canonical_primary_and_supplied_bundle_stay_distinct(self) -> None:
        ledger: list[dict] = []
        source = mapper.map_meta_source(
            {
                "meta": {
                    "source_encoding": "utf-8",
                    "source_hash": PRIMARY,
                    "primary_text_hash": PRIMARY,
                    "parse_complete": True,
                }
            },
            ledger,
            work_content_hash=BUNDLE,
        )

        self.assertEqual(source["primary_text_hash"], PRIMARY)
        self.assertEqual(source["work_content_hash"], BUNDLE)
        self.assertTrue(
            any(
                row["aat"] == "meta.primary_text_hash"
                and row["parser_ir"] == "source.primary_text_hash"
                for row in ledger
            )
        )
        self.assertFalse(any(row["parser_ir"] == "source.work_content_hash" for row in ledger))

    def test_historical_source_hash_is_primary_fallback(self) -> None:
        ledger: list[dict] = []
        source = mapper.map_meta_source(
            {
                "meta": {
                    "source_encoding": "utf-8",
                    "source_hash": PRIMARY,
                    "parse_complete": True,
                }
            },
            ledger,
        )

        self.assertEqual(source["primary_text_hash"], PRIMARY)
        self.assertEqual(source["work_content_hash"], PRIMARY)
        self.assertTrue(
            any(
                row["aat"] == "meta.source_hash" and row["parser_ir"] == "source.primary_text_hash"
                for row in ledger
            )
        )

    def test_conflicting_aat_aliases_are_rejected(self) -> None:
        with self.assertRaisesRegex(ValueError, "primary_text_hash"):
            mapper.map_meta_source(
                {
                    "meta": {
                        "source_encoding": "utf-8",
                        "source_hash": PRIMARY,
                        "primary_text_hash": BUNDLE,
                        "parse_complete": True,
                    }
                },
                [],
            )

    def test_generated_mapping_declares_external_work_identity(self) -> None:
        rules = Counter({("AMBIGUITY", "meta.source_hash", "source.primary_text_hash"): 1})
        key = next(iter(rules))
        document = mapping_doc.build_mapping_document_from_counts(
            rules,
            {key: "meta.source_hash"},
            {key: "historical fallback"},
            repo_root=Path(__file__).resolve().parents[3] / "data" / "abc-schemas",
        )

        self.assertEqual(document["mapping_version"], "0.4.0")
        self.assertFalse(
            any(
                row["aat_pointer"] == "meta.source_hash"
                and row["parser_ir_pointer"] == "source.work_content_hash"
                for row in document["transform_rule_descriptions"]
            )
        )
        self.assertTrue(
            any(
                row["parser_ir_pointer"] == "source.work_content_hash"
                and row["source"] == "converter_policy"
                for row in document["synthetic_evidence_descriptions"]
            )
        )


if __name__ == "__main__":
    unittest.main()
