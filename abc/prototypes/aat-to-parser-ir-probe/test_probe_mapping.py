#!/usr/bin/env python
import importlib.util
import pathlib
import unittest


PROBE_DIR = pathlib.Path(__file__).resolve().parent


def load_mapper():
    spec = importlib.util.spec_from_file_location("aat_probe_map", PROBE_DIR / "map.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


mapper = load_mapper()


def load_mapping_doc():
    spec = importlib.util.spec_from_file_location(
        "aat_probe_mapping_doc", PROBE_DIR / "mapping_doc.py"
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class ProbeMappingTest(unittest.TestCase):
    def test_ruby_direction_projects_into_parser_ir(self):
        ledger = []
        node = mapper.map_inline(
            {
                "kind": "ruby",
                "base": "猫",
                "reading": "ねこ",
                "direction": "right",
                "span": {"line_start": 1, "line_end": 1, "byte_start": 0, "byte_end": 3},
            },
            0,
            ledger,
            "blocks[0].content[0]",
        )

        self.assertEqual("right", node["ruby"]["direction"])
        self.assertFalse(
            any(entry["aat"].endswith(".ruby.direction") for entry in ledger),
            "ruby.direction should be a direct projection after ADR 0024, not a divergence entry",
        )

    def test_style_inline_container_maps_to_emphasis_without_unsupported(self):
        ledger = []
        node = mapper.map_inline(
            {
                "kind": "style",
                "style_type": "boten",
                "content": [
                    {"kind": "text", "value": "ふんどし"},
                ],
            },
            0,
            ledger,
            "blocks[0].content[1]",
        )

        self.assertEqual(
            {"type": "emphasis", "text": "ふんどし", "style": "boten"},
            {k: node[k] for k in ("type", "text", "style")},
        )
        self.assertFalse(
            any(entry["category"] == "UNSUPPORTED" for entry in ledger),
            "style nodes are represented by parser-IR emphasis under I-09",
        )

    def test_unsupported_bucket_drops_occurrence_indices(self):
        self.assertEqual(
            "blocks[].content[].warigaki",
            mapper.aat_pointer_bucket("blocks[12].content[3].warigaki"),
        )
        self.assertEqual(
            "meta.source_encoding",
            mapper.aat_pointer_bucket("meta.source_encoding=utf-8-bom"),
        )
        self.assertEqual(
            "blocks[][block=paragraph]",
            mapper.aat_pointer_bucket("blocks[12][block=paragraph]"),
        )

    def test_lossy_windows_31j_projects_to_shift_jis_with_ambiguity(self):
        ledger = []
        source = mapper.map_meta_source(
            {
                "meta": {
                    "source_encoding": "windows-31j-lossy",
                    "source_hash": "sha256:" + "1" * 64,
                    "adapter": "aozora-rs",
                    "adapter_version": "probe",
                    "parse_complete": True,
                    "warnings": [],
                }
            },
            ledger,
        )

        self.assertEqual("Shift_JIS", source["encoding"])
        self.assertFalse(any(entry["category"] == "UNSUPPORTED" for entry in ledger))
        self.assertTrue(
            any(
                entry["category"] == "AMBIGUITY"
                and entry["aat"] == "meta.source_encoding=windows-31j-lossy"
                and entry["parser_ir"] == "source.encoding"
                for entry in ledger
            )
        )

    def test_generated_mapping_document_folds_measured_rule_buckets(self):
        mapping_doc = load_mapping_doc()
        doc = mapping_doc.build_mapping_document(
            [
                mapper.ledger(
                    "AMBIGUITY",
                    "blocks[0].content[1].style",
                    "emphasis",
                    "style mapped to emphasis",
                ),
                mapper.ledger(
                    "AMBIGUITY",
                    "blocks[2].content[3].style",
                    "emphasis",
                    "style mapped to emphasis",
                ),
                mapper.ledger("LOSS", "meta.adapter", "(none)", "adapter identity dropped"),
            ]
        )

        rules = doc["transform_rule_descriptions"]
        self.assertEqual(2, len(rules))
        self.assertEqual(["A-01", "L-01"], [rule["rule_id"] for rule in rules])
        self.assertIn("Observed 2 occurrences", rules[0]["description"])
        # The hash of schemas/parser-ir.schema.json under the abc canonicalizer.
        # Restated here, so it must be rotated with the schema; the authoritative
        # copy is :parser_ir_schema_hash in data/release-parser-identity-v1.edn,
        # and a mismatch between the two means one of them was missed rather than
        # that this probe found a defect.
        self.assertEqual(
            "sha256:43a6a6d86ca5eca062508e6cae633d19bf5248f15c5bb46153a6d8580ea916ec",
            doc["target_parser_ir_schema_hash"],
        )
        self.assertRegex(doc["mapping_schema_hash"], r"^sha256:[0-9a-f]{64}$")


if __name__ == "__main__":
    unittest.main()
