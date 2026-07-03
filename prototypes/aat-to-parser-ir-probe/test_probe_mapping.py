#!/usr/bin/env python3
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


if __name__ == "__main__":
    unittest.main()
