from __future__ import annotations

import importlib.util
import pathlib
import xml.etree.ElementTree as ET

import pytest


AB_ROOT = pathlib.Path(__file__).resolve().parents[3]
SCRIPT = AB_ROOT / "reports/parser-ir/publication-bundle-validate.py"
PRESERVATION_SCHEMA = (
    AB_ROOT.parent / "ab-validator/research/schemas/parser-ir-publication-preservation.schema.json"
)
if not PRESERVATION_SCHEMA.is_file():
    PRESERVATION_SCHEMA = (
        AB_ROOT / "research/schemas/parser-ir-publication-preservation.schema.json"
    )


@pytest.fixture
def module():
    spec = importlib.util.spec_from_file_location("publication_bundle_validate", SCRIPT)
    assert spec and spec.loader
    loaded = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(loaded)
    return loaded


@pytest.fixture
def values():
    digest = "sha256:" + "1" * 64
    parser_ir = {"nodes": [{"type": "text", "text": "body", "source_pointer": "p0"}]}
    preservation = {
        "schema_id": "https://w3id.org/abc/schemas/parser-ir-publication-preservation.schema.json",
        "schema_version": "0.3.0",
        "schema_hash": digest,
        "parser_ir": {"schema_id": "x", "schema_hash": digest, "work_id": "w1"},
        "tei": {"profile_id": "p", "profile_hash": digest},
        "source": {
            "corpus_snapshot_hash": digest,
            "work_content_hash": digest,
            "source_path": "w1.txt",
            "encoding": "UTF-8",
            "normalization": "source",
        },
        "producer": {"agent": "test", "generated_at": "2026-07-17T00:00:00Z"},
        "mapping": None,
        "coverage": {"record_count": 0, "classes": []},
        "records": [],
    }
    return {
        "parser_ir": parser_ir,
        "preservation": preservation,
        "tei_root": ET.fromstring("<TEI/>"),
        "plaintext": "body",
        "tei_manifest": {},
        "plaintext_manifest": {},
        "tei_validation": {"status": "passed"},
        "source_region": None,
        "parser_ir_schema": {"type": "object", "additionalProperties": True},
        "preservation_schema": __import__("json").loads(PRESERVATION_SCHEMA.read_text()),
    }


def test_empty_schema_valid_preservation_discloses_zero_counts(module, values):
    result = module.validate_values(**values)
    assert result["structure_check_candidates"]["preservation_schema_valid"] is True
    assert result["counts"]["preservation_records"] == 0


def test_schema_invalid_but_join_valid_ir_keeps_independent_structure_evidence(module, values):
    values["parser_ir_schema"] = {"type": "object", "additionalProperties": False}
    result = module.validate_values(**values)
    assert result["supporting_preconditions"]["parser_ir_schema_valid"] is False
    assert result["join_input"] == {"status": "valid", "errors": []}
    assert result["structure_check_candidates"]["plaintext_body_only"] is True


def test_join_invalid_ir_is_unavailable_not_structure_failure(module, values):
    values["parser_ir"] = {"nodes": [{"type": "text", "text": 7}]}
    result = module.validate_values(**values)
    assert result["join_input"]["status"] == "invalid"
    assert result["structure_check_candidates"] is None


def test_supporting_source_status_does_not_change_structure(module, values):
    absent = module.validate_values(**values)
    values["source_region"] = {"gate_status": "SOURCE_AUTHORITY_GATE_FAIL"}
    red = module.validate_values(**values)
    assert absent["structure_check_candidates"] == red["structure_check_candidates"]


def test_old_or_open_preservation_sidecar_fails_full_schema(module, values):
    old = dict(values["preservation"], schema_version="0.2.0")
    assert module.validate_schema(old, values["preservation_schema"])
    open_value = dict(values["preservation"], attacker_field=True)
    assert module.validate_schema(open_value, values["preservation_schema"])


def test_markdown_renders_split_v2_blocks(module):
    summary = {
        "schema_version": module.SCHEMA_VERSION,
        "verdict": module.PASSED_VERDICT,
        "structure_check_candidates": {"plaintext_body_only": True},
        "supporting_preconditions": {"parser_ir_schema_valid": False},
        "join_input": {"status": "valid"},
        "counts": {"preservation_records": 0},
        "failures": [],
    }
    rendered = module.render_markdown(summary)
    assert "Structure check candidates" in rendered
    assert "Join input" in rendered
    assert "Supporting preconditions" in rendered
    assert "Counts" in rendered
