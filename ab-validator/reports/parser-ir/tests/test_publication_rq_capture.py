from __future__ import annotations

import importlib.util
import pathlib

import pytest


AB_ROOT = pathlib.Path(__file__).resolve().parents[3]
SCRIPT = AB_ROOT / "reports/parser-ir/publication-rq-capture.py"
H = "sha256:" + "1" * 64


@pytest.fixture
def module():
    spec = importlib.util.spec_from_file_location("publication_rq_capture", SCRIPT)
    assert spec and spec.loader
    loaded = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(loaded)
    return loaded


@pytest.fixture
def capture_input():
    ids = ["000001_1", "000002_2", "000003_3"]
    corpus = {
        "corpus_id": "fixture",
        "corpus_snapshot_hash": H,
        "list_hash": H,
        "entries": [{"work_id": work_id, "source_sha256": H} for work_id in ids],
    }
    authority = {
        "qualification_identity_ref": H,
        "policy_hash": H,
        "preservation_schema_hash": H,
        "validator_semantics_hash": H,
        "census_hash": H,
    }
    publication = {
        "join_input_valid": True,
        "structure_check_candidates": {"plaintext_body_only": True},
        "counts": {
            "preservation_records": 1,
            "tei_preservation_references": 0,
            "non_null_tei_pointers": 0,
            "non_null_source_pointers": 1,
            "by_construct": {"span_coordinates": 1},
        },
        "artifacts": [],
    }
    return {
        "corpus": corpus,
        "authority": authority,
        "works": [
            {
                "work_id": work_id,
                "source_sha256": H,
                "parser_disposition": "parsed",
                "publication": publication,
            }
            for work_id in ids
        ],
    }


def test_capture_rejects_incomplete_members(module, tmp_path, capture_input):
    capture_input["works"].pop()
    with pytest.raises(module.CaptureError, match="exact pinned corpus membership"):
        module.capture(capture_input, tmp_path / "store")


def test_failed_work_has_no_publication_evidence(module, tmp_path, capture_input):
    capture_input["works"][0] = {
        "work_id": "000001_1",
        "source_sha256": H,
        "parser_disposition": "failed",
        "failure": {"kind": "exit", "code": 1},
    }
    result = module.capture(capture_input, tmp_path / "store")
    locator = result.index["records"][0]["locator"]
    record = __import__("json").loads((tmp_path / "store" / locator).read_text())
    assert "publication" not in record


def test_capture_is_order_independent_and_canonical(module, tmp_path, capture_input):
    first = module.capture(capture_input, tmp_path / "a")
    capture_input["works"].reverse()
    second = module.capture(capture_input, tmp_path / "b")
    assert first.index_bytes == second.index_bytes
    assert first.manifest_bytes == second.manifest_bytes


def test_metadata_prefix_cannot_become_record_identity(module, tmp_path, capture_input):
    capture_input["works"][0]["work_id"] = "000001"
    with pytest.raises(module.CaptureError, match="exact pinned corpus membership"):
        module.capture(capture_input, tmp_path / "store")


def test_source_identity_mismatch_fails_closed(module, tmp_path, capture_input):
    capture_input["works"][0]["source_sha256"] = "sha256:" + "2" * 64
    with pytest.raises(module.CaptureError, match="source identity mismatch"):
        module.capture(capture_input, tmp_path / "store")
