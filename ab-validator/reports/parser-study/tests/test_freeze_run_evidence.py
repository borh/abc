import importlib.util
import json
from pathlib import Path

import pytest


ROOT = Path(__file__).parents[1]
SUMMARY = ROOT / "runs/aozora-parser-neutral-comparison-2026-07/run-manifests.json"
PREREGISTRATION = ROOT.parents[1] / "docs/studies/aozora-parser-comparison-preregistration.json"
SPEC = importlib.util.spec_from_file_location(
    "freeze_run_evidence", ROOT / "freeze_run_evidence.py"
)
assert SPEC is not None and SPEC.loader is not None
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


def load_summary() -> dict[str, object]:
    return json.loads(SUMMARY.read_bytes())


def test_checked_summary_has_exact_closed_lane_matrix() -> None:
    MODULE.validate_summary(load_summary())


def test_missing_lane_is_rejected() -> None:
    summary = load_summary()
    runs = summary["runs"]
    assert isinstance(runs, list)
    runs.pop()
    with pytest.raises(ValueError, match="exact 20-lane matrix"):
        MODULE.validate_summary(summary)


def test_host_capture_must_be_closed_unavailable_classification() -> None:
    summary = load_summary()
    summary["host_capture"] = {"status": "unavailable"}
    with pytest.raises(ValueError, match="host capture"):
        MODULE.validate_summary(summary)


def test_command_must_correspond_to_candidate_and_mode() -> None:
    summary = load_summary()
    runs = summary["runs"]
    assert isinstance(runs, list) and isinstance(runs[0], dict)
    execution = runs[0]["execution_sha256"]
    contracts = summary["execution_contracts"]
    assert isinstance(contracts, dict) and isinstance(contracts[execution], dict)
    contracts[execution]["candidate"] = "aozora2"
    with pytest.raises(ValueError, match="command binding"):
        MODULE.validate_summary(summary)


def test_canonical_bytes_are_byte_identical() -> None:
    checked = SUMMARY.read_bytes()
    assert MODULE.canonical_bytes(load_summary()) == checked


def test_protocol_mutation_is_rejected() -> None:
    preregistration = PREREGISTRATION.read_bytes().replace(
        b'"schema_version": 1', b'"schema_version": 2'
    )
    with pytest.raises(ValueError, match="protocol hash"):
        MODULE.validate_frozen_bindings(preregistration, load_summary(), {})


@pytest.mark.parametrize(
    ("field", "value", "message"),
    [
        ("study_id", "wrong-study", "study id"),
        ("timeout_seconds", 299, "timeout"),
    ],
)
def test_summary_frozen_scalar_mutation_is_rejected(
    field: str, value: object, message: str
) -> None:
    summary = load_summary()
    summary[field] = value
    with pytest.raises(ValueError, match=message):
        MODULE.validate_frozen_bindings(PREREGISTRATION.read_bytes(), summary, {})


def test_candidate_revision_mutation_is_rejected() -> None:
    summary = load_summary()
    candidates = summary["candidates"]
    assert isinstance(candidates, dict) and isinstance(candidates["aozora2"], dict)
    candidates["aozora2"]["parser_revision"] = "wrong"
    with pytest.raises(ValueError, match="candidate revisions"):
        MODULE.validate_frozen_bindings(PREREGISTRATION.read_bytes(), summary, {})


def test_inventory_content_hash_and_count_mutation_is_rejected(tmp_path: Path) -> None:
    inventory = tmp_path / "inventory.json"
    inventory.write_text(
        '{"items": [{"id": "one", "path": "one.txt", "sha256": "sha256:' + "0" * 64 + '"}]}'
    )
    summary = load_summary()
    summary_inventories = summary["inventories"]
    assert isinstance(summary_inventories, dict)
    summary_inventories["official-notation-vectors"] = {
        **summary_inventories["official-notation-vectors"],
        "items": 2,
    }
    with pytest.raises(ValueError, match="inventory hash or count"):
        MODULE.validate_frozen_bindings(
            PREREGISTRATION.read_bytes(),
            summary,
            {"official-notation-vectors": inventory},
        )
