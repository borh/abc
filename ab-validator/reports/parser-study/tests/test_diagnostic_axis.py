import importlib.util
import json
from pathlib import Path

import pytest
from jsonschema import Draft202012Validator


SCRIPT = Path(__file__).parents[1] / "diagnostic_axis.py"
SPEC = importlib.util.spec_from_file_location("diagnostic_axis", SCRIPT)
assert SPEC and SPEC.loader
axis = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(axis)

REPO = Path(__file__).parents[3]
FIXTURE = json.loads(
    (REPO / "docs/studies/fixtures/parser-comparison-diagnostics-v1.json").read_text()
)
POLICY = json.loads((REPO / "data/parser-study-diagnostic-lanes-v1.json").read_text())
EVIDENCE_VALIDATOR = Draft202012Validator(
    json.loads((REPO / "schemas/parser-study-axis-evidence.schema.json").read_text())
)
INDEX_VALIDATOR = Draft202012Validator(
    json.loads((REPO / "schemas/parser-study-evidence-index.schema.json").read_text())
)


def envelope(case: dict, version: int = 3) -> bytes:
    severity = (
        "warning"
        if case["expected_label"]
        in {
            "orphan_ruby_close",
            "invalid_gaiji_coordinate",
        }
        else "error"
    )
    return json.dumps(
        {
            "schemaVersion": version,
            "data": [
                {
                    "code": case["expected_label"],
                    "severity": severity,
                    "span": case["expected_span_utf8"],
                }
            ],
        }
    ).encode()


def lane(projector: str = "schema3") -> dict:
    return {
        "candidate": "ab-aozora",
        "measurement_mode": "native",
        "parser_revision": "revision",
        "adapter_revision": None,
        "program": "ab-aozora",
        "argv": ["--mode", "diagnostics"],
        "projector": projector,
    }


def members(
    tmp_path: Path,
    projector: str = "schema3",
    *,
    returncode: int | None = 0,
    timed_out: bool = False,
    malformed_case: int | None = None,
    known_on_none: bool = False,
) -> list[dict]:
    tmp_path.mkdir(parents=True, exist_ok=True)
    result = []
    for position, case in enumerate(FIXTURE["cases"]):
        payload = (
            b"not-json"
            if malformed_case == position
            else envelope(case, 3 if projector != "inspect_v2" else 2)
        )
        if projector == "none" and not known_on_none:
            payload = b"plain converter output"
        path = tmp_path / f"{case['id']}.stdout"
        path.write_bytes(payload)
        result.append(
            {
                "candidate": "ab-aozora",
                "measurement_mode": "native",
                "case_id": case["id"],
                "status": "timed_out" if timed_out else "exited",
                "returncode": None if timed_out else returncode,
                "stdout": {
                    "locator": path.name,
                    "sha256": axis.sha256(payload),
                    "bytes": len(payload),
                },
            }
        )
    return result


def derive(tmp_path: Path, projector: str = "schema3", **kwargs) -> dict:
    return axis.derive_lane(
        lane(projector),
        members(tmp_path, projector, **kwargs),
        FIXTURE["cases"],
        tmp_path,
        fixture_ref={"sha256": "sha256:" + "1" * 64, "bytes": 100},
        capture_ref={"sha256": "sha256:" + "2" * 64, "bytes": 200},
        labels=FIXTURE["labels"],
    )


def assert_disposition(record: dict, disposition: str) -> None:
    assert [metric["metric"] for metric in record["metrics"]] == axis.DIAGNOSTIC_METRICS
    assert all(metric["disposition"] == disposition for metric in record["metrics"])
    assert all(metric.get("value") is None for metric in record["metrics"])


def metric(record: dict, name: str) -> dict:
    return next(value for value in record["metrics"] if value["metric"] == name)


def test_structured_lane_emits_six_measured_metrics_with_denominator_four(
    tmp_path: Path,
) -> None:
    record = derive(tmp_path)
    assert all(value["disposition"] == "measured" for value in record["metrics"])
    assert metric(record, "diagnostic_presence")["value"] == {
        "kind": "ratio",
        "numerator": 4,
        "denominator": 4,
    }
    assert metric(record, "false_negative")["value"] == {
        "kind": "ratio",
        "numerator": 0,
        "denominator": 4,
    }


@pytest.mark.parametrize("process", [{"returncode": 2}, {"timed_out": True}])
def test_projector_none_process_failure_is_noncomparable(tmp_path: Path, process: dict) -> None:
    assert_disposition(derive(tmp_path, "none", **process), "non_comparable")


def test_missing_or_hash_mismatched_capture_is_unavailable(tmp_path: Path) -> None:
    lane_members = members(tmp_path)
    (tmp_path / lane_members[0]["stdout"]["locator"]).write_bytes(b"tampered")
    assert_disposition(
        axis.derive_lane(
            lane(),
            lane_members,
            FIXTURE["cases"],
            tmp_path,
            fixture_ref={"sha256": "sha256:" + "1" * 64, "bytes": 100},
            capture_ref={"sha256": "sha256:" + "2" * 64, "bytes": 200},
        ),
        "unavailable",
    )
    assert_disposition(
        axis.derive_lane(
            lane(),
            lane_members[:-1],
            FIXTURE["cases"],
            tmp_path,
            fixture_ref={"sha256": "sha256:" + "1" * 64, "bytes": 100},
            capture_ref={"sha256": "sha256:" + "2" * 64, "bytes": 200},
        ),
        "unavailable",
    )


def test_structured_nonzero_exit_with_usable_envelope_is_measured(tmp_path: Path) -> None:
    record = derive(tmp_path, returncode=2)
    assert all(value["disposition"] == "measured" for value in record["metrics"])


def test_structured_lane_without_usable_envelope_is_failed(tmp_path: Path) -> None:
    assert_disposition(derive(tmp_path, malformed_case=2, returncode=2), "failed")


def test_known_envelope_on_projector_none_rejects_stale_policy(tmp_path: Path) -> None:
    record = derive(tmp_path, "none", known_on_none=True)
    assert_disposition(record, "unavailable")
    assert all("stale lane policy" in value["reason"] for value in record["metrics"])


def test_all_derived_dispositions_validate_the_closed_evidence_schema(tmp_path: Path) -> None:
    records = [
        derive(tmp_path / "measured"),
        derive(tmp_path / "non-comparable", "none"),
        derive(tmp_path / "failed", malformed_case=0),
    ]
    lane_members = members(tmp_path / "unavailable")
    (tmp_path / "unavailable" / lane_members[0]["stdout"]["locator"]).write_bytes(b"tampered")
    records.append(
        axis.derive_lane(
            lane(),
            lane_members,
            FIXTURE["cases"],
            tmp_path / "unavailable",
            fixture_ref={"sha256": "sha256:" + "1" * 64, "bytes": 100},
            capture_ref={"sha256": "sha256:" + "2" * 64, "bytes": 200},
            labels=FIXTURE["labels"],
        )
    )
    for record in records:
        EVIDENCE_VALIDATOR.validate(record)


def test_closed_index_rejects_omission_extra_identity_and_fixture_drift(
    tmp_path: Path,
) -> None:
    records = [
        axis.unavailable_record(
            policy_lane,
            "capture unavailable",
            {"sha256": "sha256:" + "1" * 64, "bytes": 100},
            {"sha256": "sha256:" + "2" * 64, "bytes": 200},
        )
        for policy_lane in POLICY["lanes"]
    ]
    evidence_root = tmp_path / "evidence"
    index_path = evidence_root / "evidence-index.json"
    axis.write_records(records, evidence_root, index_path)
    INDEX_VALIDATOR.validate(json.loads(index_path.read_text()))
    axis.verify_index(evidence_root, index_path, POLICY, "sha256:" + "1" * 64)

    index = json.loads(index_path.read_text())
    for mutate in (
        lambda value: value["records"].pop(),
        lambda value: value["records"].append(value["records"][0]),
        lambda value: value["records"][0].update(candidate="aozora2"),
    ):
        changed = json.loads(json.dumps(index))
        mutate(changed)
        index_path.write_bytes(axis.canonical_bytes(changed))
        with pytest.raises(ValueError):
            axis.verify_index(evidence_root, index_path, POLICY, "sha256:" + "1" * 64)

    axis.write_records(records, evidence_root, index_path)
    with pytest.raises(ValueError, match="fixture"):
        axis.verify_index(evidence_root, index_path, POLICY, "sha256:" + "3" * 64)
