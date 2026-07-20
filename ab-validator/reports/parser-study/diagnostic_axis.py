#!/usr/bin/env python3
"""Derive authenticated diagnostic-axis evidence from the bounded raw capture."""

from __future__ import annotations

import argparse
import hashlib
import importlib.util
import json
import sys
from pathlib import Path, PurePosixPath
from typing import Any, Callable


DIAGNOSTIC_METRICS = [
    "diagnostic_presence",
    "stable_code",
    "severity",
    "relevant_span",
    "false_positive",
    "false_negative",
]


def _load_module(name: str, path: Path) -> Any:
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"cannot load {path}")
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


REPORTS = Path(__file__).parents[1]
SCORING = _load_module(
    "parser_study_diagnostics_scoring",
    REPORTS / "parser-conformance" / "diagnostics_scoring.py",
)
CAPTURE = _load_module(
    "parser_study_diagnostic_capture", Path(__file__).with_name("diagnostic_capture.py")
)


def sha256(data: bytes) -> str:
    return "sha256:" + hashlib.sha256(data).hexdigest()


def canonical_bytes(value: Any) -> bytes:
    return (
        json.dumps(value, ensure_ascii=False, sort_keys=True, separators=(",", ":")) + "\n"
    ).encode()


def _file_identity(path: Path) -> dict[str, Any]:
    digest = hashlib.sha256()
    size = 0
    with path.open("rb") as handle:
        while chunk := handle.read(1024 * 1024):
            digest.update(chunk)
            size += len(chunk)
    return {
        "sha256": "sha256:" + digest.hexdigest(),
        "bytes": size,
        "media_type": "application/json",
    }


def _atomic_write(path: Path, data: bytes) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(path.name + ".tmp")
    temporary.write_bytes(data)
    temporary.replace(path)


def _input(
    role: str,
    artifact: str,
    locator: str,
    content_ref: dict[str, Any] | None,
    reason: str,
) -> dict[str, Any]:
    if content_ref is None:
        return {"role": role, "artifact": artifact, "state": "absent", "reason": reason}
    return {
        "role": role,
        "artifact": artifact,
        "state": "present",
        "content_ref": {
            "sha256": content_ref["sha256"],
            "bytes": content_ref["bytes"],
            "media_type": "application/json",
        },
        "locator": locator,
    }


def _base_record(
    lane: dict[str, Any],
    fixture_ref: dict[str, Any],
    capture_ref: dict[str, Any] | None,
    capture_reason: str,
) -> dict[str, Any]:
    return {
        "schema_id": "https://w3id.org/abc/schemas/parser-study-axis-evidence-v1",
        "schema_version": 1,
        "study_id": "aozora-parser-neutral-comparison-2026-07",
        "candidate": lane["candidate"],
        "axis": "diagnostics",
        "measurement_mode": lane["measurement_mode"],
        "parser_revision": lane["parser_revision"],
        "adapter_revision": lane["adapter_revision"],
        "corpus_hash": fixture_ref["sha256"],
        "required_inputs": [
            _input(
                "source_markup",
                "diagnostic_fixture",
                "docs/studies/fixtures/parser-comparison-diagnostics-v1.json",
                fixture_ref,
                "diagnostic fixture unavailable",
            ),
            _input(
                "third_party_capture",
                "diagnostic_fixture_capture",
                "capture-manifest.json",
                capture_ref,
                capture_reason,
            ),
        ],
        "metrics": [],
        "case_witnesses": [],
    }


def _disposition_metrics(disposition: str, reason: str) -> list[dict[str, Any]]:
    return [
        {"metric": metric, "disposition": disposition, "reason": reason}
        for metric in DIAGNOSTIC_METRICS
    ]


def unavailable_record(
    lane: dict[str, Any],
    reason: str,
    fixture_ref: dict[str, Any],
    capture_ref: dict[str, Any] | None = None,
) -> dict[str, Any]:
    record = _base_record(lane, fixture_ref, capture_ref, reason)
    record["metrics"] = _disposition_metrics("unavailable", reason)
    return record


def _read_payload(bundle_root: Path, content_ref: Any) -> bytes:
    if not isinstance(content_ref, dict):
        raise ValueError("missing stdout content reference")
    locator = content_ref.get("locator")
    if not isinstance(locator, str) or not CAPTURE.safe_relative(locator):
        raise ValueError("unsafe stdout locator")
    root = bundle_root.resolve()
    path = (bundle_root / locator).resolve()
    if not path.is_relative_to(root) or not path.is_file():
        raise ValueError("missing stdout member")
    data = path.read_bytes()
    if sha256(data) != content_ref.get("sha256") or len(data) != content_ref.get("bytes"):
        raise ValueError("stdout content mismatch")
    return data


def _witness(member: dict[str, Any], status: str, diagnostics: list[Any] | None) -> dict[str, Any]:
    return {
        "case_id": member["case_id"],
        "process_status": member["status"],
        "returncode": member["returncode"],
        "projection_status": status,
        "diagnostic_count": None if diagnostics is None else len(diagnostics),
        "relevant_diagnostic_count": None,
        "false_positive_count": None,
    }


def _projector(name: str) -> Callable[[bytes], list[Any]]:
    if name == "schema3":
        return SCORING.project_schema3_envelope
    if name == "inspect_v2":
        return SCORING.project_inspect_v2_envelope
    raise ValueError(f"unsupported projector: {name}")


def derive_lane(
    lane: dict[str, Any],
    members: list[dict[str, Any]],
    cases: list[dict[str, Any]],
    bundle_root: Path,
    *,
    fixture_ref: dict[str, Any],
    capture_ref: dict[str, Any],
    labels: dict[str, Any] | None = None,
) -> dict[str, Any]:
    expected_keys = [(lane["candidate"], lane["measurement_mode"], case["id"]) for case in cases]
    observed_keys = [
        (member.get("candidate"), member.get("measurement_mode"), member.get("case_id"))
        for member in members
    ]
    if observed_keys != expected_keys:
        return unavailable_record(lane, "capture membership mismatch", fixture_ref)
    try:
        payloads = [_read_payload(bundle_root, member.get("stdout")) for member in members]
    except ValueError as error:
        return unavailable_record(lane, str(error), fixture_ref)

    if lane["projector"] == "none":
        for payload in payloads:
            for probe in (
                SCORING.project_schema3_envelope,
                SCORING.project_inspect_v2_envelope,
            ):
                try:
                    probe(payload)
                except ValueError:
                    continue
                record = _base_record(lane, fixture_ref, capture_ref, "capture unavailable")
                record["metrics"] = _disposition_metrics(
                    "unavailable", "stale lane policy suppresses a known structured envelope"
                )
                record["case_witnesses"] = [
                    _witness(member, "not_configured", None) for member in members
                ]
                return record
        record = _base_record(lane, fixture_ref, capture_ref, "capture unavailable")
        record["metrics"] = _disposition_metrics(
            "non_comparable", "lane has no governed structured diagnostic channel"
        )
        record["case_witnesses"] = [_witness(member, "not_configured", None) for member in members]
        return record

    project = _projector(lane["projector"])
    projected: list[list[Any]] = []
    witnesses: list[dict[str, Any]] = []
    for member, payload in zip(members, payloads, strict=True):
        try:
            diagnostics = project(payload)
        except ValueError:
            witnesses.append(_witness(member, "invalid", None))
            record = _base_record(lane, fixture_ref, capture_ref, "capture unavailable")
            record["metrics"] = _disposition_metrics(
                "failed", "at least one case lacks a usable structured diagnostic envelope"
            )
            record["case_witnesses"] = [
                *witnesses,
                *[_witness(remaining, "invalid", None) for remaining in members[len(witnesses) :]],
            ]
            return record
        projected.append(diagnostics)
        witnesses.append(_witness(member, "usable", diagnostics))

    scores = []
    for case, diagnostics, witness in zip(cases, projected, witnesses, strict=True):
        severity = (
            labels[case["expected_label"]]["severity"]
            if labels is not None
            else (
                "warning_or_error"
                if case["expected_label"] in {"orphan_ruby_close", "invalid_gaiji_coordinate"}
                else "error"
            )
        )
        expected = SCORING.ExpectedDiagnostic(
            severity=severity,
            span_start=case["expected_span_utf8"]["start"],
            span_end=case["expected_span_utf8"]["end"],
        )
        score = SCORING.score_case(expected, diagnostics)
        scores.append(score)
        witness["relevant_diagnostic_count"] = sum(
            diagnostic.span_start < expected.span_end and expected.span_start < diagnostic.span_end
            for diagnostic in diagnostics
        )
        witness["false_positive_count"] = score.false_positive
    totals = SCORING.aggregate_cases(scores)
    values = {
        "diagnostic_presence": totals.presence,
        "stable_code": totals.stable_code,
        "severity": totals.severity,
        "relevant_span": totals.relevant_span,
        "false_positive": totals.false_positive,
        "false_negative": totals.false_negative,
    }
    record = _base_record(lane, fixture_ref, capture_ref, "capture unavailable")
    record["metrics"] = [
        {
            "metric": metric,
            "disposition": "measured",
            "value": {"kind": "ratio", "numerator": values[metric], "denominator": len(cases)},
        }
        for metric in DIAGNOSTIC_METRICS
    ]
    record["case_witnesses"] = witnesses
    return record


def write_records(
    records: list[dict[str, Any]], output_root: Path, index_path: Path
) -> dict[str, Any]:
    entries = []
    for record in records:
        relative = Path("diagnostics") / (
            f"{record['candidate']}--{record['measurement_mode']}.json"
        )
        data = canonical_bytes(record)
        _atomic_write(output_root / relative, data)
        entries.append(
            {
                "candidate": record["candidate"],
                "axis": "diagnostics",
                "measurement_mode": record["measurement_mode"],
                "record_ref": {
                    "sha256": sha256(data),
                    "bytes": len(data),
                    "media_type": "application/json",
                },
                "locator": relative.as_posix(),
            }
        )
    index = {
        "schema_id": "https://w3id.org/abc/schemas/parser-study-evidence-index-v1",
        "schema_version": 1,
        "study_id": "aozora-parser-neutral-comparison-2026-07",
        "records": entries,
    }
    _atomic_write(index_path, canonical_bytes(index))
    return index


def _safe_evidence_path(root: Path, locator: Any) -> Path:
    if not isinstance(locator, str):
        raise ValueError("invalid evidence locator")
    pure = PurePosixPath(locator)
    if pure.is_absolute() or any(part in ("", ".", "..") for part in pure.parts):
        raise ValueError("unsafe evidence locator")
    resolved_root = root.resolve()
    path = (root / locator).resolve()
    if not path.is_relative_to(resolved_root) or not path.is_file():
        raise ValueError("missing or escaping evidence record")
    return path


def verify_index(
    evidence_root: Path,
    index_path: Path,
    policy: dict[str, Any] | None,
    fixture_hash: str | None,
) -> dict[str, Any]:
    index = json.loads(index_path.read_bytes())
    lane_pairs = (
        [(lane["candidate"], lane["measurement_mode"]) for lane in policy["lanes"]]
        if policy is not None
        else list(CAPTURE.EXPECTED_LANES)
    )
    expected = [(candidate, "diagnostics", mode) for candidate, mode in lane_pairs]
    observed = [
        (entry.get("candidate"), entry.get("axis"), entry.get("measurement_mode"))
        for entry in index.get("records", [])
    ]
    if observed != expected:
        raise ValueError("evidence index is not the closed diagnostic lane set")
    for entry in index["records"]:
        path = _safe_evidence_path(evidence_root, entry.get("locator"))
        identity = _file_identity(path)
        if {key: identity[key] for key in ("sha256", "bytes", "media_type")} != entry.get(
            "record_ref"
        ):
            raise ValueError("evidence record content mismatch")
        record = json.loads(path.read_bytes())
        if (
            record.get("candidate") != entry["candidate"]
            or record.get("axis") != "diagnostics"
            or record.get("measurement_mode") != entry["measurement_mode"]
        ):
            raise ValueError("evidence record identity mismatch")
        source_input = next(
            value
            for value in record.get("required_inputs", [])
            if value.get("artifact") == "diagnostic_fixture"
        )
        if record.get("corpus_hash") != source_input.get("content_ref", {}).get("sha256"):
            raise ValueError("record corpus hash does not authenticate its fixture input")
        if fixture_hash is not None and record.get("corpus_hash") != fixture_hash:
            raise ValueError("diagnostic fixture hash mismatch")
    return index


def derive_all(args: argparse.Namespace) -> None:
    preregistration = json.loads(args.preregistration.read_bytes())
    fixture = json.loads(args.fixture.read_bytes())
    policy = json.loads(args.lane_policy.read_bytes())
    fixture_ref = _file_identity(args.fixture)
    expected_hash = preregistration["fixture_manifests"]["diagnostics"]["sha256"]
    if fixture_ref["sha256"] != expected_hash:
        raise ValueError("diagnostic fixture does not match preregistration")
    capture_ref: dict[str, Any] | None = None
    failure: str | None = None
    try:
        manifest = CAPTURE.verify(
            args.fixture,
            args.lane_policy,
            args.bundle_root,
            args.capture_manifest,
        )
        capture_ref = _file_identity(args.capture_manifest)
    except (OSError, KeyError, TypeError, ValueError, json.JSONDecodeError) as error:
        manifest = {"members": []}
        failure = f"capture authentication failed: {error}"
    records = []
    for lane in policy["lanes"]:
        lane_members = [
            member
            for member in manifest["members"]
            if member.get("candidate") == lane["candidate"]
            and member.get("measurement_mode") == lane["measurement_mode"]
        ]
        if failure is not None or capture_ref is None:
            records.append(unavailable_record(lane, failure or "capture unavailable", fixture_ref))
        else:
            records.append(
                derive_lane(
                    lane,
                    lane_members,
                    fixture["cases"],
                    args.bundle_root,
                    fixture_ref=fixture_ref,
                    capture_ref=capture_ref,
                    labels=fixture["labels"],
                )
            )
    write_records(records, args.output_root, args.output_root / "evidence-index.json")


def verify_all(args: argparse.Namespace) -> None:
    capture_manifest = json.loads((args.bundle_root / "capture-manifest.json").read_bytes())
    fixture_hash = capture_manifest["fixture_ref"]["sha256"]
    index = verify_index(args.evidence_root, args.index, None, fixture_hash)
    for entry in index["records"]:
        record = json.loads((args.evidence_root / entry["locator"]).read_bytes())
        capture_input = next(
            value
            for value in record["required_inputs"]
            if value["artifact"] == "diagnostic_fixture_capture"
        )
        if capture_input["state"] == "present":
            path = _safe_evidence_path(args.bundle_root, capture_input["locator"])
            identity = _file_identity(path)
            if identity["sha256"] != capture_input["content_ref"]["sha256"]:
                raise ValueError("capture manifest content mismatch")


def main() -> None:
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest="command", required=True)
    derive = subparsers.add_parser("derive")
    derive.add_argument("--preregistration", type=Path, required=True)
    derive.add_argument("--fixture", type=Path, required=True)
    derive.add_argument("--lane-policy", type=Path, required=True)
    derive.add_argument("--bundle-root", type=Path, required=True)
    derive.add_argument("--capture-manifest", type=Path, required=True)
    derive.add_argument("--output-root", type=Path, required=True)
    verify = subparsers.add_parser("verify")
    verify.add_argument("--bundle-root", type=Path, required=True)
    verify.add_argument("--evidence-root", type=Path, required=True)
    verify.add_argument("--index", type=Path, required=True)
    args = parser.parse_args()
    if args.command == "derive":
        derive_all(args)
    else:
        verify_all(args)


if __name__ == "__main__":
    main()
