from __future__ import annotations

import json
from dataclasses import dataclass


@dataclass(frozen=True)
class ActualDiagnostic:
    code: str
    severity: str
    span_start: int
    span_end: int


@dataclass(frozen=True)
class ExpectedDiagnostic:
    severity: str
    span_start: int
    span_end: int


@dataclass(frozen=True)
class DiagnosticCaseScore:
    presence: int
    stable_code: int
    severity: int
    relevant_span: int
    false_positive: int
    false_negative: int


DiagnosticMetricCounts = DiagnosticCaseScore


def _load_envelope(payload: bytes, schema_version: int, label: str) -> list[dict]:
    try:
        value = json.loads(payload)
    except (UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValueError(f"invalid JSON: {error}") from error
    if value.get("schemaVersion") != schema_version or not isinstance(value.get("data"), list):
        raise ValueError(f"unsupported {label} envelope")
    return value["data"]


def _project(entries: list[dict], code_key: str) -> list[ActualDiagnostic]:
    projected = []
    for entry in entries:
        try:
            projected.append(
                ActualDiagnostic(
                    code=entry[code_key],
                    severity=entry["severity"],
                    span_start=entry["span"]["start"],
                    span_end=entry["span"]["end"],
                )
            )
        except (KeyError, TypeError) as error:
            raise ValueError(f"entry missing {code_key}/severity/span: {entry!r}") from error
    return projected


def project_schema3_envelope(payload: bytes) -> list[ActualDiagnostic]:
    return _project(_load_envelope(payload, 3, "diagnostics"), "code")


def project_inspect_v2_envelope(payload: bytes) -> list[ActualDiagnostic]:
    return _project(_load_envelope(payload, 2, "inspect"), "kind")


def _overlaps(expected: ExpectedDiagnostic, actual: ActualDiagnostic) -> bool:
    return actual.span_start < expected.span_end and expected.span_start < actual.span_end


def score_case(expected: ExpectedDiagnostic, actual: list[ActualDiagnostic]) -> DiagnosticCaseScore:
    relevant = [item for item in actual if _overlaps(expected, item)]
    allowed_severities = (
        {"warning", "error"} if expected.severity == "warning_or_error" else {expected.severity}
    )
    return DiagnosticCaseScore(
        presence=int(bool(relevant)),
        stable_code=int(any(item.code for item in relevant)),
        severity=int(any(item.severity in allowed_severities for item in relevant)),
        relevant_span=int(
            any(
                item.span_start == expected.span_start and item.span_end == expected.span_end
                for item in relevant
            )
        ),
        false_positive=sum(not _overlaps(expected, item) for item in actual),
        false_negative=int(not relevant),
    )


def aggregate_cases(scores: list[DiagnosticCaseScore]) -> DiagnosticMetricCounts:
    return DiagnosticMetricCounts(
        presence=sum(score.presence for score in scores),
        stable_code=sum(score.stable_code for score in scores),
        severity=sum(score.severity for score in scores),
        relevant_span=sum(score.relevant_span for score in scores),
        false_positive=sum(score.false_positive for score in scores),
        false_negative=sum(score.false_negative for score in scores),
    )
