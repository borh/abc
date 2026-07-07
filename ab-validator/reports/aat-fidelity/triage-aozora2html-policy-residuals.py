#!/usr/bin/env python
"""Build actionable aozora2html policy residual worksets and samples."""

from __future__ import annotations

import argparse
import json
from collections import Counter
from pathlib import Path
from typing import Any

FAMILIES = ("warigaki", "kunten")
MAX_DISPLAY_SOURCE_LINES = 8
RESIDUAL_BUCKETS = (
    "adapter_timeout_or_protocol_error",
    "schema_invalid_or_no_aat",
    "parse_incomplete",
    "report_failed_other_property",
    "source_feature_without_aat_observation",
)
NEXT_ACTION = {
    "adapter_timeout_or_protocol_error": "adapter timeout diagnosis",
    "schema_invalid_or_no_aat": "schema or AAT persistence fix",
    "parse_incomplete": "adapter parse-completeness fix",
    "report_failed_other_property": "adapter-oracle characterization",
    "source_feature_without_aat_observation": "source detector versus adapter observation triage",
}


def read_json(path: Path) -> Any:
    return json.loads(path.read_text())


def write_json(path: Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, ensure_ascii=False, indent=2) + "\n")


def load_work_index(run_dir: Path) -> dict[str, dict[str, Any]]:
    index = read_json(run_dir / "index.json")
    if not isinstance(index, dict):
        raise SystemExit("index.json is not an object")
    works = index.get("works")
    if not isinstance(works, list):
        raise SystemExit("index.json lacks works array")
    out: dict[str, dict[str, Any]] = {}
    for work in works:
        if isinstance(work, dict) and isinstance(work.get("id"), str):
            out[str(work["id"])] = work
    return out


def report_payloads_for_work(work_id: str, run_dir: Path) -> list[tuple[Path, dict[str, Any]]]:
    reports: list[tuple[Path, dict[str, Any]]] = []
    for path in sorted((run_dir / "check-reports").rglob(f"{work_id}*.json")):
        try:
            payload = read_json(path)
        except Exception:
            continue
        if isinstance(payload, dict) and str(payload.get("work_id", "")) == work_id:
            reports.append((path, payload))
    return reports


def aat_paths_for_work(work_id: str, run_dir: Path) -> list[str]:
    paths: list[str] = []
    for path in sorted((run_dir / "aat").rglob(f"{work_id}*.json")):
        try:
            payload = read_json(path)
        except Exception:
            continue
        if isinstance(payload, dict) and str(payload.get("work_id", "")) == work_id:
            paths.append(str(path.relative_to(run_dir)))
    return paths


def aat_payloads_for_work(work_id: str, run_dir: Path) -> list[tuple[Path, dict[str, Any]]]:
    payloads: list[tuple[Path, dict[str, Any]]] = []
    for path in sorted((run_dir / "aat").rglob(f"{work_id}*.json")):
        try:
            payload = read_json(path)
        except Exception:
            continue
        if isinstance(payload, dict) and str(payload.get("work_id", "")) == work_id:
            payloads.append((path, payload))
    return payloads


def syntax_keys_for_work(work_id: str, run_dir: Path) -> list[str]:
    keys: set[str] = set()
    for _path, payload in aat_payloads_for_work(work_id, run_dir):
        syntax = (
            payload.get("meta", {})
            .get("semantic_summary", {})
            .get("syntax", {})
        )
        if isinstance(syntax, dict):
            keys.update(str(key) for key in syntax.keys())
    return sorted(keys)


def failure_properties(work_id: str, run_dir: Path) -> list[str]:
    props: set[str] = set()
    for _path, payload in report_payloads_for_work(work_id, run_dir):
        results = payload.get("results", {}) if isinstance(payload, dict) else {}
        if not isinstance(results, dict):
            continue
        for prop, result in results.items():
            if isinstance(result, dict) and result.get("pass") is False:
                props.add(str(prop))
    return sorted(props)


def first_report_path(work_id: str, run_dir: Path) -> str:
    reports = report_payloads_for_work(work_id, run_dir)
    if not reports:
        return ""
    return str(reports[0][0].relative_to(run_dir))


def evidence_run_for_work(
    *,
    work_id: str,
    run_dir: Path,
    retry_run_dir: Path | None,
) -> tuple[str, Path]:
    if retry_run_dir is not None and (
        report_payloads_for_work(work_id, retry_run_dir)
        or aat_paths_for_work(work_id, retry_run_dir)
    ):
        return "retry", retry_run_dir
    return "baseline", run_dir


def source_lines(family: str, work: dict[str, Any]) -> list[str]:
    feature_lines = work.get("feature_lines", {})
    if not isinstance(feature_lines, dict):
        return []
    features = ("warigaki",) if family == "warigaki" else ("kaeriten", "okurigana")
    out: list[str] = []
    for feature in features:
        raw_lines = feature_lines.get(feature, [])
        if not isinstance(raw_lines, list):
            continue
        prefix = "warigaki" if feature == "warigaki" else f"kunten.{feature}"
        for line in raw_lines:
            out.append(f"{prefix}:{line}")
    return out


def display_source_lines(lines: list[str]) -> str:
    if len(lines) <= MAX_DISPLAY_SOURCE_LINES:
        return ", ".join(lines)
    shown = lines[:MAX_DISPLAY_SOURCE_LINES]
    hidden = len(lines) - MAX_DISPLAY_SOURCE_LINES
    return f"{', '.join(shown)} (+{hidden} more)"


def display_syntax_keys(keys: list[str]) -> str:
    if len(keys) <= MAX_DISPLAY_SOURCE_LINES:
        return ", ".join(keys)
    shown = keys[:MAX_DISPLAY_SOURCE_LINES]
    hidden = len(keys) - MAX_DISPLAY_SOURCE_LINES
    return f"{', '.join(shown)} (+{hidden} more)"


def evidence_note(bucket: str) -> str:
    if bucket == "adapter_timeout_or_protocol_error":
        return "adapter runtime/protocol failure"
    if bucket == "schema_invalid_or_no_aat":
        return "schema or AAT persistence failure"
    if bucket == "parse_incomplete":
        return "parse-completeness failure"
    if bucket == "report_failed_other_property":
        return "AAT present but non-policy property failures need oracle characterization"
    if bucket == "source_feature_without_aat_observation":
        return "valid AAT without family observation; adapter/source-feature mapping candidate"
    raise AssertionError(f"unknown bucket: {bucket}")


def bucket_evidence(
    *,
    work_ids: list[str],
    run_dir: Path,
    retry_run_dir: Path | None,
) -> dict[str, Any]:
    report_present = 0
    aat_present = 0
    clean_reports = 0
    failed_properties: Counter[str] = Counter()
    evidence_runs: Counter[str] = Counter()

    for work_id in work_ids:
        evidence_label, evidence_dir = evidence_run_for_work(
            work_id=work_id,
            run_dir=run_dir,
            retry_run_dir=retry_run_dir,
        )
        evidence_runs[evidence_label] += 1
        reports = report_payloads_for_work(work_id, evidence_dir)
        aat_paths = aat_paths_for_work(work_id, evidence_dir)
        failures = failure_properties(work_id, evidence_dir)
        if reports:
            report_present += 1
        if aat_paths:
            aat_present += 1
        if reports and not failures:
            clean_reports += 1
        failed_properties.update(failures)

    return {
        "works": len(work_ids),
        "report_present": report_present,
        "aat_present": aat_present,
        "clean_reports": clean_reports,
        "failed_properties": dict(sorted(failed_properties.items())),
        "evidence_runs": dict(sorted(evidence_runs.items())),
    }


def sample_row(
    *,
    family: str,
    bucket: str,
    work_id: str,
    run_dir: Path,
    retry_run_dir: Path | None,
    index: dict[str, dict[str, Any]],
) -> dict[str, Any]:
    work = index.get(work_id, {})
    evidence_label, evidence_dir = evidence_run_for_work(
        work_id=work_id,
        run_dir=run_dir,
        retry_run_dir=retry_run_dir,
    )
    aat_paths = aat_paths_for_work(work_id, evidence_dir)
    return {
        "family": family,
        "bucket": bucket,
        "work_id": work_id,
        "evidence_run": evidence_label,
        "txt_path": str(work.get("txt_path", "")) if isinstance(work, dict) else "",
        "html_path": str(work.get("html_path", "")) if isinstance(work, dict) else "",
        "source_lines": source_lines(family, work) if isinstance(work, dict) else [],
        "report_path": first_report_path(work_id, evidence_dir),
        "aat_path": aat_paths[0] if aat_paths else "",
        "syntax_keys": syntax_keys_for_work(work_id, evidence_dir),
        "failure_properties": failure_properties(work_id, evidence_dir),
        "next_action": NEXT_ACTION[bucket],
    }


def markdown_table(rows: list[dict[str, Any]]) -> list[str]:
    if not rows:
        return ["_No samples._", ""]
    lines = [
        "| Work ID | Run | Source Lines | Syntax Keys | Failures | Report | AAT | Next action |",
        "|---|---|---|---|---|---|---|---|",
    ]
    for row in rows:
        lines.append(
            "| {work_id} | {evidence_run} | {source_lines} | {syntax_keys} | {failures} | `{report}` | `{aat}` | {next_action} |".format(
                work_id=row["work_id"],
                evidence_run=row["evidence_run"],
                source_lines=display_source_lines(row["source_lines"]),
                syntax_keys=display_syntax_keys(row["syntax_keys"]),
                failures=", ".join(row["failure_properties"]),
                report=row["report_path"],
                aat=row["aat_path"],
                next_action=row["next_action"],
            )
        )
    lines.append("")
    return lines


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--run-dir", type=Path, required=True)
    parser.add_argument("--audit-summary-json", type=Path, required=True)
    parser.add_argument("--out-md", type=Path, required=True)
    parser.add_argument("--summary-json", type=Path, required=True)
    parser.add_argument("--worksets-dir", type=Path, required=True)
    parser.add_argument("--limit-per-bucket", type=int, default=10)
    args = parser.parse_args()

    if args.limit_per_bucket <= 0:
        raise SystemExit("--limit-per-bucket must be positive")

    run_dir = args.run_dir.resolve()
    summary = read_json(args.audit_summary_json)
    retry_run_dir = None
    retry = summary.get("retry")
    if isinstance(retry, dict) and isinstance(retry.get("run_dir"), str):
        retry_run_dir = Path(retry["run_dir"]).resolve()
    buckets = summary.get("buckets", {})
    if not isinstance(buckets, dict):
        raise SystemExit("audit summary lacks buckets object")
    index = load_work_index(run_dir)

    args.worksets_dir.mkdir(parents=True, exist_ok=True)
    residual_sets: dict[str, dict[str, list[str]]] = {}
    residual_union: set[str] = set()
    sample_rows: dict[str, dict[str, list[dict[str, Any]]]] = {}
    bucket_counts: dict[str, dict[str, int]] = {}
    bucket_evidence_summary: dict[str, dict[str, dict[str, Any]]] = {}

    for family in FAMILIES:
        family_buckets = buckets.get(family, {})
        if not isinstance(family_buckets, dict):
            raise SystemExit(f"audit summary lacks buckets.{family}")
        residual_sets[family] = {}
        sample_rows[family] = {}
        bucket_counts[family] = {}
        bucket_evidence_summary[family] = {}
        for bucket in RESIDUAL_BUCKETS:
            work_ids = family_buckets.get(bucket, [])
            if not isinstance(work_ids, list):
                raise SystemExit(f"bucket is not a list: {family}.{bucket}")
            sorted_ids = sorted({str(work_id) for work_id in work_ids})
            residual_sets[family][bucket] = sorted_ids
            bucket_counts[family][bucket] = len(sorted_ids)
            bucket_evidence_summary[family][bucket] = bucket_evidence(
                work_ids=sorted_ids,
                run_dir=run_dir,
                retry_run_dir=retry_run_dir,
            )
            residual_union.update(sorted_ids)
            write_json(args.worksets_dir / f"{family}-{bucket}.json", sorted_ids)
            sample_rows[family][bucket] = [
                sample_row(
                    family=family,
                    bucket=bucket,
                    work_id=work_id,
                    run_dir=run_dir,
                    retry_run_dir=retry_run_dir,
                    index=index,
                )
                for work_id in sorted_ids[: args.limit_per_bucket]
            ]

    residual_union_list = sorted(residual_union)
    write_json(args.worksets_dir / "policy-residual-union.json", residual_union_list)

    lines = [
        "# Aozora2html Policy Residual Triage",
        "",
        f"- run_dir: `{run_dir}`",
        f"- retry_run_dir: `{retry_run_dir}`" if retry_run_dir is not None else "- retry_run_dir: null",
        f"- audit_summary: `{args.audit_summary_json}`",
        f"- residual_union_count: {len(residual_union_list)}",
        "",
        "## Residual Bucket Counts",
        "",
        "| Family | Bucket | Works | Next action |",
        "|---|---|---:|---|",
    ]
    for family in FAMILIES:
        for bucket in RESIDUAL_BUCKETS:
            lines.append(
                f"| {family} | {bucket} | {bucket_counts[family][bucket]} | {NEXT_ACTION[bucket]} |"
            )
    lines.extend(
        [
            "",
            "## Residual Evidence Summary",
            "",
            "| Family | Bucket | Works | Reports | AAT | Clean reports | Failed properties | Evidence runs | Evidence note |",
            "|---|---|---:|---:|---:|---:|---|---|---|",
        ]
    )
    for family in FAMILIES:
        for bucket in RESIDUAL_BUCKETS:
            evidence = bucket_evidence_summary[family][bucket]
            failures = ", ".join(
                f"{prop}:{count}"
                for prop, count in evidence["failed_properties"].items()
            )
            runs = ", ".join(
                f"{label}:{count}"
                for label, count in evidence["evidence_runs"].items()
            )
            lines.append(
                "| {family} | {bucket} | {works} | {reports} | {aat} | {clean} | {failures} | {runs} | {note} |".format(
                    family=family,
                    bucket=bucket,
                    works=evidence["works"],
                    reports=evidence["report_present"],
                    aat=evidence["aat_present"],
                    clean=evidence["clean_reports"],
                    failures=failures,
                    runs=runs,
                    note=evidence_note(bucket),
                )
            )
    lines.extend(["", "## Samples", ""])
    for family in FAMILIES:
        for bucket in RESIDUAL_BUCKETS:
            rows = sample_rows[family][bucket]
            if not rows:
                continue
            lines.extend([f"### {family} {bucket}", ""])
            lines.append(f"Next action: {NEXT_ACTION[bucket]}")
            lines.append("")
            lines.extend(markdown_table(rows))

    args.out_md.parent.mkdir(parents=True, exist_ok=True)
    args.out_md.write_text("\n".join(lines))
    output_summary = {
        "run_dir": str(run_dir),
        "retry_run_dir": str(retry_run_dir) if retry_run_dir is not None else None,
        "audit_summary_json": str(args.audit_summary_json),
        "worksets_dir": str(args.worksets_dir),
        "bucket_counts": bucket_counts,
        "bucket_evidence": bucket_evidence_summary,
        "residual_union_count": len(residual_union_list),
        "worksets": {
            **{
                f"{family}.{bucket}": str(args.worksets_dir / f"{family}-{bucket}.json")
                for family in FAMILIES
                for bucket in RESIDUAL_BUCKETS
            },
            "policy_residual_union": str(args.worksets_dir / "policy-residual-union.json"),
        },
        "next_actions": NEXT_ACTION,
    }
    write_json(args.summary_json, output_summary)
    print(f"wrote residual triage report: {args.out_md}")
    print(f"wrote residual triage summary: {args.summary_json}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
