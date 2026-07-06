#!/usr/bin/env python3
"""Extract compact aozora2html warigaki/kunten policy evidence samples."""

from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Any

FAMILIES = ("warigaki", "kunten")
BUCKETS = (
    "observed_in_aat",
    "adapter_timeout_or_protocol_error",
    "schema_invalid_or_no_aat",
    "parse_incomplete",
    "report_failed_other_property",
    "source_feature_without_aat_observation",
)
SECTION_TITLES = {
    "observed_in_aat": {
        "warigaki": "Observed warigaki",
        "kunten": "Observed kunten",
    },
    "source_feature_without_aat_observation": {
        "warigaki": "Warigaki source feature without AAT observation",
        "kunten": "Kunten source feature without AAT observation",
    },
}
CHILD_KEYS = {"content", "children", "base_content", "reading_content", "upper", "lower"}


def read_json(path: Path) -> Any:
    return json.loads(path.read_text())


def write_json(path: Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, ensure_ascii=False, indent=2) + "\n")


def walk_nodes(value: Any, path: str = "$") -> list[tuple[str, dict[str, Any]]]:
    out: list[tuple[str, dict[str, Any]]] = []
    if isinstance(value, dict):
        out.append((path, value))
        for key, child in value.items():
            out.extend(walk_nodes(child, f"{path}.{key}"))
    elif isinstance(value, list):
        for index, child in enumerate(value):
            out.extend(walk_nodes(child, f"{path}[{index}]"))
    return out


def is_kunten_node(node: dict[str, Any]) -> bool:
    return (
        node.get("style_type") in {"kaeriten", "okurigana"}
        or (
            node.get("kind") == "ruby"
            and node.get("x-annotation-type") == "okurigana"
        )
    )


def preview_node(node: dict[str, Any]) -> dict[str, Any]:
    preview: dict[str, Any] = {}
    for key, value in node.items():
        if key in CHILD_KEYS:
            continue
        if isinstance(value, (dict, list)):
            continue
        preview[key] = value
    return preview


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


def first_report_path(work_id: str, run_dir: Path) -> str:
    reports = report_payloads_for_work(work_id, run_dir)
    if not reports:
        return ""
    return str(reports[0][0].relative_to(run_dir))


def aat_paths_for_work(work_id: str, run_dir: Path) -> list[Path]:
    paths: list[Path] = []
    for path in sorted((run_dir / "aat").rglob(f"{work_id}*.json")):
        try:
            payload = read_json(path)
        except Exception:
            continue
        if isinstance(payload, dict) and str(payload.get("work_id", "")) == work_id:
            paths.append(path)
    return paths


def failed_properties(summary: dict[str, Any], work_id: str) -> list[str]:
    props: list[str] = []
    for family_buckets in summary.get("failure_overlap_work_ids", {}).values():
        if not isinstance(family_buckets, dict):
            continue
        for prop, work_ids in family_buckets.items():
            if isinstance(work_ids, list) and work_id in work_ids:
                props.append(str(prop))
    return sorted(set(props))


def load_report_failed_properties(work_id: str, run_dir: Path) -> list[str]:
    props: list[str] = []
    for _path, payload in report_payloads_for_work(work_id, run_dir):
        results = payload.get("results", {}) if isinstance(payload, dict) else {}
        if not isinstance(results, dict):
            continue
        for prop, result in results.items():
            if isinstance(result, dict) and result.get("pass") is False:
                props.append(str(prop))
    return sorted(set(props))


def find_observed_sample(
    *,
    family: str,
    work_id: str,
    run_dir: Path,
) -> dict[str, Any]:
    for path in aat_paths_for_work(work_id, run_dir):
        aat = read_json(path)
        for node_path, node in walk_nodes(aat):
            if family == "warigaki" and node.get("kind") == "warigaki":
                return {
                    "aat_path": str(path.relative_to(run_dir)),
                    "node_path": node_path,
                    "node_kind": str(node.get("kind", "")),
                    "syntax_id": "",
                    "node_preview": preview_node(node),
                }
            if family == "kunten" and is_kunten_node(node):
                syntax_suffix = (
                    node.get("style_type")
                    if node.get("style_type") in {"kaeriten", "okurigana"}
                    else node.get("x-annotation-type")
                )
                return {
                    "aat_path": str(path.relative_to(run_dir)),
                    "node_path": node_path,
                    "node_kind": str(node.get("kind", "")),
                    "syntax_id": f"kunten.{syntax_suffix}",
                    "node_preview": preview_node(node),
                }
        syntax = (
            aat.get("meta", {})
            .get("semantic_summary", {})
            .get("syntax", {})
        )
        if family == "kunten" and isinstance(syntax, dict):
            for syntax_id, observations in sorted(syntax.items()):
                if not str(syntax_id).startswith("kunten."):
                    continue
                first = observations[0] if isinstance(observations, list) and observations else {}
                return {
                    "aat_path": str(path.relative_to(run_dir)),
                    "node_path": "meta.semantic_summary.syntax",
                    "node_kind": str(first.get("kind", "")) if isinstance(first, dict) else "",
                    "syntax_id": str(syntax_id),
                    "node_preview": first if isinstance(first, dict) else {},
                }
    return {
        "aat_path": "",
        "node_path": "",
        "node_kind": "",
        "syntax_id": "",
        "node_preview": {},
    }


def has_observed_sample(observed: dict[str, Any]) -> bool:
    return bool(observed["aat_path"])


def sample_evidence(
    *,
    family: str,
    bucket: str,
    work_id: str,
    run_dir: Path,
    retry_run_dir: Path | None,
) -> tuple[str, Path, dict[str, Any]]:
    if bucket == "observed_in_aat":
        observed = find_observed_sample(family=family, work_id=work_id, run_dir=run_dir)
        if has_observed_sample(observed):
            return "baseline", run_dir, observed
        if retry_run_dir is not None:
            retry_observed = find_observed_sample(
                family=family,
                work_id=work_id,
                run_dir=retry_run_dir,
            )
            if has_observed_sample(retry_observed):
                return "retry", retry_run_dir, retry_observed
        return "baseline", run_dir, observed

    evidence_label = "baseline"
    evidence_dir = run_dir
    if retry_run_dir is not None and report_payloads_for_work(work_id, retry_run_dir):
        evidence_label = "retry"
        evidence_dir = retry_run_dir
    paths = aat_paths_for_work(work_id, evidence_dir)
    return evidence_label, evidence_dir, {
        "aat_path": str(paths[0].relative_to(evidence_dir)) if paths else "",
        "node_path": "",
        "node_kind": "",
        "syntax_id": "",
        "node_preview": {},
    }


def sample_row(
    *,
    family: str,
    bucket: str,
    work_id: str,
    run_dir: Path,
    retry_run_dir: Path | None,
) -> dict[str, Any]:
    evidence_label, evidence_dir, observed = sample_evidence(
        family=family,
        bucket=bucket,
        work_id=work_id,
        run_dir=run_dir,
        retry_run_dir=retry_run_dir,
    )
    return {
        "family": family,
        "bucket": bucket,
        "work_id": work_id,
        "evidence_run": evidence_label,
        "report_path": first_report_path(work_id, evidence_dir),
        "aat_path": observed["aat_path"],
        "node_path": observed["node_path"],
        "node_kind": observed["node_kind"],
        "syntax_id": observed["syntax_id"],
        "node_preview": observed["node_preview"],
        "failure_properties": load_report_failed_properties(work_id, evidence_dir),
    }


def sample_bucket(
    *,
    family: str,
    bucket: str,
    work_ids: list[str],
    run_dir: Path,
    retry_run_dir: Path | None,
    limit: int,
) -> list[dict[str, Any]]:
    return [
        sample_row(
            family=family,
            bucket=bucket,
            work_id=work_id,
            run_dir=run_dir,
            retry_run_dir=retry_run_dir,
        )
        for work_id in sorted(work_ids)[:limit]
    ]


def markdown_table(rows: list[dict[str, Any]]) -> list[str]:
    if not rows:
        return ["_No samples._", ""]
    lines = [
        "| Work ID | Run | Report | AAT | Node Path | Kind | Syntax | Failures | Preview |",
        "|---|---|---|---|---|---|---|---|---|",
    ]
    for row in rows:
        preview = json.dumps(row["node_preview"], ensure_ascii=False, sort_keys=True)
        if len(preview) > 180:
            preview = preview[:177] + "..."
        failures = ", ".join(row["failure_properties"])
        lines.append(
            "| {work_id} | {run} | `{report}` | `{aat}` | `{node_path}` | {kind} | {syntax} | {failures} | `{preview}` |".format(
                work_id=row["work_id"],
                run=row["evidence_run"],
                report=row["report_path"],
                aat=row["aat_path"],
                node_path=row["node_path"],
                kind=row["node_kind"],
                syntax=row["syntax_id"],
                failures=failures,
                preview=preview.replace("|", "\\|"),
            )
        )
    lines.append("")
    return lines


def section_title(family: str, bucket: str) -> str:
    if bucket in SECTION_TITLES:
        return SECTION_TITLES[bucket][family]
    return f"{family} {bucket}"


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--run-dir", type=Path, required=True)
    parser.add_argument("--audit-md", type=Path, required=True)
    parser.add_argument("--audit-summary-json", type=Path, required=True)
    parser.add_argument("--out-md", type=Path, required=True)
    parser.add_argument("--summary-json", type=Path, required=True)
    parser.add_argument("--limit-per-bucket", type=int, default=10)
    args = parser.parse_args()

    if args.limit_per_bucket <= 0:
        raise SystemExit("--limit-per-bucket must be positive")
    if not args.audit_md.exists():
        raise SystemExit(f"missing audit markdown: {args.audit_md}")

    run_dir = args.run_dir.resolve()
    summary = read_json(args.audit_summary_json)
    retry_run_dir = None
    retry = summary.get("retry")
    if isinstance(retry, dict) and isinstance(retry.get("run_dir"), str):
        retry_run_dir = Path(retry["run_dir"]).resolve()
    buckets = summary.get("buckets", {})
    if not isinstance(buckets, dict):
        raise SystemExit("audit summary lacks buckets object")

    samples: dict[str, dict[str, list[dict[str, Any]]]] = {}
    sample_counts: dict[str, dict[str, int]] = {}
    for family in FAMILIES:
        family_buckets = buckets.get(family, {})
        if not isinstance(family_buckets, dict):
            raise SystemExit(f"audit summary lacks buckets.{family}")
        samples[family] = {}
        sample_counts[family] = {}
        for bucket in BUCKETS:
            work_ids = family_buckets.get(bucket, [])
            if not isinstance(work_ids, list):
                raise SystemExit(f"audit summary bucket is not a list: {family}.{bucket}")
            rows = sample_bucket(
                family=family,
                bucket=bucket,
                work_ids=[str(work_id) for work_id in work_ids],
                run_dir=run_dir,
                retry_run_dir=retry_run_dir,
                limit=args.limit_per_bucket,
            )
            samples[family][bucket] = rows
            sample_counts[family][bucket] = len(rows)

    lines = [
        "# Aozora2html Policy Samples",
        "",
        f"- run_dir: `{run_dir}`",
        f"- retry_run_dir: `{retry_run_dir}`" if retry_run_dir is not None else "- retry_run_dir: null",
        f"- audit: `{args.audit_md}`",
        f"- limit_per_bucket: {args.limit_per_bucket}",
        "",
    ]
    for family in FAMILIES:
        for bucket in BUCKETS:
            rows = samples[family][bucket]
            if not rows and bucket not in {"observed_in_aat", "source_feature_without_aat_observation"}:
                continue
            lines.extend([f"## {section_title(family, bucket)}", ""])
            lines.extend(markdown_table(rows))

    args.out_md.parent.mkdir(parents=True, exist_ok=True)
    args.out_md.write_text("\n".join(lines))

    output_summary = {
        "run_dir": str(run_dir),
        "retry_run_dir": str(retry_run_dir) if retry_run_dir is not None else None,
        "sample_counts": sample_counts,
        "families": list(FAMILIES),
        "buckets_sampled": list(BUCKETS),
        "limit_per_bucket": args.limit_per_bucket,
    }
    write_json(args.summary_json, output_summary)
    print(f"wrote policy samples: {args.out_md}")
    print(f"wrote policy sample summary: {args.summary_json}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
