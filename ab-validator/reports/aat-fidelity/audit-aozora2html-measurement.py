#!/usr/bin/env python
"""Audit aozora2html full-corpus measurement trust for policy decisions."""

from __future__ import annotations

import argparse
import json
import os
import subprocess
from collections import Counter, defaultdict
from pathlib import Path
from typing import Any

BUCKETS = [
    "observed_in_aat",
    "adapter_timeout_or_protocol_error",
    "schema_invalid_or_no_aat",
    "parse_incomplete",
    "report_failed_other_property",
    "source_feature_without_aat_observation",
]
INCOMPLETE_BUCKETS = {
    "adapter_timeout_or_protocol_error",
    "schema_invalid_or_no_aat",
    "parse_incomplete",
}
POLICY_RELEVANT_RESIDUAL_BUCKETS = INCOMPLETE_BUCKETS | {
    "report_failed_other_property",
}
ADAPTER_ERROR_PROPERTIES = {
    "adapter_timeout",
    "adapter_protocol_error",
    "fatal_error",
}


def read_json(path: Path) -> Any:
    return json.loads(path.read_text())


def write_json(path: Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, ensure_ascii=False, indent=2) + "\n")


def sql_literal(value: str) -> str:
    return "'" + value.replace("'", "''") + "'"


def duckdb_json(db_path: Path, sql: str) -> list[dict[str, Any]]:
    duckdb_bin = os.environ.get("AB_DUCKDB_BIN") or os.environ.get("DUCKDB") or "duckdb"
    completed = subprocess.run(
        [duckdb_bin, "-json", str(db_path), sql],
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    if completed.returncode != 0:
        raise SystemExit(
            f"duckdb query failed for {db_path}:\n{completed.stderr.strip()}"
        )
    if not completed.stdout.strip():
        return []
    payload = json.loads(completed.stdout)
    if not isinstance(payload, list):
        raise SystemExit(f"duckdb query did not return a JSON array: {sql}")
    return payload


def duckdb_bool(value: Any) -> bool | None:
    if value is None:
        return None
    if isinstance(value, bool):
        return value
    if isinstance(value, str):
        if value.lower() == "true":
            return True
        if value.lower() == "false":
            return False
    raise SystemExit(f"unexpected DuckDB boolean value: {value!r}")


def load_metadata(run_dir: Path) -> dict[str, Any]:
    metadata_path = run_dir / "metadata.json"
    if not metadata_path.exists():
        raise SystemExit(f"missing metadata.json: {metadata_path}")
    metadata = read_json(metadata_path)
    if not isinstance(metadata, dict):
        raise SystemExit(f"metadata.json is not an object: {metadata_path}")
    report_id = metadata.get("report_id")
    if not isinstance(report_id, str) or not report_id:
        raise SystemExit(f"metadata.json lacks report_id: {metadata_path}")
    return metadata


def load_source_sets(run_dir: Path) -> dict[str, list[str]]:
    index_path = run_dir / "index.json"
    index = read_json(index_path)
    if not isinstance(index, dict):
        raise SystemExit(f"index.json is not an object: {index_path}")
    by_feature = index.get("by_feature")
    if not isinstance(by_feature, dict):
        raise SystemExit(f"index.json lacks by_feature object: {index_path}")

    def feature_set(name: str) -> set[str]:
        raw = by_feature.get(name, [])
        if not isinstance(raw, list):
            raise SystemExit(f"index by_feature.{name} is not a list")
        return {str(work_id) for work_id in raw}

    warigaki = feature_set("warigaki")
    kaeriten = feature_set("kaeriten")
    okurigana = feature_set("okurigana")
    kunten = kaeriten | okurigana
    policy_union = warigaki | kunten
    return {
        "warigaki": sorted(warigaki),
        "kaeriten": sorted(kaeriten),
        "okurigana": sorted(okurigana),
        "kunten": sorted(kunten),
        "policy_union": sorted(policy_union),
    }


def walk_nodes(value: Any, path: str = "$") -> list[tuple[str, dict[str, Any]]]:
    out: list[tuple[str, dict[str, Any]]] = []
    if isinstance(value, dict):
        out.append((path, value))
        for key, child in value.items():
            child_path = f"{path}.{key}"
            out.extend(walk_nodes(child, child_path))
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


def semantic_kunten_count(aat: dict[str, Any]) -> int:
    syntax = (
        aat.get("meta", {})
        .get("semantic_summary", {})
        .get("syntax", {})
    )
    if not isinstance(syntax, dict):
        return 0
    count = 0
    for syntax_id, observations in syntax.items():
        if not str(syntax_id).startswith("kunten."):
            continue
        if isinstance(observations, list):
            count += len(observations)
    return count


def scan_aat_observations(run_dir: Path) -> dict[str, dict[str, Any]]:
    aat_root = run_dir / "aat"
    if not aat_root.exists():
        raise SystemExit(f"missing persisted AAT directory: {aat_root}")

    observations: dict[str, dict[str, Any]] = {}
    for path in sorted(aat_root.rglob("*.json")):
        try:
            aat = read_json(path)
        except Exception as exc:  # noqa: BLE001 - report file path in failure
            raise SystemExit(f"failed to parse AAT JSON {path}: {exc}") from exc
        if not isinstance(aat, dict):
            continue
        work_id = str(aat.get("work_id", ""))
        if not work_id:
            continue
        current = observations.setdefault(
            work_id,
            {
                "aat_paths": [],
                "warigaki_nodes": 0,
                "kunten_nodes": 0,
                "kunten_semantic_observations": 0,
            },
        )
        current["aat_paths"].append(str(path.relative_to(run_dir)))
        semantic_kunten = semantic_kunten_count(aat)
        current["kunten_semantic_observations"] += semantic_kunten
        for _node_path, node in walk_nodes(aat):
            if node.get("kind") == "warigaki":
                current["warigaki_nodes"] += 1
            if is_kunten_node(node):
                current["kunten_nodes"] += 1
    return observations


def load_report_state(run_dir: Path, report_id: str) -> dict[str, dict[str, Any]]:
    db_path = run_dir / "fidelity.duckdb"
    if not db_path.exists():
        raise SystemExit(f"missing fidelity.duckdb: {db_path}")
    rid = sql_literal(report_id)
    table_counts = {
        row["table_name"]: int(row["rows"])
        for row in duckdb_json(
            db_path,
            f"""
            SELECT 'aat_batch_reports' AS table_name, count(*)::UBIGINT AS rows
            FROM aat_batch_reports WHERE report_id = {rid}
            UNION ALL
            SELECT 'aat_batch_property_results', count(*)::UBIGINT
            FROM aat_batch_property_results WHERE report_id = {rid}
            UNION ALL
            SELECT 'aat_batch_source_derived_nodes', count(*)::UBIGINT
            FROM aat_batch_source_derived_nodes WHERE report_id = {rid}
            UNION ALL
            SELECT 'aat_batch_semantic_summary', count(*)::UBIGINT
            FROM aat_batch_semantic_summary WHERE report_id = {rid}
            """,
        )
    }
    if table_counts.get("aat_batch_reports", 0) == 0:
        raise SystemExit(f"report_id not found in DuckDB: {report_id}")

    state: dict[str, dict[str, Any]] = {}
    for row in duckdb_json(
        db_path,
        f"""
        SELECT work_id, report_path, aat_path, schema_valid, parse_complete,
               failing_result_count
        FROM aat_batch_reports
        WHERE report_id = {rid}
        ORDER BY work_id, report_path
        """,
    ):
        work_id = str(row["work_id"])
        current = state.setdefault(
            work_id,
            {
                "report_paths": [],
                "aat_paths": [],
                "schema_valid_values": [],
                "parse_complete_values": [],
                "failing_result_count": 0,
                "failed_properties": set(),
                "duckdb_table_counts": table_counts,
            },
        )
        current["report_paths"].append(str(row["report_path"]))
        if row.get("aat_path") is not None:
            current["aat_paths"].append(str(row["aat_path"]))
        current["schema_valid_values"].append(duckdb_bool(row.get("schema_valid")))
        current["parse_complete_values"].append(duckdb_bool(row.get("parse_complete")))
        current["failing_result_count"] += int(row.get("failing_result_count") or 0)

    for row in duckdb_json(
        db_path,
        f"""
        SELECT work_id, property
        FROM aat_batch_property_results
        WHERE report_id = {rid} AND NOT pass
        ORDER BY work_id, property
        """,
    ):
        work_id = str(row["work_id"])
        state.setdefault(
            work_id,
            {
                "report_paths": [],
                "aat_paths": [],
                "schema_valid_values": [],
                "parse_complete_values": [],
                "failing_result_count": 0,
                "failed_properties": set(),
                "duckdb_table_counts": table_counts,
            },
        )["failed_properties"].add(str(row["property"]))

    for current in state.values():
        current["failed_properties"] = sorted(current["failed_properties"])
    return state


def has_observation(
    family: str,
    work_id: str,
    observations: dict[str, dict[str, Any]],
) -> bool:
    observed = observations.get(work_id, {})
    if family == "warigaki":
        return int(observed.get("warigaki_nodes", 0)) > 0
    if family == "kunten":
        return (
            int(observed.get("kunten_nodes", 0)) > 0
            or int(observed.get("kunten_semantic_observations", 0)) > 0
        )
    raise AssertionError(f"unknown family: {family}")


def classify_work(
    *,
    family: str,
    work_id: str,
    report_state: dict[str, dict[str, Any]],
    observations: dict[str, dict[str, Any]],
    retry_report_state: dict[str, dict[str, Any]] | None = None,
    retry_observations: dict[str, dict[str, Any]] | None = None,
) -> str:
    if has_observation(family, work_id, observations):
        return "observed_in_aat"
    if retry_observations and has_observation(family, work_id, retry_observations):
        return "observed_in_aat"

    state = report_state.get(work_id)
    if retry_report_state and work_id in retry_report_state:
        state = retry_report_state[work_id]
    if state is None:
        return "schema_invalid_or_no_aat"

    failed_properties = set(state.get("failed_properties", []))
    if failed_properties & ADAPTER_ERROR_PROPERTIES:
        return "adapter_timeout_or_protocol_error"

    schema_values = state.get("schema_valid_values", [])
    has_schema_valid = any(value is True for value in schema_values)
    has_aat_path = bool(state.get("aat_paths"))
    if not has_schema_valid or not has_aat_path:
        return "schema_invalid_or_no_aat"

    parse_values = state.get("parse_complete_values", [])
    has_parse_complete = any(value is True for value in parse_values)
    has_parse_incomplete = any(value is False for value in parse_values)
    if has_parse_incomplete and not has_parse_complete:
        return "parse_incomplete"

    other_failures = failed_properties - {"schema_valid", "parse_completeness"}
    if other_failures:
        return "report_failed_other_property"

    return "source_feature_without_aat_observation"


def classify_family(
    *,
    family: str,
    work_ids: list[str],
    report_state: dict[str, dict[str, Any]],
    observations: dict[str, dict[str, Any]],
    retry_report_state: dict[str, dict[str, Any]] | None = None,
    retry_observations: dict[str, dict[str, Any]] | None = None,
) -> dict[str, list[str]]:
    buckets = {bucket: [] for bucket in BUCKETS}
    for work_id in sorted(set(work_ids)):
        bucket = classify_work(
            family=family,
            work_id=work_id,
            report_state=report_state,
            observations=observations,
            retry_report_state=retry_report_state,
            retry_observations=retry_observations,
        )
        buckets[bucket].append(work_id)
    accounted = sum(len(values) for values in buckets.values())
    if accounted != len(set(work_ids)):
        raise SystemExit(
            f"bucket accounting mismatch for {family}: {accounted} != {len(set(work_ids))}"
        )
    return buckets


def failure_overlap(
    source_sets: dict[str, list[str]],
    report_state: dict[str, dict[str, Any]],
) -> dict[str, dict[str, int]]:
    out: dict[str, dict[str, int]] = {}
    for family in ("warigaki", "kunten"):
        counter: Counter[str] = Counter()
        for work_id in source_sets[family]:
            for prop in report_state.get(work_id, {}).get("failed_properties", []):
                counter[prop] += 1
        out[family] = dict(sorted(counter.items()))
    return out


def write_worksets(
    *,
    worksets_dir: Path,
    buckets: dict[str, dict[str, list[str]]],
) -> dict[str, str]:
    worksets_dir.mkdir(parents=True, exist_ok=True)

    def incomplete(family: str) -> list[str]:
        ids: set[str] = set()
        for bucket in INCOMPLETE_BUCKETS:
            ids.update(buckets[family][bucket])
        return sorted(ids)

    warigaki = incomplete("warigaki")
    kunten = incomplete("kunten")
    union = sorted(set(warigaki) | set(kunten))
    files = {
        "warigaki_incomplete": worksets_dir / "warigaki-incomplete.json",
        "kunten_incomplete": worksets_dir / "kunten-incomplete.json",
        "policy_incomplete_union": worksets_dir / "policy-incomplete-union.json",
    }
    write_json(files["warigaki_incomplete"], warigaki)
    write_json(files["kunten_incomplete"], kunten)
    write_json(files["policy_incomplete_union"], union)
    for path in files.values():
        payload = read_json(path)
        if not isinstance(payload, list) or not all(isinstance(item, str) for item in payload):
            raise SystemExit(f"workset is not a JSON array of strings: {path}")
        if payload != sorted(set(payload)):
            raise SystemExit(f"workset is not unique and sorted: {path}")
    return {name: str(path) for name, path in files.items()}


def summarize_observations(observations: dict[str, dict[str, Any]]) -> dict[str, int]:
    return {
        "warigaki_works": sum(
            1 for item in observations.values() if int(item.get("warigaki_nodes", 0)) > 0
        ),
        "warigaki_nodes": sum(
            int(item.get("warigaki_nodes", 0)) for item in observations.values()
        ),
        "kunten_works": sum(
            1
            for item in observations.values()
            if int(item.get("kunten_nodes", 0)) > 0
            or int(item.get("kunten_semantic_observations", 0)) > 0
        ),
        "kunten_nodes": sum(
            int(item.get("kunten_nodes", 0)) for item in observations.values()
        ),
        "kunten_semantic_observations": sum(
            int(item.get("kunten_semantic_observations", 0))
            for item in observations.values()
        ),
        "kunten_observations": sum(
            int(item.get("kunten_nodes", 0))
            + int(item.get("kunten_semantic_observations", 0))
            for item in observations.values()
        ),
    }


def write_markdown(
    *,
    path: Path,
    run_dir: Path,
    report_id: str,
    source_counts: dict[str, int],
    observed_counts: dict[str, int],
    bucket_counts: dict[str, dict[str, int]],
    failure_counts: dict[str, dict[str, int]],
    worksets: dict[str, str],
    retry_dir: Path | None,
) -> None:
    lines = [
        "# Aozora2html Measurement Trust Audit",
        "",
        f"- run_dir: `{run_dir}`",
        f"- report_id: `{report_id}`",
    ]
    if retry_dir is not None:
        lines.append(f"- retry run: `{retry_dir}`")
    lines.extend(
        [
            "",
            "## Source Counts",
            "",
            f"- source warigaki works: {source_counts['warigaki']}",
            f"- source kunten works: {source_counts['kunten']}",
            f"- source policy union works: {source_counts['policy_union']}",
            "",
            "## AAT Observed Counts",
            "",
            "| Metric | Value |",
            "|---|---:|",
            f"| warigaki works | {observed_counts['warigaki_works']} |",
            f"| warigaki nodes | {observed_counts['warigaki_nodes']} |",
            f"| kunten works | {observed_counts['kunten_works']} |",
            f"| kunten nodes | {observed_counts['kunten_nodes']} |",
            f"| kunten semantic observations | {observed_counts['kunten_semantic_observations']} |",
            f"| kunten observations | {observed_counts['kunten_observations']} |",
            "",
        ]
    )
    for family in ("warigaki", "kunten"):
        lines.extend(
            [
                f"## {family} Trust Buckets",
                "",
                "| Bucket | Works |",
                "|---|---:|",
            ]
        )
        for bucket in BUCKETS:
            lines.append(f"| {bucket} | {bucket_counts[family][bucket]} |")
        lines.append("")
        lines.extend(
            [
                f"## {family} Failure Overlap",
                "",
            ]
        )
        if failure_counts[family]:
            lines.extend(["| Property | Works |", "|---|---:|"])
            for prop, count in failure_counts[family].items():
                lines.append(f"| {prop} | {count} |")
        else:
            lines.append("_No failed properties overlap this source-feature family._")
        lines.append("")
    lines.extend(
        [
            "## Generated Worksets",
            "",
            f"- warigaki incomplete: `{worksets['warigaki_incomplete']}`",
            f"- kunten incomplete: `{worksets['kunten_incomplete']}`",
            f"- policy incomplete union: `{worksets['policy_incomplete_union']}`",
            "",
        ]
    )
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("\n".join(lines))


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--run-dir", type=Path, required=True)
    parser.add_argument("--retry-run-dir", type=Path)
    parser.add_argument("--out-md", type=Path, required=True)
    parser.add_argument("--summary-json", type=Path, required=True)
    parser.add_argument("--worksets-dir", type=Path, required=True)
    args = parser.parse_args()

    run_dir = args.run_dir.resolve()
    metadata = load_metadata(run_dir)
    report_id = str(metadata["report_id"])
    source_sets = load_source_sets(run_dir)
    report_state = load_report_state(run_dir, report_id)
    observations = scan_aat_observations(run_dir)

    retry_report_state = None
    retry_observations = None
    retry_summary: dict[str, Any] | None = None
    if args.retry_run_dir is not None:
        retry_dir = args.retry_run_dir.resolve()
        retry_metadata = load_metadata(retry_dir)
        retry_report_id = str(retry_metadata["report_id"])
        retry_report_state = load_report_state(retry_dir, retry_report_id)
        retry_observations = scan_aat_observations(retry_dir)
        retry_summary = {
            "run_dir": str(retry_dir),
            "report_id": retry_report_id,
            "aat_observed_counts": summarize_observations(retry_observations),
        }

    buckets = {
        family: classify_family(
            family=family,
            work_ids=source_sets[family],
            report_state=report_state,
            observations=observations,
            retry_report_state=retry_report_state,
            retry_observations=retry_observations,
        )
        for family in ("warigaki", "kunten")
    }
    bucket_counts = {
        family: {bucket: len(ids) for bucket, ids in family_buckets.items()}
        for family, family_buckets in buckets.items()
    }
    source_counts = {name: len(values) for name, values in source_sets.items()}
    observed_counts = summarize_observations(observations)
    failures = failure_overlap(source_sets, report_state)
    worksets = write_worksets(worksets_dir=args.worksets_dir, buckets=buckets)

    summary = {
        "run_dir": str(run_dir),
        "report_id": report_id,
        "source_sets": source_sets,
        "source_counts": source_counts,
        "aat_observed_counts": observed_counts,
        "bucket_counts": bucket_counts,
        "buckets": buckets,
        "failure_overlap": failures,
        "worksets": worksets,
        "retry": retry_summary,
        "verdict_inputs": {
            "has_policy_relevant_residuals": any(
                bucket_counts[family][bucket] > 0
                for family in ("warigaki", "kunten")
                for bucket in POLICY_RELEVANT_RESIDUAL_BUCKETS
            ),
            "has_source_feature_without_aat_observation": any(
                bucket_counts[family]["source_feature_without_aat_observation"] > 0
                for family in ("warigaki", "kunten")
            ),
        },
    }
    write_json(args.summary_json, summary)
    write_markdown(
        path=args.out_md,
        run_dir=run_dir,
        report_id=report_id,
        source_counts=source_counts,
        observed_counts=observed_counts,
        bucket_counts=bucket_counts,
        failure_counts=failures,
        worksets=worksets,
        retry_dir=args.retry_run_dir.resolve() if args.retry_run_dir else None,
    )
    print(f"wrote audit report: {args.out_md}")
    print(f"wrote audit summary: {args.summary_json}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
