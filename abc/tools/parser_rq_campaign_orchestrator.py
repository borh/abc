#!/usr/bin/env python3
"""Authenticate and execute the fixed parser-RQ production graph."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Any


LEGACY_LANE_ENVIRONMENT = (
    "PARSER_RQ_CORE_CAPTURE",
    "PARSER_RQ_SOURCE_CAPTURE",
    "PARSER_RQ_PREDICATE_CAPTURE",
    "PARSER_RQ_DIAGNOSTIC_CAPTURE",
    "PARSER_RQ_PUBLICATION_CAPTURE",
    "PARSER_RQ_RESOURCE_CAPTURE",
)

EXPECTED_GRAPH = (
    ("core_attempt", "capture-core"),
    ("predicate_hardening", "capture-predicate-pair"),
    ("source_recognition", "capture-source"),
    ("diagnostic_gap", "derive-diagnostic-gap"),
    ("publication_structure", "capture-publication"),
    ("resource", "capture-resource"),
)

EXPECTED_INSTALLED_MEMBERS = (
    "core_attempt",
    "source_recognition",
    "diagnostic_gap",
    "diagnostic_completeness",
    "parser_ir_conformance",
    "publication_structure",
    "resource",
)

REPOSITORY_DRIVERS = (
    Path("abc/tools/parser_rq_campaign_site.py"),
    Path("ab-validator/reports/parser-ir/parser-rq-core-attempt-capture.py"),
    Path("ab-validator/reports/parser-ir/parser-rq-predicate-hardening-capture.py"),
    Path("ab-validator/reports/parser-ir/publication-rq-capture.py"),
    Path("ab-validator/reports/parser-ir/publication-bundle-validate.py"),
    Path("ab-validator/reports/parser-ir/parser-rq-resource-capture.py"),
    Path("ab-validator/reports/parser-ir/parser-rq-resource-wrapper.py"),
)


class PreparationFailed(ValueError):
    """The campaign cannot consume its one authorized capture attempt."""


@dataclass(frozen=True)
class CampaignConfig:
    candidate: Path
    authorization: Path
    provenance: Path
    readiness_receipt: Path
    site_policy: Path
    site_descriptor: Path
    candidate_tree: Path
    evidence_tree: Path
    staging_root: Path
    production: bool


@dataclass(frozen=True)
class AuthenticatedCampaign:
    config: CampaignConfig
    graph: dict[str, Any]
    provenance: dict[str, Any]
    readiness_receipt: dict[str, Any]
    site_policy: dict[str, Any]
    site_descriptor: dict[str, Any]
    operations: tuple[str, ...]
    executables: dict[str, Path]
    drivers: dict[str, Path]
    clojure_prefix: tuple[str, ...]


def _canonical_bytes(value: object) -> bytes:
    encoded = json.dumps(value, ensure_ascii=True, sort_keys=True, separators=(",", ":"))
    return encoded.replace("/", "\\/").encode()


def content_ref(value: dict[str, object], *, excluding: str | None = None) -> str:
    projected = (
        value
        if excluding is None
        else {key: item for key, item in value.items() if key != excluding}
    )
    return "sha256:" + hashlib.sha256(_canonical_bytes(projected)).hexdigest()


def _read_object(path: Path, label: str) -> dict[str, Any]:
    try:
        value = json.loads(path.read_bytes())
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
        raise PreparationFailed(f"{label} is not valid JSON") from error
    if not isinstance(value, dict):
        raise PreparationFailed(f"{label} is not a JSON object")
    return value


def _below(root: Path, path: Path, label: str) -> Path:
    try:
        resolved_root = root.resolve(strict=True)
        resolved = path.resolve(strict=True)
        resolved.relative_to(resolved_root)
    except (OSError, ValueError) as error:
        raise PreparationFailed(f"{label} is not below its authenticated tree") from error
    return resolved


def _authenticate_executable(path: Path, record: dict[str, Any]) -> None:
    try:
        payload = path.read_bytes()
    except OSError as error:
        raise PreparationFailed(f"candidate executable is unavailable: {path}") from error
    if record.get("bytes") != len(payload):
        raise PreparationFailed(f"candidate executable byte count differs: {path.name}")
    expected = "sha256:" + hashlib.sha256(payload).hexdigest()
    if record.get("sha256") != expected:
        raise PreparationFailed(f"candidate executable hash differs: {path.name}")


def authenticate_inputs(config: CampaignConfig) -> AuthenticatedCampaign:
    inherited = sorted(name for name in LEGACY_LANE_ENVIRONMENT if name in os.environ)
    if inherited:
        raise PreparationFailed("legacy lane environment is forbidden: " + ", ".join(inherited))

    candidate_tree = config.candidate_tree.resolve(strict=True)
    evidence_tree = config.evidence_tree.resolve(strict=True)
    for label, path in (
        ("candidate", config.candidate),
        ("authorization", config.authorization),
        ("provenance", config.provenance),
        ("readiness receipt", config.readiness_receipt),
        ("site policy", config.site_policy),
        ("site descriptor", config.site_descriptor),
    ):
        _below(evidence_tree, path, label)

    graph_path = _below(
        candidate_tree,
        candidate_tree / "abc/data/parser-rq-production-graph-v1.json",
        "production graph",
    )
    graph = _read_object(graph_path, "production graph")
    if graph.get("policy_hash") != content_ref(graph, excluding="policy_hash"):
        raise PreparationFailed("production graph hash does not authenticate")
    graph_pairs = tuple(
        (row.get("name"), row.get("operation"))
        for row in graph.get("members", [])
        if isinstance(row, dict)
    )
    if graph_pairs != EXPECTED_GRAPH:
        raise PreparationFailed("production graph operation membership or order differs")
    if tuple(graph.get("installed_members", ())) != EXPECTED_INSTALLED_MEMBERS:
        raise PreparationFailed("production graph installed membership differs")

    provenance = _read_object(config.provenance, "provenance")
    receipt = _read_object(config.readiness_receipt, "readiness receipt")
    site_policy = _read_object(config.site_policy, "site policy")
    site_descriptor = _read_object(config.site_descriptor, "site descriptor")
    if provenance.get("status") != "reproducible":
        raise PreparationFailed("candidate provenance is not reproducible")
    if receipt.get("production_graph_hash") != graph.get("policy_hash"):
        raise PreparationFailed("readiness receipt binds another production graph")
    if receipt.get("provenance_core_ref") != provenance.get("provenance_core_ref"):
        raise PreparationFailed("readiness receipt binds another provenance core")
    if config.production and site_policy.get("replica_status") != "configured":
        raise PreparationFailed("production evidence replica is unconfigured")

    graph_executables = graph.get("executables")
    provenance_executables = provenance.get("executables")
    if not isinstance(graph_executables, list) or not isinstance(provenance_executables, list):
        raise PreparationFailed("executable membership is absent")
    graph_by_name = {row.get("name"): row for row in graph_executables if isinstance(row, dict)}
    records_by_name = {
        row.get("name"): row for row in provenance_executables if isinstance(row, dict)
    }
    if (
        len(graph_by_name) != len(graph_executables)
        or len(records_by_name) != len(provenance_executables)
        or set(graph_by_name) != set(records_by_name)
    ):
        raise PreparationFailed("provenance executable membership differs from graph")
    executables: dict[str, Path] = {}
    for name, reviewed in graph_by_name.items():
        record = records_by_name[name]
        for field in ("adapter", "adapter_version", "argv_template"):
            if record.get(field) != reviewed.get(field):
                raise PreparationFailed(f"provenance {name} {field} differs from graph")
        output = Path(str(record.get("nix_output", "")))
        executable = output / "bin" / str(name)
        _authenticate_executable(executable, record)
        executables[str(name)] = executable

    drivers = {
        str(relative): _below(candidate_tree, candidate_tree / relative, str(relative))
        for relative in REPOSITORY_DRIVERS
    }
    return AuthenticatedCampaign(
        config=config,
        graph=graph,
        provenance=provenance,
        readiness_receipt=receipt,
        site_policy=site_policy,
        site_descriptor=site_descriptor,
        operations=tuple(operation for _, operation in graph_pairs),
        executables=executables,
        drivers=drivers,
        clojure_prefix=(
            "nix",
            "develop",
            "--no-write-lock-file",
            str(candidate_tree / "abc"),
            "--command",
            "clojure",
        ),
    )


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--candidate", type=Path, required=True)
    parser.add_argument("--authorization", type=Path, required=True)
    parser.add_argument("--provenance", type=Path, required=True)
    parser.add_argument("--readiness-receipt", type=Path, required=True)
    parser.add_argument("--site-policy", type=Path, required=True)
    parser.add_argument("--site-descriptor", type=Path, required=True)
    parser.add_argument("--candidate-tree", type=Path, required=True)
    parser.add_argument("--evidence-tree", type=Path, required=True)
    parser.add_argument("--staging-root", type=Path, required=True)
    parser.add_argument("--production", action="store_true")
    return parser


def main(argv: list[str] | None = None) -> int:
    parser = _parser()
    try:
        args = parser.parse_args(argv)
        authenticate_inputs(CampaignConfig(**vars(args)))
    except (OSError, PreparationFailed) as error:
        print(str(error), file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
