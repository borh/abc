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


class ProtocolError(ValueError):
    """The fixed production graph or a producer violated its closed protocol."""


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


@dataclass(frozen=True)
class LockCapability:
    fd: int
    device: int
    inode: int


@dataclass(frozen=True)
class RuntimePaths:
    root: Path
    runtime: Path
    corpus: Path
    source_corpus: Path
    identity: Path
    core_root: Path
    predicate_root: Path
    source_root: Path
    diagnostic_root: Path
    publication_root: Path
    resource_root: Path

    @classmethod
    def below(cls, root: Path) -> RuntimePaths:
        return cls(
            root=root,
            runtime=root / "runtime-inputs.json",
            corpus=root / "runtime-corpus.json",
            source_corpus=root / "source-accountability-corpus.json",
            identity=root / "qualification-identity.json",
            core_root=root / "lanes/core",
            predicate_root=root / "lanes/predicate",
            source_root=root / "lanes/source",
            diagnostic_root=root / "lanes/diagnostic",
            publication_root=root / "lanes/publication",
            resource_root=root / "lanes/resource",
        )


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


def _driver(campaign: AuthenticatedCampaign, relative: str) -> str:
    return str(campaign.drivers[relative])


def operation_argv(
    operation: str,
    campaign: AuthenticatedCampaign,
    paths: RuntimePaths,
    lock: LockCapability,
) -> tuple[str, ...]:
    candidate_tree = campaign.config.candidate_tree.resolve()
    abc_root = candidate_tree / "abc"
    ab_root = candidate_tree / "ab-validator"
    if operation == "capture-core":
        return (
            sys.executable,
            _driver(
                campaign,
                "ab-validator/reports/parser-ir/parser-rq-core-attempt-capture.py",
            ),
            "--runtime",
            str(paths.runtime),
            "--policy",
            str(abc_root / "data/parser-rq-core-attempt-policy-v1.json"),
            "--ab-check",
            str(campaign.executables["ab-check"]),
            "--corpus-root",
            str(candidate_tree),
            "--corpus-index",
            str(abc_root / "data/parser-release-qualification-corpus.edn"),
            "--time-executable",
            str(Path(str(campaign.site_descriptor["primary_store_root"])) / "executables/time"),
            "--staging-root",
            str(paths.core_root / "records"),
            "--inherited-lock-fd",
            str(lock.fd),
            "--lock-device",
            str(lock.device),
            "--lock-inode",
            str(lock.inode),
            "--out",
            str(paths.core_root / "core-index.json"),
        )
    if operation == "capture-predicate-pair":
        return (
            sys.executable,
            _driver(
                campaign,
                "ab-validator/reports/parser-ir/parser-rq-predicate-hardening-capture.py",
            ),
            "--corpus",
            str(paths.corpus),
            "--source-root",
            str(candidate_tree),
            "--ab-aozora",
            str(campaign.executables["ab-aozora"]),
            "--converter",
            str(campaign.executables["ab-aat-to-parser-ir"]),
            "--mapping",
            str(ab_root / "data/aat-to-parser-ir-mapping-v2.json"),
            "--abc-root",
            str(abc_root),
            "--identity-ref",
            str(campaign.readiness_receipt["qualification_identity_ref"]),
            "--parser-policy",
            str(abc_root / "data/parser-rq-parser-ir-conformance-policy-v1.json"),
            "--store",
            str(paths.predicate_root / "store"),
            "--output",
            str(paths.predicate_root / "output"),
        )
    if operation == "capture-source":
        return (
            str(campaign.executables["ab-parser-rq-source-accountability"]),
            "capture-corpus",
            "--corpus",
            str(paths.source_corpus),
            "--source-root",
            str(candidate_tree),
            "--parser-ir-root",
            str(paths.predicate_root / "output/parser-ir"),
            "--qualification",
            str(paths.identity),
            "--taxonomy",
            str(abc_root / "data/parser-rq-ignored-regions-v1.json"),
            "--store-root",
            str(paths.source_root / "store"),
            "--output-dir",
            str(paths.source_root / "output"),
        )
    if operation == "derive-diagnostic-gap":
        return (
            str(campaign.executables["ab-parser-rq-diagnostic-authorization"]),
            "capture-corpus",
            "--index",
            str(paths.source_root / "output/source-recognition-index.json"),
            "--policy",
            str(abc_root / "data/parser-rq-ab-aozora-diagnostic-gap-v1.json"),
            "--out",
            str(paths.diagnostic_root / "diagnostic-gap.json"),
        )
    if operation == "capture-publication":
        return (
            sys.executable,
            _driver(
                campaign,
                "ab-validator/reports/parser-ir/publication-rq-capture.py",
            ),
            "--input",
            str(paths.publication_root / "capture-input.json"),
            "--store",
            str(paths.publication_root / "store"),
            "--manifest-out",
            str(paths.publication_root / "manifest.json"),
            "--index-out",
            str(paths.publication_root / "index.json"),
        )
    if operation == "capture-resource":
        return (
            sys.executable,
            _driver(
                campaign,
                "ab-validator/reports/parser-ir/parser-rq-resource-capture.py",
            ),
            "--policy",
            str(abc_root / "data/parser-rq-resource-policy-v1.json"),
            "--wrapper",
            _driver(
                campaign,
                "ab-validator/reports/parser-ir/parser-rq-resource-wrapper.py",
            ),
            "--command-template",
            str(paths.resource_root / "command-template.json"),
            "--records-root",
            str(paths.resource_root / "records"),
            "--out",
            str(paths.resource_root / "index.json"),
        )
    raise ProtocolError(f"unknown production operation: {operation}")


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
