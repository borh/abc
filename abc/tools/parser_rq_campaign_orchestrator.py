#!/usr/bin/env python3
"""Authenticate and execute the fixed parser-RQ production graph."""

from __future__ import annotations

import argparse
import fcntl
import hashlib
import json
import os
import subprocess
import sys
from dataclasses import dataclass
from datetime import UTC, datetime
from pathlib import Path
from typing import Any, Callable, Protocol


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


@dataclass(frozen=True)
class Terminal:
    status: str
    reason: str | None = None


class Runner(Protocol):
    def run(self, argv: tuple[str, ...], *, cwd: Path, pass_fds: tuple[int, ...] = ()) -> int: ...


class SubprocessRunner:
    def run(self, argv: tuple[str, ...], *, cwd: Path, pass_fds: tuple[int, ...] = ()) -> int:
        return subprocess.run(argv, cwd=cwd, pass_fds=pass_fds, check=False).returncode


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
            str(paths.diagnostic_root / "input-index.json"),
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


def _atomic_json(path: Path, value: object) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(f".{path.name}.{os.getpid()}.tmp")
    try:
        with temporary.open("xb") as stream:
            stream.write(_canonical_bytes(value))
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temporary, path)
    finally:
        temporary.unlink(missing_ok=True)


def _run_checked(
    runner: Runner,
    argv: tuple[str, ...],
    cwd: Path,
    *,
    pass_fds: tuple[int, ...] = (),
) -> None:
    if runner.run(argv, cwd=cwd, pass_fds=pass_fds) != 0:
        raise ProtocolError(f"command failed: {argv[0]}")


def _acquire_lock(path: Path) -> LockCapability:
    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor = os.open(path, os.O_RDWR | os.O_CREAT, 0o600)
    try:
        fcntl.flock(descriptor, fcntl.LOCK_EX | fcntl.LOCK_NB)
        stat = os.fstat(descriptor)
        return LockCapability(descriptor, stat.st_dev, stat.st_ino)
    except Exception:
        os.close(descriptor)
        raise


def _lock_retained(lock: LockCapability) -> bool:
    try:
        descriptor = os.fstat(lock.fd)
        target = Path(os.readlink(f"/proc/self/fd/{lock.fd}"))
        current = target.stat()
    except OSError:
        return False
    return (
        (descriptor.st_dev, descriptor.st_ino)
        == (current.st_dev, current.st_ino)
        == (lock.device, lock.inode)
    )


def _materialize_runtime(paths: RuntimePaths) -> None:
    runtime = _read_object(paths.runtime, "runtime inputs")
    if runtime.get("schema_version") != "abc/parser-rq-runtime-inputs/v1":
        raise ProtocolError("runtime input schema identity differs")
    candidate = runtime.get("candidate")
    if not isinstance(candidate, dict) or not isinstance(
        candidate.get("qualification_identity"), dict
    ):
        raise ProtocolError("runtime qualification identity is absent")
    _atomic_json(paths.corpus, runtime.get("corpus"))
    _atomic_json(paths.source_corpus, runtime.get("source_accountability_corpus"))
    _atomic_json(paths.identity, candidate["qualification_identity"])


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        while chunk := stream.read(1024 * 1024):
            digest.update(chunk)
    return "sha256:" + digest.hexdigest()


def _prepare_diagnostic_input(campaign: AuthenticatedCampaign, paths: RuntimePaths) -> None:
    store = paths.source_root / "store"
    generation_index = _read_object(
        paths.source_root / "output/classified-source-generation-index.json",
        "classified-source generation index",
    )
    recognition_index = _read_object(
        paths.source_root / "output/source-recognition-index.json",
        "source-recognition index",
    )
    generation_by_work: dict[str, dict[str, Any]] = {}
    for row in generation_index.get("records", []):
        if not isinstance(row, dict):
            raise ProtocolError("classified-source generation row is malformed")
        manifest = _read_object(store / str(row.get("locator")), "classified-source generation")
        if manifest.get("generation_ref") != row.get("sha256"):
            raise ProtocolError("classified-source generation identity differs")
        generation_by_work[str(row.get("work_id"))] = manifest
    records: list[dict[str, object]] = []
    for row in recognition_index.get("records", []):
        if not isinstance(row, dict):
            raise ProtocolError("source-recognition row is malformed")
        work_id = str(row.get("work_id"))
        generation = generation_by_work.get(work_id)
        if generation is None:
            raise ProtocolError("source-recognition generation is absent")
        members = generation.get("members")
        if not isinstance(members, dict):
            raise ProtocolError("classified-source generation members are malformed")
        decoded = members.get("decoded_source")
        diagnostics = members.get("raw_diagnostics")
        if not isinstance(decoded, dict) or not isinstance(diagnostics, dict):
            raise ProtocolError("classified-source diagnostic inputs are absent")
        records.append(
            {
                "work_id": work_id,
                "capture_generation_ref": generation["generation_ref"],
                "decoded_source": str(store / str(decoded["artifact_ref"])),
                "decoded_source_hash": decoded["value_hash"],
                "raw_diagnostics": str(store / str(diagnostics["artifact_ref"])),
                "raw_diagnostics_hash": diagnostics["value_hash"],
                "source_recognition": str(store / str(row["locator"])),
                "source_recognition_hash": row["sha256"],
                "source_recognition_locator": row["locator"],
            }
        )
    policy = campaign.config.candidate_tree / "abc/data/parser-rq-ab-aozora-diagnostic-gap-v1.json"
    _atomic_json(
        paths.diagnostic_root / "input-index.json",
        {
            "qualification_identity_ref": recognition_index["qualification_identity_ref"],
            "corpus_generation_ref": recognition_index["corpus_generation_ref"],
            "policy_hash": _sha256(policy),
            "records": records,
        },
    )


def _blob_member(root: Path, path: Path) -> dict[str, object]:
    relative = path.relative_to(root).as_posix()
    media_type = "application/json" if path.suffix == ".json" else "text/plain"
    return {
        "locator": relative,
        "ref": {
            "sha256": _sha256(path),
            "bytes": path.stat().st_size,
            "media_type": media_type,
        },
    }


def _install_bytes(root: Path, locator: str, payload: bytes) -> None:
    path = root / locator
    path.parent.mkdir(parents=True, exist_ok=True)
    if path.exists() and path.read_bytes() != payload:
        raise ProtocolError(f"store locator collision: {locator}")
    path.write_bytes(payload)


def _source_projection_values(
    campaign: AuthenticatedCampaign, paths: RuntimePaths, *, diagnostic: bool
) -> tuple[dict[str, object], dict[str, Any]]:
    store = paths.source_root / "store"
    output = paths.source_root / "output"
    aggregate_path = output / "source-recognition-aggregate.json"
    index_path = output / "source-recognition-index.json"
    for locator, source in (
        ("recognition-aggregate.json", aggregate_path),
        ("recognition-index.json", index_path),
        ("identity.json", paths.identity),
    ):
        _install_bytes(store, locator, source.read_bytes())
    if diagnostic:
        corpus = _read_object(paths.diagnostic_root / "diagnostic-gap.json", "diagnostic gap")
        aggregate = corpus.get("aggregate")
        works = corpus.get("works")
        if not isinstance(aggregate, dict) or not isinstance(works, list):
            raise ProtocolError("diagnostic-gap corpus output is malformed")
        _install_bytes(store, "diagnostic-gap-aggregate.json", _canonical_bytes(aggregate))
        for work in works:
            if not isinstance(work, dict) or not isinstance(work.get("work_id"), str):
                raise ProtocolError("diagnostic-gap work output is malformed")
            _install_bytes(
                store,
                f"diagnostic-gap-results/{work['work_id']}.json",
                _canonical_bytes(work),
            )
        policy = (
            campaign.config.candidate_tree / "abc/data/parser-rq-ab-aozora-diagnostic-gap-v1.json"
        )
        _install_bytes(store, "diagnostic-gap-policy.json", policy.read_bytes())
    blobs = [
        _blob_member(store, path)
        for path in sorted(store.rglob("*"))
        if path.is_file() and not path.is_symlink()
    ]
    aggregate = _read_object(aggregate_path, "source-recognition aggregate")
    eligible = aggregate.get("eligible_bytes")
    if not isinstance(eligible, int) or eligible < 0:
        raise ProtocolError("source-recognition denominator is malformed")
    manifest: dict[str, object] = {
        "blobs": blobs,
        "denominator": {"value": eligible, "unit": "decoded_utf8_bytes"},
    }
    return manifest, aggregate


def _projection_input(
    operation: str, campaign: AuthenticatedCampaign, paths: RuntimePaths
) -> tuple[str, dict[str, object], dict[str, tuple[str, ...]]]:
    runtime = _read_object(paths.runtime, "runtime inputs")
    candidate = runtime.get("candidate")
    if not isinstance(candidate, dict):
        raise ProtocolError("runtime candidate is malformed")
    identity_ref = campaign.readiness_receipt["qualification_identity_ref"]
    abc_root = campaign.config.candidate_tree / "abc"
    if operation == "capture-core":
        return (
            "core",
            {
                "qualification_identity_ref": identity_ref,
                "store": {"root": str(paths.core_root / "records")},
                "policy": _read_object(
                    abc_root / "data/parser-rq-core-attempt-policy-v1.json", "core policy"
                ),
                "candidate": candidate,
                "index": _read_object(paths.core_root / "core-index.json", "core index"),
            },
            {"core_attempt": ("fatal_failures", "wall_time_seconds", "timeouts")},
        )
    if operation == "capture-predicate-pair":
        output = paths.predicate_root / "output"
        return (
            "predicate-pair",
            {
                "qualification_identity_ref": identity_ref,
                "store": {"root": str(paths.predicate_root / "store")},
                "diagnostic_policy": _read_object(
                    abc_root / "data/parser-rq-diagnostic-completeness-policy-v1.json",
                    "diagnostic policy",
                ),
                "parser_ir_policy": _read_object(
                    abc_root / "data/parser-rq-parser-ir-conformance-policy-v1.json",
                    "Parser-IR policy",
                ),
                "diagnostic_index": _read_object(
                    output / "raw-diagnostics-index.json", "raw diagnostic index"
                ),
                "parser_ir_index": _read_object(output / "parser-ir-index.json", "Parser-IR index"),
            },
            {
                "diagnostic_completeness": ("diagnostic_completeness",),
                "parser_ir_conformance": ("parser_ir_schema_validation",),
            },
        )
    if operation in {"capture-source", "derive-diagnostic-gap"}:
        manifest, aggregate = _source_projection_values(
            campaign, paths, diagnostic=operation == "derive-diagnostic-gap"
        )
        if operation == "capture-source":
            return (
                "source-recognition",
                {
                    "qualification_identity_ref": identity_ref,
                    "store": {"root": str(paths.source_root / "store")},
                    "manifest": manifest,
                    "aggregate": aggregate,
                    "identity": candidate["qualification_identity"],
                },
                {"source_recognition": ("source_span_coverage",)},
            )
        return (
            "diagnostic-gap",
            {
                "qualification_identity_ref": identity_ref,
                "store": {"root": str(paths.source_root / "store")},
                "manifest": manifest,
                "identity": candidate["qualification_identity"],
            },
            {"diagnostic_gap": ("silent_drops",)},
        )
    if operation == "capture-publication":
        return (
            "publication",
            {
                "qualification_identity_ref": identity_ref,
                "store": {"root": str(paths.publication_root / "store")},
                "manifest": _read_object(
                    paths.publication_root / "manifest.json", "publication manifest"
                ),
                "index": _read_object(paths.publication_root / "index.json", "publication index"),
                "identity": candidate["qualification_identity"],
            },
            {"publication_structure": ("publication_structure",)},
        )
    if operation == "capture-resource":
        index = _read_object(paths.resource_root / "index.json", "resource index")
        records = index.get("records")
        if not isinstance(records, list):
            raise ProtocolError("resource records are malformed")
        return (
            "resource",
            {
                "qualification_identity_ref": identity_ref,
                "policy": _read_object(
                    abc_root / "data/parser-rq-resource-policy-v1.json", "resource policy"
                ),
                "identity": _read_object(
                    campaign.config.candidate_tree
                    / "ab-validator/data/parser-rq-resource-identity-v1.json",
                    "resource identity",
                ),
                "index": index,
                "records": records,
            },
            {"resource": ("peak_cgroup_memory_bytes",)},
        )
    raise ProtocolError(f"unknown projection operation: {operation}")


def _project_operation(
    operation: str,
    campaign: AuthenticatedCampaign,
    paths: RuntimePaths,
    runner: Runner,
    cwd: Path,
) -> None:
    command, inputs, members = _projection_input(operation, campaign, paths)
    projection_root = paths.root / "projections" / operation
    input_path = projection_root / "inputs.json"
    output_path = projection_root / "projected.json"
    _atomic_json(input_path, inputs)
    _run_checked(
        runner,
        _clojure(
            campaign,
            "-M:abc/parser-rq-member",
            command,
            "--inputs",
            str(input_path),
            "--out",
            str(output_path),
        ),
        cwd,
    )
    projected = _read_object(output_path, "member projection")
    expected_keys = {key for keys in members.values() for key in keys}
    if set(projected) != expected_keys:
        raise ProtocolError("member projection output membership differs")
    for member, keys in members.items():
        _atomic_json(paths.root / f"{member}.json", {key: projected[key] for key in keys})


def _clojure(
    campaign: AuthenticatedCampaign, alias: str, command: str, *args: str
) -> tuple[str, ...]:
    return campaign.clojure_prefix + (alias, command, *args)


def _prepare_commands(
    campaign: AuthenticatedCampaign, paths: RuntimePaths
) -> tuple[tuple[str, ...], ...]:
    config = campaign.config
    return (
        _clojure(
            campaign,
            "-M:abc/parser-rq-campaign",
            "verify-authorization-record",
            "--candidate",
            str(config.candidate),
            "--provenance",
            str(config.provenance),
            "--graph",
            str(config.candidate_tree / "abc/data/parser-rq-production-graph-v1.json"),
            "--receipt",
            str(config.readiness_receipt),
            "--authorization",
            str(config.authorization),
        ),
        (
            sys.executable,
            _driver(campaign, "abc/tools/parser_rq_campaign_site.py"),
            "recheck-readiness",
            "--policy",
            str(config.site_policy),
            "--site-descriptor",
            str(config.site_descriptor),
            "--receipt",
            str(config.readiness_receipt),
        ),
        _clojure(
            campaign,
            "-M:abc/parser-rq-campaign",
            "runtime-inputs",
            "--candidate",
            str(config.candidate),
            "--authorization",
            str(config.authorization),
            "--out",
            str(paths.runtime),
        ),
    )


def execute_graph(
    campaign: AuthenticatedCampaign,
    runner: Runner,
    *,
    now: Callable[[], datetime] = lambda: datetime.now(UTC),
) -> Terminal:
    config = campaign.config
    paths = RuntimePaths.below(config.staging_root)
    cwd = paths.root / "detached-cwd"
    cwd.mkdir(parents=True, exist_ok=True)
    capture_started = False
    lock: LockCapability | None = None
    try:
        for command in _prepare_commands(campaign, paths):
            _run_checked(runner, command, cwd)
        _materialize_runtime(paths)
        lock = _acquire_lock(Path(str(campaign.site_descriptor["campaign_lock_path"])))
        captured_at = now().astimezone(UTC).isoformat().replace("+00:00", "Z")
        temporal = _clojure(
            campaign,
            "-M:abc/parser-rq-campaign",
            "verify-authorization",
            "--candidate",
            str(config.candidate),
            "--provenance",
            str(config.provenance),
            "--graph",
            str(config.candidate_tree / "abc/data/parser-rq-production-graph-v1.json"),
            "--receipt",
            str(config.readiness_receipt),
            "--authorization",
            str(config.authorization),
            "--utc",
            captured_at,
            "--clock-synchronized",
            "true",
        )
        _run_checked(runner, temporal, cwd)
        if not _lock_retained(lock):
            raise ProtocolError("campaign lock was lost before capture start")
        _atomic_json(
            paths.root / "capture-start.json",
            {"status": "started", "capture_started_at_utc": captured_at},
        )
        capture_started = True
        for operation in campaign.operations:
            if not _lock_retained(lock):
                raise ProtocolError("campaign lock was lost during capture")
            if operation == "derive-diagnostic-gap":
                _prepare_diagnostic_input(campaign, paths)
            command = operation_argv(operation, campaign, paths, lock)
            inherited = (lock.fd,) if operation == "capture-core" else ()
            _run_checked(runner, command, cwd, pass_fds=inherited)
            _project_operation(operation, campaign, paths, runner, cwd)
        expected = {f"{name}.json" for name in EXPECTED_INSTALLED_MEMBERS}
        actual = {path.name for path in paths.root.glob("*.json") if path.name in expected}
        if actual != expected:
            raise ProtocolError("canonical member set is incomplete")
        _run_checked(
            runner,
            _clojure(
                campaign,
                "-M:abc/parser-rq-campaign",
                "compose",
                "--candidate",
                str(config.candidate),
                "--authorization",
                str(config.authorization),
                "--capture-root",
                str(paths.root),
                "--capture-started-at",
                captured_at,
                "--out",
                str(paths.root / "measurements.edn"),
            ),
            cwd,
        )
        _run_checked(
            runner,
            _clojure(
                campaign,
                "-M:abc/parser-rq-campaign",
                "verify-capture",
                "--candidate",
                str(config.candidate),
                "--authorization",
                str(config.authorization),
                "--capture-root",
                str(paths.root),
            ),
            cwd,
        )
        return Terminal("captured")
    except (OSError, KeyError, ValueError) as error:
        if not capture_started:
            return Terminal("preparation_failed", str(error))
        _atomic_json(
            paths.root / "unavailable-terminal.json",
            {"status": "unavailable", "reason": str(error)},
        )
        return Terminal("unavailable", str(error))
    finally:
        if lock is not None:
            os.close(lock.fd)


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
        campaign = authenticate_inputs(CampaignConfig(**vars(args)))
        terminal = execute_graph(campaign, SubprocessRunner())
        if terminal.status != "captured":
            print(terminal.reason or terminal.status, file=sys.stderr)
            return 2
    except (OSError, PreparationFailed) as error:
        print(str(error), file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
