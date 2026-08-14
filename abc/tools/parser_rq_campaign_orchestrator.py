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
from typing import Any, Callable, Collection, Protocol

sys.path.insert(0, str(Path(__file__).resolve().parent))
from legacy_json_c14n import canonical_json  # noqa: E402


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

# The only operations that read corpus BYTES, and so the only ones needing a
# resolved corpus root. Every other operation works from the runtime corpus
# object alone, so it must not acquire a dependency on one.
CORPUS_BYTE_CONSUMERS = ("capture-core", "capture-resource")

EXPECTED_INSTALLED_MEMBERS = (
    "core_attempt",
    "source_recognition",
    "diagnostic_gap",
    "diagnostic_completeness",
    "parser_ir_conformance",
    "publication_structure",
    "resource",
)

# Policies that restate corpus membership, and the field each restates it in.
# The restatement is closed-membership authentication, not duplication: an
# instrument folds an exact workset and fails closed on any divergence. But a
# restatement only authenticates if something compares it to the authority it
# restates, and two of these were not compared. Resource measured its own
# policy's work ids against an index built from the same policy, so a stale
# policy silently measured a subset; core-attempt failed only indirectly, when
# a work with no corpus source produced no report and the missing-report error
# named the symptom. The publication policy is absent because it carries no
# membership -- publication membership lives in its fixtures and capture input.
MEMBERSHIP_POLICIES = (
    ("abc/data/parser-rq-core-attempt-policy-v1.json", "expected_work_ids"),
    ("abc/data/parser-rq-diagnostic-completeness-policy-v1.json", "expected_work_ids"),
    ("abc/data/parser-rq-parser-ir-conformance-policy-v1.json", "expected_work_ids"),
    ("abc/data/parser-rq-resource-policy-v1.json", "work_ids"),
)

PUBLICATION_ARTIFACTS = {
    "tei.xml": "application/tei+xml",
    "plain.txt": "text/plain; charset=utf-8",
    "preservation.json": "application/json",
    "tei.manifest.json": "application/json",
    "plaintext.manifest.json": "application/json",
    "tei-validation-result.json": "application/json",
}

RESOURCE_COMMAND_HASH = "sha256:d242d1afa7af9533b84be49fb758075aa92c9757a590bb18d9eb3e21fe87e5bc"

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
    ab_check_index: Path
    ab_check_work_ids: Path
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
            ab_check_index=root / "ab-check-index.json",
            ab_check_work_ids=root / "ab-check-work-ids.json",
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


def _wiring_commands(campaign: AuthenticatedCampaign) -> tuple[tuple[str, ...], ...]:
    executable_commands = (
        (str(campaign.executables["ab-check"]), "--help"),
        (str(campaign.executables["ab-aozora"]), "--version"),
        (str(campaign.executables["ab-aat-to-parser-ir"]), "--help"),
        (
            str(campaign.executables["ab-parser-rq-source-accountability"]),
            "capture-corpus",
            "--help",
        ),
        (
            str(campaign.executables["ab-parser-rq-diagnostic-authorization"]),
            "capture-corpus",
            "--help",
        ),
    )
    driver_commands = tuple(
        (sys.executable, str(path), "--help") for path in sorted(campaign.drivers.values())
    )
    return executable_commands + driver_commands


def _verify_wiring(campaign: AuthenticatedCampaign, runner: Runner, cwd: Path) -> None:
    for command in _wiring_commands(campaign):
        _run_checked(runner, command, cwd)


def _canonical_bytes(value: object) -> bytes:
    return canonical_json(value).encode()


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
    ):
        _below(evidence_tree, path, label)
    try:
        config.site_descriptor.resolve(strict=True)
    except OSError as error:
        raise PreparationFailed("site descriptor is unavailable") from error

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
    site_descriptor = _read_object(config.site_descriptor, "site descriptor")
    if provenance.get("status") != "reproducible":
        raise PreparationFailed("candidate provenance is not reproducible")
    if receipt.get("production_graph_hash") != graph.get("policy_hash"):
        raise PreparationFailed("readiness receipt binds another production graph")
    if receipt.get("provenance_core_ref") != provenance.get("provenance_core_ref"):
        raise PreparationFailed("readiness receipt binds another provenance core")
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
            "bash",
            "-c",
            'cd "$0" && exec clojure "$@"',
            str(candidate_tree / "abc"),
        ),
    )


def _driver(campaign: AuthenticatedCampaign, relative: str) -> str:
    return str(campaign.drivers[relative])


def resolve_corpus_root(candidate_tree: Path, corpus_root: object) -> Path:
    """Physical corpus root for every capture that reads corpus bytes.

    The governed corpus artifact declares `corpus_root` as a repository-relative
    path; this resolves it below the authenticated candidate tree. It exists so
    that knowledge lives in exactly one place: previously the literal
    `ab-validator/crates/ab-index/tests/fixtures/corpus` was restated in two
    capture paths, spelled two different ways, and neither read the governed
    value — so the three could silently disagree.
    """
    if not isinstance(corpus_root, str) or not corpus_root:
        raise ProtocolError("governed corpus root is malformed")
    relative = Path(corpus_root)
    if relative.is_absolute() or ".." in relative.parts:
        raise ProtocolError("governed corpus root escapes the candidate tree")
    try:
        tree = candidate_tree.resolve(strict=True)
        resolved = (tree / relative).resolve(strict=True)
    except OSError as error:
        raise ProtocolError("governed corpus root is unavailable") from error
    if not resolved.is_relative_to(tree):
        raise ProtocolError("governed corpus root escapes the candidate tree")
    if not resolved.is_dir():
        raise ProtocolError("governed corpus root is not a directory")
    return resolved


def _runtime_corpus_root(campaign: AuthenticatedCampaign, paths: RuntimePaths) -> Path:
    runtime = _read_object(paths.runtime, "runtime inputs")
    corpus = runtime.get("corpus")
    return resolve_corpus_root(
        campaign.config.candidate_tree,
        corpus.get("corpus_root") if isinstance(corpus, dict) else None,
    )


def operation_argv(
    operation: str,
    campaign: AuthenticatedCampaign,
    paths: RuntimePaths,
    lock: LockCapability,
    corpus_root: Path | None = None,
) -> tuple[str, ...]:
    candidate_tree = campaign.config.candidate_tree.resolve()
    abc_root = candidate_tree / "abc"
    ab_root = candidate_tree / "ab-validator"
    if operation == "capture-core":
        if corpus_root is None:
            raise ProtocolError("core capture requires a governed corpus root")
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
            "--adapter",
            str(campaign.executables["ab-aozora"]),
            "--corpus-root",
            str(corpus_root),
            "--corpus-index",
            str(paths.ab_check_index),
            "--work-ids",
            str(paths.ab_check_work_ids),
            "--time-executable",
            str(campaign.executables["ab-check"].parent / "time"),
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


def _file_blob(path: Path, media_type: str) -> dict[str, object]:
    try:
        with path.open("rb") as stream:
            digest = hashlib.file_digest(stream, "sha256").hexdigest()
        size = path.stat().st_size
    except OSError as error:
        raise ProtocolError(f"publication artifact is unavailable: {path.name}") from error
    return {"sha256": f"sha256:{digest}", "bytes": size, "media_type": media_type}


def _publication_capture_input(
    campaign: AuthenticatedCampaign, paths: RuntimePaths
) -> dict[str, object]:
    runtime = _read_object(paths.runtime, "runtime inputs")
    corpus = runtime.get("corpus")
    if not isinstance(corpus, dict) or not isinstance(corpus.get("entries"), list):
        raise ProtocolError("publication corpus is malformed")
    abc_root = campaign.config.candidate_tree / "abc"
    policy = _read_object(
        abc_root / "data/parser-rq-publication-policy-v1.json", "publication policy"
    )
    fixtures = _read_object(
        abc_root / "data/parser-rq-publication-fixtures-v1.json", "publication census"
    )
    fixture_works = fixtures.get("works")
    entries = corpus["entries"]
    expected = {row.get("work_id") for row in entries if isinstance(row, dict)}
    if not isinstance(fixture_works, dict) or expected != set(fixture_works):
        raise ProtocolError("publication census does not have exact corpus membership")
    works: list[dict[str, object]] = []
    for entry in entries:
        if not isinstance(entry, dict):
            raise ProtocolError("publication corpus entry is malformed")
        work_id = entry.get("work_id")
        source_sha256 = entry.get("source_sha256")
        if not isinstance(work_id, str) or not isinstance(source_sha256, str):
            raise ProtocolError("publication corpus identity is malformed")
        summary = _read_object(
            paths.publication_root / "validated" / f"{work_id}.json",
            f"publication validator summary for {work_id}",
        )
        join_input = summary.get("join_input")
        checks = summary.get("structure_check_candidates")
        counts = summary.get("counts")
        if (
            not isinstance(join_input, dict)
            or not isinstance(checks, dict)
            or not isinstance(counts, dict)
        ):
            raise ProtocolError(f"publication validator summary is incomplete for {work_id}")
        artifacts = [
            _file_blob(paths.publication_root / "materialized" / work_id / relative, media_type)
            for relative, media_type in PUBLICATION_ARTIFACTS.items()
        ]
        works.append(
            {
                "work_id": work_id,
                "source_sha256": source_sha256,
                "parser_disposition": "parsed",
                "publication": {
                    "join_input_valid": join_input.get("status") == "valid",
                    "structure_check_candidates": checks,
                    "counts": counts,
                    "artifacts": artifacts,
                },
            }
        )
    preservation = policy.get("preservation_schema")
    validator = policy.get("validator")
    if not isinstance(preservation, dict) or not isinstance(validator, dict):
        raise ProtocolError("publication policy authority is malformed")
    return {
        "corpus": corpus,
        "authority": {
            "qualification_identity_ref": campaign.readiness_receipt["qualification_identity_ref"],
            "policy_hash": policy.get("policy_hash"),
            "preservation_schema_hash": preservation.get("hash"),
            "validator_semantics_hash": validator.get("semantics_hash"),
            "census_hash": fixtures.get("census_hash"),
        },
        "works": works,
    }


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
    corpus = runtime.get("corpus")
    entries = corpus.get("entries") if isinstance(corpus, dict) else None
    corpus_root = corpus.get("corpus_root") if isinstance(corpus, dict) else None
    if not isinstance(entries, list) or not isinstance(corpus_root, str):
        raise ProtocolError("runtime corpus is malformed")
    index_works: list[dict[str, object]] = []
    work_ids: list[str] = []
    prefix = f"{corpus_root.rstrip('/')}/"
    for entry in entries:
        if not isinstance(entry, dict):
            raise ProtocolError("runtime corpus entry is malformed")
        work_id = entry.get("work_id")
        source_path = entry.get("source_path")
        if not isinstance(work_id, str) or not isinstance(source_path, str):
            raise ProtocolError("runtime corpus identity is malformed")
        if not source_path.startswith(prefix):
            raise ProtocolError("runtime source is outside the pinned corpus root")
        relative = source_path[len(prefix) :]
        index_works.append({"id": work_id, "txt_path": relative, "features": []})
        work_ids.append(work_id)
    _atomic_json(paths.ab_check_index, {"works": index_works})
    _atomic_json(paths.ab_check_work_ids, work_ids)


def _authenticate_policy_membership(candidate_tree: Path, corpus: dict[str, Any]) -> None:
    """Every membership-carrying policy must name exactly the governed corpus.

    Order is significant. The instruments compare their expected and actual
    work-id lists as sequences, so a policy holding the right works in the
    wrong order is a divergence there and must be one here.

    `expected_sources` additionally pins each work's source bytes, which the
    corpus pins too. Membership equality alone would let a policy claim the
    right works against the wrong bytes, so the hashes are compared as well.

    `expected_diagnostics` is the same shape of claim one level further in:
    the diagnostic-completeness instrument measures each work against it, so
    a restatement that disagrees with the corpus would silently move what the
    predicate is testing.
    """
    entries = corpus.get("entries")
    if not isinstance(entries, list):
        raise ProtocolError("governed corpus membership is malformed")
    work_ids: list[str] = []
    source_hashes: dict[str, object] = {}
    expected_diagnostics: dict[str, list[str]] = {}
    for entry in entries:
        if not isinstance(entry, dict) or not isinstance(entry.get("work_id"), str):
            raise ProtocolError("governed corpus membership is malformed")
        work_ids.append(entry["work_id"])
        source_hashes[entry["work_id"]] = entry.get("source_sha256")
        codes = entry.get("expected_diagnostics")
        if not isinstance(codes, list) or any(not isinstance(code, str) for code in codes):
            raise ProtocolError("governed corpus membership is malformed")
        expected_diagnostics[entry["work_id"]] = sorted(codes)
    for relative, field in MEMBERSHIP_POLICIES:
        policy = _read_object(candidate_tree / relative, f"{relative} policy")
        declared = policy.get(field)
        if not isinstance(declared, list) or any(
            not isinstance(work_id, str) for work_id in declared
        ):
            raise ProtocolError(f"{relative} membership is malformed")
        if declared != work_ids:
            raise ProtocolError(f"{relative} membership differs from the governed corpus")
        declared_diagnostics = policy.get("expected_diagnostics")
        if declared_diagnostics is not None:
            if not isinstance(declared_diagnostics, dict) or any(
                not isinstance(codes, list) or any(not isinstance(code, str) for code in codes)
                for codes in declared_diagnostics.values()
            ):
                raise ProtocolError(f"{relative} membership is malformed")
            if {
                work: sorted(codes) for work, codes in declared_diagnostics.items()
            } != expected_diagnostics:
                raise ProtocolError(
                    f"{relative} diagnostic expectation differs from the governed corpus"
                )
        sources = policy.get("expected_sources")
        if sources is None:
            continue
        if not isinstance(sources, list) or len(sources) != len(work_ids):
            raise ProtocolError(f"{relative} membership is malformed")
        for entry in sources:
            if not isinstance(entry, dict) or entry.get("work_id") not in source_hashes:
                raise ProtocolError(f"{relative} membership is malformed")
            if entry.get("source_sha256") != source_hashes[entry["work_id"]]:
                raise ProtocolError(f"{relative} source identity differs from the governed corpus")


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
        manifest_path = store / str(row.get("locator"))
        if _sha256(manifest_path) != row.get("sha256"):
            raise ProtocolError("classified-source generation blob identity differs")
        manifest = _read_object(manifest_path, "classified-source generation")
        if manifest.get("work_id") != row.get("work_id"):
            raise ProtocolError("classified-source generation work identity differs")
        generation_by_work[str(row.get("work_id"))] = manifest
    records: list[dict[str, object]] = []
    for row in recognition_index.get("records", []):
        if not isinstance(row, dict):
            raise ProtocolError("source-recognition row is malformed")
        work_id = str(row.get("work_id"))
        generation = generation_by_work.get(work_id)
        if generation is None:
            raise ProtocolError("source-recognition generation is absent")
        if generation.get("generation_ref") != row.get("capture_generation_ref"):
            raise ProtocolError("source-recognition generation identity differs")
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
            "policy_artifact_hash": _sha256(policy),
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
                {"source_recognition": ("source_span_coverage", "metadata_attribution")},
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
        runtime = _read_object(paths.runtime, "runtime inputs")
        corpus = runtime.get("corpus")
        entries = corpus.get("entries") if isinstance(corpus, dict) else None
        if not isinstance(entries, list):
            raise ProtocolError("publication corpus is malformed")
        publication_identity = {
            **candidate["qualification_identity"],
            "identity_ref": identity_ref,
            "entries": entries,
        }
        return (
            "publication",
            {
                "qualification_identity_ref": identity_ref,
                "store": {"root": str(paths.publication_root / "store")},
                "manifest": _read_object(
                    paths.publication_root / "manifest.json", "publication manifest"
                ),
                "index": _read_object(paths.publication_root / "index.json", "publication index"),
                "identity": publication_identity,
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


def _prepare_publication(
    campaign: AuthenticatedCampaign,
    paths: RuntimePaths,
    runner: Runner,
    cwd: Path,
) -> None:
    abc_root = campaign.config.candidate_tree / "abc"
    ab_root = campaign.config.candidate_tree / "ab-validator"
    fixture_root = abc_root / "test/fixtures/parser-rq/publication-inputs"
    fixtures_path = abc_root / "data/parser-rq-publication-fixtures-v1.json"
    materialized = paths.publication_root / "materialized"
    validated = paths.publication_root / "validated"
    runtime = _read_object(paths.runtime, "runtime inputs")
    qualification_identity = runtime.get("candidate", {}).get("qualification_identity", {})
    corpus_snapshot_hash = qualification_identity.get("corpus_snapshot_hash")
    if not isinstance(corpus_snapshot_hash, str):
        raise ProtocolError("publication corpus snapshot identity is malformed")
    _atomic_json(
        paths.predicate_root / "output/parser-ir/source.manifest.json",
        {"manifest_identity_object": {"corpus_snapshot_hash": corpus_snapshot_hash}},
    )
    _run_checked(
        runner,
        _clojure(
            campaign,
            "-M:abc/parser-rq-publication-materialize",
            "--parser-ir-root",
            str(paths.predicate_root / "output/parser-ir"),
            "--fixture-root",
            str(fixture_root),
            "--fixtures",
            str(fixtures_path),
            "--output-root",
            str(materialized),
        ),
        cwd,
    )
    corpus = runtime.get("corpus")
    entries = corpus.get("entries") if isinstance(corpus, dict) else None
    if not isinstance(entries, list):
        raise ProtocolError("publication corpus is malformed")
    parser_git_rev = (
        runtime.get("candidate", {}).get("qualification_identity", {}).get("parser_git_rev")
    )
    if not isinstance(parser_git_rev, str):
        raise ProtocolError("publication candidate revision is malformed")
    for entry in entries:
        if not isinstance(entry, dict) or not isinstance(entry.get("work_id"), str):
            raise ProtocolError("publication corpus entry is malformed")
        work_id = entry["work_id"]
        _run_checked(
            runner,
            (
                sys.executable,
                _driver(
                    campaign,
                    "ab-validator/reports/parser-ir/publication-bundle-validate.py",
                ),
                "--parser-ir",
                str(paths.predicate_root / "output/parser-ir" / f"{work_id}.json"),
                "--parser-ir-schema",
                str(abc_root / "schemas/parser-ir.schema.json"),
                "--preservation-schema",
                str(abc_root / "schemas/parser-ir-publication-preservation.schema.json"),
                "--validator-identity",
                str(ab_root / "data/parser-rq-publication-validator-v1.json"),
                "--publication-dir",
                str(materialized / work_id),
                "--abc-commit",
                parser_git_rev,
                "--command",
                "parser-rq-production-graph/v1:capture-publication",
                "--summary-json",
                str(validated / f"{work_id}.json"),
                "--report-md",
                str(validated / f"{work_id}.md"),
            ),
            cwd,
        )
    _atomic_json(
        paths.publication_root / "capture-input.json", _publication_capture_input(campaign, paths)
    )


def _prepare_resource(
    campaign: AuthenticatedCampaign, paths: RuntimePaths, corpus_root: Path | None
) -> None:
    if corpus_root is None:
        raise ProtocolError("resource capture requires a governed corpus root")
    runtime = _read_object(paths.runtime, "runtime inputs")
    corpus = runtime.get("corpus")
    entries = corpus.get("entries") if isinstance(corpus, dict) else None
    if not isinstance(entries, list):
        raise ProtocolError("resource corpus is malformed")
    work_ids_root = paths.resource_root / "work-ids"
    for entry in entries:
        if not isinstance(entry, dict) or not isinstance(entry.get("work_id"), str):
            raise ProtocolError("resource corpus entry is malformed")
        work_id = entry["work_id"]
        _atomic_json(work_ids_root / f"{work_id}.json", [work_id])
    argv = [
        str(campaign.executables["ab-check"]),
        "--index",
        str(paths.ab_check_index),
        "--corpus",
        str(corpus_root),
        "--adapter",
        str(campaign.executables["ab-aozora"]),
        "--output",
        str(paths.resource_root / "parser-output/{work_id}"),
        "--work-ids",
        str(work_ids_root / "{work_id}.json"),
        "--jobs",
        "1",
        "--per-work-timeout",
        "60s",
    ]
    policy = _read_object(
        campaign.config.candidate_tree / "abc/data/parser-rq-resource-policy-v1.json",
        "resource policy",
    )
    if policy.get("production_command_hash") != RESOURCE_COMMAND_HASH:
        raise ProtocolError("resource production command identity differs")
    _atomic_json(
        paths.resource_root / "command-template.json",
        {"production_command_hash": RESOURCE_COMMAND_HASH, "argv": argv},
    )


def _execute_operation(
    operation: str,
    campaign: AuthenticatedCampaign,
    paths: RuntimePaths,
    lock: LockCapability,
    runner: Runner,
    cwd: Path,
) -> None:
    if operation == "derive-diagnostic-gap":
        _prepare_diagnostic_input(campaign, paths)
    if operation == "capture-publication":
        _prepare_publication(campaign, paths, runner, cwd)
    corpus_root = (
        _runtime_corpus_root(campaign, paths) if operation in CORPUS_BYTE_CONSUMERS else None
    )
    if operation == "capture-resource":
        _prepare_resource(campaign, paths, corpus_root)
    command = operation_argv(operation, campaign, paths, lock, corpus_root)
    inherited = (lock.fd,) if operation == "capture-core" else ()
    _run_checked(runner, command, cwd, pass_fds=inherited)
    _project_operation(operation, campaign, paths, runner, cwd)


def _assert_member_set(paths: RuntimePaths, expected_members: Collection[str]) -> None:
    expected = {f"{name}.json" for name in expected_members}
    installed = {f"{name}.json" for name in EXPECTED_INSTALLED_MEMBERS}
    actual = {path.name for path in paths.root.glob("*.json") if path.name in installed}
    if actual != expected:
        raise ProtocolError(
            f"canonical member mismatch: expected={sorted(expected)} actual={sorted(actual)}"
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
        _verify_wiring(campaign, runner, cwd)
        _materialize_runtime(paths)
        # Before the lock, before any capture: a policy that disagrees with the
        # governed corpus can only produce evidence about a workset nobody
        # authorized, so the divergence is worth naming here rather than
        # surfacing later as a missing report.
        _authenticate_policy_membership(
            config.candidate_tree, _read_object(paths.corpus, "governed corpus")
        )
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
            _execute_operation(operation, campaign, paths, lock, runner, cwd)
        _assert_member_set(paths, EXPECTED_INSTALLED_MEMBERS)
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
