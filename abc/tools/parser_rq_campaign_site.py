#!/usr/bin/env python3
"""Authenticate portable runtime prerequisites for parser RQ capture."""

from __future__ import annotations

import argparse
import fcntl
import hashlib
import importlib.util
import json
import os
import shutil
import subprocess
import sys
import uuid
from dataclasses import dataclass
from pathlib import Path
from typing import Protocol


@dataclass(frozen=True)
class RuntimeFacts:
    clock_synchronized: bool
    lock_available: bool


class Probe(Protocol):
    def verify_writable(self, root: Path) -> dict[str, bool]: ...


class SiteUnavailable(ValueError):
    pass


def _canonical_bytes(value: object) -> bytes:
    encoded = json.dumps(value, ensure_ascii=True, sort_keys=True, separators=(",", ":"))
    return encoded.replace("/", "\\/").encode()


def content_ref(value: dict[str, object], *, excluding: str | None = None) -> str:
    projected = value if excluding is None else {k: v for k, v in value.items() if k != excluding}
    return "sha256:" + hashlib.sha256(_canonical_bytes(projected)).hexdigest()


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


def authenticate_runtime(descriptor: dict[str, object], facts: RuntimeFacts, probe: Probe) -> None:
    if set(descriptor) != {
        "schema_id",
        "schema_version",
        "evidence_store_root",
        "scratch_root",
        "campaign_lock_path",
    }:
        raise SiteUnavailable("runtime descriptor violates its closed contract")
    if (
        descriptor["schema_id"]
        != "https://w3id.org/abc/schemas/parser-rq-site-descriptor.schema.json"
        or descriptor["schema_version"] != "3.0.0"
    ):
        raise SiteUnavailable("runtime descriptor identity is unsupported")
    roots = [Path(str(descriptor[key])) for key in ("evidence_store_root", "scratch_root")]
    lock_path = Path(str(descriptor["campaign_lock_path"]))
    if not all(path.is_absolute() for path in [*roots, lock_path]):
        raise SiteUnavailable("runtime descriptor paths must be absolute")
    if not facts.clock_synchronized:
        raise SiteUnavailable("local clock is not synchronized")
    if not facts.lock_available:
        raise SiteUnavailable("campaign lock is unavailable")
    probe.verify_writable(roots[0].resolve())
    probe.verify_writable(roots[1].resolve())


def _authenticate_graph(graph: dict[str, object]) -> None:
    if graph.get("policy_hash") != content_ref(graph, excluding="policy_hash"):
        raise SiteUnavailable("production graph hash does not authenticate")


def preflight_runtime(
    graph: dict[str, object],
    descriptor: dict[str, object],
    facts: RuntimeFacts,
    probe: Probe,
    *,
    evidence_tree_clean: bool,
    independent_build_capability: str,
) -> None:
    _authenticate_graph(graph)
    if not evidence_tree_clean:
        raise SiteUnavailable("evidence tree is not clean")
    if independent_build_capability != "passed":
        raise SiteUnavailable("independent build capability did not pass")
    authenticate_runtime(descriptor, facts, probe)


def seal_readiness(
    graph: dict[str, object],
    descriptor: dict[str, object],
    facts: RuntimeFacts,
    probe: Probe,
    *,
    candidate_ref: str,
    qualification_identity_ref: str,
    provenance_core_ref: str,
    candidate_git_rev: str,
    candidate_tree_clean: bool,
    evidence_base_git_rev: str,
    evidence_tree_clean: bool,
    corpus_snapshot_hash: str,
    corpus_list_hash: str,
) -> dict[str, object]:
    _authenticate_graph(graph)
    if not candidate_tree_clean or not evidence_tree_clean:
        raise SiteUnavailable("candidate and evidence trees must be clean")
    authenticate_runtime(descriptor, facts, probe)
    value: dict[str, object] = {
        "schema_id": "https://w3id.org/abc/schemas/parser-rq-readiness-receipt.schema.json",
        "schema_version": "2.0.0",
        "candidate_ref": candidate_ref,
        "qualification_identity_ref": qualification_identity_ref,
        "provenance_core_ref": provenance_core_ref,
        "production_graph_hash": graph["policy_hash"],
        "production_graph_version": graph["schema_version"],
        "candidate_git_rev": candidate_git_rev,
        "candidate_tree_clean": True,
        "evidence_base_git_rev": evidence_base_git_rev,
        "evidence_tree_clean": True,
        "corpus_snapshot_hash": corpus_snapshot_hash,
        "corpus_list_hash": corpus_list_hash,
    }
    value["readiness_receipt_ref"] = content_ref(value, excluding="readiness_receipt_ref")
    return value


def recheck_readiness(
    receipt: dict[str, object],
    descriptor: dict[str, object],
    facts: RuntimeFacts,
    probe: Probe,
) -> None:
    if receipt.get("readiness_receipt_ref") != content_ref(
        receipt, excluding="readiness_receipt_ref"
    ):
        raise SiteUnavailable("readiness receipt does not authenticate")
    authenticate_runtime(descriptor, facts, probe)


class FileProbe:
    def verify_writable(self, root: Path) -> dict[str, bool]:
        root = root.resolve(strict=True)
        path = root / f".parser-rq-site-probe-{uuid.uuid4().hex}"
        payload = os.urandom(64)
        created = False
        try:
            with path.open("xb") as stream:
                created = True
                stream.write(payload)
                stream.flush()
                os.fsync(stream.fileno())
            _fsync_directory(root)
            digest = hashlib.sha256()
            with path.open("rb") as stream:
                while chunk := stream.read(1024 * 1024):
                    digest.update(chunk)
            if digest.digest() != hashlib.sha256(payload).digest():
                raise SiteUnavailable("evidence probe bytes differ")
            path.unlink()
            created = False
            _fsync_directory(root)
        finally:
            if created:
                path.unlink(missing_ok=True)
        return {
            "create": True,
            "fsync_file": True,
            "fsync_directory": True,
            "stream_read": True,
            "remove": True,
        }


def _fsync_directory(path: Path) -> None:
    descriptor = os.open(path, os.O_RDONLY | os.O_DIRECTORY)
    try:
        os.fsync(descriptor)
    finally:
        os.close(descriptor)


def _clock_synchronized() -> bool:
    completed = subprocess.run(
        ["timedatectl", "show", "-p", "NTPSynchronized", "--value"],
        check=False,
        capture_output=True,
        text=True,
    )
    return completed.returncode == 0 and completed.stdout.strip().lower() == "yes"


def _lock_available(path: Path) -> bool:
    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor = os.open(path, os.O_RDWR | os.O_CREAT, 0o600)
    try:
        try:
            fcntl.flock(descriptor, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            return False
        fcntl.flock(descriptor, fcntl.LOCK_UN)
        return True
    finally:
        os.close(descriptor)


def collect_runtime_facts(descriptor: dict[str, object]) -> RuntimeFacts:
    return RuntimeFacts(
        clock_synchronized=_clock_synchronized(),
        lock_available=_lock_available(Path(str(descriptor["campaign_lock_path"]))),
    )


def independent_build_capability(repository_root: Path, scratch_root: Path) -> str:
    module_path = (
        repository_root
        / "ab-validator"
        / "reports"
        / "parser-ir"
        / "parser-rq-campaign-provenance.py"
    )
    specification = importlib.util.spec_from_file_location(
        "parser_rq_campaign_provenance_for_site", module_path
    )
    if specification is None or specification.loader is None:
        raise SiteUnavailable("provenance realization module cannot be loaded")
    provenance = importlib.util.module_from_spec(specification)
    specification.loader.exec_module(provenance)
    capability_root = scratch_root / f"build-capability-{uuid.uuid4().hex}"
    runner = provenance.SubprocessRunner()
    try:
        request = provenance.resolve_realize_request(
            repository_root,
            "build-a",
            capability_root / "store",
            capability_root / "build.log",
            runner,
            package_ref=f"{repository_root / 'abc'}#packages.x86_64-linux.parser-rq-build-capability-probe",
        )
        provenance.realize_target(request, runner)
    except (OSError, ValueError) as error:
        raise SiteUnavailable("independent build capability probe failed") from error
    finally:
        shutil.rmtree(capability_root, ignore_errors=True)
    return "passed"


def _read_json(path: Path) -> dict[str, object]:
    value = json.loads(path.read_bytes())
    if not isinstance(value, dict):
        raise SiteUnavailable(f"{path} is not a JSON object")
    return value


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    common = argparse.ArgumentParser(add_help=False)
    common.add_argument("--site-descriptor", type=Path, required=True)
    preflight = commands.add_parser("preflight-site", parents=[common])
    preflight.add_argument("--graph", type=Path, required=True)
    preflight.add_argument("--evidence-tree-clean", choices=("true", "false"), required=True)
    seal = commands.add_parser("seal-readiness", parents=[common])
    seal.add_argument("--graph", type=Path, required=True)
    for name in (
        "candidate-ref",
        "qualification-identity-ref",
        "provenance-core-ref",
        "candidate-git-rev",
        "evidence-base-git-rev",
        "corpus-snapshot-hash",
        "corpus-list-hash",
    ):
        seal.add_argument(f"--{name}", required=True)
    seal.add_argument("--candidate-tree-clean", choices=("true", "false"), required=True)
    seal.add_argument("--evidence-tree-clean", choices=("true", "false"), required=True)
    seal.add_argument("--out", type=Path, required=True)
    recheck = commands.add_parser("recheck-readiness", parents=[common])
    recheck.add_argument("--receipt", type=Path, required=True)
    return parser


def main(argv: list[str] | None = None) -> int:
    parser = _parser()
    try:
        args = parser.parse_args(argv)
    except SystemExit as error:
        if argv is not None:
            return int(error.code)
        raise
    try:
        descriptor = _read_json(args.site_descriptor)
        facts = collect_runtime_facts(descriptor)
        probe = FileProbe()
        if args.command == "preflight-site":
            repository_root = Path(__file__).resolve().parents[2]
            preflight_runtime(
                _read_json(args.graph),
                descriptor,
                facts,
                probe,
                evidence_tree_clean=args.evidence_tree_clean == "true",
                independent_build_capability=independent_build_capability(
                    repository_root, Path(str(descriptor["scratch_root"]))
                ),
            )
        elif args.command == "seal-readiness":
            value = seal_readiness(
                _read_json(args.graph),
                descriptor,
                facts,
                probe,
                candidate_ref=args.candidate_ref,
                qualification_identity_ref=args.qualification_identity_ref,
                provenance_core_ref=args.provenance_core_ref,
                candidate_git_rev=args.candidate_git_rev,
                candidate_tree_clean=args.candidate_tree_clean == "true",
                evidence_base_git_rev=args.evidence_base_git_rev,
                evidence_tree_clean=args.evidence_tree_clean == "true",
                corpus_snapshot_hash=args.corpus_snapshot_hash,
                corpus_list_hash=args.corpus_list_hash,
            )
            _atomic_json(args.out, value)
        elif args.command == "recheck-readiness":
            recheck_readiness(_read_json(args.receipt), descriptor, facts, probe)
        else:
            raise SiteUnavailable("unknown site command")
    except (OSError, KeyError, ValueError, json.JSONDecodeError) as error:
        print(str(error), file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
