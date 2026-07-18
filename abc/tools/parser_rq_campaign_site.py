#!/usr/bin/env python3
"""Authenticate the host and external evidence replica for parser RQ capture."""

from __future__ import annotations

import argparse
import fcntl
import hashlib
import importlib.util
import json
import os
import shutil
import socket
import subprocess
import sys
import uuid
from dataclasses import dataclass
from pathlib import Path
from typing import Protocol


@dataclass(frozen=True)
class MountFact:
    root: Path
    source: str
    filesystem_type: str
    filesystem_id: str
    remote_authority: str | None = None
    authority_addresses: tuple[str, ...] = ()
    bind_mount: bool = False


@dataclass(frozen=True)
class SiteFacts:
    kernel_hostname: str
    stable_addresses: tuple[str, ...]
    local_addresses: tuple[str, ...]
    primary_mount: MountFact
    replica_mount: MountFact
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


def _mount_value(mount: MountFact) -> dict[str, object]:
    value: dict[str, object] = {
        "root": str(mount.root),
        "source": mount.source,
        "filesystem_type": mount.filesystem_type,
        "filesystem_id": mount.filesystem_id,
    }
    if mount.remote_authority is not None:
        value["remote_authority"] = mount.remote_authority
    return value


def _site_projection(
    descriptor: dict[str, object], facts: SiteFacts, replica_probe: dict[str, bool]
) -> dict[str, object]:
    return {
        "kernel_hostname": facts.kernel_hostname,
        "stable_host_label": descriptor["stable_host_label"],
        "stable_addresses": sorted(set(facts.stable_addresses)),
        "local_addresses": sorted(set(facts.local_addresses)),
        "primary_mount": _mount_value(facts.primary_mount),
        "replica_mount": _mount_value(facts.replica_mount),
        "clock_synchronized": facts.clock_synchronized,
        "lock_available": facts.lock_available,
        "replica_probe": replica_probe,
    }


def authenticate_site(
    policy: dict[str, object],
    descriptor: dict[str, object],
    facts: SiteFacts,
    probe: Probe,
) -> dict[str, object]:
    if policy.get("policy_hash") != content_ref(policy, excluding="policy_hash"):
        raise SiteUnavailable("site policy hash does not authenticate")
    if policy.get("replica_status") != "configured":
        raise SiteUnavailable("external replica is unconfigured")
    for key in (
        "stable_host_label",
        "kernel_hostname",
        "primary_failure_domain",
        "replica_failure_domain",
    ):
        if policy.get(key) != descriptor.get(key):
            raise SiteUnavailable(f"site descriptor {key} disagrees with policy")
    if facts.kernel_hostname != policy.get("kernel_hostname"):
        raise SiteUnavailable("kernel hostname disagrees with policy")
    if not set(facts.stable_addresses).intersection(facts.local_addresses):
        raise SiteUnavailable("stable host label has no local address")
    primary_root = Path(str(descriptor["primary_store_root"])).resolve()
    replica_root = Path(str(descriptor["replica_store_root"])).resolve()
    if facts.primary_mount.root != primary_root or facts.replica_mount.root != replica_root:
        raise SiteUnavailable("mount roots disagree with the site descriptor")
    if primary_root == replica_root:
        raise SiteUnavailable("primary and replica roots are not distinct")
    if facts.primary_mount.source == facts.replica_mount.source:
        raise SiteUnavailable("primary and replica mount sources are not distinct")
    if facts.primary_mount.filesystem_id == facts.replica_mount.filesystem_id:
        raise SiteUnavailable("primary and replica filesystem identities are not distinct")
    replica = facts.replica_mount
    if replica.source.startswith("/dev/loop"):
        raise SiteUnavailable("loop replica source is not an external failure domain")
    if replica.source.startswith("/dev/"):
        raise SiteUnavailable("local block replica source is not an external failure domain")
    if replica.bind_mount:
        raise SiteUnavailable("bind replica mount is not an external failure domain")
    allowlist = policy.get("remote_filesystem_allowlist")
    if not isinstance(allowlist, list) or replica.filesystem_type not in allowlist:
        raise SiteUnavailable("replica filesystem type is not remotely approved")
    if replica.filesystem_type != policy.get("replica_mount_class"):
        raise SiteUnavailable("replica filesystem type disagrees with policy mount class")
    if replica.remote_authority != policy.get("replica_authority"):
        raise SiteUnavailable("replica remote authority disagrees with policy")
    if not replica.authority_addresses or set(replica.authority_addresses).intersection(
        facts.local_addresses
    ):
        raise SiteUnavailable("replica remote authority resolves locally")
    if not facts.clock_synchronized:
        raise SiteUnavailable("local clock is not synchronized")
    if not facts.lock_available:
        raise SiteUnavailable("campaign lock is unavailable")
    replica_probe = probe.verify_writable(replica_root)
    return _site_projection(descriptor, facts, replica_probe)


def preflight_site(
    policy: dict[str, object],
    graph: dict[str, object],
    descriptor: dict[str, object],
    facts: SiteFacts,
    probe: Probe,
    *,
    evidence_git_rev: str,
    evidence_tree_clean: bool,
    independent_build_capability: str,
) -> dict[str, object]:
    if graph.get("policy_hash") != content_ref(graph, excluding="policy_hash"):
        raise SiteUnavailable("production graph hash does not authenticate")
    if not evidence_tree_clean:
        raise SiteUnavailable("evidence tree is not clean")
    if independent_build_capability != "passed":
        raise SiteUnavailable("independent build capability did not pass")
    site_facts = authenticate_site(policy, descriptor, facts, probe)
    value: dict[str, object] = {
        "schema_id": "https://w3id.org/abc/schemas/parser-rq-site-preflight.schema.json",
        "schema_version": "1.0.0",
        "evidence_git_rev": evidence_git_rev,
        "evidence_tree_clean": True,
        "site_descriptor_hash": content_ref(descriptor),
        "site_policy_hash": str(policy["policy_hash"]),
        "production_graph_hash": str(graph["policy_hash"]),
        "independent_build_capability": "passed",
        "site_facts": site_facts,
    }
    value["report_ref"] = content_ref(value, excluding="report_ref")
    return value


def seal_readiness(
    report: dict[str, object],
    policy: dict[str, object],
    graph: dict[str, object],
    descriptor: dict[str, object],
    facts: SiteFacts,
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
    if graph.get("policy_hash") != content_ref(graph, excluding="policy_hash"):
        raise SiteUnavailable("production graph hash does not authenticate")
    if not candidate_tree_clean or not evidence_tree_clean:
        raise SiteUnavailable("candidate and evidence trees must be clean")
    if report.get("report_ref") != content_ref(report, excluding="report_ref"):
        raise SiteUnavailable("site preflight report does not authenticate")
    if report.get("site_policy_hash") != policy.get("policy_hash") or report.get(
        "production_graph_hash"
    ) != graph.get("policy_hash"):
        raise SiteUnavailable("site preflight policy identity has drifted")
    site_facts = authenticate_site(policy, descriptor, facts, probe)
    value: dict[str, object] = {
        "schema_id": "https://w3id.org/abc/schemas/parser-rq-readiness-receipt.schema.json",
        "schema_version": "1.0.0",
        "site_preflight_report_ref": report["report_ref"],
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
        "site_facts": site_facts,
    }
    value["readiness_receipt_ref"] = content_ref(value, excluding="readiness_receipt_ref")
    return value


def recheck_readiness(
    receipt: dict[str, object],
    policy: dict[str, object],
    descriptor: dict[str, object],
    facts: SiteFacts,
    probe: Probe,
) -> None:
    if receipt.get("readiness_receipt_ref") != content_ref(
        receipt, excluding="readiness_receipt_ref"
    ):
        raise SiteUnavailable("readiness receipt does not authenticate")
    current = authenticate_site(policy, descriptor, facts, probe)
    if current != receipt.get("site_facts"):
        raise SiteUnavailable("current facts disagree with sealed site facts")


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
                raise SiteUnavailable("replica probe bytes differ")
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


def _mount_fact(root: Path) -> MountFact:
    resolved = root.resolve(strict=True)
    best: tuple[int, list[str], list[str]] | None = None
    for line in Path("/proc/self/mountinfo").read_text().splitlines():
        before, separator, after = line.partition(" - ")
        if not separator:
            continue
        fields = before.split()
        trailing = after.split()
        if len(fields) < 6 or len(trailing) < 2:
            continue
        mount_point = Path(fields[4].replace("\\040", " "))
        try:
            resolved.relative_to(mount_point)
        except ValueError:
            continue
        score = len(mount_point.parts)
        if best is None or score > best[0]:
            best = (score, fields, trailing)
    if best is None:
        raise SiteUnavailable(f"no mount contains {resolved}")
    _, fields, trailing = best
    filesystem_type, source = trailing[:2]
    authority = _remote_authority(source, filesystem_type)
    addresses = () if authority is None else _resolved_addresses(authority)
    options = set(fields[5].split(",")) | set(fields[6:])
    return MountFact(
        root=resolved,
        source=source,
        filesystem_type=filesystem_type,
        filesystem_id=str(os.stat(resolved).st_dev),
        remote_authority=authority,
        authority_addresses=addresses,
        bind_mount="bind" in options,
    )


def _remote_authority(source: str, filesystem_type: str) -> str | None:
    if filesystem_type in {"nfs", "nfs4"} and ":" in source:
        return source.rsplit(":", 1)[0].strip("[]")
    if filesystem_type == "cifs" and source.startswith("//"):
        return source[2:].split("/", 1)[0]
    if filesystem_type in {"ceph", "fuse.sshfs"} and ":" in source:
        return source.split(":", 1)[0].split("@")[-1].strip("[]")
    return None


def _resolved_addresses(host: str) -> tuple[str, ...]:
    try:
        return tuple(sorted({row[4][0] for row in socket.getaddrinfo(host, None)}))
    except socket.gaierror as error:
        raise SiteUnavailable(f"cannot resolve {host}") from error


def _local_addresses() -> tuple[str, ...]:
    completed = subprocess.run(["ip", "-j", "address"], check=False, capture_output=True, text=True)
    if completed.returncode != 0:
        raise SiteUnavailable("local interface addresses cannot be read")
    try:
        interfaces = json.loads(completed.stdout)
        return tuple(
            sorted(
                {
                    address["local"]
                    for interface in interfaces
                    for address in interface.get("addr_info", [])
                    if isinstance(address.get("local"), str)
                }
            )
        )
    except (json.JSONDecodeError, TypeError, KeyError) as error:
        raise SiteUnavailable("local interface address output is malformed") from error


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


def collect_site_facts(descriptor: dict[str, object]) -> SiteFacts:
    stable_label = str(descriptor["stable_host_label"])
    return SiteFacts(
        kernel_hostname=socket.gethostname().split(".", 1)[0],
        stable_addresses=_resolved_addresses(stable_label),
        local_addresses=_local_addresses(),
        primary_mount=_mount_fact(Path(str(descriptor["primary_store_root"]))),
        replica_mount=_mount_fact(Path(str(descriptor["replica_store_root"]))),
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
            package_ref=(
                f"{repository_root / 'abc'}#packages.x86_64-linux.parser-rq-build-capability-probe"
            ),
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
    common.add_argument("--policy", type=Path, required=True)
    common.add_argument("--site-descriptor", type=Path, required=True)
    preflight = commands.add_parser("preflight-site", parents=[common])
    preflight.add_argument("--graph", type=Path, required=True)
    preflight.add_argument("--evidence-git-rev", required=True)
    preflight.add_argument("--evidence-tree-clean", choices=("true", "false"), required=True)
    preflight.add_argument("--out", type=Path, required=True)
    seal = commands.add_parser("seal-readiness", parents=[common])
    seal.add_argument("--preflight", type=Path, required=True)
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
        policy = _read_json(args.policy)
        descriptor = _read_json(args.site_descriptor)
        facts = collect_site_facts(descriptor)
        probe = FileProbe()
        if args.command == "preflight-site":
            repository_root = Path(__file__).resolve().parents[2]
            value = preflight_site(
                policy,
                _read_json(args.graph),
                descriptor,
                facts,
                probe,
                evidence_git_rev=args.evidence_git_rev,
                evidence_tree_clean=args.evidence_tree_clean == "true",
                independent_build_capability=independent_build_capability(
                    repository_root, Path(str(descriptor["scratch_root"]))
                ),
            )
            _atomic_json(args.out, value)
        elif args.command == "seal-readiness":
            value = seal_readiness(
                _read_json(args.preflight),
                policy,
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
            recheck_readiness(_read_json(args.receipt), policy, descriptor, facts, probe)
        else:
            raise SiteUnavailable("unknown site command")
    except (OSError, KeyError, ValueError, json.JSONDecodeError) as error:
        print(str(error), file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
