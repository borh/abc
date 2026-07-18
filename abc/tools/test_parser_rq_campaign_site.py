from __future__ import annotations

import importlib.util
import fcntl
import os
import sys
from dataclasses import replace
from pathlib import Path
from typing import Any

import pytest


MODULE_PATH = Path(__file__).with_name("parser_rq_campaign_site.py")
SPEC = importlib.util.spec_from_file_location("parser_rq_campaign_site", MODULE_PATH)
assert SPEC and SPEC.loader
site = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = site
SPEC.loader.exec_module(site)


HASH = "sha256:" + "a" * 64
GIT = "a" * 40


class FakeProbe:
    def __init__(self, *, read_matches: bool = True) -> None:
        self.calls: list[tuple[str, Path]] = []
        self.read_matches = read_matches

    def verify_writable(self, root: Path) -> dict[str, bool]:
        for operation in (
            "create",
            "fsync-file",
            "fsync-directory",
            "stream-read",
            "remove",
            "fsync-directory",
        ):
            self.calls.append((operation, root))
        if not self.read_matches:
            raise site.SiteUnavailable("replica probe bytes differ")
        return {
            "create": True,
            "fsync_file": True,
            "fsync_directory": True,
            "stream_read": True,
            "remove": True,
        }


def configured_policy() -> dict[str, Any]:
    policy = {
        "schema_id": "https://w3id.org/abc/schemas/parser-rq-site-policy.schema.json",
        "schema_version": "1.0.0",
        "stable_host_label": "hinoki.hyakutake-barbel.ts.net",
        "kernel_hostname": "hinoki",
        "production_graph_version": "abc/parser-rq-production-graph/v1",
        "primary_failure_domain": "hinoki-primary",
        "replica_status": "configured",
        "replica_failure_domain": "remote-archive",
        "replica_mount_class": "nfs4",
        "replica_authority": "archive.example",
        "remote_filesystem_allowlist": ["nfs", "nfs4", "cifs"],
    }
    policy["policy_hash"] = site.content_ref(policy)
    return policy


def graph_policy() -> dict[str, Any]:
    graph = {
        "schema_version": "abc/parser-rq-production-graph/v1",
        "installed_members": [],
        "members": [],
        "executables": [],
    }
    graph["policy_hash"] = site.content_ref(graph)
    return graph


def test_content_ref_matches_committed_jcs_policy_hash() -> None:
    graph = site._read_json(MODULE_PATH.parents[1] / "data/parser-rq-production-graph-v1.json")
    assert site.content_ref(graph, excluding="policy_hash") == graph["policy_hash"]


def descriptor(tmp_path: Path) -> dict[str, Any]:
    return {
        "schema_id": "https://w3id.org/abc/schemas/parser-rq-site-descriptor.schema.json",
        "schema_version": "1.0.0",
        "stable_host_label": "hinoki.hyakutake-barbel.ts.net",
        "kernel_hostname": "hinoki",
        "corpus_root": str(tmp_path / "corpus"),
        "primary_store_root": str(tmp_path / "primary"),
        "replica_store_root": str(tmp_path / "replica"),
        "campaign_lock_path": str(tmp_path / "campaign.lock"),
        "scratch_root": str(tmp_path / "scratch"),
        "primary_failure_domain": "hinoki-primary",
        "replica_failure_domain": "remote-archive",
    }


def facts(tmp_path: Path) -> Any:
    return site.SiteFacts(
        kernel_hostname="hinoki",
        stable_addresses=("100.64.0.1",),
        local_addresses=("127.0.0.1", "100.64.0.1"),
        primary_mount=site.MountFact(
            root=(tmp_path / "primary").resolve(),
            source="/dev/mapper/main",
            filesystem_type="ext4",
            filesystem_id="1:2",
        ),
        replica_mount=site.MountFact(
            root=(tmp_path / "replica").resolve(),
            source="archive.example:/parser-rq",
            filesystem_type="nfs4",
            filesystem_id="3:4",
            remote_authority="archive.example",
            authority_addresses=("192.0.2.10",),
        ),
        clock_synchronized=True,
        lock_available=True,
    )


def test_authenticate_site_accepts_short_hostname_and_local_stable_address(
    tmp_path: Path,
) -> None:
    probe = FakeProbe()
    value = site.authenticate_site(
        configured_policy(), descriptor(tmp_path), facts(tmp_path), probe
    )
    assert value["kernel_hostname"] == "hinoki"
    assert value["stable_addresses"] == ["100.64.0.1"]
    assert value["replica_probe"]["stream_read"] is True
    assert [operation for operation, _ in probe.calls] == [
        "create",
        "fsync-file",
        "fsync-directory",
        "stream-read",
        "remove",
        "fsync-directory",
    ]


@pytest.mark.parametrize(
    ("mutation", "message"),
    [
        (lambda value: replace(value, kernel_hostname="other"), "kernel hostname"),
        (lambda value: replace(value, stable_addresses=("192.0.2.1",)), "stable host"),
        (
            lambda value: replace(
                value, replica_mount=replace(value.replica_mount, root=value.primary_mount.root)
            ),
            "roots",
        ),
        (
            lambda value: replace(
                value,
                replica_mount=replace(value.replica_mount, source=value.primary_mount.source),
            ),
            "mount sources",
        ),
        (
            lambda value: replace(
                value,
                replica_mount=replace(
                    value.replica_mount, filesystem_id=value.primary_mount.filesystem_id
                ),
            ),
            "filesystem identities",
        ),
        (
            lambda value: replace(
                value, replica_mount=replace(value.replica_mount, source="/dev/sdb1")
            ),
            "local block",
        ),
        (
            lambda value: replace(
                value, replica_mount=replace(value.replica_mount, source="/dev/loop0")
            ),
            "loop",
        ),
        (
            lambda value: replace(
                value, replica_mount=replace(value.replica_mount, bind_mount=True)
            ),
            "bind",
        ),
        (
            lambda value: replace(
                value, replica_mount=replace(value.replica_mount, filesystem_type="ext4")
            ),
            "filesystem type",
        ),
        (
            lambda value: replace(
                value,
                replica_mount=replace(value.replica_mount, remote_authority="other.example"),
            ),
            "remote authority",
        ),
        (
            lambda value: replace(
                value,
                replica_mount=replace(value.replica_mount, authority_addresses=("100.64.0.1",)),
            ),
            "resolves locally",
        ),
        (lambda value: replace(value, clock_synchronized=False), "clock"),
    ],
)
def test_authenticate_site_rejects_invalid_host_or_failure_domain(
    tmp_path: Path, mutation: Any, message: str
) -> None:
    probe = FakeProbe()
    with pytest.raises(site.SiteUnavailable, match=message):
        site.authenticate_site(
            configured_policy(), descriptor(tmp_path), mutation(facts(tmp_path)), probe
        )


def test_unconfigured_replica_fails_before_probe(tmp_path: Path) -> None:
    policy = configured_policy()
    policy["replica_status"] = "unconfigured"
    for key in ("replica_failure_domain", "replica_mount_class", "replica_authority"):
        policy.pop(key)
    policy["policy_hash"] = site.content_ref(policy, excluding="policy_hash")
    probe = FakeProbe()
    with pytest.raises(site.SiteUnavailable, match="external replica is unconfigured"):
        site.authenticate_site(policy, descriptor(tmp_path), facts(tmp_path), probe)
    assert probe.calls == []


def test_policy_hash_drift_fails_before_probe(tmp_path: Path) -> None:
    policy = configured_policy()
    policy["replica_authority"] = "substituted.example"
    probe = FakeProbe()
    with pytest.raises(site.SiteUnavailable, match="policy hash"):
        site.authenticate_site(policy, descriptor(tmp_path), facts(tmp_path), probe)
    assert probe.calls == []


def test_preflight_and_seal_contain_no_authority_or_observations(tmp_path: Path) -> None:
    policy = configured_policy()
    graph = graph_policy()
    report = site.preflight_site(
        policy,
        graph,
        descriptor(tmp_path),
        facts(tmp_path),
        FakeProbe(),
        evidence_git_rev=GIT,
        evidence_tree_clean=True,
        independent_build_capability="passed",
    )
    receipt = site.seal_readiness(
        report,
        policy,
        graph,
        descriptor(tmp_path),
        facts(tmp_path),
        FakeProbe(),
        candidate_ref=HASH,
        qualification_identity_ref=HASH,
        provenance_core_ref=HASH,
        candidate_git_rev=GIT,
        candidate_tree_clean=True,
        evidence_base_git_rev=GIT,
        evidence_tree_clean=True,
        corpus_snapshot_hash=HASH,
        corpus_list_hash=HASH,
    )
    for value in (report, receipt):
        assert not ({"not_before", "not_after", "observations", "measurements"} & value.keys())
    assert receipt["site_preflight_report_ref"] == report["report_ref"]
    assert receipt["readiness_receipt_ref"] == site.content_ref(
        receipt, excluding="readiness_receipt_ref"
    )


def test_recheck_repeats_full_probe_and_rejects_mount_or_bytes_change(
    tmp_path: Path,
) -> None:
    policy = configured_policy()
    graph = graph_policy()
    initial_facts = facts(tmp_path)
    report = site.preflight_site(
        policy,
        graph,
        descriptor(tmp_path),
        initial_facts,
        FakeProbe(),
        evidence_git_rev=GIT,
        evidence_tree_clean=True,
        independent_build_capability="passed",
    )
    receipt = site.seal_readiness(
        report,
        policy,
        graph,
        descriptor(tmp_path),
        initial_facts,
        FakeProbe(),
        candidate_ref=HASH,
        qualification_identity_ref=HASH,
        provenance_core_ref=HASH,
        candidate_git_rev=GIT,
        candidate_tree_clean=True,
        evidence_base_git_rev=GIT,
        evidence_tree_clean=True,
        corpus_snapshot_hash=HASH,
        corpus_list_hash=HASH,
    )
    probe = FakeProbe()
    site.recheck_readiness(receipt, policy, descriptor(tmp_path), initial_facts, probe)
    assert len(probe.calls) == 6

    changed_mount = replace(
        initial_facts,
        replica_mount=replace(initial_facts.replica_mount, filesystem_id="9:9"),
    )
    with pytest.raises(site.SiteUnavailable, match="sealed site facts"):
        site.recheck_readiness(receipt, policy, descriptor(tmp_path), changed_mount, FakeProbe())
    with pytest.raises(site.SiteUnavailable, match="probe bytes differ"):
        site.recheck_readiness(
            receipt,
            policy,
            descriptor(tmp_path),
            initial_facts,
            FakeProbe(read_matches=False),
        )


def test_real_file_probe_removes_disposable_value(tmp_path: Path) -> None:
    assert site.FileProbe().verify_writable(tmp_path) == {
        "create": True,
        "fsync_file": True,
        "fsync_directory": True,
        "stream_read": True,
        "remove": True,
    }
    assert list(tmp_path.iterdir()) == []


def test_lock_probe_is_nonblocking_and_releases_capability(tmp_path: Path) -> None:
    lock = tmp_path / "campaign.lock"
    assert site._lock_available(lock) is True
    descriptor_number = os.open(lock, os.O_RDWR)
    try:
        fcntl.flock(descriptor_number, fcntl.LOCK_EX | fcntl.LOCK_NB)
        assert site._lock_available(lock) is False
    finally:
        fcntl.flock(descriptor_number, fcntl.LOCK_UN)
        os.close(descriptor_number)
    assert site._lock_available(lock) is True


def test_preflight_cli_has_no_caller_asserted_build_capability() -> None:
    parser = site._parser()
    subcommands = next(action for action in parser._actions if action.dest == "command")
    preflight = subcommands.choices["preflight-site"]
    assert "independent_build_capability" not in {action.dest for action in preflight._actions}


def test_every_site_cli_subcommand_has_help() -> None:
    for command in ("preflight-site", "seal-readiness", "recheck-readiness"):
        assert site.main([command, "--help"]) == 0
