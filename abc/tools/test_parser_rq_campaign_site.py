from __future__ import annotations

import fcntl
import importlib.util
import os
import sys
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
    def __init__(self, *, fail: bool = False) -> None:
        self.calls: list[Path] = []
        self.fail = fail

    def verify_writable(self, root: Path) -> dict[str, bool]:
        self.calls.append(root)
        if self.fail:
            raise site.SiteUnavailable("probe bytes differ")
        return {
            "create": True,
            "fsync_file": True,
            "fsync_directory": True,
            "stream_read": True,
            "remove": True,
        }


def graph_policy() -> dict[str, Any]:
    graph = {
        "schema_version": "abc/parser-rq-production-graph/v1",
        "installed_members": [],
        "members": [],
        "executables": [],
    }
    graph["policy_hash"] = site.content_ref(graph)
    return graph


def descriptor(tmp_path: Path) -> dict[str, object]:
    return {
        "schema_id": "https://w3id.org/abc/schemas/parser-rq-site-descriptor.schema.json",
        "schema_version": "3.0.0",
        "evidence_store_root": str(tmp_path / "evidence"),
        "scratch_root": str(tmp_path / "scratch"),
        "campaign_lock_path": str(tmp_path / "campaign.lock"),
    }


def prepare_roots(value: dict[str, object]) -> None:
    for key in ("evidence_store_root", "scratch_root"):
        Path(str(value[key])).mkdir(parents=True)


def runtime_facts(*, clock_synchronized: bool = True, lock_available: bool = True) -> Any:
    return site.RuntimeFacts(
        clock_synchronized=clock_synchronized,
        lock_available=lock_available,
    )


def readiness(tmp_path: Path) -> tuple[dict[str, object], dict[str, object]]:
    value = descriptor(tmp_path)
    prepare_roots(value)
    graph = graph_policy()
    receipt = site.seal_readiness(
        graph,
        value,
        runtime_facts(),
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
    return value, receipt


def test_content_ref_matches_committed_graph_hash() -> None:
    graph = site._read_json(MODULE_PATH.parents[1] / "data/parser-rq-production-graph-v1.json")
    assert site.content_ref(graph, excluding="policy_hash") == graph["policy_hash"]


def test_runtime_authentication_is_place_portable(tmp_path: Path) -> None:
    for name in ("first", "second"):
        value = descriptor(tmp_path / name)
        prepare_roots(value)
        probe = FakeProbe()
        site.authenticate_runtime(value, runtime_facts(), probe)
        assert probe.calls == [
            Path(str(value["evidence_store_root"])).resolve(),
            Path(str(value["scratch_root"])).resolve(),
        ]


def test_runtime_authentication_declares_no_corpus_place(tmp_path: Path) -> None:
    """The corpus a capture reads is a governed value resolved below the
    authenticated candidate tree, not a runtime place the operator selects. A
    descriptor that still names one violates the closed contract rather than
    being tolerated and ignored, so no operator can believe it selects bytes."""
    value = descriptor(tmp_path)
    prepare_roots(value)
    corpus = tmp_path / "corpus"
    corpus.mkdir()
    with pytest.raises(site.SiteUnavailable, match="closed contract"):
        site.authenticate_runtime(
            {**value, "corpus_root": str(corpus)}, runtime_facts(), FakeProbe()
        )


def test_runtime_authentication_rejects_the_superseded_descriptor_version(
    tmp_path: Path,
) -> None:
    value = descriptor(tmp_path)
    prepare_roots(value)
    with pytest.raises(site.SiteUnavailable, match="unsupported"):
        site.authenticate_runtime(
            {**value, "schema_version": "2.0.0"}, runtime_facts(), FakeProbe()
        )


@pytest.mark.parametrize(
    ("facts", "message"),
    [
        (False, "clock"),
        (True, "lock"),
    ],
)
def test_runtime_authentication_rejects_failed_volatile_fact(
    tmp_path: Path, facts: bool, message: str
) -> None:
    value = descriptor(tmp_path)
    prepare_roots(value)
    runtime = (
        runtime_facts(clock_synchronized=facts)
        if message == "clock"
        else runtime_facts(lock_available=not facts)
    )
    with pytest.raises(site.SiteUnavailable, match=message):
        site.authenticate_runtime(value, runtime, FakeProbe())


def test_runtime_authentication_rejects_probe_failure(tmp_path: Path) -> None:
    value = descriptor(tmp_path)
    prepare_roots(value)
    with pytest.raises(site.SiteUnavailable, match="probe bytes differ"):
        site.authenticate_runtime(value, runtime_facts(), FakeProbe(fail=True))


def test_preflight_returns_no_persisted_report(tmp_path: Path) -> None:
    value = descriptor(tmp_path)
    prepare_roots(value)
    assert (
        site.preflight_runtime(
            graph_policy(),
            value,
            runtime_facts(),
            FakeProbe(),
            evidence_tree_clean=True,
            independent_build_capability="passed",
        )
        is None
    )


def test_readiness_contains_no_runtime_place_or_preflight_value(tmp_path: Path) -> None:
    _, receipt = readiness(tmp_path)
    forbidden = {
        "site_preflight_report_ref",
        "site_descriptor_hash",
        "site_facts",
        "kernel" + "_hostname",
        "corpus_root",
        "evidence_store_root",
        "scratch_root",
        "campaign_lock_path",
    }
    assert not (forbidden & receipt.keys())
    assert receipt["schema_version"] == "2.0.0"
    assert receipt["readiness_receipt_ref"] == site.content_ref(
        receipt, excluding="readiness_receipt_ref"
    )


def test_recheck_authenticates_receipt_and_repeats_writable_probes(
    tmp_path: Path,
) -> None:
    value, receipt = readiness(tmp_path)
    probe = FakeProbe()
    site.recheck_readiness(receipt, value, runtime_facts(), probe)
    assert len(probe.calls) == 2
    with pytest.raises(site.SiteUnavailable, match="does not authenticate"):
        site.recheck_readiness(
            {**receipt, "candidate_ref": "sha256:" + "b" * 64},
            value,
            runtime_facts(),
            FakeProbe(),
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


def test_cli_has_no_policy_or_persisted_preflight_option() -> None:
    parser = site._parser()
    subcommands = next(action for action in parser._actions if action.dest == "command")
    for command in ("preflight-site", "seal-readiness", "recheck-readiness"):
        options = {
            option
            for action in subcommands.choices[command]._actions
            for option in action.option_strings
        }
        assert "--policy" not in options
    assert "--out" not in {
        option
        for action in subcommands.choices["preflight-site"]._actions
        for option in action.option_strings
    }


def test_every_site_cli_subcommand_has_help() -> None:
    for command in ("preflight-site", "seal-readiness", "recheck-readiness"):
        assert site.main([command, "--help"]) == 0


# --- Kernel clock discipline ------------------------------------------------
#
# `_clock_synchronized` had no test at all: every case above injects
# `RuntimeFacts` directly, so nothing exercised how the fact is obtained. It
# was obtained by shelling out to `timedatectl`, which needs a reachable
# systemd D-Bus and therefore fails on a correctly synchronized host whenever
# the bus is not attached -- containers, sandboxes, CI. That failure is
# indistinguishable from a genuinely unsynchronized clock.


def clock_reading(*, code: int = 0, status: int = 0x2001, maxerror: int = 368_000) -> Any:
    return site.ClockReading(code=code, status=status, maxerror=maxerror)


def test_a_disciplined_kernel_clock_is_synchronized() -> None:
    assert site.clock_synchronized_from(clock_reading()) is True


def test_an_undisciplined_clock_is_not_synchronized() -> None:
    # STA_UNSYNC is set at boot and only an NTP client that disciplines the
    # clock clears it, so a host with no time source fails closed.
    assert site.clock_synchronized_from(clock_reading(status=0x2041)) is False


def test_a_clock_whose_error_bound_has_grown_is_not_synchronized() -> None:
    # The kernel widens maxerror once nothing is correcting the clock, so a
    # daemon that synced and then died expires on its own.
    assert site.clock_synchronized_from(clock_reading(maxerror=16_000_000)) is False
    assert site.clock_synchronized_from(clock_reading(maxerror=15_999_999)) is True


def test_a_clock_in_error_state_is_not_synchronized() -> None:
    assert site.clock_synchronized_from(clock_reading(code=5)) is False


def test_an_unavailable_reading_is_not_synchronized() -> None:
    # No reading is not a passing reading.
    assert site.clock_synchronized_from(None) is False


def test_the_kernel_reading_is_obtainable_without_a_message_bus() -> None:
    """The regression this replaces: no subprocess, no bus, no systemd.

    Asserted as a shape, not a value -- whether *this* host is synchronized is
    not this test's business.
    """
    reading = site.read_kernel_clock()
    assert reading is not None
    assert isinstance(reading.code, int)
    assert isinstance(reading.status, int)
    assert isinstance(reading.maxerror, int)
    # A plausible `struct timex` prefix: adjtimex reports one of the documented
    # clock states, and the status word only uses its low bits. A wrong struct
    # layout would show up here as garbage rather than as a silent wrong answer.
    assert 0 <= reading.code <= 5
    assert 0 <= reading.status <= 0xFFFF
