from __future__ import annotations

import importlib.util
import hashlib
import json
import sys
import os
import subprocess
from dataclasses import replace
from datetime import UTC, datetime
from pathlib import Path
from typing import Any

import pytest


MODULE_PATH = Path(__file__).with_name("parser_rq_campaign_orchestrator.py")
SPEC = importlib.util.spec_from_file_location("parser_rq_campaign_orchestrator", MODULE_PATH)
assert SPEC and SPEC.loader
orchestrator = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = orchestrator
SPEC.loader.exec_module(orchestrator)


OPERATIONS = (
    "capture-core",
    "capture-predicate-pair",
    "capture-source",
    "derive-diagnostic-gap",
    "capture-publication",
    "capture-resource",
)
INSTALLED = (
    "core_attempt",
    "source_recognition",
    "diagnostic_gap",
    "diagnostic_completeness",
    "parser_ir_conformance",
    "publication_structure",
    "resource",
)


def write_json(path: Path, value: object) -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value))
    return path


def fixture(tmp_path: Path) -> Any:
    candidate_tree = tmp_path / "candidate-tree"
    evidence_tree = tmp_path / "evidence-tree"
    (candidate_tree / "abc/data").mkdir(parents=True)
    (candidate_tree / "abc/tools").mkdir(parents=True)
    graph: dict[str, Any] = {
        "schema_version": "abc/parser-rq-production-graph/v1",
        "members": [
            {"name": "core_attempt", "operation": "capture-core"},
            {"name": "predicate_hardening", "operation": "capture-predicate-pair"},
            {"name": "source_recognition", "operation": "capture-source"},
            {"name": "diagnostic_gap", "operation": "derive-diagnostic-gap"},
            {"name": "publication_structure", "operation": "capture-publication"},
            {"name": "resource", "operation": "capture-resource"},
        ],
        "installed_members": list(INSTALLED),
        "executables": [
            {
                "name": name,
                "adapter": name,
                "adapter_version": "1.0.0",
                "argv_template": ["{executable}"],
            }
            for name in (
                "ab-check",
                "ab-aozora",
                "ab-aat-to-parser-ir",
                "ab-parser-rq-source-accountability",
                "ab-parser-rq-diagnostic-authorization",
            )
        ],
    }
    graph["policy_hash"] = orchestrator.content_ref(graph, excluding="policy_hash")
    write_json(candidate_tree / "abc/data/parser-rq-production-graph-v1.json", graph)
    write_json(
        candidate_tree / "abc/data/parser-rq-ab-aozora-diagnostic-gap-v1.json",
        {"fixture": True},
    )
    for name in (
        "parser-rq-core-attempt-policy-v1.json",
        "parser-rq-diagnostic-completeness-policy-v1.json",
        "parser-rq-parser-ir-conformance-policy-v1.json",
        "parser-rq-resource-policy-v1.json",
    ):
        write_json(candidate_tree / "abc/data" / name, {"fixture": True})
    write_json(
        candidate_tree / "abc/data/parser-rq-publication-policy-v1.json",
        {
            "policy_hash": "sha256:" + "4" * 64,
            "preservation_schema": {"hash": "sha256:" + "5" * 64},
            "validator": {"semantics_hash": "sha256:" + "6" * 64},
        },
    )
    write_json(
        candidate_tree / "abc/data/parser-rq-publication-fixtures-v1.json",
        {"census_hash": "sha256:" + "7" * 64, "works": {}},
    )
    write_json(
        candidate_tree / "abc/data/parser-rq-resource-policy-v1.json",
        {
            "policy_hash": "sha256:" + "8" * 64,
            "production_command_hash": orchestrator.RESOURCE_COMMAND_HASH,
            "work_ids": [],
        },
    )
    write_json(
        candidate_tree / "ab-validator/data/parser-rq-resource-identity-v1.json",
        {"fixture": True},
    )
    for relative in orchestrator.REPOSITORY_DRIVERS:
        path = candidate_tree / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("driver")
    executables = []
    for row in graph["executables"]:
        output = tmp_path / "nix-store" / row["name"]
        executable = output / "bin" / row["name"]
        executable.parent.mkdir(parents=True)
        executable.write_text("binary")
        if row["name"] == "ab-check":
            (executable.parent / "time").write_text("time")
        executables.append(
            {
                **row,
                "nix_output": str(output),
                "nar_hash": "sha256:" + "a" * 64,
                "sha256": "sha256:" + hashlib.sha256(b"binary").hexdigest(),
                "bytes": 6,
                "parser_git_rev": "a" * 40,
            }
        )
    provenance = {
        "status": "reproducible",
        "provenance_core_ref": "sha256:" + "c" * 64,
        "executables": executables,
    }
    receipt = {
        "readiness_receipt_ref": "sha256:" + "d" * 64,
        "qualification_identity_ref": "sha256:" + "e" * 64,
        "production_graph_hash": graph["policy_hash"],
        "provenance_core_ref": provenance["provenance_core_ref"],
    }
    evidence_store = tmp_path / "evidence-store"
    evidence_store.mkdir()
    scratch = tmp_path / "scratch"
    scratch.mkdir()
    corpus = tmp_path / "corpus"
    corpus.mkdir()
    site_descriptor = {
        "schema_id": "https://w3id.org/abc/schemas/parser-rq-site-descriptor.schema.json",
        "schema_version": "2.0.0",
        "campaign_lock_path": str(tmp_path / "campaign.lock"),
        "evidence_store_root": str(evidence_store),
        "scratch_root": str(scratch),
        "corpus_root": str(corpus),
    }
    evidence_tree.mkdir()
    candidate = evidence_tree / "candidate.edn"
    authorization = evidence_tree / "authorization.edn"
    candidate.write_text("{}")
    authorization.write_text("{}")
    return orchestrator.CampaignConfig(
        candidate=candidate,
        authorization=authorization,
        provenance=write_json(evidence_tree / "provenance.json", provenance),
        readiness_receipt=write_json(evidence_tree / "receipt.json", receipt),
        site_descriptor=write_json(evidence_tree / "site-descriptor.json", site_descriptor),
        candidate_tree=candidate_tree,
        evidence_tree=evidence_tree,
        staging_root=tmp_path / "staging",
        production=True,
    )


def test_authentication_closes_graph_provenance_and_detached_paths(tmp_path: Path) -> None:
    campaign = orchestrator.authenticate_inputs(fixture(tmp_path))
    assert campaign.operations == OPERATIONS
    assert tuple(campaign.graph["installed_members"]) == INSTALLED
    assert set(campaign.executables) == {
        "ab-check",
        "ab-aozora",
        "ab-aat-to-parser-ir",
        "ab-parser-rq-source-accountability",
        "ab-parser-rq-diagnostic-authorization",
    }
    assert campaign.executables["ab-check"].name == "ab-check"
    assert campaign.clojure_prefix == (
        "nix",
        "develop",
        "--no-write-lock-file",
        str(campaign.config.candidate_tree / "abc"),
        "--command",
        "bash",
        "-c",
        'cd "$0" && exec clojure "$@"',
        str(campaign.config.candidate_tree / "abc"),
    )
    assert all(
        path.is_relative_to(campaign.config.candidate_tree) for path in campaign.drivers.values()
    )


@pytest.mark.parametrize(
    "mutation",
    (
        lambda config: write_json(
            config.candidate_tree / "abc/data/parser-rq-production-graph-v1.json",
            {"policy_hash": "sha256:" + "0" * 64},
        ),
        lambda config: write_json(
            config.readiness_receipt,
            {
                "production_graph_hash": "sha256:" + "0" * 64,
                "provenance_core_ref": "sha256:" + "c" * 64,
            },
        ),
        lambda config: write_json(config.provenance, {"status": "unavailable"}),
    ),
)
def test_authentication_fails_closed_on_identity_or_membership_drift(
    tmp_path: Path, mutation
) -> None:
    config = fixture(tmp_path)
    mutation(config)
    with pytest.raises(orchestrator.PreparationFailed):
        orchestrator.authenticate_inputs(config)


def test_public_cli_is_closed_and_legacy_lane_environment_is_rejected(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    parser = orchestrator._parser()
    destinations = {action.dest for action in parser._actions}
    assert destinations == {
        "help",
        "candidate",
        "authorization",
        "provenance",
        "readiness_receipt",
        "site_descriptor",
        "candidate_tree",
        "evidence_tree",
        "staging_root",
        "production",
    }
    assert "lane_command" not in destinations
    config = fixture(tmp_path)
    monkeypatch.setenv("PARSER_RQ_CORE_CAPTURE", "ignored")
    with pytest.raises(orchestrator.PreparationFailed, match="legacy lane environment"):
        orchestrator.authenticate_inputs(config)


def test_production_parser_has_no_removed_place_or_copy_option() -> None:
    parser = orchestrator._parser()
    option_strings = {option for action in parser._actions for option in action.option_strings}
    assert "--site-" + "policy" not in option_strings
    assert not any("rep" + "lica" in option for option in option_strings)


def test_prepare_commands_recheck_only_runtime_descriptor(tmp_path: Path) -> None:
    campaign = orchestrator.authenticate_inputs(fixture(tmp_path))
    paths = orchestrator.RuntimePaths.below(campaign.config.staging_root)
    commands = orchestrator._prepare_commands(campaign, paths)
    recheck = next(command for command in commands if "recheck-readiness" in command)
    assert "--site-descriptor" in recheck
    assert "--policy" not in recheck


def test_operation_argv_is_exhaustive_and_uses_only_authenticated_coordinates(
    tmp_path: Path,
) -> None:
    campaign = orchestrator.authenticate_inputs(fixture(tmp_path))
    paths = orchestrator.RuntimePaths.below(campaign.config.staging_root)
    lock = orchestrator.LockCapability(fd=9, device=10, inode=11)
    commands = {
        operation: orchestrator.operation_argv(operation, campaign, paths, lock)
        for operation in campaign.operations
    }
    assert commands["capture-core"][0] == sys.executable
    assert "--ab-check" in commands["capture-core"]
    assert str(campaign.executables["ab-check"]) in commands["capture-core"]
    assert "--adapter" in commands["capture-core"]
    assert str(campaign.executables["ab-aozora"]) in commands["capture-core"]
    assert str(campaign.executables["ab-check"].parent / "time") in commands["capture-core"]
    assert commands["capture-predicate-pair"][0] == sys.executable
    assert str(campaign.executables["ab-aozora"]) in commands["capture-predicate-pair"]
    assert str(campaign.executables["ab-aat-to-parser-ir"]) in commands["capture-predicate-pair"]
    assert commands["capture-source"][:2] == (
        str(campaign.executables["ab-parser-rq-source-accountability"]),
        "capture-corpus",
    )
    assert "--generation-index" not in commands["capture-source"]
    assert commands["derive-diagnostic-gap"][:2] == (
        str(campaign.executables["ab-parser-rq-diagnostic-authorization"]),
        "capture-corpus",
    )
    assert commands["capture-publication"][0] == sys.executable
    assert commands["capture-resource"][0] == sys.executable
    with pytest.raises(orchestrator.ProtocolError):
        orchestrator.operation_argv("unknown", campaign, paths, lock)


@pytest.mark.skipif(
    "PARSER_RQ_CANDIDATE_ROOT" not in os.environ,
    reason="real candidate bundle is supplied by the monorepo wiring check",
)
def test_real_candidate_and_repository_producer_clis_are_wired(tmp_path: Path) -> None:
    campaign = orchestrator.authenticate_inputs(fixture(tmp_path))
    candidate_root = Path(os.environ["PARSER_RQ_CANDIDATE_ROOT"])
    repository_root = Path(os.environ["PARSER_RQ_REPOSITORY_ROOT"])
    assert (candidate_root / "bin/time").is_file()
    campaign = replace(
        campaign,
        executables={name: candidate_root / "bin" / name for name in campaign.executables},
        drivers={
            relative.as_posix(): repository_root / relative
            for relative in orchestrator.REPOSITORY_DRIVERS
        },
    )

    orchestrator._verify_wiring(campaign, orchestrator.SubprocessRunner(), tmp_path)

    work_ids = ("000001_1", "000002_2", "000003_3")
    index = write_json(
        tmp_path / "index.json",
        {
            "works": [
                {
                    "features": [],
                    "id": work_id,
                    "txt_path": txt_path,
                }
                for work_id, txt_path in zip(
                    work_ids,
                    (
                        "cards/000001/files/1_ruby/test.txt",
                        "cards/000002/files/2_gaiji/test.txt",
                        "cards/000003/files/3_both/test.txt",
                    ),
                    strict=True,
                )
            ]
        },
    )
    selected = write_json(tmp_path / "work-ids.json", list(work_ids))
    reports = tmp_path / "reports"
    subprocess.run(
        [
            str(candidate_root / "bin/ab-check"),
            "--index",
            str(index),
            "--corpus",
            str(repository_root / "ab-validator/crates/ab-index/tests/fixtures/corpus"),
            "--adapter",
            str(candidate_root / "bin/ab-aozora"),
            "--output",
            str(reports),
            "--work-ids",
            str(selected),
            "--jobs",
            "1",
            "--per-work-timeout",
            "60s",
        ],
        check=True,
    )
    core_path = repository_root / "ab-validator/reports/parser-ir/parser-rq-core-attempt-capture.py"
    core_spec = importlib.util.spec_from_file_location("parser_rq_core_attempt_capture", core_path)
    assert core_spec and core_spec.loader
    core_capture = importlib.util.module_from_spec(core_spec)
    sys.modules[core_spec.name] = core_capture
    core_spec.loader.exec_module(core_capture)
    assert set(core_capture._closed_reports(reports, set(work_ids))) == set(work_ids)


def test_wiring_commands_exercise_the_production_rust_subcommands(tmp_path: Path) -> None:
    campaign = orchestrator.authenticate_inputs(fixture(tmp_path))

    commands = orchestrator._wiring_commands(campaign)

    assert (
        str(campaign.executables["ab-parser-rq-source-accountability"]),
        "capture-corpus",
        "--help",
    ) in commands
    assert (
        str(campaign.executables["ab-parser-rq-diagnostic-authorization"]),
        "capture-corpus",
        "--help",
    ) in commands


def test_publication_capture_input_rehashes_the_closed_artifact_set(tmp_path: Path) -> None:
    campaign = orchestrator.authenticate_inputs(fixture(tmp_path))
    paths = orchestrator.RuntimePaths.below(campaign.config.staging_root)
    identity_ref = campaign.readiness_receipt["qualification_identity_ref"]
    write_json(
        paths.runtime,
        {
            "candidate": {"qualification_identity": {"parser_git_rev": "a" * 40}},
            "corpus": {
                "corpus_id": "fixture",
                "corpus_snapshot_hash": "sha256:" + "1" * 64,
                "list_hash": "sha256:" + "2" * 64,
                "entries": [{"work_id": "w1", "source_sha256": "sha256:" + "3" * 64}],
            },
        },
    )
    write_json(
        campaign.config.candidate_tree / "abc/data/parser-rq-publication-policy-v1.json",
        {
            "policy_hash": "sha256:" + "4" * 64,
            "preservation_schema": {"hash": "sha256:" + "5" * 64},
            "validator": {"semantics_hash": "sha256:" + "6" * 64},
        },
    )
    write_json(
        campaign.config.candidate_tree / "abc/data/parser-rq-publication-fixtures-v1.json",
        {"census_hash": "sha256:" + "7" * 64, "works": {"w1": {}}},
    )
    write_json(
        paths.publication_root / "validated/w1.json",
        {
            "join_input": {"status": "valid"},
            "structure_check_candidates": {"plaintext_body_only": True},
            "counts": {
                "preservation_records": 1,
                "tei_preservation_references": 1,
                "non_null_tei_pointers": 1,
                "non_null_source_pointers": 1,
                "by_construct": {"source_identity": 1},
            },
        },
    )
    materialized = paths.publication_root / "materialized/w1"
    for relative in orchestrator.PUBLICATION_ARTIFACTS:
        destination = materialized / relative
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_bytes(relative.encode())

    capture = orchestrator._publication_capture_input(campaign, paths)

    assert capture["authority"]["qualification_identity_ref"] == identity_ref
    assert capture["works"][0]["work_id"] == "w1"
    artifacts = capture["works"][0]["publication"]["artifacts"]
    assert len(artifacts) == len(orchestrator.PUBLICATION_ARTIFACTS)
    assert all(set(row) == {"sha256", "bytes", "media_type"} for row in artifacts)


class FakeRunner:
    def __init__(self, staging: Path, *, fail_at: str | None = None) -> None:
        self.staging = staging
        self.fail_at = fail_at
        self.calls: list[tuple[str, ...]] = []
        self.pass_fds: list[tuple[int, ...]] = []
        self.cwds: list[Path] = []

    def run(self, argv: tuple[str, ...], *, cwd: Path, pass_fds: tuple[int, ...] = ()) -> int:
        self.calls.append(argv)
        self.pass_fds.append(pass_fds)
        self.cwds.append(cwd)
        joined = " ".join(argv)
        if self.fail_at and self.fail_at in joined:
            return 2
        if "runtime-inputs" in argv:
            output = Path(argv[argv.index("--out") + 1])
            write_json(
                output,
                {
                    "schema_version": "abc/parser-rq-runtime-inputs/v1",
                    "candidate": {
                        "qualification_identity": {
                            "instrument_versions": {},
                            "parser_git_rev": "a" * 40,
                        }
                    },
                    "corpus": {"corpus_root": "ab-validator/corpus", "entries": []},
                    "source_accountability_corpus": [],
                },
            )
        if "parser-rq-core-attempt-capture.py" in joined:
            write_json(self.staging / "lanes/core/core-index.json", {"fixture": True})
        if "parser-rq-predicate-hardening-capture.py" in joined:
            predicate_output = self.staging / "lanes/predicate/output"
            write_json(
                predicate_output / "raw-diagnostics-index.json",
                {"expected_work_ids": [], "records": []},
            )
            write_json(
                predicate_output / "parser-ir-index.json",
                {"expected_work_ids": [], "records": []},
            )
        member_by_operation = {
            "parser-rq-core-attempt-capture.py": ("core_attempt",),
            "parser-rq-predicate-hardening-capture.py": (
                "diagnostic_completeness",
                "parser_ir_conformance",
            ),
            "capture-corpus --corpus": ("source_recognition",),
            "capture-corpus --index": ("diagnostic_gap",),
            "publication-rq-capture.py": ("publication_structure",),
            "parser-rq-resource-capture.py": ("resource",),
        }
        for marker, members in member_by_operation.items():
            if marker in joined:
                for member in members:
                    write_json(self.staging / f"{member}.json", {"fixture": True})
        if "capture-corpus --corpus" in joined:
            source_output = self.staging / "lanes/source/output"
            (self.staging / "lanes/source/store").mkdir(parents=True, exist_ok=True)
            write_json(
                source_output / "classified-source-generation-index.json",
                {"records": []},
            )
            write_json(
                source_output / "source-recognition-index.json",
                {
                    "qualification_identity_ref": "sha256:" + "e" * 64,
                    "corpus_generation_ref": "sha256:" + "f" * 64,
                    "records": [],
                },
            )
            write_json(
                source_output / "source-recognition-aggregate.json",
                {"eligible_bytes": 0},
            )
        if "capture-corpus --index" in joined:
            write_json(
                self.staging / "lanes/diagnostic/diagnostic-gap.json",
                {"aggregate": {"eligible": True}, "works": []},
            )
        if "publication-rq-capture.py" in joined:
            write_json(self.staging / "lanes/publication/manifest.json", {"blobs": []})
            write_json(self.staging / "lanes/publication/index.json", {"records": []})
        if "parser-rq-resource-capture.py" in joined:
            write_json(
                self.staging / "lanes/resource/index.json",
                {"records": []},
            )
        if "-M:abc/parser-rq-member" in argv:
            command = argv[argv.index("-M:abc/parser-rq-member") + 1]
            keys = {
                "core": ("fatal_failures", "wall_time_seconds", "timeouts"),
                "predicate-pair": (
                    "diagnostic_completeness",
                    "parser_ir_schema_validation",
                ),
                "source-recognition": ("source_span_coverage",),
                "diagnostic-gap": ("silent_drops",),
                "publication": ("publication_structure",),
                "resource": ("peak_cgroup_memory_bytes",),
            }[command]
            output = Path(argv[argv.index("--out") + 1])
            write_json(
                output,
                {
                    key: {
                        "value": 0.0,
                        "identity_ref": "sha256:" + "e" * 64,
                    }
                    for key in keys
                },
            )
        return 0


def test_execute_graph_distinguishes_preparation_from_consumed_unavailability(
    tmp_path: Path,
) -> None:
    campaign = orchestrator.authenticate_inputs(fixture(tmp_path))
    prestart = FakeRunner(campaign.config.staging_root, fail_at="recheck-readiness")
    result = orchestrator.execute_graph(
        campaign,
        prestart,
        now=lambda: datetime(2026, 7, 18, tzinfo=UTC),
    )
    assert result.status == "preparation_failed"
    assert not (campaign.config.staging_root / "capture-start.json").exists()

    poststart = FakeRunner(campaign.config.staging_root, fail_at="capture-corpus --corpus")
    result = orchestrator.execute_graph(
        campaign,
        poststart,
        now=lambda: datetime(2026, 7, 18, tzinfo=UTC),
    )
    assert result.status == "unavailable"
    assert (campaign.config.staging_root / "capture-start.json").is_file()
    terminal = json.loads((campaign.config.staging_root / "unavailable-terminal.json").read_bytes())
    assert terminal["status"] == "unavailable"
    assert sum("capture-corpus --corpus" in " ".join(call) for call in poststart.calls) == 1


def test_execute_graph_keeps_one_lock_through_closed_composition(tmp_path: Path) -> None:
    campaign = orchestrator.authenticate_inputs(fixture(tmp_path))
    runner = FakeRunner(campaign.config.staging_root)
    result = orchestrator.execute_graph(
        campaign,
        runner,
        now=lambda: datetime(2026, 7, 18, tzinfo=UTC),
    )
    assert result.status == "captured", result.reason
    inherited = [fds for fds in runner.pass_fds if fds]
    assert len(inherited) == 1
    assert len(inherited[0]) == 1
    with pytest.raises(OSError):
        __import__("os").fstat(inherited[0][0])
    assert len(set(runner.cwds)) == 1
    assert runner.cwds[0].name == "detached-cwd"
