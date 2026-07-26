from __future__ import annotations

import importlib.util
import hashlib
import inspect
import json
import os
import re
import shutil
import subprocess
import sys
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


# The corpus_root the governed corpus artifact declares
# (abc/data/parser-release-qualification-corpus.edn). Capture used to restate
# this literal instead of reading it.
GOVERNED_CORPUS_ROOT = "ab-validator/crates/ab-index/tests/fixtures/corpus"


def fixture(tmp_path: Path) -> Any:
    candidate_tree = tmp_path / "candidate-tree"
    evidence_tree = tmp_path / "evidence-tree"
    (candidate_tree / "abc/data").mkdir(parents=True)
    (candidate_tree / "abc/tools").mkdir(parents=True)
    (candidate_tree / GOVERNED_CORPUS_ROOT).mkdir(parents=True)
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
    # Membership is declared empty to agree with FakeRunner's empty runtime
    # corpus, matching the shape the resource policy below already used. The
    # run now authenticates every one of these against the governed corpus, so
    # a placeholder that declares no membership at all is a divergence.
    for name, membership in (
        (
            "parser-rq-core-attempt-policy-v1.json",
            {"expected_work_ids": [], "expected_sources": []},
        ),
        ("parser-rq-diagnostic-completeness-policy-v1.json", {"expected_work_ids": []}),
        ("parser-rq-parser-ir-conformance-policy-v1.json", {"expected_work_ids": []}),
        ("parser-rq-resource-policy-v1.json", {"work_ids": []}),
    ):
        write_json(candidate_tree / "abc/data" / name, {"fixture": True, **membership})
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
    site_descriptor = {
        "schema_id": "https://w3id.org/abc/schemas/parser-rq-site-descriptor.schema.json",
        "schema_version": "3.0.0",
        "campaign_lock_path": str(tmp_path / "campaign.lock"),
        "evidence_store_root": str(evidence_store),
        "scratch_root": str(scratch),
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
        operation: orchestrator.operation_argv(
            operation, campaign, paths, lock, campaign.config.candidate_tree / GOVERNED_CORPUS_ROOT
        )
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
        orchestrator.operation_argv(
            "unknown", campaign, paths, lock, campaign.config.candidate_tree / GOVERNED_CORPUS_ROOT
        )


def real_campaign_config(tmp_path: Path, repository_root: Path, candidate_root: Path) -> Any:
    base = fixture(tmp_path)
    graph = json.loads(
        (repository_root / "abc/data/parser-rq-production-graph-v1.json").read_bytes()
    )
    executable_rows = []
    for reviewed in graph["executables"]:
        executable = candidate_root / "bin" / reviewed["name"]
        payload = executable.read_bytes()
        executable_rows.append(
            {
                **reviewed,
                "nix_output": str(candidate_root),
                "nar_hash": "sha256:" + "a" * 64,
                "sha256": "sha256:" + hashlib.sha256(payload).hexdigest(),
                "bytes": len(payload),
                "parser_git_rev": "a" * 40,
            }
        )
    provenance = {
        "status": "reproducible",
        "provenance_core_ref": "sha256:" + "c" * 64,
        "executables": executable_rows,
    }
    write_json(base.provenance, provenance)
    write_json(
        base.readiness_receipt,
        {
            "readiness_receipt_ref": "sha256:" + "d" * 64,
            "qualification_identity_ref": "sha256:" + "e" * 64,
            "production_graph_hash": graph["policy_hash"],
            "provenance_core_ref": provenance["provenance_core_ref"],
        },
    )
    return orchestrator.CampaignConfig(
        candidate=base.candidate,
        authorization=base.authorization,
        provenance=base.provenance,
        readiness_receipt=base.readiness_receipt,
        site_descriptor=base.site_descriptor,
        candidate_tree=repository_root,
        evidence_tree=base.evidence_tree,
        staging_root=base.staging_root,
        production=True,
    )


def real_authenticated_campaign(tmp_path: Path, repository_root: Path, candidate_root: Path) -> Any:
    return orchestrator.authenticate_inputs(
        real_campaign_config(tmp_path, repository_root, candidate_root)
    )


def bounded_candidate_tree(tmp_path: Path, repository_root: Path) -> Path:
    candidate_tree = tmp_path / "bounded-candidate-tree"
    shutil.copytree(repository_root / "abc", candidate_tree / "abc")
    for relative in (
        Path("data"),
        Path("reports"),
        Path("crates/ab-index/tests/fixtures/corpus"),
    ):
        shutil.copytree(
            repository_root / "ab-validator" / relative,
            candidate_tree / "ab-validator" / relative,
        )
    return candidate_tree


def bounded_identity() -> dict[str, object]:
    return {
        "aat_version": 2,
        "aat_adapter": "ab-aozora",
        "aat_adapter_version": "ab-aozora 0.6.0 aat-schema 2 facade 0.3.0 wire-schema 3 (git unknown)",
        "parser_git_rev": "a" * 40,
        "mapping_id": "https://w3id.org/abc/mappings/aat-v2-to-parser-ir-v1/generated-probe",
        "mapping_version": "0.5.0",
        "mapping_hash": "sha256:73e1df7af281848c7249003a6df7945cad3bcf7e29f35fff7c9da2bb3c695413",
        "mapping_schema_hash": "sha256:e6af01115ccdb7c5cad086eee4c458230f6b6f55e0dfee7791730b48994283e2",
        "parser_ir_schema_id": "https://w3id.org/abc/schemas/parser-ir.schema.json",
        "parser_ir_schema_hash": "sha256:43a6a6d86ca5eca062508e6cae633d19bf5248f15c5bb46153a6d8580ea916ec",
        "corpus_snapshot_hash": "sha256:63d8d53a9a0ef8ec80c921d7fb17d142f231fbc061066fb8056b951ffcfbe47e",
        "corpus_list_hash": "sha256:ace3fa3f4fb6565d46276d8276b4a2e183c58e595f27f0e3149d7395ca6554dd",
        "predicate_set_hash": "sha256:bec4fff7ab46003667df6115accf16da88260e02a003a07ab5537e8f5851c203",
        "instrument_versions": {
            "diagnostic_completeness": "ab-aozora --mode diagnostics envelope entries {code,severity,source,span}",
            "fatal_failures": "parser-rq-core-attempt-v1",
            "parser_ir_schema_validation": "ab-aat-to-parser-ir convert with validate_output_parser_ir",
            "peak_cgroup_memory_bytes": "parser-rq-resource-v1",
            "publication_structure": "reports/parser-ir/publication-bundle-validate.py against parser-ir-publication-preservation.schema.json",
            "silent_drops": "parser-rq-diagnostic-authorization-v1",
            "source_span_coverage": "parser-rq-source-recognition-v1",
            "timeouts": "parser-rq-core-attempt-v1",
            "wall_time_seconds": "parser-rq-core-attempt-v1",
        },
    }


def bounded_authenticated_campaign(
    tmp_path: Path, candidate_tree: Path, candidate_root: Path
) -> Any:
    config = real_campaign_config(tmp_path, candidate_tree, candidate_root)
    receipt = json.loads(config.readiness_receipt.read_bytes())
    receipt["qualification_identity_ref"] = orchestrator.content_ref(bounded_identity())
    write_json(config.readiness_receipt, receipt)
    return orchestrator.authenticate_inputs(config)


def write_bounded_runtime(path: Path, repository_root: Path) -> None:
    corpus_root = "ab-validator/crates/ab-index/tests/fixtures/corpus"
    sources = (
        ("000001_1", "cards/000001/files/1_ruby/test.txt"),
        ("000002_2", "cards/000002/files/2_gaiji/test.txt"),
        ("000003_3", "cards/000003/files/3_both/test.txt"),
    )
    entries = []
    source_entries = []
    for work_id, relative in sources:
        source_path = f"{corpus_root}/{relative}"
        digest = (
            "sha256:" + hashlib.sha256((repository_root / source_path).read_bytes()).hexdigest()
        )
        entries.append({"work_id": work_id, "source_path": source_path, "source_sha256": digest})
        source_entries.append(
            {"work_id": work_id, "source_path": source_path, "original_sha256": digest}
        )
    identity = bounded_identity()
    identity_ref = orchestrator.content_ref(identity)
    write_json(
        path,
        {
            "schema_version": "abc/parser-rq-runtime-inputs/v1",
            "candidate": {
                "candidate_ref": "sha256:" + "b" * 64,
                "qualification_identity_ref": identity_ref,
                "qualification_identity": identity,
            },
            "corpus": {
                "corpus_id": "abc/parser-release-qualification-corpus/v1",
                "corpus_root": corpus_root,
                "corpus_snapshot_hash": identity["corpus_snapshot_hash"],
                "list_hash": "sha256:" + "3" * 64,
                "entries": entries,
            },
            "source_accountability_corpus": source_entries,
            "authorization": {
                "authorization_ref": "sha256:" + "f" * 64,
                "authorization_ordinal": 1,
                "candidate_ref": "sha256:" + "b" * 64,
                "qualification_identity_ref": identity_ref,
                "not_before_utc": "2026-01-01T00:00:00Z",
                "not_after_utc": "2027-01-01T00:00:00Z",
                "repetitions": 3,
                "reduction": "maximum",
            },
        },
    )


@pytest.mark.skipif(
    "PARSER_RQ_CANDIDATE_ROOT" not in os.environ,
    reason="real candidate bundle is supplied by the monorepo wiring check",
)
def test_real_candidate_and_repository_producer_clis_are_wired(tmp_path: Path) -> None:
    candidate_root = Path(os.environ["PARSER_RQ_CANDIDATE_ROOT"])
    repository_root = Path(os.environ["PARSER_RQ_REPOSITORY_ROOT"])
    assert (candidate_root / "bin/time").is_file()
    campaign = real_authenticated_campaign(tmp_path, repository_root, candidate_root)

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


@pytest.mark.skipif(
    "PARSER_RQ_CANDIDATE_ROOT" not in os.environ,
    reason="real candidate bundle is supplied by the monorepo wiring check",
)
def test_real_candidate_authentication_requires_adapter_row(tmp_path: Path) -> None:
    candidate_root = Path(os.environ["PARSER_RQ_CANDIDATE_ROOT"])
    repository_root = Path(os.environ["PARSER_RQ_REPOSITORY_ROOT"])
    config = real_campaign_config(tmp_path, repository_root, candidate_root)
    provenance = json.loads(config.provenance.read_bytes())
    provenance["executables"] = [
        row for row in provenance["executables"] if row["name"] != "ab-aozora"
    ]
    write_json(config.provenance, provenance)

    with pytest.raises(orchestrator.PreparationFailed, match="membership differs"):
        orchestrator.authenticate_inputs(config)


@pytest.mark.skipif(
    "PARSER_RQ_CANDIDATE_ROOT" not in os.environ,
    reason="real candidate bundle is supplied by the monorepo wiring check",
)
def test_real_candidate_core_argv_uses_authenticated_adapter(tmp_path: Path) -> None:
    candidate_root = Path(os.environ["PARSER_RQ_CANDIDATE_ROOT"])
    repository_root = Path(os.environ["PARSER_RQ_REPOSITORY_ROOT"])
    campaign = real_authenticated_campaign(tmp_path, repository_root, candidate_root)
    paths = orchestrator.RuntimePaths.below(campaign.config.staging_root)
    lock = orchestrator.LockCapability(fd=9, device=10, inode=11)

    argv = orchestrator.operation_argv(
        "capture-core", campaign, paths, lock, campaign.config.candidate_tree / GOVERNED_CORPUS_ROOT
    )

    adapter_index = argv.index("--adapter") + 1
    assert Path(argv[adapter_index]) == campaign.executables["ab-aozora"]


@pytest.mark.skipif(
    "PARSER_RQ_CANDIDATE_ROOT" not in os.environ,
    reason="real candidate bundle is supplied by the monorepo wiring check",
)
def test_bounded_production_chain_uses_shared_composition(tmp_path: Path) -> None:
    candidate_root = Path(os.environ["PARSER_RQ_CANDIDATE_ROOT"])
    repository_root = Path(os.environ["PARSER_RQ_REPOSITORY_ROOT"])
    candidate_tree = bounded_candidate_tree(tmp_path, repository_root)
    campaign = bounded_authenticated_campaign(tmp_path, candidate_tree, candidate_root)
    campaign = replace(
        campaign,
        clojure_prefix=(
            "bash",
            "-c",
            'cd "$0" && exec clojure "$@"',
            str(candidate_tree / "abc"),
        ),
    )
    paths = orchestrator.RuntimePaths.below(campaign.config.staging_root)
    write_bounded_runtime(paths.runtime, candidate_tree)
    orchestrator._materialize_runtime(paths)
    cwd = paths.root / "detached-cwd"
    cwd.mkdir(parents=True)
    lock = orchestrator._acquire_lock(tmp_path / "bounded-campaign.lock")
    try:
        for operation in campaign.operations:
            if operation == "capture-resource":
                break
            orchestrator._execute_operation(
                operation,
                campaign,
                paths,
                lock,
                orchestrator.SubprocessRunner(),
                cwd,
            )
            if operation == "capture-core":
                report_root = paths.core_root / "records/repetition-1/reports"
                reports = list(report_root.rglob("*.json"))
                assert reports
                assert any(path.parent != report_root for path in reports)
                assert all(re.fullmatch(r".+-[0-9a-f]{12}\.json", path.name) for path in reports)
            if operation == "capture-publication":
                publication_index = json.loads((paths.publication_root / "index.json").read_bytes())
                assert publication_index["corpus_id"] == (
                    "abc/parser-release-qualification-corpus/v1"
                )
                source_manifest = json.loads(
                    (paths.predicate_root / "output/parser-ir/source.manifest.json").read_bytes()
                )
                assert (
                    source_manifest["manifest_identity_object"]["corpus_snapshot_hash"]
                    == (bounded_identity()["corpus_snapshot_hash"])
                )
    finally:
        os.close(lock.fd)

    orchestrator._assert_member_set(
        paths,
        {
            "core_attempt",
            "diagnostic_completeness",
            "parser_ir_conformance",
            "source_recognition",
            "diagnostic_gap",
            "publication_structure",
        },
    )


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
                "corpus_root": GOVERNED_CORPUS_ROOT,
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


def test_resource_projection_binds_the_qualification_identity(tmp_path: Path) -> None:
    campaign = orchestrator.authenticate_inputs(fixture(tmp_path))
    paths = orchestrator.RuntimePaths.below(campaign.config.staging_root)
    write_json(
        paths.runtime,
        {
            "candidate": {
                "qualification_identity": {
                    "instrument_versions": {},
                    "parser_git_rev": "a" * 40,
                }
            }
        },
    )
    write_json(paths.resource_root / "index.json", {"records": []})

    command, inputs, members = orchestrator._projection_input("capture-resource", campaign, paths)

    assert command == "resource"
    assert (
        inputs["qualification_identity_ref"]
        == (campaign.readiness_receipt["qualification_identity_ref"])
    )
    assert "identity" not in inputs
    assert members == {"resource": ("peak_cgroup_memory_bytes",)}


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
                            "corpus_snapshot_hash": "sha256:" + "1" * 64,
                        }
                    },
                    "corpus": {"corpus_root": GOVERNED_CORPUS_ROOT, "entries": []},
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


def test_execute_operation_passes_lock_fd_only_to_core(tmp_path: Path) -> None:
    campaign = orchestrator.authenticate_inputs(fixture(tmp_path))
    paths = orchestrator.RuntimePaths.below(campaign.config.staging_root)
    runner = FakeRunner(campaign.config.staging_root)
    lock = orchestrator.LockCapability(fd=9, device=10, inode=11)
    cwd = paths.root / "detached-cwd"
    cwd.mkdir(parents=True)
    write_json(
        paths.runtime,
        {
            "candidate": {
                "qualification_identity": {
                    "instrument_versions": {},
                    "parser_git_rev": "a" * 40,
                }
            },
            "corpus": {"corpus_root": GOVERNED_CORPUS_ROOT, "entries": []},
        },
    )

    orchestrator._execute_operation("capture-core", campaign, paths, lock, runner, cwd)
    orchestrator._execute_operation("capture-predicate-pair", campaign, paths, lock, runner, cwd)

    core_call = next(
        index
        for index, argv in enumerate(runner.calls)
        if "parser-rq-core-attempt-capture.py" in " ".join(argv)
    )
    predicate_call = next(
        index
        for index, argv in enumerate(runner.calls)
        if "parser-rq-predicate-hardening-capture.py" in " ".join(argv)
    )
    assert runner.pass_fds[core_call] == (lock.fd,)
    assert runner.pass_fds[predicate_call] == ()
    assert set(runner.cwds) == {cwd}


def test_assert_member_set_rejects_missing_and_extra_members(tmp_path: Path) -> None:
    paths = orchestrator.RuntimePaths.below(tmp_path / "staging")
    write_json(paths.root / "core_attempt.json", {})

    with pytest.raises(orchestrator.ProtocolError, match="canonical member mismatch"):
        orchestrator._assert_member_set(paths, ("core_attempt", "source_recognition"))

    write_json(paths.root / "source_recognition.json", {})
    write_json(paths.root / "resource.json", {})
    with pytest.raises(orchestrator.ProtocolError, match="canonical member mismatch"):
        orchestrator._assert_member_set(paths, ("core_attempt", "source_recognition"))


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


def test_resolve_corpus_root_reproduces_the_retired_hardcoded_path(tmp_path: Path) -> None:
    """CHARACTERIZATION: for the governed corpus_root, the resolver yields exactly
    the path the two retired hardcodes produced, so unifying them changed nothing
    about which bytes capture reads."""
    campaign = orchestrator.authenticate_inputs(fixture(tmp_path))
    candidate_tree = campaign.config.candidate_tree
    # The two retired expressions, spelled as they were in each capture path.
    retired_core = candidate_tree / "ab-validator" / "crates/ab-index/tests/fixtures/corpus"
    retired_resource = candidate_tree / "ab-validator/crates/ab-index/tests/fixtures/corpus"

    resolved = orchestrator.resolve_corpus_root(candidate_tree, GOVERNED_CORPUS_ROOT)

    assert resolved == retired_core.resolve(strict=True)
    assert resolved == retired_resource.resolve(strict=True)


def test_resolve_corpus_root_fails_closed_on_escaping_or_absent_roots(tmp_path: Path) -> None:
    campaign = orchestrator.authenticate_inputs(fixture(tmp_path))
    tree = campaign.config.candidate_tree
    for bad in (None, "", 7, "/etc", "../outside", "ab-validator/does-not-exist"):
        with pytest.raises(orchestrator.ProtocolError):
            orchestrator.resolve_corpus_root(tree, bad)


def test_resolve_corpus_root_rejects_a_file(tmp_path: Path) -> None:
    campaign = orchestrator.authenticate_inputs(fixture(tmp_path))
    tree = campaign.config.candidate_tree
    (tree / "ab-validator/not-a-dir").write_text("x")
    with pytest.raises(orchestrator.ProtocolError):
        orchestrator.resolve_corpus_root(tree, "ab-validator/not-a-dir")


def test_core_and_resource_capture_read_one_corpus_root(tmp_path: Path) -> None:
    """Both consumers must receive the SAME resolved root. Fixing only one capture
    path would leave the other reading the old location."""
    campaign = orchestrator.authenticate_inputs(fixture(tmp_path))
    paths = orchestrator.RuntimePaths.below(campaign.config.staging_root)
    lock = orchestrator.LockCapability(fd=9, device=10, inode=11)
    resolved = orchestrator.resolve_corpus_root(
        campaign.config.candidate_tree, GOVERNED_CORPUS_ROOT
    )

    argv = orchestrator.operation_argv("capture-core", campaign, paths, lock, resolved)

    assert argv[argv.index("--corpus-root") + 1] == str(resolved)
    # _prepare_resource passes the same value through to ab-check's --corpus.
    source = inspect.getsource(orchestrator._prepare_resource)
    assert "fixtures/corpus" not in source, "resource capture still restates the corpus path"
    assert "str(corpus_root)" in source


# Finding D of the instrument-semantics audit. diagnostic-completeness,
# parser-IR conformance, and publication already compare their membership
# against a corpus-derived authority. Resource compared its policy to itself,
# and core-attempt only failed indirectly -- a policy work absent from the
# corpus produced no report, and the missing-report error named a symptom
# rather than the divergence. Both now fail closed on the divergence itself.
MEMBERSHIP_SOURCES = (
    ("w1", "sha256:" + "1" * 64),
    ("w2", "sha256:" + "2" * 64),
)


def membership_corpus(sources: tuple[tuple[str, str], ...] = MEMBERSHIP_SOURCES) -> dict[str, Any]:
    return {
        "corpus_id": "abc/parser-release-qualification-corpus/v1",
        "corpus_root": GOVERNED_CORPUS_ROOT,
        "entries": [
            {
                "work_id": work_id,
                "source_path": f"{GOVERNED_CORPUS_ROOT}/{work_id}.txt",
                "source_sha256": digest,
            }
            for work_id, digest in sources
        ],
    }


def write_membership_policies(
    candidate_tree: Path,
    sources: tuple[tuple[str, str], ...] = MEMBERSHIP_SOURCES,
) -> None:
    work_ids = [work_id for work_id, _ in sources]
    expected_sources = [
        {"work_id": work_id, "source_sha256": digest} for work_id, digest in sources
    ]
    write_json(
        candidate_tree / "abc/data/parser-rq-core-attempt-policy-v1.json",
        {"expected_work_ids": work_ids, "expected_sources": expected_sources},
    )
    for relative in (
        "abc/data/parser-rq-diagnostic-completeness-policy-v1.json",
        "abc/data/parser-rq-parser-ir-conformance-policy-v1.json",
    ):
        write_json(candidate_tree / relative, {"expected_work_ids": work_ids})
    write_json(
        candidate_tree / "abc/data/parser-rq-resource-policy-v1.json",
        {"work_ids": work_ids},
    )


def test_policy_membership_agreeing_with_the_corpus_is_authenticated(tmp_path: Path) -> None:
    campaign = orchestrator.authenticate_inputs(fixture(tmp_path))
    write_membership_policies(campaign.config.candidate_tree)

    orchestrator._authenticate_policy_membership(
        campaign.config.candidate_tree, membership_corpus()
    )


@pytest.mark.parametrize(
    ("relative", "field", "value"),
    [
        ("abc/data/parser-rq-resource-policy-v1.json", "work_ids", ["w1"]),
        ("abc/data/parser-rq-resource-policy-v1.json", "work_ids", ["w1", "w2", "w3"]),
        ("abc/data/parser-rq-resource-policy-v1.json", "work_ids", ["w2", "w1"]),
        (
            "abc/data/parser-rq-core-attempt-policy-v1.json",
            "expected_work_ids",
            ["w1", "w3"],
        ),
        (
            "abc/data/parser-rq-diagnostic-completeness-policy-v1.json",
            "expected_work_ids",
            ["w1"],
        ),
        (
            "abc/data/parser-rq-parser-ir-conformance-policy-v1.json",
            "expected_work_ids",
            ["w1"],
        ),
    ],
)
def test_policy_membership_divergence_fails_closed(
    tmp_path: Path, relative: str, field: str, value: list[str]
) -> None:
    campaign = orchestrator.authenticate_inputs(fixture(tmp_path))
    write_membership_policies(campaign.config.candidate_tree)
    policy = json.loads((campaign.config.candidate_tree / relative).read_bytes())
    policy[field] = value
    write_json(campaign.config.candidate_tree / relative, policy)

    with pytest.raises(orchestrator.ProtocolError, match="membership differs"):
        orchestrator._authenticate_policy_membership(
            campaign.config.candidate_tree, membership_corpus()
        )


def test_core_attempt_source_hash_divergence_fails_closed(tmp_path: Path) -> None:
    """`expected_sources` pins bytes the corpus also pins; they must agree.

    Membership equality alone would let a policy claim the right works against
    the wrong bytes.
    """
    campaign = orchestrator.authenticate_inputs(fixture(tmp_path))
    write_membership_policies(campaign.config.candidate_tree)
    relative = "abc/data/parser-rq-core-attempt-policy-v1.json"
    policy = json.loads((campaign.config.candidate_tree / relative).read_bytes())
    policy["expected_sources"][1]["source_sha256"] = "sha256:" + "9" * 64
    write_json(campaign.config.candidate_tree / relative, policy)

    with pytest.raises(orchestrator.ProtocolError, match="source identity differs"):
        orchestrator._authenticate_policy_membership(
            campaign.config.candidate_tree, membership_corpus()
        )


def test_membership_authentication_rejects_a_malformed_policy(tmp_path: Path) -> None:
    campaign = orchestrator.authenticate_inputs(fixture(tmp_path))
    write_membership_policies(campaign.config.candidate_tree)
    write_json(
        campaign.config.candidate_tree / "abc/data/parser-rq-resource-policy-v1.json",
        {"work_ids": "w1"},
    )

    with pytest.raises(orchestrator.ProtocolError, match="membership is malformed"):
        orchestrator._authenticate_policy_membership(
            campaign.config.candidate_tree, membership_corpus()
        )


def test_committed_policies_agree_with_the_governed_corpus() -> None:
    """Characterization: the present corpus and policies already agree.

    The membership authentication above changes no outcome on the workset in
    force. It changes what happens when they stop agreeing.
    """
    repository_root = Path(__file__).resolve().parents[2]
    corpus_edn = (repository_root / "abc/data/parser-release-qualification-corpus.edn").read_text(
        encoding="utf-8"
    )
    work_ids = re.findall(r':work_id "([^"]+)"', corpus_edn)
    digests = re.findall(r':source_sha256 "([^"]+)"', corpus_edn)
    assert len(work_ids) == len(digests) == 3

    orchestrator._authenticate_policy_membership(
        repository_root,
        {
            "entries": [
                {"work_id": work_id, "source_sha256": digest}
                for work_id, digest in zip(work_ids, digests, strict=True)
            ]
        },
    )


def test_execute_graph_refuses_to_capture_under_diverging_membership(tmp_path: Path) -> None:
    """The authentication must be reached by the run, not merely callable.

    It fails before `capture-start.json` exists, so a campaign whose policies
    disagree with the governed corpus produces no evidence at all rather than
    evidence about a workset nobody authorized.
    """
    campaign = orchestrator.authenticate_inputs(fixture(tmp_path))
    write_json(
        campaign.config.candidate_tree / "abc/data/parser-rq-resource-policy-v1.json",
        {
            "policy_hash": "sha256:" + "8" * 64,
            "production_command_hash": orchestrator.RESOURCE_COMMAND_HASH,
            "work_ids": ["a-work-the-corpus-does-not-contain"],
        },
    )

    result = orchestrator.execute_graph(
        campaign,
        FakeRunner(campaign.config.staging_root),
        now=lambda: datetime(2026, 7, 18, tzinfo=UTC),
    )

    assert result.status == "preparation_failed"
    assert "membership differs from the governed corpus" in result.reason
    assert not (campaign.config.staging_root / "capture-start.json").exists()
