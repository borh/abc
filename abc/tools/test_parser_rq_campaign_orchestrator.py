from __future__ import annotations

import importlib.util
import hashlib
import json
import sys
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
        "production_graph_hash": graph["policy_hash"],
        "provenance_core_ref": provenance["provenance_core_ref"],
    }
    site_policy = {"replica_status": "configured"}
    site_descriptor = {"campaign_lock_path": str(tmp_path / "campaign.lock")}
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
        site_policy=write_json(evidence_tree / "site-policy.json", site_policy),
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
        "clojure",
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
        "site_policy",
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
