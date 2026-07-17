from __future__ import annotations

import importlib.util
import json
import pathlib
import shutil

import pytest


REPO_ROOT = pathlib.Path(__file__).resolve().parents[4]
SCRIPT = REPO_ROOT / "ab-validator/reports/parser-ir/predicate-hardening-identity.py"


@pytest.fixture
def module():
    spec = importlib.util.spec_from_file_location("predicate_hardening_identity", SCRIPT)
    assert spec and spec.loader
    loaded = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(loaded)
    return loaded


@pytest.mark.parametrize("instrument", ["diagnostic-completeness", "parser-ir-conformance"])
def test_reviewed_sources_equal_discovered_owned_closure(module, instrument):
    assert module.discover_owned_sources(REPO_ROOT, instrument) == {
        REPO_ROOT / path for path in module.INSTRUMENTS[instrument].reviewed_sources
    }


def test_helper_byte_mutation_changes_semantic_hash(module, tmp_path):
    root = tmp_path / "repo"
    config = module.INSTRUMENTS["diagnostic-completeness"]
    for path in (*config.reviewed_sources, *config.artifacts):
        destination = root / path
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_bytes((REPO_ROOT / path).read_bytes())
    before = module.build_manifest(root, "diagnostic-completeness")
    helper = root / "abc/src/abc/tools/parser_rq_capture.clj"
    helper.write_text(helper.read_text(encoding="utf-8") + "\n", encoding="utf-8")
    after = module.build_manifest(root, "diagnostic-completeness")
    assert before["validator_semantics_hash"] != after["validator_semantics_hash"]


def test_added_or_removed_helper_fails_reviewed_set_equality(module, tmp_path):
    root = tmp_path / "repo"
    shutil.copytree(REPO_ROOT / "abc/src", root / "abc/src")
    for path in module.INSTRUMENTS["diagnostic-completeness"].artifacts:
        destination = root / path
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_bytes((REPO_ROOT / path).read_bytes())
    helper = root / "abc/src/abc/tools/new_semantic_helper.clj"
    helper.write_text("(ns abc.tools.new-semantic-helper)\n", encoding="utf-8")
    entry = root / "abc/src/abc/tools/parser_rq_diagnostic_completeness.clj"
    entry.write_text(
        entry.read_text(encoding="utf-8").replace(
            "(:require", "(:require [abc.tools.new-semantic-helper]\n            ", 1
        ),
        encoding="utf-8",
    )
    with pytest.raises(ValueError, match="reviewed semantic closure differs"):
        module.build_manifest(root, "diagnostic-completeness")

    module.INSTRUMENTS["diagnostic-completeness"].reviewed_sources = tuple(
        path
        for path in module.INSTRUMENTS["diagnostic-completeness"].reviewed_sources
        if path.name != "parser_rq_capture.clj"
    )
    with pytest.raises(ValueError, match="reviewed semantic closure differs"):
        module.build_manifest(REPO_ROOT, "diagnostic-completeness")


def test_schema_mutation_changes_manifest_and_policy_identity(module, tmp_path):
    root = tmp_path / "repo"
    config = module.INSTRUMENTS["parser-ir-conformance"]
    for path in (*config.reviewed_sources, *config.artifacts):
        destination = root / path
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_bytes((REPO_ROOT / path).read_bytes())
    before_manifest = module.build_manifest(root, "parser-ir-conformance")
    before_policy = module.build_policy(root, "parser-ir-conformance", before_manifest, ["fixture"])
    schema_path = root / "abc/schemas/parser-ir.schema.json"
    schema = json.loads(schema_path.read_text(encoding="utf-8"))
    schema["$comment"] = "semantic mutation"
    schema_path.write_text(json.dumps(schema), encoding="utf-8")
    after_manifest = module.build_manifest(root, "parser-ir-conformance")
    after_policy = module.build_policy(root, "parser-ir-conformance", after_manifest, ["fixture"])
    assert before_manifest["validator_semantics_hash"] != after_manifest["validator_semantics_hash"]
    assert before_policy["policy_hash"] != after_policy["policy_hash"]


def test_policies_have_distinct_closed_identities(module):
    diagnostic_manifest = module.build_manifest(REPO_ROOT, "diagnostic-completeness")
    parser_manifest = module.build_manifest(REPO_ROOT, "parser-ir-conformance")
    diagnostic = module.build_policy(
        REPO_ROOT, "diagnostic-completeness", diagnostic_manifest, ["work-a"]
    )
    parser_ir = module.build_policy(
        REPO_ROOT,
        "parser-ir-conformance",
        parser_manifest,
        ["valid", "invalid", "no-output"],
    )
    assert diagnostic["policy_id"] != parser_ir["policy_id"]
    assert diagnostic["policy_hash"] != parser_ir["policy_hash"]
    assert diagnostic["policy_hash"] == module.projected_hash(diagnostic, "policy_hash")
    assert parser_ir["policy_hash"] == module.projected_hash(parser_ir, "policy_hash")
