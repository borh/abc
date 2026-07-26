from __future__ import annotations

import importlib.util
import json
import pathlib
import shutil

import pytest


REPO_ROOT = pathlib.Path(__file__).resolve().parents[4]
SCRIPT = REPO_ROOT / "ab-validator/reports/parser-ir/predicate-hardening-identity.py"

# The instrument policies this generator authors, and the membership they pin.
# The equality test below reads work ids from the committed policy on purpose:
# it isolates the semantic-identity question from corpus membership, which is a
# separate authority (see finding D of the audit named below).
COMMITTED_POLICIES = {
    "diagnostic-completeness": "abc/data/parser-rq-diagnostic-completeness-policy-v1.json",
    "parser-ir-conformance": "abc/data/parser-rq-parser-ir-conformance-policy-v1.json",
}

# Step 1a of
# abc/docs/superpowers/reports/2026-07-26-parser-rq-instrument-semantics-audit.md.
# Until that repair, every test in this file errored at import because the Nix
# check staged only the ab-validator subtree, so two committed faults went
# unobserved. They are recorded here as strict xfails rather than left red:
# step 1c regenerates the affected identities and removes the markers, and
# `strict=True` turns a forgotten marker into a failure once the fault is gone.
STALE_CLOSURE = pytest.mark.xfail(
    strict=True,
    reason=(
        "A1: the diagnostic-completeness reviewed closure names "
        "abc/src/abc/tools/evidence_io.clj, deleted in b042c7cd, plus three "
        "sources that are no longer required. The generator refuses to run, so "
        "the committed policy's validator_semantics_hash covers bytes that no "
        "longer exist."
    ),
)

COMMITTED_DRIFT = pytest.mark.xfail(
    strict=True,
    reason=(
        "A2: the committed parser-IR policy no longer equals its regenerated "
        "form. No reviewed source changed since 2026-07-17; the manifest binds "
        "ab-validator/Cargo.lock wholesale, which moved for an unrelated "
        "workspace package. Scope is decided in step 1b before regeneration."
    ),
)


@pytest.fixture
def module():
    spec = importlib.util.spec_from_file_location("predicate_hardening_identity", SCRIPT)
    assert spec and spec.loader
    loaded = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(loaded)
    return loaded


@pytest.mark.parametrize(
    "instrument",
    [
        pytest.param("diagnostic-completeness", marks=STALE_CLOSURE),
        "parser-ir-conformance",
    ],
)
def test_reviewed_sources_equal_discovered_owned_closure(module, instrument):
    assert module.discover_owned_sources(REPO_ROOT, instrument) == {
        REPO_ROOT / path for path in module.INSTRUMENTS[instrument].reviewed_sources
    }


@STALE_CLOSURE
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


@STALE_CLOSURE
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


@COMMITTED_DRIFT
@pytest.mark.parametrize("instrument", sorted(COMMITTED_POLICIES))
def test_committed_policy_equals_its_regenerated_form(module, instrument):
    """The governed policy must be exactly what this generator produces.

    Nothing asserted this before. The other tests here check closure discovery,
    mutation sensitivity, and the self-consistency of *generated* values, so a
    committed policy could drift from the implementation it claims to bind with
    no test disagreeing. This is the check that makes regeneration load-bearing.
    """
    committed = json.loads((REPO_ROOT / COMMITTED_POLICIES[instrument]).read_text(encoding="utf-8"))
    manifest = module.build_manifest(REPO_ROOT, instrument)
    generated = module.build_policy(
        REPO_ROOT, instrument, manifest, list(committed["expected_work_ids"])
    )
    assert generated == committed
