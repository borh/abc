from __future__ import annotations

import hashlib
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

# The manifests of record. Nothing asserted these matched their generator
# either, and they carry the disclosure the policy hash only summarizes.
COMMITTED_MANIFESTS = {
    "diagnostic-completeness": (
        "ab-validator/data/parser-rq-diagnostic-completeness-validator-v1.json"
    ),
    "parser-ir-conformance": (
        "ab-validator/data/parser-rq-parser-ir-conformance-validator-v1.json"
    ),
}


@pytest.fixture
def module():
    spec = importlib.util.spec_from_file_location("predicate_hardening_identity", SCRIPT)
    assert spec and spec.loader
    loaded = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(loaded)
    return loaded


@pytest.mark.parametrize("instrument", sorted(COMMITTED_POLICIES))
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
    for path in (*config.reviewed_sources, *config.artifacts, module.CARGO_LOCK):
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
        REPO_ROOT,
        "diagnostic-completeness",
        diagnostic_manifest,
        ["work-a"],
        {"work-a": []},
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


@pytest.mark.parametrize("instrument", ["diagnostic-completeness"])
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
        REPO_ROOT,
        instrument,
        manifest,
        list(committed["expected_work_ids"]),
        committed.get("expected_diagnostics"),
    )
    assert generated == committed


@pytest.mark.parametrize("instrument", ["diagnostic-completeness"])
def test_committed_manifest_equals_its_regenerated_form(module, instrument):
    committed = json.loads(
        (REPO_ROOT / COMMITTED_MANIFESTS[instrument]).read_text(encoding="utf-8")
    )
    assert module.build_manifest(REPO_ROOT, instrument) == committed


def test_historical_parser_policy_reproduces_from_retained_evidence(module, tmp_path):
    instrument = "parser-ir-conformance"
    manifest = json.loads((REPO_ROOT / COMMITTED_MANIFESTS[instrument]).read_text())
    policy = json.loads((REPO_ROOT / COMMITTED_POLICIES[instrument]).read_text())
    schema = REPO_ROOT / (
        "abc/test/fixtures/parser-rq/predicate-hardening-capture/parser-ir-0.7.0.schema.json"
    )
    assert manifest["validator_semantics_hash"] == module.projected_hash(
        manifest, "validator_semantics_hash"
    )
    assert manifest["artifacts"] == [
        {
            "path": "abc/schemas/parser-ir.schema.json",
            "sha256": "sha256:" + hashlib.sha256(schema.read_bytes()).hexdigest(),
        }
    ]
    retained_root = tmp_path / "retained"
    destination = retained_root / "abc/schemas/parser-ir.schema.json"
    destination.parent.mkdir(parents=True)
    shutil.copyfile(schema, destination)
    assert (
        module.build_policy(retained_root, instrument, manifest, policy["expected_work_ids"])
        == policy
    )

    # The recorded manifest describes the historical converter. Current source
    # bytes authenticate a new instrument; they cannot reproduce that capture.
    current = module.build_manifest(REPO_ROOT, instrument)
    assert current["validator_semantics_hash"] == module.projected_hash(
        current, "validator_semantics_hash"
    )
    for entry in (*current["sources"], *current["artifacts"]):
        assert (
            entry["sha256"]
            == "sha256:" + hashlib.sha256((REPO_ROOT / entry["path"]).read_bytes()).hexdigest()
        )
    current_policy = module.build_policy(
        REPO_ROOT, instrument, current, policy["expected_work_ids"]
    )
    assert current_policy["validator_semantics_hash"] == current["validator_semantics_hash"]
    assert current_policy["policy_hash"] == module.projected_hash(current_policy, "policy_hash")
    if current["sources"] != manifest["sources"] or current["artifacts"] != manifest["artifacts"]:
        assert current["validator_semantics_hash"] != manifest["validator_semantics_hash"]
        assert current_policy["policy_hash"] != policy["policy_hash"]


def _write_lock(path: pathlib.Path, packages: list[dict[str, object]]) -> None:
    body = []
    for package in packages:
        entry = [
            "[[package]]",
            f'name = "{package["name"]}"',
            f'version = "{package["version"]}"',
        ]
        if "checksum" in package:
            entry.append(f'checksum = "{package["checksum"]}"')
        if package.get("dependencies"):
            listed = ",\n".join(f' "{name}"' for name in package["dependencies"])
            entry.append(f"dependencies = [\n{listed},\n]")
        body.append("\n".join(entry))
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("\n\n".join(body) + "\n", encoding="utf-8")


BASE_LOCK = [
    {"name": "subject", "version": "0.1.0", "dependencies": ["leaf", "jsonschema"]},
    {"name": "leaf", "version": "1.2.3", "checksum": "aa"},
    {"name": "jsonschema", "version": "0.46.9", "checksum": "bb"},
]


DUPLICATE_LOCK = [
    {"name": "subject", "version": "0.1.0", "dependencies": ["dup 1.0.0", "mid", "jsonschema"]},
    {"name": "mid", "version": "0.1.0", "dependencies": ["dup 2.0.0"]},
    {"name": "dup", "version": "1.0.0", "checksum": "11"},
    {"name": "dup", "version": "2.0.0", "checksum": "22"},
    {"name": "jsonschema", "version": "0.46.9", "checksum": "bb"},
]


def test_projection_keeps_co_resolved_versions_of_one_name_distinct(module, tmp_path):
    """Thirty-six names in this workspace resolve to several versions at once.

    Keying the closure by name alone would let one version's entry stand in for
    another's, so the projection would carry an arbitrary version and a bump of
    the one actually compiled could leave it unmoved. Against the real lockfile
    that mistake dropped 53 packages from the closure.
    """
    lock = tmp_path / "ab-validator/Cargo.lock"
    _write_lock(lock, DUPLICATE_LOCK)
    projection = module.locked_dependency_projection(tmp_path, "subject")
    assert [entry["version"] for entry in projection if entry["name"] == "dup"] == [
        "1.0.0",
        "2.0.0",
    ]
    bumped = [
        {**package, "checksum": "33"}
        if (package["name"], package["version"]) == ("dup", "2.0.0")
        else package
        for package in DUPLICATE_LOCK
    ]
    _write_lock(lock, bumped)
    assert module.locked_dependency_projection(tmp_path, "subject") != projection


def test_ambiguous_bare_dependency_fails_closed(module, tmp_path):
    """Cargo omits the version only when the name is unambiguous.

    A bare name with several candidates means this reader has misunderstood the
    lockfile, and guessing a version would forge part of the identity.
    """
    lock = tmp_path / "ab-validator/Cargo.lock"
    _write_lock(lock, [{**DUPLICATE_LOCK[0], "dependencies": ["dup"]}, *DUPLICATE_LOCK[1:]])
    with pytest.raises(ValueError, match="does not select one locked package"):
        module.locked_dependency_projection(tmp_path, "subject")


def test_projection_ignores_workspace_packages_the_subject_does_not_use(module, tmp_path):
    """An unrelated sibling package must not rotate this instrument's identity.

    This is the observed regression, not a hypothetical: adding
    `ab-aozora-capture` to the workspace moved the whole-lockfile hash while
    `ab-aat-to-parser-ir`'s own lock entry stayed byte-identical and no resolved
    third-party version changed. Binding the lockfile wholesale made every
    workspace edit look like a semantic change to this instrument.
    """
    lock = tmp_path / "ab-validator/Cargo.lock"
    _write_lock(lock, BASE_LOCK)
    before = module.locked_dependency_projection(tmp_path, "subject")
    _write_lock(
        lock,
        [*BASE_LOCK, {"name": "unrelated-sibling", "version": "0.6.0", "dependencies": ["leaf"]}],
    )
    assert module.locked_dependency_projection(tmp_path, "subject") == before


def test_projection_rotates_when_a_transitive_dependency_moves(module, tmp_path):
    lock = tmp_path / "ab-validator/Cargo.lock"
    _write_lock(lock, BASE_LOCK)
    before = module.locked_dependency_projection(tmp_path, "subject")
    bumped = [
        {**package, "version": "1.2.4", "checksum": "cc"} if package["name"] == "leaf" else package
        for package in BASE_LOCK
    ]
    _write_lock(lock, bumped)
    assert module.locked_dependency_projection(tmp_path, "subject") != before


def test_projection_rotates_when_the_subject_gains_a_dependency(module, tmp_path):
    lock = tmp_path / "ab-validator/Cargo.lock"
    _write_lock(lock, BASE_LOCK)
    before = module.locked_dependency_projection(tmp_path, "subject")
    _write_lock(
        lock,
        [
            {**BASE_LOCK[0], "dependencies": ["leaf", "jsonschema", "added"]},
            *BASE_LOCK[1:],
            {"name": "added", "version": "0.1.0", "checksum": "dd"},
        ],
    )
    assert module.locked_dependency_projection(tmp_path, "subject") != before


def test_projection_marks_workspace_local_members_as_uncovered(module):
    """Local members carry no checksum, so their bytes are outside this identity.

    Asserting the marker keeps the gap legible in the manifest instead of
    letting a version-only entry read as though it bound the crate's source.
    See ADR `package-scoped-instrument-dependency-identity`.
    """
    projection = module.locked_dependency_projection(REPO_ROOT, "ab-aat-to-parser-ir")
    local = {entry["name"] for entry in projection if entry.get("origin") == "workspace-local"}
    assert "ab-aozora-aat" in local
    assert all("checksum" not in entry for entry in projection if entry["name"] in local)


def test_runtime_crate_version_is_read_from_the_lock_not_restated(module, tmp_path):
    """A hardcoded `jsonschema_crate` could assert a version nothing compiled."""
    lock = tmp_path / "ab-validator/Cargo.lock"
    _write_lock(lock, BASE_LOCK)
    projection = module.locked_dependency_projection(tmp_path, "subject")
    assert module.locked_version(projection, "jsonschema") == "0.46.9"
    with pytest.raises(ValueError, match="absent .* or ambiguous"):
        module.locked_version(projection, "not-a-dependency")
    # A name resolved at two versions cannot be reported as one, either.
    _write_lock(lock, DUPLICATE_LOCK)
    with pytest.raises(ValueError, match="absent .* or ambiguous"):
        module.locked_version(module.locked_dependency_projection(tmp_path, "subject"), "dup")


def test_a_declared_dependency_missing_from_the_lock_fails_closed(module, tmp_path):
    """Skipping an unresolvable entry would drop a package with nothing said."""
    lock = tmp_path / "ab-validator/Cargo.lock"
    _write_lock(lock, [package for package in BASE_LOCK if package["name"] != "jsonschema"])
    with pytest.raises(ValueError, match="does not select one locked package"):
        module.locked_dependency_projection(tmp_path, "subject")
    # Same for a dependency that names a version no entry carries.
    _write_lock(lock, [{**BASE_LOCK[0], "dependencies": ["leaf 9.9.9"]}, *BASE_LOCK[1:]])
    with pytest.raises(ValueError, match="does not select one locked package"):
        module.locked_dependency_projection(tmp_path, "subject")


def test_a_work_without_a_governed_expectation_cannot_enter_the_policy(module):
    """Membership and expectation are one claim, not two that may disagree.

    The diagnostic-completeness instrument divides by the expected workset, so
    a work present in one list and absent from the other would either be
    measured against nothing or counted in a denominator it has no term in.
    """
    manifest = module.build_manifest(REPO_ROOT, "diagnostic-completeness")
    for expectation in (None, {}, {"work-a": []}, {"work-a": [], "work-c": []}):
        with pytest.raises(ValueError, match="one governed expectation per expected work"):
            module.build_policy(
                REPO_ROOT,
                "diagnostic-completeness",
                manifest,
                ["work-a", "work-b"],
                expectation,
            )


def test_expectation_codes_are_sorted_into_the_policy_hash(module):
    """Two callers naming the same codes in different orders must agree.

    The corpus is the authority and its order is incidental; leaving it in
    would make `policy_hash` depend on how the expectation was typed.
    """
    manifest = module.build_manifest(REPO_ROOT, "diagnostic-completeness")
    ordered = module.build_policy(
        REPO_ROOT,
        "diagnostic-completeness",
        manifest,
        ["work-a"],
        {"work-a": ["nested-ruby", "unclosed-bracket"]},
    )
    reversed_codes = module.build_policy(
        REPO_ROOT,
        "diagnostic-completeness",
        manifest,
        ["work-a"],
        {"work-a": ["unclosed-bracket", "nested-ruby"]},
    )
    assert ordered == reversed_codes


def test_work_diagnostics_arguments_parse_into_one_expectation(module):
    assert module.parse_work_diagnostics(["a=", "b=nested-ruby,unclosed-bracket"]) == {
        "a": [],
        "b": ["nested-ruby", "unclosed-bracket"],
    }
    for malformed in (["a"], ["=x"]):
        with pytest.raises(ValueError, match="is not WORK_ID=CODE,CODE"):
            module.parse_work_diagnostics(malformed)
    with pytest.raises(ValueError, match="more than once"):
        module.parse_work_diagnostics(["a=", "a=nested-ruby"])
