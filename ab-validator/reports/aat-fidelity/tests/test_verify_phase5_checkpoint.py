import json
import shlex
import subprocess
import sys
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[1] / "verify-phase5-checkpoint.py"
AB_VALIDATOR_ROOT = Path(__file__).resolve().parents[3]

C5 = "5" * 40
V5 = f"ab-aozora 0.6.0 aat-schema 2 facade 0.3.0 wire-schema 3 (git {C5})"

C4_COMMIT = "27772b1b75c9ceeb0b724095bbbb47f774f3a275"

REAL_MAPPING_FILE = AB_VALIDATOR_ROOT / "data" / "aat-to-parser-ir-mapping-v2.json"
REAL_FROZEN_MAPPING = AB_VALIDATOR_ROOT / "data" / "aat-to-parser-ir-mapping-v2-0.3.0.json"
REAL_CONVERSION_FIXTURE = (
    AB_VALIDATOR_ROOT
    / "docs"
    / "superpowers"
    / "reports"
    / "2026-07-12-ab-aozora-phase4-c4-conversion-audit.summary.json"
)

MAPPING_VERSION = "0.4.0"
MAPPING_HASH = "sha256:cf177bee98af086fe728cbc1942e4f631b26ed5bb55aedc7d91b21f467c41f30"
FROZEN_MAPPING_HASH = "sha256:7249cd727ef2da90dcd591e6009bead9235fe1c140697ee6bd70aacd9e85ee40"


def gate_summary(gate, commit, version, bin_sha):
    return {
        "stage": "c5",
        "gate": gate,
        "verdict": "PASS",
        "candidate": {"commit": commit, "bin_sha256": bin_sha, "version": version},
    }


def audit_doc():
    return {
        "mode": "bare-toggle-adoption",
        "compared": 17886,
        "classes": {"identical": 16304, "toggle_adopted": 1582},
        "details": {
            "adopted_yokogumi_pairs": 1582,
            "adopted_keigakomi_pairs": 25,
            "declined_markers": 24,
            "declined_by_reason": {
                "orphan_open": 10,
                "orphan_close": 0,
                "reopen_rollback": 14,
                "interleave": 0,
            },
        },
        "verdict": "PASS",
    }


def conversion_doc():
    doc = json.loads(REAL_CONVERSION_FIXTURE.read_text())
    doc["mapping"] = dict(
        doc["mapping"], mapping_version=MAPPING_VERSION, mapping_hash=MAPPING_HASH
    )
    return doc


def coverage_doc():
    return {
        "verdict": "IR_PUBLICATION_COVERAGE_COMPLETE",
        "source_authority_gate": {
            "gate_status": "SOURCE_AUTHORITY_GATE_PASS",
            "works_scanned": 17886,
            "unallowlisted_unknown_markers_total": 0,
        },
        "source_region_contract": {
            "verdict": "SOURCE_REGION_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION"
        },
        "custom_contract": {"verdict": "CUSTOM_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION"},
        "tei_profile_contract": {"verdict": "TEI_PROFILE_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION"},
        "parser_evidence_coverage": {"verdict": "FIVE_PARSER_EVIDENCE_COMPLETE"},
    }


def write_artifacts(tmp_path, mutate=None):
    docs = {
        "delta": gate_summary("delta", C5, V5, "5" * 64),
        "conformance": gate_summary("conformance", C5, V5, "5" * 64),
        "perf": gate_summary("perf", C5, V5, "5" * 64),
        "audit": audit_doc(),
        "conversion": conversion_doc(),
        "coverage": coverage_doc(),
        "admission_capture_text": "{:candidate-count 1, :status :admitted}",
        "admission_script_text": "print(':status :admitted')\n",
    }
    if mutate:
        mutate(docs)
    paths = {}
    for key, doc in docs.items():
        if key in ("admission_capture_text", "admission_script_text"):
            continue
        p = tmp_path / f"{key}.json"
        p.write_text(json.dumps(doc))
        paths[key] = str(p)

    admission_capture = tmp_path / "admission_capture.txt"
    admission_capture.write_text(docs["admission_capture_text"])
    paths["admission_capture"] = str(admission_capture)

    admission_script = tmp_path / "admission_cmd.py"
    admission_script.write_text(docs["admission_script_text"])
    paths["admission_cmd"] = f"{shlex.quote(sys.executable)} {shlex.quote(str(admission_script))}"
    return paths


def git(repo, *args):
    subprocess.run(["git", *args], cwd=repo, check=True, capture_output=True, text=True)


def git_commit(repo, msg):
    subprocess.run(
        ["git", "-c", "user.email=test@example.com", "-c", "user.name=Test", "commit", "-m", msg],
        cwd=repo,
        check=True,
        capture_output=True,
        text=True,
    )


def write(path, content):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content)


def run_set_json(adapters):
    return json.dumps({"schema_version": 1, "run_set_id": "test", "adapters": adapters})


def adapter(adapter_id, contains):
    return {
        "aat_dir": "x",
        "run_descriptor": "y",
        "expected": {"adapter_id": adapter_id, "adapter_version_contains": contains},
    }


def build_repoint_repo(
    tmp_path,
    child_ab_aozora_contains=C5,
    child_adapter_count_drop=False,
    extra_stray_path=False,
    parent_ab_aozora_contains=C4_COMMIT,
):
    repo = tmp_path / "repo"
    repo.mkdir()
    git(repo, "init", "-q")

    run_set_path = repo / "ab-validator" / "reports" / "aat-fidelity" / "run-sets" / "current.json"

    base_adapters = {
        "aozora2": adapter("aozora2", "aozora2-adapter"),
        "aozora-rs": adapter("aozora-rs", "aozora-rs-adapter"),
        "aozora2html": adapter("aozora2html", "aozora2html-adapter"),
        "aozora-epub3": adapter("aozora-epub3", "aozora-epub3-adapter"),
    }

    parent_adapters = dict(base_adapters)
    parent_adapters["ab-aozora"] = adapter("ab-aozora", parent_ab_aozora_contains)
    write(run_set_path, run_set_json(parent_adapters))
    # The admission re-run's cwd is <repo-root>/abc (the Clojure deps
    # context); the fixture repo needs that directory to exist even though
    # the repoint commit never touches it.
    write(repo / "abc" / ".keep", "")
    git(repo, "add", "-A")
    git_commit(repo, "parent: C4 is the publication lane")

    child_adapters = dict(base_adapters)
    if not child_adapter_count_drop:
        child_adapters["ab-aozora"] = adapter("ab-aozora", child_ab_aozora_contains)
    write(run_set_path, run_set_json(child_adapters))
    if extra_stray_path:
        write(repo / "ab-validator" / "reports" / "stray.json", "{}\n")
    git(repo, "add", "-A")
    git_commit(repo, "repoint: ab-aozora candidate is C5")

    sha = subprocess.run(
        ["git", "rev-parse", "HEAD"], cwd=repo, check=True, capture_output=True, text=True
    ).stdout.strip()
    return sha, str(run_set_path)


def run(
    tmp_path,
    mutate=None,
    repoint_kwargs=None,
    c5=C5,
    mapping_version=MAPPING_VERSION,
    mapping_hash=MAPPING_HASH,
):
    artifacts_dir = tmp_path / "artifacts"
    artifacts_dir.mkdir(exist_ok=True)
    paths = write_artifacts(artifacts_dir, mutate)
    sha, run_set_path = build_repoint_repo(tmp_path, **(repoint_kwargs or {}))
    argv = [
        sys.executable,
        str(SCRIPT),
        "--c5-gates",
        paths["delta"],
        paths["conformance"],
        paths["perf"],
        "--audit",
        paths["audit"],
        "--conversion",
        paths["conversion"],
        "--admission-capture",
        paths["admission_capture"],
        "--admission-cmd",
        paths["admission_cmd"],
        "--repoint-commit",
        sha,
        "--run-set",
        run_set_path,
        "--coverage",
        paths["coverage"],
        "--c5",
        c5,
        "--mapping-file",
        str(REAL_MAPPING_FILE),
        "--mapping-version",
        mapping_version,
        "--mapping-hash",
        mapping_hash,
        "--frozen-mapping",
        str(REAL_FROZEN_MAPPING),
    ]
    return subprocess.run(argv, capture_output=True, text=True)


def test_all_pass(tmp_path):
    p = run(tmp_path)
    assert p.returncode == 0 and "CHECKPOINT OK" in p.stdout, p.stderr


def test_stage_verdict_fail(tmp_path):
    def mutate(d):
        d["conformance"]["verdict"] = "FAIL"

    assert run(tmp_path, mutate).returncode == 1


def test_stage_commit_mismatch(tmp_path):
    def mutate(d):
        d["perf"]["candidate"]["commit"] = "6" * 40

    assert run(tmp_path, mutate).returncode == 1


def test_stage_bin_mismatch(tmp_path):
    def mutate(d):
        d["perf"]["candidate"]["bin_sha256"] = "9" * 64

    assert run(tmp_path, mutate).returncode == 1


def test_stage_wrong_version_pattern(tmp_path):
    def mutate(d):
        d["perf"]["candidate"]["version"] = (
            f"ab-aozora 0.5.0 aat-schema 2 facade 0.3.0 wire-schema 3 (git {C5})"
        )

    assert run(tmp_path, mutate).returncode == 1


def test_audit_wrong_adopted_count(tmp_path):
    def mutate(d):
        d["audit"]["details"]["adopted_yokogumi_pairs"] = 1583

    assert run(tmp_path, mutate).returncode == 1


def test_audit_declined_by_reason_mismatch(tmp_path):
    def mutate(d):
        d["audit"]["details"]["declined_by_reason"]["orphan_open"] = 11

    assert run(tmp_path, mutate).returncode == 1


def test_audit_wrong_mode(tmp_path):
    def mutate(d):
        d["audit"]["mode"] = "container-rewrite"

    assert run(tmp_path, mutate).returncode == 1


def test_audit_not_pass(tmp_path):
    def mutate(d):
        d["audit"]["verdict"] = "FAIL"

    assert run(tmp_path, mutate).returncode == 1


def test_conversion_failure_count(tmp_path):
    def mutate(d):
        d["conversion"]["totals"]["files_failed"] = 1

    assert run(tmp_path, mutate).returncode == 1


def test_conversion_wrong_mapping_hash(tmp_path):
    def mutate(d):
        d["conversion"]["mapping"]["mapping_hash"] = "sha256:" + "0" * 64

    assert run(tmp_path, mutate).returncode == 1


def test_conversion_wrong_mapping_version(tmp_path):
    def mutate(d):
        d["conversion"]["mapping"]["mapping_version"] = "0.3.0"

    assert run(tmp_path, mutate).returncode == 1


def test_mapping_generation_wrong_hash_arg(tmp_path):
    # Conversion's own mapping_hash is set to match the wrong --mapping-hash
    # arg (so check_conversion passes), isolating the failure to
    # check_mapping_generation's canonical hash of the REAL live mapping
    # file, which cannot match an arbitrary wrong arg.
    wrong_hash = "sha256:" + "1" * 64

    def mutate(d):
        d["conversion"]["mapping"]["mapping_hash"] = wrong_hash

    p = run(tmp_path, mutate, mapping_hash=wrong_hash)
    assert p.returncode == 1


def test_mapping_generation_wrong_version_arg(tmp_path):
    # Same isolation trick for mapping_version.
    wrong_version = "0.9.9"

    def mutate(d):
        d["conversion"]["mapping"]["mapping_version"] = wrong_version

    p = run(tmp_path, mutate, mapping_version=wrong_version)
    assert p.returncode == 1


def test_mapping_generation_frozen_hash_drift(tmp_path, monkeypatch):
    # Copy the frozen mapping file and mutate one field so its canonical
    # hash drifts from the pinned 0.3.0 value, then point --frozen-mapping
    # at the drifted copy directly (bypassing the `run()` helper's fixed
    # --frozen-mapping wiring).
    drifted = tmp_path / "drifted-frozen-mapping.json"
    doc = json.loads(REAL_FROZEN_MAPPING.read_text())
    doc["rules_total"] = (doc.get("rules_total") or 0) + 1
    drifted.write_text(json.dumps(doc))

    artifacts_dir = tmp_path / "artifacts"
    artifacts_dir.mkdir(exist_ok=True)
    paths = write_artifacts(artifacts_dir)
    sha, run_set_path = build_repoint_repo(tmp_path)
    argv = [
        sys.executable,
        str(SCRIPT),
        "--c5-gates",
        paths["delta"],
        paths["conformance"],
        paths["perf"],
        "--audit",
        paths["audit"],
        "--conversion",
        paths["conversion"],
        "--admission-capture",
        paths["admission_capture"],
        "--admission-cmd",
        paths["admission_cmd"],
        "--repoint-commit",
        sha,
        "--run-set",
        run_set_path,
        "--coverage",
        paths["coverage"],
        "--c5",
        C5,
        "--mapping-file",
        str(REAL_MAPPING_FILE),
        "--mapping-version",
        MAPPING_VERSION,
        "--mapping-hash",
        MAPPING_HASH,
        "--frozen-mapping",
        str(drifted),
    ]
    p = subprocess.run(argv, capture_output=True, text=True)
    assert p.returncode == 1


def test_admission_capture_missing_admitted(tmp_path):
    def mutate(d):
        d["admission_capture_text"] = "{:candidate-count 1, :status :rejected}"

    assert run(tmp_path, mutate).returncode == 1


def test_admission_rerun_silent_true(tmp_path):
    # `true` exits 0 but prints nothing -- the live re-run must still fail
    # even though the (untouched) capture file carries :status :admitted.
    artifacts_dir = tmp_path / "artifacts"
    artifacts_dir.mkdir(exist_ok=True)
    paths = write_artifacts(artifacts_dir)
    paths["admission_cmd"] = "true"
    sha, run_set_path = build_repoint_repo(tmp_path)
    argv = [
        sys.executable,
        str(SCRIPT),
        "--c5-gates",
        paths["delta"],
        paths["conformance"],
        paths["perf"],
        "--audit",
        paths["audit"],
        "--conversion",
        paths["conversion"],
        "--admission-capture",
        paths["admission_capture"],
        "--admission-cmd",
        "true",
        "--repoint-commit",
        sha,
        "--run-set",
        run_set_path,
        "--coverage",
        paths["coverage"],
        "--c5",
        C5,
        "--mapping-file",
        str(REAL_MAPPING_FILE),
        "--mapping-version",
        MAPPING_VERSION,
        "--mapping-hash",
        MAPPING_HASH,
        "--frozen-mapping",
        str(REAL_FROZEN_MAPPING),
    ]
    p = subprocess.run(argv, capture_output=True, text=True)
    assert p.returncode == 1


def test_repoint_extra_path(tmp_path):
    p = run(tmp_path, repoint_kwargs={"extra_stray_path": True})
    assert p.returncode == 1


def test_repoint_parent_missing_c4(tmp_path):
    p = run(tmp_path, repoint_kwargs={"parent_ab_aozora_contains": "some-other-commit"})
    assert p.returncode == 1


def test_repoint_child_missing_ab_aozora(tmp_path):
    p = run(tmp_path, repoint_kwargs={"child_adapter_count_drop": True})
    assert p.returncode == 1


def test_repoint_child_wrong_contains(tmp_path):
    p = run(tmp_path, repoint_kwargs={"child_ab_aozora_contains": "6" * 40})
    assert p.returncode == 1


def test_coverage_verdict_not_complete(tmp_path):
    def mutate(d):
        d["coverage"]["verdict"] = "IR_PUBLICATION_COVERAGE_BLOCKED_CUSTOM_CONTRACT_MISSING"

    assert run(tmp_path, mutate).returncode == 1


def test_coverage_custom_contract_not_confirmed(tmp_path):
    def mutate(d):
        d["coverage"]["custom_contract"]["verdict"] = "CUSTOM_CONTRACT_CANDIDATE_PROVIDED"

    assert run(tmp_path, mutate).returncode == 1


def test_coverage_source_authority_gate_fail(tmp_path):
    def mutate(d):
        d["coverage"]["source_authority_gate"]["unallowlisted_unknown_markers_total"] = 1

    assert run(tmp_path, mutate).returncode == 1


def test_coverage_evidence_not_complete(tmp_path):
    def mutate(d):
        d["coverage"]["parser_evidence_coverage"]["verdict"] = "FOUR_PARSER_EVIDENCE_COMPLETE"

    assert run(tmp_path, mutate).returncode == 1


def test_malformed_c5(tmp_path):
    p = run(tmp_path, c5="not-a-sha")
    assert p.returncode == 1
