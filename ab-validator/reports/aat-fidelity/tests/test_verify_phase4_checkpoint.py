import json
import shlex
import subprocess
import sys
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[1] / "verify-phase4-checkpoint.py"
C3 = "3" * 40
C4 = "4" * 40
V3 = f"ab-aozora 0.4.0 aat-schema 2 facade 0.3.0 wire-schema 3 (git {C3})"
V4 = f"ab-aozora 0.5.0 aat-schema 2 facade 0.3.0 wire-schema 3 (git {C4})"

MAPPING_HASH = "sha256:7249cd727ef2da90dcd591e6009bead9235fe1c140697ee6bd70aacd9e85ee40"


def gate_summary(stage, gate, commit, version, bin_sha, details):
    return {
        "stage": stage,
        "gate": gate,
        "verdict": "PASS",
        "candidate": {"commit": commit, "bin_sha256": bin_sha, "version": version},
        "details": details,
    }


def audit_doc(version):
    return {
        "mapping": {"mapping_version": "0.3.0", "mapping_hash": MAPPING_HASH},
        "totals": {"files_attempted": 17886, "files_succeeded": 17886, "files_failed": 0},
        "compatibility_candidates": [{"aat_adapter_version": version}],
    }


def write_artifacts(tmp_path, mutate=None):
    docs = {
        "c3_migration": gate_summary(
            "c3",
            "migration",
            C3,
            V3,
            "1" * 64,
            {
                "mode": "v2-migration",
                "compared": 17886,
                "classes": {"migrated": 17864, "jizume_rewritten": 4, "ruby_left_rewritten": 18},
            },
        ),
        "c3_conformance": gate_summary(
            "c3",
            "conformance",
            C3,
            V3,
            "1" * 64,
            {"must_fail": 0, "must_skip": 0, "differing_full": 0, "differing_seed": 0},
        ),
        "c3_perf": gate_summary("c3", "perf", C3, V3, "1" * 64, {"new_timeouts": 0}),
        "c4_confinement": gate_summary(
            "c4",
            "confinement",
            C4,
            V4,
            "2" * 64,
            {
                "mode": "source-note-append",
                "compared": 17886,
                "classes": {"identical": 151, "source_note_appended": 17735},
                "split_works_with_terminal_provenance": 17735,
            },
        ),
        "c4_conformance": gate_summary(
            "c4",
            "conformance",
            C4,
            V4,
            "2" * 64,
            {"must_fail": 0, "must_skip": 0, "differing_full": 0, "differing_seed": 0},
        ),
        "c4_perf": gate_summary(
            "c4", "perf", C4, V4, "2" * 64, {"new_timeouts": 0, "activation_floor_ok": True}
        ),
        "audit_c3": audit_doc(V3),
        "audit_c4": audit_doc(V4),
        "split": {
            "works_with_terminal_provenance": 17735,
            "works_scanned": 17886,
            "verdict": "TERMINAL_PROVENANCE_SPLIT_OK",
        },
        "coverage": {
            "source_authority_gate": {
                "gate_status": "SOURCE_AUTHORITY_GATE_PASS",
                "unallowlisted_unknown_markers_total": 0,
            },
            "parser_evidence_coverage": {"verdict": "FIVE_PARSER_EVIDENCE_COMPLETE"},
        },
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


def build_activation_repo(
    tmp_path,
    parent_has_ab_aozora=False,
    child_has_ab_aozora=True,
    extra_stray_path=False,
):
    repo = tmp_path / "repo"
    repo.mkdir()
    git(repo, "init", "-q")

    base_adapters = {
        "aozora2": adapter("aozora2", "aozora2-adapter"),
        "aozora-rs": adapter("aozora-rs", "aozora-rs-adapter"),
        "aozora2html": adapter("aozora2html", "aozora2html-adapter"),
        "aozora-epub3": adapter("aozora-epub3", "aozora-epub3-adapter"),
    }

    run_set_path = repo / "ab-validator" / "reports" / "aat-fidelity" / "run-sets" / "current.json"
    contract_path = (
        repo / "ab-validator" / "docs" / "handoffs" / "ir-publication-coverage-contract.md"
    )
    justfile_path = repo / "ab-validator" / "justfile"
    archive_path = (
        repo
        / "ab-validator"
        / "reports"
        / "aat-fidelity"
        / "run-sets"
        / "2026-07-12-aozora-legacy-archive.json"
    )

    parent_adapters = dict(base_adapters)
    parent_adapters["aozora"] = adapter("aozora", "aozora-adapter")
    if parent_has_ab_aozora:
        parent_adapters["ab-aozora"] = adapter("ab-aozora", C4)
    write(run_set_path, run_set_json(parent_adapters))
    write(contract_path, "old contract\n")
    write(justfile_path, "old justfile\n")
    git(repo, "add", "-A")
    git_commit(repo, "parent: pre-activation state")

    child_adapters = dict(base_adapters)
    if child_has_ab_aozora:
        child_adapters["ab-aozora"] = adapter("ab-aozora", C4)
    write(run_set_path, run_set_json(child_adapters))
    write(contract_path, "new contract: ab-aozora is the publication lane\n")
    write(justfile_path, "new justfile\n")
    write(archive_path, run_set_json({"aozora": adapter("aozora", "aozora-adapter")}))
    write(repo / "abc" / "examples" / "ab-validator-output" / "divergence.json", "{}\n")
    write(repo / "abc" / "test" / "abc" / "tools" / "materialize_import_test.clj", ";; ok\n")
    if extra_stray_path:
        write(repo / "ab-validator" / "reports" / "stray.json", "{}\n")
    git(repo, "add", "-A")
    git_commit(repo, "activation: ab-aozora is the publication lane")

    sha = subprocess.run(
        ["git", "rev-parse", "HEAD"], cwd=repo, check=True, capture_output=True, text=True
    ).stdout.strip()
    return sha, str(run_set_path)


def run(tmp_path, mutate=None, activation_kwargs=None, c3=C3, c4=C4):
    artifacts_dir = tmp_path / "artifacts"
    artifacts_dir.mkdir(exist_ok=True)
    paths = write_artifacts(artifacts_dir, mutate)
    sha, run_set_path = build_activation_repo(tmp_path, **(activation_kwargs or {}))
    argv = [
        sys.executable,
        str(SCRIPT),
        "--c3-gates",
        paths["c3_migration"],
        paths["c3_conformance"],
        paths["c3_perf"],
        "--c4-gates",
        paths["c4_confinement"],
        paths["c4_conformance"],
        paths["c4_perf"],
        "--audit-c3",
        paths["audit_c3"],
        "--audit-c4",
        paths["audit_c4"],
        "--split",
        paths["split"],
        "--admission-capture",
        paths["admission_capture"],
        "--admission-cmd",
        paths["admission_cmd"],
        "--activation-commit",
        sha,
        "--run-set",
        run_set_path,
        "--coverage",
        paths["coverage"],
        "--c3",
        c3,
        "--c4",
        c4,
    ]
    return subprocess.run(argv, capture_output=True, text=True)


def test_all_pass(tmp_path):
    p = run(tmp_path)
    assert p.returncode == 0 and "CHECKPOINT OK" in p.stdout, p.stderr


def test_verdict_fail(tmp_path):
    def mutate(d):
        d["c3_conformance"]["verdict"] = "FAIL"

    assert run(tmp_path, mutate).returncode == 1


def test_commit_mismatch(tmp_path):
    def mutate(d):
        d["c4_perf"]["candidate"]["commit"] = C3

    assert run(tmp_path, mutate).returncode == 1


def test_bin_mismatch(tmp_path):
    def mutate(d):
        d["c4_perf"]["candidate"]["bin_sha256"] = "9" * 64

    assert run(tmp_path, mutate).returncode == 1


def test_wrong_version_pattern(tmp_path):
    def mutate(d):
        # embeds the correct c4 commit but the c3 stage's version shape.
        d["c4_confinement"]["candidate"]["version"] = V3.replace(C3, C4)

    assert run(tmp_path, mutate).returncode == 1


def test_duplicate_candidates(tmp_path):
    assert run(tmp_path, c4=C3).returncode == 1


def test_migration_class_sum(tmp_path):
    def mutate(d):
        d["c3_migration"]["details"]["classes"]["migrated"] = 17000

    assert run(tmp_path, mutate).returncode == 1


def test_confinement_wrong_mode(tmp_path):
    def mutate(d):
        d["c4_confinement"]["details"]["mode"] = "container-rewrite"

    assert run(tmp_path, mutate).returncode == 1


def test_confinement_append_count_mismatch(tmp_path):
    def mutate(d):
        d["split"]["works_with_terminal_provenance"] = 17000

    assert run(tmp_path, mutate).returncode == 1


def test_audit_failed_files(tmp_path):
    def mutate(d):
        d["audit_c4"]["totals"]["files_failed"] = 1

    assert run(tmp_path, mutate).returncode == 1


def test_audit_wrong_mapping(tmp_path):
    def mutate(d):
        d["audit_c3"]["mapping"]["mapping_version"] = "0.2.8"

    assert run(tmp_path, mutate).returncode == 1


def test_admission_rerun_fails(tmp_path):
    def mutate(d):
        d["admission_script_text"] = "import sys\nsys.exit(1)\n"

    assert run(tmp_path, mutate).returncode == 1


def test_activation_commit_extra_path(tmp_path):
    p = run(tmp_path, activation_kwargs={"extra_stray_path": True})
    assert p.returncode == 1


def test_activation_parent_already_active(tmp_path):
    p = run(tmp_path, activation_kwargs={"parent_has_ab_aozora": True})
    assert p.returncode == 1


def test_activation_child_missing_lane(tmp_path):
    p = run(tmp_path, activation_kwargs={"child_has_ab_aozora": False})
    assert p.returncode == 1


def test_acceptance_counter_nonzero(tmp_path):
    def mutate(d):
        d["coverage"]["source_authority_gate"]["unallowlisted_unknown_markers_total"] = 1

    assert run(tmp_path, mutate).returncode == 1
