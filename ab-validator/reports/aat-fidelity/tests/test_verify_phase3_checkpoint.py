import json
import subprocess
import sys
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[1] / "verify-phase3-checkpoint.py"
C0, C1, C2 = "a" * 40, "b" * 40, "c" * 40
V0 = f"ab-aozora 0.1.0 aat-schema 1 facade 0.1.0 wire-schema 2 (git {C0})"
V1 = f"ab-aozora 0.2.0 aat-schema 1 facade 0.2.0 wire-schema 3 (git {C1})"
V2 = f"ab-aozora 0.3.0 aat-schema 1 facade 0.2.0 wire-schema 3 (git {C2})"


MAPPING_HASH = "sha256:952620ced4eb22f9771e6a10c3a1d4d93de604a8c33e360311f82b6e1eafc5b7"


def summary(stage, gate, commit, version, bin_sha="f" * 64, verdict="PASS", details=None):
    return {"stage": stage, "gate": gate, "verdict": verdict,
            "candidate": {"commit": commit, "bin_sha256": bin_sha, "version": version},
            "details": details or {}}


def audit(version):
    return {"mapping": {"mapping_version": "0.2.8", "mapping_hash": MAPPING_HASH},
            "totals": {"files_attempted": 17886, "files_succeeded": 17886,
                       "files_failed": 0},
            "compatibility_candidates": [{"aat_adapter_version": version}]}


def write_all(tmp_path, mutate=None):
    docs = {
        "s0p": summary("stage0", "parity", C0, V0, bin_sha="0" * 64,
                       details={"compared": 17886, "missing_count": 0,
                                "bytes": {"diverged_count": 0}}),
        "s0c": summary("stage0", "conformance", C0, V0, bin_sha="0" * 64),
        "s0f": summary("stage0", "perf", C0, V0, bin_sha="0" * 64,
                       details={"new_timeouts": 0}),
        "rap": summary("rotation-a", "delta", C1, V1, bin_sha="1" * 64,
                       details={"mode": "container-rewrite", "compared": 17886,
                                "classes": {"identical": 17700, "rewritten": 186,
                                            "span_confined": 0}, "verdict": "PASS"}),
        "rac": summary("rotation-a", "conformance", C1, V1, bin_sha="1" * 64,
                       details={"must_fail": 0, "must_skip": 0}),
        "raf": summary("rotation-a", "perf", C1, V1, bin_sha="1" * 64,
                       details={"new_timeouts": 0}),
        "rbp": summary("rotation-b", "confinement", C2, V2, bin_sha="2" * 64,
                       details={"mode": "span-confinement", "compared": 17886,
                                "classes": {"identical": 0, "rewritten": 0,
                                            "span_confined": 17886}, "verdict": "PASS"}),
        "rbc": summary("rotation-b", "conformance", C2, V2, bin_sha="2" * 64,
                       details={"must_fail": 0, "must_skip": 0}),
        "rbf": summary("rotation-b", "perf", C2, V2, bin_sha="2" * 64,
                       details={"new_timeouts": 0}),
        "auda": audit(V1),
        "audb": audit(V2),
    }
    if mutate:
        mutate(docs)
    paths = {}
    for key, doc in docs.items():
        p = tmp_path / f"{key}.json"
        p.write_text(json.dumps(doc))
        paths[key] = str(p)
    return paths


def run(paths, c2=C2):
    return subprocess.run(
        [sys.executable, str(SCRIPT),
         "--stage0", paths["s0p"], paths["s0c"], paths["s0f"],
         "--rotation-a", paths["rap"], paths["rac"], paths["raf"],
         "--rotation-b", paths["rbp"], paths["rbc"], paths["rbf"],
         "--audit-a", paths["auda"], "--audit-b", paths["audb"],
         "--c0", C0, "--c1", C1, "--c2", c2],
        capture_output=True, text=True)


def test_all_pass(tmp_path):
    p = run(write_all(tmp_path))
    assert p.returncode == 0 and "CHECKPOINT OK" in p.stdout, p.stderr


def test_verdict_fail_rejected(tmp_path):
    def mutate(d): d["rap"]["verdict"] = "FAIL"
    assert run(write_all(tmp_path, mutate)).returncode == 1


def test_commit_mismatch_within_stage_rejected(tmp_path):
    def mutate(d): d["rac"]["candidate"]["commit"] = C2
    assert run(write_all(tmp_path, mutate)).returncode == 1


def test_bin_mismatch_within_stage_rejected(tmp_path):
    def mutate(d): d["raf"]["candidate"]["bin_sha256"] = "9" * 64
    assert run(write_all(tmp_path, mutate)).returncode == 1


def test_wrong_version_pattern_rejected(tmp_path):
    def mutate(d): d["rbp"]["candidate"]["version"] = V1.replace(C1, C2)
    assert run(write_all(tmp_path, mutate)).returncode == 1


def test_duplicate_candidates_across_stages_rejected(tmp_path):
    def mutate(d):
        for k in ("rbp", "rbc", "rbf"):
            d[k]["candidate"]["commit"] = C1
            d[k]["candidate"]["version"] = V2.replace(C2, C1)
    assert run(write_all(tmp_path, mutate), c2=C1).returncode == 1


def test_delta_class_totals_must_sum_to_compared(tmp_path):
    def mutate(d): d["rap"]["details"]["classes"]["identical"] = 17000
    assert run(write_all(tmp_path, mutate)).returncode == 1


def test_confinement_wrong_mode_rejected(tmp_path):
    def mutate(d): d["rbp"]["details"]["mode"] = "container-rewrite"
    assert run(write_all(tmp_path, mutate)).returncode == 1


def test_audit_failed_files_rejected(tmp_path):
    def mutate(d): d["audb"]["totals"]["files_failed"] = 1
    assert run(write_all(tmp_path, mutate)).returncode == 1


def test_audit_wrong_mapping_rejected(tmp_path):
    def mutate(d): d["auda"]["mapping"]["mapping_version"] = "0.2.4"
    assert run(write_all(tmp_path, mutate)).returncode == 1


def test_audit_identity_mismatch_rejected(tmp_path):
    def mutate(d): d["auda"]["compatibility_candidates"][0]["aat_adapter_version"] = V2
    assert run(write_all(tmp_path, mutate)).returncode == 1
