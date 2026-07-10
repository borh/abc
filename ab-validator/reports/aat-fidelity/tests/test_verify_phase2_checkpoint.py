import json
import pathlib
import subprocess
import sys

SCRIPT = pathlib.Path(__file__).resolve().parents[1] / "verify-phase2-checkpoint.py"
COMMIT = "9cbb7b7b22a3c1779b99300a3fbe3e8d0ec12b33"
BIN_SHA_PARITY = "0f699aa57d8420f76269ccd0c302d9a7578d02246876ad18443b2847803851e0"
BIN_SHA_ECHO = "4a1504c8b01a6d5f021b907e023426810a4fff610c4fa051096331e6d822aae3"
VERSION = f"ab-aozora 0.1.0 aat-schema 1 facade 0.1.0 wire-schema 2 (git {COMMIT})"


def _summaries():
    """Build three valid gate summary docs for a fixed candidate commit."""
    parity = {
        "gate": "absorption-parity",
        "candidate": {
            "commit": COMMIT,
            "bin_sha256": BIN_SHA_PARITY,
            "version": VERSION,
        },
        "verdict": "PASS",
        "details": {
            "compared": 17886,
            "missing_count": 0,
            "bytes_diverged_count": 0,
        },
    }
    perf = {
        "gate": "perf",
        "candidate": {
            "commit": COMMIT,
            "bin_sha256": BIN_SHA_PARITY,
            "version": VERSION,
        },
        "verdict": "PASS",
        "details": {
            "new_timeouts": False,
        },
    }
    echo = {
        "gate": "conformance-echo",
        "candidate": {
            "commit": COMMIT,
            "bin_sha256": BIN_SHA_ECHO,
            "version": VERSION,
        },
        "verdict": "PASS",
        "details": {
            "vectors_compared": 157,
            "differing_count": 0,
        },
    }
    return {"parity": parity, "perf": perf, "echo": echo}


def _write(tmp_path, docs):
    paths = {}
    for name, doc in docs.items():
        path = tmp_path / f"{name}.json"
        path.write_text(json.dumps(doc))
        paths[name] = path
    return paths


def _run(paths, candidate_commit=COMMIT):
    proc = subprocess.run(
        [
            sys.executable,
            str(SCRIPT),
            str(paths["parity"]),
            str(paths["perf"]),
            str(paths["echo"]),
            "--candidate-commit",
            candidate_commit,
        ],
        capture_output=True,
        text=True,
    )
    return proc


def test_all_valid_exits_zero(tmp_path):
    docs = _summaries()
    paths = _write(tmp_path, docs)
    proc = _run(paths)
    assert proc.returncode == 0
    assert "CHECKPOINT OK" in proc.stdout


def test_wrong_verdict_fails(tmp_path):
    docs = _summaries()
    docs["perf"]["verdict"] = "FAIL"
    paths = _write(tmp_path, docs)
    proc = _run(paths)
    assert proc.returncode == 1
    assert "CHECKPOINT FAIL" in proc.stderr
    assert "verdict is not PASS" in proc.stderr


def test_commit_mismatch_fails(tmp_path):
    docs = _summaries()
    docs["echo"]["candidate"]["commit"] = "a" * 40
    paths = _write(tmp_path, docs)
    proc = _run(paths)
    assert proc.returncode == 1
    assert "CHECKPOINT FAIL" in proc.stderr
    assert "candidate commits disagree/mismatch" in proc.stderr


def test_version_not_embedding_commit_fails(tmp_path):
    docs = _summaries()
    other_version = "ab-aozora 0.1.0 aat-schema 1 facade 0.1.0 wire-schema 2 (git " + "b" * 40 + ")"
    for name in ("parity", "perf", "echo"):
        docs[name]["candidate"]["version"] = other_version
    paths = _write(tmp_path, docs)
    proc = _run(paths)
    assert proc.returncode == 1
    assert "CHECKPOINT FAIL" in proc.stderr
    assert "does not embed the candidate commit" in proc.stderr


def test_parity_perf_bin_sha256_disagreement_fails(tmp_path):
    docs = _summaries()
    docs["perf"]["candidate"]["bin_sha256"] = "f" * 64
    paths = _write(tmp_path, docs)
    proc = _run(paths)
    assert proc.returncode == 1
    assert "CHECKPOINT FAIL" in proc.stderr
    assert "parity and perf attest different binaries" in proc.stderr


def test_nonzero_bytes_diverged_count_fails(tmp_path):
    docs = _summaries()
    docs["parity"]["details"]["bytes_diverged_count"] = 3
    paths = _write(tmp_path, docs)
    proc = _run(paths)
    assert proc.returncode == 1
    assert "CHECKPOINT FAIL" in proc.stderr
    assert "bytes_diverged_count != 0" in proc.stderr


def test_nonzero_differing_count_fails(tmp_path):
    docs = _summaries()
    docs["echo"]["details"]["differing_count"] = 2
    paths = _write(tmp_path, docs)
    proc = _run(paths)
    assert proc.returncode == 1
    assert "CHECKPOINT FAIL" in proc.stderr
    assert "differing_count != 0" in proc.stderr


def test_missing_field_fails(tmp_path):
    docs = _summaries()
    del docs["perf"]["details"]["new_timeouts"]
    paths = _write(tmp_path, docs)
    proc = _run(paths)
    assert proc.returncode == 1
    assert "CHECKPOINT FAIL" in proc.stderr
    assert "missing field details.new_timeouts" in proc.stderr


def test_malformed_candidate_commit_fails(tmp_path):
    docs = _summaries()
    paths = _write(tmp_path, docs)
    proc = _run(paths, candidate_commit="not-a-sha")
    assert proc.returncode == 1
    assert "CHECKPOINT FAIL" in proc.stderr
    assert "--candidate-commit must be a full 40-hex sha" in proc.stderr
