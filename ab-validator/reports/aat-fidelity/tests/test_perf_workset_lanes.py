"""Tests for run-perf-workset.py's per-lane argv interface (Task 6).

Hermetic: no nix, no network, no git — a stub bash executable stands in for
both lanes' argv and --*-id-bin, and a tiny workset JSON + matching-sha256
corpus file replace the real pinned perf-workset.json. Runs the script as a
real subprocess (not imported as a module) so the CLI parsing and full
resolve-lane-identity/fail-closed behavior are exercised end-to-end, exactly
as Task 9's real invocation will use it.
"""

from __future__ import annotations

import hashlib
import json
import subprocess
import sys
from pathlib import Path

_SCRIPT = Path(__file__).resolve().parents[1] / "run-perf-workset.py"

_STUB = """#!/usr/bin/env bash
if [ "${1:-}" = "--version" ]; then
  echo "stub-perf-lane 1.2.3"
  exit 0
fi
cat > /dev/null
exit 0
"""


def _write_stub(path: Path) -> Path:
    path.write_text(_STUB, encoding="utf-8")
    path.chmod(0o755)
    return path


def _write_workset(tmp_path: Path) -> tuple[Path, Path]:
    corpus = tmp_path / "corpus"
    corpus.mkdir()
    source_path = corpus / "work-a.txt"
    source_path.write_bytes(b"stub source bytes\n")
    source_sha256 = hashlib.sha256(source_path.read_bytes()).hexdigest()
    workset = {
        "workset_id": "test-workset-v1",
        "protocol": {
            "warmup_runs": 1,
            "measured_runs": 1,
            "median_regression_block_threshold_pct": 1000,
            "per_work_timeout_seconds": 5,
        },
        "works": [
            {
                "work_id": "work-a",
                "corpus_relpath": "work-a.txt",
                "source_sha256": source_sha256,
            }
        ],
    }
    workset_path = tmp_path / "perf-workset.json"
    workset_path.write_text(json.dumps(workset), encoding="utf-8")
    return workset_path, corpus


def _run(args: list) -> subprocess.CompletedProcess:
    return subprocess.run(
        [sys.executable, str(_SCRIPT), *args],
        capture_output=True,
        text=True,
        timeout=30,
    )


def test_report_records_lane_identity(tmp_path: Path) -> None:
    stub = _write_stub(tmp_path / "stub-adapter")
    workset_path, corpus = _write_workset(tmp_path)
    out_path = tmp_path / "report.json"

    result = _run(
        [
            "--workset",
            str(workset_path),
            "--baseline-cmd",
            f"{stub} --mode aat",
            "--baseline-id-bin",
            str(stub),
            "--candidate-cmd",
            f"{stub} --mode aat",
            "--candidate-id-bin",
            str(stub),
            "--corpus",
            str(corpus),
            "--out",
            str(out_path),
            "--runs",
            "1",
        ]
    )
    assert result.returncode == 0, result.stdout + result.stderr

    report = json.loads(out_path.read_text())
    assert report["summary"]["verdict"] == "PASS"
    expected_sha256 = hashlib.sha256(stub.read_bytes()).hexdigest()
    for label in ("baseline", "candidate"):
        lane = report["bins"][label]
        assert lane["argv"] == [str(stub), "--mode", "aat"]
        assert lane["id_bin"] == str(stub)
        assert lane["id_bin_sha256"] == expected_sha256
        assert lane["id_bin_version"] == "stub-perf-lane 1.2.3"


def test_missing_id_bin_fails_closed(tmp_path: Path) -> None:
    stub = _write_stub(tmp_path / "stub-adapter")
    workset_path, corpus = _write_workset(tmp_path)
    out_path = tmp_path / "report.json"
    missing = tmp_path / "does-not-exist"

    result = _run(
        [
            "--workset",
            str(workset_path),
            "--baseline-cmd",
            f"{stub} --mode aat",
            "--baseline-id-bin",
            str(stub),
            "--candidate-cmd",
            f"{stub} --mode aat",
            "--candidate-id-bin",
            str(missing),
            "--corpus",
            str(corpus),
            "--out",
            str(out_path),
            "--runs",
            "1",
        ]
    )
    assert result.returncode == 2, result.stdout + result.stderr
    assert not out_path.exists()
