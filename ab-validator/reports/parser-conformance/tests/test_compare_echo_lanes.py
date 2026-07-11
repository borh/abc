import json
import pathlib
import subprocess
import sys

SCRIPT = pathlib.Path(__file__).resolve().parents[1] / "compare-echo-lanes.py"


def row(vector, adapter, status="pass", failures=(), skips=(), warnings=()):
    return {
        "vector": vector,
        "adapter": adapter,
        "status": status,
        "failures": list(failures),
        "skips": list(skips),
        "warnings": list(warnings),
        "feature": "x",
        "level": "must",
    }


def run(tmp_path, rows, *args):
    summary = tmp_path / "summary.json"
    summary.write_text(json.dumps({"rows": rows}))
    return subprocess.run(
        [
            sys.executable,
            str(SCRIPT),
            str(summary),
            "--lane-a",
            "aozora-adapter",
            "--lane-b",
            "ab-aozora",
            *args,
        ],
        capture_output=True,
        text=True,
    )


def test_echo_passes(tmp_path):
    rows = [
        row("v1", "aozora-adapter"),
        row("v1", "ab-aozora"),
        row("v2", "aozora-adapter", "fail", failures=["nodes: x"]),
        row("v2", "ab-aozora", "fail", failures=["nodes: x"]),
    ]
    proc = run(tmp_path, rows)
    assert proc.returncode == 0
    assert json.loads(proc.stdout)["vectors_compared"] == 2


def test_status_divergence_fails(tmp_path):
    rows = [
        row("v1", "aozora-adapter", "pass"),
        row("v1", "ab-aozora", "fail", failures=["nodes: y"]),
    ]
    proc = run(tmp_path, rows)
    assert proc.returncode == 1
    assert json.loads(proc.stdout)["differing_count"] == 1


def test_vector_missing_in_one_lane_is_structural(tmp_path):
    rows = [row("v1", "aozora-adapter"), row("v1", "ab-aozora"), row("v2", "aozora-adapter")]
    assert run(tmp_path, rows).returncode == 2


def test_duplicate_vector_is_structural(tmp_path):
    rows = [row("v1", "aozora-adapter"), row("v1", "aozora-adapter"), row("v1", "ab-aozora")]
    assert run(tmp_path, rows).returncode == 2


def test_absent_lane_label_is_structural(tmp_path):
    rows = [row("v1", "aozora-adapter")]
    assert run(tmp_path, rows).returncode == 2
