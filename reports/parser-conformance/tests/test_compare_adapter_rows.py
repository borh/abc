import json
import subprocess
import sys
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[1] / "compare-adapter-rows.py"


def summary(rows):
    return {"rows": rows}


def row(vector="v1", status="pass", adapter="ab-aozora", **kw):
    base = {"vector": vector, "feature": "f", "level": "must", "adapter": adapter,
            "status": status, "failures": [], "warnings": [], "skips": []}
    base.update(kw)
    return base


def run(tmp_path, old_rows, new_rows, adapter="ab-aozora"):
    old = tmp_path / "old.json"
    new = tmp_path / "new.json"
    old.write_text(json.dumps(summary(old_rows)))
    new.write_text(json.dumps(summary(new_rows)))
    return subprocess.run([sys.executable, str(SCRIPT), str(old), str(new),
                           "--adapter", adapter], capture_output=True, text=True)


def test_identical_rows_pass(tmp_path):
    rows = [row(), row(vector="v2", status="warning")]
    assert run(tmp_path, rows, rows).returncode == 0


def test_status_change_fails(tmp_path):
    p = run(tmp_path, [row()], [row(status="fail")])
    assert p.returncode == 1
    assert "v1" in p.stdout


def test_other_adapters_ignored(tmp_path):
    assert run(tmp_path, [row(), row(adapter="aozora", status="fail")],
               [row(), row(adapter="aozora", status="pass")]).returncode == 0


def test_missing_vector_fails(tmp_path):
    assert run(tmp_path, [row(), row(vector="v2")], [row()]).returncode == 1


def test_adapter_absent_is_reference_error(tmp_path):
    assert run(tmp_path, [row(adapter="x")], [row(adapter="x")]).returncode == 2
