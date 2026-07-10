import json
import pathlib
import subprocess
import sys

SCRIPT = pathlib.Path(__file__).resolve().parents[1] / "compare-aat-dumps.py"


def write(d: pathlib.Path, name: str, doc: dict) -> None:
    d.mkdir(parents=True, exist_ok=True)
    (d / name).write_text(json.dumps(doc))


def run(a, b):
    proc = subprocess.run(
        [sys.executable, str(SCRIPT), str(a), str(b)], capture_output=True, text=True
    )
    return proc.returncode, json.loads(proc.stdout)


def test_adapter_version_under_meta_is_normalized(tmp_path):
    a, b = tmp_path / "a", tmp_path / "b"
    doc = {"meta": {"adapter": "aozora", "adapter_version": "upstream x.y"}, "blocks": []}
    write(a, "w.json", doc)
    write(b, "w.json", {**doc, "meta": {"adapter": "aozora", "adapter_version": "shim z"}})
    code, out = run(a, b)
    assert code == 0 and out["diverged_count"] == 0


def test_any_other_difference_diverges(tmp_path):
    a, b = tmp_path / "a", tmp_path / "b"
    write(a, "w.json", {"meta": {"adapter": "aozora", "adapter_version": "v"}, "blocks": []})
    write(b, "w.json", {"meta": {"adapter": "AOZORA2", "adapter_version": "v"}, "blocks": []})
    code, out = run(a, b)
    assert code == 1 and out["diverged_count"] == 1


def test_missing_files_diverge(tmp_path):
    a, b = tmp_path / "a", tmp_path / "b"
    write(a, "w1.json", {"meta": {}})
    write(a, "w2.json", {"meta": {}})
    write(b, "w1.json", {"meta": {}})
    code, out = run(a, b)
    assert code == 1 and out["missing_count"] == 1
