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
    assert code == 0 and out["semantic"]["diverged_count"] == 0


def test_adapter_under_meta_is_also_normalized(tmp_path):
    # Phase 2: BOTH /meta/adapter and /meta/adapter_version are sanctioned
    # identity pointers (candidate emits "ab-aozora" where reference says
    # "aozora") -- semantic normalize now strips both.
    a, b = tmp_path / "a", tmp_path / "b"
    doc_a = {"meta": {"adapter": "aozora", "adapter_version": "v"}, "blocks": []}
    doc_b = {"meta": {"adapter": "ab-aozora", "adapter_version": "v"}, "blocks": []}
    write(a, "w.json", doc_a)
    write(b, "w.json", doc_b)
    code, out = run(a, b)
    assert code == 0 and out["semantic"]["diverged_count"] == 0


def test_any_other_difference_diverges(tmp_path):
    a, b = tmp_path / "a", tmp_path / "b"
    write(a, "w.json", {"meta": {"adapter": "aozora", "adapter_version": "v"}, "blocks": []})
    write(b, "w.json", {"meta": {"adapter": "aozora", "adapter_version": "v"}, "blocks": ["x"]})
    code, out = run(a, b)
    assert code == 1 and out["semantic"]["diverged_count"] == 1


def test_missing_files_diverge(tmp_path):
    a, b = tmp_path / "a", tmp_path / "b"
    write(a, "w1.json", {"meta": {}})
    write(a, "w2.json", {"meta": {}})
    write(b, "w1.json", {"meta": {}})
    code, out = run(a, b)
    assert code == 1 and out["missing_count"] == 1


def _doc(adapter="aozora", version="v1", extra=""):
    return (
        '{"blocks":[],"meta":{"adapter":"%s","adapter_version":"%s",'
        '"parse_complete":true%s},"version":1,"work_id":"w"}' % (adapter, version, extra)
    )


def _mkdumps(tmp_path, doc_a: str, doc_b: str):
    a, b = tmp_path / "a", tmp_path / "b"
    a.mkdir(parents=True, exist_ok=True)
    b.mkdir(parents=True, exist_ok=True)
    (a / "w.json").write_text(doc_a)
    (b / "w.json").write_text(doc_b)
    return a, b


def _run(a, b, *args):
    return subprocess.run(
        [sys.executable, str(SCRIPT), str(a), str(b), *args],
        capture_output=True,
        text=True,
    )


def test_bytes_pass_when_only_identity_differs(tmp_path):
    a, b = _mkdumps(tmp_path, _doc("aozora", "old"), _doc("ab-aozora", "new"))
    assert _run(a, b, "--bytes").returncode == 0


def test_bytes_fail_on_key_order_drift_that_semantic_misses(tmp_path):
    reordered = (
        '{"version":1,"work_id":"w","blocks":[],"meta":{"adapter":"aozora",'
        '"adapter_version":"v1","parse_complete":true}}'
    )
    a, b = _mkdumps(tmp_path, _doc(), reordered)
    result = _run(a, b, "--bytes")
    assert result.returncode == 1
    summary = json.loads(result.stdout)
    assert summary["semantic"]["diverged_count"] == 0
    assert summary["bytes"]["diverged_count"] == 1


def test_bytes_fail_closed_when_identity_pattern_repeats(tmp_path):
    # a block object legitimately carrying an "adapter" key duplicates the
    # serialized needle "adapter":"aozora" at the raw-byte level (a value
    # INSIDE a JSON string would be escaped and would not match) -> the
    # exactly-once check must refuse to substitute -> exit 2
    doc = (
        '{"blocks":[{"adapter":"aozora"}],"meta":{"adapter":"aozora",'
        '"adapter_version":"v1","parse_complete":true},"version":1,"work_id":"w"}'
    )
    a, b = _mkdumps(tmp_path, doc, doc)
    assert _run(a, b, "--bytes").returncode == 2


def test_bytes_fail_closed_on_missing_pointer(tmp_path):
    doc = '{"blocks":[],"meta":{"adapter":"aozora"},"version":1,"work_id":"w"}'
    a, b = _mkdumps(tmp_path, doc, doc)
    assert _run(a, b, "--bytes").returncode == 2
