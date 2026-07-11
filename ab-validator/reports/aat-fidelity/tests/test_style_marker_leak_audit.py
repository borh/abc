import importlib.util
import json
import pathlib
import subprocess
import sys

SCRIPT = pathlib.Path(__file__).resolve().parents[1] / "style-marker-leak-audit.py"


def load_module():
    spec = importlib.util.spec_from_file_location("style_marker_leak_audit", SCRIPT)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def write_doc(directory: pathlib.Path, name: str, blocks) -> None:
    directory.mkdir(parents=True, exist_ok=True)
    (directory / name).write_text(json.dumps({"blocks": blocks}, ensure_ascii=False))


def run_audit(tmp_path: pathlib.Path, input_dir: pathlib.Path, limit: int = 30):
    summary = tmp_path / "summary.json"
    report = tmp_path / "report.md"
    proc = subprocess.run(
        [
            sys.executable,
            str(SCRIPT),
            str(input_dir),
            "--summary-json",
            str(summary),
            "--report-md",
            str(report),
            "--example-limit",
            str(limit),
            "--input-label",
            "fixture",
        ],
        capture_output=True,
        text=True,
    )
    return proc, json.loads(summary.read_text()), report.read_text()


def style(style_type: str, *content):
    return {"kind": "style", "style_type": style_type, "content": list(content)}


def text(value):
    return {"kind": "text", "value": value}


def paragraph(*content):
    return {"kind": "paragraph", "content": list(content)}


def test_marker_occurrences_are_shortest_and_partial_openers_are_separate():
    module = load_module()
    markers, partial = module.marker_occurrences("［＃foo］x］［＃bar］y［＃open")
    assert markers == ["［＃foo］", "［＃bar］"]
    assert partial == 1


def test_recursive_audit_uses_nearest_style_and_ignores_prose_markers(tmp_path):
    input_dir = tmp_path / "aat"
    write_doc(
        input_dir,
        "b.json",
        [
            paragraph(
                text("outside［＃prose］"),
                style(
                    "bold",
                    text("［＃太字］visible［＃太字終わり］"),
                    style("emphasis", text("［＃傍点］inner［＃傍点終わり］")),
                ),
            )
        ],
    )
    proc, summary, _ = run_audit(tmp_path, input_dir)
    assert proc.returncode == 0
    assert summary["totals"]["affected_style_nodes"] == 2
    assert summary["totals"]["affected_text_nodes"] == 2
    assert summary["totals"]["marker_occurrences"] == 4
    assert summary["by_style_type"] == {"bold": 2, "emphasis": 2}


def test_partial_openers_do_not_become_complete_findings(tmp_path):
    input_dir = tmp_path / "aat"
    write_doc(input_dir, "a.json", [paragraph(style("bold", text("before［＃open")))])
    proc, summary, report = run_audit(tmp_path, input_dir)
    assert proc.returncode == 0
    assert summary["totals"]["marker_occurrences"] == 0
    assert summary["totals"]["unmatched_marker_open"] == 1
    assert "split across text nodes" in report


def test_malformed_inputs_are_recorded_and_exit_one(tmp_path):
    input_dir = tmp_path / "aat"
    input_dir.mkdir()
    (input_dir / "bad-json.json").write_text("{")
    (input_dir / "bad-blocks.json").write_text(json.dumps({"blocks": {}}))
    write_doc(input_dir, "bad-text.json", [paragraph(style("bold", text(42)))])
    proc, summary, _ = run_audit(tmp_path, input_dir)
    assert proc.returncode == 1
    assert summary["totals"]["files_scanned"] == 3
    assert summary["totals"]["malformed_files"] == 3
    assert [row["file_stem"] for row in summary["malformed_inputs"]] == [
        "bad-blocks",
        "bad-json",
        "bad-text",
    ]


def test_examples_are_deterministic_diverse_and_capped_per_file(tmp_path):
    input_dir = tmp_path / "aat"
    for name in ["a", "b", "c"]:
        write_doc(
            input_dir,
            f"{name}.json",
            [
                paragraph(
                    style("bold", text(f"［＃太字］{name}［＃太字終わり］")),
                    style("bouten", text(f"［＃丸傍点］{name}［＃丸傍点終わり］")),
                    style("emphasis", text(f"［＃斜体］{name}［＃斜体終わり］")),
                )
            ],
        )
    proc, summary, report = run_audit(tmp_path, input_dir, limit=6)
    assert proc.returncode == 0
    assert len(summary["examples"]) == 6
    assert {row["style_type"] for row in summary["examples"]} == {
        "bold",
        "bouten",
        "emphasis",
    }
    per_file = {}
    for row in summary["examples"]:
        per_file[row["file_stem"]] = per_file.get(row["file_stem"], 0) + 1
    assert max(per_file.values()) == 2
    assert "［＃太字］…［＃太字終わり］" in {row["signature"] for row in summary["examples"]}

    second = tmp_path / "second"
    second.mkdir()
    proc2, summary2, report2 = run_audit(second, input_dir, limit=6)
    assert proc2.returncode == 0
    assert summary2 == summary
    assert report2 == report


def test_quoted_target_signature_is_retained():
    module = load_module()
    assert module.marker_signature(["［＃「ピアノ」は太字］"]) == "［＃「ピアノ」は太字］"
