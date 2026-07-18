from __future__ import annotations

import importlib.util
import json
from pathlib import Path


MODULE_PATH = Path(__file__).with_name("parser-rq-resource-capture.py")
SPEC = importlib.util.spec_from_file_location("parser_rq_resource_capture", MODULE_PATH)
assert SPEC and SPEC.loader
resource = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(resource)


def test_resource_cli_runs_closed_work_order_and_installs_index_atomically(
    tmp_path: Path, monkeypatch
) -> None:
    policy = tmp_path / "policy.json"
    command = tmp_path / "command.json"
    output = tmp_path / "resource-index.json"
    policy.write_text(json.dumps({"work_ids": ["w2", "w1"]}))
    command.write_text(json.dumps(["parser", "--work-id", "{work_id}"]))
    calls: list[str] = []

    def fake_run(wrapper: Path, work_id: str, record: Path, argv: list[str]) -> None:
        calls.append(work_id)
        assert argv == ["parser", "--work-id", work_id]
        record.write_text(json.dumps({"work_id": work_id, "status": "measured"}))

    monkeypatch.setattr(resource, "run_wrapper", fake_run)
    assert (
        resource.main(
            [
                "--policy",
                str(policy),
                "--wrapper",
                str(tmp_path / "wrapper.py"),
                "--command-template",
                str(command),
                "--records-root",
                str(tmp_path / "records"),
                "--out",
                str(output),
            ]
        )
        == 0
    )
    assert calls == ["w2", "w1"]
    assert json.loads(output.read_bytes())["work_ids"] == ["w2", "w1"]
    assert not list(tmp_path.glob(".resource-index.json.*.tmp"))


def test_resource_cli_help() -> None:
    assert resource.main(["--help"]) == 0
