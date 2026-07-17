import importlib.util
import json
import os
import pathlib
import sys


MODULE = pathlib.Path(__file__).parents[1] / "parser-rq-resource-wrapper.py"


def load_module():
    spec = importlib.util.spec_from_file_location("parser_rq_resource_wrapper", MODULE)
    assert spec and spec.loader
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def fake_cgroup(tmp_path, *, peak="1024", swap="0", events="oom_kill 0\n"):
    (tmp_path / "memory.peak").write_text(peak + "\n", encoding="ascii")
    (tmp_path / "memory.swap.peak").write_text(swap + "\n", encoding="ascii")
    (tmp_path / "memory.events").write_text(events, encoding="ascii")
    (tmp_path / "cgroup.procs").write_text(f"{os.getpid()}\n", encoding="ascii")
    return tmp_path


def test_measured_peak_is_exact(tmp_path):
    wrapper = load_module()
    result = wrapper.capture_work(
        [sys.executable, "-c", "pass"], 0.1, cgroup_dir=fake_cgroup(tmp_path)
    )
    assert result == {
        "status": "measured",
        "peak_cgroup_memory_bytes": 1024,
        "peak_swap_bytes": 0,
        "right_censored": False,
        "oom_kill": False,
        "child_exit_code": 0,
    }


def test_malformed_counter_is_unavailable(tmp_path):
    wrapper = load_module()
    result = wrapper.capture_work(
        [sys.executable, "-c", "pass"], 0.1, cgroup_dir=fake_cgroup(tmp_path, peak="nope")
    )
    assert result["status"] == "unavailable"
    assert result["reason"] == "counter_invalid"
    assert "peak_cgroup_memory_bytes" not in result


def test_oom_above_threshold_is_right_censored(tmp_path):
    wrapper = load_module()
    result = wrapper.capture_work(
        [sys.executable, "-c", "raise SystemExit(1)"],
        0.1,
        cgroup_dir=fake_cgroup(
            tmp_path, peak=str(2147483649), events="oom_kill 1\n"
        ),
    )
    assert result["status"] == "ceiling_clipped"
    assert result["right_censored"] is True
    assert result["oom_kill"] is True


def test_cli_rejects_cgroup_override(tmp_path):
    wrapper = load_module()
    assert wrapper.main(["--cgroup-dir", str(tmp_path), "--", "/bin/true"]) == 2
