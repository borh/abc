import importlib.util
import pathlib
import sys


MODULE = pathlib.Path(__file__).parents[1] / "parser-rq-resource-capture.py"


def load_module():
    spec = importlib.util.spec_from_file_location("parser_rq_resource_capture", MODULE)
    assert spec and spec.loader
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def test_systemd_argv_pins_service_properties():
    capture = load_module()
    argv = capture.build_systemd_run("000001_1", ["wrapper", "--", "parser"])
    joined = " ".join(argv)
    assert "--user" in argv
    assert "--service-type=exec" in argv
    assert "--scope" not in argv
    for value in (
        "MemoryAccounting=yes",
        "MemoryMax=3221225472",
        "MemorySwapMax=0",
        "OOMPolicy=continue",
        "Delegate=no",
    ):
        assert value in joined


def test_capture_index_is_serial_and_closed():
    capture = load_module()
    calls = []

    def runner(work_id):
        calls.append(work_id)
        return {"work_id": work_id, "status": "measured", "peak_cgroup_memory_bytes": 1}

    result = capture.capture_index({"work_ids": ["a", "b"]}, runner)
    assert calls == ["a", "b"]
    assert [record["work_id"] for record in result["records"]] == ["a", "b"]
