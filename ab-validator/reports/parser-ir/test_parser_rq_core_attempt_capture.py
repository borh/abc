import importlib.util
import json
import pathlib
import sys
from datetime import UTC, datetime

import pytest


MODULE = pathlib.Path(__file__).with_name("parser-rq-core-attempt-capture.py")
HASH = "sha256:" + ("a" * 64)


def load_module():
    spec = importlib.util.spec_from_file_location("parser_rq_core_attempt_capture", MODULE)
    assert spec and spec.loader
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


@pytest.mark.parametrize("raw, expected", [(b"0\n", 0.0), (b"12.50\n", 12.5), (b"0.001", 0.001)])
def test_parse_elapsed_record_accepts_one_plain_decimal_line(raw, expected):
    capture = load_module()
    assert capture.parse_elapsed_record(raw) == expected


@pytest.mark.parametrize(
    "raw",
    [
        b"",
        b"\n",
        b"1\n2\n",
        b"-1\n",
        b"nan\n",
        b"inf\n",
        b"1e3\n",
        b"1,5\n",
        b"1.0 seconds\n",
        b" 1.0\n",
        b"1.0 \n",
        b"\xff\n",
    ],
)
def test_parse_elapsed_record_rejects_ambiguous_timing_bytes(raw):
    capture = load_module()
    with pytest.raises(ValueError):
        capture.parse_elapsed_record(raw)


@pytest.mark.parametrize(
    "report, expected",
    [
        ({"results": {"parse_completeness": {"pass": True}}}, "parsed"),
        ({"results": {"fatal_error": {"pass": False}}}, "fatal_error"),
        ({"results": {"adapter_timeout": {"pass": False}}}, "adapter_timeout"),
        ({"results": {"adapter_protocol_error": {"pass": False}}}, "protocol_error"),
    ],
)
def test_classify_work_pins_the_closed_disposition_set(report, expected):
    capture = load_module()
    assert capture.classify_work(report) == expected


def authorization(**changes):
    value = {
        "authorization_ordinal": 1,
        "candidate_ref": HASH,
        "not_before_utc": "2026-07-17T00:00:00Z",
        "not_after_utc": "2026-07-17T01:00:00Z",
        "repetitions": 3,
        "reduction": "maximum",
        "host_policy_ref": HASH,
    }
    value.update(changes)
    return value


def test_validate_execution_window_is_closed_and_utc():
    capture = load_module()
    auth = authorization()
    capture.validate_execution_window(datetime(2026, 7, 17, 0, 0, tzinfo=UTC), auth)
    capture.validate_execution_window(datetime(2026, 7, 17, 1, 0, tzinfo=UTC), auth)
    with pytest.raises(ValueError, match="outside authorized capture window"):
        capture.validate_execution_window(datetime(2026, 7, 17, 1, 0, 1, tzinfo=UTC), auth)
    with pytest.raises(ValueError, match="timezone-aware"):
        capture.validate_execution_window(datetime(2026, 7, 17, 0, 30), auth)


def test_capture_repetitions_is_serial_closed_and_lock_retaining(tmp_path):
    capture = load_module()
    calls = []
    active = False
    elapsed = iter((b"1.00\n", b"1.25\n", b"1.10\n"))

    def run_command(argv, env):
        nonlocal active
        assert not active
        active = True
        assert env["LC_ALL"] == "C"
        timing_path = pathlib.Path(argv[argv.index("-o") + 1])
        report_dir = pathlib.Path(argv[argv.index("--output-dir") + 1])
        timing_path.write_bytes(next(elapsed))
        report_dir.mkdir(parents=True, exist_ok=True)
        for work_id in ("w1", "w2"):
            (report_dir / f"{work_id}.json").write_text(
                json.dumps(
                    {
                        "adapter": "ab-aozora-aat",
                        "adapter_version": "1.0.0",
                        "work_id": work_id,
                        "results": {"parse_completeness": {"pass": True}},
                    }
                ),
                encoding="utf-8",
            )
        calls.append(list(argv))
        active = False
        return 0

    contexts = iter(
        {
            "captured_at_utc": f"2026-07-17T00:00:0{tick}Z",
            "load_average_1m": 0.1,
            "memory_pressure": {"some_avg10": 0.0, "full_avg10": 0.0},
            "competing_units": [],
        }
        for tick in range(6)
    )
    config = capture.CaptureConfig(
        authorization=authorization(),
        expected_works={"w1": HASH, "w2": HASH},
        argv_template=("ab-check", "--output-dir", "{report_dir}"),
        time_executable="/nix/store/time/bin/time",
        staging_root=tmp_path / "capture",
        lock_path=tmp_path / "campaign.lock",
        qualification_identity_ref=HASH,
        candidate_ref=HASH,
        policy_hash=HASH,
        now=lambda: datetime(2026, 7, 17, 0, 30, tzinfo=UTC),
        run_command=run_command,
        context_reader=lambda: next(contexts),
    )

    index = capture.capture_repetitions(config)

    assert len(calls) == 3
    assert [attempt["repetition"] for attempt in index["attempts"]] == [1, 2, 3]
    assert all(attempt["lock_retained"] for attempt in index["attempts"])
    assert len(index["records"]) == 6
    assert {(row["work_id"], row["repetition"]) for row in index["records"]} == {
        ("w1", 1),
        ("w2", 1),
        ("w1", 2),
        ("w2", 2),
        ("w1", 3),
        ("w2", 3),
    }
    assert all(
        argv[:6] == ["/nix/store/time/bin/time", "-f", "%e", "-o", argv[4], "--"] for argv in calls
    )


@pytest.mark.parametrize(
    "change, message",
    [
        ({"authorization_ordinal": 2}, "ordinal"),
        ({"repetitions": 2}, "three repetitions"),
        ({"reduction": "minimum"}, "maximum reduction"),
    ],
)
def test_capture_repetitions_rejects_policy_drift_before_execution(tmp_path, change, message):
    capture = load_module()
    calls = []
    config = capture.CaptureConfig(
        authorization=authorization(**change),
        expected_works={"w1": HASH},
        argv_template=("ab-check", "--output-dir", "{report_dir}"),
        time_executable="time",
        staging_root=tmp_path / "capture",
        lock_path=tmp_path / "campaign.lock",
        qualification_identity_ref=HASH,
        candidate_ref=HASH,
        policy_hash=HASH,
        now=lambda: datetime(2026, 7, 17, 0, 30, tzinfo=UTC),
        run_command=lambda argv, env: calls.append((argv, env)) or 0,
        context_reader=lambda: {},
    )
    with pytest.raises(ValueError, match=message):
        capture.capture_repetitions(config)
    assert calls == []
