import importlib.util
import fcntl
import json
import os
import pathlib
import sys
from contextlib import contextmanager
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


@contextmanager
def held_lock(capture, path):
    descriptor = os.open(path, os.O_RDWR | os.O_CREAT, 0o600)
    fcntl.flock(descriptor, fcntl.LOCK_EX | fcntl.LOCK_NB)
    stat = os.fstat(descriptor)
    try:
        yield capture.LockCapability(descriptor, stat.st_dev, stat.st_ino)
    finally:
        try:
            fcntl.flock(descriptor, fcntl.LOCK_UN)
        except OSError:
            pass
        try:
            os.close(descriptor)
        except OSError:
            pass


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

    lock_path = tmp_path / "campaign.lock"

    def run_command(argv, env, pass_fds):
        nonlocal active
        assert not active
        active = True
        assert env["LC_ALL"] == "C"
        assert pass_fds == (lock.fd,)
        contender = os.open(lock_path, os.O_RDWR)
        try:
            with pytest.raises(BlockingIOError):
                fcntl.flock(contender, fcntl.LOCK_EX | fcntl.LOCK_NB)
        finally:
            os.close(contender)
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
        qualification_identity_ref=HASH,
        candidate_ref=HASH,
        policy_hash=HASH,
        now=lambda: datetime(2026, 7, 17, 0, 30, tzinfo=UTC),
        run_command=run_command,
        context_reader=lambda: next(contexts),
    )

    with held_lock(capture, lock_path) as lock:
        index = capture.capture_repetitions(config, lock)

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
        argv[:7] == ["/nix/store/time/bin/time", "-q", "-f", "%e", "-o", argv[5], "--"]
        for argv in calls
    )


def test_capture_repetitions_reports_command_failure_without_timing_mask(tmp_path):
    capture = load_module()

    def run_command(argv, _env, _pass_fds):
        pathlib.Path(argv[argv.index("-o") + 1]).write_bytes(b"0.00\n")
        return 1

    config = capture.CaptureConfig(
        authorization=authorization(),
        expected_works={"w1": HASH},
        argv_template=("ab-check",),
        time_executable="time",
        staging_root=tmp_path / "capture",
        qualification_identity_ref=HASH,
        candidate_ref=HASH,
        policy_hash=HASH,
        now=lambda: datetime(2026, 7, 17, 0, 30, tzinfo=UTC),
        run_command=run_command,
        context_reader=lambda: {},
    )
    with held_lock(capture, tmp_path / "campaign.lock") as lock:
        with pytest.raises(ValueError, match="core attempt repetition 1 exited 1"):
            capture.capture_repetitions(config, lock)


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
        qualification_identity_ref=HASH,
        candidate_ref=HASH,
        policy_hash=HASH,
        now=lambda: datetime(2026, 7, 17, 0, 30, tzinfo=UTC),
        run_command=lambda argv, env, pass_fds: calls.append((argv, env, pass_fds)) or 0,
        context_reader=lambda: {},
    )
    with held_lock(capture, tmp_path / "campaign.lock") as lock:
        with pytest.raises(ValueError, match=message):
            capture.capture_repetitions(config, lock)
    assert calls == []


@pytest.mark.parametrize("failure", ["replace", "close"])
def test_capture_repetitions_rejects_replaced_or_lost_inherited_lock(tmp_path, failure):
    capture = load_module()
    calls = []
    lock_path = tmp_path / "campaign.lock"

    def run_command(argv, env, pass_fds):
        repetition = len(calls) + 1
        calls.append(repetition)
        timing_path = pathlib.Path(argv[argv.index("-o") + 1])
        report_dir = pathlib.Path(argv[argv.index("--output-dir") + 1])
        timing_path.write_bytes(b"1.0\n")
        report_dir.mkdir(parents=True, exist_ok=True)
        (report_dir / "w1.json").write_text(
            json.dumps(
                {
                    "work_id": "w1",
                    "results": {"parse_completeness": {"pass": True}},
                }
            )
        )
        if repetition == 2 and failure == "replace":
            replacement = tmp_path / "replacement.lock"
            replacement.write_bytes(b"")
            os.replace(replacement, lock_path)
        if repetition == 2 and failure == "close":
            os.close(pass_fds[0])
        return 0

    config = capture.CaptureConfig(
        authorization=authorization(),
        expected_works={"w1": HASH},
        argv_template=("ab-check", "--output-dir", "{report_dir}"),
        time_executable="time",
        staging_root=tmp_path / "capture",
        qualification_identity_ref=HASH,
        candidate_ref=HASH,
        policy_hash=HASH,
        now=lambda: datetime(2026, 7, 17, 0, 30, tzinfo=UTC),
        run_command=run_command,
        context_reader=lambda: {},
    )
    with held_lock(capture, lock_path) as lock:
        with pytest.raises(ValueError, match="exclusive campaign lock was not retained"):
            capture.capture_repetitions(config, lock)
    assert calls == [1, 2]


def test_production_cli_requires_inherited_lock_and_emits_index_atomically(tmp_path, monkeypatch):
    capture = load_module()
    runtime = tmp_path / "runtime.json"
    policy = tmp_path / "policy.json"
    output = tmp_path / "core-index.json"
    runtime.write_text(
        json.dumps(
            {
                "schema_version": "abc/parser-rq-runtime-inputs/v1",
                "candidate": {
                    "candidate_ref": HASH,
                    "qualification_identity_ref": HASH,
                },
                "authorization": authorization(),
            }
        )
    )
    policy.write_text(
        json.dumps(
            {
                "policy_hash": HASH,
                "expected_sources": [{"work_id": "w1", "source_sha256": HASH}],
                "argv_template": [
                    "ab-check",
                    "--index",
                    "{index}",
                    "--corpus",
                    "{corpus}",
                    "--adapter",
                    "ab-aozora",
                    "--output-dir",
                    "{report_dir}",
                    "--work-ids",
                    "{work_ids_file}",
                ],
            }
        )
    )
    observed = {}

    def fake_capture(config, lock):
        observed["config"] = config
        observed["lock"] = lock
        return {"status": "captured"}

    monkeypatch.setattr(capture, "capture_repetitions", fake_capture)
    lock_path = tmp_path / "campaign.lock"
    with held_lock(capture, lock_path) as lock:
        assert (
            capture.main(
                [
                    "--runtime",
                    str(runtime),
                    "--policy",
                    str(policy),
                    "--ab-check",
                    "/nix/store/parser/bin/ab-check",
                    "--adapter",
                    "/nix/store/parser/bin/ab-aozora",
                    "--corpus-root",
                    str(tmp_path / "corpus"),
                    "--corpus-index",
                    str(tmp_path / "index.json"),
                    "--work-ids",
                    str(tmp_path / "work-ids.json"),
                    "--time-executable",
                    "/nix/store/time/bin/time",
                    "--staging-root",
                    str(tmp_path / "staging"),
                    "--inherited-lock-fd",
                    str(lock.fd),
                    "--lock-device",
                    str(lock.device),
                    "--lock-inode",
                    str(lock.inode),
                    "--out",
                    str(output),
                ]
            )
            == 0
        )
    assert json.loads(output.read_bytes()) == {"status": "captured"}
    assert observed["lock"] == lock
    assert observed["config"].argv_template[0] == "/nix/store/parser/bin/ab-check"
    assert observed["config"].argv_template[2] == str(tmp_path / "index.json")
    assert observed["config"].argv_template[6] == "/nix/store/parser/bin/ab-aozora"
    assert observed["config"].argv_template[-1] == str(tmp_path / "work-ids.json")
    assert not list(tmp_path.glob(".core-index.json.*.tmp"))


def test_core_capture_cli_help_and_no_lock_path_option() -> None:
    capture = load_module()
    assert capture.main(["--help"]) == 0
    assert "lock_path" not in {action.dest for action in capture._parser()._actions}
    assert "candidate" not in {action.dest for action in capture._parser()._actions}
    assert "authorization" not in {action.dest for action in capture._parser()._actions}
    assert "ab_check" in {action.dest for action in capture._parser()._actions}
    assert "adapter" in {action.dest for action in capture._parser()._actions}
