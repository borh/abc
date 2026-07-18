#!/usr/bin/env python3
"""Capture the closed raw inputs for parser release core-attempt predicates."""

from __future__ import annotations

import argparse
import hashlib
import json
import math
import os
import pathlib
import re
import subprocess
import sys
from collections.abc import Callable, Mapping, Sequence
from dataclasses import dataclass, field
from datetime import UTC, datetime


PLAIN_DECIMAL = re.compile(rb"(?:0|[1-9][0-9]*)(?:\.[0-9]+)?\n?")
ERROR_RESULTS = {"fatal_error", "adapter_timeout", "adapter_protocol_error"}

RunCommand = Callable[[Sequence[str], Mapping[str, str], tuple[int, ...]], int]
ContextReader = Callable[[], dict[str, object]]


def parse_elapsed_record(raw: bytes) -> float:
    """Parse exactly one non-negative GNU-time decimal record."""
    if PLAIN_DECIMAL.fullmatch(raw) is None:
        raise ValueError("elapsed record must be one plain non-negative decimal line")
    value = float(raw.removesuffix(b"\n").decode("ascii"))
    if not math.isfinite(value):
        raise ValueError("elapsed record must be finite")
    return value


def classify_work(report: dict[str, object]) -> str:
    """Project an authenticated ab-check report onto the closed disposition set."""
    results = report.get("results")
    if not isinstance(results, dict) or not results:
        return "protocol_error"
    for result in results.values():
        if not isinstance(result, dict) or not isinstance(result.get("pass"), bool):
            return "protocol_error"
    present = [name for name in ERROR_RESULTS if name in results]
    if len(present) > 1:
        return "protocol_error"
    if not present:
        return "parsed"
    name = present[0]
    result = results[name]
    if not isinstance(result, dict) or result.get("pass") is not False:
        return "protocol_error"
    return {
        "fatal_error": "fatal_error",
        "adapter_timeout": "adapter_timeout",
        "adapter_protocol_error": "protocol_error",
    }[name]


def _parse_utc(value: object, field_name: str) -> datetime:
    if not isinstance(value, str):
        raise ValueError(f"{field_name} must be an RFC 3339 timestamp")
    try:
        parsed = datetime.fromisoformat(value.replace("Z", "+00:00"))
    except ValueError as error:
        raise ValueError(f"{field_name} must be an RFC 3339 timestamp") from error
    if parsed.tzinfo is None:
        raise ValueError(f"{field_name} must include a timezone")
    return parsed.astimezone(UTC)


def validate_execution_window(now: datetime, authorization: dict[str, object]) -> None:
    """Fail unless now is inside the authorization's inclusive UTC window."""
    if now.tzinfo is None:
        raise ValueError("capture clock must be timezone-aware")
    not_before = _parse_utc(authorization.get("not_before_utc"), "not_before_utc")
    not_after = _parse_utc(authorization.get("not_after_utc"), "not_after_utc")
    if not_before > not_after:
        raise ValueError("authorized capture window is reversed")
    if not not_before <= now.astimezone(UTC) <= not_after:
        raise ValueError("outside authorized capture window")


def _run_command(argv: Sequence[str], env: Mapping[str, str], pass_fds: tuple[int, ...]) -> int:
    return subprocess.run(list(argv), check=False, env=dict(env), pass_fds=pass_fds).returncode


def _memory_pressure() -> dict[str, float]:
    values = {"some_avg10": 0.0, "full_avg10": 0.0}
    try:
        rows = pathlib.Path("/proc/pressure/memory").read_text(encoding="ascii").splitlines()
    except OSError:
        return values
    for row in rows:
        parts = row.split()
        if not parts or parts[0] not in {"some", "full"}:
            continue
        avg10 = next((part for part in parts[1:] if part.startswith("avg10=")), None)
        if avg10 is not None:
            values[f"{parts[0]}_avg10"] = float(avg10.split("=", 1)[1])
    return values


def _competing_units() -> list[str]:
    result = subprocess.run(
        [
            "systemctl",
            "--user",
            "list-units",
            "--type=service",
            "--state=running",
            "--no-legend",
            "--plain",
        ],
        check=False,
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        text=True,
    )
    if result.returncode != 0:
        return []
    return sorted(
        {
            row.split()[0]
            for row in result.stdout.splitlines()
            if row.split() and any(token in row.lower() for token in ("parser", "aozora"))
        }
    )


def _attempt_context() -> dict[str, object]:
    return {
        "captured_at_utc": datetime.now(UTC).isoformat().replace("+00:00", "Z"),
        "load_average_1m": os.getloadavg()[0],
        "memory_pressure": _memory_pressure(),
        "competing_units": _competing_units(),
    }


@dataclass(frozen=True)
class CaptureConfig:
    authorization: dict[str, object]
    expected_works: dict[str, str]
    argv_template: tuple[str, ...]
    time_executable: str
    staging_root: pathlib.Path
    qualification_identity_ref: str
    candidate_ref: str
    policy_hash: str
    now: Callable[[], datetime] = field(default=lambda: datetime.now(UTC))
    run_command: RunCommand = field(default=_run_command)
    context_reader: ContextReader = field(default=_attempt_context)


@dataclass(frozen=True)
class LockCapability:
    fd: int
    device: int
    inode: int


def _json_bytes(value: object) -> bytes:
    return (json.dumps(value, sort_keys=True, separators=(",", ":")) + "\n").encode()


def _write_json(path: pathlib.Path, value: object) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(f".{path.name}.{os.getpid()}.tmp")
    try:
        with temporary.open("xb") as stream:
            stream.write(_json_bytes(value))
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temporary, path)
    finally:
        temporary.unlink(missing_ok=True)


def _blob(path: pathlib.Path, root: pathlib.Path, media_type: str) -> dict[str, object]:
    raw = path.read_bytes()
    return {
        "sha256": f"sha256:{hashlib.sha256(raw).hexdigest()}",
        "bytes": len(raw),
        "media_type": media_type,
        "locator": path.relative_to(root).as_posix(),
    }


def _validate_authorization(config: CaptureConfig) -> None:
    auth = config.authorization
    if auth.get("authorization_ordinal") != 1:
        raise ValueError("capture authorization ordinal must be 1")
    if auth.get("repetitions") != 3:
        raise ValueError("capture authorization must require exactly three repetitions")
    if auth.get("reduction") != "maximum":
        raise ValueError("capture authorization must require maximum reduction")
    if auth.get("candidate_ref") != config.candidate_ref:
        raise ValueError("capture authorization candidate_ref mismatch")
    if not config.expected_works:
        raise ValueError("capture requires a non-empty closed work set")
    validate_execution_window(config.now(), auth)


def _lock_retained(lock: LockCapability) -> bool:
    try:
        handle_stat = os.fstat(lock.fd)
        target = os.readlink(f"/proc/self/fd/{lock.fd}")
        if not target.startswith("/") or target.endswith(" (deleted)"):
            return False
        path_stat = os.stat(target)
    except OSError:
        return False
    return (
        (path_stat.st_dev, path_stat.st_ino)
        == (
            lock.device,
            lock.inode,
        )
        == (
            handle_stat.st_dev,
            handle_stat.st_ino,
        )
    )


def _load_report(path: pathlib.Path, work_id: str) -> dict[str, object]:
    try:
        value = json.loads(path.read_bytes())
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValueError(f"missing or invalid report for {work_id}") from error
    if not isinstance(value, dict) or value.get("work_id") != work_id:
        raise ValueError(f"report identity mismatch for {work_id}")
    return value


def capture_repetitions(config: CaptureConfig, lock: LockCapability) -> dict[str, object]:
    """Run and record exactly three serial, locked core attempts."""
    _validate_authorization(config)
    if not _lock_retained(lock):
        raise ValueError("exclusive campaign lock was not retained")
    config.staging_root.mkdir(parents=True, exist_ok=True)
    attempts: list[dict[str, object]] = []
    records: list[dict[str, object]] = []
    for repetition in range(1, 4):
        if not _lock_retained(lock):
            raise ValueError("exclusive campaign lock was not retained")
        validate_execution_window(config.now(), config.authorization)
        repetition_root = config.staging_root / f"repetition-{repetition}"
        report_dir = repetition_root / "reports"
        report_dir.mkdir(parents=True, exist_ok=False)
        elapsed_path = repetition_root / "elapsed.txt"
        argv = [
            token.replace("{report_dir}", str(report_dir)).replace("{repetition}", str(repetition))
            for token in config.argv_template
        ]
        command = [
            config.time_executable,
            "-f",
            "%e",
            "-o",
            str(elapsed_path),
            "--",
            *argv,
        ]
        before = config.context_reader()
        was_inheritable = os.get_inheritable(lock.fd)
        try:
            os.set_inheritable(lock.fd, True)
            exit_status = config.run_command(command, {**os.environ, "LC_ALL": "C"}, (lock.fd,))
        finally:
            try:
                os.set_inheritable(lock.fd, was_inheritable)
            except OSError:
                pass
        after = config.context_reader()
        retained = _lock_retained(lock)
        if not retained:
            raise ValueError("exclusive campaign lock was not retained")
        raw_elapsed = elapsed_path.read_bytes()
        parse_elapsed_record(raw_elapsed)
        attempts.append(
            {
                "repetition": repetition,
                "elapsed_record": _blob(elapsed_path, config.staging_root, "text/plain"),
                "argv": argv,
                "exit_status": exit_status,
                "lock_retained": retained,
                "before": before,
                "after": after,
            }
        )
        if exit_status != 0:
            raise ValueError(f"core attempt repetition {repetition} exited {exit_status}")
        for work_id, source_sha256 in config.expected_works.items():
            report_path = report_dir / f"{work_id}.json"
            report = _load_report(report_path, work_id)
            disposition = classify_work(report)
            report_blob = _blob(report_path, config.staging_root, "application/json")
            record: dict[str, object] = {
                "schema_id": "https://w3id.org/abc/schemas/parser-rq-core-attempt-work.schema.json",
                "schema_version": "1.0.0",
                "work_id": work_id,
                "source_sha256": source_sha256,
                "qualification_identity_ref": config.qualification_identity_ref,
                "candidate_ref": config.candidate_ref,
                "policy_hash": config.policy_hash,
                "repetition": repetition,
                "status": "measured",
                "disposition": disposition,
                "report": report_blob,
            }
            if disposition == "protocol_error":
                record["status"] = "protocol_error"
                record["reason"] = "adapter_protocol_error"
                record.pop("disposition")
            record_path = repetition_root / "records" / f"{work_id}.json"
            _write_json(record_path, record)
            records.append(
                {
                    "work_id": work_id,
                    "repetition": repetition,
                    "record": _blob(record_path, config.staging_root, "application/json"),
                }
            )
    return {
        "schema_id": "https://w3id.org/abc/schemas/parser-rq-core-attempt-index.schema.json",
        "schema_version": "1.0.0",
        "qualification_identity_ref": config.qualification_identity_ref,
        "candidate_ref": config.candidate_ref,
        "policy_hash": config.policy_hash,
        "expected_work_ids": list(config.expected_works),
        "repetitions": 3,
        "reduction": "maximum",
        "attempts": attempts,
        "records": records,
    }


def _read_json(path: pathlib.Path) -> dict[str, object]:
    try:
        value = json.loads(path.read_bytes())
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValueError(f"{path} is not valid JSON") from error
    if not isinstance(value, dict):
        raise ValueError(f"{path} is not a JSON object")
    return value


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--runtime", type=pathlib.Path, required=True)
    parser.add_argument("--policy", type=pathlib.Path, required=True)
    parser.add_argument("--corpus-root", type=pathlib.Path, required=True)
    parser.add_argument("--corpus-index", type=pathlib.Path, required=True)
    parser.add_argument("--time-executable", required=True)
    parser.add_argument("--staging-root", type=pathlib.Path, required=True)
    parser.add_argument("--inherited-lock-fd", type=int, required=True)
    parser.add_argument("--lock-device", type=int, required=True)
    parser.add_argument("--lock-inode", type=int, required=True)
    parser.add_argument("--out", type=pathlib.Path, required=True)
    return parser


def main(argv: list[str] | None = None) -> int:
    parser = _parser()
    try:
        args = parser.parse_args(argv)
    except SystemExit as error:
        if argv is not None:
            return int(error.code)
        raise
    try:
        runtime = _read_json(args.runtime)
        if runtime.get("schema_version") != "abc/parser-rq-runtime-inputs/v1":
            raise ValueError("runtime input schema identity is invalid")
        candidate = runtime.get("candidate")
        authorization = runtime.get("authorization")
        if not isinstance(candidate, dict) or not isinstance(authorization, dict):
            raise ValueError("runtime candidate or authorization is malformed")
        policy = _read_json(args.policy)
        expected_sources = policy.get("expected_sources")
        argv_template = policy.get("argv_template")
        if not isinstance(expected_sources, list) or not all(
            isinstance(row, dict)
            and isinstance(row.get("work_id"), str)
            and isinstance(row.get("source_sha256"), str)
            for row in expected_sources
        ):
            raise ValueError("core policy expected_sources are malformed")
        if not isinstance(argv_template, list) or not all(
            isinstance(token, str) for token in argv_template
        ):
            raise ValueError("core policy argv_template is malformed")
        expected_works = {
            str(row["work_id"]): str(row["source_sha256"]) for row in expected_sources
        }
        if len(expected_works) != len(expected_sources):
            raise ValueError("core policy work membership is duplicated")
        replacements = {
            "{index}": str(args.corpus_index),
            "{corpus}": str(args.corpus_root),
            "{work_ids}": ",".join(expected_works),
        }
        resolved_template = tuple(replacements.get(token, token) for token in argv_template)
        config = CaptureConfig(
            authorization=authorization,
            expected_works=expected_works,
            argv_template=resolved_template,
            time_executable=args.time_executable,
            staging_root=args.staging_root,
            qualification_identity_ref=str(candidate["qualification_identity_ref"]),
            candidate_ref=str(candidate["candidate_ref"]),
            policy_hash=str(policy["policy_hash"]),
        )
        lock = LockCapability(args.inherited_lock_fd, args.lock_device, args.lock_inode)
        _write_json(args.out, capture_repetitions(config, lock))
    except (OSError, KeyError, ValueError) as error:
        print(str(error), file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
