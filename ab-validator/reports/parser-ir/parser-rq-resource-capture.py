#!/usr/bin/env python3
"""Orchestrate serial transient-service resource captures."""

from __future__ import annotations

import argparse
import json
import os
import pathlib
import platform
import subprocess
import sys
import uuid
from collections.abc import Callable
from typing import Any


def build_systemd_run(work_id: str, wrapper_argv: list[str]) -> list[str]:
    unit = f"parser-rq-{work_id}-{uuid.uuid4().hex}.service"
    return [
        "systemd-run",
        "--user",
        "--wait",
        "--collect",
        "--service-type=exec",
        f"--unit={unit}",
        "--property=MemoryAccounting=yes",
        "--property=MemoryMax=3221225472",
        "--property=MemorySwapMax=0",
        "--property=OOMPolicy=continue",
        "--property=Delegate=no",
        *wrapper_argv,
    ]


def capture_index(
    policy: dict[str, Any], runner: Callable[[str], dict[str, Any]]
) -> dict[str, Any]:
    expected = list(policy["work_ids"])
    records = [runner(work_id) for work_id in expected]
    actual = [record.get("work_id") for record in records]
    if actual != expected or len(set(actual)) != len(expected):
        return {
            "status": "unavailable",
            "reason": "index_incomplete",
            "work_ids": expected,
            "records": records,
        }
    return {"status": "captured", "work_ids": expected, "records": records}


def attempt_context() -> dict[str, str]:
    return {
        "kernel_release": platform.release(),
        "system": platform.platform(),
    }


def write_index(path: pathlib.Path, value: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(f".{path.name}.{os.getpid()}.tmp")
    try:
        temporary.write_text(
            json.dumps(value, sort_keys=True, separators=(",", ":")) + "\n",
            encoding="utf-8",
        )
        os.replace(temporary, path)
    finally:
        temporary.unlink(missing_ok=True)


def run_wrapper(
    wrapper: pathlib.Path, work_id: str, record: pathlib.Path, command: list[str]
) -> None:
    completed = subprocess.run(
        [
            sys.executable,
            str(wrapper),
            "--output",
            str(record),
            "--",
            *command,
        ],
        check=False,
    )
    if completed.returncode != 0:
        raise ValueError(f"resource wrapper failed for {work_id}")


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--policy", type=pathlib.Path, required=True)
    parser.add_argument("--wrapper", type=pathlib.Path, required=True)
    parser.add_argument("--command-template", type=pathlib.Path, required=True)
    parser.add_argument("--records-root", type=pathlib.Path, required=True)
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
        policy = json.loads(args.policy.read_bytes())
        template = json.loads(args.command_template.read_bytes())
        if not isinstance(template, list) or not all(isinstance(value, str) for value in template):
            raise ValueError("resource command template is malformed")
        args.records_root.mkdir(parents=True, exist_ok=True)

        def capture_one(work_id: str) -> dict[str, Any]:
            record = args.records_root / f"{work_id}.json"
            command = [value.replace("{work_id}", work_id) for value in template]
            run_wrapper(args.wrapper, work_id, record, command)
            value = json.loads(record.read_bytes())
            if not isinstance(value, dict):
                raise ValueError(f"resource record is malformed for {work_id}")
            value.setdefault("work_id", work_id)
            return value

        write_index(args.out, capture_index(policy, capture_one))
    except (OSError, ValueError, json.JSONDecodeError, KeyError) as error:
        print(str(error), file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
