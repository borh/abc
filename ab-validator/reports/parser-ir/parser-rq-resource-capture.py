#!/usr/bin/env python3
"""Orchestrate serial transient-service resource captures."""

from __future__ import annotations

import json
import pathlib
import platform
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
    path.write_text(
        json.dumps(value, sort_keys=True, separators=(",", ":")) + "\n",
        encoding="utf-8",
    )
