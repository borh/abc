#!/usr/bin/env python3
"""Capture cgroup-v2 process-tree memory for one Parser-IR command."""

from __future__ import annotations

import argparse
import json
import os
import pathlib
import subprocess
import sys
import time


THRESHOLD_BYTES = 2147483648


def own_cgroup_dir() -> pathlib.Path:
    for line in pathlib.Path("/proc/self/cgroup").read_text(encoding="ascii").splitlines():
        hierarchy, controllers, relative = line.split(":", 2)
        if hierarchy == "0" and not controllers:
            return pathlib.Path("/sys/fs/cgroup") / relative.lstrip("/")
    raise ValueError("unified cgroup-v2 membership unavailable")


def _integer(path: pathlib.Path) -> int:
    text = path.read_text(encoding="ascii").strip()
    if not text.isascii() or not text.isdecimal():
        raise ValueError(f"non-integer counter: {path.name}")
    return int(text)


def _events(path: pathlib.Path) -> dict[str, int]:
    return {key: int(value) for key, value in (line.split() for line in path.read_text(encoding="ascii").splitlines())}


def capture_work(
    command: list[str], closure_timeout_s: float, *, cgroup_dir: pathlib.Path | None = None
) -> dict[str, object]:
    directory = cgroup_dir or own_cgroup_dir()
    completed = subprocess.run(command, check=False)
    deadline = time.monotonic() + closure_timeout_s
    while True:
        pids = {int(value) for value in (directory / "cgroup.procs").read_text(encoding="ascii").split()}
        if pids <= {os.getpid()}:
            break
        if time.monotonic() >= deadline:
            return {"status": "unavailable", "reason": "lingering_descendant", "child_exit_code": completed.returncode}
        time.sleep(0.01)
    try:
        peak = _integer(directory / "memory.peak")
        swap = _integer(directory / "memory.swap.peak") if (directory / "memory.swap.peak").exists() else 0
        events = _events(directory / "memory.events")
    except FileNotFoundError:
        return {"status": "unavailable", "reason": "counter_unavailable", "child_exit_code": completed.returncode}
    except (ValueError, UnicodeError):
        return {"status": "unavailable", "reason": "counter_invalid", "child_exit_code": completed.returncode}
    oom = events.get("oom_kill", 0) > 0
    if swap != 0:
        return {"status": "unavailable", "reason": "identity_mismatch", "child_exit_code": completed.returncode}
    if oom and peak > THRESHOLD_BYTES:
        return {"status": "ceiling_clipped", "peak_cgroup_memory_bytes": peak, "peak_swap_bytes": swap, "right_censored": True, "oom_kill": True, "child_exit_code": completed.returncode}
    if oom:
        return {"status": "unavailable", "reason": "unexpected_oom", "child_exit_code": completed.returncode}
    if completed.returncode != 0:
        return {"status": "unavailable", "reason": "command_failed", "child_exit_code": completed.returncode}
    return {"status": "measured", "peak_cgroup_memory_bytes": peak, "peak_swap_bytes": swap, "right_censored": False, "oom_kill": False, "child_exit_code": completed.returncode}


def main(argv: list[str] | None = None) -> int:
    argv = list(sys.argv[1:] if argv is None else argv)
    if "--cgroup-dir" in argv:
        return 2
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", required=True, type=pathlib.Path)
    parser.add_argument("--closure-timeout", type=float, default=5.0)
    parser.add_argument("command", nargs=argparse.REMAINDER)
    args = parser.parse_args(argv)
    command = args.command[1:] if args.command[:1] == ["--"] else args.command
    if not command:
        parser.error("a command after -- is required")
    result = capture_work(command, args.closure_timeout)
    temporary = args.output.with_suffix(args.output.suffix + ".tmp")
    temporary.write_text(json.dumps(result, sort_keys=True, separators=(",", ":")) + "\n", encoding="utf-8")
    os.replace(temporary, args.output)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
