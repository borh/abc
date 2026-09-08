"""Read a resolved fidelity lock, the compute stage's sole dump-selection input.

The lock is emitted by `resolve-run-set.py` and consumed read-only here. It mirrors
`aat_runs`' resolution interface (`*_aat_dirs` / `*_aat_globs`) so compute tools swap
one for the other without touching their logic; the lock is a *closed value*: no
env, no CWD, no `/db` discovery happen when reading it (that is resolve's job).
"""

from __future__ import annotations

import json
import os
from pathlib import Path
from typing import Any

JsonObject = dict[str, Any]

LOCK_FORMAT = "fidelity-lock/v1"


def load_lock(path: str | os.PathLike[str]) -> JsonObject:
    """Load and shallow-validate a lock file. Fails closed on an unrecognised format."""
    lock = json.loads(Path(path).read_text(encoding="utf-8"))
    if not isinstance(lock, dict):
        raise ValueError(f"fidelity lock must be a JSON object: {path}")
    fmt = lock.get("lock_format")
    if fmt != LOCK_FORMAT:
        raise ValueError(
            f"unrecognised fidelity lock_format {fmt!r} (expected {LOCK_FORMAT!r}): {path}"
        )
    if not isinstance(lock.get("adapters"), dict) or not lock["adapters"]:
        raise ValueError(f"fidelity lock must contain a non-empty adapters object: {path}")
    return lock


def lock_adapters(lock: JsonObject) -> JsonObject:
    adapters = lock.get("adapters")
    if not isinstance(adapters, dict):
        raise ValueError("fidelity lock must contain an adapters object")
    return adapters


def lock_aat_dirs(lock: JsonObject, order: list[str] | None = None) -> dict[str, str]:
    """Return adapter label -> resolved AAT directory, in `order` (default: lock order)."""
    adapters = lock_adapters(lock)
    labels = order or list(adapters)
    dirs: dict[str, str] = {}
    for label in labels:
        entry = adapters.get(label)
        if not isinstance(entry, dict) or not isinstance(entry.get("aat_dir"), str):
            raise ValueError(f"fidelity lock: adapter {label}: missing resolved aat_dir")
        dirs[label] = entry["aat_dir"]
    return dirs


def lock_aat_globs(lock: JsonObject, order: list[str] | None = None) -> dict[str, str]:
    """Return adapter label -> resolved `*.json` glob."""
    return {
        label: str(Path(path) / "*.json")
        for label, path in lock_aat_dirs(lock, order=order).items()
    }


def lock_run_set_id(lock: JsonObject) -> str | None:
    value = lock.get("run_set_id")
    return value if isinstance(value, str) else None


def lock_db_root(lock: JsonObject) -> str | None:
    """The AB_DB_ROOT deployment binding recorded at resolve time (provenance)."""
    value = lock.get("db_root")
    return value if isinstance(value, str) else None
