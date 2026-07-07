"""Common evidence-record helpers for report scripts."""

from __future__ import annotations

from pathlib import Path
from typing import Any

from reports.lib.hashing import file_sha256
from reports.lib.io import read_json
from reports.lib.paths import display_path


def read_json_object(path: Path) -> dict[str, Any]:
    """Read a JSON file that must contain an object."""
    value = read_json(path)
    if not isinstance(value, dict):
        raise SystemExit(f"{path} must contain a JSON object")
    return value


def as_int(value: Any) -> int:
    """Read a report counter as an integer, treating absent values as zero."""
    if value is None:
        return 0
    if isinstance(value, bool):
        return int(value)
    if isinstance(value, int):
        return value
    if isinstance(value, float):
        return int(value)
    raise SystemExit(f"expected numeric value, got {value!r}")


def input_record(path: Path) -> dict[str, str]:
    """Return the standard report input evidence record for a file."""
    return {"path": display_path(path), "hash": file_sha256(path)}
