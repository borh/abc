"""JSON and file I/O helpers for report scripts."""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any


def read_json(path: Path) -> Any:
    """Read and parse a JSON file."""
    return json.loads(path.read_text(encoding="utf-8"))


def write_json(path: Path, value: Any, *, indent: int = 2) -> None:
    """Write JSON, creating parent directories and preserving a trailing newline."""
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        json.dumps(value, indent=indent, ensure_ascii=False) + "\n",
        encoding="utf-8",
    )
