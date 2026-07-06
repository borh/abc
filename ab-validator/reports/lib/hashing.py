"""Content-addressable hashing utilities."""

from __future__ import annotations

import hashlib
from pathlib import Path


def sha256_hex(data: str | bytes) -> str:
    """Return a digest in the report-standard ``sha256:<hex>`` format."""
    if isinstance(data, str):
        data = data.encode("utf-8")
    return "sha256:" + hashlib.sha256(data).hexdigest()


def file_sha256(path: Path) -> str:
    """Return a file digest in the report-standard ``sha256:<hex>`` format."""
    return "sha256:" + hashlib.sha256(path.read_bytes()).hexdigest()
