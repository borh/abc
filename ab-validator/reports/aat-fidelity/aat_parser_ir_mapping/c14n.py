#!/usr/bin/env python
"""Canonical JSON helpers for ABC schema hashes.

abc-legacy-json-c14n-v0:
1. parse JSON;
2. serialize UTF-8 JSON with sorted object keys and compact separators;
3. escape every "/" as "\\/", including slashes inside string values;
4. SHA-256 the resulting bytes and prefix with "sha256:".
"""

from __future__ import annotations

import argparse
import hashlib
import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "lib"))
from legacy_json_c14n import canonical_json  # noqa: E402


def schema_hash(path: Path) -> str:
    value = json.loads(path.read_text(encoding="utf-8"))
    payload = canonical_json(value).encode("utf-8")
    return "sha256:" + hashlib.sha256(payload).hexdigest()


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("paths", type=Path, nargs="+")
    args = parser.parse_args()
    for path in args.paths:
        print(f"{path}\t{schema_hash(path)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
