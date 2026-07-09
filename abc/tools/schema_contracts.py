#!/usr/bin/env python
"""Compatibility wrapper for the monorepo schema contract generator."""

from __future__ import annotations

import runpy
import sys
from pathlib import Path


if __name__ == "__main__":
    root = Path(__file__).resolve().parents[2]
    sys.argv = [
        str(root / "scripts" / "abc_schema_contracts.py"),
        "--profile",
        "abc",
        *sys.argv[1:],
    ]
    runpy.run_path(sys.argv[0], run_name="__main__")
