"""Smoke tests for the aozora2html adapter skeleton (Task 1 Step 5)."""

from __future__ import annotations

import json
import re
import subprocess
from pathlib import Path

import jsonschema
import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
RUN_SH = REPO_ROOT / "adapters" / "aozora2html" / "run.sh"
SCHEMA = json.loads((REPO_ROOT / "data" / "aat-schema.json").read_text())

# Minimal aozora-bunko-format text. The header section ends at the first empty
# line; chuuki blocks delimit the body. Transcoding to Shift_JIS happens in run.sh.
MINIMAL_AOZORA = (
    "テスト\n"
    "著者\n"
    "\n"
    "-------------------------------------------------------\n"
    "凡例\n"
    "-------------------------------------------------------\n"
    "吾輩《わがはい》は猫である。\n"
    "底本：テスト\n"
)


def _run(stdin_bytes: bytes, *args: str) -> bytes:
    return subprocess.check_output(
        ["bash", str(RUN_SH), *args],
        input=stdin_bytes,
    )


def test_version_format() -> None:
    out = subprocess.check_output(["bash", str(RUN_SH), "--version"], text=True).strip()
    assert re.match(r"^aozora2html-adapter \d+\.\d+\.\d+ [0-9a-f]{7,40}$", out), out


def test_envelope_passes_schema() -> None:
    raw = _run(MINIMAL_AOZORA.encode("utf-8"), "--mode", "aat")
    aat = json.loads(raw)
    jsonschema.validate(aat, SCHEMA)
    assert aat["meta"]["adapter"] == "aozora2html"
    assert aat["meta"]["source_encoding"] == "utf-8"
    assert aat["meta"]["source_hash"].startswith("sha256:")


if __name__ == "__main__":
    raise SystemExit(pytest.main([__file__, "-v"]))
