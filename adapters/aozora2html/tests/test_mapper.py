"""Smoke and golden tests for the aozora2html adapter."""

from __future__ import annotations

import json
import re
import subprocess
from pathlib import Path

import jsonschema
import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
RUN_SH = REPO_ROOT / "adapters" / "aozora2html" / "aozora2html-adapter"
FIXTURE_DIR = Path(__file__).resolve().parent / "fixtures"
SCHEMA = json.loads((REPO_ROOT / "data" / "aat-schema.json").read_text())

# Minimal aozora-bunko-format text used for envelope verification.
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

FIXTURES = sorted(p.stem for p in FIXTURE_DIR.glob("*.txt"))


def _run(stdin_bytes: bytes, *args: str) -> bytes:
    return subprocess.check_output(
        ["bash", str(RUN_SH), *args],
        input=stdin_bytes,
    )


def _canonicalize(aat: dict) -> dict:
    """Strip fields that may legitimately drift between commits."""
    out = json.loads(json.dumps(aat))
    meta = out.get("meta", {})
    # adapter_version records the upstream parser identity; goldens should not
    # pin it.
    meta.pop("adapter_version", None)
    return out


def test_version_format() -> None:
    out = subprocess.check_output(
        ["bash", str(RUN_SH), "--version"], text=True
    ).strip()
    assert re.match(r"^aozora2html-adapter \d+\.\d+\.\d+ gem-\d+\.\d+\.\d+$", out), out


def test_envelope_passes_schema() -> None:
    raw = _run(MINIMAL_AOZORA.encode("utf-8"), "--mode", "aat")
    aat = json.loads(raw)
    jsonschema.validate(aat, SCHEMA)
    assert aat["meta"]["adapter"] == "aozora2html"
    assert aat["meta"]["source_encoding"] == "utf-8"
    assert aat["meta"]["source_hash"].startswith("sha256:")


def test_stdin_without_trailing_newline_is_normalized_for_parser() -> None:
    raw = _run(MINIMAL_AOZORA.rstrip("\n").encode("utf-8"), "--mode", "aat")
    aat = json.loads(raw)
    jsonschema.validate(aat, SCHEMA)
    assert aat["meta"]["parse_complete"] is True
    assert aat["blocks"]
    assert any(
        inline.get("kind") == "ruby"
        and inline.get("base") == "吾輩"
        and inline.get("reading") == "わがはい"
        for block in aat["blocks"]
        for inline in block.get("content", [])
    )


def test_fragment_stdin_is_wrapped_for_parser_without_changing_source_hash() -> None:
    fragment = "吾輩《わがはい》は猫である。"
    raw = _run(fragment.encode("utf-8"), "--mode", "aat")
    aat = json.loads(raw)
    jsonschema.validate(aat, SCHEMA)

    assert aat["meta"]["parse_complete"] is True
    assert aat["meta"]["source_hash"] == "sha256:ef709900663e4c7e77235b88422ceb252eb50283ff2872b3f93ed61133b45b6b"
    assert any(
        inline.get("kind") == "ruby"
        and inline.get("base") == "吾輩"
        and inline.get("reading") == "わがはい"
        for block in aat["blocks"]
        for inline in block.get("content", [])
    )


@pytest.mark.parametrize("fixture", FIXTURES)
def test_fixture_passes_aat_schema(fixture: str) -> None:
    txt_path = FIXTURE_DIR / f"{fixture}.txt"
    raw = _run(txt_path.read_bytes(), "--mode", "aat")
    aat = json.loads(raw)
    jsonschema.validate(aat, SCHEMA)


@pytest.mark.parametrize("fixture", FIXTURES)
def test_fixture_matches_golden(fixture: str) -> None:
    txt_path = FIXTURE_DIR / f"{fixture}.txt"
    golden_path = FIXTURE_DIR / f"{fixture}.aat.json"
    raw = _run(txt_path.read_bytes(), "--mode", "aat")
    actual = json.loads(raw)
    expected = json.loads(golden_path.read_text())
    assert _canonicalize(actual) == _canonicalize(expected)


if __name__ == "__main__":
    raise SystemExit(pytest.main([__file__, "-v"]))
