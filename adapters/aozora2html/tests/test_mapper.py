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


def test_warichu_maps_to_warigaki_node() -> None:
    raw = _run((FIXTURE_DIR / "warichu_basic.txt").read_bytes(), "--mode", "aat")
    aat = json.loads(raw)
    jsonschema.validate(aat, SCHEMA)

    warigaki = [
        inline
        for block in aat["blocks"]
        for inline in block.get("content", [])
        if inline.get("kind") == "warigaki"
    ]
    assert warigaki == [
        {
            "kind": "warigaki",
            "upper": [{"kind": "text", "value": "注釈の上行"}],
            "lower": [{"kind": "text", "value": "注釈の下行"}],
        }
    ]


def test_jis_gaiji_image_fallback_is_source_derived_from_marker() -> None:
    source = "耳朶を※［＃「てへん＋掌」、第4水準2-13-47］えて"
    raw = _run(source.encode("utf-8"), "--mode", "aat")
    aat = json.loads(raw)
    jsonschema.validate(aat, SCHEMA)

    assert aat["blocks"] == [
        {
            "kind": "paragraph",
            "content": [
                {"kind": "text", "value": "耳朶を"},
                {
                    "kind": "gaiji",
                    "description": "「てへん＋掌」、第4水準2-13-47",
                    "resolved": "撑",
                    "jis_code": "2-13-47",
                    "unresolved_reason": None,
                    "x-provenance": "source-derived",
                },
                {"kind": "text", "value": "えて"},
            ],
        }
    ]


def test_unicode_gaiji_plain_text_is_source_derived_from_marker() -> None:
    source = "※［＃「口＋世」、U+546D］は珍しい字。"
    raw = _run(source.encode("utf-8"), "--mode", "aat")
    aat = json.loads(raw)
    jsonschema.validate(aat, SCHEMA)

    assert aat["blocks"] == [
        {
            "kind": "paragraph",
            "content": [
                {
                    "kind": "gaiji",
                    "description": "「口＋世」、U+546D",
                    "resolved": "呭",
                    "jis_code": None,
                    "unresolved_reason": None,
                    "x-provenance": "source-derived",
                },
                {"kind": "text", "value": "は珍しい字。"},
            ],
        }
    ]


def test_dakuten_katakana_gaiji_image_fallback_is_source_derived() -> None:
    source = "※［＃濁点付き片仮名ヱ、1-7-84］エル"
    raw = _run(source.encode("utf-8"), "--mode", "aat")
    aat = json.loads(raw)
    jsonschema.validate(aat, SCHEMA)

    assert aat["blocks"] == [
        {
            "kind": "paragraph",
            "content": [
                {
                    "kind": "gaiji",
                    "description": "濁点付き片仮名ヱ、1-7-84",
                    "resolved": "ヹ",
                    "jis_code": "1-7-84",
                    "unresolved_reason": None,
                    "x-provenance": "source-derived",
                },
                {"kind": "text", "value": "エル"},
            ],
        }
    ]


def test_source_note_image_maps_to_source_derived_figure() -> None:
    raw = _run((FIXTURE_DIR / "figure_image_caption.txt").read_bytes(), "--mode", "aat")
    aat = json.loads(raw)
    jsonschema.validate(aat, SCHEMA)

    figures = [
        inline
        for block in aat["blocks"]
        for inline in block.get("content", [])
        if inline.get("kind") == "figure"
    ]
    assert figures == [
        {
            "kind": "figure",
            "filename": "fig01.png",
            "alt": "挿絵",
            "css_class": "source-note",
            "width": 400,
            "height": 300,
            "caption": None,
            "x-provenance": "source-derived",
        }
    ]


def test_rendered_image_caption_attaches_caption_to_figure() -> None:
    source = "［＃「猫の図」のキャプション付きの図（fig00001_01.png、横321×縦123）入る］\n猫の図［＃「猫の図」はキャプション］"
    raw = _run(source.encode("utf-8"), "--mode", "aat")
    aat = json.loads(raw)
    jsonschema.validate(aat, SCHEMA)

    figures = [
        inline
        for block in aat["blocks"]
        for inline in block.get("content", [])
        if inline.get("kind") == "figure"
    ]
    assert figures == [
        {
            "kind": "figure",
            "filename": "fig00001_01.png",
            "alt": "猫の図",
            "css_class": "illustration",
            "width": 321,
            "height": 123,
            "caption": [{"kind": "text", "value": "猫の図"}],
            "x-caption-provenance": "source-derived",
        }
    ]


def test_inline_image_annotation_maps_to_figure() -> None:
    source = "猫の図（fig00001_01.png、横321×縦123）入る"
    raw = _run(source.encode("utf-8"), "--mode", "aat")
    aat = json.loads(raw)
    jsonschema.validate(aat, SCHEMA)

    assert aat["blocks"] == [
        {
            "kind": "paragraph",
            "content": [
                {
                    "kind": "figure",
                    "filename": "fig00001_01.png",
                    "alt": "猫の図",
                    "css_class": "source-text",
                    "width": 321,
                    "height": 123,
                    "caption": None,
                    "x-provenance": "source-derived",
                }
            ],
        }
    ]


def test_source_heading_note_maps_to_heading_block() -> None:
    source = "第一章［＃「第一章」は大見出し］"
    raw = _run(source.encode("utf-8"), "--mode", "aat")
    aat = json.loads(raw)
    jsonschema.validate(aat, SCHEMA)

    assert aat["blocks"] == [
        {
            "kind": "heading",
            "level": 1,
            "style": "normal",
            "content": [{"kind": "text", "value": "第一章"}],
            "x-provenance": "source-derived",
        }
    ]


def test_source_heading_no_particle_note_maps_to_heading_block() -> None:
    source = "序章［＃「序章」の大見出し］"
    raw = _run(source.encode("utf-8"), "--mode", "aat")
    aat = json.loads(raw)
    jsonschema.validate(aat, SCHEMA)

    assert aat["blocks"] == [
        {
            "kind": "heading",
            "level": 1,
            "style": "normal",
            "content": [{"kind": "text", "value": "序章"}],
            "x-provenance": "source-derived",
        }
    ]


@pytest.mark.parametrize(
    ("source", "level", "style"),
    [
        ("同行見出し［＃「同行見出し」は同行中見出し］", 2, "dogyo"),
        ("窓見出し［＃「窓見出し」は窓小見出し］", 3, "mado"),
    ],
)
def test_source_heading_note_preserves_heading_style(
    source: str, level: int, style: str
) -> None:
    raw = _run(source.encode("utf-8"), "--mode", "aat")
    aat = json.loads(raw)
    jsonschema.validate(aat, SCHEMA)

    assert aat["blocks"] == [
        {
            "kind": "heading",
            "level": level,
            "style": style,
            "content": [{"kind": "text", "value": source.split("［", 1)[0]}],
            "x-provenance": "source-derived",
        }
    ]


def test_explicit_line_break_note_maps_to_break_text() -> None:
    raw = _run("前［＃改行］後".encode("utf-8"), "--mode", "aat")
    aat = json.loads(raw)
    jsonschema.validate(aat, SCHEMA)

    assert aat["blocks"] == [
        {
            "kind": "paragraph",
            "content": [
                {
                    "kind": "text",
                    "value": "前\n後",
                    "x-break-kind": "line",
                    "x-provenance": "source-derived",
                }
            ],
        }
    ]


def test_page_break_note_maps_to_marker_paragraph() -> None:
    source = "前の段落。\n［＃改ページ］\n後の段落。"
    raw = _run(source.encode("utf-8"), "--mode", "aat")
    aat = json.loads(raw)
    jsonschema.validate(aat, SCHEMA)

    assert aat["blocks"] == [
        {"kind": "paragraph", "content": [{"kind": "text", "value": "前の段落。"}]},
        {
            "kind": "paragraph",
            "content": [],
            "x-break-kind": "page",
            "x-provenance": "source-derived",
        },
        {"kind": "paragraph", "content": [{"kind": "text", "value": "後の段落。"}]},
    ]


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
