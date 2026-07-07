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
    out = subprocess.check_output(["bash", str(RUN_SH), "--version"], text=True).strip()
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
    assert (
        aat["meta"]["source_hash"]
        == "sha256:ef709900663e4c7e77235b88422ceb252eb50283ff2872b3f93ed61133b45b6b"
    )
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


def test_split_unresolved_gaiji_marker_is_source_derived() -> None:
    source = "小書きの※［＃小書き片仮名ン、237-11］もある。"
    raw = _run(source.encode("utf-8"), "--mode", "aat")
    aat = json.loads(raw)
    jsonschema.validate(aat, SCHEMA)

    assert aat["blocks"] == [
        {
            "kind": "paragraph",
            "content": [
                {"kind": "text", "value": "小書きの"},
                {
                    "kind": "gaiji",
                    "description": "小書き片仮名ン、237-11",
                    "resolved": "",
                    "jis_code": None,
                    "unresolved_reason": "unresolved",
                    "x-provenance": "source-derived",
                },
                {"kind": "text", "value": "もある。"},
            ],
        }
    ]


@pytest.mark.parametrize(
    ("source", "expected_content"),
    [
        (
            "胡麻塩おやじ［＃「おやじ」に白ゴマ傍点］",
            [
                {"kind": "text", "value": "胡麻塩"},
                {
                    "kind": "style",
                    "style_type": "boten",
                    "content": [{"kind": "text", "value": "おやじ"}],
                    "x-boten-kind": "white_sesame",
                    "x-provenance": "source-derived",
                },
            ],
        ),
        (
            "この傍線［＃「傍線」に二重傍線］です。",
            [
                {"kind": "text", "value": "この"},
                {
                    "kind": "style",
                    "style_type": "bousen",
                    "content": [{"kind": "text", "value": "傍線"}],
                    "x-line-kind": "double",
                    "x-provenance": "source-derived",
                },
                {"kind": "text", "value": "です。"},
            ],
        ),
        (
            "太字［＃「太字」は太字］と斜体［＃「斜体」は斜体］",
            [
                {
                    "kind": "style",
                    "style_type": "bold",
                    "content": [{"kind": "text", "value": "太字"}],
                    "x-provenance": "source-derived",
                },
                {"kind": "text", "value": "と"},
                {
                    "kind": "style",
                    "style_type": "italic",
                    "content": [{"kind": "text", "value": "斜体"}],
                    "x-provenance": "source-derived",
                },
            ],
        ),
        (
            "大きい［＃「大きい」は2段階大きな文字］",
            [
                {
                    "kind": "font_size",
                    "size_type": "larger",
                    "level": 2,
                    "content": [{"kind": "text", "value": "大きい"}],
                    "x-provenance": "source-derived",
                },
            ],
        ),
        (
            "囲み［＃「囲み」は罫囲み］",
            [
                {
                    "kind": "keigakomi",
                    "content": [{"kind": "text", "value": "囲み"}],
                    "x-provenance": "source-derived",
                },
            ],
        ),
        (
            "語［＃「語」の左に傍点］",
            [
                {
                    "kind": "style",
                    "style_type": "boten",
                    "content": [{"kind": "text", "value": "語"}],
                    "x-placement": "left",
                    "x-provenance": "source-derived",
                },
            ],
        ),
    ],
)
def test_inline_decoration_classes_are_source_derived_to_aat_nodes(
    source: str,
    expected_content: list[dict],
) -> None:
    raw = _run(source.encode("utf-8"), "--mode", "aat")
    aat = json.loads(raw)
    jsonschema.validate(aat, SCHEMA)

    assert aat["blocks"] == [{"kind": "paragraph", "content": expected_content}]


@pytest.mark.parametrize(
    ("source", "expected_content"),
    [
        (
            "［＃ここから太字］\n太字\n［＃ここで太字終わり］\n［＃ここから斜体］\n斜体\n［＃ここで斜体終わり］",
            [
                {
                    "kind": "style",
                    "style_type": "bold",
                    "content": [{"kind": "text", "value": "太字"}],
                    "x-provenance": "source-derived",
                },
                {
                    "kind": "style",
                    "style_type": "italic",
                    "content": [{"kind": "text", "value": "斜体"}],
                    "x-provenance": "source-derived",
                },
            ],
        ),
        (
            "［＃ここから2段階大きな文字］\n大きい\n［＃ここで大きな文字終わり］",
            [
                {
                    "kind": "font_size",
                    "size_type": "larger",
                    "level": 2,
                    "content": [{"kind": "text", "value": "大きい"}],
                    "x-provenance": "source-derived",
                },
            ],
        ),
    ],
)
def test_block_decoration_classes_are_source_derived_to_aat_nodes(
    source: str,
    expected_content: list[dict],
) -> None:
    raw = _run(source.encode("utf-8"), "--mode", "aat")
    aat = json.loads(raw)
    jsonschema.validate(aat, SCHEMA)

    assert aat["blocks"] == [{"kind": "paragraph", "content": expected_content}]


@pytest.mark.parametrize(
    ("source", "expected_blocks"),
    [
        (
            "字下げ行［＃この行2字下げ］",
            [
                {
                    "kind": "paragraph",
                    "content": [
                        {
                            "kind": "style",
                            "style_type": "jisage_line",
                            "content": [{"kind": "text", "value": "字下げ行"}],
                            "x-indent": 2,
                            "x-provenance": "source-derived",
                        }
                    ],
                }
            ],
        ),
        (
            "右寄せ［＃この行地付き］",
            [
                {
                    "kind": "paragraph",
                    "content": [
                        {
                            "kind": "style",
                            "style_type": "chitsuki",
                            "content": [{"kind": "text", "value": "右寄せ"}],
                            "x-align": "right",
                            "x-provenance": "source-derived",
                        }
                    ],
                }
            ],
        ),
        (
            "［＃ここから字詰め4］\n本文\n［＃ここで字詰め終わり］",
            [
                {
                    "kind": "paragraph",
                    "content": [
                        {
                            "kind": "style",
                            "style_type": "jizume",
                            "content": [{"kind": "text", "value": "本文"}],
                            "x-width": 4,
                            "x-provenance": "source-derived",
                        }
                    ],
                }
            ],
        ),
        (
            "［＃ここから2字下げ、折り返して4字下げ］\n本文\n［＃ここで字下げ終わり］",
            [
                {
                    "kind": "paragraph",
                    "content": [
                        {
                            "kind": "style",
                            "style_type": "burasage",
                            "content": [{"kind": "text", "value": "本文"}],
                            "x-indent-first": 2,
                            "x-indent-rest": 4,
                            "x-provenance": "source-derived",
                        }
                    ],
                }
            ],
        ),
        (
            "［＃ここから２字下げ］\n字下げされた段落。\n［＃ここで字下げ終わり］",
            [
                {
                    "kind": "jisage_block",
                    "children": [
                        {
                            "kind": "paragraph",
                            "content": [{"kind": "text", "value": "字下げされた段落。"}],
                        }
                    ],
                    "x-indent": 2,
                }
            ],
        ),
    ],
)
def test_indentation_source_notes_are_source_derived(
    source: str,
    expected_blocks: list[dict],
) -> None:
    raw = _run(source.encode("utf-8"), "--mode", "aat")
    aat = json.loads(raw)
    jsonschema.validate(aat, SCHEMA)

    assert aat["blocks"] == expected_blocks


@pytest.mark.parametrize(
    ("source", "expected_content"),
    [
        (
            "これは※［＃底本では欠字］である。",
            [
                {"kind": "text", "value": "これは"},
                {
                    "kind": "gaiji",
                    "description": "底本では欠字",
                    "resolved": "",
                    "jis_code": None,
                    "unresolved_reason": "unresolved",
                    "x-provenance": "source-derived",
                },
                {"kind": "text", "value": "である。"},
            ],
        ),
        (
            "／＼",
            [
                {
                    "kind": "gaiji",
                    "description": "くの字点",
                    "resolved": "〳〵",
                    "jis_code": None,
                    "unresolved_reason": None,
                    "x-provenance": "source-derived",
                }
            ],
        ),
        (
            "繁雑な日本の 〔e'tiquette〕 も、",
            [
                {"kind": "text", "value": "繁雑な日本の "},
                {
                    "kind": "accent",
                    "code": "1-09-63",
                    "name": "アキュートアクセント付きE小文字",
                    "resolved": "é",
                    "x-provenance": "source-derived",
                },
                {"kind": "text", "value": "tiquette も、"},
            ],
        ),
        (
            "漢［＃返り点一］文",
            [
                {"kind": "text", "value": "漢"},
                {
                    "kind": "style",
                    "style_type": "kaeriten",
                    "content": [],
                    "x-marker": "一",
                    "x-provenance": "source-derived",
                },
                {"kind": "text", "value": "文"},
            ],
        ),
        (
            "本文［＃左頁］続き",
            [
                {"kind": "text", "value": "本文"},
                {"kind": "text", "value": "", "x-editor-note": "左頁"},
                {"kind": "text", "value": "続き"},
            ],
        ),
    ],
)
def test_small_inline_source_markers_are_source_derived(
    source: str,
    expected_content: list[dict],
) -> None:
    raw = _run(source.encode("utf-8"), "--mode", "aat")
    aat = json.loads(raw)
    jsonschema.validate(aat, SCHEMA)

    assert aat["blocks"] == [{"kind": "paragraph", "content": expected_content}]


@pytest.mark.parametrize(
    ("source", "expected_content"),
    [
        (
            "ABC［＃「ABC」の横組み］",
            [
                {
                    "kind": "yokogumi",
                    "content": [{"kind": "text", "value": "ABC"}],
                    "x-provenance": "source-derived",
                }
            ],
        ),
        (
            "12［＃「12」の縦中横］",
            [
                {
                    "kind": "tcy",
                    "content": [{"kind": "text", "value": "12"}],
                    "x-provenance": "source-derived",
                }
            ],
        ),
        (
            "［＃ここから縦中横］\n12\n［＃ここで縦中横終わり］",
            [
                {
                    "kind": "tcy",
                    "content": [{"kind": "text", "value": "12"}],
                    "x-provenance": "source-derived",
                }
            ],
        ),
    ],
)
def test_layout_source_markers_are_source_derived(
    source: str,
    expected_content: list[dict],
) -> None:
    raw = _run(source.encode("utf-8"), "--mode", "aat")
    aat = json.loads(raw)
    jsonschema.validate(aat, SCHEMA)

    assert aat["blocks"] == [{"kind": "paragraph", "content": expected_content}]


@pytest.mark.parametrize(
    ("source", "expected_blocks", "expected_syntax"),
    [
        (
            "本文に［＃割書］注［＃割書終わり］が入る。",
            [
                {
                    "kind": "paragraph",
                    "content": [
                        {"kind": "text", "value": "本文に"},
                        {
                            "kind": "warigaki",
                            "upper": [{"kind": "text", "value": "注"}],
                            "lower": [],
                            "x-provenance": "source-derived",
                        },
                        {"kind": "text", "value": "が入る。"},
                    ],
                }
            ],
            [
                {
                    "kind": "warigaki",
                    "value": {
                        "lower_projection": "",
                        "upper_projection": "注",
                    },
                    "provenance": "source-derived",
                }
            ],
        ),
        (
            "米《べー》リンスキー［＃ここから割り注］魯国の批評家［＃ここで割り注終わり］",
            [
                {
                    "kind": "paragraph",
                    "content": [
                        {
                            "kind": "ruby",
                            "base": "米",
                            "reading": "べー",
                            "direction": "right",
                        },
                        {"kind": "text", "value": "リンスキー"},
                        {
                            "kind": "warigaki",
                            "upper": [{"kind": "text", "value": "魯国の批評家"}],
                            "lower": [],
                            "x-provenance": "source-derived",
                        },
                    ],
                }
            ],
            [
                {
                    "kind": "warigaki",
                    "value": {
                        "lower_projection": "",
                        "upper_projection": "魯国の批評家",
                    },
                    "provenance": "source-derived",
                }
            ],
        ),
        (
            "［＃ここから割り注］上［＃改行］下［＃ここで割り注終わり］",
            [
                {
                    "kind": "paragraph",
                    "content": [
                        {
                            "kind": "warigaki",
                            "upper": [{"kind": "text", "value": "上"}],
                            "lower": [{"kind": "text", "value": "下"}],
                            "x-provenance": "source-derived",
                        }
                    ],
                }
            ],
            [
                {
                    "kind": "warigaki",
                    "value": {
                        "lower_projection": "下",
                        "upper_projection": "上",
                    },
                    "provenance": "source-derived",
                }
            ],
        ),
        (
            "［＃割り注］注［＃割り注終わり］",
            [
                {
                    "kind": "paragraph",
                    "content": [
                        {
                            "kind": "warigaki",
                            "upper": [{"kind": "text", "value": "注"}],
                            "lower": [],
                            "x-provenance": "source-derived",
                        }
                    ],
                }
            ],
            [
                {
                    "kind": "warigaki",
                    "value": {
                        "lower_projection": "",
                        "upper_projection": "注",
                    },
                    "provenance": "source-derived",
                }
            ],
        ),
        (
            "［＃ここからキャプション］\n猫の図\n［＃ここでキャプション終わり］",
            [
                {
                    "kind": "caption_block",
                    "children": [
                        {
                            "kind": "paragraph",
                            "content": [{"kind": "text", "value": "猫の図"}],
                        }
                    ],
                    "x-provenance": "source-derived",
                }
            ],
            None,
        ),
    ],
)
def test_warigaki_and_caption_source_markers_are_source_derived(
    source: str,
    expected_blocks: list[dict],
    expected_syntax: list[dict] | None,
) -> None:
    raw = _run(source.encode("utf-8"), "--mode", "aat")
    aat = json.loads(raw)
    jsonschema.validate(aat, SCHEMA)

    assert aat["blocks"] == expected_blocks
    assert (
        aat["meta"]["semantic_summary"]["syntax"].get("warigaki.parenthetical") == expected_syntax
    )


@pytest.mark.parametrize(
    ("source", "expected_content"),
    [
        (
            "青空文庫［＃「青空文庫」の左に「あおぞらぶんこ」のルビ］",
            [
                {
                    "kind": "ruby",
                    "base": "青空文庫",
                    "reading": "あおぞらぶんこ",
                    "direction": "left",
                    "x-provenance": "source-derived",
                }
            ],
        ),
        (
            "青空文庫《あおぞらぶんこ》［＃「青空文庫」の左に「aozora bunko」のルビ］",
            [
                {
                    "kind": "ruby",
                    "base": "青空文庫",
                    "reading": "あおぞらぶんこ",
                    "direction": "right",
                    "x-left-reading": "aozora bunko",
                    "x-provenance": "source-derived",
                }
            ],
        ),
        (
            "※［＃「口＋愛」、第3水準1-15-23］《おくび》が出た。",
            [
                {
                    "kind": "ruby",
                    "base": "噯",
                    "reading": "おくび",
                    "direction": "right",
                    "base_content": [
                        {
                            "kind": "gaiji",
                            "description": "「口＋愛」、第3水準1-15-23",
                            "resolved": "噯",
                            "jis_code": "1-15-23",
                            "unresolved_reason": None,
                            "x-provenance": "source-derived",
                        }
                    ],
                    "x-provenance": "source-derived",
                },
                {"kind": "text", "value": "が出た。"},
            ],
        ),
        (
            "吹喋［＃「喋」の「ママ」の注記］",
            [
                {"kind": "text", "value": "吹"},
                {
                    "kind": "ruby",
                    "base": "喋",
                    "reading": "ママ",
                    "direction": "right",
                    "x-annotation-type": "chuuki",
                    "x-provenance": "source-derived",
                },
            ],
        ),
        (
            "支部長の顔にさっと血が流れ［＃「血が流れ」に「×」の傍記］た",
            [
                {"kind": "text", "value": "支部長の顔にさっと"},
                {
                    "kind": "ruby",
                    "base": "血が流れ",
                    "reading": "××××",
                    "direction": "right",
                    "x-annotation-type": "bouki",
                    "x-provenance": "source-derived",
                },
                {"kind": "text", "value": "た"},
            ],
        ),
        (
            "漢［＃訓点送り仮名「読」］文",
            [
                {"kind": "text", "value": "漢"},
                {
                    "kind": "ruby",
                    "base": "",
                    "reading": "読",
                    "direction": "right",
                    "x-annotation-type": "okurigana",
                    "x-provenance": "source-derived",
                },
                {"kind": "text", "value": "文"},
            ],
        ),
        (
            "参照［＃「参照」に「強調」の傍点］",
            [
                {
                    "kind": "style",
                    "style_type": "boten",
                    "content": [{"kind": "text", "value": "参照"}],
                    "x-frontref": "強調",
                    "x-provenance": "source-derived",
                }
            ],
        ),
        (
            "胡麻塩おやじ［＃「おやじ」に傍点］",
            [
                {"kind": "text", "value": "胡麻塩"},
                {
                    "kind": "style",
                    "style_type": "boten",
                    "content": [{"kind": "text", "value": "おやじ"}],
                    "x-provenance": "source-derived",
                },
            ],
        ),
    ],
)
def test_ruby_and_reference_source_notes_are_source_derived(
    source: str,
    expected_content: list[dict],
) -> None:
    raw = _run(source.encode("utf-8"), "--mode", "aat")
    aat = json.loads(raw)
    jsonschema.validate(aat, SCHEMA)

    assert aat["blocks"] == [{"kind": "paragraph", "content": expected_content}]


def test_nested_ruby_note_is_preserved_as_source_derived_raw() -> None:
    source = (
        "青空文庫《あおぞらぶんこ》［＃「青空文庫《あおぞらぶんこ》」の左に「aozora bunko」のルビ］"
    )
    raw = _run(source.encode("utf-8"), "--mode", "aat")
    aat = json.loads(raw)
    jsonschema.validate(aat, SCHEMA)

    assert any(
        node.get("kind") == "raw" and node.get("x-error-kind") == "nested_ruby_forbidden"
        for block in aat["blocks"]
        for node in block.get("content", [])
    )


def test_ruby_base_excludes_embedded_source_notes() -> None:
    source = "｜宜引［＃「引」は小書き右寄せ］縞《いいしま》"
    raw = _run(source.encode("utf-8"), "--mode", "aat")
    aat = json.loads(raw)
    jsonschema.validate(aat, SCHEMA)

    assert aat["blocks"] == [
        {
            "kind": "paragraph",
            "content": [
                {
                    "kind": "ruby",
                    "base": "宜引縞",
                    "reading": "いいしま",
                    "direction": "right",
                }
            ],
        }
    ]


def test_ruby_base_excludes_embedded_kaeriten() -> None:
    source = "｜先自侮而後人侮［＃レ］之《まずみずからあなどる》"
    raw = _run(source.encode("utf-8"), "--mode", "aat")
    aat = json.loads(raw)
    jsonschema.validate(aat, SCHEMA)

    assert aat["blocks"] == [
        {
            "kind": "paragraph",
            "content": [
                {
                    "kind": "ruby",
                    "base": "先自侮而後人侮之",
                    "reading": "まずみずからあなどる",
                    "direction": "right",
                    "base_content": [
                        {"kind": "text", "value": "先自侮而後人侮"},
                        {
                            "kind": "style",
                            "style_type": "kaeriten",
                            "content": [],
                            "x-marker": "レ",
                            "x-provenance": "parser",
                        },
                        {"kind": "text", "value": "之"},
                    ],
                }
            ],
        }
    ]


def test_unmatched_ruby_base_delimiter_is_not_visible() -> None:
    source = "八ヶ月もの間｜空家になっていたんです。"
    raw = _run(source.encode("utf-8"), "--mode", "aat")
    aat = json.loads(raw)
    jsonschema.validate(aat, SCHEMA)

    assert aat["blocks"] == [
        {
            "kind": "paragraph",
            "content": [{"kind": "text", "value": "八ヶ月もの間空家になっていたんです。"}],
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
def test_source_heading_note_preserves_heading_style(source: str, level: int, style: str) -> None:
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
