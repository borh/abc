import json
import subprocess
import sys
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[1] / "audit-aat-delta.py"
LEGACY_WARNING = (
    "aozora upstream spans are sanitized-source byte offsets; "
    "line_start and line_end are synthesized as 1"
)


def meta(
    version="ab-aozora 0.2.0 aat-schema 1 facade 0.2.0 wire-schema 3 (git aaa)", warnings=None
):
    return {
        "adapter": "ab-aozora",
        "adapter_version": version,
        "parse_complete": True,
        "source_encoding": "utf-8",
        "source_hash": "sha256:x",
        "warnings": warnings or [],
    }


def doc(blocks, **meta_kw):
    return {"version": 1, "work_id": "stdin", "blocks": blocks, "meta": meta(**meta_kw)}


def text(value, start=0, end=None):
    end = (start + len(value.encode())) if end is None else end
    return {
        "kind": "text",
        "value": value,
        "span": {"byte_start": start, "byte_end": end, "line_start": 1, "line_end": 1},
    }


def raw_marker(source, marker_kind, start=0):
    return {
        "kind": "raw",
        "source": source,
        "x-source-marker-kind": marker_kind,
        "x-provenance": "parser-derived",
        "span": {
            "byte_start": start,
            "byte_end": start + len(source.encode()),
            "line_start": 1,
            "line_end": 1,
        },
    }


def para(*content):
    return {"kind": "paragraph", "content": list(content)}


def write_dump(tmp_path, name, docs):
    d = tmp_path / name
    d.mkdir()
    for work, document in docs.items():
        (d / f"{work}.json").write_bytes(
            json.dumps(document, ensure_ascii=False, sort_keys=True).encode() + b"\n"
        )
    return d


def run(mode, base, cand, tmp_path):
    out = tmp_path / "summary.json"
    proc = subprocess.run(
        [sys.executable, str(SCRIPT), mode, str(base), str(cand), "--summary-json", str(out)],
        capture_output=True,
        text=True,
    )
    summary = json.loads(out.read_text()) if out.exists() else None
    return proc.returncode, summary, proc.stderr


OPEN = "［＃ここから罫囲み］"
CLOSE = "［＃ここで罫囲み終わり］"


def test_identical_dumps_pass(tmp_path):
    docs = {"w1": doc([para(text("あ\n"))])}
    base = write_dump(tmp_path, "a", docs)
    cand = write_dump(tmp_path, "b", docs)
    code, summary, _ = run("container-rewrite", base, cand, tmp_path)
    assert code == 0 and summary["verdict"] == "PASS"


def test_identity_pointer_change_is_class3(tmp_path):
    base = write_dump(tmp_path, "a", {"w1": doc([para(text("あ\n"))])})
    cand = write_dump(
        tmp_path,
        "b",
        {
            "w1": doc(
                [para(text("あ\n"))],
                version="ab-aozora 0.2.0 aat-schema 1 facade 0.2.0 wire-schema 3 (git bbb)",
            )
        },
    )
    code, summary, _ = run("container-rewrite", base, cand, tmp_path)
    assert code == 0


def test_same_paragraph_pair_rewrites_to_container(tmp_path):
    # Baseline inner node "\n中身\n" (span 10..18); the grammar strips the
    # boundary newlines (value → "中身", span untouched — mirroring the Rust
    # strip helpers, which mutate values only) and strips the leading "\n"
    # of the post-close text.
    inner = text("\n中身\n", 10)  # span 10..18
    base = write_dump(
        tmp_path,
        "a",
        {
            "w1": doc(
                [
                    para(
                        text("前\n"),
                        raw_marker(OPEN, "containerOpen", 4),
                        inner,
                        raw_marker(CLOSE, "containerClose", 40),
                        text("\n後\n", 70),
                    )
                ]
            )
        },
    )
    cand = write_dump(
        tmp_path,
        "b",
        {
            "w1": doc(
                [
                    para(text("前\n")),
                    {"kind": "keigakomi_block", "children": [para(text("中身", 10, 18))]},
                    para(text("後\n", 70, 75)),
                ]
            )
        },
    )
    code, summary, err = run("container-rewrite", base, cand, tmp_path)
    assert code == 0, (summary, err)
    assert summary["classes"]["rewritten"] == 1


HEADING = {
    "kind": "heading",
    "level": 3,
    "style": "normal",
    "content": [text("見出し")],
    "x-provenance": "source-derived",
}


def test_cross_paragraph_pair_rewrites_to_container(tmp_path):
    # Open in one paragraph, close in a later paragraph, a non-paragraph
    # block in between: head strips its leading "\n", tail strips its
    # trailing "\n", the middle block is carried verbatim, and the
    # post-close text strips its leading "\n". ALL spans stay at their
    # baseline values (the Rust strip helpers mutate values only).
    base = write_dump(
        tmp_path,
        "a",
        {
            "w1": doc(
                [
                    para(text("前\n"), raw_marker(OPEN, "containerOpen", 4), text("\n頭", 40)),
                    HEADING,
                    para(
                        text("尾\n", 50),
                        raw_marker(CLOSE, "containerClose", 60),
                        text("\n後\n", 90),
                    ),
                ]
            )
        },
    )
    cand = write_dump(
        tmp_path,
        "b",
        {
            "w1": doc(
                [
                    para(text("前\n")),
                    {
                        "kind": "keigakomi_block",
                        "children": [para(text("頭", 40, 44)), HEADING, para(text("尾", 50, 54))],
                    },
                    para(text("後\n", 90, 95)),
                ]
            )
        },
    )
    code, summary, err = run("container-rewrite", base, cand, tmp_path)
    assert code == 0, (summary, err)
    assert summary["classes"]["rewritten"] == 1


def test_unrelated_change_in_marker_work_fails(tmp_path):
    inner = text("\n中身\n", 10)
    base = write_dump(
        tmp_path,
        "a",
        {
            "w1": doc(
                [
                    para(
                        text("前\n"),
                        raw_marker(OPEN, "containerOpen", 4),
                        inner,
                        raw_marker(CLOSE, "containerClose", 40),
                    )
                ]
            )
        },
    )
    cand = write_dump(
        tmp_path,
        "b",
        {
            "w1": doc(
                [
                    para(text("変わった\n")),  # unrelated text change smuggled in
                    {"kind": "keigakomi_block", "children": [para(text("中身", 10, 18))]},
                ]
            )
        },
    )
    code, _, _ = run("container-rewrite", base, cand, tmp_path)
    assert code == 2


def test_diff_without_markers_fails(tmp_path):
    base = write_dump(tmp_path, "a", {"w1": doc([para(text("あ\n"))])})
    cand = write_dump(tmp_path, "b", {"w1": doc([para(text("い\n"))])})
    code, _, _ = run("container-rewrite", base, cand, tmp_path)
    assert code == 2


def test_missing_file_fails(tmp_path):
    base = write_dump(tmp_path, "a", {"w1": doc([para(text("あ\n"))])})
    cand = write_dump(tmp_path, "b", {})
    (tmp_path / "b").mkdir(exist_ok=True)
    code, _, _ = run("container-rewrite", base, cand, tmp_path)
    assert code == 2


def style_chitsuki(content):
    # push_chitsuki_paragraph shape: no-span block-assembly wrapper.
    return {
        "kind": "style",
        "style_type": "chitsuki",
        "content": list(content),
        "x-align": "right",
        "x-offset": 0,
        "x-provenance": "source-derived",
    }


def style_span(content, start, end):
    # Inline style built in inline_content: CARRIES a span, existed whole
    # in the flat stream at strip time — Rust's boundary strip no-ops on it.
    return {
        "kind": "style",
        "style_type": "bold",
        "content": list(content),
        "span": {"byte_start": start, "byte_end": end, "line_start": 1, "line_end": 1},
    }


def test_tail_in_chitsuki_wrapper_stripped(tmp_path):
    # Close marker starts its paragraph (tail empty): the flat stream's
    # last inner node is the chitsuki wrapper's text — Rust stripped it
    # BEFORE the wrapper assembled. The head keeps its trailing newline.
    base = write_dump(
        tmp_path,
        "a",
        {
            "w1": doc(
                [
                    para(text("前\n"), raw_marker(OPEN, "containerOpen", 4), text("\n頭\n", 40)),
                    para(style_chitsuki([text("著者\n", 50)])),
                    para(raw_marker(CLOSE, "containerClose", 60), text("\n後\n", 90)),
                ]
            )
        },
    )
    cand = write_dump(
        tmp_path,
        "b",
        {
            "w1": doc(
                [
                    para(text("前\n")),
                    {
                        "kind": "keigakomi_block",
                        "children": [
                            para(text("頭\n", 40, 45)),
                            para(style_chitsuki([text("著者", 50, 57)])),
                        ],
                    },
                    para(text("後\n", 90, 95)),
                ]
            )
        },
    )
    code, summary, err = run("container-rewrite", base, cand, tmp_path)
    assert code == 0, (summary, err)
    assert summary["classes"]["rewritten"] == 1


def test_span_carrying_style_at_tail_not_stripped(tmp_path):
    # A span-carrying inline style at the tail existed whole in the flat
    # stream: Rust's strip no-ops on it. Candidate keeps the trailing
    # newline inside it too.
    inner_style = style_span([text("末尾\n", 20)], 20, 27)
    base = write_dump(
        tmp_path,
        "a",
        {
            "w1": doc(
                [
                    para(
                        text("前\n"),
                        raw_marker(OPEN, "containerOpen", 4),
                        text("\n中身", 10),
                        inner_style,
                        raw_marker(CLOSE, "containerClose", 40),
                        text("\n後\n", 70),
                    )
                ]
            )
        },
    )
    cand = write_dump(
        tmp_path,
        "b",
        {
            "w1": doc(
                [
                    para(text("前\n")),
                    {
                        "kind": "keigakomi_block",
                        "children": [para(text("中身", 10, 17), inner_style)],
                    },
                    para(text("後\n", 70, 75)),
                ]
            )
        },
    )
    code, summary, err = run("container-rewrite", base, cand, tmp_path)
    assert code == 0, (summary, err)
    assert summary["classes"]["rewritten"] == 1


def test_candidate_stripping_span_styled_tail_fails(tmp_path):
    # The other direction: a candidate that stripped INSIDE the
    # span-carrying style did something Rust does not do — exit 2.
    base = write_dump(
        tmp_path,
        "a",
        {
            "w1": doc(
                [
                    para(
                        text("前\n"),
                        raw_marker(OPEN, "containerOpen", 4),
                        text("\n中身", 10),
                        style_span([text("末尾\n", 20)], 20, 27),
                        raw_marker(CLOSE, "containerClose", 40),
                        text("\n後\n", 70),
                    )
                ]
            )
        },
    )
    cand = write_dump(
        tmp_path,
        "b",
        {
            "w1": doc(
                [
                    para(text("前\n")),
                    {
                        "kind": "keigakomi_block",
                        "children": [
                            para(text("中身", 10, 17), style_span([text("末尾", 20, 27)], 20, 27))
                        ],
                    },
                    para(text("後\n", 70, 75)),
                ]
            )
        },
    )
    code, _, _ = run("container-rewrite", base, cand, tmp_path)
    assert code == 2


def test_intervening_jisage_block_aborts_pairing(tmp_path):
    # A jisage_block between open and close was assembled FROM a
    # containerOpen marker: Rust's flat close-scan aborted on it, so the
    # pair is unadmitted and stays verbatim; the other pair still rewrites.
    jisage = {"kind": "jisage_block", "x-indent": 2, "children": [para(text("じ\n", 150))]}
    aborted_open = para(text("違\n", 100), raw_marker(OPEN, "containerOpen", 110))
    late_close = para(raw_marker(CLOSE, "containerClose", 200), text("x\n", 230))
    base = write_dump(
        tmp_path,
        "a",
        {
            "w1": doc(
                [
                    para(
                        text("あ\n"),
                        raw_marker(OPEN, "containerOpen", 4),
                        text("\n中身\n", 10),
                        raw_marker(CLOSE, "containerClose", 40),
                        text("\n後\n", 70),
                    ),
                    aborted_open,
                    jisage,
                    late_close,
                ]
            )
        },
    )
    cand = write_dump(
        tmp_path,
        "b",
        {
            "w1": doc(
                [
                    para(text("あ\n")),
                    {"kind": "keigakomi_block", "children": [para(text("中身", 10, 18))]},
                    para(text("後\n", 70, 75)),
                    aborted_open,
                    jisage,
                    late_close,
                ]
            )
        },
    )
    code, summary, err = run("container-rewrite", base, cand, tmp_path)
    assert code == 0, (summary, err)
    assert summary["classes"]["rewritten"] == 1


YOPEN = "［＃ここから横組み］"
YCLOSE = "［＃ここで横組み終わり］"


def test_nested_pair_inner_admitted_outer_raw(tmp_path):
    # 001558 corpus pattern: yokogumi open, keigakomi open, keigakomi
    # close, yokogumi close — all inline in ONE paragraph. Rust tries each
    # node in stream order: the yokogumi open is unadmitted (its close
    # scan aborts on the inner containerOpen) and stays raw; the inner
    # keigakomi pair IS admitted. The outer close also stays raw, in the
    # post paragraph.
    base = write_dump(
        tmp_path,
        "a",
        {
            "w1": doc(
                [
                    para(
                        text("あ\n"),
                        raw_marker(YOPEN, "containerOpen", 4),
                        text("\nX", 40),
                        raw_marker(OPEN, "containerOpen", 50),
                        text("\n中身\n", 80),
                        raw_marker(CLOSE, "containerClose", 100),
                        text("\nY", 130),
                        raw_marker(YCLOSE, "containerClose", 140),
                        text("\n後\n", 170),
                    )
                ]
            )
        },
    )
    cand = write_dump(
        tmp_path,
        "b",
        {
            "w1": doc(
                [
                    para(text("あ\n"), raw_marker(YOPEN, "containerOpen", 4), text("\nX", 40)),
                    {"kind": "keigakomi_block", "children": [para(text("中身", 80, 88))]},
                    para(
                        text("Y", 130, 132),
                        raw_marker(YCLOSE, "containerClose", 140),
                        text("\n後\n", 170),
                    ),
                ]
            )
        },
    )
    code, summary, err = run("container-rewrite", base, cand, tmp_path)
    assert code == 0, (summary, err)
    assert summary["classes"]["rewritten"] == 1


def test_post_close_chitsuki_paragraph_not_stripped(tmp_path):
    # Post empty (close ends its paragraph): Rust's post-close flag is
    # consumed by the next flat node — the chitsuki alignEnd MARKER, not
    # the text inside the wrapper. The wrapper text keeps its newline.
    chitsuki_para = para(style_chitsuki([text("\n地付き", 60)]))
    base = write_dump(
        tmp_path,
        "a",
        {
            "w1": doc(
                [
                    para(
                        text("前\n"),
                        raw_marker(OPEN, "containerOpen", 4),
                        text("\n中身\n", 10),
                        raw_marker(CLOSE, "containerClose", 40),
                    ),
                    chitsuki_para,
                ]
            )
        },
    )
    cand = write_dump(
        tmp_path,
        "b",
        {
            "w1": doc(
                [
                    para(text("前\n")),
                    {"kind": "keigakomi_block", "children": [para(text("中身", 10, 18))]},
                    chitsuki_para,
                ]
            )
        },
    )
    code, summary, err = run("container-rewrite", base, cand, tmp_path)
    assert code == 0, (summary, err)
    assert summary["classes"]["rewritten"] == 1


def test_top_level_array_is_reference_error(tmp_path):
    # Valid JSON, wrong shape: fail-closed (exit 2, never an uncaught
    # traceback / exit 1).
    base = write_dump(tmp_path, "a", {"w1": doc([para(text("あ\n"))])})
    cand_dir = tmp_path / "b"
    cand_dir.mkdir()
    (cand_dir / "w1.json").write_text("[1, 2, 3]\n")
    code, _, _ = run("container-rewrite", base, cand_dir, tmp_path)
    assert code == 2


def test_blocks_as_object_is_reference_error(tmp_path):
    # "blocks" as an object instead of a list: fail-closed (exit 2, never
    # an uncaught traceback / exit 1).
    base = write_dump(tmp_path, "a", {"w1": doc({"0": para(text("あ\n"))})})
    cand = write_dump(tmp_path, "b", {"w1": doc([para(text("い\n"))])})
    code, _, _ = run("container-rewrite", base, cand, tmp_path)
    assert code == 2


# --- span-confinement mode ---


def spanned(value, bs, be, ls, le):
    return {
        "kind": "text",
        "value": value,
        "span": {"byte_start": bs, "byte_end": be, "line_start": ls, "line_end": le},
    }


def test_span_only_change_passes(tmp_path):
    base = write_dump(
        tmp_path,
        "a",
        {
            "w1": doc(
                [para(spanned("あ\n", 0, 4, 1, 1))],
                warnings=[{"message": LEGACY_WARNING, "line": 1}],
            )
        },
    )
    cand = write_dump(tmp_path, "b", {"w1": doc([para(spanned("あ\n", 100, 104, 3, 3))])})
    code, summary, err = run("span-confinement", base, cand, tmp_path)
    assert code == 0, (summary, err)


def test_value_change_fails_in_span_mode(tmp_path):
    base = write_dump(tmp_path, "a", {"w1": doc([para(spanned("あ\n", 0, 4, 1, 1))])})
    cand = write_dump(tmp_path, "b", {"w1": doc([para(spanned("い\n", 0, 4, 1, 1))])})
    code, _, _ = run("span-confinement", base, cand, tmp_path)
    assert code == 2


def test_invalid_span_fails(tmp_path):
    base = write_dump(tmp_path, "a", {"w1": doc([para(spanned("あ\n", 0, 4, 1, 1))])})
    cand = write_dump(tmp_path, "b", {"w1": doc([para(spanned("あ\n", 4, 0, 1, 1))])})
    code, _, _ = run("span-confinement", base, cand, tmp_path)
    assert code == 2


def test_null_span_fails_in_span_mode(tmp_path):
    # A candidate node with "span": null must not slip past masking as a
    # false PASS — non-dict spans are rejected.
    base = write_dump(tmp_path, "a", {"w1": doc([para(spanned("あ\n", 0, 4, 1, 1))])})
    cand = write_dump(
        tmp_path, "b", {"w1": doc([para({"kind": "text", "value": "あ\n", "span": None})])}
    )
    code, _, _ = run("span-confinement", base, cand, tmp_path)
    assert code == 2


def test_wholesale_line1_synthesis_trips(tmp_path):
    nodes = [spanned(f"x{i}\n", i * 4, i * 4 + 3, 1, 1) for i in range(12)]
    base = write_dump(tmp_path, "a", {"w1": doc([para(*nodes)])})
    cand = write_dump(tmp_path, "b", {"w1": doc([para(*nodes)])})
    code, _, _ = run("span-confinement", base, cand, tmp_path)
    assert code == 2


# --- v2-migration mode (Phase 4, rotation C3) ---


def v2_doc(blocks, warnings=None):
    """A schema-v2-shaped candidate document (version 2, meta() reused for
    everything except the warnings shape, which v2 restructures)."""
    return {
        "version": 2,
        "work_id": "stdin",
        "blocks": blocks,
        "meta": meta(warnings=warnings),
    }


def test_v2_migration_mechanical_only_passes(tmp_path):
    base = write_dump(
        tmp_path,
        "a",
        {
            "w1": doc(
                [
                    {
                        "kind": "jisage_block",
                        "x-indent": 2,
                        "children": [para(text("本文\n", 0))],
                    }
                ],
                warnings=[{"message": "some_warning", "line": 3}],
            )
        },
    )
    cand = write_dump(
        tmp_path,
        "b",
        {
            "w1": v2_doc(
                [
                    {
                        "kind": "jisage_block",
                        "indent": 2,
                        "children": [para(text("本文\n", 0))],
                    }
                ],
                warnings=[
                    {
                        "code": "some-warning",
                        "severity": "warning",
                        "message": "some_warning",
                        "span": {
                            "byte_start": 0,
                            "byte_end": 1,
                            "line_start": 3,
                            "line_end": 3,
                        },
                    }
                ],
            )
        },
    )
    code, summary, err = run("v2-migration", base, cand, tmp_path)
    assert code == 0, (summary, err)
    assert summary["classes"]["migrated"] == 1


def test_v2_migration_rejects_changed_right_ruby(tmp_path):
    ruby_node = {
        "kind": "ruby",
        "base": "漢字",
        "reading": "かんじ",
        "direction": "right",
        "span": {"byte_start": 0, "byte_end": 10, "line_start": 1, "line_end": 1},
    }
    base = write_dump(tmp_path, "a", {"w1": doc([para(ruby_node)])})
    changed = dict(ruby_node, reading="ちがう")
    cand = write_dump(tmp_path, "b", {"w1": v2_doc([para(changed)])})
    code, _, _ = run("v2-migration", base, cand, tmp_path)
    assert code == 2


def test_v2_migration_left_ruby_upgrade(tmp_path):
    raw_ruby = raw_marker("名［＃「名」の左に「な」のルビ］", "ruby", 0)
    base = write_dump(tmp_path, "a", {"w1": doc([para(text("前\n", 100), raw_ruby)])})
    upgraded = {
        "kind": "ruby",
        "base": "名",
        "reading": "な",
        "direction": "left",
        "span": raw_ruby["span"],
    }
    cand = write_dump(tmp_path, "b", {"w1": v2_doc([para(text("前\n", 100), upgraded)])})
    code, summary, err = run("v2-migration", base, cand, tmp_path)
    assert code == 0, (summary, err)
    assert summary["classes"]["ruby_left_rewritten"] == 1


def test_v2_migration_rejects_unexpected_raw_to_ruby_upgrade(tmp_path):
    # Source does not match the left-ruby marker form at all — must survive
    # as raw; a candidate that upgrades it anyway is a defect.
    raw_ruby = raw_marker("［＃「変」の注記］", "ruby", 0)
    base = write_dump(tmp_path, "a", {"w1": doc([para(raw_ruby)])})
    wrongly_upgraded = {
        "kind": "ruby",
        "base": "変",
        "reading": "",
        "direction": "left",
        "span": raw_ruby["span"],
    }
    cand = write_dump(tmp_path, "b", {"w1": v2_doc([para(wrongly_upgraded)])})
    code, _, _ = run("v2-migration", base, cand, tmp_path)
    assert code == 2


JOPEN = "［＃ここから２１字詰め］"
JCLOSE = "［＃ここで字詰め終わり］"


def test_v2_migration_jizume_formation(tmp_path):
    inner = text("\n中身\n", 10)  # span 10..18
    base = write_dump(
        tmp_path,
        "a",
        {
            "w1": doc(
                [
                    para(
                        text("前\n"),
                        raw_marker(JOPEN, "containerOpen", 4),
                        inner,
                        raw_marker(JCLOSE, "containerClose", 40),
                        text("\n後\n", 70),
                    )
                ]
            )
        },
    )
    cand = write_dump(
        tmp_path,
        "b",
        {
            "w1": v2_doc(
                [
                    para(text("前\n")),
                    {
                        "kind": "jizume_block",
                        "width": 21,
                        "children": [para(text("中身", 10, 18))],
                    },
                    para(text("後\n", 70, 75)),
                ]
            )
        },
    )
    code, summary, err = run("v2-migration", base, cand, tmp_path)
    assert code == 0, (summary, err)
    assert summary["classes"]["jizume_rewritten"] == 1


def test_v2_migration_rejects_leftover_x_layout(tmp_path):
    base = write_dump(
        tmp_path,
        "a",
        {
            "w1": doc(
                [
                    {
                        "kind": "jisage_block",
                        "x-indent": 3,
                        "children": [para(text("本文\n", 0))],
                    }
                ]
            )
        },
    )
    cand = write_dump(
        tmp_path,
        "b",
        {
            "w1": v2_doc(
                [
                    {
                        "kind": "jisage_block",
                        "x-indent": 3,  # bug: never renamed to "indent"
                        "children": [para(text("本文\n", 0))],
                    }
                ]
            )
        },
    )
    code, _, _ = run("v2-migration", base, cand, tmp_path)
    assert code == 2


def test_v2_migration_warning_code_mismatch(tmp_path):
    base = write_dump(
        tmp_path,
        "a",
        {"w1": doc([para(text("あ\n"))], warnings=[{"message": "some_warning", "line": 1}])},
    )
    cand = write_dump(
        tmp_path,
        "b",
        {
            "w1": v2_doc(
                [para(text("あ\n"))],
                warnings=[
                    {
                        "code": "wrong-code",
                        "severity": "warning",
                        "message": "some_warning",
                        "span": {
                            "byte_start": 0,
                            "byte_end": 4,
                            "line_start": 1,
                            "line_end": 1,
                        },
                    }
                ],
            )
        },
    )
    code, _, _ = run("v2-migration", base, cand, tmp_path)
    assert code == 2


def burasage_style(content, first, rest):
    # push_burasage_paragraph shape, v1 (pre-rename) key names.
    return {
        "kind": "style",
        "style_type": "burasage",
        "content": list(content),
        "x-indent-first": first,
        "x-indent-rest": rest,
        "x-provenance": "source-derived",
    }


def test_v2_migration_compound_jizume_wrap(tmp_path):
    # v1's compound-container burasage classification already discards any
    # 字詰め clause on its marker (burasage_container_indent is unchanged
    # since the initial port) — so the baseline shape below is exactly what
    # a real v1 dump produces whether or not the original marker carried a
    # 字詰め clause. The audit cannot re-derive the width from baseline
    # alone; it adopts the candidate's jizume_block wrapper (verifying
    # width and exact wrapped-content invariants) mirroring Task 6's wrap.
    base_para = para(burasage_style([text("本文", 10, 16)], 6, 7))
    base = write_dump(tmp_path, "a", {"w1": doc([base_para])})
    cand_para = para(
        {
            "kind": "style",
            "style_type": "burasage",
            "content": [text("本文", 10, 16)],
            "indent_first": 6,
            "indent_rest": 7,
            "x-provenance": "source-derived",
        }
    )
    cand = write_dump(
        tmp_path,
        "b",
        {"w1": v2_doc([{"kind": "jizume_block", "width": 21, "children": [cand_para]}])},
    )
    code, summary, err = run("v2-migration", base, cand, tmp_path)
    assert code == 0, (summary, err)
    assert summary["classes"]["jizume_rewritten"] == 1
