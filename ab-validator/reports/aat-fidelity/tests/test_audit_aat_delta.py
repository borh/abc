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


def run(mode, base, cand, tmp_path, *extra):
    out = tmp_path / "summary.json"
    argv = [sys.executable, str(SCRIPT), mode, str(base), str(cand), "--summary-json", str(out)]
    argv.extend(extra)
    proc = subprocess.run(argv, capture_output=True, text=True)
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


def test_v2_migration_rejects_prefixed_left_ruby_marker(tmp_path):
    # `pre` = "前文" is neither "" nor base ("X") — this is NOT an
    # admissible left-ruby marker per the anchored regex, so the raw node
    # must survive unchanged. A candidate that upgrades it anyway (the old
    # unanchored `.search()` would have falsely accepted this) is rejected.
    raw_ruby = raw_marker("前文［＃「X」の左に「Y」のルビ］", "ruby", 0)
    base = write_dump(tmp_path, "a", {"w1": doc([para(raw_ruby)])})
    wrongly_upgraded = {
        "kind": "ruby",
        "base": "X",
        "reading": "Y",
        "direction": "left",
        "span": raw_ruby["span"],
    }
    cand = write_dump(tmp_path, "b", {"w1": v2_doc([para(wrongly_upgraded)])})
    code, _, _ = run("v2-migration", base, cand, tmp_path)
    assert code == 2


def test_v2_migration_echoed_base_left_ruby_still_upgrades(tmp_path):
    # `pre` = "名" equals base ("名") — the echoed-base prefix form is still
    # admissible and upgrades, even under the anchored regex.
    raw_ruby = raw_marker("名［＃「名」の左に「な」のルビ］", "ruby", 0)
    base = write_dump(tmp_path, "a", {"w1": doc([para(raw_ruby)])})
    upgraded = {
        "kind": "ruby",
        "base": "名",
        "reading": "な",
        "direction": "left",
        "span": raw_ruby["span"],
    }
    cand = write_dump(tmp_path, "b", {"w1": v2_doc([para(upgraded)])})
    code, summary, err = run("v2-migration", base, cand, tmp_path)
    assert code == 0, (summary, err)


JOPEN = "［＃ここから２１字詰め］"
JCLOSE = "［＃ここで字詰め終わり］"


def _line_width_base(tmp_path):
    # A standalone `［＃ここからN字詰め］ … ［＃ここで字詰め終わり］` pair as a v1
    # baseline leaves it a RAW containerOpen/containerClose pair — Phase 3 (and,
    # post-C3-fix, Phase 4) never form a jizume_block from the standalone
    # line-width form (spec §6.6, `line-width-open`).
    inner = text("\n中身\n", 10)  # span 10..18
    return write_dump(
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


def test_v2_migration_line_width_container_stays_raw(tmp_path):
    # C3 gate fix (mirror of the Rust `line_width_container` pin): the
    # standalone `字詰め` line-width form migrates MECHANICALLY only — the
    # candidate keeps the raw containerOpen/containerClose pair, so the doc
    # lands in `migrated`, never `jizume_rewritten`.
    base = _line_width_base(tmp_path)
    cand = write_dump(
        tmp_path,
        "b",
        {
            "w1": v2_doc(
                [
                    para(
                        text("前\n"),
                        raw_marker(JOPEN, "containerOpen", 4),
                        text("\n中身\n", 10),
                        raw_marker(JCLOSE, "containerClose", 40),
                        text("\n後\n", 70),
                    )
                ]
            )
        },
    )
    code, summary, err = run("v2-migration", base, cand, tmp_path)
    assert code == 0, (summary, err)
    assert summary["classes"]["migrated"] == 1
    assert summary["classes"]["jizume_rewritten"] == 0
    assert summary["details"].get("compound_jizume_adopted", 0) == 0


def test_v2_migration_rejects_standalone_jizume_block(tmp_path):
    # A candidate that forms a jizume_block from the standalone line-width
    # form is the over-match the C3 gate caught — it must be REJECTED, since
    # the forward rewrite leaves the pair raw.
    base = _line_width_base(tmp_path)
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
    code, _, _ = run("v2-migration", base, cand, tmp_path)
    assert code == 2


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
    assert summary["details"]["compound_jizume_adopted"] == 1


def test_v2_migration_rejects_warning_missing_span_when_line_present(tmp_path):
    # Baseline warning carries a line; candidate warning has no span at
    # all — the candidate must carry a dict span whenever baseline had a
    # line, so this is rejected (was previously silently accepted since the
    # old check only fired when a dict span happened to already be present).
    base = write_dump(
        tmp_path,
        "a",
        {"w1": doc([para(text("あ\n"))], warnings=[{"message": "some_warning", "line": 5}])},
    )
    cand = write_dump(
        tmp_path,
        "b",
        {
            "w1": v2_doc(
                [para(text("あ\n"))],
                warnings=[
                    {
                        "code": "some-warning",
                        "severity": "warning",
                        "message": "some_warning",
                    }
                ],
            )
        },
    )
    code, _, _ = run("v2-migration", base, cand, tmp_path)
    assert code == 2


def typed_left_ruby(base_text, reading, span):
    return {
        "kind": "ruby",
        "base": base_text,
        "reading": reading,
        "direction": "left",
        "span": span,
    }


def test_v2_migration_gaiji_base_left_ruby_upgrades(tmp_path):
    # Failure shape 1 (corpus 001395_49905): the left-ruby BASE is itself an
    # embedded gaiji reference `※［＃「漸／耳」、第4水準2-85-15］`. Its inner `」`
    # and `］` are LITERAL base text — the parser resolves the base via
    # `alloc.content_plain` and `ruby_node` emits a typed `direction:"left"`
    # node verbatim. The old `[^」]`/`[^］]` char-class regex mis-anchored on
    # the inner brackets and left the node raw; the structural parser types it.
    src = "［＃「※［＃「漸／耳」、第4水準2-85-15］」の左に「にい」のルビ］"
    raw_ruby = raw_marker(src, "ruby", 200)
    base = write_dump(tmp_path, "a", {"w1": doc([para(text("前\n", 100, 107), raw_ruby)])})
    upgraded = typed_left_ruby("※［＃「漸／耳」、第4水準2-85-15］", "にい", raw_ruby["span"])
    cand = write_dump(tmp_path, "b", {"w1": v2_doc([para(text("前\n", 100, 107), upgraded)])})
    code, summary, err = run("v2-migration", base, cand, tmp_path)
    assert code == 0, (summary, err)
    assert summary["classes"]["ruby_left_rewritten"] == 1


def test_v2_migration_compound_gaiji_base_left_ruby_upgrades(tmp_path):
    # Failure shape 2 (corpus 001395_49891): the base is a COMPOUND run with a
    # gaiji in the middle — `銅※［＃「金＋拔のつくり」、第3水準1-93-6］子` — again
    # captured whole by the structural split, not the char-class regex.
    src = "［＃「銅※［＃「金＋拔のつくり」、第3水準1-93-6］子」の左に「どびょうし」のルビ］"
    raw_ruby = raw_marker(src, "ruby", 300)
    base = write_dump(tmp_path, "a", {"w1": doc([para(text("や\n", 100, 104), raw_ruby)])})
    upgraded = typed_left_ruby(
        "銅※［＃「金＋拔のつくり」、第3水準1-93-6］子", "どびょうし", raw_ruby["span"]
    )
    cand = write_dump(tmp_path, "b", {"w1": v2_doc([para(text("や\n", 100, 104), upgraded)])})
    code, summary, err = run("v2-migration", base, cand, tmp_path)
    assert code == 0, (summary, err)
    assert summary["classes"]["ruby_left_rewritten"] == 1


def chitsuki_v2(content):
    # push_chitsuki_paragraph shape after the v2 key rename (x-align→align,
    # x-offset→offset_from_end): the candidate-side chitsuki wrapper.
    return {
        "kind": "style",
        "style_type": "chitsuki",
        "content": list(content),
        "align": "right",
        "offset_from_end": 0,
        "x-provenance": "source-derived",
    }


def test_v2_migration_chitsuki_left_ruby_line_remerges(tmp_path):
    # Failure shape 3 (corpus 000933_47550): a raw left-ruby marker inside a
    # 地付き (chitsuki) line was `find_next_raw_boundary` in v1, so it ended the
    # chitsuki run and the marker + trailing text spilled into a following
    # plain paragraph. In v2 the marker is a typed (non-raw) ruby, so the
    # chitsuki line extends through it and the trailing text. The forward
    # rewrite must re-absorb that paragraph into the chitsuki style content.
    right_ruby = {
        "kind": "ruby",
        "base": "食",
        "reading": "ヲセ",
        "direction": "right",
        "span": {"byte_start": 510, "byte_end": 537, "line_start": 1, "line_end": 1},
    }
    marker = raw_marker("［＃「食」の左に「クヘ」のルビ］", "ruby", 537)
    left_span = marker["span"]
    base = write_dump(
        tmp_path,
        "a",
        {
            "w1": doc(
                [
                    para(style_chitsuki([text("\n汁", 500, 510), right_ruby])),
                    para(marker, text("と　すゝめ", 594)),
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
                    para(
                        chitsuki_v2(
                            [
                                text("\n汁", 500, 510),
                                right_ruby,
                                typed_left_ruby("食", "クヘ", left_span),
                                text("と　すゝめ", 594),
                            ]
                        )
                    ),
                ]
            )
        },
    )
    code, summary, err = run("v2-migration", base, cand, tmp_path)
    assert code == 0, (summary, err)
    assert summary["classes"]["ruby_left_rewritten"] == 1


def test_v2_migration_chitsuki_left_ruby_remerge_stops_at_inner_raw(tmp_path):
    # Re-merge is bounded by the NEXT raw node inside the absorbed line
    # (mirror of find_next_raw_boundary): only the run up to that raw node
    # joins the chitsuki; the raw node + its tail stay as a plain paragraph.
    right_ruby = {
        "kind": "ruby",
        "base": "食",
        "reading": "ヲセ",
        "direction": "right",
        "span": {"byte_start": 510, "byte_end": 537, "line_start": 1, "line_end": 1},
    }
    marker = raw_marker("［＃「食」の左に「クヘ」のルビ］", "ruby", 537)
    inner_raw = raw_marker("［＃ここから罫囲み］", "containerOpen", 620)
    base = write_dump(
        tmp_path,
        "a",
        {
            "w1": doc(
                [
                    para(style_chitsuki([text("\n汁", 500, 510), right_ruby])),
                    para(marker, text("と　すゝめ", 594), inner_raw, text("\n後", 650)),
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
                    para(
                        chitsuki_v2(
                            [
                                text("\n汁", 500, 510),
                                right_ruby,
                                typed_left_ruby("食", "クヘ", marker["span"]),
                                text("と　すゝめ", 594),
                            ]
                        )
                    ),
                    para(inner_raw, text("\n後", 650)),
                ]
            )
        },
    )
    code, summary, err = run("v2-migration", base, cand, tmp_path)
    assert code == 0, (summary, err)


def test_v2_migration_rejects_warning_extra_key(tmp_path):
    # Candidate warning has a key outside the allowed v2 warning shape
    # {"code", "severity", "message", "span", "path"} — rejected even
    # though every other field is otherwise correct.
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
                        "code": "some-warning",
                        "severity": "warning",
                        "message": "some_warning",
                        "span": {
                            "byte_start": 0,
                            "byte_end": 4,
                            "line_start": 1,
                            "line_end": 1,
                        },
                        "extra": "nope",
                    }
                ],
            )
        },
    )
    code, _, _ = run("v2-migration", base, cand, tmp_path)
    assert code == 2


# --- source-note-append mode (Phase 4, rotation C4) ---


def source_note(content, placement="back", region_class="terminal_provenance", span=None):
    content = list(content)
    if span is None:
        first_span = content[0]["span"]
        last_span = content[-1]["span"]
        span = {
            "byte_start": first_span["byte_start"],
            "byte_end": last_span["byte_end"],
            "line_start": first_span["line_start"],
            "line_end": last_span["line_end"],
        }
    return {
        "kind": "source_note",
        "placement": placement,
        "region_class": region_class,
        "content": content,
        "span": span,
    }


def test_append_mode_identical_pass(tmp_path):
    blocks = [para(text("本文\n", 0, 7))]
    base = write_dump(tmp_path, "a", {"w1": v2_doc(blocks)})
    cand = write_dump(tmp_path, "b", {"w1": v2_doc(blocks)})
    code, summary, err = run("source-note-append", base, cand, tmp_path)
    assert code == 0, (summary, err)
    assert summary["classes"]["identical"] == 1
    assert summary["classes"]["source_note_appended"] == 0


def test_append_mode_valid_source_note_append(tmp_path):
    base_blocks = [para(text("本文\n", 0, 7))]
    base = write_dump(tmp_path, "a", {"w1": v2_doc(base_blocks)})
    note = source_note(
        [
            spanned("底本：「作品集」文庫社\n", 100, 133, 3, 3),
            spanned("　1990（平成2）年5月10日発行\n", 133, 166, 4, 4),
        ]
    )
    cand = write_dump(tmp_path, "b", {"w1": v2_doc(base_blocks + [note])})
    code, summary, err = run("source-note-append", base, cand, tmp_path)
    assert code == 0, (summary, err)
    assert summary["classes"]["source_note_appended"] == 1
    assert summary["classes"]["identical"] == 0


def test_append_mode_multiple_source_note_blocks_pass(tmp_path):
    # Multiple source_note blocks per work are legal — a blank/colophon line
    # can split contiguous terminal-provenance groups into distinct blocks;
    # the "appended blocks all source_note" invariant applies to each.
    base_blocks = [para(text("本文\n", 0, 7))]
    base = write_dump(tmp_path, "a", {"w1": v2_doc(base_blocks)})
    note1 = source_note([spanned("底本：「作品集」文庫社\n", 100, 133, 3, 3)])
    note2 = source_note([spanned("入力：someone\n", 200, 220, 6, 6)])
    cand = write_dump(tmp_path, "b", {"w1": v2_doc(base_blocks + [note1, note2])})
    code, summary, err = run("source-note-append", base, cand, tmp_path)
    assert code == 0, (summary, err)
    assert summary["classes"]["source_note_appended"] == 1


def test_append_mode_rejects_body_drift(tmp_path):
    base = write_dump(tmp_path, "a", {"w1": v2_doc([para(text("本文\n", 0, 7))])})
    cand = write_dump(tmp_path, "b", {"w1": v2_doc([para(text("変わった\n", 0, 7))])})
    code, _, _ = run("source-note-append", base, cand, tmp_path)
    assert code == 2


def test_append_mode_rejects_non_source_note_append(tmp_path):
    base_blocks = [para(text("本文\n", 0, 7))]
    base = write_dump(tmp_path, "a", {"w1": v2_doc(base_blocks)})
    extra = para(text("追記\n", 100, 107))
    cand = write_dump(tmp_path, "b", {"w1": v2_doc(base_blocks + [extra])})
    code, _, _ = run("source-note-append", base, cand, tmp_path)
    assert code == 2


def test_append_mode_rejects_wrong_region_class(tmp_path):
    base_blocks = [para(text("本文\n", 0, 7))]
    base = write_dump(tmp_path, "a", {"w1": v2_doc(base_blocks)})
    note = source_note(
        [spanned("入力：someone\n", 100, 120, 3, 3)], region_class="colophon_metadata"
    )
    cand = write_dump(tmp_path, "b", {"w1": v2_doc(base_blocks + [note])})
    code, _, _ = run("source-note-append", base, cand, tmp_path)
    assert code == 2


def test_append_mode_rejects_inserted_not_appended(tmp_path):
    # The extra source_note is a well-formed block, but INSERTED before the
    # last baseline block rather than appended after all of them — the
    # candidate's leading blocks no longer equal the baseline prefix.
    base_blocks = [para(text("前\n", 0, 5)), para(text("後\n", 100, 105))]
    base = write_dump(tmp_path, "a", {"w1": v2_doc(base_blocks)})
    note = source_note([spanned("底本：X\n", 200, 212, 5, 5)])
    cand_blocks = [base_blocks[0], note, base_blocks[1]]
    cand = write_dump(tmp_path, "b", {"w1": v2_doc(cand_blocks)})
    code, _, _ = run("source-note-append", base, cand, tmp_path)
    assert code == 2


def test_append_mode_rejects_warning_drift(tmp_path):
    base_blocks = [para(text("本文\n", 0, 7))]
    base = write_dump(
        tmp_path,
        "a",
        {
            "w1": v2_doc(
                base_blocks,
                warnings=[{"code": "some-warning", "severity": "warning", "message": "m"}],
            )
        },
    )
    cand = write_dump(
        tmp_path,
        "b",
        {
            "w1": v2_doc(
                base_blocks,
                warnings=[{"code": "other-warning", "severity": "warning", "message": "m"}],
            )
        },
    )
    code, _, _ = run("source-note-append", base, cand, tmp_path)
    assert code == 2


def test_append_mode_rejects_degenerate_span(tmp_path):
    base_blocks = [para(text("本文\n", 0, 7))]
    base = write_dump(tmp_path, "a", {"w1": v2_doc(base_blocks)})
    # byte_end == byte_start: degenerate content span
    note = source_note([spanned("底本：X\n", 100, 100, 3, 3)])
    cand = write_dump(tmp_path, "b", {"w1": v2_doc(base_blocks + [note])})
    code, _, _ = run("source-note-append", base, cand, tmp_path)
    assert code == 2


def test_append_mode_rejects_terminator_stripped_values(tmp_path):
    # Two adjacent content values where the first lacks its line terminator
    # — without it the two values would concatenate into one line; the
    # per-line invariant (values never concatenate lines) is violated.
    base_blocks = [para(text("本文\n", 0, 7))]
    base = write_dump(tmp_path, "a", {"w1": v2_doc(base_blocks)})
    note = source_note(
        [
            spanned("底本：「作品集」文庫社", 100, 130, 3, 3),  # stripped terminator
            spanned("　1990（平成2）年5月10日発行\n", 130, 163, 4, 4),
        ]
    )
    cand = write_dump(tmp_path, "b", {"w1": v2_doc(base_blocks + [note])})
    code, _, _ = run("source-note-append", base, cand, tmp_path)
    assert code == 2


# --- bare-toggle-adoption mode (Phase 5, rotation C5) -----------------------
#
# NOTE: this file already defines `raw_marker(source, marker_kind, start)`
# above (used by container-rewrite/v2-migration/source-note-append tests).
# Its signature is incompatible with what these tests need (explicit
# byte_start/byte_end independent of source length, plus the real AAT
# `x-source-marker-kind` for a bare-toggle marker, which is
# containerOpen/containerClose — not "directive"), so a same-named
# redefinition here would silently shadow it and break every earlier test
# that calls it. Hence a distinctly-named helper, `bare_marker`.

_BARE_TOGGLE_OPEN_SOURCES = {"［＃横組み］", "［＃罫囲み］"}


def bare_marker(source, line=1, bs=0, be=1):
    marker_kind = "containerOpen" if source in _BARE_TOGGLE_OPEN_SOURCES else "containerClose"
    return {
        "kind": "raw",
        "source": source,
        "x-provenance": "parser-derived",
        "x-source-marker-kind": marker_kind,
        "span": {"line_start": line, "line_end": line, "byte_start": bs, "byte_end": be},
    }


def toggle_container(kind, content, line=1, bs=0, be=1):
    return {
        "kind": kind,
        "content": content,
        "span": {"line_start": line, "line_end": line, "byte_start": bs, "byte_end": be},
    }


def test_bare_toggle_identical_dumps_pass(tmp_path):
    docs = {"w1": doc([para(bare_marker("［＃横組み］", bs=0, be=12))])}
    base = write_dump(tmp_path, "a", docs)
    cand = write_dump(tmp_path, "b", docs)
    code, summary, err = run(
        "bare-toggle-adoption", base, cand, tmp_path,
        "--expected-adopted-yokogumi", "0",
        "--expected-adopted-keigakomi", "0",
        "--expected-declined", "1",
    )
    assert code == 0, (summary, err)
    assert summary["classes"]["identical"] == 1
    assert summary["details"]["declined_markers"] == 1


def test_bare_toggle_adoption_rewrite_passes(tmp_path):
    inner = {
        "kind": "text", "value": "（Hare）",
        "span": {"line_start": 1, "line_end": 1, "byte_start": 12, "byte_end": 22},
    }
    base_docs = {
        "w1": doc([para(
            bare_marker("［＃横組み］", bs=0, be=12), inner,
            bare_marker("［＃横組み終わり］", bs=22, be=40),
        )])
    }
    cand_docs = {"w1": doc([para(toggle_container("yokogumi", [inner], bs=0, be=40))])}
    base = write_dump(tmp_path, "a", base_docs)
    cand = write_dump(tmp_path, "b", cand_docs)
    code, summary, err = run(
        "bare-toggle-adoption", base, cand, tmp_path,
        "--expected-adopted-yokogumi", "1",
        "--expected-adopted-keigakomi", "0",
        "--expected-declined", "0",
    )
    assert code == 0, (summary, err)
    assert summary["classes"]["toggle_adopted"] == 1
    assert summary["details"]["adopted_yokogumi_pairs"] == 1


def test_bare_toggle_wrong_kind_fails(tmp_path):
    inner = {
        "kind": "text", "value": "x",
        "span": {"line_start": 1, "line_end": 1, "byte_start": 12, "byte_end": 13},
    }
    base_docs = {
        "w1": doc([para(
            bare_marker("［＃横組み］", bs=0, be=12), inner,
            bare_marker("［＃横組み終わり］", bs=13, be=31),
        )])
    }
    cand_docs = {"w1": doc([para(toggle_container("keigakomi", [inner], bs=0, be=31))])}
    base = write_dump(tmp_path, "a", base_docs)
    cand = write_dump(tmp_path, "b", cand_docs)
    code, _, _ = run("bare-toggle-adoption", base, cand, tmp_path)
    assert code == 2


def test_bare_toggle_content_loss_fails(tmp_path):
    inner = {
        "kind": "text", "value": "x",
        "span": {"line_start": 1, "line_end": 1, "byte_start": 12, "byte_end": 13},
    }
    base_docs = {
        "w1": doc([para(
            bare_marker("［＃横組み］", bs=0, be=12), inner,
            bare_marker("［＃横組み終わり］", bs=13, be=31),
        )])
    }
    cand_docs = {"w1": doc([para(toggle_container("yokogumi", [], bs=0, be=31))])}
    base = write_dump(tmp_path, "a", base_docs)
    cand = write_dump(tmp_path, "b", cand_docs)
    code, _, _ = run("bare-toggle-adoption", base, cand, tmp_path)
    assert code == 2


def test_bare_toggle_declined_marker_mutation_fails(tmp_path):
    base_docs = {"w1": doc([para(bare_marker("（例）［＃横組み］", bs=0, be=20))])}
    cand_docs = {"w1": doc([para(bare_marker("（例）［＃横組み］", bs=0, be=21))])}  # span drifted
    base = write_dump(tmp_path, "a", base_docs)
    cand = write_dump(tmp_path, "b", cand_docs)
    code, _, _ = run("bare-toggle-adoption", base, cand, tmp_path)
    assert code == 2


def test_bare_toggle_expected_counter_mismatch_fails(tmp_path):
    docs = {"w1": doc([para(text("plain\n"))])}
    base = write_dump(tmp_path, "a", docs)
    cand = write_dump(tmp_path, "b", docs)
    code, _, _ = run(
        "bare-toggle-adoption", base, cand, tmp_path,
        "--expected-adopted-yokogumi", "1582",
    )
    assert code == 2


def test_bare_toggle_independence_missing_adoption_fails(tmp_path):
    # Baseline implies (via classify_tokens) exactly one valid yokogumi
    # adoption, but the candidate left the markers raw (byte-identical to
    # baseline) — the independent derivation must catch this even though
    # there is no structural diff to inspect (review P5-4).
    inner = {
        "kind": "text", "value": "x",
        "span": {"line_start": 1, "line_end": 1, "byte_start": 12, "byte_end": 13},
    }
    docs = {
        "w1": doc([para(
            bare_marker("［＃横組み］", bs=0, be=12), inner,
            bare_marker("［＃横組み終わり］", bs=13, be=31),
        )])
    }
    base = write_dump(tmp_path, "a", docs)
    cand = write_dump(tmp_path, "b", docs)
    code, _, _ = run("bare-toggle-adoption", base, cand, tmp_path)
    assert code == 2


def test_bare_toggle_independence_invalid_adoption_fails(tmp_path):
    # Baseline's yokogumi/keigakomi markers interleave on one line, so
    # classify_tokens invalidates BOTH constructs — no adoption is expected.
    # A candidate that nevertheless wraps the yokogumi span in a toggle
    # container (byte-exact and structurally recoverable) must still be
    # rejected: the independent derivation, not structural recoverability,
    # decides adoption validity (review P5-4, the converse direction).
    open_y = bare_marker("［＃横組み］", bs=0, be=12)
    open_k = bare_marker("［＃罫囲み］", bs=12, be=24)
    inner = {
        "kind": "text", "value": "x",
        "span": {"line_start": 1, "line_end": 1, "byte_start": 24, "byte_end": 25},
    }
    close_y = bare_marker("［＃横組み終わり］", bs=25, be=43)
    close_k = bare_marker("［＃罫囲み終わり］", bs=43, be=61)
    base_docs = {"w1": doc([para(open_y, open_k, inner, close_y, close_k)])}
    cand_docs = {
        "w1": doc([para(
            toggle_container("yokogumi", [open_k, inner], bs=0, be=43), close_k,
        )])
    }
    base = write_dump(tmp_path, "a", base_docs)
    cand = write_dump(tmp_path, "b", cand_docs)
    code, _, _ = run("bare-toggle-adoption", base, cand, tmp_path)
    assert code == 2


def test_bare_toggle_compensating_cross_line_adoption_fails(tmp_path):
    # PROBE 1 (plan amendment 0d323a72): per-WORK totals alone admit a
    # compensating false-pass. Baseline line 1 is a valid yokogumi pair
    # (expected: 1 adoption); line 2 is an interleaved y/k line (expected:
    # 0 adoptions; declined 4 = orphan_open 1 + reopen_rollback 2 +
    # interleave 1). A candidate that leaves line 1 RAW but wrongly wraps
    # line 2's yokogumi span cancels in the totals: observed yokogumi 1 ==
    # expected 1, declined 4 (line-1 pair 2 + line-2 open_k/close_k 2) ==
    # expected 4. Only the per-LINE binding (observed line 1 {0,0} !=
    # expected {1,0}) catches it — must exit 2.
    open_y1 = bare_marker("［＃横組み］", line=1, bs=0, be=12)
    inner1 = {
        "kind": "text", "value": "x",
        "span": {"line_start": 1, "line_end": 1, "byte_start": 12, "byte_end": 13},
    }
    close_y1 = bare_marker("［＃横組み終わり］", line=1, bs=13, be=31)
    open_y2 = bare_marker("［＃横組み］", line=2, bs=32, be=44)
    open_k2 = bare_marker("［＃罫囲み］", line=2, bs=44, be=56)
    inner2 = {
        "kind": "text", "value": "y",
        "span": {"line_start": 2, "line_end": 2, "byte_start": 56, "byte_end": 57},
    }
    close_y2 = bare_marker("［＃横組み終わり］", line=2, bs=57, be=75)
    close_k2 = bare_marker("［＃罫囲み終わり］", line=2, bs=75, be=93)
    base_docs = {
        "w1": doc([para(
            open_y1, inner1, close_y1,
            open_y2, open_k2, inner2, close_y2, close_k2,
        )])
    }
    cand_docs = {
        "w1": doc([para(
            open_y1, inner1, close_y1,  # line 1 left raw (missed adoption)
            toggle_container("yokogumi", [open_k2, inner2], line=2, bs=32, be=75),
            close_k2,  # line 2 wrongly adopted (grammar declares it invalid)
        )])
    }
    base = write_dump(tmp_path, "a", base_docs)
    cand = write_dump(tmp_path, "b", cand_docs)
    code, _, err = run("bare-toggle-adoption", base, cand, tmp_path)
    assert code == 2, err
    assert "line" in err


def test_bare_toggle_declined_by_reason_breakdown(tmp_path):
    # Passing-path coverage for details.declined_by_reason. Baseline (and
    # identical candidate) carries three declined-only lines; hand-computed
    # per the Contract 1 grammar:
    #   line 1 (interleave: y-open, k-open, y-close, k-close):
    #     y-close vs top-of-stack k -> interleave_events 1, both invalid,
    #     nothing popped; k-close pops the k pair -> rolled back (2);
    #     y still open at EOL -> orphan_open 1. Declined 4.
    #   line 2 (lone y-open): orphan_open 1. Declined 1.
    #   line 3 (y-open, y-open, y-close): second open is a same-construct
    #     reopen (y invalid); the close pops one candidate pair -> rolled
    #     back (2); the other open survives to EOL -> orphan_open 1.
    #     Declined 3.
    # Totals: orphan_open 3, orphan_close 0, reopen_rollback 4,
    # interleave 1; declined_markers 8 (= 4 + 1 + 3).
    inner1 = {
        "kind": "text", "value": "a",
        "span": {"line_start": 1, "line_end": 1, "byte_start": 24, "byte_end": 25},
    }
    docs = {
        "w1": doc([para(
            # line 1: improper interleave
            bare_marker("［＃横組み］", line=1, bs=0, be=12),
            bare_marker("［＃罫囲み］", line=1, bs=12, be=24),
            inner1,
            bare_marker("［＃横組み終わり］", line=1, bs=25, be=43),
            bare_marker("［＃罫囲み終わり］", line=1, bs=43, be=61),
            # line 2: orphan open
            bare_marker("［＃横組み］", line=2, bs=62, be=74),
            # line 3: same-construct reopen
            bare_marker("［＃横組み］", line=3, bs=75, be=87),
            bare_marker("［＃横組み］", line=3, bs=87, be=99),
            bare_marker("［＃横組み終わり］", line=3, bs=99, be=117),
        )])
    }
    base = write_dump(tmp_path, "a", docs)
    cand = write_dump(tmp_path, "b", docs)
    code, summary, err = run(
        "bare-toggle-adoption", base, cand, tmp_path,
        "--expected-adopted-yokogumi", "0",
        "--expected-adopted-keigakomi", "0",
        "--expected-declined", "8",
    )
    assert code == 0, (summary, err)
    assert summary["classes"]["identical"] == 1
    assert summary["details"]["declined_markers"] == 8
    assert summary["details"]["declined_by_reason"] == {
        "orphan_open": 3,
        "orphan_close": 0,
        "reopen_rollback": 4,
        "interleave": 1,
    }
