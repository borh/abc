#!/usr/bin/env python3
"""Fail-closed AAT delta audit between two dumps (Phase 3 rotations).

container-rewrite (rotation A): every difference must be explained by the
three-class taxonomy of the Phase 3 design spec —
  1. identity pointers (/meta/adapter_version),
  2. works whose baseline carries well-paired keigakomi/yokogumi container
     markers, checked by FORWARD REWRITE + DEEP EQUALITY: an independent
     reimplementation of ab-aozora-aat's classification over the baseline
     JSON must reproduce the candidate exactly,
  3. everything else byte-identical after identity substitution.

span-confinement (rotation B): after dropping /meta/adapter_version,
masking every span object, masking warning "line" values, and dropping the
legacy synthesized-span warning from the baseline, the documents must be
deeply equal; candidate spans must satisfy field invariants; the
line-synthesis tripline flags wholesale line=1 output.

v2-migration (Phase 4, rotation C3): forward-rewrites a schema-v1 baseline
document to its expected schema-v2 form — layout key renames, left-ruby
typing, jizume_block formation (paired + compound), a warnings-shape
projection, and the root version bump — and requires deep equality with
the v2 candidate. Mirrors ab-aozora-aat/src/lib.rs's Phase 4 emission
semantics ONLY (never corpus-fitted); a mismatch on real corpus data is a
controller escalation, same discipline as container-rewrite.

Exit 0 = PASS. Exit 2 = ANY unclassified difference or reference error
(fail-closed; there is no exit 1). A container-rewrite mismatch on real
corpus data is an ESCALATION per the spec — do not weaken the grammar to
invariants.
"""

import argparse
import copy
import json
import pathlib
import re
import sys

LEGACY_WARNING = (
    "aozora upstream spans are sanitized-source byte offsets; "
    "line_start and line_end are synthesized as 1"
)
CONSTRUCTS = {
    "keigakomi_block": ("［＃ここから罫囲み］", "罫囲み"),
    "yokogumi_block": ("［＃ここから横組み］", "横組み"),
}


def die(msg):
    print(f"AUDIT FAIL: {msg}", file=sys.stderr)
    raise SystemExit(2)


def load_dir(d):
    files = {p.name: p for p in sorted(pathlib.Path(d).glob("*.json"))}
    if not files:
        die(f"no *.json under {d}")
    return files


def strip_identity(doc):
    doc = copy.deepcopy(doc)
    if isinstance(doc.get("meta"), dict):
        doc["meta"].pop("adapter_version", None)
    return doc


# --- container-rewrite grammar (mirror of blocks_from_inline_content) ------


def is_raw(node, marker_kind):
    return (
        isinstance(node, dict)
        and node.get("kind") == "raw"
        and node.get("x-source-marker-kind") == marker_kind
    )


def open_kind(node):
    """Return `(kind, extra_fields)` for an admissible containerOpen raw,
    or `None`. `extra_fields` merges into the rewritten block dict.

    C3 gate fix (mirror of the Rust classifier): the standalone
    `［＃ここからN字詰め］` is the `line-width` container family (upstream
    notation spec §6.6, `line-width-open`), NOT a typed `jizume_block`; it
    stays a raw containerOpen/containerClose pair. No pure-jizume open is
    admitted here — jizume width is only ever adopted from the compound
    indent wrap in `adopt_compound_jizume`.
    """
    if is_raw(node, "containerOpen"):
        source = (node.get("source") or "").strip()
        for kind, (marker, _) in CONSTRUCTS.items():
            if source == marker:
                return kind, {}
    return None


def close_matches(node, needle):
    return is_raw(node, "containerClose") and needle in (node.get("source") or "")


JIZUME_NEEDLE = "字詰め"

_FULLWIDTH_DIGIT_SHIFT = ord("0") - ord("０")
_KANJI_DIGITS = {"一": 1, "二": 2, "三": 3, "四": 4, "五": 5, "六": 6, "七": 7, "八": 8, "九": 9}


def _aozora_digit(ch):
    if "0" <= ch <= "9":
        return ch
    if "０" <= ch <= "９":
        return chr(ord(ch) + _FULLWIDTH_DIGIT_SHIFT)
    return None


def _parse_kanji_number_before(prefix):
    # Mirror of `parse_kanji_number_before`: a trailing run of kanji digits
    # (optionally with a '十' tens-marker), read backwards from `prefix`'s
    # end; anything before the run is ignored, matching the Rust original.
    run = []
    for ch in reversed(prefix):
        if ch in _KANJI_DIGITS or ch == "十":
            run.append(ch)
        else:
            break
    if not run:
        return None
    run.reverse()
    text = "".join(run)
    if "十" in text:
        tens_s, _, ones_s = text.partition("十")
        tens = 1 if tens_s == "" else _KANJI_DIGITS.get(tens_s[0])
        if tens is None:
            return None
        ones = 0 if ones_s == "" else _KANJI_DIGITS.get(ones_s[0])
        if ones is None:
            return None
        return tens * 10 + ones
    return _KANJI_DIGITS.get(text[0])


def parse_aozora_number_before(source, needle):
    """Python transcription of `parse_aozora_number_before`: the digit (or
    kanji-numeral) run immediately preceding `needle`'s first occurrence."""
    idx = source.find(needle)
    if idx == -1:
        return None
    prefix = source[:idx]
    digits = []
    for ch in reversed(prefix):
        digit = _aozora_digit(ch)
        if digit is not None:
            digits.insert(0, digit)
        elif digits:
            break
    if digits:
        return int("".join(digits))
    return _parse_kanji_number_before(prefix)


def jizume_open_width(source):
    """Python transcription of `jizume_open_chars`: the width of a `字詰め`
    open marker — the standalone line-width form or the FINAL clause of a
    compound indent container. `None` if `source` isn't a jizume-open marker.

    C3 gate fix: the standalone `［＃ここからN字詰め］` is the `line-width`
    container family (spec §6.6, `line-width-open`) and stays a raw
    containerOpen/containerClose pair — it is never formed into a
    `jizume_block` by the forward rewrite (mirror of the Rust recognizer,
    whose standalone emission arm was likewise removed). This predicate is
    retained as the documented mirror of the retained `jizume_open_chars`."""
    marker = source.strip()
    if not marker.startswith("［＃ここから") or not marker.endswith("］"):
        return None
    if JIZUME_NEEDLE not in marker:
        return None
    _, after = marker.split(JIZUME_NEEDLE, 1)
    if after != "］":
        return None
    return parse_aozora_number_before(marker, JIZUME_NEEDLE)


def strip_leading_newline(node):
    if isinstance(node, dict) and node.get("kind") == "text":
        value = node.get("value", "")
        if value.startswith("\n"):
            node = dict(node, value=value[1:])
    return node


def strip_trailing_newline(node):
    if isinstance(node, dict) and node.get("kind") == "text":
        value = node.get("value", "")
        if value.endswith("\n"):
            node = dict(node, value=value[:-1])
    return node


def make_para(content):
    # Mirrors push_paragraph_if_not_empty: only an EMPTY content list is
    # dropped. Empty text nodes produced by boundary stripping stay —
    # the Rust strip helpers mutate values without removing nodes.
    return {"kind": "paragraph", "content": list(content)} if content else None


def is_empty_text(node):
    return isinstance(node, dict) and node.get("kind") == "text" and node.get("value", "") == ""


def is_wrapper_style(node):
    # Block-assembly style wrapper (chitsuki/burasage): assembled AROUND
    # flat stream nodes and carries NO span field. Inline styles built in
    # inline_content carry a span and existed whole in the flat stream —
    # Rust's boundary strip saw them as non-text nodes and no-op'd.
    return (
        isinstance(node, dict)
        and node.get("kind") == "style"
        and "span" not in node
        and isinstance(node.get("content"), list)
    )


def is_container_derived_block(block):
    # Blocks assembled FROM a containerOpen marker: in Rust's flat close
    # scan (find_matching_container_close) that marker was still inline
    # and ABORTED the scan — jisage_block from ［＃ここから…字下げ］,
    # burasage-style paragraphs from ［＃ここから…折り返して…］. keigakomi_block
    # / yokogumi_block / jizume_block are the SAME abort source for
    # v2-migration mode, whose baseline already has them pre-classified
    # (unlike container-rewrite mode, where they're this pass's OWN
    # output and never appear pre-formed in `blocks[j]` for j>i — so this
    # is a harmless no-op there). Chitsuki paragraphs (alignEnd) and
    # headings (headingHint) come from non-containerOpen markers and do
    # not abort.
    if not isinstance(block, dict):
        return False
    if block.get("kind") in ("jisage_block", "keigakomi_block", "yokogumi_block", "jizume_block"):
        return True
    if block.get("kind") == "paragraph":
        content = block.get("content", [])
        return bool(content) and (
            is_wrapper_style(content[0]) and content[0].get("style_type") == "burasage"
        )
    return False


def strip_trailing_leaf(node):
    # Flat-stream trailing strip when the flat-last node was consumed into
    # a no-span style wrapper: descend to the last leaf and strip if it is
    # text. Never descend into span-carrying nodes — Rust saw those whole
    # in the stream and no-op'd on them.
    if isinstance(node, dict) and node.get("kind") == "text":
        return strip_trailing_newline(node)
    if is_wrapper_style(node):
        inner = node["content"]
        if inner:
            last = strip_trailing_leaf(inner[-1])
            if last is not inner[-1]:
                return dict(node, content=inner[:-1] + [last])
    return node


def strip_trailing_in_block(block):
    """Apply the flat-last boundary strip inside the last middle block.

    Rust strips the flat inner stream's LAST node BEFORE assembly
    (strip_boundary_newlines). When the close marker starts its own
    paragraph (tail empty), that node is the last stream node consumed by
    the last middle block — only paragraph-kind blocks (plain, or
    chitsuki/burasage no-span style wrappers) end on a stream node.
    jisage_block ends on its consumed close marker and heading on its
    consumed hint: Rust no-op'd on those raws, so we never descend there.
    """
    if not (isinstance(block, dict) and block.get("kind") == "paragraph"):
        return block
    content = block.get("content", [])
    if not content:
        return block
    last = strip_trailing_leaf(content[-1])
    if last is content[-1]:
        return block
    return dict(block, content=content[:-1] + [last])


def apply_post_close_strip(rest):
    # Rust's strip_next_leading_newline (lib.rs ~347-359) consumes on the
    # NEXT stream node. When the close marker ended its paragraph (post
    # empty), that node opens the next block. Only a plain paragraph
    # starts with a stream node; chitsuki/burasage paragraphs, jisage
    # blocks, and headings start with a marker the assembler consumed
    # (flag cleared with no effect). An emptied text node is DROPPED; a
    # paragraph emptied by the drop disappears (push_paragraph_if_not_empty).
    if not rest:
        return rest
    block = rest[0]
    if not (isinstance(block, dict) and block.get("kind") == "paragraph"):
        return rest
    content = block.get("content", [])
    if not content or is_wrapper_style(content[0]):
        return rest
    first = strip_leading_newline(content[0])
    content = ([] if is_empty_text(first) else [first]) + content[1:]
    if not content:
        return rest[1:]
    return [dict(block, content=content)] + rest[1:]


def scan_segment(nodes, start, needle):
    """Mirror find_matching_container_close over one content slice."""
    for k in range(start, len(nodes)):
        node = nodes[k]
        if is_raw(node, "containerOpen"):
            return "abort", k
        if close_matches(node, needle):
            return "close", k
    return None, None


def rewrite_blocks(blocks):
    """One grammar pass over a block list; returns (rewritten, count).

    Admits only the fixed-marker container constructs (CONSTRUCTS). The
    standalone `字詰め` line-width form is deliberately NOT admitted (C3 gate
    fix) — it stays a raw containerOpen/containerClose pair per spec §6.6.
    """
    out, count, i = [], 0, 0
    while i < len(blocks):
        block = blocks[i]
        if isinstance(block, dict) and isinstance(block.get("children"), list):
            children, inner_count = rewrite_blocks(block["children"])
            count += inner_count
            block = dict(block, children=children)
        if not (isinstance(block, dict) and block.get("kind") == "paragraph"):
            out.append(block)
            i += 1
            continue
        content = block.get("content", [])
        # Rust dispatches arms PER NODE in stream order: an open whose
        # close scan fails (abort or exhaustion) is pushed raw and the
        # linear scan continues — a LATER open in the same paragraph can
        # still be admitted (e.g. an inner keigakomi pair nested inside an
        # unadmitted yokogumi pair). Mirror by trying each open in order.
        admitted = None
        for oi, node in enumerate(content):
            hit = open_kind(node)
            if hit is None:
                continue
            kind, extra = hit
            needle = CONSTRUCTS[kind][1]
            # Scan forward through the flat stream for the matching close;
            # any containerOpen aborts. First the open paragraph's
            # remainder, then each following block (paragraph content is
            # scanned; container-derived blocks abort — their containerOpen
            # was inline at Rust's scan time).
            close_block_index = ci = None
            state, k = scan_segment(content, oi + 1, needle)
            if state == "close":
                close_block_index, ci = i, k
            elif state is None:
                j = i + 1
                while j < len(blocks):
                    nxt = blocks[j]
                    if is_container_derived_block(nxt):
                        break  # its containerOpen was inline in Rust's scan
                    if isinstance(nxt, dict) and nxt.get("kind") == "paragraph":
                        state, k = scan_segment(nxt.get("content", []), 0, needle)
                        if state == "close":
                            close_block_index, ci = j, k
                        if state is not None:
                            break
                    j += 1
            if close_block_index is not None:
                admitted = (oi, kind, extra, close_block_index, ci)
                break
        if admitted is None:
            out.append(block)  # no admissible pair: candidate must equal baseline
            i += 1
            continue
        oi, kind, extra, close_block_index, ci = admitted
        pre = content[:oi]
        if close_block_index == i:
            inner = content[oi + 1 : ci]
            post = content[ci + 1 :]
            if inner:
                inner = [strip_leading_newline(inner[0])] + inner[1:]
                inner = inner[:-1] + [strip_trailing_newline(inner[-1])]
            inner_para = make_para(inner)
            children = [inner_para] if inner_para else []
        else:
            head = content[oi + 1 :]
            close_content = blocks[close_block_index].get("content", [])
            tail = close_content[:ci]
            post = close_content[ci + 1 :]
            middle = blocks[i + 1 : close_block_index]
            if head:
                head = [strip_leading_newline(head[0])] + head[1:]
            # Trailing boundary strip lands on the FLAT stream's last
            # inner node: in the tail slice if non-empty; otherwise on the
            # last stream node consumed by the last middle block;
            # otherwise (no middle) at the end of the head.
            if tail:
                tail = tail[:-1] + [strip_trailing_newline(tail[-1])]
            elif middle:
                middle = middle[:-1] + [strip_trailing_in_block(middle[-1])]
            elif head:
                head = head[:-1] + [strip_trailing_newline(head[-1])]
            children = []
            head_para = make_para(head)
            if head_para:
                children.append(head_para)
            children.extend(middle)
            tail_para = make_para(tail)
            if tail_para:
                children.append(tail_para)
        if not children:
            children = [{"kind": "paragraph", "content": []}]
        pre_para = make_para(pre)
        if pre_para:
            out.append(pre_para)
        out.append({"kind": kind, **extra, "children": children})
        count += 1
        # strip_next_leading_newline after close: strip post[0]'s leading
        # newline; if that empties the text node, DROP it (the Rust
        # post-close path removes emptied nodes — asymmetric with
        # strip_boundary_newlines, which keeps them).
        rest = blocks[close_block_index + 1 :]
        if post:
            first = strip_leading_newline(post[0])
            post = ([] if is_empty_text(first) else [first]) + post[1:]
        else:
            # Close ended its paragraph: the flag consumes on the first
            # stream node of the NEXT block instead.
            rest = apply_post_close_strip(rest)
        post_para = make_para(post)
        rewritten_rest, rest_count = rewrite_blocks(
            ([post_para] if post_para else []) + rest
        )
        out.extend(rewritten_rest)
        return out, count + rest_count
    return out, count


def container_rewrite_mode(base_doc, cand_doc, name, summary):
    base = strip_identity(base_doc)
    cand = strip_identity(cand_doc)
    if base == cand:
        summary["classes"]["identical"] += 1
        return
    rewritten_blocks, count = rewrite_blocks(base.get("blocks", []))
    rewritten = dict(base, blocks=rewritten_blocks)
    if count == 0:
        die(
            f"{name}: differs but baseline has no well-paired "
            f"keigakomi/yokogumi markers (unclassified)"
        )
    if rewritten != cand:
        die(
            f"{name}: candidate is not exactly the grammar's rewrite "
            f"({count} container(s) rewritten) — escalate per spec"
        )
    summary["classes"]["rewritten"] += 1


# --- span-confinement -------------------------------------------------------


def mask_spans(node, spans_out):
    if isinstance(node, dict):
        # Intercept the "span" KEY regardless of value type: a null or
        # otherwise non-dict span must reach the invariant loop below and
        # fail there, not mask equal on both sides as a false PASS.
        return {
            k: (spans_out.append(v) or None) if k == "span" else mask_spans(v, spans_out)
            for k, v in node.items()
        }
    if isinstance(node, list):
        return [mask_spans(v, spans_out) for v in node]
    return node


def span_confinement_mode(base_doc, cand_doc, name, summary):
    base = strip_identity(base_doc)
    cand = strip_identity(cand_doc)
    meta = base.get("meta", {})
    meta["warnings"] = [w for w in meta.get("warnings", []) if w.get("message") != LEGACY_WARNING]
    for doc in (base, cand):
        for w in doc.get("meta", {}).get("warnings", []):
            if "line" in w:
                w["line"] = None
    cand_spans = []
    base_masked = mask_spans(base, [])
    cand_masked = mask_spans(cand, cand_spans)
    if base_masked != cand_masked:
        die(f"{name}: non-span difference in span-confinement mode")
    all_line1 = True
    for span in cand_spans:
        keys = {"byte_start", "byte_end", "line_start", "line_end"}
        if not isinstance(span, dict) or not keys <= set(span):
            # null spans and wire-shaped {start,end} spans (none expected
            # in AAT) fail too
            die(f"{name}: span missing AAT fields: {span}")
        if not (
            isinstance(span["byte_start"], int)
            and isinstance(span["byte_end"], int)
            and span["byte_end"] >= span["byte_start"]
            and span["line_end"] >= span["line_start"] >= 1
        ):
            die(f"{name}: invalid span {span}")
        if span["line_start"] != 1 or span["line_end"] != 1:
            all_line1 = False
    if len(cand_spans) >= 10 and all_line1:
        die(f"{name}: {len(cand_spans)} spans all line 1/1 — synthesis tripline")
    summary["classes"]["span_confined"] += 1


# --- v2-migration (Phase 4, rotation C3) ------------------------------------

WARNING_SEVERITIES = {"error", "warning", "note"}
WARNING_ALLOWED_KEYS = {"code", "severity", "message", "span", "path"}
WARNING_REQUIRED_KEYS = {"code", "severity", "message"}

# Anchored end-to-end against the TRIMMED source: `pre` captures whatever
# precedes the marker (typically the echoed base text some upstream markers
# carry, e.g. "名［＃「名」の左に「な」のルビ］"). Anything with a `pre` that
# is neither empty nor an exact echo of `base` is NOT an admissible left-ruby
# marker and must survive as raw — a bare `.search()` would silently accept
# arbitrary garbage before the marker.
LEFT_RUBY_RE = re.compile(
    r"^(?P<pre>[^］]*?)［＃「(?P<base>[^」]+)」の左に「(?P<reading>[^」]+)」のルビ］$"
)


def migrate_warnings(base_meta, cand_meta, name):
    """Contract item 2: per-index invariant check (severity/span are new
    information a forward rewrite cannot derive, so they're checked, not
    reproduced). Caller sentinel-replaces both arrays afterward."""
    base_warnings = base_meta.get("warnings") or []
    cand_warnings = cand_meta.get("warnings") or []
    if len(base_warnings) != len(cand_warnings):
        die(f"{name}: warnings count changed ({len(base_warnings)} -> {len(cand_warnings)})")
    for base_w, cand_w in zip(base_warnings, cand_warnings):
        if not isinstance(cand_w, dict):
            die(f"{name}: candidate warning is not an object: {cand_w!r}")
        if cand_w.get("message") != base_w.get("message"):
            die(
                f"{name}: warning message changed "
                f"({base_w.get('message')!r} -> {cand_w.get('message')!r})"
            )
        expected_code = str(base_w.get("message", "")).replace("_", "-")
        if cand_w.get("code") != expected_code:
            die(
                f"{name}: warning code mismatch (expected {expected_code!r}, got {cand_w.get('code')!r})"
            )
        cand_keys = set(cand_w)
        if not cand_keys <= WARNING_ALLOWED_KEYS:
            die(
                f"{name}: warning has unexpected keys: "
                f"{sorted(cand_keys - WARNING_ALLOWED_KEYS)}"
            )
        if not cand_keys >= WARNING_REQUIRED_KEYS:
            die(
                f"{name}: warning missing required keys: "
                f"{sorted(WARNING_REQUIRED_KEYS - cand_keys)}"
            )
        if cand_w.get("severity") not in WARNING_SEVERITIES:
            die(f"{name}: warning severity invalid: {cand_w.get('severity')!r}")
        if base_w.get("line") is not None:
            if not isinstance(cand_w.get("span"), dict):
                die(f"{name}: warning missing span while baseline line is present")
            if cand_w["span"].get("line_start") != base_w["line"]:
                die(f"{name}: warning span.line_start does not match baseline line")


def rename_layout_keys(node):
    """Contract item 3: mechanical schema-v2 key renames, values untouched."""
    kind = node.get("kind")
    renamed = dict(node)
    if kind in ("jisage_block", "heading"):
        if "x-indent" in renamed:
            renamed["indent"] = renamed.pop("x-indent")
    elif kind == "style":
        style_type = renamed.get("style_type")
        if style_type == "chitsuki":
            if "x-align" in renamed:
                renamed["align"] = renamed.pop("x-align")
            if "x-offset" in renamed:
                renamed["offset_from_end"] = renamed.pop("x-offset")
        elif style_type == "burasage":
            if "x-indent-first" in renamed:
                renamed["indent_first"] = renamed.pop("x-indent-first")
            if "x-indent-rest" in renamed:
                renamed["indent_rest"] = renamed.pop("x-indent-rest")
    return renamed


def rewrite_left_ruby(node):
    """Contract item 4: a raw ruby marker whose source is the left-ruby
    form ［＃「base」の左に「reading」のルビ］ upgrades to a typed ruby node
    (base/reading pulled from the marker's OWN 「」-quoted segments — the
    marker's echoed base prefix, if any, is not consulted); anything else
    survives unchanged (broken/non-left ruby stays raw). Matched full-anchor
    against the TRIMMED source: a `pre` prefix that is neither empty nor an
    exact echo of `base` means this is NOT an admissible left-ruby marker
    (e.g. arbitrary text glued in front) and the raw node survives unchanged."""
    if not is_raw(node, "ruby"):
        return node
    source = (node.get("source") or "").strip()
    m = LEFT_RUBY_RE.match(source)
    if not m or m.group("pre") not in ("", m.group("base")):
        return node
    return {
        "kind": "ruby",
        "base": m.group("base"),
        "reading": m.group("reading"),
        "direction": "left",
        "span": node.get("span"),
    }


def migrate_tree(node, counts):
    """Recursive node-level pass applying items 3+4 (layout renames, left-
    ruby upgrade) everywhere in the tree; `counts["ruby_left"]` tallies
    upgrades fired for classification."""
    if isinstance(node, list):
        return [migrate_tree(v, counts) for v in node]
    if isinstance(node, dict):
        renamed = rename_layout_keys(node)
        rewritten = rewrite_left_ruby(renamed)
        if rewritten is not renamed:
            counts["ruby_left"] += 1
        return {k: migrate_tree(v, counts) for k, v in rewritten.items()}
    return node


def is_burasage_paragraph(node):
    return (
        isinstance(node, dict)
        and node.get("kind") == "paragraph"
        and isinstance(node.get("content"), list)
        and len(node["content"]) == 1
        and isinstance(node["content"][0], dict)
        and node["content"][0].get("kind") == "style"
        and node["content"][0].get("style_type") == "burasage"
    )


def adopt_compound_jizume(base_node, cand_node, name):
    """Contract item 5, compound form: a compound container's 字詰め clause
    is discarded by v1's burasage classification (`burasage_container_indent`
    is unchanged since the initial port — Task 6's git history confirms it)
    — so its width is NOT derivable from baseline output alone, unlike the
    pure/standalone form. Wherever baseline already classified a burasage
    paragraph, ADOPT candidate's jizume_block wrapper when present,
    verifying every invariant a forward rewrite CAN check (width a
    positive int, wrapped content byte-identical to what v1 classified,
    no stray keys) — mirroring Task 6's wrap rule structurally. Anything
    else about candidate's shape is left to the caller's final deep-equality
    check. Returns (adopted_node, adopted_count): the count of DISTINCT
    compound wraps accepted in this subtree (0 if none), which the caller
    tallies into summary["details"]["compound_jizume_adopted"] — a
    magnitude signal, not just a presence flag."""
    if is_burasage_paragraph(base_node):
        if isinstance(cand_node, dict) and cand_node.get("kind") == "jizume_block":
            if set(cand_node) != {"kind", "width", "children"}:
                die(f"{name}: jizume_block has unexpected keys: {sorted(cand_node)}")
            width = cand_node.get("width")
            if not (isinstance(width, int) and not isinstance(width, bool) and width > 0):
                die(f"{name}: jizume_block width invalid: {width!r}")
            if cand_node.get("children") != [base_node]:
                die(f"{name}: compound jizume_block does not wrap exactly the burasage paragraph")
            return cand_node, 1
        return base_node, 0
    if isinstance(base_node, list):
        if not isinstance(cand_node, list) or len(base_node) != len(cand_node):
            return base_node, 0
        count = 0
        out = []
        for b, c in zip(base_node, cand_node):
            nb, ch = adopt_compound_jizume(b, c, name)
            out.append(nb)
            count += ch
        return out, count
    if isinstance(base_node, dict):
        if not isinstance(cand_node, dict):
            return base_node, 0
        count = 0
        out = {}
        for k, v in base_node.items():
            nv, ch = adopt_compound_jizume(v, cand_node.get(k), name)
            out[k] = nv
            count += ch
        return out, count
    return base_node, 0


def v2_migration_mode(base_doc, cand_doc, name, summary):
    base = strip_identity(base_doc)
    cand = strip_identity(cand_doc)
    base["version"] = 2  # contract item 1
    migrate_warnings(base.get("meta", {}), cand.get("meta", {}), name)  # contract item 2
    base.setdefault("meta", {})["warnings"] = "__warnings_checked__"
    cand.setdefault("meta", {})["warnings"] = "__warnings_checked__"
    counts = {"ruby_left": 0}
    migrated_blocks = migrate_tree(base.get("blocks", []), counts)  # items 3+4
    # C3 gate fix: the standalone `字詰め` line-width form is NOT formed into a
    # jizume_block (spec §6.6 — it stays a raw containerOpen/containerClose
    # pair, exactly as the v1 baseline already left it). `rewrite_blocks`
    # therefore does no jizume formation here; jizume enters only via the
    # compound-indent wrap adopted below.
    migrated_blocks, _ = rewrite_blocks(migrated_blocks)
    adopted_blocks, compound_adopted = adopt_compound_jizume(
        migrated_blocks, cand.get("blocks"), name
    )  # item 5, compound form
    rewritten = dict(base, blocks=adopted_blocks)
    if rewritten != cand:
        die(f"{name}: candidate is not exactly the v2-migration grammar's rewrite")
    if compound_adopted:
        # Only compound-container adoptions count toward the jizume magnitude
        # signal; the standalone line-width form never forms a jizume_block.
        summary["details"]["compound_jizume_adopted"] += compound_adopted
    ruby_fired = counts["ruby_left"] > 0
    jizume_fired = compound_adopted > 0
    if ruby_fired and jizume_fired:
        summary["classes"]["ruby_left_rewritten"] += 1
        summary["details"]["both"] += 1
    elif ruby_fired:
        summary["classes"]["ruby_left_rewritten"] += 1
    elif jizume_fired:
        summary["classes"]["jizume_rewritten"] += 1
    else:
        summary["classes"]["migrated"] += 1


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("mode", choices=["container-rewrite", "span-confinement", "v2-migration"])
    ap.add_argument("baseline_dir")
    ap.add_argument("candidate_dir")
    ap.add_argument("--summary-json", required=True)
    args = ap.parse_args()
    base_files = load_dir(args.baseline_dir)
    cand_files = load_dir(args.candidate_dir)
    missing = sorted(set(base_files) ^ set(cand_files))
    if missing:
        die(f"file sets differ: {missing[:10]}")
    if args.mode == "v2-migration":
        # No "identical" bucket: every document changes at least
        # mechanically (root version bump), so a doc lands in "migrated"
        # at minimum.
        classes = {"migrated": 0, "jizume_rewritten": 0, "ruby_left_rewritten": 0}
    else:
        # Byte-identical shape to the pre-Task-8 output — untouched.
        classes = {"identical": 0, "rewritten": 0, "span_confined": 0}
    summary = {
        "mode": args.mode,
        "compared": len(base_files),
        "classes": classes,
        "verdict": "PASS",
    }
    if args.mode == "v2-migration":
        summary["details"] = {"both": 0, "compound_jizume_adopted": 0}
    handler = {
        "container-rewrite": container_rewrite_mode,
        "span-confinement": span_confinement_mode,
        "v2-migration": v2_migration_mode,
    }[args.mode]
    for name in sorted(base_files):
        # Fail-closed: ANY per-work exception (unreadable file, valid JSON
        # of the wrong shape, unexpected structure deep in a handler) exits
        # 2 via die(). die() raises SystemExit, which is a BaseException —
        # not caught by the `except Exception` below — so handler verdicts
        # pass through unchanged.
        try:
            base_doc = json.loads(base_files[name].read_bytes())
            cand_doc = json.loads(cand_files[name].read_bytes())
            handler(base_doc, cand_doc, name, summary)
        except Exception as err:
            die(f"{name}: processing failed ({type(err).__name__}: {err})")
    pathlib.Path(args.summary_json).write_text(json.dumps(summary, indent=2) + "\n")
    print(json.dumps(summary, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
