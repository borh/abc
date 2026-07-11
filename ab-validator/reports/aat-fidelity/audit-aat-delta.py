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

Exit 0 = PASS. Exit 2 = ANY unclassified difference or reference error
(fail-closed; there is no exit 1). A container-rewrite mismatch on real
corpus data is an ESCALATION per the spec — do not weaken the grammar to
invariants.
"""
import argparse
import copy
import json
import pathlib
import sys

LEGACY_WARNING = ("aozora upstream spans are sanitized-source byte offsets; "
                  "line_start and line_end are synthesized as 1")
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
    return (isinstance(node, dict) and node.get("kind") == "raw"
            and node.get("x-source-marker-kind") == marker_kind)


def open_kind(node):
    if is_raw(node, "containerOpen"):
        source = (node.get("source") or "").strip()
        for kind, (marker, _) in CONSTRUCTS.items():
            if source == marker:
                return kind
    return None


def close_matches(node, needle):
    return is_raw(node, "containerClose") and needle in (node.get("source") or "")


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
    return (isinstance(node, dict) and node.get("kind") == "text"
            and node.get("value", "") == "")


def is_wrapper_style(node):
    # Block-assembly style wrapper (chitsuki/burasage): assembled AROUND
    # flat stream nodes and carries NO span field. Inline styles built in
    # inline_content carry a span and existed whole in the flat stream —
    # Rust's boundary strip saw them as non-text nodes and no-op'd.
    return (isinstance(node, dict) and node.get("kind") == "style"
            and "span" not in node and isinstance(node.get("content"), list))


def is_container_derived_block(block):
    # Blocks assembled FROM a containerOpen marker: in Rust's flat close
    # scan (find_matching_container_close) that marker was still inline
    # and ABORTED the scan — jisage_block from ［＃ここから…字下げ］,
    # burasage-style paragraphs from ［＃ここから…折り返して…］.
    # Chitsuki paragraphs (alignEnd) and headings (headingHint) come from
    # non-containerOpen markers and do not abort.
    if not isinstance(block, dict):
        return False
    if block.get("kind") == "jisage_block":
        return True
    if block.get("kind") == "paragraph":
        content = block.get("content", [])
        return bool(content) and (
            is_wrapper_style(content[0])
            and content[0].get("style_type") == "burasage")
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
    """One grammar pass over a block list; returns (rewritten, count)."""
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
        for oi, kind in ((j, open_kind(n)) for j, n in enumerate(content)
                        if open_kind(n)):
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
                admitted = (oi, kind, close_block_index, ci)
                break
        if admitted is None:
            out.append(block)  # no admissible pair: candidate must equal baseline
            i += 1
            continue
        oi, kind, close_block_index, ci = admitted
        pre = content[:oi]
        if close_block_index == i:
            inner = content[oi + 1:ci]
            post = content[ci + 1:]
            if inner:
                inner = [strip_leading_newline(inner[0])] + inner[1:]
                inner = inner[:-1] + [strip_trailing_newline(inner[-1])]
            inner_para = make_para(inner)
            children = [inner_para] if inner_para else []
        else:
            head = content[oi + 1:]
            close_content = blocks[close_block_index].get("content", [])
            tail = close_content[:ci]
            post = close_content[ci + 1:]
            middle = blocks[i + 1:close_block_index]
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
        out.append({"kind": kind, "children": children})
        count += 1
        # strip_next_leading_newline after close: strip post[0]'s leading
        # newline; if that empties the text node, DROP it (the Rust
        # post-close path removes emptied nodes — asymmetric with
        # strip_boundary_newlines, which keeps them).
        rest = blocks[close_block_index + 1:]
        if post:
            first = strip_leading_newline(post[0])
            post = ([] if is_empty_text(first) else [first]) + post[1:]
        else:
            # Close ended its paragraph: the flag consumes on the first
            # stream node of the NEXT block instead.
            rest = apply_post_close_strip(rest)
        post_para = make_para(post)
        rewritten_rest, rest_count = rewrite_blocks(
            ([post_para] if post_para else []) + rest)
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
        die(f"{name}: differs but baseline has no well-paired "
            f"keigakomi/yokogumi markers (unclassified)")
    if rewritten != cand:
        die(f"{name}: candidate is not exactly the grammar's rewrite "
            f"({count} container(s) rewritten) — escalate per spec")
    summary["classes"]["rewritten"] += 1


# --- span-confinement -------------------------------------------------------

def mask_spans(node, spans_out):
    if isinstance(node, dict):
        # Intercept the "span" KEY regardless of value type: a null or
        # otherwise non-dict span must reach the invariant loop below and
        # fail there, not mask equal on both sides as a false PASS.
        return {k: (spans_out.append(v) or None) if k == "span"
                else mask_spans(v, spans_out) for k, v in node.items()}
    if isinstance(node, list):
        return [mask_spans(v, spans_out) for v in node]
    return node


def span_confinement_mode(base_doc, cand_doc, name, summary):
    base = strip_identity(base_doc)
    cand = strip_identity(cand_doc)
    meta = base.get("meta", {})
    meta["warnings"] = [w for w in meta.get("warnings", [])
                        if w.get("message") != LEGACY_WARNING]
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
        if not (isinstance(span["byte_start"], int) and isinstance(span["byte_end"], int)
                and span["byte_end"] >= span["byte_start"]
                and span["line_end"] >= span["line_start"] >= 1):
            die(f"{name}: invalid span {span}")
        if span["line_start"] != 1 or span["line_end"] != 1:
            all_line1 = False
    if len(cand_spans) >= 10 and all_line1:
        die(f"{name}: {len(cand_spans)} spans all line 1/1 — synthesis tripline")
    summary["classes"]["span_confined"] += 1


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("mode", choices=["container-rewrite", "span-confinement"])
    ap.add_argument("baseline_dir")
    ap.add_argument("candidate_dir")
    ap.add_argument("--summary-json", required=True)
    args = ap.parse_args()
    base_files = load_dir(args.baseline_dir)
    cand_files = load_dir(args.candidate_dir)
    missing = sorted(set(base_files) ^ set(cand_files))
    if missing:
        die(f"file sets differ: {missing[:10]}")
    summary = {"mode": args.mode, "compared": len(base_files),
               "classes": {"identical": 0, "rewritten": 0, "span_confined": 0},
               "verdict": "PASS"}
    handler = (container_rewrite_mode if args.mode == "container-rewrite"
               else span_confinement_mode)
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
