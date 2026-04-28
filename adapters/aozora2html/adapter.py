#!/usr/bin/env python3
"""aozora2html adapter: maps the Ruby parser's XHTML output to AAT JSON."""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import sys
from pathlib import Path
from typing import Any

from lxml import etree

ADAPTER_NAME = "aozora2html"
ADAPTER_VERSION = "aozora2html-adapter 0.1.0 9ca5395"

XHTML_NS = "http://www.w3.org/1999/xhtml"
NS = {"x": XHTML_NS}


def detect_encoding(raw: bytes) -> tuple[str, str]:
    if raw.startswith(b"\xef\xbb\xbf"):
        return raw[3:].decode("utf-8"), "utf-8-bom"
    try:
        return raw.decode("utf-8"), "utf-8"
    except UnicodeDecodeError:
        return raw.decode("cp932"), "windows-31j"


def source_hash(raw: bytes) -> str:
    return "sha256:" + hashlib.sha256(raw).hexdigest()


def parse_xhtml(xhtml_bytes: bytes) -> etree._Element:
    parser = etree.XMLParser(recover=True, ns_clean=False, resolve_entities=False)
    return etree.fromstring(xhtml_bytes, parser=parser)


def find_main_text(root: etree._Element) -> etree._Element | None:
    candidates = root.xpath(
        ".//x:div[contains(@class, 'main_text')]", namespaces=NS
    )
    return candidates[0] if candidates else None


def el_class(el: etree._Element) -> str:
    return el.get("class", "") or ""


def el_local(el: etree._Element) -> str:
    tag = el.tag
    if isinstance(tag, str) and tag.startswith(f"{{{XHTML_NS}}}"):
        return tag.split("}", 1)[1]
    return tag if isinstance(tag, str) else "?"


def text_only(el: etree._Element) -> str:
    return "".join(el.itertext())


def map_inline(
    node: etree._Element | str,
    warnings: list[dict[str, Any]],
    summary: dict[str, list[dict[str, Any]]],
    ruby_reading: str | None = None,
) -> list[dict[str, Any]]:
    """Map a child of a paragraph-like container to AAT inline nodes."""
    if isinstance(node, str):
        if not node:
            return []
        return [{"kind": "text", "value": node}]

    name = el_local(node)
    cls = el_class(node)

    if name == "ruby":
        return [map_ruby(node, warnings, summary)]

    if name == "img":
        return [map_img_gaiji(node, warnings, summary, ruby_reading)]

    if name == "span" and cls.startswith("gaiji"):
        return [map_span_gaiji(node, warnings, summary, ruby_reading)]

    if name == "em":
        children = walk_inline_children(node, warnings, summary, ruby_reading)
        return [{
            "kind": "style",
            "style_type": cls or "em",
            "content": children,
        }]

    if name == "br":
        return [{"kind": "raw", "source": "<br/>"}]

    if name == "hr":
        return [{"kind": "raw", "source": "<hr/>"}]

    if name == "span":
        # Generic span: pass through as a style container if it has a class,
        # otherwise flatten its content.
        children = walk_inline_children(node, warnings, summary, ruby_reading)
        if cls:
            return [{
                "kind": "style",
                "style_type": cls,
                "content": children,
            }]
        return children

    if name == "a":
        # Anchors: keep the inner text, drop the link.
        return walk_inline_children(node, warnings, summary, ruby_reading)

    # Unmapped element — emit a warning and keep its visible text so we don't
    # silently lose content.
    warnings.append({
        "message": f"unmapped XHTML element <{name}>",
        "path": f"/blocks/.../{name}",
    })
    children = walk_inline_children(node, warnings, summary, ruby_reading)
    if not children:
        text = text_only(node)
        if text:
            children = [{"kind": "text", "value": text}]
    return [{
        "kind": "style",
        "style_type": f"unmapped-{name}",
        "content": children,
        "x-aozora2html-unmapped": name,
    }]


def walk_inline_children(
    el: etree._Element,
    warnings: list[dict[str, Any]],
    summary: dict[str, list[dict[str, Any]]],
    ruby_reading: str | None = None,
) -> list[dict[str, Any]]:
    out: list[dict[str, Any]] = []
    if el.text:
        out.append({"kind": "text", "value": el.text})
    for child in el:
        out.extend(map_inline(child, warnings, summary, ruby_reading))
        if child.tail:
            out.append({"kind": "text", "value": child.tail})
    return out


def map_ruby(
    el: etree._Element,
    warnings: list[dict[str, Any]],
    summary: dict[str, list[dict[str, Any]]],
) -> dict[str, Any]:
    rb_els = el.xpath("./x:rb", namespaces=NS)
    rt_els = el.xpath("./x:rt", namespaces=NS)
    rt_el = rt_els[0] if rt_els else None
    reading = text_only(rt_el) if rt_el is not None else ""
    placement = "right"
    if rt_el is not None:
        rt_class = el_class(rt_el)
        if "left" in rt_class:
            placement = "left"

    base_text_parts: list[str] = []
    base_inline: list[dict[str, Any]] = []
    has_gaiji_base = False
    if rb_els:
        rb_el = rb_els[0]
        if rb_el.text:
            base_text_parts.append(rb_el.text)
            base_inline.append({"kind": "text", "value": rb_el.text})
        for child in rb_el:
            child_inline = map_inline(child, warnings, summary, reading)
            base_inline.extend(child_inline)
            for n in child_inline:
                if n.get("kind") == "gaiji":
                    has_gaiji_base = True
            ct = text_only(child)
            if ct:
                base_text_parts.append(ct)
            if child.tail:
                base_text_parts.append(child.tail)
                base_inline.append({"kind": "text", "value": child.tail})
    else:
        # No <rb> wrapper — treat element text as base.
        if el.text:
            base_text_parts.append(el.text)
            base_inline.append({"kind": "text", "value": el.text})

    base_str = "".join(base_text_parts)

    summary.setdefault("ruby.basic", []).append({
        "kind": "ruby",
        "value": {
            "base_projection": base_str,
            "reading": reading,
            "placement": placement,
        },
        "provenance": "parser",
    })

    if has_gaiji_base:
        # base_projection mirrors ab-ir inline_visible_text: text values plus
        # resolved gaiji characters (empty string when unresolved). Recompute
        # from base_inline so unresolved gaiji do not contribute.
        proj_parts: list[str] = []
        for n in base_inline:
            if n.get("kind") == "text":
                proj_parts.append(n["value"])
            elif n.get("kind") == "gaiji" and n.get("resolved"):
                proj_parts.append(n["resolved"])
        summary.setdefault("gaiji_ruby.inline_base", []).append({
            "kind": "gaiji_ruby",
            "value": {
                "base_projection": "".join(proj_parts),
                "reading": reading,
                "placement": placement,
            },
            "provenance": "parser",
        })

    aat_node: dict[str, Any] = {
        "kind": "ruby",
        "base": base_str,
        "reading": reading,
    }
    if placement == "left":
        aat_node["direction"] = "left"
    else:
        aat_node["direction"] = "right"
    return aat_node


def map_img_gaiji(
    el: etree._Element,
    warnings: list[dict[str, Any]],
    summary: dict[str, list[dict[str, Any]]],
    ruby_reading: str | None,
) -> dict[str, Any]:
    alt = el.get("alt", "") or ""
    src = el.get("src", "") or ""
    is_gaiji = "gaiji" in src
    if not is_gaiji:
        return {
            "kind": "raw",
            "source": etree.tostring(el, encoding="unicode"),
        }
    description = alt or "unknown"
    node = {
        "kind": "gaiji",
        "description": description,
        "resolved": None,
        "jis_code": None,
        "unresolved_reason": "image_fallback",
    }
    summary.setdefault("gaiji.marker", []).append({
        "kind": "gaiji",
        "value": {
            "source": "",
            "description": description,
            "description_format": None,
            "kind": "Image",
            "resolved": None,
            "ruby_reading": ruby_reading,
        },
        "provenance": "parser",
    })
    return node


def map_span_gaiji(
    el: etree._Element,
    warnings: list[dict[str, Any]],
    summary: dict[str, list[dict[str, Any]]],
    ruby_reading: str | None,
) -> dict[str, Any]:
    cls = el_class(el)
    text = text_only(el)
    description = cls
    resolved = text or None
    node = {
        "kind": "gaiji",
        "description": description,
        "resolved": resolved,
        "jis_code": None,
        "unresolved_reason": None if resolved else "unresolved_in_span",
    }
    summary.setdefault("gaiji.marker", []).append({
        "kind": "gaiji",
        "value": {
            "source": "",
            "description": description,
            "description_format": None,
            "kind": "UnicodeCodepoint" if resolved else "Unknown",
            "resolved": resolved,
            "ruby_reading": ruby_reading,
        },
        "provenance": "parser",
    })
    return node


def map_block(
    el: etree._Element,
    warnings: list[dict[str, Any]],
    summary: dict[str, list[dict[str, Any]]],
) -> list[dict[str, Any]]:
    name = el_local(el)
    cls = el_class(el)

    if name in {"h1", "h2", "h3"}:
        level = int(name[1:])
        content = walk_inline_children(el, warnings, summary)
        return [{
            "kind": "heading",
            "level": level,
            "style": cls or "normal",
            "content": content,
        }]

    if name == "div":
        m = re.match(r"jisage_(\d+)", cls)
        if m:
            indent = int(m.group(1))
            children = paragraphs_from_container(el, warnings, summary)
            return [{
                "kind": "jisage_block",
                "children": children,
                "x-indent": indent,
            }]
        # Headings via div+class (Aozora's own midashi convention)
        midashi_match = re.match(
            r"(?:.*\s)?(o|naka|ko)-midashi(?:\s|$)",
            cls,
        )
        if midashi_match:
            level_map = {"o": 1, "naka": 2, "ko": 3}
            level = level_map[midashi_match.group(1)]
            content = walk_inline_children(el, warnings, summary)
            return [{
                "kind": "heading",
                "level": level,
                "style": cls,
                "content": content,
            }]

    if name == "p":
        content = walk_inline_children(el, warnings, summary)
        return [{"kind": "paragraph", "content": content}]

    if name == "br":
        return []

    # Fallback: treat as paragraph if it has any text/inline content.
    content = walk_inline_children(el, warnings, summary)
    if content:
        warnings.append({
            "message": f"unmapped block element <{name}>",
            "path": f"/blocks/.../{name}",
        })
        return [{"kind": "paragraph", "content": content}]
    return []


def xhtml_to_aat(
    xhtml_bytes: bytes,
    source_text: str,
    encoding: str,
    src_hash: str,
) -> dict[str, Any]:
    root = parse_xhtml(xhtml_bytes)
    main = find_main_text(root)
    blocks: list[dict[str, Any]] = []
    warnings: list[dict[str, Any]] = []
    summary: dict[str, list[dict[str, Any]]] = {}

    parse_complete = main is not None

    if main is None:
        # Fallback to body text — produces one paragraph with the visible text.
        body_els = root.xpath(".//x:body", namespaces=NS)
        body = body_els[0] if body_els else root
        text = text_only(body).strip()
        blocks = [{
            "kind": "paragraph",
            "content": [{"kind": "text", "value": text}],
        }]
        warnings.append({"message": "no <div class=\"main_text\"> found"})
    else:
        # Walk children of main_text. A direct child can be:
        #  - a paragraph-equivalent (text node, span, ruby, img, em, br at top level)
        #  - a block (h1-3, div, p)
        # Aozora2html flattens most paragraphs by separating them with <br/>.
        # Build paragraphs by splitting on <br/> sequences.
        blocks = paragraphs_from_main(main, warnings, summary)

    aat: dict[str, Any] = {
        "version": 1,
        "work_id": "stdin",
        "blocks": blocks,
        "meta": {
            "adapter": ADAPTER_NAME,
            "adapter_version": ADAPTER_VERSION,
            "source_encoding": encoding,
            "source_hash": src_hash,
            "parse_complete": parse_complete,
            "warnings": warnings,
        },
    }
    if summary:
        aat["meta"]["semantic_summary"] = {"syntax": summary}
        # Mirror ab-ir's projection.warning rows so ab-compare sees them.
        if warnings:
            pw = []
            for w in warnings:
                pw.append({
                    "kind": "projection_warning",
                    "value": {
                        "syntax_id": "aozora2html.unmapped",
                        "message": w["message"],
                    },
                    "provenance": "projection",
                })
            summary.setdefault("projection.warning", []).extend(pw)
    return aat


def paragraphs_from_main(
    main: etree._Element,
    warnings: list[dict[str, Any]],
    summary: dict[str, list[dict[str, Any]]],
) -> list[dict[str, Any]]:
    return paragraphs_from_container(main, warnings, summary)


def paragraphs_from_container(
    main: etree._Element,
    warnings: list[dict[str, Any]],
    summary: dict[str, list[dict[str, Any]]],
) -> list[dict[str, Any]]:
    """Walk a flow container and split into paragraphs separated by <br/>.

    Aozora2html emits inline-flow content with <br/> as line breaks; runs of
    inline content between block-level children become paragraphs.
    """
    blocks: list[dict[str, Any]] = []
    current_inline: list[dict[str, Any]] = []

    def flush_paragraph() -> None:
        nonlocal current_inline
        if current_inline:
            # Strip trailing pure-whitespace text-only content that's just
            # newlines between siblings.
            blocks.append({"kind": "paragraph", "content": current_inline})
            current_inline = []

    if main.text:
        current_inline.append({"kind": "text", "value": main.text})

    for child in main:
        name = el_local(child)
        cls = el_class(child)

        # Block-level child: flush any accumulated inline into a paragraph,
        # then emit the block.
        is_block = (
            name in {"h1", "h2", "h3", "p"}
            or (name == "div" and (
                re.match(r"jisage_\d+", cls)
                or re.search(r"(o|naka|ko)-midashi", cls)
            ))
        )

        if is_block:
            flush_paragraph()
            blocks.extend(map_block(child, warnings, summary))
        elif name == "br":
            flush_paragraph()
        else:
            current_inline.extend(map_inline(child, warnings, summary))

        if child.tail:
            current_inline.append({"kind": "text", "value": child.tail})

    flush_paragraph()
    # Drop empty/whitespace-only paragraphs.
    blocks = [
        b for b in blocks
        if b.get("kind") != "paragraph"
        or any(_inline_has_content(n) for n in b.get("content", []))
    ]
    return blocks


def _inline_has_content(n: dict[str, Any]) -> bool:
    if n.get("kind") == "text":
        return bool((n.get("value") or "").strip())
    return True


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description="aozora2html adapter")
    parser.add_argument("--mode", choices=["aat", "html"], default="aat")
    parser.add_argument("--source")
    parser.add_argument("--xhtml")
    parser.add_argument("--version", action="store_true")
    parser.add_argument("--parser-failed", action="store_true")
    parser.add_argument("--parser-error-file")
    args = parser.parse_args(argv)

    if args.version:
        print(ADAPTER_VERSION)
        return 0

    if not args.source or not args.xhtml:
        print("error: --source and --xhtml are required for --mode aat/html",
              file=sys.stderr)
        return 1

    src_path = Path(args.source)
    xhtml_path = Path(args.xhtml)

    raw = src_path.read_bytes()
    text, encoding = detect_encoding(raw)
    src_hash = source_hash(raw)

    if args.parser_failed:
        msg = ""
        if args.parser_error_file and Path(args.parser_error_file).exists():
            msg = Path(args.parser_error_file).read_text(
                encoding="utf-8", errors="replace"
            ).strip()[:500]
        aat = {
            "version": 1,
            "work_id": "stdin",
            "blocks": [],
            "meta": {
                "adapter": ADAPTER_NAME,
                "adapter_version": ADAPTER_VERSION,
                "source_encoding": encoding,
                "source_hash": src_hash,
                "parse_complete": False,
                "warnings": [{
                    "message": f"aozora2html parser aborted: {msg}" if msg else "aozora2html parser aborted",
                }],
            },
        }
        json.dump(aat, sys.stdout, ensure_ascii=False, separators=(",", ":"))
        sys.stdout.write("\n")
        return 0

    xhtml_bytes = xhtml_path.read_bytes() if xhtml_path.exists() else b""

    if args.mode == "html":
        sys.stdout.buffer.write(xhtml_bytes)
        return 0

    aat = xhtml_to_aat(xhtml_bytes, text, encoding, src_hash)
    json.dump(aat, sys.stdout, ensure_ascii=False, separators=(",", ":"))
    sys.stdout.write("\n")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
