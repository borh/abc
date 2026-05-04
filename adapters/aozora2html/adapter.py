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
ADAPTER_VERSION = "aozora2html-adapter 0.1.0 gem-3.0.1"

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

    if name == "span" and "warichu" in cls.split():
        return [map_warichu(node, warnings, summary)]

    if name == "span" and "notes" in cls.split():
        return map_source_note(node, warnings, summary)

    if name == "span" and "caption" in cls.split():
        return [map_caption_span(node, warnings, summary)]

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


def map_warichu(
    el: etree._Element,
    warnings: list[dict[str, Any]],
    summary: dict[str, list[dict[str, Any]]],
) -> dict[str, Any]:
    text = text_only(el)
    inner = text.strip()
    if inner.startswith("（") and inner.endswith("）"):
        inner = inner[1:-1]
    if "／" in inner:
        upper_text, lower_text = inner.split("／", 1)
    elif "/" in inner:
        upper_text, lower_text = inner.split("/", 1)
    else:
        upper_text, lower_text = inner, ""
    node = {
        "kind": "warigaki",
        "upper": [{"kind": "text", "value": upper_text}] if upper_text else [],
        "lower": [{"kind": "text", "value": lower_text}] if lower_text else [],
    }
    summary.setdefault("warichu.basic", []).append({
        "kind": "warigaki",
        "value": {
            "upper_projection": upper_text,
            "lower_projection": lower_text,
        },
        "provenance": "parser",
    })
    return node


def map_source_note(
    el: etree._Element,
    warnings: list[dict[str, Any]],
    summary: dict[str, list[dict[str, Any]]],
) -> list[dict[str, Any]]:
    note = text_only(el)
    figure = source_note_figure(note)
    if figure is not None:
        summary.setdefault("figure.image_inline", []).append({
            "kind": "figure",
            "value": {
                "filename": figure["filename"],
                "alt": figure["alt"],
                "width": figure.get("width"),
                "height": figure.get("height"),
            },
            "provenance": "source-derived",
        })
        return [figure]

    caption = source_note_inline_caption(note)
    if caption is not None:
        summary.setdefault("caption.inline", []).append({
            "kind": "caption",
            "value": {
                "text": inline_visible_text(caption["content"]),
            },
            "provenance": "source-derived",
        })
        return [caption]

    return [{
        "kind": "style",
        "style_type": "notes",
        "content": [{"kind": "text", "value": note}],
    }]


def map_caption_span(
    el: etree._Element,
    warnings: list[dict[str, Any]],
    summary: dict[str, list[dict[str, Any]]],
) -> dict[str, Any]:
    content = walk_inline_children(el, warnings, summary)
    summary.setdefault("caption.inline", []).append({
        "kind": "caption",
        "value": {"text": inline_visible_text(content)},
        "provenance": "parser",
    })
    return {
        "kind": "caption",
        "content": content,
    }


def source_note_figure(note: str) -> dict[str, Any] | None:
    m = re.match(
        r"^［＃(?P<alt>.+?)（(?P<filename>[^、）]+\.(?:png|jpe?g|gif))、横(?P<width>[０-９0-9]+)×縦(?P<height>[０-９0-9]+)）入る］$",
        note,
        flags=re.IGNORECASE,
    )
    if not m:
        return None
    return {
        "kind": "figure",
        "filename": m.group("filename"),
        "alt": normalize_figure_alt(m.group("alt")),
        "css_class": "source-note",
        "width": parse_aozora_int(m.group("width")),
        "height": parse_aozora_int(m.group("height")),
        "caption": None,
        "x-provenance": "source-derived",
    }


def source_note_inline_caption(note: str) -> dict[str, Any] | None:
    m = re.match(r"^［＃「(?P<caption>.+?)」のキャプション］$", note)
    if not m:
        return None
    return {
        "kind": "caption",
        "content": [{"kind": "text", "value": m.group("caption")}],
        "x-provenance": "source-derived",
    }


def normalize_figure_alt(raw: str) -> str:
    text = raw.strip()
    if text.startswith("「") and "」" in text:
        return text[1:text.index("」")]
    text = re.sub(r"のキャプション付きの図$", "", text)
    return text.strip("「」")


def parse_aozora_int(value: str) -> int:
    table = str.maketrans("０１２３４５６７８９", "0123456789")
    return int(value.translate(table))


def parse_optional_int(value: str | None) -> int | None:
    if value is None or not value:
        return None
    try:
        return parse_aozora_int(value)
    except ValueError:
        return None


def inline_visible_text(nodes: list[dict[str, Any]]) -> str:
    parts: list[str] = []
    for node in nodes:
        kind = node.get("kind")
        if kind == "text":
            parts.append(node.get("value", ""))
        elif kind == "gaiji":
            parts.append(node.get("resolved") or "")
        elif "content" in node:
            parts.append(inline_visible_text(node.get("content", [])))
    return "".join(parts)


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
    if has_gaiji_base:
        aat_node["base_content"] = base_inline
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
        node = {
            "kind": "figure",
            "filename": Path(src).name,
            "alt": normalize_figure_alt(alt),
            "css_class": el_class(el),
            "width": parse_optional_int(el.get("width")),
            "height": parse_optional_int(el.get("height")),
            "caption": None,
        }
        summary.setdefault("figure.image_inline", []).append({
            "kind": "figure",
            "value": {
                "filename": node["filename"],
                "alt": node["alt"],
                "width": node["width"],
                "height": node["height"],
            },
            "provenance": "parser",
        })
        return node
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
        midashi_kind = midashi_kind_from_class(cls)
        if midashi_kind:
            level_map = {"o": 1, "naka": 2, "ko": 3}
            level = level_map[midashi_kind]
            content = walk_inline_children(el, warnings, summary)
            return [{
                "kind": "heading",
                "level": level,
                "style": heading_style_from_class(cls),
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

    blocks = normalize_source_derived_blocks(blocks, summary, source_text)
    blocks = attach_following_captions(blocks, summary)

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
                or midashi_kind_from_class(cls)
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


def midashi_kind_from_class(cls: str) -> str | None:
    tokens = set(cls.split())
    for token in ("o-midashi", "naka-midashi", "ko-midashi"):
        if token in tokens:
            return token.split("-", 1)[0]
    return None


def heading_style_from_class(cls: str) -> str:
    tokens = set(cls.split())
    if any(token.startswith("mado-") or token == "mado" for token in tokens):
        return "mado"
    if any(token.startswith("dogyo-") or token == "dogyo" for token in tokens):
        return "dogyo"
    return "normal"


def normalize_source_derived_blocks(
    blocks: list[dict[str, Any]],
    summary: dict[str, list[dict[str, Any]]],
    source_text: str,
) -> list[dict[str, Any]]:
    out: list[dict[str, Any]] = []
    for block in blocks:
        if block.get("kind") == "paragraph":
            normalized = normalize_source_derived_paragraph(block, summary, source_text)
            out.extend(normalized)
        elif block.get("kind") == "heading":
            normalized_heading = heading_from_source_note(source_text, block) or dict(block)
            normalized_heading["style"] = heading_style_from_class(
                normalized_heading.get("style", "")
            )
            out.append(normalized_heading)
        else:
            out.append(block)
    return strip_text_after_page_break(out)


def heading_from_source_note(
    source_text: str, block: dict[str, Any]
) -> dict[str, Any] | None:
    content_text = inline_visible_text(block.get("content", []))
    m = re.search(
        r"［＃「(?P<target>.+?)」(?:は|の)(?P<style>同行|窓)?(?P<size>大|中|小)見出し］",
        source_text,
    )
    if not m or m.group("target") != content_text:
        return None
    level_map = {"大": 1, "中": 2, "小": 3}
    style = "normal"
    if m.group("style") == "同行":
        style = "dogyo"
    elif m.group("style") == "窓":
        style = "mado"
    return {
        "kind": "heading",
        "level": level_map[m.group("size")],
        "style": style,
        "content": block.get("content", []),
        "x-provenance": "source-derived",
    }


def strip_text_after_page_break(blocks: list[dict[str, Any]]) -> list[dict[str, Any]]:
    out: list[dict[str, Any]] = []
    previous_was_page_break = False
    for block in blocks:
        current = block
        if previous_was_page_break and current.get("kind") == "paragraph":
            content = current.get("content", [])
            if content and content[0].get("kind") == "text":
                current = dict(current)
                current["content"] = [dict(content[0]), *content[1:]]
                current["content"][0]["value"] = current["content"][0].get("value", "").lstrip()
        out.append(current)
        previous_was_page_break = (
            current.get("kind") == "paragraph"
            and current.get("x-break-kind") == "page"
        )
    return out


def normalize_source_derived_paragraph(
    block: dict[str, Any],
    summary: dict[str, list[dict[str, Any]]],
    source_text: str,
) -> list[dict[str, Any]]:
    content = block.get("content", [])
    meaningful = [
        node
        for node in content
        if node.get("kind") != "text" or (node.get("value") or "").strip()
    ]
    if len(meaningful) == 1 and (
        meaningful[0].get("kind") == "style"
        and str(meaningful[0].get("style_type", "")).startswith("unmapped-h")
    ):
        heading = heading_from_source_note(
            source_text,
            {
                "kind": "heading",
                "level": 1,
                "style": "normal",
                "content": meaningful[0].get("content", []),
            },
        )
        if heading is not None:
            return [heading]
    if len(meaningful) == 1 and meaningful[0].get("kind") == "text":
        figure = source_text_figure(meaningful[0].get("value", ""))
        if figure is not None:
            summary.setdefault("figure.image_inline", []).append({
                "kind": "figure",
                "value": {
                    "filename": figure["filename"],
                    "alt": figure["alt"],
                    "width": figure.get("width"),
                    "height": figure.get("height"),
                },
                "provenance": "source-derived",
            })
            return [{"kind": "paragraph", "content": [figure]}]

    line_break = paragraph_line_break_text(content)
    if line_break is not None:
        return [{
            "kind": "paragraph",
            "content": [{
                "kind": "text",
                "value": line_break,
                "x-break-kind": "line",
                "x-provenance": "source-derived",
            }],
        }]

    page_break = paragraph_is_note(content, "［＃改ページ］")
    if page_break:
        return [{
            "kind": "paragraph",
            "content": [],
            "x-break-kind": "page",
            "x-provenance": "source-derived",
        }]

    return [block]


def source_text_figure(text: str) -> dict[str, Any] | None:
    m = re.match(
        r"^(?P<alt>.+?)（(?P<filename>[^、）]+\.(?:png|jpe?g|gif))、横(?P<width>[０-９0-9]+)×縦(?P<height>[０-９0-9]+)）入る$",
        text.strip(),
        flags=re.IGNORECASE,
    )
    if not m:
        return None
    return {
        "kind": "figure",
        "filename": m.group("filename"),
        "alt": normalize_figure_alt(m.group("alt")),
        "css_class": "source-text",
        "width": parse_aozora_int(m.group("width")),
        "height": parse_aozora_int(m.group("height")),
        "caption": None,
        "x-provenance": "source-derived",
    }


def paragraph_line_break_text(content: list[dict[str, Any]]) -> str | None:
    meaningful = [
        node
        for node in content
        if node.get("kind") != "text" or (node.get("value") or "").strip()
    ]
    if len(meaningful) != 3:
        return None
    before, note, after = meaningful
    if before.get("kind") != "text" or after.get("kind") != "text":
        return None
    if not node_is_note(note, "［＃改行］"):
        return None
    return f"{before.get('value', '')}\n{after.get('value', '')}"


def paragraph_is_note(content: list[dict[str, Any]], note_text: str) -> bool:
    meaningful = [
        node
        for node in content
        if node.get("kind") != "text" or (node.get("value") or "").strip()
    ]
    return len(meaningful) == 1 and node_is_note(meaningful[0], note_text)


def node_is_note(node: dict[str, Any], note_text: str) -> bool:
    if node.get("kind") != "style" or node.get("style_type") != "notes":
        return False
    return inline_visible_text(node.get("content", [])) == note_text


def _inline_has_content(n: dict[str, Any]) -> bool:
    if n.get("kind") == "text":
        return bool((n.get("value") or "").strip())
    return True


def attach_following_captions(
    blocks: list[dict[str, Any]],
    summary: dict[str, list[dict[str, Any]]],
) -> list[dict[str, Any]]:
    out: list[dict[str, Any]] = []
    index = 0
    while index < len(blocks):
        block = blocks[index]
        figure = single_figure(block)
        if figure is not None and index + 1 < len(blocks):
            caption, remainder = first_caption_and_remainder(blocks[index + 1])
            if caption is not None and figure.get("caption") is None:
                figure["caption"] = caption.get("content", [])
                figure["x-caption-provenance"] = "source-derived"
                summary.setdefault("figure.image_caption", []).append({
                    "kind": "figure_caption",
                    "value": {
                        "filename": figure.get("filename"),
                        "caption": inline_visible_text(figure["caption"]),
                    },
                    "provenance": "source-derived",
                })
                out.append(block)
                if remainder is not None:
                    out.append(remainder)
                index += 2
                continue
        out.append(block)
        index += 1
    return out


def single_figure(block: dict[str, Any]) -> dict[str, Any] | None:
    if block.get("kind") != "paragraph":
        return None
    meaningful = [
        node
        for node in block.get("content", [])
        if node.get("kind") != "text" or (node.get("value") or "").strip()
    ]
    if len(meaningful) == 1 and meaningful[0].get("kind") == "figure":
        return meaningful[0]
    return None


def first_caption_and_remainder(
    block: dict[str, Any],
) -> tuple[dict[str, Any] | None, dict[str, Any] | None]:
    if block.get("kind") != "paragraph":
        return None, block
    content = block.get("content", [])
    for index, node in enumerate(content):
        if node.get("kind") == "caption":
            remainder_content = [
                item
                for item_index, item in enumerate(content)
                if item_index != index
                and (item.get("kind") != "text" or (item.get("value") or "").strip())
            ]
            if not remainder_content:
                return node, None
            remainder = dict(block)
            remainder["content"] = remainder_content
            return node, remainder
    return None, block


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
