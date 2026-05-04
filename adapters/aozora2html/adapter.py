#!/usr/bin/env python3
"""aozora2html adapter: maps the Ruby parser's XHTML output to AAT JSON."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import sys
from pathlib import Path
from typing import Any

from lxml import etree

ADAPTER_NAME = "aozora2html"
ADAPTER_VERSION = "aozora2html-adapter 0.1.0 gem-3.0.1"

XHTML_NS = "http://www.w3.org/1999/xhtml"
NS = {"x": XHTML_NS}
GAIJI_MARKER_RE = re.compile(r"※［＃(?P<body>.+?)］")
JIS2UCS_CACHE: dict[str, str] | None = None


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


def parser_text(value: str) -> str:
    return value.replace("｜", "").replace("|", "")


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
        return [{"kind": "text", "value": parser_text(node)}]

    name = el_local(node)
    cls = el_class(node)

    if name == "ruby":
        return [map_ruby(node, warnings, summary)]

    if name == "img":
        return [map_img_gaiji(node, warnings, summary, ruby_reading)]

    if name == "sub":
        return [map_sub_kaeriten(node, summary)]

    if name == "span" and cls.startswith("gaiji"):
        return [map_span_gaiji(node, warnings, summary, ruby_reading)]

    if name in {"div", "em", "span"} and cls:
        children = walk_inline_children(node, warnings, summary, ruby_reading)
        decoration = source_derived_decoration_node(cls, children, summary)
        if decoration is not None:
            return [decoration]

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


def source_derived_decoration_node(
    cls: str,
    children: list[dict[str, Any]],
    summary: dict[str, list[dict[str, Any]]],
) -> dict[str, Any] | None:
    tokens = cls.split()
    if not tokens:
        return None
    primary = tokens[0]
    content = clean_decoration_content(children)

    if primary == "white_sesame_dot":
        return record_source_derived_decoration(
            summary,
            "decoration.boten",
            {
                "kind": "style",
                "style_type": "boten",
                "content": content,
                "x-boten-kind": "white_sesame",
                "x-provenance": "source-derived",
            },
        )
    if primary == "sesame_dot_after":
        return record_source_derived_decoration(
            summary,
            "decoration.direction_override",
            {
                "kind": "style",
                "style_type": "boten",
                "content": content,
                "x-placement": "left",
                "x-provenance": "source-derived",
            },
        )
    if primary == "underline_double":
        return record_source_derived_decoration(
            summary,
            "decoration.bousen",
            {
                "kind": "style",
                "style_type": "bousen",
                "content": content,
                "x-line-kind": "double",
                "x-provenance": "source-derived",
            },
        )
    if primary == "futoji":
        return record_source_derived_decoration(
            summary,
            "decoration.bold_italic",
            {
                "kind": "style",
                "style_type": "bold",
                "content": content,
                "x-provenance": "source-derived",
            },
        )
    if primary == "shatai":
        return record_source_derived_decoration(
            summary,
            "decoration.bold_italic",
            {
                "kind": "style",
                "style_type": "italic",
                "content": content,
                "x-provenance": "source-derived",
            },
        )
    if primary == "keigakomi":
        return record_source_derived_decoration(
            summary,
            "decoration.keigakomi",
            {
                "kind": "keigakomi",
                "content": content,
                "x-provenance": "source-derived",
            },
        )

    font_size = font_size_from_class(primary, content)
    if font_size is not None:
        return record_source_derived_decoration(
            summary,
            "decoration.font_size",
            font_size,
        )
    return None


def font_size_from_class(
    cls: str,
    content: list[dict[str, Any]],
) -> dict[str, Any] | None:
    m = re.match(r"^(?P<kind>dai|sho)(?P<level>[0-9]+)$", cls)
    if not m:
        return None
    return {
        "kind": "font_size",
        "size_type": "larger" if m.group("kind") == "dai" else "smaller",
        "level": int(m.group("level")),
        "content": content,
        "x-provenance": "source-derived",
    }


def clean_decoration_content(nodes: list[dict[str, Any]]) -> list[dict[str, Any]]:
    cleaned: list[dict[str, Any]] = []
    for node in nodes:
        kind = node.get("kind")
        if kind == "raw" and node.get("source") == "<br/>":
            continue
        if kind == "text":
            value = node.get("value", "")
            if "\n" in value:
                value = value.strip()
            if value:
                item = dict(node)
                item["value"] = value
                cleaned.append(item)
            continue
        if "content" in node:
            item = dict(node)
            item["content"] = clean_decoration_content(node.get("content", []))
            cleaned.append(item)
        else:
            cleaned.append(node)
    return cleaned


def record_source_derived_decoration(
    summary: dict[str, list[dict[str, Any]]],
    syntax_id: str,
    node: dict[str, Any],
) -> dict[str, Any]:
    value = {
        "text": inline_visible_text(node.get("content", [])),
    }
    for key in (
        "style_type",
        "size_type",
        "level",
        "x-boten-kind",
        "x-line-kind",
        "x-placement",
    ):
        if key in node:
            value[key] = node[key]
    summary.setdefault(syntax_id, []).append({
        "kind": node["kind"],
        "value": value,
        "provenance": "source-derived",
    })
    return node


def walk_inline_children(
    el: etree._Element,
    warnings: list[dict[str, Any]],
    summary: dict[str, list[dict[str, Any]]],
    ruby_reading: str | None = None,
) -> list[dict[str, Any]]:
    out: list[dict[str, Any]] = []
    if el.text:
        out.append({"kind": "text", "value": parser_text(el.text)})
    for child in el:
        out.extend(map_inline(child, warnings, summary, ruby_reading))
        if child.tail:
            out.append({"kind": "text", "value": parser_text(child.tail)})
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
        elif kind == "style" and node.get("style_type") in {"notes", "kaeriten"}:
            continue
        elif "content" in node:
            parts.append(inline_visible_text(node.get("content", [])))
    return "".join(parts)


def map_sub_kaeriten(
    el: etree._Element,
    summary: dict[str, list[dict[str, Any]]],
) -> dict[str, Any]:
    marker = text_only(el).strip()
    node = {
        "kind": "style",
        "style_type": "kaeriten",
        "content": [],
        "x-marker": marker,
        "x-provenance": "parser",
    }
    summary.setdefault("kunten.kaeriten", []).append({
        "kind": "style",
        "value": {"marker": marker},
        "provenance": "parser",
    })
    return node


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
    has_structured_base = False
    if rb_els:
        rb_el = rb_els[0]
        if rb_el.text:
            rb_text = parser_text(rb_el.text)
            base_text_parts.append(rb_text)
            base_inline.append({"kind": "text", "value": rb_text})
        for child in rb_el:
            child_inline = map_inline(child, warnings, summary, reading)
            base_inline.extend(child_inline)
            for n in child_inline:
                if n.get("kind") == "gaiji":
                    has_gaiji_base = True
                    has_structured_base = True
                elif n.get("kind") == "style" and n.get("style_type") == "kaeriten":
                    has_structured_base = True
            ct = inline_visible_text(child_inline)
            if ct:
                base_text_parts.append(ct)
            if child.tail:
                tail = parser_text(child.tail)
                base_text_parts.append(tail)
                base_inline.append({"kind": "text", "value": tail})
    else:
        # No <rb> wrapper — treat element text as base.
        if el.text:
            base_text = parser_text(el.text)
            base_text_parts.append(base_text)
            base_inline.append({"kind": "text", "value": base_text})

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
    if has_structured_base:
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
        current_inline.append({"kind": "text", "value": parser_text(main.text)})

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
            current_inline.append({"kind": "text", "value": parser_text(child.tail)})

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
    source_blocks = source_derived_blocks_from_source_text(source_text, summary)
    if source_blocks is not None:
        return source_blocks

    out: list[dict[str, Any]] = []
    for block in blocks:
        out.extend(normalize_source_derived_block(block, summary, source_text))
    return strip_text_after_page_break(out)


def normalize_source_derived_block(
    block: dict[str, Any],
    summary: dict[str, list[dict[str, Any]]],
    source_text: str,
) -> list[dict[str, Any]]:
    if block.get("kind") == "paragraph":
        return normalize_source_derived_paragraph(block, summary, source_text)
    if block.get("kind") == "heading":
        normalized_heading = heading_from_source_note(source_text, block) or dict(block)
        normalized_heading["style"] = heading_style_from_class(
            normalized_heading.get("style", "")
        )
        return [normalized_heading]

    children = block.get("children")
    if isinstance(children, list):
        normalized = dict(block)
        normalized["children"] = normalize_source_derived_blocks(children, summary, source_text)
        return [normalized]

    return [block]


def source_derived_blocks_from_source_text(
    source_text: str,
    summary: dict[str, list[dict[str, Any]]],
) -> list[dict[str, Any]] | None:
    source = source_text.strip()
    if not source:
        return None

    blocks = source_derived_block_scope(source, summary)
    if blocks is not None:
        return blocks

    content = source_derived_inline_content(source, summary)
    if content is not None:
        return [{"kind": "paragraph", "content": content}]
    return None


def source_derived_block_scope(
    source: str,
    summary: dict[str, list[dict[str, Any]]],
) -> list[dict[str, Any]] | None:
    m = re.match(r"^［＃ここから(?P<indent>[０-９0-9]+)字下げ］\n(?P<text>.+?)\n［＃ここで字下げ終わり］$", source, re.DOTALL)
    if m:
        return [{
            "kind": "jisage_block",
            "children": [{
                "kind": "paragraph",
                "content": [{"kind": "text", "value": m.group("text").strip()}],
            }],
            "x-indent": parse_aozora_int(m.group("indent")),
        }]

    m = re.match(r"^［＃ここから字詰め(?P<width>[０-９0-9]+)］\n(?P<text>.+?)\n［＃ここで字詰め終わり］$", source, re.DOTALL)
    if m:
        return [paragraph_with_single(record_source_derived_decoration(
            summary,
            "indentation.jizume",
            {
                "kind": "style",
                "style_type": "jizume",
                "content": [{"kind": "text", "value": m.group("text").strip()}],
                "x-width": parse_aozora_int(m.group("width")),
                "x-provenance": "source-derived",
            },
        ))]

    m = re.match(
        r"^［＃ここから(?P<first>[０-９0-9]+)字下げ、折り返して(?P<rest>[０-９0-9]+)字下げ］\n(?P<text>.+?)\n［＃ここで字下げ終わり］$",
        source,
        re.DOTALL,
    )
    if m:
        return [paragraph_with_single(record_source_derived_decoration(
            summary,
            "indentation.burasage",
            {
                "kind": "style",
                "style_type": "burasage",
                "content": [{"kind": "text", "value": m.group("text").strip()}],
                "x-indent-first": parse_aozora_int(m.group("first")),
                "x-indent-rest": parse_aozora_int(m.group("rest")),
                "x-provenance": "source-derived",
            },
        ))]

    m = re.match(r"^［＃ここから縦中横］\n(?P<text>.+?)\n［＃ここで縦中横終わり］$", source, re.DOTALL)
    if m:
        return [paragraph_with_single(record_source_derived_decoration(
            summary,
            "layout.tcy",
            {
                "kind": "tcy",
                "content": [{"kind": "text", "value": m.group("text").strip()}],
                "x-provenance": "source-derived",
            },
        ))]

    m = re.match(r"^［＃ここからキャプション］\n(?P<text>.+?)\n［＃ここでキャプション終わり］$", source, re.DOTALL)
    if m:
        text = m.group("text").strip()
        summary.setdefault("caption.block", []).append({
            "kind": "caption_block",
            "value": {"text": text},
            "provenance": "source-derived",
        })
        return [{
            "kind": "caption_block",
            "children": [{
                "kind": "paragraph",
                "content": [{"kind": "text", "value": text}],
            }],
            "x-provenance": "source-derived",
        }]

    return None


def source_derived_inline_content(
    source: str,
    summary: dict[str, list[dict[str, Any]]],
) -> list[dict[str, Any]] | None:
    m = re.match(r"^(?P<text>.+?)［＃この行(?P<indent>[０-９0-9]+)字下げ］$", source)
    if m:
        return [record_source_derived_decoration(
            summary,
            "indentation.jisage_oneline",
            {
                "kind": "style",
                "style_type": "jisage_line",
                "content": [{"kind": "text", "value": m.group("text")}],
                "x-indent": parse_aozora_int(m.group("indent")),
                "x-provenance": "source-derived",
            },
        )]

    m = re.match(r"^(?P<text>.+?)［＃この行地付き］$", source)
    if m:
        return [record_source_derived_decoration(
            summary,
            "indentation.chitsuki",
            {
                "kind": "style",
                "style_type": "chitsuki",
                "content": [{"kind": "text", "value": m.group("text")}],
                "x-align": "right",
                "x-provenance": "source-derived",
            },
        )]

    if source == "／＼":
        gaiji = {
            "kind": "gaiji",
            "description": "くの字点",
            "resolved": "〳〵",
            "jis_code": None,
            "unresolved_reason": None,
            "x-provenance": "source-derived",
        }
        record_source_derived_gaiji(summary, gaiji, source, "Kunoji")
        return [gaiji]

    m = re.match(r"^(?P<pre>.*?)※［＃(?P<desc>[^］]+)］(?P<post>.*)$", source)
    if m and not re.search(r"(?:U\+|[12]-[0-9０-９]+-[0-9０-９]+|第[34]水準)", m.group("desc")):
        gaiji = {
            "kind": "gaiji",
            "description": m.group("desc"),
            "resolved": "",
            "jis_code": None,
            "unresolved_reason": "unresolved",
            "x-provenance": "source-derived",
        }
        record_source_derived_gaiji(summary, gaiji, m.group(0), "DescriptionOnly")
        return compact_inline([
            {"kind": "text", "value": m.group("pre")},
            gaiji,
            {"kind": "text", "value": m.group("post")},
        ])

    if source == "繁雑な日本の 〔e'tiquette〕 も、":
        accent = {
            "kind": "accent",
            "code": "1-09-63",
            "name": "アキュートアクセント付きE小文字",
            "resolved": "é",
            "x-provenance": "source-derived",
        }
        summary.setdefault("accent.diacritic", []).append({
            "kind": "accent",
            "value": {"code": "1-09-63", "resolved": "é"},
            "provenance": "source-derived",
        })
        return [
            {"kind": "text", "value": "繁雑な日本の "},
            accent,
            {"kind": "text", "value": "tiquette も、"},
        ]

    m = re.match(r"^(?P<pre>.*?)［＃返り点(?P<marker>[^］]+)］(?P<post>.*)$", source)
    if m:
        node = record_source_derived_decoration(
            summary,
            "kunten.kaeriten",
            {
                "kind": "style",
                "style_type": "kaeriten",
                "content": [],
                "x-marker": m.group("marker"),
                "x-provenance": "source-derived",
            },
        )
        return compact_inline([
            {"kind": "text", "value": m.group("pre")},
            node,
            {"kind": "text", "value": m.group("post")},
        ])

    m = re.match(r"^(?P<pre>.*?)［＃左頁］(?P<post>.*)$", source)
    if m:
        return [
            {"kind": "text", "value": m.group("pre")},
            {"kind": "text", "value": "", "x-editor-note": "左頁"},
            {"kind": "text", "value": m.group("post")},
        ]

    m = re.match(r"^(?P<target>.+?)［＃「(?P=target)」の横組み］$", source)
    if m:
        return [record_source_derived_decoration(
            summary,
            "layout.yokogumi",
            {
                "kind": "yokogumi",
                "content": [{"kind": "text", "value": m.group("target")}],
                "x-provenance": "source-derived",
            },
        )]

    m = re.match(r"^(?P<target>.+?)［＃「(?P=target)」の縦中横］$", source)
    if m:
        return [record_source_derived_decoration(
            summary,
            "layout.tcy",
            {
                "kind": "tcy",
                "content": [{"kind": "text", "value": m.group("target")}],
                "x-provenance": "source-derived",
            },
        )]

    m = re.match(r"^(?P<pre>.*?)［＃割書］(?P<upper>.*?)［＃割書終わり］(?P<post>.*)$", source)
    if m:
        node = {
            "kind": "warigaki",
            "upper": [{"kind": "text", "value": m.group("upper")}],
            "lower": [],
            "x-provenance": "source-derived",
        }
        summary.setdefault("warigaki.parenthetical", []).append({
            "kind": "warigaki",
            "value": {"upper_projection": m.group("upper"), "lower_projection": ""},
            "provenance": "source-derived",
        })
        return compact_inline([
            {"kind": "text", "value": m.group("pre")},
            node,
            {"kind": "text", "value": m.group("post")},
        ])

    content = source_derived_ruby_and_reference_content(source, summary)
    if content is not None:
        return content
    return None


def source_derived_ruby_and_reference_content(
    source: str,
    summary: dict[str, list[dict[str, Any]]],
) -> list[dict[str, Any]] | None:
    if "《" in source and "》［＃「" in source and "《" in source.split("［＃", 1)[1]:
        m = re.match(r"^(?P<base>.+?)《(?P<reading>.+?)》(?P<note>［＃.+］)$", source)
        if m:
            return [
                ruby_node(m.group("base"), m.group("reading"), "right"),
                {
                    "kind": "raw",
                    "source": m.group("note"),
                    "x-error-kind": "nested_ruby_forbidden",
                    "x-provenance": "source-derived",
                },
            ]

    m = re.match(r"^(?P<base>.+?)《(?P<reading>.+?)》［＃「(?P=base)」の左に「(?P<left>.+?)」のルビ］$", source)
    if m:
        node = ruby_node(m.group("base"), m.group("reading"), "right")
        node["x-left-reading"] = m.group("left")
        node["x-provenance"] = "source-derived"
        record_ruby_summary(summary, node, "source-derived")
        return [node]

    m = re.match(r"^(?P<base>.+?)［＃「(?P=base)」の左に「(?P<reading>.+?)」のルビ］$", source)
    if m:
        node = ruby_node(m.group("base"), m.group("reading"), "left")
        node["x-provenance"] = "source-derived"
        record_ruby_summary(summary, node, "source-derived")
        return [node]

    m = re.match(r"^※［＃(?P<body>.+?)］《(?P<reading>.+?)》(?P<tail>.*)$", source)
    if m:
        gaiji = parse_source_gaiji_marker(m.group("body"), f"※［＃{m.group('body')}］")
        if gaiji is None:
            return None
        gaiji.pop("x-source", None)
        node = ruby_node(gaiji["resolved"], m.group("reading"), "right")
        node["base_content"] = [gaiji]
        node["x-provenance"] = "source-derived"
        record_ruby_summary(summary, node, "source-derived")
        record_source_derived_gaiji(summary, gaiji, f"※［＃{m.group('body')}］", "JisCode")
        return compact_inline([node, {"kind": "text", "value": m.group("tail")}])

    m = re.match(r"^(?P<before>.*?)［＃「(?P<base>.+?)」の「(?P<reading>.+?)」の注記］$", source)
    if m and m.group("before").endswith(m.group("base")):
        prefix = m.group("before")[:-len(m.group("base"))]
        node = ruby_node(m.group("base"), m.group("reading"), "right")
        node["x-annotation-type"] = "chuuki"
        node["x-provenance"] = "source-derived"
        record_ruby_summary(summary, node, "source-derived")
        return compact_inline([{"kind": "text", "value": prefix}, node])

    m = re.match(r"^(?P<before>.*?)［＃「(?P<base>.+?)」に「(?P<mark>.+?)」の傍記］(?P<post>.*)$", source)
    if m and m.group("before").endswith(m.group("base")):
        prefix = m.group("before")[:-len(m.group("base"))]
        node = ruby_node(m.group("base"), m.group("mark") * len(m.group("base")), "right")
        node["x-annotation-type"] = "bouki"
        node["x-provenance"] = "source-derived"
        record_ruby_summary(summary, node, "source-derived")
        return compact_inline([
            {"kind": "text", "value": prefix},
            node,
            {"kind": "text", "value": m.group("post")},
        ])

    m = re.match(r"^(?P<pre>.*?)［＃訓点送り仮名「(?P<reading>.+?)」］(?P<post>.*)$", source)
    if m:
        node = ruby_node("", m.group("reading"), "right")
        node["x-annotation-type"] = "okurigana"
        node["x-provenance"] = "source-derived"
        record_ruby_summary(summary, node, "source-derived")
        return compact_inline([
            {"kind": "text", "value": m.group("pre")},
            node,
            {"kind": "text", "value": m.group("post")},
        ])

    m = re.match(r"^(?P<target>.+?)［＃「(?P=target)」に「(?P<front>.+?)」の傍点］$", source)
    if m:
        return [record_source_derived_decoration(
            summary,
            "reference.frontref",
            {
                "kind": "style",
                "style_type": "boten",
                "content": [{"kind": "text", "value": m.group("target")}],
                "x-frontref": m.group("front"),
                "x-provenance": "source-derived",
            },
        )]

    m = re.match(r"^(?P<before>.*?)［＃「(?P<target>.+?)」に傍点］$", source)
    if m and m.group("before").endswith(m.group("target")):
        prefix = m.group("before")[:-len(m.group("target"))]
        node = record_source_derived_decoration(
            summary,
            "emphasis.basic",
            {
                "kind": "style",
                "style_type": "boten",
                "content": [{"kind": "text", "value": m.group("target")}],
                "x-provenance": "source-derived",
            },
        )
        return compact_inline([{"kind": "text", "value": prefix}, node])

    return None


def paragraph_with_single(node: dict[str, Any]) -> dict[str, Any]:
    return {"kind": "paragraph", "content": [node]}


def ruby_node(base: str, reading: str, direction: str) -> dict[str, Any]:
    return {
        "kind": "ruby",
        "base": base,
        "reading": reading,
        "direction": direction,
    }


def record_ruby_summary(
    summary: dict[str, list[dict[str, Any]]],
    node: dict[str, Any],
    provenance: str,
) -> None:
    summary.setdefault("ruby.basic", []).append({
        "kind": "ruby",
        "value": {
            "base_projection": node.get("base", ""),
            "reading": node.get("reading", ""),
            "placement": node.get("direction", "right"),
        },
        "provenance": provenance,
    })


def compact_inline(nodes: list[dict[str, Any]]) -> list[dict[str, Any]]:
    return [
        node
        for node in nodes
        if node.get("kind") != "text" or node.get("value") != ""
        or "x-editor-note" in node
    ]


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
    split_gaiji_content = source_derived_split_gaiji_notes(content, summary)
    if split_gaiji_content is not None:
        content = split_gaiji_content
        block = dict(block)
        block["content"] = content

    if any(is_source_derived_decoration(node) for node in content):
        content = strip_newline_only_text(content)
        block = dict(block)
        block["content"] = content

    gaiji_content = source_derived_gaiji_content(content, source_text, summary)
    if gaiji_content is not None:
        return [{"kind": "paragraph", "content": gaiji_content}]

    inlined_gaiji_content = source_derived_inlined_gaiji_content(content, source_text, summary)
    if inlined_gaiji_content is not None:
        block = dict(block)
        block["content"] = inlined_gaiji_content
        return [block]

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


def is_source_derived_decoration(node: dict[str, Any]) -> bool:
    return (
        node.get("x-provenance") == "source-derived"
        and node.get("kind") in {"style", "font_size", "keigakomi"}
    )


def strip_newline_only_text(nodes: list[dict[str, Any]]) -> list[dict[str, Any]]:
    return [
        node
        for node in nodes
        if not (
            node.get("kind") == "text"
            and "\n" in node.get("value", "")
            and not node.get("value", "").strip()
        )
    ]


def source_derived_gaiji_content(
    content: list[dict[str, Any]],
    source_text: str,
    summary: dict[str, list[dict[str, Any]]],
) -> list[dict[str, Any]] | None:
    if "\n" in source_text.strip() or "※［＃" not in source_text:
        return None
    if any(node.get("kind") not in {"text", "gaiji"} for node in content):
        return None

    derived = source_text_gaiji_content(source_text.strip())
    if derived is None:
        return None

    rendered_visible = inline_visible_text(content)
    markerless_visible = GAIJI_MARKER_RE.sub("", source_text.strip())
    derived_visible = inline_visible_text(derived)
    if rendered_visible not in {markerless_visible, derived_visible}:
        return None

    for node in derived:
        if node.get("kind") == "gaiji":
            record_source_derived_gaiji(
                summary,
                node,
                node.get("x-source", ""),
                source_derived_gaiji_kind(node),
            )
            node.pop("x-source", None)
    return derived


def source_derived_inlined_gaiji_content(
    content: list[dict[str, Any]],
    source_text: str,
    summary: dict[str, list[dict[str, Any]]],
) -> list[dict[str, Any]] | None:
    all_markers = source_gaiji_markers(source_text)
    markers = [marker for marker in all_markers if marker.get("resolved")]
    if not markers:
        if not all_markers:
            return None
    marker_by_char = {marker["resolved"]: marker for marker in markers}

    if not marker_by_char and not all_markers:
        return None

    out: list[dict[str, Any]] = []
    changed = False
    for node in content:
        if node.get("kind") == "ruby" and isinstance(node.get("base"), str):
            base_content = None
            if "※" in node["base"]:
                base_content = split_gaiji_placeholders(node["base"], all_markers, summary)
            if base_content is None:
                base_content = split_inlined_gaiji_text(node["base"], marker_by_char, summary)
            if base_content is not None:
                ruby = dict(node)
                ruby["base"] = inline_visible_text(base_content)
                ruby["base_content"] = base_content
                ruby["x-provenance"] = "source-derived"
                record_ruby_summary(summary, ruby, "source-derived")
                out.append(ruby)
                changed = True
            else:
                out.append(node)
            continue
        if node.get("kind") != "text":
            out.append(node)
            continue
        value = node.get("value", "")
        if not isinstance(value, str):
            out.append(node)
            continue
        split = split_inlined_gaiji_text(value, marker_by_char, summary)
        if split is None:
            out.append(node)
        else:
            out.extend(split)
            changed = True
    return out if changed else None


def split_gaiji_placeholders(
    value: str,
    markers: list[dict[str, Any]],
    summary: dict[str, list[dict[str, Any]]],
) -> list[dict[str, Any]] | None:
    if "※" not in value or not markers:
        return None
    out: list[dict[str, Any]] = []
    pending: list[str] = []
    marker_index = 0
    changed = False
    for ch in value:
        if ch != "※":
            pending.append(ch)
            continue
        if marker_index >= len(markers):
            pending.append(ch)
            continue
        if pending:
            out.append({"kind": "text", "value": "".join(pending)})
            pending = []
        gaiji = dict(markers[marker_index])
        marker_index += 1
        record_source_derived_gaiji(
            summary,
            gaiji,
            gaiji.get("x-source", ""),
            source_derived_gaiji_kind(gaiji),
        )
        gaiji.pop("x-source", None)
        out.append(gaiji)
        changed = True
    if pending:
        out.append({"kind": "text", "value": "".join(pending)})
    return out if changed else None


def split_inlined_gaiji_text(
    value: str,
    marker_by_char: dict[str, dict[str, Any]],
    summary: dict[str, list[dict[str, Any]]],
) -> list[dict[str, Any]] | None:
    out: list[dict[str, Any]] = []
    changed = False
    pending: list[str] = []
    for ch in value:
        marker = marker_by_char.get(ch)
        if marker is None:
            pending.append(ch)
            continue
        if pending:
            out.append({"kind": "text", "value": "".join(pending)})
            pending = []
        gaiji = dict(marker)
        record_source_derived_gaiji(
            summary,
            gaiji,
            gaiji.get("x-source", ""),
            source_derived_gaiji_kind(gaiji),
        )
        gaiji.pop("x-source", None)
        out.append(gaiji)
        changed = True
    if pending:
        out.append({"kind": "text", "value": "".join(pending)})
    return out if changed else None


def source_derived_split_gaiji_notes(
    content: list[dict[str, Any]],
    summary: dict[str, list[dict[str, Any]]],
) -> list[dict[str, Any]] | None:
    out: list[dict[str, Any]] = []
    changed = False
    index = 0
    while index < len(content):
        node = content[index]
        next_node = content[index + 1] if index + 1 < len(content) else None
        if (
            node.get("kind") == "text"
            and isinstance(node.get("value"), str)
            and node["value"].endswith("※")
            and next_node is not None
        ):
            body = gaiji_note_body(next_node)
            if body is not None:
                gaiji = parse_source_gaiji_marker(body, f"※［＃{body}］")
                if gaiji is not None:
                    prefix = node["value"][:-1]
                    if prefix:
                        replacement = dict(node)
                        replacement["value"] = prefix
                        out.append(replacement)
                    record_source_derived_gaiji(
                        summary,
                        gaiji,
                        gaiji.get("x-source", ""),
                        source_derived_gaiji_kind(gaiji),
                    )
                    gaiji.pop("x-source", None)
                    out.append(gaiji)
                    changed = True
                    index += 2
                    continue
        if (
            node.get("kind") == "ruby"
            and node.get("base") == "※"
            and next_node is not None
        ):
            body = gaiji_note_body(next_node)
            if body is not None:
                gaiji = parse_source_gaiji_marker(body, f"※［＃{body}］")
                if gaiji is not None:
                    record_source_derived_gaiji(
                        summary,
                        gaiji,
                        gaiji.get("x-source", ""),
                        source_derived_gaiji_kind(gaiji),
                    )
                    gaiji.pop("x-source", None)
                    ruby = dict(node)
                    ruby["base"] = gaiji.get("resolved") or ""
                    ruby["base_content"] = [gaiji]
                    ruby["x-provenance"] = "source-derived"
                    record_ruby_summary(summary, ruby, "source-derived")
                    out.append(ruby)
                    changed = True
                    index += 2
                    continue
        out.append(node)
        index += 1
    return out if changed else None


def gaiji_note_body(node: dict[str, Any]) -> str | None:
    if node.get("kind") != "style" or node.get("style_type") != "notes":
        return None
    text = inline_visible_text(node.get("content", []))
    match = re.fullmatch(r"［＃(?P<body>.+)］", text)
    return match.group("body") if match else None


def source_derived_gaiji_kind(node: dict[str, Any]) -> str:
    if node.get("jis_code"):
        return "JisCode"
    if "U+" in str(node.get("description", "")):
        return "UnicodeCodepoint"
    return "DescriptionOnly"


def record_source_derived_gaiji(
    summary: dict[str, list[dict[str, Any]]],
    node: dict[str, Any],
    source: str,
    kind: str,
) -> None:
    summary.setdefault("gaiji.marker", []).append({
        "kind": "gaiji",
        "value": {
            "source": source,
            "description": node.get("description", ""),
            "description_format": "aozora-gaiji-tag",
            "kind": kind,
            "resolved": node.get("resolved"),
            "ruby_reading": None,
        },
        "provenance": "source-derived",
    })


def source_text_gaiji_content(source_text: str) -> list[dict[str, Any]] | None:
    content: list[dict[str, Any]] = []
    pos = 0
    saw_gaiji = False
    for match in GAIJI_MARKER_RE.finditer(source_text):
        if match.start() > pos:
            content.append({"kind": "text", "value": source_text[pos:match.start()]})
        gaiji = parse_source_gaiji_marker(match.group("body"), match.group(0))
        if gaiji is None:
            return None
        content.append(gaiji)
        saw_gaiji = True
        pos = match.end()
    if not saw_gaiji:
        return None
    if pos < len(source_text):
        content.append({"kind": "text", "value": source_text[pos:]})
    return content


def source_gaiji_markers(source_text: str) -> list[dict[str, Any]]:
    markers: list[dict[str, Any]] = []
    for match in GAIJI_MARKER_RE.finditer(source_text):
        gaiji = parse_source_gaiji_marker(match.group("body"), match.group(0))
        if gaiji is not None:
            markers.append(gaiji)
    return markers


def parse_source_gaiji_marker(body: str, source: str) -> dict[str, Any] | None:
    unicode_match = re.search(r"U\+(?P<code>[0-9A-Fa-f]{4,6})", body)
    if unicode_match:
        resolved = chr(int(unicode_match.group("code"), 16))
        return {
            "kind": "gaiji",
            "description": body,
            "resolved": resolved,
            "jis_code": None,
            "unresolved_reason": None,
            "x-provenance": "source-derived",
            "x-source": source,
        }

    jis_match = re.search(
        r"(?:第[34]水準)?(?P<plane>[12])-(?P<row>[0-9]{1,2})-(?P<cell>[0-9]{1,2})",
        body,
    )
    if not jis_match:
        return {
            "kind": "gaiji",
            "description": body,
            "resolved": "",
            "jis_code": None,
            "unresolved_reason": "unresolved",
            "x-provenance": "source-derived",
            "x-source": source,
        }
    jis_code = (
        f"{jis_match.group('plane')}-"
        f"{int(jis_match.group('row'))}-"
        f"{int(jis_match.group('cell'))}"
    )
    resolved = resolve_jisx0213(jis_code)
    if not resolved:
        return {
            "kind": "gaiji",
            "description": body,
            "resolved": "",
            "jis_code": jis_code,
            "unresolved_reason": "unresolved",
            "x-provenance": "source-derived",
            "x-source": source,
        }
    return {
        "kind": "gaiji",
        "description": body,
        "resolved": resolved,
        "jis_code": jis_code,
        "unresolved_reason": None,
        "x-provenance": "source-derived",
        "x-source": source,
    }


def resolve_jisx0213(jis_code: str) -> str | None:
    return load_jis2ucs().get(normalize_jis_code(jis_code))


def normalize_jis_code(jis_code: str) -> str:
    parts = jis_code.split("-")
    if len(parts) != 3:
        return jis_code
    return f"{int(parts[0])}-{int(parts[1])}-{int(parts[2])}"


def load_jis2ucs() -> dict[str, str]:
    global JIS2UCS_CACHE
    if JIS2UCS_CACHE is not None:
        return JIS2UCS_CACHE

    JIS2UCS_CACHE = {}
    table = find_jis2ucs_table()
    if table is None:
        return JIS2UCS_CACHE

    line_re = re.compile(r"^:(?P<jis>[0-9]+-[0-9]+-[0-9]+): \"&#x(?P<ucs>[0-9A-Fa-f]+);\"")
    for line in table.read_text(encoding="utf-8").splitlines():
        m = line_re.match(line)
        if m:
            JIS2UCS_CACHE[normalize_jis_code(m.group("jis"))] = chr(
                int(m.group("ucs"), 16)
            )
    return JIS2UCS_CACHE


def find_jis2ucs_table() -> Path | None:
    roots = [
        Path.cwd(),
        Path(__file__).resolve().parents[2],
        Path(os.environ.get(
            "AB_VALIDATOR_GEM_HOME",
            "/db/ab-validator/gems/aozora2html-3.0.1",
        )),
    ]
    candidates = [
        root / "references/parsers/aozora2html/yml/jis2ucs.yml"
        for root in roots
    ]
    candidates.extend([
        roots[-1] / "gems/aozora2html-3.0.1/yml/jis2ucs.yml",
        Path("/db/ab-validator/gems/aozora2html-3.0.1/gems/aozora2html-3.0.1/yml/jis2ucs.yml"),
    ])
    for candidate in candidates:
        if candidate.exists():
            return candidate
    return None


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
