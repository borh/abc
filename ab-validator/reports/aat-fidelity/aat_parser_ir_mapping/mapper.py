#!/usr/bin/env python
"""DISPOSABLE PROBE — AAT v1 -> ABC parser-IR mapper.

Throwaway. Settles ONE question: are AAT/parser-IR divergences survivable in a
concrete mapping, or do they reveal genuinely distinct concerns?

Honest best-effort: preserve what survives; emit a DIVERGENCE LEDGER entry for
every node/field the target cannot represent without loss/ambiguity/invention.
Categories: LOSS, AMBIGUITY, INVENTION, UNSUPPORTED, STRUCTURAL.

Structural transform rule (flat-nodes vs nested-blocks):
  - Flatten `blocks[].content[]` (and `blocks[].children[]` for block_containers)
    into a single parser-IR `nodes[]` in document order.
  - Paragraph block boundaries are emitted by parser-IR v1.1 as top-level
    `paragraphs[]` ranges. Other block containers are NOT emitted as parser-IR
    nodes; their *inline* children are emitted and the block boundary itself is
    recorded as a STRUCTURAL entry.
  - A running `offset` advances by emitted parser-IR node span end. When AAT
    spans are absent, the fallback end is the projected visible text's UTF-8
    byte length. This keeps synthesized spans monotonic and records the
    approximation as an AMBIGUITY entry.

Run: python map.py aat-sample.json
"""

import json
import re
import sys

# Hardcoded ABC parser-IR target identity (INVENTION: producer must pinch
# ABC's schema_hash; AAT carries no such identifier).
PARSER_IR_SCHEMA_ID = "https://w3id.org/abc/schemas/parser-ir.schema.json"
PARSER_IR_SCHEMA_HASH = "sha256:43a6a6d86ca5eca062508e6cae633d19bf5248f15c5bb46153a6d8580ea916ec"

ENC_MAP = {
    "utf-8": "UTF-8",
    "utf-8-bom": "UTF-8",
    "windows-31j": "Shift_JIS",
    "windows-31j-lossy": "Shift_JIS",
}

ENC_DIVERGENCE = {
    "utf-8-bom": "source encoding has a BOM marker; parser-IR records only UTF-8",
    "windows-31j-lossy": "source decoding was lossy; parser-IR records only the Shift_JIS source family",
}


def ledger(category, aat, target, note):
    return {"category": category, "aat": aat, "parser_ir": target, "note": note}


def aat_pointer_bucket(pointer):
    """Fold occurrence-specific paths into rule-ish AAT pointers for aggregation."""
    bucket = re.sub(r"\[[0-9]+\]", "[]", pointer)
    return re.sub(r"^([A-Za-z0-9_.]+)=.*$", r"\1", bucket)


def utf8_len(value):
    return len((value or "").encode("utf-8"))


def span_end(span, fallback_end):
    if span is None:
        return fallback_end
    return span.get("byte_end", fallback_end)


def map_span(aat_span, fallback_start, fallback_end, ledger_list, path):
    """AAT decoded-utf8 byte offsets -> parser-IR span.

    Production AAT spans are mostly absent. Parser-IR requires spans, so missing
    spans are an explicit AMBIGUITY bucket rather than an INVENTION sidecar. The
    fallback end advances by projected UTF-8 byte length.
    """
    if aat_span is None:
        ledger_list.append(
            ledger(
                "AMBIGUITY",
                f"{path}.span",
                "span",
                "AAT node has no serialized span; parser-IR requires decoded_utf8 span, so a projected UTF-8 fallback span was synthesized",
            )
        )
        return {
            "start": fallback_start,
            "end": fallback_end,
            "line": None,
            "column": None,
            "coordinate_system": "decoded_utf8",
        }
    if aat_span.get("line_end") not in (None, aat_span.get("line_start")):
        ledger_list.append(
            ledger(
                "LOSS",
                f"{path}.span.line_end",
                "span.line",
                "AAT span has line_end but parser-IR span carries only one line field",
            )
        )
    return {
        "start": aat_span.get("byte_start", fallback_start),
        "end": aat_span.get("byte_end", fallback_end),
        "line": aat_span.get("line_start"),
        "column": None,
        "coordinate_system": "decoded_utf8",
    }


def text_projection(node, ledger_list, path):
    """Best-effort display text projection for parser-IR nodes that only hold text."""
    kind = node.get("kind")
    if kind == "text":
        return node.get("value", "")
    if kind == "ruby":
        ledger_list.append(
            ledger(
                "LOSS",
                f"{path}.ruby.reading",
                "(emphasis.text)",
                "style text projection kept ruby base text only; reading not represented inside emphasis.text",
            )
        )
        return node.get("base", "")
    if kind == "gaiji":
        ledger_list.append(
            ledger(
                "AMBIGUITY",
                f"{path}.gaiji.resolved",
                "(emphasis.text)",
                "style text projection used resolved gaiji string when available",
            )
        )
        return node.get("resolved") or ""
    if kind in ("style", "font_size", "tcy", "keigakomi", "yokogumi", "caption"):
        return "".join(
            text_projection(child, ledger_list, f"{path}.content[{i}]")
            for i, child in enumerate(node.get("content", []))
        )
    if kind == "warigaki":
        ledger_list.append(
            ledger(
                "UNSUPPORTED",
                f"{path}.warigaki",
                "(emphasis.text)",
                "parser-IR has no warigaki node; style text projection flattened upper/lower visible text",
            )
        )
        return "".join(
            text_projection(child, ledger_list, f"{path}.{group}[{i}]")
            for group in ("upper", "lower")
            for i, child in enumerate(node.get(group, []))
        )
    ledger_list.append(
        ledger(
            "LOSS",
            f"{path}.{kind}",
            "(emphasis.text)",
            f"style text projection dropped inline kind '{kind}'",
        )
    )
    return ""


def plain_text_projection(node):
    """Visible text projection for compatibility text fields when structure survives elsewhere."""
    kind = node.get("kind")
    if kind == "text":
        return node.get("value", "")
    if kind == "ruby":
        return node.get("base", "")
    if kind == "gaiji":
        return node.get("resolved") or node.get("description", "")
    if kind == "accent":
        return node.get("resolved") or node.get("name", "")
    if kind in ("style", "font_size", "tcy", "keigakomi", "yokogumi", "caption"):
        return "".join(plain_text_projection(child) for child in node.get("content", []))
    if kind == "warigaki":
        return "".join(
            plain_text_projection(child)
            for group in ("upper", "lower")
            for child in node.get(group, [])
        )
    if kind == "figure":
        return node.get("alt", "")
    return ""


def layout_scope(node):
    kind = node.get("kind")
    if kind == "font_size":
        return {
            "kind": "font-size",
            "source": "aat-inline",
            "size_type": node.get("size_type", "unknown"),
            "level": node.get("level", 0),
        }
    if kind == "tcy":
        return {"kind": "tcy", "source": "aat-inline", "marker": node.get("marker")}
    if kind == "keigakomi":
        return {
            "kind": "keigakomi",
            "source": "aat-inline",
            "border": node.get("border"),
            "marker": node.get("marker"),
        }
    if kind == "yokogumi":
        return {
            "kind": "yokogumi",
            "source": "aat-inline",
            "direction": "horizontal",
            "marker": node.get("marker"),
        }
    return None


def map_inline(node, offset, ledger_list, path):
    kind = node.get("kind")
    span = node.get("span")

    if kind == "text":
        value = node.get("value", "")
        fallback_end = offset + utf8_len(value)
        pir_span = map_span(span, offset, fallback_end, ledger_list, path)
        return {
            "type": "text",
            "span": pir_span,
            "text": value,
        }, span_end(span, fallback_end)

    if kind == "ruby":
        base = node.get("base", "")
        fallback_end = offset + utf8_len(base)
        pir_span = map_span(span, offset, fallback_end, ledger_list, path)
        scope = "explicit"  # INVENTION: AAT has no scope; default to explicit.
        ledger_list.append(
            ledger(
                "INVENTION",
                "(none)",
                "ruby.scope",
                "AAT has no scope field; defaulted to 'explicit'",
            )
        )
        if node.get("reading_content"):
            ledger_list.append(
                ledger(
                    "LOSS",
                    f"{path}.ruby.reading_content",
                    "(none)",
                    "nested ruby reading_content substructure flattened away",
                )
            )
        result = {
            "type": "ruby",
            "span": pir_span,
            "ruby": {
                "base": base,
                "reading": node.get("reading", ""),
                "scope": scope,
                "direction": node.get("direction"),
            },
        }
        if "base_content" in node:
            children = []
            child_offset = offset
            for i, child in enumerate(node["base_content"]):
                mapped, child_offset = map_inline(
                    child, child_offset, ledger_list, f"{path}.ruby.base_content[{i}]"
                )
                if mapped is not None:
                    children.append(mapped)
            result["inline_children"] = children
        return result, span_end(span, fallback_end)

    if kind == "gaiji":
        resolved_str = node.get("resolved")
        visible = resolved_str if resolved_str is not None else node.get("description", "")
        fallback_end = offset + utf8_len(visible)
        pir_span = map_span(span, offset, fallback_end, ledger_list, path)
        # AAT resolved = string|null; parser-IR resolved = boolean
        resolved_bool = resolved_str is not None
        # raw_marker INVENTION: AAT gives description, not the source marker.
        ledger_list.append(
            ledger(
                "INVENTION",
                f"{path}.gaiji.description",
                "gaiji.raw_marker",
                "AAT has no raw source marker; used description as raw_marker",
            )
        )
        # resolved semantic mismatch
        ledger_list.append(
            ledger(
                "AMBIGUITY",
                f"{path}.gaiji.resolved",
                "gaiji.resolved",
                "AAT resolved is string (the chosen char); parser-IR resolved is boolean (was it resolved?)",
            )
        )
        if node.get("unresolved_reason"):
            ledger_list.append(
                ledger(
                    "LOSS",
                    f"{path}.gaiji.unresolved_reason",
                    "(none)",
                    f"unresolved_reason='{node['unresolved_reason']}' has no parser-IR field",
                )
            )
        if resolved_str is None:
            ledger_list.append(
                ledger(
                    "LOSS",
                    "(none)",
                    "gaiji.unicode",
                    "Unresolved AAT gaiji has no Unicode text to project",
                )
            )
        # jis_code -> reference (semantic stretch)
        if node.get("jis_code"):
            ledger_list.append(
                ledger(
                    "AMBIGUITY",
                    f"{path}.gaiji.jis_code",
                    "gaiji.reference",
                    "jis_code mapped to reference; different identifier spaces",
                )
            )
        return {
            "type": "gaiji",
            "span": pir_span,
            "gaiji": {
                "raw_marker": node.get("description", ""),
                "reference": node.get("jis_code"),
                "unicode": resolved_str,
                "ivs": None,
                "image_or_glyph_fallback": None,
                "resolved": resolved_bool,
            },
        }, span_end(span, fallback_end)

    if kind == "accent":
        # parser-IR has no accent node; closest is emphasis(text, style).
        text = node.get("resolved") or node.get("name", "")
        fallback_end = offset + utf8_len(text)
        pir_span = map_span(span, offset, fallback_end, ledger_list, path)
        ledger_list.append(
            ledger(
                "AMBIGUITY",
                f"{path}.accent",
                "emphasis",
                "accent mapped to emphasis; accent code/name semantics not preserved",
            )
        )
        ledger_list.append(
            ledger(
                "INVENTION",
                f"{path}.accent.code",
                "emphasis.style",
                f"used accent.code='{node.get('code')}' as free-form style string",
            )
        )
        if node.get("name"):
            ledger_list.append(
                ledger(
                    "LOSS",
                    f"{path}.accent.name",
                    "(none)",
                    f"accent.name='{node['name']}' has no parser-IR field",
                )
            )
        return {
            "type": "emphasis",
            "span": pir_span,
            "text": text,
            "style": node.get("code", ""),
        }, span_end(span, fallback_end)

    if kind == "figure":
        # parser-IR image: src, alt, path_hint. filename->src is a name, not a resolved path.
        src = node.get("filename", "")
        fallback_end = offset + utf8_len(src)
        pir_span = map_span(span, offset, fallback_end, ledger_list, path)
        ledger_list.append(
            ledger(
                "INVENTION",
                f"{path}.figure.filename",
                "image.src",
                "filename is not a resolved source path; used as src",
            )
        )
        for f in ("css_class", "width", "height"):
            if node.get(f) is not None:
                ledger_list.append(
                    ledger(
                        "LOSS",
                        f"{path}.figure.{f}",
                        "(none)",
                        f"{f}={node[f]} dropped; parser-IR image has no such field",
                    )
                )
        if node.get("caption"):
            ledger_list.append(
                ledger(
                    "LOSS",
                    f"{path}.figure.caption",
                    "(none)",
                    "nested figure caption[] dropped (could be emitted as caption node, scope target ambiguous)",
                )
            )
        return {
            "type": "image",
            "span": pir_span,
            "src": src,
            "alt": node.get("alt"),
            "path_hint": None,
        }, span_end(span, fallback_end)

    if kind == "warigaki":
        # No parser-IR warigaki. Best-effort: flatten upper then lower as text nodes; structure lost.
        ledger_list.append(
            ledger(
                "UNSUPPORTED",
                f"{path}.warigaki",
                "(none)",
                "parser-IR has no warigaki node; upper/lower flattened to text nodes, split-line structure lost",
            )
        )
        return None, span_end(span, offset)  # caller emits child text nodes inline

    if kind == "raw":
        ledger_list.append(
            ledger(
                "UNSUPPORTED",
                f"{path}.raw",
                "(none)",
                "parser-IR has no raw node; faithful escape hatch dropped",
            )
        )
        return None, span_end(span, offset + utf8_len(node.get("source", "")))

    if kind == "style":
        projected_text = text_projection(node, ledger_list, path)
        fallback_end = offset + utf8_len(projected_text)
        pir_span = map_span(span, offset, fallback_end, ledger_list, path)
        ledger_list.append(
            ledger(
                "AMBIGUITY",
                f"{path}.style",
                "emphasis",
                "style inline_container mapped to emphasis; parser-IR does not preserve nested inline container identity",
            )
        )
        return {
            "type": "emphasis",
            "span": pir_span,
            "text": projected_text,
            "style": node.get("style_type", ""),
        }, span_end(span, fallback_end)

    if kind in ("font_size", "tcy", "keigakomi", "yokogumi"):
        projected_text = plain_text_projection(node)
        fallback_end = offset + utf8_len(projected_text)
        pir_span = map_span(span, offset, fallback_end, ledger_list, path)
        return {
            "type": "layout-span",
            "span": pir_span,
            "text": projected_text,
            "layout": layout_scope(node),
        }, span_end(span, fallback_end)

    if kind == "caption":
        ledger_list.append(
            ledger(
                "UNSUPPORTED",
                f"{path}.{kind}",
                "emphasis(?)",
                f"inline_container kind '{kind}' has no first-class parser-IR node; only emphasis.text/style exist",
            )
        )
        return None, span_end(span, offset + utf8_len(text_projection(node, ledger_list, path)))

    ledger_list.append(
        ledger(
            "UNSUPPORTED",
            f"{path}.{kind}",
            "(none)",
            f"unknown inline kind '{kind}' has no parser-IR equivalent",
        )
    )
    return None, span_end(span, offset)


def is_paragraph_layout_style(node):
    if node.get("kind") != "style":
        return False
    style_type = node.get("style_type")
    if style_type == "burasage":
        return node.get("x-indent-first") is not None and node.get("x-indent-rest") is not None
    if style_type == "chitsuki":
        return node.get("x-offset") is not None and node.get("x-align", "right") == "right"
    if style_type in ("jisage", "line-jisage", "jisage_line"):
        return node.get("x-indent") is not None
    if style_type == "jizume":
        return node.get("x-width") is not None
    return False


def paragraph_only_jisage_block(block):
    if block.get("kind") != "jisage_block":
        return False
    children = block.get("children", [])
    return bool(children) and all(child.get("kind") == "paragraph" for child in children)


def map_block(block, nodes, ledger_list, offset, path):
    kind = block.get("kind")
    if kind != "paragraph" and not paragraph_only_jisage_block(block):
        # Non-paragraph block boundaries are still not first-class parser-IR
        # structures. Paragraphs are represented by top-level paragraphs[].
        ledger_list.append(
            ledger(
                "STRUCTURAL",
                f"{path}.{kind}",
                "(none)",
                f"block container of kind '{kind}' has no parser-IR node; boundary + span + style lost, only inlines emitted",
            )
        )

    if kind == "heading":
        # parser-IR heading keeps a visible text projection and structured inline_children.
        parts = []
        inline_children = []
        child_offset = offset
        for i, child in enumerate(block.get("content", [])):
            if child.get("kind") == "warigaki":
                parts.append(text_projection(child, ledger_list, f"{path}.heading.content[{i}]"))
            else:
                parts.append(plain_text_projection(child))
            mapped, child_offset = map_inline(
                child, child_offset, ledger_list, f"{path}.heading.content[{i}]"
            )
            if mapped is not None:
                inline_children.append(mapped)
        level = block.get("level", 1)
        # AAT level max 3, parser-IR max 6 -> fits, but range divergence recorded.
        ledger_list.append(
            ledger(
                "AMBIGUITY",
                f"{path}.heading.level",
                "heading.level",
                "AAT heading.level range 1-3 vs parser-IR 1-6; values fit but domain differs",
            )
        )
        if block.get("style"):
            ledger_list.append(
                ledger(
                    "LOSS",
                    f"{path}.heading.style",
                    "(none)",
                    f"heading.style='{block['style']}' dropped",
                )
            )
        heading_text = "".join(parts)
        fallback_end = offset + utf8_len(heading_text)
        nodes.append(
            {
                "type": "heading",
                "span": map_span(block.get("span"), offset, fallback_end, ledger_list, path),
                "text": heading_text,
                "inline_children": inline_children,
                "level": level,
            }
        )
        if "indent" in block or "x-indent" in block:
            nodes[-1]["indent"] = block.get("indent", block.get("x-indent"))
        offset = span_end(block.get("span"), fallback_end)

    elif kind == "paragraph":
        content = block.get("content", [])
        path_prefix = f"{path}.content"
        if (
            len(content) == 1
            and isinstance(content[0], dict)
            and is_paragraph_layout_style(content[0])
        ):
            content = content[0].get("content", [])
            path_prefix = f"{path}.content[0].content"
        for i, child in enumerate(content):
            cpath = f"{path_prefix}[{i}]"
            if child.get("kind") == "warigaki":
                ledger_list.append(
                    ledger(
                        "UNSUPPORTED",
                        f"{cpath}.warigaki",
                        "(none)",
                        "parser-IR has no warigaki node; upper/lower flattened to text nodes, split-line structure lost",
                    )
                )
                # flatten upper/lower
                for grp in ("upper", "lower"):
                    for j, sub in enumerate(child.get(grp, [])):
                        n, offset = map_inline(
                            sub,
                            offset,
                            ledger_list,
                            f"{cpath}.warigaki.{grp}[{j}]",
                        )
                        if n is not None:
                            nodes.append(n)
            else:
                n, offset = map_inline(child, offset, ledger_list, cpath)
                if n is not None:
                    nodes.append(n)

    elif kind in (
        "jisage_block",
        "quote_block",
        "keigakomi_block",
        "yokogumi_block",
        "caption_block",
    ):
        # Emit a best-effort parser-IR node for the block kind, then recurse children.
        if kind == "jisage_block" and paragraph_only_jisage_block(block):
            pass
        elif kind == "jisage_block":
            ledger_list.append(
                ledger(
                    "INVENTION",
                    f"{path}.jisage_block",
                    "indentation",
                    "mapped to indentation node; depth unknown -> defaulted 1",
                )
            )
            fallback_end = offset
            nodes.append(
                {
                    "type": "indentation",
                    "span": map_span(block.get("span"), offset, fallback_end, ledger_list, path),
                    "depth": 1,
                    "text": None,
                }
            )
            offset = span_end(block.get("span"), fallback_end)
        elif kind == "quote_block":
            ledger_list.append(
                ledger(
                    "AMBIGUITY",
                    f"{path}.quote_block",
                    "quote",
                    "quote_block container vs quote open/close/inline marker node; nesting semantics differ",
                )
            )
            fallback_end = offset
            nodes.append(
                {
                    "type": "quote",
                    "span": map_span(block.get("span"), offset, fallback_end, ledger_list, path),
                    "marker_type": "unknown",
                    "nesting_level": None,
                    "text": None,
                }
            )
            offset = span_end(block.get("span"), fallback_end)
        elif kind == "caption_block":
            ledger_list.append(
                ledger(
                    "AMBIGUITY",
                    f"{path}.caption_block",
                    "caption",
                    "caption_block vs caption node (text+target); target linkage lost",
                )
            )
        else:
            ledger_list.append(
                ledger(
                    "UNSUPPORTED",
                    f"{path}.{kind}",
                    "(none)",
                    f"block_container kind '{kind}' has no parser-IR equivalent",
                )
            )
        for i, child in enumerate(block.get("children", [])):
            offset = map_block(child, nodes, ledger_list, offset, f"{path}.children[{i}]")

    else:
        ledger_list.append(
            ledger("UNSUPPORTED", f"{path}.{kind}", "(none)", f"unknown block kind '{kind}'")
        )
    return offset


def map_meta_source(aat, ledger_list, work_content_hash=None):
    """AAT meta -> parser-IR source + top-level identity."""
    meta = aat.get("meta", {})
    enc_in = meta.get("source_encoding", "utf-8")
    enc_out = ENC_MAP.get(enc_in)
    if enc_out is None:
        ledger_list.append(
            ledger(
                "UNSUPPORTED",
                f"meta.source_encoding={enc_in}",
                "source.encoding",
                f"encoding '{enc_in}' has no parser-IR enum value",
            )
        )
        enc_out = "unknown"
    elif enc_in in ENC_DIVERGENCE:
        ledger_list.append(
            ledger(
                "AMBIGUITY",
                f"meta.source_encoding={enc_in}",
                "source.encoding",
                ENC_DIVERGENCE[enc_in],
            )
        )
    source_hash = meta.get("source_hash", "sha256:" + "0" * 64)
    primary_text_hash = meta.get("primary_text_hash", source_hash)
    if "primary_text_hash" in meta and primary_text_hash != source_hash:
        raise ValueError("AAT meta.primary_text_hash must equal historical meta.source_hash alias")
    primary_pointer = (
        "meta.primary_text_hash" if "primary_text_hash" in meta else "meta.source_hash"
    )
    ledger_list.append(
        ledger(
            "AMBIGUITY",
            primary_pointer,
            "source.primary_text_hash",
            "AAT parser-input identity projects to parser-IR primary_text_hash; source_hash is the historical fallback",
        )
    )
    # normalization: parser-IR requires it; AAT has none
    ledger_list.append(
        ledger(
            "INVENTION",
            "(none)",
            "source.normalization",
            "parser-IR requires normalization enum; AAT has none -> defaulted 'source'",
        )
    )
    # source_path: AAT has none
    ledger_list.append(
        ledger(
            "INVENTION",
            "(none)",
            "source.source_path",
            "parser-IR source_path optional; AAT has none -> null",
        )
    )
    src = {
        "work_content_hash": work_content_hash or primary_text_hash,
        "primary_text_hash": primary_text_hash,
        "source_path": None,
        "encoding": enc_out,
        "normalization": "source",
    }
    # Producer identity now projects to parser-IR derived_from. parse_complete
    # remains adapter-fidelity metadata preserved in the divergence bundle.
    ledger_list.append(
        ledger(
            "LOSS",
            "meta.parse_complete",
            "(none)",
            f"parse_complete={meta.get('parse_complete')!r} preserved in divergence bundle; parser-IR derived_from does not model parse completeness",
        )
    )
    # metrics: heavy fidelity block dropped entirely
    if meta.get("metrics"):
        ledger_list.append(
            ledger(
                "LOSS",
                "meta.metrics",
                "(none)",
                "performance/fallback metrics dropped; adapter-fidelity concern not representable",
            )
        )
    if meta.get("semantic_summary"):
        ledger_list.append(
            ledger(
                "LOSS",
                "meta.semantic_summary",
                "(none)",
                "semantic_summary provenance dropped; adapter-fidelity concern not representable",
            )
        )
    return src


def map_warnings(aat, ledger_list):
    out = []
    for w in aat.get("meta", {}).get("warnings", []):
        # parser-IR diagnostic requires severity + code. AAT warning has only message(+line/path).
        ledger_list.append(
            ledger(
                "INVENTION",
                "(none)",
                "warnings[].severity",
                "AAT warning has no severity -> defaulted 'warning'",
            )
        )
        ledger_list.append(
            ledger(
                "INVENTION",
                "(none)",
                "warnings[].code",
                "AAT warning has no code -> defaulted 'AAT_WARNING'",
            )
        )
        if w.get("line"):
            ledger_list.append(
                ledger(
                    "AMBIGUITY",
                    "meta.warnings[].line",
                    "warnings[].span.line",
                    "AAT warning.line vs parser-IR diagnostic.span; coordinate not assembled",
                )
            )
        out.append(
            {
                "severity": "warning",
                "code": "AAT_WARNING",
                "message": w.get("message", ""),
                "span": None,
                "construct": None,
                "recovery": None,
            }
        )
    return out


def main():
    aat_path = sys.argv[1] if len(sys.argv) > 1 else "aat-sample.json"
    with open(aat_path, encoding="utf-8") as f:
        aat = json.load(f)

    ledger_list = []
    nodes = []
    offset = 0
    for i, block in enumerate(aat.get("blocks", [])):
        offset = map_block(block, nodes, ledger_list, offset, f"blocks[{i}]")

    source = map_meta_source(aat, ledger_list)
    # Top-level identity: schema_id/schema_hash INVENTION (AAT has only version=1)
    ledger_list.append(
        ledger(
            "INVENTION",
            "(top-level)",
            "schema_id/schema_hash",
            "parser-IR requires schema_id+schema_hash; AAT supplies only version=1; producer must hardcode ABC's identifier",
        )
    )
    warnings = map_warnings(aat, ledger_list)
    # errors: AAT has none
    ledger_list.append(
        ledger(
            "INVENTION",
            "(none)",
            "errors[]",
            "parser-IR requires errors[]; AAT has no errors concept -> defaulted empty",
        )
    )

    pir = {
        "schema_id": PARSER_IR_SCHEMA_ID,
        "schema_hash": PARSER_IR_SCHEMA_HASH,
        "source": source,
        "nodes": nodes,
        "warnings": warnings,
        "errors": [],
    }

    # Report
    print("=== GENERATED parser-ir.json (probe) ===")
    print(json.dumps(pir, ensure_ascii=False, indent=2))
    print()
    print("=== DIVERGENCE LEDGER ===")
    from collections import Counter

    cats = Counter(e["category"] for e in ledger_list)
    print(f"Total entries: {len(ledger_list)}")
    for c in ("LOSS", "AMBIGUITY", "INVENTION", "UNSUPPORTED", "STRUCTURAL"):
        print(f"  {c}: {cats.get(c, 0)}")
    print()
    print("AAT field/node | parser-IR target | category | note")
    print("--- | --- | --- | ---")
    for e in ledger_list:
        print(f"{e['aat']} | {e['parser_ir']} | {e['category']} | {e['note']}")

    with open("parser-ir.probe.json", "w", encoding="utf-8") as f:
        json.dump(pir, f, ensure_ascii=False, indent=2)
    with open("ledger.md", "w", encoding="utf-8") as f:
        f.write("# Divergence Ledger (probe output)\n\n")
        f.write(f"Total entries: {len(ledger_list)}\n\n")
        for c in ("LOSS", "AMBIGUITY", "INVENTION", "UNSUPPORTED", "STRUCTURAL"):
            f.write(f"- {c}: {cats.get(c, 0)}\n")
        f.write("\n| AAT field/node | parser-IR target | category | note |\n")
        f.write("| --- | --- | --- | --- |\n")
        for e in ledger_list:
            f.write(f"| {e['aat']} | {e['parser_ir']} | {e['category']} | {e['note']} |\n")


if __name__ == "__main__":
    main()
