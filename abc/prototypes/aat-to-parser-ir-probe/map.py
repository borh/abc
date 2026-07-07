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
  - Block containers (paragraph/heading/block_container) are NOT emitted as
    parser-IR nodes; parser-IR has no paragraph/block concept. Their *inline*
    children are emitted; the block boundary itself is recorded as a LOSS entry.
  - A running `offset` advances by the AAT byte_end - byte_start of each
    emitted inline to give parser-IR `span.start/end` (char-ish offsets), since
    parser-IR span semantics are unspecified in schema while AAT spans are
    decoded-UTF8 byte offsets. line is taken from AAT line_start; column is
    unknown (null). This is itself an AMBIGUITY entry (see STRUCTURAL/SPAN).

Run: python map.py aat-sample.json
"""
import json, re, sys

# Hardcoded ABC parser-IR target identity (INVENTION: producer must pinch
# ABC's schema_hash; AAT carries no such identifier).
PARSER_IR_SCHEMA_ID = "https://w3id.org/abc/schemas/parser-ir.schema.json"
PARSER_IR_SCHEMA_HASH = "sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396"

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


def map_span(aat_span, offset):
    """AAT decoded-utf8 byte offsets -> parser-IR start/end (proxy char offsets)
    + line. column unknown."""
    if aat_span is None:
        return {"start": offset, "end": offset, "line": None, "column": None}
    return {
        "start": aat_span.get("byte_start", offset),
        "end": aat_span.get("byte_end", offset),
        "line": aat_span.get("line_start"),
        "column": None,
    }


def text_projection(node, ledger_list, path):
    """Best-effort display text projection for parser-IR nodes that only hold text."""
    kind = node.get("kind")
    if kind == "text":
        return node.get("value", "")
    if kind == "ruby":
        ledger_list.append(ledger("LOSS", f"{path}.ruby.reading",
                                  "(emphasis.text)", "style text projection kept ruby base text only; reading not represented inside emphasis.text"))
        return node.get("base", "")
    if kind == "gaiji":
        ledger_list.append(ledger("AMBIGUITY", f"{path}.gaiji.resolved",
                                  "(emphasis.text)", "style text projection used resolved gaiji string when available"))
        return node.get("resolved") or ""
    if kind in ("style", "font_size", "tcy", "keigakomi", "yokogumi", "caption"):
        return "".join(text_projection(child, ledger_list, f"{path}.content[{i}]")
                       for i, child in enumerate(node.get("content", [])))
    ledger_list.append(ledger("LOSS", f"{path}.{kind}",
                              "(emphasis.text)", f"style text projection dropped inline kind '{kind}'"))
    return ""


def map_inline(node, offset, ledger_list, path):
    kind = node.get("kind")
    span = node.get("span")
    pir_span = map_span(span, offset)

    if kind == "text":
        return {
            "type": "text", "span": pir_span, "text": node.get("value", ""),
        }

    if kind == "ruby":
        # base_content/reading_content nesting is still outside parser-IR's ruby shape.
        scope = "explicit"  # INVENTION: AAT has no scope; default to explicit.
        ledger_list.append(ledger("INVENTION", f"{path}.ruby.scope", "ruby.scope",
                                  "AAT has no scope field; defaulted to 'explicit'"))
        if node.get("base_content") or node.get("reading_content"):
            ledger_list.append(ledger("LOSS", f"{path}.ruby.base_content/reading_content",
                                      "(none)", "nested ruby substructure flattened away"))
        return {
            "type": "ruby", "span": pir_span,
            "ruby": {"base": node.get("base", ""),
                     "reading": node.get("reading", ""),
                     "scope": scope,
                     "direction": node.get("direction")},
        }

    if kind == "gaiji":
        # AAT resolved = string|null; parser-IR resolved = boolean
        resolved_str = node.get("resolved")
        resolved_bool = bool(resolved_str) if resolved_str is not None else False
        mac = node.get("raw_marker")
        # raw_marker INVENTION: AAT gives description, not the source marker.
        ledger_list.append(ledger("INVENTION", f"{path}.gaiji.raw_marker",
                                  "gaiji.raw_marker", "AAT has no raw source marker; used description as raw_marker"))
        # resolved semantic mismatch
        ledger_list.append(ledger("AMBIGUITY", f"{path}.gaiji.resolved",
                                  "gaiji.resolved", "AAT resolved is string (the chosen char); parser-IR resolved is boolean (was it resolved?)"))
        if node.get("unresolved_reason"):
            ledger_list.append(ledger("LOSS", f"{path}.gaiji.unresolved_reason",
                                      "(none)", f"unresolved_reason='{node['unresolved_reason']}' has no parser-IR field"))
        # unicode: AAT may embed in resolved string; cannot extract
        ledger_list.append(ledger("LOSS", f"{path}.gaiji.unicode",
                                  "gaiji.unicode", "AAT does not separate unicode codepoint from resolved string"))
        # jis_code -> reference (semantic stretch)
        if node.get("jis_code"):
            ledger_list.append(ledger("AMBIGUITY", f"{path}.gaiji.jis_code",
                                      "gaiji.reference", "jis_code mapped to reference; different identifier spaces"))
        return {
            "type": "gaiji", "span": pir_span,
            "gaiji": {
                "raw_marker": node.get("description", ""),
                "reference": node.get("jis_code"),
                "unicode": None,
                "ivs": None,
                "image_or_glyph_fallback": None,
                "resolved": resolved_bool,
            },
        }

    if kind == "accent":
        # parser-IR has no accent node; closest is emphasis(text, style).
        text = node.get("resolved") or node.get("name", "")
        ledger_list.append(ledger("AMBIGUITY", f"{path}.accent",
                                  "emphasis", "accent mapped to emphasis; accent code/name semantics not preserved"))
        ledger_list.append(ledger("INVENTION", f"{path}.accent.code",
                                  "emphasis.style", f"used accent.code='{node.get('code')}' as free-form style string"))
        if node.get("name"):
            ledger_list.append(ledger("LOSS", f"{path}.accent.name",
                                      "(none)", f"accent.name='{node['name']}' has no parser-IR field"))
        return {
            "type": "emphasis", "span": pir_span,
            "text": text, "style": node.get("code", ""),
        }

    if kind == "figure":
        # parser-IR image: src, alt, path_hint. filename->src is a name, not a resolved path.
        ledger_list.append(ledger("INVENTION", f"{path}.figure.filename",
                                  "image.src", "filename is not a resolved source path; used as src"))
        for f in ("css_class", "width", "height"):
            if node.get(f) is not None:
                ledger_list.append(ledger("LOSS", f"{path}.figure.{f}",
                                          "(none)", f"{f}={node[f]} dropped; parser-IR image has no such field"))
        if node.get("caption"):
            ledger_list.append(ledger("LOSS", f"{path}.figure.caption",
                                      "(none)", "nested figure caption[] dropped (could be emitted as caption node, scope target ambiguous)"))
        return {
            "type": "image", "span": pir_span,
            "src": node.get("filename", ""), "alt": node.get("alt"), "path_hint": None,
        }

    if kind == "warigaki":
        # No parser-IR warigaki. Best-effort: flatten upper then lower as text nodes; structure lost.
        ledger_list.append(ledger("UNSUPPORTED", f"{path}.warigaki",
                                  "(none)", "parser-IR has no warigaki node; upper/lower flattened to text nodes, split-line structure lost"))
        return None  # caller emits child text nodes inline

    if kind == "raw":
        ledger_list.append(ledger("UNSUPPORTED", f"{path}.raw",
                                  "(none)", "parser-IR has no raw node; faithful escape hatch dropped"))
        return None

    if kind == "style":
        ledger_list.append(ledger("AMBIGUITY", f"{path}.style",
                                  "emphasis", "style inline_container mapped to emphasis; parser-IR does not preserve nested inline container identity"))
        return {
            "type": "emphasis", "span": pir_span,
            "text": text_projection(node, ledger_list, path),
            "style": node.get("style_type", ""),
        }

    # inline_container kinds: font_size, tcy, keigakomi, yokogumi, caption
    if kind in ("font_size", "tcy", "keigakomi", "yokogumi", "caption"):
        ledger_list.append(ledger("UNSUPPORTED", f"{path}.{kind}",
                                  "emphasis(?)", f"inline_container kind '{kind}' has no first-class parser-IR node; only emphasis.text/style exist"))
        return None

    ledger_list.append(ledger("UNSUPPORTED", f"{path}.{kind}", "(none)",
                              f"unknown inline kind '{kind}' has no parser-IR equivalent"))
    return None


def map_block(block, nodes, ledger_list, offset, path):
    kind = block.get("kind")
    # Every block boundary is a LOSS: parser-IR has no paragraph/block node.
    ledger_list.append(ledger("STRUCTURAL", f"{path}[block={kind}]",
                              "(none)", f"block container of kind '{kind}' has no parser-IR node; boundary + span + style lost, only inlines emitted"))

    if kind == "heading":
        # parser-IR heading: single text string + level. AAT heading has content[] inlines.
        # Concatenate inline text projections; nested ruby/gaiji inside heading lost.
        parts = []
        for i, child in enumerate(block.get("content", [])):
            if child.get("kind") == "text":
                parts.append(child.get("value", ""))
            else:
                ledger_list.append(ledger("LOSS", f"{path}.heading.content[{i}]={child.get('kind')}",
                                          "(none)", "non-text inline inside heading flattened to text projection; structure lost"))
        level = block.get("level", 1)
        # AAT level max 3, parser-IR max 6 -> fits, but range divergence recorded.
        ledger_list.append(ledger("AMBIGUITY", f"{path}.heading.level",
                                  "heading.level", "AAT heading.level range 1-3 vs parser-IR 1-6; values fit but domain differs"))
        if block.get("style"):
            ledger_list.append(ledger("LOSS", f"{path}.heading.style",
                                      "(none)", f"heading.style='{block['style']}' dropped"))
        nodes.append({
            "type": "heading", "span": map_span(block.get("span"), offset),
            "text": "".join(parts), "level": level,
        })

    elif kind == "paragraph":
        for i, child in enumerate(block.get("content", [])):
            cpath = f"{path}.content[{i}]"
            if child.get("kind") == "warigaki":
                ledger_list.append(ledger("UNSUPPORTED", f"{cpath}.warigaki",
                                          "(none)", "parser-IR has no warigaki node; upper/lower flattened to text nodes, split-line structure lost"))
                # flatten upper/lower
                for grp in ("upper", "lower"):
                    for j, sub in enumerate(child.get(grp, [])):
                        n = map_inline(sub, offset, ledger_list, f"{cpath}.warigaki.{grp}[{j}]")
                        if n is not None:
                            nodes.append(n)
            else:
                n = map_inline(child, offset, ledger_list, cpath)
                if n is not None:
                    nodes.append(n)

    elif kind in ("jisage_block", "quote_block", "keigakomi_block",
                  "yokogumi_block", "caption_block"):
        # Emit a best-effort parser-IR node for the block kind, then recurse children.
        if kind == "jisage_block":
            ledger_list.append(ledger("INVENTION", f"{path}.jisage_block",
                                      "indentation", "mapped to indentation node; depth unknown -> defaulted 1"))
            nodes.append({"type": "indentation", "span": map_span(block.get("span"), offset),
                          "depth": 1, "text": None})
        elif kind == "quote_block":
            ledger_list.append(ledger("AMBIGUITY", f"{path}.quote_block",
                                      "quote", "quote_block container vs quote open/close/inline marker node; nesting semantics differ"))
            nodes.append({"type": "quote", "span": map_span(block.get("span"), offset),
                          "marker_type": "unknown", "nesting_level": None, "text": None})
        elif kind == "caption_block":
            ledger_list.append(ledger("AMBIGUITY", f"{path}.caption_block",
                                      "caption", "caption_block vs caption node (text+target); target linkage lost"))
        else:
            ledger_list.append(ledger("UNSUPPORTED", f"{path}.{kind}",
                                      "(none)", f"block_container kind '{kind}' has no parser-IR equivalent"))
        for i, child in enumerate(block.get("children", [])):
            map_block(child, nodes, ledger_list, offset, f"{path}.children[{i}]")

    else:
        ledger_list.append(ledger("UNSUPPORTED", f"{path}.{kind}",
                                  "(none)", f"unknown block kind '{kind}'"))


def map_meta_source(aat, ledger_list):
    """AAT meta -> parser-IR source + top-level identity."""
    meta = aat.get("meta", {})
    enc_in = meta.get("source_encoding", "utf-8")
    enc_out = ENC_MAP.get(enc_in)
    if enc_out is None:
        ledger_list.append(ledger("UNSUPPORTED", f"meta.source_encoding={enc_in}",
                                  "source.encoding", f"encoding '{enc_in}' has no parser-IR enum value"))
        enc_out = "unknown"
    elif enc_in in ENC_DIVERGENCE:
        ledger_list.append(ledger("AMBIGUITY", f"meta.source_encoding={enc_in}",
                                  "source.encoding", ENC_DIVERGENCE[enc_in]))
    # work_content_hash vs source_hash: different semantics (content vs raw source bytes)
    ledger_list.append(ledger("AMBIGUITY", "meta.source_hash",
                              "source.work_content_hash",
                              "AAT hashes raw source bytes; parser-IR work_content_hash is content hash; identifier semantics differ"))
    # normalization: parser-IR requires it; AAT has none
    ledger_list.append(ledger("INVENTION", "(none)",
                              "source.normalization", "parser-IR requires normalization enum; AAT has none -> defaulted 'source'"))
    # source_path: AAT has none
    ledger_list.append(ledger("INVENTION", "(none)",
                              "source.source_path", "parser-IR source_path optional; AAT has none -> null"))
    src = {
        "work_content_hash": meta.get("source_hash", "sha256:" + "0"*64),
        "source_path": None,
        "encoding": enc_out,
        "normalization": "source",
    }
    # Producer identity / fidelity metadata has no parser-IR home.
    for f in ("adapter", "adapter_version", "parse_complete"):
        ledger_list.append(ledger("LOSS", f"meta.{f}", "(none)",
                                  f"{f}={meta.get(f)!r} dropped; parser-IR carries no producer identity / parse status"))
    # metrics: heavy fidelity block dropped entirely
    if meta.get("metrics"):
        ledger_list.append(ledger("LOSS", "meta.metrics",
                                  "(none)", "performance/fallback metrics dropped; adapter-fidelity concern not representable"))
    if meta.get("semantic_summary"):
        ledger_list.append(ledger("LOSS", "meta.semantic_summary",
                                  "(none)", "semantic_summary provenance dropped; adapter-fidelity concern not representable"))
    return src


def map_warnings(aat, ledger_list):
    out = []
    for w in aat.get("meta", {}).get("warnings", []):
        # parser-IR diagnostic requires severity + code. AAT warning has only message(+line/path).
        ledger_list.append(ledger("INVENTION", "meta.warnings[].severity",
                                  "warnings[].severity", "AAT warning has no severity -> defaulted 'warning'"))
        ledger_list.append(ledger("INVENTION", "meta.warnings[].code",
                                  "warnings[].code", "AAT warning has no code -> defaulted 'AAT_WARNING'"))
        if w.get("line"):
            ledger_list.append(ledger("AMBIGUITY", "meta.warnings[].line",
                                      "warnings[].span.line", "AAT warning.line vs parser-IR diagnostic.span; coordinate not assembled"))
        out.append({
            "severity": "warning", "code": "AAT_WARNING",
            "message": w.get("message", ""), "span": None,
            "construct": None, "recovery": None,
        })
    return out


def main():
    aat_path = sys.argv[1] if len(sys.argv) > 1 else "aat-sample.json"
    with open(aat_path, encoding="utf-8") as f:
        aat = json.load(f)

    ledger_list = []
    nodes = []
    offset = 0
    for i, block in enumerate(aat.get("blocks", [])):
        map_block(block, nodes, ledger_list, offset, f"blocks[{i}]")

    source = map_meta_source(aat, ledger_list)
    # Top-level identity: schema_id/schema_hash INVENTION (AAT has only version=1)
    ledger_list.append(ledger("INVENTION", "(top-level)",
                              "schema_id/schema_hash",
                              "parser-IR requires schema_id+schema_hash; AAT supplies only version=1; producer must hardcode ABC's identifier"))
    warnings = map_warnings(aat, ledger_list)
    # errors: AAT has none
    ledger_list.append(ledger("INVENTION", "(none)",
                              "errors[]", "parser-IR requires errors[]; AAT has no errors concept -> defaulted empty"))

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
