# Parser-IR Protocol Corrections Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Correct the mapping, hashing, divergence-bundle, and span protocol artifacts that currently block `crates/ab-aat-to-parser-ir`.

**Architecture:** Keep the protocol correction in the existing Python mapping generator and schema/test layer. Do not create the Rust crate in this plan. The generated mapping path stays stable at `data/aat-to-parser-ir-mapping-v1.json`, but its internal `mapping_version` is bumped to `0.1.1` after the corrected generator is run across aozora-rs plus current aozora2html evidence.

**Tech Stack:** Python 3, `jsonschema` via `uv run --isolated --with`, shell smoke tests, JSON Schema draft 2020-12, existing ABC schemas under `../abc/schemas`, existing AAT schema at `data/aat-schema.json`.

## Global Constraints

- Do not implement `crates/ab-aat-to-parser-ir` in this plan.
- Do not make `ab-ir` the adapter or parser-IR contract.
- Do not hand-copy the old 27-rule synthesized table.
- Do not start manifest identity or ABC compatibility-registry hardening here.
- Do not add AAT v2 vocabulary.
- Do not repair aozora2html timeout/protocol/parse-incomplete buckets in this plan.
- Do not treat exploratory unsupported handling as production mapping policy.
- Preserve `data/aat-to-parser-ir-mapping-v1.json` as the stable artifact path; change `mapping_version` to `0.1.1`.
- Generated mapping hashes use `abc-legacy-json-c14n-v0`: parse JSON, serialize sorted-key compact UTF-8 JSON, escape `/` as `\/`, SHA-256 bytes, prefix with `sha256:`.

---

## File Structure

- Create `reports/aat-fidelity/aat_parser_ir_mapping/c14n.py`: shared canonical JSON and schema-hash implementation.
- Modify `reports/aat-fidelity/aat_parser_ir_mapping/mapping_doc.py`: consume `c14n.schema_hash`, accept `mapping_version`, keep stable path but emit `0.1.1`.
- Modify `reports/aat-fidelity/aat_parser_ir_mapping/mapper.py`: correct invalid AAT pointers, add explicit missing-span ledger, advance synthesized span offsets by projected UTF-8 byte length, and keep warigaki as a measured `UNSUPPORTED` rule only when measured inputs contain it.
- Modify `reports/aat-fidelity/aat_parser_ir_mapping/generate.py`: accept multiple `--aat-dir`, pass `mapping_version`, validate pointer contracts, and report input dirs.
- Create `reports/aat-fidelity/aat_parser_ir_mapping/validate_contract.py`: validate generated mappings against ABC schema and AAT pointer grammar.
- Create `data/aat-parser-ir-divergence-bundle-v1.schema.json`: local bundle schema whose `records[]` shape matches ABC's per-entry divergence record schema.
- Modify `tests/aat-parser-ir-mapping-smoke.sh`: cover c14n hashes, aozora-rs zero-unsupported evidence, generated `0.1.1` artifact checks, and required rule-identity checks that replace the old fixed 25-rule invariant.
- Create `tests/aat-parser-ir-schema-hash-smoke.sh`: direct hash canonicalization smoke.
- Create `tests/aat-parser-ir-divergence-bundle-smoke.sh`: validates bundle schema plus ABC per-record schema compatibility.
- Create `tests/aat-parser-ir-mapping-policy-smoke.sh`: small fixture proving span, pointer, and warigaki mapping policy before the full run.
- Regenerate `data/aat-to-parser-ir-mapping-v1.json`.
- Regenerate `docs/superpowers/reports/2026-07-03-aat-parser-ir-mapping-generation.md`.
- Update `docs/superpowers/specs/2026-07-03-ab-aat-to-parser-ir-crate-design.md` from blocked to implementation-ready only if all protocol gates are green.

---

### Task 1: Pin `abc-legacy-json-c14n-v0`

**Files:**
- Create: `reports/aat-fidelity/aat_parser_ir_mapping/c14n.py`
- Modify: `reports/aat-fidelity/aat_parser_ir_mapping/mapping_doc.py`
- Create: `tests/aat-parser-ir-schema-hash-smoke.sh`
- Modify: `tests/aat-parser-ir-mapping-smoke.sh`

**Interfaces:**
- Produces: `c14n.canonical_json(value: object) -> str`
- Produces: `c14n.schema_hash(path: pathlib.Path) -> str`
- Consumes: existing ABC schema files at `../abc/schemas/aat-parser-ir-mapping.schema.json` and `../abc/schemas/parser-ir.schema.json`

- [ ] **Step 1: Add the shared canonicalization module**

Create `reports/aat-fidelity/aat_parser_ir_mapping/c14n.py`:

```python
#!/usr/bin/env python
"""Canonical JSON helpers for ABC schema hashes.

abc-legacy-json-c14n-v0:
1. parse JSON;
2. serialize UTF-8 JSON with sorted object keys and compact separators;
3. escape every "/" as "\\/", including slashes inside string values;
4. SHA-256 the resulting bytes and prefix with "sha256:".
"""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path


def canonical_json(value: object) -> str:
    return json.dumps(
        value,
        ensure_ascii=False,
        sort_keys=True,
        separators=(",", ":"),
    ).replace("/", "\\/")


def schema_hash(path: Path) -> str:
    value = json.loads(path.read_text(encoding="utf-8"))
    payload = canonical_json(value).encode("utf-8")
    return "sha256:" + hashlib.sha256(payload).hexdigest()


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("paths", type=Path, nargs="+")
    args = parser.parse_args()
    for path in args.paths:
        print(f"{path}\t{schema_hash(path)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
```

- [ ] **Step 2: Use `c14n.schema_hash` in `mapping_doc.py`**

In `reports/aat-fidelity/aat_parser_ir_mapping/mapping_doc.py`, remove the local `hashlib` import and the local `canonical_json` / `schema_hash` functions. Add this import after the existing imports:

```python
import c14n
```

Then change both schema-hash calls in `build_mapping_document_from_counts`:

```python
"mapping_schema_hash": c14n.schema_hash(repo_root / "schemas" / "aat-parser-ir-mapping.schema.json"),
```

```python
"target_parser_ir_schema_hash": c14n.schema_hash(repo_root / "schemas" / "parser-ir.schema.json"),
```

- [ ] **Step 3: Add the schema-hash smoke test**

Create `tests/aat-parser-ir-schema-hash-smoke.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
abc_root="$repo_root/../abc"

out="$(python "$repo_root/reports/aat-fidelity/aat_parser_ir_mapping/c14n.py" \
  "$abc_root/schemas/aat-parser-ir-mapping.schema.json" \
  "$abc_root/schemas/parser-ir.schema.json")"

printf '%s\n' "$out"

printf '%s\n' "$out" | rg -F \
  "$abc_root/schemas/aat-parser-ir-mapping.schema.json	sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4"
printf '%s\n' "$out" | rg -F \
  "$abc_root/schemas/parser-ir.schema.json	sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396"
```

Make it executable:

```bash
chmod +x tests/aat-parser-ir-schema-hash-smoke.sh
```

- [ ] **Step 4: Wire the hash smoke into the mapping smoke**

At the top of `tests/aat-parser-ir-mapping-smoke.sh`, after `mkdir -p "$out_dir"`, add:

```bash
bash "$repo_root/tests/aat-parser-ir-schema-hash-smoke.sh"
```

- [ ] **Step 5: Run the focused smoke**

Run:

```bash
bash tests/aat-parser-ir-schema-hash-smoke.sh
```

Expected: both expected hashes print and `rg` exits 0.

- [ ] **Step 6: Commit Task 1**

```bash
git add \
  reports/aat-fidelity/aat_parser_ir_mapping/c14n.py \
  reports/aat-fidelity/aat_parser_ir_mapping/mapping_doc.py \
  tests/aat-parser-ir-schema-hash-smoke.sh \
  tests/aat-parser-ir-mapping-smoke.sh
git commit -m "test: pin parser-ir schema hash canonicalization"
```

---

### Task 2: Define the Divergence Bundle Contract

**Files:**
- Create: `data/aat-parser-ir-divergence-bundle-v1.schema.json`
- Create: `tests/aat-parser-ir-divergence-bundle-smoke.sh`

**Interfaces:**
- Consumes: ABC per-entry record schema `../abc/schemas/aat-parser-ir-divergence.schema.json`
- Produces: local bundle schema `data/aat-parser-ir-divergence-bundle-v1.schema.json`

- [ ] **Step 1: Add the local bundle schema**

Create `data/aat-parser-ir-divergence-bundle-v1.schema.json`:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://abc.local/schemas/aat-parser-ir-divergence-bundle-v1.json",
  "title": "AAT to Parser-IR Divergence Bundle",
  "type": "object",
  "required": [
    "schema_id",
    "schema_version",
    "work_id",
    "mapping",
    "target",
    "aat",
    "preserved_aat_meta",
    "summary",
    "records"
  ],
  "additionalProperties": false,
  "properties": {
    "schema_id": {
      "const": "https://abc.local/schemas/aat-parser-ir-divergence-bundle-v1.json"
    },
    "schema_version": {
      "type": "string",
      "pattern": "^[0-9]+\\.[0-9]+\\.[0-9]+$"
    },
    "work_id": { "type": "string" },
    "mapping": {
      "type": "object",
      "required": ["mapping_id", "mapping_version", "mapping_schema_hash"],
      "additionalProperties": false,
      "properties": {
        "mapping_id": { "type": "string", "format": "uri" },
        "mapping_version": { "type": "string" },
        "mapping_schema_hash": { "$ref": "#/$defs/hash" }
      }
    },
    "target": {
      "type": "object",
      "required": ["parser_ir_schema_id", "parser_ir_schema_hash"],
      "additionalProperties": false,
      "properties": {
        "parser_ir_schema_id": { "type": "string", "format": "uri" },
        "parser_ir_schema_hash": { "$ref": "#/$defs/hash" }
      }
    },
    "aat": {
      "type": "object",
      "required": [
        "version",
        "adapter",
        "adapter_version",
        "source_hash",
        "parse_complete"
      ],
      "additionalProperties": false,
      "properties": {
        "version": { "type": "integer", "minimum": 1 },
        "adapter": { "type": "string" },
        "adapter_version": { "type": "string" },
        "source_hash": { "$ref": "#/$defs/hash" },
        "parse_complete": { "type": "boolean" }
      }
    },
    "preserved_aat_meta": {
      "type": "object",
      "required": ["metrics", "semantic_summary"],
      "additionalProperties": false,
      "properties": {
        "metrics": { "type": ["object", "null"] },
        "semantic_summary": { "type": ["object", "null"] }
      }
    },
    "summary": {
      "type": "object",
      "required": ["LOSS", "INVENTION", "AMBIGUITY", "UNSUPPORTED", "STRUCTURAL"],
      "additionalProperties": false,
      "properties": {
        "LOSS": { "type": "integer", "minimum": 0 },
        "INVENTION": { "type": "integer", "minimum": 0 },
        "AMBIGUITY": { "type": "integer", "minimum": 0 },
        "UNSUPPORTED": { "type": "integer", "minimum": 0 },
        "STRUCTURAL": { "type": "integer", "minimum": 0 }
      }
    },
    "records": {
      "type": "array",
      "items": { "$ref": "#/$defs/divergence_record" }
    }
  },
  "$defs": {
    "hash": {
      "type": "string",
      "pattern": "^sha256:[0-9a-f]{64}$"
    },
    "divergence_record": {
      "type": "object",
      "required": ["rule_id", "category", "message", "count", "first_path"],
      "additionalProperties": false,
      "properties": {
        "rule_id": { "type": "string", "pattern": "^[A-Z]+-[0-9]+$" },
        "category": {
          "enum": ["LOSS", "INVENTION", "AMBIGUITY", "UNSUPPORTED", "STRUCTURAL"]
        },
        "aat_pointer": { "type": ["string", "null"] },
        "parser_ir_pointer": { "type": ["string", "null"] },
        "source_value": { "type": ["string", "integer", "boolean", "null"] },
        "target_value": { "type": ["string", "integer", "boolean", "null"] },
        "message": { "type": "string" },
        "count": { "type": "integer", "minimum": 1 },
        "first_path": { "type": ["string", "null"] }
      }
    }
  }
}
```

- [ ] **Step 2: Add the bundle smoke test**

Create `tests/aat-parser-ir-divergence-bundle-smoke.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
abc_root="$repo_root/../abc"
out_dir="${AB_DB_ROOT:-/db/ab-validator}/aat-fidelity/divergence-bundle-smoke"
bundle="$out_dir/bundle.json"
record="$out_dir/record.json"

rm -rf "$out_dir"
mkdir -p "$out_dir"

cat > "$bundle" <<'JSON'
{
  "schema_id": "https://abc.local/schemas/aat-parser-ir-divergence-bundle-v1.json",
  "schema_version": "0.1.0",
  "work_id": "fixture",
  "mapping": {
    "mapping_id": "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe",
    "mapping_version": "0.1.1",
    "mapping_schema_hash": "sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4"
  },
  "target": {
    "parser_ir_schema_id": "https://w3id.org/abc/schemas/parser-ir.schema.json",
    "parser_ir_schema_hash": "sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396"
  },
  "aat": {
    "version": 1,
    "adapter": "fixture",
    "adapter_version": "fixture 0.1.0",
    "source_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
    "parse_complete": true
  },
  "preserved_aat_meta": {
    "metrics": null,
    "semantic_summary": null
  },
  "summary": {
    "LOSS": 0,
    "INVENTION": 0,
    "AMBIGUITY": 1,
    "UNSUPPORTED": 0,
    "STRUCTURAL": 0
  },
  "records": [
    {
      "rule_id": "A-06",
      "category": "AMBIGUITY",
      "aat_pointer": "meta.source_hash",
      "parser_ir_pointer": "source.work_content_hash",
      "source_value": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
      "target_value": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
      "message": "AAT hashes raw source bytes; parser-IR work_content_hash is content hash; identifier semantics differ",
      "count": 1,
      "first_path": "meta.source_hash"
    }
  ]
}
JSON

jq '.records[0]' "$bundle" > "$record"

uv run --isolated --no-project --with 'jsonschema>=4.0' python - <<PY
import json
from pathlib import Path
from jsonschema import Draft202012Validator

repo = Path("$repo_root")
abc = Path("$abc_root")
bundle = json.loads(Path("$bundle").read_text())
record = json.loads(Path("$record").read_text())

Draft202012Validator(
    json.loads((repo / "data/aat-parser-ir-divergence-bundle-v1.schema.json").read_text())
).validate(bundle)
Draft202012Validator(
    json.loads((abc / "schemas/aat-parser-ir-divergence.schema.json").read_text())
).validate(record)
PY
```

Make it executable:

```bash
chmod +x tests/aat-parser-ir-divergence-bundle-smoke.sh
```

- [ ] **Step 3: Run the bundle smoke**

Run:

```bash
bash tests/aat-parser-ir-divergence-bundle-smoke.sh
```

Expected: command exits 0.

- [ ] **Step 4: Commit Task 2**

```bash
git add \
  data/aat-parser-ir-divergence-bundle-v1.schema.json \
  tests/aat-parser-ir-divergence-bundle-smoke.sh
git commit -m "feat: define parser-ir divergence bundle schema"
```

---

### Task 3: Correct Mapper Ledger and Span Policy

**Files:**
- Modify: `reports/aat-fidelity/aat_parser_ir_mapping/mapper.py`
- Modify: `reports/aat-fidelity/aat_parser_ir_mapping/generate.py`
- Modify: `reports/aat-fidelity/aat_parser_ir_mapping/mapping_doc.py`
- Create: `tests/aat-parser-ir-mapping-policy-smoke.sh`

**Interfaces:**
- Consumes: `mapper.ledger(category, aat, target, note)`
- Produces: ledger entries whose `aat` values are either real AAT schema paths or `(none)`
- Produces: mapping `mapping_version = "0.1.1"`

- [ ] **Step 1: Add mapping version support**

In `reports/aat-fidelity/aat_parser_ir_mapping/mapping_doc.py`, change the signature:

```python
def build_mapping_document_from_counts(
    rule_counts,
    first_path_by_rule,
    first_note_by_rule,
    repo_root=REPO_ROOT,
    mapping_version="0.1.1",
):
```

Change the returned mapping version:

```python
"mapping_version": mapping_version,
```

Change `build_mapping_document` to pass through the version:

```python
def build_mapping_document(ledger_entries, repo_root=REPO_ROOT, mapping_version="0.1.1"):
    counts, first_path, first_note = summarize_ledger(ledger_entries)
    return build_mapping_document_from_counts(
        counts,
        first_path,
        first_note,
        repo_root,
        mapping_version=mapping_version,
    )
```

In `main()`, add:

```python
parser.add_argument("--mapping-version", default="0.1.1")
```

and call:

```python
doc = build_mapping_document(json.load(f), mapping_version=args.mapping_version)
```

- [ ] **Step 2: Emit missing-span ambiguity ledger entries**

In `reports/aat-fidelity/aat_parser_ir_mapping/mapper.py`, add these helpers above `map_span`:

```python
def utf8_len(value):
    return len((value or "").encode("utf-8"))


def span_end(span, fallback_end):
    if span is None:
        return fallback_end
    return span.get("byte_end", fallback_end)
```

Then replace `map_span` with:

```python
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
```

Then update `map_inline` so it returns `(node_or_none, next_offset)`, not just a node. Replace the beginning and the `text` branch with:

```python
def map_inline(node, offset, ledger_list, path):
    kind = node.get("kind")
    span = node.get("span")

    if kind == "text":
        value = node.get("value", "")
        fallback_end = offset + utf8_len(value)
        pir_span = map_span(span, offset, fallback_end, ledger_list, path)
        return {
            "type": "text", "span": pir_span, "text": value,
        }, span_end(span, fallback_end)
```

For the `ruby` branch, compute the fallback from the projected base before constructing the node:

```python
base = node.get("base", "")
fallback_end = offset + utf8_len(base)
pir_span = map_span(span, offset, fallback_end, ledger_list, path)
```

and end the branch with:

```python
        }, span_end(span, fallback_end)
```

For the `gaiji` branch, compute the fallback from the visible resolved string when present, otherwise from the description:

```python
visible = node.get("resolved") or node.get("description", "")
fallback_end = offset + utf8_len(visible)
pir_span = map_span(span, offset, fallback_end, ledger_list, path)
```

and end the branch with:

```python
        }, span_end(span, fallback_end)
```

For the `accent`, `figure`, and `style` branches, compute `fallback_end` from the projected text or filename before calling `map_span`, then return `(node, span_end(span, fallback_end))`. For example, the `style` branch should become:

```python
    if kind == "style":
        projected_text = text_projection(node, ledger_list, path)
        fallback_end = offset + utf8_len(projected_text)
        pir_span = map_span(span, offset, fallback_end, ledger_list, path)
        ledger_list.append(ledger("AMBIGUITY", f"{path}.style",
                                  "emphasis", "style inline_container mapped to emphasis; parser-IR does not preserve nested inline container identity"))
        return {
            "type": "emphasis", "span": pir_span,
            "text": projected_text,
            "style": node.get("style_type", ""),
        }, span_end(span, fallback_end)
```

For dropped inline nodes, return `(None, next_offset)` rather than `None`. Use visible text projection for dropped containers and source length for raw nodes:

```python
    if kind == "warigaki":
        ledger_list.append(ledger("UNSUPPORTED", f"{path}.warigaki",
                                  "(none)", "parser-IR has no warigaki node; upper/lower flattened to text nodes, split-line structure lost"))
        return None, span_end(span, offset)

    if kind == "raw":
        ledger_list.append(ledger("UNSUPPORTED", f"{path}.raw",
                                  "(none)", "parser-IR has no raw node; faithful escape hatch dropped"))
        return None, span_end(span, offset + utf8_len(node.get("source", "")))

    if kind in ("font_size", "tcy", "keigakomi", "yokogumi", "caption"):
        ledger_list.append(ledger("UNSUPPORTED", f"{path}.{kind}",
                                  "emphasis(?)", f"inline_container kind '{kind}' has no first-class parser-IR node; only emphasis.text/style exist"))
        return None, span_end(span, offset + utf8_len(text_projection(node, ledger_list, path)))
```

For the final unknown-kind branch, return:

```python
    return None, span_end(span, offset)
```

Then update every `map_inline` caller to thread offsets. In the paragraph branch:

```python
        for i, child in enumerate(block.get("content", [])):
            cpath = f"{path}.content[{i}]"
            if child.get("kind") == "warigaki":
                ledger_list.append(ledger("UNSUPPORTED", f"{cpath}.warigaki",
                                          "(none)", "parser-IR has no warigaki node; upper/lower flattened to text nodes, split-line structure lost"))
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
```

Change `map_block` to return the next offset. For heading, compute the fallback from concatenated heading text:

```python
        heading_text = "".join(parts)
        fallback_end = offset + utf8_len(heading_text)
        nodes.append({
            "type": "heading",
            "span": map_span(block.get("span"), offset, fallback_end, ledger_list, path),
            "text": heading_text,
            "level": level,
        })
        offset = span_end(block.get("span"), fallback_end)
```

For block container nodes, update the span calls and recurse with offset assignment:

```python
            fallback_end = offset
            nodes.append({"type": "indentation", "span": map_span(block.get("span"), offset, fallback_end, ledger_list, path),
                          "depth": 1, "text": None})
            offset = span_end(block.get("span"), fallback_end)
```

```python
        for i, child in enumerate(block.get("children", [])):
            offset = map_block(child, nodes, ledger_list, offset, f"{path}.children[{i}]")
```

End `map_block` with:

```python
    return offset
```

Finally, in `generate.py`'s `map_aat_document`, change:

```python
mapper.map_block(block, nodes, ledger_list, offset, f"blocks[{index}]")
```

to:

```python
offset = mapper.map_block(block, nodes, ledger_list, offset, f"blocks[{index}]")
```

Make the same assignment in `mapper.py`'s `main()`.

- [ ] **Step 3: Remove the stale offset claim from the probe docstring**

In the top docstring of `mapper.py`, replace:

```python
  - A running `offset` advances by the AAT byte_end - byte_start of each
    emitted inline to give parser-IR `span.start/end` (char-ish offsets), since
    parser-IR span semantics are unspecified in schema while AAT spans are
    decoded-UTF8 byte offsets. line is taken from AAT line_start; column is
    unknown (null). This is itself an AMBIGUITY entry (see STRUCTURAL/SPAN).
```

with:

```python
  - A running `offset` advances by emitted parser-IR node span end. When AAT
    spans are absent, the fallback end is the projected visible text's UTF-8
    byte length. This keeps synthesized spans monotonic and records the
    approximation as an AMBIGUITY entry.
```

- [ ] **Step 4: Correct invalid gaiji and invented-field pointers**

In `mapper.py`, change the ruby scope ledger from:

```python
ledger_list.append(ledger("INVENTION", f"{path}.ruby.scope", "ruby.scope",
                          "AAT has no scope field; defaulted to 'explicit'"))
```

to:

```python
ledger_list.append(ledger("INVENTION", "(none)", "ruby.scope",
                          "AAT has no scope field; defaulted to 'explicit'"))
```

Change the gaiji raw-marker ledger from:

```python
ledger_list.append(ledger("INVENTION", f"{path}.gaiji.raw_marker",
                          "gaiji.raw_marker", "AAT has no raw source marker; used description as raw_marker"))
```

to:

```python
ledger_list.append(ledger("INVENTION", f"{path}.gaiji.description",
                          "gaiji.raw_marker", "AAT has no raw source marker; used description as raw_marker"))
```

Change the gaiji unicode ledger from:

```python
ledger_list.append(ledger("LOSS", f"{path}.gaiji.unicode",
                          "gaiji.unicode", "AAT does not separate unicode codepoint from resolved string"))
```

to:

```python
ledger_list.append(ledger("LOSS", "(none)",
                          "gaiji.unicode", "AAT does not separate unicode codepoint from resolved string"))
```

In `map_warnings`, change warning code/severity ledgers to use `(none)`:

```python
ledger_list.append(ledger("INVENTION", "(none)",
                          "warnings[].severity", "AAT warning has no severity -> defaulted 'warning'"))
ledger_list.append(ledger("INVENTION", "(none)",
                          "warnings[].code", "AAT warning has no code -> defaulted 'AAT_WARNING'"))
```

- [ ] **Step 5: Correct block semantic pointer shapes**

In `map_block`, change the structural ledger from:

```python
ledger_list.append(ledger("STRUCTURAL", f"{path}[block={kind}]",
                          "(none)", f"block container of kind '{kind}' has no parser-IR node; boundary + span + style lost, only inlines emitted"))
```

to:

```python
ledger_list.append(ledger("STRUCTURAL", f"{path}.{kind}",
                          "(none)", f"block container of kind '{kind}' has no parser-IR node; boundary + span + style lost, only inlines emitted"))
```

In the `heading` branch, change:

```python
ledger_list.append(ledger("LOSS", f"{path}.heading.content[{i}]={child.get('kind')}",
                          "(none)", "non-text inline inside heading flattened to text projection; structure lost"))
```

to:

```python
ledger_list.append(ledger("LOSS", f"{path}.heading.content[{i}].{child.get('kind')}",
                          "(none)", "non-text inline inside heading flattened to text projection; structure lost"))
```

Keep `heading.level` and `heading.style` paths; Task 4's pointer validator treats kind-qualified paths as valid AAT pointer grammar.

- [ ] **Step 6: Let `generate.py` accept a mapping version**

In `reports/aat-fidelity/aat_parser_ir_mapping/generate.py`, add:

```python
parser.add_argument("--mapping-version", default="0.1.1")
```

Then pass it:

```python
mapping_document = mapping_doc.build_mapping_document_from_counts(
    mapping_rule_counter,
    first_path_by_rule,
    first_note_by_rule,
    repo_root=args.abc_root.resolve(),
    mapping_version=args.mapping_version,
)
```

Add the version to the summary:

```python
"mapping_version": mapping_document["mapping_version"],
```

- [ ] **Step 7: Add a policy smoke fixture**

Create `tests/aat-parser-ir-mapping-policy-smoke.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="${AB_DB_ROOT:-/db/ab-validator}/aat-fidelity/aat-parser-ir-mapping-policy-smoke"
aat_dir="$out_dir/aat"

rm -rf "$out_dir"
mkdir -p "$aat_dir"

cat > "$aat_dir/policy.json" <<'JSON'
{
  "version": 1,
  "work_id": "policy",
  "meta": {
    "adapter": "fixture",
    "adapter_version": "fixture 0.1.0",
    "source_encoding": "windows-31j-lossy",
    "source_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
    "parse_complete": true,
    "warnings": [
      { "message": "fixture warning" }
    ]
  },
  "blocks": [
    {
      "kind": "paragraph",
      "content": [
        {
          "kind": "gaiji",
          "description": "U+4E00",
          "resolved": "一",
          "jis_code": null,
          "unresolved_reason": null
        },
        {
          "kind": "warigaki",
          "upper": [{ "kind": "text", "value": "上" }],
          "lower": [{ "kind": "text", "value": "下" }]
        },
        {
          "kind": "style",
          "style_type": "kaeriten",
          "content": [{ "kind": "text", "value": "レ" }]
        }
      ]
    }
  ]
}
JSON

uv run --isolated --no-project --with 'jsonschema>=4.0' \
  "$repo_root/reports/aat-fidelity/aat_parser_ir_mapping/generate.py" \
  --aat-dir "$aat_dir" \
  --abc-root "$repo_root/../abc" \
  --mapping-version 0.1.1 \
  --out "$out_dir/mapping.json" \
  --summary-json "$out_dir/summary.json"

jq -e '.mapping_version == "0.1.1"' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "UNSUPPORTED" and (.description | test("warigaki")))' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "AMBIGUITY" and .parser_ir_pointer == "span")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .aat_pointer == "blocks[].content[].gaiji.description" and .parser_ir_pointer == "gaiji.raw_marker")' "$out_dir/mapping.json"
jq -e 'all(.transform_rule_descriptions[]; .aat_pointer != "blocks[].content[].gaiji.raw_marker" and .aat_pointer != "blocks[].content[].gaiji.unicode")' "$out_dir/mapping.json"

python - <<PY
import json
from pathlib import Path
import sys

repo_root = Path("$repo_root")
sys.path.insert(0, str(repo_root / "reports/aat-fidelity/aat_parser_ir_mapping"))
import generate

aat = json.loads((Path("$aat_dir") / "policy.json").read_text())
_ledger, nodes, _block_kinds, _inline_kinds, _has_warigaki = generate.map_aat_document(aat)
spans = [node["span"] for node in nodes]
assert spans, "fixture should emit parser-IR nodes"
assert any(span["end"] > 0 for span in spans), spans
for previous, current in zip(spans, spans[1:]):
    assert current["start"] >= previous["start"], spans
    assert current["end"] >= current["start"], spans
PY
```

Make it executable:

```bash
chmod +x tests/aat-parser-ir-mapping-policy-smoke.sh
```

- [ ] **Step 8: Run the policy smoke**

Run:

```bash
bash tests/aat-parser-ir-mapping-policy-smoke.sh
```

Expected: command exits 0, the generated fixture mapping contains `UNSUPPORTED`, `span`, and corrected gaiji description rules, and the parser-IR nodes produced by the probe have monotonic synthesized spans with at least one non-zero end offset.

- [ ] **Step 9: Commit Task 3**

```bash
git add \
  reports/aat-fidelity/aat_parser_ir_mapping/mapper.py \
  reports/aat-fidelity/aat_parser_ir_mapping/generate.py \
  reports/aat-fidelity/aat_parser_ir_mapping/mapping_doc.py \
  tests/aat-parser-ir-mapping-policy-smoke.sh
git commit -m "fix: correct parser-ir mapping ledger policy"
```

---

### Task 4: Add Mapping Contract Validation

**Files:**
- Create: `reports/aat-fidelity/aat_parser_ir_mapping/validate_contract.py`
- Modify: `reports/aat-fidelity/aat_parser_ir_mapping/generate.py`
- Modify: `tests/aat-parser-ir-mapping-policy-smoke.sh`

**Interfaces:**
- Produces: `validate_contract.validate_mapping_contract(mapping: dict, aat_schema: dict) -> None`
- Consumes: generated mapping documents from `mapping_doc.build_mapping_document_from_counts`

- [ ] **Step 1: Add the mapping contract validator**

Create `reports/aat-fidelity/aat_parser_ir_mapping/validate_contract.py`:

```python
from __future__ import annotations

import re

MAX_POINTER_DEPTH = 12
MAX_ARRAY_DEPTH = 6


class MappingContractError(ValueError):
    pass


def _deref(schema: dict, node: dict) -> dict:
    ref = node.get("$ref")
    if not ref:
        return node
    prefix = "#/$defs/"
    if not ref.startswith(prefix):
        raise MappingContractError(f"unsupported ref: {ref}")
    return schema["$defs"][ref[len(prefix):]]


def _kind_values(node: dict) -> list[str]:
    kind = node.get("properties", {}).get("kind", {})
    if "const" in kind:
        return [kind["const"]]
    enum = kind.get("enum")
    if isinstance(enum, list):
        return [value for value in enum if isinstance(value, str)]
    return []


def _collect_paths(
    schema: dict,
    node: dict,
    prefix: str,
    out: set[str],
    depth: int = 0,
) -> None:
    if depth > MAX_POINTER_DEPTH:
        return
    node = _deref(schema, node)
    if "oneOf" in node:
        for child in node["oneOf"]:
            resolved = _deref(schema, child)
            for kind in _kind_values(resolved):
                if not prefix:
                    continue
                out.add(f"{prefix}.{kind}")
                _collect_paths(schema, resolved, f"{prefix}.{kind}", out, depth + 1)
            _collect_paths(schema, resolved, prefix, out, depth + 1)
        return
    if node.get("type") == "array":
        out.add(prefix)
        if prefix.count("[]") >= MAX_ARRAY_DEPTH:
            return
        _collect_paths(schema, node["items"], f"{prefix}[]", out, depth + 1)
        return
    if node.get("type") == "object" or "properties" in node:
        if prefix:
            out.add(prefix)
        for name, child in node.get("properties", {}).items():
            if name == "kind":
                continue
            child_prefix = f"{prefix}.{name}" if prefix else name
            out.add(child_prefix)
            _collect_paths(schema, child, child_prefix, out, depth + 1)


def allowed_aat_pointers(aat_schema: dict) -> set[str]:
    out: set[str] = set()
    _collect_paths(aat_schema, aat_schema, "", out)
    return out


_INDEX_RE = re.compile(r"\[[0-9]+\]")


def fold_pointer(pointer: str) -> str:
    return _INDEX_RE.sub("[]", pointer)


def validate_mapping_contract(mapping: dict, aat_schema: dict) -> None:
    allowed = allowed_aat_pointers(aat_schema)
    for rule in mapping.get("transform_rule_descriptions", []):
        pointer = rule.get("aat_pointer")
        if pointer is None:
            continue
        folded = fold_pointer(pointer)
        if folded not in allowed:
            raise MappingContractError(
                f"{rule.get('rule_id')} has non-schema AAT pointer {pointer!r}"
            )
```

- [ ] **Step 2: Integrate contract validation into `generate.py`**

In `generate.py`, add:

```python
import validate_contract
```

Add an argument that resolves by default from this repository, not from the caller's current directory:

```python
repo_root = SCRIPT_DIR.parents[2]
parser.add_argument("--aat-schema", type=Path, default=repo_root / "data/aat-schema.json")
```

After `validate_mapping(mapping_document, args.abc_root.resolve())`, add:

```python
aat_schema_path = args.aat_schema
if not aat_schema_path.is_absolute():
    aat_schema_path = repo_root / aat_schema_path
validate_contract.validate_mapping_contract(
    mapping_document,
    json.loads(aat_schema_path.read_text(encoding="utf-8")),
)
```

- [ ] **Step 3: Extend the policy smoke to exercise contract validation**

In `tests/aat-parser-ir-mapping-policy-smoke.sh`, add this assertion after the current `jq` checks:

```bash
python - <<PY
import json
from pathlib import Path
import sys

repo_root = Path("$repo_root")
sys.path.insert(0, str(repo_root / "reports/aat-fidelity/aat_parser_ir_mapping"))
import validate_contract

mapping = json.loads(Path("$out_dir/mapping.json").read_text())
schema = json.loads((repo_root / "data/aat-schema.json").read_text())
validate_contract.validate_mapping_contract(mapping, schema)
PY
```

- [ ] **Step 4: Run the policy smoke**

Run:

```bash
bash tests/aat-parser-ir-mapping-policy-smoke.sh
```

Expected: command exits 0. If it fails on a pointer, fix the corresponding ledger source pointer rather than weakening the validator.

- [ ] **Step 5: Commit Task 4**

```bash
git add \
  reports/aat-fidelity/aat_parser_ir_mapping/validate_contract.py \
  reports/aat-fidelity/aat_parser_ir_mapping/generate.py \
  tests/aat-parser-ir-mapping-policy-smoke.sh
git commit -m "test: validate parser-ir mapping pointer contracts"
```

---

### Task 5: Regenerate Mapping v1.1 From Measured Corpora

**Files:**
- Modify: `reports/aat-fidelity/aat_parser_ir_mapping/generate.py`
- Modify: `tests/aat-parser-ir-mapping-smoke.sh`
- Modify: `data/aat-to-parser-ir-mapping-v1.json`
- Modify: `docs/superpowers/reports/2026-07-03-aat-parser-ir-mapping-generation.md`
- Modify: `docs/superpowers/specs/2026-07-03-ab-aat-to-parser-ir-crate-design.md`

**Interfaces:**
- Consumes: aozora-rs AAT corpus at `scratch/morph-full-corpus/aats/aozora-rs-adapter`
- Consumes: aozora2html AAT corpus at `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter`
- Produces: stable mapping artifact `data/aat-to-parser-ir-mapping-v1.json` with `mapping_version = "0.1.1"`

- [ ] **Step 1: Allow multiple measured AAT inputs**

In `generate.py`, replace:

```python
parser.add_argument("--aat-dir", type=Path, required=True)
```

with:

```python
parser.add_argument("--aat-dir", type=Path, action="append", required=True)
```

Replace:

```python
files = sorted(args.aat_dir.glob("*.json"))
if not files:
    raise SystemExit(f"no AAT JSON files found under {args.aat_dir}")
```

with:

```python
files = []
for aat_dir in args.aat_dir:
    files.extend(sorted(aat_dir.glob("*.json")))
if not files:
    dirs = ", ".join(str(path) for path in args.aat_dir)
    raise SystemExit(f"no AAT JSON files found under: {dirs}")
```

Add to the summary:

```python
"aat_dirs": [str(path) for path in args.aat_dir],
```

In `write_report`, after the summary table, add:

```python
    lines.extend(["", "## Inputs", ""])
    lines.extend(f"- `{path}`" for path in summary["aat_dirs"])
    lines.append("")
```

- [ ] **Step 2: Update mapping smoke assertions**

In `tests/aat-parser-ir-mapping-smoke.sh`, keep the first generation as an aozora-rs-only zero-unsupported gate:

```bash
uv run --isolated --no-project --with 'jsonschema>=4.0' \
  "$repo_root/reports/aat-fidelity/aat_parser_ir_mapping/generate.py" \
  --aat-dir "$aat_dir" \
  --abc-root "$abc_root" \
  --mapping-version 0.1.1 \
  --out "$out_dir/aozora-rs-only.mapping.json" \
  --summary-json "$out_dir/aozora-rs-only.summary.json" \
  --assert-zero-unsupported

jq -e '.files_scanned == 17894' "$out_dir/aozora-rs-only.summary.json"
jq -e '.files_with_unsupported == 0' "$out_dir/aozora-rs-only.summary.json"
```

Then add the production combined generation:

```bash
aozora2html_dir="${AB_AOZORA2HTML_AAT_DIR:-/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter}"

uv run --isolated --no-project --with 'jsonschema>=4.0' \
  "$repo_root/reports/aat-fidelity/aat_parser_ir_mapping/generate.py" \
  --aat-dir "$aat_dir" \
  --aat-dir "$aozora2html_dir" \
  --abc-root "$abc_root" \
  --mapping-version 0.1.1 \
  --out "$out_dir/mapping.json" \
  --summary-json "$out_dir/summary.json"

jq -e '.mapping_version == "0.1.1"' "$out_dir/mapping.json"
jq -e '.mapping_schema_hash == "sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4"' "$out_dir/summary.json"
jq -e '.target_parser_ir_schema_hash == "sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396"' "$out_dir/summary.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "UNSUPPORTED" and (.description | test("warigaki")))' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "AMBIGUITY" and .parser_ir_pointer == "span")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .aat_pointer == "blocks[].content[].gaiji.description" and .parser_ir_pointer == "gaiji.raw_marker")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "AMBIGUITY" and .aat_pointer == "meta.source_hash" and .parser_ir_pointer == "source.work_content_hash")' "$out_dir/mapping.json"
jq -e 'all(.transform_rule_descriptions[]; .aat_pointer != "blocks[].content[].gaiji.raw_marker" and .aat_pointer != "blocks[].content[].gaiji.unicode")' "$out_dir/mapping.json"
```

- [ ] **Step 3: Regenerate the committed mapping artifact and report**

Run:

```bash
uv run --isolated --no-project --with 'jsonschema>=4.0' \
  reports/aat-fidelity/aat_parser_ir_mapping/generate.py \
  --aat-dir scratch/morph-full-corpus/aats/aozora-rs-adapter \
  --aat-dir /db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter \
  --abc-root ../abc \
  --mapping-version 0.1.1 \
  --out data/aat-to-parser-ir-mapping-v1.json \
  --summary-json scratch/aat-parser-ir-mapping-summary.json \
  --report-md docs/superpowers/reports/2026-07-03-aat-parser-ir-mapping-generation.md
```

Expected: command exits 0 and writes the committed mapping/report.

- [ ] **Step 4: Verify the regenerated artifact**

Run:

```bash
jq -e '.mapping_version == "0.1.1"' data/aat-to-parser-ir-mapping-v1.json
jq -e '.mapping_schema_hash == "sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4"' data/aat-to-parser-ir-mapping-v1.json
jq -e '.target_parser_ir_schema_hash == "sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396"' data/aat-to-parser-ir-mapping-v1.json
jq -e 'any(.transform_rule_descriptions[]; .category == "UNSUPPORTED" and (.description | test("warigaki")))' data/aat-to-parser-ir-mapping-v1.json
jq -e 'any(.transform_rule_descriptions[]; .category == "AMBIGUITY" and .parser_ir_pointer == "span")' data/aat-to-parser-ir-mapping-v1.json
jq -e 'any(.transform_rule_descriptions[]; .aat_pointer == "blocks[].content[].gaiji.description" and .parser_ir_pointer == "gaiji.raw_marker")' data/aat-to-parser-ir-mapping-v1.json
jq -e 'any(.transform_rule_descriptions[]; .category == "AMBIGUITY" and .aat_pointer == "meta.source_hash" and .parser_ir_pointer == "source.work_content_hash")' data/aat-to-parser-ir-mapping-v1.json
jq -e 'all(.transform_rule_descriptions[]; .aat_pointer != "blocks[].content[].gaiji.raw_marker" and .aat_pointer != "blocks[].content[].gaiji.unicode")' data/aat-to-parser-ir-mapping-v1.json
```

Expected: all `jq` commands exit 0.

- [ ] **Step 5: Update the crate design spec status**

In `docs/superpowers/specs/2026-07-03-ab-aat-to-parser-ir-crate-design.md`, change:

```markdown
Status: blocked pending protocol corrections
```

to:

```markdown
Status: proposed after protocol corrections
```

Then add a short note under `## Review Corrections`:

```markdown
Protocol correction status: `data/aat-to-parser-ir-mapping-v1.json` now carries `mapping_version = 0.1.1`, includes measured aozora2html `UNSUPPORTED` warigaki evidence, uses `abc-legacy-json-c14n-v0`, and validates against the local AAT pointer contract.
```

Also update the divergence bundle example in that spec so every `mapping_version` example is `"0.1.1"`, not `"0.1.0"`.

- [ ] **Step 6: Run mapping smokes**

Run:

```bash
bash tests/aat-parser-ir-schema-hash-smoke.sh
bash tests/aat-parser-ir-divergence-bundle-smoke.sh
bash tests/aat-parser-ir-mapping-policy-smoke.sh
bash tests/aat-parser-ir-mapping-smoke.sh
```

Expected: all commands exit 0.

- [ ] **Step 7: Commit Task 5**

```bash
git add \
  reports/aat-fidelity/aat_parser_ir_mapping/generate.py \
  tests/aat-parser-ir-mapping-smoke.sh \
  data/aat-to-parser-ir-mapping-v1.json \
  docs/superpowers/reports/2026-07-03-aat-parser-ir-mapping-generation.md \
  docs/superpowers/specs/2026-07-03-ab-aat-to-parser-ir-crate-design.md
git commit -m "feat: regenerate parser-ir mapping protocol v1.1"
```

---

### Task 6: Final Verification and Handoff Update

**Files:**
- Read: all files changed by Tasks 1-5
- Modify if needed: `docs/superpowers/reports/2026-07-03-post-measurement-abc-sync.md`

**Interfaces:**
- Consumes: protocol-corrected `data/aat-to-parser-ir-mapping-v1.json`
- Produces: a final report state that says the Rust crate implementation plan may start

- [ ] **Step 1: Check whether ABC sync needs a mapping-version update**

Run:

```bash
rg -n "mapping artifact|mapping_version|0\\.1\\.0|generated mapping rules|UNSUPPORTED|Next Gate" docs/superpowers/reports/2026-07-03-post-measurement-abc-sync.md
```

If it mentions `0.1.0` or "zero `UNSUPPORTED`" without clarifying that this was aozora-rs-only evidence, edit it to say:

```markdown
- mapping artifact: `data/aat-to-parser-ir-mapping-v1.json`
- mapping version: `0.1.1`
- aozora-rs measured corpus: 17,894 files scanned, 0 files with `UNSUPPORTED`
- combined generated mapping evidence includes current aozora2html measured unsupported warigaki policy and span-synthesis policy.
```

- [ ] **Step 2: Run Python syntax verification**

Run:

```bash
python -m compileall reports/aat-fidelity/aat_parser_ir_mapping
```

Expected: exits 0.

- [ ] **Step 3: Run focused smokes**

Run:

```bash
bash tests/aat-parser-ir-schema-hash-smoke.sh
bash tests/aat-parser-ir-divergence-bundle-smoke.sh
bash tests/aat-parser-ir-mapping-policy-smoke.sh
bash tests/aat-parser-ir-mapping-smoke.sh
```

Expected: all commands exit 0.

- [ ] **Step 4: Run broad report regression smokes**

Run:

```bash
bash tests/aozora2html-measurement-audit-smoke.sh
bash tests/aozora2html-policy-samples-smoke.sh
bash tests/aozora2html-policy-residual-triage-smoke.sh
bash tests/aozora2html-source-feature-gap-classifier-smoke.sh
```

Expected: all commands exit 0.

- [ ] **Step 5: Run formatting/diff checks**

Run:

```bash
git diff --check
git status --short
```

Expected: `git diff --check` exits 0. `git status --short` shows only intended files plus any pre-existing unrelated handoff files.

- [ ] **Step 6: Commit final report update if needed**

If Step 1 changed `docs/superpowers/reports/2026-07-03-post-measurement-abc-sync.md`, commit it:

```bash
git add docs/superpowers/reports/2026-07-03-post-measurement-abc-sync.md
git commit -m "docs: sync parser-ir protocol correction status"
```

If Step 1 did not change the sync report, do not create an empty commit.

- [ ] **Step 7: Record the next allowed work**

At the end of the final response for this plan execution, state:

```text
Protocol corrections are complete. The next allowed plan is the `crates/ab-aat-to-parser-ir` implementation plan using the corrected `data/aat-to-parser-ir-mapping-v1.json` artifact.
```

Do not start that crate implementation in the same execution unless explicitly asked.

---

## Self-Review

- Spec coverage: Gate 1 is Task 1; Gate 2 is Task 2; Gate 3 is Task 5; Gate 4 is Tasks 3-4 with a terminating pointer validator and multi-value block-container kind expansion; Gate 5 is Task 3 plus Task 5 with a span rule and a smoke assertion that synthesized spans advance beyond `0,0`. The final route to the Rust crate is Task 6.
- Red-flag scan: every new file has full content and every modification has exact snippets.
- Type consistency: `mapping_version`, `abc-legacy-json-c14n-v0`, `records[]`, `preserved_aat_meta`, `UnmeasuredDivergencePolicy::Refuse`, and `RecordExploratory` names match the corrected design spec.
