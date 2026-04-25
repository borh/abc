# Aozora Bunko Parser Validation Harness — Design Spec

**Date:** 2026-04-25
**Status:** Revised after design review

---

## 1. Purpose

Build a suite of **independent, composable tools** that evaluate Aozora Bunko parsers against the official corpus. The goal is to answer a single question well:

> **"Does parser X correctly handle the features it claims to support, across the works in the corpus that use those features?"**

Once this works for one parser, we add comparators for cross-parser analysis. The current ABC parser is **excluded** — this harness evaluates reference parsers to inform which one ABC should adopt.

The `.txt` source is authoritative. Official XHTML is one valid rendering — deviations are analyzed, not treated as bugs.

---

## 2. Principles

1. **Tools, not a framework.** Each component is a standalone binary. They share data directories and JSON formats, not internal Rust APIs.
2. **JSON is the wire format.** The Abstract Annotation Tree (AAT) is a JSON schema, not a Rust type system. Adapters emit JSON; tools consume JSON.
3. **AAT is a semantic tree, not parser events.** Blocks are nested objects and inline content is an ordered array. Adapters must fold parser start/end events before emitting AAT.
4. **Start with one parser.** Build the indexer + property tests for `aozora2` first. Add rendering comparison next. Then cross-parser diff. Then the next parser.
5. **Reproducibility is mandatory.** Every report records adapter version, content hash, and timestamp. A report from today must be diffable against a report from next month.
6. **Measure before optimizing the protocol.** JSON is the canonical interchange format until benchmark data shows it is the bottleneck. If it is, add a measured cache/sidecar format without changing report semantics.
7. **Don't parse the reference.** Compare against official XHTML directly in the rendering layer. Don't round-trip through AAT.

---

## 3. Tool Decomposition

```
ab-validator/                    # workspace root
├── Cargo.toml
├── README.md
│
├── crates/
│   ├── ab-index/                # Crate 1: Corpus feature indexer
│   │   └── src/main.rs
│   ├── ab-check/                # Crate 2: Property-based invariant testing
│   │   └── src/main.rs
│   ├── ab-compare/              # Crate 3: AAT JSON diff between two files
│   │   └── src/main.rs
│   ├── ab-render-diff/          # Crate 4: HTML normalization + diff
│   │   └── src/main.rs
│   └── ab-report/               # Crate 5: Report aggregation + run-to-run diff
│       └── src/main.rs
│
├── adapters/                    # External adapter programs (not workspace crates)
│   ├── aozora2/
│   │   └── src/main.rs          # Links aozora-core, emits AAT JSON
│   ├── aozora-rs/
│   │   └── adapter.rs
│   ├── aozora-epub3/
│   │   ├── run.sh               # Runs AozoraEpub3.jar, parses XHTML
│   │   └── adapter.py           # Emits AAT JSON from XHTML
│   └── aozora-parser-js/
│       └── run.sh               # Runs node, walks nested arrays to JSON
│
└── data/
    ├── feature-patterns.toml    # Regex patterns for feature detection
    ├── block-patterns.toml      # Source block start/end marker definitions
    ├── body-patterns.toml       # Body/colophon boundary heuristics
    └── aat-schema.json          # JSON Schema for AAT wire format
```

---

## 4. Source Encoding Contract

Aozora Bunko `.txt` files are not assumed to be UTF-8 on disk. Tools and adapters must follow the same boundary rule:

1. Read raw source bytes from the corpus file or stdin.
2. Decode to UTF-8 before regex detection, parsing, property checks, or AAT emission.
3. Detect encoding in this order: UTF-8 BOM, valid UTF-8, then Shift_JIS/Windows-31J fallback using `encoding_rs`.
4. Record the detected encoding in AAT metadata as `meta.source_encoding` with values such as `utf-8`, `utf-8-bom`, or `windows-31j`.
5. Define `span.line_start` and `span.line_end` as 1-indexed line numbers in the decoded UTF-8 source.
6. Define `span.byte_start` and `span.byte_end` as UTF-8 byte offsets in the decoded source, not raw Shift_JIS byte offsets and not character indexes.
7. Define `meta.source_hash` as SHA256 of the original raw source bytes, so source identity is independent of the chosen decoder.

If decoding fails, the tool records a fatal decode error for that work. Adapters exit with code 1 for undecodable input because they cannot emit trustworthy AAT spans or text.

---

## 5. Tool: `ab-index`

### Responsibility
Scan the Aozora Bunko `.txt` corpus, detect which spec features each work uses, and write an index file.

### Why this is first
Before we can validate any parser, we need to know which works exercise which features. This is a one-time investment that every other tool depends on.

### Data Model (JSON output)

```json
{
  "version": 1,
  "corpus_root": "/path/to/aozorabunko",
  "corpus_hash": "sha256:...",
  "generated_at": "2026-04-25T18:00:00Z",
  "works_count": 18542,
  "works": [
    {
      "id": "000148_799",
      "txt_path": "cards/000148/files/799_ruby_19091/...",
      "html_path": "cards/000148/files/799_ruby_19091/...",
      "features": ["ruby", "gaiji", "jisage_block"],
      "feature_lines": {
        "ruby": [42, 89, 156],
        "gaiji": [203],
        "jisage_block": [1]
      }
    }
  ],
  "by_feature": {
    "ruby": ["000148_799", "000148_800", ...],
    "gaiji": ["000148_799", ...]
  }
}
```

### Feature Detection

Features are detected via regex patterns stored in `data/feature-patterns.toml`. The pattern set is derived from `chuki_tag.txt` and the official annotation guide. Each feature has:

```toml
[features.ruby]
pattern = '《[^》]+》'
description = "Ruby annotation (furigana)"

[features.gaiji]
pattern = '※［＃[^］]+］'
description = "External character (gaiji)"

[features.jisage_line]
pattern = '［＃[０-９\d]+字下げ］'
description = "Single-line indentation"

# ... etc.
```

**Feature index semantics:** The index is a sampling and routing aid, not an oracle. Regex detection intentionally favors recall over precision: it may over-report malformed or literal markup, and it may under-report legacy or unusual notation that does not match the configured patterns. `ab-check` and adapter diagnostics remain authoritative for a specific work.

**Note on feature granularity:** We do not enumerate all 91 features as distinct patterns initially. We group closely related features (e.g., all 傍点 variants under `boten`) and split only when the data shows parsers disagree on the sub-variants. The index can be regenerated with finer granularity as needed.

### CLI

```bash
# Build index (one-time, ~5 min for full corpus)
ab-index --corpus /path/to/aozorabunko --output index.json

# Query: which works have feature X?
ab-index --query ruby --index index.json

# Query: which works have ALL of these features?
ab-index --query-all ruby,gaiji,jisage_block --index index.json

# Sample N works per feature, for human inspection and benchmark sample generation
# Output: JSON array of work IDs
ab-index --sample 50 --features ruby,gaiji --index index.json --output sample.json
```

Query and sample commands write JSON arrays of work IDs. `--sample` output is consumed by humans, benchmark sample files, and tools that accept `--work-ids`.

### Implementation Notes
- Use `walkdir` for filesystem traversal and `rayon` for parallel feature detection (CPU-bound regex matching on file contents). No async I/O needed — the work is CPU-bound after reading.
- Decode each source file with the shared source encoding contract before regex detection.
- All stored paths are normalized relative paths with `/` separators, such as `cards/000148/files/...`, regardless of host OS.
- `corpus_hash` is deterministic across machines: SHA256 over the sorted list of `(relative_posix_path, file_size, file_sha256)` tuples. Do not include absolute paths, platform separators, or mtimes in `corpus_hash`.
- The byte stream for `corpus_hash` is `relative_posix_path`, NUL, decimal `file_size`, NUL, lowercase hex `file_sha256`, newline for each sorted file entry.
- File content hashing happens in the same parallel per-file pass as file reading and feature detection, so reproducibility does not add an extra full-corpus traversal.
- Cache invalidation may use a separate local cache key containing mtimes for speed, but reports and indexes use `corpus_hash` for reproducibility.
- `by_feature_combo` is **intentionally omitted** from the index. Combinatorial explosion is not useful. Query tools can intersect `by_feature` on demand.

---

## 6. Tool: `ab-check`

### Responsibility
Run property-based invariant checks on a parser's output for a set of works.

### Why this is second
Property tests answer the highest-value question: "Does the parser lose or misrepresent information from the source?" This is cheaper than full AAT comparison and catches the most serious bugs.

### Input
- A `.txt` file
- The parser's AAT JSON output (produced by the adapter)

### Properties

| Property | Check |
|----------|-------|
| `schema_valid` | AAT JSON conforms to `data/aat-schema.json`; semantic properties are skipped when this fails. |
| `parse_completeness` | `meta.parse_complete` is true; warnings are reported but do not fail this property. |
| `visible_text_body_order` | For works with a detected body region, AAT visible text is accounted for in decoded body-source order: ordinary text and ruby bases match source characters, while inline gaiji nodes match source gaiji markers. Works without body boundaries are skipped for this property. |
| `ruby_completeness` | Every source ruby marker `《...》` that is not inside an editor note has a corresponding inline `ruby` node with source line diagnostics. "Inside an editor note" means the marker falls between `［＃` and the next `］` in the decoded source line. |
| `gaiji_resolution` | Every source gaiji marker `※［＃...］` has a corresponding inline `gaiji` node with either `resolved` set or `unresolved_reason` set. |
| `block_balance` | Every source block-start marker defined in `data/block-patterns.toml` has a matching source block-end marker, or the AAT records an unclosed block warning with a source location. |
| `no_dropped_lines` | Non-empty decoded body-region source lines that contain visible text contribute at least one visible-text node to the AAT projection. |
| `heading_level_consistency` | Inline headings (`［＃「…」は大見出し］`) and block headings (`［＃大見出し］`) agree on level within a work. |

### Diagnostic Rules

- Diagnostics must include the source line when the failure is detected from the `.txt` side.
- Diagnostics should include an AAT `path` and node `span` when the failure concerns emitted AAT content.
- Checks that depend on regex feature detection must mark their result with `"confidence": "heuristic"`.
- Batch checks invoke adapters through a bounded subprocess worker pool. The default worker count is the logical CPU count; `--jobs N` overrides it.
- Each adapter subprocess has a per-work timeout. The default is 60 seconds; `--per-work-timeout 120s` overrides it.
- `ab-check` embeds `data/aat-schema.json` at compile time so the binary is portable and does not depend on the caller's working directory.
- `ab-check` compiles the JSON Schema validator once at startup and shares it across workers.

### CLI

```bash
# Run all properties on a single work
ab-check --txt work.txt --aat work.aozora2.json --output check.json

# Run on a sampled batch (ab-check does its own sampling from the index)
ab-check --index index.json --sample 50 --features ruby,gaiji --adapter aozora2 --output checks/ --jobs 8 --per-work-timeout 60s

# Run on all works that have one or more features
ab-check --index index.json --features ruby --adapter aozora2 --output checks/

# Run on an explicit work-id sample generated by ab-index
ab-check --index index.json --work-ids sample.json --adapter aozora2 --output checks/
```

### Output (JSON)

```json
{
  "adapter": "aozora2",
  "adapter_version": "0.1.3",
  "work_id": "000148_799",
  "results": {
    "schema_valid": {"pass": true},
    "parse_completeness": {"pass": true},
    "visible_text_body_order": {"pass": true, "confidence": "heuristic"},
    "ruby_completeness": {
      "pass": false,
      "message": "Ruby marker on line 89 has no corresponding AAT ruby node",
      "line": 89,
      "confidence": "heuristic"
    },
    "gaiji_resolution": {"pass": true},
    "block_balance": {"pass": true}
  }
}
```

---

## 7. Tool: `ab-compare`

### Responsibility
Compare two AAT JSON files and report semantic differences.

### When to use
After `ab-check` passes for a parser, use `ab-compare` to see how its output differs from another parser on the same works. This is the cross-parser analysis layer.

### AAT JSON Schema (Wire Format)

The AAT JSON is intentionally small, but it is a semantic tree. It is not a complete spec implementation; it is a **normalization target** for property checks and parser comparison.

The top level contains a `blocks` array. A block contains either a flat `content` array of inline nodes or a nested `children` array of block nodes. Inline wrappers such as `style`, `font_size`, and `caption` may contain their own inline `content`; `ruby` uses `base` and `reading`; `warigaki` uses `upper` and `lower`. Parser event tokens such as `block_start` and `block_end` are never valid AAT nodes.

```json
{
  "version": 1,
  "work_id": "000148_799",
  "blocks": [
    {
      "kind": "paragraph",
      "span": {"line_start": 10, "line_end": 10, "byte_start": 184, "byte_end": 229},
      "content": [
        {
          "kind": "ruby",
          "base": "吾輩",
          "reading": "わがはい",
          "span": {"line_start": 10, "line_end": 10, "byte_start": 184, "byte_end": 208}
        },
        {"kind": "text", "value": "は猫である。", "span": {"line_start": 10, "line_end": 10, "byte_start": 208, "byte_end": 229}}
      ]
    },
    {
      "kind": "heading",
      "level": 1,
      "style": "normal",
      "span": {"line_start": 12, "line_end": 12, "byte_start": 240, "byte_end": 268},
      "content": [{"kind": "text", "value": "第一章", "span": {"line_start": 12, "line_end": 12, "byte_start": 240, "byte_end": 249}}]
    },
    {
      "kind": "jisage_block",
      "span": {"line_start": 20, "line_end": 24, "byte_start": 400, "byte_end": 512},
      "children": [
        {
          "kind": "paragraph",
          "span": {"line_start": 21, "line_end": 21, "byte_start": 428, "byte_end": 470},
          "content": [
            {"kind": "text", "value": "長さは", "span": {"line_start": 21, "line_end": 21, "byte_start": 428, "byte_end": 437}},
            {
              "kind": "gaiji",
              "description": "「口＋世」、U+546D",
              "resolved": "吋",
              "jis_code": null,
              "unresolved_reason": null,
              "span": {"line_start": 21, "line_end": 21, "byte_start": 437, "byte_end": 465}
            },
            {"kind": "text", "value": "で示す。", "span": {"line_start": 21, "line_end": 21, "byte_start": 465, "byte_end": 470}}
          ]
        }
      ]
    }
  ],
  "meta": {
    "adapter": "aozora2",
    "adapter_version": "0.1.3",
    "source_encoding": "windows-31j",
    "source_hash": "sha256:...",
    "parse_complete": true,
    "warnings": [
      {"line": 42, "message": "Heuristic ruby base detection (no ｜ prefix)"}
    ]
  }
}
```

**Design rules:**
- Every `kind` is a string from the canonical kind list. Unknown source markup must be emitted as `raw`; unknown AAT node kinds are schema errors.
- Blocks are nested with `children`; inline content is ordered with `content`.
- Block nodes must not contain both `content` and `children`; inline nodes must not contain `children`.
- Source `span` is optional only when the adapter cannot recover source locations. New native adapters should emit it for every block and inline node.
- Unknown markup is preserved inline as `{"kind": "raw", "source": "...", "span": {...}}` so nothing is silently dropped.
- Gaiji is emitted inline at the place where it appears. A later report layer may deduplicate gaiji references, but the AAT protocol does not use top-level gaiji annotations.
- Normalization rules are applied during adapter emission where they are parser semantics, and during comparison where they are comparison policy. Adapters must not erase information needed for diagnostics.

### Canonical AAT Kinds

| Category | Kinds |
|----------|-------|
| Block nodes | `paragraph`, `heading`, `jisage_block`, `quote_block`, `keigakomi_block`, `yokogumi_block`, `caption_block` |
| Inline leaf nodes | `text`, `ruby`, `gaiji`, `accent`, `raw` |
| Inline wrapper nodes | `style`, `font_size`, `tcy`, `yokogumi`, `caption`, `warigaki` |

Per-kind required fields:

| Kind | Required fields | Optional fields |
|------|-----------------|-----------------|
| `paragraph` | `kind`, `content` | `span` |
| `heading` | `kind`, `level`, `style`, `content` | `span` |
| `*_block` | `kind`, `children` | `span`, block-specific numeric parameters |
| `text` | `kind`, `value` | `span` |
| `ruby` | `kind`, `base`, `reading` | `span`, `direction` |
| `gaiji` | `kind`, `description`, `resolved`, `unresolved_reason` | `span`, `jis_code` |
| `accent` | `kind`, `code`, `name`, `resolved` | `span` |
| `raw` | `kind`, `source` | `span` |
| `style` | `kind`, `style_type`, `content` | `span`, `class_name` |
| `font_size` | `kind`, `size_type`, `level`, `content` | `span` |
| `tcy`, `yokogumi`, `caption` | `kind`, `content` | `span` |
| `warigaki` | `kind`, `upper`, `lower` | `span` |

The JSON Schema uses `additionalProperties: false` for node objects, with `patternProperties` allowing reserved extension keys that start with `x-`. Extension keys do not affect visible-text projection or semantic comparison unless a later schema version promotes them.

### Visible Text Projection

Every consumer uses the same AAT visible-text projection:

| Node kind | Projection |
|-----------|------------|
| `text` | `value` |
| `ruby` | `base`; the `reading` is annotation text and is not projected into main text |
| `gaiji` | `resolved` when present; otherwise `description` |
| `raw` | `source` |
| `style`, `font_size`, `tcy`, `yokogumi`, `caption` | Projection of child `content` |
| `warigaki` | Projection of `upper`, then `lower`, separated by parser-defined punctuation only if the adapter emits it as text |
| block nodes | Projection of `content` or `children` in order |

Adapters must not duplicate visible text. For example, `吾輩《わがはい》は猫である。` projects as `吾輩は猫である。`; the ruby base must not also appear in a sibling text node.

### Body Region Heuristics

`visible_text_body_order` and `no_dropped_lines` operate only on a decoded body region. Body-region detection is configured in `data/body-patterns.toml`:

- `end_colophon = '^底本[：:]'` ends the body before the first colophon line.
- `skip_preface_until` may define standard Aozora separator lines when present.
- If no reliable body region can be detected, these properties return `{"pass": null, "skipped": true, "reason": "body_region_not_detected"}`.

The MVP does not claim full text preservation for all works. It uses this property as an order/smoke signal on works with known boundaries, while ruby, gaiji, block, and heading properties provide feature-specific assertions.

### Block Marker Definitions

`block_balance` is driven by `data/block-patterns.toml`, not ad hoc regexes inside `ab-check`. The file contains named start/end pairs derived from `chuki_tag.txt`, for example:

```toml
[[blocks]]
name = "jisage_block"
start = '［＃ここから[０-９\\d]+字下げ］'
end = '［＃ここで字下げ終わり］'

[[blocks]]
name = "quote_block"
start = '［＃ここから引用］'
end = '［＃ここで引用終わり］'
```

The check reports the block `name`, start line, expected end pattern, and actual mismatched end when available.

### Comparison Rules

| Aspect | Rule |
|--------|------|
| Text content | Compare visible-text projections with NFKC normalization and whitespace collapsed to a single space |
| Ruby | Compare `base` + `reading` independently of how base was detected |
| Gaiji | Compare `resolved` codepoint; `null` matches `null` |
| Headings | Compare `level` + `style` + text content |
| Blocks | Compare `kind` + nested content; known layout-only wrapper differences may be downgraded to info |
| Unknown nodes | Always flagged as difference (parsers disagree on what it is) |
| Spans | Do not affect semantic equality, but include them in diagnostic output when present |

### Format And Performance Policy

JSON remains the canonical AAT interchange format for phase 1 because it is inspectable, language-neutral, easy to validate with JSON Schema, and usable by Rust, Python, JVM, and Node adapters without shared libraries. This is a deliberate interoperability choice, not a claim that JSON is permanently optimal.

Performance decisions are benchmark-driven:

- Keep JSON as the report format even if a faster internal cache format is added.
- Measure adapter runtime, JSON serialization, JSON parsing, schema validation, property checks, and filesystem traversal separately.
- If JSON serialization/parsing plus schema validation exceeds 15% of end-to-end batch runtime on a representative corpus sample, add a measured alternative such as JSON Lines batching, zstd-compressed JSON cache files, CBOR, MessagePack, or an `rkyv` sidecar cache. The alternative must be optional and reproducible from canonical JSON.
- Do not replace the adapter protocol until at least two adapters demonstrate the same bottleneck. Parser correctness and diagnostic clarity remain higher priority than speculative throughput.

### CLI

```bash
# Compare two AAT files for the same work
ab-compare --aat1 work.aozora2.json --aat2 work.aozora-rs.json --output diff.json

# Batch compare
ab-compare --batch sample.json --adapter1 aozora2 --adapter2 aozora-rs --output diffs/
```

### Output

```json
{
  "work_id": "000148_799",
  "parser_a": "aozora2",
  "parser_b": "aozora-rs",
  "differences": [
    {
      "path": "blocks[0].content[1]",
      "type": "ruby_base_detection",
      "a": {"kind": "ruby", "base": "吾輩", "reading": "わがはい"},
      "b": {"kind": "ruby", "base": "吾", "reading": "わがはい"}
    },
    {
      "path": "blocks[3]",
      "type": "missing_block",
      "a": {"kind": "heading", "level": 1, ...},
      "b": null
    }
  ]
}
```

---

## 8. Tool: `ab-render-diff`

### Responsibility
Normalize and compare rendered HTML against official XHTML.

### Design
- Does **not** use AAT. Works directly on HTML strings.
- The official XHTML is one reference point. The parser's output is normalized and diffed against it.
- Divergences are classified by severity.

### Normalization Pipeline

1. Parse HTML/XHTML into a DOM tree (`tl` or `html5ever`).
2. Preserve original attributes for classification, then build a canonical comparison view that removes non-semantic attributes only after recording them.
3. Normalize whitespace: collapse consecutive whitespace to single space; trim inside block elements.
4. Expand well-formed ruby to canonical form: `<ruby>base<rt>reading</rt></ruby>`. Malformed ruby is flagged as a warning, not silently fixed.
5. Resolve relative image paths to absolute (using work directory).
6. Sort attributes alphabetically.
7. Serialize back to a canonical HTML string.

### Divergence Classification

| Severity | Condition |
|----------|-----------|
| **Critical** | Missing text content, missing image, wrong heading level, unbalanced tags, ruby base/reading mismatch, gaiji rendered as `?` or omitted |
| **Warning** | Different element type for same semantic role, missing alt text, different gaiji resolution strategy (image vs. character) |
| **Info** | CSS class differences, extra wrapper `<div>`, whitespace inside inline elements |

### CLI

```bash
# Compare parser HTML against official XHTML for one work
ab-render-diff --work work.txt --html work.html --official official.html --output diff.json

# Batch
ab-render-diff --batch sample.json --adapter aozora2 --corpus /path/to/aozorabunko --output render-diffs/
```

---

## 9. Tool: `ab-report`

### Responsibility
Aggregate outputs from other tools, generate human-readable reports, and support run-to-run comparison.

### CLI

```bash
# Generate summary report from a run
ab-report --checks checks/ --diffs diffs/ --render-diffs render-diffs/ --output report.md

# Compare two runs
ab-report diff-runs --run1 reports/2026-04-25/ --run2 reports/2026-05-25/ --output diff-report.md
```

### Report Structure

```
reports/
├── run.json                    # Metadata: timestamp, adapter versions, corpus hash
├── index.json -> ../index.json # Symlink to corpus index
├── checks/
│   ├── summary.md              # Pass/fail per property per adapter
│   └── per-work/
│       └── 000148_799.json
├── diffs/
│   ├── summary.md              # Per-feature disagreement rates
│   └── per-work/
│       └── 000148_799.json
├── render-diffs/
│   ├── summary.md
│   └── per-work/
│       └── 000148_799.json
└── cross-analysis.md           # "Parser X fails feature Y, which correlates with Z divergences"
```

### Reproducibility

Every report includes:
```json
{
  "run_id": "2026-04-25T18:00:00Z",
  "corpus_hash": "sha256:...",
  "adapters": {
    "aozora2": {"version": "0.1.3", "binary_hash": "sha256:..."},
    "aozora-rs": {"version": "0.2.1", "binary_hash": "sha256:..."},
    "aozora-epub3": {"version": "1.3.0", "jar_hash": "sha256:..."}
  }
}
```

### Deterministic Output

All machine-readable outputs use deterministic JSON:

- Use stable structs or `BTreeMap` for object fields whose order is not fixed by a Rust struct.
- Serialize with `serde_json::to_writer_pretty`.
- Sort arrays that represent sets, including `features`, `by_feature` work IDs, difference lists with equal severity, and aggregated report entries.
- When a CLI `--output` path is a directory, use stable per-work filenames:
  - `ab-check`: `checks/{adapter}/{work_id}.json`
  - `ab-compare`: `diffs/{adapter_a}_vs_{adapter_b}/{work_id}.json`
  - `ab-render-diff`: `render-diffs/{adapter}/{work_id}.json`

These conventions keep run-to-run diffs meaningful.

---

## 10. Benchmarking

### Responsibility
Provide repeatable measurements so format, parser, and concurrency decisions are made from data rather than guesses.

### Benchmark Scope

| Benchmark | Measures |
|-----------|----------|
| `index_full` | Full-corpus traversal, path normalization, feature regex detection, and `corpus_hash` computation |
| `adapter_single` | One adapter parsing one representative work and emitting AAT JSON |
| `adapter_batch` | Bounded parallel adapter execution across a sampled set |
| `aat_json_io` | AAT JSON serialization, parsing, and schema validation cost independent of parser cost |
| `check_properties` | `ab-check` property traversal and source matching after AAT is already loaded |
| `render_diff` | HTML parse, normalization, and diff classification |

### Benchmark Inputs

- Tiny fixtures for regression-level benches.
- A stable 100-work mixed-feature sample from the index, pinned to the `corpus_hash` it was generated from.
- A stable 1,000-work stress sample before evaluating protocol or concurrency changes, pinned to the `corpus_hash` it was generated from.
- Benchmark commands fail fast when the current index `corpus_hash` does not match the sample file's pinned `corpus_hash`.

### Output

Benchmarks write machine-readable JSON plus a short Markdown summary:

```json
{
  "run_id": "2026-04-25T18:00:00Z",
  "corpus_hash": "sha256:...",
  "git_rev": "...",
  "benchmarks": {
    "aat_json_io": {"p50_ms": 1.7, "p95_ms": 4.3},
    "adapter_batch": {"works": 100, "jobs": 8, "total_s": 18.2}
  }
}
```

### Decision Rules

- Treat the first benchmark run as a baseline, not a pass/fail gate.
- A proposed faster format or cache must include before/after benchmark data on the same sample.
- Keep the simplest protocol that meets the success criteria. Add binary or compressed sidecars only when measurements show JSON overhead is material.

---

## 11. Adapter Specification

Adapters are **external programs** that read a `.txt` file and emit AAT JSON to stdout. They are not Rust crates in the workspace.

### Interface

```bash
adapter --mode aat < input.txt > output.aat.json
adapter --mode html < input.txt > output.html
adapter --version
```

The adapter is responsible for:
1. Running the parser.
2. Decoding stdin bytes with the shared source encoding contract.
3. Normalizing the output to AAT JSON schema.
4. Folding parser event streams into nested AAT blocks when the parser exposes start/end block events.
5. Writing valid JSON or HTML to stdout.
6. Exiting with the specified code on parse failure or partial output.

### Built-in Adapters

| Adapter | Language | Modes | Notes |
|---------|----------|-------|-------|
| `aozora2` | Rust | `--mode aat`, `--mode html` | Links `aozora-core` directly |
| `aozora-rs` | Rust | `--mode aat`, `--mode html` | Links `aozora-rs-core` |
| `aozora-epub3` | Python/Shell | `--mode aat`, `--mode html` | Runs `AozoraEpub3.jar`; `--mode aat` parses XHTML back to AAT |
| `aozora-parser-js` | Node.js | `--mode aat` only | PEG.js parser produces no HTML renderer |

### Adapter Interface

Adapters are executables named `<name>-adapter` discovered on `AB_ADAPTER_PATH` or `$PATH`.

```bash
# Naming convention: aozora2-adapter, test-adapter, etc.
# Mode 1: emit AAT JSON (for ab-check, ab-compare)
aozora2-adapter --mode aat < input.txt > output.aat.json

# Mode 2: emit rendered HTML (for ab-render-diff)
aozora2-adapter --mode html < input.txt > output.html

# Exit codes:
#   0 = success
#   1 = fatal error (can't parse at all)
#   2 = partial success (output produced, but meta.parse_complete = false)
```

`--version` prints one line:

```text
<adapter-name> <adapter-version> <git-rev-or-build-id>
```

`ab-check` records this value before batch execution. Exit-code handling is fixed:

| Exit code | stdout | `ab-check` behavior |
|-----------|--------|---------------------|
| `0` | Complete AAT JSON | Validate schema and run all properties |
| `1` | Ignored | Record `fatal_error` with stderr and skip schema/property checks |
| `2` | Partial AAT JSON | Validate schema and run all properties; `parse_completeness` should fail because `meta.parse_complete` is false |
| other | Ignored | Record `adapter_protocol_error` and skip schema/property checks |

Adapter subprocesses are killed after the caller's per-work timeout. Timeout results are recorded as `adapter_timeout` and semantic properties are skipped.

### Adapter Discovery

Adapters are discovered via:
1. `AB_ADAPTER_PATH` environment variable (colon-separated list of directories)
2. Fallback: `$PATH`
3. Each tool takes `--adapter-dir` to override
4. Binary name: `<name>-adapter` (e.g., `aozora2-adapter`, `test-adapter`)

### Adapter Version Pinning

Each adapter directory contains a lockfile or checksum file (e.g., `Cargo.lock`, `package-lock.json`, JAR checksum) that is recorded in every report. Git dependencies must be pinned by `rev` or recorded through a committed `Cargo.lock`. Reproducibility is enforced: if the adapter binary changes, the report metadata reflects it.

---

## 12. Dependencies

| Crate | Purpose |
|-------|---------|
| `clap` | CLI for all tools |
| `serde` + `serde_json` | JSON I/O |
| `jsonschema` | AAT schema validation before semantic checks |
| `regex` | Feature detection |
| `criterion` | Repeatable microbenchmarks and throughput benchmarks |
| `encoding_rs` | UTF-8 / Windows-31J source decoding |
| `tl` | Default HTML parsing |
| `scraper` | HTML DOM traversal only if selector-heavy normalization becomes necessary |
| `sha2` | Content hashing |
| `rayon` | Parallel processing for CPU-bound tools (`ab-index`, `ab-check`) |
| `similar` | Text diff |
| `chrono` | Timestamps |
| `anyhow` | Error handling |

---

## 13. Implementation Order

| Phase | Tool | Goal |
|-------|------|------|
| 1 | `ab-index` | Can build feature index for full corpus in < 5 min |
| 2 | `ab-check` + test adapter | Can validate schema/property pipeline on fixtures |
| 3 | `aozora2` adapter + `ab-check` | Can validate aozora2's structural correctness on sampled works |
| 4 | Benchmarks | Can measure index, adapter, AAT JSON I/O, schema validation, and `ab-check` costs |
| 5 | `ab-render-diff` | Can diff aozora2's HTML against official XHTML |
| 6 | `aozora-rs` adapter + `ab-check` | Can validate aozora-rs |
| 7 | `ab-render-diff` for aozora-rs | Can diff aozora-rs's HTML against official XHTML |
| 8 | `ab-compare` | Can compare aozora2 vs. aozora-rs AAT output (both validated) |
| 9 | `ab-report` | Can aggregate all outputs into cross-parser reports |
| 10 | `aozora-parser-js` adapter | Can evaluate the legacy PEG.js parser |

---

## 14. Success Criteria

1. `ab-index` builds a feature index for the full corpus in < 5 minutes.
2. `ab-check` runs property tests on a 100-work sample in < 1 minute with bounded adapter parallelism.
3. Adapters emit schema-valid nested AAT with no `block_start` or `block_end` event nodes.
4. Benchmarks report separate timings for parser execution, JSON I/O, schema validation, property checks, and filesystem traversal.
5. `ab-render-diff` produces a divergence report with severity classification.
6. `ab-compare` detects semantic differences between two parsers' AAT outputs.
7. `ab-report` aggregates all outputs and supports run-to-run comparison.
8. Every report is reproducible: adapter versions and corpus hashes are recorded.

---

The implementation plan must stay aligned with the AAT protocol in this spec: nested block objects, flat inline content arrays, inline gaiji, visible-text projection rules, and optional source spans on emitted nodes.
