# Aozora Bunko Parser Validation Harness — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build `ab-validator` — a suite of standalone tools for evaluating Aozora Bunko parsers against the official corpus.

**Architecture:** Five independent Rust binaries (`ab-index`, `ab-check`, `ab-compare`, `ab-render-diff`, `ab-report`) plus external adapter programs. JSON AAT is the canonical wire format: nested blocks, flat inline content arrays, inline gaiji, optional source spans, and no block start/end event nodes. Benchmarks decide whether to add optional cache or sidecar formats later. Start with `ab-index`, `ab-check`, `aozora2`, and performance baselines for one parser, then iterate.

**Tech Stack:** Rust 2024 edition, `serde`/`serde_json`, `jsonschema`, `regex`, `clap`, `anyhow`, `sha2`, `chrono`, `rayon` (for CPU-bound work), bounded subprocess worker pools for adapters, `criterion` (benchmarks), `tl` (HTML parsing).

---

## File Structure

```
ab-validator/
├── Cargo.toml                    # Workspace definition
├── Cargo.lock
├── README.md
│
├── crates/
│   ├── ab-index/                 # Corpus feature indexer
│   │   ├── Cargo.toml
│   │   └── src/
│   │       ├── main.rs           # CLI + orchestration
│   │       ├── encoding.rs       # Source byte decoding to UTF-8
│   │       ├── features.rs       # FeatureDetector: from_toml, detect
│   │       └── index.rs          # CorpusIndex struct + I/O
│   │   └── tests/fixtures/corpus/
│   │
│   ├── ab-check/                 # Property-based invariant testing
│   │   ├── Cargo.toml
│   │   └── src/
│   │       ├── main.rs           # CLI
│   │       ├── encoding.rs       # Source byte decoding to UTF-8
│   │       ├── properties.rs     # Property trait + implementations
│   │       ├── aat.rs            # AAT traversal + visible-text projection helpers
│   │       └── check.rs          # Run properties, produce report
│   │   └── benches/
│   │       ├── aat_json_io.rs    # JSON parse + schema validation benchmarks
│   │       └── check_properties.rs
│   │
│   ├── ab-render-diff/           # HTML normalization + diff (future)
│   ├── ab-compare/               # AAT JSON diff (future)
│   └── ab-report/                # Report aggregation (future)
│
├── adapters/
│   ├── test-adapter/             # Hard-coded AAT for pipeline validation
│   │   └── test-adapter
│   └── aozora2/                  # aozora-core adapter
│       ├── Cargo.toml
│       ├── src/main.rs           # Links aozora-core, emits AAT JSON
│       └── benches/adapter_bench.rs
│
├── benchmarks/
│   ├── README.md                 # How to run and interpret benchmarks
│   └── samples/
│       ├── mixed-100.json        # Stable sample work IDs from index
│       └── stress-1000.json      # Larger sample for format/concurrency decisions
│
└── data/
    ├── feature-patterns.toml     # Regex patterns for feature detection
    ├── block-patterns.toml       # Block start/end marker definitions
    ├── body-patterns.toml        # Body/colophon boundary heuristics
    └── aat-schema.json           # JSON Schema for AAT wire format

.github/
└── workflows/ci.yml              # fmt, clippy, test
```

---

## Task 1: Workspace Skeleton

**Files:**
- Create: `ab-validator/Cargo.toml`
- Create: `ab-validator/README.md`
- Create: `ab-validator/data/feature-patterns.toml`
- Create: `ab-validator/data/block-patterns.toml`
- Create: `ab-validator/data/body-patterns.toml`
- Create: `ab-validator/.github/workflows/ci.yml`

**Interface contracts:**
- Workspace uses Rust 2024 edition.
- All crates share workspace dependencies via `workspace = true`.

**Integration test:**
- `cargo build` succeeds with no errors.

- [ ] **Step 1: Create workspace root `Cargo.toml`**

```toml
[workspace]
members = [
    "crates/ab-index",
    "crates/ab-check",
]
resolver = "2"

[workspace.package]
version = "0.1.0"
edition = "2024"
license = "MIT OR Apache-2.0"

[workspace.dependencies]
anyhow = "1.0"
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
clap = { version = "4.5", features = ["derive"] }
regex = "1.10"
sha2 = "0.10"
encoding_rs = "0.8"
chrono = { version = "0.4", features = ["serde"] }
tl = "0.7"
rayon = "1.10"
walkdir = "2.5"
toml = "0.8"
unicode-normalization = "0.1"
jsonschema = "0.46"
criterion = "0.8" # use as dev-dependency in crates that define benches
```

- [ ] **Step 2: Create `data/feature-patterns.toml`**

Derive patterns from `references/parsers/AozoraEpub3-JDK21/chuki_tag.txt` and the PARSER_REPORT feature taxonomy. Each feature needs:
- A unique name (e.g., `ruby`, `boten`, `jisage_line`)
- A regex pattern that matches the markup in `.txt` files
- A human-readable description

Start with the ~35 patterns from the PARSER_REPORT's feature table (L1–L13, H1–H9, E1–E7, K1–K3, M1–M20, G1–G4, O1–O29, grouped where parsers agree on sub-variants).

- [ ] **Step 3: Create `data/block-patterns.toml` and `data/body-patterns.toml`**

`data/block-patterns.toml` contains named source-side block pairs:

```toml
[[blocks]]
name = "jisage_block"
start = '［＃ここから[０-９\d]+字下げ］'
end = '［＃ここで字下げ終わり］'

[[blocks]]
name = "quote_block"
start = '［＃ここから引用］'
end = '［＃ここで引用終わり］'
```

`data/body-patterns.toml` contains body-boundary heuristics:

```toml
[body]
end_colophon = '^底本[：:]'
separator = '^-{20,}$'
```

- [ ] **Step 4: Create CI workflow**

`.github/workflows/ci.yml`:

```yaml
name: CI

on:
  push:
  pull_request:

jobs:
  rust:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      - run: cd ab-validator && cargo fmt --check
      - run: cd ab-validator && cargo clippy --all-targets -- -D warnings
      - run: cd ab-validator && cargo test
```

- [ ] **Step 5: Integration test — build succeeds**

```bash
cd ab-validator
cargo build
```
Expected: `Finished dev [unoptimized + debuginfo] target(s)`

- [ ] **Step 6: Commit**

```bash
git add .
git commit -m "feat: workspace skeleton + feature patterns"
```

---

## Task 2: `ab-index` — Corpus Feature Indexer

**Files:**
- Create: `ab-validator/crates/ab-index/Cargo.toml`
- Create: `ab-validator/crates/ab-index/src/encoding.rs`
- Create: `ab-validator/crates/ab-index/src/features.rs`
- Create: `ab-validator/crates/ab-index/src/index.rs`
- Create: `ab-validator/crates/ab-index/src/main.rs`
- Create: `ab-validator/crates/ab-index/tests/fixtures/corpus/cards/000001/files/1_ruby/test.txt`
- Create: `ab-validator/crates/ab-index/tests/fixtures/corpus/cards/000002/files/2_gaiji/test.txt`
- Create: `ab-validator/crates/ab-index/tests/fixtures/corpus/cards/000003/files/3_both/test.txt`

**Crate manifest:**

`crates/ab-index/Cargo.toml`:

```toml
[package]
name = "ab-index"
version.workspace = true
edition.workspace = true
license.workspace = true

[dependencies]
anyhow.workspace = true
chrono.workspace = true
clap.workspace = true
encoding_rs.workspace = true
rayon.workspace = true
regex.workspace = true
serde.workspace = true
serde_json.workspace = true
sha2.workspace = true
toml.workspace = true
walkdir.workspace = true
```

**Interface contracts:**

`FeatureDetector` (in `features.rs`):
```rust
impl FeatureDetector {
    fn from_toml(path: &Path) -> Result<Self>;           // Compile regexes from TOML
    fn detect(&self, text: &str) -> HashMap<String, Vec<usize>>;  // feature → line numbers
    fn feature_names(&self) -> Vec<&str>;
}

fn normalize_relative_path(path: &Path) -> String;       // Always returns POSIX-style separators
```

`encoding.rs`:
```rust
struct DecodedSource {
    text: String,
    encoding: String, // "utf-8", "utf-8-bom", or "windows-31j"
    raw_sha256: String,
}

fn decode_source_bytes(bytes: &[u8]) -> Result<DecodedSource>;
```

`CorpusIndex` (in `index.rs`):
```rust
struct Index {
    version: u32,                    // Always 1
    corpus_root: String,
    corpus_hash: String,             // SHA256 over sorted path/size/content-hash tuples
    generated_at: String,            // RFC3339
    works_count: usize,
    works: Vec<WorkEntry>,
    by_feature: HashMap<String, Vec<String>>,  // feature → work_ids
}

struct WorkEntry {
    id: String,                      // "{card}_{file}" (e.g., "000148_799")
    txt_path: String,                // Relative to corpus_root
    html_path: Option<String>,       // Relative to corpus_root, null if absent
    features: Vec<String>,           // Feature names present in this work
    feature_lines: HashMap<String, Vec<usize>>,  // feature → 1-indexed line numbers
}
```

**Work ID convention:** Parse card number and file number from the Aozora Bunko path structure: `cards/{card}/files/{file}/...`. If parsing fails, fall back to sanitized filename.

**Implementation notes:**
- Use `walkdir` for filesystem traversal.
- Use `rayon` for parallel feature detection (CPU-bound regex matching).
- Skip non-work `.txt` files (README, dotfiles).
- Decode source bytes before regex matching: UTF-8 BOM, valid UTF-8, then Windows-31J fallback via `encoding_rs`.
- Store all paths relative to `corpus_root` with `/` separators, even on Windows.
- `corpus_hash`: SHA256 of the sorted list of `(relative_posix_path, file_size, file_sha256)` tuples. Do not include absolute paths, platform separators, or mtimes.
- The exact byte stream for `corpus_hash` is `relative_posix_path`, NUL, decimal `file_size`, NUL, lowercase hex `file_sha256`, newline for each sorted file entry.
- Compute `file_sha256` in the same rayon per-file pass that reads file contents for regex detection. Do not add a second full-corpus traversal just to compute `corpus_hash`.
- A local cache may use mtimes for speed, but that cache key must be separate from `corpus_hash` and must not appear in reproducibility reports.
- Treat feature detection as a high-recall sampling index, not a parser. Regex patterns may over-report malformed/literal markup and may under-report legacy notation. Preserve `feature_lines` so later diagnostics can point to likely source locations.

**CLI:**
```bash
ab-index --corpus /path/to/aozorabunko --output index.json
ab-index --query ruby --index index.json
ab-index --query-all ruby,gaiji --index index.json

# Sample for human inspection, benchmark samples, and --work-ids consumers
ab-index --sample 50 --features ruby,gaiji --index index.json --output sample.json
```

`--query`, `--query-all`, and `--sample` write JSON arrays of work IDs to stdout unless `--output` is provided.

**Integration test:**
- Create the checked-in test corpus under `crates/ab-index/tests/fixtures/corpus/` with 3 works: one with ruby, one with gaiji, one with both.
- Include one Windows-31J encoded fixture and assert feature detection still works after decoding.
- Run `ab-index`, assert index contains correct features per work.
- Assert every emitted `txt_path` and `html_path` uses `/` separators and is relative to the corpus root.
- Assert `corpus_hash` is unchanged when the same fixture paths are represented with platform-native separators before normalization.
- Run `--query ruby`, assert it returns the right work IDs.
- Run `--sample 1 --features ruby,gaiji`, assert it returns both works.

- [ ] **Step 1: Create crate structure**
- [ ] **Step 2: Implement FeatureDetector**
- [ ] **Step 3: Implement CorpusIndex builder**
- [ ] **Step 4: Implement CLI**
- [ ] **Step 5: Integration test with fixture corpus**
- [ ] **Step 6: Commit**

```bash
git add crates/ab-index
git commit -m "feat: ab-index corpus feature indexer"
```

---

## Task 3: AAT JSON Schema

**Files:**
- Create: `ab-validator/data/aat-schema.json`
- Create: `ab-validator/data/fixtures/aat-valid-nested.json`
- Create: `ab-validator/data/fixtures/aat-invalid-event-node.json`

**Interface contract:**

AAT JSON is the wire format between adapters and tools. Every adapter emits this shape.

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

**Rules:**
- `kind` is always a string. New kinds can be added without breaking consumers.
- Canonical block kinds: `paragraph`, `heading`, `jisage_block`, `quote_block`, `keigakomi_block`, `yokogumi_block`, `caption_block`.
- Canonical inline leaf kinds: `text`, `ruby`, `gaiji`, `accent`, `raw`.
- Canonical inline wrapper kinds: `style`, `font_size`, `tcy`, `yokogumi`, `caption`, `warigaki`.
- Blocks are nested with `children`; inline content is a flat ordered `content` array.
- Block nodes must not contain both `content` and `children`.
- Inline nodes must not contain `children`.
- Adapter event tokens such as `block_start` and `block_end` are invalid AAT output.
- Gaiji is emitted inline where it occurs, not in a top-level annotations array.
- Ruby contributes its `base` to visible-text projection; the base must not be duplicated in sibling text nodes.
- `span` is optional for compatibility, but native adapters should emit it on every block and inline node.
- `span.line_start` and `span.line_end` are decoded UTF-8 line numbers; `span.byte_start` and `span.byte_end` are UTF-8 byte offsets in the decoded source text.
- `meta.source_hash` hashes raw source bytes; `meta.source_encoding` records the decoder used for parser input.
- `meta.parse_complete: false` means the adapter hit an unrecoverable error but produced partial output.
- `meta.warnings` are advisory; they do not affect `parse_complete`.
- Unknown markup is preserved as `{"kind": "raw", "source": "...", "span": {...}}`.
- Unknown AAT node kinds are schema errors. Future non-semantic extensions use keys beginning with `x-`.

**Schema file content:**

`data/aat-schema.json` must encode the canonical kind list and per-kind required fields:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://abc.local/schemas/aat-v1.json",
  "type": "object",
  "required": ["version", "work_id", "blocks", "meta"],
  "additionalProperties": false,
  "properties": {
    "version": {"const": 1},
    "work_id": {"type": "string"},
    "blocks": {"type": "array", "items": {"$ref": "#/$defs/block"}},
    "meta": {"$ref": "#/$defs/meta"}
  },
  "$defs": {
    "span": {
      "type": "object",
      "required": ["line_start", "line_end", "byte_start", "byte_end"],
      "additionalProperties": false,
      "properties": {
        "line_start": {"type": "integer", "minimum": 1},
        "line_end": {"type": "integer", "minimum": 1},
        "byte_start": {"type": "integer", "minimum": 0},
        "byte_end": {"type": "integer", "minimum": 0}
      }
    },
    "meta": {
      "type": "object",
      "required": ["adapter", "adapter_version", "source_encoding", "source_hash", "parse_complete", "warnings"],
      "additionalProperties": false,
      "properties": {
        "adapter": {"type": "string"},
        "adapter_version": {"type": "string"},
        "source_encoding": {"enum": ["utf-8", "utf-8-bom", "windows-31j"]},
        "source_hash": {"type": "string", "pattern": "^sha256:[0-9a-f]{64}$"},
        "parse_complete": {"type": "boolean"},
        "warnings": {"type": "array", "items": {"$ref": "#/$defs/warning"}}
      }
    },
    "warning": {
      "type": "object",
      "required": ["message"],
      "additionalProperties": false,
      "properties": {
        "line": {"type": "integer", "minimum": 1},
        "path": {"type": "string"},
        "message": {"type": "string"}
      }
    },
    "extension": {"patternProperties": {"^x-": true}},
    "block": {
      "oneOf": [
        {"$ref": "#/$defs/paragraph"},
        {"$ref": "#/$defs/heading"},
        {"$ref": "#/$defs/block_container"}
      ]
    },
    "inline": {
      "oneOf": [
        {"$ref": "#/$defs/text"},
        {"$ref": "#/$defs/ruby"},
        {"$ref": "#/$defs/gaiji"},
        {"$ref": "#/$defs/accent"},
        {"$ref": "#/$defs/raw"},
        {"$ref": "#/$defs/inline_container"},
        {"$ref": "#/$defs/warigaki"}
      ]
    },
    "paragraph": {
      "type": "object",
      "required": ["kind", "content"],
      "additionalProperties": false,
      "patternProperties": {"^x-": true},
      "properties": {
        "kind": {"const": "paragraph"},
        "span": {"$ref": "#/$defs/span"},
        "content": {"type": "array", "items": {"$ref": "#/$defs/inline"}}
      }
    },
    "heading": {
      "type": "object",
      "required": ["kind", "level", "style", "content"],
      "additionalProperties": false,
      "patternProperties": {"^x-": true},
      "properties": {
        "kind": {"const": "heading"},
        "level": {"type": "integer", "minimum": 1, "maximum": 3},
        "style": {"type": "string"},
        "span": {"$ref": "#/$defs/span"},
        "content": {"type": "array", "items": {"$ref": "#/$defs/inline"}}
      }
    },
    "block_container": {
      "type": "object",
      "required": ["kind", "children"],
      "additionalProperties": false,
      "patternProperties": {"^x-": true},
      "properties": {
        "kind": {"enum": ["jisage_block", "quote_block", "keigakomi_block", "yokogumi_block", "caption_block"]},
        "span": {"$ref": "#/$defs/span"},
        "children": {"type": "array", "items": {"$ref": "#/$defs/block"}}
      }
    },
    "text": {
      "type": "object",
      "required": ["kind", "value"],
      "additionalProperties": false,
      "patternProperties": {"^x-": true},
      "properties": {"kind": {"const": "text"}, "value": {"type": "string"}, "span": {"$ref": "#/$defs/span"}}
    },
    "ruby": {
      "type": "object",
      "required": ["kind", "base", "reading"],
      "additionalProperties": false,
      "patternProperties": {"^x-": true},
      "properties": {"kind": {"const": "ruby"}, "base": {"type": "string"}, "reading": {"type": "string"}, "direction": {"enum": ["right", "left"]}, "span": {"$ref": "#/$defs/span"}}
    },
    "gaiji": {
      "type": "object",
      "required": ["kind", "description", "resolved", "unresolved_reason"],
      "additionalProperties": false,
      "patternProperties": {"^x-": true},
      "properties": {
        "kind": {"const": "gaiji"},
        "description": {"type": "string"},
        "resolved": {"type": ["string", "null"]},
        "jis_code": {"type": ["string", "null"]},
        "unresolved_reason": {"type": ["string", "null"]},
        "span": {"$ref": "#/$defs/span"}
      }
    },
    "accent": {
      "type": "object",
      "required": ["kind", "code", "name", "resolved"],
      "additionalProperties": false,
      "patternProperties": {"^x-": true},
      "properties": {"kind": {"const": "accent"}, "code": {"type": "string"}, "name": {"type": "string"}, "resolved": {"type": ["string", "null"]}, "span": {"$ref": "#/$defs/span"}}
    },
    "raw": {
      "type": "object",
      "required": ["kind", "source"],
      "additionalProperties": false,
      "patternProperties": {"^x-": true},
      "properties": {"kind": {"const": "raw"}, "source": {"type": "string"}, "span": {"$ref": "#/$defs/span"}}
    },
    "inline_container": {
      "type": "object",
      "required": ["kind", "content"],
      "additionalProperties": false,
      "patternProperties": {"^x-": true},
      "properties": {
        "kind": {"enum": ["style", "font_size", "tcy", "yokogumi", "caption"]},
        "style_type": {"type": "string"},
        "size_type": {"type": "string"},
        "level": {"type": "integer"},
        "class_name": {"type": "string"},
        "span": {"$ref": "#/$defs/span"},
        "content": {"type": "array", "items": {"$ref": "#/$defs/inline"}}
      }
    },
    "warigaki": {
      "type": "object",
      "required": ["kind", "upper", "lower"],
      "additionalProperties": false,
      "patternProperties": {"^x-": true},
      "properties": {
        "kind": {"const": "warigaki"},
        "span": {"$ref": "#/$defs/span"},
        "upper": {"type": "array", "items": {"$ref": "#/$defs/inline"}},
        "lower": {"type": "array", "items": {"$ref": "#/$defs/inline"}}
      }
    }
  }
}
```

**Integration test:**
- Validate the schema file is valid JSON.
- Validate a fixture with nested blocks, ruby, inline gaiji, raw nodes, and spans against the schema.
- Validate that a fixture containing `{"kind": "block_start"}` fails schema validation.

- [ ] **Step 1: Write schema file**
- [ ] **Step 2: Commit**

```bash
git add data/aat-schema.json
git commit -m "feat: AAT JSON schema definition"
```

---

## Task 4: Test Adapter

**Files:**
- Create: `ab-validator/adapters/test-adapter/test-adapter` (executable shell script)

**Purpose:** Hard-coded adapter that produces a known-good AAT JSON for a known input. Used to validate the `ab-index → adapter → ab-check` pipeline before any real parser is integrated.

**Interface contract:**
```bash
# Naming convention: <name>-adapter binary on $PATH or AB_ADAPTER_PATH
test-adapter --mode aat < input.txt > output.aat.json
test-adapter --mode html < input.txt > output.html
test-adapter --version
```

**Behavior:**
- Reads `input.txt`.
- Decodes stdin bytes using the shared source encoding contract.
- If the content contains real Aozora ruby markup (`《》`), emits an AAT with a `ruby` node whose `base` contributes to visible text and is not duplicated as sibling text.
- If the content contains real Aozora gaiji markup (`※［＃…］`), emits an inline `gaiji` node.
- Otherwise emits a minimal AAT with one text block.
- Always sets `meta.parse_complete: true`, `meta.adapter: "test-adapter"`, `meta.source_encoding`, and `meta.source_hash`.
- `--version` prints `test-adapter 0.0.1 test`.
- Exit code 0 means success; exit code 1 means fatal adapter error; exit code 2 means partial AAT was emitted with `meta.parse_complete: false`.
- The adapter is a shell script named `test-adapter` (executable, on PATH or invoked by explicit path).

**Integration test:**
```bash
echo "吾輩《わがはい》は猫である。" | ./adapters/test-adapter/test-adapter --mode aat > /tmp/test.aat.json
jq '.blocks[0].content[0].kind' /tmp/test.aat.json  # should output "ruby"
jq -r '[.blocks[0].content[] | if .kind == "ruby" then .base else .value end] | join("")' /tmp/test.aat.json  # should output "吾輩は猫である。"
```

- [ ] **Step 1: Write test-adapter executable shell script**

Make it executable: `chmod +x adapters/test-adapter/test-adapter`
- [ ] **Step 2: Integration test — verify output shape**
- [ ] **Step 3: Commit**

```bash
git add adapters/test-adapter
git commit -m "feat: test-adapter for pipeline validation"
```

---

## Task 5: `ab-check` — Property-Based Invariant Testing

**Files:**
- Create: `ab-validator/crates/ab-check/Cargo.toml`
- Create: `ab-validator/crates/ab-check/src/encoding.rs`
- Create: `ab-validator/crates/ab-check/src/properties.rs`
- Create: `ab-validator/crates/ab-check/src/check.rs`
- Create: `ab-validator/crates/ab-check/src/aat.rs`
- Create: `ab-validator/crates/ab-check/src/main.rs`
- Create: `ab-validator/crates/ab-check/fixtures/test_ruby.txt`
- Create: `ab-validator/crates/ab-check/fixtures/test_plain.txt`
- Create: `ab-validator/crates/ab-check/fixtures/test_bad_duplicate_ruby.aat.json`

**Crate manifest:**

`crates/ab-check/Cargo.toml`:

```toml
[package]
name = "ab-check"
version.workspace = true
edition.workspace = true
license.workspace = true

[dependencies]
anyhow.workspace = true
clap.workspace = true
encoding_rs.workspace = true
jsonschema.workspace = true
rayon.workspace = true
regex.workspace = true
serde.workspace = true
serde_json.workspace = true
unicode-normalization.workspace = true

[dev-dependencies]
criterion.workspace = true

[[bench]]
name = "aat_json_io"
harness = false

[[bench]]
name = "check_properties"
harness = false
```

**Interface contracts:**

`Property` trait (in `properties.rs`):
```rust
pub trait Property {
    fn name(&self) -> &'static str;
    fn check(&self, txt: &str, aat: &Value) -> Result<(), PropertyViolation>;
}

pub struct PropertyViolation {
    pub property: &'static str,
    pub message: String,
    pub line: Option<usize>,
    pub path: Option<String>,
    pub confidence: &'static str, // "strict" or "heuristic"
}
```

`aat.rs` helper contract:
```rust
enum VisibleFragment<'a> {
    Text { value: String, path: String, node: &'a Value },
    Gaiji { resolved: Option<String>, description: String, path: String, node: &'a Value },
}

fn visible_text_projection(aat: &Value) -> String;
fn visible_text_fragments<'a>(aat: &'a Value) -> Vec<VisibleFragment<'a>>;
fn inline_nodes_by_kind<'a>(aat: &'a Value, kind: &str) -> Vec<(String, &'a Value)>; // JSON path + node
fn node_line(node: &Value) -> Option<usize>; // Reads span.line_start when present
```

**Built-in properties:**

| Property | Check |
|----------|-------|
| `schema_valid` | AAT JSON conforms to `data/aat-schema.json`; semantic properties are skipped when this fails. |
| `parse_completeness` | `meta.parse_complete` is `true`. |
| `visible_text_body_order` | For works with a detected body region, AAT visible text is accounted for in decoded body-source order. Works without body boundaries are skipped for this property. |
| `ruby_completeness` | Every source `《...》` ruby marker that is not inside `［＃...］` on the decoded source line has a corresponding inline `ruby` node in AAT. |
| `gaiji_resolution` | Every source `※［＃...］` marker has a corresponding inline `gaiji` node with `resolved` set or `unresolved_reason` set. |
| `block_balance` | Source block-start markers from `data/block-patterns.toml` match source block-end markers, or AAT warnings explicitly report an unclosed block with line information. |
| `no_dropped_lines` | Non-empty decoded body-region source lines that contain visible text contribute visible text to the projection. |
| `heading_level_consistency` | Inline headings and block headings agree on normalized `level` within a work. |

**Normalization for `visible_text_body_order`:**
- Traverse the AAT with the shared projection rules from the design spec: `text.value`, `ruby.base`, `gaiji.resolved` or `gaiji.description`, `raw.source`, and recursive child projection.
- Decode source bytes before matching. `span.byte_start` and `span.byte_end` refer to UTF-8 byte offsets in the decoded source.
- Restrict source matching to the body region from `data/body-patterns.toml`. If no body region is detected, mark this property skipped rather than failed.
- Apply NFKC normalization to decoded source and AAT text fragments before matching.
- Match ordinary text and ruby bases as an ordered subsequence of source characters. Match each inline `gaiji` node against the next source gaiji marker (`※［＃...］`) rather than requiring the resolved Unicode character to appear in the raw source.
- Remove Aozora annotation delimiters only for clearly bounded inline ruby markers (`｜`, `《...》`) and gaiji marker wrappers (`※［＃...］`) when checking feature-specific counts. Do not attempt to strip all markup into a perfect plain-text source.
- Collapse whitespace to a single space.
- Mark this property as `"confidence": "heuristic"` because source-side markup detection is intentionally not a full parser.

**Output format (JSON):**
```json
{
  "adapter": "test-adapter",
  "adapter_version": "0.0.1",
  "work_id": "test_work",
  "results": {
    "schema_valid": {"pass": true},
    "parse_completeness": {"pass": true},
    "visible_text_body_order": {"pass": true, "confidence": "heuristic"},
    "ruby_completeness": {
      "pass": false,
      "message": "Ruby marker on line 3 has no corresponding AAT ruby node",
      "line": 3,
      "confidence": "heuristic"
    },
    "gaiji_resolution": {"pass": true},
    "block_balance": {"pass": true}
  }
}
```

**Deterministic JSON output:**
- Use stable structs or `BTreeMap` for maps.
- Sort feature arrays, work IDs, warning lists with equal location, and result keys before serialization.
- Write with `serde_json::to_writer_pretty`.

**CLI:**
```bash
# Single work (adapter output already produced)
ab-check --txt work.txt --aat work.aat.json --output check.json

# Batch: ab-check invokes the adapter itself, sampling from the index
ab-check --index index.json --sample 50 --features ruby,gaiji --adapter test-adapter --output checks/ --jobs 8 --per-work-timeout 60s

# Feature-specific batch (all works with one or more features)
ab-check --index index.json --features ruby --adapter test-adapter --output checks/

# Explicit sample generated by ab-index
ab-check --index index.json --work-ids sample.json --adapter test-adapter --output checks/
```

When `--output` is a directory, write per-work output to `checks/{adapter}/{work_id}.json`.

**Adapter invocation:**
```bash
# ab-check discovers adapter via AB_ADAPTER_PATH or $PATH
# Adapter binary name: <name>-adapter
# Full invocation: <name>-adapter --mode aat < work.txt > work.aat.json
# Batch invocation uses a bounded subprocess worker pool. Default jobs = logical CPU count; --jobs N overrides it.
# Each work has --per-work-timeout, default 60s.
```

Adapter exit handling:
- Exit 0: validate schema and run all properties.
- Exit 1: record `fatal_error` with stderr and skip schema/property checks.
- Exit 2: validate partial AAT and run all properties; `parse_completeness` fails when `meta.parse_complete` is false.
- Any other exit code: record `adapter_protocol_error` and skip schema/property checks.
- Timeout: kill the child process, record `adapter_timeout`, and skip schema/property checks.
- Before a batch, run `<adapter> --version` once and record the exact line in every report.

**Schema validation:**
- Before running properties, validate the AAT JSON against the schema embedded with `include_str!("../../../data/aat-schema.json")`.
- Do not resolve `data/aat-schema.json` from the current working directory at runtime; `ab-check` must remain portable when called from any directory.
- Compile the `jsonschema::Validator` once at startup and share it across workers with `Arc`.
- Invalid AAT produces a check report with `schema_valid: {"pass": false, "message": "...", "path": "..."}` and skips semantic properties for that work.
- Valid AAT includes `schema_valid: {"pass": true}` in the output report.

**Integration test:**
- Create `fixtures/test_ruby.txt` containing `吾輩《わがはい》は猫である。`.
- Run `test-adapter --mode aat < fixtures/test_ruby.txt > /tmp/test_ruby.aat.json`.
- Run `ab-check --txt fixtures/test_ruby.txt --aat /tmp/test_ruby.aat.json` — assert `ruby_completeness` passes and `visible_text_body_order` passes or is skipped only when the fixture intentionally lacks body boundaries.
- Create `fixtures/test_plain.txt` containing `吾輩は猫である。` (no ruby).
- Run `test-adapter --mode aat < fixtures/test_plain.txt > /tmp/test_plain.aat.json`.
- Run `ab-check --txt fixtures/test_plain.txt --aat /tmp/test_plain.aat.json` — assert `ruby_completeness` passes vacuously (no ruby in source, so no ruby required in AAT).
- Create `fixtures/test_bad_duplicate_ruby.aat.json` where sibling text also contains the ruby base.
- Run `ab-check --txt fixtures/test_ruby.txt --aat fixtures/test_bad_duplicate_ruby.aat.json` — assert `visible_text_body_order` fails because the projection duplicates visible text.

- [ ] **Step 1: Create crate structure**
- [ ] **Step 2: Implement Property trait + built-in properties**
- [ ] **Step 3: Implement adapter invocation in check.rs**
- [ ] **Step 4: Implement CLI**
- [ ] **Step 5: Integration test with test-adapter**
- [ ] **Step 6: Commit**

```bash
git add crates/ab-check
git commit -m "feat: ab-check property-based invariant testing"
```

---

## Task 6: `aozora2` Adapter

**Files:**
- Create: `ab-validator/adapters/aozora2/Cargo.toml`
- Create: `ab-validator/adapters/aozora2/src/main.rs`

**Dependency strategy:**
`aozora-core` is not on crates.io. Use a pinned `git` dependency to the upstream repository and commit the adapter `Cargo.lock`. The explicit `package` key documents that the crate comes from the upstream workspace.

```toml
[package]
name = "aozora2-adapter"
version = "0.1.0"
edition = "2024"
license = "MIT OR Apache-2.0"

[dependencies]
aozora-core = { git = "https://github.com/takahashim/aozora2", package = "aozora-core", rev = "93420b53c7d52579a0ca3fde466cef8ce6d89879" }
anyhow = "1.0"
clap = { version = "4.5", features = ["derive"] }
encoding_rs = "0.8"
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
sha2 = "0.10"

[dev-dependencies]
criterion = "0.8"

[[bench]]
name = "adapter_bench"
harness = false
```

Note: The adapter crate is **not** a workspace member (it's in `adapters/`, not `crates/`), so it cannot use `workspace = true` for dependencies. Pin exact versions in the adapter's Cargo.toml.

**Interface contract:**
- Reads raw `.txt` bytes from stdin and decodes them with the shared source encoding contract.
- Uses `aozora-core::tokenizer::tokenize` + `aozora-core::parser::parse` to produce AST.
- Maps `aozora-core::node::Node` enum to nested AAT JSON.
- Folds `Node::BlockStart` / `Node::BlockEnd` into nested block objects before output.
- Emits AAT JSON to stdout.
- `--mode aat`: produces AAT JSON.
- `--mode html`: produces HTML (uses `aozora-core` HTML renderer if available, otherwise exits 1).
- `--version`: prints `aozora2-adapter 0.1.0 93420b53c7d52579a0ca3fde466cef8ce6d89879`.
- Exit 0 for complete output, 1 for fatal decode/parse/protocol errors, 2 for partial AAT with `meta.parse_complete: false`.

**Mapping rules (aozora-core Node → AAT):**

| aozora-core Node | AAT kind |
|------------------|----------|
| `Node::Text(s)` | `{"kind": "text", "value": s}` |
| `Node::Ruby { children, ruby, .. }` | `{"kind": "ruby", "base": flatten(children), "reading": flatten(ruby)}` |
| `Node::Gaiji { description, unicode, jis_code }` | `{"kind": "gaiji", "description": description, "resolved": unicode, "jis_code": jis_code, "unresolved_reason": null-or-reason}` |
| `Node::BlockStart { block_type, params }` | Push a new block frame; do not emit an inline node |
| `Node::BlockEnd { block_type, params }` | Pop the matching block frame into its parent `children`; warn on mismatch |
| `Node::Accent { .. }` | `{"kind": "accent", ...}` |
| Other | `{"kind": "raw", "source": "..."}` |

**Block folding rules:**
- Maintain a stack initialized with a synthetic root frame.
- Normal inline nodes append to the current leaf paragraph/content frame.
- `BlockStart` pushes a block frame whose `kind` is derived from `block_type`.
- `BlockEnd` pops the matching frame into the parent. Mismatched or missing ends append a warning to `meta.warnings` and set `meta.parse_complete` to `false` only if the adapter cannot produce coherent nested output.
- End of input closes remaining frames and records an unclosed-block warning for each frame.
- The final AAT `blocks` array is the synthetic root's children; no `block_start` or `block_end` nodes are emitted.

**Source spans:**
- If `aozora-core` does not expose source offsets on nodes, emit `span` only where the adapter can derive it safely from token positions or source scanning.
- `span.byte_start` and `span.byte_end` are UTF-8 byte offsets in the decoded source text.
- Missing spans are allowed, but warnings and property violations must still use source-side line numbers when checks detect a source marker.
- `meta.source_hash` is SHA256 of raw stdin bytes; `meta.source_encoding` is the detected decoding used for parser input.

**Integration test:**
- Build adapter.
- Run against a real Aozora Bunko `.txt` file.
- Validate output JSON against `aat-schema.json`.
- Assert the output contains no `block_start` or `block_end` AAT nodes.
- Run `ab-check` on the output — at minimum `parse_completeness` should pass.

- [ ] **Step 1: Create adapter crate with git dependency**
- [ ] **Step 2: Implement Node → AAT mapping**
- [ ] **Step 3: Build and test against real corpus work**
- [ ] **Step 4: Commit**

```bash
git add adapters/aozora2
git commit -m "feat: aozora2 adapter"
```

---

## Task 7: Benchmark Baselines

**Files:**
- Create: `ab-validator/benchmarks/README.md`
- Create: `ab-validator/benchmarks/samples/mixed-100.json`
- Create: `ab-validator/benchmarks/samples/stress-1000.json`
- Modify: `ab-validator/crates/ab-check/Cargo.toml` (add Criterion bench targets/dev-dependency)
- Create: `ab-validator/crates/ab-check/benches/aat_json_io.rs`
- Create: `ab-validator/crates/ab-check/benches/check_properties.rs`
- Modify: `ab-validator/adapters/aozora2/Cargo.toml` (add Criterion bench target/dev-dependency)
- Create: `ab-validator/adapters/aozora2/benches/adapter_bench.rs`

**Purpose:** Establish repeatable performance baselines before changing the wire format, concurrency strategy, parser adapter structure, or schema validation strategy.

**Sample file format:**

```json
{
  "corpus_hash": "sha256:...",
  "generated_from_index": "index.json",
  "work_ids": ["000148_799"]
}
```

**Benchmark contracts:**
- `aat_json_io`: measures AAT JSON parse, serialize, and schema validation cost on representative AAT fixtures.
- `check_properties`: measures AAT traversal and property checks after JSON is already loaded.
- `adapter_bench`: measures `aozora-core` tokenize + parse + AAT emission separately from `ab-check`.
- Samples are JSON objects with `corpus_hash` and `work_ids`. They are generated once from the index and checked in as stable benchmark inputs.
- Benchmark commands must fail before measuring when the current index `corpus_hash` does not match the sample file's `corpus_hash`.
- Benchmark output includes corpus hash, adapter version, git revision if available, sample name, job count, p50, p95, and total wall-clock time where applicable.

**Format decision rule:**
- Keep JSON as the canonical adapter and report format until benchmark evidence shows JSON parse + serialize + schema validation exceeds 15% of end-to-end batch runtime on `mixed-100` or blocks the success criteria.
- If JSON is material overhead, add an optional cache/sidecar format in a later task. Candidate formats include JSON Lines batching, zstd-compressed JSON, CBOR, MessagePack, and `rkyv`. The sidecar must be reproducible from canonical JSON and must not replace JSON reports.

**Commands:**
```bash
cd ab-validator
cargo bench -p ab-check --bench aat_json_io
cargo bench -p ab-check --bench check_properties

cd adapters/aozora2
cargo bench --bench adapter_bench
```

**Integration test:**
- Run the benches against tiny fixtures first and confirm they produce measurements without requiring the full corpus.
- Assert `benchmarks/samples/mixed-100.json` rejects an index with a mismatched `corpus_hash`.
- Run `mixed-100` once before adding a second parser adapter.
- Record baseline results under `benchmarks/results/` when executing the implementation plan.

- [ ] **Step 1: Create benchmark documentation and sample-file format**
- [ ] **Step 2: Add `ab-check` JSON I/O benchmark**
- [ ] **Step 3: Add `ab-check` property benchmark**
- [ ] **Step 4: Add `aozora2` adapter benchmark**
- [ ] **Step 5: Run tiny-fixture benchmark smoke test**
- [ ] **Step 6: Commit**

```bash
git add benchmarks crates/ab-check/benches adapters/aozora2/benches
git commit -m "test: add parser validation benchmark baselines"
```

---

## Phase 3: Rendering And Additional Parser Adapters (Future)

**Task 8: `ab-render-diff` for aozora2** — HTML normalization + tolerant diff against official XHTML.
- Input: Adapter `--mode html` output + official `.html`.
- Output: Divergence report (Critical / Warning / Info).
- **Blocked on:** aozora2 adapter `--mode html` support.

**Task 9: `aozora-rs` Adapter + `ab-check`** — Validate the second Rust parser against the same AAT contract.
- Input: Sampled works from `ab-index`.
- Output: AAT JSON and check reports.
- **Blocked on:** Phase 2 AAT/check pipeline stability.

**Task 10: `ab-render-diff` for aozora-rs** — HTML normalization + tolerant diff against official XHTML.
- Input: Adapter `--mode html` output + official `.html`.
- Output: Divergence report (Critical / Warning / Info).
- **Blocked on:** aozora-rs adapter `--mode html` support.

---

## Phase 4: Cross-Parser Comparison And Reporting (Future)

**Task 11: `ab-compare`** — AAT JSON diff between two parsers.
- Input: Two AAT JSON files for the same work.
- Output: Semantic diff report at `diffs/{adapter_a}_vs_{adapter_b}/{work_id}.json`.
- **Blocked on:** Having at least two working adapters.

**Task 12: `ab-report`** — Aggregate + run-to-run comparison.
- Summarize `ab-check` results across a batch.
- Support `diff-runs` for longitudinal tracking.
- **Blocked on:** Having check results from multiple runs.

---

## Self-Review Checklist

| Requirement | Location | Status |
|-------------|----------|--------|
| Adapter naming convention (`<name>-adapter`) | §Adapter invocation | ✅ Specified |
| Adapter discovery (`AB_ADAPTER_PATH`, `$PATH`) | §Adapter invocation | ✅ Specified |
| Adapter `--mode aat` / `--mode html` | §Test adapter, §aozora2 adapter | ✅ Specified |
| `sample.json` consumer | §ab-index CLI, §ab-check CLI, §Benchmark Baselines | ✅ Used by `--work-ids` and benchmark samples |
| Unified feature flag | §ab-check CLI | ✅ Uses `--features` for one or many features |
| AAT schema validation before checks | §AAT schema, §ab-check | ✅ Specified |
| Embedded schema loading | §ab-check | ✅ Uses `include_str!` |
| Source encoding contract | §ab-index, §ab-check, §aozora2 adapter | ✅ UTF-8 after decode, Windows-31J fallback |
| Adapter exit code semantics | §ab-check, §aozora2 adapter | ✅ Specified |
| Adapter version capture | §ab-check, §aozora2 adapter | ✅ `--version` required |
| Adapter timeout | §ab-check | ✅ `--per-work-timeout` |
| `meta.parse_complete` semantics | §AAT schema | ✅ `parse_completeness` property fails if false |
| `meta.warnings` semantics | §AAT schema | ✅ Advisory, recorded but don't affect pass/fail |
| Nested AAT blocks, no event nodes | §AAT schema, §aozora2 adapter | ✅ Specified |
| Inline gaiji | §AAT schema, §ab-check, §aozora2 adapter | ✅ Specified |
| Source span policy | §AAT schema, §ab-check, §aozora2 adapter | ✅ Specified |
| Visible-text projection | §AAT schema, §ab-check | ✅ Specified |
| Body-region gating for visible text | §ab-check | ✅ Skips when body boundary unavailable |
| Heuristic check confidence | §ab-check | ✅ Specified |
| Deterministic path normalization | §ab-index | ✅ POSIX-style relative paths |
| Deterministic `corpus_hash` | §ab-index | ✅ Content-based, no mtimes |
| Deterministic JSON output | §ab-check | ✅ Stable ordering and pretty serialization |
| Batch output filenames | §ab-check, future task notes | ✅ `checks/{adapter}/{work_id}.json` etc. |
| Benchmark baselines | §Benchmark Baselines | ✅ Specified |
| Benchmark samples pinned to corpus hash | §Benchmark Baselines | ✅ Specified |
| JSON format decision rule | §Benchmark Baselines | ✅ Data-driven threshold |
| `ab-discover` removed | Not present | ✅ |
| Task numbering | Sequential 1–7, future tasks 8–12 | ✅ |
| Inline code minimized | Interface contracts | ✅ |
| `edition = "2024"` | Cargo.toml | ✅ Kept per user instruction |
| `aozora-core` dependency | git dependency | ✅ Pinned with `rev` |
| `rayon` for CPU-bound work; subprocess pool for adapters | ab-index, ab-check | ✅ Specified |
| Work ID convention | card_file format | ✅ Specified |
| `corpus_hash` in Index | Index struct | ✅ Specified |
| `corpus_hash` byte encoding | §ab-index | ✅ NUL-delimited tuple format |
| Test-adapter | adapters/test-adapter | ✅ Present |
| Integration tests per task | Each task | ✅ Specified |
| MVP implementation scope | Through aozora2 adapter and benchmark baseline | ✅ |
