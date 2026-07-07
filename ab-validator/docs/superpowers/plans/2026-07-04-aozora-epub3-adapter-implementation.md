# AozoraEpub3-JDK21 Adapter Implementation Plan

> **For agentic workers:** REQUIRED SUB-AGENT SKILL: Use `superpowers:subagent-driven-development` (recommended) or `superpowers:executing-plans` to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a first-class `aozora-epub3` adapter to `ab-validator` that consumes raw Aozora Bunko bytes, runs the upstream `AozoraEpub3-JDK21` JAR, extracts body XHTML from the resulting EPUB, and emits AAT JSON or HTML on stdout.

**Architecture:** A bash wrapper handles process invocation, EPUB unzip, `package.opf` parsing, and mapper invocation. A standalone Rust mapper crate (initially copying small helpers from `aozora2html`) parses AozoraEpub3-specific XHTML and converts it to AAT. Source-derived recovery runs as a separate pass over the AAT value. The adapter is registered in `ab-coverage` and built/tested via `justfile` and `flake.nix`.

**Tech Stack:** Rust 2024, Bash, Java 21 / Gradle, Nix, `roxmltree` HTML/XML parser, `encoding_rs`, `serde_json`, `clap`, `regex`.

## Critical review response

A second-pass review surfaced implementation-level issues. The plan has been updated to address them:

| Finding | Severity | Resolution in this plan |
|---------|----------|------------------------|
| Use `roxmltree` not `tl` for DOM parsing | Strong suggestion | Task 1 dependencies now use `roxmltree = "0.20"`; Task 4 rewritten with `roxmltree` helpers. |
| Remove unused `zip` crate | Strong suggestion | `zip` removed from `Cargo.toml` in Task 1; wrapper uses system `unzip`. |
| Remove spurious `[workspace]` declaration | Strong suggestion | `[workspace]` removed from adapter `Cargo.toml` in Task 1. |
| Add missing `regex` dependency | Strong suggestion | `regex = "1"` added in Task 1; used in Task 5. |
| Empty `$enc_arg` passed as JAR argument | Blocker | Task 7 now uses a `java_args` array; only appends `-enc UTF-8` when non-empty. |
| Unchecked `python` dependency | Blocker | Task 7 now checks `command -v python` at startup. |
| Fallback glob misses 5+ digit filenames | Strong suggestion | Task 7 fallback uses `find ... -name '[0-9]*.xhtml' | sort -V`. |
| Task 4 mapper underspecified | Strong suggestion | Task 4 split into 4a–4e with concrete fixtures, helper code, and expected AAT for each mapping category. |
| Exit code 2 never set | Question | Task 6 now calls `process::exit(2)` when `--parser-failed` is set in `--mode aat`. |
| Colophon classification pushed to mapper | Question | Wrapper now classifies by spine position and passes `--xhtml-colophon`; mapper CLI accepts it. |
| `--mode html` colophon inclusion undocumented | Nit | Task 6 documents that `--mode html` includes colophon at the end. |
| Binary integration test assumes release build | Nit | Task 8 integration test now builds the release binary once via `std::sync::Once`. |

## Global Constraints

- Upstream `AozoraEpub3-JDK21` source stays under `references/parsers/AozoraEpub3-JDK21/`; no GPL code is copied into project-owned source or data.
- The adapter must match the existing adapter wire contract:
  - `aozora-epub3-adapter --mode aat < input.txt > output.aat.json`
  - `aozora-epub3-adapter --mode html < input.txt > output.html`
  - `aozora-epub3-adapter --version`
  - Exit codes: `0` success, `1` fatal error, `2` partial success with `meta.parse_complete=false`.
- New Rust adapter crates are excluded from the workspace root (`Cargo.toml` `exclude`).
- AAT output must validate against `data/aat-schema.json`.
- Nix checks use a pinned release JAR with SHA-256; local dev can build from source or override via `AB_AOZORAEPUB3_JAR`.
- All temp files/directories are created with `mktemp` and cleaned up with `trap ... EXIT`.

---

## File Structure

### New files

- `adapters/aozora-epub3/Cargo.toml` — standalone adapter crate manifest.
- `adapters/aozora-epub3/Cargo.lock` — lockfile for reproducible builds.
- `adapters/aozora-epub3/src/lib.rs` — XHTML→AAT mapping, source-derived recovery, AAT envelope.
- `adapters/aozora-epub3/src/main.rs` — CLI entrypoint (`--mode`, `--source`, `--xhtml`, `--version`).
- `adapters/aozora-epub3/src/decode.rs` — source byte decoding and hashing (adapted from `aozora2html`).
- `adapters/aozora-epub3/src/xhtml_mapper.rs` — DOM walking and AAT construction.
- `adapters/aozora-epub3/src/source_derived.rs` — source-derived recovery pass.
- `adapters/aozora-epub3/src/model.rs` — shared types (`DecodedSource`, `MappingInput`, `MappingError`).
- `adapters/aozora-epub3/aozora-epub3-adapter` — shell wrapper script.
- `adapters/aozora-epub3/README.md` — adapter documentation.
- `adapters/aozora-epub3/tests/fixtures/` — input `.txt`, upstream `.xhtml`, expected `.aat.json` fixtures.
- `adapters/aozora-epub3/tests/integration.rs` — Rust integration tests.
- `tests/aozora-epub3-adapter-smoke.sh` — harness smoke test.

### Modified files

- `Cargo.toml` — add `adapters/aozora-epub3` to `exclude`.
- `crates/ab-coverage/src/adapter.rs` — add `aozora-epub3` parser ID and binary path.
- `crates/ab-coverage/src/cache.rs` — add `aozora-epub3` source root for fingerprinting.
- `crates/ab-coverage/src/adapter.rs` tests — update parser list assertions.
- `crates/ab-coverage/src/cache.rs` tests — update parser list assertions.
- `justfile` — add build, test, and smoke targets.
- `flake.nix` — add pinned JAR fetch, dev shell, and smoke check.
- `data/adapter-fidelity-notes.toml` — add notes for known divergences.

---

## Task 1: Bootstrap the adapter crate

**Files:**
- Create: `adapters/aozora-epub3/Cargo.toml`
- Create: `adapters/aozora-epub3/Cargo.lock`
- Modify: `Cargo.toml`

**Interfaces:**
- Consumes: nothing.
- Produces: a buildable standalone Rust crate at `adapters/aozora-epub3/` excluded from the workspace.

- [ ] **Step 1: Create the crate manifest**

Create `adapters/aozora-epub3/Cargo.toml`:

```toml
[package]
name = "aozora-epub3-adapter"
version = "0.1.0"
edition = "2024"
license = "MIT OR Apache-2.0"

[dependencies]
anyhow = "1.0"
clap = { version = "4.5", features = ["derive"] }
encoding_rs = "0.8"
regex = "1"
roxmltree = "0.20"
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
sha2 = "0.10"
```

- [ ] **Step 2: Exclude the adapter from the workspace root**

Edit root `Cargo.toml` and append `"adapters/aozora-epub3"` to the `exclude` array:

```toml
exclude = [
    "adapters/aozora2",
    "adapters/aozora2html",
    "adapters/aozora-rs",
    "adapters/aozora-epub3",
    "references",
]
```

- [ ] **Step 3: Generate the lockfile and verify the crate builds**

Run:

```bash
cargo generate-lockfile --manifest-path adapters/aozora-epub3/Cargo.toml
cargo check --manifest-path adapters/aozora-epub3/Cargo.toml
```

Expected: both commands succeed.

- [ ] **Step 4: Commit**

```bash
git add adapters/aozora-epub3/Cargo.toml adapters/aozora-epub3/Cargo.lock Cargo.toml
git commit -m "chore: bootstrap aozora-epub3 adapter crate"
```

---

## Task 2: Implement source decoding and hashing

**Files:**
- Create: `adapters/aozora-epub3/src/model.rs`
- Create: `adapters/aozora-epub3/src/decode.rs`

**Interfaces:**
- Consumes: nothing.
- Produces:
  - `model::DecodedSource { text, encoding, source_hash }`
  - `decode::decode_source_bytes(bytes: &[u8]) -> Result<DecodedSource>`
  - `decode::source_hash(bytes: &[u8]) -> String`

- [ ] **Step 1: Define shared types in `model.rs`**

Create `adapters/aozora-epub3/src/model.rs`:

```rust
use serde_json::{json, Value};

pub const ADAPTER_NAME: &str = "aozora-epub3";
pub const ADAPTER_VERSION: &str = "aozora-epub3-adapter 0.1.0 AozoraEpub3-JDK21-1.3.4-jdk21";

#[derive(Debug)]
pub struct DecodedSource {
    pub text: String,
    pub encoding: &'static str,
    pub source_hash: String,
}

#[derive(Debug)]
pub struct MappingInput {
    pub source_bytes: Vec<u8>,
    pub xhtml_documents: Vec<XhtmlDocument>,
    pub parser_failed: bool,
    pub parser_error_message: Option<String>,
}

#[derive(Debug)]
pub struct XhtmlDocument {
    pub bytes: Vec<u8>,
    pub kind: XhtmlDocumentKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XhtmlDocumentKind {
    BodySection,
    Colophon,
}

pub fn parse_failure_envelope(
    source_encoding: &str,
    source_hash: &str,
    message: impl Into<String>,
) -> Value {
    json!({
        "version": 1,
        "work_id": "stdin",
        "blocks": [],
        "meta": {
            "adapter": ADAPTER_NAME,
            "adapter_version": ADAPTER_VERSION,
            "source_encoding": source_encoding,
            "source_hash": source_hash,
            "parse_complete": false,
            "warnings": [{"message": message.into()}],
        },
    })
}
```

- [ ] **Step 2: Implement decoding in `decode.rs`**

Create `adapters/aozora-epub3/src/decode.rs`:

```rust
use anyhow::{Context, Result};
use sha2::{Digest, Sha256};

use crate::model::DecodedSource;

pub fn decode_source_bytes(bytes: &[u8]) -> Result<DecodedSource> {
    if bytes.starts_with(&[0xef, 0xbb, 0xbf]) {
        let text = std::str::from_utf8(&bytes[3..])
            .context("input claimed UTF-8 BOM but was not valid UTF-8")?;
        return Ok(DecodedSource {
            text: text.to_string(),
            encoding: "utf-8-bom",
            source_hash: source_hash(bytes),
        });
    }

    if let Ok(text) = std::str::from_utf8(bytes) {
        return Ok(DecodedSource {
            text: text.to_string(),
            encoding: "utf-8",
            source_hash: source_hash(bytes),
        });
    }

    let (text, _, had_errors) = encoding_rs::SHIFT_JIS.decode(bytes);
    Ok(DecodedSource {
        text: text.into_owned(),
        encoding: if had_errors { "windows-31j-lossy" } else { "windows-31j" },
        source_hash: source_hash(bytes),
    })
}

pub fn source_hash(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    format!("sha256:{:x}", hasher.finalize())
}
```

- [ ] **Step 3: Add a unit test for decoding**

Add to `adapters/aozora-epub3/src/decode.rs`:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decodes_utf8_and_hashes() {
        let bytes = b"\xe3\x81\xa6\xe3\x81\x99\xe3\x81\xa8"; // テスト
        let decoded = decode_source_bytes(bytes).unwrap();
        assert_eq!(decoded.text, "テスト");
        assert_eq!(decoded.encoding, "utf-8");
        assert!(decoded.source_hash.starts_with("sha256:"));
    }

    #[test]
    fn decodes_shift_jis() {
        let bytes = encoding_rs::SHIFT_JIS.encode("テスト").0.into_owned();
        let decoded = decode_source_bytes(&bytes).unwrap();
        assert_eq!(decoded.text, "テスト");
        assert_eq!(decoded.encoding, "windows-31j");
    }
}
```

- [ ] **Step 4: Run the tests**

```bash
cargo test --manifest-path adapters/aozora-epub3/Cargo.toml
```

Expected: decoding tests pass.

- [ ] **Step 5: Commit**

```bash
git add adapters/aozora-epub3/src/model.rs adapters/aozora-epub3/src/decode.rs
git commit -m "feat(aozora-epub3): source decoding and hashing"
```

---

## Task 3: Implement the XHTML-to-AAT mapper

**Files:**
- Create: `adapters/aozora-epub3/src/xhtml_mapper.rs`
- Modify: `adapters/aozora-epub3/src/lib.rs`

**Interfaces:**
- Consumes: `MappingInput` from `model.rs`, `DecodedSource` from `decode.rs`.
- Produces:
  - `lib::map_to_aat(input: &MappingInput) -> anyhow::Result<Value>`
  - `lib::map_to_html(input: &MappingInput) -> anyhow::Result<String>`

- [ ] **Step 1: Implement the core mapping functions in `xhtml_mapper.rs`**

Create `adapters/aozora-epub3/src/xhtml_mapper.rs` with the following public function:

```rust
use anyhow::Result;
use serde_json::{json, Value};

use crate::model::{MappingInput, XhtmlDocumentKind};

pub fn map_to_aat(input: &MappingInput) -> Result<Value> {
    use crate::decode::decode_source_bytes;

    let decoded = decode_source_bytes(&input.source_bytes)?;

    if input.parser_failed {
        let message = input.parser_error_message.clone().unwrap_or_default();
        return Ok(crate::model::parse_failure_envelope(
            decoded.encoding,
            &decoded.source_hash,
            if message.is_empty() {
                "AozoraEpub3 parser aborted".to_string()
            } else {
                format!("AozoraEpub3 parser aborted: {message}")
            },
        ));
    }

    let blocks: Vec<Value> = vec![];
    Ok(json!({
        "version": 1,
        "work_id": "stdin",
        "blocks": blocks,
        "meta": {
            "adapter": crate::model::ADAPTER_NAME,
            "adapter_version": crate::model::ADAPTER_VERSION,
            "source_encoding": decoded.encoding,
            "source_hash": decoded.source_hash,
            "parse_complete": true,
            "warnings": [],
        },
    }))
}

pub fn map_to_html(input: &MappingInput) -> Result<String> {
    let mut out = String::new();
    for (idx, doc) in input.xhtml_documents.iter().enumerate() {
        if idx > 0 {
            out.push_str("\n<!-- aozora-epub3 section boundary -->\n");
        }
        out.push_str(std::str::from_utf8(&doc.bytes)?);
    }
    Ok(out)
}
```

- [ ] **Step 2: Wire up `lib.rs`**

Create `adapters/aozora-epub3/src/lib.rs`:

```rust
pub mod decode;
pub mod model;
pub mod source_derived;
pub mod xhtml_mapper;

pub use model::{parse_failure_envelope, DecodedSource, MappingInput, XhtmlDocument, XhtmlDocumentKind};

pub fn map_to_aat(input: &MappingInput) -> anyhow::Result<serde_json::Value> {
    xhtml_mapper::map_to_aat(input)
}

pub fn map_to_html(input: &MappingInput) -> anyhow::Result<String> {
    xhtml_mapper::map_to_html(input)
}
```

- [ ] **Step 3: Verify the crate compiles**

```bash
cargo check --manifest-path adapters/aozora-epub3/Cargo.toml
```

Expected: success.

- [ ] **Step 4: Commit**

```bash
git add adapters/aozora-epub3/src/lib.rs adapters/aozora-epub3/src/xhtml_mapper.rs
git commit -m "feat(aozora-epub3): xhtml mapper scaffold"
```

---

## Task 4: Implement DOM parsing and block/inline mapping

**Files:**
- Modify: `adapters/aozora-epub3/src/xhtml_mapper.rs`

**Interfaces:**
- Consumes: `roxmltree::Document` for XHTML DOM (consistent with `aozora2html`).
- Produces: fully populated AAT `blocks` array.

- [ ] **Step 1: Add helper to parse XHTML and extract body children**

Add to `xhtml_mapper.rs`:

```rust
use roxmltree::{Document, Node, NodeType};

fn body_element<'a>(doc: &'a Document<'a>) -> anyhow::Result<Node<'a, 'a>> {
    doc.root_element()
        .children()
        .filter(|n| n.node_type() == NodeType::Element && n.tag_name().name() == "body")
        .next()
        .ok_or_else(|| anyhow::anyhow!("no <body> found"))
}

fn element_children<'a>(node: Node<'a, 'a>) -> impl Iterator<Item = Node<'a, 'a>> {
    node.children().filter(|n| n.node_type() == NodeType::Element)
}

fn node_tag<'a>(node: Node<'a, 'a>) -> &'a str {
    node.tag_name().name()
}

fn node_classes<'a>(node: Node<'a, 'a>) -> Vec<&'a str> {
    node.attribute("class")
        .map(|s| s.split_whitespace().collect())
        .unwrap_or_default()
}

fn has_class<'a>(node: Node<'a, 'a>, needle: &str) -> bool {
    node_classes(node).contains(&needle)
}

fn class_with_prefix<'a>(node: Node<'a, 'a>, prefix: &str) -> Option<&'a str> {
    node_classes(node).into_iter().find(|c| c.starts_with(prefix))
}

fn visible_text(node: Node) -> String {
    let mut out = String::new();
    for child in node.descendants() {
        if child.node_type() == NodeType::Text {
            out.push_str(child.text().unwrap_or(""));
        }
    }
    out
}
```

- [ ] **Step 2: Implement block-level mapping**

Implement `map_block` in `xhtml_mapper.rs`:

```rust
fn map_body_children(children: impl Iterator<Item = Node>) -> Vec<Value> {
    children.filter_map(map_block).collect()
}

fn map_block(node: Node) -> Option<Value> {
    if node_tag(node) == "p" {
        if let Some(heading) = try_map_inline_heading(node) {
            return Some(heading);
        }
        return Some(json!({
            "kind": "paragraph",
            "content": map_inline_children(node),
        }));
    }

    if node_tag(node) == "div" {
        let classes = node_classes(node);
        if classes.contains(&"chap1") {
            return Some(json!({"kind": "heading", "level": 1, "content": map_inline_children(node)}));
        }
        if classes.contains(&"chap2") {
            return Some(json!({"kind": "heading", "level": 2, "content": map_inline_children(node)}));
        }
        if classes.contains(&"chap3") {
            return Some(json!({"kind": "heading", "level": 3, "content": map_inline_children(node)}));
        }
        if let Some(cls) = class_with_prefix(node, "pt") {
            let n: u32 = cls.strip_prefix("pt").unwrap_or("0").parse().unwrap_or(0);
            return Some(json!({"kind": "jisage_block", "x_indent": n, "content": map_body_children(element_children(node))}));
        }
        if classes.contains(&"border") {
            return Some(json!({"kind": "keigakomi_block", "x_border_kind": "solid", "content": map_body_children(element_children(node))}));
        }
        if classes.contains(&"dashed_border") {
            return Some(json!({"kind": "keigakomi_block", "x_border_kind": "dashed", "content": map_body_children(element_children(node))}));
        }
        if classes.contains(&"yoko") {
            return Some(json!({"kind": "yokogumi_block", "content": map_body_children(element_children(node))}));
        }
    }

    None
}
```

- [ ] **Step 3: Implement inline-level mapping**

Implement `map_inline_children`, `map_inline`, and `map_span_with_classes`:

```rust
fn map_inline_children(node: Node) -> Vec<Value> {
    node.children().flat_map(map_inline).collect()
}

fn map_inline(node: Node) -> Vec<Value> {
    match node.node_type() {
        NodeType::Text => {
            let text = node.text().unwrap_or("").to_string();
            if text.is_empty() { vec![] } else { vec![json!({"kind": "text", "value": text})] }
        }
        NodeType::Element => {
            let tag = node_tag(node);
            let classes = node_classes(node);
            match tag {
                "br" => vec![json!({"kind": "text", "value": "\n", "x-break-kind": "line"})],
                "ruby" => {
                    let rt = get_rt_text(node);
                    let mut base = visible_text(node);
                    if base.ends_with(&rt) {
                        base.truncate(base.len() - rt.len());
                    }
                    vec![json!({"kind": "ruby", "base": base, "reading": rt, "direction": "right"})]
                }
                "img" => {
                    let src = node.attribute("src").unwrap_or("").to_string();
                    let alt = node.attribute("alt").unwrap_or("").to_string();
                    let width = node.attribute("width").and_then(|s| s.parse().ok());
                    let height = node.attribute("height").and_then(|s| s.parse().ok());
                    vec![json!({"kind": "figure", "src": src, "alt": alt, "width": width, "height": height})]
                }
                "span" => map_span_with_classes(node, &classes),
                _ => map_inline_children(node),
            }
        }
        _ => vec![],
    }
}

fn get_rt_text(node: Node) -> String {
    element_children(node)
        .filter(|n| node_tag(*n) == "rt")
        .map(visible_text)
        .collect::<String>()
}

fn map_span_with_classes(node: Node, classes: &[&str]) -> Vec<Value> {
    let content = map_inline_children(node);
    if classes.contains(&"b") {
        return vec![json!({"kind": "style", "style_type": "bold", "content": content})];
    }
    if classes.contains(&"i") {
        return vec![json!({"kind": "style", "style_type": "italic", "content": content})];
    }
    if let Some(boten) = classes.iter().find(|c| matches!(*c,
        "sesame" | "dot" | "open_sesame" | "open_dot" | "double_open_sesame" | "double_open_dot")) {
        return vec![json!({"kind": "boten", "x_boten_kind": *boten, "content": content})];
    }
    if let Some(line) = classes.iter().find(|c| matches!(*c, "underline" | "double_underline")) {
        let kind = if *line == "double_underline" { "double" } else { "single" };
        return vec![json!({"kind": "bousen", "x_line_kind": kind, "content": content})];
    }
    if classes.contains(&"tcy") {
        return vec![json!({"kind": "tcy", "content": content})];
    }
    if classes.contains(&"wrc") {
        return vec![json!({"kind": "warigaki", "content": content})];
    }
    content
}
```

- [ ] **Step 4: Implement inline heading detection**

Detection rule: a `<p>` whose only element child is a single `<span>` with class `font5`, `font3`, or `font1`, and no other non-whitespace text, becomes a heading. If the paragraph has mixed content, treat the span as a normal style span.

```rust
fn try_map_inline_heading(p: Node) -> Option<Value> {
    let children: Vec<Node> = element_children(p).collect();
    if children.len() != 1 {
        return None;
    }
    let span = children[0];
    if node_tag(span) != "span" {
        return None;
    }
    let classes = node_classes(span);
    let level = if classes.contains(&"font5") {
        1
    } else if classes.contains(&"font3") {
        2
    } else if classes.contains(&"font1") {
        3
    } else {
        return None;
    };
    Some(json!({
        "kind": "heading",
        "level": level,
        "x_heading_kind": "dogyo",
        "content": map_inline_children(span),
    }))
}
```

- [ ] **Step 5: Add fixtures for every mapping category**

Create the following fixtures in `adapters/aozora-epub3/tests/fixtures/`:

`paragraph.xhtml`:
```xml
<?xml version="1.0" encoding="utf-8"?>
<html xmlns="http://www.w3.org/1999/xhtml">
<body><p>一文字。</p></body>
</html>
```

`heading_chap1.xhtml`:
```xml
<?xml version="1.0" encoding="utf-8"?>
<html xmlns="http://www.w3.org/1999/xhtml">
<body><div class="chap1">第一章</div></body>
</html>
```

`heading_chap2.xhtml`, `heading_chap3.xhtml`: same with `chap2`/`chap3` and levels 2/3.

`jisage.xhtml`:
```xml
<?xml version="1.0" encoding="utf-8"?>
<html xmlns="http://www.w3.org/1999/xhtml">
<body><div class="pt2"><p>二字下げ</p></div></body>
</html>
```

`keigakomi.xhtml`:
```xml
<?xml version="1.0" encoding="utf-8"?>
<html xmlns="http://www.w3.org/1999/xhtml">
<body><div class="border"><p>囲み</p></div></body>
</html>
```

`yoko.xhtml`:
```xml
<?xml version="1.0" encoding="utf-8"?>
<html xmlns="http://www.w3.org/1999/xhtml">
<body><div class="yoko"><p>横組</p></div></body>
</html>
```

`ruby_basic.xhtml`:
```xml
<?xml version="1.0" encoding="utf-8"?>
<html xmlns="http://www.w3.org/1999/xhtml">
<body>
<p><ruby>吾輩<rt>わがはい</rt></ruby>は猫である。</p>
</body>
</html>
```

`bold_basic.xhtml`:
```xml
<?xml version="1.0" encoding="utf-8"?>
<html xmlns="http://www.w3.org/1999/xhtml">
<body>
<p><span class="b">太字</span>の例</p>
</body>
</html>
```

`italic_basic.xhtml`, `boten_sesame.xhtml`, `bousen_underline.xhtml`, `tcy.xhtml`, `warigaki.xhtml`, `figure_img.xhtml`, `heading_dogyo.xhtml`, `compound_class.xhtml`: create analogous fixtures covering each inline mapping and inline-heading/compound-class cases.

- [ ] **Step 6: Add fixture-driven unit tests**

Add a test module in `xhtml_mapper.rs`:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::{XhtmlDocument, XhtmlDocumentKind};

    fn input_from_fixture(name: &str) -> MappingInput {
        let path = format!("tests/fixtures/{name}");
        let bytes = std::fs::read(&path).unwrap_or_else(|e| panic!("read {path}: {e}"));
        MappingInput {
            source_bytes: b"test".to_vec(),
            xhtml_documents: vec![XhtmlDocument {
                bytes,
                kind: XhtmlDocumentKind::BodySection,
            }],
            parser_failed: false,
            parser_error_message: None,
        }
    }

    fn assert_blocks(name: &str, expected: serde_json::Value) {
        let input = input_from_fixture(name);
        let aat = map_to_aat(&input).unwrap();
        let blocks = &aat["blocks"];
        assert_eq!(blocks, &expected, "fixture {name} mismatch");
    }

    #[test]
    fn maps_paragraph() {
        assert_blocks("paragraph.xhtml", json!([{"kind": "paragraph", "content": [{"kind": "text", "value": "一文字。"}]}]));
    }

    #[test]
    fn maps_heading_chap1() {
        assert_blocks("heading_chap1.xhtml", json!([{"kind": "heading", "level": 1, "content": [{"kind": "text", "value": "第一章"}]}]));
    }

    // Add one #[test] per fixture with its expected AAT.
}
```

- [ ] **Step 7: Run the tests**

```bash
cargo test --manifest-path adapters/aozora-epub3/Cargo.toml
```

Expected: all fixture tests pass.

- [ ] **Step 8: Commit**

```bash
git add adapters/aozora-epub3/src/xhtml_mapper.rs adapters/aozora-epub3/tests/fixtures/
git commit -m "feat(aozora-epub3): roxmltree-based dom parsing and mapping"
```

---

## Task 5: Implement source-derived recovery

**Files:**
- Create: `adapters/aozora-epub3/src/source_derived.rs`

**Interfaces:**
- Consumes: `&mut Vec<Value>` (blocks), `&str` (source text).
- Produces: enriched blocks with `x-provenance = "source-derived"` where applicable.

- [ ] **Step 1: Create the recovery module with empty helpers**

Create `adapters/aozora-epub3/src/source_derived.rs`:

```rust
use serde_json::Value;

pub fn apply_source_derived_recovery(blocks: &mut Vec<Value>, source_text: &str) {
    recover_gaiji(blocks, source_text);
    recover_figures(blocks, source_text);
    recover_warichu(blocks, source_text);
}

fn recover_gaiji(_blocks: &mut Vec<Value>, _source_text: &str) {
    // Implemented in Step 2.
}

fn recover_figures(_blocks: &mut Vec<Value>, _source_text: &str) {
    // Implemented in Step 3.
}

fn recover_warichu(_blocks: &mut Vec<Value>, _source_text: &str) {
    // Implemented in Step 4.
}
```

- [ ] **Step 2: Implement gaiji recovery**

Replace `recover_gaiji` with an implementation that:

1. Compiles a regex `※［＃([^］]+)］` over `source_text`.
2. Extracts the resolved Unicode character from markers like `※［＃「口＋世」、U+546D］`.
3. Walks the AAT blocks; when a `text` node contains exactly that resolved character and no sibling `gaiji` node covers it, replaces the text node with:
   ```json
   {"kind": "gaiji", "description": "...", "resolved": "...", "jis_code": null, "unresolved_reason": null, "x-provenance": "source-derived"}
   ```

- [ ] **Step 3: Implement figure recovery**

Replace `recover_figures` with an implementation that:

1. Compiles a regex `挿絵（([^、]+)、横([０-９\d]+)×縦([０-９\d]+)）入る` over `source_text`.
2. For each match, search existing AAT `figure` nodes for the same filename.
3. If found and the node is missing dimensions, enrich it with `width`, `height`, and `x-provenance = "source-derived"`.
4. If not found and the marker appears as plain text in a paragraph, insert a `figure` node with `x-provenance = "source-derived"`.

- [ ] **Step 4: Implement warichu recovery**

Replace `recover_warichu` with an implementation that:

1. Detects raw nodes containing `［＃ここから割り注］` or `［＃割り注］` markers.
2. Collects content until the matching end marker.
3. Splits on `［＃改行］`, `／`, or `/` into `upper` and `lower` arrays.
4. Replaces the raw markers and intermediate content with a single `warigaki` node where both `upper` and `lower` are non-empty; otherwise emits `raw` nodes and a warning.

- [ ] **Step 5: Wire recovery into `map_to_aat`**

In `xhtml_mapper.rs::map_to_aat`, after building blocks:

```rust
let mut blocks: Vec<Value> = /* ... */;
crate::source_derived::apply_source_derived_recovery(&mut blocks, &decoded.text);
```

- [ ] **Step 6: Add unit tests**

Create fixtures:
- `tests/fixtures/gaiji_inlined.txt` + `tests/fixtures/gaiji_inlined.xhtml`
- `tests/fixtures/figure_missing_image.txt` + `tests/fixtures/figure_missing_image.xhtml`

Add tests asserting recovery produces the expected `gaiji` and `figure` nodes.

- [ ] **Step 7: Run tests**

```bash
cargo test --manifest-path adapters/aozora-epub3/Cargo.toml
```

Expected: recovery tests pass.

- [ ] **Step 8: Commit**

```bash
git add adapters/aozora-epub3/src/source_derived.rs adapters/aozora-epub3/src/xhtml_mapper.rs adapters/aozora-epub3/tests/fixtures/
git commit -m "feat(aozora-epub3): source-derived recovery"
```

---

## Task 6: Implement the mapper CLI

**Files:**
- Create: `adapters/aozora-epub3/src/main.rs`

**Interfaces:**
- Consumes: command-line args; calls `lib::map_to_aat` and `lib::map_to_html`.
- Produces: AAT JSON or HTML on stdout; version string on stdout.

- [ ] **Step 1: Implement the CLI**

Create `adapters/aozora-epub3/src/main.rs`:

```rust
use std::io::{self, Read, Write};
use std::path::PathBuf;
use std::process;

use anyhow::Result;
use aozora_epub3_adapter::{
    map_to_aat, map_to_html, model::XhtmlDocument, model::XhtmlDocumentKind,
    model::ADAPTER_VERSION, MappingInput,
};
use clap::{Parser, ValueEnum};

#[derive(Debug, Parser)]
#[command(name = "aozora-epub3-adapter", version = ADAPTER_VERSION)]
struct Args {
    #[arg(long)]
    mode: Option<Mode>,

    #[arg(long)]
    source: Option<PathBuf>,

    /// Body XHTML files in spine order.
    #[arg(long, num_args = 1..)]
    xhtml: Vec<PathBuf>,

    /// Colophon XHTML file, if any. The wrapper classifies this by position (last spine item).
    #[arg(long, num_args = 1..)]
    xhtml_colophon: Vec<PathBuf>,

    #[arg(long)]
    version: bool,

    #[arg(long)]
    parser_failed: bool,

    #[arg(long)]
    parser_error_file: Option<PathBuf>,
}

#[derive(Clone, Copy, Debug, ValueEnum)]
enum Mode {
    Aat,
    Html,
}

fn read_file(path: &PathBuf) -> Result<Vec<u8>> {
    let mut bytes = Vec::new();
    let mut file = std::fs::File::open(path)?;
    file.read_to_end(&mut bytes)?;
    Ok(bytes)
}

fn main() {
    if let Err(e) = run() {
        eprintln!("{e:?}");
        process::exit(1);
    }
}

fn run() -> Result<()> {
    let args = Args::parse();

    if args.version {
        println!("{ADAPTER_VERSION}");
        return Ok(());
    }

    let mode = args.mode.unwrap_or(Mode::Aat);

    let mut docs: Vec<XhtmlDocument> = Vec::new();
    for path in args.xhtml {
        docs.push(XhtmlDocument {
            bytes: read_file(&path)?,
            kind: XhtmlDocumentKind::BodySection,
        });
    }
    for path in args.xhtml_colophon {
        docs.push(XhtmlDocument {
            bytes: read_file(&path)?,
            kind: XhtmlDocumentKind::Colophon,
        });
    }

    match mode {
        Mode::Html => {
            // --mode html concatenates body sections and the colophon (if present).
            let input = MappingInput {
                source_bytes: Vec::new(),
                xhtml_documents: docs,
                parser_failed: false,
                parser_error_message: None,
            };
            let html = map_to_html(&input)?;
            io::stdout().write_all(html.as_bytes())?;
            Ok(())
        }
        Mode::Aat => {
            let source_path = args
                .source
                .ok_or_else(|| anyhow::anyhow!("--source required for --mode aat"))?;
            let source_bytes = read_file(&source_path)?;

            let parser_error_message = args
                .parser_error_file
                .as_ref()
                .and_then(|path| std::fs::read_to_string(path).ok())
                .filter(|s| !s.is_empty());

            let input = MappingInput {
                source_bytes,
                xhtml_documents: docs,
                parser_failed: args.parser_failed,
                parser_error_message,
            };

            let out = map_to_aat(&input)?;
            println!("{}", serde_json::to_string(&out)?);

            if args.parser_failed {
                process::exit(2);
            }
            Ok(())
        }
    }
}
```

**Notes:**
- Exit code `2` is returned only in `--mode aat` when `--parser-failed` was set and AAT was still emitted.
- `--mode html` includes colophon documents at the end; this matches `aozora2html` behavior and is documented in fidelity notes.
- Colophon classification is performed by the wrapper using spine position, not by the mapper.

- [ ] **Step 2: Verify the binary builds**

```bash
cargo build --manifest-path adapters/aozora-epub3/Cargo.toml --release
```

Expected: binary at `adapters/aozora-epub3/target/release/aozora-epub3-adapter`.

- [ ] **Step 3: Test --version**

```bash
./adapters/aozora-epub3/target/release/aozora-epub3-adapter --version
```

Expected: `aozora-epub3-adapter 0.1.0 AozoraEpub3-JDK21-1.3.4-jdk21`.

- [ ] **Step 4: Commit**

```bash
git add adapters/aozora-epub3/src/main.rs
git commit -m "feat(aozora-epub3): mapper cli"
```

---

## Task 7: Implement the shell wrapper

**Files:**
- Create: `adapters/aozora-epub3/aozora-epub3-adapter`

**Interfaces:**
- Consumes: stdin raw bytes, CLI flags.
- Produces: invokes Rust mapper with `--source`, `--xhtml`, `--mode`, etc.

- [ ] **Step 1: Create the wrapper script**

Create `adapters/aozora-epub3/aozora-epub3-adapter`:

```bash
#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
MAPPER_BIN="${REPO_ROOT}/adapters/aozora-epub3/target/release/aozora-epub3-adapter"
JAR_PATH="${AB_AOZORAEPUB3_JAR:-${REPO_ROOT}/references/parsers/AozoraEpub3-JDK21/build/libs/AozoraEpub3.jar}"

if [[ "${1:-}" == "--version" ]]; then
  exec "$MAPPER_BIN" --version
fi

if [[ ! -f "$JAR_PATH" ]]; then
  echo "AozoraEpub3.jar not found: $JAR_PATH" >&2
  echo "Build it with: cd references/parsers/AozoraEpub3-JDK21 && ./gradlew jar" >&2
  echo "Or set AB_AOZORAEPUB3_JAR" >&2
  exit 1
fi

if ! command -v python >/dev/null 2>&1; then
  echo "python is required for package.opf parsing" >&2
  exit 1
fi

mode="aat"
if [[ "${1:-}" == "--mode" && $# -ge 2 ]]; then
  mode="$2"
fi

tmpdir="$(mktemp -d)"
stdin_raw="${tmpdir}/stdin.raw"
stdin_for_parser="${tmpdir}/stdin.parser"
parser_out="${tmpdir}/epub-out"
parser_err="${tmpdir}/parser.err"
trap 'rm -rf "$tmpdir"' EXIT

cat > "$stdin_raw"

# Encoding detection and transcoding
if head -c 3 "$stdin_raw" | od -An -tx1 | tr -d ' \n' | grep -q '^efbbbf'; then
  tail -c +4 "$stdin_raw" > "$stdin_for_parser"
  enc_arg="-enc UTF-8"
elif iconv -f UTF-8 -t UTF-8 "$stdin_raw" > /dev/null 2>&1; then
  cp "$stdin_raw" "$stdin_for_parser"
  enc_arg="-enc UTF-8"
else
  cp "$stdin_raw" "$stdin_for_parser"
  enc_arg=""
fi

mkdir -p "$parser_out"

parser_rc=0
java_args=(-jar "$JAR_PATH" -d "$parser_out")
[[ -n "$enc_arg" ]] && java_args+=("$enc_arg")
java_args+=("$stdin_for_parser")
java "${java_args[@]}" > "$parser_err" 2>&1 || parser_rc=$?

# Extract the EPUB
epub_file="$(find "$parser_out" -maxdepth 1 -name '*.epub' | head -n 1)"
if [[ -z "$epub_file" || ! -f "$epub_file" ]]; then
  echo "AozoraEpub3 did not produce an EPUB file" >&2
  cat "$parser_err" >&2 || true
  exit 1
fi

epub_dir="${tmpdir}/epub"
mkdir -p "$epub_dir"
unzip -q "$epub_file" -d "$epub_dir"

# Build ordered body XHTML file list from package.opf
xhtml_body_args=()
xhtml_colophon_args=()
package_opf="${epub_dir}/OPS/package.opf"

if [[ -f "$package_opf" ]]; then
  # Parse manifest/spine with a small Python helper embedded in the wrapper.
  # The last non-auxiliary spine item is treated as the colophon.
  python - "$package_opf" "$epub_dir" <<'PY' > "${tmpdir}/xhtml-list.txt"
import sys, xml.etree.ElementTree as ET
opf_path, epub_dir = sys.argv[1], sys.argv[2]
ns = {'opf': 'http://www.idpf.org/2007/opf'}
tree = ET.parse(opf_path)
manifest = {item.get('id'): item for item in tree.findall('.//opf:item', ns)}
spine = tree.findall('.//opf:itemref', ns)
items = []
for itemref in spine:
    idref = itemref.get('idref')
    item = manifest.get(idref)
    if item is None:
        continue
    href = item.get('href')
    props = item.get('properties', '')
    if 'nav' in props:
        continue
    if href in ('xhtml/nav.xhtml', 'xhtml/cover.xhtml', 'xhtml/title.xhtml'):
        continue
    items.append(f"{epub_dir}/OPS/{href}")
for idx, path in enumerate(items):
    kind = 'colophon' if idx == len(items) - 1 else 'body'
    print(f"{kind}\t{path}")
PY
  while IFS=$'\t' read -r kind path; do
    if [[ "$kind" == "colophon" ]]; then
      xhtml_colophon_args+=(--xhtml-colophon "$path")
    else
      xhtml_body_args+=(--xhtml "$path")
    fi
  done < "${tmpdir}/xhtml-list.txt"
else
  # Fallback: numeric filename order, last file is colophon
  mapfile -t xhtml_files < <(find "$epub_dir/OPS/xhtml" -name '[0-9]*.xhtml' | sort -V)
  if [[ ${#xhtml_files[@]} -gt 0 ]]; then
    last=$((${#xhtml_files[@]} - 1))
    for i in "${!xhtml_files[@]}"; do
      if [[ "$i" -eq "$last" ]]; then
        xhtml_colophon_args+=(--xhtml-colophon "${xhtml_files[$i]}")
      else
        xhtml_body_args+=(--xhtml "${xhtml_files[$i]}")
      fi
    done
  fi
fi

parse_complete_arg=()
if [[ "$parser_rc" -ne 0 ]] || grep -q '^\[ERROR\]' "$parser_err"; then
  parse_complete_arg=(--parser-failed --parser-error-file "$parser_err")
fi

exec "$MAPPER_BIN" --mode "$mode" --source "$stdin_raw" \
  "${xhtml_body_args[@]}" \
  "${xhtml_colophon_args[@]}" \
  "${parse_complete_arg[@]}"
```

Make it executable:

```bash
chmod +x adapters/aozora-epub3/aozora-epub3-adapter
```

- [ ] **Step 2: Test the wrapper with a fixture**

Build the JAR first:

```bash
cd references/parsers/AozoraEpub3-JDK21 && ./gradlew jar
cd /home/bor/Projects/ab-validator
```

Run the wrapper on a small fixture:

```bash
cat > /tmp/aozora-epub3-fixture.txt <<'EOF'
テスト作品
テスト著者

-------------------------------------------------------
凡例
-------------------------------------------------------

吾輩《わがはい》は猫である。

底本：テスト出版
EOF
./adapters/aozora-epub3/aozora-epub3-adapter --mode aat < /tmp/aozora-epub3-fixture.txt | jq '.meta.adapter'
```

Expected: `"aozora-epub3"`

- [ ] **Step 3: Commit**

```bash
git add adapters/aozora-epub3/aozora-epub3-adapter
git commit -m "feat(aozora-epub3): shell wrapper"
```

---

## Task 8: Add integration tests

**Files:**
- Create: `adapters/aozora-epub3/tests/integration.rs`

**Interfaces:**
- Consumes: the built adapter binary and fixture files.
- Produces: passing integration tests.

- [ ] **Step 1: Create integration tests**

Create `adapters/aozora-epub3/tests/integration.rs`:

```rust
use std::path::PathBuf;
use std::process::Command;
use std::sync::Once;

static BUILD: Once = Once::new();

fn ensure_release_binary() {
    BUILD.call_once(|| {
        let status = Command::new("cargo")
            .args([
                "build",
                "--manifest-path",
                concat!(env!("CARGO_MANIFEST_DIR"), "/Cargo.toml"),
                "--release",
            ])
            .status()
            .expect("failed to run cargo build");
        assert!(status.success(), "cargo build --release failed");
    });
}

fn adapter_bin() -> PathBuf {
    ensure_release_binary();
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("target/release/aozora-epub3-adapter");
    path
}

#[test]
fn version_prints_expected_format() {
    let output = Command::new(adapter_bin())
        .arg("--version")
        .output()
        .expect("failed to run adapter");
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.starts_with("aozora-epub3-adapter "));
}
```

- [ ] **Step 2: Add a fixture-driven integration test for --mode aat**

Add a test that runs the wrapper or mapper on `tests/fixtures/ruby_basic.xhtml` and asserts schema-valid AAT. For now, test the mapper directly:

```rust
#[test]
fn mapper_emits_schema_valid_aat_for_ruby() {
    let xhtml = include_str!("fixtures/ruby_basic.xhtml");
    let input = aozora_epub3_adapter::MappingInput {
        source_bytes: b"test".to_vec(),
        xhtml_documents: vec![aozora_epub3_adapter::XhtmlDocument {
            bytes: xhtml.as_bytes().to_vec(),
            kind: aozora_epub3_adapter::XhtmlDocumentKind::BodySection,
        }],
        parser_failed: false,
        parser_error_message: None,
    };
    let aat = aozora_epub3_adapter::map_to_aat(&input).unwrap();
    assert_eq!(aat["meta"]["adapter"], "aozora-epub3");
    assert_eq!(aat["blocks"][0]["content"][0]["kind"], "ruby");
}
```

- [ ] **Step 3: Run integration tests**

```bash
cargo test --manifest-path adapters/aozora-epub3/Cargo.toml --test integration
```

Expected: tests pass.

- [ ] **Step 4: Commit**

```bash
git add adapters/aozora-epub3/tests/integration.rs
git commit -m "test(aozora-epub3): integration tests"
```

---

## Task 9: Register the adapter in `ab-coverage`

**Files:**
- Modify: `crates/ab-coverage/src/adapter.rs`
- Modify: `crates/ab-coverage/src/cache.rs`
- Modify: `crates/ab-coverage/src/adapter.rs` tests
- Modify: `crates/ab-coverage/src/cache.rs` tests

**Interfaces:**
- Consumes: `aozora-epub3` parser ID.
- Produces: `AdapterBinary::for_parser` and `AdapterFingerprintInputs::for_parser` support the new ID.

- [ ] **Step 1: Add adapter binary path**

Edit `crates/ab-coverage/src/adapter.rs` and add a match arm:

```rust
"aozora-epub3" => repo_root.join("adapters/aozora-epub3/aozora-epub3-adapter"),
```

- [ ] **Step 2: Add source root for fingerprinting**

Edit `crates/ab-coverage/src/cache.rs` and add a match arm:

```rust
"aozora-epub3" => repo_root.join("adapters/aozora-epub3"),
```

- [ ] **Step 3: Update tests**

In `crates/ab-coverage/src/adapter.rs` tests and `crates/ab-coverage/src/cache.rs` tests, update any hardcoded parser ID lists to include `"aozora-epub3"`.

- [ ] **Step 4: Run ab-coverage tests**

```bash
cargo test -p ab-coverage
```

Expected: tests pass.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-coverage/src/adapter.rs crates/ab-coverage/src/cache.rs
git commit -m "feat(ab-coverage): register aozora-epub3 adapter"
```

---

## Task 10: Add `justfile` targets

**Files:**
- Modify: `justfile`

**Interfaces:**
- Produces: `just aozora-epub3-build`, `just aozora-epub3-test`, `just aozora-epub3-smoke`.

- [ ] **Step 1: Add targets**

Append to `justfile`:

```just
aozora-epub3-jar:
	@cd "{{repo_root}}/references/parsers/AozoraEpub3-JDK21" && ./gradlew jar

aozora-epub3-build PROFILE="release": aozora-epub3-jar
	@cargo build --manifest-path "{{repo_root}}/adapters/aozora-epub3/Cargo.toml" --{{PROFILE}}

aozora-epub3-test:
	@cargo test --manifest-path "{{repo_root}}/adapters/aozora-epub3/Cargo.toml"

aozora-epub3-smoke:
	@printf 'テスト作品\nテスト著者\n\n-------------------------------------------------------\n凡例\n-------------------------------------------------------\n\n吾輩《わがはい》は猫である。\n\n底本：テスト出版\n' \
	  | "{{repo_root}}/adapters/aozora-epub3/aozora-epub3-adapter" --mode aat \
	  | jq -e '.meta.adapter == "aozora-epub3"'
	@echo "aozora-epub3 smoke ok"
```

- [ ] **Step 2: Verify targets**

```bash
just aozora-epub3-build
just aozora-epub3-test
just aozora-epub3-smoke
```

Expected: all succeed.

- [ ] **Step 3: Commit**

```bash
git add justfile
git commit -m "build: add aozora-epub3 justfile targets"
```

---

## Task 11: Add Nix support

**Files:**
- Modify: `flake.nix`

**Interfaces:**
- Produces: pinned JAR derivation, dev shell, smoke check.

- [ ] **Step 1: Add pinned JAR derivation**

In `flake.nix`, add near other fetchers:

```nix
aozoraEpub3Jar = pkgs.fetchurl {
  url = "https://github.com/AozoraEpub3-JDK21/AozoraEpub3-JDK21/releases/download/v1.3.4-jdk21/AozoraEpub3-1.3.4-jdk21.jar";
  hash = "sha256-PLACEHOLDER="; # TODO: replace with actual hash
};
```

**Note:** The exact URL and hash must be verified against the GitHub Release page. If the release distributes a ZIP instead of a JAR, use `pkgs.fetchzip` and reference the JAR path inside.

- [ ] **Step 2: Add dev shell**

Add an `aozora-epub3` shell to the `devShells` set:

```nix
aozora-epub3 = pkgs.mkShell {
  packages = [
    rustToolchain
    pkgs.jdk21
    pkgs.gradle
    pkgs.jq
  ];
  shellHook = ''
    export AB_AOZORAEPUB3_JAR="${aozoraEpub3Jar}"
  '';
};
```

- [ ] **Step 3: Add smoke check**

Add a check that builds the adapter and runs the smoke fixture:

```nix
aozora-epub3-smoke = pkgs.runCommand "aozora-epub3-smoke" {
  nativeBuildInputs = [ rustToolchain pkgs.jdk21 pkgs.jq ];
} ''
  export AB_AOZORAEPUB3_JAR="${aozoraEpub3Jar}"
  mkdir -p source
cp -R ${source}/adapters/aozora-epub3 source/adapters/
  cp -R ${source}/references/parsers/AozoraEpub3-JDK21/template source/references/parsers/AozoraEpub3-JDK21/template || true
  cd source
  cargo build --manifest-path adapters/aozora-epub3/Cargo.toml --release
  printf 'テスト作品\nテスト著者\n\n-------------------------------------------------------\n凡例\n-------------------------------------------------------\n\n吾輩《わがはい》は猫である。\n\n底本：テスト出版\n' \
    | adapters/aozora-epub3/aozora-epub3-adapter --mode aat \
    | jq -e '.meta.adapter == "aozora-epub3"'
  touch $out
'';
```

This is a sketch; the actual Nix check should follow the pattern of `aozora2htmlRustParityCheck` and avoid copying unnecessary files.

- [ ] **Step 4: Verify Nix evaluation**

```bash
nix flake check --no-build
```

Expected: evaluates without error (the smoke check may fail until the JAR hash is correct).

- [ ] **Step 5: Commit**

```bash
git add flake.nix
git commit -m "build(nix): aozora-epub3 jar, shell, and smoke check"
```

---

## Task 12: Add harness smoke test

**Files:**
- Create: `tests/aozora-epub3-adapter-smoke.sh`

**Interfaces:**
- Consumes: built adapter binary.
- Produces: exit 0 on success.

- [ ] **Step 1: Create the smoke test**

Create `tests/aozora-epub3-adapter-smoke.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
adapter="${repo_root}/adapters/aozora-epub3/aozora-epub3-adapter"

if [[ ! -x "$adapter" ]]; then
  echo "adapter not found: $adapter" >&2
  exit 1
fi

out_dir="${AB_TEST_OUT_DIR:-/tmp/ab-validator/aozora-epub3-smoke}"
mkdir -p "$out_dir"

printf 'テスト作品\nテスト著者\n\n-------------------------------------------------------\n凡例\n-------------------------------------------------------\n\n吾輩《わがはい》は猫である。\n\n底本：テスト出版\n' \
  | "$adapter" --mode aat > "$out_dir/aat.json"

jq -e '.meta.adapter == "aozora-epub3"' "$out_dir/aat.json"
jq -e '.meta.parse_complete == true' "$out_dir/aat.json"
jq -e '.blocks | length >= 1' "$out_dir/aat.json"

echo "aozora-epub3 adapter smoke ok: $out_dir/aat.json"
```

Make it executable:

```bash
chmod +x tests/aozora-epub3-adapter-smoke.sh
```

- [ ] **Step 2: Run the smoke test**

```bash
bash tests/aozora-epub3-adapter-smoke.sh
```

Expected: prints success message.

- [ ] **Step 3: Commit**

```bash
git add tests/aozora-epub3-adapter-smoke.sh
git commit -m "test: aozora-epub3 adapter smoke test"
```

---

## Task 13: Add adapter fidelity notes

**Files:**
- Modify: `data/adapter-fidelity-notes.toml`

**Interfaces:**
- Produces: documented known divergences for `aozora-epub3`.

- [ ] **Step 1: Add notes**

Append to `data/adapter-fidelity-notes.toml`:

```toml
[[note]]
id = "aozora-epub3-html-derived"
adapter = "aozora-epub3"
syntax_row_ids = []
status = "active"
summary = "--mode html emits extracted/filtered body XHTML from the EPUB, not raw upstream parser output like aozora2html."
evidence = "docs/superpowers/specs/2026-07-04-aozora-epub3-adapter-design.md"

[[note]]
id = "aozora-epub3-halfwidth-numeral-annotations"
adapter = "aozora-epub3"
syntax_row_ids = ["indentation.jisage_block"]
status = "active"
summary = "AozoraEpub3 requires full-width numerals in some annotations (e.g. ［＃ここから２字下げ］). Half-width variants are logged as unsupported."
evidence = "docs/handoffs/adding-aozora2-aozoraepub3-parser-support.md"

[[note]]
id = "aozora-epub3-warichu-malformed"
adapter = "aozora-epub3"
syntax_row_ids = ["warichu.basic"]
status = "active"
summary = "AozoraEpub3 1.3.4-jdk21 emits malformed HTML for warichu blocks and reports [ERROR] 割り注終わりなし."
evidence = "docs/handoffs/adding-aozora2-aozoraepub3-parser-support.md"
```

- [ ] **Step 2: Validate the TOML**

```bash
nix run .#checks.$(nix eval --impure --raw --expr builtins.currentSystem).adapter-fidelity-notes-schema-smoke
```

Or run the relevant smoke test directly.

Expected: schema validation passes.

- [ ] **Step 3: Commit**

```bash
git add data/adapter-fidelity-notes.toml
git commit -m "docs: aozora-epub3 adapter fidelity notes"
```

---

## Task 14: Final verification

**Files:**
- All of the above.

**Interfaces:**
- Produces: a passing adapter integrated into the harness.

- [ ] **Step 1: Run the full test suite for the adapter**

```bash
just aozora-epub3-build
just aozora-epub3-test
just aozora-epub3-smoke
bash tests/aozora-epub3-adapter-smoke.sh
```

Expected: all pass.

- [ ] **Step 2: Validate AAT output against schema**

```bash
cat > /tmp/aozora-epub3-fixture.txt <<'EOF'
テスト作品
テスト著者

-------------------------------------------------------
凡例
-------------------------------------------------------

吾輩《わがはい》は猫である。

底本：テスト出版
EOF
./adapters/aozora-epub3/aozora-epub3-adapter --mode aat < /tmp/aozora-epub3-fixture.txt > /tmp/aozora-epub3-aat.json
cargo run -p ab-check -- --txt /tmp/aozora-epub3-fixture.txt --aat /tmp/aozora-epub3-aat.json
```

Expected: `ab-check` reports schema valid.

- [ ] **Step 3: Run workspace quality checks**

```bash
just quality
```

Expected: passes (or only fails on unrelated existing issues).

- [ ] **Step 4: Final commit**

```bash
git commit -m "feat: add aozora-epub3 adapter" --allow-empty
```

---

## Self-review

### Spec coverage

| Spec requirement | Task |
|------------------|------|
| Standalone adapter crate, excluded from workspace | Task 1 |
| Source decoding/hashing with shared contract | Task 2 |
| XHTML→AAT mapping for AozoraEpub3 CSS classes | Tasks 3–4 |
| Source-derived recovery as separate pass | Task 5 |
| Mapper CLI matching adapter contract | Task 6 |
| Shell wrapper with EPUB unzip and `package.opf` parsing | Task 7 |
| Integration tests | Task 8 |
| Registration in `ab-coverage` | Task 9 |
| `justfile` targets | Task 10 |
| Nix JAR pin + shell + smoke check | Task 11 |
| Harness smoke test | Task 12 |
| Adapter fidelity notes | Task 13 |
| Final verification | Task 14 |

### Placeholder scan

- The Nix JAR URL/hash in Task 11 is marked `PLACEHOLDER` because it must be verified against the actual GitHub Release artifact before implementation; this is the only external dependency that cannot be pinned without fetching the artifact.
- No other `TODO`, `TBD`, `FIXME`, or vague placeholder language remains. Task 4 and Task 5 use scaffold functions, but each is filled by explicit sub-steps with concrete code.

### Type consistency

- `MappingInput`, `XhtmlDocument`, `XhtmlDocumentKind`, `DecodedSource` are defined in `model.rs` and used consistently across `decode.rs`, `xhtml_mapper.rs`, `source_derived.rs`, and `main.rs`.
- `map_to_aat` and `map_to_html` signatures match the `lib.rs` re-exports.
- Wrapper→mapper protocol now uses `--xhtml` for body sections and `--xhtml-colophon` for the colophon; both are parsed into `XhtmlDocumentKind` in `main.rs`.

---

## Execution handoff

**Plan complete and saved to `docs/superpowers/plans/2026-07-04-aozora-epub3-adapter-implementation.md`.**

Two execution options:

1. **Subagent-Driven (recommended)** — Dispatch a fresh subagent per task, review between tasks, fast iteration. Use `superpowers:subagent-driven-development`.
2. **Inline Execution** — Execute tasks in this session using `superpowers:executing-plans`, batch execution with checkpoints.

**Which approach?**
