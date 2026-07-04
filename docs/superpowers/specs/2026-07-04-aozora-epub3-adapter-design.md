# AozoraEpub3-JDK21 Adapter Design

**Date:** 2026-07-04  
**Status:** Design spec / provisional — reviewed with `rich-hickey-review` and `deepening-review` lenses  
**Scope:** Deepen the design for adding an `aozora-epub3` adapter to `ab-validator` at the same level as `aozora2`, `aozora-rs`, and `aozora2html`.  
**Prerequisite reading:** `docs/handoffs/adding-aozora2-aozoraepub3-parser-support.md`

---

## Review summary

This design was reviewed for entanglement, seam placement, values-vs-places, protocol boundaries, and trust boundaries. The main improvements from the review are:

1. The wrapper-to-mapper seam is now **value-based** (bytes in, AAT value out) instead of file-system-based.
2. EPUB document classification moves from filename heuristics to parsing **`package.opf`**, which is explicit and versionable.
3. The shared-library recommendation is split into **three concrete options** with a clear default and escape hatch, because the DOM structures of `aozora2html` and `AozoraEpub3` differ enough that a pure class-name table is a fake seam.
4. Source-derived recovery is treated as a **separate pass** over AAT values, not a hook braided into the mapper.
5. Trust boundaries (GPL source, Nix artifact) are made explicit with verification requirements.

## Critical review response

A second-pass review surfaced additional issues. The spec has been updated to address them:

| Finding | Severity | Resolution in this spec |
|---------|----------|------------------------|
| `--mode html` is a derived extraction, not raw upstream output | Strong suggestion | §8 documents it as derived; fidelity notes must record the difference. |
| Encoding path for JAR input (`-enc UTF-8` vs default MS932) ambiguous | Question | §3.1 now specifies: Shift_JIS input passes as-is; UTF-8 input transcodes and uses `-enc UTF-8`. |
| `--parser-failed` trigger conditions underspecified | Strong suggestion | §3.1 and §8 now specify: `[ERROR]` → `parser_failed`; `[WARN]` / `注記未変換` → warnings. |
| Colophon detection heuristic fragile | Strong suggestion | §5 adds defense-in-depth: content heuristic + last-spine-item fallback + never-silent-drop rule. |
| No fallback for `package.opf` parse failure | Strong suggestion | §5 adds filename-based fallback with warning. |
| Shared-primitive extraction couples epub3 to `aozora2html` refactoring | Strong suggestion | §4 now recommends Option A first (standalone mapper), extraction later. |
| `--version` should not depend on JAR presence | Nit | §8 now says version is hardcoded in Rust mapper; JAR not touched. |
| Figure source-derived recovery vs existing `<img>` nodes underspecified | Question | §7 now specifies enrichment without duplication. |
| Temp file cleanup unmentioned | Nit | §3.4 now specifies `mktemp` + `trap ... EXIT` cleanup. |

---

## 1. Problem statement

`AozoraEpub3-JDK21` is a GPL v3 Java/Gradle tool that converts Aozora Bunko text into EPUB 3. It is an upstream reference parser that the harness should evaluate, but it is not yet a first-class adapter. The integration must:

1. Respect the existing adapter wire contract (stdin bytes → AAT JSON/HTML on stdout, `--version`).
2. Not copy GPL code into project-owned source or data.
3. Fit into the existing build, test, and Nix infrastructure.
4. Produce AAT comparable to the existing adapters so cross-parser analysis is meaningful.

---

## 2. Design goals and constraints

| Goal | How we satisfy it |
|------|-------------------|
| Adapter parity | Same CLI contract, same `meta` fields, same exit-code semantics as existing adapters. |
| GPL isolation | Upstream source stays under `references/parsers/AozoraEpub3-JDK21/`; adapter invokes the JAR as a subprocess. |
| Leverage / locality | Share the *stable* parts (source decoding, AAT envelope, source-derived recovery primitives). Keep parser-specific DOM interpretation in each adapter unless a clean shared engine emerges. |
| Testability | Mapper unit tests take byte values, not disk paths. Wrapper integration tests exercise the full pipeline. |
| Reproducibility | Nix checks use a pinned release JAR with verified SHA-256; local dev can build from source or override the JAR path. |

---

## 3. Module decomposition

```
┌─────────────────────────────────────────────────────────────┐
│  aozora-epub3-adapter  (shell wrapper)                      │
│  stdin bytes → temp file → java -jar → EPUB → unzip         │
│  → read package.opf → invoke Rust mapper                    │
└──────────────────────┬──────────────────────────────────────┘
                       │ value-based protocol
                       ▼
┌─────────────────────────────────────────────────────────────┐
│  aozora-epub3 Rust mapper                                   │
│  parse XHTML bytes → normalize DOM → AAT JSON/HTML          │
│  optionally: source-derived recovery pass                   │
└─────────────────────────────────────────────────────────────┘
```

### 3.1 `aozora-epub3-adapter` (shell wrapper)

- **Role:** Process-level adapter. The only thing the harness sees.
- **Interface (external seam):**
  ```bash
  aozora-epub3-adapter --mode aat < input.txt > output.aat.json
  aozora-epub3-adapter --mode html < input.txt > output.html
  aozora-epub3-adapter --version
  ```
- **Depth:** Tiny surface; hides encoding, temp files, Java invocation, EPUB unzip, package.opf parsing, and mapper invocation.
- **Values vs places:** It owns the temporary filesystem state internally. Nothing mutable is shared across invocations. The mapper receives values (bytes), not paths.

Responsibilities:
1. Read raw stdin bytes; write to a temp `.txt` file.
2. Detect encoding with the shared contract (UTF-8 BOM / UTF-8 / Shift_JIS fallback).
3. Choose the JAR input encoding:
   - If the detected encoding is Shift_JIS / Windows-31J, pass the bytes as-is and do **not** pass `-enc UTF-8` (the JAR defaults to MS932).
   - If the detected encoding is UTF-8 (with or without BOM), transcode to UTF-8 without BOM and pass `-enc UTF-8`.
   This avoids an unnecessary Shift_JIS → UTF-8 → Shift_JIS round-trip.
4. Keep the original raw bytes for `source_hash` / `source_encoding` metadata.
5. Locate the JAR (`AB_AOZORAEPUB3_JAR` → `references/parsers/AozoraEpub3-JDK21/build/libs/AozoraEpub3.jar` → error).
6. Run `java -jar AozoraEpub3.jar -d <tmpdir> [-enc UTF-8] <input>`.
7. Unzip the resulting EPUB.
8. Parse `OPS/package.opf` to determine the spine order and identify auxiliary documents (`nav`, `cover`, `title`).
9. Detect whether the run produced warnings/errors from stderr:
   - Any line starting with `[ERROR]` → set `--parser-failed`.
   - Any line starting with `[WARN]` or `[INFO] 注記未変換` → collect as AAT warnings.
10. Invoke the Rust mapper with:
    - `--source <original-raw-bytes-file>`
    - repeated `--xhtml <body-section-file>` in spine order
    - `--mode aat|html`
    - `--parser-failed` and `--parser-error-file <stderr>` if upstream logged errors

### 3.2 Wrapper → mapper protocol

The wrapper passes **ordered body XHTML files** to the mapper. The mapper does not need to know about EPUB archives or filename conventions.

Command shape:

```bash
aozora-epub3-mapper \
  --source <raw-bytes-file> \
  --xhtml <body-0001.xhtml> \
  --xhtml <body-0002.xhtml> \
  ... \
  --mode aat \
  [--parser-failed] \
  [--parser-error-file <stderr-file>]
```

**Why this protocol?**

- It is explicit about ordering (spine order).
- It removes EPUB knowledge from the mapper.
- It makes unit tests easy: pass `--xhtml` fixtures directly.
- It mirrors `aozora2html`'s `--source` + `--xhtml` shape, so the mapper CLI pattern is familiar.

### 3.3 `aozora-epub3` Rust mapper

- **Role:** Convert AozoraEpub3's rendered XHTML bytes into AAT JSON or HTML.
- **Interface (internal seam):**
  ```rust
  pub struct MappingInput {
      pub source_bytes: Vec<u8>,
      pub xhtml_documents: Vec<XhtmlDocument>,
      pub parser_failed: bool,
      pub parser_error_message: Option<String>,
  }

  pub struct XhtmlDocument {
      pub bytes: Vec<u8>,
      pub kind: XhtmlDocumentKind, // BodySection or Colophon
  }

  pub fn map_to_aat(input: &MappingInput) -> anyhow::Result<serde_json::Value>;
  pub fn map_to_html(input: &MappingInput) -> anyhow::Result<String>;
  ```
- **Depth:** Callers hand over bytes and a classification; the mapper hides DOM parsing, semantic mapping, source-derived recovery, and AAT envelope construction.
- **Composition:** `map_to_aat` is a pure function of the input value. Tests can construct `MappingInput` directly without touching the filesystem.

### 3.4 Temp file lifecycle

The wrapper creates three kinds of temporary state per invocation:

1. A temp `.txt` file holding the (possibly transcoded) source bytes.
2. A temp output directory where AozoraEpub3 writes the `.epub`.
3. A temp directory where the EPUB is unzipped.

All three must be created with `mktemp` / `mktemp -d` and cleaned up with a `trap ... EXIT` (or equivalent) so long-running harness runs do not leak files. The original raw stdin bytes are also written to a temp file for hashing, but this file is passed to the mapper and removed by the wrapper after the mapper exits.

---

## 4. The shared-library question: three options

Both `aozora2html` and `aozora-epub3` need source decoding, AAT envelope construction, visible-text projection, and source-derived recovery. They also both walk XHTML and map CSS classes to AAT. The question is how much of the XHTML walk to share.

### Option A: Standalone `aozora-epub3` mapper, extract later

Copy and adapt the needed parts from `aozora2html` into a new crate. No shared abstraction until both adapters are working and the common pattern is obvious.

- **Pros:** Fastest path to a working adapter; no risk of premature abstraction; no change to `aozora2html`.
- **Cons:** Duplication; fixes to common logic must be applied twice.
- **When to choose:** When speed-to-adapter matters more than long-term maintenance, or when the structural differences turn out to be larger than expected.

### Option B: Shared toolkit of small, deep primitives

Extract only the *stable, parser-agnostic* utilities into a shared crate (`crates/ab-xhtml-aat/` or similar):

- `decode_source_bytes(bytes) -> DecodedSource`
- `source_hash(bytes) -> String`
- `parse_failure_envelope(...) -> Value`
- `visible_text(node) -> String`
- `normalize_figure_alt(text) -> String`
- `apply_source_derived_recovery(blocks, source_text, config) -> ()`

Each adapter keeps its own DOM-walking mapper that calls these primitives.

- **Pros:** Low-risk sharing; keeps adapter-specific DOM interpretation local; tests are small and focused.
- **Cons:** Some duplication in the DOM walk remains.
- **When to choose:** Recommended default. It shares what is genuinely common without forcing a fake unified model.

### Option C: Shared configurable XHTML→AAT engine

Build a single engine that both adapters configure with rules (selectors, class-name tables, etc.).

- **Pros:** One place for the complex walk; new XHTML-based adapters become cheap.
- **Cons:** High upfront design cost; the rule interface is likely to grow and become shallow because the two parsers emit structurally different HTML (e.g., `aozora2html` uses `<h2>` for headings, AozoraEpub3 uses `<div class="chap2">`).
- **Verdict:** **Strong suggestion to defer.** Start with Option A or B. Only pursue Option C after both adapters work and the real overlap is measurable.

**Provisional decision:** Option A first, then Option B.

Start with a standalone `aozora-epub3` mapper that copies/adapts the needed primitives from `aozora2html`. This avoids coupling the new adapter delivery to a refactoring of the existing, working `aozora2html` adapter. After both adapters are stable and the real overlap is measurable, extract shared primitives into `crates/ab-xhtml-aat/` as a separate, behavior-preserving refactoring.

---

## 5. EPUB extraction and document classification

AozoraEpub3 writes:

```text
OPS/xhtml/0001.xhtml   <- body section
OPS/xhtml/0002.xhtml   <- body section or colophon
...
OPS/xhtml/nav.xhtml    <- EPUB navigation document
OPS/xhtml/cover.xhtml  <- optional cover page
OPS/xhtml/title.xhtml  <- optional title page
```

The wrapper uses **`OPS/package.opf`** for classification instead of filename heuristics.

Example package.opf fragment:

```xml
<manifest>
  <item id="xhtml_0001" href="xhtml/0001.xhtml" media-type="application/xhtml+xml"/>
  <item id="nav" href="xhtml/nav.xhtml" properties="nav"/>
</manifest>
<spine>
  <itemref idref="xhtml_0001"/>
  ...
</spine>
```

Wrapper algorithm:

1. Parse `package.opf` if present and well-formed.
2. Build a map from `idref` → `href` using `<manifest>` + `<spine>`.
3. Skip items whose `properties` contains `nav` or whose `href` is `xhtml/cover.xhtml` or `xhtml/title.xhtml`.
4. For remaining spine items, read the file bytes.
5. Classify each file as `BodySection` or `Colophon` by content: if the first non-empty paragraph starts with `底本：` (full-width colon) or `底本:` (half-width colon), it is a colophon; otherwise body.
6. Fallback: if `package.opf` is missing or unparseable, list `OPS/xhtml/0001.xhtml`, `0002.xhtml`, … in lexical order, log a warning, and treat the last file as colophon if it starts with `底本：`.
7. Pass classified documents to the mapper via `--xhtml`.

**Colophon classification defense-in-depth:**

- Primary: first non-empty paragraph starts with `底本：`.
- Secondary: the file is the last spine item and starts with `底本：` anywhere in the first paragraph.
- Tertiary: if neither matches, classify all sections as `BodySection` and add a warning. Never silently drop a section.

**Why package.opf?**

- It is the canonical EPUB reading-order contract.
- It removes fragile filename assumptions.
- It is stable across AozoraEpub3 versions unless the tool breaks EPUB compliance.

---

## 6. XHTML → AAT mapping

AozoraEpub3 emits characteristic CSS classes. The mapper maps element + class combinations to AAT kinds.

### Block-level mappings

| Source | AozoraEpub3 XHTML | AAT |
|--------|-------------------|-----|
| paragraph | `<p>...</p>` | `paragraph` |
| empty line | `<p><br/></p>` | empty `paragraph` |
| heading (level 1) | `<div class="chap1">X</div>` | `heading` level 1, style `normal` |
| heading (level 2) | `<div class="chap2">X</div>` | `heading` level 2, style `normal` |
| heading (level 3) | `<div class="chap3">X</div>` | `heading` level 3, style `normal` |
| inline heading (large) | `<span class="font5">X</span>` | `heading` level 1, style `dogyo` |
| inline heading (medium) | `<span class="font3">X</span>` | `heading` level 2, style `dogyo` |
| inline heading (small) | `<span class="font1">X</span>` | `heading` level 3, style `dogyo` |
| jisage block | `<div class="ptN">...</div>` | `jisage_block` with `x-indent` = N |
| keigakomi | `<div class="border">...</div>` | `keigakomi_block` |
| keigakomi dashed | `<div class="dashed_border">...</div>` | `keigakomi_block` with `x-line-kind` = `dashed` |
| yokogumi block | `<div class="yoko">...</div>` | `yokogumi_block` |

### Inline mappings

| Source | AozoraEpub3 XHTML | AAT |
|--------|-------------------|-----|
| ruby | `<ruby>base<rt>reading</rt></ruby>` | `ruby` |
| bold | `<span class="b">X</span>` | `style` `bold` |
| italic | `<span class="i">X</span>` | `style` `italic` |
| boten sesame | `<span class="sesame">X</span>` | `style` `boten`, `x-boten-kind` = `sesame` |
| boten dot | `<span class="dot">X</span>` | `style` `boten`, `x-boten-kind` = `black_circle` |
| boten open sesame | `<span class="open_sesame">X</span>` | `style` `boten`, `x-boten-kind` = `white_sesame` |
| boten open dot | `<span class="open_dot">X</span>` | `style` `boten`, `x-boten-kind` = `white_circle` |
| boten left variants | `<span class="left_emp sesame">X</span>` | `style` `boten` with `x-placement` = `left` |
| underline | `<span class="underline">X</span>` | `style` `bousen`, `x-line-kind` = `solid` |
| double underline | `<span class="double_underline">X</span>` | `style` `bousen`, `x-line-kind` = `double` |
| left underline | `<span class="left_underline">X</span>` | `style` `bousen` with `x-placement` = `left` |
| tcy | `<span class="tcy"><span>X</span></span>` | `tcy` |
| warichu | `<span class="wrc">X</span>` | `warigaki` (when well-formed) |
| line break | `<br/>` | `text` with `x-break-kind` = `line` |
| resolved gaiji | plain character | `text` |

### Figure mappings

AozoraEpub3 emits `<img>` only when the referenced image file is present. The mapper creates `figure` nodes with `filename`, `alt`, `width`, `height` when available.

---

## 7. Source-derived recovery as a separate pass

AozoraEpub3 renders some source annotations into plain text or drops them. Instead of braiding source-text knowledge into the XHTML mapper, the mapper first produces AAT from the rendered XHTML, then a separate pass enriches the AAT from the source text.

This mirrors `aozora2html`'s existing structure:

```rust
let mut blocks = map_blocks_from_xhtml(...)?;
apply_source_derived_recovery(&mut blocks, &decoded.text, &config);
```

The shared primitive crate can provide `apply_source_derived_recovery` and the configuration type. The `aozora-epub3` adapter configures which recoveries are enabled.

Cases:

1. **Gaiji.** AozoraEpub3 inlines resolved characters. Scan source for `※［＃...］` and reconstruct `gaiji` nodes where the rendered text matches the resolved character.
2. **Figures.**
   - If `挿絵（...）入る` survived as plain text (image missing), reconstruct a `figure` node from the source marker.
   - If an `<img>`-based `figure` already exists and a matching source marker is found, enrich the existing node with source-derived `filename`, `width`, `height`, or `alt` only when the XHTML-derived value is missing or less specific.
   - Never create a duplicate `figure` node for the same source marker.
3. **Warichu.** When upstream emits malformed HTML, emit `raw` nodes and optionally reconstruct `warigaki` from clear source markers.
4. **Unsupported annotations.** Annotations logged as `注記未変換` can be preserved as `raw` nodes with `x-error-kind`.

Mark all source-derived nodes with `x-provenance = "source-derived"`.

---

## 8. Adapter wire contract details

### `--mode aat`

- Reads raw bytes from stdin.
- Writes a single AAT JSON object to stdout.
- Exit codes (same as `aozora2html`):
  - `0` — mapper produced schema-valid AAT.
  - `1` — fatal error (JAR missing, stdin undecodable, mapper panic).
  - `2` — partial success; AAT is emitted but `meta.parse_complete` is `false`.

`--parser-failed` is set when upstream logged one or more `[ERROR]` lines on stderr, or when the JAR exits non-zero. `[WARN]` lines are collected as AAT warnings but do **not** trigger `--parser-failed`.

### `--mode html`

- Writes the concatenated body XHTML sections to stdout, in spine order, separated by XML comments marking section boundaries.
- **Important:** unlike `aozora2html`, which emits raw upstream XHTML, `aozora-epub3` emits an **extracted and filtered** view of the EPUB body. This is a derived format, not raw upstream output. Cross-adapter `ab-render-diff` comparisons must account for this semantic difference; it should be recorded in `data/adapter-fidelity-notes.toml`.
- Does **not** include `nav.xhtml`, `cover.xhtml`, or `title.xhtml`.

### `--version`

Prints one line:

```text
aozora-epub3-adapter 0.1.0 AozoraEpub3-JDK21-1.3.4-jdk21
```

The version string is hardcoded in the Rust mapper as a constant (mirroring `aozora2html`'s `ADAPTER_VERSION`). `--version` is handled by the Rust mapper directly and does not require the JAR to be present. The upstream version component is updated when the pinned JAR is updated.

An optional build-time environment variable `AB_AOZORAEPUB3_VERSION` can inject a different version string for development or fork builds.

---

## 9. Build and Nix design

### Local development

```bash
cd references/parsers/AozoraEpub3-JDK21
./gradlew jar
just aozora-epub3-build
just aozora-epub3-test
```

The wrapper finds the JAR at `references/parsers/AozoraEpub3-JDK21/build/libs/AozoraEpub3.jar` by default.

### Nix / CI

Because Gradle has no dependency locks, the adapter cannot be built purely in Nix from source today.

**Recommended:** Use a pinned release JAR for Nix checks.

- Fetch `AozoraEpub3-1.3.4-jdk21.jar` from GitHub Releases via `pkgs.fetchurl`.
- Verify the SHA-256 published on the release page or compute it from a known-good download.
- Expose `AB_AOZORAEPUB3_JAR` override for local development.

**Trust boundary note:** A fetched binary JAR crosses a trust boundary. The Nix derivation must pin the SHA-256, and the release artifact should be attested (GitHub artifact attestation is mentioned in upstream `README.md`).

### `flake.nix` additions

- `aozoraEpub3Jar` — `pkgs.fetchurl` derivation for the pinned release JAR.
- `aozora-epub3` devShell — JDK 21 + Gradle for source builds.
- `checks.<system>.aozora-epub3-smoke` — builds the adapter and runs a fixture through it using the pinned JAR.

---

## 10. Test strategy

### Unit tests (Rust mapper)

- Fixture XHTML bytes covering each mapping row in §6.
- Assert expected AAT JSON structure.
- Test `package.opf` parsing and spine ordering in the wrapper.
- Test source-derived recovery with paired source+XHTML fixtures.

### Integration tests (wrapper)

- Build the adapter.
- Run `echo '<aozora text>' | aozora-epub3-adapter --mode aat` and validate against `data/aat-schema.json`.
- Run `--version` and assert format.
- Run `--mode html` and assert it emits only body sections in spine order.

### Cross-adapter tests

- Run `ab-compare` between `aozora-epub3` and `aozora2html` on a small fixture set.
- Add fidelity notes for expected divergences (e.g., unsupported heading forms, strict full-width numerals, malformed warichu).

---

## 11. Files to create / modify

### New files

- `adapters/aozora-epub3/Cargo.toml`
- `adapters/aozora-epub3/src/main.rs`
- `adapters/aozora-epub3/src/lib.rs`
- `adapters/aozora-epub3/aozora-epub3-adapter` (shell wrapper)
- `adapters/aozora-epub3/README.md`
- `adapters/aozora-epub3/tests/fixtures/*.txt`
- `adapters/aozora-epub3/tests/fixtures/*.xhtml`
- `adapters/aozora-epub3/tests/fixtures/*.aat.json`
- `adapters/aozora-epub3/tests/integration.rs`
- `tests/aozora-epub3-adapter-smoke.sh`

### Modified files

- `crates/ab-coverage/src/adapter.rs` — add `aozora-epub3` ID and binary path.
- `crates/ab-coverage/src/cache.rs` — add `aozora-epub3` source root.
- `crates/ab-coverage/src/adapter.rs` tests and `cache.rs` tests — update parser lists.
- `justfile` — add build/test/full-corpus targets.
- `flake.nix` — add JAR fetch, shell, and smoke check.
- `data/adapter-fidelity-notes.toml` — add notes for known divergences.

### Follow-up files (after extraction)

- `crates/ab-xhtml-aat/Cargo.toml`
- `crates/ab-xhtml-aat/src/lib.rs`
- `adapters/aozora2html/src/lib.rs` — move `decode_source_bytes`, `source_hash`, and envelope helpers to `crates/ab-xhtml-aat`.

---

## 12. Open questions and provisional decisions

| # | Question | Provisional decision | Falsifier |
|---|----------|---------------------|-----------|
| 1 | Shared library scope | **Option A first**: standalone `aozora-epub3` mapper. Extract shared primitives only after both adapters are stable. | If the standalone mapper duplicates >200 lines of encoding/hashing/envelope code, escalate to Option B earlier. |
| 2 | Input normalization for half-width numerals | Do **not** normalize; faithfully report unsupported annotations as `raw` nodes or warnings. | If cross-adapter comparison becomes meaningless due to trivial numeric-format differences, reconsider a preprocessing normalizer. |
| 3 | Warichu malformed output | Emit `raw` nodes for the markers; set `parse_complete=false`; include a warning. | If upstream fixes warichu in a later release, update the mapper. |
| 4 | Release JAR pin | Pin `AozoraEpub3-1.3.4-jdk21.jar` from GitHub Releases with SHA-256. | If no fat JAR is published or no checksum is available, switch to source-build in a non-Nix CI step. |
| 5 | Shared crate location | Deferred until extraction. If extracted, prefer `crates/ab-xhtml-aat/`. | If it creates unwanted workspace coupling, move it under `adapters/aozora2html/crates/`. |
| 6 | `--mode html` exact format | Concatenated body XHTML sections in spine order, with XML comments marking section boundaries. Documented as a derived format, not raw upstream output. | If `ab-render-diff` needs raw EPUB bytes, escalate to adding EPUB awareness to `ab-render-diff`. |

---

## 13. Summary

The design introduces a shell-wrapper adapter that invokes `AozoraEpub3.jar`, unzips the resulting EPUB, parses `package.opf` for the reading order, and passes classified body XHTML sections as byte values to a Rust mapper. The mapper is a pure function from XHTML bytes + source bytes to AAT. Source-derived recovery is a separate pass over the AAT value, not a hook inside the mapper.

The adapter strategy is deliberately incremental: start with a standalone `aozora-epub3` mapper that copies/adapts the small primitives it needs from `aozora2html`. This unbraids the new adapter delivery from any refactoring of the existing adapter. After both adapters are stable, extract shared primitives into a separate crate as a behavior-preserving refactoring.

Important operational notes: `--mode html` for `aozora-epub3` is a derived format (extracted/filtered body XHTML), not raw upstream output like `aozora2html`; this must be recorded in fidelity notes. `--version` is served by the Rust mapper without touching the JAR. `--parser-failed` is triggered by `[ERROR]` lines on stderr. EPUB document classification uses `package.opf` with filename-based fallback. The GPL boundary is preserved by keeping upstream code in `references/parsers/` and invoking it as a subprocess; the Nix boundary is preserved by pinning a verified release JAR with SHA-256.
