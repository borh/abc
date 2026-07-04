# Adding Parser Support: published/git `aozora2` and `AozoraEpub3-JDK21`

**Investigation date:** 2026-07-04  
**Scope:** How to wire the published/git-upstream versions of `aozora2` and `AozoraEpub3-JDK21` into `ab-validator` at the same level as the existing adapters.  
**Sources:** `Cargo.toml`, `flake.nix`, `justfile`, `crates/ab-check/src/check.rs`, `crates/ab-coverage/src/adapter.rs`, `crates/ab-coverage/src/cache.rs`, `adapters/aozora2/`, `adapters/aozora2html/`, `references/parsers/aozora2/`, `references/parsers/AozoraEpub3-JDK21/` (built and exercised), `docs/superpowers/specs/2026-04-25-parser-validation-harness-design.md`.

## 1. How parsers become "supported" today

The harness consumes **adapters**: standalone executables that read raw Aozora Bunko bytes on stdin and emit AAT JSON or rendered HTML/JSON on stdout.

### Adapter wire contract

```bash
<name>-adapter --mode aat < input.txt > output.aat.json
<name>-adapter --mode html < input.txt > output.html
<name>-adapter --version
```

- Exit code `0` or `2` = success (`2` means partial output; `meta.parse_complete` should be `false`).
- Exit code `1` = fatal error; `ab-check` records `fatal_error` from stderr.
- Source encoding contract: UTF-8 BOM → valid UTF-8 → Shift_JIS/Windows-31J fallback; record detected encoding and SHA-256 of raw bytes in `meta`.

### Where adapter IDs are hardcoded

| File | Current parser IDs | What it does |
|------|-------------------|--------------|
| `crates/ab-coverage/src/adapter.rs` | `aozora2`, `aozora-rs`, `aozora2html` | Maps ID → binary path. |
| `crates/ab-coverage/src/cache.rs` | `aozora2`, `aozora-rs`, `aozora2html` | Maps ID → adapter source directory for content-addressed fingerprinting. |
| `tests/` scripts | `aozora2`, `aozora-rs`, `aozora2html` | Several smoke scripts grep for these adapter names. |
| `data/adapter-fidelity-notes.toml` | `aozora2`, `aozora-rs`, `aozora2html` | Records known adapter-specific limitations. |
| `flake.nix` | `reference-aozora2`, `reference-aozora-rs`, `reference-aozora-epub3` | Builds reference sources, not adapters. `referenceAozoraEpub3` is a placeholder. |

### Adapter project layout

Each adapter is a **non-workspace** project under `adapters/<id>/` (excluded in root `Cargo.toml`). Rust adapters have their own `[workspace]` declaration.

## 2. `aozora2` — using the published or git upstream version

### Current state

- `adapters/aozora2/` already wraps `aozora-core` **0.7.1 from crates.io**.
- `flake.nix` pins `reference-aozora2-src = github:takahashim/aozora2`.
- The local directory `references/parsers/aozora2/` is **not** a separate git checkout (it shares the parent `.git`); it is just a working-tree copy of the upstream workspace.

### What needs to change

Almost nothing. The existing adapter is already the published-version adapter. The only decision is whether to switch its dependency from crates.io to the git pin so builds are reproducible against a specific upstream revision.

**Recommended change:**

1. In `adapters/aozora2/Cargo.toml`, replace:
   ```toml
   aozora-core = "0.7.1"
   ```
   with a git dependency pinned to the same revision used by `flake.nix`:
   ```toml
   aozora-core = { git = "https://github.com/takahashim/aozora2", package = "aozora-core", rev = "<pinned-rev>" }
   ```
   (or keep crates.io if the goal is to track the latest published release).

2. Update `VERSION` in `adapters/aozora2/src/lib.rs` to reflect the exact upstream source, e.g.:
   ```rust
   pub const VERSION: &str = "aozora2-adapter 0.1.0 aozora-core-0.7.1";
   ```

3. If you want to compare the local checkout against crates.io/git, add a second adapter ID (e.g. `aozora2-git`) rather than retargeting the existing one.

### Integration points if a new ID is added

- `crates/ab-coverage/src/adapter.rs::AdapterBinary::for_parser`
- `crates/ab-coverage/src/cache.rs::AdapterFingerprintInputs::for_parser`
- Version-asserting tests in `adapters/aozora2/src/lib.rs`
- `tests/` smoke scripts that enumerate adapters
- `data/adapter-fidelity-notes.toml`
- `justfile` and `flake.nix` build targets

## 3. `AozoraEpub3-JDK21` — deep-dive adapter plan

### What the upstream project is

- GitHub: `AozoraEpub3-JDK21/AozoraEpub3-JDK21`
- Language: Java 21, Gradle 9.2.1
- License: **GPL v3**
- Version: `1.3.4-jdk21` (from `src/AozoraEpub3.java` and `build.gradle`)
- Purpose: convert Aozora Bunko text → EPUB 3.3

The published artifact is a **fat JAR** produced by `./gradlew jar` at `build/libs/AozoraEpub3.jar`.

### CLI contract

```bash
java -jar AozoraEpub3.jar [-options] input_files...
```

Relevant options for an adapter:

| Option | Meaning |
|--------|---------|
| `-d <dir>` | Output directory |
| `-enc UTF-8` | Input encoding (default `MS932`) |
| `-hor` | Horizontal writing (default vertical) |
| `-ext .epub` | Output extension |
| `-of` | Output filename matches input filename |

The tool **always writes an EPUB file**; it does not have a "write XHTML to stdout" mode.

### Output structure inside the EPUB

Built and exercised locally:

```text
mimetype
META-INF/container.xml
OPS/package.opf
OPS/toc.ncx
OPS/css/vertical*.css
OPS/xhtml/0001.xhtml   <- body sections, numbered sequentially
OPS/xhtml/0002.xhtml   <- next body section or colophon
OPS/xhtml/nav.xhtml    <- EPUB navigation document
OPS/xhtml/cover.xhtml  <- only if cover image configured
OPS/images/0001.jpg    <- images renamed sequentially
```

Key comment from `Epub3Writer.java`:

> 本文は改ページでセクション毎に分割されて xhtml/以下に 0001.xhtml 0002.xhtml の連番ファイル名で格納

So the adapter must unzip the EPUB, read `OPS/xhtml/0001.xhtml`, `0002.xhtml`, … in order, and concatenate their `<body>` contents for mapping.

### Observed XHTML output (real examples)

**Ruby:**
```html
<ruby>吾輩<rt>わがはい</rt></ruby>
```

**Bold:**
```html
<span class="b">これは太字</span>
```

**Boten (sesame emphasis):**
```html
<span class="sesame">傍点</span>
```

**Headings:**
```html
<div class="chap2">中見出し</div>
<div class="chap3">小見出し</div>
<p><span class="font5">同行大見出し</span></p>
```

**Indentation (jisage):**
```html
<div class="pt2"><p>字下げの段落です。</p></div>
```
Classes are `pt1` … `pt30`.

**Keigakomi (boxed):**
```html
<div class="border"><p>罫囲みの段落です。</p></div>
```

**Line break:**
```html
<br/>
```

**Gaiji with Unicode:**
Source `※［＃「口＋世」、U+546D］` → rendered as plain text `呭`.

**Warichu — currently broken in this version:**
Source:
```
［＃ここから割り注］
上行［＃改行］下行
［＃ここで割り注終わり］
```
Output:
```html
<p><span class="wrc"></p>
<p>上行<br/>下行</p>
<p></span></p>
```
The converter also emits `[ERROR] 割り注終わりなし`. This is upstream behavior; the adapter should either emit `raw` nodes or set `parse_complete=false` and record a warning.

### Supported / unsupported annotation shapes

AozoraEpub3 is **strict about full-width numerals** in some annotations. For example:

- `［＃ここから2字下げ］` (half-width `2`) → **unsupported**, logged as `注記未変換`.
- `［＃ここから２字下げ］` (full-width `２`) → supported → `<div class="pt2">`.

This is different from `aozora2html`, which normalizes numerals. An adapter must decide whether to normalize input before handing it to AozoraEpub3 or to faithfully report upstream limitations.

Headings:
- `［＃「X」の大見出し］` → **unsupported** (`注記未変換`).
- `［＃「X」は中見出し］` / `［＃「X」は小見出し］` → supported.

Figures:
- `挿絵（fig01.png、横４００×縦３００）入る` only becomes an `<img>` if the referenced image file exists next to the input. Otherwise it stays as plain text.

### Adapter architecture recommendation

Follow the same shell-wrapper + mapper pattern as `adapters/aozora2html/`.

```
adapters/aozora-epub3/
├── aozora-epub3-adapter      # bash wrapper
├── Cargo.toml                # Rust mapper (non-workspace)
├── src/
│   ├── main.rs               # CLI: --mode aat/html, --version
│   └── lib.rs                # XHTML → AAT projection
├── tests/
│   └── fixtures/
└── README.md
```

**Wrapper responsibilities:**

1. Read stdin raw bytes; write to a temp `.txt` file.
2. Detect encoding (UTF-8 BOM / UTF-8 / Shift_JIS); transcode to the encoding expected by AozoraEpub3 (`-enc UTF-8` or default MS932).
3. Keep the **original** raw bytes for hashing/encoding metadata.
4. Run:
   ```bash
   java -jar AozoraEpub3.jar -d <tmp-out> -enc UTF-8 <tmp-input>
   ```
5. Unzip the resulting EPUB.
6. Pass the original raw bytes + list of body XHTML files to the Rust mapper.

**Mapper responsibilities:**

1. Parse each `OPS/xhtml/NNNN.xhtml` with the same HTML parser used by `aozora2html` (`tl` or similar).
2. Identify body sections vs. colophon/title pages. The colophon typically starts with `底本：`. Title/cover pages can be skipped.
3. Map XHTML to AAT:

| XHTML | AAT |
|-------|-----|
| `<p>...</p>` | `paragraph` |
| `<p><br/></p>` | empty paragraph or ignored line break |
| `<div class="chap1">` / `chap2` / `chap3` | `heading` with level 1/2/3, style `normal` |
| `<span class="font5">` / `font3` / `font1` | `heading` with style `dogyo` (inline heading) |
| `<ruby>base<rt>reading</rt></ruby>` | `ruby` |
| `<span class="b">` | `style` `bold` |
| `<span class="i">` | `style` `italic` |
| `<span class="sesame">`, `dot`, `open_sesame`, etc. | `style` `boten` with `x-boten-kind` |
| `<span class="underline">`, `double_underline`, `left_underline` | `style` `bousen` |
| `<div class="ptN">` | `jisage_block` with `x-indent` = N |
| `<div class="border">`, `dashed_border` | `keigakomi_block` |
| `<div class="yoko">` | `yokogumi_block` |
| `<span class="tcy">` | `tcy` |
| `<span class="wrc">` | `warigaki` (when well-formed) |
| `<br/>` inside a paragraph | `text` with `x-break-kind: line` |
| Plain resolved gaiji character | `text` (AozoraEpub3 inlines resolved characters) |
| `<img>` | `figure` |

4. Emit `meta.parse_complete=false` when upstream logged errors (e.g. warichu unclosed).
5. Add `x-provenance = "source-derived"` for information recovered from the original source markers rather than from rendered XHTML, following the `aozora2html` convention.

### Reuse opportunities

The `adapters/aozora2html/` Rust mapper already knows how to:
- Decode stdin bytes and compute `source_hash` / `source_encoding`.
- Walk XHTML and map CSS classes to AAT.
- Handle source-derived recovery for ruby, gaiji, warichu, figures, etc.

Consider generalizing that mapper so it can be parameterized by:
- the set of CSS class → AAT mappings, and
- whether the input is raw XHTML or an EPUB archive.

This would avoid duplicating the XHTML normalization logic.

### Build / Nix / reproducibility

Current blocker: `flake.nix` defines `referenceAozoraEpub3` as a placeholder that prints an error, because the Gradle build has no dependency locks and Maven artifacts are not vendored.

Options:

1. **Prebuilt release JAR (simplest for developers)**
   - Fetch the release JAR from GitHub Releases via `pkgs.fetchurl` with a SHA-256.
   - Pro: reproducible in Nix without building Java.
   - Con: requires trusting the published artifact; may not track a specific git revision.

2. **Manual build (simplest for the adapter itself)**
   - Document: "run `./gradlew jar` in `references/parsers/AozoraEpub3-JDK21`".
   - The adapter wrapper finds `build/libs/AozoraEpub3.jar` or fails with a helpful message.
   - Pro: no Nix complexity.
   - Con: not reproducible in CI unless the JAR is cached.

3. **Full Gradle lock + Nix Maven metadata (most reproducible)**
   - Add Gradle dependency locking to upstream or generate Nix Maven metadata.
   - Pro: pure Nix build.
   - Con: large upfront effort; may require upstream cooperation.

Recommended path for now: **Option 1 for Nix checks, Option 2 for local development**, with an environment variable `AB_AOZORAEPUB3_JAR` to override the JAR path (same pattern as `AB_AOZORA2HTML_BIN`).

### License boundary

- Keep all AozoraEpub3 source under `references/parsers/AozoraEpub3-JDK21/`.
- Do not copy GPL files into project source, data, or schemas.
- The adapter may invoke the upstream JAR as a separate process; the mapper itself remains project-owned (MIT/Apache-2.0) if it does not include upstream code.

## 4. Shared integration checklist

### Code

- `crates/ab-coverage/src/adapter.rs` — add new parser IDs and binary paths.
- `crates/ab-coverage/src/cache.rs` — add source-root mappings for fingerprinting.
- Update unit tests that assert the parser ID list.
- Update smoke tests in `tests/`.
- Add entries to `data/adapter-fidelity-notes.toml`.

### Build automation

- `justfile`: add `aozora-epub3-build`, `aozora-epub3-test`, `aozora-epub3-aat-full`.
- `flake.nix`: add a Nix app/check for the adapter; fix `referenceAozoraEpub3`.
- Root `Cargo.toml`: add new Rust adapters to `exclude`.

### Verification

- `--version`, `--mode aat`, `--mode html` smoke tests.
- Schema validation via `ab-check` on a fixture set.
- Cross-adapter comparison via `ab-compare` against `aozora2html`.
- If AAT is schema-valid, `ab-aat-to-parser-ir` works without changes.

## 5. Open questions / next steps

1. **For `aozora2`:** keep crates.io or pin to the git upstream revision? If both are needed, add a second adapter ID.
2. **For `AozoraEpub3-JDK21`:** confirm the Nix procurement strategy (release JAR fetch, manual build, or full Gradle locking).
3. **Mapper reuse:** should the `aozora2html` Rust mapper be generalized to also handle AozoraEpub3 XHTML?
4. **Input normalization:** should the adapter pre-normalize half-width numerals in annotations before passing to AozoraEpub3, or faithfully report upstream unsupported annotations?
5. **Warichu / malformed output:** decide whether to emit `raw` nodes, set `parse_complete=false`, or both when upstream produces invalid HTML.
6. **Figure handling:** confirm behavior with real image files present; design how the adapter reports missing images.

## 6. Summary

- **`aozora2`** is essentially already supported at the published-version level. The remaining work is optional: pin the dependency to the git upstream revision and possibly register a second adapter ID.
- **`AozoraEpub3-JDK21`** needs a **new adapter** following the `aozora2html` shell-wrapper + Rust-mapper pattern. The core challenge is that it only produces EPUB files, so the adapter must unzip and extract sequential body XHTML files (`OPS/xhtml/0001.xhtml`, …). It supports a subset of Aozora features and emits characteristic CSS classes (`chap1/2/3`, `ptN`, `border`, `sesame`, `b`, `i`, `tcy`, `wrc`, etc.) that can be mapped to AAT. The main blockers are reproducible JAR procurement in Nix and respecting the GPL license boundary.
