---
plan_id: "2026-04-28-aozora2html-adapter"
status: done
started: 2026-04-28
next_update:
owner: unassigned
target_prerequisites: []
---

# Aozora2html Adapter Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [x]`) syntax for tracking.

**Goal:** Make the official Ruby `aozora2html` parser a first-class subject under comparison by adapting its XHTML output into AAT JSON, then comparing it to `aozora2` and `aozora-rs` through the existing `ab-check` + `ab-compare` pipeline. This replaces the current `/tmp/ab-validator-official-html-parity.sh` approach, which compares pandoc-of-fake-HTML on both sides because neither Rust adapter actually renders HTML (`adapters/aozora2/src/main.rs:38-40`, `adapters/aozora-rs/src/lib.rs:100-101`).

**Architecture:** Add `adapters/aozora2html/` as an external adapter program following the harness spec section 3 pattern (`run.sh` + `adapter.py`). The wrapper script invokes `references/parsers/aozora2html/bin/aozora2html` on a decoded `.txt` source; the Python script parses the resulting XHTML with `lxml` and emits AAT JSON conforming to `data/aat-schema.json`.

The adapter does **not** depend on `ab-ir`. This is a known maintenance cost: any future change to `ab-ir` semantic decisions or to the AAT schema must be mirrored here by hand. The trade-off is intentional — the alternative (Rust FFI to a Ruby process) is worse — but Task 6 includes a golden-file gate that catches schema divergence early.

Once the adapter passes `ab-check`, `ab-compare` produces structured AAT diffs and semantic-summary diffs against the existing `aozora2` and `aozora-rs` reports.

**Tech Stack:** Bash (thin wrapper, ~10 lines, no encoding logic), Python 3 with `lxml` and stdlib `json`/`hashlib`/`argparse`/`jsonschema`, official Ruby `aozora2html` invoked via `nix shell nixpkgs#ruby` matching the existing parity script invocation, existing Rust workspace (`ab-check`, `ab-compare`).

---

## Adapter Contract Reference

Verified against `crates/ab-check/src/check.rs`:

- **Input:** raw source bytes on stdin. The adapter receives no file path, no work ID, no environment variable identifying the work.
- **work_id:** the adapter writes any string in `work_id` (the existing Rust adapters use `"stdin"`, see `adapters/aozora2/src/lib.rs:57` and `adapters/aozora-rs/src/lib.rs:125`). `ab-check` overwrites the field after parsing the JSON (`check.rs:319`: `root.insert("work_id".to_owned(), ...)`). The adapter should not try to derive a real work ID.
- **CLI:** `--mode {aat,html}` and `--version`. Default mode is `aat`. Trailing newline on `aat` output.
- **Encoding detection:** UTF-8 BOM, valid UTF-8, then Shift_JIS fallback. Recorded in `meta.source_encoding`. `meta.source_hash` is `"sha256:" + sha256(raw_bytes)` over the original stdin bytes, not over decoded text.
- **Adapter discovery:** `ab-check` resolves the `--adapter` argument as either a path to an executable or a workspace adapter binary name (`check.rs:470-472`). A path to `adapters/aozora2html/run.sh` works.

---

## Files

- Create: `adapters/aozora2html/run.sh` — thin wrapper: runs the Ruby parser to a temp file, then invokes `adapter.py` with two file paths.
- Create: `adapters/aozora2html/adapter.py` — does encoding detection, runs Ruby (when invoked directly) or consumes pre-rendered XHTML, parses XHTML, emits AAT JSON. Single source of truth for encoding logic.
- Create: `adapters/aozora2html/README.md` — adapter contract notes, dependency list, mapping rules summary, known limitations.
- Create: `adapters/aozora2html/tests/fixtures/` — minimal `.txt` fixtures with their expected XHTML and AAT JSON.
- Create: `adapters/aozora2html/tests/test_mapper.py` — pytest covering each fixture and schema validation.
- Create: `benchmarks/run-aat-parity.sh` — replaces `/tmp/ab-validator-official-html-parity.sh` with three-way AAT-level comparison.
- Modify: `benchmarks/README.md` — explain the migration from the pandoc parity script.
- Modify: `docs/superpowers/PLAN-EXECUTION-ORDER.md` — refresh the active queue.
- No changes expected to `data/aat-schema.json`; if a fixture forces one, see Task 2 Step 6.

---

## Task 0: Confirm Prerequisites Exist

Quick verification before any implementation. None of these should require code changes; failures here mean the plan needs rescoping.

- [x] **Step 1: Confirm `data/aozora-syntax-coverage.toml` exists**

```bash
test -f data/aozora-syntax-coverage.toml && jq -r 'true' < /dev/null || echo "MATRIX MISSING"
grep -c "^\[\[syntax\]\]" data/aozora-syntax-coverage.toml
```

Expected: file exists, several `[[syntax]]` rows present. The file is already checked in (priority-1 rows for ruby, gaiji, gaiji+ruby, headings, etc., all currently `status = "partial"`). Task 2 fixture selection cites these row IDs.

- [x] **Step 2: Confirm a usable index file exists, or note its absence**

```bash
ls /tmp/ab-validator-edge-parity-*/index.json 2>/dev/null | head -1
```

The hardcoded index paths in this plan (`/tmp/ab-validator-edge-parity-20260427T124905Z/...`) are ephemeral — they came from the old parity-script run. If they have been wiped, regenerate before Task 3:

```bash
cargo run -p ab-index -- --corpus references/aozorabunko --output /tmp/ab-index.json
cargo run -p ab-index -- --index /tmp/ab-index.json --sample 25 --features ruby,gaiji --output /tmp/ab-work-ids.json
```

The plan continues to refer to "the index file" abstractly; substitute the freshly-regenerated path when needed.

- [x] **Step 3: Confirm Ruby parser invocation environment**

The parity script runs:

```bash
RUBYLIB="$REPO_ROOT/references/parsers/aozora2html/lib:$REPO_ROOT/references/parsers/aozora2html/vendor/zip/lib" \
  ./bin/aozora2html --error-utf8 --use-unicode <input.txt> <output.html>
```

Run that on `references/parsers/aozora2html/sample/*.txt` (or any small corpus work) under `nix shell nixpkgs#ruby` to confirm it still works. The `RUBYLIB` setting is mandatory and must be in `run.sh`.

`--use-unicode` controls **gaiji output** (whether the parser emits Unicode characters or `<img>` fallback); see `references/parsers/aozora2html/lib/aozora2html.rb:153,660`. It does **not** control input encoding and it does **not** control output encoding.

**Input encoding finding (verified during Task 1 smoke test):** `lib/aozora2html.rb:157` hardcodes `File.open(input, 'rb:Shift_JIS')`, so the parser **requires** Shift_JIS-encoded input regardless of `--use-unicode`. UTF-8 input fails with `invalid byte sequence in Shift_JIS`. Additionally, `Jstream` (`lib/jstream.rb:18-35`) requires CRLF line endings and aborts otherwise. `run.sh` therefore must:

1. Detect input encoding from raw stdin bytes (UTF-8 BOM / valid UTF-8 / Shift_JIS fallback).
2. Transcode UTF-8 input to CP932 with `iconv -f UTF-8 -t CP932` before invoking Ruby. (Inputs that are already Shift_JIS pass through unchanged.)
3. Normalize line endings to CRLF (`sed -e 's/\r$//' -e 's/$/\r/'`).

`adapter.py` still receives the **original raw stdin bytes** via `--source`, so `meta.source_hash` and `meta.source_encoding` reflect the user's input, not the transcoded intermediate.

**Output finding (verified during Task 0):** the XHTML the parser writes is always Shift_JIS-encoded with a `<?xml version="1.0" encoding="Shift_JIS"?>` declaration, regardless of `--use-unicode`. `adapter.py` must read the XHTML file as bytes and let `lxml` honor the XML declaration (or decode explicitly with `cp932`). Reading the file as UTF-8 will fail or produce garbled output. The `file(1)` command on a sample run reports `Non-ISO extended-ASCII text` — that is the Shift_JIS encoding showing through.

---

## Task 1: Adapter Skeleton And Contract

**Files:**
- Create: `adapters/aozora2html/run.sh`
- Create: `adapters/aozora2html/adapter.py`
- Create: `adapters/aozora2html/README.md`

- [x] **Step 1: Define the run.sh ↔ adapter.py protocol**

Two temp files, passed as positional arguments. No JSON wrapping, no stdin multiplexing.

The Ruby invocation must match `/tmp/ab-validator-official-html-parity.sh:run_official_html` verbatim — `bash -lc` (login shell, in case Ruby/gem environment depends on profile) and `cd` into the parser directory (some `require` statements resolve relative to `$PWD`). The parity script is known to work; matching it exactly avoids debugging path/encoding issues through the `ab-check` adapter boundary.

See the live implementation at `adapters/aozora2html/run.sh`. Shape:

- Capture raw stdin to `$stdin_raw`.
- Transcode to Shift_JIS at `$sjis_src` (UTF-8 BOM stripped if present; UTF-8 → CP932; otherwise pass through as-is).
- Normalize line endings to CRLF at `$crlf_src`.
- Invoke Ruby via `nix shell nixpkgs#ruby --command bash -lc 'cd "$PARSER_DIR" && RUBYLIB=... ./bin/aozora2html --error-utf8 --use-unicode "$crlf_src" "$xhtml"' >&2`. Stderr is redirected so Ruby's progress chatter doesn't pollute the AAT JSON written to stdout.
- `exec` `adapter.py --source "$stdin_raw" --xhtml "$xhtml" --mode "$mode"` so `adapter.py` re-reads the **original** stdin bytes for hashing and encoding metadata, not the Shift_JIS intermediate.

The `bash -lc` wrapper may be vestigial — if a follow-up investigation proves it unnecessary, drop it then. This plan keeps it because the parity script keeps it, and bisecting an environment-dependent failure inside an `ab-check` subprocess is slow.

- [x] **Step 2: Implement adapter.py argument parsing and version**

`adapter.py` accepts:

- `--mode {aat,html}` (default `aat`)
- `--version`
- `--source PATH` (raw source bytes for hashing/encoding detection)
- `--xhtml PATH` (Ruby parser output, required for `--mode aat`)

For `--mode html`, copy `--xhtml` to stdout verbatim. This is the only adapter that can do honest HTML output; it is not a deferred stub.

For `--version`, print `aozora2html-adapter 0.1.0 <git-rev>` where `<git-rev>` is the short SHA of `references/parsers/aozora2html` (resolved at build time, baked into the script as a constant during initial commit; updated when the reference is bumped). Surface the same string in `meta.adapter_version`.

- [x] **Step 3: Implement encoding detection in adapter.py**

Reuse the test-adapter pattern (`adapters/test-adapter/test-adapter:19-26`) verbatim — it is the existing battle-tested Python implementation. Acknowledged duplication: there are now two encoding-detection implementations (Rust in `decode_source_bytes`, Python here). Extracting a shared helper is out of scope; the duplication is documented in `adapters/aozora2html/README.md` so a future refactor knows where to look.

```python
raw = open(source_path, "rb").read()
if raw.startswith(b"\xef\xbb\xbf"):
    text, encoding = raw[3:].decode("utf-8"), "utf-8-bom"
else:
    try:
        text, encoding = raw.decode("utf-8"), "utf-8"
    except UnicodeDecodeError:
        text, encoding = raw.decode("cp932"), "windows-31j"
source_hash = "sha256:" + hashlib.sha256(raw).hexdigest()
```

- [x] **Step 4: Emit a minimal valid AAT envelope**

Before any XHTML mapping, return a single `paragraph` block whose `content` is `[{"kind": "text", "value": text, "span": {...}}]`. This proves the envelope is schema-valid:

```python
aat = {
    "version": 1,
    "work_id": "stdin",
    "blocks": [...],
    "meta": {
        "adapter": "aozora2html",
        "adapter_version": ADAPTER_VERSION,
        "source_encoding": encoding,
        "source_hash": source_hash,
        "parse_complete": True,
        "warnings": [],
    },
}
```

Notes consistent with the schema (`data/aat-schema.json`):

- `work_id`: placeholder — `ab-check` overwrites this (`crates/ab-check/src/check.rs:319`). Use `"stdin"`.
- `meta.metrics`: omitted. The `metrics` object requires 20+ fields tied to internal Rust adapter phases (decode_ms, tokenize_ms, etc.) that have no analogue in a Ruby+Python pipeline. The schema marks `metrics` optional, so omission is valid. Document this in `README.md` and in Task 3 — `ab-compare` should not be expected to produce metrics deltas for this adapter.
- `meta.semantic_summary`: omitted in this step; populated in Task 2 Step 4.

- [x] **Step 5: Smoke test the contract**

Add `tests/test_mapper.py::test_version_format` and `::test_envelope_passes_schema`:

```python
def test_version_format():
    out = subprocess.check_output(["adapters/aozora2html/run.sh", "--version"], text=True)
    assert re.match(r"^aozora2html-adapter \d+\.\d+\.\d+ [0-9a-f]{7,40}$", out.strip())

def test_envelope_passes_schema():
    aat = run_adapter_on(b"テスト本文。\n")
    jsonschema.validate(aat, json.load(open("data/aat-schema.json")))
```

These must pass before Task 2 starts.

- [x] **Step 6: Commit**

```bash
git add adapters/aozora2html/
git commit -m "feat: add aozora2html adapter skeleton"
```

---

## Task 2: XHTML To AAT Mapping

**Files:**
- Modify: `adapters/aozora2html/adapter.py`
- Create: `adapters/aozora2html/tests/fixtures/`
- Modify: `adapters/aozora2html/tests/test_mapper.py`

- [x] **Step 1: Capture reference XHTML for each fixture**

Pick one minimal `.txt` per matrix priority-1 syntax row from `data/aozora-syntax-coverage.toml`. Run the Ruby parser via the Task 0 invocation and check both `<row>.txt` and `<row>.xhtml` into `tests/fixtures/`:

| Matrix row ID | Fixture |
|---------------|---------|
| `ruby.basic` | `吾輩《わがはい》`, `｜あのひと《...》`, left-ruby `［＃「左」の左に「ひだり」のルビ］` |
| `gaiji.marker` | `※［＃「口＋世」、U+546D］`, `※［＃小書き片仮名ン、237-11］` |
| `gaiji_ruby.inline_base` | `※［＃「口＋愛」、第3水準1-15-23］《おくび》` |
| `heading.basic` | `［＃大見出し］...［＃ここで大見出し終わり］`, `［＃「...」は大見出し］` |
| Emphasis | `［＃「...」に傍点］` |
| Indentation | `［＃ここから２字下げ］...［＃ここで字下げ終わり］` |
| Warichu | `［＃割り注］...［＃割り注終わり］` (verify the actual XHTML structure: is it a single span with delimiter, nested upper/lower spans, or table cells?) |
| Image/caption | `［＃挿絵...］` from a real corpus example |
| Page/line break | `［＃改ページ］` |

The captured XHTML is the ground truth. If the parser emits something different from what the table below predicts, trust the XHTML and update the rule.

- [x] **Step 2: Implement the XHTML → AAT mapping**

`xhtml_to_blocks(xhtml: str, source_text: str) -> tuple[list[block], list[warning]]` is a stateless function. Mapping rules verified against the schema:

| XHTML construct | AAT output | Notes |
|---|---|---|
| `<p>` element under `<body>` (or text under `<div class="main_text">`) | `paragraph` block | Schema requires `kind` + `content`. |
| `<ruby><rb>X</rb><rp>...</rp><rt>Y</rt><rp>...</rp></ruby>` | `{kind: "ruby", base: "X", reading: "Y"}` | `rp` (ruby parens fallback) is dropped. |
| `<ruby>` whose `<rt>` carries left-placement class (verify from fixture) | `{kind: "ruby", base, reading, direction: "left"}` | Schema enum is `right` or `left`. |
| `<img src="...gaiji/...png" alt="...">` | `{kind: "gaiji", description: alt, resolved: null, jis_code: null, unresolved_reason: "image_fallback"}` | `unresolved_reason` is typed as string-or-null; any string is valid. |
| `<span class="gaiji-...">X</span>` (parser resolved to glyph or sequence) | `{kind: "gaiji", description: <derived>, resolved: "X", jis_code: null, unresolved_reason: null}` | Description should come from class or surrounding markup. If absent, use `"unknown"` and emit a warning. |
| Plain text containing characters from `--use-unicode` resolution | `{kind: "text", value: "..."}` | Already-resolved gaiji is indistinguishable from ordinary text by design — we accept this conflation. |
| `<h1|h2|h3>`, `<div class="o-midashi"|"naka-midashi"|"ko-midashi">` etc. | `{kind: "heading", level: 1|2|3, style: "<class>", content: [...]}` | Schema allows level 1–3. |
| `<div class="jisage_n">` | `{kind: "jisage_block", children: [...], "x-indent": n}` | Schema's `^x-` patternProperties allows extension fields; `x-indent` is the Aozora indent count. |
| `<em class="bouten">X</em>` and similar emphasis classes | `{kind: "style", style_type: "<class>", content: [...]}` | Schema allows `inline_container` with `style_type`. |
| `<span class="warigaki">...</span>` (or whatever the fixture shows) | `{kind: "warigaki", upper: [...], lower: [...]}` | **Pending fixture verification.** If the XHTML is a single span with a delimiter, splitting heuristically here is fragile — emit a warning and fall back to a `style` container with `style_type: "warigaki_unsplit"` until the fixture clarifies. Do not invent an upper/lower split that the source XHTML does not encode. |
| `<br />` | `{kind: "raw", source: "<br/>"}` | Schema's `raw` kind is the right home for uninterpreted markup. |
| `<hr class="kaipage">` and similar break markers | `{kind: "raw", source: "<hr/>"}` | Same reasoning. |

Block-container kinds beyond `jisage_block`: the schema enum is `jisage_block | quote_block | keigakomi_block | yokogumi_block | caption_block`. **Non-goal for this plan:** mapping `quote_block`, `keigakomi_block`, `yokogumi_block`, `caption_block`. They are deferred unless a Task 2 Step 1 fixture reveals one. Encountered XHTML that has no rule emits `meta.warnings` plus an `x-aozora2html-unmapped: "<element-name>"` extension property on the produced node, and processing continues. This makes coverage gaps visible to `ab-compare` instead of silently lost.

- [x] **Step 3: Spans — omit them**

XHTML carries no byte offsets into the source. Reconstructing them by aligning `<p>` elements with source paragraphs is fragile: the Ruby parser can drop bibliographic notes, merge consecutive blank-separated lines, or reorder content (front matter / colophon). A wrong alignment produces wrong spans for every node downstream.

The schema marks `span` as optional on every `block` and `inline` kind. **Omit `span` entirely** in this increment. Per-paragraph spans that might be wrong are worse than no spans at all.

If a future increment needs spans, build them from a source-text scan (e.g. by finding the first occurrence of a paragraph's normalized text in the decoded source) and mark them `"x-span-confidence": "alignment_heuristic"` so consumers know the provenance.

- [x] **Step 4: Produce a semantic summary**

`ab-compare` keys structural comparison off `meta.semantic_summary` (`crates/ab-compare/src/aat_diff.rs`). Without it, the new parity script will report "missing on side A" for every comparison, which is no more informative than the old pandoc summary.

> **Implementor warning:** the value shapes below are copied verbatim from `crates/ab-ir/src/semantic_summary.rs:60-130`. **Read that file before writing keys.** Any deviation in key names — `base` vs `base_projection`, missing `description_format`, missing `ruby_reading` — produces zero matches at Task 3 Step 3 because `ab-compare` hashes the raw JSON of each value. The reviewer who flagged this in the prior round was correct about the failure mode; do not rely on the sketch alone.

Exact shapes to emit (one entry per syntax row keyed by matrix ID):

```json
{
  "syntax": {
    "ruby.basic": [
      {
        "kind": "ruby",
        "value": {
          "base_projection": "吾輩",
          "reading": "わがはい",
          "placement": "right"
        },
        "provenance": "parser"
      }
    ],
    "gaiji.marker": [
      {
        "kind": "gaiji",
        "value": {
          "source": "※［＃「口＋世」、U+546D］",
          "description": "「口＋世」、U+546D",
          "description_format": null,
          "kind": "UnicodeCodepoint { value: '吭' }",
          "resolved": "吭",
          "ruby_reading": null
        },
        "provenance": "parser"
      }
    ],
    "gaiji_ruby.inline_base": [
      {
        "kind": "gaiji_ruby",
        "value": {
          "base_projection": "",
          "reading": "おくび",
          "placement": "right"
        },
        "provenance": "parser"
      }
    ]
  }
}
```

Field-by-field rules verified against the Rust source:

- `placement` is the lowercase string `"right"` or `"left"` (`lib.rs:473-479` — `RubyPlacement::as_str`). Not capitalized.
- `provenance` is one of `"parser"`, `"parser_normalized"`, `"source_supplement"`, `"source_fallback"` (`lib.rs:462-471`). For aozora2html output, `"parser"` for any node lifted from the XHTML; if the mapper falls back (e.g., for unmapped elements), use `"source_supplement"` or `"source_fallback"` — pick one and document it in `README.md`.
- `gaiji.marker.value.kind` uses the Rust `Debug` derive output of `GaijiKind` — strings like `"UnicodeCodepoint { value: '吭' }"`, `"JisLevel { level: 3, row: 15, cell: 23 }"`, `"Unknown"` (`semantic_summary.rs:123` calls `format!("{:?}", ...)`). Reproducing this exactly from the Python side is brittle — the format depends on Rust derive behavior and on which variant the Rust adapter chose. **Acceptable simplification:** emit a stable Python-side string like `"UnicodeCodepoint"` (variant name only), and accept that `gaiji.marker` value hashes will mismatch the Rust adapters even when description and resolved character agree. Document this in `README.md` and call it out in the Task 3 triage. The other gaiji fields (`source`, `description`, `resolved`, `ruby_reading`) will still align and the diff remains informative.
- `gaiji.marker.value.ruby_reading` is `null` for standalone gaiji and the parent ruby's reading for gaiji nested inside ruby (`semantic_summary.rs:96-98,112,125`).
- For ruby whose base contains a gaiji node, emit **both** a `ruby.basic` entry and a `gaiji_ruby.inline_base` entry (`semantic_summary.rs:80-95`). Do not skip the `ruby.basic` entry.
- `gaiji_ruby.inline_base.value.base_projection` is the visible-text projection of the ruby base. For an unresolved gaiji base, `inline_visible_text` returns the empty string (`semantic_summary.rs:165-169`); for a resolved gaiji base it returns the resolved character. Match this exactly.
- If the mapper emits a `meta.warnings` entry with `syntax_id`, also emit a corresponding `projection.warning` row: `{"kind": "projection_warning", "value": {"syntax_id": "...", "message": "..."}, "provenance": "projection"}` (`semantic_summary.rs:40-54`).
- `source_span` is optional and omitted (same reasoning as Step 3 spans).

- [x] **Step 5: Validate every fixture against the schema and the golden AAT**

Two test families in `tests/test_mapper.py`:

```python
@pytest.mark.parametrize("fixture", FIXTURES)
def test_fixture_passes_aat_schema(fixture):
    aat = run_adapter_on(fixture.txt_bytes)
    jsonschema.validate(aat, AAT_SCHEMA)

@pytest.mark.parametrize("fixture", FIXTURES)
def test_fixture_matches_golden(fixture):
    aat = run_adapter_on(fixture.txt_bytes)
    expected = json.loads(fixture.golden_aat_path.read_text())
    assert canonicalize(aat) == canonicalize(expected)  # ignore meta.adapter_version git rev
```

The golden file is checked in alongside each fixture. Updating goldens requires reviewing the diff in the commit — this is the schema-divergence safety net the architecture section promised.

- [x] **Step 6: AAT schema changes — only if a fixture forces one**

If a captured XHTML construct has no representation in `data/aat-schema.json` and no `x-` extension can express it cleanly, propose an additive schema change:

- New optional inline kind, or
- New entry in an existing enum (`block_container.kind`, `inline_container.kind`, etc.).

Document the change in `adapters/aozora2html/README.md` with a citation to the fixture. Do not change required fields. Do not change existing enum semantics. Schema changes ripple to every adapter and to `ab-ir`, so this is a deliberate decision, not a quick patch.

- [x] **Step 7: Commit**

```bash
git add adapters/aozora2html/ data/aat-schema.json
git commit -m "feat: aozora2html XHTML to AAT mapping"
```

---

## Task 3: Wire Into `ab-check` And `ab-compare`

The existing `ab-check` and `ab-compare` already consume any AAT-emitting adapter. Nothing in their code should need to change. This task validates that assumption end-to-end on a real sample.

- [x] **Step 1: Run `ab-check` on a small sample**

Use the index from Task 0 Step 2 (regenerated if necessary):

```bash
cargo run -p ab-check -- \
  --index /tmp/ab-index.json \
  --work-ids /tmp/ab-work-ids.json \
  --corpus references/aozorabunko \
  --adapter ./adapters/aozora2html/run.sh \
  --output /tmp/ab-aozora2html-checks/ \
  --jobs 4 --per-work-timeout 120s
```

Expected:

- `schema_valid` passes for every work. A failure here indicates the mapper produces invalid JSON for a real-corpus construct that the fixture set did not exercise — capture a new fixture, fix the mapper, repeat.
- `parse_completeness` passes for every work, except for works where the Ruby parser itself reports an error (those should be a small minority; record the work IDs).
- Heuristic properties (`ruby_completeness`, `gaiji_resolution`, `visible_text_body_order`) are allowed to fail. Failures are real parity findings, not adapter bugs.

- [x] **Step 2: Triage heuristic-property failures**

Classify each failing heuristic into one of three buckets, recorded in `docs/superpowers/specs/2026-04-28-aozora2html-coverage-notes.md`:

1. **Ruby tool genuinely loses information** — record as a parity finding; no code change in this plan.
2. **Mapper drops information that the XHTML preserves** — fix the mapper, add a fixture, re-run Step 1.
3. **Property too strict for an HTML-derived adapter** — record as a follow-up; do not weaken the property in this plan (would invalidate existing comparison baselines).

- [x] **Step 3: Run `ab-compare` against `aozora2` and `aozora-rs`**

```bash
cargo run -p ab-compare -- \
  --a /tmp/ab-aozora2html-checks/ \
  --b /tmp/ab-aozora-rs-checks/ \
  --output /tmp/ab-compare-aozora2html-vs-rs.json

cargo run -p ab-compare -- \
  --a /tmp/ab-aozora2html-checks/ \
  --b /tmp/ab-aozora2-checks/ \
  --output /tmp/ab-compare-aozora2html-vs-aozora2.json
```

Expected: `aat_block_count_match` is non-zero on at least some works (the parsers agree on basic structure for simple inputs). `semantic_summary_hash_difference_counts` is populated for at least the syntax IDs `ruby.basic` and `gaiji.marker`; if a key is entirely missing from the summary on the aozora2html side, that is a Task 2 Step 4 bug.

If `semantic_summary_hash_difference_counts` is empty (no overlap at all), the syntax IDs in the Python adapter's summary do not match those produced by `crates/ab-ir/src/semantic_summary.rs`. Reconcile and re-run.

- [x] **Step 4: Commit triage notes**

```bash
git add docs/superpowers/specs/2026-04-28-aozora2html-coverage-notes.md
git commit -m "docs: aozora2html coverage triage notes"
```

---

## Task 4: Replace The Pandoc Parity Script

**Files:**
- Create: `benchmarks/run-aat-parity.sh`
- Modify: `benchmarks/README.md`

- [x] **Step 1: Write the orchestration script**

`benchmarks/run-aat-parity.sh` takes `--index PATH --work-ids PATH [--sample N]` and:

1. Runs each of `aozora2`, `aozora-rs`, `aozora2html` adapters via `ab-check` on the same work-id list, into per-adapter output directories under a single timestamped output root.
2. Runs `ab-compare` for each pair.
3. Aggregates into `summary.json`:
   - `schema_valid_failures.<adapter>`: count
   - `parse_complete_failures.<adapter>`: count
   - `aat_block_count_match.<pair>`, `aat_block_count_mismatch.<pair>`
   - `semantic_summary_hash_match.<pair>.<syntax_id>`: count
   - `semantic_summary_hash_mismatch.<pair>.<syntax_id>`: count

The script is bash + `jq`; no Python orchestration. It must not invoke pandoc and must not normalize HTML to plain text.

- [x] **Step 2: 25-work pilot before full run**

```bash
bash benchmarks/run-aat-parity.sh --index /tmp/ab-index.json --work-ids /tmp/ab-work-ids.json --sample 25
```

Verify the script completes, produces `summary.json`, and that the counts are interpretable. Catch `jq`/path errors before paying full-corpus cost.

- [x] **Step 3: Document the migration in benchmarks/README.md**

Add a section explaining: "The previous `/tmp/ab-validator-official-html-parity.sh` compared pandoc-of-text on both sides because no Rust adapter rendered HTML. The current `run-aat-parity.sh` compares structured AAT and parser-emitted semantic summaries, which is what the harness was designed for."

- [x] **Step 4: Commit**

```bash
git add benchmarks/run-aat-parity.sh benchmarks/README.md
git commit -m "feat: AAT-level three-way parser parity script"
```

---

## Task 5: Plan Queue Maintenance

**Files:**
- Modify: `docs/superpowers/PLAN-EXECUTION-ORDER.md`

- [x] **Step 1: Refresh the active queue**

`PLAN-EXECUTION-ORDER.md:14-37` still lists the seven archived plans as active. Replace that section with the current state:

- All previously queued plans are archived under `docs/superpowers/archive/`.
- Current active plan: this one.
- Open spec gaps in `docs/superpowers/specs/`:
  1. `aozora-syntax-coverage-design.md` — `ir_projection::tei` not implemented; `ab-ir` lacks structured block kinds for jisage/warichu/figure/break; matrix rows still `status = "partial"`.
  2. `parser-validation-harness-design.md` — `ab-render-diff` crate not built. After this plan lands, the AAT-level parity script reduces the pressure to build it; revisit only when AAT diffs prove insufficient.
  3. `aozora-rs-performance-design.md` — performance phases shipped; a follow-up benchmark may be useful once aozora2html is in the comparison.

- [x] **Step 2: Commit**

```bash
git add docs/superpowers/PLAN-EXECUTION-ORDER.md
git commit -m "docs: refresh superpowers plan queue"
```

---

## Task 6: Final Verification

- [x] **Step 1: Workspace and adapter checks**

```bash
cargo fmt --all -- --check
cargo test --workspace
cargo clippy --workspace --all-targets -- -D warnings
python -m pytest adapters/aozora2html/tests
bash adapters/aozora2html/run.sh --version
bash benchmarks/run-aat-parity.sh --index /tmp/ab-index.json --work-ids /tmp/ab-work-ids.json --sample 25
```

Expected: formatter passes, all tests pass, clippy clean, mapper fixture and schema tests pass, version string matches the regex, parity script writes a `summary.json`.

- [x] **Step 2: Schema-divergence golden gate**

If `data/aat-schema.json` was modified in this plan, re-run every fixture from `adapters/aozora-rs/tests/fixtures/` and `adapters/aozora2/tests/fixtures/` against the new schema. Any pre-existing fixture that no longer validates blocks the merge — additive schema changes must remain additive in practice, not just in intent.

- [x] **Step 3: Confirm the parity result is interpretable**

The old summary showed `aozora2_official_html_matches: 0, aozora2_official_html_mismatches: 179` — uniform failure with no signal. The new `summary.json` must show:

- `schema_valid_failures.aozora2html` is 0 across the sample.
- `aat_block_count_match.aozora2html_vs_aozora2` is non-zero (parsers agree on at least some works' block structure).
- `semantic_summary_hash_match.aozora2html_vs_aozora2.ruby.basic` is non-zero.
- At least one mismatch row carries a non-empty `x-aozora2html-unmapped` extension or a `meta.warnings` entry, proving the gap-visibility mechanism works.

If any of those are zero, the mapper has a bug — investigate before declaring done. Do not mark the plan complete while the new summary is as uninformative as the old one.

---

## Non-Goals

- Do not implement Rust `ir_projection::html` or `ab-render-diff`. Those remain open in their own specs.
- Do not change required fields in the AAT schema. Additive enum extensions are allowed only when a captured fixture forces them (Task 2 Step 6).
- Do not promote `aozora2html` to oracle status; it is one more subject under comparison, per `aozora-syntax-coverage-design.md` non-goals.
- Do not weaken any existing `ab-check` property to make `aozora2html` pass. Property mismatches are findings, not bugs.
- Do not produce per-inline `span` fields in the Python adapter. Spans are deferred until source-alignment is honest (Task 2 Step 3).
- Do not produce `meta.metrics` from the Python adapter. The metrics object encodes Rust adapter phase boundaries with no Ruby/Python analogue.
- Do not map `quote_block`, `keigakomi_block`, `yokogumi_block`, `caption_block` unless a fixture forces it. Initial scope is `paragraph`, `heading`, `jisage_block`.

## Success Criteria

- `adapters/aozora2html/run.sh --mode aat` produces schema-valid AAT JSON for every fixture and for every sampled corpus work.
- The mapper produces `meta.semantic_summary` whose syntax IDs match `crates/ab-ir/src/semantic_summary.rs`.
- `ab-compare` reports structured AAT and semantic-summary diffs between `aozora2html`, `aozora2`, and `aozora-rs`.
- `benchmarks/run-aat-parity.sh` replaces the pandoc-based script and emits an interpretable summary.
- `docs/superpowers/PLAN-EXECUTION-ORDER.md` reflects the current state of `plans/` and `specs/`.
- Open spec-gap items (TEI projection, structured `ab-ir` blocks, render-diff crate) are recorded in the queue for the next planning session.
