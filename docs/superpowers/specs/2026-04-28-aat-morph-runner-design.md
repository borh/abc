# AAT Morph Runner Design

Status: approved for implementation planning
Date: 2026-04-28

Scope: add the smallest executable path from checked AAT artifacts to
morphological analyses and pairwise morpheme comparisons. The runner consumes
AAT JSON produced by `ab-check --aat-output`; it does not invoke parser adapters
or read raw Aozora source files directly.

## 1. Problem

The morph analyzer adapters can already turn a `PlainTextDocument` into an
`ab_morph_diff::Analysis`, but our corpus source of truth is not a loose source
file path. The existing AAT pipeline already knows how to read the corpus index,
run an adapter, validate its output, normalize `work_id`, and write checked AAT
JSON.

Duplicating that adapter harness inside morph tooling would complect unrelated
concerns: adapter discovery, process protocol, corpus path resolution, schema
validation, analyzer loading, and morpheme diff output.

The boundary is: `ab-check` produces checked AAT, then morph tooling consumes
checked AAT.

## 2. Architecture

```text
corpus + index
  -> ab-check --adapter <adapter> --aat-output artifacts/aat
  -> ab-morph-run analyze-aat --aat-dir artifacts/aat/<adapter>
  -> analyses.jsonl
  -> comparisons.jsonl
```

`ab-check` remains responsible for adapter invocation, timeout handling, schema
validation, invariant checks, `work_id` rewriting, and checked AAT output.

`ab-plaintext` becomes the single owner of AAT visible-text projection:

```rust
pub fn from_aat_value(aat: &serde_json::Value) -> Result<PlainTextDocument, PlainTextError>;
pub fn visible_text_projection(aat: &serde_json::Value) -> String;
```

`ab-check` should stop owning a separate projection implementation and call
`ab_plaintext::visible_text_projection` instead. If `ab-check` still needs
path/node-bearing fragments, those can stay local to `ab-check`; only the plain
string projection must be shared.

`ab-morph-run` reads checked AAT JSON files, converts them to
`PlainTextDocument`, runs selected analyzers, writes `Analysis` JSONL, and
optionally writes pairwise `Comparison` JSONL.

## 3. Shared AAT Visible Text Policy

Projection semantics match current `ab-check::aat::visible_text_projection`:

- `text` nodes contribute `value`;
- `ruby` nodes contribute `base`;
- `gaiji` nodes contribute `resolved` when the field is present;
- if `resolved` is absent, `gaiji` nodes contribute `description`;
- `raw` nodes contribute `source`;
- `warigaki` traverses `upper` then `lower`;
- generic inline containers traverse `content`;
- block containers traverse `children` in order.

This means `resolved: ""` contributes empty text, while a missing `resolved`
field falls back to the gaiji description. That behavior is not ideal for every
morphological use case, but matching existing `ab-check` behavior is the
simplest first move. A stricter projection can be added later as an explicit
mode if corpus output shows gaiji descriptions produce noise.

## 4. CLI Shape

Initial command:

```bash
cargo run -p ab-morph-run -- analyze-aat \
  --aat-dir artifacts/aat/aozora2-adapter \
  --analyzer vibrato \
  --analyzer sudachi-c \
  --analyses-output artifacts/morph/analyses.jsonl \
  --comparisons-output artifacts/morph/comparisons.jsonl
```

Single-file input is also supported:

```bash
cargo run -p ab-morph-run -- analyze-aat \
  --aat path/to/work.json \
  --analyzer vibrato \
  --analyses-output /tmp/analysis.jsonl
```

CLI analyzer ids are request names:

- `vibrato`;
- `sudachi-a`;
- `sudachi-b`;
- `sudachi-c`.

Runtime analyzer ids in output come from the analyzer implementation, so the row
may contain CLI id `vibrato` but emitted analyzer id
`vibrato:unidic-cwj-202512`.

Sudachi analyzers require `AB_SUDACHI_DICT`, matching the convention already used
by `ab-morph-analyzers` ignored tests. In `nix develop`, the flake sets it to the
reproducible Sudachi full dictionary package. The flake output
`.#sudachi-dictionary-full` already exists and can be used by smoke tests.

## 5. Output Formats

Output files are truncated/replaced on each run. The runner does not append to
existing JSONL files.

### 5.1 Analysis JSONL

One JSON object per `(text_id, analyzer)`:

```json
{"text_id":"wagahai","analyzer":"vibrato:unidic-cwj-202512","analysis":{}}
```

The wrapper duplicates `analysis.text_id` and `analysis.analyzer` intentionally:
JSONL rows remain grep-friendly without deserializing the nested analysis. This
is a wire-format convenience, not a separate source of truth.

### 5.2 Comparison JSONL

One JSON object per analyzer pair for each text:

```json
{"text_id":"wagahai","from_analyzer":"vibrato:unidic-cwj-202512","to_analyzer":"sudachi-c","comparison":{}}
```

The wrapper duplicates `comparison.text_id`, `comparison.from_analyzer`, and
`comparison.to_analyzer` for the same JSONL ergonomics reason.

Pair order is deterministic: analyzers are compared in the deduplicated order
requested on the CLI, using all `i < j` pairs. Duplicate CLI analyzer ids are
ignored after their first occurrence.

## 6. Input Edge Cases and Error Policy

Fail fast on:

- invalid JSON;
- missing string `work_id`;
- unknown analyzer id;
- missing `AB_SUDACHI_DICT` when a Sudachi analyzer is requested;
- analyzer dictionary load failure;
- analyzer tokenization failure;
- morpheme comparison validation failure.

`--aat-dir` with no `*.json` files is an error. Empty output caused by an empty
AAT text is not an error; analyzers may emit zero morphemes and comparisons will
surface coverage behavior.

Batch-resume and per-work error rows can be added after we inspect real corpus
failure modes.

## 7. Non-Goals

This phase does not add:

- adapter invocation from `ab-morph-run`;
- corpus index traversal;
- AAT schema validation;
- aggregation dashboards;
- HTML/CSV reports;
- Markdown or TEI export;
- multiprocessing or rayon.

## 8. Tests

Required `ab-plaintext` tests:

- extracts `work_id` as `text_id`;
- projects text/ruby/gaiji/raw/warigaki visible text;
- `resolved: ""` contributes empty text;
- missing `resolved` falls back to `description`;
- missing `work_id` returns `PlainTextError::MissingAatWorkId`.

Required `ab-check` tests:

- `ab_check::aat::visible_text_projection` delegates to `ab_plaintext` or is
  removed from the public surface if no longer needed.

Required `ab-morph-run` tests:

- parses analyzer ids;
- deduplicates duplicate analyzer ids;
- rejects unknown analyzer ids;
- rejects empty `--aat-dir`;
- rejects Sudachi analyzer ids when `AB_SUDACHI_DICT` is unset;
- helper test for comparison rows uses valid `Analysis` construction helpers
  with correct byte spans, char spans, surfaces, and source text.

Dictionary-backed smoke tests should use:

```bash
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  cargo run -p ab-morph-run -- analyze-aat \
  --aat /tmp/aat.json \
  --analyzer vibrato \
  --analyzer sudachi-c \
  --analyses-output /tmp/analyses.jsonl \
  --comparisons-output /tmp/comparisons.jsonl
```

## 9. Self-Review

Scope check:

- The design is one subsystem: checked AAT to morph artifacts.
- It does not duplicate adapter execution or corpus index traversal.
- It does not keep two separate visible-text projection implementations.

Ambiguity check:

- AAT projection semantics are explicit and match current `ab-check` behavior.
- `work_id` is translated to `text_id` at the plaintext boundary; JSONL rows use
  `text_id` consistently.
- Output overwrite semantics, duplicate analyzers, and empty directories are
  specified.
