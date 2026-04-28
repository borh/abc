# AAT Morph Runner Design

Status: approved for implementation planning
Date: 2026-04-28

Scope: add the smallest executable path from existing AAT artifacts to
morphological analyses and pairwise morpheme comparisons. The runner consumes
AAT JSON produced by `ab-check --aat-output`; it does not invoke parser adapters
or read raw Aozora source files directly.

## 1. Problem

The morph analyzer adapters can already turn a `PlainTextDocument` into an
`ab_morph_diff::Analysis`, but our corpus source of truth is not a loose
`path/to/aozora.txt`. The existing AAT pipeline already knows how to read the
corpus index, run an adapter, validate its output, normalize `work_id`, and
write checked AAT JSON.

Duplicating that adapter harness inside morph tooling would couple unrelated
concerns:

- adapter discovery and process protocol;
- corpus/index path resolution;
- AAT schema validation;
- morph analyzer loading;
- morpheme diff output.

The simple boundary is: `ab-check` produces checked AAT, then morph tooling
consumes checked AAT.

## 2. Architecture

```text
corpus + index
  -> ab-check --adapter <adapter> --aat-output artifacts/aat
  -> ab-morph-run analyze-aat --aat-dir artifacts/aat/<adapter>
  -> analyses.jsonl
  -> comparisons.jsonl
```

### 2.1 Existing Responsibilities

`ab-check` remains responsible for:

- reading index entries and source bytes;
- invoking adapters with `--mode aat`;
- enforcing timeouts;
- validating AAT schema and invariants;
- rewriting `work_id` to the index work id;
- writing checked AAT JSON under `--aat-output`.

`ab-morph-analyzers` remains responsible for:

- loading Vibrato and Sudachi dictionaries;
- producing `Analysis` from `PlainTextDocument`;
- handling analyzer span reconstruction.

`ab-morph-diff` remains responsible for:

- comparing two `Analysis` values;
- emitting structured regions, feature diffs, and stats.

### 2.2 New Responsibilities

`ab-plaintext` gains:

```rust
pub fn from_aat_value(aat: &serde_json::Value) -> Result<PlainTextDocument, PlainTextError>;
```

This extracts:

- `text_id` from top-level `work_id`;
- `text` from visible AAT projection;
- `source_format` as `SourceFormat::AatVisibleText`.

`ab-morph-run` is a new binary crate that:

- reads checked AAT JSON files from a directory or single file;
- converts each AAT to `PlainTextDocument`;
- runs selected analyzers;
- writes analysis JSONL;
- optionally writes pairwise comparison JSONL.

## 3. AAT Visible Text Policy

The projection must match the existing validation semantics in
`crates/ab-check/src/aat.rs`:

- `text` nodes contribute `value`;
- `ruby` nodes contribute `base`;
- `gaiji` nodes contribute `resolved` when it is a non-empty string;
- unresolved `gaiji` nodes contribute an empty string, not the description;
- `raw` nodes contribute `source`;
- `warigaki` traverses `upper` then `lower`;
- generic inline containers traverse `content`;
- block containers traverse `children` in order.

This policy intentionally treats unresolved gaiji descriptions as markup, not as
plaintext to tokenize. It matches the current Aozora honbun plaintext projection
used by `ab-plaintext::from_aozora_honbun_bytes`.

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

Also support a single-file input for quick debugging:

```bash
cargo run -p ab-morph-run -- analyze-aat \
  --aat path/to/work.json \
  --analyzer vibrato \
  --analyses-output /tmp/analysis.jsonl
```

Analyzer ids:

- `vibrato`: uses `VibratoAnalyzer::unidic_cwj_default()`;
- `sudachi-a`: uses `SudachiAnalyzer::from_dictionary_path(SudachiMode::A, $AB_SUDACHI_DICT)`;
- `sudachi-b`: same for mode B;
- `sudachi-c`: same for mode C.

Sudachi analyzers require `AB_SUDACHI_DICT`. In `nix develop`, the flake sets it
to the reproducible Sudachi full dictionary package.

## 5. Output Formats

### 5.1 Analysis JSONL

One JSON object per `(work_id, analyzer)`:

```json
{"work_id":"wagahai","analyzer":"vibrato:unidic-cwj-202512","analysis":{}}
```

`analysis` is the existing `ab_morph_diff::Analysis` shape.

### 5.2 Comparison JSONL

One JSON object per analyzer pair for each work:

```json
{"work_id":"wagahai","from_analyzer":"vibrato:unidic-cwj-202512","to_analyzer":"sudachi-c","comparison":{}}
```

`comparison` is the existing `ab_morph_diff::Comparison` shape.

Pair order is deterministic: analyzers are compared in the order requested on
the CLI, using all `i < j` pairs.

## 6. Error Policy

For the first implementation, fail fast on:

- invalid JSON;
- missing `work_id`;
- unknown analyzer id;
- missing `AB_SUDACHI_DICT` when a Sudachi analyzer is requested;
- analyzer dictionary load failure;
- analyzer tokenization failure;
- morpheme comparison validation failure.

Fail-fast keeps the first runner simple and makes adapter/analyzer bugs visible.
+Batch-resume and per-work error rows can be added after we inspect real corpus
+failure modes.

## 7. Non-Goals

This phase does not add:

- adapter invocation from `ab-morph-run`;
- corpus index traversal;
- AAT schema validation;
- aggregation dashboards;
- HTML/CSV reports;
- Markdown or TEI export;
- multiprocessing or rayon.

Those can be layered later without changing the AAT-to-plaintext boundary.

## 8. Tests

Required `ab-plaintext` tests:

- extracts `work_id` as `text_id`;
- projects text/ruby/gaiji/raw/warigaki visible text;
- unresolved gaiji contributes empty text;
- missing `work_id` returns `PlainTextError::MissingAatWorkId`.

Required `ab-morph-run` tests:

- parses analyzer ids;
- rejects unknown analyzer ids;
- rejects Sudachi analyzer ids when `AB_SUDACHI_DICT` is unset;
- writes one analysis row for a single AAT and one analyzer;
- writes one comparison row for two analyzers using fake in-process analyzers or a unit-level helper, without requiring dictionaries.

Dictionary-backed end-to-end smoke tests can stay ignored and use the existing
Vibrato symlink and Nix-provided Sudachi dictionary.

## 9. Self-Review

Scope check:

- The design is intentionally one subsystem: checked AAT to morph artifacts.
- It does not duplicate `ab-check` adapter execution.
- It keeps analyzer dependencies out of `ab-plaintext`.

Ambiguity check:

- AAT projection semantics are explicit, including unresolved gaiji behavior.
- Sudachi dictionary source is explicit through `AB_SUDACHI_DICT`.
- Output row shapes and pair ordering are specified.

Known trade-off:

- The runner requires a prior `ab-check --aat-output` step. That is a deliberate
  simplicity choice; a wrapper command can automate both steps later if the
  two-stage workflow proves too verbose.
