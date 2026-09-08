# ab-validator workspace crates

This directory contains the workspace crates listed in the root `Cargo.toml`
(the member list there is authoritative).

## Adapter contract

The AAT JSON schema is the normative adapter contract:

- Schema: `data/aat-schema.json`
- Semantics: `docs/aat-contract.md`
- Adapter fidelity notes: `docs/adapter-fidelity.md`

Adapters may emit compliant AAT JSON without depending on any workspace crate.
The Rust crates below are helpers for adapters and internal tools; they are not
the contract and should not be treated as version-stable adapter APIs.

## Adapter-facing helpers

| Crate | Purpose |
| --- | --- |
| `ab-source-syntax` | Lowest-level Aozora source tokenizer: source events, source spans, lossy comparison body extraction, and source annotations. Used by adapters and source-projection checks. |
| `ab-encoding` | Shared encoding labels and normalization helpers used by source readers and validation code. |
| `ab-ir` | Parser-neutral block/inline IR, AAT JSON projection helpers, visible projection, provenance counts, and AAT selector support. Optional typed helper; the JSON schema remains the adapter contract. |

## Text and morphology pipeline

| Crate | Purpose |
| --- | --- |
| `ab-plaintext` | Converts AAT JSON or raw Aozora honbun bytes into `PlainTextDocument` for checks and morphology. |
| `ab-morph-diff` | Morpheme analysis alignment and diff model, including pairwise and n-way comparison helpers. |
| `ab-morph-analyzers` | `MorphAnalyzer` trait plus Vibrato, Sudachi, and Vaporetto implementations. |
| `ab-morph-run` | Top-level morphology runner, source resolution, summaries, reports, and warehouse integration. |
| `ab-warehouse` | Parquet warehouse schema, writer, staging/final paths, and SQL helpers. |
| `ab-ortho-detect` | Orthographic variant detection rules and heuristics. |
| `ab-ortho-detect-ml` | Machine learning classifier backend for orthographic detection. |

## Parser core and facade

| Crate | Purpose |
| --- | --- |
| `ab-aozora` | Harness CLI and executable entry point for the parser engine. |
| `ab-aozora-spec` | Core domain types, diagnostic codes, and slug definitions. |
| `ab-aozora-veb` | Fast integer-set and bit-vector data structures. |
| `ab-aozora-encoding` | Encoding detection, Shift-JIS conversion, and gaiji mapping. |
| `ab-aozora-scan` | Linear lexical trigger and delimiter scanner. |
| `ab-aozora-syntax` | Syntax grammar nodes, accent decomposition, and layout rules. |
| `ab-aozora-pipeline` | Linear lexing, pairing, classification, and normalization pipeline. |
| `ab-aozora-render` | Structured AST render target and HTML emission. |
| `ab-aozora-facade` | High-level parser facade, AST document container, and incremental splice engine. |
| `ab-aozora-proptest` | Proptest generators and invariant verification strategies. |
| `ab-aozora-corpus` | Archive indexing and parallel corpus processing helpers. |

## Evaluation, conversion, and research harness

| Crate | Purpose |
| --- | --- |
| `ab-aat` | In-memory AAT document model, serialization, and oracle evaluation. |
| `ab-aat-to-parser-ir` | CLI and library converting AAT v1 JSON into parser-IR with divergence tracking. |
| `ab-capture` | Source capture, classified provenance, and lane recording. |
| `ab-check` | CLI and backend for AAT schema validity, source projection, and parser invariants. |
| `ab-coverage` | Coverage matrix tooling and merge CLI for Aozora syntax coverage data. |
| `ab-parser-rq-source-accountability` | Research query tooling for source accountability and marker accounting. |
| `ab-parser-rq-diagnostic-authorization` | Research validation for diagnostic authorization policies. |
| `ab-rq-artifact-store` | Content-addressed storage backend for research run captures. |

## Indexing and shared utilities

| Crate | Purpose |
| --- | --- |
| `ab-index` | Feature-index builder and query CLI. |
| `ab-diff-utils` | Shared first-difference, frequency-table, and hash helpers. |
