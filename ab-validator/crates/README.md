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

Eight crates are lifted from
[P4suta/aozora](https://github.com/P4suta/aozora) at revision
`1a4f864603970983719655aa4af4525958ac2d38`, dual-licensed MIT or Apache-2.0,
and each carries `LICENSE-MIT`, `LICENSE-APACHE` and `NOTICE` beside its
sources. Seven of them keep the `ab-aozora-` prefix; `ab-notation-strategies`
descends from upstream's `aozora-proptest` and kept its licence obligation
through the rename. `ab-aozora` without a suffix is locally authored. The
Origin column below is the authority, not the name.

| Crate | Origin | Purpose |
| --- | --- | --- |
| `ab-aozora` | local | Harness CLI and executable entry point for the parser engine. |
| `ab-aozora-spec` | fork | Core domain types, diagnostic codes, and slug definitions. |
| `ab-aozora-encoding` | fork | Encoding detection, Shift-JIS conversion, and gaiji mapping. |
| `ab-aozora-syntax` | fork | Syntax grammar nodes, accent decomposition, and layout rules. |
| `ab-aozora-pipeline` | fork | Linear lexing, pairing, classification, and normalization pipeline. |
| `ab-aozora-render` | fork | Structured AST render target and HTML emission. |
| `ab-aozora-facade` | fork | High-level parser facade, AST document container, and incremental splice engine. |
| `ab-aozora-corpus` | fork | Archive indexing and parallel corpus processing helpers. |
| `ab-notation-strategies` | fork | Proptest strategies generating Aozora notation input, plus the shared proptest configuration the workspace's property suites run under. Dev-only. Descends from upstream's `aozora-proptest`. |

## Evaluation, conversion, and research harness

| Crate | Purpose |
| --- | --- |
| `ab-aat` | In-memory AAT document model, serialization, and oracle evaluation. |
| `ab-aat-to-parser-ir` | CLI and library converting AAT v1 JSON into parser-IR with divergence tracking. |
| `ab-capture` | Source capture, classified provenance, and lane recording. |
| `ab-check` | CLI and backend for AAT schema validity, source projection, and parser invariants. |
| `ab-coverage` | Coverage matrix tooling and merge CLI for Aozora syntax coverage data. |
| `ab-parser-rq-source-accountability` | Release-qualification instrument for source accountability and marker accounting. Named for the `parser-rq-source-accountability-*` records it emits. |
| `ab-parser-rq-diagnostic-authorization` | Release-qualification instrument for diagnostic authorization policies. Named for the `parser-rq-diagnostic-authorization-v1` instrument identity it publishes under. |

## Indexing and shared utilities

| Crate | Purpose |
| --- | --- |
| `ab-index` | Feature-index builder and query CLI. |
| `ab-diff-utils` | Shared first-difference, frequency-table, and hash helpers. |
| `ab-artifact-store` | Authenticated content-addressed blob publication and retrieval. Used by the release-qualification instruments and by `ab-capture`, but tied to neither. |
