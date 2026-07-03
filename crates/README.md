# ab-validator workspace crates

This directory contains the 13 Rust crates listed in the root `Cargo.toml`.
Adapter checkouts under `adapters/` are separate vendored crates and are not
workspace members.

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
| `ab-ir` | Parser-neutral block/inline IR, AAT JSON projection helpers, visible projection, provenance counts, and AAT selector support. Optional typed helper; the JSON schema remains the adapter contract. |

## Text and morphology pipeline

| Crate | Purpose |
| --- | --- |
| `ab-plaintext` | Converts AAT JSON or raw Aozora honbun bytes into `PlainTextDocument` for checks and morphology. |
| `ab-morph-diff` | Morpheme analysis alignment and diff model, including pairwise and n-way comparison helpers. |
| `ab-morph-analyzers` | `MorphAnalyzer` trait plus Vibrato, Sudachi, and Vaporetto implementations. |
| `ab-morph-run` | Top-level morphology runner, source resolution, summaries, reports, and warehouse integration. |
| `ab-warehouse` | Parquet warehouse schema, writer, staging/final paths, and SQL helpers. The on-disk Parquet schema is the durable boundary; the Rust API is mainly for `ab-morph-run`. |

## Evaluation harness

| Crate | Purpose |
| --- | --- |
| `ab-check` | CLI and backend for AAT schema validity, source projection, encoding, and parser invariant checks. |
| `ab-compare` | CLI for comparing two `ab-check` report directories; uses shared diff utilities. |
| `ab-coverage` | Coverage matrix tooling and merge CLI for Aozora syntax coverage data. |
| `ab-oracle` | Oracle correctness evaluation, audits, reports, and review-state handling. |

## Indexing and shared utilities

| Crate | Purpose |
| --- | --- |
| `ab-index` | Standalone feature-index builder and query CLI. |
| `ab-diff-utils` | Shared first-difference, frequency-table, and hash helpers, currently used by comparison tooling. |

## Notes

- Leaf CLI crates expose Rust libraries primarily as private CLI backends unless
  another crate actually consumes them.
- `ab-ir` is consumed as Rust types by at least one adapter today, but that is a
  convenience path rather than the normative adapter contract.
- Pending cleanup candidates from the classification report are not addressed
  here: unused dependencies, fake-seam review for `ab-diff-utils`, and explicit
  versioning policy for any Rust-typed adapter boundary.
