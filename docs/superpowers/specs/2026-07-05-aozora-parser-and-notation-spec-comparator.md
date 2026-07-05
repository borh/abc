# Aozora Parser Adapter and Notation-Spec Comparator

**Date:** 2026-07-05

**Status:** Draft design for the next implementation slice.

## Context

`ab-validator` currently measures four active Aozora parser adapters:

- `aozora2`
- `aozora2html`
- `aozora-rs`
- `aozora-epub3`

The next candidate parser is `P4suta/aozora`, a Rust Aozora Bunko notation parser. Its repository describes a parser for ruby, bouten, tate-chu-yoko, gaiji references, kunten/kaeriten, indentation containers, and page/section breaks. It also exposes document-level CLI commands such as `check`, `fmt`, `render`, and `inspect`, and accepts stdin with auto-detected UTF-8 or Shift_JIS input.

The same author also maintains `P4suta/aozora-notation-spec`, an unofficial draft specification with ABNF grammar, processing model, diagnostics catalogue, and machine-readable conformance vectors. The spec states that it is not affiliated with or endorsed by Aozora Bunko. It is useful comparison material, but it must not replace:

- official Aozora Bunko documentation,
- source-authority inventory over the real corpus,
- measured adapter/corpus evidence,
- ABC-owned parser-IR and TEI admission gates.

## Decision

Implement the next parser slice as **new parser adapter plus external conformance-spec comparator**.

The external notation spec is treated as an evidence source and comparator suite. It is not a normative authority for `ab-validator` or ABC. Any conflict between the external spec and the real Aozora corpus should be recorded as evidence, not silently resolved in favor of the external spec.

## Components

### 1. Pinned External References

Add two pinned flake inputs:

- `reference-aozora-src = github:P4suta/aozora`
- `reference-aozora-notation-spec-src = github:P4suta/aozora-notation-spec`

Expose packages/checks that prove the expected source surfaces exist:

- `reference-aozora` builds or installs the upstream CLI reproducibly.
- `reference-aozora-notation-spec` installs the spec files needed for comparison: `conformance/schema/vector.schema.json`, `conformance/vectors/`, `conformance/RUNNER.md`, `src/grammar/aozora.abnf`.

The implementation should pin exact revisions in `flake.lock`. If upstream publishes tagged releases, prefer a release tag for the parser adapter once a full corpus run is recorded; until then, a commit pin is acceptable.

### 2. `adapters/aozora`

Add a fifth non-workspace adapter under `adapters/aozora/`, following the existing adapter contract:

```text
aozora-adapter --mode aat < input.txt > output.aat.json
aozora-adapter --mode html < input.txt > output.html
aozora-adapter --version
```

Phase 1 should favor a shallow, auditable adapter:

- invoke the upstream `aozora` CLI or link to the pinned upstream crate,
- read the upstream JSON inspection surface for structure,
- map reviewed upstream node kinds into AAT,
- preserve unknown but source-located upstream nodes as `raw` with provenance,
- record upstream diagnostics in `meta.warnings`,
- set `meta.parse_complete=false` when diagnostics make the output incomplete.

The adapter should not invent parser-IR policy. Its job is Aozora source bytes to AAT JSON.

### 3. Adapter Registration

Register parser id `aozora` wherever parser ids are currently hardcoded:

- `Cargo.toml` exclude list
- `crates/ab-coverage/src/adapter.rs`
- `crates/ab-coverage/src/cache.rs`
- `justfile`
- full audit recipes
- TEI-EAJ structural expansion recipes
- parser-performance measurements
- adapter smoke scripts
- `data/adapter-fidelity-notes.toml`

The AAT output directory convention should be:

```text
/db/ab-validator/aat-corpus/aozora-full-<timestamp>/aat/aozora-adapter
```

### 4. Notation-Spec Comparator

Add a comparator that consumes the pinned `aozora-notation-spec` conformance vectors and evaluates adapters against them.

The comparator should produce a separate report from `data/aat-oracle-cases.toml`:

- the existing AAT oracle remains curated and source-owned by `ab-validator`;
- imported external vectors remain externally authored evidence;
- vector ids, spec revision, schema hash, and expectation level must be recorded;
- `must`, `should`, and `may` expectations must be kept distinct;
- unsupported or unmappable vector shapes should be reported, not coerced into local oracle rows.

Recommended first implementation shape:

```text
reports/parser-conformance/run-aozora-notation-spec.py
docs/superpowers/reports/YYYY-MM-DD-aozora-notation-spec-comparison.md
docs/superpowers/reports/YYYY-MM-DD-aozora-notation-spec-comparison.summary.json
```

The comparator should run all available adapters by default:

- `aozora`
- `aozora2`
- `aozora2html`
- `aozora-rs`
- `aozora-epub3`

If an adapter cannot produce a meaningful comparison for a vector, record the reason as a comparison limitation rather than as a parser failure.

### 5. Measurement Flow

The implementation must preserve the existing measurement sequence:

1. Adapter smoke against hand-picked fixtures.
2. Conformance-vector comparison against the external spec.
3. Reviewed AAT oracle comparison.
4. Source-authority coverage matrix update, if new observed constructs appear.
5. Full corpus AAT run.
6. AAT-to-parser-IR conversion audit.
7. TEI-EAJ structural expansion audit with all five adapters.
8. Parser performance measurement across all five adapters.

Do not use external vector success to claim full Aozora markup representability. The source-authority strict gate remains the proof path for corpus source markup.

## Non-Goals

- Do not treat `aozora-notation-spec` as official Aozora Bunko policy.
- Do not auto-import every external vector into `data/aat-oracle-cases.toml`.
- Do not change parser-IR or TEI schema based only on external spec vectors.
- Do not claim Level 3 TEI readiness from parser-count consensus alone.
- Do not optimize the new parser; measure it beside the others.

## Acceptance Criteria

The first implementation slice is acceptable when:

- `aozora-adapter --mode aat` emits schema-valid AAT for ruby, gaiji, kunten/kaeriten, indentation, and page-break fixtures.
- `aozora-adapter --version` records the pinned upstream parser identity.
- `ab-coverage` can run parser id `aozora` through the same cache/invocation path as the other adapters.
- The notation-spec comparator can read pinned external vectors and emit Markdown plus JSON reports.
- A smoke comparator run includes at least one passing vector and one explicitly classified unsupported/unmapped vector.
- Full parser-performance smoke includes `aozora` as a fifth parser lane.
- No flake check writes to `/db` or requires network.

## Review Notes

This design has one deliberate trust boundary: external conformance vectors can strengthen or challenge local evidence, but they cannot overwrite source-authority or ABC admission decisions. That keeps the comparator useful without letting an unofficial draft spec become the hidden owner of this project's parser contract.
