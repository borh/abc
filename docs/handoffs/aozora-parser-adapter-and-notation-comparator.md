# Aozora Parser Adapter and Notation Comparator Handoff

## Implemented

- Added parser id `aozora` as a fifth adapter lane.
- Added pinned references for `P4suta/aozora` and `P4suta/aozora-notation-spec`.
- Added `aozora-adapter` with schema-valid AAT smoke coverage.
- Fixed the adapter to preserve plain visible source gaps around upstream
  annotation spans as `text` nodes, instead of emitting only annotation nodes.
- Added notation-spec comparator reports kept separate from local AAT oracle.
- Added flake checks for the adapter smoke and notation-spec comparator smoke.

## Verification

- `cargo test --manifest-path adapters/aozora/Cargo.toml`
- `cargo test -p ab-coverage --jobs 24`
- `just aozora-smoke`
- `just parser-performance-smoke`
- `just aozora-notation-spec-comparator-smoke`
- `nix --option post-build-hook "" build .#checks.$system.aozora-smoke --print-build-logs`
- `nix --option post-build-hook "" build .#checks.$system.aozora-notation-spec-comparator-smoke --print-build-logs`

Plain `just aozora-flake-smoke` and `just aozora-notation-spec-comparator-flake-smoke` are wired, but local verification used `--option post-build-hook ""` because this host's post-build hook can stall on a remote cache copy or GC lock. The flake derivations themselves do not write to `/db`.

## First Comparator Evidence

`docs/superpowers/reports/2026-07-05-aozora-notation-spec-comparison.summary.json` measured 127 external notation-spec vectors across six lanes:

- upstream `aozora` inspect lane: 124 warnings, 3 failures.
- local AAT lanes `ab-aozora`, `aozora2`, `aozora2html`, `aozora-rs`, and `aozora-epub3`: 127 structural-warning rows each because AAT does not expose the external `aozora inspect` projection surface.

The three upstream `aozora` failures are diagnostic-shape mismatches for `pua_collision`, `tate_chu_yoko`, and `unclosed_bracket`; they are comparison evidence, not local policy.

## Full-Corpus Measurement Update

The full `aozora` AAT run completed at:

- `/db/ab-validator/aat-corpus/aozora-full-20260705T015007Z`
- AAT dir: `/db/ab-validator/aat-corpus/aozora-full-20260705T015007Z/aat/aozora-adapter`
- AAT files: 17,886

The five-adapter parser-IR conversion audit now includes `aozora`:

- report: `docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.md`
- attempted: 89,187 AAT files
- succeeded: 68,411
- failed: 20,776
- `aozora`: 996 succeeded, 16,890 failed

The `aozora` failures are dominated by `unsupported inline kind: raw`. Root cause investigation found this is primarily an adapter seam problem, not a parser-IR design result: the adapter consumes `aozora inspect nodes`, which exposes only shallow `{kind, span}` records with spans in upstream sanitized-source coordinates, then reconstructs AAT payloads by slicing this repo's decoded source text. CRLF normalization, gaiji sentinels, and upstream sanitization make those coordinate spaces diverge, producing corrupted `raw` nodes and visible text. The current CLI wrapper is therefore comparison evidence only; it is not trustworthy parser-IR evidence at corpus scale.

The TEI-EAJ structural expansion was regenerated with all five adapters:

- report: `docs/superpowers/reports/2026-07-04-tei-eaj-structural-expansion.md`
- TEI-EAJ files: 62
- rows with AAT evidence: 57
- parser-IR gap rows: 0
- adapter gap rows: 32
- evidence gap rows: 5

The five evidence-gap rows are TEI-EAJ files with no candidate work ID, not rows contaminated by `aozora` conversion failures. Adapter-specific conversion failures remain visible in row notes.

## Next Operator Measurements

1. Run parser performance measurement with all five parser lanes:
   `INDEX=/path/to/index.json SAMPLE=20 LIMIT_S=300 JOBS=24 just parser-performance-all-parsers`.
   This recipe requires a built AozoraEpub3 jar, or `AB_AOZORAEPUB3_JAR`.
2. Replace the current CLI-slicing `aozora` adapter with a library-backed adapter that consumes upstream typed parser output, or ask upstream to expose structured payload JSON and the sanitized source used for span coordinates.

The coverage-matrix strict-key gate remains on the original hand-classified
parser cells until the matrix is deliberately reclassified for `aozora-epub3`
and `aozora`; prevalence and performance measurement paths now include all
five parser adapters.

## Trust Boundary

`aozora-notation-spec` is useful comparison evidence, not the authority for local source representability, parser-IR vocabulary, or ABC TEI admission.
