# ADR 0030: Aozora Parser Selection and Fork Base

Status: Accepted
Validation scope: full-corpus
Release authority: development
Date: 2026-07-10
Accepted: 2026-07-10
Supersedes: none
Amended by: ADR 0032, ADR 0038
Amends: ADR 0002
Depends on: ADR 0002, ADR 0007, ADR 0023
Source: `ab-validator/docs/superpowers/reports/2026-07-08-aozora-parser-comparison-study.md`

## Implementation Status

As of 2026-07-10, the accepted parser-selection citations are live in
`data/parser-evidence-citations.edn` and are validated by
`abc.tools.parser-evidence` without changing registry admission.
`test/abc/tools/parser_evidence_test.clj` and
`test/abc/tools/validate_design_bundle_test.clj` cover the citation index and
admission boundary.

## Context

ADR 0002 defined parser evaluation gates but stayed Draft because no candidate
had parser-selection evidence recorded under the citation contract
(`docs/handoffs/parser-evidence-citation-contract.md`): parser-selection entries
in `data/parser-evidence-citations.edn` were `:provisional` only.

The producer side has since completed a spec-anchored comparison of five
parser candidates plus a build-fresh option
(`ab-validator/docs/superpowers/reports/2026-07-08-aozora-parser-comparison-study.md`,
hardened by `ab-validator/docs/superpowers/specs/2026-07-09-parser-comparison-followups-handoff.md`).
It measured, at pinned revisions over the pinned 17,886-work corpus:

- **conformance breadth** against a 127-vector third-party instrument
  (corroborating, not authoritative), cross-checked by an independent seed
  authored from the official 青空文庫 annotation documentation;
- **faithful capability** per parser at native granularity, with divergences
  attributed to parser versus adapter (correcting two large adapter
  distortions: the aozora2 mapper under-measured aozora-core, and the
  aozora-rs adapter's source-lexer fallback masked aozora-rs-core);
- **frequency-weighted corpus coverage** over each parser's full-corpus AAT,
  decomposed into **fidelity** (representation quality on completed works) and
  **robustness** (completion), on one uniform axis for all five parsers;
- **performance** with bounded DNF evidence, including root-causing
  aozora-core's superlinear ruby-density pathology.

Result: `aozora-pipeline` (`P4suta/aozora`) is the only candidate top-tier on
every axis — corpus coverage 0.969 (highest), 22/25 `must` vectors (the 3
fails are diagnostics-only), fidelity ~0.97 with robustness 1.000, median
1.25 s with zero timeouts — while actively maintained under a permissive
license (MIT/Apache-2.0), and proven measurement-stable across an upstream
re-pin (`5df2cfa5` → `1a4f864`) by a controlled experiment. No candidate is
conformant out of the box, and construct support across candidates is
complementary, not nested, so a consolidated parser effort is empirically
warranted rather than adopting any candidate as-is.

## Decision

### Parser selection

ABC accepts the study and its follow-ups as parser-selection evidence under
ADR 0002, and selects **`aozora-pipeline` (`P4suta/aozora`) as the base for
the consolidated production parser**. The engagement model is
**upstream-first, fork-fallback**: well-scoped fixes (the 3 diagnostics
`must` fails; typing the already-tokenized `jizume`/`yokogumi`/`keigakomi`
containers) are offered upstream, and the fork carries what upstream does not
take, plus the producer-preserved structures the publication boundary
requires (paragraphs, source notes, layout metadata, `ruby.direction`,
`decoded_utf8` spans — ADR 0024, ADR 0025).

Candidate dispositions, all evidence-backed:

- **`aozora2html` (Ruby gem)** is classified as an **indirect rendered-output
  comparator/oracle**, not a direct source-structure parser. It remains the
  reference oracle behind a subprocess boundary (GPL isolation preserved). Its
  per-work fidelity is the highest measured (0.974); its corpus coverage
  deficit is pure robustness (302 missed ruby-heavy works holding 24.7% of
  ruby mass).
- **`aozora-rs-core`** is a **throughput-only contingency** (fastest by ~7×
  over the next Rust parser), not the selected base: measured directly it
  recognizes 27% of constructs and ranks last on fidelity. Reconsider only if
  per-work throughput comes to dominate coverage.
- **`aozora-core` (aozora2)** is not a base: upstream stalled since 2026-01,
  and a work-specific superlinear ruby-density pathology times out on
  ruby-dense giants holding 13.6% of ruby mass.
- **`aozora-epub3` (Java)** is not a candidate (GPL, EPUB-oriented, emits no
  gaiji).
- **Build-fresh on `ab-source-syntax`** remains the fallback of record per
  ADR 0002's Rollback, not the selected path: highest cost with an active
  0.969-coverage permissive base available.

### Evidence policy (ADR 0002 acceptance slice)

This ADR enacts the acceptance slice defined in
`docs/handoffs/parser-evidence-citation-contract.md`:

1. That handoff is the controlling citation contract for producer evidence.
2. Conversion-audit reports are downstream **compatibility** evidence
   (admissible via ADR 0023), never parser-selection acceptance.
3. Parser selection requires candidate reports recorded by logical
   workspace-relative path and SHA-256 hash in
   `data/parser-evidence-citations.edn`; this ADR records them (below).
4. `aozora2html` is an indirect rendered-output adapter unless a separate
   source-structure oracle claim is proven.
5. Adapter-version matching stays exact; wildcard entries remain invalid.
6. TEI generation is downstream publication rendering and does not select a
   parser.

`data/parser-evidence-citations.edn` is promoted from provisional index to the
accepted ADR 0002 citation index. The `:parser-selection` entries accepted by
this ADR:

| evidence_id | logical path | sha256 |
| --- | --- | --- |
| `ab-validator/aozora-parser-comparison-study-2026-07-08` | `ab-validator/docs/superpowers/reports/2026-07-08-aozora-parser-comparison-study.md` | `sha256:8f68178186c9ccad2514c833098f3ffbc725183699757d5dc8182678c5826a59` |
| `ab-validator/parser-fork-candidacy-faithful-comparison-2026-07-08` | `ab-validator/docs/superpowers/reports/2026-07-08-parser-fork-candidacy-faithful-comparison.md` | `sha256:ec0e5b7cdef6ea0c14b6ba59af8a35eb7b742b12a3da74683f771873330933e8` |
| `ab-validator/parser-comparison-followups-2026-07-09` | `ab-validator/docs/superpowers/specs/2026-07-09-parser-comparison-followups-handoff.md` | `sha256:e13c904f36ddc4180f1326cdc375e6939716487e55318349c86c64e5158d2aa0` |

The earlier `:provisional` parser-selection entries (the 2026-07-04 bounded
performance measurement and the 2026-04-28 coverage report) remain in the
index unchanged as historical corroborating evidence; the study consolidates
and supersedes their conclusions without rewriting them.

### Boundary and identity

- The consolidated parser is a **producer-component deliverable** (the
  ab-validator side of the ADR 0007 boundary). This ADR is the "new ADR"
  ADR 0007's rollback clause requires before the producer side is promoted
  from external evaluator toward canonical parser implementation. ABC still
  validates the documented contract and evidence; it does not own transform
  logic (ADR 0023).
- **Selection is not admission.** The fork's output enters publication only as
  a new exact adapter/version tuple with producer-owned measurement evidence
  through `data/aat-parser-ir-compatibility.edn` (ADR 0023), and must meet the
  producer acceptance bar in
  `ab-validator/docs/superpowers/specs/2026-07-06-comprehensive-parser-acceptance-criteria.md`
  (source-coverage gate, parser-IR emission gate, publication-bundle gate,
  bounded performance evidence) before any release gate relies on it.
- **Reversibility holds** (ADR 0002 acceptance criterion): parser choice is
  carried by adapter/mapping identity coordinates, not by manifest identity
  rules, so reversing this selection is a new ADR plus new registry rows — no
  change to `manifest_identity_object` semantics (ADR 0001, ADR 0023).

## Rejected Alternatives

### Adopt an existing parser without a consolidated fork

Rejected by measurement: no candidate passes all 25 `must` vectors, support is
complementary rather than nested, and every candidate drops at least one real
construct entirely (study §4.9).

### Select `aozora-rs` as the primary direct parser path

Rejected. The earlier front-runner status rested on adapter output that is
~97% source-lexer fallback; measured directly, aozora-rs-core recognizes 27%
of constructs and ranks last on fidelity (0.950 reference denominator). Its
speed advantage is real and is retained as a named contingency.

### Fork `aozora-core` as the cheapest base

Rejected on two independent axes: upstream stalled since 2026-01, and the
ruby-density timeout pathology (12/30 timeouts on ruby-dense giants, all
timeouts, no crashes — a superlinear algorithm) with mediocre coverage
(0.855) even where it completes.

### Build fresh now on `ab-source-syntax`

Rejected as the present path (kept as fallback of record). The in-house lexer
is production-grade but block assembly, command semantics, Shift_JIS decode,
and gaiji resolution would all need to be written to reach a bar an active,
permissively-licensed 0.969-coverage base already clears.

## Consequences

- The remaining parser work is design and implementation on the
  `aozora-pipeline` base against the producer acceptance criteria — not
  further candidate measurement.
- The fork inherits named obligations: the three container-classification
  gaps (`jizume`, `yokogumi`, `keigakomi`), the 3 diagnostics `must` fixes,
  and the producer-preserved publication structures of ADR 0024/0025; the
  deferred warigaki/kunten vocabulary decisions (ADR 0023) become concrete
  design inputs for it.
- Upstream is under active development; re-pins of `P4suta/aozora` follow the
  controlled re-measure discipline recorded in the follow-ups handoff (diff
  `inspect` schema before widening the adapter's `schemaVersion` gate).
- ADR 0002 moves to Accepted with this ADR recorded as the amending decision;
  its gates remain the evaluation criteria of record for any future
  reversal or new candidate.

## Acceptance Criteria

- **ADR-0030-C1 — structural-invariant:** The three historical study citations have exact workspace-relative paths and byte hashes in the parser evidence index.
- **ADR-0030-C2 — structural-invariant:** ADRs 0002, 0030, 0032, and 0038 carry the reciprocal amendment links declared by the corrective decision chain.
- **ADR-0030-C3 — structural-invariant:** Historical parser-selection citations cannot satisfy the exact-registry admission boundary; absent or conflicting tuples remain rejected. Evidence boundary: `test/abc/tools/adr_evidence_capture_test.clj`.

## Rollback

Reversing the selection is a superseding ADR naming a new base (or the
build-fresh fallback), plus fresh parser-selection evidence entries; existing
compatibility registry rows, manifests, and `artifact_id` values are
unaffected because parser identity lives in adapter/mapping coordinates, not
manifest identity rules.
