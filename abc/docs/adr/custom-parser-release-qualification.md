# Custom Parser Release Qualification

## Implementation Status

Accepted. Candidate
`sha256:15affdfb677cc6a94a4a5364da68ca2d11441f899737651e727dbac90eddc5ab`
has one authorized immutable capture under qualification identity
`sha256:6f365a44b975465943da88d0e3fe4f123672e00913285a3e998ab465bc79edca`.
Its nine predicate verdicts pass, coherence is `ok`, and the exact tuple is
admitted by a fresh 17,886-work conversion audit with zero failed files. The
promotion verifier authenticates the reproducible executable provenance,
capture membership and bytes, current-registry evaluation, canonical
projections, and Accepted ADR 0040/0041 dependencies.

## Context

ADR 0002 records must-pass parser viability gates; ADR 0030 reported a 0.969
coverage observation and three failed `must` vectors while selecting a
consolidated-parser base. ADR 0038 accepts project ownership of the custom
parser for development only and states explicitly that ownership grants neither
tuple admission nor publication authority, and that comparison/citation records
cannot release a parser. Release qualification was left as future work.

Admission and release qualification are distinct gates:

- **Admission** is the exact parser/adapter/mapping tuple through ADR 0023's
  compatibility registry (`data/aat-parser-ir-compatibility.edn`). The admitted
  custom-parser row is `ab-aozora 0.6.0 aat-schema 2 facade 0.3.0 wire-schema 3
  (git 004deaf548f34a36abbc17d0f7a162df010a6292)`, mapping `0.4.0`
  (`sha256:cf177bee98af086fe728cbc1942e4f631b26ed5bb55aedc7d91b21f467c41f30`).
- **Release qualification** is this gate: predeclared predicates over a pinned
  corpus, evaluated with exact numeric comparison.

Neither comparison nor neutral evidence (ADR 0030 selection rows, the ADR 0038
neutral comparison) can satisfy either gate; the release gate consumes the
Task-6 structural evidence-class boundary in `abc.tools.parser-evidence`
(`assert-release-evidence!`, release class `#{:conversion-compatibility}`) so a
comparison/neutral citation is structurally incapable of qualifying a release.

### Admission resolution for the release candidate (ADR 0023)

The candidate's exact nine-field admission query is present in
`data/aat-parser-ir-compatibility.edn`. The immutable admission report records a
fresh full-corpus conversion audit of 17,886 works: 17,886 succeeded and zero
failed. Full-entry conflict checking and nine-field membership both resolve to
`admitted`; no historical registry row was rewritten.

## Decision

Qualify the project-owned parser for release ONLY through the predeclared
predicate set evaluated over the pinned qualification corpus with exact numeric
comparison. Promote this ADR to Accepted (and move release authority toward
publication) ONLY when every predicate verdict is `pass` from captured current
evidence for an admitted build. A predicate with no valid instrument is
`unavailable`, never `pass`; a `0.969` observation fails a `1.0` predicate
mechanically. Rounded composite coverage is descriptive and never substitutes
for a must-pass predicate.

## Consequences

The custom parser is qualified for publication for the exact committed
candidate, qualification identity, predicate set, corpus, and admitted tuple.
This authority does not float with branch HEAD: any executable, parser revision,
schema, mapping, corpus, predicate, or instrument change creates a different
candidate or qualification identity and requires new evidence. The immutable
P5 capture remains the authority for this decision; comparison and neutral
third-party-parser evidence remain non-release evidence.

## Evidence

The canonical gate projection is
`docs/reports/parser-release-qualification-report.json`. The immutable campaign
root is
`docs/reports/parser-rq/runs/15affdfb677cc6a94a4a5364da68ca2d11441f899737651e727dbac90eddc5ab/`.
The bounded contract runs in the standing Kaocha suite
(`test/abc/tools/parser_release_qualification_test.clj`,
`test/abc/tools/parser_rq_core_attempt_test.clj`,
`test/abc/tools/parser_rq_campaign_test.clj`); it covers
predicate identity, exact evaluation, release-evidence classification,
authorization, capture/evaluation resolution, and promotion failure semantics.

## Rollback

Withdraw this ADR if release qualification is redefined, or supersede it once a
committed instrument set and an admitted HEAD build produce a fully passing
captured gate. Do not repurpose comparison or neutral evidence as release
evidence.
