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

## 2026-07-25 note — historical research evidence, no longer the publication gate

Publication release authority has moved off this decision. The Accepted
`release-parser-identity-approval` decision now authenticates the release
parser as a decision-bound, content-addressed governed record,
`abc/data/release-parser-identity-v1.edn`: authentication requires BOTH the
record's integrity (its `qualification_identity_ref` and `candidate_ref`
recompute from its own bytes) AND that Accepted decision binding the exact
`candidate_ref`. `soranoha build-publication`'s release-admissibility check
(`abc.tools.publication-release/verify-release-root!`) authenticates through
that record, not through this ADR or the parser-rq campaign it governs.

This ADR's substantive claims above are unchanged and remain the historical
record of how the P5 candidate was release-qualified through the predeclared
predicate set over the pinned corpus. It is retained as historical research
evidence for that qualification work; it no longer gates publication, and no
claim above is amended or withdrawn by this note.

## 2026-07-26 note — requalified after the instrument-semantics rotation

The qualification recorded above has been superseded by a new capture. Three
governed rotations landed between them, and one capture settles all three:

- `package-scoped-instrument-dependency-identity` scoped each instrument's
  dependency identity to its own package and corrected the
  diagnostic-completeness reviewed closure, rotating both instruments'
  `validator_semantics_hash`.
- `governed-diagnostic-expectation-and-instrument-coordinate` gave instrument
  identity its own coordinate, `instrument_policy_hashes`, and made
  `diagnostic_completeness` the ratio its predicate always declared.

The new coordinates are `candidate_ref`
`sha256:24d61fc75b0dd2405a423ebc0a06bd3002e37b7d8b9ed4a76b2c21d57eb2b3ec`
and `qualification_identity_ref`
`sha256:8c1716f07a76ecf79109fe922a84878765c8dbae795b94bdff66e354d960d366`,
with `gate_status: release-qualified` and 9/9 predicates `pass` over the same
pinned three-work corpus. `verify-promotion` returns `ok` against
`docs/adr/decisions.edn`.

What changed in substance, not just in hashes. The prior capture passed
`diagnostic-completeness` over zero evidence: `diagnostic_completeness 1.0`
with `diagnostic_count 0` and `vacuous true`, where the `1.0` was a literal
and no ratio was computed anywhere. The new capture records the same `1.0`,
but as `matching_works 3` of `expected_works 3` — three works each observed to
match the diagnostic expectation their corpus entry governs. The number is
unchanged and now means something. A work emitting a diagnostic it is not
governed to emit, or failing to emit one it is, drops the ratio below 1.0 and
fails the existing comparator.

`predicate_set_hash` is unchanged at
`sha256:bec4fff7ab46003667df6115accf16da88260e02a003a07ab5537e8f5851c203`.
That is deliberate and is the point of the coordinate separation: what a
release must prove did not move, only how it was measured and how that
measurement is identified.

The 2026-07-25 note above still holds. This ADR remains historical research
evidence and does not gate publication; the requalification is recorded here
because this is where the qualification it supersedes is recorded. No claim
above is amended or withdrawn by this note.
