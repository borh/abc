# Bind Diagnostic Expectation to the Corpus, Instrument Identity to Its Own Coordinate

## Implementation Status

Accepted on 2026-07-26. Decided ahead of implementation because both changes
rotate identity and must land as governed migrations, not as code that
retroactively acquires a rationale. Step 3 of the instrument-semantics audit
implemented them as two commits — the instrument coordinate first, since it
rotates a different coordinate than the diagnostic expectation and is atomic
on its own.

Requalification is owed and not yet done: a new capture settles this record's
two rotations together with the instrument rotation recorded in
[[package-scoped-instrument-dependency-identity]].

## Context

### The diagnostic-completeness predicate passed over zero evidence

The accepted qualification's `measurements.json` records
`diagnostic_completeness` as `1.0` with `diagnostic_count: 0`,
`works_with_diagnostics: 0`, `vacuous: true`. One of the nine release
predicates contributed nothing to the verdict.

That is not an accident, and it is not a defect in the instrument. The corpus
declares `:expected_diagnostics []` on all three entries: it is *designed* to
be clean, and the instrument faithfully observed what the corpus intended.

The fault is that the intention is unenforced. `:expected_diagnostics` is read
by no capture, no instrument, and no test, and it is absent from
`corpus-entry-identity-keys` — which is `[:work_id :source_path :source_sha256
:category :expected_status]`. Editing it changes no behaviour and rotates no
identity. A governed value that authenticates nothing is decoration, the same
class of fault as the core-attempt hashes that are declared and never read.

A note on what the instrument actually measures, because its own declaration
misleads. The predicate declares `:unit "ratio"`, but `diagnostic_completeness`
is the literal `1.0` at `src/abc/tools/parser_rq_diagnostic_completeness.clj`,
and `complete_diagnostics` is assigned equal to `emitted_diagnostics`
unconditionally — the two cannot diverge. The real content of the predicate is
"the diagnostics envelope validates against the v3 schema", which requires
code, severity, source, and span, so schema-valid implies complete. It does
discriminate, against a malformed envelope. It simply had no diagnostics to
discriminate over.

### Instrument identity rides on prose

`src/abc/tools/parser_rq_campaign.clj` derives the candidate's
`instrument_versions` from the predicate set's human-readable `:instrument`
strings. For diagnostic-completeness that string is `"ab-aozora --mode
diagnostics envelope entries {code,severity,source,span}"`. It is not
`policy_hash`, not `validator_semantics_hash`, not `algorithm_version`.

So a change to instrument semantics rotates `policy_hash` while
`qualification_identity_ref` — which `candidate_ref` and the whole
admission and promotion chain bind — does not move unless someone also edits
the descriptive prose. Three layers answer three questions, and the middle one
has no coordinate:

| Layer | Question | Coordinate |
|---|---|---|
| Predicate identity | what must be true | `predicate_set_hash` |
| Instrument identity | how it was measured | *none* — prose |
| Executable provenance | which binary ran | `provenance_core_ref` |

## Decision

**A global diagnostic floor is rejected.** Requiring "at least one diagnostic
somewhere in the corpus" is satisfiable by any single diagnostic anywhere and
says nothing about whether the right works produced the right ones. It would
also fail a corpus that is governed to be clean, forcing corpus expansion
before the gate could pass at all.

**The governed per-work expectation becomes the authority instead.** Observed
diagnostics are compared against each entry's `:expected_diagnostics`, and that
field joins `corpus-entry-identity-keys`. A clean corpus passes zero
legitimately; a work that should emit a diagnostic and does not fails, and a
work that emits one it should not fails too. As the corpus grows to include
works that must emit diagnostics, the check stops being vacuous on its own —
no second gate redesign is needed, and no predicate dimension has to be
rewritten to say something the corpus already says.

**Instrument identity gets its own machine coordinate.** A new
`instrument_policy_hashes` key in the qualification identity binds each
instrument's governed authority document. It replaces reliance on the
`:instrument` prose, which stays descriptive.

What it binds is the *content* of that document — the sha256 of its canonical
JSON bytes — not the `policy_hash` the document declares about itself. One
derivation then covers every member. The ignored-regions taxonomy behind
source-recognition declares no hash at all, so a declaration-based rule would
have had to leave that member out, reintroducing the same silent gap one level
down. And where a policy does declare a `policy_hash`, that declaration is
itself among the hashed bytes, so binding the content is strictly stronger than
binding the claim.

Membership is closed over exactly the seven capture members that contribute
observations, and a named-but-missing policy fails the candidate build rather
than yielding an identity with a hole in it.

Editing the prose strings was the available alternative and is rejected: it
rotates `predicate_set_hash` as a side effect, braiding predicate identity with
instrument identity. What must be true and how it was measured are different
questions and belong to different coordinates.

The declared `:unit "ratio"` is **made true rather than corrected**. This
record originally proposed editing the declaration to match the constant the
instrument emits. The better move is the reverse: define
`diagnostic_completeness` as the ratio it already claims to be — works whose
observed diagnostics match their governed expectation, over expected works.

On the present corpus every work expects no diagnostics and emits none, so the
ratio is 3/3 = 1.0 and the outcome is unchanged. A work emitting a diagnostic
it should not, or failing to emit one it should, drops the ratio below 1.0 and
fails the existing `:= 1.0` comparator.

This matters beyond tidiness. Editing the declaration would rotate
`predicate_set_hash` — braiding the predicate contract with an instrument
concern, which is precisely what claim c2 rejects for the coordinate question.
Making the declaration true leaves the predicate contract untouched: the
dimension, comparator, threshold, and unit all stay as written, and the
hardcoded `1.0` disappears from the instrument instead. The semantics change is
carried where it belongs, by a bumped `algorithm_version` in the instrument
policy.

## Consequences

Both changes rotate identity, and they rotate different coordinates, which is
the point of separating them.

Adding `:expected_diagnostics` to the corpus entry identity keys rotates
`corpus_list_hash`. Adding `instrument_policy_hashes` rotates
`qualification_identity_ref` and therefore `candidate_ref`, and leaves
`predicate_set_hash` untouched. Requalification is owed for both, and is owed
already for the instrument rotation recorded in
[[package-scoped-instrument-dependency-identity]] — one new capture settles all
three.

`instrument_policy_hashes` is a schema change, not only a code change. The
candidate schema defines `identity` with `additionalProperties: false` and a
closed `required` list, so the key must be declared there and
`parser-rq-candidate.schema.json`'s `schema_version` bumped off `1.0.0`.

The current three-work corpus expects no diagnostics anywhere, so enforcing the
expectation changes no outcome on it. That is deliberate: the behaviour change
is characterized against the present corpus first, where the expected result is
"no change in outcome", before any membership rotation moves the corpus itself.

Enforcement makes `:expected_diagnostics` load-bearing, so the corpus can no
longer be edited casually — a wrong expectation now fails a capture rather than
sitting unread. That is the intended cost.

This record does not expand the corpus. A corpus that provably exercises no
diagnostic path remains weak evidence for this predicate, and the argument for
adding works that must emit diagnostics stands on its own; it is simply not a
gate question.

## Evidence

The per-work expectation comparison is checked in
`test/abc/tools/parser_rq_diagnostic_completeness_test.clj`: a corpus where
every work expects nothing and emits nothing passes at 3/3 rather than by
vacuity, a work emitting a diagnostic it should not drops the ratio to 2/3, a
work failing to emit one it should drops it to 0, a repeated code is a
different claim than a single one, and a work the policy does not govern
cannot be measured at all. The new identity coordinate is checked in
`test/abc/tools/parser_rq_campaign_test.clj`: the bound membership equals the
capture members exactly, a missing policy fails the build, and editing an
instrument policy rotates `qualification_identity_ref` while leaving
`predicate_set_hash` unmoved.

The committed predicate-hardening capture is the fixture that makes both
load-bearing. Regenerating it now yields `diagnostic_completeness` 0.667, not
1.0, because one of its three works is fed a source that emits a diagnostic it
is not governed to emit — the first evidence in this campaign that the
predicate discriminates at all.

One defect surfaced only through that fixture and is worth naming. The ratio
was first written `(double (/ matching expected))`, which builds an exact
Ratio and rounds through BigDecimal; 2/3 becomes `...667` on that path and
`...666` under IEEE division, so the Clojure instrument and the Python capture
driver disagreed on a value that lands in content-addressed evidence. It is
now IEEE division of two doubles on both sides. The sibling parser-IR
conformance instrument computes its ratio the same unsafe way and has not been
changed here, because doing so rotates a second instrument identity and is its
own governed change.
