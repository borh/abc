# Scope Instrument Dependency Identity to the Package, Not the Workspace

## Implementation Status

Accepted on 2026-07-26. Both predicate-hardening instruments carry regenerated
manifests and policies; the parser-IR instrument binds a package-scoped locked
dependency projection instead of the workspace lockfile's bytes.

## Context

An instrument's `validator_semantics_hash` answers one question: if this hash is
unchanged, can the instrument's verdict have changed? It is taken over the
reviewed source closure, the artifacts the instrument reads, and a runtime
block.

For `parser-ir-conformance`, `ab-validator/Cargo.lock` was one of those
artifacts, hashed whole. A workspace lockfile is not a property of one package.
Adding the unrelated `ab-aozora-capture` crate rotated this instrument's
identity even though `ab-aat-to-parser-ir`'s own lock entry stayed
byte-identical and no resolved third-party version changed anywhere in the
file. The committed policy consequently no longer equalled its regenerated form
— and nothing noticed, because the suite that would have compared them errored
at import and was built by no recipe (repaired separately in the step recorded
by the instrument-semantics audit).

Two outcomes follow from binding a value wider than the claim. Either every
unrelated workspace edit forces a requalification, or the committed policy
drifts and the equality gate becomes noise to be worked around. The second is
what happened.

The `diagnostic-completeness` instrument had a different fault with the same
root: its reviewed closure named `abc/src/abc/tools/evidence_io.clj`, deleted
under ADR 0043, plus three sources reachable only through it. Its committed
identity covered bytes that no longer exist, and the generator refused to run
at all.

A third restatement compounded both. The runtime block asserted
`"jsonschema_crate": "0.46.9"` as a hardcoded literal, derived from nothing.
The workspace declares only `jsonschema = "0.46"`, so a patch bump would have
left the manifest asserting a version the instrument no longer compiled
against.

## Decision

An instrument that depends on a Cargo workspace binds a **package-scoped locked
dependency projection**: the transitive `Cargo.lock` closure of the one package
under review, projected to each member's name, version, and checksum, sorted.
The lockfile's bytes are no longer an artifact of the instrument.

The closure is keyed by name **and version**. Thirty-six names in this
workspace resolve to more than one version simultaneously — `sha2`, `rand`,
`hashbrown`, `windows-sys` among them — and collapsing them by name alone both
lets one version's entry stand in for another's and drops the co-resolved
siblings entirely: it yielded 427 packages where the correct closure has 480.
`Cargo.lock` omits the version from a dependency entry only when the name is
unambiguous, so a bare name with several candidates is rejected rather than
guessed at.

The projection is a value in the manifest, not only a hash of one. A reader can
see which resolved graph was claimed without regenerating anything, matching how
`expected_work_ids` is disclosed alongside `expected_work_set_hash`.

`jsonschema_crate` is read out of that projection. A crate absent from the
closure fails closed rather than being restated.

The manifest schema is therefore
`parser-rq-predicate-validator-identity-v2`, and both instruments' identities
rotate in one commit: `diagnostic-completeness` because its reviewed closure is
corrected to the seven sources that survive, `parser-ir-conformance` because its
dependency scope changed.

## Consequences

Adding a workspace package that the instrument does not depend on no longer
rotates its identity. A version change anywhere in the package's own transitive
closure still does — `jsonschema`, which decides schema-valid from
schema-invalid, is in that closure and remains bound.

**Two gaps are stated rather than closed.** First, workspace-local members carry
no checksum in `Cargo.lock`. Twenty-two local crates sit in this instrument's
closure — `ab-aozora-aat`, `ab-aozora-pipeline`, `sudachi`, and others — and
their bytes are outside both the projection and `reviewed_sources`. Editing one
can change conversion behaviour while rotating nothing. The projection marks
them `"origin": "workspace-local"` so the manifest does not read as though a
version-only entry bound the crate's source. Second, `Cargo.lock` records no
feature selection, so a feature change that alters behaviour without changing
the resolved set rotates nothing either.

Neither gap is new and neither was closed by hashing the lockfile whole: a
source edit in a sibling crate never changed `Cargo.lock` at all. Binding the
whole file bought the appearance of dependency coverage without the substance,
which is the stronger reason to stop. Binding the Nix derivation output would
close the first gap, but `mkRustBin` takes the entire `ab-validator` workspace
as its source, so it rotates on strictly more unrelated changes than the
lockfile did.

The accepted qualification recorded under run
`15affdfb677cc6a94a4a5364da68ca2d11441f899737651e727dbac90eddc5ab` remains
accepted as of the inputs it names. Its capture is immutable, internally
consistent, and records the instrument identities in force when it ran; those
are now historical. A capture under the rotated identities is owed and is
tracked separately.

No measured value changes. Regenerating the predicate-hardening capture fixture
under the rotated policies re-addressed every content-addressed record, but each
blob is byte-identical once `policy_hash` is projected out, and the two index
blobs differ only in the refs those re-addressed records produced.

## Evidence

The projection's discrimination — unrelated workspace packages ignored,
transitive version moves and newly acquired dependencies rotating, co-resolved
versions of one name kept distinct, ambiguous and unresolvable dependency
entries failing closed, local members marked uncovered, the schema crate read
rather than restated — is checked in
`ab-validator/reports/parser-ir/tests/test_predicate_hardening_identity.py`,
together with the equality of both committed manifests and both committed
policies against their regenerated forms. That suite runs in the `ab-validator`
flake check `parser-rq-publication-pytest`, built by `just
parser-rq-instrument-identity`.

That the rotation changes no observation is checked in
`test/abc/tools/parser_rq_predicate_hardening_capture_test.clj`, which
re-derives the committed capture from the live policies and compares records
byte-for-byte.
