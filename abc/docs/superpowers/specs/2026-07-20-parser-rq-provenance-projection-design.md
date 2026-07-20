# Parser-RQ Provenance Projection Design

Date: 2026-07-20
Status: Approved for implementation planning

## Purpose

Prevent another authorized parser-release capture from reaching promotion with
an executable-provenance identity that the candidate cannot authenticate.

The completed P5 campaign exposed one contract defect. Candidate construction
hashed the unbound reproducibility proof:

```text
{builds, executables, provenance_core_ref, status}
```

`bind-provenance` then added transport and binding fields:

```text
{schema_id, schema_version, candidate_ref, qualification_identity_ref}
```

Promotion removed only the two binding fields before recomputing
`executable_provenance_ref`. The remaining schema fields changed the hash.
Authorization did not detect this because it authenticated the stable
`provenance_core_ref` and candidate bindings, but not the candidate's
`executable_provenance_ref`.

The evidence itself remains honest: both independent builds agree, the bound
provenance core authenticates, and all nine release predicates pass. ADR 0039
correctly remains Proposed at the campaign's terminal commit because the
then-current promotion verifier rejects the candidate/provenance relation.
Once this correction lands, that immutable candidate may be re-evaluated by the
corrected verifier. If every promotion check then passes, the correction removes
the only known technical barrier to promoting ADR 0039; it does not create a
policy reason to discard valid evidence.

## Decision

Define one Clojure projection, `candidate-provenance-value`, as the exact value
whose JCS hash is stored in `candidate.executable_provenance_ref`.

For an unbound proof, the projection is the proof unchanged. For a bound
provenance record, it removes exactly these binding-envelope fields:

- `schema_id`
- `schema_version`
- `candidate_ref`
- `qualification_identity_ref`

It retains `provenance_core_ref`. That field authenticates the semantic core
and was already present when the candidate was constructed; removing it would
silently change the established candidate identity contract.

The projection is a no-op on an unbound proof because none of the four envelope
keys is present. Candidate construction therefore produces the same
`executable_provenance_ref` before and after this correction. The behavior
change is confined to authenticating the bound representation of that proof.
This mirrors the existing `provenance-core-ref` projection, which already
removes the same envelope fields before hashing its narrower semantic core.

`executable-provenance-ref` hashes only this projection. Candidate construction,
authorization verification, direct provenance verification, and promotion all
use that one function. No Python identity definition changes: Python continues
to own creation of `provenance_core_ref` and the bound schema; Clojure
independently authenticates the values it consumes.

Because the schema envelope is deliberately outside the candidate projection,
`verify-provenance-errors` becomes explicitly bound-record-only. It separately
requires the exact v2 `schema_id`, `schema_version`, and closed eight-key set:
`status`, `builds`, `executables`, `provenance_core_ref`, and the four envelope
fields. Its current conditional candidate/qualification binding guards become
unconditional. `build-candidate` continues to validate unbound proofs with
`provenance-errors`, not `verify-provenance-errors`.

Projection answers whether two representations contain the same candidate
evidence. `provenance-errors` checks the reproducibility proof's structure.
`verify-provenance-errors` also recomputes `provenance_core_ref` to authenticate
the core-to-evidence relation; `verify-readiness-receipt` retains its existing
copy as an independent receipt-boundary check. Bound-envelope validation checks
the supplied protocol representation. No one check substitutes for the others.

## Authorization Boundary

`verify-authorization-record` must include `verify-provenance-errors`. It already
authenticates the provenance core and candidate/qualification bindings through
`verify-readiness-receipt`. The new call adds structural provenance validation,
bound-envelope validation, an independently recomputed core check, and the
candidate's `executable_provenance_ref` relation.

A capture authorization is structurally valid only when:

1. the bound provenance core authenticates its executable/build evidence;
2. its candidate and qualification bindings match the candidate; and
3. projecting the bound record reproduces the candidate's
   `executable_provenance_ref`.

This moves detection to the last non-volatile boundary before authorization is
committed and before the one-shot capture begins. Promotion must call the same
`verify-provenance-errors` function rather than retain its narrower inline hash
comparison. The two release boundaries therefore share one complete
provenance-authentication path.

## Failure Semantics

Any candidate/provenance projection mismatch detected before capture makes
authorization invalid. The operator must not rewrite the candidate or
provenance record. The contract defect is fixed before a candidate is
authorized.

The completed candidate at
`sha256:15affdfb677cc6a94a4a5364da68ca2d11441f899737651e727dbac90eddc5ab`
remains immutable. The one-shot rule forbids another capture attempt; it does
not forbid correcting a pure verifier and re-evaluating the already-published
values. After this change, promotion may be rerun against that exact candidate,
capture, evaluation, provenance record, registry, and report. Zero errors permit
the ordinary ADR 0039 promotion; any remaining error keeps it Proposed. No
published value is rewritten or reinterpreted as a different observation.

## Verification

Add focused tests that prove:

- an unbound proof and its correctly bound v2 provenance record produce the
  same `executable_provenance_ref`;
- applying the new projection to an unbound proof is byte-for-value identical
  to the pre-correction candidate hash input;
- changing a semantic proof field changes that reference;
- changing only candidate or qualification bindings does not change the
  projected reference but is rejected by the existing binding checks;
- changing a schema-envelope field does not change the projected reference,
  but is rejected by bound-envelope validation;
- changing evidence while retaining a stale `provenance_core_ref` is rejected;
- `verify-authorization-record` rejects a candidate whose
  `executable_provenance_ref` does not authenticate the supplied provenance;
- promotion routes through `verify-provenance-errors` and rejects every bound
  envelope, structure, core, identity, and candidate-reference defect that
  authorization rejects;
- the real Python `bind-provenance` output passes the Clojure authorization and
  promotion provenance checks.

Run the focused Clojure campaign tests, Python provenance tests, parser-RQ
admission/promotion smoke, comment hygiene, and `just validate-migration`.

## Alternatives Rejected

### Bind the candidate directly to `provenance_core_ref`

This is conceptually smaller but changes the candidate schema and identity
contract after evidence already exists. The current reference intentionally
binds the complete reproducibility proof, including its declared core hash.
That broader migration is unnecessary.

### Make schema fields part of the candidate hash

Candidate construction occurs before binding and therefore has no bound schema
envelope. Adding those fields earlier would duplicate Python's binding concern
and still require two representations to remain synchronized.

### Remove only schema fields inside promotion

That would fix the observed symptom while leaving candidate construction,
authorization, direct verification, and promotion free to drift again. The
projection must be named once and reused at every boundary.

## Scope Fence

This change does not alter executable bytes, build reproducibility,
`provenance_core_ref`, candidate or qualification schemas, predicate identity,
capture/evaluation values, registry admission, ADR 0040, or ADR 0039 status.
It does not retry or amend the completed P5 candidate. It may allow that
candidate's unchanged evidence to pass the corrected promotion verifier; ADR
0039 status changes only in the later, separately verified governance commit.

## Falsifiers

Reopen the design if:

- Python binding changes any field retained by `candidate-provenance-value`;
- an unbound proof and its bound record cannot project to byte-identical JCS;
- authorization cannot call the existing provenance verifier after an ordinary
  forward declaration or definition reorder; or
- the fix requires changing any already-published candidate or capture value.
