# ADR Evidence and Lifecycle Remediation Design

Date: 2026-07-11
Status: Proposed design for review
Parent: `2026-07-11-adr-logical-remediation-program-design.md`

## Purpose

Make ADR acceptance mean that appropriately typed evidence satisfies every
binding claim, and make canonical status closed over dependencies.

## Current Faults

`abc.tools.adr` currently verifies that evidence-looking paths exist beneath
allowed repository roots. It does not express what a path proves, whether the
evidence ran, its scope, or whether the observed result passed the claimed
predicate. It also permits an Accepted ADR to depend on Draft or Proposed ADRs
when a human-readable scope is present.

A Prolog reachability check over the current ADR headers identifies exactly one
Accepted-to-non-Accepted dependency counterexample: ADR 0031 reaches Proposed
ADR 0029 directly. This result establishes the immediate repair target; the
repository validator must evaluate the property over the complete graph on
every run.

## Decision

### Separate lifecycle dimensions

ADR headers gain two required fields for Accepted ADRs:

```text
Validation scope: structural | fixture | smoke-corpus | full-corpus | operational
Release authority: none | development | publication
```

`Status` continues to mean decision canonicity. These fields do not create
additional lifecycle statuses.

### Typed claim evidence

Acceptance Criteria use stable claim IDs and cite entries in a new checked-in
`docs/adr/adr-evidence.edn` registry. An entry contains:

```clojure
{:claim-id "ADR-0034-C1"
 :claim-kind :external-semantics
 :evidence-kind :external-authority
 :evidence-path "docs/evidence/..."
 :scope {:source "..." :version "..."}
 :expected {:operator := :value "..."}
 :observed {:value "..."}}
```

Supported evidence kinds are:

- `:structural-test`
- `:fixture-conformance`
- `:corpus-measurement`
- `:benchmark`
- `:external-authority`
- `:cross-implementation`
- `:expert-assessment`
- `:operational-observation`

Supported claim kinds and their admissible evidence are a closed checked-in
matrix:

| Claim kind | Admissible evidence kind |
| --- | --- |
| `:structural-invariant` | `:structural-test` |
| `:fixture-behavior` | `:fixture-conformance` |
| `:corpus-behavior` | `:corpus-measurement` |
| `:performance-bound` | `:benchmark` |
| `:external-semantics` | `:external-authority` |
| `:implementation-agreement` | `:cross-implementation` |
| `:domain-interpretation` | `:expert-assessment`, optionally corroborated by `:external-authority` |
| `:operational-behavior` | `:operational-observation` |

The matrix lives in `docs/adr/claim-evidence-compatibility.edn` and the
validator rejects unknown kinds or pairs absent from it. Multiple evidence
entries may support one claim, but every entry must be compatible.

Local executable evidence records the command and a result-artifact hash.
External authority evidence records a stable URL, retrieval date, and a local
bounded summary or excerpt hash; validation does not require network access.

### Evidence input binding and freshness

Every evidence entry carries an `:inputs` map of the exact hashes or versioned
coordinates against which it was observed. The keys are opaque to governance;
workstream contracts define their meaning. Examples include corpus snapshot,
parser build, mapping, schema, source snapshot, and fixture-set hashes.

An executable evidence entry is stale when its result artifact records inputs
different from the registry entry or when a current-contract binding named by
the claim differs. External authority evidence carries `:retrieved-at` and
`:review-after`. The validator compares that date to the committed
`docs/adr/governance-as-of.edn` date, never the process wall clock. Advancing
the reference date is a deliberate reviewed commit; passing the review date
makes evidence stale until a reviewer records a new observation. Offline
validation verifies these bindings and dates deterministically; it does not
claim the remote authority is unchanged merely because a URL exists.

`:verdict` is not stored. The validator derives the verdict from `:expected`,
`:observed`, and the operator. This removes a mutable duplicate of a computed
value.

### Dependency closure

An Accepted ADR may depend only on Accepted ADRs. Scoped dependencies remain
useful documentation but no longer waive lifecycle compatibility. When only a
scope of a larger Proposed design is canonical, that scope must be extracted
into its own Accepted ADR.

ADR 0031's dependency on Proposed ADR 0029 is repaired by extracting or
accepting the generated-decision-graph contract; it is not grandfathered.

The production validator computes this closure with an ordinary deterministic
visited-set breadth-first traversal. That algorithm owns termination on cyclic
input, stable target ordering, and the shortest witness path included in each
problem. Logic-programming or solver formulations may independently check the
reachability invariant during design review, but they are not production
dependencies and do not replace witness-path construction.

### Honest language

Governance rejects Acceptance Criteria that use `prove` or `proves` unless the
claim kind is structural and the evidence is an executable structural test.
Other criteria use `demonstrates`, `measures`, `conforms`, or `supports`.
This lexical rule is a teaching lint only. The compatibility matrix and
predicate evaluation are the actual authority checks; synonyms such as
`establishes` do not bypass them.

## Module Boundary

`abc.tools.adr` remains the Markdown/lifecycle parser. A focused
`abc.tools.adr-evidence` module owns registry parsing, claim/evidence
compatibility, predicate evaluation, evidence artifact verification, and
diagnostics. This is one cohesive protocol concern and avoids expanding the
already broad Markdown parser.

## Migration

1. Add the new fields and evidence registry in permissive audit mode.
2. Generate a report classifying every existing Accepted criterion.
3. Repair or narrow claims; do not manufacture passing evidence.
4. Extract canonical scopes currently hidden inside Proposed dependencies.
5. Remove the scoped-dependency waiver.
6. Switch evidence validation from audit to enforcement.

The governance ADR is bootstrapped under the existing gate, then immediately
evaluated by the new validator using structural-test evidence for the validator
and a full-corpus governance report for the migrated ADR set. Enforcement is
not enabled unless this self-evaluation passes. This is bootstrap validation,
not a claim that the governance rules establish their own semantic correctness.

An ADR that lacks sufficient evidence becomes Proposed through a superseding
or scoped corrective ADR; historical text remains intact.

## Failure Handling

The validator accumulates all problems. Missing evidence, incompatible kinds,
failed predicates, stale result hashes, noncanonical dependencies, and invalid
release authority are distinct machine-readable problem kinds.

## Acceptance Criteria

- Every Accepted ADR has validation scope and release authority.
- Every binding Acceptance Criterion has a unique claim ID and registry entry.
- The claim-kind domain and complete compatibility matrix are checked in and
  unknown claim kinds are rejected.
- Wrong-kind, failed, missing, stale, or unexecuted evidence is rejected.
- Accepted-to-non-Accepted dependencies are rejected regardless of scope.
- The complete ADR corpus passes without allowlists.
- The governance report distinguishes structural conformance from empirical
  and external-semantic support.

## Safe Fallback

The evidence registry and reports can be removed without changing artifact
identity. Do not restore acceptance claims whose only support was path
existence; rollback leaves those ADR scopes Proposed.
