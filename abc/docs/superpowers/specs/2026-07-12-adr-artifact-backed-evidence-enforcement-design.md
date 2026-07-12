# ADR Artifact-Backed Evidence Enforcement Design

Date: 2026-07-12
Status: Approved design for implementation planning
Parent: `2026-07-11-adr-evidence-and-lifecycle-remediation-design.md`

## Purpose

Complete the missing trust boundary between ADR Acceptance Criteria, the typed
evidence registry, immutable evidence artifacts, and the `adr-governance`
command. Governance must derive acceptance from independently checkable values,
not from mutually agreeing fields edited in one registry entry.

## Discovered Implementation Gap

The current audit-mode implementation validates ADR lifecycle fields,
relations, dependency closure, and legacy evidence paths. The separate
`abc.tools.adr-evidence` module validates claim/evidence compatibility and
predicates, but `abc.tools.adr-governance` never invokes it. ADR Markdown does
not expose parsed claim IDs or claim kinds, registry entries are not joined to
Acceptance Criteria, and evidence input freshness compares two values stored
in the same registry entry.

Enabling enforcement in this state would enforce only lifecycle metadata, not
the typed-evidence regime described by ADR 0033. The current Accepted corpus
contains 25 ADRs and 137 Acceptance Criteria, so migration must follow a real
protocol rather than manufacture nominal passing entries.

## Decision

### Separate editorial declarations from observed evidence

`docs/adr/adr-evidence.edn` remains an editorial registry. It declares which
claim an entry supports, the claim/evidence kinds, the immutable evidence
artifact coordinate, the named observation to read, and the expected
predicate. It does not store observations, input bindings, or verdicts.

Deterministic JSON evidence bundles under `docs/evidence/adr-runs/` own
producer identity, executable command or external-source provenance,
versioned input bindings, and named observations. One bundle may support many
claims. Registry entries select an observation using the bundle's canonical
hash and an observation key.

The validation flow is:

```text
Accepted ADR criterion
  -> exact claim ID and claim kind
  -> registry coverage and compatibility
  -> evidence artifact path and recomputed canonical hash
  -> named observation
  -> independently recomputed repository input hashes
  -> expected predicate
  -> derived verdict
```

### Claim syntax

Every Accepted Acceptance Criterion begins with this exact form:

```markdown
- **ADR-0033-C1 — structural-invariant:** Claim text and evidence citations.
```

Claim IDs match `^ADR-[0-9]{4}-C[1-9][0-9]*$`. The four-digit claim prefix
must match the containing ADR number. Claim IDs are unique repository-wide.
Each criterion carries exactly one claim ID and one claim kind.

Claim kinds use these exact Markdown tokens and map to the existing EDN
keywords:

| Markdown token | Registry keyword |
| --- | --- |
| `structural-invariant` | `:structural-invariant` |
| `fixture-behavior` | `:fixture-behavior` |
| `corpus-behavior` | `:corpus-behavior` |
| `performance-bound` | `:performance-bound` |
| `external-semantics` | `:external-semantics` |
| `implementation-agreement` | `:implementation-agreement` |
| `domain-interpretation` | `:domain-interpretation` |
| `operational-behavior` | `:operational-behavior` |

Proposed ADRs may use the same syntax so their intended claims can be
reviewed. Registry coverage becomes mandatory only for Accepted ADRs; a
Proposed ADR cannot be promoted until every criterion has compatible passing
coverage.

### Registry contract

An executable-evidence registry entry has this shape:

```clojure
{:claim-id "ADR-0033-C1"
 :claim-kind :structural-invariant
 :evidence-kind :structural-test
 :artifact-path "docs/evidence/adr-runs/adr-governance-tests.json"
 :artifact-hash "sha256:0000000000000000000000000000000000000000000000000000000000000000"
 :observation-key "typed-evidence-contract"
 :expected {:operator := :value true}}
```

The registry rejects `:observed`, `:inputs`, and `:verdict`. Multiple entries
may support one claim. At least one compatible passing entry is required;
every supplied entry must itself be valid so an invalid corroborating entry
cannot be hidden behind another passing entry. Exact duplicate entries are
rejected.

The registry's `:claim-kind` must equal the kind parsed from the corresponding
criterion. Evidence-kind admissibility continues to be owned by
`claim-evidence-compatibility.edn`.

### Executable run-bundle contract

An executable bundle is deterministic JSON:

```json
{
  "schema_version": "abc-adr-evidence-run-v1",
  "producer": {
    "tool": "bin/kaocha",
    "command": "bin/kaocha --focus abc.tools.adr-evidence-test",
    "revision": "0000000000000000000000000000000000000000"
  },
  "inputs": {
    "test/abc/tools/adr_evidence_test.clj": "sha256:<64 hex>",
    "src/abc/tools/adr_evidence.clj": "sha256:<64 hex>"
  },
  "observations": {
    "typed-evidence-contract": {
      "value": true,
      "details": {"tests": 4, "failures": 0, "errors": 0}
    }
  }
}
```

`schema_version`, `producer`, `inputs`, and `observations` are required and
the object is closed. `producer.tool`, `producer.command`, and
`producer.revision` are non-empty strings. Input keys are normalized
repository-relative paths with no absolute paths, `..`, symlink escapes, or
duplicate normalized forms. Input values are formatted SHA-256 strings.
Observation keys are non-empty stable identifiers. Each observation has a
required JSON `value` and optional closed `details` object.

The artifact hash is RFC 8785 canonical JSON SHA-256 using the repository's
existing hash implementation. It is computed over the complete bundle. The
validator also hashes every current input file and compares it to the bundle's
binding. Missing, changed, or escaped inputs make the evidence stale.

The producer revision records provenance but is not compared to repository
HEAD: evidence may validly originate from an earlier revision when every
declared input remains byte-identical. Undeclared transitive inputs are an
evidence-authoring error addressed by review and workstream-specific tests;
the governance validator does not attempt automatic build-system dependency
discovery.

### External-authority bundle contract

External evidence uses a separate closed deterministic JSON schema with
`schema_version: "abc-adr-external-evidence-v1"`. It records:

- a stable source URL;
- retrieval and review-after ISO dates;
- a repository-relative bounded-summary path and SHA-256 hash;
- optional additional repository input bindings; and
- named observations with the same value/details shape as executable bundles.

The validator remains offline. It recomputes the local summary and additional
input hashes, resolves the named observation, and compares `review_after` to
the explicit date in `governance-as-of.edn`. Passing the review date makes the
entry stale; it does not assert that the remote source changed.

### Aggregate governance ownership

`abc.tools.adr` owns Markdown parsing, lifecycle fields, relations, dependency
closure, criterion extraction, and claim-header syntax. It returns claim
values but does not read evidence artifacts.

`abc.tools.adr-evidence` owns registry validation, bundle loading and schema
validation, canonical artifact hashing, repository input hashing, freshness,
compatibility, observation lookup, and predicate evaluation. Its public API
returns deterministic problem values and does not exit.

`abc.tools.adr-governance` is the sole aggregate gate. For audit and enforce
modes it concatenates ADR structural problems with typed-evidence problems in
stable order. Legacy mode retains the pre-migration behavior. Audit mode
reports every problem and exits zero; enforce mode exits nonzero on any
problem.

Run-bundle generation is separate from validation. Generators may execute
tests and write deterministic reports. The offline validator only consumes
committed artifacts and recomputes their identities. This keeps evidence
execution, evidence capture, and policy evaluation separate.

## Failure Taxonomy

The aggregate report distinguishes at least:

- `:missing-claim-header`
- `:malformed-claim-header`
- `:claim-adr-mismatch`
- `:duplicate-claim-id`
- `:unknown-claim-kind`
- `:missing-claim-evidence`
- `:duplicate-evidence-entry`
- `:claim-kind-mismatch`
- `:unknown-evidence-kind`
- `:incompatible-evidence-kind`
- `:forbidden-inline-evidence-value`
- `:missing-evidence-artifact`
- `:evidence-path-traversal`
- `:evidence-real-path-escape`
- `:invalid-evidence-artifact`
- `:artifact-hash-mismatch`
- `:missing-observation`
- `:missing-evidence-input`
- `:input-hash-mismatch`
- `:predicate-failed`
- `:expired-evidence`

Problems carry ADR file, criterion index, claim ID when known, artifact path
when applicable, and a stable message. The validator accumulates problems
rather than failing at the first invalid entry.

## Migration and Enforcement

Migration proceeds without red intermediate commits:

1. Add both bundle schemas and pure validation while governance behavior is
   unchanged.
2. Parse claim headers and report their problems only in audit/enforce modes.
3. Join claims, registry entries, and artifacts in `adr-governance`; legacy
   mode remains unchanged.
4. Generate bundles from real focused checks.
5. Migrate the 137 Accepted criteria without deleting or weakening their
   substantive requirements.
6. Require an empty audit report.
7. Add ADR 0033's structural and full-corpus evidence.
8. Promote ADR 0033 and switch the Nix gate to enforcement in the same final
   transition.

The corpus migration is split into reviewable families:

- foundation, runtime, and identity;
- schema, RDF, and TEI;
- temporal, person, and ingest;
- parser, IR, and publication;
- diagrams and governance; and
- ADR 0033 self-certification and enforcement.

Claims that lack adequate evidence are not given invented passing
observations. Their governing scope remains or becomes Proposed through an
explicit corrective ADR.

## Testing

- JSON Schema tests accept complete executable and external bundles and reject
  unknown fields, malformed hashes, invalid dates, and invalid paths.
- Claim-parser tests cover valid headers, multiline criteria, malformed IDs,
  ADR-number mismatch, unknown kinds, and repository-wide duplication.
- Registry tests cover complete coverage, multiple corroborating entries,
  duplicates, forbidden inline values, kind mismatch, and compatibility.
- Artifact tests cover canonical-hash drift, missing observations, lexical and
  real-path escapes, missing inputs, and current-input hash drift.
- Predicate property tests retain agreement with the corresponding Clojure
  operators and never trust a stored verdict.
- Command tests demonstrate that audit reports typed problems while exiting
  zero and enforcement fails on the same corpus.
- A corpus test asserts the exact Accepted ADR and criterion inventory during
  migration and then asserts complete claim/evidence coverage.
- Final verification runs focused Kaocha tests, the ADR governance Nix build,
  the design-bundle validator, and root `just validate-migration`.

## Recovery

Before enforcement, retain audit mode and repair the protocol or artifacts
forward. After enforcement, stale or invalid evidence is replaced by a newly
observed, independently bound artifact. Do not recover by restoring path-only
evidence or inline self-certified observations.
