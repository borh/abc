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

The inline contract is already executable, not merely proposed:
`adr_evidence.clj/validate-entry` reads `:observed` and `:inputs`, and
`adr_evidence_test.clj` asserts passing inline observations and stale inline
input detection. Bundle migration therefore explicitly replaces this code and
its tests; the old and new contracts never coexist as two accepted entry
shapes.

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
  "input_profile": {
    "kind": "clojure-test-v1",
    "roots": ["abc.tools.adr-evidence-test"],
    "explicit": []
  },
  "inputs": {
    "test/abc/tools/adr_evidence_test.clj": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
    "src/abc/tools/adr_evidence.clj": "sha256:1111111111111111111111111111111111111111111111111111111111111111"
  },
  "observations": {
    "typed-evidence-contract": {
      "value": true,
      "details": {"tests": 4, "failures": 0, "errors": 0}
    }
  }
}
```

`schema_version`, `producer`, `input_profile`, `inputs`, and `observations` are
required and the object is closed. `producer.tool`, `producer.command`, and
`producer.revision` are non-empty strings. Input keys are normalized
repository-relative paths with no absolute paths, `..`, symlink escapes, or
duplicate normalized forms. Input values are formatted SHA-256 strings.
Observation keys are non-empty stable identifiers. Each observation has a
required JSON `value` and an optional `details` object whose string-keyed
values use the same recursively restricted JSON value grammar.

The artifact hash is RFC 8785 canonical JSON SHA-256 using the repository's
existing hash implementation. It is computed over the complete bundle. The
validator also hashes every current input file and compares it to the bundle's
binding. Missing, changed, or escaped inputs make the evidence stale.

The existing JCS implementation does not guarantee ES6-compatible
canonicalization of floating-point, `BigDecimal`, or `BigInteger` values.
Evidence bundles therefore prohibit non-integer JSON numbers. Integers must be
within the I-JSON interoperable range `[-9007199254740991, 9007199254740991]`.
Measurements that originate as decimals are encoded as scaled integers with an
explicit unit in observation details, for example `duration_ns`, `ratio_ppm`,
`bytes`, or `count`. Expected predicates use the same scaled-integer
representation. Strings may preserve a human display form but are not used for
ordered numeric predicates. This restriction applies recursively to
observation values and details and permits `:benchmark` evidence without
depending on the unresolved floating-point canonicalization path.
For every `:benchmark` registry entry, the selected observation must have an
integer value and `details.unit` must be one of `nanoseconds`, `bytes`,
`count`, or `parts-per-million`; other units require a versioned schema change.

The producer revision records provenance but is not compared to repository
HEAD: evidence may validly originate from an earlier revision when every
declared input remains byte-identical. Executable bundles also declare an
`input_profile` so freshness does not rely wholly on an author remembering the
input set. The initial profiles are:

- `clojure-test-v1`: generation starts from named test namespaces, reads their
  `ns` forms with reader evaluation disabled, recursively resolves
  repository-local `:require` dependencies under `src/` and `test/`, and binds
  every resulting file. Fixture, schema, EDN, and other non-namespace inputs
  remain explicit additions.
- `repo-files-v1`: evidence is defined by an explicit closed file set, as for
  fixture conformance, generated reports, and bounded expert assessments.
- `external-authority-v1`: the bounded local summary is mandatory and any
  additional local inputs are explicit.

The offline validator reruns the selected profile's minimum-input derivation
and rejects a bundle whose bound input set omits a derived file. Extra declared
inputs are allowed and are also hash-checked. This does not claim automatic
discovery of `load`, `require` forms evaluated outside the first `ns` form,
dynamically computed dependencies, runtime-read data files, environment
variables, external services, or arbitrary non-Clojure build graphs. Such
inputs must appear in `input_profile.explicit`. The profile guarantees the
statically visible namespace closure plus those explicit inputs; it does not
claim the complete runtime dependency graph.

### External-authority bundle contract

External evidence uses a separate closed deterministic JSON schema with
`schema_version: "abc-adr-external-evidence-v1"`. It records:

- a stable source URL;
- retrieval and review-after ISO dates;
- a repository-relative bounded-summary path and SHA-256 hash;
- optional additional repository input bindings; and
- named observations with the same value/details shape as executable bundles.

Both dates must satisfy JSON Schema `format: date` and the lexical pattern
`^[0-9]{4}-[0-9]{2}-[0-9]{2}$`. The validator also parses both values as real
calendar dates. A missing or unparseable required date is
`:invalid-evidence-artifact`; it never disables expiry by falling through to a
non-expiring state.

The validator remains offline. It recomputes the local summary and additional
input hashes, resolves the named observation, and compares `review_after` to
the explicit date in `governance-as-of.edn`. Passing the review date makes the
entry stale; it does not assert that the remote source changed.

`governance-as-of.edn` is one coordinated repository evaluation epoch. It is
advanced deliberately to re-evaluate every external source, not to refresh one
source. Refreshing one source updates that source bundle's retrieval and
review-after dates; advancing the global epoch may legitimately make other
sources stale at the same time.

### Aggregate governance ownership

`abc.tools.adr` owns Markdown parsing, lifecycle fields, relations, dependency
closure, criterion extraction, and claim-header syntax. It returns claim
values but does not read evidence artifacts.

`abc.tools.adr-evidence-bundle` owns bundle loading and schema validation,
canonical artifact hashing, repository input hashing, freshness, input-profile
derivation, and observation lookup.

`abc.tools.adr-evidence` owns registry validation, claim-to-artifact joins,
compatibility, and predicate evaluation. Both evidence modules expose pure
validation APIs that return deterministic problem values and do not exit.

`abc.tools.path-containment` owns the single lexical and real-path containment
primitive used by both legacy ADR evidence-path checks and bundle validation.
It has no governance semantics; it returns deterministic path states so the two
callers cannot drift on traversal or symlink-escape handling.

`abc.tools.adr-governance` is the sole aggregate gate. For audit and enforce
modes it concatenates ADR structural problems with typed-evidence problems in
stable order. Legacy mode retains the pre-migration behavior. Audit mode
reports every problem and exits zero; enforce mode exits nonzero on any
problem.

Run-bundle generation is separate from validation. Generators may execute
tests and write deterministic reports. The offline validator only consumes
committed artifacts and recomputes their identities. This keeps evidence
execution, evidence capture, and policy evaluation separate.

The version-1 executable capture tool requires `git status --porcelain
--untracked-files=all` to be empty both immediately before and immediately
after the evidence command. A revision from a dirty tree would not identify
the bytes used to produce the bundle, even though input hashes remain
authoritative. There is no dirty-tree override in version 1; capture output
must be written outside the repository or created only after the final
cleanliness check.

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
- `:predicate-type-mismatch`
- `:predicate-failed`
- `:expired-evidence`

Problems carry ADR file, criterion index, claim ID when known, artifact path
when applicable, and a stable message. The validator accumulates problems
rather than failing at the first invalid entry. Predicate evaluation validates
operator/operand compatibility before comparison so an incomparable value is
reported as `:predicate-type-mismatch`, not as a failed scientific bound.

Artifact-root failures such as a missing bundle, artifact-hash drift, invalid
schema, or input-hash drift are emitted once per artifact/input root cause and
carry a sorted `:affected-claim-ids` vector. Claim-local problems such as a
missing observation or failed predicate remain per claim. This prevents one
stale shared bundle from producing 137 indistinguishable diagnostics while
preserving its complete impact set.

## Migration and Enforcement

Migration proceeds without red intermediate commits:

1. Add both bundle schemas and pure bundle validation while governance
   behavior is unchanged.
2. Retire the inline `:observed`/`:inputs` validation path and rewrite
   `adr_evidence_test.clj` around artifact references in the same commit. Audit
   mode remains the only caller, so no Accepted corpus claim is promoted by a
   partially migrated contract.
3. Parse claim headers and report their problems only in audit/enforce modes.
4. Join claims, registry entries, and artifacts in `adr-governance`; legacy
   mode remains unchanged.
5. Generate bundles from real focused checks.
6. Migrate the 137 Accepted criteria without deleting or weakening their
   substantive requirements.
7. Resolve every unsupported criterion explicitly. Demotion or scoped
   correction is an expected and acceptable migration result, not an
   exceptional fallback; enforcement does not imply that all historical
   claims were supportable as written.
8. Require an empty audit report.
9. Add ADR 0033's structural and full-corpus evidence.
10. Promote ADR 0033 and switch the Nix gate to enforcement in the same final
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
  unknown fields, malformed hashes, invalid dates, invalid paths, unsafe-range
  integers, and every non-integer JSON number at any nesting depth.
- Claim-parser tests cover valid headers, multiline criteria, malformed IDs,
  ADR-number mismatch, unknown kinds, and repository-wide duplication.
- Registry tests cover complete coverage, multiple corroborating entries,
  duplicates, forbidden inline values, kind mismatch, and compatibility.
- Artifact tests cover canonical-hash drift, missing observations, lexical and
  real-path escapes, missing inputs, and current-input hash drift.
- Input-profile tests derive a transitive Clojure namespace closure, reject an
  omitted direct or transitive source namespace, accept explicit non-namespace
  fixture additions, and hash-check extra inputs.
- Predicate property tests retain agreement with the corresponding Clojure
  operators, distinguish incompatible operand types from false predicates, and
  never trust a stored verdict.
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
