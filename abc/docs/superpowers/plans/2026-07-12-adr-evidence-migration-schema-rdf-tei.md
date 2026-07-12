# ADR Evidence Migration: Schema, RDF, and TEI Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Correct the six Accepted schema/RDF/TEI ADRs to 30 honest typed claims and bind each claim to fresh artifact-backed evidence while governance remains in audit mode.

**Architecture:** This is family transaction 2 and starts only after plan 1 (`2026-07-12-adr-evidence-migration-foundation.md`) lands the shared prerequisite. Stage A adds missing assertions, corrects criteria before assigning IDs, and maps all 31 baseline rows through plan 1's ledger/inventory APIs. Stage B captures narrow observation boundaries from the clean Stage A revision, reuses plan 1's design-bundle operational bundle, joins claim-level registry entries, and drives only this family's audit problem set to zero.

**Tech Stack:** Clojure 1.12, Kaocha, Apache Jena/SHACL, networknt JSON Schema 2020-12, Jing/TEI Relax NG, ph-schematron/Saxon, Nix flakes, RFC 8785/JCS bundles, EDN registries, deterministic JSON reports.

## Global Constraints

- Work from `/home/bor/Projects/soranoha/.worktrees/adr-evidence-corpus-migration`; commands under `abc/` use component-relative paths.
- Plan 1 must be committed first. Consume, do not redefine, its baseline manifest, content-hash ledger, disposition-aware inventory, non-acceptance-section lint, descriptor/bundle layout, repaired Git-free design-bundle app, and `git-cliff-config` check.
- Exact plan 1 APIs consumed: `abc.tools.adr-claim-migration/{baseline-value,baseline-hash,validate-baseline,validate-ledger,load-migration-state}` and `abc.tools.adr-evidence-inventory/inventory-value` with migration state.
- Reuse, do not recapture, `docs/evidence/adr-runs/design-bundle-operational.json`, observation `design-bundle-exits-zero`, produced from `docs/evidence/adr-capture/design-bundle-operational.edn` by `nix run .#validate-design-bundle` in `abc/`.
- Criterion correction precedes stable claim IDs. Audit mode remains active; this plan must not promote ADR 0034, capture corpus-wide governance evidence, or enable enforcement.
- Plan 1's 36-coordinate normative-section hash guard must remain green. Moving prose means relocating only Acceptance Criteria/Implementation Status duplicates; inventoried Decision, Decision Matrix, and Hard Rule bytes never change.
- Every focused Clojure boundary uses capture descriptor version 2, a
  same-stem checked manifest under `abc/docs/evidence/adr-inputs/`, and
  `abc.tools.evidence-io`. Static namespace closure is source closure only;
  it must pass Plan 1's default-deny raw-I/O and subprocess lint.
- TEI-schema-dependent observations run only as dedicated Nix checks that
  provide both Clojure and the lock-pinned `TEI_SCHEMA_PATH`; neither a
  developer shell nor an ambient executable is evidence.
- A passing namespace supports only assertions it makes. Every runtime-read schema, fixture, context, sidecar, and generated view is an explicit descriptor input.
- Direct commands use `:operational-behavior` / `:operational-observation`. All registry predicates here are `{:operator := :value true}`; registry entries never contain observations, inputs, or verdicts.
- Capture only from a clean committed Stage A tree. The descriptor is included in its own explicit inputs, and initial output is written outside the repository.
- Do not evidence historical rotation, broad `nix flake check`, “all Turtle,” nonexistent failure-manifest materialization, or the contained Boolean-to-rights mapping.

---

## File map

- Modify tests: `abc/test/abc/tools/{validate_design_bundle,linked_art,iiif,manifest_to_rdf,metadata_record}_test.clj`.
- Minimally modify: `abc/src/abc/tools/validate_design_bundle.clj` to parameterize canonicalization fixture paths.
- Modify ADRs: `abc/docs/adr/{0006-v0-design-bundle-validation,0012-tei-odd-schematron-validation,0013-cultural-heritage-lod-profile,0014-iiif-applicability,0017-vocabulary-review,0018-predicate-rename-batch-1}.md`.
- Modify ledger/registry: `abc/docs/adr/{adr-claim-migration,adr-evidence}.edn`.
- Regenerate: `abc/docs/reports/{adr-claim-migration-inventory,adr-evidence-migration}.json`.
- Create descriptors/bundles listed in Task 6 under `abc/docs/evidence/adr-{capture,runs}/`.

### Task 1: Preflight plan 1 and record the family baseline

**Files:**
- Read: `abc/docs/superpowers/plans/2026-07-12-adr-evidence-migration-foundation.md`
- Read: `abc/docs/adr/adr-claim-migration-baseline.json`
- Read: `abc/docs/adr/adr-claim-migration.edn`

**Interfaces:**
- Consumes: all plan 1 interfaces named in Global Constraints.
- Produces: `/tmp/adr-schema-before.json`; no repository mutation.

- [ ] **Step 1: Verify predecessor and cleanliness**

```bash
test -f abc/docs/adr/adr-claim-migration-baseline.json
test -f abc/docs/adr/adr-claim-migration.edn
test -f abc/docs/evidence/adr-runs/design-bundle-operational.json
git status --short
```

Expected: all `test` commands exit 0 and status is empty. Stop otherwise.

- [ ] **Step 2: Verify shared repaired boundaries**

```bash
nix run ./abc#validate-design-bundle
system="$(nix eval --impure --raw --expr builtins.currentSystem)"
nix build --no-link "./abc#checks.${system}.git-cliff-config"
```

Expected: both exit 0; design-bundle does not invoke Git-cliff.

- [ ] **Step 3: Record the audit and inventory baseline**

```bash
cd abc
clojure -M:abc/adr-governance --mode audit --report /tmp/adr-schema-before.json
jq '[.criteria[] | select(.family == "schema-rdf-tei")] | length' docs/reports/adr-claim-migration-inventory.json
```

Expected: audit exits 0 by audit semantics and inventory prints `31`. Do not treat audit exit 0 as conformance.

### Task 2: Add direct design-bundle and canonicalization assertions

**Files:**
- Modify: `abc/src/abc/tools/validate_design_bundle.clj`
- Create: `abc/test/abc/tools/schema_validation_evidence_test.clj`

**Interfaces:**
- Consumes: plan 1's hermetic aggregate boundary.
- Produces: `validate-canonicalization!` zero-arity plus map-arity `{:expected string :identity-json path :array-a path :array-b path}` seam; direct assertions for invalid manifest, hash mismatch, wrapper delegation, and CI app wiring.

- [ ] **Step 1: Write failing tests**

Add these shapes to the new focused namespace, using `abc.tools.evidence-io`
for every repository read. The namespace contains only these four claim-level
assertions plus its runtime-closure assertion. Wrap each assertion with
`eio/with-read-trace` and use the trace-instrumented `abc.tools.files`
helpers for reads; it must not run the 62-test
`validate-design-bundle-test` family suite as evidence:

```clojure
(deftest broken-manifest-is-rejected-test
  (let [schema (files/read-json "schemas/manifest.schema.json")]
    (is (seq (validate/validation-errors schema {})))))

(deftest canonicalization-hash-mismatch-is-rejected-test
  (tfs/with-temp-dir [dir]
    ;; Write identity JSON, deliberately wrong .sha256, and equal array fixtures.
    (eio/with-ephemeral-root
      dir
      #(is (thrown-with-msg? clojure.lang.ExceptionInfo
                             #"canonical identity fixture hash mismatch"
                             (validate/validate-canonicalization!
                              {:expected (apply str (repeat 64 "0"))
                               :identity-json (str (io/file dir "identity.json"))
                               :array-a (str (io/file dir "array-a.json"))
                               :array-b (str (io/file dir "array-b.json"))}))))))

(deftest supported-entrypoint-delegates-test
  (is (string/includes? (files/read-text "bin/validate-design-bundle.sh")
                        "exec clojure -M:abc/validate-design-bundle")))

(deftest supported-ci-wiring-test
  (is (string/includes? (files/read-text ".github/workflows/validation.yml")
                        ".#validate-design-bundle")))
```

- [ ] **Step 2: Run red**

```bash
cd abc
bin/kaocha --focus abc.tools.schema-validation-evidence-test/canonicalization-hash-mismatch-is-rejected-test
```

Expected: FAIL with invalid arity because the injectable path seam does not exist.

- [ ] **Step 3: Add only the parameter seam**

```clojure
(defn validate-canonicalization!
  ([] (validate-canonicalization!
       {:expected "667a3bfa5ab9a5e52a88e2e7de15506936a13c5d6c33825b8983861787bbcdea"
        :identity-json "fixtures/canonicalization/manifest-identity-object.canonical.json"
        :array-a "fixtures/canonicalization/array-ordering-negative-a.json"
        :array-b "fixtures/canonicalization/array-ordering-negative-b.json"}))
  ([{:keys [expected identity-json array-a array-b]}]
   ;; Move the existing comparisons here unchanged.
   ))
```

Do not change comparison semantics or the aggregate call.

- [ ] **Step 4: Run green and commit**

```bash
cd abc
bin/kaocha --focus abc.tools.schema-validation-evidence-test
cd ..
git add abc/src/abc/tools/validate_design_bundle.clj \
  abc/test/abc/tools/schema_validation_evidence_test.clj
git commit -m "test(abc): pin design-bundle contract failures"
```

Expected: 0 failures/errors; commit succeeds.

### Task 3: Add direct LOD, IIIF, Turtle, and rights assertions

**Files:**
- Modify: `abc/test/abc/tools/schema_validation_evidence_test.clj`
- Read: `abc/test/abc/tools/{linked_art,iiif,manifest_to_rdf,metadata_record,shacl}_test.clj`

**Interfaces:**
- Produces: one authored focused var for every non-TEI boundary in Task 6;
  each var directly asserts the complete predicate of only the claims mapped
  to it.

- [ ] **Step 1: Write focused strengthening tests**

Add these tests to `schema_validation_evidence_test.clj`. Copy the bounded
assertion logic from the named existing tests, but wrap it in
`eio/with-read-trace` and route every repository read through
`abc.tools.files`. Do not execute an entire pre-existing namespace as evidence and
do not attach one aggregate observation to all four boundaries.

```clojure
(deftest context-declares-canonical-abc-namespace-test
  (is (= "https://w3id.org/abc/"
         (get-in (files/read-json context-path) ["@context" "abc"]))))

(deftest applicability-schema-is-valid-draft-2020-12-test
  (let [path "schemas/iiif-applicability.schema.json" value (files/read-json path)]
    (is (= "https://json-schema.org/draft/2020-12/schema" (get value "$schema")))
    (is (nil? (schema/schema-valid! value path)))))

(deftest text-only-applicability-values-test
  (let [v (files/read-json "examples/v0/example-work/iiif/applicability.json")]
    (is (= ["000127" "not_applicable" nil]
           [(get v "work_id") (get v "status") (get v "derived_manifest")]))
    (is (re-find #"text-only" (get v "reason")))))
```

Define this literal vector and assert every file, read with `files/read-text`,
contains exactly `@prefix abc: <https://w3id.org/abc/> .`; do not scan
future/unowned files dynamically:

```clojure
["examples/v0/example-work/failure-manifest.example.ttl"
 "examples/v0/example-work/lod/manifest.prov.ttl"
 "examples/v0/example-work/manifest.ttl"
 "examples/v0/example-work/metadata-record.ttl"
 "fixtures/v0/invalid/drift/merge-cardinality-one-predecessor/graph.ttl"
 "fixtures/v0/invalid/drift/rdf-participant-prov-mismatch/graph.ttl"
 "fixtures/v0/invalid/drift/shacl-missing-date/graph.ttl"
 "fixtures/v0/invalid/drift/split-cardinality-one-successor/graph.ttl"
 "fixtures/v0/invalid/drift/typing-missing-activity/graph.ttl"
 "fixtures/v0/invalid/drift/typing-missing-subclass/graph.ttl"
 "resources/abc/tools/manifest_to_rdf/example_manifest.ttl"
 "schemas/manifest.shacl.ttl"]
```

Directly assert the title reading triple uses `http://ndl.go.jp/dcndl/terms/titleTranscription`, its object is `らしょうもん`, and neither `https://w3id.org/abc/reading` nor `https://w3id.org/abc/copyrightExpired` occurs.

- [ ] **Step 2: Author the exact linked-art, IIIF, RDF, and helper boundary vars**

Add these exact vars to `schema_validation_evidence_test.clj`; copy the named
assertion logic rather than calling a whole test namespace:

- `linked-art-parity-test`: regenerate and byte-compare the three committed
  Linked Art files (`ADR-0006-C7`, `ADR-0013-C1`).
- `linked-art-result-status-and-context-hash-test`: assert successful result
  status and the recomputed current context hash (`ADR-0013-C2`).
- `linked-art-determinism-parity-hash-identity-fetch-contract-test`: assert
  two-run determinism, committed parity, context hash, identity preservation,
  and external-fetch refusal in the same focused var (`ADR-0013-C3`).
- `context-declares-canonical-abc-namespace-test`: the exact context assertion
  already shown (`ADR-0017-C1`).
- `expanded-artifact-id-uses-canonical-predicate-test`: inspect the expanded
  fixture key `https://w3id.org/abc/artifactId`, not a summarized result
  (`ADR-0017-C2`).
- `current-linked-art-parity-and-context-hash-contract-test`: assert both
  current three-file parity and current recomputed hash (`ADR-0017-C3`).
- `applicability-schema-is-valid-draft-2020-12-test`: the exact schema
  assertion already shown (`ADR-0014-C1`).
- `iiif-committed-applicability-contract-test`: assert the committed record
  validates and the manifest path is conditional on applicable status
  (`ADR-0006-C8`).
- `iiif-text-only-values-contract-test`: assert only the exact text-only
  example values (`ADR-0014-C2`).
- `iiif-invalid-combinations-contract-test`: assert missing ID, applicable
  without a manifest, and not-applicable with a manifest reject
  (`ADR-0014-C3`).
- `iiif-valid-combinations-contract-test`: assert applicable-with-path and
  rights-blocker-with-null accept (`ADR-0014-C4`).
- `bounded-turtle-prefix-inventory-test`: assert only the literal twelve-file
  inventory above (`ADR-0017-C4`).
- `manifest-rdf-parity-test`: assert the success and failure manifest RDF
  fixtures independently regenerate byte-identically (first evidence for
  `ADR-0017-C5`).
- `metadata-rdf-parity-test`: assert metadata plus persons regenerate the
  metadata Turtle fixture (second evidence for `ADR-0017-C5`).
- `metadata-title-and-legacy-predicate-containment-test`: assert the DCNDL
  title triple and absence of both legacy predicates, then compose the exact
  true/false/nil no-external-rights cases (`ADR-0018-C1`).
- `optional-rights-shacl-contract-test`: the exact four-case SHACL assertion
  below (`ADR-0018-C2`).
- `metadata-bundle-helper-contract-test`: reproduce only
  `validate-metadata-bundle-smoke-test`'s JSON/person/SHACL/Turtle assertions
  (`ADR-0018-C3`).

Each var owns its own `with-read-trace` invocation, returns its traced paths to
the same-stem closure assertion, and may use `with-ephemeral-root` only around
freshly generated outputs. No focused var calls another `deftest` Var.

- [ ] **Step 3: Add optional-rights SHACL cases**

Add a helper that inserts rights URI triples into a copy of the example metadata graph and calls public `abc.tools.shacl/validate!`. Assert:

```clojure
(deftest optional-rights-shacl-contract-test
  (is (= :ok (validate-rights-values! [])))
  (is (= :ok (validate-rights-values!
              ["https://creativecommons.org/publicdomain/mark/1.0/"])))
  (is (thrown? clojure.lang.ExceptionInfo
               (validate-rights-values! ["https://example.invalid/rights"])))
  (is (thrown? clojure.lang.ExceptionInfo
               (validate-rights-values!
                ["https://creativecommons.org/publicdomain/mark/1.0/"
                 "http://rightsstatements.org/vocab/InC/1.0/"]))))
```

- [ ] **Step 4: Prove red sensitivity**

Temporarily use `https://w3id.org/abc/wrong/` as the expected context URI and run:

```bash
cd abc
bin/kaocha --focus abc.tools.schema-validation-evidence-test/context-declares-canonical-abc-namespace-test
```

Expected: FAIL with URI mismatch. Restore the correct value. For rights, temporarily omit the second inserted triple and confirm the max-count case fails because no exception is thrown; restore the helper.

- [ ] **Step 5: Run green and commit**

```bash
cd abc
bin/kaocha --focus abc.tools.schema-validation-evidence-test
cd ..
git add abc/test/abc/tools/schema_validation_evidence_test.clj
git commit -m "test(abc): bind LOD IIIF and RDF contracts"
```

Expected: all namespaces report 0 failures/errors.

### Task 4: Correct criteria, add lifecycle, and map all baseline rows

**Files:**
- Modify: the six ADRs in the File map
- Modify: `abc/docs/adr/adr-claim-migration.edn`
- Regenerate: `abc/docs/reports/adr-claim-migration-inventory.json`
- Test: `abc/test/abc/tools/{adr,adr_evidence_inventory}_test.clj`

**Interfaces:**
- Consumes: plan 1 migration APIs.
- Produces: stable IDs `ADR-0006-C1..C9`, `ADR-0012-C1..C6`, `ADR-0013-C1..C3`, `ADR-0014-C1..C4`, `ADR-0017-C1..C5`, `ADR-0018-C1..C3` (30 live claims).

- [ ] **Step 1: Add exact lifecycle headers**

All six use `Validation scope: fixture`. ADR 0006 uses `Release authority: development`; ADRs 0012/0013/0014/0017 use `publication`; ADR 0018 uses `none`.

- [ ] **Step 2: Replace Acceptance Criteria with these exact substantive boundaries**

| ADR | Final claims |
| --- | --- |
| 0006 | C1 operational: supported Nix design-bundle exits zero. C2 structural: Bash delegates to Clojure entrypoint. C3 fixture: empty/broken manifest rejected. C4 fixture: project-RNG-valid Schematron-invalid fixture rejected with declared ID. C5 fixture: figure warning ID without error. C6 fixture: canonical expected-hash mismatch rejected. C7 fixture: three Linked Art files regenerate/compare. C8 fixture: committed IIIF record validates and manifest path is conditional on applicable status. C9 structural: CI invokes supported Nix app. |
| 0012 | C1 operational: `tei-profile-drift` regenerates and byte-compares ODD-derived RNG/Schematron. C2 fixture: valid fixtures pass project RNG and Schematron. C3 fixture: example passes pinned upstream RNG. C4 fixture: invalid title/gaiji/ruby IDs. C5 fixture: expected warnings without errors. C6 fixture: generated/committed TEI manifests reference validation-result sidecar. |
| 0013 | C1 fixture: harness regenerates three committed files byte-identically. C2 fixture: result status ok and JCS context hash. C3 fixture: two-run determinism, parity, hash, identity, and external-fetch refusal. |
| 0014 | C1 structural: schema declares and validates as 2020-12. C2 fixture: exact text-only example values. C3 fixture: missing ID and invalid status/manifest combinations reject. C4 fixture: applicable-with-path and rights-blocker-with-null accept. |
| 0017 | C1 structural: context canonical namespace. C2 fixture: expanded artifact ID at canonical predicate. C3 fixture: current three-file parity and recomputed current hash. C4 structural: bounded 12-file Turtle inventory canonical prefix. C5 fixture: success/failure manifest and metadata Turtle named parity tests. |
| 0018 | C1 fixture: DCNDL title, no legacy predicates, and true/false/nil legacy flags emit no external rights assertion in the named cases. C2 fixture: zero or one rights IRI; if present, closed set. C3 fixture: metadata helper validates JSON/person inputs, SHACL, and metadata Turtle parity. |

Use these exact bullets; append existing test citations after the substantive sentence rather than creating coverage claims:

```markdown
- **ADR-0006-C1 — operational-behavior:** The supported `nix run .#validate-design-bundle` application exits zero on the committed design bundle.
- **ADR-0006-C2 — structural-invariant:** `bin/validate-design-bundle.sh` delegates to the supported Clojure validation entry point without duplicating validation logic.
- **ADR-0006-C3 — fixture-behavior:** Manifest schema validation rejects an empty manifest fixture.
- **ADR-0006-C4 — fixture-behavior:** A project-Relax-NG-valid fixture that violates Schematron is rejected with its declared error rule ID.
- **ADR-0006-C5 — fixture-behavior:** The figure accessibility fixture reports `abc-figure-accessibility` as a warning without a Schematron error.
- **ADR-0006-C6 — fixture-behavior:** Canonical identity validation rejects a mismatched expected hash.
- **ADR-0006-C7 — fixture-behavior:** Linked Art validation regenerates and byte-compares the three committed LOD fixtures.
- **ADR-0006-C8 — fixture-behavior:** The committed IIIF applicability record validates; `derived_manifest` is required only for `applicable` status and is null otherwise.
- **ADR-0006-C9 — structural-invariant:** CI invokes the supported `.#validate-design-bundle` Nix application used for local validation.

- **ADR-0012-C1 — operational-behavior:** The `tei-profile-drift` Nix check regenerates Relax NG and Schematron from `schemas/tei-profile.odd` and byte-compares both committed artifacts.
- **ADR-0012-C2 — fixture-behavior:** Valid TEI fixtures pass project Relax NG and Schematron validation.
- **ADR-0012-C3 — fixture-behavior:** The committed example TEI passes the pinned upstream TEI P5 Relax NG validation.
- **ADR-0012-C4 — fixture-behavior:** Invalid title, gaiji, and ruby fixtures fail with their expected Schematron rule IDs.
- **ADR-0012-C5 — fixture-behavior:** Warning fixtures produce their expected warning findings without Schematron errors.
- **ADR-0012-C6 — fixture-behavior:** Generated and committed TEI manifests reference a `tei-validation-result.json` validation-result sidecar.

- **ADR-0013-C1 — fixture-behavior:** The Linked Art harness regenerates the candidate, normalized expansion, and context-validation result byte-identically to the three committed fixtures.
- **ADR-0013-C2 — fixture-behavior:** The regenerated validation result records `status: "ok"` and the JCS-recomputed context hash.
- **ADR-0013-C3 — fixture-behavior:** Focused Linked Art tests demonstrate two-run determinism, committed byte parity, context hashing, artifact-ID preservation, and refusal to fetch an unapproved external JSON-LD context.

- **ADR-0014-C1 — structural-invariant:** `schemas/iiif-applicability.schema.json` declares JSON Schema 2020-12 and validates against that meta-schema.
- **ADR-0014-C2 — fixture-behavior:** The text-only `000127` applicability fixture validates with status `not_applicable`, a text-only reason, and `derived_manifest: null`.
- **ADR-0014-C3 — fixture-behavior:** IIIF applicability validation rejects a missing work ID and both invalid status/derived-manifest combinations.
- **ADR-0014-C4 — fixture-behavior:** IIIF applicability validation accepts an `applicable` record with a manifest path and a `rights_blocker` record with no derived manifest.

- **ADR-0017-C1 — structural-invariant:** `contexts/abc-v0.jsonld` declares `abc` as `https://w3id.org/abc/`.
- **ADR-0017-C2 — fixture-behavior:** Linked Art expansion preserves the manifest artifact ID at `https://w3id.org/abc/artifactId`.
- **ADR-0017-C3 — fixture-behavior:** The three current LOD fixtures regenerate byte-identically and their committed context hash equals the JCS-recomputed context hash.
- **ADR-0017-C4 — structural-invariant:** The bounded committed Turtle inventory uses `@prefix abc: <https://w3id.org/abc/>`.
- **ADR-0017-C5 — fixture-behavior:** Success-manifest, failure-manifest, and metadata-record Turtle generators remain byte-identical to their named committed fixtures.

- **ADR-0018-C1 — fixture-behavior:** In the named metadata RDF cases, the generator emits `dcndl:titleTranscription` inside the title blank node, emits neither `abc:reading` nor `abc:copyrightExpired`, and true, false, and nil legacy copyright flags emit no external rights assertion.
- **ADR-0018-C2 — fixture-behavior:** `MetadataRecordWorkShape` permits zero or one `dcterms:rights`; when present it must be an IRI in the closed Public Domain Mark/InC set.
- **ADR-0018-C3 — fixture-behavior:** The metadata-bundle helper validates the committed metadata JSON and person inputs, SHACL graph, and byte-identical `metadata-record.ttl` fixture.
```

- [ ] **Step 3: Move unsupported prose out of acceptance**

- Put nonexistent failure-manifest materialization and warning promotion under ADR 0006 `## Future Verification` without claim headers.
- Put historical broad flake passes, ADR 0017 rotated/no-regeneration wording, and ADR 0018 equality with `bc3ea94` under `## Historical Evidence`, bounded to their original revision/date.
- State in ADR 0018 that ADR 0035 containment makes the legacy Boolean emit no external rights assertion; do not claim current manifest equality.

- [ ] **Step 4: Fill all 31 content-hash ledger dispositions**

Use plan 1's exact schema and original text hashes. Key outcomes:

- ADR 0006 original C7 splits to new C7/C8; old broad flake C8 moves out; all other rows correct to their corresponding claim.
- ADR 0012 original C1 splits derivation/project/upstream boundaries; warning materialization corrects to findings.
- ADR 0013 retains/corrects to current harness boundaries.
- ADR 0014 corrects semantic value assertions and the erroneous “four” case count.
- ADR 0017 broad flake C4 moves out; C3 loses historical rotation; C5 splits bounded prefix and named parity.
- ADR 0018 C1/C2/C3 correct to containment/helper scope; broad flake C4 and historical equality C5 move out.

Every `:correct`/`:retain` has its Task 6 planned evidence boundary and `:resulting-claim-ids`; every moved row has rationale and `[]`. Follow `validate-ledger` for overlap: never weaken it to permit ambiguous many-to-one mappings.

- [ ] **Step 5: Run red/green ledger and regenerate inventory**

```bash
cd abc
bin/kaocha --focus abc.tools.adr-test --focus abc.tools.adr-evidence-inventory-test
clojure -M:abc/adr-evidence-inventory --output docs/reports/adr-claim-migration-inventory.json
jq -e '[.criteria[] | select(.family == "schema-rdf-tei")] as $c
       | ($c|length == 30) and ($c|all(.claim_id and .claim_kind and .disposition))' docs/reports/adr-claim-migration-inventory.json
```

Expected before complete ledger: FAIL with unresolved disposition/mapping. Expected after completion: tests and `jq` pass.

- [ ] **Step 6: Commit Stage A source atomically**

```bash
cd ..
git add abc/docs/adr/0006-*.md abc/docs/adr/0012-*.md abc/docs/adr/0013-*.md \
  abc/docs/adr/0014-*.md abc/docs/adr/0017-*.md abc/docs/adr/0018-*.md \
  abc/docs/adr/adr-claim-migration.edn abc/docs/reports/adr-claim-migration-inventory.json
git commit -m "docs(adr): correct schema RDF and TEI claims"
```

### Task 5: Enforce the Stage A audit-delta gate

**Files:**
- Regenerate: `abc/docs/reports/adr-evidence-migration.json`

- [ ] **Step 1: Generate and inspect audit**

```bash
cd abc
clojure -M:abc/adr-governance --mode audit --report docs/reports/adr-evidence-migration.json
jq -e --arg family '^(ADR-)?(0006|0012|0013|0014|0017|0018)-' '
  [.problems[]
   | select(((.file // "") | test($family))
            or ((.["claim-id"] // "") | test($family))
            or any((.["affected-claim-ids"] // [])[]; test($family)))] as $p
  | ($p | length) == 30
    and ($p | all(.kind == "missing-claim-evidence"))
' docs/reports/adr-evidence-migration.json
```

Expected: exactly 30 `missing-claim-evidence` problems. ADR-0006-C1 will
reuse plan 1's bundle, but its claim-level join is intentionally absent until
this family's Stage B registration. Any lifecycle, parser, ledger, graph,
dependency, artifact, or predicate problem blocks capture.

- [ ] **Step 2: Assert no unrelated regression and commit**

Compare stable `[file,kind,"claim-id",artifact-path]` keys with
`/tmp/adr-schema-before.json`. No new outside-family root is allowed except
identified staleness for a shared bundle owned by a later plan.

```bash
cd ..
git add abc/docs/reports/adr-evidence-migration.json
git commit -m "docs(adr): record schema family Stage A audit"
git status --short
```

Expected: clean tree. This commit is the capture revision.

### Task 6: Define and capture narrow evidence boundaries

**Files:**
- Modify: `abc/flake.nix`
- Modify: `abc/test/abc/tools/{schematron,tei,materialize_publication,validate_design_bundle}_test.clj`
- Create the 29 matching `.edn`/`.json` pairs named in the boundary table
  under `abc/docs/evidence/adr-capture/` and
  `abc/docs/evidence/adr-runs/`.
- Create one same-stem manifest for each of the twenty-one focused Clojure
  descriptors under `abc/docs/evidence/adr-inputs/`.
- Create one `*-nix-clojure-closure.edn` manifest for each of the seven new
  TEI Nix/Clojure checks; `tei-profile-drift` is not a Clojure check.
- Modify: `abc/test/abc/tools/adr_evidence_capture_test.clj`
- Create: `abc/docs/evidence/adr-entries/schema-rdf-tei.edn`

**Interfaces:**
- Consumes: plan 1 capture v2/runtime-input contract and version-1
  `repo-files-v1` operational capture.
- Produces: twenty-nine claim-coherent Boolean observations plus the reused
  design-bundle operational observation, and seven new dedicated TEI Nix
  checks plus existing `tei-profile-drift`.

- [ ] **Step 1: Add hermetic TEI evidence checks**

In the checks `let`, reuse `copyWritableSource`, `cljSandboxEnv`,
`cljDepsCache`, and `tei`. Each check has `pkgs.clojure` in
`nativeBuildInputs`, exports `TEI_SCHEMA_PATH="${tei.teiAllSchema}"`, copies
the writable source, establishes `cljSandboxEnv`, and invokes Clojure directly
without `nix develop` or ambient `PATH` resolution:

```nix
adr-evidence-tei-upstream-rng =
  pkgs.runCommand "abc-adr-evidence-tei-upstream-rng"
    { nativeBuildInputs = [ pkgs.clojure ]; }
    ''
      ${copyWritableSource}
      ${cljSandboxEnv}
      export TEI_SCHEMA_PATH="${tei.teiAllSchema}"
      clojure -M:test:kaocha -m kaocha.runner \
        --focus abc.tools.tei-test/validate-example-fixture-test
      mkdir -p "$out"
      touch "$out/passed"
    '';
```

Author and focus only these exact claim predicates; never focus an entire
namespace:

- `project-rng-valid-schematron-invalid-fixture-test` proves
  `fixtures/tei/invalid/abc-bad-layout-params.xml` passes
  `schemas/tei-profile.rng`, then asserts Schematron reports exactly declared
  error ID `abc-layout-params-shape` for that same fixture. The production
  fixture catalog leaves `:project-rng?` true for this path.
- `valid-project-tei-fixtures-pass-rng-and-schematron-test` runs both project
  validators over the literal four valid fixture paths.
- the invalid-ID check focuses only the existing title, gaiji, and ruby vars;
- the figure-warning check focuses only the existing figure-accessibility Var;
- the enrichment-warning check focuses only the existing undeclared-
  enrichment Var;
- upstream RNG focuses only `abc.tools.tei-test/validate-example-fixture-test`;
- publication-sidecars focuses newly authored
  `tei-generated-manifest-references-validation-result-test` and
  `tei-committed-manifest-references-validation-result-test`.

Create a separate Nix check for each row rather than one project-fixtures
suite: `adr-evidence-tei-project-cross-schema-invalid`,
`adr-evidence-tei-project-valid-fixtures`,
`adr-evidence-tei-schematron-invalid-ids`,
`adr-evidence-tei-figure-warning`,
`adr-evidence-tei-enrichment-warning`,
`adr-evidence-tei-upstream-rng`, and
`adr-evidence-tei-publication-sidecars`. Add a flake-output test asserting all
seven names, `pkgs.clojure`, the exact focus vars, and the pinned
`TEI_SCHEMA_PATH` assignment.

Run:

```bash
system="$(nix eval --impure --raw --expr builtins.currentSystem)"
nix build --no-link "./abc#checks.${system}.adr-evidence-tei-project-cross-schema-invalid"
nix build --no-link "./abc#checks.${system}.adr-evidence-tei-project-valid-fixtures"
nix build --no-link "./abc#checks.${system}.adr-evidence-tei-schematron-invalid-ids"
nix build --no-link "./abc#checks.${system}.adr-evidence-tei-figure-warning"
nix build --no-link "./abc#checks.${system}.adr-evidence-tei-enrichment-warning"
nix build --no-link "./abc#checks.${system}.adr-evidence-tei-upstream-rng"
nix build --no-link "./abc#checks.${system}.adr-evidence-tei-publication-sidecars"
```

Expected: all seven build. A negative contract test that removes
`pkgs.clojure` or `TEI_SCHEMA_PATH` from a copied check expression must fail
before this step is green.

- [ ] **Step 2: Write descriptor and manifest contracts**

For example, `linked-art-parity.edn` has this exact shape:

```clojure
{:schema-version "abc-adr-evidence-capture-v2"
 :tool "bin/kaocha"
 :argv ["bin/kaocha" "--focus"
        "abc.tools.schema-validation-evidence-test/linked-art-parity-test"]
 :runtime-input-manifest "docs/evidence/adr-inputs/linked-art-parity.edn"
 :input-profile {:kind "clojure-test-v1"
                 :roots ["abc.tools.schema-validation-evidence-test"]
                 :explicit ["docs/evidence/adr-capture/linked-art-parity.edn"
                            "docs/evidence/adr-inputs/linked-art-parity.edn"
                            "contexts/abc-v0.jsonld"
                            "examples/v0/example-work/manifest.json"
                            "examples/v0/example-work/metadata-record.json"
                            "examples/v0/example-work/lod/linked-art-candidate.jsonld"
                            "examples/v0/example-work/lod/linked-art-expanded.normalized.json"
                            "examples/v0/example-work/lod/jsonld-context-validation-result.json"]}
 :observation-key "linked-art-parity-passes"}
```

Use these exact boundaries:

| Basename | Exact focused var or Nix check | Observation |
| --- | --- | --- |
| schema-entrypoint-delegation | `abc.tools.schema-validation-evidence-test/supported-entrypoint-delegates-test` | `supported-entrypoint-delegates` |
| schema-empty-manifest | `abc.tools.schema-validation-evidence-test/broken-manifest-is-rejected-test` | `empty-manifest-is-rejected` |
| schema-canonicalization-mismatch | `abc.tools.schema-validation-evidence-test/canonicalization-hash-mismatch-is-rejected-test` | `canonicalization-mismatch-is-rejected` |
| schema-ci-wiring | `abc.tools.schema-validation-evidence-test/supported-ci-wiring-test` | `supported-ci-wiring-is-present` |
| tei-project-cross-schema-invalid | `adr-evidence-tei-project-cross-schema-invalid` | `tei-project-cross-schema-invalid-rejected` |
| tei-project-valid-fixtures | `adr-evidence-tei-project-valid-fixtures` | `tei-project-valid-fixtures-pass` |
| tei-schematron-invalid-ids | `adr-evidence-tei-schematron-invalid-ids` | `tei-schematron-invalid-ids-pass` |
| tei-figure-warning | `adr-evidence-tei-figure-warning` | `tei-figure-warning-contract-passes` |
| tei-enrichment-warning | `adr-evidence-tei-enrichment-warning` | `tei-enrichment-warning-contract-passes` |
| tei-upstream-rng | `adr-evidence-tei-upstream-rng` | `tei-upstream-rng-fixture-passes` |
| tei-publication-sidecars | `adr-evidence-tei-publication-sidecars` | `tei-validation-sidecars-pass` |
| tei-profile-drift | existing `tei-profile-drift` | `tei-profile-derived-artifacts-match` |
| linked-art-parity | `abc.tools.schema-validation-evidence-test/linked-art-parity-test` | `linked-art-parity-passes` |
| linked-art-result-contract | `abc.tools.schema-validation-evidence-test/linked-art-result-status-and-context-hash-test` | `linked-art-result-contract-passes` |
| linked-art-full-behavior | `abc.tools.schema-validation-evidence-test/linked-art-determinism-parity-hash-identity-fetch-contract-test` | `linked-art-full-behavior-passes` |
| linked-art-context | `abc.tools.schema-validation-evidence-test/context-declares-canonical-abc-namespace-test` | `linked-art-context-is-canonical` |
| linked-art-artifact-predicate | `abc.tools.schema-validation-evidence-test/expanded-artifact-id-uses-canonical-predicate-test` | `linked-art-artifact-predicate-is-canonical` |
| linked-art-current-parity-hash | `abc.tools.schema-validation-evidence-test/current-linked-art-parity-and-context-hash-contract-test` | `linked-art-current-parity-and-hash-pass` |
| iiif-schema-contract | `abc.tools.schema-validation-evidence-test/applicability-schema-is-valid-draft-2020-12-test` | `iiif-schema-contract-passes` |
| iiif-committed-applicability | `abc.tools.schema-validation-evidence-test/iiif-committed-applicability-contract-test` | `iiif-committed-applicability-passes` |
| iiif-text-only-values | `abc.tools.schema-validation-evidence-test/iiif-text-only-values-contract-test` | `iiif-text-only-values-pass` |
| iiif-invalid-combinations | `abc.tools.schema-validation-evidence-test/iiif-invalid-combinations-contract-test` | `iiif-invalid-combinations-reject` |
| iiif-valid-combinations | `abc.tools.schema-validation-evidence-test/iiif-valid-combinations-contract-test` | `iiif-valid-combinations-pass` |
| turtle-prefix-inventory | `abc.tools.schema-validation-evidence-test/bounded-turtle-prefix-inventory-test` | `turtle-prefix-inventory-passes` |
| manifest-rdf-parity | `abc.tools.schema-validation-evidence-test/manifest-rdf-parity-test` | `manifest-rdf-parity-passes` |
| metadata-rdf-parity | `abc.tools.schema-validation-evidence-test/metadata-rdf-parity-test` | `metadata-rdf-parity-passes` |
| metadata-rdf-containment | `abc.tools.schema-validation-evidence-test/metadata-title-and-legacy-predicate-containment-test` | `metadata-rdf-containment-passes` |
| metadata-rights-shacl | `abc.tools.schema-validation-evidence-test/optional-rights-shacl-contract-test` | `metadata-rights-shacl-passes` |
| metadata-bundle-helper | `abc.tools.schema-validation-evidence-test/metadata-bundle-helper-contract-test` | `metadata-bundle-helper-passes` |

The twenty-one Clojure manifests list the exact repository paths read by their focused vars;
the vars wrap their assertions in `eio/with-read-trace` and call plan 1's
`assert-runtime-input-closure!`. Descriptor tests require exact equality among
manifest paths, observed paths, and explicit data paths, and run the direct-I/O
default-deny lint. The seven dedicated TEI Nix descriptors and `tei-profile-drift` remain
version 1 `repo-files-v1`; each binds `flake.nix`, `flake.lock`, `deps.edn`,
`deps-lock.json`, `tests.edn`,
`nix/tei-profile-artifacts.nix`, the exact schema/fixture inputs, its checked
Nix/Clojure source-closure manifest and every path expanded from that
manifest, and its descriptor. They never use a v2 runtime manifest.
Generate each Nix/Clojure closure manifest with Plan 1's
`derive-nix-clojure-source-closure` from the exact focused Vars named by that
check. The descriptor-contract test recomputes the closure and requires byte-
equal focused Vars/paths plus exact expansion into `:explicit`; delete-one and
add-unrelated-source negative cases must fail.

Extend `adr_evidence_capture_test.clj` with a table-driven assertion over all
twenty-nine descriptors. It must resolve every named Kaocha Var and every Nix
check name before capture. For each v2 row, load the closed same-stem manifest,
assert the exact observation and focused var from the table, assert descriptor
explicit inputs equal descriptor path + manifest path + manifest `:paths`, and
invoke the reachable-Var default-deny lint. For each Nix/Clojure row, assert
version 1, no `:runtime-input-manifest`, a `bash -lc` command using
`builtins.currentSystem`, and the exact check-specific `flake.nix`,
`flake.lock`, `deps.edn`, `deps-lock.json`, `tests.edn`,
`nix/tei-profile-artifacts.nix`, checked and expanded
source/test closure, schema, and fixture determinants. Missing or extra paths
fail this test.
For `tei-profile-drift`, assert the existing non-Clojure determinant set shown
below; it does not bind `deps.edn` or a Clojure closure manifest.

Direct Nix descriptor:

```clojure
{:schema-version "abc-adr-evidence-capture-v1"
 :tool "bash"
 :argv ["bash" "-lc"
        "system=$(nix eval --impure --raw --expr builtins.currentSystem); nix build --no-link .#checks.${system}.tei-profile-drift"]
 :input-profile {:kind "repo-files-v1" :roots []
                 :explicit ["docs/evidence/adr-capture/tei-profile-drift.edn"
                            "flake.nix" "flake.lock" "nix/tei-profile-artifacts.nix"
                            "schemas/tei-profile.odd" "schemas/tei-profile.rng"
                            "schemas/tei-profile.sch"]}
 :observation-key "tei-profile-derived-artifacts-match"}
```

Create `schema-rdf-tei.edn` with the exact 30 distinct claim IDs in Task 7's
table and 32 entries total.
It uses the closed registration-template shape from plan 1, contains no
artifact hashes, and includes two entries for ADR-0017-C5 because that claim
requires both manifest and metadata RDF observations, plus two entries for
ADR-0012-C5 because its plural warning-fixture claim requires both the figure
and enrichment observations.

- [ ] **Step 3: Commit checks, descriptors, and manifests before capture**

```bash
git add abc/flake.nix abc/docs/evidence/adr-capture \
  abc/docs/evidence/adr-inputs \
  abc/docs/evidence/adr-entries/schema-rdf-tei.edn \
  abc/test/abc/tools/adr_evidence_capture_test.clj \
  abc/test/abc/tools/schematron_test.clj \
  abc/test/abc/tools/tei_test.clj \
  abc/test/abc/tools/materialize_publication_test.clj \
  abc/test/abc/tools/validate_design_bundle_test.clj
git commit -m "docs(evidence): define schema RDF and TEI captures"
git status --short
```

Expected: clean tree. Descriptor self-hashes would otherwise stale immediately.

- [ ] **Step 4: Capture outside repo**

Capture all basenames with this concrete loop:

```bash
cd abc
basenames=(
  schema-entrypoint-delegation schema-empty-manifest
  schema-canonicalization-mismatch schema-ci-wiring
  tei-project-cross-schema-invalid tei-project-valid-fixtures
  tei-schematron-invalid-ids tei-figure-warning tei-enrichment-warning tei-upstream-rng
  tei-publication-sidecars tei-profile-drift linked-art-parity
  linked-art-result-contract linked-art-full-behavior linked-art-context
  linked-art-artifact-predicate linked-art-current-parity-hash
  iiif-schema-contract iiif-committed-applicability iiif-text-only-values
  iiif-invalid-combinations iiif-valid-combinations turtle-prefix-inventory
  manifest-rdf-parity metadata-rdf-parity metadata-rdf-containment
  metadata-rights-shacl metadata-bundle-helper
)
printf '%s\n' "${basenames[@]}" > /tmp/adr-schema-basenames.txt
for basename in "${basenames[@]}"; do
  clojure -M:abc/adr-evidence-capture \
    --descriptor "docs/evidence/adr-capture/${basename}.edn" \
    --output "/tmp/${basename}.json"
  test -z "$(git status --short)"
done
```

Expected: exactly 29 captures exit 0 and status is empty each time. TEI observations execute
only the dedicated Nix checks; no descriptor invokes `nix develop`.

- [ ] **Step 5: Validate and install all twenty-nine bundles**

```bash
while read -r basename; do
  f="/tmp/${basename}.json"
  jq -e '.schema_version == "abc-adr-evidence-run-v1" and ([.observations[].value] | all)' "$f"
done < /tmp/adr-schema-basenames.txt
while read -r basename; do
  cp "/tmp/${basename}.json" docs/evidence/adr-runs/
done < /tmp/adr-schema-basenames.txt
```

Expected: every `jq` passes. Do not commit until registry/report updates are ready.

### Task 7: Join 30 claims and drive family audit to zero

**Files:**
- Modify: `abc/docs/adr/adr-evidence.edn`
- Add: twenty-nine Task 6 bundles
- Use: `abc/docs/evidence/adr-entries/schema-rdf-tei.edn`
- Regenerate: both migration reports

**Interfaces:**
- Consumes: twenty-nine bundles plus plan 1's `design-bundle-operational.json`/`design-bundle-exits-zero`.

- [ ] **Step 1: Review the checked-in claim-level joins**

| Claims | Evidence kind | Bundle / observation |
| --- | --- | --- |
| 0006-C1 | operational-observation | design-bundle-operational / design-bundle-exits-zero |
| 0006-C2 | structural-test | schema-entrypoint-delegation / supported-entrypoint-delegates |
| 0006-C3 | fixture-conformance | schema-empty-manifest / empty-manifest-is-rejected |
| 0006-C6 | fixture-conformance | schema-canonicalization-mismatch / canonicalization-mismatch-is-rejected |
| 0006-C9 | structural-test | schema-ci-wiring / supported-ci-wiring-is-present |
| 0006-C4 | fixture-conformance | tei-project-cross-schema-invalid / tei-project-cross-schema-invalid-rejected |
| 0012-C2 | fixture-conformance | tei-project-valid-fixtures / tei-project-valid-fixtures-pass |
| 0012-C4 | fixture-conformance | tei-schematron-invalid-ids / tei-schematron-invalid-ids-pass |
| 0006-C5 | fixture-conformance | tei-figure-warning / tei-figure-warning-contract-passes |
| 0012-C5 | fixture-conformance | two valid entries: tei-figure-warning and tei-enrichment-warning |
| 0006-C7; 0013-C1 | fixture-conformance | linked-art-parity / linked-art-parity-passes |
| 0013-C2 | fixture-conformance | linked-art-result-contract / linked-art-result-contract-passes |
| 0013-C3 | fixture-conformance | linked-art-full-behavior / linked-art-full-behavior-passes |
| 0017-C1 | structural-test | linked-art-context / linked-art-context-is-canonical |
| 0017-C2 | fixture-conformance | linked-art-artifact-predicate / linked-art-artifact-predicate-is-canonical |
| 0017-C3 | fixture-conformance | linked-art-current-parity-hash / linked-art-current-parity-and-hash-pass |
| 0014-C1 | structural-test | iiif-schema-contract / iiif-schema-contract-passes |
| 0006-C8 | fixture-conformance | iiif-committed-applicability / iiif-committed-applicability-passes |
| 0014-C2 | fixture-conformance | iiif-text-only-values / iiif-text-only-values-pass |
| 0014-C3 | fixture-conformance | iiif-invalid-combinations / iiif-invalid-combinations-reject |
| 0014-C4 | fixture-conformance | iiif-valid-combinations / iiif-valid-combinations-pass |
| 0012-C1 | operational-observation | tei-profile-drift / tei-profile-derived-artifacts-match |
| 0012-C3 | fixture-conformance | tei-upstream-rng / tei-upstream-rng-fixture-passes |
| 0012-C6 | fixture-conformance | tei-publication-sidecars / tei-validation-sidecars-pass |
| 0017-C4 | structural-test | turtle-prefix-inventory / turtle-prefix-inventory-passes |
| 0017-C5 | fixture-conformance | two valid entries: manifest-rdf-parity and metadata-rdf-parity |
| 0018-C1 | fixture-conformance | metadata-rdf-containment / metadata-rdf-containment-passes |
| 0018-C2 | fixture-conformance | metadata-rights-shacl / metadata-rights-shacl-passes |
| 0018-C3 | fixture-conformance | metadata-bundle-helper / metadata-bundle-helper-passes |

Each template entry uses the exact claim kind, compatible keyword evidence
kind, artifact path, observation key, and
`:expected {:operator := :value true}`. It does not contain an artifact hash.

- [ ] **Step 2: Register the template and run red then green registry audit**

```bash
cd abc
bin/kaocha --focus abc.tools.adr-evidence-test
clojure -M:abc/adr-evidence-register -- \
  --entries docs/evidence/adr-entries/schema-rdf-tei.edn \
  --registry docs/adr/adr-evidence.edn
clojure -M:abc/adr-governance --mode audit --report /tmp/schema-registry.json
```

Expected before registration: `missing-claim-evidence`. The registrar derives
all canonical bundle hashes and exits 0 without hiding unrelated non-coverage
problems. Expected after: no problem for the six family ADRs; replacing missing
evidence with hash, stale-input, kind, observation, or predicate errors is
failure. A second registrar run leaves the registry byte-identical.

- [ ] **Step 3: Regenerate reports and assert gates**

```bash
clojure -M:abc/adr-evidence-inventory --output docs/reports/adr-claim-migration-inventory.json
clojure -M:abc/adr-governance --mode audit --report docs/reports/adr-evidence-migration.json
jq -e --arg family '^(ADR-)?(0006|0012|0013|0014|0017|0018)-' \
  -f nix/adr-family-clean.jq docs/reports/adr-evidence-migration.json
jq -e '[.criteria[] | select(.family == "schema-rdf-tei")] as $c | ($c|length == 30) and ($c|all(.disposition and .claim_id and .claim_kind))' docs/reports/adr-claim-migration-inventory.json
```

Expected: all exit 0.

- [ ] **Step 4: Commit Stage B atomically**

```bash
cd ..
git add abc/docs/adr/adr-evidence.edn abc/docs/evidence/adr-runs \
  abc/docs/reports/adr-evidence-migration.json abc/docs/reports/adr-claim-migration-inventory.json
git commit -m "docs(evidence): bind schema RDF and TEI claims"
```

### Task 8: Final verification and handoff

- [ ] **Step 1: Rerun captured focused boundaries**

```bash
cd abc
bin/kaocha --focus abc.tools.schema-validation-evidence-test
cd ..
system="$(nix eval --impure --raw --expr builtins.currentSystem)"
nix build --no-link "./abc#checks.${system}.adr-evidence-tei-upstream-rng"
nix build --no-link "./abc#checks.${system}.adr-evidence-tei-project-cross-schema-invalid"
nix build --no-link "./abc#checks.${system}.adr-evidence-tei-project-valid-fixtures"
nix build --no-link "./abc#checks.${system}.adr-evidence-tei-schematron-invalid-ids"
nix build --no-link "./abc#checks.${system}.adr-evidence-tei-figure-warning"
nix build --no-link "./abc#checks.${system}.adr-evidence-tei-enrichment-warning"
nix build --no-link "./abc#checks.${system}.adr-evidence-tei-publication-sidecars"
```

Expected: 0 failures/errors in the same pinned environment used for capture.

- [ ] **Step 2: Run direct and quality checks**

```bash
cd ..
nix run ./abc#validate-design-bundle
system="$(nix eval --impure --raw --expr builtins.currentSystem)"
nix build --no-link "./abc#checks.${system}.tei-profile-drift"
nix build --no-link "./abc#checks.${system}.git-cliff-config"
nix build --no-link "./abc#checks.${system}.clj-kondo"
nix build --no-link "./abc#checks.${system}.clj-nix-focused-tests"
just validate-migration
```

Expected: all exit 0.

- [ ] **Step 3: Verify deterministic reports, audit mode, and clean tree**

```bash
cd abc
clojure -M:abc/adr-governance --mode audit --report /tmp/schema-final-audit.json
cmp /tmp/schema-final-audit.json docs/reports/adr-evidence-migration.json
cd ..
rg -n 'adr-governance.*--mode audit' flake.nix
git status --short
```

Expected: `cmp` passes, Nix check remains audit mode, worktree is clean. Overall audit may retain other-family problems but has zero schema-family problems.

## Concerns for execution review

1. All TEI evidence is produced by dedicated Nix checks with Clojure and
   `TEI_SCHEMA_PATH` in their derivation environment. A devshell success is
   never accepted as substitute evidence.
2. Plan 1's ledger validator decides whether overlapping original ADR 0012 rows may map many-to-one. Follow `validate-ledger`; do not loosen it.
3. ADR 0018 stays Accepted with release authority `none`: this plan characterizes containment but does not promote ADR 0035 or restore publication authority.
4. The 12-file Turtle inventory is deliberately bounded. Adding a new Turtle file later requires an explicit reviewed update, not an accidental glob expansion.
