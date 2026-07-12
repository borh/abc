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
- Plan 1's normative-section hash guard must remain green. Moving prose means relocating only Acceptance Criteria/Implementation Status duplicates; existing Decision and Hard Rule bytes never change.
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
nix build ./abc#checks.x86_64-linux.git-cliff-config
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
- Modify: `abc/test/abc/tools/validate_design_bundle_test.clj`

**Interfaces:**
- Consumes: plan 1's hermetic aggregate boundary.
- Produces: `validate-canonicalization!` zero-arity plus map-arity `{:expected string :identity-json path :array-a path :array-b path}` seam; direct assertions for invalid manifest, hash mismatch, wrapper delegation, and CI app wiring.

- [ ] **Step 1: Write failing tests**

Add these shapes, using existing imports/helpers:

```clojure
(deftest broken-manifest-is-rejected-test
  (let [schema (files/read-json "schemas/manifest.schema.json")]
    (is (seq (validate/validation-errors schema {})))))

(deftest canonicalization-hash-mismatch-is-rejected-test
  (tfs/with-temp-dir [dir]
    ;; Write identity JSON, deliberately wrong .sha256, and equal array fixtures.
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"canonical identity fixture hash mismatch"
                          (validate/validate-canonicalization!
                           {:expected (apply str (repeat 64 "0"))
                            :identity-json (str (io/file dir "identity.json"))
                            :array-a (str (io/file dir "array-a.json"))
                            :array-b (str (io/file dir "array-b.json"))})))))

(deftest supported-entrypoint-wiring-test
  (is (string/includes? (slurp "bin/validate-design-bundle.sh")
                        "exec clojure -M:abc/validate-design-bundle"))
  (is (string/includes? (slurp ".github/workflows/validation.yml")
                        ".#validate-design-bundle")))
```

- [ ] **Step 2: Run red**

```bash
cd abc
bin/kaocha --focus abc.tools.validate-design-bundle-test/canonicalization-hash-mismatch-is-rejected-test
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
bin/kaocha --focus abc.tools.validate-design-bundle-test
cd ..
git add abc/src/abc/tools/validate_design_bundle.clj abc/test/abc/tools/validate_design_bundle_test.clj
git commit -m "test(abc): pin design-bundle contract failures"
```

Expected: 0 failures/errors; commit succeeds.

### Task 3: Add direct LOD, IIIF, Turtle, and rights assertions

**Files:**
- Modify: `abc/test/abc/tools/linked_art_test.clj`
- Modify: `abc/test/abc/tools/iiif_test.clj`
- Modify: `abc/test/abc/tools/manifest_to_rdf_test.clj`
- Modify: `abc/test/abc/tools/metadata_record_test.clj`

**Interfaces:**
- Produces: assertions for canonical context, IIIF meta-schema/text-only values, bounded Turtle prefixes, DCNDL title mapping, legacy-predicate omission, and optional-rights SHACL behavior.

- [ ] **Step 1: Write strengthening tests**

```clojure
;; linked_art_test.clj
(deftest context-declares-canonical-abc-namespace-test
  (is (= "https://w3id.org/abc/"
         (get-in (files/read-json context-path) ["@context" "abc"]))))

;; iiif_test.clj; require files and schema
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

In `manifest_to_rdf_test.clj`, define this literal vector and assert every file
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

In `metadata_record_test.clj`, directly assert the title reading triple uses `http://ndl.go.jp/dcndl/terms/titleTranscription`, its object is `らしょうもん`, and neither `https://w3id.org/abc/reading` nor `https://w3id.org/abc/copyrightExpired` occurs.

- [ ] **Step 2: Add optional-rights SHACL cases**

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

- [ ] **Step 3: Prove red sensitivity**

Temporarily use `https://w3id.org/abc/wrong/` as the expected context URI and run:

```bash
cd abc
bin/kaocha --focus abc.tools.linked-art-test/context-declares-canonical-abc-namespace-test
```

Expected: FAIL with URI mismatch. Restore the correct value. For rights, temporarily omit the second inserted triple and confirm the max-count case fails because no exception is thrown; restore the helper.

- [ ] **Step 4: Run green and commit**

```bash
cd abc
bin/kaocha --focus abc.tools.linked-art-test
bin/kaocha --focus abc.tools.iiif-test
bin/kaocha --focus abc.tools.manifest-to-rdf-test
bin/kaocha --focus abc.tools.metadata-record-test
bin/kaocha --focus abc.tools.shacl-test
cd ..
git add abc/test/abc/tools/{linked_art,iiif,manifest_to_rdf,metadata_record}_test.clj
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
- Create matching `.edn`/`.json` pairs under `abc/docs/evidence/adr-capture/` and `abc/docs/evidence/adr-runs/`:
  `schema-validation-fixtures`, `tei-schematron-fixtures`, `tei-upstream-rng`, `tei-publication-sidecars`, `tei-profile-drift`, `linked-art-fixtures`, `iiif-applicability-fixtures`, `manifest-rdf-fixtures`, `metadata-rdf-rights`.
- Create: `abc/docs/evidence/adr-entries/schema-rdf-tei.edn`

**Interfaces:**
- Consumes: `abc-adr-evidence-capture-v1` and plan 1 self-binding rule.
- Produces: nine Boolean observations with exit code 0.

- [ ] **Step 1: Write descriptor contracts**

For example, `linked-art-fixtures.edn` has this exact shape (the other rows use the same closed key set):

```clojure
{:schema-version "abc-adr-evidence-capture-v1"
 :tool "bin/kaocha"
 :argv ["bin/kaocha" "--focus" "abc.tools.linked-art-test"]
 :input-profile {:kind "clojure-test-v1"
                 :roots ["abc.tools.linked-art-test"]
                 :explicit ["docs/evidence/adr-capture/linked-art-fixtures.edn"
                            "contexts/abc-v0.jsonld"
                            "examples/v0/example-work/manifest.json"
                            "examples/v0/example-work/metadata-record.json"
                            "examples/v0/example-work/lod/linked-art-candidate.jsonld"
                            "examples/v0/example-work/lod/linked-art-expanded.normalized.json"
                            "examples/v0/example-work/lod/jsonld-context-validation-result.json"]}
 :observation-key "linked-art-fixtures-pass"}
```

Use these exact boundaries:

| Basename | Roots/command | Observation | Explicit runtime inputs (list literally in descriptor) |
| --- | --- | --- | --- |
| schema-validation-fixtures | `abc.tools.validate-design-bundle-test` | `schema-and-canonicalization-fixtures-pass` | self; manifest schema; four canonicalization fixtures; Bash wrapper; CI workflow |
| tei-schematron-fixtures | schematron + validate-design-bundle test roots | `tei-project-schema-fixtures-pass` | self; ODD/RNG/Schematron; every catalog TEI fixture; Saxon pin file read by test |
| tei-upstream-rng | argv `nix develop .#validation --command bin/kaocha --focus abc.tools.tei-test`; profile `repo-files-v1` | `tei-upstream-rng-fixture-passes` | self; `flake.nix`; `flake.lock`; `deps.edn`; `test/abc/tools/tei_test.clj`; `src/abc/tools/tei.clj`; committed example TEI; the project RNG; the lockfile pins the upstream TEI input whose store path is supplied as `TEI_SCHEMA_PATH` |
| tei-publication-sidecars | `abc.tools.materialize-publication-test` | `tei-validation-sidecars-pass` | self; manifest/validation/preservation schemas; project RNG/Schematron; all example parser-IR/source/metadata/person inputs and publication policy read by test |
| linked-art-fixtures | `abc.tools.linked-art-test` | `linked-art-fixtures-pass` | self; context; manifest; metadata; three LOD outputs |
| iiif-applicability-fixtures | `abc.tools.iiif-test` | `iiif-applicability-fixtures-pass` | self; schema; example; all positive/negative IIIF fixtures |
| manifest-rdf-fixtures | `abc.tools.manifest-to-rdf-test` | `manifest-rdf-fixtures-pass` | self; bounded 12 Turtle files; success/failure JSON; resource oracle |
| metadata-rdf-rights | metadata-record + shacl test roots | `metadata-rdf-rights-pass` | self; metadata/person JSON+schemas; SHACL; metadata TTL; manifest inputs read by SHACL tests |

Direct Nix descriptor:

```clojure
{:schema-version "abc-adr-evidence-capture-v1"
 :tool "nix"
 :argv ["nix" "build" ".#checks.x86_64-linux.tei-profile-drift"]
 :input-profile {:kind "repo-files-v1" :roots []
                 :explicit ["docs/evidence/adr-capture/tei-profile-drift.edn"
                            "flake.nix" "flake.lock" "nix/tei-profile-artifacts.nix"
                            "schemas/tei-profile.odd" "schemas/tei-profile.rng"
                            "schemas/tei-profile.sch"]}
 :observation-key "tei-profile-derived-artifacts-match"}
```

Create `schema-rdf-tei.edn` with the exact 30 claim joins in Task 7's table.
It uses the closed registration-template shape from plan 1, contains no
artifact hashes, and includes two entries for ADR-0017-C5 because that claim
requires both manifest and metadata RDF observations.

- [ ] **Step 2: Commit descriptors before capture**

```bash
git add abc/docs/evidence/adr-capture \
  abc/docs/evidence/adr-entries/schema-rdf-tei.edn
git commit -m "docs(evidence): define schema RDF and TEI captures"
git status --short
```

Expected: clean tree. Descriptor self-hashes would otherwise stale immediately.

- [ ] **Step 3: Capture outside repo**

Capture all basenames with this concrete loop:

```bash
cd abc
for basename in schema-validation-fixtures tei-schematron-fixtures \
  tei-upstream-rng tei-publication-sidecars tei-profile-drift \
  linked-art-fixtures iiif-applicability-fixtures manifest-rdf-fixtures \
  metadata-rdf-rights; do
  clojure -M:abc/adr-evidence-capture \
    --descriptor "docs/evidence/adr-capture/${basename}.edn" \
    --output "/tmp/${basename}.json"
  test -z "$(git status --short)"
done
```

Expected: capture exit 0 and status empty each time. The exact
`nix develop .#validation --command ...` argv supplies the lockfile-pinned
`TEI_SCHEMA_PATH`; never replace it with a machine-local or literal
`/nix/store/...` path.

- [ ] **Step 4: Validate and install all nine bundles**

```bash
for f in /tmp/{schema-validation-fixtures,tei-schematron-fixtures,tei-upstream-rng,tei-publication-sidecars,tei-profile-drift,linked-art-fixtures,iiif-applicability-fixtures,manifest-rdf-fixtures,metadata-rdf-rights}.json; do
  jq -e '.schema_version == "abc-adr-evidence-run-v1" and ([.observations[].value] | all)' "$f"
done
cp /tmp/{schema-validation-fixtures,tei-schematron-fixtures,tei-upstream-rng,tei-publication-sidecars,tei-profile-drift,linked-art-fixtures,iiif-applicability-fixtures,manifest-rdf-fixtures,metadata-rdf-rights}.json docs/evidence/adr-runs/
```

Expected: every `jq` passes. Do not commit until registry/report updates are ready.

### Task 7: Join 30 claims and drive family audit to zero

**Files:**
- Modify: `abc/docs/adr/adr-evidence.edn`
- Add: nine Task 6 bundles
- Use: `abc/docs/evidence/adr-entries/schema-rdf-tei.edn`
- Regenerate: both migration reports

**Interfaces:**
- Consumes: nine bundles plus plan 1's `design-bundle-operational.json`/`design-bundle-exits-zero`.

- [ ] **Step 1: Review the checked-in claim-level joins**

| Claims | Evidence kind | Bundle / observation |
| --- | --- | --- |
| 0006-C1 | operational-observation | design-bundle-operational / design-bundle-exits-zero |
| 0006-C2,C9 | structural-test | schema-validation / schema-and-canonicalization-fixtures-pass |
| 0006-C3,C6 | fixture-conformance | schema-validation / same observation |
| 0006-C4,C5; 0012-C2,C4,C5 | fixture-conformance | tei-schematron / tei-project-schema-fixtures-pass |
| 0006-C7; 0013-C1..C3; 0017-C1..C3 | structural-test for 0017-C1, fixture-conformance otherwise | linked-art / linked-art-fixtures-pass |
| 0006-C8; 0014-C1..C4 | structural-test for 0014-C1, fixture-conformance otherwise | iiif / iiif-applicability-fixtures-pass |
| 0012-C1 | operational-observation | tei-profile-drift / tei-profile-derived-artifacts-match |
| 0012-C3 | fixture-conformance | tei-upstream-rng / tei-upstream-rng-fixture-passes |
| 0012-C6 | fixture-conformance | tei-publication-sidecars / tei-validation-sidecars-pass |
| 0017-C4 | structural-test | manifest-rdf / manifest-rdf-fixtures-pass |
| 0017-C5 | fixture-conformance | two valid entries: manifest-rdf and metadata-rdf |
| 0018-C1 | fixture-conformance | metadata-rdf-rights / metadata-rdf-rights-pass |
| 0018-C2,C3 | fixture-conformance | metadata-rdf-rights / same observation |

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
bin/kaocha --focus abc.tools.validate-design-bundle-test
bin/kaocha --focus abc.tools.schematron-test
bin/kaocha --focus abc.tools.tei-test
bin/kaocha --focus abc.tools.materialize-publication-test
bin/kaocha --focus abc.tools.linked-art-test
bin/kaocha --focus abc.tools.iiif-test
bin/kaocha --focus abc.tools.manifest-to-rdf-test
bin/kaocha --focus abc.tools.metadata-record-test
bin/kaocha --focus abc.tools.shacl-test
```

Expected: 0 failures/errors in the same pinned environment used for capture.

- [ ] **Step 2: Run direct and quality checks**

```bash
cd ..
nix run ./abc#validate-design-bundle
nix build ./abc#checks.x86_64-linux.tei-profile-drift
nix build ./abc#checks.x86_64-linux.git-cliff-config
nix build ./abc#checks.x86_64-linux.clj-kondo
nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests
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

1. `tei-upstream-rng` deliberately obtains `TEI_SCHEMA_PATH` from
   `nix develop .#validation`; if that command ceases to export the variable,
   the capture must fail rather than accepting an ambient path.
2. Plan 1's ledger validator decides whether overlapping original ADR 0012 rows may map many-to-one. Follow `validate-ledger`; do not loosen it.
3. ADR 0018 stays Accepted with release authority `none`: this plan characterizes containment but does not promote ADR 0035 or restore publication authority.
4. The 12-file Turtle inventory is deliberately bounded. Adding a new Turtle file later requires an explicit reviewed update, not an accidental glob expansion.
