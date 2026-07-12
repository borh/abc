# Temporal, Person, and Ingest ADR Evidence Migration Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Correct every temporal/person/ingest Acceptance Criterion, add the missing bounded assertions, and join every resulting Accepted claim to fresh artifact-backed evidence without weakening the substantive decisions.

**Architecture:** Execute the family transaction in two clean-tree stages. Stage A adds focused tests first, then atomically corrects ADRs 0015, 0016, 0020, 0021, and 0022 together with lifecycle headers, content-hash ledger mappings, capture descriptors, inventory expectations, and the generated inventory. Stage B captures narrowly scoped immutable bundles from that clean commit, adds claim-level registry joins, regenerates reports, and proves this family's governance problem set is empty while global enforcement remains in audit mode.

**Tech Stack:** Clojure 1.12, `clojure.test`, Kaocha, Jena/SHACL, JSON Schema 2020-12, EDN, deterministic JSON/JCS SHA-256, Nix, Git.

## Global Constraints

- Plans 1 and 2 are required completed preconditions. Although this family is
  independently reviewable, execution is sequential because both prior plans
  mutate the shared ledger, registry, reports, and ADR graph.
- Consume Plan 1's immutable `docs/adr/adr-claim-migration-baseline.json`, content-hash-keyed `docs/adr/adr-claim-migration.edn`, disposition-aware inventory validator/report, `## Historical Evidence` / `## Future Verification` claim-header lint, checked capture descriptor/bundle layout, and repaired hermetic design-bundle plus separate Git-cliff checks. Do not recreate or fork those interfaces here.
- Follow `docs/superpowers/specs/2026-07-12-adr-evidence-corpus-migration-design.md` exactly.
- Audit mode remains active. This plan is not authorized to accept ADR 0034 or switch any Nix governance gate to enforcement.
- Criterion correction precedes final claim-ID assignment. Once the Stage A claim headers are committed, their IDs are stable.
- Plan 1's normative-section hash guard must remain green. Preserve original Decision/Hard Rule bytes; only duplicated criteria/status prose may move to non-acceptance sections.
- All 36 baseline rows in family `temporal-person-ingest` receive exactly one reviewed disposition and an exact `:resulting-claim-ids` vector.
- A passing namespace supports only assertions that namespace actually makes. Runtime-read schemas, SHACL, JSON fixtures, sidecars, and TTL graphs are explicit capture inputs. Every boundary also has a checked runtime-input manifest whose exact equality with the files observed by its focused execution is proved through Plan 1's shared closure contract; a hand-written `:explicit` subset is not sufficient.
- Keep legacy `nil` temporal behavior bounded to the named cases; never claim that it preserves unknown versus not-recorded knowledge state. ADR 0036 owns that future correction.
- Command-level behavior requires a subprocess/direct operational assertion. A pure helper test does not prove a CLI exit contract.
- `nix flake check` is not a timeless acceptance fact. Move the old criterion out of Acceptance Criteria; do not manufacture a passing historical observation.
- Every source/test commit is green. Criterion edits, ledger mappings, capture descriptors, inventory expectations, and regenerated inventory land in one atomic Stage A commit.
- Capture requires a clean Git worktree. Descriptors must already be committed and must include themselves in `:input-profile :explicit`.
- Generated reports are outputs; never hand-edit `docs/reports/adr-claim-migration-inventory.json` or `docs/reports/adr-evidence-migration.json`.

---

## File Structure

- `test/abc/tools/aozora_csv_test.clj` — exact named date-normalization and EDTF parser examples.
- `test/abc/tools/person_record_test.clj` — schema lexical contract, committed person fixture, RDF temporal dispatch, and Level 1 admission.
- `test/abc/tools/shacl_test.clj` — exact `PersonRecordShape` structure and malformed EDTF behavior.
- `test/abc/tools/person_drift_test.clj` — event schema/cardinality, JSON coherence, SHACL resources, RDF derivation, committed example, and validation failure families.
- `test/abc/tools/aozora_ingest_test.clj` — bounded with/without drift-sidecar identity comparison, raw-ingest independence, and synthetic schema-hash cascade changed set.
- `test/abc/tools/aozora_history_audit_test.clj` — report-level empty/update behavior and direct subprocess exit-gate matrix.
- `docs/evidence/adr-inputs/temporal-*.edn`, `person-drift-*.edn`, `aozora-*.edn` — checked exact runtime-data read manifests consumed by Plan 1's capture closure contract.
- `docs/adr/0015-temporal-modeling.md` — six corrected typed claims; lifecycle `fixture` / `publication`.
- `docs/adr/0016-edtf-level1-decade-century.md` — four corrected typed claims; Hard Rule removed from Acceptance Criteria; lifecycle `fixture` / `publication`.
- `docs/adr/0020-person-identity-drift-data-model.md` — four corrected typed claims; review checklists removed from Acceptance Criteria; lifecycle `fixture` / `publication`.
- `docs/adr/0021-person-identity-drift-harness.md` — twelve corrected typed claims; full-flake statement moved out; lifecycle `fixture` / `publication`.
- `docs/adr/0022-upstream-ingest-drift-awareness.md` — six corrected typed claims; Nix-app operation moved to Future Verification; lifecycle `operational` / `development`.
- `docs/adr/adr-claim-migration.edn` — 36 baseline-hash dispositions and final claim mappings.
- `test/abc/tools/adr_evidence_inventory_test.clj` — temporal family count/mapping expectations using Plan 1's disposition-aware interface.
- `docs/evidence/adr-capture/temporal-*.edn`, `person-drift-*.edn`, `aozora-*.edn` — checked clean-tree capture descriptors, each naming its corresponding runtime-input manifest.
- `docs/evidence/adr-runs/temporal-*.json`, `person-drift-*.json`, `aozora-*.json` — generated executable bundles.
- `docs/adr/adr-evidence.edn` — claim-level joins only; no observations, inputs, or verdicts.
- `docs/reports/adr-claim-migration-inventory.json` and `docs/reports/adr-evidence-migration.json` — deterministic generated outputs.

## Plan 1 Interfaces Consumed

- Baseline identity: `docs/adr/adr-claim-migration-baseline.json`; select ledger keys from each row's immutable `original_text_hash`, never from edited Markdown.
- Reviewed source: `docs/adr/adr-claim-migration.edn`; each key is `[adr-number original-criterion-text-hash]` and each value has one disposition, a rationale when non-retained, a planned evidence boundary for live claims, and `:resulting-claim-ids` after Stage A.
- Inventory producer: `clojure -M:abc/adr-evidence-inventory -- --output docs/reports/adr-claim-migration-inventory.json`; it rejects duplicate/unresolved baseline keys and missing resulting claim IDs.
- Evidence capture: `clojure -M:abc/adr-evidence-capture -- --descriptor PATH --output PATH`; a successful bundle contains one Boolean observation named by the descriptor.
- Runtime-input closure: each descriptor names `:runtime-input-manifest "docs/evidence/adr-inputs/<stem>.edn"`; that EDN has exact shape `{:schema-version :abc-adr-runtime-inputs-v1 :paths [...]}` with a sorted, duplicate-free vector of repository/workspace-relative runtime data paths. Focused boundary tests trace repository reads through `abc.tools.evidence-io` and require exact equality with `:paths`. Capture requires descriptor `:explicit` to equal the manifest paths plus the descriptor and manifest paths; source/test namespace closure remains mechanically derived.
- Evidence validation/join: `abc.tools.adr-evidence/validate-registry` through `clojure -M:abc/adr-governance -- --mode audit --report docs/reports/adr-evidence-migration.json`; the optional leading `--` is normalized by Plan 1 and no positional repository argument is accepted.
- Shared command checks use the evaluated system instead of a hard-coded architecture: `system=$(nix eval --raw --impure --expr builtins.currentSystem)`. `nix run ./abc#validate-design-bundle` is hermetic after Plan 1; `nix build "./abc#checks.${system}.git-cliff-config"` owns synthetic Git-cliff validation.

---

### Task 0: Verify the shared prerequisite and freeze the family baseline

**Files:** No edits.

**Interfaces:**
- Consumes: all Plan 1 interfaces listed above.
- Produces: a clean, green starting revision and a reviewed list of exactly 36 immutable family baseline keys.

- [ ] **Step 1: Verify the worktree and Plan 1 artifacts**

```bash
git status --short --branch
test -f abc/docs/adr/adr-claim-migration-baseline.json
test -f abc/docs/adr/adr-claim-migration.edn
test -f abc/docs/superpowers/plans/2026-07-12-adr-evidence-migration-foundation.md
```

Expected: clean worktree and all three files exist. If Plan 1 is absent or uncommitted, STOP; no family edits or captures are valid yet.

- [ ] **Step 2: Verify the repaired shared command boundaries**

```bash
nix run ./abc#validate-design-bundle
system=$(nix eval --raw --impure --expr builtins.currentSystem)
nix build "./abc#checks.${system}.git-cliff-config"
```

Expected: both exit 0. The first must not inspect real Git history; the second must use Plan 1's synthetic repository.

- [ ] **Step 3: Extract and count immutable family rows**

```bash
cd abc
jq '[.criteria[] | select(.family == "temporal-person-ingest")] | length' \
  docs/reports/adr-claim-migration-inventory.json
jq '[.criteria[] | select(.family == "temporal-person-ingest") | .adr] | group_by(.) | map({adr: .[0], count: length})' \
  docs/reports/adr-claim-migration-inventory.json
```

Expected: total `36`; per ADR counts are `0015=5`, `0016=4`, `0020=7`, `0021=13`, `0022=7`. Confirm each row resolves to one baseline manifest hash before editing prose.

- [ ] **Step 4: Run the inherited focused baseline**

```bash
bin/kaocha \
  --focus abc.tools.aozora-csv-test \
  --focus abc.tools.person-record-test \
  --focus abc.tools.shacl-test \
  --focus abc.tools.person-drift-test \
  --focus abc.tools.aozora-ingest-test \
  --focus abc.tools.aozora-history-audit-test
```

Expected: zero failures and zero errors. Run the drift fixture boundary separately because broad design-bundle tests may exercise unrelated toolchain surfaces:

```bash
bin/kaocha --focus abc.tools.validate-design-bundle-test/validate-drift-fixtures-smoke-test
```

Expected: `1 tests, 1 assertions, 0 failures`.

---

### Task 1: Complete the bounded temporal schema, parser, RDF, and SHACL assertions

**Files:**
- Modify: `abc/test/abc/tools/aozora_csv_test.clj`
- Modify: `abc/test/abc/tools/person_record_test.clj`
- Modify: `abc/test/abc/tools/shacl_test.clj`
- Modify: `abc/src/abc/tools/shacl.clj`

**Interfaces:**
- Consumes: `abc.tools.aozora-csv/parse-date`, `abc.tools.person-record/validate!`, `record->graph`, and `abc.tools.shacl/load-shapes-graph`.
- Produces: focused Boolean boundaries `temporal-date-normalization`, `temporal-person-record-contract`, and `temporal-person-shacl-contract`.

- [ ] **Step 1: Add the exact BCE exemplar and a direct L0 lexical accept/reject table**

Add the `前347` assertion to `parse-date-bce-astronomical-test`:

```clojure
(is (= ["-0346" [{"raw" "前347"
                    "corrected" "-0346"
                    "rule" "bce-astronomical"}]]
       (ac/parse-date "前347")))
```

Add this test to `person_record_test.clj`:

```clojure
(deftest nullable-date-l0-lexical-contract-test
  (doseq [value [nil "1892-03-01" "1904-01" "1941"
                 "-0426-01-15" "-0426-01" "-0426"]]
    (is (= :ok (pr/validate! (assoc (example-person) "date_of_birth" value)))
        (str "expected accepted temporal lexical value " (pr-str value))))
  (doseq [value ["1892?" "1984/1999" "{1984, 1986}"
                 "1892-00" "1892-13" "1892-01-00" "1892-01-32"]]
    (is (thrown? clojure.lang.ExceptionInfo
                 (pr/validate! (assoc (example-person) "date_of_birth" value)))
        (str "expected rejected temporal lexical value " (pr-str value)))))
```

- [ ] **Step 2: Run the new temporal tests and verify RED where coverage is missing**

```bash
cd abc
bin/kaocha \
  --focus abc.tools.aozora-csv-test/parse-date-bce-astronomical-test \
  --focus abc.tools.person-record-test/nullable-date-l0-lexical-contract-test
```

Expected: the new assertions compile and expose any mismatch between the live schema/calendar gate and the corrected bounded criterion. Do not weaken the table to accommodate a failure; repair only a real contract regression.

- [ ] **Step 3: Add a committed-fixture-to-RDF assertion**

```clojure
(deftest example-person-fixture-rdf-temporal-dispatch-test
  (let [record (files/read-json fixture-path)
        graph (pr/record->graph record)
        [dob] (objects-of graph "http://RDVocab.info/ElementsGr2/dateOfBirth")
        [edtf] (objects-of graph "https://w3id.org/abc/edtfDateOfBirth")]
    (is (= "1892-03-01" (.getLiteralLexicalForm dob)))
    (is (= "http://www.w3.org/2001/XMLSchema#date"
           (.getLiteralDatatypeURI dob)))
    (is (= "1892-03-01" (.getLiteralLexicalForm edtf)))
    (is (= "https://w3id.org/abc/EDTF" (.getLiteralDatatypeURI edtf)))))
```

- [ ] **Step 4: Add a structural `PersonRecordShape` test independent of the malformed fixture**

In `shacl_test.clj`, add local Jena helpers that return objects for an exact
subject/predicate pair and walk an RDF list. Assert graph structure rather
than Turtle serialization order:

```clojure
(deftest person-record-temporal-shape-contract-test
  (let [g (shacl/load-shapes-graph)
        ttl (files/read-text "schemas/manifest.shacl.ttl")
        dob-property (exact-property-shape g rdag2-date-of-birth)
        edtf-property (exact-property-shape g abc-edtf-date-of-birth)]
    (is (= #{xsd-date xsd-g-year-month xsd-g-year}
           (datatype-alternatives g dob-property)))
    (is (= #{(NodeFactory/createLiteral "1" XSDDatatype/XSDinteger)}
           (set (objects g edtf-property sh-max-count))))
    (is (= #{abc-edtf}
           (set (objects g edtf-property sh-datatype))))
    (is (string/includes? ttl
                          "sh:pattern \"^(-?\\\\d{4}"))
    (is (string/includes? ttl "|-?\\\\d{3}X|-?\\\\d{2}XX)"))
    (is (pos? (count (iterator-seq (.find g)))))))
```

Define the URI nodes with `NodeFactory/createURI`. `exact-property-shape`
must find the unique subject having the requested `sh:path` object;
`datatype-alternatives` follows `sh:or` through `rdf:first`/`rdf:rest` and
returns the `sh:datatype` objects. Fail when a property shape or list link is
missing or non-unique. Keep the lexical pattern assertion exact because the
regular-expression lexeme itself is part of the contract. This test is
separate from `validate-malformed-edtf-date-of-birth-test` so structural and
fixture claims remain independently joinable.

- [ ] **Step 5: Run GREEN focused verification**

```bash
bin/kaocha \
  --focus abc.tools.aozora-csv-test \
  --focus abc.tools.person-record-test \
  --focus abc.tools.shacl-test
```

Expected: zero failures and zero errors.

- [ ] **Step 6: Commit only bounded temporal tests**

```bash
git add test/abc/tools/aozora_csv_test.clj \
        test/abc/tools/person_record_test.clj \
        test/abc/tools/shacl_test.clj
git commit -m "test(adr): bound temporal person evidence"
```

---

### Task 2: Complete drift coherence, SHACL-resource, RDF, and committed-example assertions

**Files:**
- Modify: `abc/test/abc/tools/person_drift_test.clj`

**Interfaces:**
- Consumes: `event-json-coherence-failures`, `event->graph`, `validate-drift-events!`, both drift JSON Schemas, and `manifest.shacl.ttl`.
- Produces: one focused boundary `person-drift-contract` that directly asserts every ADR 0020/0021 invariant joined to it.

- [ ] **Step 1: Write the missing JSON-coherence tests**

```clojure
(deftest validate-event-json-coherence-rejects-duplicate-snapshot-id-test
  (let [event (base-split)
        duplicate (first (get event "participants"))
        bad (update event "participants" conj duplicate)]
    (is (some #{:duplicate-snapshot-id}
              (map :code (drift/event-json-coherence-failures bad))))))

(deftest validate-event-json-coherence-rejects-interleaved-participant-test
  (let [bad (-> (base-split)
                (assoc-in ["prov" "used"] ["pre-000879"
                                              "post-abc-000000000001"])
                (assoc-in ["prov" "was_generated_by"]
                          ["post-abc-000000000001"
                           "post-abc-000000000002"]))]
    (is (some #{:participant-in-both-used-and-generated}
              (map :code (drift/event-json-coherence-failures bad))))))

(deftest validate-event-json-coherence-rejects-noncanonical-edge-order-test
  (let [split (update-in (base-split) ["prov" "was_generated_by"] reverse)
        merge (update-in (base-merge) ["prov" "used"] reverse)]
    (is (some #{:generated-not-sorted}
              (map :code (drift/event-json-coherence-failures split))))
    (is (some #{:used-not-sorted}
              (map :code (drift/event-json-coherence-failures merge))))))
```

- [ ] **Step 2: Run RED/GREEN for coherence without changing production behavior**

```bash
cd abc
bin/kaocha \
  --focus abc.tools.person-drift-test/validate-event-json-coherence-rejects-duplicate-snapshot-id-test \
  --focus abc.tools.person-drift-test/validate-event-json-coherence-rejects-interleaved-participant-test \
  --focus abc.tools.person-drift-test/validate-event-json-coherence-rejects-noncanonical-edge-order-test
```

Expected: PASS against the existing core. If any test fails, first confirm the criterion is core and then make the smallest `event-json-coherence-failures` repair; do not narrow the claim around a real hole.

- [ ] **Step 3: Extend the RDF test to assert the missing associated-agent edge**

Add to `event->graph-materializes-types-and-derived-prov-test`:

```clojure
(is (contains? triples
               [event-iri
                "http://www.w3.org/ns/prov#wasAssociatedWith"
                "https://w3id.org/abc/agents/editorial-board"]))
```

Expected RED before the assertion only if the implementation regressed; the current source emits this triple.

- [ ] **Step 4: Add exact SHACL resource/axiom assertions**

```clojure
(deftest drift-shacl-resources-and-subclass-axioms-test
  (let [g (shacl/load-shapes-graph)]
    (doseq [shape [person-drift-event-shape
                   person-drift-split-event-shape
                   person-drift-merge-event-shape]]
      (is (graph-contains? g shape rdf-type sh-node-shape)))
    (doseq [[child parent] [[drift-event prov-activity]
                            [drift-split-event drift-event]
                            [drift-merge-event drift-event]]]
      (is (graph-contains? g child rdfs-sub-class-of parent)))))
```

Add `[abc.tools.shacl :as shacl]` and define each constant with
`NodeFactory/createURI`; `graph-contains?` must query an exact Jena triple.
Add `[abc.tools.files :as files]`; the shared read helper records the schema
path when a trace is active. Ensure `shacl/load-shapes-graph` calls
`record-read!` immediately before its Jena load rather than bypassing the
trace.
The three shapes and three subclass axioms must be asserted, not merely “the
file loads.”

- [ ] **Step 5: Add committed example count and validation assertions**

```clojure
(deftest committed-drift-example-layout-and-validation-test
  (let [persons-dir "examples/v0/example-persons"
        events (->> (file-seq (io/file persons-dir "_events"))
                    (filter #(.isFile %))
                    (filter #(string/ends-with? (.getName %) ".json")))
        indexes (->> (file-seq (io/file persons-dir "_indexes"))
                     (filter #(.isFile %))
                     (filter #(string/ends-with? (.getName %) ".json")))]
    (is (= 1 (count events)))
    (is (= 3 (count indexes)))
    (is (= {:status :ok :events 1 :indexes 3}
           (drift/validate-drift-events! {:persons-dir persons-dir})))))
```

Add `[clojure.java.io :as io]` and `[clojure.string :as string]` to the test namespace.

- [ ] **Step 6: Run GREEN drift boundaries**

```bash
bin/kaocha --focus abc.tools.person-drift-test
bin/kaocha --focus abc.tools.validate-design-bundle-test/validate-drift-fixtures-smoke-test
```

Expected: zero failures and zero errors; the exact negative-fixture var reports one passing test.

- [ ] **Step 7: Commit drift tests**

```bash
git add test/abc/tools/person_drift_test.clj
git commit -m "test(adr): close person drift evidence gaps"
```

---

### Task 3: Add bounded manifest/cascade and raw-ingest independence tests

**Files:**
- Modify: `abc/test/abc/tools/aozora_ingest_test.clj`

**Interfaces:**
- Consumes: `abc.tools.aozora-ingest/run-from-rows!`, deterministic JSON readers/writers, `person-record/record-hash`, `metadata-record/record-hash`, and `manifest/artifact-id` semantics.
- Produces: focused boundary `aozora-ingest-drift-invariants` proving only the example-slice with/without-sidecar invariant and one synthetic schema-hash changed set.

- [ ] **Step 1: Add a recursive JSON changed-path helper local to the test**

```clojure
(defn- changed-json-paths
  ([before after] (changed-json-paths [] before after))
  ([path before after]
   (cond
     (and (map? before) (map? after))
     (into #{}
           (mapcat (fn [k]
                     (changed-json-paths (conj path k)
                                         (get before k ::missing)
                                         (get after k ::missing))))
           (set/union (set (keys before)) (set (keys after))))

     (and (vector? before) (vector? after))
     (into #{}
           (mapcat (fn [i]
                     (changed-json-paths (conj path i)
                                         (get before i ::missing)
                                         (get after i ::missing))))
           (range (max (count before) (count after))))

     (= before after) #{}
     :else #{path})))
```

Add `[clojure.set :as set]`, `[abc.tools.manifest :as manifest]`, and
`[abc.tools.malli :as am]` to the namespace.

- [ ] **Step 2: Write the with/without-sidecar output identity test**

The test must run the same `synthetic-rows-with-edition` twice into isolated work/person directories, seed identical manifests from `examples/v0/example-work/manifest.json`, add only `_events` and `_indexes` between the first and second logical cases, and assert exact bytes for the generated person, metadata record, manifest, and parsed `manifest_identity_object`:

```clojure
(deftest drift-sidecars-do-not-affect-ingest-or-manifest-identity-test
  (let [without (run-isolated-ingest! synthetic-rows-with-edition false)
        with (run-isolated-ingest! synthetic-rows-with-edition true)]
    (is (= (:person-bytes without) (:person-bytes with)))
    (is (= (:metadata-bytes without) (:metadata-bytes with)))
    (is (= (:manifest-bytes without) (:manifest-bytes with)))
    (is (= (:manifest-identity-object without)
           (:manifest-identity-object with)))))
```

Implement `run-isolated-ingest!` in the same test file using existing temp-dir/delete helpers. When `with-sidecars?` is true, copy the three committed index JSON files and one event JSON file into the generated persons directory before the second identical `run-from-rows!`; do not mutate production code or the committed fixtures.

- [ ] **Step 3: Run RED before adding the helper and then GREEN**

```bash
cd abc
bin/kaocha --focus abc.tools.aozora-ingest-test/drift-sidecars-do-not-affect-ingest-or-manifest-identity-test
```

Expected RED: unresolved `run-isolated-ingest!`. After the helper is implemented, PASS with byte equality. A failure indicates a real forbidden sidecar dependency; fix that dependency rather than weakening the assertions.

- [ ] **Step 4: Add the bounded synthetic schema-hash cascade test**

Run the same isolated ingest twice while `with-redefs` changes only the return value for `schemas/person-record.schema.json`; all other schema hashes remain live. Compare parsed values and assert this exact changed set:

```clojure
(deftest person-schema-hash-cascade-is-bounded-on-example-slice-test
  (let [baseline (run-with-person-schema-hash! (manifest/schema-hash
                                                  "schemas/person-record.schema.json"))
        rotated (run-with-person-schema-hash!
                 "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")]
    (is (= #{["person" "person_record_schema_hash"]
             ["metadata" "contributors" 0 "person_record_hash"]
             ["manifest" "manifest_identity_object" "metadata_record_hash"]
             ["manifest" "artifact_id"]}
           (changed-json-paths baseline rotated)))
    (is (= (dissoc (get baseline "person") "person_record_schema_hash")
           (dissoc (get rotated "person") "person_record_schema_hash")))
    (is (= (get baseline "metadata")
           (assoc-in (get rotated "metadata")
                     ["contributors" 0 "person_record_hash"]
                     (get-in baseline
                             ["metadata" "contributors" 0
                              "person_record_hash"]))))))
```

Use `changed-json-paths` directly; do not introduce a differently named wrapper.
Define `run-with-person-schema-hash!` to return the JSON-like map
`{"person" ..., "metadata" ..., "manifest" ...}`. Add
the imports named in Step 1, capture the original `am/cached-schema-hash`
function before `with-redefs`, and redefine that Var selectively:

```clojure
(let [live-cached-schema-hash am/cached-schema-hash]
  (with-redefs [am/cached-schema-hash
                (fn [path]
                  (if (= path ingest/person-schema-path)
                    person-schema-hash
                    (live-cached-schema-hash path)))]
    (run-isolated-ingest-and-read!)))
```

Do not redefine `manifest/schema-hash`: ingestion calls the Malli cache, and
the baseline run may already have cached the original value. Run each case in
fresh isolated output directories. The claim must be worded as this exact
example-slice mutation, not a universal repository cascade.

- [ ] **Step 5: Run the complete ingest namespace GREEN**

```bash
bin/kaocha --focus abc.tools.aozora-ingest-test
```

Expected: zero failures and zero errors.

- [ ] **Step 6: Commit bounded ingest/identity tests**

```bash
git add test/abc/tools/aozora_ingest_test.clj
git commit -m "test(adr): bound drift identity and ingest independence"
```

---

### Task 4: Correct the audit report and command failure-gate observations

**Files:**
- Modify: `abc/src/abc/tools/aozora_history_audit.clj`
- Modify: `abc/test/abc/tools/aozora_history_audit_test.clj`

**Interfaces:**
- Consumes: `drift-participant-updates`, `audit!`, and existing synthetic JGit/ZIP fixture builders.
- Produces: `exit-failure? [{:keys [validation-failed? fail-on-candidates? candidate-count fail-on-drift-participant-updates? drift-participant-update-count]}] -> boolean`, report-level empty/update assertions, and an actual subprocess exit matrix.

- [ ] **Step 1: Write RED tests for the corrected failure condition**

```clojure
(deftest drift-update-gate-is-one-independent-failure-condition-test
  (is (false? (audit/exit-failure?
               {:validation-failed? false :fail-on-candidates? false
                :candidate-count 0 :fail-on-drift-participant-updates? true
                :drift-participant-update-count 0})))
  (is (true? (audit/exit-failure?
              {:validation-failed? false :fail-on-candidates? false
               :candidate-count 0 :fail-on-drift-participant-updates? true
               :drift-participant-update-count 1})))
  (is (true? (audit/exit-failure?
              {:validation-failed? true :fail-on-candidates? false
               :candidate-count 0 :fail-on-drift-participant-updates? false
               :drift-participant-update-count 0}))))
```

Run:

```bash
cd abc
bin/kaocha --focus abc.tools.aozora-history-audit-test/drift-update-gate-is-one-independent-failure-condition-test
```

Expected: FAIL because `exit-failure?` is not defined.

- [ ] **Step 2: Extract the existing Boolean without changing CLI behavior**

```clojure
(defn exit-failure?
  [{:keys [validation-failed? fail-on-candidates? candidate-count
           fail-on-drift-participant-updates? drift-participant-update-count]}]
  (boolean
   (or validation-failed?
       (and fail-on-candidates? (pos? candidate-count))
       (and fail-on-drift-participant-updates?
            (pos? drift-participant-update-count)))))
```

Replace the inline `boolean/or` in `-main` with this function using the exact computed values. Do not change flag defaults or exit semantics.

- [ ] **Step 3: Strengthen helper tests to report-level assertions**

In the existing no-sidecar and valid-sidecar tests, call the public `audit!` synthetic-ref path (as the integrated test already does) and assert:

```clojure
(is (= [] (:drift_participant_updates result)))
```

and, for one indexed changed participant:

```clojure
(is (= ["000879"]
       (mapv #(get % "person_id") (:drift_participant_updates result))))
```

Keep the existing direct helper assertions as unit localization.

- [ ] **Step 4: Add a direct subprocess exit assertion**

Reuse the test's JGit/ZIP builders to create two refs and invoke the real JVM entry point with `ProcessBuilder`, not `with-redefs`:

```clojure
(defn- run-audit-process [args]
  (let [p (.start (doto (ProcessBuilder.
                         (into ["clojure" "-M:abc/aozora-history-audit"] args))
                    (.directory (io/file "."))))]
    {:exit (.waitFor p)
     :stdout (slurp (.getInputStream p))
     :stderr (slurp (.getErrorStream p))}))
```

Add `aozora-history-audit-cli-drift-update-exit-matrix-test` with two isolated fixtures while candidate/validation counts are zero: unchanged indexed participant plus the flag exits `0`; changed indexed participant plus the flag exits `1`; changed indexed participant without the flag exits `0`. This is the direct operational observation required for ADR 0022; the pure helper test alone is not evidence for the command.

- [ ] **Step 5: Run GREEN audit tests**

```bash
bin/kaocha --focus abc.tools.aozora-history-audit-test
```

Expected: zero failures and zero errors, including all three subprocess exits.

- [ ] **Step 6: Commit the behavior-preserving extraction and tests**

```bash
git add src/abc/tools/aozora_history_audit.clj \
        test/abc/tools/aozora_history_audit_test.clj
git commit -m "test(adr): observe upstream audit failure gates"
```

---

### Task 5: Atomically correct claims, lifecycle, ledger, descriptors, and inventory (Stage A)

**Files:**
- Modify: `abc/docs/adr/0015-temporal-modeling.md`
- Modify: `abc/docs/adr/0016-edtf-level1-decade-century.md`
- Modify: `abc/docs/adr/0020-person-identity-drift-data-model.md`
- Modify: `abc/docs/adr/0021-person-identity-drift-harness.md`
- Modify: `abc/docs/adr/0022-upstream-ingest-drift-awareness.md`
- Modify: `abc/docs/adr/adr-claim-migration.edn`
- Modify: `abc/test/abc/tools/adr_evidence_inventory_test.clj`
- Create: `abc/test/abc/tools/temporal_evidence_input_test.clj`
- Create: eight manifests under `abc/docs/evidence/adr-inputs/` with the same basenames as the descriptors below.
- Create: `abc/docs/evidence/adr-capture/temporal-date-normalization.edn`
- Create: `abc/docs/evidence/adr-capture/temporal-person-record-contract.edn`
- Create: `abc/docs/evidence/adr-capture/temporal-person-shacl-contract.edn`
- Create: `abc/docs/evidence/adr-capture/person-drift-contract.edn`
- Create: `abc/docs/evidence/adr-capture/person-drift-negative-fixtures.edn`
- Create: `abc/docs/evidence/adr-capture/aozora-ingest-drift-invariants.edn`
- Create: `abc/docs/evidence/adr-capture/aozora-history-audit.edn`
- Create: `abc/docs/evidence/adr-capture/aozora-history-audit-cli.edn`
- Create: `abc/docs/evidence/adr-entries/temporal-person-ingest.edn`
- Regenerate: `abc/docs/reports/adr-claim-migration-inventory.json`

**Interfaces:**
- Consumes: Plan 1's baseline/ledger/inventory contracts and Tasks 1–4's green focused boundaries.
- Produces: final stable family claim IDs, exactly 36 reviewed baseline mappings, committed clean-tree capture descriptors, and the Stage A audit state.

- [ ] **Step 1: Apply the exact final claim set from Appendix A**

Replace the five ADR Acceptance Criteria sections with Appendix A verbatim. Add lifecycle headers:

```text
ADR 0015  Validation scope: fixture      Release authority: publication
ADR 0016  Validation scope: fixture      Release authority: publication
ADR 0020  Validation scope: fixture      Release authority: publication
ADR 0021  Validation scope: fixture      Release authority: publication
ADR 0022  Validation scope: operational  Release authority: development
```

Move, do not delete, the non-observable text identified in Appendix B. No claim header may appear under `## Historical Evidence` or `## Future Verification`.

- [ ] **Step 2: Populate all 36 ledger dispositions by immutable baseline hash**

Use Appendix B. For each original row, copy the hash from `adr-claim-migration-baseline.json`; do not hash the corrected text. Set the exact disposition, rationale, planned boundary, and resulting IDs. Assert with Plan 1's ledger validator that there are no duplicate or unresolved temporal keys.

- [ ] **Step 3: Write checked capture descriptors**

Every descriptor has exactly this shape (example shown for the CSV boundary):

```clojure
{:schema-version "abc-adr-evidence-capture-v2"
 :tool "bin/kaocha"
 :argv ["bin/kaocha" "--focus" "abc.tools.aozora-csv-test"]
 :runtime-input-manifest
 "docs/evidence/adr-inputs/temporal-date-normalization.edn"
 :input-profile
 {:kind "clojure-test-v1"
  :roots ["abc.tools.aozora-csv-test"]
  :explicit ["docs/evidence/adr-capture/temporal-date-normalization.edn"
             "docs/evidence/adr-inputs/temporal-date-normalization.edn"]}
 :observation-key "temporal-date-normalization"}
```

Use Appendix C for every descriptor's command, root, observation key, and
runtime inputs. Create the corresponding manifest with exact shape:

```clojure
{:schema-version :abc-adr-runtime-inputs-v1
 :paths ["sorted/repository-relative/runtime-data-path"]}
```

The descriptor's `:explicit` vector must equal the manifest `:paths` plus its
own descriptor path and manifest path. Source and test namespaces remain
derived by the profile and are not duplicated in the runtime manifest. For
`person-drift-negative-fixtures.edn`, mechanically enumerate every file
returned by:

```bash
rg --files fixtures/v0/invalid/drift | LC_ALL=C sort
```

and add those files, both drift schemas, and `schemas/manifest.shacl.ttl` to
the runtime manifest. No glob or directory name is a valid substitute for
exact file bindings.

- [ ] **Step 3A: Prove exact runtime-input closure for all eight boundaries**

In `temporal_evidence_input_test.clj`, use Plan 1's invocation-local,
acyclic `abc.tools.evidence-io` trace hook around each exact set of focused
test vars. Shared `files` and `hash` reads record transitively through that
hook. For every descriptor, call:

```clojure
(runtime-inputs/assert-runtime-input-closure!
 {:repo-root "."
  :workspace-root ".."
  :descriptor {:path descriptor-path :value descriptor}
  :observed-paths observed-paths})
```

where `observed-paths` is the sorted vector of repository-relative data paths
actually read through `evidence-io`. Assert exact equality among that vector,
the checked manifest's `:paths`, and descriptor `:explicit` after removing
only the descriptor and manifest paths. Run each boundary separately so a
broad namespace cannot donate unrelated reads to a narrow observation. Add a
negative test that removes one fixture path and another that adds one unread
path; both must fail closure. Run Plan 1's bypass lint over every descriptor's
complete statically derived source and test namespace closure, not only the
direct test namespace. Reject direct path-based `slurp`, reader,
`java.nio.file.Files`,
Jena, and equivalent repository reads. Only the shared `files`/`hash` helpers
and named adapters that call `record-read!` immediately before a library load
are exempt. Stream reads such as subprocess stdout/stderr are not repository
path reads and the lint must distinguish them. Add a synthetic transitive
helper containing a bypass and require
the lint to fail. Route each existing bypass through a traceable adapter
without changing the value or ownership of the read.

Create `temporal-person-ingest.edn` from Appendix D using plan 1's closed
registration-template contract. It contains every Appendix D claim/evidence
row, the exact artifact path and observation key, and
`:expected {:operator := :value true}`, but no `:artifact-hash`.

- [ ] **Step 4: Update inventory tests before regenerating**

Using Plan 1's disposition-aware row shape, assert:

```clojure
(is (= 32 (get-in value ["families" "temporal-person-ingest"])))
(is (= #{"ADR-0015-C1" "ADR-0015-C2" "ADR-0015-C3"
         "ADR-0015-C4" "ADR-0015-C5" "ADR-0015-C6"
         "ADR-0016-C1" "ADR-0016-C2" "ADR-0016-C3" "ADR-0016-C4"
         "ADR-0020-C1" "ADR-0020-C2" "ADR-0020-C3" "ADR-0020-C4"
         "ADR-0021-C1" "ADR-0021-C2" "ADR-0021-C3" "ADR-0021-C4"
         "ADR-0021-C5" "ADR-0021-C6" "ADR-0021-C7" "ADR-0021-C8"
         "ADR-0021-C9" "ADR-0021-C10" "ADR-0021-C11" "ADR-0021-C12"
         "ADR-0022-C1" "ADR-0022-C2" "ADR-0022-C3"
         "ADR-0022-C4" "ADR-0022-C5" "ADR-0022-C6"}
       (->> (get value "criteria")
            (filter #(= "temporal-person-ingest" (get % "family")))
            (map #(get % "claim_id"))
            set)))
```

Also assert all 36 baseline mappings have non-null dispositions and exact `resulting_claim_ids` from Appendix B.

- [ ] **Step 5: Run RED before regeneration**

```bash
cd abc
bin/kaocha --focus abc.tools.adr-evidence-inventory-test \
  --focus abc.tools.adr-test \
  --focus abc.tools.temporal-evidence-input-test
```

Expected: FAIL until ADR edits, ledger mappings, and inventory expectations agree exactly. Fix source/ledger mismatches; do not hand-edit the generated report.

- [ ] **Step 6: Regenerate inventory and run Stage A GREEN**

```bash
clojure -M:abc/adr-evidence-inventory -- \
  --output docs/reports/adr-claim-migration-inventory.json
bin/kaocha \
  --focus abc.tools.adr-test \
  --focus abc.tools.adr-evidence-inventory-test \
  --focus abc.tools.aozora-csv-test \
  --focus abc.tools.person-record-test \
  --focus abc.tools.shacl-test \
  --focus abc.tools.person-drift-test \
  --focus abc.tools.aozora-ingest-test \
  --focus abc.tools.aozora-history-audit-test
bin/kaocha --focus abc.tools.validate-design-bundle-test/validate-drift-fixtures-smoke-test
clojure -M:abc/adr-governance -- --mode audit \
  --report docs/reports/adr-evidence-migration.json
```

Expected: tests pass. Audit exits 0 and reports only expected missing-evidence problems for the 32 new family claims; it reports no malformed claim, lifecycle, ledger, dependency, or inventory problem.

- [ ] **Step 7: Commit the complete Stage A transaction**

```bash
git add docs/adr/0015-temporal-modeling.md \
        docs/adr/0016-edtf-level1-decade-century.md \
        docs/adr/0020-person-identity-drift-data-model.md \
        docs/adr/0021-person-identity-drift-harness.md \
        docs/adr/0022-upstream-ingest-drift-awareness.md \
        docs/adr/adr-claim-migration.edn \
        docs/evidence/adr-capture \
        docs/evidence/adr-inputs \
        docs/evidence/adr-entries/temporal-person-ingest.edn \
        docs/reports/adr-claim-migration-inventory.json \
        docs/reports/adr-evidence-migration.json \
        test/abc/tools/adr_evidence_inventory_test.clj \
        test/abc/tools/temporal_evidence_input_test.clj
git commit -m "docs(adr): bind temporal person ingest claims"
git status --short
```

Expected: commit succeeds and the final status is clean. Do not begin capture otherwise.

---

### Task 6: Capture immutable family evidence from the clean Stage A commit

**Files:**
- Create: eight JSON files under `abc/docs/evidence/adr-runs/` listed in Appendix C.

**Interfaces:**
- Consumes: committed descriptors and `:abc/adr-evidence-capture`.
- Produces: eight schema-valid executable bundles whose Boolean observations are true and whose input bindings are complete.

- [ ] **Step 1: Reconfirm clean-tree provenance**

```bash
git status --porcelain --untracked-files=all
git rev-parse --verify HEAD
```

Expected: first command prints nothing; record the revision shown by the second.

- [ ] **Step 2: Capture every bundle outside the repository, then install the complete set**

```bash
cd abc
tmpdir="$(mktemp -d)"
for name in \
  temporal-date-normalization \
  temporal-person-record-contract \
  temporal-person-shacl-contract \
  person-drift-contract \
  person-drift-negative-fixtures \
  aozora-ingest-drift-invariants \
  aozora-history-audit \
  aozora-history-audit-cli
do
  clojure -M:abc/adr-evidence-capture -- \
    --descriptor "docs/evidence/adr-capture/${name}.edn" \
    --output "${tmpdir}/${name}.json" || exit 1
done
mkdir -p docs/evidence/adr-runs
for bundle in "${tmpdir}"/*.json; do
  cp "$bundle" docs/evidence/adr-runs/
done
rm -rf "$tmpdir"
```

Expected: every command exits 0 and writes one temporary bundle with observation value `true`; only after all eight captures succeed are the files copied into the worktree. This is required because the first in-repository untracked output would make the tree dirty and block the second capture. If a command fails, install no bundles and add no registry entry; repair forward, recommit changed inputs/descriptors, return to a clean tree, and recapture every stale bundle owned by this plan.

- [ ] **Step 3: Validate observations and closure-artifact binding**

```bash
for f in docs/evidence/adr-runs/{temporal-*,person-drift-*,aozora-*}.json; do
  jq -e '.observations | to_entries | length == 1 and .[0].value.value == true' "$f"
done
for f in docs/evidence/adr-capture/{temporal-*,person-drift-*,aozora-*}.edn; do
  base="$(basename "$f" .edn)"
  jq -e --arg p "docs/evidence/adr-capture/${base}.edn" '.inputs[$p] != null' \
    "docs/evidence/adr-runs/${base}.json"
  jq -e --arg p "docs/evidence/adr-inputs/${base}.edn" '.inputs[$p] != null' \
    "docs/evidence/adr-runs/${base}.json"
done
```

Expected: all checks exit 0. Capture has already rejected any mismatch among
the traced runtime reads, the checked manifest, and descriptor explicit
inputs; these checks additionally prove both closure-control artifacts are
themselves bound into each immutable bundle.

---

### Task 7: Join claim evidence and close the family audit (Stage B)

**Files:**
- Modify: `abc/docs/adr/adr-evidence.edn`
- Add: `abc/docs/evidence/adr-runs/*.json`
- Use: `abc/docs/evidence/adr-entries/temporal-person-ingest.edn`
- Regenerate: `abc/docs/reports/adr-evidence-migration.json`
- Regenerate: `abc/docs/reports/adr-claim-migration-inventory.json`

**Interfaces:**
- Consumes: Appendix D's exact claim-to-boundary/type map and the captured bundle hashes.
- Produces: one valid registry entry per resulting claim and a zero-problem family audit.

- [ ] **Step 1: Materialize every Appendix D join through the registrar**

```bash
clojure -M:abc/adr-evidence-register -- \
  --entries docs/evidence/adr-entries/temporal-person-ingest.edn \
  --registry docs/adr/adr-evidence.edn
```

Expected: exit 0 with all 32 distinct family claim IDs registered. The
registrar derives the eight canonical JCS hashes and rejects a helper-test
bundle for the CLI claim, any missing observation, or any incompatible kind.
Run it twice and verify the second run leaves the registry byte-identical.

- [ ] **Step 2: Regenerate both reports**

```bash
clojure -M:abc/adr-evidence-inventory -- \
  --output docs/reports/adr-claim-migration-inventory.json
clojure -M:abc/adr-governance -- --mode audit \
  --report docs/reports/adr-evidence-migration.json
```

Expected: audit exits 0.

- [ ] **Step 3: Inspect content, because audit exit zero is not success**

```bash
jq -e --arg family '^(ADR-)?(0015|0016|0020|0021|0022)-' \
  -f nix/adr-family-clean.jq docs/reports/adr-evidence-migration.json
jq -e '
  [.criteria[]
   | select(.family == "temporal-person-ingest")
   | select(.disposition == null or .claim_id == null)]
  | length == 0' docs/reports/adr-claim-migration-inventory.json
```

Expected: both commands exit 0. A different problem replacing missing evidence is a blocker, not progress.

- [ ] **Step 4: Run final family and shared regression gates**

```bash
bin/kaocha \
  --focus abc.tools.adr-test \
  --focus abc.tools.adr-evidence-test \
  --focus abc.tools.adr-evidence-bundle-test \
  --focus abc.tools.adr-evidence-inventory-test \
  --focus abc.tools.adr-governance-test \
  --focus abc.tools.aozora-csv-test \
  --focus abc.tools.person-record-test \
  --focus abc.tools.shacl-test \
  --focus abc.tools.person-drift-test \
  --focus abc.tools.aozora-ingest-test \
  --focus abc.tools.aozora-history-audit-test
bin/kaocha --focus abc.tools.validate-design-bundle-test/validate-drift-fixtures-smoke-test
cd ..
git add abc/docs/adr/adr-evidence.edn \
  abc/docs/evidence/adr-runs \
  abc/docs/reports/adr-evidence-migration.json \
  abc/docs/reports/adr-claim-migration-inventory.json
git diff --cached --check
nix run ./abc#validate-design-bundle
system=$(nix eval --raw --impure --expr builtins.currentSystem)
nix build "./abc#checks.${system}.git-cliff-config"
nix build ".#checks.${system}.monorepo-adr-governance"
```

Expected: focused tests have zero failures/errors; all Nix commands exit 0; `adr-governance` remains audit-mode globally.

- [ ] **Step 5: Commit the complete Stage B transaction**

```bash
git diff --cached --name-only
git commit -m "docs(adr): evidence temporal person ingest claims"
git status --short
```

Expected: commit succeeds and status is clean.

---

## Appendix A: Exact corrected live criteria

### ADR 0015 (six claims)

1. `**ADR-0015-C1 — structural-invariant:**` `schemas/person-record.schema.json` admits null and the tested signed/unsigned full-date, year-month, and year lexical values, and rejects the tested out-of-scope and range-invalid lexicals; `test/abc/tools/person_record_test.clj` binds this contract.
2. `**ADR-0015-C2 — fixture-behavior:**` `abc.tools.aozora-csv/parse-date` performs the named pad-month, pad-day, pad-year, strip-whitespace, collapse-multi-dash, and normalize-date-separator cases, maps `前347` to `-0346`, maps `不詳`/`未詳` to `nil` with `unknown-marker` corrections, and leaves the tested calendar-impossible values uncorrected; `test/abc/tools/aozora_csv_test.clj` covers exactly those cases. This legacy null mapping does not claim to preserve temporal knowledge state.
3. `**ADR-0015-C3 — fixture-behavior:**` `abc.tools.person-record/record->graph` emits the tested `xsd:date`, `xsd:gYearMonth`, and `xsd:gYear` RDA literals with parallel `abc:EDTF` echoes, including the committed `examples/v0/example-persons/000879.json` fixture; `test/abc/tools/person_record_test.clj` covers the values and datatypes.
4. `**ADR-0015-C4 — structural-invariant:**` `schemas/manifest.shacl.ttl` gives the RDA birth/death properties maximum cardinality one with the three tested XSD datatype alternatives and constrains the EDTF echoes to maximum cardinality one, datatype `abc:EDTF`, and the live ADR-0016-widened lexical pattern; `test/abc/tools/shacl_test.clj` asserts that structure.
5. `**ADR-0015-C5 — fixture-behavior:**` `PersonRecordShape` rejects the tested malformed `abc:edtfDateOfBirth` literal and reports the EDTF path/shape context; `test/abc/tools/shacl_test.clj` covers the fixture.
6. `**ADR-0015-C6 — fixture-behavior:**` the committed 000879 person fixture retains full ISO birth/death values, validates, carries the live person schema hash, and emits the expected birth-date RDF view; `test/abc/tools/person_record_test.clj` covers the current fixture without making a historical rotation claim.

### ADR 0016 (four claims)

1. `**ADR-0016-C1 — structural-invariant:**` the live person-record schema accepts the tested Level 1 decade and BCE-century lexical values for birth and death; `test/abc/tools/person_record_test.clj` covers the accepted table.
2. `**ADR-0016-C2 — fixture-behavior:**` `parse-date` admits the tested decade markers, including `-019X`, verbatim without an audit correction; `test/abc/tools/aozora_csv_test.clj` covers those cases.
3. `**ADR-0016-C3 — fixture-behavior:**` `parse-date` maps the tested Japanese BCE-century prose, including `紀元前7世紀末` to `-06XX` and `紀元前6世紀初` to `-05XX`, and records the source phrase under `century-prose`; `test/abc/tools/aozora_csv_test.clj` covers those cases.
4. `**ADR-0016-C4 — fixture-behavior:**` `record->graph` emits the tested decade and BCE-century values only as `abc:EDTF` echoes and omits the RDA precision predicate; `test/abc/tools/person_record_test.clj` covers birth and death examples.

Remove the old Hard Rule criterion; the existing `## Hard Rule` section already preserves it without self-certification.

### ADR 0020 (four claims)

1. `**ADR-0020-C1 — fixture-behavior:**` drift participant snapshots carry `snapshot_id`, `person_id`, and `person_record_hash`, and the tested snapshot IRI is derived from the event-embedded hash; `test/abc/tools/person_drift_test.clj` covers the schema and derivation.
2. `**ADR-0020-C2 — structural-invariant:**` drift validation rejects duplicate snapshot IDs, predecessor/successor interleaving, wrong `pre-`/`post-` usage, and noncanonical participant/edge ordering; canonical order is part of the hashed event value; `test/abc/tools/person_drift_test.clj` covers those invariants.
3. `**ADR-0020-C3 — fixture-behavior:**` on the bounded example ingest slice, adding only the committed drift event/index sidecars leaves person bytes, metadata bytes, manifest bytes, and `manifest_identity_object` unchanged; `test/abc/tools/aozora_ingest_test.clj` performs the isolated before/after comparison.
4. `**ADR-0020-C4 — fixture-behavior:**` on the bounded example ingest slice, a synthetic rotation of only `person_record_schema_hash` changes exactly the person schema-hash field, contributor person hash, manifest metadata hash, and artifact ID while leaving semantic person/metadata fields unchanged; `test/abc/tools/aozora_ingest_test.clj` asserts the exact changed set.

Remove the Decision-completeness, vocabulary-review, and canonical-graph-presence checklist criteria; their substantive prose remains in Decision. Do not create an expert assessment merely to preserve those checklist bullets.

### ADR 0021 (twelve claims)

1. `**ADR-0021-C1 — fixture-behavior:**` the event schema is meta-schema-valid and accepts the tested split wrapper; `test/abc/tools/person_drift_test.clj` covers it.
2. `**ADR-0021-C2 — structural-invariant:**` the event schema requires the three participant fields and enforces the tested split/merge predecessor/successor cardinality bounds; `test/abc/tools/person_drift_test.clj` covers every boundary violation.
3. `**ADR-0021-C3 — structural-invariant:**` JSON coherence resolves used/generated snapshot IDs, requires complete disjoint coverage, and enforces participant and edge ordering; `test/abc/tools/person_drift_test.clj` covers the corresponding failure codes.
4. `**ADR-0021-C4 — fixture-behavior:**` JSON coherence rejects the tested disallowed role, unresolved CURIE prefix, and invalid agent IRI; `test/abc/tools/person_drift_test.clj` covers those cases.
5. `**ADR-0021-C5 — fixture-behavior:**` the drift index schema is meta-schema-valid and accepts the tested index sidecar; `test/abc/tools/person_drift_test.clj` covers it.
6. `**ADR-0021-C6 — structural-invariant:**` `manifest.shacl.ttl` contains the three named drift shapes and the three named subclass axioms; `test/abc/tools/person_drift_test.clj` asserts all six resources.
7. `**ADR-0021-C7 — fixture-behavior:**` `event->graph` materializes the tested RDF types and derives `prov:wasInvalidatedBy`, `prov:wasAssociatedWith`, and `prov:wasDerivedFrom`; `test/abc/tools/person_drift_test.clj` asserts the triples.
8. `**ADR-0021-C8 — fixture-behavior:**` the tested snapshot IRI is derived from the event participant's embedded `person_record_hash`; `test/abc/tools/person_drift_test.clj` covers the value without claiming an instrumented no-disk-read observation.
9. `**ADR-0021-C9 — fixture-behavior:**` the focused drift tests and negative fixtures catch the tested typing, JSON coherence, RDF participant/PROV mismatch, and three event/index referential-integrity failure families; `test/abc/tools/person_drift_test.clj` and `test/abc/tools/validate_design_bundle_test.clj` cover the exact codes.
10. `**ADR-0021-C10 — fixture-behavior:**` the committed example has exactly one event JSON and three participant index JSON files and validates as one event/three indexes; `test/abc/tools/person_drift_test.clj` covers the committed directory.
11. `**ADR-0021-C11 — fixture-behavior:**` the committed invalid drift fixtures produce the exact failure-code sets asserted by `validate-drift-fixtures-smoke-test`; `test/abc/tools/validate_design_bundle_test.clj` binds every fixture.
12. `**ADR-0021-C12 — fixture-behavior:**` on the bounded example ingest slice, adding only drift event/index sidecars leaves manifest bytes and `manifest_identity_object` unchanged; `test/abc/tools/aozora_ingest_test.clj` performs the isolated comparison.

Move old `nix flake check passes` text to `## Historical Evidence` only if Plan 1's immutable baseline provides a dated/revision-bound run artifact; otherwise replace it under `## Future Verification` with: “Release verification requires a fresh full-flake observation; it is not a timeless Acceptance Criterion.”

### ADR 0022 (six claims)

1. `**ADR-0022-C1 — fixture-behavior:**` the synthetic two-ref audit report includes `drift_participant_updates` and the matched event IDs when an indexed participant changes; `test/abc/tools/aozora_history_audit_test.clj` covers the integrated report.
2. `**ADR-0022-C2 — fixture-behavior:**` invalid drift sidecars abort the tested audit path before a report value is returned; `test/abc/tools/aozora_history_audit_test.clj` covers the exception.
3. `**ADR-0022-C3 — fixture-behavior:**` a complete synthetic audit report contains an empty `drift_participant_updates` list when no drift artifacts are present; `test/abc/tools/aozora_history_audit_test.clj` covers the report value.
4. `**ADR-0022-C4 — fixture-behavior:**` a generated hash change for the tested indexed participant appears in `drift_participant_updates`; `test/abc/tools/aozora_history_audit_test.clj` covers the bounded case.
5. `**ADR-0022-C5 — operational-behavior:**` with validation and candidate failures held false, `--fail-on-drift-participant-updates` makes the real audit process exit nonzero for a non-empty update list and does not make it fail for an empty list; other validation/candidate gates remain independent; `test/abc/tools/aozora_history_audit_test.clj` launches the subprocess and covers the exit matrix.
6. `**ADR-0022-C6 — fixture-behavior:**` on the bounded synthetic CSV slice, adding drift sidecars does not change the generated person record, metadata record, or refreshed manifest; `test/abc/tools/aozora_ingest_test.clj` covers raw-ingest independence without claiming broader undefined “source-faithfulness.”

Move the Nix-app statement to `## Future Verification`: “The `aozora-upstream-audit` app remains a supported wrapper, but a passing operational claim requires a direct, reproducible Nix-app observation with fixed upstream refs; wrapper source text is not that observation.”

---

## Appendix B: Baseline disposition map

Use immutable baseline hashes as ledger keys. `correct` rationales are the narrowing/split reasons in Appendix A; `move` means `:move-out-of-acceptance`.

| ADR | Original index | Disposition | Resulting claim IDs | Boundary/target |
|---:|---:|---|---|---|
| 15 | 0 | correct | `[ADR-0015-C1]` | temporal-person-record-contract |
| 15 | 1 | correct | `[ADR-0015-C2]` | temporal-date-normalization |
| 15 | 2 | correct | `[ADR-0015-C3]` | temporal-person-record-contract |
| 15 | 3 | correct (split) | `[ADR-0015-C4, ADR-0015-C5]` | temporal-person-shacl-contract |
| 15 | 4 | correct | `[ADR-0015-C6]` | temporal-person-record-contract |
| 16 | 0 | correct (split) | `[ADR-0016-C1, ADR-0016-C2]` | person-record and date-normalization boundaries |
| 16 | 1 | correct | `[ADR-0016-C3]` | temporal-date-normalization |
| 16 | 2 | correct | `[ADR-0016-C4]` | temporal-person-record-contract |
| 16 | 3 | move | `[]` | existing Hard Rule section |
| 20 | 0 | move | `[]` | Decision review checklist |
| 20 | 1 | correct | `[ADR-0020-C1]` | person-drift-contract |
| 20 | 2 | correct | `[ADR-0020-C2]` | person-drift-contract |
| 20 | 3 | move | `[]` | Decision vocabulary rationale |
| 20 | 4 | move | `[]` | Decision canonical graph |
| 20 | 5 | correct | `[ADR-0020-C3]` | aozora-ingest-drift-invariants |
| 20 | 6 | correct | `[ADR-0020-C4]` | aozora-ingest-drift-invariants |
| 21 | 0 | correct | `[ADR-0021-C1]` | person-drift-contract |
| 21 | 1 | correct | `[ADR-0021-C2]` | person-drift-contract |
| 21 | 2 | correct | `[ADR-0021-C3]` | person-drift-contract |
| 21 | 3 | correct | `[ADR-0021-C4]` | person-drift-contract |
| 21 | 4 | correct | `[ADR-0021-C5]` | person-drift-contract |
| 21 | 5 | correct | `[ADR-0021-C6]` | person-drift-contract |
| 21 | 6 | correct | `[ADR-0021-C7]` | person-drift-contract |
| 21 | 7 | correct | `[ADR-0021-C8]` | person-drift-contract |
| 21 | 8 | correct | `[ADR-0021-C9]` | person-drift-contract + negative fixtures |
| 21 | 9 | correct | `[ADR-0021-C10]` | person-drift-contract |
| 21 | 10 | correct | `[ADR-0021-C11]` | person-drift-negative-fixtures |
| 21 | 11 | move | `[]` | Historical Evidence if bounded, otherwise Future Verification |
| 21 | 12 | correct | `[ADR-0021-C12]` | aozora-ingest-drift-invariants |
| 22 | 0 | correct | `[ADR-0022-C1]` | aozora-history-audit |
| 22 | 1 | correct | `[ADR-0022-C2]` | aozora-history-audit |
| 22 | 2 | correct | `[ADR-0022-C3]` | aozora-history-audit |
| 22 | 3 | correct | `[ADR-0022-C4]` | aozora-history-audit |
| 22 | 4 | correct | `[ADR-0022-C5]` | aozora-history-audit-cli |
| 22 | 5 | move | `[]` | Future Verification |
| 22 | 6 | correct | `[ADR-0022-C6]` | aozora-ingest-drift-invariants |

---

## Appendix C: Capture boundaries and runtime-input manifests

Each row's data paths are the exact `:paths` of
`docs/evidence/adr-inputs/<boundary>.edn`. Every descriptor's `:explicit`
vector is that set plus its own path and the manifest path.

| Boundary / output basename | Command and profile root | Exact runtime-data paths |
|---|---|---|
| `temporal-date-normalization` | `bin/kaocha --focus abc.tools.aozora-csv-test`; root `abc.tools.aozora-csv-test` | empty vector; the boundary reads no repository data at runtime |
| `temporal-person-record-contract` | `bin/kaocha --focus abc.tools.person-record-test`; root `abc.tools.person-record-test` | `schemas/person-record.schema.json`, `examples/v0/example-persons/000879.json` |
| `temporal-person-shacl-contract` | `bin/kaocha --focus abc.tools.shacl-test`; root `abc.tools.shacl-test` | `schemas/manifest.shacl.ttl`, every committed RDF fixture read by that namespace |
| `person-drift-contract` | `bin/kaocha --focus abc.tools.person-drift-test`; root `abc.tools.person-drift-test` | both drift schemas, `schemas/manifest.shacl.ttl`, one event JSON and three index JSON files under the committed example |
| `person-drift-negative-fixtures` | exact focus `abc.tools.validate-design-bundle-test/validate-drift-fixtures-smoke-test`; root `abc.tools.validate-design-bundle-test` | both drift schemas, SHACL, every sorted file under `fixtures/v0/invalid/drift/` |
| `aozora-ingest-drift-invariants` | exact focus for the two new ingest tests; root `abc.tools.aozora-ingest-test` | person/metadata/manifest schemas, example manifest, one event and three indexes |
| `aozora-history-audit` | focus the four report/helper tests, not scan-history sampling tests; root `abc.tools.aozora-history-audit-test` | person/metadata/drift schemas and `schemas/manifest.shacl.ttl` |
| `aozora-history-audit-cli` | exact focus `aozora-history-audit-cli-drift-update-exit-matrix-test`; root `abc.tools.aozora-history-audit-test` | same schemas/SHACL; the subprocess entry and test fixture closure come from namespace derivation |

If `temporal-person-shacl-contract` reads broad success/failure fixtures through namespace setup, either enumerate those exact paths or focus extracted temporal-only vars in a new small namespace. Never accept accidental broad runtime under-binding.

---

## Appendix D: Exact registry joins

Every predicate is `{:operator := :value true}`.

| Claims | Claim kind | Evidence kind | Boundary |
|---|---|---|---|
| ADR-0015-C1, ADR-0016-C1 | `:structural-invariant` | `:structural-test` | temporal-person-record-contract |
| ADR-0015-C2, ADR-0016-C2, ADR-0016-C3 | `:fixture-behavior` | `:fixture-conformance` | temporal-date-normalization |
| ADR-0015-C3, ADR-0015-C6, ADR-0016-C4 | `:fixture-behavior` | `:fixture-conformance` | temporal-person-record-contract |
| ADR-0015-C4 | `:structural-invariant` | `:structural-test` | temporal-person-shacl-contract |
| ADR-0015-C5 | `:fixture-behavior` | `:fixture-conformance` | temporal-person-shacl-contract |
| ADR-0020-C1, ADR-0021-C1, ADR-0021-C4, ADR-0021-C5, ADR-0021-C7, ADR-0021-C8, ADR-0021-C10 | `:fixture-behavior` | `:fixture-conformance` | person-drift-contract |
| ADR-0020-C2, ADR-0021-C2, ADR-0021-C3, ADR-0021-C6 | `:structural-invariant` | `:structural-test` | person-drift-contract |
| ADR-0020-C3, ADR-0020-C4, ADR-0021-C12, ADR-0022-C6 | `:fixture-behavior` | `:fixture-conformance` | aozora-ingest-drift-invariants |
| ADR-0021-C9 | `:fixture-behavior` | `:fixture-conformance` | two corroborating entries: person-drift-contract and person-drift-negative-fixtures |
| ADR-0021-C11 | `:fixture-behavior` | `:fixture-conformance` | person-drift-negative-fixtures |
| ADR-0022-C1, ADR-0022-C2, ADR-0022-C3, ADR-0022-C4 | `:fixture-behavior` | `:fixture-conformance` | aozora-history-audit |
| ADR-0022-C5 | `:operational-behavior` | `:operational-observation` | aozora-history-audit-cli |

## Self-Review Checklist

- [ ] All 36 baseline rows appear once in Appendix B; resulting claims total 32.
- [ ] Appendix A claim IDs and kinds exactly match Appendix D.
- [ ] No moved text retains a claim header.
- [ ] ADR 0015 does not claim legacy `nil` preserves knowledge state.
- [ ] ADR 0016 Hard Rule is not self-certified as acceptance evidence.
- [ ] ADR 0020 checklists are preserved as Decision prose, not registry claims.
- [ ] Both drift-sidecar identity claims use the explicit before/after test.
- [ ] The schema cascade is bounded to one synthetic example-slice changed set.
- [ ] `prov:wasAssociatedWith`, duplicate/interleaved participants, edge ordering, SHACL resources, and committed counts have direct assertions.
- [ ] ADR 0022's command claim uses the subprocess bundle, not the helper bundle.
- [ ] Every descriptor binds itself and every dynamically read non-Clojure input.
- [ ] Audit report content, not audit exit status, proves the family is clean.
- [ ] No task switches enforcement or captures a corpus-wide governance bundle.
