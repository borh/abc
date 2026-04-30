# Person Drift Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Implement accepted ADR 0020 and ADR 0021: lineage-only person identity drift events for splits and merges, with JSON schemas, RDF/SHACL validation, indexes, example artifacts, and ABC-local person ID widening.

**Architecture:** Keep drift-specific logic in a new `abc.tools.person-drift` namespace. `validate-design-bundle` delegates event/index validation to that namespace, then continues to validate person and metadata records through the existing Flavor 1 path. The drift artifacts are audit sidecars: adding or editing them does not flow into `manifest_identity_object`; ABC-local ID widening is implemented and verified as a separate schema-hash cascade.

**Tech Stack:** Clojure 1.12, JSON Schema 2020-12 via existing `abc.tools.schema`/Malli bridge, RFC 8785 JCS hashing via `abc.tools.hash`, Apache Jena 5.3 SHACL via `abc.tools.shacl`, RDF prefixes from `abc.tools.rdf-prefixes`, deterministic JSON via `abc.tools.manifest/write-json-file!`.

---

## ADR Inputs

- ADR 0020 is accepted in commit `c47bffc`. Load-bearing choices: Position L, ABC-local IDs `abc-[0-9a-f]{12}`, audit-sidecar drift log, stable event IDs plus participant indexes, split/merge only.
- ADR 0021 is accepted in commit `c47bffc`. Load-bearing choices: `_events/` and `_indexes/` under `examples/v0/example-persons/`, wrapper JSON event files, JSON graph-coherence before RDF emission, data-side `rdf:type` materialization, three SHACL shapes, JSON-only index schema.
- Work directly on `main`; the user explicitly approved this.
- Do not modify unrelated dirty files. At plan time, `docs/adr/0017-vocabulary-review.md`, `.claude/`, `out/`, `result`, and several untracked plan files may already be dirty or untracked.

---

## File Structure

- Create: `schemas/person-drift-event.schema.json` - wrapper JSON schema for one split/merge event.
- Create: `schemas/person-drift-index.schema.json` - wrapper JSON schema for one per-person event index.
- Create: `src/abc/tools/person_drift.clj` - drift schema constants, event ID derivation, JSON graph-coherence checks, RDF graph emission, SHACL runner, index integrity validation.
- Create: `test/abc/tools/person_drift_test.clj` - unit tests for schemas, event IDs, graph-coherence failures, RDF emission, SHACL integration, and index integrity.
- Modify: `schemas/person-record.schema.json` - widen `person_id` to numeric or ABC-local.
- Modify: `schemas/metadata-record.schema.json` - widen contributor `person_id` to numeric or ABC-local.
- Modify: `schemas/manifest.shacl.ttl` - add `rdfs:` prefix, widen `PersonRecordShape` identifier branch, add drift vocabulary axioms and drift event shapes.
- Modify: `src/abc/tools/person_record.clj` - branch `person-iri` and `dcterms:identifier` literal generation for numeric vs ABC-local IDs.
- Modify: `src/abc/tools/validate_design_bundle.clj` - include the new schemas, call `validate-drift-events!`, and add `validate-drift-fixtures!`.
- Modify: `test/abc/tools/person_record_test.clj` - prove ABC-local records validate, emit the ABC-local IRI, and use `xsd:string` identifiers.
- Modify: `test/abc/tools/validate_design_bundle_test.clj` - smoke-test the new drift fixture runner and `:not-present` status.
- Modify: `nix/clj-nix-deps.edn` - include `abc.tools.person-drift-test` in `:abc/focused-test`.
- Create: `fixtures/v0/invalid/drift/` fixture subdirectories - invalid event/index bundles grouped by expected failure category.
- Create: one generated event JSON file under `examples/v0/example-persons/_events/` - one fictional split event generated in Task 8 with its content-derived filename.
- Create: `examples/v0/example-persons/_indexes/000879.json`, `examples/v0/example-persons/_indexes/abc-000000000001.json`, `examples/v0/example-persons/_indexes/abc-000000000002.json` - indexes for the fictional split.

---

## Task 1: Add Drift JSON Schemas

**Files:**
- Create: `schemas/person-drift-event.schema.json`
- Create: `schemas/person-drift-index.schema.json`
- Modify: `src/abc/tools/validate_design_bundle.clj`
- Test: `test/abc/tools/person_drift_test.clj`

- [ ] **Step 1: Create the failing schema tests**

Create `test/abc/tools/person_drift_test.clj` with the namespace and shared fixtures below. The tests call functions that do not exist yet so this step must fail.

```clojure
(ns abc.tools.person-drift-test
  (:require [arachne.aristotle :as aa]
            [abc.tools.files :as files]
            [abc.tools.malli :as am]
            [abc.tools.manifest :as manifest]
            [abc.tools.person-drift :as drift]
            [abc.tools.schema :as schema]
            [clojure.test :refer [deftest is testing use-fixtures]]))

(use-fixtures :once (fn [f] (am/install!) (f)))

(defn example-hash [suffix]
  (files/example-hash suffix))

(defn base-split-without-id []
  {"schema_id" drift/event-schema-id
   "schema_hash" (manifest/schema-hash drift/event-schema-path)
   "drift_event_type" "split"
   "date" "2026-04-30"
   "participants" [{"snapshot_id" "post-abc-000000000001"
                    "person_id" "abc-000000000001"
                    "person_record_hash" (example-hash "01")}
                   {"snapshot_id" "post-abc-000000000002"
                    "person_id" "abc-000000000002"
                    "person_record_hash" (example-hash "02")}
                   {"snapshot_id" "pre-000879"
                    "person_id" "000879"
                    "person_record_hash" (example-hash "03")}]
   "evidence" ["https://example.org/abc/drift-evidence/fictional-split-2026"]
   "prov" {"used" ["pre-000879"]
           "was_generated_by" ["post-abc-000000000001"
                               "post-abc-000000000002"]
           "qualified_association" {"agent" "https://w3id.org/abc/agents/editorial-board"
                                    "had_role" "abc:DriftEditor"}}})

(defn base-split []
  (drift/materialize-event-id (base-split-without-id)))

(defn index-for [person-id event-id]
  {"schema_id" drift/index-schema-id
   "schema_hash" (manifest/schema-hash drift/index-schema-path)
   "person_id" person-id
   "drift_event_ids" [event-id]})

(deftest drift-json-schemas-are-valid-test
  (is (nil? (schema/schema-valid!
             (files/read-json drift/event-schema-path)
             drift/event-schema-path)))
  (is (nil? (schema/schema-valid!
             (files/read-json drift/index-schema-path)
             drift/index-schema-path))))

(deftest event-schema-accepts-valid-split-test
  (is (nil? (schema/validation-errors
             (files/read-json drift/event-schema-path)
             (base-split)))))

(deftest event-schema-rejects-bad-cardinality-test
  (let [bad (assoc-in (base-split)
                      ["prov" "was_generated_by"]
                      ["post-abc-000000000001"])]
    (is (thrown? clojure.lang.ExceptionInfo
                 (when-let [errors (schema/validation-errors
                                     (files/read-json drift/event-schema-path)
                                     bad)]
                   (throw (ex-info "expected schema failure" {:errors errors})))))))

(deftest index-schema-accepts-valid-index-test
  (let [event-id (get (base-split) "drift_event_id")]
    (is (nil? (schema/validation-errors
               (files/read-json drift/index-schema-path)
               (index-for "000879" event-id))))))
```

- [ ] **Step 2: Run the failing test**

Run:

```bash
clojure -M:test -e "(require 'abc.tools.person-drift-test)"
```

Expected: FAIL because `abc.tools.person-drift` does not exist.

- [ ] **Step 3: Add schema constants and minimal ID materialization**

Create `src/abc/tools/person_drift.clj`:

```clojure
(ns abc.tools.person-drift
  (:require [abc.tools.hash :as hash]))

(def event-schema-path "schemas/person-drift-event.schema.json")
(def event-schema-id "https://w3id.org/abc/schemas/person-drift-event.schema.json")

(def index-schema-path "schemas/person-drift-index.schema.json")
(def index-schema-id "https://w3id.org/abc/schemas/person-drift-index.schema.json")

(def person-id-pattern #"^([0-9]{6}|abc-[0-9a-f]{12})$")
(def hash-pattern #"^sha256:[0-9a-f]{64}$")

(defn drift-event-id [event]
  (hash/format-sha256
   (hash/sha256-json-jcs (dissoc event "drift_event_id"))))

(defn materialize-event-id [event]
  (assoc event "drift_event_id" (drift-event-id event)))
```

- [ ] **Step 4: Add `person-drift-event.schema.json`**

Create `schemas/person-drift-event.schema.json` with this contract:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://w3id.org/abc/schemas/person-drift-event.schema.json",
  "title": "ABC Person Drift Event",
  "type": "object",
  "additionalProperties": false,
  "required": [
    "schema_id",
    "schema_hash",
    "drift_event_id",
    "drift_event_type",
    "date",
    "participants",
    "evidence",
    "prov"
  ],
  "properties": {
    "schema_id": {
      "const": "https://w3id.org/abc/schemas/person-drift-event.schema.json"
    },
    "schema_hash": { "$ref": "#/$defs/hash" },
    "drift_event_id": { "$ref": "#/$defs/hash" },
    "drift_event_type": { "type": "string", "enum": ["split", "merge"] },
    "date": { "type": "string", "format": "date" },
    "participants": {
      "type": "array",
      "minItems": 2,
      "items": { "$ref": "#/$defs/participant" }
    },
    "evidence": {
      "type": "array",
      "minItems": 1,
      "uniqueItems": true,
      "items": { "type": "string", "format": "uri" }
    },
    "prov": { "$ref": "#/$defs/prov" }
  },
  "allOf": [
    {
      "if": {
        "properties": { "drift_event_type": { "const": "split" } },
        "required": ["drift_event_type"]
      },
      "then": {
        "properties": {
          "prov": {
            "properties": {
              "used": { "minItems": 1, "maxItems": 1 },
              "was_generated_by": { "minItems": 2 }
            }
          }
        }
      }
    },
    {
      "if": {
        "properties": { "drift_event_type": { "const": "merge" } },
        "required": ["drift_event_type"]
      },
      "then": {
        "properties": {
          "prov": {
            "properties": {
              "used": { "minItems": 2 },
              "was_generated_by": { "minItems": 1, "maxItems": 1 }
            }
          }
        }
      }
    }
  ],
  "$defs": {
    "hash": {
      "type": "string",
      "pattern": "^sha256:[0-9a-f]{64}$"
    },
    "personId": {
      "type": "string",
      "pattern": "^([0-9]{6}|abc-[0-9a-f]{12})$"
    },
    "snapshotId": {
      "type": "string",
      "pattern": "^(pre|post)-([0-9]{6}|abc-[0-9a-f]{12})$"
    },
    "participant": {
      "type": "object",
      "additionalProperties": false,
      "required": ["snapshot_id", "person_id", "person_record_hash"],
      "properties": {
        "snapshot_id": { "$ref": "#/$defs/snapshotId" },
        "person_id": { "$ref": "#/$defs/personId" },
        "person_record_hash": { "$ref": "#/$defs/hash" }
      }
    },
    "prov": {
      "type": "object",
      "additionalProperties": false,
      "required": ["used", "was_generated_by", "qualified_association"],
      "properties": {
        "used": {
          "type": "array",
          "items": { "$ref": "#/$defs/snapshotId" },
          "uniqueItems": true
        },
        "was_generated_by": {
          "type": "array",
          "items": { "$ref": "#/$defs/snapshotId" },
          "uniqueItems": true
        },
        "qualified_association": {
          "type": "object",
          "additionalProperties": false,
          "required": ["agent", "had_role"],
          "properties": {
            "agent": { "type": "string", "format": "uri" },
            "had_role": { "const": "abc:DriftEditor" }
          }
        }
      }
    }
  }
}
```

- [ ] **Step 5: Add `person-drift-index.schema.json`**

Create `schemas/person-drift-index.schema.json`:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://w3id.org/abc/schemas/person-drift-index.schema.json",
  "title": "ABC Person Drift Index",
  "type": "object",
  "additionalProperties": false,
  "required": [
    "schema_id",
    "schema_hash",
    "person_id",
    "drift_event_ids"
  ],
  "properties": {
    "schema_id": {
      "const": "https://w3id.org/abc/schemas/person-drift-index.schema.json"
    },
    "schema_hash": { "$ref": "#/$defs/hash" },
    "person_id": { "$ref": "#/$defs/personId" },
    "drift_event_ids": {
      "type": "array",
      "minItems": 1,
      "uniqueItems": true,
      "items": { "$ref": "#/$defs/hash" }
    }
  },
  "$defs": {
    "hash": {
      "type": "string",
      "pattern": "^sha256:[0-9a-f]{64}$"
    },
    "personId": {
      "type": "string",
      "pattern": "^([0-9]{6}|abc-[0-9a-f]{12})$"
    }
  }
}
```

- [ ] **Step 6: Wire schemas into the existing schema pass**

In `src/abc/tools/validate_design_bundle.clj`, extend `validate-json-schemas!`:

```clojure
(let [manifest-schema (files/read-json "schemas/manifest.schema.json")
      parser-ir-schema (files/read-json "schemas/parser-ir.schema.json")
      diagnostic-schema (files/read-json "schemas/diagnostic.schema.json")
      run-summary-schema (files/read-json "schemas/run-summary.schema.json")
      manifest-inputs-schema (files/read-json "schemas/manifest-inputs.schema.json")
      comparison-report-schema (files/read-json "schemas/comparison-report.schema.json")
      tei-validation-result-schema (files/read-json "schemas/tei-validation-result.schema.json")
      iiif-applicability-schema (files/read-json "schemas/iiif-applicability.schema.json")
      person-drift-event-schema (files/read-json "schemas/person-drift-event.schema.json")
      person-drift-index-schema (files/read-json "schemas/person-drift-index.schema.json")]
  (doseq [[path schema] [["schemas/manifest.schema.json" manifest-schema]
                         ["schemas/parser-ir.schema.json" parser-ir-schema]
                         ["schemas/diagnostic.schema.json" diagnostic-schema]
                         ["schemas/run-summary.schema.json" run-summary-schema]
                         ["schemas/manifest-inputs.schema.json" manifest-inputs-schema]
                         ["schemas/comparison-report.schema.json" comparison-report-schema]
                         ["schemas/tei-validation-result.schema.json" tei-validation-result-schema]
                         ["schemas/iiif-applicability.schema.json" iiif-applicability-schema]
                         ["schemas/person-drift-event.schema.json" person-drift-event-schema]
                         ["schemas/person-drift-index.schema.json" person-drift-index-schema]]]
    (schema-valid! schema path))
```

After this `schema-valid!` loop, leave the existing example-manifest,
parser-output, JSONL, manifest-inputs, comparison-report,
TEI-validation-result, TEI sidecar, and empty-object negative checks in their
current order.

- [ ] **Step 7: Run the schema tests**

Run:

```bash
clojure -M:test -e "(require 'abc.tools.person-drift-test) (do ((requiring-resolve 'abc.tools.malli/install!)) nil) (clojure.test/run-tests 'abc.tools.person-drift-test)"
```

Expected: PASS for the three schema tests created in Step 1.

- [ ] **Step 8: Commit**

```bash
git add schemas/person-drift-event.schema.json schemas/person-drift-index.schema.json src/abc/tools/person_drift.clj test/abc/tools/person_drift_test.clj src/abc/tools/validate_design_bundle.clj
git commit -m "feat(person-drift): add drift event and index schemas"
```

---

## Task 2: Implement Event IDs and JSON Graph-Coherence Validation

**Files:**
- Modify: `src/abc/tools/person_drift.clj`
- Modify: `test/abc/tools/person_drift_test.clj`

- [ ] **Step 1: Add failing unit tests for the ADR 0020 ID protocol**

Append tests:

```clojure
(deftest drift-event-id-omits-only-id-field-test
  (let [without-id (base-split-without-id)
        with-id (drift/materialize-event-id without-id)
        rederived (drift/drift-event-id with-id)
        changed-date (assoc with-id "date" "2026-05-01")]
    (is (= (get with-id "drift_event_id") rederived))
    (is (not= (get with-id "drift_event_id")
              (drift/drift-event-id changed-date)))))

(deftest validate-event-json-coherence-accepts-base-split-test
  (is (= [] (drift/event-json-coherence-failures (base-split)))))

(deftest validate-event-json-coherence-rejects-unsorted-participants-test
  (let [event (base-split)
        bad (assoc event "participants" (vec (reverse (get event "participants"))))]
    (is (= [:participants-not-sorted]
           (mapv :code (drift/event-json-coherence-failures bad))))))

(deftest validate-event-json-coherence-rejects-dangling-snapshot-ref-test
  (let [bad (assoc-in (base-split) ["prov" "used"] ["pre-missing"])]
    (is (some #{:unknown-snapshot-reference}
              (map :code (drift/event-json-coherence-failures bad))))))

(deftest validate-event-json-coherence-rejects-uncovered-participant-test
  (let [bad (update (base-split) "participants" conj
                    {"snapshot_id" "post-abc-000000000003"
                     "person_id" "abc-000000000003"
                     "person_record_hash" (example-hash "04")})]
    (is (some #{:participant-not-covered}
              (map :code (drift/event-json-coherence-failures bad))))))

(deftest validate-event-json-coherence-rejects-prefix-usage-mismatch-test
  (let [bad (-> (base-split)
                (assoc-in ["prov" "used"] ["post-abc-000000000001"])
                (assoc-in ["prov" "was_generated_by"] ["post-abc-000000000002"
                                                        "pre-000879"]))]
    (is (some #{:snapshot-prefix-usage-mismatch}
              (map :code (drift/event-json-coherence-failures bad))))))

(deftest validate-event-json-coherence-rejects-invalid-agent-iri-test
  (let [bad (assoc-in (base-split)
                      ["prov" "qualified_association" "agent"]
                      "not an iri")]
    (is (some #{:invalid-agent-iri}
              (map :code (drift/event-json-coherence-failures bad))))))
```

- [ ] **Step 2: Run the failing tests**

Run:

```bash
clojure -M:test -e "(require 'abc.tools.person-drift-test) (do ((requiring-resolve 'abc.tools.malli/install!)) nil) (clojure.test/run-tests 'abc.tools.person-drift-test)"
```

Expected: FAIL because `event-json-coherence-failures` is not implemented.

- [ ] **Step 3: Implement JSON graph-coherence checks**

Add these functions to `src/abc/tools/person_drift.clj`. Keep failures as maps with a stable `:code` so fixture assertions can target failure categories rather than wording.

```clojure
(def drift-editor-curie "abc:DriftEditor")

(defn- sorted-lex? [xs]
  (= (vec xs) (vec (sort xs))))

(defn- duplicate-values [xs]
  (->> xs frequencies (filter (fn [[_ n]] (> n 1))) (mapv first)))

(defn- participant-ids [event]
  (mapv #(get % "snapshot_id") (get event "participants")))

(defn- used-ids [event]
  (get-in event ["prov" "used"]))

(defn- generated-ids [event]
  (get-in event ["prov" "was_generated_by"]))

(defn- valid-iri? [s]
  (try
    (let [uri (java.net.URI. s)]
      (and (.isAbsolute uri) (some? (.getScheme uri))))
    (catch Exception _ false)))

(defn event-json-coherence-failures [event]
  (let [participants (get event "participants")
        snapshot-ids (participant-ids event)
        snapshot-set (set snapshot-ids)
        used (vec (used-ids event))
        generated (vec (generated-ids event))
        used-set (set used)
        generated-set (set generated)
        referenced (set (concat used generated))
        participant-set (set snapshot-ids)
        role (get-in event ["prov" "qualified_association" "had_role"])
        agent (get-in event ["prov" "qualified_association" "agent"])]
    (vec
     (concat
      (when-not (sorted-lex? snapshot-ids)
        [{:code :participants-not-sorted
          :snapshot_ids snapshot-ids}])
      (when-not (sorted-lex? used)
        [{:code :used-not-sorted
          :snapshot_ids used}])
      (when-not (sorted-lex? generated)
        [{:code :generated-not-sorted
          :snapshot_ids generated}])
      (map (fn [id] {:code :duplicate-snapshot-id :snapshot_id id})
           (duplicate-values snapshot-ids))
      (map (fn [id] {:code :unknown-snapshot-reference :snapshot_id id})
           (sort (remove snapshot-set referenced)))
      (map (fn [id] {:code :participant-not-covered :snapshot_id id})
           (sort (clojure.set/difference participant-set referenced)))
      (map (fn [id] {:code :participant-in-both-used-and-generated :snapshot_id id})
           (sort (clojure.set/intersection used-set generated-set)))
      (map (fn [id] {:code :snapshot-prefix-usage-mismatch :snapshot_id id :usage "used"})
           (remove #(clojure.string/starts-with? % "pre-") used))
      (map (fn [id] {:code :snapshot-prefix-usage-mismatch :snapshot_id id :usage "was_generated_by"})
           (remove #(clojure.string/starts-with? % "post-") generated))
      (when-not (= drift-editor-curie role)
        [{:code :invalid-had-role :had_role role}])
      (when-not (valid-iri? agent)
        [{:code :invalid-agent-iri :agent agent}])))))

(defn assert-event-json-coherent! [event]
  (let [failures (event-json-coherence-failures event)]
    (when (seq failures)
      (throw (ex-info "person drift event JSON graph-coherence failed"
                      {:failures failures}))))
  :ok)
```

Add `clojure.set` and `clojure.string` to the namespace requires.

- [ ] **Step 4: Run the tests**

Run:

```bash
clojure -M:test -e "(require 'abc.tools.person-drift-test) (do ((requiring-resolve 'abc.tools.malli/install!)) nil) (clojure.test/run-tests 'abc.tools.person-drift-test)"
```

Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/abc/tools/person_drift.clj test/abc/tools/person_drift_test.clj
git commit -m "feat(person-drift): validate drift event JSON coherence"
```

---

## Task 3: Emit Drift RDF Graphs

**Files:**
- Modify: `src/abc/tools/person_drift.clj`
- Modify: `test/abc/tools/person_drift_test.clj`

- [ ] **Step 1: Add failing RDF emission tests**

Append:

```clojure
(defn triples [graph]
  (iterator-seq (.find graph)))

(defn triple-uris [graph]
  (set (map (fn [triple]
              [(when (.isURI (.getSubject triple))
                 (.getURI (.getSubject triple)))
               (.getURI (.getPredicate triple))
               (cond
                 (.isURI (.getObject triple)) (.getURI (.getObject triple))
                 (.isLiteral (.getObject triple)) (.getLiteralLexicalForm (.getObject triple))
                 :else (str (.getObject triple)))])
            (triples graph))))

(deftest snapshot-iri-uses-event-embedded-hash-test
  (is (= "https://w3id.org/abc/persons/000879#snapshot-000000000000"
         (drift/snapshot-iri {"snapshot_id" "pre-000879"
                              "person_id" "000879"
                              "person_record_hash" (example-hash "03")}))))

(deftest event->graph-materializes-types-and-derived-prov-test
  (let [event (base-split)
        graph (drift/event->graph event)
        triples (triple-uris graph)
        event-iri (drift/event-iri (get event "drift_event_id"))
        pre "https://w3id.org/abc/persons/000879#snapshot-000000000000"
        post-a "https://w3id.org/abc/persons/abc-000000000001#snapshot-000000000000"
        rdf-type "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"]
    (is (contains? triples [event-iri rdf-type "https://w3id.org/abc/DriftEvent"]))
    (is (contains? triples [event-iri rdf-type "http://www.w3.org/ns/prov#Activity"]))
    (is (contains? triples [event-iri rdf-type "https://w3id.org/abc/DriftSplitEvent"]))
    (is (contains? triples [event-iri "https://w3id.org/abc/driftEventType" "split"]))
    (is (contains? triples [event-iri "http://www.w3.org/ns/prov#used" pre]))
    (is (contains? triples [post-a "http://www.w3.org/ns/prov#wasGeneratedBy" event-iri]))
    (is (contains? triples [pre "http://www.w3.org/ns/prov#wasInvalidatedBy" event-iri]))
    (is (contains? triples [post-a "http://www.w3.org/ns/prov#wasDerivedFrom" pre]))
    (is (contains? triples [pre "http://www.w3.org/ns/prov#specializationOf"
                            "http://www.aozora.gr.jp/index_pages/person000879.html"]))))
```

- [ ] **Step 2: Run the failing RDF tests**

Run:

```bash
clojure -M:test -e "(require 'abc.tools.person-drift-test) (do ((requiring-resolve 'abc.tools.malli/install!)) nil) (clojure.test/run-tests 'abc.tools.person-drift-test)"
```

Expected: FAIL because `snapshot-iri`, `event-iri`, and `event->graph` do not exist.

- [ ] **Step 3: Implement graph emission**

Extend `src/abc/tools/person_drift.clj`:

```clojure
(ns abc.tools.person-drift
  (:require [abc.tools.hash :as hash]
            [abc.tools.person-record :as person-record]
            [abc.tools.rdf-prefixes :as rdf-prefixes]
            [arachne.aristotle :as aa]
            [clojure.set :as set]
            [clojure.string :as string])
  (:import [org.apache.jena.datatypes.xsd XSDDatatype]
           [org.apache.jena.graph NodeFactory Triple]))

(def rdf-type-uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
(def abc-base "https://w3id.org/abc/")
(def prov-base "http://www.w3.org/ns/prov#")
(def dcterms-base "http://purl.org/dc/terms/")

(defn- uri [s]
  (NodeFactory/createURI s))

(defn- literal [s]
  (NodeFactory/createLiteral ^String s))

(defn- date-literal [s]
  (NodeFactory/createLiteral ^String s XSDDatatype/XSDdate))

(defn- add-triple! [graph s p o]
  (.add graph (Triple/create s p o)))

(defn event-iri [drift-event-id]
  (let [hex (subs drift-event-id (count "sha256:"))]
    (str abc-base "person-drift-events/" hex)))

(defn- hash-short [person-record-hash]
  (subs person-record-hash (count "sha256:") (+ (count "sha256:") 12)))

(defn snapshot-iri [participant]
  (str (if (re-matches #"^[0-9]{6}$" (get participant "person_id"))
         (person-record/person-iri (get participant "person_id"))
         (str abc-base "persons/" (get participant "person_id")))
       "#snapshot-"
       (hash-short (get participant "person_record_hash"))))

(defn- participant-by-id [event]
  (into {} (map (juxt #(get % "snapshot_id") identity)
                (get event "participants"))))

(defn- subclass-type-uri [event-type]
  (case event-type
    "split" (str abc-base "DriftSplitEvent")
    "merge" (str abc-base "DriftMergeEvent")))

(defn event->graph [event]
  (rdf-prefixes/ensure!)
  (assert-event-json-coherent! event)
  (let [graph (aa/graph :simple)
        by-id (participant-by-id event)
        event-node (uri (event-iri (get event "drift_event_id")))
        used (get-in event ["prov" "used"])
        generated (get-in event ["prov" "was_generated_by"])
        agent (uri (get-in event ["prov" "qualified_association" "agent"]))
        assoc-node (NodeFactory/createBlankNode)]
    (doseq [type-uri [(str abc-base "DriftEvent")
                      (str prov-base "Activity")
                      (subclass-type-uri (get event "drift_event_type"))]]
      (add-triple! graph event-node (uri rdf-type-uri) (uri type-uri)))
    (add-triple! graph event-node (uri (str abc-base "driftEventType"))
                 (literal (get event "drift_event_type")))
    (add-triple! graph event-node (uri (str dcterms-base "date"))
                 (date-literal (get event "date")))
    (doseq [evidence (get event "evidence")]
      (add-triple! graph event-node (uri (str abc-base "driftEvidence")) (uri evidence)))
    (doseq [participant (get event "participants")]
      (let [snapshot-node (uri (snapshot-iri participant))
            person-node (uri (if (re-matches #"^[0-9]{6}$" (get participant "person_id"))
                               (person-record/person-iri (get participant "person_id"))
                               (str abc-base "persons/" (get participant "person_id"))))]
        (add-triple! graph snapshot-node (uri rdf-type-uri) (uri (str prov-base "Entity")))
        (add-triple! graph snapshot-node (uri (str prov-base "specializationOf")) person-node)))
    (doseq [snapshot-id used]
      (let [snapshot-node (uri (snapshot-iri (get by-id snapshot-id)))]
        (add-triple! graph event-node (uri (str prov-base "used")) snapshot-node)
        (add-triple! graph snapshot-node (uri (str prov-base "wasInvalidatedBy")) event-node)))
    (doseq [snapshot-id generated]
      (let [snapshot-node (uri (snapshot-iri (get by-id snapshot-id)))]
        (add-triple! graph snapshot-node (uri (str prov-base "wasGeneratedBy")) event-node)
        (doseq [used-id used]
          (add-triple! graph snapshot-node
                       (uri (str prov-base "wasDerivedFrom"))
                       (uri (snapshot-iri (get by-id used-id)))))))
    (add-triple! graph event-node (uri (str prov-base "wasAssociatedWith")) agent)
    (add-triple! graph event-node (uri (str prov-base "qualifiedAssociation")) assoc-node)
    (add-triple! graph assoc-node (uri rdf-type-uri) (uri (str prov-base "Association")))
    (add-triple! graph assoc-node (uri (str prov-base "agent")) agent)
    (add-triple! graph assoc-node (uri (str prov-base "hadRole")) (uri (str abc-base "DriftEditor")))
    graph))
```

- [ ] **Step 4: Run the RDF tests**

Run:

```bash
clojure -M:test -e "(require 'abc.tools.person-drift-test) (do ((requiring-resolve 'abc.tools.malli/install!)) nil) (clojure.test/run-tests 'abc.tools.person-drift-test)"
```

Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/abc/tools/person_drift.clj test/abc/tools/person_drift_test.clj
git commit -m "feat(person-drift): emit drift event RDF graphs"
```

---

## Task 4: Add SHACL Drift Shapes and Typing Coherence

**Files:**
- Modify: `schemas/manifest.shacl.ttl`
- Modify: `src/abc/tools/person_drift.clj`
- Modify: `test/abc/tools/person_drift_test.clj`

- [ ] **Step 1: Add failing SHACL tests**

Append:

```clojure
(deftest drift-event-shacl-accepts-emitted-split-test
  (let [graph (drift/event->graph (base-split))]
    (is (= :ok (drift/validate-event-shacl! graph "base split")))))

(deftest typing-coherence-accepts-emitted-types-test
  (let [event (base-split)
        graph (drift/event->graph event)
        failures (drift/typing-coherence-failures event graph)]
    (is (= [] failures))))

(deftest typing-coherence-rejects-missing-types-test
  (let [event (base-split)
        graph (aa/graph :simple)]
    (is (some #{:missing-rdf-type}
              (map :code (drift/typing-coherence-failures event graph))))))
```

- [ ] **Step 2: Run the failing tests**

Run:

```bash
clojure -M:test -e "(require 'abc.tools.person-drift-test) (do ((requiring-resolve 'abc.tools.malli/install!)) nil) (clojure.test/run-tests 'abc.tools.person-drift-test)"
```

Expected: FAIL because SHACL helpers do not exist and the shapes file does not contain drift shapes.

- [ ] **Step 3: Add SHACL vocabulary and event shapes**

Modify `schemas/manifest.shacl.ttl`:

1. Add the missing prefix near the existing prefixes:

```turtle
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
```

2. Replace the `PersonRecordShape` `dcterms:identifier` property with the ADR 0020 widening:

```turtle
  sh:property [
    sh:path dcterms:identifier ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
    sh:or (
      [ sh:datatype xsd:int ; sh:pattern "^[0-9]{6}$" ]
      [ sh:datatype xsd:string ; sh:pattern "^abc-[0-9a-f]{12}$" ]
    )
  ] ;
```

3. Add drift vocabulary axioms after `abc:ActivityShape`:

```turtle
abc:DriftEvent
  rdfs:subClassOf prov:Activity .

abc:DriftSplitEvent
  rdfs:subClassOf abc:DriftEvent .

abc:DriftMergeEvent
  rdfs:subClassOf abc:DriftEvent .
```

4. Add the drift shapes after the axioms:

```turtle
abc:PersonDriftEventShape
  a sh:NodeShape ;
  sh:targetClass abc:DriftEvent ;
  sh:property [
    sh:path rdf:type ;
    sh:hasValue prov:Activity
  ] ;
  sh:property [
    sh:path abc:driftEventType ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
    sh:in ("split" "merge")
  ] ;
  sh:property [
    sh:path dcterms:date ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
    sh:datatype xsd:date
  ] ;
  sh:property [
    sh:path abc:driftEvidence ;
    sh:minCount 1 ;
    sh:nodeKind sh:IRI
  ] .

abc:PersonDriftSplitEventShape
  a sh:NodeShape ;
  sh:targetClass abc:DriftSplitEvent ;
  sh:property [
    sh:path abc:driftEventType ;
    sh:hasValue "split"
  ] ;
  sh:property [
    sh:path prov:used ;
    sh:minCount 1 ;
    sh:maxCount 1
  ] ;
  sh:property [
    sh:path [ sh:inversePath prov:wasGeneratedBy ] ;
    sh:minCount 2
  ] .

abc:PersonDriftMergeEventShape
  a sh:NodeShape ;
  sh:targetClass abc:DriftMergeEvent ;
  sh:property [
    sh:path abc:driftEventType ;
    sh:hasValue "merge"
  ] ;
  sh:property [
    sh:path prov:used ;
    sh:minCount 2
  ] ;
  sh:property [
    sh:path [ sh:inversePath prov:wasGeneratedBy ] ;
    sh:minCount 1 ;
    sh:maxCount 1
  ] .
```

- [ ] **Step 4: Implement SHACL and typing-coherence helpers**

In `src/abc/tools/person_drift.clj`, require `abc.tools.shacl` and add:

```clojure
(defn- type-uris [graph event-node-uri]
  (let [node (uri event-node-uri)]
    (->> (iterator-seq (.find graph node (uri rdf-type-uri) nil))
         (map #(.getURI (.getObject %)))
         set)))

(defn expected-type-uris [event]
  #{(str abc-base "DriftEvent")
    (str prov-base "Activity")
    (subclass-type-uri (get event "drift_event_type"))})

(defn typing-coherence-failures [event graph]
  (let [actual (type-uris graph (event-iri (get event "drift_event_id")))
        expected (expected-type-uris event)]
    (vec
     (concat
      (map (fn [type-uri] {:code :missing-rdf-type :type type-uri})
           (sort (set/difference expected actual)))
      (map (fn [type-uri] {:code :unexpected-rdf-type :type type-uri})
           (sort (set/difference actual expected)))))))

(defn assert-typing-coherent! [event graph]
  (let [failures (typing-coherence-failures event graph)]
    (when (seq failures)
      (throw (ex-info "person drift event RDF typing coherence failed"
                      {:failures failures}))))
  :ok)

(defn validate-event-shacl! [graph label]
  (shacl/validate! {:shapes-graph (shacl/load-shapes-graph)
                    :data-graph graph
                    :label label}))
```

- [ ] **Step 5: Run focused tests**

Run:

```bash
clojure -M:test -e "(require 'abc.tools.person-drift-test) (do ((requiring-resolve 'abc.tools.malli/install!)) nil) (clojure.test/run-tests 'abc.tools.person-drift-test)"
```

Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add schemas/manifest.shacl.ttl src/abc/tools/person_drift.clj test/abc/tools/person_drift_test.clj
git commit -m "feat(person-drift): add SHACL shapes for drift events"
```

---

## Task 5: Implement Drift Event and Index Validation

**Files:**
- Modify: `src/abc/tools/person_drift.clj`
- Modify: `test/abc/tools/person_drift_test.clj`

- [ ] **Step 1: Add failing validation pipeline tests**

Append:

```clojure
(defn with-temp-dir [f]
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "abc-person-drift-test"
                      (make-array java.nio.file.attribute.FileAttribute 0)))]
    (try
      (f dir)
      (finally
        (doseq [file (reverse (file-seq dir))]
          (.delete file))))))

(defn write-json! [path value]
  (manifest/write-json-file! path value))

(deftest validate-drift-events-not-present-test
  (with-temp-dir
    (fn [dir]
      (is (= {:status :not-present}
             (drift/validate-drift-events! {:persons-dir (str dir)}))))))

(deftest validate-drift-events-ok-test
  (with-temp-dir
    (fn [dir]
      (let [event (base-split)
            event-id (get event "drift_event_id")
            events-dir (java.io.File. dir "_events")
            indexes-dir (java.io.File. dir "_indexes")]
        (.mkdirs events-dir)
        (.mkdirs indexes-dir)
        (write-json! (java.io.File. events-dir (str event-id ".json")) event)
        (doseq [person-id ["000879" "abc-000000000001" "abc-000000000002"]]
          (write-json! (java.io.File. indexes-dir (str person-id ".json"))
                       (index-for person-id event-id)))
        (is (= {:status :ok :events 1 :indexes 3}
               (drift/validate-drift-events! {:persons-dir (str dir)})))))))

(deftest validate-drift-events-rejects-broken-index-target-test
  (with-temp-dir
    (fn [dir]
      (let [indexes-dir (java.io.File. dir "_indexes")]
        (.mkdirs indexes-dir)
        (write-json! (java.io.File. indexes-dir "000879.json")
                     (index-for "000879" (example-hash "09")))
        (let [result (drift/validate-drift-events! {:persons-dir (str dir)})]
          (is (= :error (:status result)))
          (is (some #{:index-target-missing}
                    (map :code (:failures result)))))))))
```

The test namespace already aliases `abc.tools.manifest` as `manifest` from
Task 1.

- [ ] **Step 2: Run the failing tests**

Run:

```bash
clojure -M:test -e "(require 'abc.tools.person-drift-test) (do ((requiring-resolve 'abc.tools.malli/install!)) nil) (clojure.test/run-tests 'abc.tools.person-drift-test)"
```

Expected: FAIL because `validate-drift-events!` does not exist.

- [ ] **Step 3: Implement directory loading and schema validation**

In `src/abc/tools/person_drift.clj`, require `abc.tools.files`, `abc.tools.manifest`, `abc.tools.schema`, and `clojure.java.io`. Add:

```clojure
(defn- drift-dir [persons-dir dirname]
  (java.io.File. (io/file persons-dir) dirname))

(defn- json-files [dir]
  (if (.isDirectory dir)
    (->> (.listFiles dir)
         (filter #(and (.isFile %) (string/ends-with? (.getName %) ".json")))
         sort
         vec)
    []))

(defn- expected-schema-hash [schema-path]
  (manifest/schema-hash schema-path))

(defn- schema-hash-failure [artifact-kind path expected actual]
  {:code :schema-hash-mismatch
   :artifact_kind artifact-kind
   :path (str path)
   :expected expected
   :actual actual})

(defn- load-event-file [file]
  (let [event (files/read-json (str file))
        event-schema (files/read-json event-schema-path)
        live-hash (expected-schema-hash event-schema-path)]
    (when-let [errors (schema/validation-errors event-schema event)]
      (throw (ex-info (str "JSON Schema validation failed: " file)
                      {:path (str file)
                       :errors errors})))
    {:path (str file)
     :value event
     :schema-failures (when-not (= live-hash (get event "schema_hash"))
                        [(schema-hash-failure :event file live-hash (get event "schema_hash"))])}))

(defn- load-index-file [file]
  (let [index (files/read-json (str file))
        index-schema (files/read-json index-schema-path)
        live-hash (expected-schema-hash index-schema-path)]
    (when-let [errors (schema/validation-errors index-schema index)]
      (throw (ex-info (str "JSON Schema validation failed: " file)
                      {:path (str file)
                       :errors errors})))
    {:path (str file)
     :value index
     :schema-failures (when-not (= live-hash (get index "schema_hash"))
                        [(schema-hash-failure :index file live-hash (get index "schema_hash"))])}))
```

- [ ] **Step 4: Implement event validation and index integrity**

Add:

```clojure
(defn- event-failures [{:keys [path value schema-failures]}]
  (let [json-failures (mapv #(assoc % :path path) (event-json-coherence-failures value))
        graph (when (empty? json-failures) (event->graph value))
        typing-failures (if graph
                          (mapv #(assoc % :path path) (typing-coherence-failures value graph))
                          [])
        shacl-failures (if (and graph (empty? typing-failures))
                         (try
                           (validate-event-shacl! graph path)
                           []
                           (catch clojure.lang.ExceptionInfo e
                             (mapv #(assoc % :code :shacl-violation :path path)
                                   (:errors (ex-data e)))))
                         [])]
    (vec (concat schema-failures json-failures typing-failures shacl-failures))))

(defn- participants-by-person-id [event]
  (->> (get event "participants")
       (map (fn [participant] [(get participant "person_id") (get event "drift_event_id")]))
       (group-by first)
       (map (fn [[person-id pairs]]
              [person-id (set (map second pairs))]))
       (into {})))

(defn- expected-index-map [events]
  (apply merge-with set/union (map participants-by-person-id events)))

(defn- actual-index-map [indexes]
  (into {}
        (map (fn [index]
               [(get index "person_id") (set (get index "drift_event_ids"))]))
        indexes))

(defn- referential-integrity-failures [events indexes]
  (let [events-by-id (into {} (map (juxt #(get % "drift_event_id") identity) events))
        event-ids (set (keys events-by-id))
        expected (expected-index-map events)
        actual (actual-index-map indexes)
        indexed-event-ids (apply set/union #{} (vals actual))]
    (vec
     (concat
      (for [[person-id ids] actual
            event-id (sort ids)
            :when (not (contains? event-ids event-id))]
        {:code :index-target-missing
         :person_id person-id
         :drift_event_id event-id})
      (for [[person-id expected-ids] expected
            :let [actual-ids (get actual person-id #{})]
            event-id (sort (set/difference expected-ids actual-ids))]
        {:code :event-missing-from-participant-index
         :person_id person-id
         :drift_event_id event-id})
      (for [event-id (sort (set/difference event-ids indexed-event-ids))]
        {:code :orphan-event-file
         :drift_event_id event-id})))))

(defn validate-drift-events! [{:keys [persons-dir]}]
  (let [events-dir (drift-dir persons-dir "_events")
        indexes-dir (drift-dir persons-dir "_indexes")
        event-files (json-files events-dir)
        index-files (json-files indexes-dir)]
    (if (and (empty? event-files) (empty? index-files))
      {:status :not-present}
      (try
        (let [loaded-events (mapv load-event-file event-files)
              loaded-indexes (mapv load-index-file index-files)
              events (mapv :value loaded-events)
              indexes (mapv :value loaded-indexes)
              failures (vec (concat
                             (mapcat event-failures loaded-events)
                             (mapcat :schema-failures loaded-indexes)
                             (referential-integrity-failures events indexes)))]
          (if (seq failures)
            {:status :error :failures failures}
            {:status :ok :events (count events) :indexes (count indexes)}))
        (catch clojure.lang.ExceptionInfo e
          {:status :error
           :failures [{:code :exception
                       :message (ex-message e)
                       :data (ex-data e)}]})))))
```

- [ ] **Step 5: Run validation pipeline tests**

Run:

```bash
clojure -M:test -e "(require 'abc.tools.person-drift-test) (do ((requiring-resolve 'abc.tools.malli/install!)) nil) (clojure.test/run-tests 'abc.tools.person-drift-test)"
```

Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add src/abc/tools/person_drift.clj test/abc/tools/person_drift_test.clj
git commit -m "feat(person-drift): validate drift event indexes"
```

---

## Task 6: Integrate Drift Validation into `validate-design-bundle`

**Files:**
- Modify: `src/abc/tools/validate_design_bundle.clj`
- Modify: `test/abc/tools/validate_design_bundle_test.clj`

- [ ] **Step 1: Add failing integration tests**

Append to `test/abc/tools/validate_design_bundle_test.clj`:

```clojure
(deftest validate-drift-fixtures-smoke-test
  (testing "drift fixture runner returns nil when expected failures are observed"
    (is (nil? (validate/validate-drift-fixtures!
               {"fixtures/v0/invalid/drift/broken-index-target"
                #{:index-target-missing}})))))
```

- [ ] **Step 2: Run the failing integration test**

Run:

```bash
clojure -M:test -e "(require 'abc.tools.validate-design-bundle-test) (do ((requiring-resolve 'abc.tools.malli/install!)) nil) (clojure.test/run-tests 'abc.tools.validate-design-bundle-test)"
```

Expected: FAIL because `validate-drift-fixtures!` does not exist and the fixture directory does not exist.

- [ ] **Step 3: Require drift namespace and add fixture runner**

In `src/abc/tools/validate_design_bundle.clj`, add:

```clojure
[abc.tools.person-drift :as person-drift]
```

Add helpers near the Schematron fixture runner:

```clojure
(defn- validate-drift-invalid-fixture! [path expected-codes]
  (let [result (person-drift/validate-drift-events! {:persons-dir path})
        actual (set (map :code (:failures result)))]
    (when-not (= :error (:status result))
      (throw (ex-info "expected invalid drift fixture to fail"
                      {:fixture path
                       :result result})))
    (let [missing (set/difference expected-codes actual)]
      (when (seq missing)
        (throw (ex-info "missing expected drift validation failure"
                        {:fixture path
                         :missing (sort missing)
                         :actual (sort actual)}))))))

(defn validate-drift-fixtures! [invalid-fixtures]
  (doseq [[path expected-codes] invalid-fixtures]
    (validate-drift-invalid-fixture! path expected-codes)))
```

- [ ] **Step 4: Call drift validation in the main bundle flow**

In `validate-design-bundle!`, after the metadata bundle block logs `"metadata bundle ok"` and before ab-validator output checks, add:

```clojure
(tel/log! :info "==> Validating person drift events")
(let [result (person-drift/validate-drift-events!
              {:persons-dir "examples/v0/example-persons"})]
  (when (= :error (:status result))
    (throw (ex-info "person drift validation failed"
                    {:errors (:failures result)}))))
(tel/log! :info "person drift events ok")
```

After the TEI Schematron fixture pass, add:

```clojure
(tel/log! :info "==> Validating person drift negative fixtures")
(validate-drift-fixtures!
 {"fixtures/v0/invalid/drift/broken-index-target" #{:index-target-missing}})
(tel/log! :info "person drift negative fixtures ok")
```

- [ ] **Step 5: Add the first negative fixture directory**

Create `fixtures/v0/invalid/drift/broken-index-target/_indexes/000879.json`. Use the live index schema hash generated by Clojure, not a hand-typed value:

```bash
mkdir -p fixtures/v0/invalid/drift/broken-index-target/_indexes
clojure -M:test -e "(require '[abc.tools.manifest :as manifest]) (println (manifest/schema-hash \"schemas/person-drift-index.schema.json\"))"
```

Write the JSON with deterministic formatting through a Clojure one-liner:

```bash
clojure -M:test -e "(require '[abc.tools.manifest :as manifest]) (manifest/write-json-file! \"fixtures/v0/invalid/drift/broken-index-target/_indexes/000879.json\" {\"schema_id\" \"https://w3id.org/abc/schemas/person-drift-index.schema.json\" \"schema_hash\" (manifest/schema-hash \"schemas/person-drift-index.schema.json\") \"person_id\" \"000879\" \"drift_event_ids\" [\"sha256:0000000000000000000000000000000000000000000000000000000000000009\"]})"
```

- [ ] **Step 6: Run integration tests**

Run:

```bash
clojure -M:test -e "(require 'abc.tools.validate-design-bundle-test) (do ((requiring-resolve 'abc.tools.malli/install!)) nil) (clojure.test/run-tests 'abc.tools.validate-design-bundle-test)"
```

Expected: PASS.

- [ ] **Step 7: Commit**

```bash
git add src/abc/tools/validate_design_bundle.clj test/abc/tools/validate_design_bundle_test.clj fixtures/v0/invalid/drift/broken-index-target
git commit -m "feat(person-drift): wire drift validation into design bundle"
```

---

## Task 7: Add Complete Drift Negative Fixtures

**Files:**
- Create: `fixtures/v0/invalid/drift/*`
- Modify: `src/abc/tools/validate_design_bundle.clj`
- Modify: `test/abc/tools/validate_design_bundle_test.clj`

- [ ] **Step 1: Add fixture runner expectations**

Extend the invalid fixture map in `validate-design-bundle!` and `validate-drift-fixtures-smoke-test` to include:

```clojure
{"fixtures/v0/invalid/drift/broken-index-target" #{:index-target-missing}
 "fixtures/v0/invalid/drift/asymmetric-index" #{:event-missing-from-participant-index}
 "fixtures/v0/invalid/drift/orphan-event-file" #{:orphan-event-file}
 "fixtures/v0/invalid/drift/unsorted-participants" #{:participants-not-sorted}
 "fixtures/v0/invalid/drift/dangling-snapshot-ref" #{:unknown-snapshot-reference}
 "fixtures/v0/invalid/drift/invalid-role" #{:invalid-had-role}
 "fixtures/v0/invalid/drift/invalid-agent" #{:invalid-agent-iri}}
```

- [ ] **Step 2: Generate fixtures with a small Clojure script**

Run this script from the repo root. It uses the tested drift functions to materialize correct event IDs before intentionally breaking each fixture.

```bash
clojure -M:test -e "
(require '[abc.tools.manifest :as manifest]
         '[abc.tools.files :as files]
         '[abc.tools.person-drift :as drift])
(defn h [s] (files/example-hash s))
(defn base []
  {\"schema_id\" drift/event-schema-id
   \"schema_hash\" (manifest/schema-hash drift/event-schema-path)
   \"drift_event_type\" \"split\"
   \"date\" \"2026-04-30\"
   \"participants\" [{\"snapshot_id\" \"post-abc-000000000001\" \"person_id\" \"abc-000000000001\" \"person_record_hash\" (h \"01\")}
                    {\"snapshot_id\" \"post-abc-000000000002\" \"person_id\" \"abc-000000000002\" \"person_record_hash\" (h \"02\")}
                    {\"snapshot_id\" \"pre-000879\" \"person_id\" \"000879\" \"person_record_hash\" (h \"03\")}]
   \"evidence\" [\"https://example.org/abc/drift-evidence/fictional-split-2026\"]
   \"prov\" {\"used\" [\"pre-000879\"]
           \"was_generated_by\" [\"post-abc-000000000001\" \"post-abc-000000000002\"]
           \"qualified_association\" {\"agent\" \"https://w3id.org/abc/agents/editorial-board\"
                                    \"had_role\" \"abc:DriftEditor\"}}})
(defn index [person-id event-id]
  {\"schema_id\" drift/index-schema-id
   \"schema_hash\" (manifest/schema-hash drift/index-schema-path)
   \"person_id\" person-id
   \"drift_event_ids\" [event-id]})
(defn write-bundle [name event indexes]
  (let [root (str \"fixtures/v0/invalid/drift/\" name)
        event-id (get event \"drift_event_id\")]
    (.mkdirs (java.io.File. (str root \"/_events\")))
    (.mkdirs (java.io.File. (str root \"/_indexes\")))
    (manifest/write-json-file! (str root \"/_events/\" event-id \".json\") event)
    (doseq [[person-id body] indexes]
      (manifest/write-json-file! (str root \"/_indexes/\" person-id \".json\") body))))
(let [event (drift/materialize-event-id (base))
      event-id (get event \"drift_event_id\")
      full-indexes {\"000879\" (index \"000879\" event-id)
                    \"abc-000000000001\" (index \"abc-000000000001\" event-id)
                    \"abc-000000000002\" (index \"abc-000000000002\" event-id)}]
  (write-bundle \"asymmetric-index\" event (dissoc full-indexes \"abc-000000000002\"))
  (write-bundle \"orphan-event-file\" event {})
  (write-bundle \"unsorted-participants\" (drift/materialize-event-id (assoc (base) \"participants\" (vec (reverse (get (base) \"participants\"))))) full-indexes)
  (write-bundle \"dangling-snapshot-ref\" (drift/materialize-event-id (assoc-in (base) [\"prov\" \"used\"] [\"pre-999999\"]))) full-indexes)
  (write-bundle \"invalid-role\" (drift/materialize-event-id (assoc-in (base) [\"prov\" \"qualified_association\" \"had_role\"] \"abc:OtherRole\")) full-indexes)
  (write-bundle \"invalid-agent\" (drift/materialize-event-id (assoc-in (base) [\"prov\" \"qualified_association\" \"agent\"] \"not an iri\")) full-indexes))"
```

- [ ] **Step 3: Run fixture tests**

Run:

```bash
clojure -M:test -e "(require 'abc.tools.validate-design-bundle-test) (do ((requiring-resolve 'abc.tools.malli/install!)) nil) (clojure.test/run-tests 'abc.tools.validate-design-bundle-test)"
```

Expected: PASS.

- [ ] **Step 4: Commit**

```bash
git add fixtures/v0/invalid/drift src/abc/tools/validate_design_bundle.clj test/abc/tools/validate_design_bundle_test.clj
git commit -m "test(person-drift): add drift validation negative fixtures"
```

---

## Task 8: Add Example Drift Event and Prove Drift-Artifact Invariance

**Files:**
- Create: one generated event JSON file under `examples/v0/example-persons/_events/`
- Create: `examples/v0/example-persons/_indexes/000879.json`
- Create: `examples/v0/example-persons/_indexes/abc-000000000001.json`
- Create: `examples/v0/example-persons/_indexes/abc-000000000002.json`
- Modify: `test/abc/tools/person_drift_test.clj`

- [ ] **Step 1: Capture the pre-artifact manifest hash**

Run:

```bash
sha256sum examples/v0/example-work/manifest.json
```

Record the output in the task notes for this commit. Do not edit `manifest.json` in this task.

- [ ] **Step 2: Generate the example split event and indexes**

Run:

```bash
clojure -M:test -e "
(require '[abc.tools.files :as files]
         '[abc.tools.manifest :as manifest]
         '[abc.tools.person-drift :as drift])
(defn h [s] (files/example-hash s))
(def event
  (drift/materialize-event-id
   {\"schema_id\" drift/event-schema-id
    \"schema_hash\" (manifest/schema-hash drift/event-schema-path)
    \"drift_event_type\" \"split\"
    \"date\" \"2026-04-30\"
    \"participants\" [{\"snapshot_id\" \"post-abc-000000000001\" \"person_id\" \"abc-000000000001\" \"person_record_hash\" (h \"01\")}
                     {\"snapshot_id\" \"post-abc-000000000002\" \"person_id\" \"abc-000000000002\" \"person_record_hash\" (h \"02\")}
                     {\"snapshot_id\" \"pre-000879\" \"person_id\" \"000879\" \"person_record_hash\" (h \"03\")}]
    \"evidence\" [\"https://example.org/abc/drift-evidence/fictional-split-2026\"]
    \"prov\" {\"used\" [\"pre-000879\"]
            \"was_generated_by\" [\"post-abc-000000000001\" \"post-abc-000000000002\"]
            \"qualified_association\" {\"agent\" \"https://w3id.org/abc/agents/editorial-board\"
                                     \"had_role\" \"abc:DriftEditor\"}}}))
(def event-id (get event \"drift_event_id\"))
(defn index [person-id]
  {\"schema_id\" drift/index-schema-id
   \"schema_hash\" (manifest/schema-hash drift/index-schema-path)
   \"person_id\" person-id
   \"drift_event_ids\" [event-id]})
(.mkdirs (java.io.File. \"examples/v0/example-persons/_events\"))
(.mkdirs (java.io.File. \"examples/v0/example-persons/_indexes\"))
(manifest/write-json-file! (str \"examples/v0/example-persons/_events/\" event-id \".json\") event)
(doseq [person-id [\"000879\" \"abc-000000000001\" \"abc-000000000002\"]]
  (manifest/write-json-file! (str \"examples/v0/example-persons/_indexes/\" person-id \".json\") (index person-id)))
(println event-id)"
```

- [ ] **Step 3: Validate drift artifacts directly**

Run:

```bash
clojure -M:test -e "(require '[abc.tools.person-drift :as drift] '[abc.tools.malli :as am]) (am/install!) (println (drift/validate-drift-events! {:persons-dir \"examples/v0/example-persons\"}))"
```

Expected: prints a map with `:status :ok`, `:events 1`, and `:indexes 3`.

- [ ] **Step 4: Prove manifest byte identity for drift artifacts**

Run:

```bash
sha256sum examples/v0/example-work/manifest.json
git diff -- examples/v0/example-work/manifest.json examples/v0/example-work/metadata-record.json examples/v0/example-persons/000879.json
```

Expected: the `manifest.json` hash matches Step 1, and `git diff` prints no diff for the listed files.

- [ ] **Step 5: Commit**

```bash
git add examples/v0/example-persons/_events examples/v0/example-persons/_indexes test/abc/tools/person_drift_test.clj
git commit -m "test(person-drift): add example drift event sidecars"
```

---

## Task 9: Widen ABC-Local Person IDs

**Files:**
- Modify: `schemas/person-record.schema.json`
- Modify: `schemas/metadata-record.schema.json`
- Modify: `src/abc/tools/person_record.clj`
- Modify: `test/abc/tools/person_record_test.clj`

- [ ] **Step 1: Add failing ABC-local person tests**

Append to `test/abc/tools/person_record_test.clj`:

```clojure
(defn- abc-local-person []
  (assoc (example-person)
         "person_id" "abc-000000000001"
         "family_name" "ABC Local"
         "given_name" "One"))

(deftest validate-accepts-abc-local-person-id-test
  (is (= :ok (pr/validate! (abc-local-person)))))

(deftest person-iri-branches-for-abc-local-id-test
  (is (= "https://w3id.org/abc/persons/abc-000000000001"
         (pr/person-iri "abc-000000000001")))
  (is (= "http://www.aozora.gr.jp/index_pages/person000879.html"
         (pr/person-iri "000879"))))

(deftest record->graph-abc-local-identifier-is-string-test
  (let [g (pr/record->graph (abc-local-person))
        [identifier] (objects-of g "http://purl.org/dc/terms/identifier")]
    (is (= "abc-000000000001" (.getLiteralLexicalForm identifier)))
    (is (= "http://www.w3.org/2001/XMLSchema#string"
           (.getLiteralDatatypeURI identifier)))))
```

- [ ] **Step 2: Run failing person tests**

Run:

```bash
clojure -M:test -e "(require 'abc.tools.person-record-test) (do ((requiring-resolve 'abc.tools.malli/install!)) nil) (clojure.test/run-tests 'abc.tools.person-record-test)"
```

Expected: FAIL because schema and RDF code only support six-digit IDs.

- [ ] **Step 3: Widen JSON Schema patterns**

In `schemas/person-record.schema.json`, change:

```json
"person_id": { "type": "string", "pattern": "^[0-9]{6}$" }
```

to:

```json
"person_id": { "type": "string", "pattern": "^([0-9]{6}|abc-[0-9a-f]{12})$" }
```

In `schemas/metadata-record.schema.json`, change contributor `person_id` the same way.

- [ ] **Step 4: Branch person IRI and identifier literal emission**

In `src/abc/tools/person_record.clj`, add:

```clojure
(defn numeric-person-id? [person-id]
  (boolean (re-matches #"^[0-9]{6}$" person-id)))

(defn abc-local-person-id? [person-id]
  (boolean (re-matches #"^abc-[0-9a-f]{12}$" person-id)))
```

Replace `person-iri` with:

```clojure
(defn person-iri
  "Aozora numeric IDs keep their Aozora page IRI. ABC-local IDs use the
  ADR 0020 local person namespace."
  [person-id]
  (cond
    (numeric-person-id? person-id)
    (str "http://www.aozora.gr.jp/index_pages/person" person-id ".html")

    (abc-local-person-id? person-id)
    (str "https://w3id.org/abc/persons/" person-id)

    :else
    (throw (ex-info (str "unsupported person_id shape: " person-id)
                    {:person_id person-id}))))
```

Add:

```clojure
(defn- ->identifier-literal [person-id]
  (if (numeric-person-id? person-id)
    (->int-literal person-id)
    (NodeFactory/createLiteral ^String person-id XSDDatatype/XSDstring)))
```

In `person-data`, replace:

```clojure
:dcterms/identifier (->int-literal (get person "person_id"))
```

with:

```clojure
:dcterms/identifier (->identifier-literal (get person "person_id"))
```

- [ ] **Step 5: Run person tests**

Run:

```bash
clojure -M:test -e "(require 'abc.tools.person-record-test) (do ((requiring-resolve 'abc.tools.malli/install!)) nil) (clojure.test/run-tests 'abc.tools.person-record-test)"
```

Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add schemas/person-record.schema.json schemas/metadata-record.schema.json src/abc/tools/person_record.clj test/abc/tools/person_record_test.clj
git commit -m "feat(person): support ABC-local person identifiers"
```

---

## Task 10: Regenerate Schema-Hash Cascade from ABC-Local ID Widening

**Files:**
- Modify: `examples/v0/example-persons/000879.json`
- Modify: `examples/v0/example-work/metadata-record.json`
- Modify: `examples/v0/example-work/manifest.json`
- Possibly modify: `examples/v0/example-work/metadata-record.ttl` only if the generated RDF bytes change.

- [ ] **Step 1: Capture expected cascade before regeneration**

Run:

```bash
git diff -- examples/v0/example-persons/000879.json examples/v0/example-work/metadata-record.json examples/v0/example-work/manifest.json examples/v0/example-work/metadata-record.ttl
```

Expected: any diff shown here comes only from earlier committed tasks in this plan. If unrelated user edits appear, stop and inspect before continuing.

- [ ] **Step 2: Refresh example records and manifest**

Run:

```bash
nix run .#aozora-ingest -- \
  --zip references/aozorabunko/index_pages/list_person_all_extended_utf8.zip \
  --work-id 000127 \
  --output examples/v0/example-work/metadata-record.json \
  --persons-output-dir examples/v0/example-persons \
  --overwrite \
  --refresh-manifest examples/v0/example-work/manifest.json
```

If the zip path does not exist, locate the checked-in Aozora CSV zip with:

```bash
find references examples -name 'list_person_all_extended*.zip' -print
```

Then rerun the same command with the discovered zip path.

- [ ] **Step 3: Check whether TTL parity changed**

Run:

```bash
clojure -M:test -e "(require '[abc.tools.files :as files] '[abc.tools.metadata-record :as mr] '[abc.tools.validate-design-bundle :as v] '[abc.tools.shacl :as shacl] '[abc.tools.malli :as am]) (am/install!) (let [record (files/read-json \"examples/v0/example-work/metadata-record.json\") persons (#'v/validate-persons-directory! \"examples/v0/example-persons\" \"schemas/person-record.schema.json\") generated (mr/record+persons->ttl record persons)] (spit \"/tmp/abc-metadata-record.generated.ttl\" generated) (println (= (slurp \"examples/v0/example-work/metadata-record.ttl\") generated)))"
```

Expected: prints `true`. If it prints `false`, inspect `/tmp/abc-metadata-record.generated.ttl`; if the only changes are the expected RDF view from person identifier widening, replace `examples/v0/example-work/metadata-record.ttl` with the generated file and include it in the commit.

- [ ] **Step 4: Verify the expected cascade and no semantic drift**

Run:

```bash
git diff -- examples/v0/example-persons/000879.json examples/v0/example-work/metadata-record.json examples/v0/example-work/manifest.json examples/v0/example-work/metadata-record.ttl
```

Expected rotations:

- `person_record_schema_hash` changes in `examples/v0/example-persons/000879.json`.
- `person_record_hash` changes because `person_record_schema_hash` is part of the canonical identity form.
- `metadata_record_schema_hash` changes in `metadata-record.json`.
- `contributors[0].person_record_hash` changes to match the person record.
- `metadata_record_hash` in `manifest_identity_object` changes.
- `artifact_id` changes.

Expected non-rotations:

- `family_name`, `given_name`, date fields, copyright fields, and `external_links` do not change.
- TEI files, IIIF files, and drift event/index sidecars do not change.

- [ ] **Step 5: Run design bundle validation**

Run:

```bash
nix run .#validate-design-bundle
```

Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add examples/v0/example-persons/000879.json examples/v0/example-work/metadata-record.json examples/v0/example-work/manifest.json examples/v0/example-work/metadata-record.ttl
git commit -m "chore(examples): refresh schema hashes after person ID widening"
```

If `metadata-record.ttl` did not change, `git add` silently leaves it out.

---

## Task 11: Update Focused Tests and Lockfiles

**Files:**
- Modify: `nix/clj-nix-deps.edn`
- Modify: `deps-lock.json`
- Modify: `nix/legacy-deps-lock.json` only if `bin/update-clj-nix-lock` refreshes both lockfiles.

- [ ] **Step 1: Add person drift tests to the focused-test alias**

In `nix/clj-nix-deps.edn`, add `abc.tools.person-drift-test` to both the `require` form and the `test/run-tests` form inside `:abc/focused-test`.

- [ ] **Step 2: Run focused tests**

Run:

```bash
nix run .#focused-test
```

Expected: PASS.

- [ ] **Step 3: Refresh clj-nix lockfiles**

Run:

```bash
bin/update-clj-nix-lock
```

Expected: `deps-lock.json` changes only if dependency resolution metadata changed. `nix/legacy-deps-lock.json` changes only if the unified lock updater refreshes it as part of its normal contract.

- [ ] **Step 4: Commit**

```bash
git add nix/clj-nix-deps.edn deps-lock.json
test ! -f nix/legacy-deps-lock.json || git add nix/legacy-deps-lock.json
git commit -m "test(person-drift): include drift tests in focused suite"
```

If a lockfile did not change, `git add` leaves it out.

---

## Task 12: Final Verification

**Files:**
- No source edits expected.

- [ ] **Step 1: Run Clojure focused tests**

Run:

```bash
nix run .#focused-test
```

Expected: PASS.

- [ ] **Step 2: Run full design bundle validation**

Run:

```bash
nix run .#validate-design-bundle
```

Expected: PASS.

- [ ] **Step 3: Run flake check**

Run:

```bash
nix flake check
```

Expected: PASS.

- [ ] **Step 4: Inspect final diff**

Run:

```bash
git status --short
git diff --stat HEAD
```

Expected: only files from this plan are modified or untracked. Pre-existing unrelated dirty files remain unrelated and unstaged.

- [ ] **Step 5: Commit any final verification-only adjustments**

If final verification required a small fix, commit it with a message naming the concrete fix. If no files changed after Task 11, do not create an empty commit.

---

## Self-Review

1. **ADR 0020 Position L covered.** Task 8 proves drift sidecars leave `manifest.json`, `metadata-record.json`, and `000879.json` untouched. Task 10 separately handles the ABC-local schema cascade.
2. **Identifier policy covered.** Task 9 widens both JSON schemas and RDF emission. Task 4 widens `PersonRecordShape` while preserving `sh:minCount 1` and `sh:maxCount 1`.
3. **Stable event IDs covered.** Task 2 tests and implements the omit-only-`drift_event_id` JCS hashing rule.
4. **Participant snapshot binding covered.** Task 3 derives snapshot IRIs from embedded `person_record_hash` values. No current person record lookup is used in `event->graph`.
5. **JSON graph-coherence covered.** Task 2 implements ordering, uniqueness, resolution, coverage, disjointness, prefix usage, role enum, and agent IRI checks before RDF emission.
6. **RDF materialization covered.** Task 3 emits direct `rdf:type` triples for `abc:DriftEvent`, `prov:Activity`, and the split/merge subclass.
7. **SHACL covered.** Task 4 adds all three drift shapes plus subclass axioms for documentation.
8. **Index contract covered.** Task 5 validates index schemas and the three referential-integrity rules.
9. **Negative fixtures covered.** Tasks 6 and 7 create a runner separate from the TEI Schematron runner and fixtures for index and JSON graph-coherence failures.
10. **Focused tests covered.** Task 11 adds the new test namespace to the Nix focused-test alias.
11. **No placeholders scan.** The plan contains no deferred-detail markers or unspecified validation steps. Generated hashes are produced by explicit Clojure commands rather than hand-written placeholders.
12. **Risk to watch during execution.** The only ADR-level surface not explicitly named in ADR 0021 is the event node IRI. This plan uses `https://w3id.org/abc/person-drift-events/` followed by the 64-character event digest as an implementation detail for RDF validation. If reviewers consider event node IRIs public API, promote that convention into an ADR amendment before release.
