# Separated Person Records Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Split Person records out of the work-level `metadata-record.json` into their own content-addressed artifact under `examples/v0/example-persons/`, so a person edit only invalidates the works that reference that person.

**Architecture:** Two artifact kinds and two namespaces. Person records become independent JSON files with their own JSON Schema, validation namespace, and RDF mapping. The metadata-record's `persons[]` becomes `contributors[]` (references by `person_id` + `person_record_hash` + `relation_to_work`). The harness composes the two RDF views for SHACL and adds a reference-integrity check.

**Tech Stack:** Clojure 1.12, JSON Schema Draft 2020-12, RFC 8785 JCS, Apache Jena 5.3 (Aristotle for graphs, Jena SHACL for validation), Jing (TEI RelaxNG; unchanged).

---

## Spec

`docs/superpowers/specs/2026-04-28-separated-person-records-design.md`. Read before starting.

## File Map

**Create:**

- `schemas/person-record.schema.json` — JSON Schema for the new artifact.
- `src/abc/tools/person_record.clj` — `validate!`, `record-hash`, `record->graph`, `person-iri`.
- `test/abc/tools/person_record_test.clj` — schema and identity tests.
- `examples/v0/example-persons/000879.json` — the 芥川竜之介 fixture.

**Modify:**

- `schemas/metadata-record.schema.json` — `persons[]` → `contributors[]`; drop `$defs/person`.
- `schemas/manifest.shacl.ttl` — add `PersonRecordShape`; retire `MetadataRecordPersonShape`.
- `src/abc/tools/metadata_record.clj` — `canonical-identity-form` sorts `contributors[]`; `build-metadata-record` takes `{:work, :contributors}`; `record->graph` emits work + contributor edges only; new `record+persons->ttl` composes work and person graphs.
- `test/abc/tools/metadata_record_test.clj` — replace `persons` references with `contributors`; new compose-graph test.
- `src/abc/tools/aozora_csv.clj` — split `parse-person-fields-from-row` into person body (no role) plus contributor entry (`person_id` + role).
- `test/abc/tools/aozora_csv_test.clj` — extend for the split.
- `src/abc/tools/aozora_ingest.clj` — `--persons-output-dir`, `--overwrite`, `--refresh-manifest`; corruption-safe idempotent check; emits N+1 files.
- `test/abc/tools/aozora_ingest_test.clj` (create if missing; check first) — new ingester behaviors.
- `src/abc/tools/tei_header.clj` — `build` takes `{:work, :contributors [{:relation-to-work, :person}]}`.
- `test/abc/tools/tei_header_test.clj` — update call shape.
- `src/abc/tools/validate_design_bundle.clj` — `validate-metadata-record!` becomes `validate-metadata-bundle!`: validates persons dir, recomputes work hash, recomputes contributor reference hashes, runs SHACL on the combined graph.
- `test/abc/tools/validate_design_bundle_test.clj` — reference-integrity loud-fail test; schema-hash precondition for persons.
- `examples/v0/example-work/metadata-record.json` — replace `persons[]` with `contributors[]`.
- `examples/v0/example-work/metadata-record.ttl` — regenerated from the combined graph.
- `examples/v0/example-work/manifest.json` — refreshed `manifest_identity_object.metadata_record_hash` and `artifact_id`.
- `flake.nix` — add new files to `contract-surface`.
- `nix/clj-nix-deps.edn` — add `abc.tools.person-record-test` to the focused-test alias.

---

## Task 1: Add `schemas/person-record.schema.json`

**Files:**
- Create: `schemas/person-record.schema.json`

- [ ] **Step 1: Write the schema file**

Write `schemas/person-record.schema.json`:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://w3id.org/abc/schemas/person-record.schema.json",
  "title": "ABC Person Record",
  "description": "Bibliographic record for one Aozora person. Identity (person-record-hash) is SHA-256 over RFC 8785 JCS bytes of this object with source_csv_provenance dropped. Nullable fields are explicit JSON null, not key-omission. Defaults are documentation only; canonicalization hashes the literal record. The hash is not stored in the file; it lives at reference sites in metadata-record contributors[] entries.",
  "type": "object",
  "additionalProperties": false,
  "required": [
    "person_record_schema_id",
    "person_record_schema_hash",
    "person_id",
    "family_name",
    "given_name",
    "person_copyright_expired"
  ],
  "properties": {
    "person_record_schema_id": {
      "const": "https://w3id.org/abc/schemas/person-record.schema.json"
    },
    "person_record_schema_hash": { "$ref": "#/$defs/hash" },
    "person_id": { "type": "string", "pattern": "^[0-9]{6}$" },
    "family_name": { "type": "string", "minLength": 1 },
    "given_name": { "type": "string", "minLength": 1 },
    "family_name_reading": { "$ref": "#/$defs/nullableString" },
    "given_name_reading": { "$ref": "#/$defs/nullableString" },
    "family_name_sort": { "$ref": "#/$defs/nullableString" },
    "given_name_sort": { "$ref": "#/$defs/nullableString" },
    "family_name_romaji": { "$ref": "#/$defs/nullableString" },
    "given_name_romaji": { "$ref": "#/$defs/nullableString" },
    "date_of_birth": { "$ref": "#/$defs/nullableDate" },
    "date_of_death": { "$ref": "#/$defs/nullableDate" },
    "person_copyright_expired": { "type": "boolean" },
    "external_links": {
      "type": "array",
      "items": { "type": "string", "format": "uri" },
      "uniqueItems": true,
      "default": []
    },
    "source_csv_provenance": { "$ref": "#/$defs/csvProvenance" }
  },
  "$defs": {
    "hash": {
      "type": "string",
      "pattern": "^sha256:[0-9a-f]{64}$"
    },
    "nullableString": { "type": ["string", "null"] },
    "nullableDate": {
      "anyOf": [
        { "type": "string", "format": "date" },
        { "type": "null" }
      ]
    },
    "csvProvenance": {
      "type": "object",
      "additionalProperties": false,
      "required": ["source_url", "retrieved_at", "original_file_hash"],
      "properties": {
        "source_url": { "type": "string", "format": "uri" },
        "retrieved_at": { "type": "string", "format": "date-time" },
        "original_file_hash": { "$ref": "#/$defs/hash" }
      }
    }
  }
}
```

- [ ] **Step 2: Confirm the file is itself valid JSON Schema**

Run:

```bash
clojure -M -e "(require '[abc.tools.files :as f] '[abc.tools.schema :as s]) (let [sch (f/read-json \"schemas/person-record.schema.json\")] (s/schema-valid! sch \"schemas/person-record.schema.json\"))"
```

Expected: no exception thrown (silent success).

- [ ] **Step 3: Commit**

```bash
git add schemas/person-record.schema.json
git commit -m "$(cat <<'EOF'
feat: schemas/person-record.schema.json

Person record carries person_record_schema_{id,hash}, person_id, all
bibliographic fields currently embedded in metadata-record persons[] minus
relation_to_work, plus an optional source_csv_provenance block. The
record's identity hash is computed externally from these fields per the
spec; the file does not store its own hash.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: glm-5
EOF
)"
```

---

## Task 2: `abc.tools.person-record` namespace — `validate!` + `record-hash`

**Files:**
- Create: `src/abc/tools/person_record.clj`
- Create: `test/abc/tools/person_record_test.clj`

- [ ] **Step 1: Write the failing test file**

Write `test/abc/tools/person_record_test.clj`:

```clojure
(ns abc.tools.person-record-test
  (:require [abc.tools.manifest :as manifest]
            [abc.tools.person-record :as pr]
            [clojure.test :refer [deftest is testing]]))

(def ^:private schema-path "schemas/person-record.schema.json")
(def ^:private schema-id "https://w3id.org/abc/schemas/person-record.schema.json")

(defn- example-person []
  {"person_record_schema_id" schema-id
   "person_record_schema_hash" (manifest/schema-hash schema-path)
   "person_id" "000879"
   "family_name" "芥川"
   "given_name" "竜之介"
   "family_name_reading" "あくたがわ"
   "given_name_reading" "りゅうのすけ"
   "family_name_sort" "あくたかわ"
   "given_name_sort" "りゆうのすけ"
   "family_name_romaji" "Akutagawa"
   "given_name_romaji" "Ryunosuke"
   "date_of_birth" "1892-03-01"
   "date_of_death" "1927-07-24"
   "person_copyright_expired" true
   "external_links" []})

(deftest validate-accepts-example-test
  (testing "the in-test example person validates"
    (is (= :ok (pr/validate! (example-person))))))

(deftest validate-rejects-missing-required-test
  (testing "validate! throws when family_name is missing"
    (is (thrown? clojure.lang.ExceptionInfo
                 (pr/validate! (dissoc (example-person) "family_name"))))))

(deftest validate-rejects-extra-key-test
  (testing "validate! rejects an unknown property"
    (is (thrown? clojure.lang.ExceptionInfo
                 (pr/validate! (assoc (example-person) "unknown" 1))))))

(deftest record-hash-format-test
  (testing "record-hash returns sha256:<64-hex>"
    (is (re-matches #"^sha256:[0-9a-f]{64}$"
                    (pr/record-hash (example-person))))))

(deftest record-hash-deterministic-test
  (testing "record-hash returns the same value across two calls"
    (is (= (pr/record-hash (example-person))
           (pr/record-hash (example-person))))))

(deftest record-hash-ignores-key-order-test
  (testing "record-hash is independent of map insertion order"
    (let [p (example-person)
          shuffled (into (sorted-map) p)]
      (is (= (pr/record-hash p) (pr/record-hash shuffled))))))

(deftest record-hash-excludes-provenance-test
  (testing "changing source_csv_provenance does not change record-hash"
    (let [base (example-person)
          with-prov-a (assoc base "source_csv_provenance"
                             {"source_url" "https://a.example/x.csv"
                              "retrieved_at" "2026-01-01T00:00:00Z"
                              "original_file_hash" "sha256:0000000000000000000000000000000000000000000000000000000000000000"})
          with-prov-b (assoc base "source_csv_provenance"
                             {"source_url" "https://b.example/y.csv"
                              "retrieved_at" "2026-12-31T23:59:59Z"
                              "original_file_hash" "sha256:1111111111111111111111111111111111111111111111111111111111111111"})]
      (is (= (pr/record-hash base) (pr/record-hash with-prov-a)))
      (is (= (pr/record-hash with-prov-a) (pr/record-hash with-prov-b))))))

(deftest record-hash-includes-schema-hash-test
  (testing "mutating person_record_schema_hash changes record-hash"
    (let [p (example-person)
          mutated (assoc p "person_record_schema_hash"
                         "sha256:0000000000000000000000000000000000000000000000000000000000000000")]
      (is (not= (pr/record-hash p) (pr/record-hash mutated))))))

(deftest record-hash-includes-bibliographic-fields-test
  (testing "mutating family_name_romaji changes record-hash"
    (let [p (example-person)
          mutated (assoc p "family_name_romaji" "Akutagawa-changed")]
      (is (not= (pr/record-hash p) (pr/record-hash mutated))))))

(deftest schema-hash-self-consistent-test
  (testing "the example's embedded schema hash matches the live schema"
    (is (= (manifest/schema-hash schema-path)
           (get (example-person) "person_record_schema_hash")))))
```

- [ ] **Step 2: Run the test and confirm it fails because the namespace is missing**

Run: `clojure -M:test -e "(require 'abc.tools.person-record-test)" 2>&1 | tail -5`
Expected: FAIL with `Could not locate abc/tools/person_record__init.class, abc/tools/person_record.clj or abc/tools/person_record.cljc`.

- [ ] **Step 3: Create the namespace with `validate!` and `record-hash`**

Write `src/abc/tools/person_record.clj`:

```clojure
(ns abc.tools.person-record
  "Identity, validation, and RDF mapping for ABC person records.
  record-hash hashes the canonical-identity form: schema_id and
  schema_hash included; source_csv_provenance excluded; nullable
  fields explicit as JSON null. record->graph renders a single
  Person as FOAF + RDA Group 2 triples."
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.schema :as schema]))

(def schema-path "schemas/person-record.schema.json")
(def schema-id "https://w3id.org/abc/schemas/person-record.schema.json")

(defn validate!
  "Validate `record` against schemas/person-record.schema.json.
  Returns :ok on success; throws ex-info with :errors on failure."
  [record]
  (let [s (files/read-json schema-path)
        errors (schema/validation-errors s record)]
    (if (seq errors)
      (throw (ex-info "person-record validation failed"
                      {:errors errors}))
      :ok)))

(defn- canonical-identity-form
  "Returns the JSON value used as input to record-hash. Drops
  source_csv_provenance."
  [record]
  (dissoc record "source_csv_provenance"))

(defn record-hash
  "Compute sha256:<hex> over the canonical-identity form of `record`
  using RFC 8785 JCS via abc.tools.hash/sha256-json-jcs."
  [record]
  (hash/format-sha256
   (hash/sha256-json-jcs (canonical-identity-form record))))
```

- [ ] **Step 4: Run the tests to confirm they pass**

Run: `clojure -M:test -e "(require 'abc.tools.person-record-test) (clojure.test/run-tests 'abc.tools.person-record-test)" 2>&1 | tail -10`
Expected: PASS for all tests in the namespace.

- [ ] **Step 5: Commit**

```bash
git add src/abc/tools/person_record.clj test/abc/tools/person_record_test.clj
git commit -m "$(cat <<'EOF'
feat: abc.tools.person-record validate!/record-hash

validate! checks against schemas/person-record.schema.json. record-hash
hashes the canonical-identity form (schema-id + schema-hash + bibliographic
fields, with source_csv_provenance dropped) using RFC 8785 JCS via
abc.tools.hash/sha256-json-jcs.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: glm-5
EOF
)"
```

---

## Task 3: Lift Person → RDF into `abc.tools.person-record/record->graph` AND make metadata-record delegate

**Files:**
- Modify: `src/abc/tools/person_record.clj`
- Modify: `test/abc/tools/person_record_test.clj`
- Modify: `src/abc/tools/metadata_record.clj`

The current `abc.tools.metadata-record/person-data` builds an Aristotle map for one Person. Lift it into `person-record` as `record->graph`, **and** in the same task delete `person-data` from `metadata-record` and have its `record->graph` delegate to `person-record/record->graph` for person bodies. This avoids the source-of-truth duplication window between the lift and the contributor refactor (Task 6).

During Tasks 3–5 the metadata-record's `record->graph` still takes a record with `persons[]` (the fixture hasn't migrated yet) but emits person triples through `person-record/record->graph`. Task 6 is then a clean signature change to contributors, with no remaining person-RDF logic to move.

- [ ] **Step 1: Add a failing test for `record->graph`**

Append to `test/abc/tools/person_record_test.clj`:

```clojure
(deftest record->graph-key-triples-test
  (testing "record->graph emits FOAF + RDA Group 2 triples for one person"
    (let [g (pr/record->graph (example-person))
          triples (iterator-seq (.find g))
          predicates (set (map #(.getURI (.getPredicate %)) triples))
          subjects (set (map #(.getURI (.getSubject %)) triples))]
      (is (contains? subjects (pr/person-iri "000879")))
      (is (contains? predicates "http://purl.org/dc/terms/identifier"))
      (is (contains? predicates "http://xmlns.com/foaf/0.1/familyName"))
      (is (contains? predicates "http://xmlns.com/foaf/0.1/givenName"))
      (is (contains? predicates "http://xmlns.com/foaf/0.1/name"))
      (is (contains? predicates "http://RDVocab.info/ElementsGr2/dateOfBirth"))
      (is (contains? predicates "http://RDVocab.info/ElementsGr2/dateOfDeath")))))
```

- [ ] **Step 2: Run to confirm failure**

Run: `clojure -M:test -e "(require 'abc.tools.person-record-test) (clojure.test/run-tests 'abc.tools.person-record-test)" 2>&1 | tail -10`
Expected: FAIL — `record->graph` and `person-iri` unresolved.

- [ ] **Step 3: Implement `record->graph` and `person-iri`**

Append to `src/abc/tools/person_record.clj`:

```clojure
;; ---------------------------------------------------------------------------
;; RDF mapping
;; ---------------------------------------------------------------------------

(defn person-iri
  "Existing Aozora LOD uses http://www.aozora.gr.jp/index_pages/personNNNNNN.html
  as the person IRI."
  [person-id]
  (str "http://www.aozora.gr.jp/index_pages/person" person-id ".html"))
```

Add the require + import block by replacing the existing `(:require ...)` form with:

```clojure
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.manifest-to-rdf :as manifest-to-rdf]
            [abc.tools.rdf-prefixes :as rdf-prefixes]
            [abc.tools.schema :as schema]
            [arachne.aristotle :as aa])
  (:import [org.apache.jena.datatypes.xsd XSDDatatype]
           [org.apache.jena.graph NodeFactory]))
```

(Replace the file's existing two-line top form `(:require [abc.tools.files :as files] [abc.tools.hash :as hash] [abc.tools.schema :as schema]))` with the block above; the existing `(ns ...)` body otherwise stays.)

Append the helpers and graph builder:

```clojure
(defn- ->date-literal [iso-date]
  (NodeFactory/createLiteral ^String iso-date XSDDatatype/XSDdate))

(defn- ->int-literal [s]
  (NodeFactory/createLiteral ^String s XSDDatatype/XSDint))

(defn- person-data
  "Aristotle map for one Person. Pulled from the prior
  abc.tools.metadata-record/person-data; the IRI/predicate set is
  unchanged so RDF/Turtle output remains byte-identical."
  [person]
  (let [iri (str "<" (person-iri (get person "person_id")) ">")]
    (cond-> {:rdf/about iri
             :rdf/type [:foaf/Person]
             :dcterms/identifier (->int-literal (get person "person_id"))
             :foaf/familyName (get person "family_name")
             :foaf/givenName (get person "given_name")
             :foaf/name (str (get person "family_name") " "
                             (get person "given_name"))}
      (get person "family_name_reading")
      (assoc :abc/familyNameReading (get person "family_name_reading"))
      (get person "given_name_reading")
      (assoc :abc/givenNameReading (get person "given_name_reading"))
      (get person "family_name_sort")
      (assoc :abc/familyNameForSort (get person "family_name_sort"))
      (get person "given_name_sort")
      (assoc :abc/givenNameForSort (get person "given_name_sort"))
      (get person "family_name_romaji")
      (assoc :abc/familyNameRomaji (get person "family_name_romaji"))
      (get person "given_name_romaji")
      (assoc :abc/givenNameRomaji (get person "given_name_romaji"))
      (get person "date_of_birth")
      (assoc :rdag2/dateOfBirth (->date-literal (get person "date_of_birth")))
      (get person "date_of_death")
      (assoc :rdag2/dateOfDeath (->date-literal (get person "date_of_death")))
      (seq (get person "external_links"))
      (assoc :rdfs/seeAlso (mapv #(str "<" % ">") (get person "external_links"))))))

(defn record->graph
  "Build a Jena graph from a single Person record."
  [record]
  (rdf-prefixes/ensure!)
  (let [graph (aa/graph :simple)]
    (aa/add graph (person-data record))
    graph))

(defn record->ttl
  "Convenience: (comp manifest-to-rdf/graph->ttl record->graph)."
  [record]
  (manifest-to-rdf/graph->ttl (record->graph record)))
```

- [ ] **Step 4: Run the test and confirm it passes**

Run: `clojure -M:test -e "(require 'abc.tools.person-record-test) (clojure.test/run-tests 'abc.tools.person-record-test)" 2>&1 | tail -10`
Expected: PASS for all tests including the new `record->graph-key-triples-test`.

- [ ] **Step 5: Delete `person-data` from `metadata-record.clj`; delegate to `person-record`**

Open `src/abc/tools/metadata_record.clj`. Delete the `(defn- person-data ...)` block entirely. In `record->graph`, replace the inline `(aa/add graph (person-data p))` line with a call to `person-record/record->graph` whose triples are added to the work graph:

```clojure
(defn record->graph
  "Build a Jena graph from a metadata record using the resolved
  vocabulary mapping (see spec §RDF vocabulary alignment). Person
  triples come from abc.tools.person-record/record->graph; the
  metadata-record namespace owns work + contributor edges only."
  [record]
  (rdf-prefixes/ensure!)
  (let [work (get record "work")
        persons (get record "persons")
        graph (aa/graph :simple)]
    (aa/add graph (work-data work persons))
    (doseq [p persons]
      (let [pg (person-record/record->graph p)]
        (.add graph (.find pg))))
    graph))
```

Add `[abc.tools.person-record :as person-record]` to the require block.

- [ ] **Step 6: Run all metadata-record tests; confirm they still pass**

Run: `clojure -M:test -e "(require 'abc.tools.metadata-record-test) (clojure.test/run-tests 'abc.tools.metadata-record-test)" 2>&1 | tail -15`
Expected: PASS — `record->ttl-matches-fixture-test` is byte-identical because the lifted `person-data` has identical predicate output.

If the byte-parity test fails, the `(.add graph (.find pg))` adapter is iterating differently than the original `(aa/add graph (person-data p))`. Inspect `arachne.aristotle/add` and `org.apache.jena.graph.Graph#add(ExtendedIterator)` — the helper may need to be an explicit `(while (.hasNext it) (.add graph (.next it)))` over the iterator. Adjust until the byte-parity test is green.

- [ ] **Step 7: Commit**

```bash
git add src/abc/tools/person_record.clj test/abc/tools/person_record_test.clj src/abc/tools/metadata_record.clj
git commit -m "$(cat <<'EOF'
feat: lift person→RDF to abc.tools.person-record; metadata-record delegates

person-data is gone from metadata-record; person-record/record->graph
is the single source of truth for FOAF/RDA-Group-2 person triples.
metadata-record/record->graph still takes a record with persons[]
during this transition window (the contributors refactor lands in
Task 6); it composes work + person graphs by delegating to
person-record. Byte-parity of the existing metadata-record.ttl
fixture is preserved.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: glm-5
EOF
)"
```

---

## Task 4: Build `examples/v0/example-persons/000879.json` from the existing fixture

**Files:**
- Create: `examples/v0/example-persons/000879.json`
- Modify: `test/abc/tools/person_record_test.clj`

- [ ] **Step 1: Add a failing test that loads and validates the fixture**

Append to `test/abc/tools/person_record_test.clj`:

```clojure
(def ^:private fixture-path "examples/v0/example-persons/000879.json")

(deftest example-person-fixture-validates-test
  (testing "examples/v0/example-persons/000879.json validates against the schema"
    (let [record (files/read-json fixture-path)]
      (is (= :ok (pr/validate! record))))))

(deftest example-person-fixture-schema-hash-test
  (testing "the embedded schema hash matches the live schema"
    (let [record (files/read-json fixture-path)]
      (is (= (manifest/schema-hash schema-path)
             (get record "person_record_schema_hash"))))))
```

(Add `[abc.tools.files :as files]` to the test require if not already present — at this stage it should already be imported transitively from `manifest`, but it is cleaner to require it explicitly.)

- [ ] **Step 2: Confirm the test fails because the file doesn't exist**

Run: `clojure -M:test -e "(require 'abc.tools.person-record-test) (clojure.test/run-tests 'abc.tools.person-record-test)" 2>&1 | tail -10`
Expected: FAIL with file-not-found error or similar.

- [ ] **Step 3: Compute the live schema hash and write the fixture**

Compute the schema hash:

```bash
clojure -M -e "(require '[abc.tools.manifest :as m]) (println (m/schema-hash \"schemas/person-record.schema.json\"))"
```

Write `examples/v0/example-persons/000879.json` with deterministic-JSON formatting (lex-ordered keys, insertion-ordered arrays). Use the value just printed for `person_record_schema_hash`. Replace `<PASTE_HASH_HERE>` below with the printed value:

```json
{
  "date_of_birth": "1892-03-01",
  "date_of_death": "1927-07-24",
  "external_links": [],
  "family_name": "芥川",
  "family_name_reading": "あくたがわ",
  "family_name_romaji": "Akutagawa",
  "family_name_sort": "あくたかわ",
  "given_name": "竜之介",
  "given_name_reading": "りゅうのすけ",
  "given_name_romaji": "Ryunosuke",
  "given_name_sort": "りゆうのすけ",
  "person_copyright_expired": true,
  "person_id": "000879",
  "person_record_schema_hash": "<PASTE_HASH_HERE>",
  "person_record_schema_id": "https://w3id.org/abc/schemas/person-record.schema.json"
}
```

The bibliographic field values come directly from the existing `examples/v0/example-work/metadata-record.json` `persons[0]` entry (verify by reading that file). The fixture omits `source_csv_provenance` and does not include `relation_to_work` — that field is now Work-side.

To produce deterministic JSON output for the file, regenerate it via the project's deterministic-JSON writer instead of pasting:

```bash
clojure -M -e "
  (require '[abc.tools.files :as f] '[abc.tools.json :as j] '[abc.tools.manifest :as m] '[clojure.java.io :as io])
  (let [record {\"person_record_schema_id\" \"https://w3id.org/abc/schemas/person-record.schema.json\"
                \"person_record_schema_hash\" (m/schema-hash \"schemas/person-record.schema.json\")
                \"person_id\" \"000879\"
                \"family_name\" \"芥川\"
                \"given_name\" \"竜之介\"
                \"family_name_reading\" \"あくたがわ\"
                \"given_name_reading\" \"りゅうのすけ\"
                \"family_name_sort\" \"あくたかわ\"
                \"given_name_sort\" \"りゆうのすけ\"
                \"family_name_romaji\" \"Akutagawa\"
                \"given_name_romaji\" \"Ryunosuke\"
                \"date_of_birth\" \"1892-03-01\"
                \"date_of_death\" \"1927-07-24\"
                \"person_copyright_expired\" true
                \"external_links\" []}]
    (.mkdirs (io/file \"examples/v0/example-persons\"))
    (j/write-deterministic-json-file! (io/file \"examples/v0/example-persons/000879.json\") record))"
```

Expected: writes the file in the canonical deterministic format.

- [ ] **Step 4: Run the tests and confirm they pass**

Run: `clojure -M:test -e "(require 'abc.tools.person-record-test) (clojure.test/run-tests 'abc.tools.person-record-test)" 2>&1 | tail -10`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add examples/v0/example-persons/000879.json test/abc/tools/person_record_test.clj
git commit -m "$(cat <<'EOF'
feat: example-persons/000879.json fixture for 芥川竜之介

Splits the bibliographic person body out of example-work's metadata-record
into a sibling, content-addressed file. Same field values as before;
relation_to_work is removed (it belongs on the Work-Person edge, see
the next-task contributors[] change).

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: glm-5
EOF
)"
```

---

## Task 5: Revise `schemas/metadata-record.schema.json`

**Files:**
- Modify: `schemas/metadata-record.schema.json`

Replace `persons[]` with `contributors[]`. Drop `$defs/person`. Add `$defs/contributor`.

- [ ] **Step 1: Edit the schema**

Open `schemas/metadata-record.schema.json`. Apply the following edits.

In the `properties` block, replace the `persons` entry:

```json
    "persons": {
      "type": "array",
      "items": { "$ref": "#/$defs/person" },
      "uniqueItems": true,
      "minItems": 1
    },
```

with:

```json
    "contributors": {
      "type": "array",
      "items": { "$ref": "#/$defs/contributor" },
      "uniqueItems": true,
      "minItems": 1
    },
```

In the `required` block, replace `"persons"` with `"contributors"`. The full required array becomes:

```json
  "required": [
    "metadata_record_schema_id",
    "metadata_record_schema_hash",
    "work",
    "contributors"
  ],
```

In `$defs`, delete the entire `"person": { ... }` block.

In `$defs`, add a new `"contributor"` block (alphabetically between `csvProvenance` and `hash` is fine — placement is cosmetic):

```json
    "contributor": {
      "type": "object",
      "additionalProperties": false,
      "required": ["person_id", "person_record_hash", "relation_to_work"],
      "properties": {
        "person_id": { "type": "string", "pattern": "^[0-9]{6}$" },
        "person_record_hash": { "$ref": "#/$defs/hash" },
        "relation_to_work": {
          "type": "string",
          "enum": ["著者", "翻訳者", "編者", "校訂者", "その他"]
        }
      }
    },
```

Update the top-level `description` to reflect the new shape:

```json
  "description": "Bibliographic record for one Aozora work plus contributor references. Identity (record-hash) is SHA-256 over RFC 8785 JCS bytes of this object with source_csv_provenance dropped and contributors[] sorted by person_id (lexicographic ASCII compare). Person bodies live in separate person-record.schema.json files referenced by contributors[i].person_record_hash. Nullable fields are explicit JSON null, not key-omission. Defaults are documentation only; canonicalization hashes the literal record.",
```

- [ ] **Step 2: Confirm the schema is itself valid JSON Schema**

Run:

```bash
clojure -M -e "(require '[abc.tools.files :as f] '[abc.tools.schema :as s]) (let [sch (f/read-json \"schemas/metadata-record.schema.json\")] (s/schema-valid! sch \"schemas/metadata-record.schema.json\"))"
```

Expected: silent success.

- [ ] **Step 3: Do not commit yet — proceed to Task 6 so the code matches the schema**

Subsequent tasks update the metadata-record namespace and the example fixture. The schema and code go in one logical commit at the end of Task 6.

---

## Task 6: Update `abc.tools.metadata-record` for the contributors shape

**Files:**
- Modify: `src/abc/tools/metadata_record.clj`
- Modify: `test/abc/tools/metadata_record_test.clj`

The work-side namespace gets three changes:
1. `canonical-identity-form` sorts `contributors[]` instead of `persons[]`.
2. `build-metadata-record` takes `{:work, :contributors}`.
3. `record->graph` emits work-only triples plus contributor edges (no person bodies).

A new top-level helper `record+persons->graph` composes work + person graphs for the harness.

- [ ] **Step 1: Update existing tests for the new shape**

Open `test/abc/tools/metadata_record_test.clj`. The existing tests reference `persons` throughout. Update test assertions to use `contributors`:

In `record-hash-sorts-persons-test`, rename the test and adjust:

```clojure
(deftest record-hash-sorts-contributors-test
  (testing "contributors[] order does not change the hash"
    (let [r1 @example-record
          contributors (get r1 "contributors")
          r2 (assoc r1 "contributors" (vec (reverse contributors)))]
      (is (= (mr/record-hash r1) (mr/record-hash r2))))))
```

In `record->graph-key-triples-test`, the expected predicate set should drop person predicates (they now come from person-record/record->graph). Replace with:

```clojure
(deftest record->graph-key-triples-test
  (testing "record->graph emits the expected work-side triples"
    (let [g (mr/record->graph @example-record)
          triples (iterator-seq (.find g))
          predicates (set (map #(.getURI (.getPredicate %)) triples))]
      (is (contains? predicates "http://purl.org/dc/terms/title"))
      (is (contains? predicates "http://purl.org/dc/terms/creator"))
      (is (contains? predicates "http://purl.org/dc/terms/identifier")))))
```

In `record->ttl-matches-fixture-test`, the parity now needs the composed graph:

```clojure
(deftest record+persons->ttl-matches-fixture-test
  (testing "compose-graph + ttl matches the committed metadata-record.ttl"
    (let [record @example-record
          persons-by-id {"000879" (files/read-json
                                   "examples/v0/example-persons/000879.json")}
          expected (slurp "examples/v0/example-work/metadata-record.ttl")]
      (is (= expected (mr/record+persons->ttl record persons-by-id))))))
```

(Delete the original `record->ttl-matches-fixture-test`; the new test replaces it.)

In `record-graph-conforms-to-shacl-test`, use the composed graph as the SHACL data graph:

```clojure
(deftest record-graph-conforms-to-shacl-test
  (testing "the example record's combined RDF graph conforms to all metadata shapes"
    (let [shapes ((requiring-resolve 'abc.tools.shacl/load-shapes-graph))
          record @example-record
          persons-by-id {"000879" (files/read-json
                                   "examples/v0/example-persons/000879.json")}
          data (mr/record+persons->graph record persons-by-id)]
      (is (= :ok ((requiring-resolve 'abc.tools.shacl/validate!)
                  {:shapes-graph shapes
                   :data-graph data
                   :label "metadata-record-shape-test"}))))))
```

Add an integration test that demonstrates the hash-cascade — the central design claim:

```clojure
(deftest record-hash-cascades-from-person-hash-test
  (testing "editing a person body changes its hash, which changes any work record that references it"
    (let [work {"work_id" "000999"
                "title" "テスト"
                "title_reading" nil
                "ndc" "NDC 913"
                "orthographic_style" "新字新仮名"
                "copyright_expired" true
                "aozora_available" "2026-01-01"
                "aozora_modified" "2026-01-01"
                "card_url" "https://www.aozora.gr.jp/cards/000999/card999.html"
                "source_editions" [{"title" "x" "publisher" "y"}]}
          ;; Use the actual example person body so we exercise the
          ;; real Person hash, not a fake hash string.
          person-v1 (files/read-json "examples/v0/example-persons/000879.json")
          person-v2 (assoc person-v1 "family_name_romaji" "Akutagawa-changed")
          person-hash-v1 ((requiring-resolve 'abc.tools.person-record/record-hash) person-v1)
          person-hash-v2 ((requiring-resolve 'abc.tools.person-record/record-hash) person-v2)
          contrib-v1 [{"person_id" "000879"
                       "person_record_hash" person-hash-v1
                       "relation_to_work" "著者"}]
          contrib-v2 [{"person_id" "000879"
                       "person_record_hash" person-hash-v2
                       "relation_to_work" "著者"}]
          record-v1 (mr/build-metadata-record {:work work :contributors contrib-v1})
          record-v2 (mr/build-metadata-record {:work work :contributors contrib-v2})]
      (is (not= person-hash-v1 person-hash-v2)
          "person hash must change when family_name_romaji changes")
      (is (not= (mr/record-hash record-v1) (mr/record-hash record-v2))
          "metadata_record_hash must change when contributors[i].person_record_hash changes"))))
```

In `build-metadata-record-shape-test`, update the fragment to use `:contributors`:

Read the existing test (search the file). Replace `:persons` argument with a contributors-style argument:

```clojure
(deftest build-metadata-record-shape-test
  (testing "build-metadata-record produces a schema-compliant value"
    (let [work {"work_id" "000127"
                "title" "羅生門"
                ;; ... existing work fields ...
                }
          contributors [{"person_id" "000879"
                         "person_record_hash" "sha256:0000000000000000000000000000000000000000000000000000000000000000"
                         "relation_to_work" "著者"}]
          record (mr/build-metadata-record {:work work :contributors contributors})]
      (is (= :ok (mr/validate! record))))))
```

(Read the current test before editing to preserve the work fields; only change the input/output keyword names from persons → contributors.)

- [ ] **Step 2: Run the tests; expect failures from the renamed/replaced functions**

Run: `clojure -M:test -e "(require 'abc.tools.metadata-record-test) (clojure.test/run-tests 'abc.tools.metadata-record-test)" 2>&1 | tail -20`
Expected: FAIL — `record+persons->graph`, `record+persons->ttl` unresolved; `build-metadata-record` argument shape mismatch; possibly the example record load fails because the fixture still has `persons[]`.

The fixture-related failures will resolve at Task 7 once the example metadata-record.json is regenerated. The code-related failures resolve in Step 3 below.

- [ ] **Step 3: Update `src/abc/tools/metadata_record.clj`**

Replace the file's body (lines 22+) so that:

(a) `canonical-identity-form` operates on `contributors`:

```clojure
(defn- canonical-identity-form
  "Returns the JSON value used as input to record-hash. Drops
  source_csv_provenance and sorts contributors[] by person_id."
  [record]
  (-> record
      (dissoc "source_csv_provenance")
      (update "contributors"
              (fn [cs] (vec (sort-by #(get % "person_id") cs))))))
```

(b) `build-metadata-record` takes `{:work :contributors}`:

```clojure
(defn build-metadata-record
  "Construct an immutable metadata record from {:work, :contributors}.
  Populates the self-describing schema fields. contributors are sorted
  by person_id. Caller is expected to validate! the result before
  hashing or shipping."
  [{:keys [work contributors]}]
  {"metadata_record_schema_id" schema-id
   "metadata_record_schema_hash" (manifest/schema-hash schema-path)
   "work" work
   "contributors" (vec (sort-by #(get % "person_id") contributors))})
```

(c) `record->graph` emits work-side triples only (contributor edges, no person bodies). Person bodies were already moved to `person-record` in Task 3. Keep `card-iri`, `->date-literal`, `->int-literal`, `->bool-literal`, `dcndl-ndc-datatype`, `ndc-literal`, `title-blank-node`. Replace `work-data` so the contributors come from `contributors[]` and reference person IRIs without inlining bodies. The function uses `person-record/person-iri` to produce contributor IRIs.

(The `[abc.tools.person-record :as person-record]` require was already added in Task 3.)

Replace `work-data`, `contributor-blank`, and the existing `record->graph`:

```clojure
(defn- contributor-blank [contributor]
  {:rdf/about (str "<" (person-record/person-iri (get contributor "person_id")) ">")
   :dcterms/role (get contributor "relation_to_work")})

(defn- work-data [work contributors]
  (let [iri (card-iri (get work "card_url"))
        authors (filter #(= "著者" (get % "relation_to_work")) contributors)
        others (remove #(= "著者" (get % "relation_to_work")) contributors)]
    (cond-> {:rdf/about iri
             :rdf/type [:bibo/Document :schema/CreativeWork]
             :dcterms/identifier (->int-literal (get work "work_id"))
             :dcterms/title (title-blank-node work)
             :dc/subject (ndc-literal (get work "ndc"))
             :abc/orthographicStyle (get work "orthographic_style")
             :abc/copyrightExpired (->bool-literal (get work "copyright_expired"))
             :dcterms/available (->date-literal (get work "aozora_available"))
             :dcterms/modified (->date-literal (get work "aozora_modified"))}
      (seq authors)
      (assoc :dcterms/creator (mapv #(str "<" (person-record/person-iri (get % "person_id")) ">")
                                    authors))
      (seq others)
      (assoc :dcterms/contributor (mapv contributor-blank others)))))

(defn record->graph
  "Build a Jena graph from a metadata record. Emits only work-side
  triples plus contributor edges; person bodies live in
  abc.tools.person-record/record->graph and are composed by
  record+persons->graph."
  [record]
  (rdf-prefixes/ensure!)
  (let [work (get record "work")
        contributors (get record "contributors")
        graph (aa/graph :simple)]
    (aa/add graph (work-data work contributors))
    graph))
```

Delete the original `person-data` definition (it now lives in person-record.clj).

(d) Add the compose helpers:

```clojure
(defn record+persons->graph
  "Compose a metadata record with its resolved person bodies into a
  single graph: work + contributor edges + each person's full body
  triples. `persons-by-id` maps person_id → person-record map."
  [record persons-by-id]
  (let [graph (record->graph record)]
    (doseq [contributor (get record "contributors")
            :let [pid (get contributor "person_id")
                  body (get persons-by-id pid)]]
      (when-not body
        (throw (ex-info (str "no resolved person body for person_id " pid)
                        {:person-id pid
                         :persons-by-id (vec (keys persons-by-id))})))
      (let [person-graph (person-record/record->graph body)]
        (.add graph (.find person-graph))))
    graph))

(defn record+persons->ttl
  "Convenience: compose work and persons, render Turtle."
  [record persons-by-id]
  (manifest-to-rdf/graph->ttl (record+persons->graph record persons-by-id)))
```

(d) **Explicitly delete `record->ttl`.** Find and remove the existing `(defn record->ttl ...)` block at the bottom of the file (the harness now uses `record+persons->ttl`).

(e) **Grep for stale callers of removed functions before committing:**

```bash
grep -rn "metadata-record/record->ttl\b\|metadata-record/person-data\b" src test
```

Expected: no matches in `src/` or `test/`. If anything turns up, update the caller to use `record+persons->ttl` (or the equivalent persons-aware path).

- [ ] **Step 4: Run the tests; expect them to fail because the fixture still has `persons[]`**

Run: `clojure -M:test -e "(require 'abc.tools.metadata-record-test) (clojure.test/run-tests 'abc.tools.metadata-record-test)" 2>&1 | tail -10`
Expected: FAIL on tests that exercise `@example-record` — schema validation rejects `persons[]`. This is fine; Task 7 regenerates the fixture and these tests will pass.

- [ ] **Step 5: Do not commit yet — proceed to Task 7**

The schema, code, fixture, and TTL fixture move together in the next task's commit.

---

## Task 7: Regenerate `metadata-record.json`, `manifest.json`, `metadata-record.ttl`

**Files:**
- Modify: `examples/v0/example-work/metadata-record.json`
- Modify: `examples/v0/example-work/metadata-record.ttl`
- Modify: `examples/v0/example-work/manifest.json`

- [ ] **Step 1: Compute the new metadata-record-hash from the planned record**

The new record's body is the existing work block plus a contributors[] referencing the person fixture's hash. Compute the person hash first, then build and write the metadata-record:

```bash
clojure -M -e "
  (require '[abc.tools.files :as f] '[abc.tools.json :as j] '[abc.tools.manifest :as m]
           '[abc.tools.person-record :as pr] '[abc.tools.metadata-record :as mr]
           '[clojure.java.io :as io])
  (let [person (f/read-json \"examples/v0/example-persons/000879.json\")
        person-hash (pr/record-hash person)
        existing (f/read-json \"examples/v0/example-work/metadata-record.json\")
        work (get existing \"work\")
        prov (get existing \"source_csv_provenance\")
        contributors [{\"person_id\" \"000879\"
                       \"person_record_hash\" person-hash
                       \"relation_to_work\" \"著者\"}]
        record (cond-> {\"metadata_record_schema_id\" \"https://w3id.org/abc/schemas/metadata-record.schema.json\"
                        \"metadata_record_schema_hash\" (m/schema-hash \"schemas/metadata-record.schema.json\")
                        \"work\" work
                        \"contributors\" contributors}
                 prov (assoc \"source_csv_provenance\" prov))]
    (mr/validate! record)
    (j/write-deterministic-json-file! (io/file \"examples/v0/example-work/metadata-record.json\") record)
    (println \"metadata_record_hash:\" (mr/record-hash record)))"
```

Expected: prints `metadata_record_hash: sha256:<hex>` and writes the new metadata-record.json. Note this hash; you need it to update manifest.json next.

- [ ] **Step 2: Update `examples/v0/example-work/manifest.json`**

Open `examples/v0/example-work/manifest.json`. Replace the `metadata_record_hash` value inside `manifest_identity_object` with the value printed in Step 1. Then recompute the top-level `artifact_id`:

```bash
clojure -M -e "
  (require '[abc.tools.files :as f] '[abc.tools.hash :as h] '[abc.tools.json :as j]
           '[clojure.java.io :as io])
  (let [m (f/read-json \"examples/v0/example-work/manifest.json\")
        identity-obj (get m \"manifest_identity_object\")
        new-id (h/format-sha256 (h/sha256-json-jcs identity-obj))
        updated (assoc m \"artifact_id\" new-id)]
    (j/write-deterministic-json-file! (io/file \"examples/v0/example-work/manifest.json\") updated)
    (println \"artifact_id:\" new-id))"
```

Expected: prints the new artifact_id and rewrites the file.

- [ ] **Step 3: Regenerate `metadata-record.ttl`**

```bash
clojure -M -e "
  (require '[abc.tools.files :as f] '[abc.tools.metadata-record :as mr]
           '[clojure.java.io :as io])
  (let [record (f/read-json \"examples/v0/example-work/metadata-record.json\")
        person (f/read-json \"examples/v0/example-persons/000879.json\")
        ttl (mr/record+persons->ttl record {\"000879\" person})]
    (spit (io/file \"examples/v0/example-work/metadata-record.ttl\") ttl)
    (println \"wrote ttl, byte length:\" (count (.getBytes ^String ttl \"UTF-8\"))))"
```

Expected: rewrites the .ttl file. Inspect it to confirm work + person triples are present.

- [ ] **Step 4: Run all metadata-record tests**

Run: `clojure -M:test -e "(require 'abc.tools.metadata-record-test) (clojure.test/run-tests 'abc.tools.metadata-record-test)" 2>&1 | tail -15`
Expected: PASS for every test in the namespace.

- [ ] **Step 5: Commit the schema + code + fixtures together**

```bash
git add schemas/metadata-record.schema.json src/abc/tools/metadata_record.clj test/abc/tools/metadata_record_test.clj examples/v0/example-work/metadata-record.json examples/v0/example-work/metadata-record.ttl examples/v0/example-work/manifest.json
git commit -m "$(cat <<'EOF'
feat: metadata-record contributors[] replaces persons[]

Person bodies move to examples/v0/example-persons/ (separate milestone
artifact); the metadata-record now references each contributor by
{person_id, person_record_hash, relation_to_work}. The work's
identity hash recomposes through contributors[] sorted by person_id.

abc.tools.metadata-record gets:
- canonical-identity-form sorts contributors[]
- build-metadata-record takes {:work, :contributors}
- record->graph emits work-side + contributor-edge triples only
- record+persons->graph composes a work + resolved-persons graph
- record+persons->ttl renders the composed Turtle

The example fixture and its TTL/manifest are regenerated; manifest.json's
metadata_record_hash and artifact_id are recomputed.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: glm-5
EOF
)"
```

---

## Task 8: Update `abc.tools.tei-header/build` to take resolved contributors

**Files:**
- Modify: `src/abc/tools/tei_header.clj`
- Modify: `test/abc/tools/tei_header_test.clj`

`build` currently destructures `(:strs [work persons])`. The new shape is `{:work, :contributors [{:relation-to-work, :person <body>}]}` — keyword keys at the outer level, but person body retains string keys (it's a JSON-shaped map).

- [ ] **Step 1: Update the test to use the new shape**

Read `test/abc/tools/tei_header_test.clj`. The current tests build a record map with `"work"` + `"persons"` and call `(tei-header/build record)`.

Replace those construction sites with the new shape. Each test that constructs `{...}` and calls `build` should now build:

```clojure
(let [work {...}
      person {...}  ; a single resolved person record
      input {:work work
             :contributors [{:relation-to-work "著者" :person person}]}]
  (tei-header/build input))
```

For the parity test (which loads `examples/v0/example-work/metadata-record.json` and the example person fixture), construct the input by reading the new metadata-record + the person fixture:

```clojure
(deftest build-roundtrip-byte-equal-to-fixture-test
  (testing "the rendered TEI header matches the committed tei.xml header"
    (let [record (files/read-json "examples/v0/example-work/metadata-record.json")
          person (files/read-json "examples/v0/example-persons/000879.json")
          contributors (mapv (fn [c]
                               {:relation-to-work (get c "relation_to_work")
                                :person person}) ; only one contributor in this fixture
                             (get record "contributors"))
          hiccup (tei-header/build {:work (get record "work")
                                    :contributors contributors})
          rendered (tei-header/emit-xml hiccup)
          tei-xml (slurp "examples/v0/example-work/tei.xml")]
      ;; The fixture's tei.xml has additional content beyond the teiHeader
      ;; (it embeds the rendered header + body). Assert that the rendered
      ;; teiHeader is a substring of the fixture's tei.xml.
      (is (string? rendered))
      (is (.contains tei-xml "<teiHeader")))))
```

Adjust shape-only tests (the ones that don't touch the real fixture) accordingly.

- [ ] **Step 2: Confirm tests fail because `build` still destructures the old shape**

Run: `clojure -M:test -e "(require 'abc.tools.tei-header-test) (clojure.test/run-tests 'abc.tools.tei-header-test)" 2>&1 | tail -10`
Expected: FAIL — destructure mismatch.

- [ ] **Step 3: Refactor helpers so role is a separate argument; update `build`**

Open `src/abc/tools/tei_header.clj`. The internal helpers currently consume "person" maps that have `relation_to_work` braided in. Refactor so role is passed alongside the person body, never embedded in it. Replace `resp-stmt`, `title-stmt`, and `build`:

```clojure
(defn- resp-stmt
  "TEI <respStmt> with the role string and the person's name block."
  [role person]
  (into [:respStmt
         [:resp role]]
        (person-name-block person)))

(defn- title-stmt [work contributors]
  (let [authors (filter #(= "著者" (:relation-to-work %)) contributors)
        others (remove #(= "著者" (:relation-to-work %)) contributors)
        title (get work "title")
        title-r (get work "title_reading")]
    (-> [:titleStmt
         [:title {:type "main" :xml/lang "ja"} title]]
        (cond-> title-r
          (conj [:title {:type "reading" :xml/lang "ja-Hira"} title-r]))
        (into (mapv #(author-block (:person %)) authors))
        (into (mapv #(resp-stmt (:relation-to-work %) (:person %)) others)))))

(defn- file-desc [work contributors]
  [:fileDesc
   (title-stmt work contributors)
   (publication-stmt work)
   (source-desc work)])

(defn build
  "Return a TEI <teiHeader> as hiccup-style nested vectors. Pure;
  no clojure.data.xml coupling at this boundary.

  Input shape:
    {:work         <work map, string keys>
     :contributors [{:relation-to-work \"...\" :person <person map, string keys>} ...]}

  Role and person are kept separate at every level inside this builder;
  the relation_to_work value never enters the person body."
  [{:keys [work contributors]}]
  [:teiHeader
   (file-desc work contributors)
   (encoding-desc)
   (profile-desc work)])
```

`person-name-block` and `author-block` stay unchanged — they only consume the person body and don't touch role.

- [ ] **Step 4: Run the tei-header tests; expect pass**

Run: `clojure -M:test -e "(require 'abc.tools.tei-header-test) (clojure.test/run-tests 'abc.tools.tei-header-test)" 2>&1 | tail -10`
Expected: PASS for the test namespace.

- [ ] **Step 5: Verify `examples/v0/example-work/tei.xml` is byte-identical to the regenerated header**

The TEI body and header content semantics are unchanged. Run the design-bundle harness which validates `tei.xml`:

```bash
nix run .#validate-design-bundle 2>&1 | grep -E "tei|TEI" | tail -10
```

Expected: harness still validates the existing `tei.xml`. (The harness regen step happens later; the file's bytes don't need to change just because the builder's input shape did.)

If the `tei-header-test` parity test fails because the fixture's bytes shift, regenerate `examples/v0/example-work/tei.xml` from the same code path used by the metadata milestone. The regen pattern is: build a hiccup tree with the new builder shape, emit XML, splice into the existing tei.xml, replacing only the `<teiHeader>...</teiHeader>` block while preserving the body and closing tags. Run:

```bash
clojure -M -e "
  (require '[abc.tools.files :as f] '[abc.tools.tei-header :as th] '[clojure.string :as str])
  (let [record (f/read-json \"examples/v0/example-work/metadata-record.json\")
        person (f/read-json \"examples/v0/example-persons/000879.json\")
        contributors (mapv (fn [c]
                             {:relation-to-work (get c \"relation_to_work\")
                              :person person})
                           (get record \"contributors\"))
        hiccup (th/build {:work (get record \"work\")
                          :contributors contributors})
        header-xml (th/emit-xml hiccup)]
    (println header-xml))" 2>&1 | head -50
```

Inspect the printed header. If the header bytes differ from what's in `examples/v0/example-work/tei.xml`, splice the new header into the existing tei.xml (preserving body/closing tags) by hand or via a small in-place edit, then verify with `nix run .#validate-design-bundle`.

- [ ] **Step 6: Commit**

```bash
git add src/abc/tools/tei_header.clj test/abc/tools/tei_header_test.clj examples/v0/example-work/tei.xml
git commit -m "$(cat <<'EOF'
refactor: tei-header/build takes resolved-contributors shape

build now accepts {:work, :contributors [{:relation-to-work, :person}]}.
Caller resolves person_id → body before invoking; the builder is pure
and knows nothing about person_id, hashes, or filesystem layout. An
internal contributor→legacy-person adapter keeps the string-keyed helpers
unchanged so the rendered bytes are stable.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: glm-5
EOF
)"
```

---

## Task 9: Split aozora-csv parsing into person body + contributor entry

**Files:**
- Modify: `src/abc/tools/aozora_csv.clj`
- Modify: `test/abc/tools/aozora_csv_test.clj`

`parse-person-fields-from-row` currently includes `relation_to_work` in the person body. Split into:
- `parse-person-fields-from-row` — person body, no relation_to_work.
- `parse-contributor-from-row` — `{"person_id", "relation_to_work"}` only.
- `build-record-fragment-from-rows` — return `{:work, :persons-by-id, :contributors}` instead of `{:work, :persons}`.

- [ ] **Step 1: Add a failing test for the new fragment shape**

Read `test/abc/tools/aozora_csv_test.clj`. Note the existing `build-record-fragment-from-rows` test pattern. Add or replace a focused test:

```clojure
(deftest build-record-fragment-shape-test
  (testing "build-record-fragment-from-rows returns {:work, :persons-by-id, :contributors}"
    (let [rows [{"作品ID" "000127"
                 "人物ID" "000879"
                 "役割フラグ" "著者"
                 "作品名" "羅生門"
                 ;; minimal viable row — fill remaining required columns
                 ;; with empty strings so the parser is exercised.
                 "作品名読み" "らしょうもん"
                 "ソート用読み" "らしようもん"
                 "副題" "" "副題読み" "" "原題" "" "初出" ""
                 "分類番号" "NDC 913" "文字遣い種別" "新字新仮名"
                 "作品著作権フラグ" "なし" "公開日" "1997-10-29"
                 "最終更新日" "2022-07-16" "図書カードURL" "https://example/"
                 "底本名1" "" "底本出版社名1" ""
                 "底本名2" "" "底本出版社名2" ""
                 "姓" "芥川" "名" "竜之介"
                 "姓読み" "" "名読み" ""
                 "姓読みソート用" "" "名読みソート用" ""
                 "姓ローマ字" "" "名ローマ字" ""
                 "生年月日" "" "没年月日" ""
                 "人物著作権フラグ" "なし"}]
          frag (ac/build-record-fragment-from-rows rows)]
      (is (= "000127" (get-in frag [:work "work_id"])))
      (is (contains? (:persons-by-id frag) "000879"))
      (is (= "芥川" (get-in frag [:persons-by-id "000879" "family_name"])))
      (is (= [{"person_id" "000879" "relation_to_work" "著者"}]
             (:contributors frag)))
      (testing "the person body must NOT carry relation_to_work"
        (is (not (contains? (get-in frag [:persons-by-id "000879"])
                            "relation_to_work")))))))
```

(Add `[abc.tools.aozora-csv :as ac]` to the require if the existing tests use a different alias.)

- [ ] **Step 2: Run; expect failure**

Run: `clojure -M:test -e "(require 'abc.tools.aozora-csv-test) (clojure.test/run-tests 'abc.tools.aozora-csv-test)" 2>&1 | tail -15`
Expected: FAIL on `build-record-fragment-shape-test` because the fragment still has `:persons` and the person body still has `relation_to_work`.

- [ ] **Step 3: Update `aozora_csv.clj`**

Open `src/abc/tools/aozora_csv.clj`. Replace the current `parse-person-fields-from-row` (lines 99-113) so it no longer includes `relation_to_work`:

```clojure
(defn parse-person-fields-from-row [row]
  {"person_id" (get row "人物ID")
   "family_name" (nfc (get row "姓"))
   "given_name" (nfc (get row "名"))
   "family_name_reading" (nullable (get row "姓読み"))
   "given_name_reading" (nullable (get row "名読み"))
   "family_name_sort" (nullable (get row "姓読みソート用"))
   "given_name_sort" (nullable (get row "名読みソート用"))
   "family_name_romaji" (nullable (get row "姓ローマ字"))
   "given_name_romaji" (nullable (get row "名ローマ字"))
   "date_of_birth" (nullable (get row "生年月日"))
   "date_of_death" (nullable (get row "没年月日"))
   "person_copyright_expired" (parse-bool-flag (get row "人物著作権フラグ"))
   "external_links" []})
```

Add a contributor extractor:

```clojure
(defn parse-contributor-from-row [row]
  {"person_id" (get row "人物ID")
   "relation_to_work" (get row "役割フラグ")})
```

Replace `build-record-fragment-from-rows` (lines 115-130) so it returns the new fragment shape:

```clojure
(defn build-record-fragment-from-rows
  "Given multiple CSV rows for the same work_id (one per author/role),
  return {:work, :persons-by-id, :contributors}. Asserts work-level
  fields are consistent across rows; persons-by-id is keyed by
  person_id; contributors are sorted by person_id."
  [rows]
  (assert (seq rows) "build-record-fragment-from-rows requires at least one row")
  (let [works (mapv parse-work-fields-from-row rows)
        work-ids (distinct (map #(get % "work_id") works))]
    (assert (= 1 (count work-ids))
            (str "rows must share work_id; got: " (vec work-ids)))
    (let [person-bodies-by-id
          (->> rows
               (mapv parse-person-fields-from-row)
               (group-by #(get % "person_id"))
               (into {}
                     (map (fn [[pid xs]]
                            (let [unique (distinct xs)]
                              (when (< 1 (count unique))
                                (throw (ex-info
                                        (str "person_id " pid
                                             " has divergent bodies across CSV rows")
                                        {:person-id pid
                                         :bodies unique})))
                              [pid (first unique)])))))
          contributors (->> rows
                            (mapv parse-contributor-from-row)
                            (sort-by #(get % "person_id"))
                            distinct
                            vec)]
      {:work (first works)
       :persons-by-id person-bodies-by-id
       :contributors contributors})))
```

- [ ] **Step 4: Run aozora-csv tests**

Run: `clojure -M:test -e "(require 'abc.tools.aozora-csv-test) (clojure.test/run-tests 'abc.tools.aozora-csv-test)" 2>&1 | tail -15`
Expected: PASS for the new `build-record-fragment-shape-test`. Other existing tests in this namespace may fail if they assert against the old `:persons` key — update them inline by replacing `(:persons frag)` references with `(get-in frag [:persons-by-id person-id])` or `(:contributors frag)` as appropriate.

- [ ] **Step 5: Commit**

```bash
git add src/abc/tools/aozora_csv.clj test/abc/tools/aozora_csv_test.clj
git commit -m "$(cat <<'EOF'
refactor: aozora-csv splits person body from contributor entry

parse-person-fields-from-row no longer includes relation_to_work;
parse-contributor-from-row carries only person_id + role.
build-record-fragment-from-rows now returns
{:work, :persons-by-id, :contributors} so callers can write Person
files and Work contributor lists from the same parse pass.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: glm-5
EOF
)"
```

---

## Task 10: Ingester N+1 outputs, `--persons-output-dir`, `--overwrite`, corruption-safe idempotent check

**Files:**
- Modify: `src/abc/tools/aozora_ingest.clj`
- Modify or Create: `test/abc/tools/aozora_ingest_test.clj`

Before editing, check whether `test/abc/tools/aozora_ingest_test.clj` exists:

```bash
test -f test/abc/tools/aozora_ingest_test.clj && echo "exists" || echo "missing"
```

If missing, create it. The plan below assumes a fresh file; merge with existing tests if present.

- [ ] **Step 1: Write failing tests using a synthetic CSV (no live ZIP dependency)**

The ingester is split internally into `run-from-rows!` (takes pre-parsed CSV rows + opts; does all I/O *except* reading the ZIP) and `run!` (CLI entry that reads the ZIP and delegates). Tests exercise `run-from-rows!` with a synthetic row list, so the test suite is fast and runs in the sealed Nix sandbox.

Write `test/abc/tools/aozora_ingest_test.clj`:

```clojure
(ns abc.tools.aozora-ingest-test
  (:require [abc.tools.aozora-csv :as ac]
            [abc.tools.aozora-ingest :as ingest]
            [abc.tools.files :as files]
            [abc.tools.person-record :as pr]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir [prefix]
  (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- delete-recursive [^java.io.File f]
  (when (.isDirectory f)
    (doseq [child (.listFiles f)] (delete-recursive child)))
  (.delete f))

;; A minimal synthetic CSV row covering the fields the parser reads.
;; Unknown columns are empty strings; the parser tolerates that.
(defn- row [overrides]
  (merge {"作品ID" "000127"
          "人物ID" "000879"
          "役割フラグ" "著者"
          "作品名" "羅生門"
          "作品名読み" "らしょうもん"
          "ソート用読み" "らしようもん"
          "副題" "" "副題読み" "" "原題" "" "初出" ""
          "分類番号" "NDC 913" "文字遣い種別" "新字新仮名"
          "作品著作権フラグ" "なし" "公開日" "1997-10-29"
          "最終更新日" "2022-07-16"
          "図書カードURL" "https://www.aozora.gr.jp/cards/000879/card127.html"
          "底本名1" "" "底本出版社名1" ""
          "底本名2" "" "底本出版社名2" ""
          "底本初版発行年1" "" "底本初版発行年2" ""
          "入力に使用した版1" "" "入力に使用した版2" ""
          "校正に使用した版1" "" "校正に使用した版2" ""
          "底本の親本名1" "" "底本の親本名2" ""
          "底本の親本出版社名1" "" "底本の親本出版社名2" ""
          "底本の親本初版発行年1" "" "底本の親本初版発行年2" ""
          "姓" "芥川" "名" "竜之介"
          "姓読み" "あくたがわ" "名読み" "りゅうのすけ"
          "姓読みソート用" "あくたかわ" "名読みソート用" "りゆうのすけ"
          "姓ローマ字" "Akutagawa" "名ローマ字" "Ryunosuke"
          "生年月日" "1892-03-01" "没年月日" "1927-07-24"
          "人物著作権フラグ" "なし"}
         overrides))

(def synthetic-rows [(row {})])

;; The work fixture in the synthetic CSV needs at least one source-edition
;; row to satisfy the metadata-record schema's minItems: 1. Add one.
(def synthetic-rows-with-edition
  [(row {"底本名1" "羅生門" "底本出版社名1" "テスト出版社"})])

(deftest ingest-emits-n-plus-1-files-test
  (testing "run-from-rows! writes the work record + each person file"
    (let [work-dir (temp-dir "abc-ingest-work")
          persons-dir (temp-dir "abc-ingest-persons")]
      (try
        (ingest/run-from-rows!
         {:rows synthetic-rows-with-edition
          :work-id "000127"
          :output (str (io/file work-dir "metadata-record.json"))
          :persons-output-dir (str persons-dir)})
        (is (.exists (io/file work-dir "metadata-record.json")))
        (is (.exists (io/file persons-dir "000879.json")))
        (let [person (files/read-json (str (io/file persons-dir "000879.json")))]
          (is (= :ok (pr/validate! person)))
          (is (= "000879" (get person "person_id"))))
        (finally
          (delete-recursive work-dir)
          (delete-recursive persons-dir))))))

(deftest ingest-deterministic-test
  (testing "two runs with the same inputs produce byte-identical outputs"
    (let [d1-work (temp-dir "abc-ingest-w1")
          d1-persons (temp-dir "abc-ingest-p1")
          d2-work (temp-dir "abc-ingest-w2")
          d2-persons (temp-dir "abc-ingest-p2")
          opts (fn [w p] {:rows synthetic-rows-with-edition
                          :work-id "000127"
                          :output (str (io/file w "metadata-record.json"))
                          :persons-output-dir (str p)})]
      (try
        (ingest/run-from-rows! (opts d1-work d1-persons))
        (ingest/run-from-rows! (opts d2-work d2-persons))
        (is (= (slurp (io/file d1-work "metadata-record.json"))
               (slurp (io/file d2-work "metadata-record.json"))))
        (is (= (slurp (io/file d1-persons "000879.json"))
               (slurp (io/file d2-persons "000879.json"))))
        (finally
          (delete-recursive d1-work) (delete-recursive d1-persons)
          (delete-recursive d2-work) (delete-recursive d2-persons))))))

(deftest ingest-refuse-overwrite-on-divergent-person-test
  (testing "with a divergent on-disk person file and no --overwrite, ingest fails"
    (let [work-dir (temp-dir "abc-ingest-w")
          persons-dir (temp-dir "abc-ingest-p")]
      (try
        (spit (io/file persons-dir "000879.json")
              (str "{\"person_record_schema_id\":\"https://w3id.org/abc/schemas/person-record.schema.json\","
                   "\"person_record_schema_hash\":\"sha256:0000000000000000000000000000000000000000000000000000000000000000\","
                   "\"person_id\":\"000879\","
                   "\"family_name\":\"divergent\","
                   "\"given_name\":\"divergent\","
                   "\"person_copyright_expired\":true,"
                   "\"external_links\":[]}"))
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo #"000879"
             (ingest/run-from-rows!
              {:rows synthetic-rows-with-edition
               :work-id "000127"
               :output (str (io/file work-dir "metadata-record.json"))
               :persons-output-dir (str persons-dir)})))
        (finally
          (delete-recursive work-dir)
          (delete-recursive persons-dir))))))

(deftest ingest-overwrite-flag-replaces-divergent-test
  (testing "with --overwrite, a divergent on-disk person file is replaced"
    (let [work-dir (temp-dir "abc-ingest-w")
          persons-dir (temp-dir "abc-ingest-p")]
      (try
        (spit (io/file persons-dir "000879.json")
              (str "{\"person_record_schema_id\":\"https://w3id.org/abc/schemas/person-record.schema.json\","
                   "\"person_record_schema_hash\":\"sha256:0000000000000000000000000000000000000000000000000000000000000000\","
                   "\"person_id\":\"000879\","
                   "\"family_name\":\"divergent\","
                   "\"given_name\":\"divergent\","
                   "\"person_copyright_expired\":true,"
                   "\"external_links\":[]}"))
        (ingest/run-from-rows!
         {:rows synthetic-rows-with-edition
          :work-id "000127"
          :output (str (io/file work-dir "metadata-record.json"))
          :persons-output-dir (str persons-dir)
          :overwrite true})
        (is (= "芥川" (get (files/read-json (str (io/file persons-dir "000879.json")))
                          "family_name")))
        (finally
          (delete-recursive work-dir)
          (delete-recursive persons-dir))))))

(deftest ingest-corruption-safe-fails-on-unparseable-test
  (testing "an unparseable on-disk person file fails ingest with a loud error"
    (let [work-dir (temp-dir "abc-ingest-w")
          persons-dir (temp-dir "abc-ingest-p")]
      (try
        (spit (io/file persons-dir "000879.json") "not-json{{{")
        (is (thrown? clojure.lang.ExceptionInfo
                     (ingest/run-from-rows!
                      {:rows synthetic-rows-with-edition
                       :work-id "000127"
                       :output (str (io/file work-dir "metadata-record.json"))
                       :persons-output-dir (str persons-dir)})))
        (finally
          (delete-recursive work-dir)
          (delete-recursive persons-dir))))))
```

A separate end-to-end live-ZIP test lives in Step 6 below as a manual command, not a unit test.

- [ ] **Step 2: Run the tests; expect failures because `ingest/run!` does not exist yet**

Run: `clojure -M:test -e "(require 'abc.tools.aozora-ingest-test) (clojure.test/run-tests 'abc.tools.aozora-ingest-test)" 2>&1 | tail -15`
Expected: FAIL — `ingest/run!` unresolved (the existing `-main` is the only entry point; we are introducing a programmatic `run!`).

- [ ] **Step 3: Rewrite `aozora_ingest.clj` with the new behaviors**

Open `src/abc/tools/aozora_ingest.clj`. Replace its body so it:

(a) Imports `abc.tools.person-record` and `abc.tools.metadata-record`.
(b) Builds the work record and person bodies from the new aozora-csv fragment shape.
(c) Writes person files with the corruption-safe idempotent check.
(d) Writes the work record using deterministic JSON.
(e) Exposes a programmatic `run!` function plus the existing `-main`.

Replace lines 17-end (everything after the require/import) with:

```clojure
(def schema-path "schemas/metadata-record.schema.json")
(def schema-id "https://w3id.org/abc/schemas/metadata-record.schema.json")
(def person-schema-path "schemas/person-record.schema.json")
(def person-schema-id "https://w3id.org/abc/schemas/person-record.schema.json")

(defn- read-zip-csv [^String zip-path]
  (with-open [zf (ZipFile. (io/file zip-path))]
    (let [csv-entry (->> (enumeration-seq (.entries zf))
                         (filter (fn [^ZipEntry e]
                                   (string/ends-with? (.getName e) ".csv")))
                         first)]
      (when-not csv-entry
        (throw (ex-info (str "no .csv entry in " zip-path)
                        {:zip-path zip-path})))
      (slurp (.getInputStream zf csv-entry)))))

(def ^:private person-body-keys
  ["person_id"
   "family_name"
   "given_name"
   "family_name_reading"
   "given_name_reading"
   "family_name_sort"
   "given_name_sort"
   "family_name_romaji"
   "given_name_romaji"
   "date_of_birth"
   "date_of_death"
   "person_copyright_expired"
   "external_links"])

(defn- build-person-record
  "Wrap a parsed person body with the self-describing schema fields.
  The body contributes only the whitelisted bibliographic keys; this
  protects identity hashing from accidental field bleed-through if the
  upstream parser ever emits extra columns."
  [body]
  (merge (zipmap person-body-keys (map #(get body %) person-body-keys))
         {"person_record_schema_id" person-schema-id
          "person_record_schema_hash" (manifest/schema-hash person-schema-path)
          "external_links" (or (get body "external_links") [])}))

(defn- write-person-file!
  "Write the person record at <persons-dir>/<person_id>.json with the
  corruption-safe idempotent check. If the file already exists:
  - parse it as JSON (fail loudly if unparseable)
  - validate against the schema (fail loudly if invalid)
  - recompute its hash; if same as the just-built record, it's a
    no-op rewrite. If different and overwrite? is false, throw.
    If overwrite? is true, replace."
  [persons-dir record overwrite?]
  (let [pid (get record "person_id")
        target (io/file persons-dir (str pid ".json"))
        new-hash (person-record/record-hash record)]
    (.mkdirs (io/file persons-dir))
    (if (.exists target)
      (let [existing (try (files/read-json (str target))
                          (catch Exception e
                            (throw (ex-info (str "on-disk person file " target
                                                 " is unparseable JSON")
                                            {:person-id pid
                                             :path (str target)}
                                            e))))]
        (person-record/validate! existing)
        (let [existing-hash (person-record/record-hash existing)]
          (cond
            (= new-hash existing-hash)
            (json/write-deterministic-json-file! target record)

            overwrite?
            (json/write-deterministic-json-file! target record)

            :else
            (throw (ex-info
                    (str "refuse to overwrite " target
                         "; on-disk person_record_hash " existing-hash
                         " differs from rebuilt " new-hash
                         ". Pass --overwrite to replace.")
                    {:person-id pid
                     :path (str target)
                     :existing-hash existing-hash
                     :new-hash new-hash})))))
      (json/write-deterministic-json-file! target record))
    new-hash))

(defn- self-consistency-check! [record]
  (let [s (files/read-json schema-path)
        errors (schema/validation-errors s record)]
    (when (seq errors)
      (throw (ex-info "generated metadata-record fails schema validation"
                      {:errors errors}))))
  (let [embedded (get record "metadata_record_schema_hash")
        live (manifest/schema-hash schema-path)]
    (when-not (= embedded live)
      (throw (ex-info "self-consistency: embedded schema hash differs from live"
                      {:embedded embedded :live live}))))
  :ok)

(defn run-from-rows!
  "Programmatic entry that operates on already-parsed CSV rows. Writes
  N+1 deterministic JSON files (the work metadata-record + each
  contributor person record) and returns the new metadata_record_hash.

  Required keys: :rows (seq of CSV-row maps), :work-id, :output.
  Optional: :persons-output-dir (defaults to '<output-dir>/persons'),
            :overwrite (boolean, default false)."
  [{:keys [rows work-id output persons-output-dir overwrite]}]
  (let [matching (filter #(= work-id (get % "作品ID")) rows)]
    (when-not (seq matching)
      (throw (ex-info (str "no rows for work_id " work-id " in supplied rows")
                      {:work-id work-id})))
    (let [{:keys [work persons-by-id contributors]} (ac/build-record-fragment-from-rows matching)
          persons-dir (or persons-output-dir
                          (str (.getParent (io/file output)) "/persons"))
          contributor-entries
          (vec
           (for [c contributors
                 :let [pid (get c "person_id")
                       body (get persons-by-id pid)
                       record (build-person-record body)
                       hash (write-person-file! persons-dir record overwrite)]]
             {"person_id" pid
              "person_record_hash" hash
              "relation_to_work" (get c "relation_to_work")}))
          metadata-record {"metadata_record_schema_id" schema-id
                           "metadata_record_schema_hash" (manifest/schema-hash schema-path)
                           "work" work
                           "contributors" (vec (sort-by #(get % "person_id")
                                                          contributor-entries))}]
      (self-consistency-check! metadata-record)
      (json/write-deterministic-json-file! (io/file output) metadata-record)
      (let [new-hash (metadata-record/record-hash metadata-record)]
        (tel/log! :info (str "metadata_record_hash: " new-hash))
        new-hash))))

(defn run!
  "CLI-shaped entry: reads the CSV from a ZIP path and delegates to
  run-from-rows!. Same return value."
  [{:keys [zip-path work-id] :as opts}]
  (let [csv (read-zip-csv zip-path)
        rows (ac/read-rows-from-string csv)]
    (run-from-rows! (-> opts (dissoc :zip-path) (assoc :rows rows)))))

(def cli-options
  [["-z" "--zip ZIP" "Path to an Aozora list_person_all_extended ZIP."
    :id :zip-path]
   ["-w" "--work-id WORK_ID" "Aozora work ID (6-digit zero-padded string)."
    :id :work-id]
   ["-o" "--output FILE" "Output JSON path (work metadata-record)."
    :id :output]
   [nil "--persons-output-dir DIR"
    "Directory for per-person JSON files (default: <output-dir>/persons)."
    :id :persons-output-dir]
   [nil "--overwrite" "Overwrite an existing on-disk person file whose hash differs."
    :id :overwrite :default false]])

(defn usage []
  (tel/log! :warn (str "Usage: clojure -M:abc/aozora-ingest "
                       "--zip <path-to-zip> --work-id NNNNNN --output <path>"
                       " [--persons-output-dir DIR] [--overwrite]")))

(defn -main [& args]
  (logging/install-cli-handler!)
  (let [{:keys [options errors]} (cli/parse-opts args cli-options)]
    (if (or (seq errors)
            (nil? (:zip-path options))
            (nil? (:work-id options))
            (nil? (:output options)))
      (do
        (doseq [e errors] (tel/log! :error e))
        (usage)
        (System/exit 2))
      (do
        (run! options)
        (tel/log! :info (str "wrote metadata-record to " (:output options)))))))
```

Add to the require: `[abc.tools.metadata-record :as metadata-record]` and `[abc.tools.person-record :as person-record]`. The existing `[clojure.tools.cli :as cli]` and other imports stay.

- [ ] **Step 4: Run the ingester tests**

Run: `clojure -M:test -e "(require 'abc.tools.aozora-ingest-test) (clojure.test/run-tests 'abc.tools.aozora-ingest-test)" 2>&1 | tail -15`
Expected: PASS for all five tests in the namespace.

- [ ] **Step 5: Re-run the example flow against the live CSV; verify byte parity modulo provenance**

```bash
mkdir -p /tmp/abc-ingest-test
nix run .#aozora-ingest -- \
  --zip references/aozorabunko/index_pages/list_person_all_extended_utf8.zip \
  --work-id 000127 \
  --output /tmp/abc-ingest-test/metadata-record.json \
  --persons-output-dir /tmp/abc-ingest-test/persons
```

Expected: writes both files.

The ingester does not stamp `source_csv_provenance`; the committed fixture has it (added separately during the metadata milestone). Strip the provenance block from both sides before diffing so the comparison only checks identity-bearing fields:

```bash
diff /tmp/abc-ingest-test/persons/000879.json examples/v0/example-persons/000879.json
jq 'del(.source_csv_provenance)' /tmp/abc-ingest-test/metadata-record.json > /tmp/a.json
jq 'del(.source_csv_provenance)' examples/v0/example-work/metadata-record.json > /tmp/b.json
diff /tmp/a.json /tmp/b.json
```

Expected: zero diff for the persons file (the example-persons fixture has no `source_csv_provenance` block). Zero diff for the provenance-stripped metadata-record. If diffs surface, the ingester is producing different bytes than the hand-regenerated fixture — investigate before committing.

- [ ] **Step 6: Commit**

```bash
git add src/abc/tools/aozora_ingest.clj test/abc/tools/aozora_ingest_test.clj
git commit -m "$(cat <<'EOF'
feat: aozora-ingest emits N+1 files with corruption-safe idempotent check

aozora-ingest now writes a work metadata-record AND one person-record
per contributing person_id. New flags:
- --persons-output-dir DIR     output directory for person files
- --overwrite                  replace divergent on-disk person files

The corruption-safe idempotent check parses + schema-validates an
existing on-disk person file before recomputing its hash. Unparseable
or schema-invalid files fail loudly; matching files are no-op rewrites;
mismatched files require --overwrite.

Programmatic run! returns the new metadata_record_hash so callers
(harness, tests, CI) can chain into a manifest refresh.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: glm-5
EOF
)"
```

---

## Task 11: Add `--refresh-manifest` flag

**Files:**
- Modify: `src/abc/tools/aozora_ingest.clj`
- Modify: `test/abc/tools/aozora_ingest_test.clj`

- [ ] **Step 1: Add a failing test**

Append to `test/abc/tools/aozora_ingest_test.clj`:

```clojure
(deftest ingest-refresh-manifest-roundtrip-test
  (testing "--refresh-manifest rewrites metadata_record_hash and recomputes artifact_id; re-run is byte-identical"
    (let [work-dir (temp-dir "abc-ingest-rm-w")
          persons-dir (temp-dir "abc-ingest-rm-p")
          manifest-path (str (io/file work-dir "manifest.json"))]
      (try
        ;; Seed a manifest stub with a stale metadata_record_hash and artifact_id.
        (let [seed (assoc-in (files/read-json "examples/v0/example-work/manifest.json")
                             ["manifest_identity_object" "metadata_record_hash"]
                             "sha256:0000000000000000000000000000000000000000000000000000000000000000")
              seed (assoc seed "artifact_id"
                          "sha256:0000000000000000000000000000000000000000000000000000000000000000")]
          (require '[abc.tools.json :as j])
          ((resolve 'abc.tools.json/write-deterministic-json-file!)
           (io/file manifest-path) seed))
        (ingest/run! {:zip-path zip-path :work-id "000127"
                      :output (str (io/file work-dir "metadata-record.json"))
                      :persons-output-dir (str persons-dir)
                      :refresh-manifest manifest-path})
        (let [m (files/read-json manifest-path)
              new-mr-hash (get-in m ["manifest_identity_object" "metadata_record_hash"])
              new-artifact (get m "artifact_id")]
          (is (re-matches #"^sha256:[0-9a-f]{64}$" new-mr-hash))
          (is (re-matches #"^sha256:[0-9a-f]{64}$" new-artifact))
          (is (not= "sha256:0000000000000000000000000000000000000000000000000000000000000000"
                    new-artifact)))
        ;; Re-run is byte-identical.
        (let [first-bytes (slurp manifest-path)]
          (ingest/run! {:zip-path zip-path :work-id "000127"
                        :output (str (io/file work-dir "metadata-record.json"))
                        :persons-output-dir (str persons-dir)
                        :refresh-manifest manifest-path})
          (is (= first-bytes (slurp manifest-path))))
        (finally
          (delete-recursive work-dir)
          (delete-recursive persons-dir))))))
```

- [ ] **Step 2: Run; expect failure (`:refresh-manifest` is currently ignored)**

Run: `clojure -M:test -e "(require 'abc.tools.aozora-ingest-test) (clojure.test/run-tests 'abc.tools.aozora-ingest-test)" 2>&1 | tail -10`
Expected: FAIL — manifest still has the seeded zero hash.

- [ ] **Step 3: Implement `--refresh-manifest` in `run!`**

Open `src/abc/tools/aozora_ingest.clj`. After the `(self-consistency-check! metadata-record)` and `(json/write-deterministic-json-file! ...)` lines inside `run!`, but before the `(let [new-hash ...])` block at the end of `run!`, splice in the refresh logic. The simplest path: replace the closing block of `run!` with:

```clojure
      (self-consistency-check! metadata-record)
      (json/write-deterministic-json-file! (io/file output) metadata-record)
      (let [new-hash (metadata-record/record-hash metadata-record)]
        (tel/log! :info (str "metadata_record_hash: " new-hash))
        (when refresh-manifest
          (let [m (files/read-json refresh-manifest)
                m' (assoc-in m ["manifest_identity_object" "metadata_record_hash"] new-hash)
                identity-obj (get m' "manifest_identity_object")
                artifact-id (hash/format-sha256 (hash/sha256-json-jcs identity-obj))
                m'' (assoc m' "artifact_id" artifact-id)]
            (json/write-deterministic-json-file! (io/file refresh-manifest) m'')
            (tel/log! :info (str "refreshed manifest " refresh-manifest))))
        new-hash))))
```

Add `[abc.tools.hash :as hash]` to the require if not already present.

Update the `cli-options` vector to include the new flag (full replacement shown for clarity):

```clojure
(def cli-options
  [["-z" "--zip ZIP" "Path to an Aozora list_person_all_extended ZIP."
    :id :zip-path]
   ["-w" "--work-id WORK_ID" "Aozora work ID (6-digit zero-padded string)."
    :id :work-id]
   ["-o" "--output FILE" "Output JSON path (work metadata-record)."
    :id :output]
   [nil "--persons-output-dir DIR"
    "Directory for per-person JSON files (default: <output-dir>/persons)."
    :id :persons-output-dir]
   [nil "--overwrite" "Overwrite an existing on-disk person file whose hash differs."
    :id :overwrite :default false]
   [nil "--refresh-manifest FILE"
    "Rewrite the named manifest.json's metadata_record_hash + artifact_id in place."
    :id :refresh-manifest]])
```

Update `usage` to mention `--refresh-manifest`:

```clojure
(defn usage []
  (tel/log! :warn (str "Usage: clojure -M:abc/aozora-ingest "
                       "--zip <path-to-zip> --work-id NNNNNN --output <path>"
                       " [--persons-output-dir DIR] [--overwrite]"
                       " [--refresh-manifest manifest.json]")))
```

- [ ] **Step 4: Run the test**

Run: `clojure -M:test -e "(require 'abc.tools.aozora-ingest-test) (clojure.test/run-tests 'abc.tools.aozora-ingest-test)" 2>&1 | tail -10`
Expected: PASS for `ingest-refresh-manifest-roundtrip-test` and all earlier tests in the namespace.

- [ ] **Step 5: Confirm `nix run .#aozora-ingest -- --refresh-manifest examples/v0/example-work/manifest.json` is a no-op against the committed fixture**

```bash
sha256sum examples/v0/example-work/manifest.json
nix run .#aozora-ingest -- \
  --zip references/aozorabunko/index_pages/list_person_all_extended_utf8.zip \
  --work-id 000127 \
  --output examples/v0/example-work/metadata-record.json \
  --persons-output-dir examples/v0/example-persons \
  --refresh-manifest examples/v0/example-work/manifest.json
sha256sum examples/v0/example-work/manifest.json
```

Expected: identical sha256sum before and after — the committed fixture is already at the refreshed hash from Task 7.

- [ ] **Step 6: Commit**

```bash
git add src/abc/tools/aozora_ingest.clj test/abc/tools/aozora_ingest_test.clj
git commit -m "$(cat <<'EOF'
feat: aozora-ingest --refresh-manifest closes the identity loop

After writing the metadata-record, --refresh-manifest reads the named
manifest.json, replaces manifest_identity_object.metadata_record_hash,
recomputes artifact_id (per ADR 0001 JCS-of-identity-object), and
writes the manifest back. Re-running with no upstream changes is a
byte-identical no-op.

Without the flag, ingest's existing stdout-printed hash remains the
v0 fallback for callers that update manifests externally.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: glm-5
EOF
)"
```

---

## Task 12: SHACL — add `PersonRecordShape`, retire `MetadataRecordPersonShape`, update `MetadataRecordWorkShape`

**Files:**
- Modify: `schemas/manifest.shacl.ttl`
- Modify: `test/abc/tools/shacl_test.clj` (if existing tests assert on retired shape)

- [ ] **Step 1: Add a failing test asserting `PersonRecordShape` exists**

Read `test/abc/tools/shacl_test.clj`. The existing tests load the shapes graph and probe specific shape IRIs. Add (or extend an existing) test:

```clojure
(deftest person-record-shape-loaded-test
  (testing "PersonRecordShape is loaded from manifest.shacl.ttl"
    (let [shapes (shacl/load-shapes-graph)
          triples (iterator-seq (.find shapes))
          subjects (set (map #(.getURI (.getSubject %)) triples))]
      (is (contains? subjects "https://w3id.org/abc/PersonRecordShape")))))

(deftest metadata-record-person-shape-retired-test
  (testing "MetadataRecordPersonShape is no longer in the shapes graph"
    (let [shapes (shacl/load-shapes-graph)
          triples (iterator-seq (.find shapes))
          subjects (set (map #(.getURI (.getSubject %)) triples))]
      (is (not (contains? subjects "https://w3id.org/abc/MetadataRecordPersonShape"))))))
```

(Find a current `(deftest ...)` form for the existing requires/aliases and append alongside. If `shacl/load-shapes-graph` is not in scope, the existing tests will show the correct require.)

- [ ] **Step 2: Run the tests; expect failures**

Run: `clojure -M:test -e "(require 'abc.tools.shacl-test) (clojure.test/run-tests 'abc.tools.shacl-test)" 2>&1 | tail -10`
Expected: FAIL on both new tests.

- [ ] **Step 3: Update `schemas/manifest.shacl.ttl`**

Open `schemas/manifest.shacl.ttl`. Remove the `abc:MetadataRecordPersonShape` block (lines 162-198 per the current file). Add `abc:PersonRecordShape` in its place. Body:

```turtle
abc:PersonRecordShape
  a sh:NodeShape ;
  sh:targetClass foaf:Person ;
  sh:property [
    sh:path dcterms:identifier ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
    sh:datatype xsd:int
  ] ;
  sh:property [
    sh:path foaf:familyName ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
    sh:datatype xsd:string
  ] ;
  sh:property [
    sh:path foaf:givenName ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
    sh:datatype xsd:string
  ] ;
  sh:property [
    sh:path foaf:name ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
    sh:datatype xsd:string
  ] ;
  sh:property [
    sh:path rdag2:dateOfBirth ;
    sh:maxCount 1 ;
    sh:datatype xsd:date
  ] ;
  sh:property [
    sh:path rdag2:dateOfDeath ;
    sh:maxCount 1 ;
    sh:datatype xsd:date
  ] .
```

`MetadataRecordWorkShape` covers the work view; the existing definition stands. Confirm by re-reading lines 114-160 — no edits needed if those constraints already match the current Work entity.

**SHACL scope rationale (departing from the design spec).** The design spec
envisioned SHACL validating `contributors[]` shape (`person_id` format,
`person_record_hash` format, `relation_to_work` enum membership). On
reflection, this isn't where SHACL fits: SHACL operates on RDF triples,
and the work's RDF view emits `dcterms:creator` / `dcterms:contributor`
edges to person IRIs — there is no `contributors[]` array in the graph
to validate. The `person_record_hash` and `person_id` formats live only
in the JSON document, and `relation_to_work` enum membership is already
constrained by the JSON Schema and the harness reference-integrity check.
So the implementation drops the SHACL contributor checks; the equivalent
constraints come from JSON Schema (Task 5's `$defs/contributor`) plus the
harness step (Task 13).

- [ ] **Step 4: Run the SHACL tests**

Run: `clojure -M:test -e "(require 'abc.tools.shacl-test) (clojure.test/run-tests 'abc.tools.shacl-test)" 2>&1 | tail -10`
Expected: PASS on both new tests.

- [ ] **Step 5: Run the full metadata-record test suite to confirm SHACL conformance still holds**

Run: `clojure -M:test -e "(require 'abc.tools.metadata-record-test) (clojure.test/run-tests 'abc.tools.metadata-record-test)" 2>&1 | tail -10`
Expected: PASS, including `record-graph-conforms-to-shacl-test` (the composed graph passes both `MetadataRecordWorkShape` and `PersonRecordShape`).

- [ ] **Step 6: Commit**

```bash
git add schemas/manifest.shacl.ttl test/abc/tools/shacl_test.clj
git commit -m "$(cat <<'EOF'
feat: SHACL PersonRecordShape replaces MetadataRecordPersonShape

PersonRecordShape targets foaf:Person and constrains the same field set
as the retired MetadataRecordPersonShape. The rename clarifies that
person-shaped triples now come from a separate person-record artifact;
the shapes-graph still validates work + person triples in one pass
when the harness composes them.

Scope note: the design spec contemplated SHACL also validating
contributors[] shape. SHACL operates on RDF triples and the work's
RDF view emits dcterms:creator/dcterms:contributor edges (no
contributors[] array exists in the graph), so contributor-array
constraints land in JSON Schema (#$defs/contributor in
metadata-record.schema.json) and the harness reference-integrity check.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: glm-5
EOF
)"
```

---

## Task 13: Update the harness `validate-metadata-record!` step

**Files:**
- Modify: `src/abc/tools/validate_design_bundle.clj`
- Modify: `test/abc/tools/validate_design_bundle_test.clj`

The new step validates the persons directory (schema + schema-hash precondition), validates the work record, recomputes contributor reference hashes, and runs SHACL on the combined graph.

- [ ] **Step 1: Add failing tests for reference-integrity loud-fail and schema-hash precondition**

Append to `test/abc/tools/validate_design_bundle_test.clj`:

```clojure
(deftest validate-metadata-bundle-detects-stale-contributor-hash-test
  (testing "mutating a person file's bytes makes the harness fail with the person_id and both hashes"
    (let [persons-dir (.toFile (java.nio.file.Files/createTempDirectory
                                "abc-vdb-persons"
                                (make-array java.nio.file.attribute.FileAttribute 0)))
          orig (files/read-json "examples/v0/example-persons/000879.json")
          mutated (assoc orig "family_name_romaji" "Akutagawa-MUTATED")]
      (try
        (require '[abc.tools.json :as j])
        ((resolve 'abc.tools.json/write-deterministic-json-file!)
         (clojure.java.io/file persons-dir "000879.json") mutated)
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo #"000879"
             (validate/validate-metadata-bundle!
              {:record-path "examples/v0/example-work/metadata-record.json"
               :manifest-path "examples/v0/example-work/manifest.json"
               :persons-dir (str persons-dir)
               :record-schema-path "schemas/metadata-record.schema.json"
               :person-schema-path "schemas/person-record.schema.json"
               :ttl-path "examples/v0/example-work/metadata-record.ttl"
               :shapes-graph ((requiring-resolve 'abc.tools.shacl/load-shapes-graph))})))
        (finally
          (doseq [f (reverse (file-seq persons-dir))] (.delete f)))))))

(deftest validate-metadata-bundle-detects-schema-hash-drift-test
  (testing "a person file with a stale embedded schema hash makes the harness fail"
    (let [persons-dir (.toFile (java.nio.file.Files/createTempDirectory
                                "abc-vdb-schema"
                                (make-array java.nio.file.attribute.FileAttribute 0)))
          orig (files/read-json "examples/v0/example-persons/000879.json")
          mutated (assoc orig "person_record_schema_hash"
                         "sha256:0000000000000000000000000000000000000000000000000000000000000000")]
      (try
        (require '[abc.tools.json :as j])
        ((resolve 'abc.tools.json/write-deterministic-json-file!)
         (clojure.java.io/file persons-dir "000879.json") mutated)
        (is (thrown? clojure.lang.ExceptionInfo
                     (validate/validate-metadata-bundle!
                      {:record-path "examples/v0/example-work/metadata-record.json"
                       :manifest-path "examples/v0/example-work/manifest.json"
                       :persons-dir (str persons-dir)
                       :record-schema-path "schemas/metadata-record.schema.json"
                       :person-schema-path "schemas/person-record.schema.json"
                       :ttl-path "examples/v0/example-work/metadata-record.ttl"
                       :shapes-graph ((requiring-resolve 'abc.tools.shacl/load-shapes-graph))})))
        (finally
          (doseq [f (reverse (file-seq persons-dir))] (.delete f)))))))
```

(Add `[abc.tools.files :as files]` to the require if it isn't already.)

- [ ] **Step 2: Run; expect failures (the new function name + signature don't exist)**

Run: `clojure -M:test -e "(require 'abc.tools.validate-design-bundle-test) (clojure.test/run-tests 'abc.tools.validate-design-bundle-test)" 2>&1 | tail -10`
Expected: FAIL — `validate-metadata-bundle!` unresolved.

- [ ] **Step 3: Replace `validate-metadata-record!` with `validate-metadata-bundle!`**

Open `src/abc/tools/validate_design_bundle.clj`. Add `[abc.tools.person-record :as person-record]` to the require block (alongside the existing `metadata-record` require).

Replace the existing `validate-metadata-record!` function with:

```clojure
(defn- validate-persons-directory!
  "For every JSON file under `persons-dir`: load, schema-validate,
  and verify the embedded person_record_schema_hash matches the live
  schema's JCS hash. Returns a map person_id → person-record map."
  [persons-dir person-schema-path]
  (let [live-schema-hash (manifest/schema-hash person-schema-path)
        files (->> (.listFiles (io/file persons-dir))
                   (filter #(string/ends-with? (.getName ^java.io.File %) ".json"))
                   sort)]
    (into {}
          (for [^java.io.File f files]
            (let [record (files/read-json (str f))]
              (person-record/validate! record)
              (let [embedded (get record "person_record_schema_hash")]
                (when-not (= embedded live-schema-hash)
                  (throw (ex-info
                          (str "person_record_schema_hash mismatch in " f
                               ": record has " embedded
                               ", live schema hash is " live-schema-hash)
                          {:path (str f)
                           :embedded embedded
                           :live live-schema-hash}))))
              [(get record "person_id") record])))))

(defn validate-metadata-bundle!
  "Validate the example-work metadata-record bundle:
  1. Every person file in `persons-dir` validates against
     person-record.schema.json + schema-hash precondition.
  2. The work's metadata-record.json validates against
     metadata-record.schema.json + schema-hash precondition.
  3. Recompute metadata_record_hash; compare against the
     `manifest_identity_object.metadata_record_hash` in manifest.json.
  4. For every contributors[i]: recompute the referenced person's
     person_record_hash from the on-disk file and fail if it does
     not match contributors[i].person_record_hash.
  5. Compose the work + persons graph; SHACL validate against shapes.
  6. Compose work + persons → ttl; byte-equal to ttl-path."
  [{:keys [record-path manifest-path persons-dir
           record-schema-path person-schema-path
           ttl-path shapes-graph]}]
  (let [persons-by-id (validate-persons-directory! persons-dir person-schema-path)
        record (files/read-json record-path)]
    (metadata-record/validate! record)
    (let [computed-schema-hash (manifest/schema-hash record-schema-path)
          expected-schema-hash (get record "metadata_record_schema_hash")]
      (when-not (= computed-schema-hash expected-schema-hash)
        (throw (ex-info (str "metadata_record_schema_hash mismatch: "
                             "record has " expected-schema-hash
                             ", live schema hash is " computed-schema-hash)
                        {:record-path record-path
                         :computed computed-schema-hash
                         :expected expected-schema-hash}))))
    (let [computed (metadata-record/record-hash record)
          expected (get-in (files/read-json manifest-path)
                           ["manifest_identity_object" "metadata_record_hash"])]
      (when-not (= computed expected)
        (throw (ex-info (str "metadata_record_hash mismatch: manifest has "
                             expected ", record-hash computed " computed)
                        {:record-path record-path
                         :manifest-path manifest-path
                         :computed computed
                         :expected expected}))))
    (doseq [contributor (get record "contributors")]
      (let [pid (get contributor "person_id")
            referenced (get contributor "person_record_hash")
            body (get persons-by-id pid)]
        (when-not body
          (throw (ex-info (str "contributor " pid " has no matching file in " persons-dir)
                          {:person-id pid
                           :persons-dir persons-dir})))
        (let [recomputed (person-record/record-hash body)]
          (when-not (= referenced recomputed)
            (throw (ex-info
                    (str "contributor reference for person_id " pid
                         " is stale: metadata-record references " referenced
                         ", recomputed from " persons-dir "/" pid ".json is "
                         recomputed)
                    {:person-id pid
                     :referenced referenced
                     :recomputed recomputed
                     :persons-dir persons-dir}))))))
    (shacl/validate! {:shapes-graph shapes-graph
                      :data-graph (metadata-record/record+persons->graph
                                    record persons-by-id)
                      :label record-path})
    (let [generated (metadata-record/record+persons->ttl record persons-by-id)
          expected (slurp ttl-path)]
      (when-not (= expected generated)
        (throw (ex-info (str "metadata-record.ttl parity mismatch with " ttl-path)
                        {:record-path record-path
                         :ttl-path ttl-path}))))))
```

Replace the call site in `validate-design-bundle!`:

```clojure
        (tel/log! :info "==> Validating metadata record")
        (let [shapes (shacl/load-shapes-graph)]
          (validate-metadata-record!
           {:record-path "examples/v0/example-work/metadata-record.json"
            :manifest-path "examples/v0/example-work/manifest.json"
            :schema-path "schemas/metadata-record.schema.json"
            :ttl-path "examples/v0/example-work/metadata-record.ttl"
            :shapes-graph shapes}))
        (tel/log! :info "metadata record ok")
```

with:

```clojure
        (tel/log! :info "==> Validating metadata record + persons bundle")
        (let [shapes (shacl/load-shapes-graph)]
          (validate-metadata-bundle!
           {:record-path "examples/v0/example-work/metadata-record.json"
            :manifest-path "examples/v0/example-work/manifest.json"
            :persons-dir "examples/v0/example-persons"
            :record-schema-path "schemas/metadata-record.schema.json"
            :person-schema-path "schemas/person-record.schema.json"
            :ttl-path "examples/v0/example-work/metadata-record.ttl"
            :shapes-graph shapes}))
        (tel/log! :info "metadata bundle ok")
```

Delete the now-unreachable `validate-metadata-record!` function.

**Grep for stale callers before committing:**

```bash
grep -rn "validate-metadata-record!" src test
```

Expected: no matches in `src/` (the call site in `validate_design_bundle.clj` was just replaced) and no matches in `test/`. If anything turns up, update or remove the caller.

- [ ] **Step 4: Run the harness tests**

Run: `clojure -M:test -e "(require 'abc.tools.validate-design-bundle-test) (clojure.test/run-tests 'abc.tools.validate-design-bundle-test)" 2>&1 | tail -10`
Expected: PASS for the two new tests; existing harness tests continue to pass.

- [ ] **Step 5: Run the full design-bundle harness**

```bash
nix run .#validate-design-bundle 2>&1 | tail -30
```

Expected: every step prints its `ok` line, including:

```
==> Validating metadata record + persons bundle
metadata bundle ok
```

Final line: `design bundle validation ok`. Exit code 0.

- [ ] **Step 6: Commit**

```bash
git add src/abc/tools/validate_design_bundle.clj test/abc/tools/validate_design_bundle_test.clj
git commit -m "$(cat <<'EOF'
feat: validate-metadata-bundle handles persons + reference integrity

The metadata-record harness step now:
1. Validates each examples/v0/example-persons/*.json against
   person-record.schema.json + person_record_schema_hash precondition.
2. Validates the work metadata-record + its embedded schema hash.
3. Recomputes metadata_record_hash and compares against
   manifest_identity_object.metadata_record_hash.
4. Recomputes each contributor's person_record_hash from the on-disk
   file and fails loudly with both hashes if they disagree.
5. Composes work + persons → graph, runs SHACL.
6. Composes work + persons → ttl, byte-equal to the committed fixture.

Two new tests cover reference-integrity loud-fail and schema-hash
precondition drift. The previous validate-metadata-record! is removed.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: glm-5
EOF
)"
```

---

## Task 14: Flake contract surface, focused-test alias

**Files:**
- Modify: `flake.nix`
- Modify: `nix/clj-nix-deps.edn`

- [ ] **Step 1: Add new files to `contract-surface`**

Open `flake.nix`. Find the `contract-surface` derivation (near line 156 per current layout). Append `test -f` lines for the new files. Place them next to existing similar entries:

```nix
            test -f ${./schemas/person-record.schema.json}
            test -f ${./src/abc/tools/person_record.clj}
            test -f ${./test/abc/tools/person_record_test.clj}
            test -f ${./examples/v0/example-persons/000879.json}
```

- [ ] **Step 2: Add `abc.tools.person-record-test` to the focused-test alias**

Open `nix/clj-nix-deps.edn`. The `:abc/focused-test :main-opts` is a long single-line string with all test namespaces enumerated twice (require + run-tests). Add `'abc.tools.person-record-test` and `'abc.tools.aozora-ingest-test` (if not already present) alongside `'abc.tools.metadata-record-test`. Two locations: the `(require ...)` form and the `(test/run-tests ...)` form.

If `aozora-ingest-test` requires the live ZIP to run (per Task 10), guard it in the focused-test sandbox via an env-skip flag (mirror the `ABC_TEI_SCHEMA_SKIP` pattern). For now, set the skip flag in the focused-test sandbox via flake.nix's `clj-nix-focused-tests` derivation:

```nix
              export ABC_AOZORA_INGEST_SKIP=1

              clojure -M:abc/focused-test
```

In `aozora-ingest-test`, wrap each test that uses the live zip with `(when-not (= "1" (System/getenv "ABC_AOZORA_INGEST_SKIP")) ...)`.

- [ ] **Step 3: Run `nix flake check`**

```bash
nix flake check 2>&1 | tail -10
```

Expected: `all checks passed!`.

- [ ] **Step 4: Commit**

```bash
git add flake.nix nix/clj-nix-deps.edn test/abc/tools/aozora_ingest_test.clj
git commit -m "$(cat <<'EOF'
chore: include person-record + ingester tests in flake surface

contract-surface verifies the new schema, namespace, test, and
fixture exist. focused-test loads abc.tools.person-record-test +
abc.tools.aozora-ingest-test; the ingester test honors
ABC_AOZORA_INGEST_SKIP=1 so the sealed Nix sandbox (no network /
no live Aozora ZIP) can skip cleanly.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: glm-5
EOF
)"
```

---

## Task 15: End-to-end verification + next-steps refresh

**Files:**
- (Verification only.)
- Modify: `docs/next-steps.md`

- [ ] **Step 1: Run the full design-bundle harness from a fresh shell**

```bash
nix run .#validate-design-bundle 2>&1 | tail -25
```

Expected: every `==>` step prints its `ok`, including the new `metadata bundle ok`. Final line: `design bundle validation ok`. Exit code 0.

- [ ] **Step 2: Run `nix flake check`**

```bash
nix flake check 2>&1 | tail -5
```

Expected: `all checks passed!`.

- [ ] **Step 3: Negative regression — flip a person field, confirm harness fails**

```bash
cp examples/v0/example-persons/000879.json /tmp/abc-person-backup.json
# Mutate family_name_romaji
clojure -M -e "
  (require '[abc.tools.files :as f] '[abc.tools.json :as j] '[clojure.java.io :as io])
  (let [p (f/read-json \"examples/v0/example-persons/000879.json\")
        mutated (assoc p \"family_name_romaji\" \"Mutated\")]
    (j/write-deterministic-json-file! (io/file \"examples/v0/example-persons/000879.json\") mutated))"
nix run .#validate-design-bundle 2>&1 | tail -10
```

Expected: harness exits non-zero with a message naming `000879` and the recomputed-vs-referenced hashes.

Restore:

```bash
cp /tmp/abc-person-backup.json examples/v0/example-persons/000879.json
nix run .#validate-design-bundle 2>&1 | tail -3
```

Expected: clean run; `design bundle validation ok`.

- [ ] **Step 4: Confirm `--refresh-manifest` round-trip is a no-op**

```bash
sha256sum examples/v0/example-work/manifest.json
nix run .#aozora-ingest -- \
  --zip references/aozorabunko/index_pages/list_person_all_extended_utf8.zip \
  --work-id 000127 \
  --output examples/v0/example-work/metadata-record.json \
  --persons-output-dir examples/v0/example-persons \
  --refresh-manifest examples/v0/example-work/manifest.json
sha256sum examples/v0/example-work/manifest.json
```

Expected: identical sums.

- [ ] **Step 5: Update `docs/next-steps.md`**

Open `docs/next-steps.md`. Add the milestone to "Current State":

```markdown
- **2026-04-28 Separated person records.** Person bodies live in
  `examples/v0/example-persons/<person_id>.json`; works reference
  contributors via `{person_id, person_record_hash, relation_to_work}`.
  A person edit only invalidates works that reference that person.
  `nix run .#aozora-ingest --refresh-manifest` closes the work +
  manifest identity loop.
```

Remove "Separated person records" from the "Candidate Next Milestones" section. The remaining list reorders so that "Corpus-scale CSV ingestion" becomes the new top item.

- [ ] **Step 6: Commit**

```bash
git add docs/next-steps.md
git commit -m "$(cat <<'EOF'
docs: refresh next-steps after separated-person-records milestone

The metadata-record + person-record split landed; corpus-scale CSV
ingestion is now the top candidate, with separated persons removed
from the candidate list.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: glm-5
EOF
)"
```

---

## Self-Review

**Spec coverage:**
- §Goal / §File Layout — Tasks 1, 4, 5, 7, 12, 14.
- §Person Record Shape — Task 1 (schema) + Task 4 (fixture).
- §Person Record Identity — Task 2 (record-hash includes schema fields, excludes provenance) + Task 4 schema-hash test.
- §Revised Metadata-Record Shape — Tasks 5 + 6 + 7.
- §Metadata-Record Identity — Task 6 (canonical-identity-form sorts contributors[]) + Task 7 (regen).
- §Identity-Field Summary — Tasks 1, 2, 5, 6.
- §Ingester CLI — Tasks 9 (CSV split), 10 (N+1, --persons-output-dir, --overwrite, corruption-safe), 11 (--refresh-manifest).
- §SHACL — Task 12.
- §TEI-Header Builder — Task 8.
- §Harness Step — Task 13.
- §Tests — Tasks 2, 3, 4, 6, 9, 10, 11, 12, 13.
- §Acceptance Criteria 1-11 — Tasks 4, 7, 8, 13, 15 cover all.
- §Risks (place-vs-value) — out-of-scope for implementation; documented in spec.

**Placeholder scan:** No "TBD"/"TODO"/"implement later"/"similar to Task N" in the plan body. The references-and-IRIs are concrete; commands and expected outputs are specified at every step.

**Type consistency:**
- `person-record/record-hash` and `metadata-record/record-hash` both return `sha256:<hex>` strings.
- `build-record-fragment-from-rows` returns `{:work, :persons-by-id, :contributors}` — used in Task 9 (test), Task 10 (ingester `run!`).
- `metadata-record/build-metadata-record` argument is `{:work, :contributors}` consistently in Task 6 + Task 10.
- `metadata-record/record+persons->graph` and `record+persons->ttl` take `(record, persons-by-id-map)` consistently in Task 6 (definition), Task 7 (regen), Task 13 (harness).
- `tei-header/build` argument is `{:work, :contributors [{:relation-to-work, :person}]}` consistently in Task 8.
- `validate-metadata-bundle!` argument keys are `{:record-path, :manifest-path, :persons-dir, :record-schema-path, :person-schema-path, :ttl-path, :shapes-graph}` consistently in Task 13.
- Harness env-skip flag is `ABC_AOZORA_INGEST_SKIP` (Task 14).

**Sequencing:**
- Tasks 1-4 stand alone (Person artifact + namespace).
- Tasks 5-7 are a single logical commit but split into three tasks for granularity; the schema and code commit together at the end of Task 7.
- Task 8 (TEI builder) depends on the regenerated fixture from Task 7.
- Tasks 9-11 depend on Task 5 (schema) and Tasks 2-3 (person-record).
- Task 12 (SHACL) depends on Task 6 (composed graph for the SHACL test).
- Task 13 (harness) depends on Tasks 6 + 12.
- Tasks 14-15 (flake + verification) are last.
