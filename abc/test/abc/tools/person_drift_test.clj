(ns abc.tools.person-drift-test
  (:require [arachne.aristotle :as aa]
            [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [abc.tools.person-drift :as drift]
            [abc.tools.schema :as schema]
            [abc.tools.shacl :as shacl]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer [deftest is]])
  (:import [org.apache.jena.graph NodeFactory Triple]))

(deftest json-directory-listing-contract-test
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "abc-drift-list"
                      (make-array java.nio.file.attribute.FileAttribute 0)))]
    (try
      (spit (io/file dir "b.json") "{}")
      (spit (io/file dir "a.json") "{}")
      (spit (io/file dir "ignored.txt") "x")
      (.mkdirs (io/file dir "directory.json"))
      (is (= ["a.json" "b.json"]
             (mapv #(.getName ^java.io.File %) (#'drift/json-files dir))))
      (finally
        (doseq [child (.listFiles dir)]
          (when (.isDirectory child) (.delete child))
          (when (.exists child) (.delete child)))
        (.delete dir)))))

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

;; ADR 0020: a merge has >= 2 predecessors (prov.used) and exactly 1
;; successor (was_generated_by). Mirror of base-split for the merge direction.
(defn base-merge-without-id []
  {"schema_id" drift/event-schema-id
   "schema_hash" (manifest/schema-hash drift/event-schema-path)
   "drift_event_type" "merge"
   "date" "2026-04-30"
   "participants" [{"snapshot_id" "post-000003"
                    "person_id" "000003"
                    "person_record_hash" (example-hash "13")}
                   {"snapshot_id" "pre-000001"
                    "person_id" "000001"
                    "person_record_hash" (example-hash "11")}
                   {"snapshot_id" "pre-000002"
                    "person_id" "000002"
                    "person_record_hash" (example-hash "12")}]
   "evidence" ["https://example.org/abc/drift-evidence/fictional-merge-2026"]
   "prov" {"used" ["pre-000001" "pre-000002"]
           "was_generated_by" ["post-000003"]
           "qualified_association" {"agent" "https://w3id.org/abc/agents/editorial-board"
                                    "had_role" "abc:DriftEditor"}}})

(defn base-merge []
  (drift/materialize-event-id (base-merge-without-id)))

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

;; ADR 0020 cardinality invariant, bound to the real schema artifact
;; (replaces the vacuous r3-drift-cardinality.smt2 — see
;; docs/handoffs/formal-verification-assessment-critique.md §7.3). A split
;; has exactly 1 predecessor (prov.used) and >= 2 successors
;; (was_generated_by); a merge has >= 2 predecessors and exactly 1
;; successor. Each of the six violation modes below must be rejected by
;; schemas/person-drift-event.schema.json. Unlike the SMT file, this test
;; reads the real schema, so a regression that widens a minItems/maxItems
;; bound flips the verdict to failure.
(defn- reject-cardinality-violation [label event]
  (let [errors (schema/validation-errors
                (files/read-json drift/event-schema-path)
                event)]
    (is (seq errors)
        (str "expected schema to reject " label))
    errors))

(deftest event-schema-rejects-split-with-zero-predecessors-test
  ;; split needs exactly 1 predecessor; 0 violates used maxItems-ish
  (reject-cardinality-violation "split with 0 predecessors"
                                (assoc-in (base-split) ["prov" "used"] [])))

(deftest event-schema-rejects-split-with-two-predecessors-test
  ;; split needs exactly 1 predecessor; 2 violates used maxItems
  (reject-cardinality-violation "split with 2 predecessors"
                                (assoc-in (base-split)
                                          ["prov" "used"]
                                          ["pre-000001" "pre-000002"])))

(deftest event-schema-rejects-split-with-one-successor-test
  ;; split needs >= 2 successors; 1 violates was_generated_by minItems
  (reject-cardinality-violation "split with 1 successor"
                                (assoc-in (base-split)
                                          ["prov" "was_generated_by"]
                                          ["post-abc-000000000001"])))

(deftest event-schema-rejects-merge-with-one-predecessor-test
  ;; merge needs >= 2 predecessors; 1 violates used minItems
  (reject-cardinality-violation "merge with 1 predecessor"
                                (assoc-in (base-merge)
                                          ["prov" "used"]
                                          ["pre-000001"])))

(deftest event-schema-rejects-merge-with-zero-successors-test
  ;; merge needs exactly 1 successor; 0 violates was_generated_by minItems
  (reject-cardinality-violation "merge with 0 successors"
                                (assoc-in (base-merge)
                                          ["prov" "was_generated_by"]
                                          [])))

(deftest event-schema-rejects-merge-with-two-successors-test
  ;; merge needs exactly 1 successor; 2 violates was_generated_by maxItems
  (reject-cardinality-violation "merge with 2 successors"
                                (assoc-in (base-merge)
                                          ["prov" "was_generated_by"]
                                          ["post-000003" "post-000004"])))

(deftest event-schema-rejects-participant-missing-required-field-test
  (let [event-schema (files/read-json drift/event-schema-path)]
    (doseq [field ["snapshot_id" "person_id" "person_record_hash"]]
      (is (seq (schema/validation-errors
                event-schema
                (update (base-split) "participants"
                        #(mapv (fn [participant] (dissoc participant field)) %))))
          (str "expected schema to reject participants missing " field)))))

(deftest index-schema-accepts-valid-index-test
  (let [event-id (get (base-split) "drift_event_id")]
    (is (nil? (schema/validation-errors
               (files/read-json drift/index-schema-path)
               (index-for "000879" event-id))))))

(deftest drift-event-id-omits-only-id-field-test
  (let [without-id (base-split-without-id)
        with-id (drift/materialize-event-id without-id)
        rederived (drift/drift-event-id with-id)
        changed-date (assoc with-id "date" "2026-05-01")]
    (is (= (get with-id "drift_event_id") rederived))
    (is (not= (get with-id "drift_event_id")
              (drift/drift-event-id changed-date)))))

(deftest drift-event-id-is-sensitive-to-canonical-order-test
  (let [with-id (base-split)
        expected (get with-id "drift_event_id")]
    (is (not= expected
              (drift/drift-event-id
               (update with-id "participants" #(vec (reverse %))))))
    (is (not= expected
              (drift/drift-event-id
               (update-in with-id ["prov" "was_generated_by"]
                          #(vec (reverse %))))))
    (is (not= (get (base-merge) "drift_event_id")
              (drift/drift-event-id
               (update-in (base-merge) ["prov" "used"]
                          #(vec (reverse %))))))))

(deftest validate-event-json-coherence-accepts-base-split-test
  (is (= [] (drift/event-json-coherence-failures (base-split)))))

(deftest validate-event-json-coherence-accepts-base-merge-test
  (is (= [] (drift/event-json-coherence-failures (base-merge)))))

(deftest validate-event-json-coherence-rejects-unsorted-participants-test
  (let [event (base-split)
        bad (assoc event "participants" (vec (reverse (get event "participants"))))]
    (is (= [:participants-not-sorted]
           (mapv :code (drift/event-json-coherence-failures bad))))))

(deftest validate-event-json-coherence-rejects-dangling-snapshot-ref-test
  (let [bad (assoc-in (base-split) ["prov" "used"] ["pre-missing"])]
    (is (some #{:unknown-snapshot-reference}
              (map :code (drift/event-json-coherence-failures bad))))))

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

(deftest role-curie-resolution-test
  (is (= "https://w3id.org/abc/DriftEditor"
         (drift/resolve-role-curie "abc:DriftEditor"))))

(deftest validate-event-json-coherence-distinguishes-role-prefix-test
  (let [unknown-prefix (assoc-in (base-split)
                                 ["prov" "qualified_association" "had_role"]
                                 "unknown:DriftEditor")
        disallowed-role (assoc-in (base-split)
                                  ["prov" "qualified_association" "had_role"]
                                  "abc:DriftReviewer")]
    (is (some #{:unresolved-curie-prefix}
              (map :code (drift/event-json-coherence-failures unknown-prefix))))
    (is (some #{:invalid-had-role}
              (map :code (drift/event-json-coherence-failures disallowed-role))))))

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

(def ^:private person-drift-event-shape
  (NodeFactory/createURI "https://w3id.org/abc/PersonDriftEventShape"))
(def ^:private person-drift-split-event-shape
  (NodeFactory/createURI "https://w3id.org/abc/PersonDriftSplitEventShape"))
(def ^:private person-drift-merge-event-shape
  (NodeFactory/createURI "https://w3id.org/abc/PersonDriftMergeEventShape"))
(def ^:private drift-event
  (NodeFactory/createURI "https://w3id.org/abc/DriftEvent"))
(def ^:private drift-split-event
  (NodeFactory/createURI "https://w3id.org/abc/DriftSplitEvent"))
(def ^:private drift-merge-event
  (NodeFactory/createURI "https://w3id.org/abc/DriftMergeEvent"))
(def ^:private prov-activity
  (NodeFactory/createURI "http://www.w3.org/ns/prov#Activity"))
(def ^:private rdf-type
  (NodeFactory/createURI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
(def ^:private rdfs-sub-class-of
  (NodeFactory/createURI "http://www.w3.org/2000/01/rdf-schema#subClassOf"))
(def ^:private sh-node-shape
  (NodeFactory/createURI "http://www.w3.org/ns/shacl#NodeShape"))

(defn- graph-contains? [graph subject predicate object]
  (.contains graph (Triple/create subject predicate object)))

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

(deftest snapshot-iri-uses-event-embedded-hash-test
  (let [participant {"snapshot_id" "pre-000879"
                     "person_id" "000879"
                     "person_record_hash" (example-hash "03")}
        mutated (assoc participant "person_record_hash"
                       (str "sha256:f"
                            (subs (get participant "person_record_hash")
                                  (inc (count "sha256:")))))]
    (is (= "https://w3id.org/abc/persons/000879#snapshot-000000000000"
           (drift/snapshot-iri participant)))
    (is (not= (drift/snapshot-iri participant)
              (drift/snapshot-iri mutated))
        "flipping the first hash nibble changes the snapshot IRI")))

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
    (is (contains? triples
                   [event-iri
                    "http://www.w3.org/ns/prov#wasAssociatedWith"
                    "https://w3id.org/abc/agents/editorial-board"]))
    (is (contains? triples [pre "http://www.w3.org/ns/prov#specializationOf"
                            "http://www.aozora.gr.jp/index_pages/person000879.html"]))))

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
