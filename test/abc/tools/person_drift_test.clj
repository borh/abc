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
