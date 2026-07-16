(ns abc.tools.parser-rq-publication-test
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.metadata-record :as metadata-record]
            [abc.tools.person-record :as person-record]
            [abc.tools.schema :as schema]
            [clojure.test :refer [deftest is]]))

(def schema-paths
  ["schemas/parser-rq-publication-policy.schema.json"
   "schemas/parser-rq-publication-work.schema.json"
   "schemas/parser-rq-publication-index.schema.json"
   "schemas/parser-rq-publication-aggregate.schema.json"])

(deftest publication-contracts-exist-and-are-closed
  (doseq [path schema-paths]
    (let [contract (files/read-json path)]
      (is (= false (get contract "additionalProperties")))
      (is (seq (schema/validation-errors contract {"attacker_field" true}))))))

(def expected-structure-checks
  ["tei_profile_valid"
   "preservation_schema_valid"
   "tei_manifest_valid"
   "plaintext_manifest_valid"
   "tei_manifest_references_preservation"
   "tei_manifest_references_validation_result"
   "tei_abc_projection_resolves_to_sidecar"
   "preservation_tei_pointers_resolve"
   "preservation_source_pointers_resolve"
   "plaintext_body_only"
   "required_construct_census_valid"])

(defn- projected-hash [value field]
  (hash/format-sha256 (hash/sha256-json-jcs (dissoc value field))))

(deftest publication-policy-binds-authority-and-semantics
  (let [policy (files/read-json "data/parser-rq-publication-policy-v1.json")
        validator (files/read-json
                   "../ab-validator/data/parser-rq-publication-validator-v1.json")]
    (is (= expected-structure-checks (get policy "structure_checks")))
    (is (= (str "sha256:" (files/sha256-file
                            "schemas/parser-ir-publication-preservation.schema.json"))
           (get-in policy ["preservation_schema" "hash"])))
    (is (= (get validator "validator_semantics_hash")
           (get-in policy ["validator" "semantics_hash"])))
    (is (= (get policy "policy_hash") (projected-hash policy "policy_hash")))))

(deftest every-pinned-work-has-a-characterized-exercise-census
  (let [fixtures (files/read-json "data/parser-rq-publication-fixtures-v1.json")
        works (get fixtures "works")]
    (is (= #{"000001_1" "000002_2" "000003_3"} (set (keys works))))
    (is (every? (fn [[work-id work]]
                  (and (= work-id (get work "work_id"))
                       (not= work-id (get work "metadata_work_id"))
                       (every? pos? (vals (get work "required_constructs")))))
                works))
    (is (= (get fixtures "census_hash")
           (projected-hash fixtures "census_hash")))))

(deftest qualification-metadata-and-person-inputs-validate
  (let [root "test/fixtures/parser-rq/publication-inputs"
        person (files/read-json
                (str root "/persons/abc-000000000001.json"))]
    (is (= :ok (person-record/validate! person)))
    (is (= "sha256:43a86aff2b6be08f4dbb0fec89691207c420a59f742ab123d63dd2a2b5b81ead"
           (person-record/record-hash person)))
    (doseq [work-id ["000001_1" "000002_2" "000003_3"]]
      (is (= :ok (metadata-record/validate!
                  (files/read-json (str root "/" work-id
                                        "/metadata-record.json"))))))))
