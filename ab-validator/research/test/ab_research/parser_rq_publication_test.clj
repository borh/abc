(ns ab-research.parser-rq-publication-test
  (:require [ab-research.files :as files]
            [ab-research.hash :as hash]
            [soranoha.aozora.person-record :as person-record]
            [ab-research.parser-rq-capture :as capture]
            [ab-research.parser-rq-publication :as publication]
            [ab-research.parser-release-qualification :as qualification]
            [ab-research.schema :as schema]
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
                   "../data/parser-rq-publication-validator-v1.json")]
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
    (is (= :ok (person-record/validate! (files/read-json "../../soranoha/schemas/person-record.schema.json") person)))
    (is (= "sha256:407f3a6948033ef5f4a5d2dcb52b8510bf610a19db02d4c6fa31b28829209b28"
           (person-record/record-hash person)))
    (doseq [work-id ["000001_1" "000002_2" "000003_3"]]
      (is (= :ok (if (seq (schema/validation-errors
                           (files/read-json "../../soranoha/schemas/metadata-record.schema.json")
                           (files/read-json (str root "/" work-id "/metadata-record.json")))) :invalid :ok))))))

(deftest publication-envelope-uses-exact-work-denominator
  (let [root "test/fixtures/parser-rq/publication-capture"
        envelope (publication/derive-publication-envelope
                  {:root (str root "/store")}
                  (files/read-json (str root "/manifest.json"))
                  (files/read-json (str root "/index.json"))
                  (files/read-json (str root "/identity.json")))]
    (is (= 1.0000M (:value envelope)))
    (is (empty? (capture/envelope-errors envelope)))
    (is (= "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
           (:identity_ref envelope)))
    (is (= {:expected 3 :parsed 3 :eligible 3 :passed 3 :failed 0 :timed_out 0}
           (get-in envelope [:details :counts])))))

(deftest publication-observation-installs-an-envelope-not-a-scalar
  (let [envelope {:value 1.0M
                  :identity_ref
                  "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"}]
    (is (= envelope
           (:publication_structure
            (qualification/install-publication-observation {} envelope))))))
