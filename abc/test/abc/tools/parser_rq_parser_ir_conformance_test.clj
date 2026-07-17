(ns abc.tools.parser-rq-parser-ir-conformance-test
  (:require [abc.tools.hash :as hash]
            [abc.tools.json :as json]
            [abc.tools.parser-release-qualification :as qualification]
            [abc.tools.parser-rq-parser-ir-conformance :as conformance]
            [abc.tools.schema :as schema]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(def identity-ref (str "sha256:" (apply str (repeat 64 "a"))))
(def validator-ref (str "sha256:" (apply str (repeat 64 "b"))))

(def policy
  (let [value
        {:schema_id
         "https://w3id.org/abc/schemas/parser-rq-parser-ir-conformance-policy.schema.json"
         :schema_version "1.0.0"
         :policy_id "abc/parser-rq-parser-ir-conformance/v1"
         :algorithm_version "parser-ir-schema-conformance-v1"
         :expected_work_ids ["valid" "invalid" "no-output"]
         :expected_work_set_hash
         (hash/format-sha256
          (hash/sha256-json-jcs ["valid" "invalid" "no-output"]))
         :parser_ir_schema_id
         "https://w3id.org/abc/schemas/parser-ir.schema.json"
         :parser_ir_schema_hash (schema/schema-hash "schemas/parser-ir.schema.json")
         :validator_semantics_hash validator-ref
         :generated_output_denominator "schema_valid_plus_schema_invalid"
         :no_output_semantics "available_failure_when_generated_outputs_zero"
         :status_mapping
         {:allowed_statuses ["measured" "no_parser_ir_output"]
          :values {"measured" 1.0
                   "no_parser_ir_output" "no_parser_ir_output"}}}]
    (assoc value :policy_hash
           (hash/format-sha256
            (hash/sha256-json-jcs
             (json/read-json-str
              (json/write-deterministic-json-str value)))))))

(defn- fixture-store
  []
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "parser-rq-ir-conformance"
                       (make-array java.nio.file.attribute.FileAttribute 0)))]
    {:root (.getPath root) :locators (atom {})}))

(defn- publish!
  [store value]
  (let [bytes (.getBytes (str (json/write-deterministic-json-str value) "\n")
                         java.nio.charset.StandardCharsets/UTF_8)
        sha256 (hash/format-sha256 (hash/sha256-bytes bytes))
        locator (str (subs sha256 7) ".json")
        file (io/file (:root store) locator)
        ref {:sha256 sha256 :bytes (alength bytes) :media_type "application/json"}]
    (spit file (String. bytes java.nio.charset.StandardCharsets/UTF_8))
    (swap! (:locators store) assoc sha256 locator)
    ref))

(defn- record-value
  [record-policy work-id status parser-ref ledger-ref]
  (cond-> {:schema_id
           "https://w3id.org/abc/schemas/parser-rq-parser-ir-conformance-work.schema.json"
           :schema_version "1.0.0"
           :work_id work-id
           :qualification_identity_ref identity-ref
           :policy_hash (:policy_hash record-policy)
           :status status}
    (= status "schema_valid") (assoc :parser_ir parser-ref)
    (= status "schema_invalid") (assoc :parser_ir parser-ref
                                       :validation_ledger ledger-ref
                                       :validation_witnesses ["invalid"])
    (= status "no_output") (assoc :failure {:kind "conversion_failed"
                                            :message "no output"})))

(defn- entry!
  ([store work-id status]
   (entry! store policy work-id status))
  ([store record-policy work-id status]
   (let [parser-ref (when (contains? #{"schema_valid" "schema_invalid"} status)
                      (publish! store {:parser_ir work-id}))
         ledger-ref (when (= "schema_invalid" status)
                      (publish! store {:errors ["invalid"]}))
         record-ref (publish! store (record-value record-policy work-id status
                                                  parser-ref ledger-ref))]
     {:work_id work-id :record record-ref})))

(deftest authenticates-record-and-required-referenced-blobs
  (let [store (fixture-store)
        valid (entry! store "valid" "schema_valid")
        invalid (entry! store "invalid" "schema_invalid")
        no-output (entry! store "no-output" "no_output")]
    (is (= "schema_valid"
           (get-in (conformance/authenticate-record store policy identity-ref valid)
                   [:record :status])))
    (is (= "schema_invalid"
           (get-in (conformance/authenticate-record store policy identity-ref invalid)
                   [:record :status])))
    (is (= "no_output"
           (get-in (conformance/authenticate-record store policy identity-ref no-output)
                   [:record :status])))
    (testing "Parser-IR mismatch is unavailable"
      (let [parser-sha (get-in (conformance/authenticate-record
                                store policy identity-ref valid)
                               [:record :parser_ir :sha256])]
        (spit (io/file (:root store) (get @(:locators store) parser-sha))
              "tampered")
        (is (= :unavailable
               (:status (conformance/authenticate-record
                         store policy identity-ref valid))))))
    (testing "ledger mismatch is unavailable, not a measured invalid result"
      (spit (io/file (:root store)
                     (get @(:locators store)
                          (get-in (conformance/authenticate-record
                                   store policy identity-ref invalid)
                                  [:record :validation_ledger :sha256])))
            "tampered")
      (is (= :unavailable
             (:status (conformance/authenticate-record
                       store policy identity-ref invalid)))))
    (testing "work-record mismatch is unavailable"
      (let [record-store (fixture-store)
            record-entry (entry! record-store "valid" "schema_valid")]
        (spit (io/file (:root record-store)
                       (get @(:locators record-store)
                            (get-in record-entry [:record :sha256])))
              "tampered")
        (is (= :unavailable
               (:status (conformance/authenticate-record
                         record-store policy identity-ref record-entry))))))))

(deftest aggregate-uses-generated-output-denominator
  (let [store (fixture-store)
        entries [(entry! store "valid" "schema_valid")
                 (entry! store "invalid" "schema_invalid")
                 (entry! store "no-output" "no_output")]
        records (mapv #(conformance/authenticate-record
                        store policy identity-ref %)
                      entries)
        aggregate (conformance/aggregate policy
                                         ["valid" "invalid" "no-output"]
                                         records)
        observation (conformance/derive-observation policy identity-ref aggregate)]
    (is (= {:expected_works 3
            :generated_outputs 2
            :schema_valid_outputs 1
            :schema_invalid_outputs 1
            :no_output_works 1}
           (select-keys aggregate
                        [:expected_works :generated_outputs :schema_valid_outputs
                         :schema_invalid_outputs :no_output_works])))
    (is (= 0.5 (:value observation)))
    (is (double? (:value observation)))))

(deftest all-valid-is-an-exact-double-pass-and-all-no-output-is-a-failure
  (let [valid-policy (assoc policy :expected_work_ids ["valid"])
        valid-policy (assoc valid-policy :expected_work_set_hash
                            (hash/format-sha256 (hash/sha256-json-jcs ["valid"])))
        valid-policy (assoc valid-policy :policy_hash
                            (#'conformance/projected-hash valid-policy :policy_hash))
        valid-store (fixture-store)
        valid-record (conformance/authenticate-record
                      valid-store valid-policy identity-ref
                      (entry! valid-store valid-policy "valid" "schema_valid"))
        valid-aggregate (conformance/aggregate valid-policy ["valid"] [valid-record])
        valid-observation (conformance/derive-observation
                           valid-policy identity-ref valid-aggregate)
        none-policy (assoc policy :expected_work_ids ["no-output"])
        none-policy (assoc none-policy :expected_work_set_hash
                           (hash/format-sha256 (hash/sha256-json-jcs ["no-output"])))
        none-policy (assoc none-policy :policy_hash
                           (#'conformance/projected-hash none-policy :policy_hash))
        none-store (fixture-store)
        none-record (conformance/authenticate-record
                     none-store none-policy identity-ref
                     (entry! none-store none-policy "no-output" "no_output"))
        none-aggregate (conformance/aggregate none-policy ["no-output"] [none-record])
        none-observation (conformance/derive-observation none-policy identity-ref
                                                         none-aggregate)
        predicate {:predicate_id :parser-ir-schema-validation
                   :dimension "parser ir"
                   :instrument "fixture"
                   :observed_key :parser_ir_schema_validation
                   :expected {:comparator := :value 1.0}
                   :unit "ratio"}]
    (is (= 1.0 (:value valid-observation)))
    (is (double? (:value valid-observation)))
    (is (= :pass (:verdict (qualification/evaluate-predicate
                            predicate
                            {:parser_ir_schema_validation valid-observation}))))
    (is (= :no-parser-ir-output (:value none-observation)))
    (is (= :fail (:verdict (qualification/evaluate-predicate
                            predicate
                            {:parser_ir_schema_validation none-observation}))))))

(deftest omission-and-authority-mismatch-are-unavailable
  (let [store (fixture-store)
        valid (entry! store "valid" "schema_valid")
        authenticated (conformance/authenticate-record store policy identity-ref valid)]
    (is (= "unavailable"
           (:status (conformance/aggregate policy
                                           ["valid" "invalid" "no-output"]
                                           [authenticated]))))
    (is (= :unavailable
           (:status (conformance/authenticate-record
                     store (assoc policy :validator_semantics_hash identity-ref)
                     identity-ref valid))))))
