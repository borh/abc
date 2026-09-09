(ns ab-research.parser-rq-parser-ir-conformance-test
  (:require [ab-research.hash :as hash]
            [ab-research.files :as files]
            [soranoha.core.json :as record-json]
            [ab-research.parser-release-qualification :as qualification]
            [ab-research.parser-rq-parser-ir-conformance :as conformance]
            [ab-research.schema :as schema]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]))

(def identity-ref (str "sha256:" (apply str (repeat 64 "a"))))
(def committed-policy
  (let [raw (files/read-json
             "data/parser-rq-parser-ir-conformance-policy-v1.json")
        top-level (into {} (map (fn [[key value]] [(keyword key) value])) raw)
        status-map (get raw "status_mapping")]
    (assoc top-level :status_mapping
           {:allowed_statuses (get status-map "allowed_statuses")
            :values (get status-map "values")})))

(def policy
  (let [work-ids ["valid" "invalid" "no-output"]
        value (assoc committed-policy
                     :parser_ir_schema_hash (schema/schema-hash "schemas/parser-ir.schema.json")
                     :expected_work_ids work-ids
                     :expected_work_set_hash
                     (hash/format-sha256 (hash/sha256-json-jcs work-ids)))]
    (assoc value :policy_hash
           (#'conformance/projected-hash value :policy_hash))))

(defn- fixture-store
  []
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "parser-rq-ir-conformance"
                       (make-array java.nio.file.attribute.FileAttribute 0)))]
    {:root (.getPath root) :locators (atom {})}))

(defn- publish!
  [store value]
  (let [bytes (.getBytes (str (record-json/write-deterministic-json-str value) "\n")
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

(deftest committed-policy-is-schema-valid-and-closed
  (is (nil? (schema/validation-errors
             (files/read-json
              "schemas/parser-rq-parser-ir-conformance-policy.schema.json")
             (files/read-json
              "data/parser-rq-parser-ir-conformance-policy-v1.json"))))
  (let [store (fixture-store)
        entry (entry! store "valid" "schema_valid")]
    (doseq [mutated [(assoc committed-policy :parser_ir_schema_hash identity-ref)
                     (update committed-policy :expected_work_ids conj "extra")
                     (assoc-in committed-policy [:status_mapping :values "measured"] 0.5)]]
      (is (= :unavailable
             (:status (conformance/authenticate-record
                       store mutated identity-ref entry)))))))

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

(deftest recursively-keywordized-policy-preserves-status-projection
  (let [one-work-policy (assoc policy :expected_work_ids ["valid"])
        one-work-policy (assoc one-work-policy :expected_work_set_hash
                               (hash/format-sha256
                                (hash/sha256-json-jcs ["valid"])))
        one-work-policy (assoc one-work-policy :policy_hash
                               (#'conformance/projected-hash
                                one-work-policy :policy_hash))
        keywordized (walk/keywordize-keys one-work-policy)
        store (fixture-store)
        record (conformance/authenticate-record
                store keywordized identity-ref
                (entry! store keywordized "valid" "schema_valid"))
        aggregate (conformance/aggregate keywordized ["valid"] [record])
        observation (conformance/derive-observation keywordized identity-ref aggregate)]
    (is (= :ok (:status record)) (pr-str record))
    (is (= "measured" (:status aggregate)))
    (is (= 1.0 (:value observation)))))

(deftest two-valid-of-three-generated-outputs-is-ieee-division
  ;; 2/3 is the smallest ratio this instrument can produce where the two division
  ;; paths disagree: `(double (/ 2 3))` builds an exact Ratio and rounds it
  ;; through BigDecimal at DECIMAL64, landing on 0.6666666666666667, while IEEE
  ;; `(/ 2.0 3.0)` gives 0.6666666666666666. Other environments (including the
  ;; Python capture driver that writes this value into content-addressed
  ;; evidence) use IEEE division, so both implementations must agree exactly.
  ;; The twin of this guard is in parser_rq_diagnostic_completeness_test.clj.
  ;;
  ;; The committed corpus currently measures 1.0, which is exact under either
  ;; path; that is why the defect was invisible in the report rather than absent.
  (let [work-ids ["valid" "valid-2" "invalid"]
        ratio-policy (assoc policy
                            :expected_work_ids work-ids
                            :expected_work_set_hash
                            (hash/format-sha256 (hash/sha256-json-jcs work-ids)))
        ratio-policy (assoc ratio-policy :policy_hash
                            (#'conformance/projected-hash ratio-policy :policy_hash))
        store (fixture-store)
        records (mapv (fn [[work-id status]]
                        (conformance/authenticate-record
                         store ratio-policy identity-ref
                         (entry! store ratio-policy work-id status)))
                      [["valid" "schema_valid"]
                       ["valid-2" "schema_valid"]
                       ["invalid" "schema_invalid"]])
        aggregate (conformance/aggregate ratio-policy work-ids records)
        observation (conformance/derive-observation ratio-policy identity-ref
                                                    aggregate)]
    (is (= {:expected_works 3
            :generated_outputs 3
            :schema_valid_outputs 2
            :schema_invalid_outputs 1
            :no_output_works 0}
           (select-keys aggregate
                        [:expected_works :generated_outputs :schema_valid_outputs
                         :schema_invalid_outputs :no_output_works])))
    (is (= (/ 2.0 3.0) (:value observation)))
    (is (not= (double (/ 2 3)) (:value observation)))))

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
