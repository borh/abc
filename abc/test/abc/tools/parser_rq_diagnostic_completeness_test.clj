(ns abc.tools.parser-rq-diagnostic-completeness-test
  (:require [abc.tools.files :as files]
            [abc.tools.json :as json]
            [abc.tools.parser-release-qualification :as qualification]
            [abc.tools.parser-rq-diagnostic-completeness :as diagnostic]
            [abc.tools.schema :as schema]
            [clojure.test :refer [deftest is testing]]))

(def hash-a (str "sha256:" (apply str (repeat 64 "a"))))
(def policy
  (let [raw (files/read-json
             "data/parser-rq-diagnostic-completeness-policy-v1.json")
        top-level (into {} (map (fn [[key value]] [(keyword key) value])) raw)
        status-map (get raw "status_mapping")]
    (assoc top-level :status_mapping
           {:allowed_statuses (get status-map "allowed_statuses")
            :values (get status-map "values")})))

(defn- utf8-bytes
  [text]
  (.getBytes text java.nio.charset.StandardCharsets/UTF_8))

(defn- input
  [raw]
  {:work_id "work-a"
   :attempt_disposition "parsed"
   :bytes raw})

(deftest committed-policy-is-schema-valid-and-closed
  (is (nil? (schema/validation-errors
             (files/read-json
              "schemas/parser-rq-diagnostic-completeness-policy.schema.json")
             (files/read-json
              "data/parser-rq-diagnostic-completeness-policy-v1.json"))))
  (doseq [mutated [(assoc policy :raw_diagnostic_schema_hash hash-a)
                   (update policy :expected_work_ids conj "extra")
                   (assoc-in policy [:status_mapping :values "measured"] 0.5)]]
    (is (= "unavailable"
           (get-in (diagnostic/derive-work
                    mutated hash-a
                    (input (utf8-bytes "{\"schemaVersion\":3,\"data\":[]}")))
                   [:record :status])))))

(deftest valid-empty-envelope-is-an-explicit-vacuous-pass
  (let [raw (utf8-bytes "{\"schemaVersion\":3,\"data\":[]}")
        first-result (diagnostic/derive-work policy hash-a
                                             (assoc (input raw) :authorization :allow))
        second-result (diagnostic/derive-work policy hash-a
                                              (assoc (input raw) :authorization :deny))
        aggregate (diagnostic/aggregate policy ["work-a"]
                                        [(:record first-result)])
        observation (diagnostic/derive-observation policy hash-a aggregate)]
    (is (= first-result second-result))
    (is (= "complete" (get-in first-result [:record :status])))
    (is (= 0 (get-in first-result [:record :emitted_diagnostics])))
    (is (= 1.0 (:value observation)))
    (is (double? (:value observation)))
    (is (= {:diagnostic_count 0
            :works_with_diagnostics 0
            :vacuous true}
           (:details observation)))))

(deftest valid-diagnostic-envelope-counts-complete-entries
  (let [raw (utf8-bytes
             (json/write-deterministic-json-str
              {:schemaVersion 3
               :data [{:kind "unclosed_bracket"
                       :code "unclosed-bracket"
                       :severity "warning"
                       :source "source"
                       :span {:start 0 :end 1}}]}))
        record (:record (diagnostic/derive-work policy hash-a (input raw)))
        aggregate (diagnostic/aggregate policy ["work-a"] [record])]
    (is (= "complete" (:status record)))
    (is (= 1 (:emitted_diagnostics record)))
    (is (= 1 (:complete_diagnostics record)))
    (is (= 1 (:diagnostic_count aggregate)))
    (is (false? (:vacuous aggregate)))))

(deftest malformed-authenticated-bytes-are-an-available-failure
  (let [result (diagnostic/derive-work policy hash-a
                                       (input (utf8-bytes "{\"schemaVersion\":3,\"data\":[")))
        aggregate (diagnostic/aggregate policy ["work-a"] [(:record result)])
        observation (diagnostic/derive-observation policy hash-a aggregate)
        predicate {:predicate_id :diagnostic-completeness
                   :dimension "diagnostics"
                   :instrument "fixture"
                   :observed_key :diagnostic_completeness
                   :expected {:comparator := :value 1.0}
                   :unit "ratio"}]
    (is (= "invalid_diagnostic_envelope" (get-in result [:record :status])))
    (is (seq (get-in result [:record :validation_witnesses])))
    (is (= (get-in result [:record :validation_ledger])
           (get-in result [:ledger_blob :ref])))
    (is (bytes? (get-in result [:ledger_blob :bytes])))
    (is (= "invalid_diagnostic_envelope" (:status aggregate)))
    (is (= :invalid-diagnostic-envelope (:value observation)))
    (is (= :fail
           (:verdict (qualification/evaluate-predicate
                      predicate {:diagnostic_completeness observation}))))))

(deftest unavailable-and-membership-fail-closed
  (testing "absent authenticated bytes"
    (is (= "unavailable"
           (get-in (diagnostic/derive-work policy hash-a
                                           (input nil))
                   [:record :status]))))
  (testing "identity and validator-policy mismatches"
    (is (= "unavailable"
           (get-in (diagnostic/derive-work policy "not-a-hash"
                                           (input (utf8-bytes "{}")))
                   [:record :status])))
    (is (= "unavailable"
           (get-in (diagnostic/derive-work
                    (assoc policy :validator_semantics_hash hash-a)
                    hash-a (input (utf8-bytes "{}")))
                   [:record :status]))))
  (testing "closed membership"
    (let [record (:record (diagnostic/derive-work policy hash-a
                                                  (input (utf8-bytes "{\"schemaVersion\":3,\"data\":[]}"))))]
      (is (= "unavailable"
             (:status (diagnostic/aggregate policy ["work-a" "missing"]
                                            [record])))))))
