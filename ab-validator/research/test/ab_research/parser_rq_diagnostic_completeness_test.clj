(ns ab-research.parser-rq-diagnostic-completeness-test
  (:require [ab-research.files :as files]
            [ab-research.hash :as hash]
            [charred.api :as charred]
            [soranoha.core.json :as record-json]
            [ab-research.parser-release-qualification :as qualification]
            [ab-research.parser-rq-diagnostic-completeness :as diagnostic]
            [ab-research.schema :as schema]
            [clojure.string :as string]
            [clojure.walk :as walk]
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

(defn- sealed-policy
  "Reseal a policy after editing its membership, so tests exercise the
  instrument rather than a hash mismatch they did not mean to introduce."
  [value]
  (assoc value :policy_hash
         (hash/format-sha256
          (hash/sha256-json-jcs
           (charred/read-json
            (record-json/write-deterministic-json-str
             (dissoc value :policy_hash)))))))

(defn- policy-for
  "A policy governing exactly `expectation`: work id to expected codes."
  [expectation]
  (let [work-ids (vec (sort (keys expectation)))]
    (sealed-policy (assoc policy
                          :expected_work_ids work-ids
                          :expected_work_set_hash
                          (hash/format-sha256 (hash/sha256-json-jcs work-ids))
                          :expected_diagnostics expectation))))

(def fixture-policy (policy-for {"valid" []}))

(defn- utf8-bytes
  [text]
  (.getBytes text java.nio.charset.StandardCharsets/UTF_8))

(defn- input
  [raw]
  {:work_id "valid"
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

(defn- envelope-bytes
  [& codes]
  (utf8-bytes
   (record-json/write-deterministic-json-str
    {:schemaVersion 3
     ;; The v3 wire carries `kind` and `code` as parallel closed enums, one
     ;; snake_case and one kebab-case for the same diagnostic.
     :data (mapv (fn [code]
                   {:kind (string/replace code "-" "_")
                    :code code
                    :severity "warning"
                    :source "source"
                    :span {:start 0 :end 1}})
                 codes)})))

(def ^:private ratio-predicate
  {:predicate_id :diagnostic-completeness
   :dimension "diagnostics"
   :instrument "fixture"
   :observed_key :diagnostic_completeness
   :expected {:comparator := :value 1.0}
   :unit "ratio"})

(defn- measure
  "Run one governed expectation against one set of observed codes per work."
  [expectation observed]
  (let [governing (policy-for expectation)
        records (mapv (fn [[work-id codes]]
                        (:record (diagnostic/derive-work
                                  governing hash-a
                                  {:work_id work-id
                                   :attempt_disposition "parsed"
                                   :bytes (apply envelope-bytes codes)})))
                      (sort observed))
        aggregate (diagnostic/aggregate governing (vec (sort (keys expectation)))
                                        records)]
    {:records records
     :aggregate aggregate
     :observation (diagnostic/derive-observation governing hash-a aggregate)}))

(deftest a-clean-corpus-passes-by-matching-its-expectation-not-by-vacuity
  ;; On a corpus where every work expects nothing and emits nothing, the outcome
  ;; is unchanged; it represents 3/3 matching works rather than an arbitrary constant.
  (let [{:keys [aggregate observation]}
        (measure {"a" [] "b" [] "c" []} {"a" [] "b" [] "c" []})]
    (is (= 1.0 (:diagnostic_completeness aggregate)))
    (is (= 3 (:matching_works aggregate)))
    (is (true? (:vacuous aggregate)))
    (is (= 1.0 (:value observation)))
    (is (= :pass (:verdict (qualification/evaluate-predicate
                            ratio-predicate
                            {:diagnostic_completeness observation}))))))

(deftest a-work-emitting-a-diagnostic-it-should-not-drops-the-ratio
  (let [{:keys [aggregate observation]}
        (measure {"a" [] "b" [] "c" []} {"a" [] "b" ["unclosed-bracket"] "c" []})]
    (is (= 2 (:matching_works aggregate)))
    ;; IEEE division, not `(double (/ 2 3))`. Those two differ in the last
    ;; bit, and the capture driver that writes this value into evidence does
    ;; the IEEE one.
    (is (= (/ 2.0 3.0) (:diagnostic_completeness aggregate)))
    (is (false? (:vacuous aggregate)))
    (is (= :fail (:verdict (qualification/evaluate-predicate
                            ratio-predicate
                            {:diagnostic_completeness observation}))))))

(deftest a-work-failing-to-emit-an-expected-diagnostic-drops-the-ratio
  (let [{:keys [records aggregate observation]}
        (measure {"a" ["unclosed-bracket"]} {"a" []})]
    (is (false? (:matches_expectation (first records))))
    (is (= ["unclosed-bracket"] (:expected_diagnostics (first records))))
    (is (= [] (:observed_diagnostics (first records))))
    (is (= 0.0 (:diagnostic_completeness aggregate)))
    (is (= :fail (:verdict (qualification/evaluate-predicate
                            ratio-predicate
                            {:diagnostic_completeness observation}))))))

(deftest a-work-emitting-exactly-what-it-should-matches
  (let [{:keys [records aggregate]}
        (measure {"a" ["unclosed-bracket"]} {"a" ["unclosed-bracket"]})]
    (is (true? (:matches_expectation (first records))))
    (is (= 1.0 (:diagnostic_completeness aggregate)))
    (is (false? (:vacuous aggregate)))))

(deftest a-repeated-code-is-a-different-claim-than-a-single-one
  (let [{:keys [records]}
        (measure {"a" ["unclosed-bracket"]}
                 {"a" ["unclosed-bracket" "unclosed-bracket"]})]
    (is (false? (:matches_expectation (first records))))))

(deftest a-work-with-no-governed-expectation-cannot-be-measured
  ;; Membership and expectation are separate claims, and a policy that names a
  ;; work in one but not the other has no authority to measure it.
  (let [ungoverned (sealed-policy
                    (assoc fixture-policy
                           :expected_diagnostics {"other" []}))]
    (is (= "unavailable"
           (get-in (diagnostic/derive-work ungoverned hash-a
                                           (input (envelope-bytes)))
                   [:record :status])))))

(deftest valid-empty-envelope-is-an-explicit-vacuous-pass
  (let [raw (utf8-bytes "{\"schemaVersion\":3,\"data\":[]}")
        first-result (diagnostic/derive-work fixture-policy hash-a
                                             (assoc (input raw) :authorization :allow))
        second-result (diagnostic/derive-work fixture-policy hash-a
                                              (assoc (input raw) :authorization :deny))
        aggregate (diagnostic/aggregate fixture-policy ["valid"]
                                        [(:record first-result)])
        observation (diagnostic/derive-observation fixture-policy hash-a aggregate)]
    (is (= first-result second-result))
    (is (= "complete" (get-in first-result [:record :status])))
    (is (= 0 (get-in first-result [:record :emitted_diagnostics])))
    (is (= 1.0 (:value observation)))
    (is (double? (:value observation)))
    (is (= {:expected_works 1
            :matching_works 1
            :diagnostic_count 0
            :works_with_diagnostics 0
            :vacuous true}
           (:details observation)))))

(deftest recursively-keywordized-policy-preserves-status-projection
  (let [keywordized (walk/keywordize-keys fixture-policy)
        raw (utf8-bytes "{\"schemaVersion\":3,\"data\":[]}")
        record (:record (diagnostic/derive-work keywordized hash-a (input raw)))
        aggregate (diagnostic/aggregate keywordized ["valid"] [record])
        observation (diagnostic/derive-observation keywordized hash-a aggregate)]
    (is (= "measured" (:status aggregate)))
    (is (= 1.0 (:value observation)))))

(deftest valid-diagnostic-envelope-counts-and-compares-its-entries
  (let [raw (envelope-bytes "unclosed-bracket")
        record (:record (diagnostic/derive-work fixture-policy hash-a (input raw)))
        aggregate (diagnostic/aggregate fixture-policy ["valid"] [record])]
    (is (= "complete" (:status record)))
    (is (= 1 (:emitted_diagnostics record)))
    (is (= ["unclosed-bracket"] (:observed_diagnostics record)))
    (is (= 1 (:diagnostic_count aggregate)))
    (is (false? (:vacuous aggregate)))
    ;; The fixture work is governed to emit nothing, so an emitted diagnostic
    ;; is a mismatch even though the envelope itself is perfectly valid. That
    ;; distinction is the whole point of the rotation.
    (is (false? (:matches_expectation record)))
    (is (= 0.0 (:diagnostic_completeness aggregate)))))

(deftest malformed-authenticated-bytes-are-an-available-failure
  (let [result (diagnostic/derive-work fixture-policy hash-a
                                       (input (utf8-bytes "{\"schemaVersion\":3,\"data\":[")))
        aggregate (diagnostic/aggregate fixture-policy ["valid"] [(:record result)])
        observation (diagnostic/derive-observation fixture-policy hash-a aggregate)
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
           (get-in (diagnostic/derive-work fixture-policy hash-a
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
    (let [record (:record (diagnostic/derive-work fixture-policy hash-a
                                                  (input (utf8-bytes "{\"schemaVersion\":3,\"data\":[]}"))))]
      (is (= "unavailable"
             (:status (diagnostic/aggregate fixture-policy ["valid" "missing"]
                                            [record])))))))
