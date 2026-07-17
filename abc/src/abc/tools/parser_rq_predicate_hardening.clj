(ns abc.tools.parser-rq-predicate-hardening
  "Immutable construction guard for predicate-4 and predicate-5 observations.

  This module catches incoherent composition early. The release gate's
  `coherent-observations?` precondition remains the authoritative backstop."
  (:require [abc.tools.parser-release-qualification :as qualification]
            [abc.tools.parser-rq-capture :as capture]))

(def ^:private result-keys
  #{:instrument :policy_hash :corpus_list_hash :expected_work_ids :observation})

(def ^:private contracts
  {:diagnostic-envelope-completeness
   {:identity-key :diagnostic_envelope_completeness
    :measurement-key :diagnostic_completeness}
   :parser-ir-schema-conformance
   {:identity-key :parser_ir_schema_conformance
    :measurement-key :parser_ir_schema_validation}})

(defn- fail!
  [message data]
  (throw (ex-info message data)))

(defn- validate-result!
  [identity expected-instrument result]
  (when-not (and (map? result) (= result-keys (set (keys result))))
    (fail! "predicate-hardening result violates its closed contract"
           {:instrument expected-instrument}))
  (when-not (= expected-instrument (:instrument result))
    (fail! "predicate-hardening instrument is swapped or mislabeled"
           {:expected expected-instrument :actual (:instrument result)}))
  (let [{:keys [identity-key]} (get contracts expected-instrument)
        expected-policy (get-in identity [:instrument_versions identity-key])
        expected-ref (qualification/qualification-identity-ref identity)
        work-ids (:expected_work_ids result)
        envelope (:observation result)]
    (when-not (and (string? expected-policy)
                   (= expected-policy (:policy_hash result)))
      (fail! "predicate-hardening policy identity mismatch"
             {:instrument expected-instrument}))
    (when-not (= (:corpus_list_hash identity) (:corpus_list_hash result))
      (fail! "predicate-hardening corpus identity mismatch"
             {:instrument expected-instrument}))
    (when-not (and (vector? work-ids)
                   (seq work-ids)
                   (= (count work-ids) (count (set work-ids)))
                   (every? #(and (string? %) (not-empty %)) work-ids))
      (fail! "predicate-hardening membership is not a closed nonempty set"
             {:instrument expected-instrument}))
    (when (seq (capture/envelope-errors envelope))
      (fail! "predicate-hardening observation envelope is invalid"
             {:instrument expected-instrument}))
    (when-not (= expected-ref (:identity_ref envelope))
      (fail! "predicate-hardening candidate identity mismatch"
             {:instrument expected-instrument}))
    (when-not (qualification/observation-available? (:value envelope))
      (fail! "predicate-hardening observation is unavailable"
             {:instrument expected-instrument})))
  result)

(defn install-observations
  "Return a new measurement map after validating two coherent closed results.

  This is a construction guard only: it performs no capture, derivation,
  admission, registry lookup, or release decision. Gate coherence remains the
  release authority."
  [measurements qualification-identity diagnostic-result parser-ir-result]
  (when-not (map? measurements)
    (fail! "measurements must be a map" {}))
  (let [diagnostic (validate-result! qualification-identity
                                     :diagnostic-envelope-completeness
                                     diagnostic-result)
        parser-ir (validate-result! qualification-identity
                                    :parser-ir-schema-conformance
                                    parser-ir-result)
        target-keys (set (map :measurement-key (vals contracts)))]
    (when (some #(contains? measurements %) target-keys)
      (fail! "predicate-hardening observations may not overwrite measurements"
             {:target-keys target-keys}))
    (when (= (:policy_hash diagnostic) (:policy_hash parser-ir))
      (fail! "predicate-hardening instruments require distinct policy identities" {}))
    (when-not (= (set (:expected_work_ids diagnostic))
                 (set (:expected_work_ids parser-ir)))
      (fail! "predicate-hardening work memberships differ" {}))
    (assoc measurements
           :diagnostic_completeness (:observation diagnostic)
           :parser_ir_schema_validation (:observation parser-ir))))
