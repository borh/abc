(ns ab-research.parser-rq-diagnostic-completeness
  "Pure predicate-4 derivation from authenticated diagnostic-envelope bytes."
  (:require [ab-research.files :as files]
            [ab-research.hash :as hash]
            [charred.api :as charred]
            [soranoha.core.json :as record-json]
            [ab-research.parser-rq-capture :as capture]
            [ab-research.schema :as schema]
            [clojure.string :as string]))

(def ^:private work-schema-id
  "https://w3id.org/soranoha/schemas/parser-rq-diagnostic-completeness-work.schema.json")

(def ^:private aggregate-schema-id
  "https://w3id.org/soranoha/schemas/parser-rq-diagnostic-completeness-aggregate.schema.json")

(def ^:private raw-diagnostics-schema
  (delay (files/read-json "schemas/parser-rq-ab-aozora-diagnostics-v3.schema.json")))

(def ^:private policy-schema
  (delay (files/read-json "schemas/parser-rq-diagnostic-completeness-policy.schema.json")))

(def ^:private work-schema
  (delay (files/read-json "schemas/parser-rq-diagnostic-completeness-work.schema.json")))

(def ^:private aggregate-schema
  (delay (files/read-json "schemas/parser-rq-diagnostic-completeness-aggregate.schema.json")))

(defn- normalize-policy
  [policy]
  (let [policy (into {} (map (fn [[key value]]
                               [(if (string? key) (keyword key) key) value]))
                     policy)
        status-map (:status_mapping policy)
        expectation (:expected_diagnostics policy)]
    (cond-> (assoc policy :status_mapping (capture/normalize-status-map status-map))
      ;; Work ids are JSON object keys, so a recursively keywordized policy
      ;; arrives with keyword keys and a shallow one with strings. Both name
      ;; the same works; the wire form is the string.
      (map? expectation)
      (assoc :expected_diagnostics
             (into {}
                   (map (fn [[work codes]]
                          [(if (keyword? work) (name work) work) (vec codes)]))
                   expectation)))))

(defn- projected-hash
  [value field]
  (hash/format-sha256
   (hash/sha256-json-jcs
    (charred/read-json
     (record-json/write-deterministic-json-str (dissoc value field))))))

(defn- valid-policy?
  [policy]
  (and (nil? (schema/validation-errors @policy-schema policy))
       (= (:policy_hash policy) (projected-hash policy :policy_hash))
       (= (:expected_work_set_hash policy)
          (hash/format-sha256
           (hash/sha256-json-jcs (:expected_work_ids policy))))
       ;; Every expected work carries a governed expectation and no other work
       ;; does. Without this the ratio could be taken over a denominator the
       ;; corpus never authorized.
       (= (set (:expected_work_ids policy))
          (set (keys (:expected_diagnostics policy))))
       (= (:raw_diagnostic_schema_hash policy)
          (schema/schema-hash
           "schemas/parser-rq-ab-aozora-diagnostics-v3.schema.json"))))

(defn- blob-ref
  [bytes]
  {:sha256 (hash/format-sha256 (hash/sha256-bytes bytes))
   :bytes (alength bytes)
   :media_type "application/json"})

(defn- base-record
  [policy qualification-identity-ref work-id attempt-disposition]
  {:schema_id work-schema-id
   :schema_version "1.0.0"
   :work_id work-id
   :qualification_identity_ref qualification-identity-ref
   :policy_hash (:policy_hash policy)
   :attempt_disposition attempt-disposition})

(defn- strict-utf8
  [bytes]
  (let [decoder (doto (.newDecoder java.nio.charset.StandardCharsets/UTF_8)
                  (.onMalformedInput java.nio.charset.CodingErrorAction/REPORT)
                  (.onUnmappableCharacter java.nio.charset.CodingErrorAction/REPORT))]
    (str (.decode decoder (java.nio.ByteBuffer/wrap bytes)))))

(defn- validation-result
  [bytes]
  (try
    (let [value (charred/read-json (strict-utf8 bytes))
          errors (schema/validation-errors @raw-diagnostics-schema value)]
      (if (seq errors)
        {:errors (mapv pr-str (sort-by pr-str errors))}
        {:value value}))
    (catch Exception error
      {:errors [(str "diagnostic envelope is not strict valid JSON: "
                     (.getMessage error))]})))

(defn- ledger-blob
  [errors]
  (let [bytes (.getBytes (str (record-json/write-deterministic-json-str
                               {:errors errors}) "\n")
                         java.nio.charset.StandardCharsets/UTF_8)]
    {:ref (blob-ref bytes) :bytes bytes}))

(defn derive-work
  "Derive one work record from bytes already authenticated by the capture layer.

  `authenticated` carries work identity, attempt disposition, and the exact byte
  array. Additional disposition or authorization values are intentionally
  ignored: predicate 4 measures only diagnostic-envelope completeness."
  [policy qualification-identity-ref
   {:keys [work_id attempt_disposition bytes]}]
  (let [policy (normalize-policy policy)
        base (base-record policy qualification-identity-ref
                          work_id attempt_disposition)]
    (cond
      (or (not (valid-policy? policy))
          (not (and (string? qualification-identity-ref)
                    (re-matches hash/hash-pattern qualification-identity-ref)))
          (not (some #{work_id} (:expected_work_ids policy)))
          (not (contains? (:expected_diagnostics policy) work_id))
          (not (string? work_id))
          (string/blank? work_id)
          (not (contains? #{"parsed" "failed" "timeout"}
                          attempt_disposition)))
      {:record (assoc base :status "unavailable" :reason "aggregate_invalid")}

      (not (bytes? bytes))
      {:record (assoc base :status "unavailable" :reason "blob_unavailable")}

      :else
      (let [raw-ref (blob-ref bytes)
            result (validation-result bytes)]
        (if-let [value (:value result)]
          (let [diagnostics (or (get value "data") (:data value))
                diagnostic-count (count diagnostics)
                observed (vec (sort (map #(or (get % "code") (:code %)) diagnostics)))
                expected (vec (sort (get (:expected_diagnostics policy) work_id)))]
            {:record (assoc base
                            :raw_diagnostics raw-ref
                            :status "complete"
                            :emitted_diagnostics diagnostic-count
                            :expected_diagnostics expected
                            :observed_diagnostics observed
                            ;; Sorted sequence equality, so a repeated code is
                            ;; a different claim than a single one.
                            :matches_expectation (= observed expected)
                            :vacuous (zero? diagnostic-count))})
          (let [errors (:errors result)
                ledger (ledger-blob errors)]
            {:record (assoc base
                            :raw_diagnostics raw-ref
                            :status "invalid_diagnostic_envelope"
                            :validation_ledger (:ref ledger)
                            :validation_witnesses (vec (take 20 errors)))
             :ledger_blob ledger}))))))

(defn aggregate
  "Fold an exact closed workset. Membership or contract incoherence is unavailable."
  [policy expected-work-ids work-records]
  (let [policy (normalize-policy policy)
        base {:schema_id aggregate-schema-id
              :schema_version "1.0.0"
              :qualification_identity_ref (:qualification_identity_ref
                                           (first work-records))
              :policy_hash (:policy_hash policy)
              :expected_works (count expected-work-ids)
              :work_count (count work-records)}
        membership-errors (capture/closed-membership-errors expected-work-ids
                                                            work-records)
        coherent? (every? #(and (nil? (schema/validation-errors @work-schema %))
                                (= (:policy_hash policy) (:policy_hash %))
                                (= (:qualification_identity_ref base)
                                   (:qualification_identity_ref %)))
                          work-records)
        unavailable? (some #(= "unavailable" (:status %)) work-records)
        invalid-count (count (filter #(= "invalid_diagnostic_envelope"
                                         (:status %))
                                     work-records))]
    (cond
      (or (not (valid-policy? policy))
          (not= (set expected-work-ids) (set (:expected_work_ids policy)))
          (empty? expected-work-ids)
          (seq membership-errors)
          (not coherent?))
      (assoc base :status "unavailable" :reason "index_incomplete")

      unavailable?
      (assoc base :status "unavailable" :reason "blob_unavailable")

      (pos? invalid-count)
      (assoc base
             :status "invalid_diagnostic_envelope"
             :invalid_work_count invalid-count)

      :else
      (let [diagnostic-count (reduce + 0 (map :emitted_diagnostics work-records))
            works-with-diagnostics (count (filter #(pos? (:emitted_diagnostics %))
                                                  work-records))
            matching-works (count (filter :matches_expectation work-records))]
        (assoc base
               :status "measured"
               ;; The ratio the predicate declares: works whose observed
               ;; diagnostics equal their governed expectation, over the works
               ;; the corpus expects. A clean corpus passes at 3/3 because
               ;; three works each matched an empty expectation, not because
               ;; there was nothing to measure.
               ;; IEEE division of two doubles, not `(double (/ int int))`.
               ;; The latter builds an exact Ratio and rounds through
               ;; BigDecimal, which differs from other languages'
               ;; division in the last bit (for example, 2/3 becomes ...667 here and
               ;; ...666 in the Python capture driver). This value lands in
               ;; content-addressed evidence, so both implementations must agree exactly.
               :diagnostic_completeness (/ (double matching-works)
                                           (double (count expected-work-ids)))
               :matching_works matching-works
               :diagnostic_count diagnostic-count
               :works_with_diagnostics works-with-diagnostics
               :vacuous (zero? diagnostic-count))))))

(defn- observation-value
  [policy status aggregate]
  (let [value (capture/map-wire-status (:status_mapping policy) status)]
    (cond
      (map? value) :unavailable
      (= "invalid_diagnostic_envelope" value) :invalid-diagnostic-envelope
      ;; The measured value is the derived ratio, not the status map's
      ;; constant. The map still gates which statuses may produce a value at
      ;; all, exactly as it does for parser-IR conformance.
      (= "measured" status) (double (:diagnostic_completeness aggregate))
      :else value)))

(defn derive-observation
  "Project an authenticated aggregate into predicate 4's identity envelope."
  [policy qualification-identity-ref aggregate]
  (let [policy (normalize-policy policy)]
    (if (and (valid-policy? policy)
             (nil? (schema/validation-errors @aggregate-schema aggregate))
             (= qualification-identity-ref
                (:qualification_identity_ref aggregate))
             (= (:policy_hash policy) (:policy_hash aggregate)))
      (capture/observation-envelope
       qualification-identity-ref
       (observation-value policy (:status aggregate) aggregate)
       (when (= "measured" (:status aggregate))
         (select-keys aggregate
                      [:expected_works :matching_works :diagnostic_count
                       :works_with_diagnostics :vacuous])))
      (capture/observation-envelope qualification-identity-ref :unavailable
                                    {:reason :identity-mismatch}))))
