(ns ab-research.parser-rq-parser-ir-conformance
  "Authenticate and fold predicate-5 Parser-IR conformance records."
  (:require [ab-research.files :as files]
            [ab-research.hash :as hash]
            [charred.api :as charred]
            [soranoha.core.json :as record-json]
            [ab-research.parser-rq-capture :as capture]
            [ab-research.schema :as schema]
            [clojure.walk :as walk]))

(def ^:private work-schema-id
  "https://w3id.org/soranoha/schemas/parser-rq-parser-ir-conformance-work.schema.json")

(def ^:private aggregate-schema-id
  "https://w3id.org/soranoha/schemas/parser-rq-parser-ir-conformance-aggregate.schema.json")

(def ^:private policy-schema
  (delay (files/read-json "schemas/parser-rq-parser-ir-conformance-policy.schema.json")))

(def ^:private work-schema
  (delay (files/read-json "schemas/parser-rq-parser-ir-conformance-work.schema.json")))

(def ^:private aggregate-schema
  (delay (files/read-json "schemas/parser-rq-parser-ir-conformance-aggregate.schema.json")))

(defn- normalize-policy
  [policy]
  (let [policy (into {} (map (fn [[key value]]
                               [(if (string? key) (keyword key) key) value]))
                     policy)
        status-map (:status_mapping policy)]
    (assoc policy :status_mapping (capture/normalize-status-map status-map))))

(defn projected-hash
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
       (= (:parser_ir_schema_hash policy)
          (schema/schema-hash "schemas/parser-ir.schema.json"))))

(defn- strict-json
  [bytes]
  (let [decoder (doto (.newDecoder java.nio.charset.StandardCharsets/UTF_8)
                  (.onMalformedInput java.nio.charset.CodingErrorAction/REPORT)
                  (.onUnmappableCharacter java.nio.charset.CodingErrorAction/REPORT))]
    (charred/read-json
     (str (.decode decoder (java.nio.ByteBuffer/wrap bytes))))))

(defn- locator-for
  [store blob-ref]
  (let [locators (:locators store)
        locators (if (instance? clojure.lang.IDeref locators)
                   @locators
                   locators)]
    (get locators (:sha256 blob-ref))))

(defn- authenticated-bytes
  [store blob-ref]
  (when-let [locator (locator-for store blob-ref)]
    (let [result (capture/authenticated-read store blob-ref locator)]
      (when (= :ok (:status result)) (:bytes result)))))

(defn- unavailable
  [work-id qualification-identity-ref reason]
  {:status :unavailable
   :work_id work-id
   :qualification_identity_ref qualification-identity-ref
   :reason reason})

(defn authenticate-record
  "Authenticate an index record and every blob required by its typed status."
  [store policy qualification-identity-ref
   {:keys [work_id record]}]
  (let [policy (normalize-policy policy)]
    (try
      (cond
        (not (valid-policy? policy))
        (unavailable work_id qualification-identity-ref
                     "policy identity or authority has drifted")

        (not (and (string? qualification-identity-ref)
                  (re-matches hash/hash-pattern qualification-identity-ref)))
        (unavailable work_id qualification-identity-ref
                     "qualification identity is malformed")

        (not (some #{work_id} (:expected_work_ids policy)))
        (unavailable work_id qualification-identity-ref
                     "work is absent from the policy workset")

        :else
        (if-let [record-bytes (authenticated-bytes store record)]
          (let [raw-record (strict-json record-bytes)]
            (if (seq (schema/validation-errors @work-schema raw-record))
              (unavailable work_id qualification-identity-ref
                           "work record violates its closed schema")
              (let [record-value (walk/keywordize-keys raw-record)
                    status (:status record-value)
                    required-refs (case status
                                    "schema_valid" [(:parser_ir record-value)]
                                    "schema_invalid" [(:parser_ir record-value)
                                                      (:validation_ledger record-value)]
                                    "no_output" []
                                    "unavailable" []
                                    nil)
                    refs-authenticate? (and (some? required-refs)
                                            (every? #(some? (authenticated-bytes store %))
                                                    required-refs))]
                (cond
                  (not= work_id (:work_id record-value))
                  (unavailable work_id qualification-identity-ref
                               "index and work-record identities differ")

                  (not= qualification-identity-ref
                        (:qualification_identity_ref record-value))
                  (unavailable work_id qualification-identity-ref
                               "qualification identity mismatch")

                  (not= (:policy_hash policy) (:policy_hash record-value))
                  (unavailable work_id qualification-identity-ref
                               "policy identity mismatch")

                  (= "unavailable" status)
                  (unavailable work_id qualification-identity-ref
                               (or (:reason record-value)
                                   "producer marked record unavailable"))

                  (not refs-authenticate?)
                  (unavailable work_id qualification-identity-ref
                               "one or more required blobs are unavailable")

                  :else
                  {:status :ok
                   :work_id work_id
                   :qualification_identity_ref qualification-identity-ref
                   :record record-value}))))
          (unavailable work_id qualification-identity-ref
                       "work record blob is unavailable")))
      (catch Exception error
        (unavailable work_id qualification-identity-ref (.getMessage error))))))

(defn aggregate
  "Recompute predicate-5 counts over an exact authenticated closed workset."
  [policy expected-work-ids authenticated-records]
  (let [policy (normalize-policy policy)
        identity-ref (or (some->> authenticated-records
                                  (filter #(= :ok (:status %)))
                                  first :record :qualification_identity_ref)
                         (some :qualification_identity_ref authenticated-records))
        base {:schema_id aggregate-schema-id
              :schema_version "1.0.0"
              :qualification_identity_ref identity-ref
              :policy_hash (:policy_hash policy)
              :expected_works (count expected-work-ids)}
        membership-errors (capture/closed-membership-errors expected-work-ids
                                                            authenticated-records)
        records (mapv :record (filter #(= :ok (:status %))
                                      authenticated-records))
        coherent? (every? #(and (= work-schema-id (:schema_id %))
                                (= identity-ref (:qualification_identity_ref %))
                                (= (:policy_hash policy) (:policy_hash %)))
                          records)
        valid (count (filter #(= "schema_valid" (:status %)) records))
        invalid (count (filter #(= "schema_invalid" (:status %)) records))
        no-output (count (filter #(= "no_output" (:status %)) records))
        generated (+ valid invalid)
        counts {:generated_outputs generated
                :schema_valid_outputs valid
                :schema_invalid_outputs invalid
                :no_output_works no-output}]
    (cond
      (or (not (valid-policy? policy))
          (not= (set expected-work-ids) (set (:expected_work_ids policy)))
          (empty? expected-work-ids)
          (seq membership-errors)
          (not coherent?))
      (merge base counts {:status "unavailable" :reason "index_incomplete"})

      (some #(= :unavailable (:status %)) authenticated-records)
      (merge base counts {:status "unavailable" :reason "blob_unavailable"})

      (zero? generated)
      (merge base counts {:status "no_parser_ir_output"})

      :else
      (merge base counts
             {:status "measured"
              ;; IEEE division of two doubles, not `(double (/ int int))`. The
              ;; latter builds an exact Ratio and rounds it through BigDecimal,
              ;; which differs from other languages' division in the last
              ;; bit (for example, 2/3 becomes ...667 here and ...666 in the Python capture
              ;; driver). This value lands in content-addressed evidence, so both
              ;; implementations must agree exactly. Same reasoning and shape as
              ;; `parser-rq-diagnostic-completeness/aggregate`.
              :parser_ir_schema_validation (/ (double valid)
                                              (double generated))}))))

(defn- observation-value
  [policy status aggregate]
  (let [mapped (capture/map-wire-status (:status_mapping policy) status)]
    (cond
      (map? mapped) :unavailable
      (= "no_parser_ir_output" mapped) :no-parser-ir-output
      (= "measured" status) (double (:parser_ir_schema_validation aggregate))
      :else mapped)))

(defn derive-observation
  "Project a validated aggregate into predicate 5's identity envelope."
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
       (select-keys aggregate
                    [:expected_works :generated_outputs :schema_valid_outputs
                     :schema_invalid_outputs :no_output_works]))
      (capture/observation-envelope qualification-identity-ref :unavailable
                                    {:reason :aggregate-invalid}))))
