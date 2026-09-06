(ns ab-research.parser-rq-predicate-hardening-capture-test
  (:require [ab-research.files :as files]
            [charred.api :as charred]
            [ab-research.parser-rq-capture :as capture]
            [ab-research.parser-rq-diagnostic-completeness :as diagnostic]
            [ab-research.parser-rq-parser-ir-conformance :as conformance]
            [ab-research.schema :as schema]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [clojure.walk :as walk]))

(def fixture-root
  (io/file "test/fixtures/parser-rq/predicate-hardening-capture"))

(defn- read-json
  [file]
  (-> file slurp charred/read-json walk/keywordize-keys))

(defn- fixture-values
  []
  (->> (file-seq (io/file fixture-root "store"))
       (filter #(.isFile %))
       (map read-json)
       vec))

(defn- value-by-schema
  [values schema-id]
  (first (filter #(= schema-id (:schema_id %)) values)))

(deftest committed-capture-rederives-byte-identical-observations
  (let [historical-schema (str (io/file fixture-root "parser-ir-0.7.0.schema.json"))
        read-schema schema/read-schema]
    (is (= "sha256:43a6a6d86ca5eca062508e6cae633d19bf5248f15c5bb46153a6d8580ea916ec"
           (schema/schema-hash historical-schema)))
    (with-redefs [schema/read-schema
                  (fn [path]
                    (read-schema (if (= path "schemas/parser-ir.schema.json")
                                   historical-schema path)))]
      (let [manifest (read-json (io/file fixture-root "manifest.json"))
            values (fixture-values)
            store-root (io/file fixture-root "store")
            locators (into {} (map (fn [{:keys [locator ref]}]
                                     [(:sha256 ref) locator]))
                           (:blobs manifest))
            store {:root (.getPath store-root) :locators locators}
            bytes-for (fn [ref]
                        (:bytes (capture/authenticated-read
                                 store ref (get locators (:sha256 ref)))))
            diagnostic-policy
            (files/read-json "data/parser-rq-diagnostic-completeness-policy-v1.json")
            parser-policy
            (files/read-json "data/parser-rq-parser-ir-conformance-policy-v1.json")
            diagnostic-index
            (value-by-schema
             values
             "https://w3id.org/abc/schemas/parser-rq-diagnostic-completeness-index.schema.json")
            parser-index
            (value-by-schema
             values
             "https://w3id.org/abc/schemas/parser-rq-parser-ir-conformance-index.schema.json")
            diagnostic-index-schema
            (files/read-json "schemas/parser-rq-diagnostic-completeness-index.schema.json")
            parser-index-schema
            (files/read-json "schemas/parser-rq-parser-ir-conformance-index.schema.json")
            record-for (fn [entry]
                         (read-json (io/file store-root
                                             (get locators
                                                  (get-in entry [:record :sha256])))))
            diagnostic-records
            (mapv (fn [entry]
                    (let [committed (record-for entry)
                          derived (diagnostic/derive-work
                                   diagnostic-policy
                                   (:qualification_identity_ref diagnostic-index)
                                   {:work_id (:work_id committed)
                                    :attempt_disposition (:attempt_disposition committed)
                                    :bytes (bytes-for (:raw_diagnostics committed))})]
                      (is (= committed (:record derived)))
                      (:record derived)))
                  (:records diagnostic-index))
            parser-records
            (mapv #(conformance/authenticate-record
                    store parser-policy
                    (:qualification_identity_ref parser-index) %)
                  (:records parser-index))
            diagnostic-aggregate
            (diagnostic/aggregate diagnostic-policy
                                  (:expected_work_ids diagnostic-index)
                                  diagnostic-records)
            parser-aggregate
            (conformance/aggregate parser-policy
                                   (:expected_work_ids parser-index)
                                   parser-records)
            diagnostic-observation
            (diagnostic/derive-observation
             diagnostic-policy (:qualification_identity_ref diagnostic-index)
             diagnostic-aggregate)
            parser-observation
            (conformance/derive-observation
             parser-policy (:qualification_identity_ref parser-index)
             parser-aggregate)
            committed-measurements
            (first (filter #(and (contains? % :diagnostic_completeness)
                                 (contains? % :parser_ir_schema_validation))
                           values))]
        (is (empty? (capture/manifest-errors manifest)))
        (is (empty? (schema/validation-errors diagnostic-index-schema diagnostic-index)))
        (is (empty? (schema/validation-errors parser-index-schema parser-index)))
        (is (every? #(= :ok (:status %)) parser-records))
        (is (= {"no_output" 1 "schema_invalid" 1 "schema_valid" 1}
               (frequencies (map #(get-in % [:record :status]) parser-records))))
        (is (= diagnostic-aggregate
               (value-by-schema
                values
                "https://w3id.org/abc/schemas/parser-rq-diagnostic-completeness-aggregate.schema.json")))
        (is (= parser-aggregate
               (value-by-schema
                values
                "https://w3id.org/abc/schemas/parser-rq-parser-ir-conformance-aggregate.schema.json")))
        (is (= diagnostic-observation
               (:diagnostic_completeness committed-measurements)))
        (is (= parser-observation
               (:parser_ir_schema_validation committed-measurements)))))))

(deftest historical-conformance-capture-is-unavailable-under-current-schema
  (let [manifest (read-json (io/file fixture-root "manifest.json"))
        store {:root (str (io/file fixture-root "store"))
               :locators (into {} (map (fn [{:keys [locator ref]}]
                                         [(:sha256 ref) locator]))
                               (:blobs manifest))}
        policy (files/read-json "data/parser-rq-parser-ir-conformance-policy-v1.json")
        index (value-by-schema
               (fixture-values)
               "https://w3id.org/abc/schemas/parser-rq-parser-ir-conformance-index.schema.json")]
    (doseq [entry (:records index)]
      (let [result (conformance/authenticate-record
                    store policy (:qualification_identity_ref index) entry)]
        (is (= :unavailable (:status result)))
        (is (= "policy identity or authority has drifted" (:reason result)))))))
