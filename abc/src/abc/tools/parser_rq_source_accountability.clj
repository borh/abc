(ns abc.tools.parser-rq-source-accountability
  "Fail-closed ABC derivation boundary for source-accountability evidence."
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.json :as json]
            [abc.tools.parser-release-qualification :as qualification]
            [abc.tools.parser-rq-capture :as capture]
            [abc.tools.schema :as schema]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.walk :as walk]))

(def aggregate-schema
  (delay (files/read-json "schemas/parser-rq-source-accountability-aggregate.schema.json")))

(def source-accountability-instrument-version
  "parser-rq-source-accountability-v1")

(defn- unavailable
  [reason]
  {:status :unavailable :reason reason})

(defn- exact-display-ratio
  [covered eligible]
  (let [scale (inc (count (str eligible)))]
    (.divide (bigdec covered) (bigdec eligible)
             scale java.math.RoundingMode/DOWN)))

(defn- authenticated-json-value
  [store {:keys [locator ref]}]
  (try
    (let [root-file (.getCanonicalFile (io/file (:root store)))
          blob-file (.getCanonicalFile (io/file root-file locator))
          root-path (.toPath root-file)
          blob-path (.toPath blob-file)
          bytes (when (.startsWith blob-path root-path)
                  (java.nio.file.Files/readAllBytes blob-path))]
      (when (and bytes
                 (= (:bytes ref) (alength bytes))
                 (= (:sha256 ref)
                    (hash/format-sha256 (hash/sha256-bytes bytes))))
        (-> (String. bytes java.nio.charset.StandardCharsets/UTF_8)
            json/read-json-str
            walk/keywordize-keys)))
    (catch Exception _ nil)))

(defn- blob-value-present?
  [store manifest expected]
  (some #(= expected (authenticated-json-value store %)) (:blobs manifest)))

(defn- logical-ref-present?
  [manifest expected]
  (some #(= expected (get-in % [:ref :sha256])) (:blobs manifest)))

(defn- valid-uncovered?
  [uncovered uncovered-bytes eligible-bytes]
  (let [valid-span? (fn [{:keys [start end]}]
                      (and (int? start)
                           (int? end)
                           (<= 0 start)
                           (< start end)
                           (<= end eligible-bytes)))
        ordered (->> uncovered
                     (group-by :work_id)
                     vals
                     (map #(sort-by (juxt :start :end) %)))
        disjoint? (fn [spans]
                    (every? (fn [[left right]]
                              (<= (:end left) (:start right)))
                            (partition 2 1 spans)))]
    (and (vector? uncovered)
         (every? valid-span? uncovered)
         (every? disjoint? ordered)
         (= uncovered-bytes
            (reduce + 0 (map #(- (:end %) (:start %)) uncovered))))))

(defn- valid-identity?
  [identity]
  (let [required (set qualification/qualification-identity-keys)
        hash-keys (set qualification/qualification-hash-keys)
        string-keys (set qualification/qualification-string-keys)
        instruments (:instrument_versions identity)]
    (and (= required (set (keys identity)))
         (pos-int? (:aat_version identity))
         (every? #(and (string? (get identity %))
                       (not (string/blank? (get identity %))))
                 string-keys)
         (every? #(and (string? (get identity %))
                       (re-matches hash/hash-pattern (get identity %)))
                 hash-keys)
         (map? instruments)
         (= source-accountability-instrument-version
            (:source_accountability instruments))
         (every? (fn [[instrument version]]
                   (and (or (keyword? instrument)
                            (and (string? instrument)
                                 (not (string/blank? instrument))))
                        (string? version)
                        (not (string/blank? version))))
                 instruments))))

(defn- valid-aggregate?
  [aggregate identity-ref manifest]
  (let [{:keys [eligible_bytes covered_eligible_bytes uncovered_eligible_bytes
                taxonomy_hash work_completeness uncovered]} aggregate]
    (and (nil? (schema/validation-errors @aggregate-schema aggregate))
         (= "ok" (:status aggregate))
         (= "decoded_utf8" (:coordinate_system aggregate))
         (= identity-ref (:identity_ref aggregate))
         (= "parser-rq-ignored-regions-v1" (:taxonomy_version aggregate))
         (logical-ref-present? manifest taxonomy_hash)
         (true? (:complete work_completeness))
         (= (:expected work_completeness) (:observed work_completeness))
         (pos-int? eligible_bytes)
         (= eligible_bytes (+ covered_eligible_bytes uncovered_eligible_bytes))
         (valid-uncovered? uncovered uncovered_eligible_bytes eligible_bytes))))

(defn derive-source-span-envelope
  "Reverify a P0 capture and derive R1 only from authenticated integer totals."
  [store manifest aggregate identity]
  (let [verified (capture/verify-manifest store manifest)
        expected (qualification/qualification-identity-ref identity)
        denominator (:denominator manifest)]
    (cond
      (not= :ok (:status verified))
      verified

      (not (valid-identity? identity))
      (unavailable "qualification identity violates the closed P0 contract")

      (not (blob-value-present? store manifest identity))
      (unavailable "qualification identity is absent from verified manifest evidence")

      (not (blob-value-present? store manifest aggregate))
      (unavailable "aggregate argument does not match verified aggregate bytes")

      (not= "decoded_utf8_bytes" (:unit denominator))
      (unavailable "manifest denominator is not decoded UTF-8 bytes")

      (not= (:eligible_bytes aggregate) (:value denominator))
      (unavailable "manifest denominator does not equal aggregate eligible bytes")

      (not (valid-aggregate? aggregate expected manifest))
      (unavailable "aggregate violates schema, identity, taxonomy, or conservation contracts")

      :else
      {:value (exact-display-ratio (:covered_eligible_bytes aggregate)
                                   (:eligible_bytes aggregate))
       :identity_ref expected})))

(defn silent-drops-envelope
  [identity]
  {:value :instrument-missing
   :identity_ref (qualification/qualification-identity-ref identity)})
