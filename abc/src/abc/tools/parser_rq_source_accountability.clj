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

(def taxonomy-schema
  (delay (files/read-json "schemas/parser-rq-ignored-regions.schema.json")))

(def recognition-aggregate-schema
  (delay (files/read-json "schemas/parser-rq-source-recognition-aggregate.schema.json")))

(def recognition-index-schema
  (delay (files/read-json "schemas/parser-rq-source-recognition-index.schema.json")))

(def recognition-work-schema
  (delay (files/read-json "schemas/parser-rq-source-recognition-work.schema.json")))

(def source-accountability-instrument-version
  "parser-rq-source-accountability-v1")

(def source-recognition-instrument-version
  "parser-rq-source-recognition-v1")

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

(def expected-locators
  {:aggregate "aggregate.json"
   :identity "identity.json"
   :taxonomy "taxonomy.json"})

(def recognition-locators
  {:aggregate "recognition-aggregate.json"
   :index "recognition-index.json"
   :identity "identity.json"})

(defn- unambiguous-manifest?
  [manifest]
  (let [blobs (:blobs manifest)]
    (and (every? #(= 1 %) (vals (frequencies (map :locator blobs))))
         (every? #(= 1 %)
                 (vals (frequencies (map #(get-in % [:ref :sha256]) blobs)))))))

(defn- selected-member
  [manifest locator]
  (when (unambiguous-manifest? manifest)
    (let [matches (filterv #(= locator (:locator %)) (:blobs manifest))]
      (when (= 1 (count matches)) (first matches)))))

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
  [aggregate identity-ref]
  (let [{:keys [eligible_bytes covered_eligible_bytes uncovered_eligible_bytes
                taxonomy_hash work_completeness uncovered]} aggregate]
    (and (nil? (schema/validation-errors @aggregate-schema aggregate))
         (= "ok" (:status aggregate))
         (= "decoded_utf8" (:coordinate_system aggregate))
         (= identity-ref (:identity_ref aggregate))
         (= "parser-rq-ignored-regions-v1" (:taxonomy_version aggregate))
         (string? taxonomy_hash)
         (re-matches hash/hash-pattern taxonomy_hash)
         (true? (:complete work_completeness))
         (= (:expected work_completeness) (:observed work_completeness))
         (pos-int? eligible_bytes)
         (= eligible_bytes (+ covered_eligible_bytes uncovered_eligible_bytes))
         (valid-uncovered? uncovered uncovered_eligible_bytes eligible_bytes))))

(defn- valid-taxonomy?
  [taxonomy taxonomy-member aggregate]
  (and taxonomy
       (nil? (schema/validation-errors @taxonomy-schema taxonomy))
       (= (:taxonomy_version aggregate) (:taxonomy_version taxonomy))
       (= (:coordinate_system aggregate) (:coordinate_system taxonomy))
       (= (:taxonomy_hash aggregate) (get-in taxonomy-member [:ref :sha256]))))

(defn derive-source-span-envelope
  "Reverify a P0 capture and derive R1 only from authenticated integer totals."
  [store manifest aggregate identity]
  (let [verified (capture/verify-manifest store manifest)
        expected (qualification/qualification-identity-ref identity)
        denominator (:denominator manifest)
        aggregate-member (selected-member manifest (:aggregate expected-locators))
        identity-member (selected-member manifest (:identity expected-locators))
        taxonomy-member (selected-member manifest (:taxonomy expected-locators))
        authenticated-aggregate (some->> aggregate-member
                                         (authenticated-json-value store))
        authenticated-identity (some->> identity-member
                                        (authenticated-json-value store))
        taxonomy (some->> taxonomy-member
                          (authenticated-json-value store))]
    (cond
      (not= :ok (:status verified))
      verified

      (not (valid-identity? identity))
      (unavailable "qualification identity violates the closed P0 contract")

      (not= identity authenticated-identity)
      (unavailable "qualification identity is absent from verified manifest evidence")

      (not= aggregate authenticated-aggregate)
      (unavailable "aggregate argument does not match verified aggregate bytes")

      (not (valid-taxonomy? taxonomy taxonomy-member aggregate))
      (unavailable "taxonomy is absent, ambiguous, invalid, or inconsistent")

      (not= "decoded_utf8_bytes" (:unit denominator))
      (unavailable "manifest denominator is not decoded UTF-8 bytes")

      (not= (:eligible_bytes aggregate) (:value denominator))
      (unavailable "manifest denominator does not equal aggregate eligible bytes")

      (not (valid-aggregate? aggregate expected))
      (unavailable "aggregate violates schema, identity, taxonomy, or conservation contracts")

      :else
      {:value (exact-display-ratio (:covered_eligible_bytes aggregate)
                                   (:eligible_bytes aggregate))
       :identity_ref expected})))

(defn silent-drops-envelope
  [identity]
  {:value :instrument-missing
   :identity_ref (qualification/qualification-identity-ref identity)})

(defn- recognition-identity-valid?
  [identity]
  (and (valid-identity? identity)
       (= source-recognition-instrument-version
          (get-in identity [:instrument_versions :source_recognition]))))

(defn- corpus-generation-ref
  [index]
  (try
    (hash/format-sha256
     (hash/sha256-json-rfc8785-safe-integer-v1
      (walk/stringify-keys (dissoc index :corpus_generation_ref))))
    (catch Exception _ nil)))

(defn- interval-bytes
  [intervals]
  (reduce + 0 (map #(- (:end %) (:start %)) intervals)))

(defn- valid-intervals?
  [intervals eligible]
  (and (vector? intervals)
       (every? (fn [{:keys [start end]}]
                 (and (int? start) (int? end)
                      (<= 0 start) (< start end) (<= end eligible)))
               intervals)
       (every? (fn [[left right]] (<= (:end left) (:start right)))
               (partition 2 1 intervals))))

(defn- interval-complement
  [intervals eligible]
  (loop [cursor 0
         remaining intervals
         complement []]
    (if-let [{:keys [start end]} (first remaining)]
      (recur end
             (next remaining)
             (cond-> complement (< cursor start)
                     (conj {:start cursor :end start})))
      (cond-> complement (< cursor eligible)
              (conj {:start cursor :end eligible})))))

(defn- valid-recognition-work?
  [record index-entry index]
  (let [{:keys [eligible_bytes recognized_bytes accounted_bytes
                semantic_gap_bytes unaccounted_bytes recognized accounted
                semantic_gaps unaccounted]} record]
    (and (nil? (schema/validation-errors @recognition-work-schema record))
         (= "ok" (:status record))
         (= (:qualification_identity_ref index)
            (:qualification_identity_ref record))
         (= (:policy_hash index) (:policy_hash record))
         (= (:work_id index-entry) (:work_id record))
         (= (:capture_generation_ref index-entry)
            (:capture_generation_ref record))
         (every? #(valid-intervals? % eligible_bytes)
                 [recognized accounted semantic_gaps unaccounted])
         (= recognized_bytes (interval-bytes recognized))
         (= accounted_bytes (interval-bytes accounted))
         (= semantic_gap_bytes (interval-bytes semantic_gaps))
         (= unaccounted_bytes (interval-bytes unaccounted))
         (<= recognized_bytes accounted_bytes eligible_bytes)
         (= semantic_gaps (interval-complement recognized eligible_bytes))
         (= unaccounted (interval-complement accounted eligible_bytes))
         (= eligible_bytes (+ recognized_bytes semantic_gap_bytes))
         (= eligible_bytes (+ accounted_bytes unaccounted_bytes)))))

(defn- manifest-record
  [store manifest entry]
  (let [member (selected-member manifest (:locator entry))
        expected-ref (select-keys entry [:sha256 :bytes :media_type])]
    (when (= expected-ref (:ref member))
      (authenticated-json-value store member))))

(defn- aggregate-work-intervals
  [records key]
  (vec
   (mapcat (fn [record]
             (map #(assoc % :work_id (:work_id record)) (get record key)))
           records)))

(defn- valid-recognition-fold?
  [index aggregate records]
  (let [identity-keys [:qualification_identity_ref :corpus_generation_ref
                       :corpus_generation_algorithm :policy_hash
                       :membership_ref :coordinate_system]
        entries (:records index)
        record-ids (mapv :work_id records)
        completeness (:work_completeness aggregate)]
    (and (nil? (schema/validation-errors @recognition-index-schema index))
         (nil? (schema/validation-errors @recognition-aggregate-schema aggregate))
         (= "ok" (:status index) (:status aggregate))
         (= (:corpus_generation_ref index) (corpus-generation-ref index))
         (= (:expected_work_ids index) (mapv :work_id entries) record-ids)
         (= (:expected_work_count index) (:record_count index) (count records))
         (every? true? (map valid-recognition-work? records entries (repeat index)))
         (every? #(= (get index %) (get aggregate %)) identity-keys)
         (= {:expected (count records) :observed (count records) :complete true}
            completeness)
         (every? (fn [key]
                   (= (get aggregate key)
                      (reduce + 0 (map #(get % key) records))))
                 [:eligible_bytes :recognized_bytes :accounted_bytes
                  :semantic_gap_bytes :unaccounted_bytes])
         (= (:semantic_gaps aggregate)
            (aggregate-work-intervals records :semantic_gaps))
         (= (:unaccounted aggregate)
            (aggregate-work-intervals records :unaccounted)))))

(defn derive-source-recognition-envelope
  "Derive ledger-authoritative R1 from one fully authenticated P0 capture.
  Captures predating recognition remain instrument-missing; partial or
  incoherent recognition captures fail closed."
  [store manifest aggregate identity]
  (let [expected (qualification/qualification-identity-ref identity)
        recognition-locator-set (set (vals (select-keys recognition-locators
                                                        [:aggregate :index])))
        recognition-present? (some #(contains? recognition-locator-set (:locator %))
                                   (:blobs manifest))
        aggregate-member (selected-member manifest (:aggregate recognition-locators))
        index-member (selected-member manifest (:index recognition-locators))]
    (if-not recognition-present?
      {:value :instrument-missing :identity_ref expected}
      (let [verified (capture/verify-manifest store manifest)
            identity-member (selected-member manifest (:identity recognition-locators))
            authenticated-aggregate (some->> aggregate-member
                                             (authenticated-json-value store))
            index (some->> index-member (authenticated-json-value store))
            authenticated-identity (some->> identity-member
                                            (authenticated-json-value store))
            records (when index
                      (mapv #(manifest-record store manifest %) (:records index)))
            denominator (:denominator manifest)]
        (cond
          (not= :ok (:status verified)) verified
          (not (recognition-identity-valid? identity))
          (unavailable "qualification identity lacks the source-recognition instrument")
          (not= identity authenticated-identity)
          (unavailable "qualification identity is absent from verified manifest evidence")
          (not= aggregate authenticated-aggregate)
          (unavailable "aggregate argument does not match verified recognition aggregate bytes")
          (some nil? records)
          (unavailable "recognition index members are absent or do not match manifest bindings")
          (not= expected (:qualification_identity_ref index)
                (:qualification_identity_ref aggregate))
          (unavailable "recognition evidence does not match qualification identity")
          (not= "decoded_utf8_bytes" (:unit denominator))
          (unavailable "manifest denominator is not decoded UTF-8 bytes")
          (not= (:eligible_bytes aggregate) (:value denominator))
          (unavailable "manifest denominator does not equal recognition eligible bytes")
          (not (valid-recognition-fold? index aggregate records))
          (unavailable "recognition aggregate is not the authenticated exact corpus fold")
          :else
          {:value (if (zero? (:eligible_bytes aggregate))
                    1.0M
                    (exact-display-ratio (:recognized_bytes aggregate)
                                         (:eligible_bytes aggregate)))
           :identity_ref expected})))))
