(ns abc.tools.parser-rq-source-accountability
  "Fail-closed ABC derivation boundary for source-accountability evidence."
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.json :as json]
            [abc.tools.parser-release-qualification :as qualification]
            [abc.tools.parser-rq-capture :as capture]
            [abc.tools.schema :as schema]
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

(def diagnostic-gap-result-schema
  (delay (files/read-json "schemas/parser-rq-diagnostic-gap-result.schema.json")))

(def diagnostic-gap-aggregate-schema
  (delay (files/read-json "schemas/parser-rq-diagnostic-gap-aggregate.schema.json")))

(def raw-diagnostics-schema
  (delay (files/read-json "schemas/parser-rq-ab-aozora-diagnostics-v3.schema.json")))

(def diagnostic-gap-policy-schema
  (delay (files/read-json "schemas/parser-rq-diagnostic-gap-policy.schema.json")))

(def source-accountability-index-schema
  (delay (files/read-json "schemas/parser-rq-source-accountability-index.schema.json")))

(def capture-generation-schema
  (delay (files/read-json "schemas/parser-rq-capture-generation.schema.json")))

(def classified-source-ledger-schema
  (delay (files/read-json "schemas/parser-rq-classified-source-ledger.schema.json")))

(def classified-source-authority
  (delay (-> (files/read-json "data/parser-rq-classified-source-authority-v1.json")
             walk/keywordize-keys)))

(def source-accountability-instrument-version
  "parser-rq-source-accountability-v1")

(def source-recognition-instrument-version
  "parser-rq-source-recognition-v1")

(def ^:private diagnostic-gap-policy-v1-hash
  "sha256:cec4fc8a06a833897b008f9b8b3172f3f9bc86ec0c11a1632760f063a9a065d5")

(def ^:private diagnostic-gap-live-codes
  ["source-contains-pua" "unclosed-bracket" "unmatched-close"
   "accent-decomposition-applied" "unresolved-gaiji" "mismatched-container-close"
   "empty-ruby-reading" "nested-ruby" "unrecognised-container-directive"
   "tcy-target-not-found" "bouten-target-ambiguous" "forward-referent-not-stylable"
   "break-in-single-line-container" "bracketed-kaeriten-no-pair"
   "kaeriten-outside-kanbun" "mismatched-bouten-container" "non-canonical-directive"
   "residual-annotation-marker" "unregistered-sentinel" "registry-out-of-order"
   "registry-position-mismatch"])

(defn- unavailable
  [reason]
  {:status :unavailable :reason reason})

(defn- exact-display-ratio
  [covered eligible]
  (let [scale (inc (count (str eligible)))]
    (.divide (bigdec covered) (bigdec eligible)
             scale java.math.RoundingMode/DOWN)))

(defn- authenticated-member-bytes
  [store {:keys [locator ref]}]
  (if-let [authenticated (:authenticated_blobs store)]
    (get authenticated locator)
    (some-> (capture/authenticated-read store ref locator) :bytes)))

(defn- authenticated-json-value
  [store {:keys [ref] :as member}]
  (try
    (let [bytes (authenticated-member-bytes store member)]
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
        store (assoc store :authenticated_blobs (:authenticated_blobs verified))
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

(defn- unique-member-by-hash
  [manifest expected-hash]
  (let [members (filterv #(= expected-hash (get-in % [:ref :sha256])) (:blobs manifest))]
    (when (= 1 (count members)) (first members))))

(defn- authenticated-blob-bytes
  [store member]
  (when member (authenticated-member-bytes store member)))

(defn- strict-utf8
  [bytes]
  (when bytes
    (try
      (let [decoder (doto (.newDecoder java.nio.charset.StandardCharsets/UTF_8)
                      (.onMalformedInput java.nio.charset.CodingErrorAction/REPORT)
                      (.onUnmappableCharacter java.nio.charset.CodingErrorAction/REPORT))]
        (str (.decode decoder (java.nio.ByteBuffer/wrap bytes))))
      (catch Exception _ nil))))

(defn- projected-policy-hash
  [policy]
  (try
    (hash/format-sha256
     (hash/sha256-json-jcs (walk/stringify-keys (dissoc policy :policy_hash))))
    (catch Exception _ nil)))

(defn- disposition-counts-coherent?
  [result dispositions]
  (let [counts (frequencies dispositions)]
    (and (= (:diagnostic_count result) (count dispositions))
         (= (:authorizing_diagnostic_count result)
            (get counts "authorize_exact_span" 0))
         (= (:observe_only_diagnostic_count result)
            (get counts "observe_only" 0))
         (zero? (get counts "reject_internal" 0)))))

(defn- normalized-intervals
  [intervals]
  (reduce (fn [result interval]
            (if-let [previous (peek result)]
              (if (<= (:start interval) (:end previous))
                (conj (pop result) (assoc previous :end (max (:end previous) (:end interval))))
                (conj result interval))
              [interval]))
          [] (sort-by (juxt :start :end) intervals)))

(defn- subtract-interval
  [source cuts]
  (loop [cursor (:start source), remaining cuts, result []]
    (if-let [cut (first remaining)]
      (let [result (if (< cursor (:start cut))
                     (conj result {:start cursor :end (:start cut)}) result)]
        (recur (max cursor (:end cut)) (next remaining) result))
      (cond-> result (< cursor (:end source))
              (conj {:start cursor :end (:end source)})))))

(declare valid-recognition-fold? manifest-record)
(declare authenticated-generation)

(defn- valid-gap-result?
  [store manifest result expected policy-hash policy-artifact-hash qualification-ref policy]
  (let [recognition (:source_recognition_evidence result)
        authorization (:diagnostic_authorization_evidence result)
        generation (authenticated-generation store manifest
                                             (:capture_generation_ref result))
        generation-raw (get-in generation [:members :raw_diagnostics])
        generation-source (get-in generation [:members :decoded_source])
        r1-member (unique-member-by-hash manifest (:value_hash recognition))
        raw-member (unique-member-by-hash manifest (:raw_diagnostics_hash authorization))
        source-member (unique-member-by-hash manifest (:decoded_source_hash authorization))
        policy-member (unique-member-by-hash manifest policy-artifact-hash)
        raw (some->> raw-member (authenticated-json-value store))
        r1 (some->> r1-member (authenticated-json-value store))
        source-bytes (authenticated-blob-bytes store source-member)
        decoded-source (strict-utf8 source-bytes)
        decoded-slice (fn [{:keys [start end]}]
                        (when (and source-bytes (int? start) (int? end)
                                   (<= 0 start) (< start end) (<= end (alength source-bytes)))
                          (try
                            (let [decoder (doto (.newDecoder java.nio.charset.StandardCharsets/UTF_8)
                                            (.onMalformedInput java.nio.charset.CodingErrorAction/REPORT)
                                            (.onUnmappableCharacter java.nio.charset.CodingErrorAction/REPORT))]
                              (str (.decode decoder
                                            (java.nio.ByteBuffer/wrap source-bytes start (- end start)))))
                            (catch Exception _ nil))))
        rules (into {} (map (juxt :code clojure.core/identity) (:rules policy)))
        policy-codes (mapv :code (:rules policy))
        diagnostics (:data raw)
        dispositions (mapv #(get-in rules [(:code %) :disposition]) diagnostics)
        authorized (normalized-intervals
                    (mapv :span (keep-indexed
                                 (fn [index diagnostic]
                                   (when (= "authorize_exact_span" (nth dispositions index))
                                     diagnostic)) diagnostics)))
        gaps (:semantic_gaps r1)
        authorized-in-gap? (every? (fn [interval]
                                     (some #(and (<= (:start %) (:start interval))
                                                 (<= (:end interval) (:end %))) gaps))
                                   authorized)
        silent (when authorized-in-gap?
                 (vec (mapcat (fn [gap]
                                (subtract-interval gap
                                                   (filter #(and (< (:start %) (:end gap))
                                                                 (< (:start gap) (:end %)))
                                                           authorized))) gaps)))
        interval-bytes (fn [intervals]
                         (reduce + 0 (map #(- (:end %) (:start %)) intervals)))]
    (and (nil? (schema/validation-errors @diagnostic-gap-result-schema result))
         (nil? (schema/validation-errors @diagnostic-gap-policy-schema policy))
         (= "ok" (:status result))
         (= (:work_id expected) (:work_id result) (:work_id recognition)
            (:decoded_source_hash authorization))
         (= qualification-ref (:qualification_identity_ref result)
            (:qualification_identity_ref recognition))
         (= (:capture_generation_ref expected) (:capture_generation_ref result)
            (:capture_generation_ref recognition))
         generation
         (= qualification-ref (:qualification_identity_ref generation))
         (= (:work_id result) (:work_id generation))
         (= (:raw_diagnostics_hash authorization) (:value_hash generation-raw)
            (get-in raw-member [:ref :sha256]))
         (= (:artifact_ref generation-raw) (:locator raw-member))
         (= (:raw_diagnostics_bytes authorization) (get-in raw-member [:ref :bytes]))
         (= (:decoded_source_hash authorization) (:value_hash generation-source)
            (get-in source-member [:ref :sha256]))
         (= (:artifact_ref generation-source) (:locator source-member))
         (= (:sha256 expected) (:value_hash recognition)
            (:source_recognition_hash authorization))
         (= policy-hash (:policy_hash result) (:policy_hash authorization))
         (= policy-artifact-hash (:policy_artifact_hash authorization)
            (get-in policy-member [:ref :sha256]))
         (= (:policy_artifact_bytes authorization) (get-in policy-member [:ref :bytes]))
         (= policy-hash (:policy_hash policy) (projected-policy-hash policy))
         (= diagnostic-gap-policy-v1-hash policy-hash)
         (= 21 (count (:rules policy)) (count rules))
         (= diagnostic-gap-live-codes policy-codes)
         (= (count policy-codes) (count (set policy-codes)))
         (= (:raw_diagnostic_schema_hash policy)
            (hash/format-sha256
             (hash/sha256-json-jcs @raw-diagnostics-schema)))
         (= "partitions-semantic-gaps-of" (:relation recognition))
         (= (assoc (:ref r1-member) :locator (:locator r1-member))
            (:artifact_ref recognition))
         source-member policy-member raw-member
         (string? decoded-source)
         (= (:raw_diagnostics_bytes authorization) (get-in raw-member [:ref :bytes]))
         (nil? (schema/validation-errors @raw-diagnostics-schema raw))
         (disposition-counts-coherent? result dispositions)
         (every? some? dispositions)
         (every? #(some? (decoded-slice (:span %))) diagnostics)
         (not-any? #{"reject_internal"} dispositions)
         (= (count diagnostics)
            (count (set (map (juxt :code :severity :source
                                   #(get-in % [:span :start]) #(get-in % [:span :end]))
                             diagnostics))))
         (every? (fn [diagnostic]
                   (= (select-keys diagnostic [:code :kind :severity :source])
                      (select-keys (get rules (:code diagnostic))
                                   [:code :kind :severity :source])))
                 diagnostics)
         (every? (fn [diagnostic]
                   (if (= "source-contains-pua" (:code diagnostic))
                     (let [codepoint (:codepoint diagnostic)
                           slice (decoded-slice (:span diagnostic))
                           scalar (when (and (string? codepoint) (= 1 (.codePointCount codepoint 0 (.length codepoint))))
                                    (.codePointAt codepoint 0))]
                       (and (= slice codepoint) scalar
                            (or (<= 0xE000 scalar 0xF8FF)
                                (<= 0xF0000 scalar 0xFFFFD)
                                (<= 0x100000 scalar 0x10FFFD))))
                     (nil? (:codepoint diagnostic))))
                 diagnostics)
         (= authorized (:authorized_intervals result))
         (= silent (:silent_intervals result))
         (= (:authorized_bytes result) (interval-bytes authorized))
         (= (:silent_bytes result) (interval-bytes silent))
         (= (:silent_drop_count result) (count silent))
         (= (:diagnostic_count result)
            (+ (:authorizing_diagnostic_count result)
               (:observe_only_diagnostic_count result)))
         (= (:vacuous result) (zero? (:diagnostic_count result))))))

(defn silent-drops-envelope
  "Derive the existing R2 observation from an exact diagnostic-gap corpus fold.
  The one-argument form preserves the pre-instrument historical state."
  ([identity]
   {:value :instrument-missing
    :identity_ref (qualification/qualification-identity-ref identity)})
  ([identity _diagnostic-gap-aggregate _recognition-index _recognition-aggregate]
   (unavailable "R2 requires authenticated capture artifacts, not caller-supplied maps"))
  ([store manifest identity]
   (let [expected (qualification/qualification-identity-ref identity)
         verified (capture/verify-manifest store manifest)
         store (assoc store :authenticated_blobs (:authenticated_blobs verified))
         identity-member (selected-member manifest "identity.json")
         aggregate-member (selected-member manifest "diagnostic-gap-aggregate.json")
         index-member (selected-member manifest "recognition-index.json")
         recognition-member (selected-member manifest "recognition-aggregate.json")
         authenticated-identity (some->> identity-member (authenticated-json-value store))
         diagnostic-gap-aggregate (some->> aggregate-member (authenticated-json-value store))
         recognition-index (some->> index-member (authenticated-json-value store))
         recognition-aggregate (some->> recognition-member (authenticated-json-value store))
         recognition-records (when recognition-index
                               (mapv #(manifest-record store manifest %)
                                     (:records recognition-index)))
         expected-work-ids (:expected_work_ids recognition-index)
         authorized (:authorized_bytes diagnostic-gap-aggregate)
         silent (:silent_bytes diagnostic-gap-aggregate)
         semantic-gaps (:semantic_gap_bytes recognition-aggregate)
         silent-count (:silent_drop_count diagnostic-gap-aggregate)
         results (->> (:blobs manifest)
                      (keep #(authenticated-json-value store %))
                      (filter #(and (map? %) (= "abc/parser-rq-diagnostic-gap-result/v1"
                                                (:schema_version %))))
                      vec)
         result-by-work (into {} (map (fn [result] [(:work_id result) result]) results))
         expected-records (:records recognition-index)
         coherent? (and (= :ok (:status verified))
                        (nil? (schema/validation-errors @diagnostic-gap-aggregate-schema
                                                        diagnostic-gap-aggregate))
                        (= identity authenticated-identity)
                        (not-any? nil? recognition-records)
                        (valid-recognition-fold? store manifest recognition-index
                                                 recognition-aggregate recognition-records)
                        (= "ok" (:status diagnostic-gap-aggregate))
                        (= expected
                           (:qualification_identity_ref recognition-index)
                           (:qualification_identity_ref recognition-aggregate)
                           (:qualification_identity_ref diagnostic-gap-aggregate))
                        (= (:corpus_generation_ref recognition-index)
                           (:corpus_generation_ref recognition-aggregate)
                           (:corpus_generation_ref diagnostic-gap-aggregate))
                        (= expected-work-ids
                           (:expected_work_ids diagnostic-gap-aggregate)
                           (:observed_work_ids diagnostic-gap-aggregate))
                        (= (count expected-work-ids)
                           (count (set expected-work-ids)))
                        (= expected-work-ids (mapv :work_id expected-records))
                        (= (set expected-work-ids) (set (keys result-by-work)))
                        (= (count results) (count result-by-work))
                        (every? (fn [record]
                                  (when-let [result (get result-by-work (:work_id record))]
                                    (valid-gap-result? store manifest result record
                                                       (:policy_hash diagnostic-gap-aggregate)
                                                       (:policy_artifact_hash diagnostic-gap-aggregate)
                                                       expected
                                                       (some->> (unique-member-by-hash
                                                                 manifest
                                                                 (:policy_artifact_hash diagnostic-gap-aggregate))
                                                                (authenticated-json-value store)))))
                                expected-records)
                        (every? (fn [key]
                                  (= (get diagnostic-gap-aggregate key)
                                     (reduce + 0 (map #(get % key) results))))
                                [:authorized_bytes :silent_bytes :silent_drop_count
                                 :diagnostic_count :authorizing_diagnostic_count
                                 :observe_only_diagnostic_count])
                        (= (:authorized_interval_count diagnostic-gap-aggregate)
                           (reduce + 0 (map #(count (:authorized_intervals %)) results)))
                        (every? #(and (int? %) (<= 0 %))
                                [authorized silent semantic-gaps silent-count
                                 (:diagnostic_count diagnostic-gap-aggregate)
                                 (:authorizing_diagnostic_count diagnostic-gap-aggregate)
                                 (:observe_only_diagnostic_count diagnostic-gap-aggregate)
                                 (:authorized_interval_count diagnostic-gap-aggregate)])
                        (= (:diagnostic_count diagnostic-gap-aggregate)
                           (+ (:authorizing_diagnostic_count diagnostic-gap-aggregate)
                              (:observe_only_diagnostic_count diagnostic-gap-aggregate)))
                        (= (:vacuous diagnostic-gap-aggregate)
                           (zero? (:diagnostic_count diagnostic-gap-aggregate)))
                        (= semantic-gaps (+ authorized silent)))]
     (if coherent?
       {:value silent-count :identity_ref expected}
       (unavailable "diagnostic-gap aggregate is not the exact R1-bound corpus partition")))))

(defn- recognition-identity-valid?
  [identity]
  (and (valid-identity? identity)
       (= source-recognition-instrument-version
          (get-in identity [:instrument_versions :source_span_coverage]))))

(defn- corpus-generation-ref
  [index]
  (try
    (hash/format-sha256
     (hash/sha256-json-rfc8785-safe-integer-v1
      (walk/stringify-keys (dissoc index :corpus_generation_ref))))
    (catch Exception _ nil)))

(defn- projected-ref
  [value excluded-key]
  (try
    (hash/format-sha256
     (hash/sha256-json-rfc8785-safe-integer-v1
      (walk/stringify-keys (dissoc value excluded-key))))
    (catch Exception _ nil)))

(defn- content-locator
  [sha256 extension]
  (when (and (string? sha256) (re-matches hash/hash-pattern sha256))
    (let [digest (subs sha256 7)]
      (str "sha256/" (subs digest 0 2) "/" digest "." extension))))

(defn- authenticated-generation
  [store p0-manifest capture-ref]
  (let [generation (->> (:blobs p0-manifest)
                        (keep #(authenticated-json-value store %))
                        (filter #(= capture-ref (:generation_ref %)))
                        first)]
    (when (and generation
               (nil? (schema/validation-errors @capture-generation-schema generation))
               (= "abc/parser-rq-capture-generation/v1" (:schema_version generation))
               (= capture-ref (:generation_ref generation))
               (= capture-ref (projected-ref generation :generation_ref)))
      generation)))

(defn- valid-generation-member?
  [store p0-manifest generation member-name extension]
  (let [{:keys [artifact_ref value_hash]} (get-in generation [:members member-name])
        expected-locator (content-locator value_hash extension)
        p0-member (selected-member p0-manifest artifact_ref)]
    (and (= artifact_ref expected-locator)
         p0-member
         (= value_hash (get-in p0-member [:ref :sha256])))))

(defn- valid-ledger-chain?
  [store p0-manifest record index-entry index]
  (let [generation (authenticated-generation store p0-manifest
                                             (:capture_generation_ref record))
        ledger-member (get-in generation [:members :classified_source_ledger])
        ledger-p0-member (selected-member p0-manifest (:artifact_ref ledger-member))
        ledger (some->> ledger-p0-member (authenticated-json-value store))
        authority @classified-source-authority
        parser-member (get-in generation [:members :parser_output])]
    (and generation
         (= (:work_id index-entry) (:work_id generation))
         (= (:qualification_identity_ref index)
            (:qualification_identity_ref generation))
         (= (:ledger record)
            {:sha256 (:value_hash ledger-member)
             :bytes (get-in ledger-p0-member [:ref :bytes])
             :media_type "application/json"
             :locator (:artifact_ref ledger-member)})
         (nil? (schema/validation-errors @classified-source-ledger-schema ledger))
         (= (:qualification_identity_ref index)
            (:qualification_identity_ref ledger))
         (= (:work_id index-entry) (get-in ledger [:original_source :value_hash]))
         (= (str "source/" (:work_id index-entry))
            (get-in ledger [:original_source :artifact_ref]))
         (= "decoded_utf8" (:coordinate_system ledger))
         (= (:policy_hash index) (:policy_hash ledger)
            (get-in authority [:policy :identity_hash]))
         (= (:ledger_schema_hash ledger)
            (get-in authority [:ledger_schema :identity_hash]))
         (= (select-keys (get-in generation [:members :decoded_source])
                         [:artifact_ref :value_hash])
            (select-keys (:decoded_source ledger) [:artifact_ref :value_hash]))
         (valid-generation-member? store p0-manifest generation :decoded_source "txt")
         (valid-generation-member? store p0-manifest generation :parser_output "json")
         (valid-generation-member? store p0-manifest generation :raw_diagnostics "json")
         (valid-generation-member? store p0-manifest generation
                                   :classified_source_ledger "json")
         (every? (fn [entry]
                   (if-let [target (:target_identity entry)]
                     (= (select-keys target [:artifact_ref :value_hash])
                        (select-keys parser-member [:artifact_ref :value_hash]))
                     true))
                 (:entries ledger)))))

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
       ;; Producers emit maximal, canonical intervals.  Adjacent intervals are
       ;; therefore as invalid as overlapping intervals at this trust boundary.
       (every? (fn [[left right]] (< (:end left) (:start right)))
               (partition 2 1 intervals))))

(defn- interval-subset?
  "True when every byte in canonical `subset` occurs in canonical `superset`."
  [subset superset]
  (loop [remaining-subset subset
         remaining-superset superset]
    (if-let [{sub-start :start sub-end :end} (first remaining-subset)]
      (if-let [{super-start :start super-end :end} (first remaining-superset)]
        (cond
          (<= super-end sub-start)
          (recur remaining-subset (next remaining-superset))

          (and (<= super-start sub-start) (<= sub-end super-end))
          (recur (next remaining-subset) remaining-superset)

          :else false)
        false)
      true)))

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
  [store p0-manifest record index-entry index]
  (let [{:keys [eligible_bytes recognized_bytes accounted_bytes
                semantic_gap_bytes unaccounted_bytes recognized accounted
                semantic_gaps unaccounted]} record]
    (and (nil? (schema/validation-errors @recognition-work-schema record))
         (valid-ledger-chain? store p0-manifest record index-entry index)
         (= "ok" (:status record))
         (= source-recognition-instrument-version (:instrument_version record))
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
         (interval-subset? recognized accounted)
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
  [store p0-manifest index aggregate records]
  (let [identity-keys [:qualification_identity_ref :corpus_generation_ref
                       :corpus_generation_algorithm :policy_hash
                       :membership_ref :coordinate_system]
        entries (:records index)
        record-ids (mapv :work_id records)
        completeness (:work_completeness aggregate)
        membership-member (selected-member
                           p0-manifest
                           (content-locator (:membership_ref index) "json"))
        membership (some->> membership-member (authenticated-json-value store))
        membership-work-ids (mapv :work_id (:records membership))]
    (and (nil? (schema/validation-errors @recognition-index-schema index))
         (nil? (schema/validation-errors @recognition-aggregate-schema aggregate))
         (nil? (schema/validation-errors @source-accountability-index-schema membership))
         (= "ok" (:status membership))
         (empty? (:errors membership))
         (= (:membership_ref index) (get-in membership-member [:ref :sha256]))
         (= (:qualification_identity_ref index) (:identity_ref membership))
         (= (:coordinate_system index) (:coordinate_system membership))
         (= (:expected_work_count membership)
            (:record_count membership)
            (count (:records membership)))
         (= (count membership-work-ids) (count (set membership-work-ids)))
         (= (:expected_work_ids index) membership-work-ids)
         (= "ok" (:status index) (:status aggregate))
         (= (:corpus_generation_ref index) (corpus-generation-ref index))
         (= (:expected_work_ids index) (mapv :work_id entries) record-ids)
         (= (:expected_work_count index) (:record_count index) (count records))
         (every? true? (map #(valid-recognition-work? store p0-manifest %1 %2 index)
                            records entries))
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
        index-member (selected-member manifest (:index recognition-locators))
        verified (capture/verify-manifest store manifest)
        store (assoc store :authenticated_blobs (:authenticated_blobs verified))
        identity-member (selected-member manifest (:identity recognition-locators))
        authenticated-identity (some->> identity-member
                                        (authenticated-json-value store))
        authenticated-declares-recognition?
        (= source-recognition-instrument-version
           (get-in authenticated-identity [:instrument_versions :source_span_coverage]))]
    (cond
      (not= :ok (:status verified)) verified
      (not (valid-identity? authenticated-identity))
      (unavailable "qualification identity is absent or invalid in verified manifest evidence")
      (and (not authenticated-declares-recognition?) (not recognition-present?))
      {:value :instrument-missing :identity_ref expected}
      (not authenticated-declares-recognition?)
      (unavailable "recognition evidence is present but the authenticated identity does not declare it")
      (not recognition-present?)
      (unavailable "authenticated identity declares recognition but its evidence is absent")
      :else
      (let [authenticated-aggregate (some->> aggregate-member
                                             (authenticated-json-value store))
            index (some->> index-member (authenticated-json-value store))
            records (when index
                      (mapv #(manifest-record store manifest %) (:records index)))
            denominator (:denominator manifest)]
        (cond
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
          (not (valid-recognition-fold? store manifest index aggregate records))
          (unavailable "recognition aggregate is not the authenticated exact corpus fold")
          :else
          {:value (if (zero? (:eligible_bytes aggregate))
                    1.0M
                    (exact-display-ratio (:recognized_bytes aggregate)
                                         (:eligible_bytes aggregate)))
           :identity_ref expected})))))
