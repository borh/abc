(ns abc.tools.parser-rq-publication
  "Authenticate and derive the parser release publication-structure observation."
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.json :as json]
            [abc.tools.parser-rq-capture :as capture]
            [abc.tools.schema :as schema]
            [clojure.walk :as walk])
  (:import [java.math RoundingMode]))

(def publication-instrument-version
  "reports/parser-ir/publication-bundle-validate.py against parser-ir-publication-preservation.schema.json")

(defn- unavailable [reason]
  {:status :unavailable :reason reason})

(defn- keywordize [value]
  (walk/keywordize-keys value))

(defn- parse-bytes [bytes]
  (json/read-json-str (String. ^bytes bytes java.nio.charset.StandardCharsets/UTF_8)))

(defn- projected-hash [value field]
  (hash/format-sha256
   (hash/sha256-json-jcs (dissoc value field))))

(defn- schema-errors [path value]
  (schema/validation-errors (files/read-json path) value))

(defn- census-pass? [required observed]
  (every? (fn [[construct minimum]]
            (>= (get observed construct 0) minimum))
          required))

(defn derive-publication-envelope
  "Authenticate one closed capture and derive predicate 6 without reopening blobs."
  [store manifest index identity]
  (let [manifest (keywordize manifest)
        index-value (if (every? keyword? (keys index)) index (keywordize index))
        identity (keywordize identity)
        verified (capture/verify-manifest store manifest)]
    (if-not (= :ok (:status verified))
      verified
      (try
        (let [authenticated (:authenticated_blobs verified)
              decoded (into {} (map (fn [[locator bytes]]
                                      [locator (parse-bytes bytes)])) authenticated)
              index-match (some (fn [[_ value]]
                                  (when (= (keywordize value) index-value) value)) decoded)
              policy (files/read-json "data/parser-rq-publication-policy-v1.json")
              census (files/read-json "data/parser-rq-publication-fixtures-v1.json")
              preservation-hash
              (str "sha256:"
                   (files/sha256-file
                    "schemas/parser-ir-publication-preservation.schema.json"))
              work-schema "schemas/parser-rq-publication-work.schema.json"
              index-schema "schemas/parser-rq-publication-index.schema.json"
              identity-ref (or (:identity_ref identity)
                               (:qualification_identity_ref identity))
              expected-entries (:entries identity)
              expected-by-id (into {} (map (juxt :work_id :source_sha256)
                                           expected-entries))
              raw-checks (set (butlast (get policy "structure_checks")))]
          (cond
            (nil? index-match)
            (unavailable "caller index is not one of the authenticated blobs")

            (seq (schema-errors index-schema index-match))
            (unavailable "publication index violates its closed schema")

            (not= (get policy "policy_hash")
                  (projected-hash policy "policy_hash"))
            (unavailable "publication policy hash has drifted")

            (not= (get census "census_hash")
                  (projected-hash census "census_hash"))
            (unavailable "publication census hash has drifted")

            (not= identity-ref (:qualification_identity_ref index-value))
            (unavailable "qualification identity mismatch")

            (not= publication-instrument-version
                  (get-in identity [:instrument_versions :publication_structure]))
            (unavailable "qualification identity has the wrong publication instrument")

            (not= (:policy_hash index-value) (get policy "policy_hash"))
            (unavailable "index policy identity differs from committed authority")

            (not= (:census_hash index-value) (get census "census_hash"))
            (unavailable "index census identity differs from committed authority")

            (not= (:preservation_schema_hash index-value) preservation-hash)
            (unavailable "index preservation schema identity differs from authority")

            (not= (:validator_semantics_hash index-value)
                  (get-in policy ["validator" "semantics_hash"]))
            (unavailable "index validator semantics identity differs from authority")

            (not= expected-by-id
                  (into {} (map (juxt :work_id :source_sha256)
                                (:records index-value))))
            (unavailable "index does not have exact pinned corpus membership")

            :else
            (let [records
                  (mapv (fn [{:keys [locator]}]
                          (get decoded locator))
                        (:records index-value))
                  invalid-record? (some #(or (nil? %)
                                             (seq (schema-errors work-schema %))) records)
                  records (mapv keywordize records)]
              (if invalid-record?
                (unavailable "one or more publication work records are unavailable")
                (let [parsed (filterv #(= "parsed" (:parser_disposition %)) records)
                      failed (filterv #(= "failed" (:parser_disposition %)) records)
                      timed-out (filterv #(= "timeout" (:parser_disposition %)) records)
                      authority-keys [:qualification_identity_ref :policy_hash
                                      :preservation_schema_hash
                                      :validator_semantics_hash :census_hash]
                      paired? (every? true?
                                      (map (fn [index-record record]
                                             (= (select-keys index-record
                                                             [:work_id :source_sha256])
                                                (select-keys record
                                                             [:work_id :source_sha256])))
                                           (:records index-value) records))
                      coherent? (every? (fn [record]
                                          (= (select-keys index-value authority-keys)
                                             (select-keys record authority-keys)))
                                        records)
                      projected
                      (mapv
                       (fn [record]
                         (let [work-id (:work_id record)
                               candidates (get-in record [:publication
                                                          :structure_check_candidates])
                               required (-> census (get "works") (get work-id)
                                            (get "required_constructs") keywordize)
                               observed (get-in record [:publication :counts :by_construct])
                               census-ok? (census-pass? required observed)
                               checks (assoc candidates
                                             :required_construct_census_valid census-ok?)]
                           {:work_id work-id
                            :checks checks
                            :join-valid (true? (get-in record [:publication
                                                               :join_input_valid]))
                            :passed (every? true? (vals checks))
                            :closed (= raw-checks
                                       (set (map name (keys candidates))))}))
                       parsed)
                      eligible (count (filter #(and (:closed %) (:join-valid %))
                                              projected))
                      passed (count (filter :passed projected))]
                  (cond
                    (not paired?) (unavailable "index/work record identity pairing mismatch")
                    (not coherent?) (unavailable "work authority identities are incoherent")
                    (some (complement :closed) projected)
                    (unavailable "raw publication check membership differs from policy")
                    (zero? (count parsed)) (unavailable "zero parsed publication denominator")
                    (not= eligible (count parsed))
                    (unavailable "publication join input is unavailable")
                    :else
                    {:value (.divide (bigdec passed) (bigdec eligible) 4 RoundingMode/DOWN)
                     :identity_ref identity-ref
                     :details
                     {:counts {:expected (count records)
                               :parsed (count parsed)
                               :eligible eligible
                               :passed passed
                               :failed (count failed)
                               :timed_out (count timed-out)}
                      :denominator_shrink_witnesses
                      (mapv #(select-keys % [:work_id :parser_disposition])
                            (concat failed timed-out))
                      :failed_work_witnesses
                      (mapv (fn [{:keys [work_id checks]}]
                              {:work_id work_id
                               :failed_checks (->> checks
                                                   (keep (fn [[check ok?]]
                                                           (when-not ok? (name check))))
                                                   sort vec)})
                            (remove :passed projected))}}))))))
        (catch Exception error
          (unavailable (.getMessage error)))))))
