(ns abc.tools.parser-rq-source-recognition
  "Closed validators for source-recognition evidence artifacts: per-work
  byte conservation, index membership, aggregate folds, and
  cross-artifact coherence in the decoded_utf8 coordinate system.
  Inputs are JSON values (string- or keyword-keyed)."
  (:require [abc.tools.hash :as hash]
            [abc.tools.parser-rq-decoded-utf8 :as decoded-utf8]
            [clojure.walk :as walk]))

(defn corpus-generation-ref
  "Canonical safe-integer identity of a recognition index without its
  self-reference. Nil when the index leaves the authenticated JSON
  domain."
  [index]
  (try
    (hash/format-sha256
     (hash/sha256-json-rfc8785-safe-integer-v1
      (walk/stringify-keys (dissoc (walk/keywordize-keys index)
                                   :corpus_generation_ref))))
    (catch Exception _ nil)))

(defn work-errors [record]
  (let [record (walk/keywordize-keys record)]
    (if (not= "ok" (:status record))
      []
      (let [regions (:regions record)
            ;; The body region is the frame; eligible bytes is its length.
            ;; Bounding an absolute interval by a length is only correct while
            ;; the region starts at zero.
            body-start (get-in regions [:body :start] 0)
            body-end (get-in regions [:body :end] (:eligible_bytes record))
            recognized (:recognized record)
            accounted (:accounted record)
            semantic-gaps (:semantic_gaps record)
            unaccounted (:unaccounted record)]
        (vec
         (concat
          (mapcat #(decoded-utf8/canonical-interval-errors
                    (first %) (second %) body-start body-end)
                  [["recognized" recognized] ["accounted" accounted]
                   ["semantic_gaps" semantic-gaps]
                   ["unaccounted" unaccounted]])
          (when-not (decoded-utf8/interval-subset? recognized accounted)
            ["recognized intervals are not a subset of accounted intervals"])
          (when-not (= recognized
                       (sort-by (fn [interval]
                                  [(:start interval) (:end interval)])
                                recognized))
            ["recognized intervals are not in canonical order"])
          (when-not (= (:recognized_bytes record)
                       (decoded-utf8/interval-bytes recognized))
            ["recognized_bytes does not equal recognized intervals"])
          (when-not (= (:accounted_bytes record)
                       (decoded-utf8/interval-bytes accounted))
            ["accounted_bytes does not equal accounted intervals"])
          (when-not (= (:semantic_gap_bytes record)
                       (decoded-utf8/interval-bytes semantic-gaps))
            ["semantic_gap_bytes does not equal semantic gaps"])
          (when-not (= (:unaccounted_bytes record)
                       (decoded-utf8/interval-bytes unaccounted))
            ["unaccounted_bytes does not equal unaccounted intervals"])
          (when-not (= semantic-gaps
                       (decoded-utf8/interval-complement recognized body-start body-end))
            ["semantic gaps are not the exact recognized complement"])
          (when-not (= unaccounted
                       (decoded-utf8/interval-complement accounted body-start body-end))
            ["unaccounted intervals are not the exact accounted complement"])
          (when-not (= (- body-end body-start) (+ (:recognized_bytes record)
                                                  (:semantic_gap_bytes record)))
            ["recognized byte conservation does not hold"])
          (when-not (= (- body-end body-start) (+ (:accounted_bytes record)
                                                  (:unaccounted_bytes record)))
            ["accounted byte conservation does not hold"])
          ;; The metadata population, in its own frame. Header and tail are
          ;; distinct regions so a failure localizes to one end of the file,
          ;; but they fold into one measure because they qualify under one
          ;; conjunctive predicate.
          ;;
          ;; The metadata measure is ATTRIBUTION, not byte accounting: only a
          ;; fact whose role the policy names attributing counts, and never a
          ;; `preserved_opaque` one. The body measure above stays accounting.
          ;; The two words are not interchangeable and the field names differ
          ;; so no reader can average them into a single whole-file ratio.
          (let [metadata (:metadata record)
                header-end (get-in regions [:header :end])
                tail-start (get-in regions [:tail :start])
                decoded-bytes (get-in regions [:tail :end])]
            (concat
             (when-not (and (some? regions) (some? metadata))
               ["regions or metadata are absent"])
             (when (and regions metadata)
               (concat
                (when-not (and (= 0 (get-in regions [:header :start]))
                               (= header-end body-start)
                               (= body-end tail-start))
                  ["regions do not partition the decoded source"])
                (when-not (= decoded-bytes
                             (+ (- body-end body-start) (:eligible_bytes metadata)))
                  ["body and metadata regions do not conserve the decoded source"])
                (when-not (= (:eligible_bytes metadata)
                             (+ (:attributed_bytes metadata)
                                (:unattributed_bytes metadata)))
                  ["metadata byte conservation does not hold"])
                (when-not (every? (fn [interval]
                                    (or (<= (:end interval) header-end)
                                        (>= (:start interval) tail-start)))
                                  (concat (:attributed metadata)
                                          (:unattributed metadata)))
                  ["a metadata interval escapes the header and tail"])))))))))))

(defn index-errors [index]
  (let [index (walk/keywordize-keys index)
        expected-ids (get index :expected_work_ids [])
        record-ids (mapv :work_id (get index :records []))
        record-id-set (set record-ids)
        asserted-ref (:corpus_generation_ref index)
        computed-ref (corpus-generation-ref index)]
    (vec
     (concat
      (when (nil? computed-ref)
        ["corpus generation index is outside the authenticated safe-integer JSON domain"])
      (when (and computed-ref (not= asserted-ref computed-ref))
        ["corpus_generation_ref does not authenticate the closed record index"])
      (when-not (= (count expected-ids) (:expected_work_count index))
        ["expected_work_count does not equal expected membership"])
      (when-not (= (count record-ids) (:record_count index))
        ["record_count does not equal records"])
      (when-not (= (count expected-ids) (count (distinct expected-ids)))
        ["expected membership contains duplicate work IDs"])
      (when-not (= (count record-ids) (count (distinct record-ids)))
        ["record index contains duplicate work IDs"])
      (when-not (= record-ids
                   (filterv #(contains? record-id-set %) expected-ids))
        ["record index is not an ordered subset of expected membership"])
      (when (and (= "ok" (:status index))
                 (not= (set expected-ids) record-id-set))
        ["available record index does not exactly match expected membership"])))))

(defn- work-interval-errors [label intervals]
  (mapcat
   (fn [[work-id work-intervals]]
     (decoded-utf8/canonical-interval-errors
      (str label " for " work-id)
      (mapv #(select-keys % [:start :end]) work-intervals)
      (reduce max 0 (map :end work-intervals))))
   (group-by :work_id intervals)))

(defn aggregate-errors [aggregate]
  (let [aggregate (walk/keywordize-keys aggregate)]
    (if (not= "ok" (:status aggregate))
      []
      (let [{:keys [eligible_bytes recognized_bytes accounted_bytes
                    semantic_gap_bytes unaccounted_bytes
                    semantic_gaps unaccounted]} aggregate]
        (vec
         (concat
          (work-interval-errors "semantic gaps" semantic_gaps)
          (work-interval-errors "unaccounted" unaccounted)
          (when (> recognized_bytes accounted_bytes)
            ["aggregate recognized_bytes exceeds accounted_bytes"])
          (when (> accounted_bytes eligible_bytes)
            ["aggregate accounted_bytes exceeds eligible_bytes"])
          (when-not (= semantic_gap_bytes
                       (decoded-utf8/interval-bytes semantic_gaps))
            ["aggregate semantic_gap_bytes does not equal witnesses"])
          (when-not (= unaccounted_bytes
                       (decoded-utf8/interval-bytes unaccounted))
            ["aggregate unaccounted_bytes does not equal witnesses"])
          (when-not (= eligible_bytes (+ recognized_bytes semantic_gap_bytes))
            ["aggregate recognized byte conservation does not hold"])
          (when-not (= eligible_bytes (+ accounted_bytes unaccounted_bytes))
            ["aggregate accounted byte conservation does not hold"])))))))

(defn- aggregate-gaps [records key]
  (vec (mapcat (fn [record]
                 (map #(assoc % :work_id (:work_id record))
                      (get record key)))
               records)))

(defn coherence-errors [index aggregate records]
  (let [index (walk/keywordize-keys index)
        aggregate (walk/keywordize-keys aggregate)
        records (mapv walk/keywordize-keys records)
        identity-keys [:qualification_identity_ref :policy_hash
                       :coordinate_system]
        completeness (:work_completeness aggregate)
        indexed-ids (set (map :work_id (get index :records [])))
        entries-by-id (group-by :work_id (get index :records []))
        records-by-id (group-by :work_id records)
        ok-records (->> (get index :records [])
                        (map #(first (get records-by-id (:work_id %))))
                        (filter #(= "ok" (:status %))))]
    (vec
     (concat
      (when-not (= indexed-ids (set (keys records-by-id)))
        ["loaded records do not exactly match indexed membership"])
      (when-not (every? #(= 1 (count %)) (vals records-by-id))
        ["loaded records contain duplicate work IDs"])
      (mapcat
       (fn [record]
         (let [work-id (:work_id record)
               entry (first (get entries-by-id work-id))]
           (concat
            (keep (fn [key]
                    (when-not (= (get index key) (get record key))
                      (str "work " work-id " has mismatched " (name key))))
                  identity-keys)
            (when-not (= (:capture_generation_ref entry)
                         (:capture_generation_ref record))
              [(str "work " work-id
                    " does not match its indexed capture_generation_ref")]))))
       records)
      (keep (fn [key]
              (when-not (= (get index key) (get aggregate key))
                (str "aggregate has mismatched " (name key))))
            identity-keys)
      (when-not (= (:corpus_generation_ref index)
                   (:corpus_generation_ref aggregate))
        ["aggregate has mismatched corpus_generation_ref"])
      (when-not (= (:corpus_generation_algorithm index)
                   (:corpus_generation_algorithm aggregate))
        ["aggregate has mismatched corpus_generation_algorithm"])
      (when-not (= (:membership_ref index) (:membership_ref aggregate))
        ["aggregate has mismatched membership_ref"])
      (when (= "ok" (:status aggregate))
        (concat
         (when-not (= "ok" (:status index))
           ["available aggregate requires an available index"])
         (when-not (true? (:complete completeness))
           ["available aggregate requires complete work membership"])
         (when-not (= (:expected completeness) (:expected_work_count index))
           ["aggregate expected count does not equal index membership"])
         (when-not (= (:observed completeness) (:record_count index))
           ["aggregate observed count does not equal index records"])
         (when-not (= (:observed completeness) (count records))
           ["aggregate observed count does not equal loaded records"])
         (when-not (= (:expected completeness) (:observed completeness))
           ["available aggregate expected and observed counts differ"])
         (when-not (every? #(= "ok" (:status %)) records)
           ["available aggregate contains unavailable records"])
         (for [key [:eligible_bytes :recognized_bytes :accounted_bytes
                    :semantic_gap_bytes :unaccounted_bytes]
               :let [expected (reduce + 0 (map #(get % key) ok-records))]
               :when (not= expected (get aggregate key))]
           (str "aggregate " (name key) " does not equal work records"))
         (when-not (= (:semantic_gaps aggregate)
                      (aggregate-gaps ok-records :semantic_gaps))
           ["aggregate semantic gaps do not equal work records"])
         (when-not (= (:unaccounted aggregate)
                      (aggregate-gaps ok-records :unaccounted))
           ["aggregate unaccounted intervals do not equal work records"])))))))
