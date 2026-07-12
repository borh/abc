(ns abc.sim.oracle
  "Oracle utilities: catalog projection, endpoint confusability predicate,
  model diff for accounting, report normalization, expected candidates.
  These read model states and applied intents; they never re-derive the
  classifier's decisions."
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [clojure.walk :as walk]))

(defn projection
  "Restrict a model to what the CSV can express: works and persons that
  participate in at least one edge."
  [m]
  (let [edge-pids (reduce set/union #{} (vals (:edges m)))
        edge-wids (set (map first (keys (:edges m))))]
    {:persons (into (sorted-map) (filter #(edge-pids (key %))) (:persons m))
     :works (into (sorted-map) (filter #(edge-wids (key %))) (:works m))
     :edges (:edges m)}))

(defn- id-sets [prev cur]
  (let [p (projection prev) c (projection cur)]
    {:p p :c c
     :removed (set/difference (set (keys (:persons p))) (set (keys (:persons c))))
     :added (set/difference (set (keys (:persons c))) (set (keys (:persons p))))}))

(defn confusable?
  "True when some edge's endpoint diff is candidate-shaped: a replacement
  whose previous pid-set ⊆ globally-removed and current pid-set ⊆
  globally-added, with cardinality 1→many or many→1."
  [prev cur]
  (let [{:keys [p c removed added]} (id-sets prev cur)]
    (boolean
     (some (fn [k]
             (let [ps (get (:edges p) k #{})
                   cs (get (:edges c) k #{})]
               (and (seq ps) (seq cs) (not= ps cs)
                    (not (set/subset? ps cs))
                    (not (set/subset? cs ps))
                    (set/subset? ps removed)
                    (set/subset? cs added)
                    (or (and (= 1 (count ps)) (< 1 (count cs)))
                        (and (< 1 (count ps)) (= 1 (count cs)))))))
           (set/union (set (keys (:edges p))) (set (keys (:edges c))))))))

(defn model-diff
  "Accounting oracle over projected endpoints (P4)."
  [prev cur]
  (let [{:keys [p c removed added]} (id-sets prev cur)
        shared (set/intersection (set (keys (:persons p))) (set (keys (:persons c))))
        corrected (set (filter #(not= (get-in p [:persons %]) (get-in c [:persons %]))
                               shared))
        edge-keys (set/union (set (keys (:edges p))) (set (keys (:edges c))))
        counts (reduce (fn [acc k]
                         (let [ps (get (:edges p) k #{}) cs (get (:edges c) k #{})]
                           (cond
                             (= ps cs) acc
                             (set/subset? ps cs) (update acc :additions inc)
                             (set/subset? cs ps) (update acc :removals inc)
                             :else (update acc :replacements inc))))
                       {:additions 0 :removals 0 :replacements 0}
                       edge-keys)]
    {:added-pids added :removed-pids removed :corrected-pids corrected
     :persons-previous (count (:persons p)) :persons-current (count (:persons c))
     :works-previous (count (:works p)) :works-current (count (:works c))
     :edge-counts counts}))

(def ^:private locator-keys
  ;; filesystem locators ONLY — :ingest (counts, skipped work ids) is
  ;; deliberately retained: P14 must detect stale-work-dir contamination
  ;; of ingest results, not just of the drift section.
  ["previous_dir" "current_dir" "input_dir" "input-dir"
   :previous_dir :current_dir :input-dir
   :aozora-repo :work-dir :corpus-dirs :extracted-zips])

(defn semantic-report
  "Strip run-location fields so report equality is meaningful across
  differing work dirs (spec §Report normalization)."
  [report]
  (walk/postwalk (fn [x] (if (map? x) (apply dissoc x locator-keys) x))
                 report))

(defn- edge-candidates [intent source-ids target-ids]
  (vec (for [[wid rel] (:edges intent)]
         {"work_id" wid
          "relation_to_work" rel
          "source_person_ids" (vec (sort source-ids))
          "target_person_ids" (vec (sort target-ids))})))

(defn expected-split-candidates [intent]
  (edge-candidates intent [(-> intent :event :pid)] (-> intent :event :targets)))

(defn expected-merge-candidates [intent]
  (edge-candidates intent (-> intent :event :pids) [(-> intent :event :target)]))

(defn expected-replacements
  "Ambiguous-replacement entries for a conservatism-tier intent's rewritten
  edges, from the window's endpoint states (P3)."
  [prev cur intent]
  (vec (for [[wid rel :as k] (:edges intent)]
         {"work_id" wid
          "relation_to_work" rel
          "previous_person_ids" (vec (sort (get-in prev [:edges k] #{})))
          "current_person_ids" (vec (sort (get-in cur [:edges k] #{})))})))

(defn expected-participant-updates
  "Predicted drift_participant_updates for the window prev → cur,
  restricted to {person_id, change_type} (P15). Presence in the projected
  endpoints decides added/removed; model person-map equality decides
  hash_changed — person_record_hash is a pure function of the person's
  own fields (spec §Key facts). Sorted by person_id."
  [prev cur participant-pids]
  (let [p (:persons (projection prev))
        c (:persons (projection cur))]
    (vec
     (keep (fn [pid]
             (let [pp (get p pid) cp (get c pid)]
               (cond
                 (and pp cp (not= pp cp))
                 {"person_id" pid "change_type" "hash_changed"}

                 (and pp (nil? cp))
                 {"person_id" pid "change_type" "removed"}

                 (and cp (nil? pp))
                 {"person_id" pid "change_type" "added"})))
           (sort participant-pids)))))

(defn card-pid
  "Person directory for a work's content zip: smallest pid over all of the
  work's edges in the projection. Deterministic; shared by render (URL and
  on-disk path) and the selection oracle. Intentionally NOT the slug's
  person_id, which follows catalog-index's last-row-wins rule."
  [proj wid]
  (->> (:edges proj)
       (keep (fn [[[ewid _rel] pids]] (when (= ewid wid) pids)))
       (reduce into (sorted-set))
       first))

(defn- winning-rows
  "basename → row via catalog-index's documented last-row-wins reduction
  over the shared row projection (render/model->rows). The oracle consumes
  the SUT's actual input rows — it never reconstructs their ordering."
  [rows]
  (reduce (fn [acc row]
            (let [url (get row "テキストファイルURL")]
              (if (string/blank? url)
                acc
                (assoc acc (last (string/split url #"/")) row))))
          {}
          rows))

(defn expected-selection
  "Predicted build-publication source selection. rows = (render/model->rows m);
  sources = (render/content-sources m). :selected is sorted by
  :text_zip_relpath (the SUT sorts selected_sources by relpath). :rejected is
  the exact [path reason] set: the two decoys write-aozora-root! always
  plants plus the catalog ZIP itself."
  [rows sources]
  (let [wins (winning-rows rows)
        selected (for [[wid {:keys [basename relpath source-hash]}] sources
                       :let [row (get wins basename)]
                       :when row
                       :let [pid (get row "人物ID")]]
                   {:work_id wid
                    :person_id pid
                    :slug (str wid "_" pid "_" wid "_t")
                    :text_zip_relpath relpath
                    :source_hash source-hash})]
    {:selected (vec (sort-by :text_zip_relpath selected))
     :rejected #{["cards/999999/files/decoy.zip" "not-catalog-text-zip"]
                 ["support/tools.zip" "not-under-cards-files"]
                 ["index_pages/list_person_all_extended_utf8.zip" "not-under-cards-files"]}}))

(defn expected-statuses
  "slug → \"reused\"|\"passed\" over the current selection: reused iff the
  identical slug existed previously with the identical source hash
  (byte-identical zip). Slugs absent from the current selection are absent.
  \"skipped\" is never predicted — pub-dirs are built in a fresh temp root,
  so the skip branch is unreachable in normal runs; properties assert its
  count is 0."
  [prev-selection cur-selection]
  (let [prev (into {} (map (juxt :slug :source_hash)) (:selected prev-selection))]
    (into (sorted-map)
          (map (fn [{:keys [slug source_hash]}]
                 [slug (if (= source_hash (get prev slug)) "reused" "passed")]))
          (:selected cur-selection))))
