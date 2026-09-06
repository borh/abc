(ns abc.sim.oracle
  "Oracle utilities: catalog projection, endpoint confusability predicate,
  model diff for accounting, report normalization, expected candidates.
  These read model states and applied intents; they never re-derive the
  classifier's decisions."
  (:require [abc.tools.hash :as hash]
            [abc.tools.jcs :as jcs]
            [clojure.set :as set]
            [clojure.string :as string]
            [clojure.walk :as walk])
  (:import [com.ibm.icu.lang UCharacter]
           [java.nio.charset StandardCharsets]
           [java.text Normalizer Normalizer$Form]))

(def ^:private bundle-construction "abc-source-bundle-v1")

(defn- normalize-identity-path [path]
  (Normalizer/normalize (string/replace path "\\" "/") Normalizer$Form/NFC))

(defn- safe-identity-path? [path]
  (let [segments (string/split path #"/" -1)]
    (and (not (string/blank? path))
         (not (string/starts-with? path "/"))
         (not (re-find #"^[A-Za-z]:" path))
         (not-any? #{"" "." ".."} segments))))

(defn- packaging-metadata? [path]
  (or (string/starts-with? path "__MACOSX/")
      (string/starts-with? (last (string/split path #"/")) "._")))

(defn- primary-candidate? [path]
  (and (string/ends-with? (string/lower-case path) ".txt")
       (not (packaging-metadata? path))))

(defn- reject! [reason data]
  (throw (ex-info (str "oracle source bundle admission rejected: " (name reason))
                  (assoc data :reason reason))))

(defn- oracle-raw-members
  "Independent model-to-member projection; intentionally does not use the
  render namespace or production source-bundle code."
  [m wid]
  (let [{:keys [text images]} (get-in m [:contents wid])]
    (into [[(str wid ".txt") (.getBytes ^String text StandardCharsets/UTF_8)]]
          (map (fn [[path content]]
                 [path (.getBytes ^String content StandardCharsets/UTF_8)]))
          images)))

(defn- oracle-members [m wid]
  (let [members (mapv (fn [[raw-path ^bytes bytes]]
                        (let [path (normalize-identity-path raw-path)]
                          {:raw-path raw-path
                           :path path
                           :bytes bytes
                           :member-hash (hash/format-sha256
                                         (hash/sha256-bytes bytes))}))
                      (oracle-raw-members m wid))]
    (when-let [unsafe (first (remove #(safe-identity-path? (:path %)) members))]
      (reject! :unsafe-member-path
               {:decoded-path (:raw-path unsafe)
                :normalized-path (:path unsafe)}))
    (when-let [[path collisions]
               (first (sort-by key
                               (filter #(> (count (val %)) 1)
                                       (group-by :path members))))]
      (reject! :duplicate-member-path
               {:path path :member-count (count collisions)}))
    (when-let [[folded collisions]
               (first (sort-by key
                               (filter #(> (count (val %)) 1)
                                       (group-by #(UCharacter/foldCase
                                                   ^String (:path %) true)
                                                 members))))]
      (reject! :case-fold-member-path-collision
               {:folded-path folded
                :paths (->> collisions (map :path) sort vec)}))
    (let [sorted-members (sort-by :path members)
          candidates (filterv #(primary-candidate? (:path %)) sorted-members)]
      (case (count candidates)
        0 (reject! :no-primary-text-member {:candidates []})
        1 {:members sorted-members :primary (first candidates)}
        (reject! :multiple-primary-text-members
                 {:candidates (mapv :path candidates)})))))

(defn expected-content-identity
  "Independent abc-source-bundle-v1 identity from model members plus the
  separately rendered archive bytes. No production inspector/constructor is
  called, making render/inspector mistakes falsifiable."
  [m wid ^bytes archive-bytes]
  (let [{oracle-members :members primary-member :primary}
        (oracle-members m wid)
        members (mapv (fn [{:keys [path member-hash]}]
                        {"path" path "member_hash" member-hash})
                      oracle-members)
        primary (:path primary-member)
        identity-object {"construction" bundle-construction
                         "members" members
                         "primary_text_member" primary}
        primary-hash (:member-hash primary-member)]
    {:identity-object identity-object
     :members members
     :archive-hash (hash/format-sha256 (hash/sha256-bytes archive-bytes))
     :bundle-hash
     (hash/format-sha256
      (hash/sha256-bytes
       (jcs/rfc8785-string-domain-json-bytes identity-object)))
     :primary-text-member primary
     :primary-text-hash primary-hash}))

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
