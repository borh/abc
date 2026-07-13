(ns abc.tools.person-drift-history
  "Conservative history audit for generated Aozora corpus snapshots.

  This tool compares two post-ingest corpus directories. It classifies
  ordinary person-record and contributor-edge changes, and only emits
  split/merge candidates when a same-work/same-relation edge shows a
  one-to-many or many-to-one replacement backed by retired/new person IDs."
  (:require [abc.tools.cli :as abc-cli]
            [abc.tools.files :as files]
            [abc.tools.json :as abc-json]
            [abc.tools.person-record :as person-record]
            [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as string]))

(def ^:private identity-fields
  ["person_record_schema_id"
   "person_record_schema_hash"
   "person_id"
   "family_name"
   "given_name"
   "family_name_reading"
   "given_name_reading"
   "family_name_sort"
   "given_name_sort"
   "family_name_romaji"
   "given_name_romaji"
   "date_of_birth"
   "date_of_death"
   "person_copyright_expired"
   "external_links"])

(def ^:private cli-options
  [["-p" "--previous-dir DIR" "Previous generated corpus directory"]
   ["-c" "--current-dir DIR" "Current generated corpus directory"]
   ["-o" "--output FILE" "Write JSON report to FILE instead of stdout"]
   [nil "--fail-on-candidates" "Exit 1 when split/merge candidates are present"]
   ["-h" "--help"]])

(defn- usage [summary]
  (str "Usage: clojure -M:abc/person-drift-history -- --previous-dir DIR --current-dir DIR [--output FILE]\n\n"
       "Compares generated corpus snapshots containing works/ and persons/ directories.\n\n"
       summary))

(defn- json-file? [path]
  (and (fs/regular-file? path)
       (string/ends-with? (str (fs/file-name path)) ".json")))

(defn- list-json-files [dir]
  (let [path (fs/path dir)]
    (when-not (fs/directory? path)
      (throw (ex-info (str "expected directory: " dir) {:dir dir})))
    (->> (fs/list-dir path)
         (filter json-file?)
         (sort-by (comp str fs/file-name))
         (map #(io/file (str %))))))

(defn- file-stem [path]
  (let [name (str (fs/file-name path))]
    (subs name 0 (- (count name) (count ".json")))))

(defn- read-persons [root]
  (into (sorted-map)
        (map (fn [file]
               (let [record (files/read-json file)]
                 [(get record "person_id" (file-stem file)) record])))
        (list-json-files (fs/path root "persons"))))

(defn- read-works [root]
  (into (sorted-map)
        (map (fn [file]
               [(file-stem file) (files/read-json file)]))
        (list-json-files (fs/path root "works"))))

(defn- corpus [root]
  (let [persons (read-persons root)
        works (read-works root)]
    {"persons" persons
     "works" works
     "edges" (reduce-kv
              (fn [edges work-id work]
                (reduce
                 (fn [edges contributor]
                   (let [person-id (get contributor "person_id")
                         relation (get contributor "relation_to_work")]
                     (if (and person-id relation)
                       (update edges [work-id relation] (fnil conj #{}) person-id)
                       edges)))
                 edges
                 (get work "contributors" [])))
              (sorted-map)
              works)}))

(defn person-hashes
  "Return a sorted map of person_id to person_record_hash for a generated
  corpus root containing persons/*.json."
  [root]
  (into (sorted-map)
        (map (fn [[person-id record]]
               [person-id (person-record/record-hash record)]))
        (read-persons root)))

(defn- sorted-ids [ids]
  (vec (sort ids)))

(defn- changed-fields [previous current]
  (->> identity-fields
       (filter #(not= (get previous %) (get current %)))
       vec))

(defn- metadata-corrections [previous-persons current-persons]
  (->> (set/intersection (set (keys previous-persons))
                         (set (keys current-persons)))
       sort
       (keep (fn [person-id]
               (let [previous (get previous-persons person-id)
                     current (get current-persons person-id)
                     previous-hash (person-record/record-hash previous)
                     current-hash (person-record/record-hash current)]
                 (when (not= previous-hash current-hash)
                   {"person_id" person-id
                    "changed_fields" (changed-fields previous current)
                    "previous_hash" previous-hash
                    "current_hash" current-hash}))))
       vec))

(defn- edge-change [[work-id relation] previous current]
  (cond
    (= previous current)
    nil

    (set/subset? previous current)
    {"change_type" "addition"
     "work_id" work-id
     "relation_to_work" relation
     "previous_person_ids" (sorted-ids previous)
     "current_person_ids" (sorted-ids current)
     "added_person_ids" (sorted-ids (set/difference current previous))}

    (set/subset? current previous)
    {"change_type" "removal"
     "work_id" work-id
     "relation_to_work" relation
     "previous_person_ids" (sorted-ids previous)
     "current_person_ids" (sorted-ids current)
     "removed_person_ids" (sorted-ids (set/difference previous current))}

    :else
    {"change_type" "replacement"
     "work_id" work-id
     "relation_to_work" relation
     "previous_person_ids" (sorted-ids previous)
     "current_person_ids" (sorted-ids current)}))

(defn- contributor-edge-changes [previous-edges current-edges]
  (->> (set/union (set (keys previous-edges))
                  (set (keys current-edges)))
       sort
       (keep (fn [edge]
               (edge-change edge
                            (get previous-edges edge #{})
                            (get current-edges edge #{}))))
       vec))

(defn- replacement? [change]
  (= "replacement" (get change "change_type")))

(defn- split-candidate? [removed-person-ids added-person-ids change]
  (let [previous (set (get change "previous_person_ids"))
        current (set (get change "current_person_ids"))]
    (and (replacement? change)
         (= 1 (count previous))
         (< 1 (count current))
         (set/subset? previous removed-person-ids)
         (set/subset? current added-person-ids))))

(defn- merge-candidate? [removed-person-ids added-person-ids change]
  (let [previous (set (get change "previous_person_ids"))
        current (set (get change "current_person_ids"))]
    (and (replacement? change)
         (< 1 (count previous))
         (= 1 (count current))
         (set/subset? previous removed-person-ids)
         (set/subset? current added-person-ids))))

(defn- candidate [source-key target-key change]
  {"work_id" (get change "work_id")
   "relation_to_work" (get change "relation_to_work")
   source-key (get change "previous_person_ids")
   target-key (get change "current_person_ids")})

(defn- split-candidates [removed-person-ids added-person-ids edge-changes]
  (->> edge-changes
       (filter #(split-candidate? removed-person-ids added-person-ids %))
       (map #(candidate "source_person_ids" "target_person_ids" %))
       vec))

(defn- merge-candidates [removed-person-ids added-person-ids edge-changes]
  (->> edge-changes
       (filter #(merge-candidate? removed-person-ids added-person-ids %))
       (map #(candidate "source_person_ids" "target_person_ids" %))
       vec))

(defn- ambiguous-replacements [edge-changes split-candidates merge-candidates]
  (let [candidate-keys (set (map (juxt #(get % "work_id")
                                       #(get % "relation_to_work")
                                       #(get % "source_person_ids")
                                       #(get % "target_person_ids"))
                                 (concat split-candidates merge-candidates)))]
    (->> edge-changes
         (filter replacement?)
         (remove (fn [change]
                   (contains? candidate-keys
                              [(get change "work_id")
                               (get change "relation_to_work")
                               (get change "previous_person_ids")
                               (get change "current_person_ids")])))
         (map #(select-keys % ["work_id" "relation_to_work"
                               "previous_person_ids" "current_person_ids"]))
         vec)))

(defn- summary [previous current added removed corrections edge-changes splits merges ambiguous]
  {"persons_previous" (count (get previous "persons"))
   "persons_current" (count (get current "persons"))
   "works_previous" (count (get previous "works"))
   "works_current" (count (get current "works"))
   "added_person_ids" (count added)
   "removed_person_ids" (count removed)
   "metadata_corrections" (count corrections)
   "contributor_edge_additions" (count (filter #(= "addition" (get % "change_type")) edge-changes))
   "contributor_edge_removals" (count (filter #(= "removal" (get % "change_type")) edge-changes))
   "contributor_edge_replacements" (count (filter replacement? edge-changes))
   "split_candidates" (count splits)
   "merge_candidates" (count merges)
   "ambiguous_replacements" (count ambiguous)})

(defn report
  "Return a deterministic JSON-ready map comparing two generated corpus roots."
  [{:keys [previous-dir current-dir]}]
  (when-not previous-dir
    (throw (ex-info "--previous-dir is required" {})))
  (when-not current-dir
    (throw (ex-info "--current-dir is required" {})))
  (let [previous (corpus previous-dir)
        current (corpus current-dir)
        previous-person-ids (set (keys (get previous "persons")))
        current-person-ids (set (keys (get current "persons")))
        added (sorted-ids (set/difference current-person-ids previous-person-ids))
        removed (sorted-ids (set/difference previous-person-ids current-person-ids))
        added-set (set added)
        removed-set (set removed)
        corrections (metadata-corrections (get previous "persons")
                                          (get current "persons"))
        edge-changes (contributor-edge-changes (get previous "edges")
                                               (get current "edges"))
        splits (split-candidates removed-set added-set edge-changes)
        merges (merge-candidates removed-set added-set edge-changes)
        ambiguous (ambiguous-replacements edge-changes splits merges)]
    {"status" "ok"
     "previous_dir" previous-dir
     "current_dir" current-dir
     "summary" (summary previous current added removed corrections
                        edge-changes splits merges ambiguous)
     "added_person_ids" added
     "removed_person_ids" removed
     "metadata_corrections" corrections
     "contributor_edge_changes" edge-changes
     "split_candidates" splits
     "merge_candidates" merges
     "ambiguous_replacements" ambiguous}))

(defn write-report! [{:keys [output] :as opts}]
  (let [result (report opts)]
    (if output
      (abc-json/write-deterministic-json-file! output result)
      (println (abc-json/write-deterministic-json-str result)))
    result))

(defn -main [& args]
  (abc-cli/run-cli!
   args
   {:cli-options cli-options
    :usage-fn    usage
    :run         (fn [{:keys [options]}]
                   (let [result (write-report! options)
                         candidate-count (+ (get-in result ["summary" "split_candidates"])
                                            (get-in result ["summary" "merge_candidates"]))]
                     (assoc result ::exit-fail?
                            (and (:fail-on-candidates options) (pos? candidate-count)))))
    :fail?       ::exit-fail?}))
