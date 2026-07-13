(ns abc.tools.source-bundle-report
  (:require [abc.tools.json :as json]
            [abc.tools.source-bundle :as source-bundle]
            [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.string :as string]))

(def pinned-aozorabunko-commit
  "0e9ea3e586eb0aa34039fabfc85a407d2f98b165")

(defn- corpus-zips [root]
  (let [cards (fs/path root "cards")]
    (->> (if (fs/directory? cards) (fs/list-dir cards) [])
         (filter fs/directory?)
         (sort-by (comp str fs/file-name))
         (map #(fs/path % "files"))
         (mapcat #(if (fs/directory? %) (fs/list-dir %) []))
         (filter #(and (fs/regular-file? %)
                       (string/ends-with? (str (fs/file-name %)) ".zip")))
         (sort-by str))))

(defn- relative-path [root file]
  (-> (fs/relativize root file)
      str
      (string/replace "\\" "/")))

(defn- sevenzip-listable? [zip-file]
  (let [binary (or (System/getenv "ABC_7ZZ_BIN") "7zz")]
    (zero? (:exit (process/sh [binary "l" "-slt" (str zip-file)])))))

(def measurement-construction
  "abc-source-bundle-streamed-evidence-v1")

(defn- empty-summary []
  {"measurement_construction" measurement-construction
   "readable_zip_count" 0
   "unreadable_zip_count" 0
   "admitted_zip_count" 0
   "rejected_zip_count" 0
   "rejection_reason_counts" (sorted-map)
   "semantic_text_member_counts" {}
   "utf8_flagged_entry_count" 0
   "legacy_flagged_entry_count" 0
   "nfc_collision_bundle_count" 0
   "unicode_case_collision_bundle_count" 0
   "max_member_count" 0
   "max_member_bytes" 0
   "max_total_bytes" 0
   "declared_actual_size_mismatch_member_count" 0
   "declared_actual_size_mismatches" []
   "java_unreadable_7zz_listable_count" 0
   "java_unreadable_7zz_unlistable_count" 0
   "damaged_paths" []})

(defn- record-rejection [summary reason]
  (-> summary
      (update "rejected_zip_count" inc)
      (update-in ["rejection_reason_counts" (name reason)] (fnil inc 0))))

(defn- admission-reason [scan]
  (try
    (source-bundle/admit-scan! scan)
    nil
    (catch clojure.lang.ExceptionInfo t
      (if (source-bundle/admission-error? t)
        (:reason (ex-data t))
        (throw t)))))

(defn- report-mismatches [root zip-file scan]
  (mapv (fn [{:keys [member-path declared-bytes actual-bytes]}]
          {"archive_path" (relative-path root zip-file)
           "member_path" member-path
           "declared_bytes" declared-bytes
           "actual_bytes" actual-bytes})
        (get-in scan [:stats :declared-actual-size-mismatches])))

(defn- record-readable [summary root zip-file scan reason]
  (let [{:keys [member-count max-member-bytes total-bytes utf8-count
                legacy-count]} (:stats scan)
        mismatches (report-mismatches root zip-file scan)
        semantic-count (count (:semantic-text-candidates scan))]
    (cond->
     (-> summary
         (update "readable_zip_count" inc)
         (update-in ["semantic_text_member_counts" (str semantic-count)]
                    (fnil inc 0))
         (update "utf8_flagged_entry_count" + utf8-count)
         (update "legacy_flagged_entry_count" + legacy-count)
         (update "max_member_count" max member-count)
         (update "max_member_bytes" max max-member-bytes)
         (update "max_total_bytes" max total-bytes)
         (update "declared_actual_size_mismatch_member_count"
                 + (count mismatches))
         (update "declared_actual_size_mismatches" into mismatches)
         (cond-> (get-in scan [:collision-evidence :nfc-collision?])
           (update "nfc_collision_bundle_count" inc))
         (cond-> (get-in scan [:collision-evidence
                               :unicode-case-collision?])
           (update "unicode_case_collision_bundle_count" inc)))
      (nil? reason) (update "admitted_zip_count" inc)
      reason (record-rejection reason))))

(defn- record-unreadable [summary root zip-file]
  (let [listable? (sevenzip-listable? zip-file)]
    (-> summary
        (update "unreadable_zip_count" inc)
        (record-rejection :unreadable-zip)
        (update (if listable?
                  "java_unreadable_7zz_listable_count"
                  "java_unreadable_7zz_unlistable_count") inc)
        (update "damaged_paths" conj (relative-path root zip-file)))))

(defn measure!
  "Measure only cards/*/files/*.zip beneath aozora-root. The returned map is
  deterministic and contains paths relative to aozora-root."
  [aozora-root]
  (reduce
   (fn [summary zip-file]
     (try
       (let [scan (source-bundle/scan-zip (fs/file zip-file))]
         (record-readable summary aozora-root zip-file scan
                          (admission-reason scan)))
       (catch clojure.lang.ExceptionInfo t
         (if-not (source-bundle/admission-error? t)
           (throw t)
           (if (= :unreadable-zip (:reason (ex-data t)))
             (record-unreadable summary aozora-root zip-file)
             (throw t))))))
   (empty-summary)
   (corpus-zips aozora-root)))

(defn -main [& args]
  (when-not (= 2 (count args))
    (throw (ex-info
            "usage: clojure -M -m abc.tools.source-bundle-report AOZORA_ROOT OUTPUT_JSON"
            {:args args})))
  (let [[aozora-root output] args]
    (json/write-deterministic-json-file!
     output
     (assoc (measure! aozora-root)
            "aozorabunko_commit" pinned-aozorabunko-commit))))
