(ns abc.tools.aozora-history-audit
  "Audit an upstream Aozora git change by extracting the CSV ZIP at two refs,
  ingesting both snapshots, validating the current corpus, and running the
  conservative person-drift history report."
  (:require [abc.git :as abc-git]
            [abc.tools.aozora-ingest :as ingest]
            [abc.tools.json :as abc-json]
            [abc.tools.person-drift :as drift]
            [abc.tools.person-drift-history :as drift-history]
            [abc.tools.validate-corpus :as validate-corpus]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]]))

(def ^:private default-zip-path
  "index_pages/list_person_all_extended_utf8.zip")

(def ^:private default-work-dir
  "out/aozora-history-audit")

(def ^:private cli-options
  [["-r" "--aozora-repo DIR" "Local Aozora Bunko git checkout"]
   ["-p" "--previous-ref REF" "Previous upstream ref/commit"]
   ["-c" "--current-ref REF" "Current upstream ref/commit"]
   [nil "--zip-path PATH" "Path to the list_person_all_extended ZIP inside the upstream repo"
    :default default-zip-path]
   [nil "--work-dir DIR" "Tool-owned working directory; previous.zip, current.zip, previous-corpus/, and current-corpus/ are deleted before each run"
    :default default-work-dir]
   ["-o" "--output FILE" "Write JSON report to FILE instead of stdout"]
   [nil "--fail-on-candidates" "Exit 1 when split/merge candidates are present"]
   ["-h" "--help"]])

(defn- usage [summary]
  (str "Usage: clojure -M:abc/aozora-history-audit -- "
       "--aozora-repo DIR --previous-ref REF --current-ref REF [--output FILE]\n\n"
       "Extracts " default-zip-path " at both refs, runs corpus ingest, "
       "validates the current corpus, then runs person-drift-history. "
       "--work-dir is tool-owned: previous.zip, current.zip, previous-corpus/, "
       "and current-corpus/ are deleted before each run.\n\n"
       summary))

(defn- normalize-cli-args [args]
  (if (= "--" (first args))
    (rest args)
    args))

(defn- delete-recursive! [^java.io.File file]
  (when (.exists file)
    (when (.isDirectory file)
      (doseq [child (.listFiles file)]
        (delete-recursive! child)))
    (when-not (.delete file)
      (throw (ex-info (str "failed to delete " (.getPath file))
                      {:path (.getPath file)})))))

(defn- prepare-owned-path! [path]
  (let [file (io/file path)]
    (delete-recursive! file)
    file))

(defn- extract-zip! [repo ref zip-path output-file]
  (abc-git/write-blob-at! repo ref zip-path output-file))

(defn- ingest-corpus! [zip-path output-dir ref source-path]
  (ingest/run-corpus-from-zip!
   {:zip-path zip-path
    :output-dir output-dir
    :overwrite true
    :source-url (str "git:" ref ":" source-path)}))

(defn- index-json-files [indexes-dir]
  (->> (or (.listFiles (io/file indexes-dir)) (make-array java.io.File 0))
       (filter #(and (.isFile ^java.io.File %)
                     (string/ends-with? (.getName ^java.io.File %) ".json")))
       (sort-by #(.getName ^java.io.File %))))

(defn- drift-index-map [persons-dir]
  (let [result (drift/validate-drift-events! {:persons-dir persons-dir})]
    (case (:status result)
      :not-present
      {}

      :ok
      (let [indexes-dir (io/file persons-dir "_indexes")]
        (into (sorted-map)
              (map (fn [file]
                     (let [index (abc-json/read-json-file file)]
                       [(get index "person_id")
                        (vec (sort (get index "drift_event_ids")))])))
              (index-json-files indexes-dir)))

      :error
      (throw (ex-info "drift sidecars failed validation"
                      {:persons-dir persons-dir
                       :failures (:failures result)})))))

(defn drift-participant-updates
  "Return review entries for generated person-record changes that touch
  person_ids already mentioned by accepted drift indexes."
  [{:keys [previous-dir current-dir drift-persons-dir]}]
  (if-not drift-persons-dir
    []
    (let [drift-indexes (drift-index-map drift-persons-dir)
          previous-hashes (drift-history/person-hashes previous-dir)
          current-hashes (drift-history/person-hashes current-dir)]
      (vec
       (keep (fn [[person-id event-ids]]
               (let [previous-hash (get previous-hashes person-id)
                     current-hash (get current-hashes person-id)]
                 (cond
                   (= previous-hash current-hash)
                   nil

                   (and previous-hash current-hash)
                   {"person_id" person-id
                    "change_type" "hash_changed"
                    "previous_hash" previous-hash
                    "current_hash" current-hash
                    "drift_event_ids" event-ids}

                   previous-hash
                   {"person_id" person-id
                    "change_type" "removed"
                    "previous_hash" previous-hash
                    "current_hash" nil
                    "drift_event_ids" event-ids}

                   current-hash
                   {"person_id" person-id
                    "change_type" "added"
                    "previous_hash" nil
                    "current_hash" current-hash
                    "drift_event_ids" event-ids})))
             drift-indexes)))))

(defn audit!
  "Run the full upstream history audit and return a keyword-keyed report map.

  Only the current corpus is validated before reporting drift. The previous
  corpus is generated from the previous ref for comparison, but this tool does
  not treat historical validation failures as a separate gate."
  [{:keys [aozora-repo previous-ref current-ref zip-path work-dir
           drift-persons-dir]
    :or {zip-path default-zip-path
         work-dir default-work-dir}}]
  (when-not aozora-repo
    (throw (ex-info "--aozora-repo is required" {})))
  (when-not previous-ref
    (throw (ex-info "--previous-ref is required" {})))
  (when-not current-ref
    (throw (ex-info "--current-ref is required" {})))
  (let [repo (abc-git/load-git-repo aozora-repo)
        work-root (io/file work-dir)
        previous-zip (io/file work-root "previous.zip")
        current-zip (io/file work-root "current.zip")
        previous-corpus (io/file work-root "previous-corpus")
        current-corpus (io/file work-root "current-corpus")]
    (try
      (prepare-owned-path! previous-zip)
      (prepare-owned-path! current-zip)
      (prepare-owned-path! previous-corpus)
      (prepare-owned-path! current-corpus)
      (extract-zip! repo previous-ref zip-path previous-zip)
      (extract-zip! repo current-ref zip-path current-zip)
      (let [previous-ingest (ingest-corpus! (str previous-zip)
                                            (str previous-corpus)
                                            previous-ref
                                            zip-path)
            current-ingest (ingest-corpus! (str current-zip)
                                           (str current-corpus)
                                           current-ref
                                           zip-path)
            validation (validate-corpus/validate-corpus!
                        {:input-dir (str current-corpus)})
            drift-report (drift-history/report
                          {:previous-dir (str previous-corpus)
                           :current-dir (str current-corpus)})
            drift-participant-updates (drift-participant-updates
                                       {:previous-dir (str previous-corpus)
                                        :current-dir (str current-corpus)
                                        :drift-persons-dir drift-persons-dir})
            validation-failed? (pos? (or (:failed validation) 0))]
        {:status (if validation-failed? "validation_failed" "ok")
         :aozora-repo aozora-repo
         :zip-path zip-path
         :previous-ref previous-ref
         :current-ref current-ref
         :work-dir work-dir
         :extracted-zips {:previous (str previous-zip)
                          :current (str current-zip)}
         :corpus-dirs {:previous (str previous-corpus)
                       :current (str current-corpus)}
         :ingest {:previous previous-ingest
                  :current current-ingest}
         :validation {:current validation}
         :drift drift-report
         :drift-participant-updates drift-participant-updates})
      (finally
        (.close repo)))))

(defn- write-or-print! [result output]
  (if output
    (abc-json/write-deterministic-json-file! output result)
    (println (json/write-json-str (abc-json/prepare-deterministic-json result)
                                  :indent-str "  ")))
  result)

(defn -main [& args]
  (let [{:keys [options errors summary]} (parse-opts (normalize-cli-args args)
                                                     cli-options)]
    (cond
      (:help options)
      (do (println (usage summary))
          (System/exit 0))

      (seq errors)
      (do (binding [*out* *err*]
            (doseq [error errors] (println error))
            (println)
            (println (usage summary)))
          (System/exit 2))

      :else
      (try
        (let [result (write-or-print! (audit! (dissoc options :output))
                                      (:output options))
              validation-failed? (= "validation_failed" (:status result))
              candidate-count (+ (or (get-in result [:drift "summary" "split_candidates"]) 0)
                                 (or (get-in result [:drift "summary" "merge_candidates"]) 0))]
          (when (or validation-failed?
                    (and (:fail-on-candidates options) (pos? candidate-count)))
            (System/exit 1)))
        (catch clojure.lang.ExceptionInfo ex
          (binding [*out* *err*]
            (println (.getMessage ex))
            (when-let [data (seq (ex-data ex))]
              (println (pr-str data))))
          (System/exit 2))))))
