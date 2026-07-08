(ns abc.tools.soranoha-build-publication
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.aozora-csv :as aozora-csv]
            [abc.tools.aozora-ingest :as aozora-ingest]
            [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.json :as abc-json]
            [abc.tools.manifest :as manifest]
            [abc.tools.schema :as schema]
            [abc.tools.workflow :as workflow]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as string]
            [clojure.tools.cli :as cli])
  (:import [java.nio.file Files StandardCopyOption]
           [java.util.zip ZipEntry ZipFile]))

(def config-schema-path
  "schemas/soranoha-publication-build-config.schema.json")

(def cli-options
  [[nil "--aozora-root DIR" "Official aozorabunko checkout root."
    :id :aozora-root]
   [nil "--config FILE" "Soranoha publication build config JSON."
    :id :config]
   [nil "--snapshot-date DATE" "Snapshot date in UTC calendar form, YYYY-MM-DD."
    :id :snapshot-date]
   [nil "--output-root DIR" "Final output root."
    :id :output-root]
   [nil "--replace" "Replace an existing output root after a successful build."
    :id :replace :default false]])

(defn- normalized-path [file]
  (string/replace (str file) "\\" "/"))

(defn- zip-file? [file]
  (and (.isFile file)
       (string/ends-with? (.getName file) ".zip")))

(defn- aozora-work-zip? [root file]
  (let [rel (normalized-path
             (.relativize (.toPath (.getCanonicalFile (io/file root)))
                          (.toPath (.getCanonicalFile (io/file file)))))]
    (when (re-matches #"^cards/[0-9]{6}/files/[^/]+\.zip$" rel)
      rel)))

(defn- read-catalog-zip [aozora-root]
  (let [zip-file (io/file aozora-root "index_pages"
                          "list_person_all_extended_utf8.zip")]
    (when-not (.isFile zip-file)
      (throw (ex-info "official catalog ZIP is missing"
                      {:path (str zip-file)})))
    (with-open [zf (ZipFile. zip-file)]
      (let [entry (->> (enumeration-seq (.entries zf))
                       (filter (fn [^ZipEntry e]
                                 (string/ends-with? (.getName e) ".csv")))
                       first)]
        (when-not entry
          (throw (ex-info "official catalog ZIP contains no CSV entry"
                          {:path (str zip-file)})))
        (let [bytes (with-open [in (.getInputStream zf entry)]
                      (.readAllBytes in))]
          {:catalog-zip zip-file
           :csv-entry (.getName entry)
           :csv-bytes bytes
           :csv-text (String. bytes "UTF-8")
           :catalog-csv-hash (hash/format-sha256
                              (hash/sha256-bytes bytes))})))))

(defn- text-url-basename [row]
  (some-> (get row "テキストファイルURL")
          string/trim
          (string/split #"/")
          last))

(defn- row-work-id [row]
  (get row "作品ID"))

(defn- row-person-id [row]
  (get row "人物ID"))

(defn- catalog-index [rows]
  (reduce (fn [idx row]
            (if-let [basename (text-url-basename row)]
              (assoc idx basename row)
              idx))
          {}
          rows))

(defn- work-zip-files [aozora-root]
  (->> (file-seq (io/file aozora-root))
       (filter zip-file?)
       (map (fn [file]
              {:file file
               :relpath (aozora-work-zip? aozora-root file)}))
       vec))

(defn- first-text-member [zip-file]
  (with-open [zf (ZipFile. (io/file zip-file))]
    (or (some (fn [^ZipEntry entry]
                (when (and (not (.isDirectory entry))
                           (string/ends-with?
                            (string/lower-case (.getName entry))
                            ".txt"))
                  (.getName entry)))
              (enumeration-seq (.entries zf)))
        (throw (ex-info "work ZIP contains no .txt member"
                        {:path (str zip-file)})))))

(defn- slug [work-id person-id relpath]
  (let [basename (.getName (io/file relpath))
        stem (subs basename 0 (- (count basename) (count ".zip")))]
    (str work-id "_" person-id "_" stem)))

(defn- parser-ir [work-hash]
  {"schema_hash" (manifest/schema-hash "schemas/parser-ir.schema.json")
   "source" {"work_content_hash" work-hash
             "encoding" "Shift_JIS"
             "normalization" "source"}
   "derived_from" {"aat_adapter" "aozora2html"
                   "aat_adapter_version" "soranoha-build-publication-smoke-v1"
                   "aat_version" 1
                   "mapping_id" "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe"
                   "mapping_schema_hash" (files/example-hash "38")
                   "mapping_version" "0.2.0"}
   "sentence_segmentation" {"schema_version" "sentence-segmentation-v1"
                            "splitter_id" "ab-plaintext-japanese-v1"
                            "coordinate_system" "decoded_utf8"
                            "coverage" "body-paragraphs"}
   "nodes" []
   "warnings" []
   "errors" []})

(defn- official-source [row relpath zip-member source-file source-hash]
  {"work_id" (row-work-id row)
   "card_person_id" (row-person-id row)
   "text_url" (get row "テキストファイルURL")
   "text_zip_relpath" relpath
   "zip_member" zip-member
   "source_hash" source-hash
   "source_bytes" (hash/byte-length source-file)})

(defn- write-materialized-work!
  [{:keys [rows catalog-provenance materialized-root selected]}]
  (let [{:keys [row file relpath]} selected
        work-id (row-work-id row)
        person-id (row-person-id row)
        work-hash (hash/format-sha256 (files/sha256-file file))
        zip-member (first-text-member file)
        work-dir (io/file materialized-root "works"
                          (slug work-id person-id relpath))
        metadata-file (io/file work-dir "metadata-record.json")]
    (.mkdirs work-dir)
    (abc-json/write-deterministic-json-file!
     (io/file work-dir "aat.json")
     {"version" 1
      "work_id" work-id
      "blocks" []})
    (abc-json/write-deterministic-json-file!
     (io/file work-dir "parser-ir.json")
     (parser-ir work-hash))
    (abc-json/write-deterministic-json-file!
     (io/file work-dir "official-source.json")
     (official-source row relpath zip-member file work-hash))
    (spit (io/file work-dir "warnings.jsonl") "")
    (aozora-ingest/run-from-rows!
     {:rows rows
      :work-id work-id
      :output (str metadata-file)
      :persons-output-dir (str (io/file materialized-root "persons"))
      :overwrite true
      :source-csv-provenance catalog-provenance})
    {:work_id work-id
     :person_id person-id
     :slug (.getName work-dir)
     :text_zip_relpath relpath
     :source_hash work-hash
     :zip_member zip-member}))

(defn- selection-report [selected rejected]
  {"selected_source_count" (count selected)
   "rejected_source_count" (count rejected)
   "selected_sources" (mapv (fn [source]
                              {"work_id" (:work_id source)
                               "person_id" (:person_id source)
                               "slug" (:slug source)
                               "text_zip_relpath" (:text_zip_relpath source)
                               "source_hash" (:source_hash source)
                               "zip_member" (:zip_member source)})
                            selected)
   "rejected_sources" (mapv identity rejected)})

(defn- materialize-selected-sources!
  [{:keys [aozora-root output-root]}]
  (let [{:keys [csv-text catalog-csv-hash]} (read-catalog-zip aozora-root)
        rows (aozora-csv/read-rows-from-string csv-text)
        rows-by-basename (catalog-index rows)
        materialized-root (io/file output-root "materialized-root")
        catalog-provenance {"source_url" nil
                            "retrieved_at" nil
                            "original_file_hash" catalog-csv-hash}
        candidates (work-zip-files aozora-root)
        selected-candidates (->> candidates
                                 (keep (fn [{:keys [file relpath]}]
                                         (when relpath
                                           (when-let [row (get rows-by-basename
                                                               (.getName file))]
                                             {:file file
                                              :relpath relpath
                                              :row row}))))
                                 (sort-by :relpath)
                                 vec)
        selected (mapv #(write-materialized-work!
                         {:rows rows
                          :catalog-provenance catalog-provenance
                          :materialized-root materialized-root
                          :selected %})
                       selected-candidates)
        selected-relpaths (set (map :relpath selected-candidates))
        rejected (->> candidates
                      (remove #(contains? selected-relpaths (:relpath %)))
                      (mapv (fn [{:keys [file relpath]}]
                              {"path" (or relpath
                                          (normalized-path
                                           (.relativize
                                            (.toPath (.getCanonicalFile
                                                      (io/file aozora-root)))
                                            (.toPath (.getCanonicalFile file)))))
                               "reason" (cond
                                          (nil? relpath)
                                          "not-under-cards-files"

                                          (not (contains? rows-by-basename
                                                          (.getName file)))
                                          "not-catalog-text-zip"

                                          :else
                                          "not-selected")})))]
    (when-not (seq selected)
      (throw (ex-info "no catalog-backed work ZIPs were selected"
                      {:aozora_root (str aozora-root)})))
    (let [report (selection-report selected rejected)]
      (abc-json/write-deterministic-json-file!
       (io/file output-root "source-selection-report.json")
       report)
      {:materialized-root materialized-root
       :report report
       :selected selected})))

(defn- read-config [path]
  (let [config-file (let [file (io/file path)]
                      (if (.isFile file)
                        file
                        (let [path-text (str path)]
                          (if (string/starts-with? path-text "abc/")
                            (io/file (subs path-text (count "abc/")))
                            file))))
        config (files/read-json config-file)
        config-schema (files/read-json config-schema-path)]
    (when-let [errors (schema/validation-errors config-schema config)]
      (throw (ex-info "publication build config schema validation failed"
                      {:path (str config-file)
                       :errors errors})))
    config))

(defn- git-sh [aozora-root & args]
  (try
    (let [{:keys [exit out]} (apply shell/sh "git" "-C" (str aozora-root) args)]
      (when (zero? exit)
        (string/trim out)))
    (catch java.io.IOException _
      nil)))

(defn- git-provenance [aozora-root]
  (let [commit (or (git-sh aozora-root "rev-parse" "HEAD")
                   (let [head (io/file aozora-root ".git" "HEAD")]
                     (when (.isFile head)
                       (string/trim (slurp head)))))
        dirty-output (git-sh aozora-root "status" "--porcelain" "--"
                             "cards" "index_pages")]
    {"aozora_git_commit" commit
     "aozora_git_dirty" (if (nil? dirty-output)
                          false
                          (not (string/blank? dirty-output)))
     "dirty_scope" "cards index_pages"}))

(defn- build-plan [opts config materialization-result]
  {"build_schema_version" "soranoha-build-publication-v0"
   "config_hash" (analysis-identity/hash-json-value config)
   "config" config
   "aozora_root" (str (:aozora-root opts))
   "snapshot_date" (:snapshot-date opts)
   "git" (git-provenance (:aozora-root opts))
   "materialized_root" (str (:materialized-root materialization-result))
   "source_selection_report" "source-selection-report.json"
   "request_set_label" (get config "request_set_label")
   "snapshot_scope" (get config "snapshot_scope")
   "selected_source_count" (get-in materialization-result
                                   [:report "selected_source_count"])})

(defn- parse-args [args]
  (let [{:keys [options errors]} (cli/parse-opts args cli-options)]
    (when (seq errors)
      (throw (ex-info (string/join "\n" errors) {:errors errors})))
    (doseq [required [:aozora-root :config :output-root]]
      (when (string/blank? (get options required))
        (throw (ex-info (str (name required) " is required")
                        {:missing required}))))
    options))

(defn- prepare-output-root! [output-root replace?]
  (let [output-root-file (io/file output-root)]
    (when (and (.exists output-root-file) (not replace?))
      (throw (ex-info "output-root already exists; pass --replace to replace it after a successful build"
                      {:output_root (str output-root-file)})))
    (.mkdirs (or (.getParentFile output-root-file) (io/file ".")))
    (io/file (str output-root ".tmp-" (System/nanoTime)))))

(defn- promote-output-root! [tmp-root output-root replace?]
  (let [target (io/file output-root)]
    (when (and replace? (.exists target))
      (files/delete-tree! target))
    (Files/move (.toPath (io/file tmp-root))
                (.toPath target)
                (into-array StandardCopyOption
                            [StandardCopyOption/ATOMIC_MOVE]))
    target))

(defn- build-publication-steps [publication-rehearsal-fn]
  [{:id :materialize-source-selection
    :requires [:aozora-root :output-root]
    :produces [:materialization-result]
    :run (fn [{:keys [aozora-root output-root]}]
           (let [result (materialize-selected-sources!
                         {:aozora-root aozora-root
                          :output-root output-root})
                 selection-report-file (io/file output-root
                                                "source-selection-report.json")]
             {:state-updates {:materialization-result result}
              :outputs [{:role "source-selection-report"
                         :path (str selection-report-file)
                         :content_hash
                         (manifest/file-hash selection-report-file)}]}))}
   {:id :write-build-records
    :requires [:opts :config-value :materialization-result :output-root]
    :produces [:build-plan]
    :run (fn [{:keys [opts config-value materialization-result output-root]}]
           (let [plan (build-plan opts config-value materialization-result)
                 config-file (io/file output-root "build-config.json")
                 plan-file (io/file output-root "build-plan.json")]
             (abc-json/write-deterministic-json-file! config-file config-value)
             (abc-json/write-deterministic-json-file! plan-file plan)
             {:state-updates {:build-plan plan}
              :outputs [{:role "build-config"
                         :path (str config-file)
                         :content_hash (manifest/file-hash config-file)}
                        {:role "build-plan"
                         :path (str plan-file)
                         :content_hash (manifest/file-hash plan-file)}]}))}
   {:id :publication-rehearsal
    :requires [:build-plan :config-value :materialization-result :output-root
               :snapshot-date]
    :produces [:rehearsal-result]
    :run (fn [{:keys [config-value materialization-result output-root
                      snapshot-date]}]
           (let [rehearsal-root (io/file output-root "rehearsal")
                 exit-code (publication-rehearsal-fn
                            (str (:materialized-root materialization-result))
                            (str rehearsal-root)
                            (get config-value "request_set_label")
                            (get config-value "snapshot_scope")
                            snapshot-date)]
             (when-not (zero? exit-code)
               (throw (ex-info "publication rehearsal failed"
                               {:exit_code exit-code
                                :rehearsal_root (str rehearsal-root)})))
             {:state-updates {:rehearsal-result {:exit-code exit-code
                                                 :rehearsal-root rehearsal-root}}
              :outputs [{:role "rehearsal-workflow-run"
                         :path (str (io/file rehearsal-root
                                             "workflow-run.json"))}]}))}])

(defn build-publication!
  [publication-rehearsal-fn args]
  (let [{:keys [aozora-root config snapshot-date output-root replace]
         :as opts} (parse-args args)
        config-value (read-config config)]
    (when (string/blank? snapshot-date)
      (throw (ex-info "snapshot-date is required for build-publication"
                      {:config config})))
    (let [tmp-root (prepare-output-root! output-root replace)]
      (.mkdirs tmp-root)
      (let [opts (assoc opts :output-root tmp-root)
            _ (workflow/run-workflow!
               {:workflow-id "soranoha.build-publication.v1"
                :run-id (str "build-publication:" snapshot-date)
                :output-root tmp-root
                :initial-state {:aozora-root aozora-root
                                :config-value config-value
                                :snapshot-date snapshot-date
                                :output-root tmp-root
                                :opts opts}
                :steps (build-publication-steps publication-rehearsal-fn)})
            final-root (promote-output-root! tmp-root output-root replace)]
        (println "build_publication_root:" (str final-root))
        (println "materialized_root:" (str (io/file final-root
                                                    "materialized-root")))
        (println "rehearsal_root:" (str (io/file final-root
                                                 "rehearsal")))
        0))))
