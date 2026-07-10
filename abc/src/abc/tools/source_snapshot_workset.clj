(ns abc.tools.source-snapshot-workset
  (:require [abc.tools.cli :as abc-cli]
            [abc.tools.files :as files]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [taoensso.telemere :as tel]))

(def required-file-names
  {:aat-path "aat.json"
   :parser-ir-path "parser-ir.json"
   :metadata-record-path "metadata-record.json"
   :official-source-path "official-source.json"})

(def resolved-path-key
  {:aat_path :resolved_aat_path
   :parser_ir_path :resolved_parser_ir_path
   :metadata_record_path :resolved_metadata_record_path
   :official_source_path :resolved_official_source_path
   :source_manifest_path :resolved_source_manifest_path})

(def official-text-zip-relpath-pattern
  #"^cards/[0-9]{6}/files/[^/]+\.zip$")

(def hash-pattern
  #"^sha256:[0-9a-f]{64}$")

(defn- map-value [m k]
  (or (get m k)
      (get m (name k))))

(defn- required-value [m k context]
  (or (map-value m k)
      (throw (ex-info (str context " missing required key " k)
                      {:key k :context context :value m}))))

(defn- normalize-path [path]
  (string/replace (str path) "\\" "/"))

(defn- canonical-file [path]
  (.getCanonicalFile (io/file path)))

(defn- absolute-path? [path]
  (.isAbsolute (io/file path)))

(defn- resolve-path [base-dir path]
  (when path
    (if (absolute-path? path)
      path
      (str (.getCanonicalFile (io/file base-dir path))))))

(defn- resolve-work-paths [work base-dir]
  (reduce (fn [resolved k]
            (if-let [path (map-value resolved k)]
              (assoc resolved (resolved-path-key k)
                     (resolve-path base-dir path))
              resolved))
          work
          (keys resolved-path-key)))

(defn- relative-path [from-dir to-file]
  (files/relative-path (canonical-file from-dir) (canonical-file to-file)))

(defn- candidate-work-dir? [dir]
  (some #(.exists (io/file dir %)) (vals required-file-names)))

(defn- work-dirs [input-root]
  (->> (file-seq (io/file input-root))
       (filter #(.isDirectory %))
       (filter candidate-work-dir?)
       (sort-by #(normalize-path
                  (.relativize (.toPath (canonical-file input-root))
                               (.toPath (canonical-file %)))))
       vec))

(defn- required-file [work-dir file-name label]
  (let [file (io/file work-dir file-name)]
    (when-not (.isFile file)
      (throw (ex-info (str "work directory " (normalize-path work-dir)
                           " missing " file-name)
                      {:work-dir (str work-dir)
                       :file-name file-name
                       :label label})))
    file))

(defn- author-person-id [metadata-record]
  (or (some (fn [contributor]
              (when (= "著者" (map-value contributor :relation_to_work))
                (map-value contributor :person_id)))
            (map-value metadata-record :contributors))
      (map-value (first (map-value metadata-record :contributors)) :person_id)))

(defn- valid-official-source? [official-source work-id]
  (and (= work-id (map-value official-source :work_id))
       (re-matches official-text-zip-relpath-pattern
                   (or (map-value official-source :text_zip_relpath)
                       ""))
       (re-matches hash-pattern
                   (or (map-value official-source :source_hash)
                       ""))))

(defn- official-source [work-dir work-id]
  (let [official-source-file (required-file work-dir
                                            "official-source.json"
                                            :official-source-path)
        official-source (files/read-json official-source-file)]
    (when-not (valid-official-source? official-source work-id)
      (throw (ex-info "official-source.json is not an Aozora card/files work source"
                      {:work-dir (str work-dir)
                       :work-id work-id
                       :official-source-path (str official-source-file)
                       :official-source official-source})))
    official-source-file))

(defn- work-entry [path-base work-dir]
  (let [aat-file (required-file work-dir "aat.json" :aat-path)
        parser-ir-file (required-file work-dir "parser-ir.json" :parser-ir-path)
        metadata-record-file (required-file work-dir
                                            "metadata-record.json"
                                            :metadata-record-path)
        metadata-record (files/read-json metadata-record-file)
        work (required-value metadata-record :work "metadata record")
        work-id (required-value work :work_id "metadata work")
        title (required-value work :title "metadata work")
        official-source-file (official-source work-dir work-id)
        person-id (or (author-person-id metadata-record)
                      (throw (ex-info "metadata record missing contributor person_id"
                                      {:metadata-record-path
                                       (str metadata-record-file)})))
        slug (-> (relative-path path-base work-dir)
                 (string/split #"/")
                 last)]
    {:slug slug
     :title title
     :work_id work-id
     :person_id person-id
     :card_url (map-value work :card_url)
     :aat_path (relative-path path-base aat-file)
     :parser_ir_path (relative-path path-base parser-ir-file)
     :metadata_record_path (relative-path path-base metadata-record-file)
     :official_source_path (relative-path path-base official-source-file)
     :source_manifest_path (relative-path path-base
                                          (io/file work-dir
                                                   "source.manifest.json"))}))

(defn workset-from-root
  [{:keys [input-root path-base snapshot-scope snapshot-date]}]
  (let [input-root-file (canonical-file
                         (or input-root
                             (throw (ex-info "input-root is required" {}))))
        path-base-file (canonical-file (or path-base input-root-file))
        works (->> (work-dirs input-root-file)
                   (map #(work-entry path-base-file %))
                   (sort-by (juxt :work_id :slug))
                   vec)]
    (when-not (seq works)
      (throw (ex-info "input root contains no materialized work directories"
                      {:input-root (str input-root-file)})))
    {:snapshot_scope (or snapshot-scope
                         (throw (ex-info "snapshot-scope is required" {})))
     :snapshot_date (or snapshot-date
                        (throw (ex-info "snapshot-date is required" {})))
     :works works}))

(defn write-workset!
  [{:keys [output-path] :as opts}]
  (let [output-file (io/file (or output-path
                                 (throw (ex-info "output-path is required" {}))))
        output-parent (or (.getParentFile output-file) (io/file "."))
        value (workset-from-root (assoc opts :path-base output-parent))]
    (.mkdirs output-parent)
    (spit output-file (str (pr-str value) "\n"))
    {:output output-file
     :works-count (count (:works value))}))

(defn read-workset [path]
  (let [workset-file (io/file path)
        base-dir (.getParentFile (.getCanonicalFile workset-file))
        value (files/read-edn workset-file)]
    (when-not (seq (map-value value :works))
      (throw (ex-info "workset must contain non-empty :works"
                      {:workset-path path})))
    (assoc value :works (mapv #(resolve-work-paths % base-dir)
                              (map-value value :works)))))

(defn usage [_summary]
  (str "Usage: clojure -M:abc/source-snapshot-workset "
       "--input-root ROOT --output workset.edn "
       "--snapshot-scope SCOPE --snapshot-date YYYY-MM-DD"))

(def cli-options
  [["-i" "--input-root DIR" "Materialized corpus root containing work directories."
    :id :input-root]
   ["-o" "--output FILE" "Output EDN workset path."
    :id :output-path]
   [nil "--snapshot-scope SCOPE" "Source snapshot scope string."
    :id :snapshot-scope]
   [nil "--snapshot-date DATE" "Source snapshot date, normally YYYY-MM-DD."
    :id :snapshot-date]])

(defn -main [& args]
  (abc-cli/run-cli!
   args
   {:cli-options cli-options
    :required    [:input-root :output-path :snapshot-scope :snapshot-date]
    :usage-fn    usage
    :run         (fn [{:keys [options]}]
                   (let [{:keys [input-root output-path snapshot-scope snapshot-date]} options
                         {:keys [output works-count]}
                         (write-workset! {:input-root input-root
                                          :output-path output-path
                                          :snapshot-scope snapshot-scope
                                          :snapshot-date snapshot-date})]
                     (tel/log! :info (str "wrote " works-count
                                          " source snapshot work entries to "
                                          output))))}))
