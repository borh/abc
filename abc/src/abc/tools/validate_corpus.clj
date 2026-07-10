(ns abc.tools.validate-corpus
  "CLI: validate every metadata-record + person-record under a corpus
  directory produced by `aozora-ingest --all`. Performs three checks
  per work:
    1. JSON schema validation of the metadata-record on disk.
    2. JSON schema validation of each referenced person-record.
    3. SHACL validation of the combined RDF graph.

  Reports a summary {:works-checked, :failed, :first-failures}.
  Person bodies are cached by person_id; the SHACL shapes graph is
  loaded once."
  (:require [abc.tools.cli :as abc-cli]
            [abc.tools.files :as files]
            [abc.tools.metadata-record :as metadata-record]
            [abc.tools.person-record :as person-record]
            [abc.tools.shacl :as shacl]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [taoensso.telemere :as tel]))

(defn- list-json-files [^java.io.File dir]
  (->> (.listFiles dir)
       (filter (fn [^java.io.File f]
                 (and (.isFile f)
                      (string/ends-with? (.getName f) ".json"))))
       (sort-by #(.getName ^java.io.File %))))

(defn- person-loader
  "Memoized loader: person_id -> parsed JSON body. Throws if the file
  is missing or fails person-schema validation."
  [^java.io.File persons-dir]
  (let [cache (atom {})]
    (fn [pid]
      (or (get @cache pid)
          (let [f (io/file persons-dir (str pid ".json"))]
            (when-not (.exists f)
              (throw (ex-info (str "person file missing: " (.getPath f))
                              {:person-id pid :path (.getPath f)})))
            (let [body (files/read-json (.getPath f))]
              (person-record/validate! body)
              (swap! cache assoc pid body)
              body))))))

(defn- check-work
  "Returns nil on success; a {:work-id, :stage, :error} map on failure."
  [^java.io.File work-file shapes-graph load-person]
  (let [work-id (string/replace (.getName work-file) #"\.json$" "")]
    (try
      (let [record (files/read-json (.getPath work-file))]
        (metadata-record/validate! record)
        (let [persons-by-id (into {}
                                  (for [c (get record "contributors")
                                        :let [pid (get c "person_id")]]
                                    [pid (load-person pid)]))
              data-graph (metadata-record/record+persons->graph record persons-by-id)]
          (shacl/validate! {:shapes-graph shapes-graph
                            :data-graph data-graph
                            :label (str "work " work-id)})
          nil))
      (catch clojure.lang.ExceptionInfo e
        {:work-id work-id
         :error (ex-message e)
         :data (ex-data e)}))))

(defn validate-corpus!
  "Validate every work under <input-dir>/works/ and its referenced
  persons under <input-dir>/persons/. Returns
  {:works-checked, :failed, :first-failures}.

  :max-failures (default 10) caps the failure list returned in
  :first-failures. The :failed count covers all failures regardless."
  [{:keys [input-dir max-failures]
    :or {max-failures 10}}]
  (let [works-dir (io/file input-dir "works")
        persons-dir (io/file input-dir "persons")]
    (when-not (.isDirectory works-dir)
      (throw (ex-info (str "no works/ subdirectory under " input-dir)
                      {:input-dir input-dir})))
    (when-not (.isDirectory persons-dir)
      (throw (ex-info (str "no persons/ subdirectory under " input-dir)
                      {:input-dir input-dir})))
    (let [shapes-graph (shacl/load-shapes-graph)
          load-person (person-loader persons-dir)
          work-files (list-json-files works-dir)
          failures (atom [])
          checked (atom 0)]
      (doseq [wf work-files]
        (swap! checked inc)
        (when-let [failure (check-work wf shapes-graph load-person)]
          (swap! failures conj failure)))
      {:works-checked @checked
       :failed (count @failures)
       :first-failures (vec (take max-failures @failures))})))

(def cli-options
  [["-i" "--input-dir DIR"
    "Corpus root containing works/ and persons/ subdirectories."
    :id :input-dir]
   [nil "--max-failures N" "Cap on returned failure list (default: 10)."
    :id :max-failures
    :default 10
    :parse-fn #(Integer/parseInt %)]])

(defn usage [_summary]
  "Usage: clojure -M:abc/validate-corpus --input-dir <DIR>")

(defn -main [& args]
  (abc-cli/run-cli!
   args
   {:cli-options cli-options
    :required    [:input-dir]
    :usage-fn    usage
    :run         (fn [{:keys [options]}]
                   (let [{:keys [works-checked failed first-failures] :as result}
                         (validate-corpus! options)]
                     (tel/log! :info (str "checked " works-checked " works, " failed " failed"))
                     (doseq [f first-failures]
                       (tel/log! :error (str (:work-id f) ": " (:error f))))
                     result))
    :fail?       (fn [{:keys [failed]}] (pos? failed))}))
