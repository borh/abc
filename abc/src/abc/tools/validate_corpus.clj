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
            [babashka.fs :as fs]
            [clojure.string :as string]
            [taoensso.telemere :as tel]))

(defn- list-json-files [dir]
  (->> (fs/list-dir dir)
       (filter #(and (fs/regular-file? %)
                     (string/ends-with? (str (fs/file-name %)) ".json")))
       (sort-by #(str (fs/file-name %)))
       (map fs/file)))

(defn- person-loader
  "Memoized loader: person_id -> parsed JSON body. Throws if the file
  is missing or fails person-schema validation."
  [persons-dir]
  (let [cache (atom {})]
    (fn [pid]
      (or (get @cache pid)
          (let [f (fs/file persons-dir (str pid ".json"))]
            (when-not (fs/exists? f)
              (throw (ex-info (str "person file missing: " (str f))
                              {:person-id pid :path (str f)})))
            (let [body (files/read-json f)]
              (person-record/validate! body)
              (swap! cache assoc pid body)
              body))))))

(defn- check-work
  "Returns nil on success; a {:work-id, :stage, :error} map on failure."
  [work-file shapes-graph load-person]
  (let [work-id (string/replace (str (fs/file-name work-file)) #"\.json$" "")]
    (try
      (let [record (files/read-json work-file)]
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
  (let [works-dir (fs/file input-dir "works")
        persons-dir (fs/file input-dir "persons")]
    (when-not (fs/directory? works-dir)
      (throw (ex-info (str "no works/ subdirectory under " input-dir)
                      {:input-dir input-dir})))
    (when-not (fs/directory? persons-dir)
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
