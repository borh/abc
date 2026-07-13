(ns abc.tools.schema
  (:require [abc.tools.hash :as hash]
            [abc.tools.json :as abc-json]
            [abc.tools.malli :as am]
            [babashka.fs :as fs]
            [charred.api :as json]
            [clojure.string :as str])
  (:import [com.networknt.schema SchemaRegistry InputFormat SpecificationVersion]))

(defn read-schema [file]
  (abc-json/read-json-file file))

(defn schema-hash [file]
  (hash/format-sha256
   (hash/sha256-json-jcs (read-schema file))))

(def checked-in-schema-paths
  {:manifest "schemas/manifest.schema.json"
   :parser-ir "schemas/parser-ir.schema.json"
   :diagnostic "schemas/diagnostic.schema.json"
   :run-summary "schemas/run-summary.schema.json"
   :manifest-inputs "schemas/manifest-inputs.schema.json"
   :comparison-report "schemas/comparison-report.schema.json"})

(defn checked-in-schema-hashes []
  (into {}
        (map (fn [[k path]]
               [k (schema-hash path)]))
        checked-in-schema-paths))

;; ---------------------------------------------------------------------------
;; SchemaRegistry — created once at namespace load; thread-safe and caches
;; Schema objects keyed by $id / content. Draft 2020-12 is the default dialect
;; when $schema is absent from the schema data.
;; ---------------------------------------------------------------------------
(defn- checked-in-schema-resources
  ([] (checked-in-schema-resources (fs/path "schemas")))
  ([schema-dir]
   (if-not (fs/directory? schema-dir)
     {}
     (into {}
           (keep (fn [path]
                   (when (and (fs/regular-file? path)
                              (str/ends-with? (str (fs/file-name path))
                                              ".schema.json"))
                     (let [file (fs/file path)
                           content (slurp file)
                           schema (abc-json/read-json-file file)]
                       (when-let [schema-id (get schema "$id")]
                         [schema-id content])))))
           (->> (tree-seq fs/directory?
                          (fn [path]
                            (try
                              (sort-by str (fs/list-dir path))
                              (catch java.io.IOException _
                                [])
                              (catch SecurityException _
                                [])))
                          (fs/path schema-dir))
                (sort-by #(str (fs/relativize schema-dir %))))))))

(def ^:private schema-registry
  (SchemaRegistry/withDefaultDialect
   SpecificationVersion/DRAFT_2020_12
   (reify java.util.function.Consumer
     (accept [_ builder]
       (.schemas builder (checked-in-schema-resources))))))

(defn- ^:private instance-location->path-segments
  "Convert a networknt instance-location JSON Pointer string (e.g.
  \"/properties/foo/bar\") into a seq of path segments (e.g.
  [\"foo\" \"bar\"]), matching the m3 :document-path convention.
  Returns an empty vector for the root location."
  [^String location]
  (if (or (nil? location) (= location ""))
    []
    (into [] (remove empty?) (str/split location #"/"))))

(defn- ^:private error->map
  "Convert a single com.networknt.schema.Error into the Clojure map
  shape expected by abc.tools.malli/humanize-validation-errors."
  [^com.networknt.schema.Error e]
  {:document-path (instance-location->path-segments (str (.getInstanceLocation e)))
   :schema-path   (str (.getSchemaLocation e))
   :message       (.getMessage e)})

(defn validation-errors [schema value]
  (let [schema-json (json/write-json-str schema)
        value-json  (json/write-json-str value)
        schema-obj  (.getSchema schema-registry schema-json)
        errors      (.validate schema-obj value-json InputFormat/JSON)]
    (when (seq errors)
      (mapv error->map errors))))

(defn validation-errors-humanized
  "Return [errors humanized-strings] for `value` against `schema`.
  Both nil/empty when `value` validates."
  [schema value]
  (let [errors (validation-errors schema value)]
    (if (seq errors)
      [errors (am/humanize-validation-errors errors)]
      [nil nil])))

(defn validate-json! [schema path]
  (when-let [errors (validation-errors schema (abc-json/read-json-file path))]
    (throw (ex-info (str "JSON Schema validation failed: " path)
                    {:path (str path)
                     :errors errors}))))

(defn validate-jsonl! [schema values path]
  (doseq [value values]
    (when-let [errors (validation-errors schema value)]
      (throw (ex-info (str "JSON Schema validation failed: " path)
                      {:path (str path)
                       :value value
                       :errors errors})))))

(defn schema-valid! [schema path]
  (when-let [errors (validation-errors {"$schema" "https://json-schema.org/draft/2020-12/schema"}
                                       schema)]
    (throw (ex-info (str "Invalid JSON Schema: " path)
                    {:path (str path)
                     :errors errors}))))
