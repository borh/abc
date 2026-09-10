(ns ab-research.schema
  (:require [ab-research.files :as files]
            [ab-research.hash :as hash]
            [soranoha.core.json :as record-json]
            [babashka.fs :as fs]
            [charred.api :as json]
            [clojure.string :as str])
  (:import [com.networknt.schema SchemaRegistry InputFormat SpecificationVersion]))

(defn read-schema [file]
  (record-json/read-json-file file))

(defn schema-hash [file]
  (hash/format-sha256
   (hash/sha256-json-jcs (read-schema file))))

(let [cache (atom {})]
  (defn cached-schema
    "Read and parse the JSON Schema at `path` exactly once per JVM.
    Identity-stable: callers can compare with `identical?`."
    [path]
    (or (get @cache path)
        (let [v (read-schema path)]
          (swap! cache assoc path v)
          v)))

  (defn cached-schema-hash
    "Compute and cache the schema-bytes hash for `path`. Delegates to
    `schema-hash` so the on-disk hash contract is preserved exactly."
    [path]
    (or (get @cache [::hash path])
        (let [v (schema-hash path)]
          (swap! cache assoc [::hash path] v)
          v))))

;; SchemaRegistry: created once at namespace load; thread-safe and caches
;; Schema objects keyed by $id / content. Draft 2020-12 is the default dialect
;; when $schema is absent from the schema data.
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
                           schema (record-json/read-json-file file)]
                       (when-let [schema-id (get schema "$id")]
                         [schema-id content])))))
           (->> (files/sorted-path-seq schema-dir)
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
  shape expected by `humanize-validation-errors`."
  [^com.networknt.schema.Error e]
  {:document-path (instance-location->path-segments (str (.getInstanceLocation e)))
   :schema-path   (str (.getSchemaLocation e))
   :message       (.getMessage e)})

(defn- m3-leaf-errors
  "Walk an m3 error tree. m3 nests errors via `:errors`; leaves carry
  `:document-path`, `:schema-path`, and `:message`. Yields a flat seq
  of leaf maps (descending into `:errors` when present, ignoring
  intermediate composite-schema messages)."
  [node]
  (cond
    (sequential? node) (mapcat m3-leaf-errors node)
    (and (map? node) (seq (:errors node))) (mapcat m3-leaf-errors (:errors node))
    (map? node) [(select-keys node [:document-path :schema-path :message])]
    :else nil))

(defn humanize-validation-errors
  "Format an m3 error vector into a flat sequence of readable strings.
  Returns an empty vector when `errors` is nil or empty."
  [errors]
  (->> (m3-leaf-errors errors)
       (mapv (fn [{:keys [document-path message]}]
               (let [path (when (seq document-path)
                            (str/join "/" (map str document-path)))]
                 (cond
                   (and path message) (str path ": " message)
                   message message
                   path path
                   :else (pr-str document-path)))))))

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
      [errors (humanize-validation-errors errors)]
      [nil nil])))

(defn validate-json! [schema path]
  (when-let [errors (validation-errors schema (record-json/read-json-file path))]
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
