(ns soranoha.core.schema
  (:require [soranoha.core.json :as record-json]
            [charred.api :as json]
            [clojure.string :as str])
  (:import [com.networknt.schema SchemaRegistry InputFormat SpecificationVersion]))

(defn read-schema [file]
  (record-json/read-json-file file))

(def ^:private schema-registry
  (SchemaRegistry/withDefaultDialect SpecificationVersion/DRAFT_2020_12))

(defn- ^:private instance-location->path-segments
  "Convert a networknt instance-location JSON Pointer string (e.g.
  \"/properties/foo/bar\") into a seq of path segments (e.g.
  [\"foo\" \"bar\"]), for structured validation diagnostics.
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

(defn humanize-validation-errors
  "Format validation errors as readable strings.
  Returns an empty vector when `errors` is nil or empty."
  [errors]
  (->> errors
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
