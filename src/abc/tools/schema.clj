(ns abc.tools.schema
  (:require [abc.tools.hash :as hash]
            [abc.tools.json :as abc-json]
            [abc.tools.malli :as am]
            [m3.json-schema :as m3]))

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

(defn validation-errors [schema value]
  (let [result (m3/validate schema value {:draft :draft2020-12})]
    (when-not (:valid? result)
      (:errors result))))

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
