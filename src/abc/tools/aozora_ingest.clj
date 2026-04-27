(ns abc.tools.aozora-ingest
  "CLI: build a metadata-record JSON from an Aozora list_person_all_extended
  ZIP slice for a given work-id. Validates the result against the schema and
  asserts schema-hash self-consistency before writing."
  (:require [abc.tools.aozora-csv :as ac]
            [abc.tools.files :as files]
            [abc.tools.json :as json]
            [abc.tools.logging :as logging]
            [abc.tools.manifest :as manifest]
            [abc.tools.schema :as schema]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.tools.cli :as cli]
            [taoensso.telemere :as tel])
  (:import [java.util.zip ZipFile ZipEntry]))

(def schema-path "schemas/metadata-record.schema.json")
(def schema-id "https://w3id.org/abc/schemas/metadata-record.schema.json")

(defn- read-zip-csv
  "Open `zip-path`; locate the first .csv entry; return its contents as
  a UTF-8 string. Aozora's list_person_all_extended_utf8.zip ships
  exactly one CSV inside."
  [^String zip-path]
  (with-open [zf (ZipFile. (io/file zip-path))]
    (let [csv-entry (->> (enumeration-seq (.entries zf))
                         (filter (fn [^ZipEntry e]
                                   (string/ends-with? (.getName e) ".csv")))
                         first)]
      (when-not csv-entry
        (throw (ex-info (str "no .csv entry in " zip-path)
                        {:zip-path zip-path})))
      (slurp (.getInputStream zf csv-entry)))))

(defn- rows-for-work [csv-text work-id]
  (filter (fn [row] (= work-id (get row "作品ID")))
          (ac/read-rows-from-string csv-text)))

(defn build-metadata-record-from-zip
  "Slice the CSV inside `zip-path` to rows whose 作品ID matches
  `work-id`, parse them, and return a metadata-record value with the
  self-describing schema-id and schema-hash fields populated."
  [{:keys [zip-path work-id]}]
  (let [csv (read-zip-csv zip-path)
        rows (rows-for-work csv work-id)]
    (when-not (seq rows)
      (throw (ex-info (str "no rows for work_id " work-id " in " zip-path)
                      {:zip-path zip-path :work-id work-id})))
    (let [{:keys [work persons]} (ac/build-record-fragment-from-rows rows)]
      {"metadata_record_schema_id" schema-id
       "metadata_record_schema_hash" (manifest/schema-hash schema-path)
       "work" work
       "persons" persons})))

(defn- self-consistency-check!
  "After building, validate against the schema and assert the embedded
  schema-hash matches the live schema's hash."
  [record]
  (let [s (files/read-json schema-path)
        errors (schema/validation-errors s record)]
    (when (seq errors)
      (throw (ex-info "generated metadata-record fails schema validation"
                      {:errors errors}))))
  (let [embedded (get record "metadata_record_schema_hash")
        live (manifest/schema-hash schema-path)]
    (when-not (= embedded live)
      (throw (ex-info "self-consistency: embedded schema hash differs from live"
                      {:embedded embedded :live live}))))
  :ok)

(def cli-options
  [["-z" "--zip ZIP" "Path to an Aozora list_person_all_extended ZIP."
    :id :zip-path]
   ["-w" "--work-id WORK_ID" "Aozora work ID (6-digit zero-padded string)."
    :id :work-id]
   ["-o" "--output FILE" "Output JSON path."
    :id :output]])

(defn usage []
  (tel/log! :warn (str "Usage: clojure -M:abc/aozora-ingest "
                       "--zip <path-to-zip> --work-id NNNNNN --output <path-to-record.json>")))

(defn -main [& args]
  (logging/install-cli-handler!)
  (let [{:keys [options errors]} (cli/parse-opts args cli-options)]
    (if (or (seq errors)
            (nil? (:zip-path options))
            (nil? (:work-id options))
            (nil? (:output options)))
      (do
        (doseq [e errors] (tel/log! :error e))
        (usage)
        (System/exit 2))
      (let [record (build-metadata-record-from-zip options)]
        (self-consistency-check! record)
        (json/write-deterministic-json-file! (io/file (:output options)) record)
        (tel/log! :info (str "wrote metadata-record to " (:output options)))))))
