(ns abc.tools.aozora-ingest
  "CLI: build a metadata-record JSON + N person-record JSON files from
  an Aozora list_person_all_extended ZIP slice for a given work-id.
  Validates the result against the schema and asserts schema-hash
  self-consistency before writing. Optionally refreshes a manifest
  (Task 11)."
  (:require [abc.tools.aozora-csv :as ac]
            [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.json :as json]
            [abc.tools.logging :as logging]
            [abc.tools.manifest :as manifest]
            [abc.tools.metadata-record :as metadata-record]
            [abc.tools.person-record :as person-record]
            [abc.tools.schema :as schema]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.tools.cli :as cli]
            [taoensso.telemere :as tel])
  (:import [java.util.zip ZipFile ZipEntry]))

(def schema-path "schemas/metadata-record.schema.json")
(def schema-id "https://w3id.org/abc/schemas/metadata-record.schema.json")
(def person-schema-path "schemas/person-record.schema.json")
(def person-schema-id "https://w3id.org/abc/schemas/person-record.schema.json")

(defn- read-zip-csv [^String zip-path]
  (with-open [zf (ZipFile. (io/file zip-path))]
    (let [csv-entry (->> (enumeration-seq (.entries zf))
                         (filter (fn [^ZipEntry e]
                                   (string/ends-with? (.getName e) ".csv")))
                         first)]
      (when-not csv-entry
        (throw (ex-info (str "no .csv entry in " zip-path)
                        {:zip-path zip-path})))
      (slurp (.getInputStream zf csv-entry)))))

(def ^:private person-body-keys
  ["person_id"
   "family_name"
   "given_name"
   "family_name_reading"
   "given_name_reading"
   "family_name_sort"
   "given_name_sort"
   "family_name_romaji"
   "given_name_romaji"
   "date_of_birth"
   "date_of_death"
   "person_copyright_expired"
   "external_links"])

(defn- build-person-record
  "Wrap a parsed person body with the self-describing schema fields.
  The body contributes only the whitelisted bibliographic keys; this
  protects identity hashing from accidental field bleed-through if the
  upstream parser ever emits extra columns."
  [body]
  (merge (zipmap person-body-keys (map #(get body %) person-body-keys))
         {"person_record_schema_id" person-schema-id
          "person_record_schema_hash" (manifest/schema-hash person-schema-path)
          "external_links" (or (get body "external_links") [])}))

(defn- write-person-file!
  "Write the person record at <persons-dir>/<person_id>.json with the
  corruption-safe idempotent check. If the file already exists:
  - parse it as JSON (fail loudly if unparseable)
  - validate against the schema (fail loudly if invalid)
  - recompute its hash; if same as the just-built record, no-op rewrite
  - if different and overwrite? is false, throw
  - if different and overwrite? is true, replace
  Returns the just-built record's person_record_hash."
  [persons-dir record overwrite?]
  (let [pid (get record "person_id")
        target (io/file persons-dir (str pid ".json"))
        new-hash (person-record/record-hash record)]
    (.mkdirs (io/file persons-dir))
    (if (.exists target)
      (let [existing (try (files/read-json (str target))
                          (catch Exception e
                            (throw (ex-info (str "on-disk person file " target
                                                 " is unparseable JSON")
                                            {:person-id pid
                                             :path (str target)}
                                            e))))]
        (person-record/validate! existing)
        (let [existing-hash (person-record/record-hash existing)]
          (cond
            (= new-hash existing-hash)
            (json/write-deterministic-json-file! target record)

            overwrite?
            (json/write-deterministic-json-file! target record)

            :else
            (throw (ex-info
                    (str "refuse to overwrite " target
                         "; on-disk person_record_hash " existing-hash
                         " differs from rebuilt " new-hash
                         ". Pass --overwrite to replace.")
                    {:person-id pid
                     :path (str target)
                     :existing-hash existing-hash
                     :new-hash new-hash})))))
      (json/write-deterministic-json-file! target record))
    new-hash))

(defn- self-consistency-check! [record]
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

(defn run-from-rows!
  "Programmatic entry that operates on already-parsed CSV rows. Writes
  N+1 deterministic JSON files (the work metadata-record + each
  contributor person record) and returns the new metadata_record_hash.

  Required keys: :rows (seq of CSV-row maps), :work-id, :output.
  Optional: :persons-output-dir (defaults to '<output-dir>/persons'),
            :overwrite (boolean, default false),
            :refresh-manifest (path; if set, rewrites that manifest's
              metadata_record_hash and recomputes artifact_id)."
  [{:keys [rows work-id output persons-output-dir overwrite refresh-manifest]}]
  (let [matching (filter #(= work-id (get % "作品ID")) rows)]
    (when-not (seq matching)
      (throw (ex-info (str "no rows for work_id " work-id " in supplied rows")
                      {:work-id work-id})))
    (let [{:keys [work persons-by-id contributors]} (ac/build-record-fragment-from-rows matching)
          persons-dir (or persons-output-dir
                          (str (.getParent (io/file output)) "/persons"))
          contributor-entries
          (vec
           (for [c contributors
                 :let [pid (get c "person_id")
                       body (get persons-by-id pid)
                       record (build-person-record body)
                       new-hash (write-person-file! persons-dir record overwrite)]]
             {"person_id" pid
              "person_record_hash" new-hash
              "relation_to_work" (get c "relation_to_work")}))
          metadata-rec {"metadata_record_schema_id" schema-id
                        "metadata_record_schema_hash" (manifest/schema-hash schema-path)
                        "work" work
                        "contributors" (vec (sort-by #(get % "person_id")
                                                     contributor-entries))}]
      (self-consistency-check! metadata-rec)
      (.mkdirs (.getParentFile (io/file output)))
      (json/write-deterministic-json-file! (io/file output) metadata-rec)
      (let [new-hash (metadata-record/record-hash metadata-rec)]
        (tel/log! :info (str "metadata_record_hash: " new-hash))
        (when refresh-manifest
          (let [m (files/read-json refresh-manifest)
                m' (assoc-in m ["manifest_identity_object" "metadata_record_hash"] new-hash)
                identity-obj (get m' "manifest_identity_object")
                artifact-id (hash/format-sha256 (hash/sha256-json-jcs identity-obj))
                m'' (assoc m' "artifact_id" artifact-id)]
            (json/write-deterministic-json-file! (io/file refresh-manifest) m'')
            (tel/log! :info (str "refreshed manifest " refresh-manifest))))
        new-hash))))

(defn run!
  "CLI-shaped entry: reads the CSV from a ZIP path and delegates to
  run-from-rows!. Same return value."
  [{:keys [zip-path] :as opts}]
  (let [csv (read-zip-csv zip-path)
        rows (ac/read-rows-from-string csv)]
    (run-from-rows! (-> opts (dissoc :zip-path) (assoc :rows rows)))))

(def cli-options
  [["-z" "--zip ZIP" "Path to an Aozora list_person_all_extended ZIP."
    :id :zip-path]
   ["-w" "--work-id WORK_ID" "Aozora work ID (6-digit zero-padded string)."
    :id :work-id]
   ["-o" "--output FILE" "Output JSON path (work metadata-record)."
    :id :output]
   [nil "--persons-output-dir DIR"
    "Directory for per-person JSON files (default: <output-dir>/persons)."
    :id :persons-output-dir]
   [nil "--overwrite" "Overwrite an existing on-disk person file whose hash differs."
    :id :overwrite :default false]
   [nil "--refresh-manifest FILE"
    "Rewrite the named manifest.json's metadata_record_hash + artifact_id in place."
    :id :refresh-manifest]])

(defn usage []
  (tel/log! :warn (str "Usage: clojure -M:abc/aozora-ingest "
                       "--zip <path-to-zip> --work-id NNNNNN --output <path>"
                       " [--persons-output-dir DIR] [--overwrite]"
                       " [--refresh-manifest manifest.json]")))

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
      (do
        (run! options)
        (tel/log! :info (str "wrote metadata-record to " (:output options)))))))
