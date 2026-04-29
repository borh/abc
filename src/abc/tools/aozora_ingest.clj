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
            [abc.tools.malli :as am]
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

(defn- read-zip-csv
  "Returns {:csv, :provenance}. :provenance carries
  source_url (caller-supplied or nil), retrieved_at (the CSV entry's
  stored mtime as ISO instant when available, else nil), and
  original_file_hash (sha256 of the CSV bytes, always computable).
  This is the only fact-set the ingester knows about the source."
  [^String zip-path source-url]
  (with-open [zf (ZipFile. (io/file zip-path))]
    (let [csv-entry (->> (enumeration-seq (.entries zf))
                         (filter (fn [^ZipEntry e]
                                   (string/ends-with? (.getName e) ".csv")))
                         first)]
      (when-not csv-entry
        (throw (ex-info (str "no .csv entry in " zip-path)
                        {:zip-path zip-path})))
      (let [bytes (with-open [in (.getInputStream zf csv-entry)]
                    (.readAllBytes in))
            mtime (.getLastModifiedTime ^ZipEntry csv-entry)
            retrieved-at (when mtime (str (.toInstant mtime)))]
        {:csv (String. bytes "UTF-8")
         :provenance {"source_url" source-url
                      "retrieved_at" retrieved-at
                      "original_file_hash"
                      (hash/format-sha256 (hash/sha256-bytes bytes))}}))))

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
  upstream parser ever emits extra columns.

  When `provenance-base` is non-nil, attach `source_csv_provenance`
  with parser-emitted `corrections` merged in as `parse_corrections`
  (omitted when no corrections). When nil (programmatic / synthetic
  callers), emit no provenance — corrections are then dropped, per
  ADR 0015's two-mode contract."
  [body corrections provenance-base]
  (let [base (merge (zipmap person-body-keys (map #(get body %) person-body-keys))
                    {"person_record_schema_id" person-schema-id
                     "person_record_schema_hash" (am/cached-schema-hash person-schema-path)
                     "external_links" (or (get body "external_links") [])})]
    (cond-> base
      provenance-base
      (assoc "source_csv_provenance"
             (cond-> provenance-base
               (seq corrections) (assoc "parse_corrections" (vec corrections)))))))

(defn- write-person-file!
  "Write the person record at <persons-dir>/<person_id>.json with the
  corruption-safe idempotent check. The freshly built record is
  schema-validated before any hashing/writing — a bad shape (e.g. a
  date that fell through parse-date as raw passthrough) fails loudly
  here rather than being canonicalized and only caught later by SHACL.
  If the file already exists:
  - parse it as JSON (fail loudly if unparseable)
  - validate against the schema (fail loudly if invalid)
  - recompute its hash; if same as the just-built record, no-op rewrite
  - if different and overwrite? is false, throw
  - if different and overwrite? is true, replace
  Returns the just-built record's person_record_hash."
  [persons-dir record overwrite?]
  (person-record/validate! record)
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
  (let [errors (schema/validation-errors (am/cached-schema schema-path) record)]
    (when (seq errors)
      (throw (ex-info "generated metadata-record fails schema validation"
                      {:errors errors}))))
  (let [embedded (get record "metadata_record_schema_hash")
        live (am/cached-schema-hash schema-path)]
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
              metadata_record_hash and recomputes artifact_id),
            :source-csv-provenance (a map with at least
              `original_file_hash`; if supplied, each person record
              gets a `source_csv_provenance` block carrying it plus
              parser-emitted `parse_corrections`. Omit when the rows
              are synthetic or the source has no recoverable identity.)"
  [{:keys [rows work-id output persons-output-dir overwrite refresh-manifest
           source-csv-provenance]}]
  (let [matching (filter #(= work-id (get % "作品ID")) rows)]
    (when-not (seq matching)
      (throw (ex-info (str "no rows for work_id " work-id " in supplied rows")
                      {:work-id work-id})))
    (let [{:keys [work persons-by-id contributors corrections-by-pid]}
          (ac/build-record-fragment-from-rows matching)
          persons-dir (or persons-output-dir
                          (str (.getParent (io/file output)) "/persons"))
          _ (doseq [[pid corrs] (sort-by key corrections-by-pid)
                    c corrs]
              (tel/log! :info
                        (str "parse-correction person=" pid
                             " field=" (get c "field")
                             " rule=" (get c "rule")
                             " raw=" (pr-str (get c "raw"))
                             " corrected=" (pr-str (get c "corrected")))))
          contributor-entries
          (vec
           (for [c contributors
                 :let [pid (get c "person_id")
                       body (get persons-by-id pid)
                       corrs (get corrections-by-pid pid)
                       record (build-person-record body corrs source-csv-provenance)
                       new-hash (write-person-file! persons-dir record overwrite)]]
             {"person_id" pid
              "person_record_hash" new-hash
              "relation_to_work" (get c "relation_to_work")}))
          metadata-rec {"metadata_record_schema_id" schema-id
                        "metadata_record_schema_hash" (am/cached-schema-hash schema-path)
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

(defn run-corpus!
  "Two-stage corpus ingest. Groups rows by work_id and writes:
    - <output-dir>/works/<work_id>.json   (one metadata-record per work)
    - <output-dir>/persons/<person_id>.json (deduplicated across works)

  Returns {:works-written N :persons-written M
           :works-skipped K :skipped-work-ids [...]}.

  Cross-work person dedup is handled by the corruption-safe
  write-person-file! call: identical bodies produce identical hashes
  and are no-op rewrites; divergent bodies throw unless :overwrite is
  true. Within a single work, build-record-fragment-from-rows already
  enforces consistency.

  Tolerance: per-record `person-record/validate!` failures inside
  `run-from-rows!` are caught here per work, logged with the offending
  work_id and reason, and counted in :works-skipped. The single-work
  CLI path keeps the legacy fail-loud behavior (run-from-rows! still
  throws); only the corpus path absorbs the failure so a handful of
  out-of-grammar dates cannot abort an end-to-end run. ADR 0015.

  Required keys: :rows, :output-dir.
  Optional: :overwrite (boolean, default false),
            :source-csv-provenance (forwarded to run-from-rows!)."
  [{:keys [rows output-dir overwrite source-csv-provenance]}]
  (let [works-dir (io/file output-dir "works")
        persons-dir (io/file output-dir "persons")
        rows-by-work (group-by #(get % "作品ID") rows)
        person-ids (atom #{})
        skipped (atom [])]
    (.mkdirs works-dir)
    (.mkdirs persons-dir)
    (doseq [[work-id work-rows] (sort-by key rows-by-work)]
      (try
        (run-from-rows!
         {:rows work-rows
          :work-id work-id
          :output (str (io/file works-dir (str work-id ".json")))
          :persons-output-dir (str persons-dir)
          :overwrite (boolean overwrite)
          :source-csv-provenance source-csv-provenance})
        (doseq [r work-rows]
          (swap! person-ids conj (get r "人物ID")))
        (catch clojure.lang.ExceptionInfo e
          (swap! skipped conj work-id)
          (let [{:keys [errors-humanized field value]} (ex-data e)
                hint (or (some-> errors-humanized first)
                         (when (and field value)
                           (str field "=" (pr-str value)))
                         "no detail")]
            (tel/log! :warn
                      (str "skipped work " work-id ": "
                           (.getMessage e) " — " hint))))))
    {:works-written (- (count rows-by-work) (count @skipped))
     :persons-written (count @person-ids)
     :works-skipped (count @skipped)
     :skipped-work-ids @skipped}))

(defn run!
  "CLI-shaped entry: reads the CSV from a ZIP path and delegates to
  run-from-rows!. Same return value."
  [{:keys [zip-path source-url] :as opts}]
  (let [{:keys [csv provenance]} (read-zip-csv zip-path source-url)
        rows (ac/read-rows-from-string csv)]
    (run-from-rows! (-> opts
                        (dissoc :zip-path :source-url)
                        (assoc :rows rows
                               :source-csv-provenance provenance)))))

(defn run-corpus-from-zip!
  "CLI-shaped entry: reads the CSV from a ZIP path and delegates to
  run-corpus!."
  [{:keys [zip-path source-url] :as opts}]
  (let [{:keys [csv provenance]} (read-zip-csv zip-path source-url)
        rows (ac/read-rows-from-string csv)]
    (run-corpus! (-> opts
                     (dissoc :zip-path :source-url)
                     (assoc :rows rows
                            :source-csv-provenance provenance)))))

(def cli-options
  [["-z" "--zip ZIP" "Path to an Aozora list_person_all_extended ZIP."
    :id :zip-path]
   ["-w" "--work-id WORK_ID" "Aozora work ID (6-digit zero-padded string)."
    :id :work-id]
   ["-o" "--output FILE" "Output JSON path (work metadata-record). Single-work mode."
    :id :output]
   [nil "--all"
    "Corpus mode: ingest every work in the CSV. Requires --output-dir."
    :id :all? :default false]
   [nil "--output-dir DIR"
    "Corpus-mode output root (writes <DIR>/works/ and <DIR>/persons/)."
    :id :output-dir]
   [nil "--persons-output-dir DIR"
    "Single-work mode: directory for per-person JSON files (default: <output-dir>/persons)."
    :id :persons-output-dir]
   [nil "--overwrite" "Overwrite an existing on-disk person file whose hash differs."
    :id :overwrite :default false]
   [nil "--refresh-manifest FILE"
    "Rewrite the named manifest.json's metadata_record_hash + artifact_id in place."
    :id :refresh-manifest]
   [nil "--source-url URL"
    "Canonical upstream URL of the CSV (recorded in source_csv_provenance.source_url; defaults to null when omitted)."
    :id :source-url]])

(defn usage []
  (tel/log! :warn (str "Usage:\n"
                       "  Single-work mode: clojure -M:abc/aozora-ingest "
                       "--zip <path-to-zip> --work-id NNNNNN --output <path>"
                       " [--persons-output-dir DIR] [--overwrite]"
                       " [--refresh-manifest manifest.json]\n"
                       "  Corpus mode:      clojure -M:abc/aozora-ingest "
                       "--zip <path-to-zip> --all --output-dir <DIR> [--overwrite]")))

(defn -main [& args]
  (logging/install-cli-handler!)
  (let [{:keys [options errors]} (cli/parse-opts args cli-options)
        {:keys [zip-path all? output-dir work-id output]} options
        corpus? (boolean all?)
        invalid? (or (seq errors)
                     (nil? zip-path)
                     (if corpus?
                       (nil? output-dir)
                       (or (nil? work-id) (nil? output))))]
    (if invalid?
      (do
        (doseq [e errors] (tel/log! :error e))
        (usage)
        (System/exit 2))
      (if corpus?
        (let [{:keys [works-written persons-written works-skipped]}
              (run-corpus-from-zip! options)]
          (tel/log! :info (str "wrote " works-written " works and "
                               persons-written " persons under " output-dir
                               (when (pos? works-skipped)
                                 (str "; skipped " works-skipped
                                      " works (see warnings)")))))
        (do
          (run! options)
          (tel/log! :info (str "wrote metadata-record to " output)))))))
