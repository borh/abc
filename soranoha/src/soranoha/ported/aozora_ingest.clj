(ns soranoha.ported.aozora-ingest
  "CLI: build a metadata-record JSON + N person-record JSON files from
  an Aozora list_person_all_extended ZIP slice for a given work-id.
  Validates the result against the schema and asserts schema-hash
  self-consistency before writing. Optionally refreshes a manifest
  ."
  (:refer-clojure :exclude [run!])
  (:require [soranoha.ported.aozora-csv :as ac]
            [soranoha.ported.files :as files]
            [soranoha.ported.hash :as hash]
            [soranoha.ported.json :as json]
            [soranoha.ported.logging :as logging]
            [soranoha.ported.metadata-record :as metadata-record]
            [soranoha.ported.person-record :as person-record]
            [soranoha.ported.schema :as schema]
            [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.tools.cli :as cli])
  (:import [java.util.zip ZipFile ZipEntry]))

(def schema-path "schemas/metadata-record.schema.json")
(def schema-id "https://w3id.org/abc/schemas/metadata-record.schema.json")
(def person-schema-path "schemas/person-record.schema.json")
(def person-schema-id "https://w3id.org/abc/schemas/person-record.schema.json")

(defn- open-zip
  "Open zip-path as a ZipFile, wrapping the unreadable-archive case as
  ex-info {:zip-path} with the ZipException chained (failure taxonomy:
  ZIP source validation)."
  ^ZipFile [^String zip-path]
  (try
    (ZipFile. (io/file zip-path))
    (catch java.util.zip.ZipException e
      (throw (ex-info (str zip-path " is not a readable ZIP archive")
                      {:zip-path zip-path}
                      e)))))

(defn- read-zip-csv
  "Returns {:csv, :provenance}. :provenance carries
  source_url (caller-supplied or nil), retrieved_at (the CSV entry's
  stored mtime as ISO instant when available, else nil), and
  original_file_hash (sha256 of the CSV bytes, always computable).
  This is the only fact-set the ingester knows about the source."
  [^String zip-path source-url]
  (with-open [zf (open-zip zip-path)]
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
                     "person_record_schema_hash" (schema/cached-schema-hash person-schema-path)
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
    (files/create-dirs! persons-dir)
    (if (files/exists? target)
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
  (let [errors (schema/validation-errors (schema/cached-schema schema-path) record)]
    (when (seq errors)
      (throw (ex-info "generated metadata-record fails schema validation"
                      {:errors errors}))))
  (let [embedded (get record "metadata_record_schema_hash")
        live (schema/cached-schema-hash schema-path)]
    (when-not (= embedded live)
      (throw (ex-info "self-consistency: embedded schema hash differs from live"
                      {:embedded embedded :live live}))))
  :ok)

(defn- build-work-plan
  "Pure build phase for one work: parse the fragment, build and
  schema-validate every contributor person record, assemble the
  metadata-record, and self-consistency-check it. NO filesystem writes —
  any row/work fault throws ex-info here, before a single byte lands on
  disk. Returns {:work-id :person-records {pid record} :metadata-rec}."
  [{:keys [rows work-id source-csv-provenance]}]
  (let [matching (filter #(= work-id (get % "作品ID")) rows)]
    (when-not (seq matching)
      (throw (ex-info (str "no rows for work_id " work-id " in supplied rows")
                      {:work-id work-id})))
    (let [{:keys [work persons-by-id contributors corrections-by-pid]}
          (ac/build-record-fragment-from-rows matching)
          _ (doseq [[pid corrs] (sort-by key corrections-by-pid)
                    c corrs]
              (logging/log! :debug
                            (str "parse-correction person=" pid
                                 " field=" (get c "field")
                                 " rule=" (get c "rule")
                                 " raw=" (pr-str (get c "raw"))
                                 " corrected=" (pr-str (get c "corrected")))))
          person-records
          (into (sorted-map)
                (map (fn [[pid body]]
                       (let [record (build-person-record
                                     body (get corrections-by-pid pid)
                                     source-csv-provenance)]
                         (person-record/validate! record)
                         [pid record]))
                     persons-by-id))
          contributor-entries
          (vec (for [c contributors
                     :let [pid (get c "person_id")]]
                 {"person_id" pid
                  "person_record_hash" (person-record/record-hash
                                        (get person-records pid))
                  "relation_to_work" (get c "relation_to_work")}))
          metadata-rec {"metadata_record_schema_id" schema-id
                        "metadata_record_schema_hash" (schema/cached-schema-hash schema-path)
                        "work" work
                        "contributors" (vec (sort-by #(get % "person_id")
                                                     contributor-entries))}]
      (self-consistency-check! metadata-rec)
      {:work-id work-id
       :person-records person-records
       :metadata-rec metadata-rec})))

(defn run-from-rows!
  "Programmatic entry that operates on already-parsed CSV rows. Writes
  N+1 deterministic JSON files (the work metadata-record + each
  contributor person record) and returns the new metadata_record_hash.
  All validation happens before the first write; a validation failure
  leaves no files behind.

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
  (let [{:keys [person-records metadata-rec]}
        (build-work-plan {:rows rows :work-id work-id
                          :source-csv-provenance source-csv-provenance})
        persons-dir (or persons-output-dir
                        (str (fs/parent output) "/persons"))]
    (doseq [[_pid record] person-records]
      (write-person-file! persons-dir record (boolean overwrite)))
    (files/create-dirs! (fs/parent output))
    (json/write-deterministic-json-file! (io/file output) metadata-rec)
    (let [new-hash (metadata-record/record-hash metadata-rec)]
      (logging/log! :debug (str "metadata_record_hash: " new-hash))
      (when refresh-manifest
        (let [m (files/read-json refresh-manifest)
              m' (assoc-in m ["manifest_identity_object" "metadata_record_hash"] new-hash)
              identity-obj (get m' "manifest_identity_object")
              artifact-id (hash/format-sha256 (hash/sha256-json-jcs identity-obj))
              m'' (assoc m' "artifact_id" artifact-id)]
          (json/write-deterministic-json-file! (io/file refresh-manifest) m'')
          (logging/log! :info (str "refreshed manifest " refresh-manifest))))
      new-hash)))

(defn- append-carrier [carriers carrier]
  (conj (or carriers []) carrier))

(defn run-corpus!
  "Two-stage corpus ingest. Groups rows by work_id and writes:
    - <output-dir>/works/<work_id>.json   (one metadata-record per work)
    - <output-dir>/persons/<person_id>.json (deduplicated across works)

  Returns {:works-written N :persons-written M
           :works-skipped K :skipped-work-ids [...]
           :person-conflicts [...]}.

  Corpus-level policy: every work is fully built and schema-validated
  (via build-work-plan) before any writes happen, so a skipped work
  leaves no new or modified files behind. When surviving works carry
  divergent bodies for the same person_id, the body from the smallest
  work_id wins deterministically; every affected work is still written,
  with its contributor entries referencing the winning record's hash.
  Each such conflict is warn-logged and reported in :person-conflicts as
  {\"person_id\" pid \"chosen_work_id\" wid \"work_ids\" [wids…]}.
  Pre-existing on-disk divergence without :overwrite still fails loudly
  via write-person-file! — a cross-RUN conflict is an environment fault,
  not a row fault, and is not caught here.

  Tolerance: per-work faults (fragment guards, `person-record/validate!`
  per contributor, `self-consistency-check!`) are caught here per work,
  logged with the offending work_id and reason, and counted in
  :works-skipped. The single-work CLI path keeps the legacy fail-loud
  behavior (run-from-rows! still throws); only the corpus path absorbs
  the failure so a handful of out-of-grammar dates cannot abort an
  end-to-end run. ADR 0015.

  Required keys: :rows, :output-dir.
  Optional: :overwrite (boolean, default false),
            :source-csv-provenance (forwarded to build-work-plan)."
  [{:keys [rows output-dir overwrite source-csv-provenance]}]
  (let [works-dir (io/file output-dir "works")
        persons-dir (io/file output-dir "persons")
        rows-by-work (group-by #(get % "作品ID") rows)
        {:keys [plans skipped]}
        (reduce
         (fn [acc [work-id work-rows]]
           (try
             (update acc :plans conj
                     (build-work-plan {:rows work-rows
                                       :work-id work-id
                                       :source-csv-provenance source-csv-provenance}))
             (catch clojure.lang.ExceptionInfo e
               (let [{:keys [errors-humanized field value]} (ex-data e)
                     hint (or (some-> errors-humanized first)
                              (when (and field value)
                                (str field "=" (pr-str value)))
                              "no detail")]
                 (logging/log! :warn
                               (str "skipped work " work-id ": "
                                    (.getMessage e) " — " hint)))
               (update acc :skipped conj work-id))))
         {:plans [] :skipped []}
         (sort-by key rows-by-work))
        ;; corpus-level shared-person reconciliation: records arrive sorted by
        ;; work id, so the FIRST carrier of a pid is the smallest work id.
        carriers-by-pid
        (reduce (fn [m {:keys [work-id person-records]}]
                  (reduce-kv (fn [m pid record]
                               (update m pid append-carrier
                                       {:work-id work-id :record record}))
                             m person-records))
                (sorted-map)
                plans)
        resolutions
        (mapv (fn [[pid carriers]]
                (let [winner (first carriers)
                      hashes (distinct (map #(person-record/record-hash (:record %))
                                            carriers))]
                  {:pid pid
                   :record (:record winner)
                   :hash (first hashes)
                   :conflict (when (< 1 (count hashes))
                               {"person_id" pid
                                "chosen_work_id" (:work-id winner)
                                "work_ids" (vec (distinct (map :work-id carriers)))})}))
              carriers-by-pid)
        chosen-hash (into {} (map (fn [resolution]
                                    [(:pid resolution) (:hash resolution)])
                                  resolutions))
        conflicts (vec (keep :conflict resolutions))]
    (doseq [c conflicts]
      (logging/log! :warn
                    (str "person " (get c "person_id")
                         " has divergent bodies across works "
                         (get c "work_ids")
                         "; keeping the body from work " (get c "chosen_work_id"))))
    (files/create-dirs! works-dir)
    (files/create-dirs! persons-dir)
    (doseq [{:keys [record]} resolutions]
      (write-person-file! persons-dir record (boolean overwrite)))
    (doseq [{:keys [work-id metadata-rec]} plans
            :let [rec (update metadata-rec "contributors"
                              (fn [cs]
                                (mapv #(assoc % "person_record_hash"
                                              (get chosen-hash (get % "person_id")))
                                      cs)))]]
      (json/write-deterministic-json-file!
       (io/file works-dir (str work-id ".json")) rec))
    {:works-written (count plans)
     :persons-written (count resolutions)
     :works-skipped (count skipped)
     :skipped-work-ids skipped
     :person-conflicts conflicts}))

(defn- rows-from-zip
  "Read and parse the CSV entry at zip-path. Fails loudly when the entry
  has no data rows (failure taxonomy: corpus source validation) —
  a silent zero-row corpus must never look like a successful run."
  [zip-path source-url]
  (let [{:keys [csv provenance]} (read-zip-csv zip-path source-url)
        rows (ac/read-rows-from-string csv)]
    (when (empty? rows)
      (throw (ex-info (str "CSV entry in " zip-path " has no data rows")
                      {:zip-path zip-path :row-count (count rows)})))
    {:rows rows :provenance provenance}))

(defn run!
  "CLI-shaped entry: reads the CSV from a ZIP path and delegates to
  run-from-rows!. Same return value."
  [{:keys [zip-path source-url] :as opts}]
  (let [{:keys [rows provenance]} (rows-from-zip zip-path source-url)]
    (run-from-rows! (-> opts
                        (dissoc :zip-path :source-url)
                        (assoc :rows rows
                               :source-csv-provenance provenance)))))

(defn run-corpus-from-zip!
  "CLI-shaped entry: reads the CSV from a ZIP path and delegates to
  run-corpus!."
  [{:keys [zip-path source-url] :as opts}]
  (let [{:keys [rows provenance]} (rows-from-zip zip-path source-url)]
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
  (logging/log! :warn (str "Usage:\n"
                           "  Single-work mode: clojure -M:abc/aozora-ingest "
                           "--zip <path-to-zip> --work-id NNNNNN --output <path>"
                           " [--persons-output-dir DIR] [--overwrite]"
                           " [--refresh-manifest manifest.json]\n"
                           "  Corpus mode:      clojure -M:abc/aozora-ingest "
                           "--zip <path-to-zip> --all --output-dir <DIR> [--overwrite]")))

(defn merge-catalog-defaults
  "Pure: fill `:zip-path`/`:source-url` from catalog fallbacks (the env values
  the `aozora-ingest` flake app injects) only when the CLI omitted them. An
  explicit CLI value always wins; a blank/absent fallback is treated as nil."
  [options {:keys [zip source-url]}]
  (let [present (fn [v] (when-not (string/blank? v) v))]
    (cond-> options
      (nil? (:zip-path options)) (assoc :zip-path (present zip))
      (nil? (:source-url options)) (assoc :source-url (present source-url)))))

(defn- with-catalog-env-defaults
  "Fill --zip / --source-url from the canonical-catalog env vars set by the
  `aozora-ingest` flake app when the flags are omitted."
  [options]
  (merge-catalog-defaults options
                          {:zip (System/getenv "ABC_AOZORA_CATALOG_ZIP")
                           :source-url (System/getenv "ABC_AOZORA_CATALOG_URL")}))

(defn -main [& args]
  (logging/install-cli-handler!)
  (let [{:keys [options errors]} (cli/parse-opts args cli-options)
        options (with-catalog-env-defaults options)
        {:keys [zip-path all? output-dir work-id output]} options
        corpus? (boolean all?)
        invalid? (or (seq errors)
                     (nil? zip-path)
                     (if corpus?
                       (nil? output-dir)
                       (or (nil? work-id) (nil? output))))]
    (if invalid?
      (do
        (doseq [e errors] (logging/log! :error e))
        (usage)
        (System/exit 2))
      (if corpus?
        (let [{:keys [works-written persons-written works-skipped]}
              (run-corpus-from-zip! options)]
          (logging/log! :info (str "wrote " works-written " works and "
                                   persons-written " persons under " output-dir
                                   (when (pos? works-skipped)
                                     (str "; skipped " works-skipped
                                          " works (see warnings)")))))
        (do
          (run! options)
          (logging/log! :info (str "wrote metadata-record to " output)))))))
