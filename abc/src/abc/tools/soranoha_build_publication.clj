(ns abc.tools.soranoha-build-publication
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.aozora-csv :as aozora-csv]
            [abc.tools.aozora-ingest :as aozora-ingest]
            [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.json :as abc-json]
            [abc.tools.manifest :as manifest]
            [abc.tools.materialize-publication :as materialize-publication]
            [abc.tools.parallel :as parallel]
            [abc.tools.publication-policy :as publication-policy]
            [abc.tools.schema :as schema]
            [abc.tools.source-bundle :as source-bundle]
            [abc.tools.workflow :as workflow]
            [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [java.nio.file Files StandardCopyOption]
           [java.util.zip ZipEntry ZipFile]))

(def config-schema-path
  "schemas/soranoha-publication-build-config.schema.json")

(defn- normalized-path [file]
  (string/replace (str file) "\\" "/"))

(defn- zip-file? [file]
  (and (files/file? file)
       (string/ends-with? (str (fs/file-name file)) ".zip")))

(defn- normalized-abs-path
  "Absolute, `.`/`..`-normalized path that does NOT resolve symlinks — unlike
  getCanonicalFile. Keeps files under a symlinked root (e.g. the zero-copy
  aozorabunko-corpus symlinkJoin) instead of escaping to the symlink targets."
  [f]
  (fs/normalize (fs/absolutize f)))

(defn- aozora-work-zip? [root file]
  (let [rel (normalized-path
             (fs/relativize (normalized-abs-path root)
                            (normalized-abs-path file)))]
    (when (re-matches #"^cards/[0-9]{6}/files/[^/]+\.zip$" rel)
      rel)))

(defn- read-catalog-zip [aozora-root]
  (let [zip-file (io/file aozora-root "index_pages"
                          "list_person_all_extended_utf8.zip")]
    (when-not (files/file? zip-file)
      (throw (ex-info "official catalog ZIP is missing"
                      {:path (str zip-file)})))
    (with-open [zf (ZipFile. zip-file)]
      (let [entry (->> (enumeration-seq (.entries zf))
                       (filter (fn [^ZipEntry e]
                                 (string/ends-with? (.getName e) ".csv")))
                       first)]
        (when-not entry
          (throw (ex-info "official catalog ZIP contains no CSV entry"
                          {:path (str zip-file)})))
        (let [bytes (with-open [in (.getInputStream zf entry)]
                      (.readAllBytes in))]
          {:catalog-zip zip-file
           :csv-entry (.getName entry)
           :csv-bytes bytes
           :csv-text (String. bytes "UTF-8")
           :catalog-csv-hash (hash/format-sha256
                              (hash/sha256-bytes bytes))})))))

(defn- text-url-basename [row]
  (some-> (get row "テキストファイルURL")
          string/trim
          (string/split #"/")
          last))

(defn- row-work-id [row]
  (get row "作品ID"))

(defn- row-person-id [row]
  (get row "人物ID"))

(defn- catalog-index [rows]
  (reduce (fn [idx row]
            (if-let [basename (text-url-basename row)]
              (assoc idx basename row)
              idx))
          {}
          rows))

(defn- work-zip-files [aozora-root]
  (->> (files/sorted-path-seq aozora-root)
       (map fs/file)
       (filter zip-file?)
       (map (fn [file]
              {:file file
               :relpath (aozora-work-zip? aozora-root file)}))
       vec))

(defn- slug [work-id person-id relpath]
  (let [basename (.getName (io/file relpath))
        stem (subs basename 0 (- (count basename) (count ".zip")))]
    (str work-id "_" person-id "_" stem)))

;; ── Real source→parser-IR derivation via the owned, Nix-built adapters ──
;; The adapter/converter binaries are provided by the flake through env vars
;; (mirroring the existing AB_AAT_TO_PARSER_IR_BIN wiring used by
;; annotation-join-stats-run). Keeping them as an injected boundary keeps
;; build-publication hermetic and lets parser_profile select the adapter.

(defn- env-value [k]
  (let [v (System/getenv k)]
    (when-not (string/blank? v) v)))

(defn- require-env [k what]
  (or (env-value k)
      (throw (ex-info (str what " unavailable; set " k)
                      {:env_var k}))))

(defn- resolve-adapter
  "Resolve the source→AAT adapter for the configured parser_profile. Only
  aozora2html is wired today; any other profile is an explicit, loud error so a
  build never silently falls back to a stub."
  [parser-profile]
  (case parser-profile
    ("aozora2html" "aozora2html-v1" "aozora2html-smoke-v1")
    {:adapter-id "aozora2html"
     :wrapper (require-env "AB_AOZORA2HTML_ADAPTER" "aozora2html adapter wrapper")
     :extra-env {"AB_AOZORA2HTML_BIN"
                 (require-env "AB_AOZORA2HTML_BIN" "aozora2html parser")
                 "AB_AOZORA2HTML_MAPPER_BIN"
                 (require-env "AB_AOZORA2HTML_MAPPER_BIN" "aozora2html rust mapper")}}
    (throw (ex-info "unsupported parser_profile for real materialization"
                    {:parser_profile parser-profile
                     :supported ["aozora2html"]}))))

(defn- run-process!
  "Run a subprocess inheriting the current environment plus extra-env, feeding
  stdin-bytes, returning {:exit :out-bytes :err}."
  [{:keys [args stdin-bytes extra-env]}]
  (let [{:keys [exit out err]}
        @(process/process args
                          {:in stdin-bytes
                           :out :bytes
                           :err :string
                           :extra-env extra-env})]
    {:exit exit :out-bytes out :err err}))

(defn- write-aat!
  "Run the aozora2html adapter wrapper (parse + align) over the raw source
  bytes, writing the AAT JSON to aat-file."
  [aat-file {:keys [adapter source-bytes]}]
  (let [{:keys [wrapper extra-env]} adapter
        {:keys [exit out-bytes err]}
        (run-process! {:args [wrapper "--mode" "aat"]
                       :stdin-bytes source-bytes
                       :extra-env extra-env})]
    (when-not (zero? exit)
      (throw (ex-info "aozora2html adapter failed" {:exit exit :stderr err})))
    (io/make-parents aat-file)
    (with-open [os (io/output-stream aat-file)]
      (.write os ^bytes out-bytes))
    aat-file))

(defn- convert-aat->parser-ir!
  "Run ab-aat-to-parser-ir convert, emitting parser-IR + divergence sidecar."
  [{:keys [aat-file parser-ir-file divergence-file work-content-hash]}]
  (let [convert-bin (require-env "AB_AAT_TO_PARSER_IR_BIN" "ab-aat-to-parser-ir")
        mapping (require-env "AB_AAT_TO_PARSER_IR_MAPPING"
                             "aat→parser-IR mapping document")
        {:keys [exit err]}
        (run-process! {:args [convert-bin "convert"
                              "--aat" aat-file
                              "--mapping" mapping
                              "--work-content-hash" work-content-hash
                              "--parser-ir-out" parser-ir-file
                              "--divergence-out" divergence-file]})]
    (when-not (zero? exit)
      (throw (ex-info "ab-aat-to-parser-ir convert failed"
                      {:exit exit :stderr err})))
    parser-ir-file))

(defn- real-derive-parser-ir!
  "Production source→parser-IR: resolve the adapter for the profile, run the
  aozora2html adapter to AAT, then ab-aat-to-parser-ir convert. Adapter
  resolution is lazy here so the injectable boundary below can be stubbed
  without the adapter binaries present."
  [{:keys [parser-profile source-bytes work-content-hash aat-file parser-ir-file
           divergence-file]}]
  (let [adapter (resolve-adapter parser-profile)]
    (write-aat! aat-file {:adapter adapter :source-bytes source-bytes})
    (convert-aat->parser-ir! {:aat-file (str aat-file)
                              :work-content-hash work-content-hash
                              :parser-ir-file (str parser-ir-file)
                              :divergence-file (str divergence-file)})))

(def ^{:dynamic true
       :doc "Injectable source→parser-IR boundary. Bound to a stub in tests so
             the workflow can be exercised without the adapter binaries."}
  *derive-parser-ir!* real-derive-parser-ir!)

(defn invoke-derive-parser-ir! [options]
  (*derive-parser-ir!* options))

(defn- corpus-snapshot-hash
  "Content-addressed identity of this build's source snapshot: the pinned
  catalog + git commit + snapshot date. Shared by every work in the build so
  publication manifests share one corpus_snapshot_hash."
  [{:keys [snapshot-date catalog-csv-hash aozora-git-commit]}]
  (analysis-identity/hash-json-value
   {"kind" "soranoha-build-publication-source-snapshot-v1"
    "snapshot_date" snapshot-date
    "catalog_csv_hash" catalog-csv-hash
    "aozora_git_commit" aozora-git-commit}))

(defn- work-source-manifest [work-hash corpus-hash]
  {"manifest_schema_id" "https://w3id.org/abc/schemas/manifest.schema.json"
   "artifact_kind" "source"
   "manifest_identity_object" {"corpus_snapshot_hash" corpus-hash
                               "work_content_hash" work-hash}
   "notes" (str "Source manifest emitted by soranoha build-publication real "
                "materialization.")})

(defn- official-source
  [row relpath source-file {:keys [archive-hash bundle-hash
                                   primary-text-member primary-text-hash]}]
  {"work_id" (row-work-id row)
   "card_person_id" (row-person-id row)
   "text_url" (get row "テキストファイルURL")
   "text_zip_relpath" relpath
   "zip_member" primary-text-member
   "source_hash" archive-hash
   "archive_hash" archive-hash
   "bundle_hash" bundle-hash
   "primary_text_member" primary-text-member
   "primary_text_hash" primary-text-hash
   "source_bytes" (hash/byte-length source-file)})

(defn- assert-parser-identities!
  [parser-ir-file expected-work-hash expected-primary-hash]
  (let [parser-ir (abc-json/read-json-file parser-ir-file)
        actual-work-hash (get-in parser-ir ["source" "work_content_hash"])
        actual-primary-hash (get-in parser-ir ["source" "primary_text_hash"])]
    (when-not (and (= expected-work-hash actual-work-hash)
                   (= expected-primary-hash actual-primary-hash))
      (throw (ex-info "parser-IR source identity does not match inspected bundle"
                      {:expected-work-content-hash expected-work-hash
                       :actual-work-content-hash actual-work-hash
                       :expected-primary-text-hash expected-primary-hash
                       :actual-primary-text-hash actual-primary-hash})))
    parser-ir))

(defn- write-materialized-work!
  [{:keys [rows catalog-provenance materialized-root selected parser-profile
           corpus-hash]}]
  (let [{:keys [row file relpath]} selected
        work-id (row-work-id row)
        person-id (row-person-id row)
        inspection (source-bundle/inspect-zip file)
        work-hash (:bundle-hash inspection)
        archive-hash (:archive-hash inspection)
        primary-text-member (:primary-text-member inspection)
        primary-text-hash (:primary-text-hash inspection)
        source-bytes (:primary-text-bytes inspection)
        work-dir (io/file materialized-root "works"
                          (slug work-id person-id relpath))
        aat-file (io/file work-dir "aat.json")
        parser-ir-file (io/file work-dir "parser-ir.json")
        divergence-file (io/file work-dir "divergence.json")
        source-bundle-file (io/file work-dir "source-bundle.json")
        source-manifest-file (io/file work-dir "source.manifest.json")
        persons-dir (io/file materialized-root "persons")
        metadata-file (io/file work-dir "metadata-record.json")]
    (files/create-dirs! work-dir)
    ;; Real AAT + parser-IR from the owned adapters (replaces the former stub),
    ;; through the injectable boundary so tests can stub it.
    (source-bundle/write-manifest! source-bundle-file inspection)
    (invoke-derive-parser-ir! {:parser-profile parser-profile
                               :source-bytes source-bytes
                               :work-content-hash work-hash
                               :aat-file aat-file
                               :parser-ir-file parser-ir-file
                               :divergence-file divergence-file})
    (assert-parser-identities! parser-ir-file work-hash primary-text-hash)
    ;; Source truth + the source manifest publication materialization requires.
    (abc-json/write-deterministic-json-file!
     (io/file work-dir "official-source.json")
     (official-source row relpath file inspection))
    (abc-json/write-deterministic-json-file!
     source-manifest-file
     (work-source-manifest work-hash corpus-hash))
    (files/write-text! (io/file work-dir "warnings.jsonl") "")
    (aozora-ingest/run-from-rows!
     {:rows rows
      :work-id work-id
      :output (str metadata-file)
      :persons-output-dir (str persons-dir)
      :overwrite true
      :source-csv-provenance catalog-provenance})
    {:work_id work-id
     :person_id person-id
     :slug (.getName work-dir)
     :text_zip_relpath relpath
     :source_hash archive-hash
     :archive_hash archive-hash
     :bundle_hash work-hash
     :work_content_hash work-hash
     :primary_text_member primary-text-member
     :primary_text_hash primary-text-hash
     :zip_member primary-text-member
     :source_bundle_path (str source-bundle-file)
     :aat_path (str aat-file)
     :parser_ir_path (str parser-ir-file)
     :source_manifest_path (str source-manifest-file)
     :metadata_record_path (str metadata-file)
     :persons_dir (str persons-dir)}))

(defn- selection-report [selected rejected]
  {"selected_source_count" (count selected)
   "rejected_source_count" (count rejected)
   "selected_sources" (mapv (fn [source]
                              {"work_id" (:work_id source)
                               "person_id" (:person_id source)
                               "slug" (:slug source)
                               "text_zip_relpath" (:text_zip_relpath source)
                               "source_hash" (:source_hash source)
                               "archive_hash" (:archive_hash source)
                               "bundle_hash" (:bundle_hash source)
                               "work_content_hash" (:work_content_hash source)
                               "primary_text_member" (:primary_text_member source)
                               "primary_text_hash" (:primary_text_hash source)
                               "zip_member" (:zip_member source)})
                            selected)
   "rejected_sources" (mapv identity rejected)})

(defn- source-bundle-admission-error [t]
  (loop [cause t
         seen #{}]
    (cond
      (nil? cause) nil
      (contains? seen cause) nil
      (not (instance? clojure.lang.ExceptionInfo cause)) nil
      (true? (::source-bundle/admission-error (ex-data cause)))
      cause
      :else (recur (.getCause ^Throwable cause) (conj seen cause)))))

(defn- derive-failure [candidate t]
  (let [d (ex-data t)
        actual (or (:actual d)
                   (:actual-bytes d)
                   (:declared-bytes d)
                   (:member-count d))]
    (cond-> {"work_id" (row-work-id (:row candidate))
             "person_id" (row-person-id (:row candidate))
             "text_zip_relpath" (:relpath candidate)
             "error" (.getMessage t)
             "reason" (some-> (:reason d) name)}
      (:archive-path d) (assoc "archive_path" (:archive-path d))
      (:path d) (assoc "path" (:path d))
      (:decoded-path d) (assoc "decoded_path" (:decoded-path d))
      (:normalized-path d) (assoc "normalized_path" (:normalized-path d))
      (:limit d) (assoc "limit" (:limit d))
      (some? actual) (assoc "actual" actual)
      (:actual-bytes d) (assoc "actual_bytes" (:actual-bytes d))
      (:declared-bytes d) (assoc "declared_bytes" (:declared-bytes d))
      (:member-count d) (assoc "member_count" (:member-count d))
      (:paths d) (assoc "paths" (:paths d))
      (:folded-path d) (assoc "folded_path" (:folded-path d))
      (:candidates d) (assoc "candidates" (:candidates d)))))

(defn derive-selected-candidate [[context candidate]]
  (if (:continue-on-failure context)
    (try
      {:ok (write-materialized-work! (assoc context :selected candidate))}
      (catch Throwable t
        (if-let [admission (source-bundle-admission-error t)]
          {:failed (derive-failure candidate admission)}
          (throw t))))
    {:ok (write-materialized-work! (assoc context :selected candidate))}))

(defn- materialize-selected-sources!
  [{:keys [aozora-root output-root parser-profile snapshot-date
           aozora-git-commit continue-on-failure concurrency]}]
  (let [{:keys [csv-text catalog-csv-hash]} (read-catalog-zip aozora-root)
        rows (aozora-csv/read-rows-from-string csv-text)
        rows-by-basename (catalog-index rows)
        materialized-root (io/file output-root "materialized-root")
        corpus-hash (corpus-snapshot-hash {:snapshot-date snapshot-date
                                           :catalog-csv-hash catalog-csv-hash
                                           :aozora-git-commit aozora-git-commit})
        catalog-provenance {"source_url" nil
                            "retrieved_at" nil
                            "original_file_hash" catalog-csv-hash}
        candidates (work-zip-files aozora-root)
        selected-candidates (->> candidates
                                 (keep (fn [{:keys [file relpath]}]
                                         (when relpath
                                           (when-let [row (get rows-by-basename
                                                               (.getName file))]
                                             {:file file
                                              :relpath relpath
                                              :row row}))))
                                 (sort-by :relpath)
                                 vec)
        derive-context {:rows rows
                        :catalog-provenance catalog-provenance
                        :materialized-root materialized-root
                        :parser-profile parser-profile
                        :corpus-hash corpus-hash
                        :continue-on-failure continue-on-failure}
        ;; A single corrupt/unreadable work ZIP (e.g. a zip Java's reader
        ;; rejects with "invalid CEN header") must not abort a whole-corpus
        ;; derive. With continue_on_failure, record and skip it; otherwise fail
        ;; loudly as before.
        results (parallel/ordered-pmap
                 concurrency
                 derive-selected-candidate
                 (mapv (fn [candidate] [derive-context candidate])
                       selected-candidates))
        selected (vec (keep :ok results))
        derive-failures (vec (keep :failed results))
        selected-relpaths (set (map :relpath selected-candidates))
        rejected (->> candidates
                      (remove #(contains? selected-relpaths (:relpath %)))
                      (mapv (fn [{:keys [file relpath]}]
                              {"path" (or relpath
                                          (normalized-path
                                           (fs/relativize
                                            (normalized-abs-path aozora-root)
                                            (normalized-abs-path file))))
                               "reason" (cond
                                          (nil? relpath)
                                          "not-under-cards-files"

                                          (not (contains? rows-by-basename
                                                          (.getName file)))
                                          "not-catalog-text-zip"

                                          :else
                                          "not-selected")})))]
    (when (and (empty? selected) (empty? derive-failures))
      (throw (ex-info "no catalog-backed work ZIPs were successfully derived"
                      {:aozora_root (str aozora-root)
                       :derive_failed_count (count derive-failures)})))
    (let [report (-> (selection-report selected rejected)
                     (assoc "derive_failed_count" (count derive-failures)
                            "derive_failures" derive-failures
                            "release_admissible" (empty? derive-failures)))]
      (abc-json/write-deterministic-json-file!
       (io/file output-root "source-selection-report.json")
       report)
      {:materialized-root materialized-root
       :corpus-snapshot-hash corpus-hash
       :report report
       :selected selected})))

(defn- read-config [path]
  (let [config-file (let [file (io/file path)]
                      (if (files/file? file)
                        file
                        (let [path-text (str path)]
                          (if (string/starts-with? path-text "abc/")
                            (io/file (subs path-text (count "abc/")))
                            file))))
        config (files/read-json config-file)
        config-schema (files/read-json config-schema-path)]
    (when-let [errors (schema/validation-errors config-schema config)]
      (throw (ex-info "publication build config schema validation failed"
                      {:path (str config-file)
                       :errors errors})))
    config))

(defn- git-sh [aozora-root & args]
  (try
    (let [{:keys [exit out]}
          (process/sh (into ["git" "-C" (str aozora-root)] args))]
      (when (zero? exit)
        (string/trim out)))
    (catch java.io.IOException _
      nil)))

(defn- git-provenance [aozora-root]
  (let [commit (or (git-sh aozora-root "rev-parse" "HEAD")
                   (let [head (io/file aozora-root ".git" "HEAD")]
                     (when (files/file? head)
                       (string/trim (files/read-text head)))))
        dirty-output (git-sh aozora-root "status" "--porcelain" "--"
                             "cards" "index_pages")]
    {"aozora_git_commit" commit
     "aozora_git_dirty" (if (nil? dirty-output)
                          false
                          (not (string/blank? dirty-output)))
     "dirty_scope" "cards index_pages"}))

(defn- build-plan [opts config materialization-result]
  {"build_schema_version" "soranoha-build-publication-v0"
   "config_hash" (analysis-identity/hash-json-value config)
   "config" config
   "aozora_root" (str (:aozora-root opts))
   "snapshot_date" (:snapshot-date opts)
   "git" (git-provenance (:aozora-root opts))
   "materialized_root" (str (:materialized-root materialization-result))
   "source_selection_report" "source-selection-report.json"
   "request_set_label" (get config "request_set_label")
   "snapshot_scope" (get config "snapshot_scope")
   "selected_source_count" (get-in materialization-result
                                   [:report "selected_source_count"])
   "concurrency" (:concurrency opts)})

(defn- resolve-invocation-path
  "Resolve a relative path arg against the caller's working directory. The app
  launcher cd's to the pinned source root before Clojure starts, so relative
  paths would otherwise resolve there (and fail); ABC_INVOCATION_PWD carries the
  original cwd. Absolute paths and the no-env case (tests, direct clojure -M)
  pass through unchanged."
  [path]
  (if (string/blank? path)
    path
    (let [file (io/file path)
          base (System/getenv "ABC_INVOCATION_PWD")]
      (if (or (.isAbsolute file) (string/blank? base))
        path
        (str (io/file base path))))))

(defn- resolve-concurrency
  "0 (or nil) means every available core; otherwise the requested count."
  [requested]
  (let [n (long (or requested 0))]
    (if (pos? n) n (.availableProcessors (Runtime/getRuntime)))))

(defn- parse-args [options]
  (reduce (fn [opts k] (update opts k resolve-invocation-path))
          options
          [:aozora-root :config :output-root]))

(defn- prepare-output-root! [output-root replace?]
  (let [output-root-file (io/file output-root)]
    (when (and (files/exists? output-root-file) (not replace?))
      (throw (ex-info "output-root already exists; pass --replace to replace it after a successful build"
                      {:output_root (str output-root-file)})))
    (files/create-dirs! (or (fs/parent output-root-file) (fs/path ".")))
    (io/file (str output-root ".tmp-" (System/nanoTime)))))

(defn- promote-output-root! [tmp-root output-root replace?]
  (let [target (io/file output-root)]
    (when (and replace? (files/exists? target))
      (files/delete-tree! target))
    (Files/move (.toPath (io/file tmp-root))
                (.toPath target)
                (into-array StandardCopyOption
                            [StandardCopyOption/ATOMIC_MOVE]))
    target))

(defn- generated-at-for [snapshot-date]
  (str snapshot-date "T00:00:00Z"))

(defn- publication-up-to-date?
  "Content-addressed skip: a work's publication is reusable when a prior run
  materialized it from the identical source (work_content_hash). Prevents
  recomputation and makes drift observable as a hash mismatch."
  [pub-dir work-hash]
  (let [marker (io/file pub-dir "source_work_content_hash.txt")]
    (and (files/file? (fs/file pub-dir "tei.manifest.json"))
         (files/file? marker)
         (= work-hash (string/trim (files/read-text marker))))))

(defn- copy-dir-files!
  "Copy the flat set of publication artifacts from one dir to another."
  [from to]
  (files/create-dirs! to)
  (doseq [f (files/list-files from)]
    (files/copy-file! (str f) (fs/file to (fs/file-name f))))
  (fs/file to))

(defn- relative-to-output-root
  "Path of file relative to output-root, so publications-report.json survives
  the tmp-root -> output-root promotion instead of pinning a since-renamed-away
  absolute path (also keeps the report byte-identical regardless of
  concurrency or the output-root's own absolute location)."
  [output-root file]
  (normalized-path (fs/relativize (normalized-abs-path output-root)
                                  (normalized-abs-path file))))

(defn- materialize-one-publication!
  [{:keys [output-root prior-output-root generated-at continue-on-failure work]}]
  (let [{:keys [slug work_content_hash parser_ir_path source_manifest_path
                metadata_record_path persons_dir]} work
        pub-dir (io/file output-root "publications" slug)
        prior-pub-dir (when prior-output-root
                        (io/file prior-output-root "publications" slug))]
    (cond
      ;; Already materialized in this output-root from identical source.
      (publication-up-to-date? pub-dir work_content_hash)
      {:slug slug :status "skipped"
       :tei_manifest (relative-to-output-root output-root
                                              (io/file pub-dir "tei.manifest.json"))}

      ;; A prior promoted build holds a byte-identical-source publication —
      ;; copy it forward instead of recomputing (content-addressed cache hit).
      (and prior-pub-dir (publication-up-to-date? prior-pub-dir work_content_hash))
      (do (copy-dir-files! prior-pub-dir pub-dir)
          {:slug slug :status "reused"
           :tei_manifest (relative-to-output-root output-root
                                                  (io/file pub-dir "tei.manifest.json"))})

      :else
      (try
        (let [result (materialize-publication/materialize-publication!
                      {:parser-ir-path parser_ir_path
                       :source-manifest-path source_manifest_path
                       :metadata-record-path metadata_record_path
                       :persons-dir persons_dir
                       :output-dir (str pub-dir)
                       :generated-at generated-at})]
          (files/write-text! (io/file pub-dir "source_work_content_hash.txt")
                             work_content_hash)
          {:slug slug :status "passed"
           :tei (relative-to-output-root output-root (:tei result))
           :tei_manifest (relative-to-output-root output-root
                                                  (:tei-manifest result))
           :tei_validation_result (relative-to-output-root
                                   output-root (:tei-validation-result result))})
        (catch Throwable t
          (if continue-on-failure
            {:slug slug :status "failed" :error (.getMessage t)}
            (throw t)))))))

(defn materialize-publication-item [[context work]]
  (materialize-one-publication! (assoc context :work work)))

(defn- materialize-publications!
  [{:keys [output-root prior-output-root materialization-result config-value
           snapshot-date concurrency]}]
  (let [continue-on-failure (boolean (get config-value "continue_on_failure"))
        generated-at (generated-at-for snapshot-date)
        context {:output-root output-root
                 :prior-output-root prior-output-root
                 :generated-at generated-at
                 :continue-on-failure continue-on-failure}
        results (parallel/ordered-pmap
                 concurrency
                 materialize-publication-item
                 (mapv (fn [work] [context work])
                       (:selected materialization-result)))]
    {:results results
     :report {"schema_version" "soranoha-build-publication-publications-v1"
              "corpus_snapshot_hash" (:corpus-snapshot-hash
                                      materialization-result)
              "publication_count" (count results)
              "passed" (count (filter #(= "passed" (:status %)) results))
              "reused" (count (filter #(= "reused" (:status %)) results))
              "skipped" (count (filter #(= "skipped" (:status %)) results))
              "failed" (count (filter #(= "failed" (:status %)) results))
              "publications" (mapv (fn [r]
                                     {"slug" (:slug r)
                                      "status" (:status r)
                                      "tei" (:tei r)
                                      "tei_manifest" (:tei_manifest r)
                                      "error" (:error r)})
                                   results)}}))

(defn materialize-source-selection-step
  [{:keys [aozora-root output-root config-value snapshot-date opts]}]
  (let [result (materialize-selected-sources!
                {:aozora-root aozora-root
                 :output-root output-root
                 :parser-profile (get config-value "parser_profile")
                 :snapshot-date snapshot-date
                 :aozora-git-commit (get (git-provenance aozora-root)
                                         "aozora_git_commit")
                 :continue-on-failure
                 (boolean (get config-value "continue_on_failure"))
                 :concurrency (:concurrency opts)})
        selection-report-file (io/file output-root
                                       "source-selection-report.json")]
    {:state-updates {:materialization-result result}
     :status (if (get-in result [:report "release_admissible"])
               :passed
               :partial)
     :outputs [{:role "source-selection-report"
                :path (str selection-report-file)
                :content_hash (manifest/file-hash selection-report-file)}]}))

(defn write-build-records-step
  [{:keys [opts config-value materialization-result output-root]}]
  (let [plan (build-plan opts config-value materialization-result)
        config-file (io/file output-root "build-config.json")
        plan-file (io/file output-root "build-plan.json")]
    (abc-json/write-deterministic-json-file! config-file config-value)
    (abc-json/write-deterministic-json-file! plan-file plan)
    {:state-updates {:build-plan plan}
     :outputs [{:role "build-config"
                :path (str config-file)
                :content_hash (manifest/file-hash config-file)}
               {:role "build-plan"
                :path (str plan-file)
                :content_hash (manifest/file-hash plan-file)}]}))

(defn materialize-publications-step
  [{:keys [config-value materialization-result output-root
           prior-output-root snapshot-date opts]}]
  (let [{:keys [results report]}
        (materialize-publications!
         {:output-root output-root
          :prior-output-root prior-output-root
          :materialization-result materialization-result
          :config-value config-value
          :snapshot-date snapshot-date
          :concurrency (:concurrency opts)})
        report-file (io/file output-root "publications"
                             "publications-report.json")]
    (abc-json/write-deterministic-json-file! report-file report)
    {:status (if (some #(= "failed" (:status %)) results)
               :partial
               :passed)
     :state-updates {:publication-result report}
     :outputs [{:role "publications-report"
                :path (str report-file)
                :content_hash (manifest/file-hash report-file)}]}))

(defn- build-publication-steps []
  [{:id :materialize-source-selection
    :requires [:aozora-root :output-root :config-value :snapshot-date :opts]
    :produces [:materialization-result]
    :run materialize-source-selection-step}
   {:id :write-build-records
    :requires [:opts :config-value :materialization-result :output-root]
    :produces [:build-plan]
    :run write-build-records-step}
   {:id :materialize-publications
    :requires [:build-plan :config-value :materialization-result :output-root
               :prior-output-root :snapshot-date :opts]
    :produces [:publication-result]
    :run materialize-publications-step}])

(defn build-publication!
  [options]
  (let [{:keys [aozora-root config snapshot-date output-root replace]
         :as opts} (parse-args options)
        config-value (read-config config)]
    (when (string/blank? snapshot-date)
      (throw (ex-info "snapshot-date is required for build-publication"
                      {:config config})))
    (publication-policy/assert-release-allowed!)
    (let [prior-output-root (when (files/directory? output-root)
                              (str output-root))
          tmp-root (prepare-output-root! output-root replace)]
      (files/create-dirs! tmp-root)
      (let [opts (-> opts
                     (assoc :output-root tmp-root)
                     (update :concurrency resolve-concurrency))
            workflow-result
            (workflow/run-workflow!
             {:workflow-id "soranoha.build-publication.v1"
              :run-id (str "build-publication:" snapshot-date)
              :output-root tmp-root
              :initial-state {:aozora-root aozora-root
                              :config-value config-value
                              :snapshot-date snapshot-date
                              :output-root tmp-root
                              :prior-output-root prior-output-root
                              :opts opts}
              :steps (build-publication-steps)})
            final-root (promote-output-root! tmp-root output-root replace)]
        (println "build_publication_root:" (str final-root))
        (println "materialized_root:" (str (io/file final-root
                                                    "materialized-root")))
        (println "publications_root:" (str (io/file final-root
                                                    "publications")))
        (if (= "passed" (get-in workflow-result [:run "status"]))
          0
          1)))))
