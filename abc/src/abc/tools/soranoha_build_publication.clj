(ns abc.tools.soranoha-build-publication
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.aozora-csv :as aozora-csv]
            [abc.tools.aozora-ingest :as aozora-ingest]
            [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.json :as abc-json]
            [abc.tools.manifest :as manifest]
            [abc.tools.materialize-publication :as materialize-publication]
            [abc.tools.schema :as schema]
            [abc.tools.workflow :as workflow]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as string]
            [clojure.tools.cli :as cli])
  (:import [java.nio.file Files StandardCopyOption]
           [java.util.zip ZipEntry ZipFile]))

(def config-schema-path
  "schemas/soranoha-publication-build-config.schema.json")

(def cli-options
  [[nil "--aozora-root DIR" "Official aozorabunko checkout root."
    :id :aozora-root]
   [nil "--config FILE" "Soranoha publication build config JSON."
    :id :config]
   [nil "--snapshot-date DATE" "Snapshot date in UTC calendar form, YYYY-MM-DD."
    :id :snapshot-date]
   [nil "--output-root DIR" "Final output root."
    :id :output-root]
   [nil "--replace" "Replace an existing output root after a successful build."
    :id :replace :default false]])

(defn- normalized-path [file]
  (string/replace (str file) "\\" "/"))

(defn- zip-file? [file]
  (and (.isFile file)
       (string/ends-with? (.getName file) ".zip")))

(defn- normalized-abs-path
  "Absolute, `.`/`..`-normalized path that does NOT resolve symlinks — unlike
  getCanonicalFile. Keeps files under a symlinked root (e.g. the zero-copy
  aozorabunko-corpus symlinkJoin) instead of escaping to the symlink targets."
  [f]
  (.normalize (.toAbsolutePath (.toPath (io/file f)))))

(defn- aozora-work-zip? [root file]
  (let [rel (normalized-path
             (.relativize (normalized-abs-path root)
                          (normalized-abs-path file)))]
    (when (re-matches #"^cards/[0-9]{6}/files/[^/]+\.zip$" rel)
      rel)))

(defn- read-catalog-zip [aozora-root]
  (let [zip-file (io/file aozora-root "index_pages"
                          "list_person_all_extended_utf8.zip")]
    (when-not (.isFile zip-file)
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
  (->> (file-seq (io/file aozora-root))
       (filter zip-file?)
       (map (fn [file]
              {:file file
               :relpath (aozora-work-zip? aozora-root file)}))
       vec))

(defn- first-text-member [zip-file]
  (with-open [zf (ZipFile. (io/file zip-file))]
    (or (some (fn [^ZipEntry entry]
                (when (and (not (.isDirectory entry))
                           (string/ends-with?
                            (string/lower-case (.getName entry))
                            ".txt"))
                  (.getName entry)))
              (enumeration-seq (.entries zf)))
        (throw (ex-info "work ZIP contains no .txt member"
                        {:path (str zip-file)})))))

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

(defn- zip-member-bytes [zip-file zip-member]
  (with-open [zf (ZipFile. (io/file zip-file))]
    (if-let [entry (.getEntry zf zip-member)]
      (with-open [in (.getInputStream zf entry)]
        (.readAllBytes in))
      (throw (ex-info "zip member missing"
                      {:zip (str zip-file) :member zip-member})))))

(defn- run-process!
  "Run a subprocess inheriting the current environment plus extra-env, feeding
  stdin-bytes, returning {:exit :out-bytes :err}."
  [{:keys [args stdin-bytes extra-env]}]
  (let [pb (ProcessBuilder. ^java.util.List (mapv str args))]
    (doseq [[k v] extra-env]
      (.put (.environment pb) (str k) (str v)))
    (let [proc (.start pb)]
      (with-open [os (.getOutputStream proc)]
        (when stdin-bytes (.write os ^bytes stdin-bytes)))
      (let [out (.readAllBytes (.getInputStream proc))
            err (slurp (.getErrorStream proc))
            exit (.waitFor proc)]
        {:exit exit :out-bytes out :err err}))))

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
  [{:keys [aat-file parser-ir-file divergence-file]}]
  (let [convert-bin (require-env "AB_AAT_TO_PARSER_IR_BIN" "ab-aat-to-parser-ir")
        mapping (require-env "AB_AAT_TO_PARSER_IR_MAPPING"
                             "aat→parser-IR mapping document")
        {:keys [exit err]}
        (run-process! {:args [convert-bin "convert"
                              "--aat" aat-file
                              "--mapping" mapping
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
  [{:keys [parser-profile source-bytes aat-file parser-ir-file
           divergence-file]}]
  (let [adapter (resolve-adapter parser-profile)]
    (write-aat! aat-file {:adapter adapter :source-bytes source-bytes})
    (convert-aat->parser-ir! {:aat-file (str aat-file)
                              :parser-ir-file (str parser-ir-file)
                              :divergence-file (str divergence-file)})))

(def ^{:dynamic true
       :doc "Injectable source→parser-IR boundary. Bound to a stub in tests so
             the workflow can be exercised without the adapter binaries."}
  *derive-parser-ir!* real-derive-parser-ir!)

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

(defn- official-source [row relpath zip-member source-file source-hash]
  {"work_id" (row-work-id row)
   "card_person_id" (row-person-id row)
   "text_url" (get row "テキストファイルURL")
   "text_zip_relpath" relpath
   "zip_member" zip-member
   "source_hash" source-hash
   "source_bytes" (hash/byte-length source-file)})

(defn- write-materialized-work!
  [{:keys [rows catalog-provenance materialized-root selected parser-profile
           corpus-hash]}]
  (let [{:keys [row file relpath]} selected
        work-id (row-work-id row)
        person-id (row-person-id row)
        work-hash (hash/format-sha256 (files/sha256-file file))
        zip-member (first-text-member file)
        source-bytes (zip-member-bytes file zip-member)
        work-dir (io/file materialized-root "works"
                          (slug work-id person-id relpath))
        aat-file (io/file work-dir "aat.json")
        parser-ir-file (io/file work-dir "parser-ir.json")
        divergence-file (io/file work-dir "divergence.json")
        source-manifest-file (io/file work-dir "source.manifest.json")
        persons-dir (io/file materialized-root "persons")
        metadata-file (io/file work-dir "metadata-record.json")]
    (.mkdirs work-dir)
    ;; Real AAT + parser-IR from the owned adapters (replaces the former stub),
    ;; through the injectable boundary so tests can stub it.
    (*derive-parser-ir!* {:parser-profile parser-profile
                          :source-bytes source-bytes
                          :aat-file aat-file
                          :parser-ir-file parser-ir-file
                          :divergence-file divergence-file})
    ;; Source truth + the source manifest publication materialization requires.
    (abc-json/write-deterministic-json-file!
     (io/file work-dir "official-source.json")
     (official-source row relpath zip-member file work-hash))
    (abc-json/write-deterministic-json-file!
     source-manifest-file
     (work-source-manifest work-hash corpus-hash))
    (spit (io/file work-dir "warnings.jsonl") "")
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
     :source_hash work-hash
     :zip_member zip-member
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
                               "zip_member" (:zip_member source)})
                            selected)
   "rejected_sources" (mapv identity rejected)})

(defn- materialize-selected-sources!
  [{:keys [aozora-root output-root parser-profile snapshot-date
           aozora-git-commit]}]
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
        selected (mapv #(write-materialized-work!
                         {:rows rows
                          :catalog-provenance catalog-provenance
                          :materialized-root materialized-root
                          :parser-profile parser-profile
                          :corpus-hash corpus-hash
                          :selected %})
                       selected-candidates)
        selected-relpaths (set (map :relpath selected-candidates))
        rejected (->> candidates
                      (remove #(contains? selected-relpaths (:relpath %)))
                      (mapv (fn [{:keys [file relpath]}]
                              {"path" (or relpath
                                          (normalized-path
                                           (.relativize
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
    (when-not (seq selected)
      (throw (ex-info "no catalog-backed work ZIPs were selected"
                      {:aozora_root (str aozora-root)})))
    (let [report (selection-report selected rejected)]
      (abc-json/write-deterministic-json-file!
       (io/file output-root "source-selection-report.json")
       report)
      {:materialized-root materialized-root
       :corpus-snapshot-hash corpus-hash
       :report report
       :selected selected})))

(defn- read-config [path]
  (let [config-file (let [file (io/file path)]
                      (if (.isFile file)
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
    (let [{:keys [exit out]} (apply shell/sh "git" "-C" (str aozora-root) args)]
      (when (zero? exit)
        (string/trim out)))
    (catch java.io.IOException _
      nil)))

(defn- git-provenance [aozora-root]
  (let [commit (or (git-sh aozora-root "rev-parse" "HEAD")
                   (let [head (io/file aozora-root ".git" "HEAD")]
                     (when (.isFile head)
                       (string/trim (slurp head)))))
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
                                   [:report "selected_source_count"])})

(defn- parse-args [args]
  (let [{:keys [options errors]} (cli/parse-opts args cli-options)]
    (when (seq errors)
      (throw (ex-info (string/join "\n" errors) {:errors errors})))
    (doseq [required [:aozora-root :config :output-root]]
      (when (string/blank? (get options required))
        (throw (ex-info (str (name required) " is required")
                        {:missing required}))))
    options))

(defn- prepare-output-root! [output-root replace?]
  (let [output-root-file (io/file output-root)]
    (when (and (.exists output-root-file) (not replace?))
      (throw (ex-info "output-root already exists; pass --replace to replace it after a successful build"
                      {:output_root (str output-root-file)})))
    (.mkdirs (or (.getParentFile output-root-file) (io/file ".")))
    (io/file (str output-root ".tmp-" (System/nanoTime)))))

(defn- promote-output-root! [tmp-root output-root replace?]
  (let [target (io/file output-root)]
    (when (and replace? (.exists target))
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
    (and (.isFile (io/file pub-dir "tei.manifest.json"))
         (.isFile marker)
         (= work-hash (string/trim (slurp marker))))))

(defn- copy-dir-files!
  "Copy the flat set of publication artifacts from one dir to another."
  [from to]
  (.mkdirs (io/file to))
  (doseq [f (.listFiles (io/file from))
          :when (.isFile f)]
    (files/copy-file! (str f) (io/file to (.getName f))))
  to)

(defn- materialize-one-publication!
  [{:keys [output-root prior-output-root generated-at continue-on-failure work]}]
  (let [{:keys [slug source_hash parser_ir_path source_manifest_path
                metadata_record_path persons_dir]} work
        pub-dir (io/file output-root "publications" slug)
        prior-pub-dir (when prior-output-root
                        (io/file prior-output-root "publications" slug))]
    (cond
      ;; Already materialized in this output-root from identical source.
      (publication-up-to-date? pub-dir source_hash)
      {:slug slug :status "skipped"
       :tei_manifest (str (io/file pub-dir "tei.manifest.json"))}

      ;; A prior promoted build holds a byte-identical-source publication —
      ;; copy it forward instead of recomputing (content-addressed cache hit).
      (and prior-pub-dir (publication-up-to-date? prior-pub-dir source_hash))
      (do (copy-dir-files! prior-pub-dir pub-dir)
          {:slug slug :status "reused"
           :tei_manifest (str (io/file pub-dir "tei.manifest.json"))})

      :else
      (try
        (let [result (materialize-publication/materialize-publication!
                      {:parser-ir-path parser_ir_path
                       :source-manifest-path source_manifest_path
                       :metadata-record-path metadata_record_path
                       :persons-dir persons_dir
                       :output-dir (str pub-dir)
                       :generated-at generated-at})]
          (spit (io/file pub-dir "source_work_content_hash.txt") source_hash)
          {:slug slug :status "passed"
           :tei (str (:tei result))
           :tei_manifest (str (:tei-manifest result))
           :tei_validation_result (str (:tei-validation-result result))})
        (catch Throwable t
          (if continue-on-failure
            {:slug slug :status "failed" :error (.getMessage t)}
            (throw t)))))))

(defn- materialize-publications!
  [{:keys [output-root prior-output-root materialization-result config-value
           snapshot-date]}]
  (let [continue-on-failure (boolean (get config-value "continue_on_failure"))
        generated-at (generated-at-for snapshot-date)
        results (mapv (fn [work]
                        (materialize-one-publication!
                         {:output-root output-root
                          :prior-output-root prior-output-root
                          :generated-at generated-at
                          :continue-on-failure continue-on-failure
                          :work work}))
                      (:selected materialization-result))]
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

(defn- build-publication-steps []
  [{:id :materialize-source-selection
    :requires [:aozora-root :output-root :config-value :snapshot-date]
    :produces [:materialization-result]
    :run (fn [{:keys [aozora-root output-root config-value snapshot-date]}]
           (let [result (materialize-selected-sources!
                         {:aozora-root aozora-root
                          :output-root output-root
                          :parser-profile (get config-value "parser_profile")
                          :snapshot-date snapshot-date
                          :aozora-git-commit (get (git-provenance aozora-root)
                                                  "aozora_git_commit")})
                 selection-report-file (io/file output-root
                                                "source-selection-report.json")]
             {:state-updates {:materialization-result result}
              :outputs [{:role "source-selection-report"
                         :path (str selection-report-file)
                         :content_hash
                         (manifest/file-hash selection-report-file)}]}))}
   {:id :write-build-records
    :requires [:opts :config-value :materialization-result :output-root]
    :produces [:build-plan]
    :run (fn [{:keys [opts config-value materialization-result output-root]}]
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
                         :content_hash (manifest/file-hash plan-file)}]}))}
   {:id :materialize-publications
    :requires [:build-plan :config-value :materialization-result :output-root
               :prior-output-root :snapshot-date]
    :produces [:publication-result]
    :run (fn [{:keys [config-value materialization-result output-root
                      prior-output-root snapshot-date]}]
           (let [{:keys [results report]}
                 (materialize-publications!
                  {:output-root output-root
                   :prior-output-root prior-output-root
                   :materialization-result materialization-result
                   :config-value config-value
                   :snapshot-date snapshot-date})
                 report-file (io/file output-root "publications"
                                      "publications-report.json")]
             (abc-json/write-deterministic-json-file! report-file report)
             {:status (if (some #(= "failed" (:status %)) results)
                        :partial
                        :passed)
              :state-updates {:publication-result report}
              :outputs [{:role "publications-report"
                         :path (str report-file)
                         :content_hash (manifest/file-hash report-file)}]}))}])

(defn build-publication!
  [args]
  (let [{:keys [aozora-root config snapshot-date output-root replace]
         :as opts} (parse-args args)
        config-value (read-config config)]
    (when (string/blank? snapshot-date)
      (throw (ex-info "snapshot-date is required for build-publication"
                      {:config config})))
    (let [prior-output-root (let [f (io/file output-root)]
                              (when (.isDirectory f) (str f)))
          tmp-root (prepare-output-root! output-root replace)]
      (.mkdirs tmp-root)
      (let [opts (assoc opts :output-root tmp-root)
            _ (workflow/run-workflow!
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
        0))))
