(ns abc.tools.soranoha-build-publication
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.aozora-csv :as aozora-csv]
            [abc.tools.aozora-ingest :as aozora-ingest]
            [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.json :as abc-json]
            [abc.tools.manifest :as manifest]
            [abc.tools.materialize-publication :as materialize-publication]
            [abc.tools.metadata-record :as metadata-record]
            [abc.tools.parallel :as parallel]
            [abc.tools.parser-release-authority :as parser-release-authority]
            [abc.tools.publication-policy :as publication-policy]
            [abc.tools.publication-release :as publication-release]
            [abc.tools.schema :as schema]
            [abc.tools.snapshot-index :as snapshot-index]
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

(defn- card-directory
  "The contributor card directory a work ZIP lives under, e.g. \"001030\" for
  cards/001030/files/47896_ruby_49619.zip. Aozora files one work_id under
  several contributor cards, and `person_id` comes from primary-person metadata
  rather than the directory, so this is the ONLY element that distinguishes
  those copies. Fails closed rather than yielding a slug that cannot address a
  unique source; the pattern matches `aozora-work-zip?`, which every selected
  candidate has already satisfied."
  [relpath]
  (or (second (re-matches #"^cards/([0-9]{6})/files/[^/]+\.zip$" relpath))
      (throw (ex-info "work ZIP relpath names no card directory"
                      {:code "unslugifiable-source-relpath"
                       :text_zip_relpath relpath}))))

(defn- slug
  "Publication identity for one source. Injective over selected sources, and a
  function of that source's own coordinates ALONE — never of what else the
  corpus contains, so adding or removing an unrelated source can never change
  another work's identity."
  [work-id person-id relpath]
  (let [basename (.getName (io/file relpath))
        stem (subs basename 0 (- (count basename) (count ".zip")))]
    (str work-id "_" person-id "_" (card-directory relpath) "_" stem)))

(defn- candidate-slug-collisions
  "PURE. Candidate slug claims grouped by slug, keeping only slugs claimed more
  than once. Runs BEFORE archive inspection: the slug is a function of
  (work_id, person_id, relpath) alone, so a collision is knowable without
  opening a ZIP — and must be known before any `works/<slug>` write."
  [candidates]
  (->> candidates
       (map (fn [{:keys [row relpath]}]
              {"slug" (slug (row-work-id row) (row-person-id row) relpath)
               "text_zip_relpath" relpath}))
       (group-by #(get % "slug"))
       (filter (fn [[_ claims]] (< 1 (count claims))))
       (sort-by key)
       (mapv (fn [[work-slug claims]]
               {"slug" work-slug
                "sources" (mapv (fn [claim]
                                  {"text_zip_relpath" (get claim "text_zip_relpath")})
                                (sort-by #(get % "text_zip_relpath") claims))}))))

(defn- assert-candidate-slugs-unique!
  "Return `candidates` when every candidate claims a distinct slug; otherwise
  throw before any slug-addressed filesystem write occurs. A candidate claims
  its identity from the catalog, so an unreadable archive does not withdraw the
  claim: `continue_on_failure` must not resolve a collision."
  [candidates]
  (let [collisions (candidate-slug-collisions candidates)]
    (when (seq collisions)
      (throw (ex-info "selected sources claim duplicate publication slugs"
                      {:code "publication-slug-collision"
                       :collisions collisions})))
    candidates))

;; ── Real source→parser-IR derivation via the owned, Nix-built adapters ──
;; The adapter/converter binaries are provided by the flake through env vars
;; (mirroring the existing AB_AAT_TO_PARSER_IR_BIN wiring used by
;; annotation-join-stats-run). Keeping them as an injected boundary keeps
;; build-publication hermetic and lets parser_profile select the adapter.

(def ^{:dynamic true
       :doc "Injectable environment lookup (name → value). Bound to a map
             lookup in tests so adapter/profile resolution can be exercised
             without mutating the JVM environment."}
  *env*
  (fn [k] (System/getenv k)))

(defn- env-value [k]
  (let [v (*env* k)]
    (when-not (string/blank? v) v)))

(defn- require-env [k what]
  (or (env-value k)
      (throw (ex-info (str what " unavailable; set " k)
                      {:env_var k}))))

(defn- resolve-adapter
  "Resolve the source→AAT adapter AND its aat→parser-IR mapping pin for the
  configured parser_profile. The mapping selects the AAT schema version the
  converter accepts (the v1 mapping rejects ab-aozora's AAT v2), so it rides
  with the profile instead of being a build-wide global. Any other profile is
  an explicit, loud error so a build never silently falls back to a stub."
  [parser-profile]
  (case parser-profile
    ;; The project-owned parser (ADR 0038/0039): one native stdin→AAT binary,
    ;; no external renderer and no separate mapper — the binary is the whole
    ;; adapter identity, paired with the v2 mapping (AAT schema 2). The
    ;; third-party comparison lanes (aozora2html et al.) are retired — ADR
    ;; third-party-comparison-retirement.
    "ab-aozora"
    {:adapter-id "ab-aozora"
     :wrapper (require-env "AB_AOZORA_BIN" "ab-aozora parser")
     :extra-env {}
     :mapping (require-env "AB_AAT_TO_PARSER_IR_MAPPING_V2"
                           "aat→parser-IR v2 mapping document")}

    (throw (ex-info "unsupported parser_profile for real materialization"
                    {:parser_profile parser-profile
                     :supported ["ab-aozora"]}))))

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
  "Run the profile's source→AAT adapter over the raw source bytes, writing
  the AAT JSON to aat-file."
  [aat-file {:keys [adapter source-bytes]}]
  (let [{:keys [adapter-id wrapper extra-env]} adapter
        {:keys [exit out-bytes err]}
        (run-process! {:args [wrapper "--mode" "aat"]
                       :stdin-bytes source-bytes
                       :extra-env extra-env})]
    (when-not (zero? exit)
      (throw (ex-info (str adapter-id " adapter failed")
                      {:adapter adapter-id :exit exit :stderr err})))
    (io/make-parents aat-file)
    (with-open [os (io/output-stream aat-file)]
      (.write os ^bytes out-bytes))
    aat-file))

(defn- convert-aat->parser-ir!
  "Run ab-aat-to-parser-ir convert with the profile-pinned mapping, emitting
  parser-IR + divergence sidecar. The convert binary is the once-resolved
  converter path carried on the adapter (never re-resolved per work)."
  [{:keys [aat-file parser-ir-file divergence-file work-content-hash mapping
           convert-bin]}]
  (let [{:keys [exit err]}
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
  "Production source→parser-IR: run the source→AAT adapter, then
  ab-aat-to-parser-ir convert, using the ONCE-resolved adapter/converter value
  passed in (env vars are resolved a single time up front, never per work)."
  [{:keys [adapter source-bytes work-content-hash aat-file parser-ir-file
           divergence-file]}]
  (write-aat! aat-file {:adapter adapter :source-bytes source-bytes})
  (convert-aat->parser-ir! {:aat-file (str aat-file)
                            :work-content-hash work-content-hash
                            :parser-ir-file (str parser-ir-file)
                            :divergence-file (str divergence-file)
                            :mapping (:mapping adapter)
                            :convert-bin (:converter-bin adapter)}))

(def ^{:dynamic true
       :doc "Injectable source→parser-IR boundary. Bound to a stub in tests so
             the workflow can be exercised without the adapter binaries."}
  *derive-parser-ir!* real-derive-parser-ir!)

(defn invoke-derive-parser-ir! [options]
  (*derive-parser-ir!* options))

;; ── Authenticated parser runtime identity ──────────────────────────────────
;; Computed ONCE, before source derivation, from the once-resolved adapter and
;; converter. `parser_runtime_identity_object` is deliberately path-free (no
;; Nix store path); `parser_config_hash` is JCS SHA-256 over it. For the owned
;; parser the configured candidate is authenticated through
;; parser-release-authority/authenticate and the runtime coordinates are
;; compared field-by-field; divergences are RECORDED as problems rather than
;; collapsed into an "admitted" boolean.

(def ^{:doc "The parser argv template every profile invokes the source→AAT
             adapter with (`[wrapper --mode aat]`), templated path-free."}
  parser-argv-template ["{executable}" "--mode" "{mode}"])

(def ^{:doc "The converter subcommand publication renders with, templated
             path-free at the same granularity the qualification provenance
             records (`[{executable} <subcommand>]`)."}
  converter-argv-template ["{executable}" "convert"])

(defn- runtime-authenticate-options
  "Repo-relative authority paths for the release-parser-identity record, matching
  the layout parser-release-authority/authenticate reads (abc working dir)."
  [record-path]
  {:release_parser_identity_path record-path
   :decisions_path "docs/adr/decisions.edn"})

(defn- coordinate-problem [coordinate expected actual]
  (when (not= expected actual)
    {:kind :parser-runtime-coordinate-mismatch
     :coordinate coordinate
     :expected expected
     :actual actual}))

(defn- authenticate-runtime
  "Authenticate the configured candidate for the resolved (canonical) runtime
  identity object, comparing its coordinates against the authenticated
  qualification identity and executable provenance. Returns
  {:problems [...] :candidate-ref sha :qualification-identity-ref sha}. A
  diagnostic profile with no configured release-parser-identity record records
  one absent-candidate problem and null refs; an authentication failure surfaces
  as authority problems with null refs, never a throw."
  [parser-profile identity-object record-path]
  (if (string/blank? record-path)
    {:problems [{:kind :absent-parser-candidate
                 :message "no release_parser_identity configured; runtime parser identity is unauthenticated"
                 :parser_profile parser-profile}]
     :candidate-ref nil
     :qualification-identity-ref nil}
    (try
      (let [auth (parser-release-authority/authenticate
                  (runtime-authenticate-options record-path))
            qualification (:qualification-identity auth)
            executables (:executables (:executable-provenance auth))
            by-name (into {} (map (juxt :name identity)) executables)
            parser-exe (get by-name (get identity-object "adapter_id"))
            converter-exe (get by-name "ab-aat-to-parser-ir")]
        {:problems
         (vec
          (keep identity
                [(coordinate-problem "adapter_id"
                                     (:aat_adapter qualification)
                                     (get identity-object "adapter_id"))
                 (coordinate-problem "parser_build_hash"
                                     (:sha256 parser-exe)
                                     (get identity-object "parser_build_hash"))
                 (coordinate-problem "converter_build_hash"
                                     (:sha256 converter-exe)
                                     (get identity-object "converter_build_hash"))
                 (coordinate-problem "aat_parser_ir_mapping_hash"
                                     (:mapping_hash qualification)
                                     (get identity-object "aat_parser_ir_mapping_hash"))
                 (coordinate-problem "parser_ir_schema_hash"
                                     (:parser_ir_schema_hash qualification)
                                     (get identity-object "parser_ir_schema_hash"))]))
         :candidate-ref (:candidate-ref auth)
         :qualification-identity-ref (:qualification-identity-ref auth)})
      (catch clojure.lang.ExceptionInfo error
        {:problems (mapv (fn [problem] (assoc problem :kind :parser-authority-problem))
                         (or (:problems (ex-data error))
                             [{:message (ex-message error)}]))
         :candidate-ref nil
         :qualification-identity-ref nil}))))

(defn- real-resolve-parser-runtime!
  "Resolve the adapter and converter ONCE, hash their actual bytes, hash the
  mapping via its GOVERNED JSON construction, read the target parser-IR schema
  hash from the mapping, and compute the path-free config hash over the CANONICAL
  parser_runtime_identity_object (the exact shape the snapshot index and the
  release verifier recompute).

  Parser resolution is independent of source trust: a `fixture` build resolves
  the same real adapter and produces the same concrete runtime identity as an
  `official-git` build. Fixture affects SOURCE TRUST and release admissibility
  (recorded by source-provenance!), not which parser rendered the corpus, so a
  fixture build renders real publications into a non-admissible root instead of
  being unrenderable."
  [{:keys [parser-profile release-parser-identity-path]}]
  (let [adapter (resolve-adapter parser-profile)
        converter-bin (require-env "AB_AAT_TO_PARSER_IR_BIN" "ab-aat-to-parser-ir")
        mapping-doc (files/read-json (:mapping adapter))
        identity-object {"adapter_id" (:adapter-id adapter)
                         "adapter_argv_template" parser-argv-template
                         "converter_argv_template" converter-argv-template
                         "parser_build_hash"
                         (hash/format-sha256 (hash/sha256-file (:wrapper adapter)))
                         "converter_build_hash"
                         (hash/format-sha256 (hash/sha256-file converter-bin))
                         "aat_parser_ir_mapping_hash"
                         (hash/sha256-json-abc-legacy-v0 mapping-doc)
                         "parser_ir_schema_hash"
                         (get mapping-doc "target_parser_ir_schema_hash")}
        auth (authenticate-runtime parser-profile identity-object
                                   release-parser-identity-path)]
    {:adapter (assoc adapter :converter-bin converter-bin)
     :parser-runtime-identity identity-object
     :parser-config-hash (hash/format-sha256
                          (hash/sha256-json-jcs identity-object))
     :candidate-ref (:candidate-ref auth)
     :qualification-identity-ref (:qualification-identity-ref auth)
     :problems (:problems auth)}))

(def ^{:dynamic true
       :doc "Injectable authenticated-parser-runtime boundary. Bound to a stub
             in tests so the workflow can be exercised without the adapter
             binaries and authenticated parser-runtime evidence present."}
  *resolve-parser-runtime!* real-resolve-parser-runtime!)

(defn invoke-resolve-parser-runtime! [options]
  (*resolve-parser-runtime!* options))

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

(defn- inspect-selected-work!
  "Selection-phase work: inspect the archive, write the source bundle and the
  official-source record, materialize the metadata record, and return the sorted
  source identity row (including metadata_record_hash). No parser is invoked
  here — the corpus identity is a function of official source facts only.

  Operational per-work records stay in the flat works/<slug>/ directory (the
  source-snapshot workset builder and other consumers require that exact flat
  layout, and it is NOT part of the closed release scan). The closure-verified
  source and parser-IR manifests are written later, in the render phase, under
  publications/<slug>/ where the workset builder never scans."
  [{:keys [rows catalog-provenance materialized-root selected]}]
  (let [{:keys [row file relpath]} selected
        work-id (row-work-id row)
        person-id (row-person-id row)
        inspection (source-bundle/inspect-zip file)
        work-hash (:bundle-hash inspection)
        archive-hash (:archive-hash inspection)
        primary-text-member (:primary-text-member inspection)
        primary-text-hash (:primary-text-hash inspection)
        source-bytes (:primary-text-bytes inspection)
        work-slug (slug work-id person-id relpath)
        work-dir (io/file materialized-root "works" work-slug)
        source-bundle-file (io/file work-dir "source-bundle.json")
        persons-dir (io/file materialized-root "persons")
        metadata-file (io/file work-dir "metadata-record.json")]
    (files/create-dirs! work-dir)
    (source-bundle/write-manifest! source-bundle-file inspection)
    (abc-json/write-deterministic-json-file!
     (io/file work-dir "official-source.json")
     (official-source row relpath file inspection))
    (aozora-ingest/run-from-rows!
     {:rows rows
      :work-id work-id
      :output (str metadata-file)
      :persons-output-dir (str persons-dir)
      :overwrite true
      :source-csv-provenance catalog-provenance})
    (let [metadata-record-hash (metadata-record/record-hash
                                (abc-json/read-json-file metadata-file))]
      {:work_id work-id
       :person_id person-id
       :slug work-slug
       :text_zip_relpath relpath
       :source_hash archive-hash
       :archive_hash archive-hash
       :bundle_hash work-hash
       :work_content_hash work-hash
       :primary_text_member primary-text-member
       :primary_text_hash primary-text-hash
       :metadata_record_hash metadata-record-hash
       :zip_member primary-text-member
       :source_bytes source-bytes
       :work_dir (str work-dir)
       :source_bundle_path (str source-bundle-file)
       :parser_ir_path (str (io/file work-dir "parser-ir.json"))
       :metadata_record_path (str metadata-file)
       :persons_dir (str persons-dir)})))

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

(defn select-candidate [[context candidate]]
  (if (:continue-on-failure context)
    (try
      {:ok (inspect-selected-work! (assoc context :selected candidate))}
      (catch Throwable t
        (if-let [admission (source-bundle-admission-error t)]
          {:failed (derive-failure candidate admission)}
          (throw t))))
    {:ok (inspect-selected-work! (assoc context :selected candidate))}))

(defn- source-selection-identity-object
  "The whole-corpus source identity object the snapshot index hashes into
  source_selection_hash. Built from official Git/catalog/archive/bundle/
  primary-text/metadata facts only — never from parser output. Sources are
  canonicalized (sorted) by the snapshot-index builder."
  [{:keys [trust-mode git-commit catalog-csv-hash snapshot-date selected]}]
  {"trust_mode" trust-mode
   "aozora_git_commit" git-commit
   "catalog_csv_hash" catalog-csv-hash
   "snapshot_date" snapshot-date
   "sources" (mapv (fn [s]
                     {"work_id" (:work_id s)
                      "person_id" (:person_id s)
                      "slug" (:slug s)
                      "text_zip_relpath" (:text_zip_relpath s)
                      "archive_hash" (:archive_hash s)
                      "bundle_hash" (:bundle_hash s)
                      "primary_text_member" (:primary_text_member s)
                      "primary_text_hash" (:primary_text_hash s)
                      "metadata_record_hash" (:metadata_record_hash s)})
                   selected)})

(defn- materialize-selected-sources!
  "Source selection: return the sorted source identity rows before any parser
  rendering. Computes source_selection_hash from the official facts and
  passes it as the corpus_snapshot_hash the source and publication manifests
  share. Parser output enters only in the render phase."
  [{:keys [aozora-root output-root snapshot-date source-trust-mode
           aozora-git-commit continue-on-failure concurrency]}]
  (let [{:keys [csv-text catalog-csv-hash]} (read-catalog-zip aozora-root)
        rows (aozora-csv/read-rows-from-string csv-text)
        rows-by-basename (catalog-index rows)
        materialized-root (io/file output-root "materialized-root")
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
        select-context {:rows rows
                        :catalog-provenance catalog-provenance
                        :materialized-root materialized-root
                        :continue-on-failure continue-on-failure}
        ;; Identity before inspection: `inspect-selected-work!` writes into
        ;; works/<slug> from inside the parallel map below, so two candidates
        ;; claiming one slug would silently overwrite each other. Assert here,
        ;; before any slug-addressed write.
        _ (assert-candidate-slugs-unique! selected-candidates)
        ;; A single corrupt/unreadable work ZIP (e.g. a zip Java's reader
        ;; rejects with "invalid CEN header") must not abort a whole-corpus
        ;; selection. With continue_on_failure, record and skip it; otherwise
        ;; fail loudly as before.
        results (parallel/ordered-pmap
                 concurrency
                 select-candidate
                 (mapv (fn [candidate] [select-context candidate])
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
                                          "not-selected")})))
        source-selection (source-selection-identity-object
                          {:trust-mode source-trust-mode
                           :git-commit aozora-git-commit
                           :catalog-csv-hash catalog-csv-hash
                           :snapshot-date snapshot-date
                           :selected selected})
        corpus-hash (snapshot-index/source-selection-hash source-selection)]
    (when (and (empty? selected) (empty? derive-failures))
      (throw (ex-info "no catalog-backed work ZIPs were successfully derived"
                      {:aozora_root (str aozora-root)
                       :derive_failed_count (count derive-failures)})))
    (let [report (-> (selection-report selected rejected)
                     (assoc "derive_failed_count" (count derive-failures)
                            "derive_failures" derive-failures))]
      (abc-json/write-deterministic-json-file!
       (io/file output-root "source-selection-report.json")
       report)
      {:materialized-root materialized-root
       :corpus-snapshot-hash corpus-hash
       :source-selection-identity-object source-selection
       :catalog-csv-hash catalog-csv-hash
       :derive-failures derive-failures
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

(defn- git-command
  "Run `git -C aozora-root args...`, returning {:exit :out} or nil when git
  cannot be executed at all (missing binary / IO error). Never confuses an
  unavailable inspection with a clean result."
  [aozora-root args]
  (try
    (let [{:keys [exit out]}
          (process/sh (into ["git" "-C" (str aozora-root)] args))]
      {:exit exit :out (some-> out string/trim)})
    (catch java.io.IOException _
      nil)))

(defn- source-provenance!
  "The explicit source-trust boundary (replaces the former nil-means-clean
  git-provenance). Returns the recorded provenance value for the configured
  source_trust_mode, or throws — fail closed — when official-Git state cannot
  be proven clean.

  - `official-git`: requires a successful `git rev-parse HEAD` and
    `git status --porcelain -- cards index_pages`. A failed/absent Git
    inspection is unknown, never clean (`source-git-unavailable`); a non-blank
    relevant status is dirty (`source-git-dirty`). Callers must run this before
    any temporary-root write.
  - `fixture`: an explicit, recorded non-release trust value. Records a null
    Git commit and never invokes Git; makes release admissibility fail."
  [{:keys [source-trust-mode aozora-root]}]
  (case source-trust-mode
    "fixture"
    {"source_trust_mode" "fixture"
     "aozora_git_commit" nil
     "aozora_git_dirty" false
     "dirty_scope" "cards index_pages"
     "release_source" false}

    "official-git"
    (let [rev (git-command aozora-root ["rev-parse" "HEAD"])
          status (git-command aozora-root ["status" "--porcelain" "--"
                                           "cards" "index_pages"])]
      (when (or (nil? rev) (not (zero? (:exit rev))) (string/blank? (:out rev))
                (nil? status) (not (zero? (:exit status))))
        (throw (ex-info "official source Git state is unprovable; refusing to release"
                        {:code "source-git-unavailable"
                         :aozora_root (str aozora-root)})))
      (when-not (string/blank? (:out status))
        (throw (ex-info "official source has uncommitted changes under relevant paths"
                        {:code "source-git-dirty"
                         :aozora_root (str aozora-root)
                         :dirty_scope "cards index_pages"})))
      {"source_trust_mode" "official-git"
       "aozora_git_commit" (:out rev)
       "aozora_git_dirty" false
       "dirty_scope" "cards index_pages"
       "release_source" true})

    (throw (ex-info "unsupported source_trust_mode"
                    {:code "source-trust-mode-unsupported"
                     :source_trust_mode source-trust-mode}))))

(defn- parser-runtime-plan
  "Operational projection of the authenticated parser runtime identity: the
  path-free identity object, its config hash, and the recorded problems (as
  strings, since a build record is JSON). No adapter/store path leaks in."
  [parser-runtime]
  {"parser_runtime_identity_object" (:parser-runtime-identity parser-runtime)
   "parser_config_hash" (:parser-config-hash parser-runtime)
   "problems" (mapv pr-str (:problems parser-runtime))})

(defn- parser-identity-for-materialization
  "Project the ONCE-computed authenticated parser runtime identity into the
  materialize-publication! `:parser-identity` shape. Path-free and derived
  entirely from the runtime identity value resolved before source derivation —
  never re-reads env vars or parser authority. The coordinate keys are the
  canonical runtime-object keys, so the manifest's parser_config_hash equals the
  index's parser_config_hash and each per-work manifest coordinate agrees with
  the single runtime object."
  [parser-runtime]
  (let [runtime-identity (:parser-runtime-identity parser-runtime)]
    {:parser-build-hash (get runtime-identity "parser_build_hash")
     :parser-config-hash (:parser-config-hash parser-runtime)
     :mapping-hash (get runtime-identity "aat_parser_ir_mapping_hash")
     :parser-ir-schema-hash (get runtime-identity "parser_ir_schema_hash")}))

(defn- build-plan
  "Every place-valued field here is a RELATIVE logical locator under the
  eventual output-root, never an absolute path: not the source aozora_root,
  not the discarded temporary root the build actually wrote to, and not the
  installed output-root itself. build-plan.json is release identity, not an
  operational trace of this process's filesystem layout."
  [opts config materialization-result source-provenance parser-runtime]
  {"build_schema_version" "soranoha-build-publication-v0"
   "config_hash" (analysis-identity/hash-json-value config)
   "config" config
   "snapshot_date" (:snapshot-date opts)
   "source_provenance" source-provenance
   "parser_runtime" (parser-runtime-plan parser-runtime)
   "materialized_root" "materialized-root"
   "source_selection_report" "source-selection-report.json"
   "publications" "publications"
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

(defn- relative-to-output-root
  "Path of file relative to output-root, so publications-report.json survives
  the tmp-root -> output-root promotion instead of pinning a since-renamed-away
  absolute path (also keeps the report byte-identical regardless of
  concurrency or the output-root's own absolute location)."
  [output-root file]
  (normalized-path (fs/relativize (normalized-abs-path output-root)
                                  (normalized-abs-path file))))

;; ── The closed release identity policies ────────────────────────────────────
;; The failure and layout policies the direct build stamps into the one live
;; snapshot-index 0.2.0 value. They are release identity, not tunables.

(def ^:private release-failure-policy
  {"allow_nonzero_failures" false
   "max_failure_rate" 0
   "per_diagnostic_tolerances" {}})

(def ^:private release-layout-policy
  {"loose_artifact_kinds" ["source" "parser-ir" "tei" "plaintext"]
   "batched_artifact_kinds" []
   "batch_target_work_count" 250
   "archive_format" "tar.zst"})

(defn- parser-ir-validation-status [parser-ir]
  (cond
    (seq (get parser-ir "errors")) "failed"
    (seq (get parser-ir "warnings")) "warning"
    :else "passed"))

(defn- artifact-reference
  "Loose per-work artifact reference for the snapshot index. Identity fields come
  from the written manifest; the locator is a relative loose path under the
  output-root (staging may move it without changing artifact identity)."
  [output-root work-slug kind manifest-file manifest-value]
  {"artifact_id" (get manifest-value "artifact_id")
   "artifact_kind" kind
   "work_slug" work-slug
   "sidecar_role" nil
   "validation_status" (get manifest-value "validation_status")
   "manifest_content_hash" (manifest/file-hash manifest-file)
   "content_hash" (get-in manifest-value ["content" "content_hash"])
   "locator" {"kind" "loose"
              "path" (relative-to-output-root output-root manifest-file)}})

(defn- write-source-manifest!
  "Write the per-work source manifest into publications/<slug>/, naming its
  co-located source-bundle.json COPY as content and using the whole-corpus
  source_selection_hash as corpus_snapshot_hash. Returns
  {:reference … :manifest-path …}. The bundle is copied (not moved) so the flat
  materialized-root/works/<slug>/source-bundle.json remains for the retained
  source-snapshot workset consumer, which requires that flat layout."
  [output-root pub-dir generated-at corpus-hash work]
  (files/create-dirs! pub-dir)
  (let [bundle-copy (io/file pub-dir "source-bundle.json")
        source-manifest-file (io/file pub-dir "source.manifest.json")
        _ (fs/copy (:source_bundle_path work) bundle-copy {:replace-existing true})
        content (manifest/content bundle-copy "application/json"
                                  "source-bundle.json" files/sha256-file)
        m (manifest/source-bundle-artifact-manifest
           {:corpus-snapshot-hash corpus-hash
            :work-content-hash (:work_content_hash work)
            :metadata-record-hash (:metadata_record_hash work)
            :content content
            :generated-at generated-at
            :activity-id "https://w3id.org/abc/activity/soranoha-build-publication-source"
            :agent "abc.tools.soranoha-build-publication"
            :notes "Source bundle manifest emitted by soranoha build-publication."})]
    (manifest/write-json-file! source-manifest-file m)
    {:reference (artifact-reference output-root (:slug work) "source"
                                    source-manifest-file m)
     :manifest-path (str source-manifest-file)}))

(defn- write-parser-ir-manifest!
  "Write the per-work parser-IR manifest into publications/<slug>/, naming its
  co-located parser-ir.json COPY as content and binding the once-resolved runtime
  parser identity. Returns its reference. The parser-IR is copied (not moved) so
  the flat materialized-root/works/<slug>/parser-ir.json remains for the retained
  workset consumer."
  [output-root pub-dir generated-at corpus-hash parser-identity work parser-ir]
  (let [ir-copy (io/file pub-dir "parser-ir.json")
        manifest-file (io/file pub-dir "parser-ir.manifest.json")
        _ (fs/copy (:parser_ir_path work) ir-copy {:replace-existing true})
        content (manifest/content ir-copy "application/json"
                                  "parser-ir.json" files/sha256-file)
        inputs {"corpus_snapshot_hash" corpus-hash
                "work_content_hash" (:work_content_hash work)
                "parser_build_hash" (:parser-build-hash parser-identity)
                "parser_config_hash" (:parser-config-hash parser-identity)
                "mapping_hash" (:mapping-hash parser-identity)
                "parser_ir_schema_hash" (or (:parser-ir-schema-hash parser-identity)
                                            (get parser-ir "schema_hash"))}
        m (manifest/parser-ir-artifact-manifest
           {:manifest-inputs inputs
            :content content
            :validation-status (parser-ir-validation-status parser-ir)
            :generated-at generated-at
            :activity-id "https://w3id.org/abc/activity/soranoha-build-publication-parser-ir"
            :agent "abc.tools.soranoha-build-publication"
            :notes "Parser-IR manifest emitted by soranoha build-publication."})]
    (manifest/write-json-file! manifest-file m)
    (artifact-reference output-root (:slug work) "parser-ir" manifest-file m)))

(defn- render-one-work!
  "Render one selected work in the fresh temporary root and return its four
  artifact references (source, parser-ir, plaintext, tei). Every closure-verified
  per-work artifact lives together under publications/<slug>/. Source manifest
  write, parser derivation, and the parser-IR identity assertion are HARD: a
  corrupt derive or a parser-IR/bundle identity mismatch aborts the build even in
  best-effort mode. Only the publication render is caught as a recorded per-work
  failure when continue_on_failure is set."
  [{:keys [output-root generated-at continue-on-failure parser-identity adapter
           corpus-hash work]}]
  (let [{:keys [slug work_content_hash primary_text_hash source_bytes
                parser_ir_path metadata_record_path persons_dir work_dir]} work
        pub-dir (io/file output-root "publications" slug)
        parser-ir-file (io/file parser_ir_path)
        aat-file (io/file work_dir "aat.json")
        divergence-file (io/file work_dir "divergence.json")
        {source-ref :reference source-manifest-path :manifest-path}
        (write-source-manifest! output-root pub-dir generated-at corpus-hash work)
        _ (invoke-derive-parser-ir! {:adapter adapter
                                     :source-bytes source_bytes
                                     :work-content-hash work_content_hash
                                     :aat-file aat-file
                                     :parser-ir-file parser-ir-file
                                     :divergence-file divergence-file})
        parser-ir (assert-parser-identities! parser-ir-file work_content_hash
                                             primary_text_hash)
        parser-ir-ref (write-parser-ir-manifest! output-root pub-dir generated-at
                                                 corpus-hash parser-identity work
                                                 parser-ir)]
    (try
      (let [result (materialize-publication/materialize-publication!
                    {:parser-ir-path (str parser-ir-file)
                     :source-manifest-path source-manifest-path
                     :metadata-record-path metadata_record_path
                     :persons-dir persons_dir
                     :output-dir (str pub-dir)
                     :generated-at generated-at
                     :parser-identity parser-identity})
            plaintext-ref (artifact-reference
                           output-root slug "plaintext" (:plaintext-manifest result)
                           (files/read-json (:plaintext-manifest result)))
            tei-ref (artifact-reference
                     output-root slug "tei" (:tei-manifest result)
                     (files/read-json (:tei-manifest result)))]
        {:slug slug :status "passed"
         :references [source-ref parser-ir-ref plaintext-ref tei-ref]
         :tei (relative-to-output-root output-root (:tei result))
         :tei_manifest (relative-to-output-root output-root
                                                (:tei-manifest result))})
      (catch Throwable t
        (if continue-on-failure
          {:slug slug :status "failed" :error (.getMessage t)
           :references [source-ref parser-ir-ref]
           :failure {"stage" "render"
                     "work_slug" slug
                     "code" "publication-render-failed"
                     "message" (.getMessage t)}}
          (throw t))))))

(defn materialize-publication-item [[context work]]
  (render-one-work! (assoc context :work work)))

(defn- selection-failure-records
  "Project source-selection admission failures into stable snapshot-index failure
  records (only stage/work_slug/code enter failure identity; message is a
  diagnostic)."
  [derive-failures]
  (mapv (fn [f]
          {"stage" "source"
           "work_slug" (slug (get f "work_id") (get f "person_id")
                             (get f "text_zip_relpath"))
           "code" (or (get f "reason") "source-selection-failed")
           "message" (get f "error")})
        derive-failures))

(defn- materialize-publications!
  "Render pass: for every selected work write valid source and parser-IR
  manifests, derive the parser-IR, render the publication, and collect all four
  references. After all works finish, construct and write the one live
  snapshot-index 0.2.0 value from the actual selection, parser runtime identity,
  references, and failures."
  [{:keys [output-root materialization-result config-value
           snapshot-date concurrency parser-runtime]}]
  (let [continue-on-failure (boolean (get config-value "continue_on_failure"))
        generated-at (generated-at-for snapshot-date)
        corpus-hash (:corpus-snapshot-hash materialization-result)
        context {:output-root output-root
                 :generated-at generated-at
                 :continue-on-failure continue-on-failure
                 :adapter (:adapter parser-runtime)
                 :corpus-hash corpus-hash
                 :parser-identity (parser-identity-for-materialization
                                   parser-runtime)}
        results (parallel/ordered-pmap
                 concurrency
                 materialize-publication-item
                 (mapv (fn [work] [context work])
                       (:selected materialization-result)))
        references (vec (mapcat :references results))
        failures (vec (concat (selection-failure-records
                               (:derive-failures materialization-result))
                              (keep :failure results)))
        index (snapshot-index/build-snapshot-index
               {:snapshot-date snapshot-date
                :generated-at generated-at
                :source-selection (:source-selection-identity-object
                                   materialization-result)
                :parser-runtime-identity (:parser-runtime-identity parser-runtime)
                :candidate-ref (:candidate-ref parser-runtime)
                :qualification-identity-ref (:qualification-identity-ref
                                             parser-runtime)
                :failure-policy release-failure-policy
                :layout-policy release-layout-policy
                :schema-hashes [(manifest/schema-hash
                                 snapshot-index/snapshot-index-schema-path)]
                :failures failures
                :artifact-references references})]
    (snapshot-index/write-snapshot-index!
     index (str (io/file output-root "snapshot-index.json")))
    {:results results
     :index index
     :report {"schema_version" "soranoha-build-publication-publications-v1"
              "corpus_snapshot_hash" corpus-hash
              "publication_count" (count results)
              "passed" (count (filter #(= "passed" (:status %)) results))
              "failed" (count (filter #(= "failed" (:status %)) results))
              "publications" (mapv (fn [r]
                                     {"slug" (:slug r)
                                      "status" (:status r)
                                      "tei" (:tei r)
                                      "tei_manifest" (:tei_manifest r)
                                      "error" (:error r)})
                                   results)}}))

(defn materialize-source-selection-step
  [{:keys [aozora-root output-root config-value snapshot-date opts
           source-provenance]}]
  (let [result (materialize-selected-sources!
                {:aozora-root aozora-root
                 :output-root output-root
                 :snapshot-date snapshot-date
                 :source-trust-mode (get source-provenance "source_trust_mode")
                 :aozora-git-commit (get source-provenance "aozora_git_commit")
                 :continue-on-failure
                 (boolean (get config-value "continue_on_failure"))
                 :concurrency (:concurrency opts)})
        selection-report-file (io/file output-root
                                       "source-selection-report.json")]
    {:state-updates {:materialization-result result}
     :status (if (empty? (:derive-failures result))
               :passed
               :partial)
     :outputs [{:role "source-selection-report"
                :path (str selection-report-file)
                :content_hash (manifest/file-hash selection-report-file)}]}))

(defn write-build-records-step
  [{:keys [opts config-value materialization-result output-root
           source-provenance parser-runtime]}]
  (let [plan (build-plan opts config-value materialization-result
                         source-provenance parser-runtime)
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
           snapshot-date opts parser-runtime]}]
  (let [{:keys [results report index]}
        (materialize-publications!
         {:output-root output-root
          :materialization-result materialization-result
          :config-value config-value
          :snapshot-date snapshot-date
          :concurrency (:concurrency opts)
          :parser-runtime parser-runtime})
        snapshot-index-file (io/file output-root "snapshot-index.json")]
    {:status (if (some #(= "failed" (:status %)) results)
               :partial
               :passed)
     :state-updates {:publication-result report
                     :snapshot-index index}
     :outputs [{:role "snapshot-index"
                :path (str snapshot-index-file)
                :content_hash (manifest/file-hash snapshot-index-file)}]}))

(defn- build-publication-steps []
  [{:id :materialize-source-selection
    :requires [:aozora-root :output-root :config-value :snapshot-date :opts
               :source-provenance :parser-runtime]
    :produces [:materialization-result]
    :run materialize-source-selection-step}
   {:id :write-build-records
    :requires [:opts :config-value :materialization-result :output-root
               :source-provenance :parser-runtime]
    :produces [:build-plan]
    :run write-build-records-step}
   {:id :materialize-publications
    :requires [:build-plan :config-value :materialization-result :output-root
               :snapshot-date :opts :parser-runtime]
    :produces [:publication-result]
    :run materialize-publications-step}])

;; ── Release evaluation wiring ────────────────────────────────────────────────
;; Repo-relative authority sources (the build runs with cwd = the abc root). The
;; verifier reads these bytes itself; the build never precomputes closure
;; problems, parsed authority values, hashes, or a verdict.

(def release-authority-sources
  {:release-parser-identity-path "data/release-parser-identity-v1.edn"
   :decisions-path "docs/adr/decisions.edn"})

(defn- verify-result->json
  "JSON-safe (string-keyed) projection of the fresh verifier result written into
  publications-report.json. That derived report is excluded from the closed scan
  and from index identity."
  [verify-result]
  {"release_admissible" (:admissible? verify-result)
   "release_problems" (mapv (fn [p]
                              (into {} (map (fn [[k v]] [(name k) v])) p))
                            (:problems verify-result))
   "authority_hashes" (into {} (map (fn [[k v]] [(name k) v]))
                            (:authority-hashes verify-result))})

(defn- write-publications-report!
  [tmp-root render-report verify-result]
  (let [report-file (io/file tmp-root "publications" "publications-report.json")]
    (abc-json/write-deterministic-json-file!
     report-file
     (merge render-report (verify-result->json verify-result)))
    report-file))

(defn build-publication!
  [options]
  (let [{:keys [aozora-root config snapshot-date output-root replace]
         :as opts} (parse-args options)
        config-value (read-config config)]
    (when (string/blank? snapshot-date)
      (throw (ex-info "snapshot-date is required for build-publication"
                      {:config config})))
    ;; Prove source trust and compute the authenticated parser runtime identity
    ;; BEFORE any temporary-root write: an unprovable/dirty official source
    ;; throws here, before prepare-output-root!, so no partial root is created.
    ;; The rights gate is NOT asserted here: release admissibility (including
    ;; rights) is decided solely by verify-release-root! over the completed root,
    ;; so a rights-blocked or fixture build installs an inspectable diagnostic
    ;; root instead of failing before it can render.
    (let [source-trust-mode (get config-value "source_trust_mode")
          source-provenance (source-provenance! {:source-trust-mode source-trust-mode
                                                 :aozora-root aozora-root})
          parser-runtime (invoke-resolve-parser-runtime!
                          {:parser-profile (get config-value "parser_profile")
                           :source-trust-mode source-trust-mode
                           :release-parser-identity-path
                           (get config-value "release_parser_identity")})
          tmp-root (prepare-output-root! output-root replace)]
      (files/create-dirs! tmp-root)
      (let [opts (-> opts
                     (assoc :output-root tmp-root)
                     (update :concurrency resolve-concurrency))]
        (try
          ;; Assemble every identity-bearing file in the temporary candidate.
          (let [workflow-result
                (workflow/run-workflow!
                 {:workflow-id "soranoha.build-publication.v1"
                  :run-id (str "build-publication:" snapshot-date)
                  :output-root tmp-root
                  :initial-state {:aozora-root aozora-root
                                  :config-value config-value
                                  :snapshot-date snapshot-date
                                  :output-root tmp-root
                                  :source-provenance source-provenance
                                  :parser-runtime parser-runtime
                                  :opts opts}
                  :steps (build-publication-steps)})
                ;; Recompute admissibility fail-closed over the COMPLETED
                ;; candidate. verify-release-root! reads the candidate the index
                ;; names, recomputes the closure, and derives authority hashes
                ;; from the same bytes its loaders parsed. We pass paths only.
                verify-result
                (publication-release/verify-release-root!
                 {:root (str tmp-root)
                  :parser-authority-sources release-authority-sources
                  :rights-policy-path publication-policy/policy-path})]
            (write-publications-report!
             tmp-root (get-in workflow-result [:state :publication-result])
             verify-result)
            (let [final-root (promote-output-root! tmp-root output-root replace)]
              (println "build_publication_root:" (str final-root))
              (println "materialized_root:" (str (io/file final-root
                                                          "materialized-root")))
              (println "publications_root:" (str (io/file final-root
                                                          "publications")))
              (println "release_admissible:" (:admissible? verify-result))
              (if (:admissible? verify-result) 0 1)))
          (catch Throwable t
            ;; Strict exception (a workflow step throws) or a strict verifier
            ;; error (an unreadable/malformed authority the loaders reject):
            ;; delete the temporary candidate root; the prior target is
            ;; untouched.
            (files/delete-tree! tmp-root)
            (throw t)))))))
