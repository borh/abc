(ns abc.tools.soranoha
  (:refer-clojure :exclude [run!])
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.manifest :as manifest]
            [abc.tools.materialize-analysis :as materialize-analysis]
            [abc.tools.materialize-publication :as materialize-publication]
            [abc.tools.materialize-source-snapshot :as materialize-source-snapshot]
            [abc.tools.parser-evidence :as parser-evidence]
            [abc.tools.request-set-resolver :as request-set-resolver]
            [abc.tools.schema :as schema]
            [abc.tools.source-snapshot-workset :as source-snapshot-workset]
            [abc.tools.snapshot-index :as snapshot-index]
            [abc.tools.soranoha-layout-report :as layout-report]
            [abc.tools.soranoha-stage-publication :as stage-publication]
            [abc.tools.tar :as tar]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [java.nio.charset StandardCharsets]))

(defn request-set-labels []
  (request-set-resolver/request-set-labels))

(defn- read-resolved-request-set-file [path]
  (let [request-set (files/read-json path)
        request-set-schema (files/read-json "schemas/request-set.schema.json")
        computed-id (analysis-identity/request-set-id request-set)]
    (when-let [errors (schema/validation-errors request-set-schema request-set)]
      (throw (ex-info "Request set schema validation failed"
                      {:path (str path)
                       :errors errors})))
    (when-not (= computed-id (get request-set "request_set_id"))
      (throw (ex-info "request_set_id does not match request_set_identity_object"
                      {:path (str path)
                       :expected computed-id
                       :actual (get request-set "request_set_id")})))
    request-set))

(defn read-request-set [label-or-path]
  (let [file (io/file label-or-path)]
    (if (.isFile file)
      (read-resolved-request-set-file file)
      (request-set-resolver/resolve-request-set label-or-path))))

(defn list-request-sets! []
  (doseq [label (request-set-labels)]
    (println label))
  0)

(defn explain-request-set! [label]
  (let [request-set (read-request-set label)
        computed-id (analysis-identity/request-set-id request-set)]
    (println "label:" (get request-set "label"))
    (println "request_set_id:" (get request-set "request_set_id"))
    (println "computed_request_set_id:" computed-id)
    (println "source_definition_path:" (get-in request-set ["resolution"
                                                            "source_definition_path"]))
    (if (= computed-id (get request-set "request_set_id"))
      0
      (do
        (binding [*out* *err*]
          (println "request_set_id does not match request_set_identity_object"))
        1))))

(defn resolve-request-set!
  [label output-path subject-source-path]
  (let [request-set (request-set-resolver/resolve-request-set
                     label
                     {:subject-source-path subject-source-path})
        output-file (io/file output-path)
        subjects-count (count (get-in request-set
                                      ["request_set_identity_object"
                                       "subjects"]))]
    (manifest/write-json-file! output-file request-set)
    (println "request_set:" (str output-file))
    (println "request_set_label:" (get request-set "label"))
    (println "request_set_id:" (get request-set "request_set_id"))
    (println "subjects_count:" subjects-count)
    0))

(def generated-snapshot-layout-policy
  {"loose_artifact_kinds" ["tei" "plaintext"]
   "batched_artifact_kinds" ["analysis" "tokenized"]
   "batch_target_work_count" 250
   "archive_format" "tar.zst"})

(def generated-snapshot-failure-policy
  {"allow_nonzero_failures" true
   "max_failure_rate" nil
   "per_diagnostic_tolerances" {}})

(def generated-snapshot-schema-hash-paths
  ["schemas/manifest.schema.json"
   "schemas/request-set.schema.json"
   "schemas/snapshot-index.schema.json"
   "schemas/analysis-result.schema.json"])

(defn- snapshot-plan-if-present [label]
  (let [path (snapshot-index/snapshot-plan-path label)]
    (when (.isFile (io/file path))
      (snapshot-index/read-snapshot-plan label))))

(defn- source-snapshot-workset-path [request-set]
  (or (get-in request-set ["resolution" "source_snapshot_workset_path"])
      (when-let [source-snapshot-path (get-in request-set
                                              ["resolution"
                                               "subject_source_path"])]
        (when-let [parent (.getParentFile (io/file source-snapshot-path))]
          (str (io/file parent
                        request-set-resolver/source-snapshot-workset-file-name))))))

(defn- work-value [work k]
  (or (get work k)
      (get work (name k))))

(defn- work-file-path [work k]
  (or (work-value work (source-snapshot-workset/resolved-path-key k))
      (work-value work k)))

(defn- parser-ir-work-content-hash [work]
  (get-in (files/read-json (work-file-path work :parser_ir_path))
          ["source" "work_content_hash"]))

(defn- generated-analysis-metrics []
  [{"metric_id" "fixture-line-count"
    "value" 0
    "value_type" "integer"
    "denominator" nil
    "unit" "line"
    "status" "passed"}])

(defn- generated-materialization [request-set work]
  (let [snapshot-hash (get-in request-set ["request_set_identity_object"
                                           "corpus_snapshot_hash"])
        slug (work-value work :slug)
        work-content-hash (parser-ir-work-content-hash work)]
    {"artifact_subdir" slug
     "work_content_hash" work-content-hash
     "parser_ir_path" (work-file-path work :parser_ir_path)
     "source_manifest_path" (work-file-path work :source_manifest_path)
     "metadata_record_path" (work-file-path work :metadata_record_path)
     "persons_dir" "examples/v0/example-persons"
     "parser_identity" {"parser_build_hash" nil
                        "parser_config_hash" nil
                        "aat_parser_ir_mapping_hash" nil}
     "analysis_subject" {"logical_path" (work-file-path work :aat_path)
                         "git_ref" snapshot-hash}
     "analysis_metrics" (generated-analysis-metrics)}))

(defn- source-snapshot-date [request-set]
  (when-let [path (get-in request-set ["resolution" "subject_source_path"])]
    (get-in (files/read-json path)
            ["snapshot_identity_object" "snapshot_date"])))

(defn- generated-snapshot-plan [request-set]
  (let [label (get request-set "label")
        workset-path (source-snapshot-workset-path request-set)]
    (when-not workset-path
      (throw (ex-info "No snapshot plan or generated source-snapshot workset is available"
                      {:request_set_label label
                       :snapshot_plan_path (snapshot-index/snapshot-plan-path
                                            label)})))
    (let [workset (source-snapshot-workset/read-workset workset-path)
          snapshot-date (or (source-snapshot-date request-set)
                            "2026-07-07")]
      {"request_set_label" label
       "snapshot_label" (str "soranoha-snapshot-" snapshot-date "-00")
       "generated_at" (str snapshot-date "T00:00:00Z")
       "manifest_index_hash" (analysis-identity/hash-json-value
                              {"kind" "generated-loose-manifest-index-v1"
                               "request_set_id" (get request-set
                                                     "request_set_id")})
       "failure_policy" generated-snapshot-failure-policy
       "layout_policy" generated-snapshot-layout-policy
       "schema_hash_paths" generated-snapshot-schema-hash-paths
       "schema_hashes" []
       "parser_evidence_hashes" (parser-evidence/citable-hashes)
       "materializations" (mapv #(generated-materialization request-set %)
                                (work-value workset :works))
       "manifest_references" []})))

(defn- snapshot-plan [request-set]
  (or (snapshot-plan-if-present (get request-set "label"))
      (generated-snapshot-plan request-set)))

(defn build-snapshot-index [label-or-path]
  (let [request-set (read-request-set label-or-path)
        plan (snapshot-plan request-set)]
    (snapshot-index/build-snapshot-index-from-plan request-set plan)))

(defn snapshot-index! [label output-path]
  (let [snapshot (build-snapshot-index label)
        output-file (snapshot-index/write-snapshot-index! snapshot output-path)]
    (println "snapshot_index:" (str output-file))
    (println "snapshot_label:" (get snapshot "snapshot_label"))
    (println "snapshot_identity_hash:" (get snapshot "snapshot_identity_hash"))
    (println "request_set_label:" (get snapshot "request_set_label"))
    0))

(defn default-snapshot-root [label]
  (io/file "target" "soranoha" label))

(defn- delete-tree! [file]
  (let [file (io/file file)]
    (when (.exists file)
      (doseq [entry (reverse (file-seq file))]
        (.delete entry)))))

(defn- copy-file! [source target]
  (let [target (io/file target)]
    (when-let [parent (.getParentFile target)]
      (.mkdirs parent))
    (io/copy (io/file source) target)
    target))

(defn- compact-hashes [& values]
  (vec (keep identity values)))

(defn- relative-path [root file]
  (str (.relativize (.toPath (io/file root))
                    (.toPath (io/file file)))))

(defn- loose-manifest-reference [root manifest-file]
  {"manifest_path" (str (io/file manifest-file))
   "locator" {"kind" "loose"
              "path" (relative-path root manifest-file)}})

(defn- copied-parser-identity [materialization]
  (get materialization "parser_identity" {}))

(defn- parser-ir-value [materialization]
  (cond-> (files/read-json (get materialization "parser_ir_path"))
    (seq (get materialization "parser_ir_source"))
    (update "source" merge (get materialization "parser_ir_source"))))

(defn- write-parser-ir-file! [materialization target]
  (if (seq (get materialization "parser_ir_source"))
    (manifest/write-json-file! target (parser-ir-value materialization))
    (copy-file! (get materialization "parser_ir_path") target)))

(defn- parser-ir-manifest
  [{:keys [parser-ir-file warnings-file source-manifest parser-identity
           generated-at]}]
  (let [parser-ir (files/read-json parser-ir-file)
        source-identity (get source-manifest "manifest_identity_object")
        parser-ir-schema-hash (get parser-ir "schema_hash")
        identity-inputs {"corpus_snapshot_hash" (get source-identity
                                                     "corpus_snapshot_hash")
                         "work_content_hash" (get-in parser-ir
                                                     ["source"
                                                      "work_content_hash"])
                         "parser_build_hash" (get parser-identity
                                                  "parser_build_hash")
                         "parser_config_hash" (get parser-identity
                                                   "parser_config_hash")
                         "mapping_hash" (get parser-identity
                                             "aat_parser_ir_mapping_hash")
                         "parser_ir_schema_hash" parser-ir-schema-hash}
        identity-object (manifest/identity-object
                         identity-inputs
                         {:manifest-schema-hash (manifest/schema-hash
                                                 "schemas/manifest.schema.json")
                          :output-format-spec-hash parser-ir-schema-hash})
        warnings-file (when (and warnings-file (.exists (io/file warnings-file)))
                        (io/file warnings-file))]
    (manifest/artifact-manifest
     {:artifact-kind "parser-ir"
      :validation-status "passed"
      :identity-object identity-object
      :content (manifest/content parser-ir-file
                                 "application/json"
                                 "parser-ir.json"
                                 files/sha256-file)
      :sidecars (cond-> []
                  warnings-file
                  (conj {"role" "warnings"
                         "hash" (manifest/file-hash warnings-file)
                         "media_type" "application/jsonl"
                         "path_hint" "warnings.jsonl"}))
      :generated-at generated-at
      :activity-id "https://w3id.org/abc/activity/materialize-smoke-parser-ir"
      :agent "abc.tools.soranoha"
      :plan-hash nil
      :used (compact-hashes (get identity-inputs "corpus_snapshot_hash")
                            (get identity-inputs "work_content_hash")
                            (get identity-inputs "parser_build_hash")
                            (get identity-inputs "parser_config_hash")
                            (get identity-inputs "mapping_hash")
                            parser-ir-schema-hash)
      :was-derived-from (compact-hashes (get identity-inputs
                                             "corpus_snapshot_hash")
                                        (get identity-inputs
                                             "work_content_hash"))
      :notes "Generated as the parser-IR producer manifest for a Soranoha smoke snapshot."})))

(defn- write-parser-ir-artifacts!
  [{:keys [artifact-base materialization generated-at]}]
  (let [parser-dir (io/file artifact-base "parser-ir")
        parser-ir-file (write-parser-ir-file! materialization
                                              (io/file parser-dir
                                                       "parser-ir.json"))
        warnings-source (get materialization "warnings_path")
        warnings-file (when warnings-source
                        (copy-file! warnings-source
                                    (io/file parser-dir "warnings.jsonl")))
        source-manifest (files/read-json
                         (get materialization "source_manifest_path"))
        manifest-file (io/file parser-dir "parser-ir.manifest.json")]
    (manifest/write-json-file!
     manifest-file
     (parser-ir-manifest {:parser-ir-file parser-ir-file
                          :warnings-file warnings-file
                          :source-manifest source-manifest
                          :parser-identity (copied-parser-identity
                                            materialization)
                          :generated-at generated-at}))
    {:parser-ir-file parser-ir-file
     :manifest-file manifest-file}))

(defn- write-publication-artifacts!
  [{:keys [root artifact-base materialization parser-ir-file generated-at]}]
  (let [build-dir (io/file root ".build" "publication")
        plaintext-dir (io/file artifact-base "plaintext")
        tei-dir (io/file artifact-base "tei")
        result (materialize-publication/materialize-publication!
                {:parser-ir-path parser-ir-file
                 :source-manifest-path (get materialization
                                            "source_manifest_path")
                 :metadata-record-path (get materialization
                                            "metadata_record_path")
                 :persons-dir (get materialization "persons_dir")
                 :output-dir build-dir
                 :generated-at generated-at})
        plaintext-file (copy-file! (:plaintext result)
                                   (io/file plaintext-dir "plain.txt"))
        plaintext-manifest-file (copy-file! (:plaintext-manifest result)
                                            (io/file plaintext-dir
                                                     "plaintext.manifest.json"))
        tei-file (copy-file! (:tei result) (io/file tei-dir "tei.xml"))
        tei-manifest-file (copy-file! (:tei-manifest result)
                                      (io/file tei-dir "tei.manifest.json"))]
    (copy-file! (:tei-validation-result result)
                (io/file tei-dir "tei-validation-result.json"))
    (copy-file! (:preservation result)
                (io/file tei-dir "preservation.json"))
    (delete-tree! (io/file root ".build"))
    {:plaintext-file plaintext-file
     :plaintext-manifest-file plaintext-manifest-file
     :tei-file tei-file
     :tei-manifest-file tei-manifest-file}))

(defn- analysis-recipe [request-set]
  (let [recipe-id (get-in request-set ["resolved_recipe_labels" 0 "recipe_id"])]
    (when-not recipe-id
      (throw (ex-info "Request set has no resolved analysis recipe"
                      {:request_set_label (get request-set "label")})))
    (files/read-json (str "data/analysis-recipes/" recipe-id ".json"))))

(defn- analysis-subject [request-set materialization]
  (let [subjects (get-in request-set ["request_set_identity_object"
                                      "subjects"])
        selector {"source_id" (or (get materialization "source_id")
                                  (get-in materialization
                                          ["analysis_subject" "source_id"]))
                  "work_id" (or (get materialization "work_id")
                                (get-in materialization
                                        ["analysis_subject" "work_id"]))
                  "work_content_hash" (or (get materialization
                                               "work_content_hash")
                                          (get-in materialization
                                                  ["parser_ir_source"
                                                   "work_content_hash"]))}
        selected (cond
                   (integer? (get materialization "subject_index"))
                   (nth subjects (get materialization "subject_index") nil)

                   (some val selector)
                   (some (fn [subject]
                           (when (every? (fn [[k v]]
                                           (or (nil? v)
                                               (= v (get subject k))))
                                         selector)
                             subject))
                         subjects)

                   :else
                   (first subjects))]
    (when-not selected
      (throw (ex-info "Materialization entry does not match a request-set subject"
                      {:request_set_label (get request-set "label")
                       :selector selector
                       :subject_index (get materialization "subject_index")})))
    (merge selected (get materialization "analysis_subject" {}))))

(defn- write-analysis-artifacts!
  [{:keys [artifact-base materialization request-set producer-manifest-file
           generated-at]}]
  (let [analysis-dir (io/file artifact-base "analysis")
        result (materialize-analysis/materialize-analysis!
                {:producer-manifest (files/read-json producer-manifest-file)
                 :recipe (analysis-recipe request-set)
                 :subject (analysis-subject request-set materialization)
                 :metrics (get materialization "analysis_metrics")
                 :output-dir analysis-dir
                 :generated-at generated-at})]
    {:analysis-result-file (:analysis-result result)
     :analysis-manifest-file (:manifest result)}))

(defn- materialization-entries [label plan]
  (cond
    (seq (get plan "materializations"))
    (get plan "materializations")

    (get plan "materialization")
    [(get plan "materialization")]

    :else
    (throw (ex-info "Snapshot plan has no materialization section"
                    {:request_set_label label}))))

(defn- artifact-base [root materialization]
  (if-let [subdir (get materialization "artifact_subdir")]
    (io/file root "artifacts" "works" subdir)
    (io/file root "artifacts")))

(defn- runtime-environment []
  {"java_version" (System/getProperty "java.version")
   "os_name" (System/getProperty "os.name")
   "os_arch" (System/getProperty "os.arch")
   "available_processors" (.availableProcessors (Runtime/getRuntime))})

(defn- run-summary
  [{:keys [request-set snapshot manifest-count materialization-count]}]
  (let [identity-object (get snapshot "snapshot_index_identity_object")]
    {"schema_id" "https://w3id.org/abc/soranoha-run-summary-v0.json"
     "run_summary_version" "0.1.0"
     "generated_at" (get snapshot "generated_at")
     "request_set_label" (get request-set "label")
     "request_set_id" (get request-set "request_set_id")
     "snapshot_label" (get snapshot "snapshot_label")
     "snapshot_identity_hash" (get snapshot "snapshot_identity_hash")
     "source_snapshot_hash" (get identity-object "source_snapshot_hash")
     "snapshot_summary" (get snapshot "summary")
     "artifact_manifest_count" manifest-count
     "materialization_count" materialization-count
     "parser_evidence_hashes" (get identity-object "parser_evidence_hashes")
     "schema_hashes" (get identity-object "schema_hashes")
     "layout_policy_hash" (get identity-object "layout_policy_hash")
     "failure_policy_hash" (get identity-object "failure_policy_hash")
     "manifest_index_hash" (get identity-object "manifest_index_hash")
     "runtime_environment" (runtime-environment)
     "notes" "Operational reproduction summary. This file is not part of snapshot identity; verify snapshot_identity_hash and artifact hashes through snapshot-index.json."}))

(defn- write-run-summary! [path opts]
  (manifest/write-json-file! path (run-summary opts))
  path)

(defn- materialize-entry!
  [{:keys [root materialization request-set generated-at]}]
  (let [artifact-base (artifact-base root materialization)
        parser-result (write-parser-ir-artifacts!
                       {:artifact-base artifact-base
                        :materialization materialization
                        :generated-at generated-at})
        publication-result (write-publication-artifacts!
                            {:root root
                             :artifact-base artifact-base
                             :materialization materialization
                             :parser-ir-file (:parser-ir-file parser-result)
                             :generated-at generated-at})
        analysis-result (when (seq (get request-set
                                        "resolved_recipe_labels"))
                          (write-analysis-artifacts!
                           {:artifact-base artifact-base
                            :materialization materialization
                            :request-set request-set
                            :producer-manifest-file (:manifest-file
                                                     parser-result)
                            :generated-at generated-at}))]
    (cond-> [(:manifest-file parser-result)
             (:plaintext-manifest-file publication-result)
             (:tei-manifest-file publication-result)]
      analysis-result
      (conj (:analysis-manifest-file analysis-result)))))

(defn- materialize-snapshot-root! [label-or-path root]
  (let [request-set (read-request-set label-or-path)
        label (get request-set "label")
        plan (snapshot-plan request-set)
        materializations (materialization-entries label plan)]
    (delete-tree! root)
    (.mkdirs (io/file root))
    (let [generated-at (get plan "generated_at")
          manifest-files (mapcat
                          (fn [materialization]
                            (materialize-entry!
                             {:root root
                              :materialization materialization
                              :request-set request-set
                              :generated-at generated-at}))
                          materializations)
          generated-plan (assoc plan
                                "manifest_references"
                                (mapv #(loose-manifest-reference root %)
                                      manifest-files))
          snapshot (snapshot-index/build-snapshot-index-from-plan
                    request-set generated-plan)
          output-file (snapshot-index/write-snapshot-index!
                       snapshot
                       (str (io/file root "snapshot-index.json")))
          run-summary-file (io/file root "run-summary.json")]
      (write-run-summary! run-summary-file
                          {:request-set request-set
                           :snapshot snapshot
                           :manifest-count (count manifest-files)
                           :materialization-count (count materializations)})
      {:root root
       :snapshot snapshot
       :snapshot-index-file output-file
       :run-summary-file run-summary-file})))

(defn reproduce! [label-or-path]
  (let [request-set (read-request-set label-or-path)
        label (get request-set "label")
        {:keys [snapshot snapshot-index-file]} (materialize-snapshot-root!
                                                label-or-path
                                                (default-snapshot-root label))]
    (println "snapshot_index:" (str snapshot-index-file))
    (println "snapshot_label:" (get snapshot "snapshot_label"))
    (println "snapshot_identity_hash:" (get snapshot "snapshot_identity_hash"))
    (println "request_set_label:" (get snapshot "request_set_label"))
    0))

(defn snapshot-index-path [path]
  (let [file (io/file path)]
    (if (.isDirectory file)
      (let [snapshot-index-file (io/file file "snapshot-index.json")]
        (if (.isFile snapshot-index-file)
          snapshot-index-file
          (io/file file "index.json")))
      file)))

(defn- snapshot-root-path [path]
  (let [file (io/file path)]
    (if (.isDirectory file)
      file
      (or (.getParentFile file)
          (io/file ".")))))

(defn read-valid-snapshot-index [path]
  (let [snapshot-path (snapshot-index-path path)
        snapshot (files/read-json snapshot-path)
        snapshot-schema (files/read-json "schemas/snapshot-index.schema.json")]
    (when-let [errors (schema/validation-errors snapshot-schema snapshot)]
      (throw (ex-info "Snapshot index schema validation failed"
                      {:path (str snapshot-path)
                       :errors errors})))
    (snapshot-index/validate-snapshot-index! snapshot)
    snapshot))

(defn- compare-reference-field! [label reference expected actual]
  (when-not (= expected actual)
    (throw (ex-info (str "Referenced snapshot manifest " label
                         " mismatch")
                    {:locator (get reference "locator")
                     :field label
                     :expected expected
                     :actual actual}))))

(defn- validate-manifest-reference-fields! [reference manifest-value]
  (doseq [[field expected actual]
          [["artifact_id" (get reference "artifact_id")
            (get manifest-value "artifact_id")]
           ["artifact_kind" (get reference "artifact_kind")
            (get manifest-value "artifact_kind")]
           ["validation_status" (get reference "validation_status")
            (get manifest-value "validation_status")]
           ["content_hash" (get reference "content_hash")
            (get-in manifest-value ["content" "content_hash"])]]]
    (compare-reference-field! field reference expected actual)))

(defn- validate-loose-reference! [root reference]
  (let [relative-path (get-in reference ["locator" "path"])
        manifest-file (io/file root relative-path)]
    (when-not (.isFile manifest-file)
      (throw (ex-info "Referenced snapshot manifest does not exist"
                      {:locator (get reference "locator")
                       :path (str manifest-file)})))
    (compare-reference-field! "manifest_content_hash"
                              reference
                              (get reference "manifest_content_hash")
                              (manifest/file-hash manifest-file))
    (let [manifest-value (files/read-json manifest-file)]
      (validate-manifest-reference-fields! reference manifest-value))))

(defn- validate-archive-member-reference! [root reference]
  (let [archive-path (get-in reference ["locator" "archive_path"])
        member-path (get-in reference ["locator" "member_path"])
        archive-file (io/file root archive-path)]
    (when-not (.isFile archive-file)
      (throw (ex-info "Referenced snapshot archive does not exist"
                      {:locator (get reference "locator")
                       :path (str archive-file)})))
    (let [manifest-bytes (or (tar/member-bytes archive-file member-path)
                             (throw
                              (ex-info
                               "Referenced snapshot archive does not contain member"
                               {:path (str archive-file)
                                :member_path member-path})))]
      (compare-reference-field! "manifest_content_hash"
                                reference
                                (get reference "manifest_content_hash")
                                (str "sha256:" (hash/sha256-bytes
                                                manifest-bytes)))
      (validate-manifest-reference-fields!
       reference
       (json/read-json (String. manifest-bytes StandardCharsets/UTF_8))))))

(defn- validate-snapshot-root-references! [root snapshot]
  (doseq [reference (get snapshot "artifact_references" [])
          :let [locator-kind (get-in reference ["locator" "kind"])]]
    (case locator-kind
      "loose" (validate-loose-reference! root reference)
      "archive-member" (validate-archive-member-reference! root reference)
      nil (throw (ex-info "Snapshot artifact reference has no locator kind"
                          {:reference reference}))
      true))
  true)

(defn- validate-run-summary! [root snapshot]
  (let [run-summary-file (io/file root "run-summary.json")]
    (when (.isFile run-summary-file)
      (let [summary (files/read-json run-summary-file)]
        (doseq [[field expected actual]
                [["request_set_label" (get snapshot "request_set_label")
                  (get summary "request_set_label")]
                 ["snapshot_label" (get snapshot "snapshot_label")
                  (get summary "snapshot_label")]
                 ["snapshot_identity_hash" (get snapshot "snapshot_identity_hash")
                  (get summary "snapshot_identity_hash")]
                 ["snapshot_summary" (get snapshot "summary")
                  (get summary "snapshot_summary")]
                 ["parser_evidence_hashes"
                  (get-in snapshot ["snapshot_index_identity_object"
                                    "parser_evidence_hashes"])
                  (get summary "parser_evidence_hashes")]]]
          (when-not (= expected actual)
            (throw (ex-info (str "Run summary " field " mismatch")
                            {:path (str run-summary-file)
                             :field field
                             :expected expected
                             :actual actual})))))))
  true)

(defn- read-required-run-summary [root]
  (let [run-summary-file (io/file root "run-summary.json")]
    (when-not (.isFile run-summary-file)
      (throw (ex-info "Snapshot root has no run-summary.json"
                      {:path (str run-summary-file)})))
    (files/read-json run-summary-file)))

(defn- publication-report [snapshot run-summary validation]
  (let [identity-object (get snapshot "snapshot_index_identity_object")]
    {"schema_id" "https://w3id.org/abc/soranoha-publication-report-v0.json"
     "report_version" "0.1.0"
     "generated_at" (get snapshot "generated_at")
     "request_set_label" (get snapshot "request_set_label")
     "request_set_id" (get identity-object "request_set_id")
     "snapshot_label" (get snapshot "snapshot_label")
     "snapshot_identity_hash" (get snapshot "snapshot_identity_hash")
     "source_snapshot_hash" (get identity-object "source_snapshot_hash")
     "snapshot_summary" (get snapshot "summary")
     "artifact_kind_counts" (layout-report/artifact-kind-counts snapshot)
     "manifest_reference_count" (count (get snapshot "artifact_references" []))
     "artifact_manifest_count" (get run-summary "artifact_manifest_count")
     "materialization_count" (get run-summary "materialization_count")
     "parser_evidence_hashes" (get identity-object "parser_evidence_hashes")
     "schema_hashes" (get identity-object "schema_hashes")
     "layout_policy_hash" (get identity-object "layout_policy_hash")
     "failure_policy_hash" (get identity-object "failure_policy_hash")
     "manifest_index_hash" (get identity-object "manifest_index_hash")
     "runtime_environment" (get run-summary "runtime_environment")
     "validation" validation
     "notes" "Citable reproduction evidence report. This file is not part of snapshot identity; cite snapshot_identity_hash, request_set_id, and artifact hashes from snapshot-index.json."}))

(defn publication-report! [snapshot-root output-path]
  (let [root (io/file snapshot-root)
        snapshot (read-valid-snapshot-index root)]
    (when-not (.isDirectory root)
      (throw (ex-info "publication-report requires a snapshot root directory"
                      {:path (str root)})))
    (validate-snapshot-root-references! root snapshot)
    (validate-run-summary! root snapshot)
    (let [run-summary (read-required-run-summary root)
          validation {"snapshot_root_valid" true
                      "checked_manifest_references" (count (get snapshot
                                                                "artifact_references"
                                                                []))
                      "run_summary_valid" true}
          report (publication-report snapshot run-summary validation)
          output-file (io/file output-path)]
      (manifest/write-json-file! output-file report)
      (println "publication_report:" (str output-file))
      (println "snapshot_identity_hash:" (get snapshot
                                              "snapshot_identity_hash"))
      (println "request_set_label:" (get snapshot "request_set_label"))
      0)))

(defn layout-report! [snapshot-root output-path]
  (let [root (io/file snapshot-root)
        snapshot (read-valid-snapshot-index root)]
    (when-not (.isDirectory root)
      (throw (ex-info "layout-report requires a snapshot root directory"
                      {:path (str root)})))
    (validate-snapshot-root-references! root snapshot)
    (let [validation {"snapshot_root_valid" true
                      "checked_manifest_references" (count (get snapshot
                                                                "artifact_references"
                                                                []))}
          report (layout-report/build-report root snapshot validation)
          output-file (io/file output-path)]
      (manifest/write-json-file! output-file report)
      (println "layout_report:" (str output-file))
      (println "snapshot_identity_hash:" (get snapshot
                                              "snapshot_identity_hash"))
      (println "request_set_label:" (get snapshot "request_set_label"))
      0)))

(defn stage-publication! [snapshot-root output-root]
  (let [root (io/file snapshot-root)
        output-root (io/file output-root)
        snapshot (read-valid-snapshot-index root)]
    (when-not (.isDirectory root)
      (throw (ex-info "stage-publication requires a snapshot root directory"
                      {:path (str root)})))
    (validate-snapshot-root-references! root snapshot)
    (validate-run-summary! root snapshot)
    (let [{:keys [index-file snapshot archive-count]}
          (stage-publication/stage-publication! {:snapshot-root root
                                                 :staged-root output-root
                                                 :snapshot snapshot})]
      (println "staged_index:" (str index-file))
      (println "snapshot_identity_hash:" (get snapshot
                                              "snapshot_identity_hash"))
      (println "request_set_label:" (get snapshot "request_set_label"))
      (println "archive_count:" archive-count)
      0)))

(defn explain-snapshot! [path]
  (let [snapshot (read-valid-snapshot-index path)
        summary (get snapshot "summary")]
    (println "snapshot_label:" (get snapshot "snapshot_label"))
    (println "snapshot_identity_hash:" (get snapshot "snapshot_identity_hash"))
    (println "request_set_label:" (get snapshot "request_set_label"))
    (println "request_set_id:" (get-in snapshot ["snapshot_index_identity_object"
                                                 "request_set_id"]))
    (println "total_artifacts:" (get summary "total_artifacts"))
    (println "success_count:" (get summary "success_count"))
    (println "failure_count:" (get summary "failure_count"))
    (println "failure_rate:" (get summary "failure_rate"))
    0))

(defn validate! [snapshot-root]
  (let [snapshot (read-valid-snapshot-index snapshot-root)]
    (when (.isDirectory (io/file snapshot-root))
      (validate-snapshot-root-references! (snapshot-root-path snapshot-root)
                                          snapshot)
      (validate-run-summary! (snapshot-root-path snapshot-root) snapshot))
    (println "snapshot_valid: true")
    (println "snapshot_label:" (get snapshot "snapshot_label"))
    (println "snapshot_identity_hash:" (get snapshot "snapshot_identity_hash"))
    0))

(defn- generated-at-from-date [snapshot-date]
  (str snapshot-date "T00:00:00Z"))

(defn source-snapshot!
  [input-root output-root snapshot-scope snapshot-date]
  (let [output-root-file (io/file output-root)
        workset-file (io/file output-root-file "source-snapshot.workset.edn")
        snapshot-file (io/file output-root-file "source-snapshot.json")
        {:keys [works-count]} (source-snapshot-workset/write-workset!
                               {:input-root input-root
                                :output-path (str workset-file)
                                :snapshot-scope snapshot-scope
                                :snapshot-date snapshot-date})
        {:keys [snapshot-hash]} (materialize-source-snapshot/materialize-source-snapshot!
                                 {:workset-path (str workset-file)
                                  :output-path (str snapshot-file)
                                  :generated-at (generated-at-from-date
                                                 snapshot-date)})]
    (println "source_snapshot_workset:" (str workset-file))
    (println "source_snapshot:" (str snapshot-file))
    (println "source_snapshot_hash:" snapshot-hash)
    (println "works_count:" works-count)
    0))

(defn usage []
  (string/join
   "\n"
   ["usage: soranoha <command> [args]"
    ""
    "commands:"
    "  list-request-sets"
    "  explain-request-set <label-or-request-set-json>"
    "  resolve-request-set <label> <output-path> <source-snapshot-path>"
    "  snapshot-index <label-or-request-set-json> <output-path>"
    "  reproduce <label-or-request-set-json>"
    "  validate <snapshot-root-or-index>"
    "  explain-snapshot <snapshot-index>"
    "  publication-report <snapshot-root> <output-path>"
    "  layout-report <snapshot-root> <output-path>"
    "  stage-publication <snapshot-root> <output-root>"
    "  source-snapshot <materialized-root> <output-root> <snapshot-scope> <snapshot-date>"]))

(def commands
  {"list-request-sets" {:args 0
                        :run (fn [] (list-request-sets!))}
   "explain-request-set" {:args 1
                          :run explain-request-set!}
   "resolve-request-set" {:args 3
                          :run resolve-request-set!}
   "snapshot-index" {:args 2
                     :run snapshot-index!}
   "reproduce" {:args 1
                :run reproduce!}
   "validate" {:args 1
               :run validate!}
   "explain-snapshot" {:args 1
                       :run explain-snapshot!}
   "publication-report" {:args 2
                         :run publication-report!}
   "layout-report" {:args 2
                    :run layout-report!}
   "stage-publication" {:args 2
                        :run stage-publication!}
   "source-snapshot" {:args 4
                      :run source-snapshot!}})

(defn run! [args]
  (let [[command & rest-args] args]
    (cond
      (or (nil? command) (= command "help") (= command "--help"))
      (do (println (usage)) 0)

      (not (contains? commands command))
      (do (binding [*out* *err*]
            (println "unknown command:" command)
            (println (usage)))
          2)

      (not= (:args (get commands command)) (count rest-args))
      (do (binding [*out* *err*]
            (println "wrong arity for command:" command)
            (println (usage)))
          2)

      :else
      (try
        (apply (:run (get commands command)) rest-args)
        (catch clojure.lang.ExceptionInfo e
          (binding [*out* *err*]
            (println (ex-message e)))
          1)))))

(defn -main [& args]
  (System/exit (run! args)))
