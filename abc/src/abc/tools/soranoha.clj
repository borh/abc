(ns abc.tools.soranoha
  (:refer-clojure :exclude [run!])
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.annotation-join-stats :as annotation-join-stats]
            [abc.tools.annotation-join-stats-run :as annotation-join-stats-run]
            [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [abc.tools.materialize-source-snapshot :as materialize-source-snapshot]
            [abc.tools.publication-policy :as publication-policy]
            [abc.tools.publication-release :as publication-release]
            [abc.tools.request-set-resolver :as request-set-resolver]
            [abc.tools.schema :as schema]
            [abc.tools.source-snapshot-workset :as source-snapshot-workset]
            [abc.tools.snapshot-index :as snapshot-index]
            [abc.tools.soranoha-build-publication :as build-publication]
            [abc.tools.soranoha-layout-report :as layout-report]
            [abc.tools.soranoha-stage-publication :as stage-publication]
            [babashka.fs :as fs]
            [babashka.cli :as cli]
            [clojure.java.io :as io]))

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
    (if (fs/regular-file? file)
      (read-resolved-request-set-file file)
      (request-set-resolver/resolve-request-set label-or-path))))

(defn list-request-sets! []
  (doseq [label (request-set-resolver/request-set-labels)]
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

(defn- print-snapshot-summary!
  "Print the standard snapshot date / identity-hash / source-selection-hash
  triple to stdout (shared by the snapshot-consuming commands)."
  [snapshot]
  (let [identity-object (get snapshot "snapshot_index_identity_object")]
    (println "snapshot_date:" (get snapshot "snapshot_date"))
    (println "snapshot_identity_hash:" (get snapshot "snapshot_identity_hash"))
    (println "source_selection_hash:" (get identity-object "source_selection_hash"))))

(defn- snapshot-root-path [path]
  (let [file (io/file path)]
    (if (fs/directory? file)
      file
      (or (fs/parent file)
          (io/file ".")))))

;; Repo-relative parser-authority + rights-policy source paths the retained
;; projections use to RECOMPUTE current admissibility. verify-release-root!
;; authenticates the decision-bound release-parser-identity record.
(def release-authority-sources
  {:release-parser-identity-path "data/release-parser-identity-v1.edn"
   :decisions-path "docs/adr/decisions.edn"})

(defn- print-release-verdict!
  "Recompute and print the CURRENT release admissibility of an installed root
  plus the loader-derived authority hashes. This never trusts or rewrites the
  build-time result in publications/publications-report.json; it recomputes the
  closure and re-authenticates the candidate the index names."
  [root]
  (let [{:keys [admissible? authority-hashes]}
        (publication-release/verify-release-root!
         {:root root
          :parser-authority-sources release-authority-sources
          :rights-policy-path publication-policy/policy-path})]
    (println "release_admissible:" admissible?)
    (println "decisions_file_hash:" (:decisions-file authority-hashes))
    (println "record_file_hash:" (:record-file authority-hashes))
    (println "rights_policy_file_hash:" (:rights-policy-file authority-hashes))))

(defn- publication-report [snapshot validation]
  (let [identity-object (get snapshot "snapshot_index_identity_object")]
    {"schema_id" "https://w3id.org/abc/soranoha-publication-report-v0.json"
     "report_version" "0.2.0"
     "generated_at" (get snapshot "generated_at")
     "snapshot_date" (get snapshot "snapshot_date")
     "snapshot_identity_hash" (get snapshot "snapshot_identity_hash")
     "source_selection_hash" (get identity-object "source_selection_hash")
     "candidate_ref" (get identity-object "candidate_ref")
     "qualification_identity_ref" (get identity-object "qualification_identity_ref")
     "parser_config_hash" (get identity-object "parser_config_hash")
     "artifact_set_hash" (get identity-object "artifact_set_hash")
     "failure_set_hash" (get identity-object "failure_set_hash")
     "layout_policy_hash" (get identity-object "layout_policy_hash")
     "failure_policy_hash" (get identity-object "failure_policy_hash")
     "schema_hashes" (get identity-object "schema_hashes")
     "snapshot_summary" (get snapshot "summary")
     "artifact_kind_counts" (layout-report/artifact-kind-counts snapshot)
     "manifest_reference_count" (count (get snapshot "artifact_references" []))
     "failure_count" (count (get snapshot "failures" []))
     "validation" validation
     "notes" "Citable reproduction evidence report. This file is not part of snapshot identity; cite snapshot_identity_hash, source_selection_hash, parser_config_hash, and artifact hashes from snapshot-index.json."}))

(defn- write-publication-report-file! [root snapshot output-path]
  (snapshot-index/assert-closed-root! root snapshot)
  (let [validation {"snapshot_root_valid" true
                    "checked_manifest_references" (count (get snapshot
                                                              "artifact_references"
                                                              []))}
        report (publication-report snapshot validation)
        output-file (io/file output-path)]
    (manifest/write-json-file! output-file report)
    {:file output-file
     :report report}))

(defn publication-report! [snapshot-root output-path]
  (let [root (io/file snapshot-root)
        snapshot (snapshot-index/read-valid-snapshot-index root)]
    (when-not (fs/directory? root)
      (throw (ex-info "publication-report requires a snapshot root directory"
                      {:path (str root)})))
    (let [{:keys [file]} (write-publication-report-file! root
                                                         snapshot
                                                         output-path)]
      (println "publication_report:" (str file))
      (println "snapshot_identity_hash:" (get snapshot
                                              "snapshot_identity_hash"))
      (println "snapshot_date:" (get snapshot "snapshot_date"))
      0)))

(defn- write-layout-report-file! [root snapshot output-path]
  (snapshot-index/assert-closed-root! root snapshot)
  (let [validation {"snapshot_root_valid" true
                    "checked_manifest_references" (count (get snapshot
                                                              "artifact_references"
                                                              []))}
        report (layout-report/build-report root snapshot validation)
        output-file (io/file output-path)]
    (manifest/write-json-file! output-file report)
    {:file output-file
     :report report}))

(defn layout-report! [snapshot-root output-path]
  (let [root (io/file snapshot-root)
        snapshot (snapshot-index/read-valid-snapshot-index root)]
    (when-not (fs/directory? root)
      (throw (ex-info "layout-report requires a snapshot root directory"
                      {:path (str root)})))
    (let [{:keys [file]} (write-layout-report-file! root
                                                    snapshot
                                                    output-path)]
      (println "layout_report:" (str file))
      (println "snapshot_identity_hash:" (get snapshot
                                              "snapshot_identity_hash"))
      (println "snapshot_date:" (get snapshot "snapshot_date"))
      0)))

(defn stage-publication! [snapshot-root output-root]
  (publication-policy/assert-release-allowed!)
  (let [root (io/file snapshot-root)
        output-root (io/file output-root)
        snapshot (snapshot-index/read-valid-snapshot-index root)]
    (when-not (fs/directory? root)
      (throw (ex-info "stage-publication requires a snapshot root directory"
                      {:path (str root)})))
    (snapshot-index/assert-closed-root! root snapshot)
    (let [{:keys [index-file snapshot archive-count]}
          (stage-publication/stage-publication! {:snapshot-root root
                                                 :staged-root output-root
                                                 :snapshot snapshot})]
      (println "staged_index:" (str index-file))
      (println "snapshot_identity_hash:" (get snapshot
                                              "snapshot_identity_hash"))
      (println "snapshot_date:" (get snapshot "snapshot_date"))
      (println "archive_count:" archive-count)
      0)))

(defn explain-snapshot! [path]
  (let [snapshot (snapshot-index/read-valid-snapshot-index path)
        summary (get snapshot "summary")
        identity-object (get snapshot "snapshot_index_identity_object")]
    (print-snapshot-summary! snapshot)
    (println "parser_config_hash:" (get identity-object "parser_config_hash"))
    (println "failure_set_hash:" (get identity-object "failure_set_hash"))
    (println "total_artifacts:" (get summary "total_artifacts"))
    (println "success_count:" (get summary "success_count"))
    (println "failure_count:" (get summary "failure_count"))
    (println "failure_rate:" (get summary "failure_rate"))
    (when (fs/directory? (io/file path))
      (print-release-verdict! (io/file path)))
    0))

(defn validate! [snapshot-root]
  (let [snapshot (snapshot-index/read-valid-snapshot-index snapshot-root)]
    (when (fs/directory? snapshot-root)
      (snapshot-index/assert-closed-root! (snapshot-root-path snapshot-root)
                                          snapshot))
    (println "snapshot_valid: true")
    (println "snapshot_date:" (get snapshot "snapshot_date"))
    (println "snapshot_identity_hash:" (get snapshot "snapshot_identity_hash"))
    (when (fs/directory? (io/file snapshot-root))
      (print-release-verdict! (io/file snapshot-root)))
    0))

(defn- generated-at-from-date [snapshot-date]
  (str snapshot-date "T00:00:00Z"))

(defn- write-source-snapshot-root!
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
    {:workset-file workset-file
     :snapshot-file snapshot-file
     :snapshot (files/read-json snapshot-file)
     :snapshot-hash snapshot-hash
     :works-count works-count}))

(defn source-snapshot!
  [input-root output-root snapshot-scope snapshot-date]
  (let [{:keys [workset-file snapshot-file snapshot-hash works-count]}
        (write-source-snapshot-root! input-root
                                     output-root
                                     snapshot-scope
                                     snapshot-date)]
    (println "source_snapshot_workset:" (str workset-file))
    (println "source_snapshot:" (str snapshot-file))
    (println "source_snapshot_hash:" snapshot-hash)
    (println "works_count:" works-count)
    0))

(defn- positional-command [command-name doc arg-keys run]
  {:cmds [command-name]
   :fn (fn [{:keys [opts]}]
         (apply run (map opts arg-keys)))
   :doc doc
   :args->opts arg-keys
   :spec (into {}
               (map (fn [arg-key]
                      [arg-key {:ref (str "<" (name arg-key) ">")
                                :coerce :string
                                :require true}]))
               arg-keys)
   :restrict true})

(def command-table
  [(positional-command "list-request-sets"
                       "List checked-in request sets."
                       []
                       (fn [] (list-request-sets!)))
   (positional-command "explain-request-set"
                       "Explain a request set and its identity."
                       [:label-or-request-set-json]
                       (fn [value] (explain-request-set! value)))
   (positional-command "resolve-request-set"
                       "Resolve a request set against a source snapshot."
                       [:label :output-path :source-snapshot-path]
                       (fn [& args] (apply resolve-request-set! args)))
   (positional-command "validate"
                       "Validate a snapshot root or index."
                       [:snapshot-root-or-index]
                       (fn [value] (validate! value)))
   (positional-command "explain-snapshot"
                       "Explain a snapshot index and its identity."
                       [:snapshot-index]
                       (fn [value] (explain-snapshot! value)))
   (positional-command "publication-report"
                       "Write a publication report for a snapshot."
                       [:snapshot-root :output-path]
                       (fn [& args] (apply publication-report! args)))
   (positional-command "layout-report"
                       "Write a static-layout comparison report."
                       [:snapshot-root :output-path]
                       (fn [& args] (apply layout-report! args)))
   (positional-command "stage-publication"
                       "Stage a publication layout from a snapshot."
                       [:snapshot-root :output-root]
                       (fn [& args] (apply stage-publication! args)))
   (positional-command "source-snapshot"
                       "Materialize a publication source snapshot."
                       [:materialized-root :output-root :snapshot-scope :snapshot-date]
                       (fn [& args] (apply source-snapshot! args)))
   {:cmds ["build-publication"]
    :fn (fn [{:keys [opts]}]
          (build-publication/build-publication! opts))
    :doc "Build publication artifacts from an official Aozora checkout."
    :spec {:aozora-root {:ref "DIR"
                         :coerce :string
                         :desc "Official aozorabunko checkout root."
                         :require true}
           :config {:ref "FILE"
                    :coerce :string
                    :desc "Soranoha publication build config JSON."
                    :require true}
           :snapshot-date {:ref "DATE"
                           :coerce :string
                           :desc "Snapshot date, YYYY-MM-DD."
                           :require true}
           :output-root {:ref "DIR"
                         :coerce :string
                         :desc "Final output root."
                         :require true}
           :replace {:coerce :boolean
                     :desc "Replace an existing output root after a successful build."}
           :concurrency {:ref "N"
                         :coerce :long
                         :default 0
                         :validate {:pred #(>= % 0) :ex-msg "must be >= 0"}
                         :desc "Worker threads (0 = all cores)."}}
    :order [:aozora-root :config :snapshot-date :output-root
            :replace :concurrency :help]
    :restrict true}
   (positional-command "annotation-join-stats"
                       "Compare parser IR with morphological token output."
                       [:parser-ir-dir :tokens-dir :out-dir]
                       (fn [parser-ir-dir tokens-dir out-dir]
                         (annotation-join-stats/run-join-stats!
                          parser-ir-dir tokens-dir out-dir)))
   (positional-command "annotation-join-stats-run"
                       "Run annotation join statistics from a plan."
                       [:plan-json :out-root]
                       (fn [plan-file out-root]
                         (annotation-join-stats-run/run-annotation-join-stats-run!
                          {:plan-file plan-file
                           :out-root out-root
                           :converter-bin (System/getenv "AB_AAT_TO_PARSER_IR_BIN")
                           :tokenizer-bin (System/getenv "AB_MORPH_RUN_BIN")})))])

(defn normalize-help-args [args]
  (let [args (vec args)]
    (cond
      (empty? args) ["--help"]
      (= ["help"] args) ["--help"]
      (and (= "help" (first args)) (= 2 (count args)))
      [(second args) "--help"]
      :else args)))

(defn- usage-error! [data]
  (binding [*out* *err*]
    (println (cli/format-command-error data)))
  (throw (ex-info "soranoha CLI usage error"
                  {:soranoha/usage-error true})))

(defn run! [args]
  (try
    (let [result (binding [cli/*exit-fn* (fn [_] nil)]
                   (cli/dispatch command-table
                                 (normalize-help-args args)
                                 {:prog "soranoha"
                                  :help true
                                  :error-fn usage-error!}))]
      (if (integer? result) result 0))
    (catch clojure.lang.ExceptionInfo e
      (if (:soranoha/usage-error (ex-data e))
        2
        (do (binding [*out* *err*]
              (println (ex-message e)))
            1)))))

(defn -main [& args]
  (System/exit (run! args)))
