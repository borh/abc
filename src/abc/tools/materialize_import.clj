(ns abc.tools.materialize-import
  (:require [abc.tools.files :as files]
            [abc.tools.logging :as logging]
            [abc.tools.manifest :as manifest]
            [clojure.java.io :as io]
            [clojure.tools.cli :as cli]
            [taoensso.telemere :as tel]))

(def default-generated-at "2026-04-26T00:00:00Z")

(defn imported-file [input-dir name]
  (io/file input-dir name))

(defn run-summary-status [input-dir]
  ;; v0 simplification: reads entire file. TODO: stream to handle
  ;; run summaries with thousands of work results.
  (let [summary-file (io/file input-dir "run-summary.jsonl")
        events (when (.exists summary-file)
                 (files/read-json-lines summary-file))]
    (or (some #(when (= "run-complete" (get % "event"))
                 (get % "status"))
              events)
        "warning")))

(defn parser-ir-manifest [input-dir manifest-inputs generated-at]
  (let [parser-ir-file (imported-file input-dir "parser-ir.json")
        warnings-file (imported-file input-dir "warnings.jsonl")
        manifest-schema-hash (manifest/schema-hash "schemas/manifest.schema.json")
        identity-object (manifest/identity-object
                         manifest-inputs
                         {:manifest-schema-hash manifest-schema-hash
                          :output-format-spec-hash (get manifest-inputs "parser_ir_schema_hash")})]
    (manifest/artifact-manifest
     {:artifact-kind "parser-ir"
      :validation-status (run-summary-status input-dir)
      :identity-object identity-object
      :content (manifest/content parser-ir-file
                                 "application/json"
                                 "parser-ir.json"
                                 files/sha256-file)
      :sidecars [{"role" "warnings"
                  "hash" (str "sha256:" (files/sha256-file warnings-file))
                  "media_type" "application/jsonl"
                  "path_hint" "warnings.jsonl"}]
      :generated-at generated-at
      :activity-id "https://w3id.org/abc/activity/materialize-imported-parser-ir"
      :agent "abc.tools.materialize-import"
      :plan-hash nil
      :used [(get manifest-inputs "work_content_hash")
             (get manifest-inputs "parser_build_hash")
             (get manifest-inputs "parser_config_hash")
             (get manifest-inputs "parser_ir_schema_hash")]
      :was-derived-from [(get manifest-inputs "work_content_hash")]
      :notes "Generated from imported ab-validator parser IR output."})))

(defn warnings-manifest [input-dir manifest-inputs generated-at]
  (let [warnings-file (imported-file input-dir "warnings.jsonl")
        manifest-schema-hash (manifest/schema-hash "schemas/manifest.schema.json")
        identity-object (manifest/identity-object
                         manifest-inputs
                         {:manifest-schema-hash manifest-schema-hash
                          :output-format-spec-hash (get manifest-inputs "diagnostic_schema_hash")})]
    (manifest/artifact-manifest
     {:artifact-kind "warnings"
      :validation-status (run-summary-status input-dir)
      :identity-object identity-object
      :content (manifest/content warnings-file
                                 "application/jsonl"
                                 "warnings.jsonl"
                                 files/sha256-file)
      :sidecars []
      :generated-at generated-at
      :activity-id "https://w3id.org/abc/activity/materialize-imported-warnings"
      :agent "abc.tools.materialize-import"
      :plan-hash nil
      :used [(get manifest-inputs "work_content_hash")
             (get manifest-inputs "parser_build_hash")
             (get manifest-inputs "parser_config_hash")
             (get manifest-inputs "diagnostic_schema_hash")]
      :was-derived-from [(get manifest-inputs "work_content_hash")]
      :notes "Generated from imported ab-validator warning sidecar output."})))

(defn materialize-import! [{:keys [input-dir output-dir generated-at]
                            :or {generated-at default-generated-at}}]
  (let [input-dir (io/file input-dir)
        output-dir (io/file output-dir)
        manifest-inputs (files/read-json (imported-file input-dir "manifest-inputs.json"))
        parser-file (io/file output-dir "parser-ir.manifest.json")
        warnings-file (io/file output-dir "warnings.manifest.json")]
    (manifest/write-json-file! parser-file
                               (parser-ir-manifest input-dir manifest-inputs generated-at))
    (manifest/write-json-file! warnings-file
                               (warnings-manifest input-dir manifest-inputs generated-at))
    {:parser-ir parser-file
     :warnings warnings-file}))

(defn usage []
  (tel/log! :warn "Usage: clojure -M:abc/materialize-import <input-dir> <output-dir> [--generated-at instant]"))

(def cli-options
  [[nil "--generated-at INSTANT" "UTC generation timestamp for deterministic fixtures"
    :id :generated-at]])

(defn -main [& args]
  (logging/install-cli-handler!)
  (let [{:keys [options arguments errors]} (cli/parse-opts args cli-options)
        [input-dir output-dir positional-generated-at & extra] arguments
        generated-at (or (:generated-at options)
                         positional-generated-at
                         default-generated-at)]
    (if (or (seq errors) (nil? input-dir) (nil? output-dir) (seq extra))
      (do
        (doseq [error errors]
          (tel/log! :error error))
        (usage)
        (System/exit 2))
      (do
        (materialize-import! {:input-dir input-dir
                              :output-dir output-dir
                              :generated-at generated-at})
        (tel/log! :info (str "materialized imported parser output to " output-dir))))))
