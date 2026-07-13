(ns abc.tools.materialize-import
  (:require [abc.tools.cli :as abc-cli]
            [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [babashka.fs :as fs]
            [taoensso.telemere :as tel]))

(def default-generated-at "2026-04-26T00:00:00Z")

(defn imported-file [input-dir name]
  (fs/file input-dir name))

(defn run-summary-status [input-dir]
  ;; Reads entire file into memory; large run summaries may OOM.
  
  (let [summary-file (fs/file input-dir "run-summary.jsonl")
        events (when (fs/exists? summary-file)
                 (files/read-json-lines summary-file))]
    (or (some #(when (= "run-complete" (get % "event"))
                 (get % "status"))
              events)
        "warning")))

(defn- sidecar [role file media-type path-hint]
  {"role" role
   "hash" (str "sha256:" (files/sha256-file file))
   "media_type" media-type
   "path_hint" path-hint})

(defn- parser-ir-sidecars [input-dir]
  (let [warnings-file (imported-file input-dir "warnings.jsonl")
        divergence-bundle-file (imported-file input-dir "divergence.json")
        divergence-file (imported-file input-dir "divergence.jsonl")
        source-region-coverage-file (imported-file input-dir "source-region-coverage.json")]
    (cond-> [(sidecar "warnings" warnings-file "application/jsonl" "warnings.jsonl")]
      (fs/exists? divergence-bundle-file)
      (conj (sidecar "mapping-divergence"
                     divergence-bundle-file
                     "application/json"
                     "divergence.json"))

      (and (not (fs/exists? divergence-bundle-file))
           (fs/exists? divergence-file))
      (conj (sidecar "mapping-divergence"
                     divergence-file
                     "application/jsonl"
                     "divergence.jsonl"))

      (fs/exists? source-region-coverage-file)
      (conj (sidecar "source-region-coverage"
                     source-region-coverage-file
                     "application/json"
                     "source-region-coverage.json")))))

(defn parser-ir-manifest [input-dir manifest-inputs generated-at]
  (let [parser-ir-file (imported-file input-dir "parser-ir.json")
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
      :sidecars (parser-ir-sidecars input-dir)
      :generated-at generated-at
      :activity-id "https://w3id.org/abc/activity/materialize-imported-parser-ir"
      :agent "abc.tools.materialize-import"
      :plan-hash nil
      :used [(get manifest-inputs "work_content_hash")
             (get manifest-inputs "parser_build_hash")
             (get manifest-inputs "parser_config_hash")
             (get manifest-inputs "mapping_hash")
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
  (let [input-dir (fs/file input-dir)
        output-dir (fs/file output-dir)
        manifest-inputs (files/read-json (imported-file input-dir "manifest-inputs.json"))
        parser-file (fs/file output-dir "parser-ir.manifest.json")
        warnings-file (fs/file output-dir "warnings.manifest.json")]
    (manifest/write-json-file! parser-file
                               (parser-ir-manifest input-dir manifest-inputs generated-at))
    (manifest/write-json-file! warnings-file
                               (warnings-manifest input-dir manifest-inputs generated-at))
    {:parser-ir parser-file
     :warnings warnings-file}))

(defn usage [_summary]
  "Usage: clojure -M:abc/materialize-import <input-dir> <output-dir> [--generated-at instant]")

(def cli-options
  [[nil "--generated-at INSTANT" "UTC generation timestamp for deterministic fixtures"
    :id :generated-at]])

(defn -main [& args]
  (abc-cli/run-cli!
   args
   {:cli-options cli-options
    :min-args    2
    :max-args    3
    :usage-fn    usage
    :run         (fn [{:keys [options arguments]}]
                   (let [[input-dir output-dir positional-generated-at] arguments
                         generated-at (or (:generated-at options)
                                          positional-generated-at
                                          default-generated-at)]
                     (materialize-import! {:input-dir input-dir
                                           :output-dir output-dir
                                           :generated-at generated-at})
                     (tel/log! :info (str "materialized imported parser output to "
                                          output-dir))))}))
