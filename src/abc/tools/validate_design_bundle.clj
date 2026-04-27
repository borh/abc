(ns abc.tools.validate-design-bundle
  (:require [abc.tools.files :as files]
            [abc.tools.logging :as logging]
            [abc.tools.manifest-index :as manifest-index]
            [abc.tools.manifest-to-rdf :as manifest-to-rdf]
            [abc.tools.manifest :as manifest]
            [abc.tools.materialize-import :as materialize]
            [abc.tools.schema :as schema]
            [abc.tools.shacl :as shacl]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as string]
            [taoensso.telemere :as tel]))

(def required-manifest-input-keys
  #{"producer"
    "producer_version"
    "work_id"
    "corpus_snapshot_hash"
    "work_content_hash"
    "parser_build_hash"
    "parser_config_hash"
    "parser_ir_schema_hash"
    "diagnostic_schema_hash"
    "warning_sidecar_hash"
    "run_summary_hash"
    "comparison_report_hash"})

(defn manifest-input-errors [manifest-inputs]
  (let [missing (sort (set/difference required-manifest-input-keys
                                      (set (keys manifest-inputs))))
        missing-error (when (seq missing)
                        (str "ab-validator manifest inputs missing keys: "
                             (string/join ", " missing)))
        hash-errors (for [[k v] (sort-by key manifest-inputs)
                          :when (string/ends-with? k "_hash")
                          :when (not (and (string? v)
                                          (re-matches files/hash-pattern v)))]
                      (str "ab-validator manifest input " k
                           " is not a sha256 hash: " v))]
    (vec (concat (when missing-error [missing-error])
                 hash-errors))))

(defn run-summary-errors [events]
  (let [event-types (mapv #(get % "event") events)
        start-count (count (filter #{"run-start"} event-types))
        complete-count (count (filter #{"run-complete"} event-types))
        run-ids (->> events
                     (keep #(get % "run_id"))
                     set)]
    (vec
     (concat
      (when (not= 1 start-count)
        ["ab-validator run summary must contain exactly one run-start event"])
      (when (not= 1 complete-count)
        ["ab-validator run summary must contain exactly one run-complete event"])
      (when (not= ["run-start"] (subvec event-types 0 (min 1 (count event-types))))
        ["ab-validator run summary must start with run-start"])
      (when (not= ["run-complete"] (subvec event-types (max 0 (dec (count event-types)))))
        ["ab-validator run summary must end with run-complete"])
      (when (< 1 (count run-ids))
        ["ab-validator run summary events must all use the same run_id"])
      (for [event events
            :when (not (contains? event "run_id"))]
        (str "run summary event is missing run_id: " event))))))

(defn comparison-report-errors [comparison-report]
  (vec
   (concat
    (when (not= "abc.ab-validator-comparison.v0"
                (get comparison-report "report_schema"))
      ["ab-validator comparison report has an unexpected report_schema"])
    (when-not (seq (get comparison-report "parser_candidates"))
      ["ab-validator comparison report must list parser_candidates"]))))

(defn schema-hash-errors [manifest-inputs]
  (let [expected-parser-ir (manifest/schema-hash "schemas/parser-ir.schema.json")
        expected-diagnostic (manifest/schema-hash "schemas/diagnostic.schema.json")
        actual-parser-ir (get manifest-inputs "parser_ir_schema_hash")
        actual-diagnostic (get manifest-inputs "diagnostic_schema_hash")]
    (vec
     (concat
      (when (and actual-parser-ir (not= expected-parser-ir actual-parser-ir))
        [(str "ab-validator parser_ir_schema_hash " actual-parser-ir
              " does not match ABC parser IR schema hash " expected-parser-ir)])
      (when (and actual-diagnostic (not= expected-diagnostic actual-diagnostic))
        [(str "ab-validator diagnostic_schema_hash " actual-diagnostic
              " does not match ABC diagnostic schema hash " expected-diagnostic)])))))

(defn parser-ir-schema-hash-errors [parser-ir]
  (let [expected-parser-ir (manifest/schema-hash "schemas/parser-ir.schema.json")
        actual-parser-ir (get parser-ir "schema_hash")]
    (vec
     (when (not= expected-parser-ir actual-parser-ir)
       [(str "ab-validator parser IR schema_hash " actual-parser-ir
             " does not match ABC parser IR schema hash " expected-parser-ir)]))))

(defn validation-errors [schema value]
  (schema/validation-errors schema value))

(defn validate-json! [schema path]
  (schema/validate-json! schema path))

(defn validate-json-lines! [schema path {:keys [require-nonempty]}]
  (let [values (files/read-json-lines path)]
    (when (and require-nonempty (empty? values))
      (throw (ex-info (str path " must contain at least one JSON object")
                      {:path (str path)})))
    (schema/validate-jsonl! schema values path)))

(defn schema-valid! [schema path]
  (schema/schema-valid! schema path))

(defn run-command! [& command]
  (let [process (ProcessBuilder. command)
        _ (.inheritIO process)
        started (.start process)
        exit-code (.waitFor started)]
    (when-not (zero? exit-code)
      (throw (ex-info (str "Command failed: " (string/join " " command))
                      {:command command
                       :exit-code exit-code})))))

(defn validate-json-schemas! [extra-manifest-paths]
  (let [manifest-schema (files/read-json "schemas/manifest.schema.json")
        parser-ir-schema (files/read-json "schemas/parser-ir.schema.json")
        diagnostic-schema (files/read-json "schemas/diagnostic.schema.json")
        run-summary-schema (files/read-json "schemas/run-summary.schema.json")
        manifest-inputs-schema (files/read-json "schemas/manifest-inputs.schema.json")
        comparison-report-schema (files/read-json "schemas/comparison-report.schema.json")]
    (doseq [[path schema] [["schemas/manifest.schema.json" manifest-schema]
                           ["schemas/parser-ir.schema.json" parser-ir-schema]
                           ["schemas/diagnostic.schema.json" diagnostic-schema]
                           ["schemas/run-summary.schema.json" run-summary-schema]
                           ["schemas/manifest-inputs.schema.json" manifest-inputs-schema]
                           ["schemas/comparison-report.schema.json" comparison-report-schema]]]
      (schema-valid! schema path))
    (doseq [path (concat ["examples/v0/example-work/source.manifest.json"
                          "examples/v0/example-work/manifest.json"
                          "examples/v0/example-work/failure-manifest.example.json"]
                         extra-manifest-paths)]
      (validate-json! manifest-schema path))
    (doseq [path ["examples/v0/example-work/parser-ir.json"
                  "examples/ab-validator-output/parser-ir.json"]]
      (validate-json! parser-ir-schema path))
    (doseq [path ["examples/v0/example-work/warnings.jsonl"
                  "examples/ab-validator-output/warnings.jsonl"]]
      (validate-json-lines! diagnostic-schema path {:require-nonempty true}))
    (validate-json-lines! run-summary-schema
                          "examples/ab-validator-output/run-summary.jsonl"
                          {:require-nonempty false})
    (validate-json! manifest-inputs-schema
                    "examples/ab-validator-output/manifest-inputs.json")
    (validate-json! comparison-report-schema
                    "examples/ab-validator-output/comparison-report.json")
    (when-not (validation-errors manifest-schema {})
      (throw (ex-info "manifest schema accepted an empty object"
                      {:schema "schemas/manifest.schema.json"})))))

(defn check-errors! [errors]
  (when (seq errors)
    (throw (ex-info (string/join "\n" errors)
                    {:errors errors}))))

(defn validate-ab-validator-output! []
  (let [manifest-inputs (files/read-json (files/path "examples" "ab-validator-output" "manifest-inputs.json"))]
    (check-errors! (manifest-input-errors manifest-inputs))
    (check-errors! (schema-hash-errors manifest-inputs)))
  (check-errors!
   (parser-ir-schema-hash-errors
    (files/read-json (files/path "examples" "ab-validator-output" "parser-ir.json"))))
  (check-errors!
   (run-summary-errors
    (files/read-json-lines (files/path "examples" "ab-validator-output" "run-summary.jsonl"))))
  (check-errors!
   (comparison-report-errors
    (files/read-json (files/path "examples" "ab-validator-output" "comparison-report.json")))))

(defn validate-canonicalization! []
  (let [expected "9d49ff018a43ac2b24323276424cc325e3a5d0a22716144c8800f9fec0911f0a"
        actual (files/sha256-file (files/path "fixtures" "canonicalization"
                                              "manifest-identity-object.canonical.json"))
        array-a (files/sha256-file (files/path "fixtures" "canonicalization"
                                               "array-ordering-negative-a.json"))
        array-b (files/sha256-file (files/path "fixtures" "canonicalization"
                                               "array-ordering-negative-b.json"))]
    (when-not (= expected actual)
      (throw (ex-info "canonical identity fixture hash mismatch"
                      {:expected expected
                       :actual actual})))
    (when (= array-a array-b)
      (throw (ex-info "array-ordering negative fixtures produced the same digest"
                      {:digest array-a})))))

(defn validate-xml! []
  (run-command! "xmllint" "--noout"
                "schemas/tei-profile.odd"
                "examples/v0/example-work/tei.xml"))

(defn validate-git-cliff! []
  (run-command! "git-cliff" "--config" "cliff.toml" "--unreleased" "--strip" "header"
                "--output" "/tmp/abc-changelog-check.md"))

;; Spec format: "<severity>: <focus> <path> — <message> (<label>)".
;; We append "[<source>]" because the source shape IRI is high-signal
;; for debugging and the spec did not pin punctuation, only fields.
(defn- render-violation [{:keys [severity focus-node path message label source]}]
  (str (or severity "Violation") ": "
       (or focus-node "?") " "
       (or path "")
       (when message (str " — " message))
       (when label (str " (" label ")"))
       (when source (str " [" source "]"))))

(defn validate-shacl!
  "Validate every manifest in `manifest-paths` against the shapes graph.
  Aggregates all violations and throws once at the end if any are found."
  [shapes-graph manifest-paths]
  (let [violations
        (reduce
         (fn [acc path]
           (try
             (let [m (files/read-json path)
                   data (manifest-to-rdf/manifest->graph m)]
               (shacl/validate! {:shapes-graph shapes-graph
                                 :data-graph data
                                 :label (str path)})
               acc)
             (catch clojure.lang.ExceptionInfo e
               (into acc (:errors (ex-data e))))))
         []
         manifest-paths)]
    (when (seq violations)
      (throw (ex-info "SHACL validation failed"
                      {:errors (mapv render-violation violations)})))))

(defn validate-design-bundle! []
  (let [materialized-dir (.toFile (java.nio.file.Files/createTempDirectory
                                   "abc-materialized-import"
                                   (make-array java.nio.file.attribute.FileAttribute 0)))]
    (try
      (tel/log! :info "==> Materializing imported ab-validator output")
      (let [materialized (materialize/materialize-import!
                          {:input-dir (files/path "examples" "ab-validator-output")
                           :output-dir materialized-dir
                           :generated-at materialize/default-generated-at})]
        (tel/log! :info "materialized import ok")
        (tel/log! :info "==> Validating JSON schemas and examples")
        (validate-json-schemas! (vals materialized))
        (tel/log! :info "json schema validation ok")
        (tel/log! :info "==> Checking materialized manifest index")
        (manifest-index/validate-no-reproducibility-conflicts!
         (manifest-index/index-manifest-files (vals materialized)))
        (tel/log! :info "materialized manifest index ok")
        (tel/log! :info "==> Checking materialized RDF views")
        (doseq [manifest-path (vals materialized)]
          (manifest-to-rdf/manifest->ttl (files/read-json manifest-path)))
        (tel/log! :info "materialized RDF views ok")
        (tel/log! :info "==> Validating SHACL shapes")
        (let [shapes (shacl/load-shapes-graph)
              targets (concat (vals materialized)
                              ["examples/v0/example-work/manifest.json"
                               "examples/v0/example-work/failure-manifest.example.json"])]
          (validate-shacl! shapes targets))
        (tel/log! :info "shacl shapes ok"))
      (tel/log! :info "==> Checking imported ab-validator output")
      (validate-ab-validator-output!)
      (tel/log! :info "ab-validator output ok")
      (tel/log! :info "==> Checking canonicalization fixtures")
      (validate-canonicalization!)
      (tel/log! :info "canonicalization fixtures ok")
      (tel/log! :info "==> Checking XML fixtures")
      (validate-xml!)
      (tel/log! :info "xml fixtures ok")
      (tel/log! :info "==> Checking git-cliff configuration")
      (validate-git-cliff!)
      (tel/log! :info "git-cliff config ok")
      (tel/log! :info "design bundle validation ok")
      (finally
        (doseq [file (reverse (file-seq materialized-dir))]
          (.delete file))))))

(defn -main [& _args]
  (logging/install-cli-handler!)
  (try
    (validate-design-bundle!)
    (catch Throwable t
      (tel/log! {:level :error :error t} "design bundle validation failed")
      (when-let [errors (:errors (ex-data t))]
        (doseq [error errors]
          (tel/log! :error (str "- " error))))
      (System/exit 1))))
