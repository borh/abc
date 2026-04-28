(ns abc.tools.validate-design-bundle
  (:require ;; logging first so its load-time SLF4J filter is in place
            ;; before deps that emit chatty INFO logs (Aristotle/Jena/
            ;; Apache SSHD) get pulled in by other requires.
   [abc.tools.logging :as logging]
   [abc.tools.files :as files]
   [abc.tools.manifest-index :as manifest-index]
   [abc.tools.manifest-to-rdf :as manifest-to-rdf]
   [abc.tools.manifest :as manifest]
   [abc.tools.materialize-import :as materialize]
   [abc.tools.schema :as schema]
   [abc.tools.metadata-record :as metadata-record]
   [abc.tools.person-record :as person-record]
   [abc.tools.shacl :as shacl]
   [abc.tools.schematron :as schematron]
   [abc.tools.tei :as tei]
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
        comparison-report-schema (files/read-json "schemas/comparison-report.schema.json")
        tei-validation-result-schema (files/read-json "schemas/tei-validation-result.schema.json")]
    (doseq [[path schema] [["schemas/manifest.schema.json" manifest-schema]
                           ["schemas/parser-ir.schema.json" parser-ir-schema]
                           ["schemas/diagnostic.schema.json" diagnostic-schema]
                           ["schemas/run-summary.schema.json" run-summary-schema]
                           ["schemas/manifest-inputs.schema.json" manifest-inputs-schema]
                           ["schemas/comparison-report.schema.json" comparison-report-schema]
                           ["schemas/tei-validation-result.schema.json" tei-validation-result-schema]]]
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
    (validate-json! tei-validation-result-schema
                    "examples/v0/example-work/tei-validation-result.json")
    (let [manifest (files/read-json "examples/v0/example-work/manifest.json")
          validation-sidecars (filter #(= "validation-result" (get % "role"))
                                      (get manifest "sidecars"))]
      (when-not (some #(= "tei-validation-result.json" (get % "path_hint"))
                      validation-sidecars)
        (throw (ex-info "example TEI manifest must reference tei-validation-result.json"
                        {:manifest "examples/v0/example-work/manifest.json"}))))
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
                "schemas/tei-profile.sch"
                "schemas/tei-profile.rng"
                "examples/v0/example-work/tei.xml"
                "fixtures/tei/valid/rashomon-minimal.xml"
                "fixtures/tei/valid/source-span-local-ref.xml"
                "fixtures/tei/valid/transcription-enrichment-declared.xml"
                "fixtures/tei/invalid/missing-title.xml"
                "fixtures/tei/invalid/missing-source-work-id.xml"
                "fixtures/tei/invalid/gaiji-missing-ref.xml"
                "fixtures/tei/invalid/ruby-missing-reading.xml"
                "fixtures/tei/invalid/source-span-external-ref.xml"
                "fixtures/tei/warnings/figure-missing-desc.xml"
                "fixtures/tei/warnings/transcription-enrichment-undeclared.xml"))

(defn- render-tei-violation
  "Render a violation map to a single line. Label is supplied by the
  harness pass per-file rather than copied into every violation map."
  [label {:keys [severity line column message]}]
  (let [sev (cond
              (keyword? severity) (string/upper-case (name severity))
              (string? severity) (string/upper-case severity)
              :else "VIOLATION")]
    (str sev ": " label " " (or line "?") ":" (or column "?")
         " — " (or message "(no message)"))))

(defn validate-tei!
  "Validate every TEI document in `xml-paths` against the schema at
  `schema-path`. Warnings are logged via Telemere but do not fail the
  step; errors and fatals are aggregated and thrown at the end."
  [^String schema-path xml-paths]
  (when (or (nil? schema-path) (= "" schema-path))
    (throw (ex-info "TEI RelaxNG schema path must be set."
                    {:error :missing-schema-path})))
  (let [per-file-results
        (mapv (fn [path]
                (let [{:keys [violations]}
                      (tei/validate! {:schema-path schema-path
                                      :xml-path (str path)
                                      :label (str path)})]
                  {:label (str path) :violations violations}))
              xml-paths)
        all-warnings (mapcat (fn [{:keys [label violations]}]
                               (->> violations
                                    (filter #(= :warning (:severity %)))
                                    (map #(vector label %))))
                             per-file-results)
        all-failures (mapcat (fn [{:keys [label violations]}]
                               (->> violations
                                    (filter #(#{:error :fatal} (:severity %)))
                                    (map #(vector label %))))
                             per-file-results)]
    (doseq [[label v] all-warnings]
      (tel/log! :warn (render-tei-violation label v)))
    (when (seq all-failures)
      (throw (ex-info "TEI RelaxNG validation failed"
                      {:errors (mapv (fn [[label v]] (render-tei-violation label v))
                                     all-failures)})))))

(defn- schematron-error? [finding]
  (= :error (:severity finding)))

(defn- schematron-warning? [finding]
  (= :warning (:severity finding)))

(defn- rule-ids [findings]
  (set (map :rule-id findings)))

(defn- render-schematron-finding [{:keys [label rule-id severity message]}]
  (str (string/upper-case (name severity)) ": "
       label " " rule-id " — " message))

(defn- validate-schematron-valid-fixture! [schema-path path]
  (let [{:keys [findings]} (schematron/validate! {:schema-path schema-path
                                                  :xml-path path
                                                  :label path})
        errors (filter schematron-error? findings)]
    (when (seq errors)
      (throw (ex-info "unexpected Schematron error in valid TEI fixture"
                      {:fixture path
                       :errors (mapv render-schematron-finding errors)})))))

(defn- validate-schematron-warning-fixture! [schema-path path expected-rules]
  (let [{:keys [findings]} (schematron/validate! {:schema-path schema-path
                                                  :xml-path path
                                                  :label path})
        actual-warnings (rule-ids (filter schematron-warning? findings))
        missing (set/difference expected-rules actual-warnings)
        errors (filter schematron-error? findings)]
    (when (seq errors)
      (throw (ex-info "unexpected Schematron error in warning TEI fixture"
                      {:fixture path
                       :errors (mapv render-schematron-finding errors)})))
    (when (seq missing)
      (throw (ex-info "missing expected Schematron warning rule"
                      {:fixture path
                       :missing (sort missing)
                       :actual (sort actual-warnings)})))))

(defn- validate-schematron-invalid-fixture! [schema-path path expected-rules]
  (let [{:keys [findings]} (schematron/validate! {:schema-path schema-path
                                                  :xml-path path
                                                  :label path})
        actual-errors (rule-ids (filter schematron-error? findings))
        missing (set/difference expected-rules actual-errors)]
    (when (seq missing)
      (throw (ex-info "missing expected Schematron rule"
                      {:fixture path
                       :missing (sort missing)
                       :actual (sort actual-errors)})))))

(defn validate-tei-schematron!
  [{:keys [schema-path valid-fixtures warning-fixtures invalid-fixtures]}]
  (doseq [path valid-fixtures]
    (validate-schematron-valid-fixture! schema-path path))
  (doseq [[path expected-rules] warning-fixtures]
    (validate-schematron-warning-fixture! schema-path path expected-rules))
  (doseq [[path expected-rules] invalid-fixtures]
    (validate-schematron-invalid-fixture! schema-path path expected-rules)))

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

(defn- validate-persons-directory!
  "For every JSON file under `persons-dir`: load, schema-validate,
  and verify the embedded person_record_schema_hash matches the live
  schema's JCS hash. Returns a map person_id → person-record map."
  [persons-dir person-schema-path]
  (let [live-schema-hash (manifest/schema-hash person-schema-path)
        files (->> (.listFiles (io/file persons-dir))
                   (filter #(string/ends-with? (.getName ^java.io.File %) ".json"))
                   sort)]
    (into {}
          (for [^java.io.File f files]
            (let [record (files/read-json (str f))]
              (person-record/validate! record)
              (let [embedded (get record "person_record_schema_hash")]
                (when-not (= embedded live-schema-hash)
                  (throw (ex-info
                          (str "person_record_schema_hash mismatch in " f
                               ": record has " embedded
                               ", live schema hash is " live-schema-hash)
                          {:path (str f)
                           :embedded embedded
                           :live live-schema-hash}))))
              [(get record "person_id") record])))))

(defn validate-metadata-bundle!
  "Validate the example-work metadata-record bundle:
  1. Every person file in `persons-dir` validates against
     person-record.schema.json + schema-hash precondition.
  2. The work's metadata-record.json validates against
     metadata-record.schema.json + schema-hash precondition.
  3. Recompute metadata_record_hash; compare against the
     `manifest_identity_object.metadata_record_hash` in manifest.json.
  4. For every contributors[i]: recompute the referenced person's
     person_record_hash from the on-disk file and fail if it does
     not match contributors[i].person_record_hash.
  5. Compose the work + persons graph; SHACL validate against shapes.
  6. Compose work + persons → ttl; byte-equal to ttl-path."
  [{:keys [record-path manifest-path persons-dir
           record-schema-path person-schema-path
           ttl-path shapes-graph]}]
  (let [persons-by-id (validate-persons-directory! persons-dir person-schema-path)
        record (files/read-json record-path)]
    (metadata-record/validate! record)
    (let [computed-schema-hash (manifest/schema-hash record-schema-path)
          expected-schema-hash (get record "metadata_record_schema_hash")]
      (when-not (= computed-schema-hash expected-schema-hash)
        (throw (ex-info (str "metadata_record_schema_hash mismatch: "
                             "record has " expected-schema-hash
                             ", live schema hash is " computed-schema-hash)
                        {:record-path record-path
                         :computed computed-schema-hash
                         :expected expected-schema-hash}))))
    (let [computed (metadata-record/record-hash record)
          expected (get-in (files/read-json manifest-path)
                           ["manifest_identity_object" "metadata_record_hash"])]
      (when-not (= computed expected)
        (throw (ex-info (str "metadata_record_hash mismatch: manifest has "
                             expected ", record-hash computed " computed)
                        {:record-path record-path
                         :manifest-path manifest-path
                         :computed computed
                         :expected expected}))))
    (doseq [contributor (get record "contributors")]
      (let [pid (get contributor "person_id")
            referenced (get contributor "person_record_hash")
            body (get persons-by-id pid)]
        (when-not body
          (throw (ex-info (str "contributor " pid " has no matching file in " persons-dir)
                          {:person-id pid
                           :persons-dir persons-dir})))
        (let [recomputed (person-record/record-hash body)]
          (when-not (= referenced recomputed)
            (throw (ex-info
                    (str "contributor reference for person_id " pid
                         " is stale: metadata-record references " referenced
                         ", recomputed from " persons-dir "/" pid ".json is "
                         recomputed)
                    {:person-id pid
                     :referenced referenced
                     :recomputed recomputed
                     :persons-dir persons-dir}))))))
    (shacl/validate! {:shapes-graph shapes-graph
                      :data-graph (metadata-record/record+persons->graph
                                   record persons-by-id)
                      :label record-path})
    (let [generated (metadata-record/record+persons->ttl record persons-by-id)
          expected (slurp ttl-path)]
      (when-not (= expected generated)
        (throw (ex-info (str "metadata-record.ttl parity mismatch with " ttl-path)
                        {:record-path record-path
                         :ttl-path ttl-path}))))))

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
        (tel/log! :info "shacl shapes ok")
        (tel/log! :info "==> Validating metadata record + persons bundle")
        (let [shapes (shacl/load-shapes-graph)]
          (validate-metadata-bundle!
           {:record-path "examples/v0/example-work/metadata-record.json"
            :manifest-path "examples/v0/example-work/manifest.json"
            :persons-dir "examples/v0/example-persons"
            :record-schema-path "schemas/metadata-record.schema.json"
            :person-schema-path "schemas/person-record.schema.json"
            :ttl-path "examples/v0/example-work/metadata-record.ttl"
            :shapes-graph shapes}))
        (tel/log! :info "metadata bundle ok"))
      (tel/log! :info "==> Checking imported ab-validator output")
      (validate-ab-validator-output!)
      (tel/log! :info "ab-validator output ok")
      (tel/log! :info "==> Checking canonicalization fixtures")
      (validate-canonicalization!)
      (tel/log! :info "canonicalization fixtures ok")
      (tel/log! :info "==> Checking XML fixtures")
      (validate-xml!)
      (tel/log! :info "xml fixtures ok")
      (tel/log! :info "==> Validating TEI against P5 RelaxNG")
      (validate-tei! (System/getenv "TEI_SCHEMA_PATH")
                     ["examples/v0/example-work/tei.xml"])
      (tel/log! :info "tei rng validation ok")
      (tel/log! :info "==> Validating TEI against project RelaxNG")
      (validate-tei! "schemas/tei-profile.rng"
                     ["examples/v0/example-work/tei.xml"
                      "fixtures/tei/valid/rashomon-minimal.xml"
                      "fixtures/tei/valid/source-span-local-ref.xml"
                      "fixtures/tei/valid/transcription-enrichment-declared.xml"
                      "fixtures/tei/warnings/figure-missing-desc.xml"
                      "fixtures/tei/warnings/transcription-enrichment-undeclared.xml"
                      "fixtures/tei/invalid/missing-title.xml"
                      "fixtures/tei/invalid/missing-source-work-id.xml"
                      "fixtures/tei/invalid/gaiji-missing-ref.xml"
                      "fixtures/tei/invalid/ruby-missing-reading.xml"
                      "fixtures/tei/invalid/source-span-external-ref.xml"])
      (tel/log! :info "tei project rng validation ok")
      (tel/log! :info "==> Validating TEI against project Schematron")
      (validate-tei-schematron!
       {:schema-path "schemas/tei-profile.sch"
        :valid-fixtures ["examples/v0/example-work/tei.xml"
                         "fixtures/tei/valid/rashomon-minimal.xml"
                         "fixtures/tei/valid/source-span-local-ref.xml"
                         "fixtures/tei/valid/transcription-enrichment-declared.xml"]
        :warning-fixtures {"fixtures/tei/warnings/figure-missing-desc.xml"
                           #{"abc-figure-accessibility"}
                           "fixtures/tei/warnings/transcription-enrichment-undeclared.xml"
                           #{"abc-transcription-vs-annotation"}}
        :invalid-fixtures {"fixtures/tei/invalid/missing-title.xml"
                           #{"abc-tei-header-title"}
                           "fixtures/tei/invalid/missing-source-work-id.xml"
                           #{"abc-tei-header-source-work-id"}
                           "fixtures/tei/invalid/gaiji-missing-ref.xml"
                           #{"abc-gaiji-reference"}
                           "fixtures/tei/invalid/ruby-missing-reading.xml"
                           #{"abc-ruby-complete"}
                           "fixtures/tei/invalid/source-span-external-ref.xml"
                           #{"abc-source-span-reference"}}})
      (tel/log! :info "tei schematron validation ok")
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
