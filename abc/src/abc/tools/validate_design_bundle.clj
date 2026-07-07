(ns abc.tools.validate-design-bundle
  (:require ;; logging first so its load-time SLF4J filter is in place
            ;; before deps that emit chatty INFO logs (Aristotle/Jena/
            ;; Apache SSHD) get pulled in by other requires.
   [abc.tools.logging :as logging]
   [abc.tools.aat-parser-ir-compat :as compat]
   [abc.tools.files :as files]
   [abc.tools.iiif :as iiif]
   [abc.tools.linked-art :as linked-art]
   [abc.tools.malli :as am]
   [abc.tools.manifest-index :as manifest-index]
   [abc.tools.manifest-to-rdf :as manifest-to-rdf]
   [abc.tools.manifest :as manifest]
   [abc.tools.materialize-import :as materialize]
   [abc.tools.materialize-publication :as publication]
   [abc.tools.schema :as schema]
   [abc.tools.snapshot-index :as snapshot-index]
   [abc.tools.metadata-record :as metadata-record]
   [abc.tools.parser-evidence :as parser-evidence]
   [abc.tools.person-drift :as person-drift]
   [abc.tools.person-record :as person-record]
   [abc.tools.shacl :as shacl]
   [abc.tools.schematron :as schematron]
   [abc.tools.source-region-contract :as source-region]
   [abc.tools.tei :as tei]
   [arachne.aristotle :as aa]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as string]
   [taoensso.telemere :as tel]))

(def legacy-parser-ir-schema-hashes
  #{"sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396"
    "sha256:8e56871965e647e40ade08fd9dd580a3516d33905be17957cc79750bd42ea64d"
    "sha256:da916a3a92f64d985cb98f9b2ddc7f562e660fd0c3dbe0c902392d3764b0158a"
    "sha256:c081f2365e2159e6e608733c4eb4e6fdf1fa80203ccd3d5e1f2afc533da8d411"})

(defn accepted-parser-ir-schema-hashes []
  (conj legacy-parser-ir-schema-hashes
        (manifest/schema-hash "schemas/parser-ir.schema.json")))

(defn- parser-ir-schema-hash-accepted? [value]
  (contains? (accepted-parser-ir-schema-hashes) value))

(defn schema-hash-errors [manifest-inputs]
  (let [expected-parser-ir (manifest/schema-hash "schemas/parser-ir.schema.json")
        expected-diagnostic (manifest/schema-hash "schemas/diagnostic.schema.json")
        actual-parser-ir (get manifest-inputs "parser_ir_schema_hash")
        actual-diagnostic (get manifest-inputs "diagnostic_schema_hash")]
    (vec
     (concat
      (when (and actual-parser-ir
                 (not (parser-ir-schema-hash-accepted? actual-parser-ir)))
        [(str "ab-validator parser_ir_schema_hash " actual-parser-ir
              " does not match ABC parser IR schema hash " expected-parser-ir)])
      (when (and actual-diagnostic (not= expected-diagnostic actual-diagnostic))
        [(str "ab-validator diagnostic_schema_hash " actual-diagnostic
              " does not match ABC diagnostic schema hash " expected-diagnostic)])))))

(defn parser-ir-schema-hash-errors [parser-ir]
  (let [expected-parser-ir (manifest/schema-hash "schemas/parser-ir.schema.json")
        actual-parser-ir (get parser-ir "schema_hash")]
    (vec
     (when (not (parser-ir-schema-hash-accepted? actual-parser-ir))
       [(str "ab-validator parser IR schema_hash " actual-parser-ir
             " does not match ABC parser IR schema hash " expected-parser-ir)]))))

(defn- duplicate-paragraph-id-errors [paragraphs]
  (->> paragraphs
       (map #(get % "id"))
       frequencies
       (keep (fn [[paragraph-id count]]
               (when (> count 1)
                 (str "parser IR paragraphs[] contains duplicate id " paragraph-id))))
       sort))

(defn- node-range-label [start end]
  (str start ".." end))

(defn- source-note-node-in-range? [nodes start end]
  (boolean
   (some #(= "source-note" (get % "type"))
         (subvec nodes start end))))

(defn parser-ir-paragraph-coherence-errors [parser-ir]
  (let [nodes (vec (get parser-ir "nodes" []))
        paragraphs (vec (get parser-ir "paragraphs" []))]
    (vec
     (concat
      (duplicate-paragraph-id-errors paragraphs)
      (loop [remaining paragraphs
             previous-end 0
             errors []]
        (if-let [paragraph (first remaining)]
          (let [paragraph-id (get paragraph "id")
                node-range (get paragraph "node_range")
                start (get node-range "start")
                end (get node-range "end")
                outside? (not (and (integer? start)
                                   (integer? end)
                                   (<= 0 start end (count nodes))))
                non-monotonic? (and (integer? start)
                                    (< start previous-end))
                source-note-missing? (and (not outside?)
                                          (= "source-note" (get paragraph "role"))
                                          (= "direct" (get paragraph "classification"))
                                          (not (source-note-node-in-range?
                                                nodes start end)))
                errors (cond-> errors
                         outside?
                         (conj (str "parser IR paragraph " paragraph-id
                                    " node_range " (node-range-label start end)
                                    " is outside nodes[] length " (count nodes)))

                         (and (not outside?) non-monotonic?)
                         (conj (str "parser IR paragraph " paragraph-id
                                    " node_range starts before previous paragraph end "
                                    previous-end))

                         source-note-missing?
                         (conj (str "parser IR paragraph " paragraph-id
                                    " has role source-note but no source-note node in node_range")))]
            (recur (rest remaining)
                   (if (and (integer? end) (not outside?))
                     end
                     previous-end)
                   errors))
          errors))))))

(defn derived-from-compatibility-query [parser-ir manifest-inputs]
  (let [derived-from (get parser-ir "derived_from")]
    {:aat_version (get derived-from "aat_version")
     :aat_adapter (get derived-from "aat_adapter")
     :aat_adapter_version (get derived-from "aat_adapter_version")
     :mapping_id (get derived-from "mapping_id")
     :mapping_version (get derived-from "mapping_version")
     :mapping_hash (get manifest-inputs "mapping_hash")
     :mapping_schema_hash (get derived-from "mapping_schema_hash")
     :parser_ir_schema_id (get parser-ir "schema_id")
     :parser_ir_schema_hash (get parser-ir "schema_hash")}))

(defn divergence-bundle-compatibility-query [parser-ir manifest-inputs divergence-bundle]
  (let [mapping (get divergence-bundle "mapping")
        target (get divergence-bundle "target")
        aat (get divergence-bundle "aat")]
    {:aat_version (get aat "version")
     :aat_adapter (get aat "adapter")
     :aat_adapter_version (get aat "adapter_version")
     :mapping_id (get mapping "mapping_id")
     :mapping_version (get mapping "mapping_version")
     :mapping_hash (get manifest-inputs "mapping_hash")
     :mapping_schema_hash (get mapping "mapping_schema_hash")
     :parser_ir_schema_id (or (get target "parser_ir_schema_id")
                              (get parser-ir "schema_id"))
     :parser_ir_schema_hash (or (get target "parser_ir_schema_hash")
                                (get parser-ir "schema_hash"))}))

(def compatibility-derived-from-keys
  ["aat_version"
   "aat_adapter"
   "aat_adapter_version"
   "mapping_id"
   "mapping_version"
   "mapping_schema_hash"])

(defn missing-derived-from-key-errors [derived-from]
  (->> compatibility-derived-from-keys
       (remove #(contains? derived-from %))
       (mapv #(str "AAT parser-IR compatibility requires parser IR derived_from." %))))

(defn divergence-bundle-target-errors [parser-ir divergence-bundle]
  (let [target (get divergence-bundle "target")]
    (vec
     (concat
      (when (not= (get parser-ir "schema_id")
                  (get target "parser_ir_schema_id"))
        [(str "AAT parser-IR divergence bundle target parser_ir_schema_id "
              (get target "parser_ir_schema_id")
              " does not match parser IR schema_id "
              (get parser-ir "schema_id"))])
      (when (not= (get parser-ir "schema_hash")
                  (get target "parser_ir_schema_hash"))
        [(str "AAT parser-IR divergence bundle target parser_ir_schema_hash "
              (get target "parser_ir_schema_hash")
              " does not match parser IR schema_hash "
              (get parser-ir "schema_hash"))])))))

(defn compatibility-mismatch-error [{:keys [aat_adapter
                                            aat_version
                                            mapping_id
                                            mapping_version
                                            mapping_hash
                                            mapping_schema_hash
                                            parser_ir_schema_id
                                            parser_ir_schema_hash]}]
  (str "AAT parser-IR compatibility registry has no entry for adapter " aat_adapter
       ", AAT version " aat_version
       ", mapping " mapping_id " " mapping_version
       ", mapping hash " mapping_hash
       ", mapping schema hash " mapping_schema_hash
       ", parser IR schema id " parser_ir_schema_id
       ", parser IR schema hash " parser_ir_schema_hash))

(defn compatibility-errors
  ([parser-ir manifest-inputs]
   (compatibility-errors (compat/load-registry) parser-ir manifest-inputs nil))
  ([registry parser-ir manifest-inputs]
   (compatibility-errors registry parser-ir manifest-inputs nil))
  ([registry parser-ir manifest-inputs divergence-bundle]
   (let [derived-from (get parser-ir "derived_from")
         mapping-hash (get manifest-inputs "mapping_hash")]
     (vec
      (cond
        (and (nil? derived-from) (nil? divergence-bundle) (nil? mapping-hash))
        []

        (nil? mapping-hash)
        ["AAT parser-IR compatibility requires manifest inputs mapping_hash when parser IR mapping provenance is present"]

        (and (nil? derived-from) (nil? divergence-bundle))
        ["AAT parser-IR compatibility requires parser IR derived_from or divergence bundle when manifest inputs mapping_hash is present"]

        :else
        (let [metadata-errors (concat
                               (when derived-from
                                 (missing-derived-from-key-errors derived-from))
                               (when divergence-bundle
                                 (divergence-bundle-target-errors parser-ir divergence-bundle)))]
          (if (seq metadata-errors)
            metadata-errors
            (let [queries (cond-> []
                            derived-from
                            (conj (derived-from-compatibility-query parser-ir manifest-inputs))
                            divergence-bundle
                            (conj (divergence-bundle-compatibility-query
                                   parser-ir manifest-inputs divergence-bundle)))]
              (->> queries
                   (remove #(compat/compatible? registry %))
                   (mapv compatibility-mismatch-error))))))))))

(defn source-region-policy-errors [policy]
  (source-region/policy-errors policy))

(defn source-region-coverage-errors [coverage policy]
  (source-region/coverage-errors coverage policy))

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

(defn check-errors! [errors]
  (when (seq errors)
    (throw (ex-info (string/join "\n" errors)
                    {:errors errors}))))

(defn validate-json-schemas! [extra-manifest-paths]
  (let [manifest-schema (files/read-json "schemas/manifest.schema.json")
        parser-ir-schema (files/read-json "schemas/parser-ir.schema.json")
        diagnostic-schema (files/read-json "schemas/diagnostic.schema.json")
        run-summary-schema (files/read-json "schemas/run-summary.schema.json")
        manifest-inputs-schema (files/read-json "schemas/manifest-inputs.schema.json")
        comparison-report-schema (files/read-json "schemas/comparison-report.schema.json")
        aat-parser-ir-mapping-schema (files/read-json "schemas/aat-parser-ir-mapping.schema.json")
        aat-parser-ir-divergence-schema (files/read-json "schemas/aat-parser-ir-divergence.schema.json")
        aat-parser-ir-divergence-bundle-schema (files/read-json "schemas/aat-parser-ir-divergence-bundle.schema.json")
        parser-ir-publication-preservation-schema (files/read-json "schemas/parser-ir-publication-preservation.schema.json")
        analysis-recipe-schema (files/read-json "schemas/analysis-recipe.schema.json")
        analysis-result-schema (files/read-json "schemas/analysis-result.schema.json")
        snapshot-index-schema (files/read-json "schemas/snapshot-index.schema.json")
        source-region-coverage-schema (files/read-json "schemas/source-region-coverage.schema.json")
        tei-validation-result-schema (files/read-json "schemas/tei-validation-result.schema.json")
        iiif-applicability-schema (files/read-json "schemas/iiif-applicability.schema.json")
        person-drift-event-schema (files/read-json "schemas/person-drift-event.schema.json")
        person-drift-index-schema (files/read-json "schemas/person-drift-index.schema.json")]
    (doseq [[path schema] [["schemas/manifest.schema.json" manifest-schema]
                           ["schemas/parser-ir.schema.json" parser-ir-schema]
                           ["schemas/diagnostic.schema.json" diagnostic-schema]
                           ["schemas/run-summary.schema.json" run-summary-schema]
                           ["schemas/manifest-inputs.schema.json" manifest-inputs-schema]
                           ["schemas/comparison-report.schema.json" comparison-report-schema]
                           ["schemas/aat-parser-ir-mapping.schema.json" aat-parser-ir-mapping-schema]
                           ["schemas/aat-parser-ir-divergence.schema.json" aat-parser-ir-divergence-schema]
                           ["schemas/aat-parser-ir-divergence-bundle.schema.json" aat-parser-ir-divergence-bundle-schema]
                           ["schemas/parser-ir-publication-preservation.schema.json" parser-ir-publication-preservation-schema]
                           ["schemas/analysis-recipe.schema.json" analysis-recipe-schema]
                           ["schemas/analysis-result.schema.json" analysis-result-schema]
                           ["schemas/snapshot-index.schema.json" snapshot-index-schema]
                           ["schemas/source-region-coverage.schema.json" source-region-coverage-schema]
                           ["schemas/tei-validation-result.schema.json" tei-validation-result-schema]
                           ["schemas/iiif-applicability.schema.json" iiif-applicability-schema]
                           ["schemas/person-drift-event.schema.json" person-drift-event-schema]
                           ["schemas/person-drift-index.schema.json" person-drift-index-schema]]]
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
    (validate-json! source-region-coverage-schema
                    "examples/ab-validator-output/source-region-coverage.json")
    (validate-json! aat-parser-ir-divergence-bundle-schema
                    "examples/ab-validator-output/divergence.json")
    (validate-json! analysis-recipe-schema
                    "data/analysis-recipes/literary-basic-ja-v1.json")
    (validate-json! analysis-result-schema
                    "examples/v0/example-work/analysis-result.json")
    (validate-json! snapshot-index-schema
                    "examples/v0/snapshot/snapshot-index.json")
    (snapshot-index/validate-snapshot-index!
     (files/read-json "examples/v0/snapshot/snapshot-index.json"))
    (doseq [record (get (files/read-json "examples/ab-validator-output/divergence.json") "records")]
      (check-errors! (validation-errors aat-parser-ir-divergence-schema record)))
    (validate-json! tei-validation-result-schema
                    "examples/v0/example-work/tei-validation-result.json")
    (let [manifest (files/read-json "examples/v0/example-work/manifest.json")
          validation-sidecars (filter #(= "validation-result" (get % "role"))
                                      (get manifest "sidecars"))]
      (when-not (some #(= "tei-validation-result.json" (get % "path_hint"))
                      validation-sidecars)
        (throw (ex-info "example TEI manifest must reference tei-validation-result.json"
                        {:manifest "examples/v0/example-work/manifest.json"}))))
    (compat/validate-registry! (compat/load-registry))
    (parser-evidence/validate-index! (parser-evidence/load-index))
    (when-not (validation-errors manifest-schema {})
      (throw (ex-info "manifest schema accepted an empty object"
                      {:schema "schemas/manifest.schema.json"})))))

(defn validate-ab-validator-output! []
  (let [manifest-inputs (files/read-json (files/path "examples" "ab-validator-output" "manifest-inputs.json"))
        parser-ir (files/read-json (files/path "examples" "ab-validator-output" "parser-ir.json"))
        source-region-coverage (files/read-json
                                (files/path "examples" "ab-validator-output"
                                            "source-region-coverage.json"))
        source-region-policy (files/read-json
                              (files/path "data"
                                          "source-region-publication-policy-v0.json"))
        divergence-file (files/path "examples" "ab-validator-output" "divergence.json")
        divergence-bundle (when (.exists divergence-file)
                            (files/read-json divergence-file))]
    (am/explain-or-throw! ::am/manifest-inputs manifest-inputs
                          "ab-validator manifest inputs")
    (check-errors! (schema-hash-errors manifest-inputs))
    (check-errors! (parser-ir-schema-hash-errors parser-ir))
    (check-errors! (parser-ir-paragraph-coherence-errors parser-ir))
    (check-errors! (source-region-coverage-errors source-region-coverage
                                                  source-region-policy))
    (check-errors! (compatibility-errors (compat/load-registry)
                                         parser-ir
                                         manifest-inputs
                                         divergence-bundle)))
  (am/explain-or-throw! ::am/run-summary-events
                        (files/read-json-lines (files/path "examples" "ab-validator-output" "run-summary.jsonl"))
                        "ab-validator run summary")
  (am/explain-or-throw! ::am/comparison-report
                        (files/read-json (files/path "examples" "ab-validator-output" "comparison-report.json"))
                        "ab-validator comparison report"))

(defn validate-canonicalization! []
  (let [expected "667a3bfa5ab9a5e52a88e2e7de15506936a13c5d6c33825b8983861787bbcdea"
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
                "fixtures/tei/invalid/abc-bad-layout-params.xml"
                "fixtures/tei/invalid/abc-bad-preservation-record.xml"
                "fixtures/tei/invalid/abc-missing-vocab-version.xml"
                "fixtures/tei/invalid/char-empty-decl.xml"
                "fixtures/tei/invalid/gaiji-dangling-ref.xml"
                "fixtures/tei/invalid/gaiji-missing-ref.xml"
                "fixtures/tei/invalid/header-no-language.xml"
                "fixtures/tei/invalid/missing-source-work-id.xml"
                "fixtures/tei/invalid/missing-title.xml"
                "fixtures/tei/invalid/ruby-empty-base.xml"
                "fixtures/tei/invalid/ruby-empty-reading.xml"
                "fixtures/tei/invalid/ruby-missing-reading.xml"
                "fixtures/tei/invalid/source-span-dangling-ref.xml"
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
        expected-set (set expected-rules)
        actual-set (set actual-errors)
        missing (set/difference expected-set actual-set)
        ;; Exact-equality coverage (review §3 root-cause fix): a fixture
        ;; must fire EXACTLY its declared rule set, not a superset. The
        ;; prior asymmetry (missing-only) is how the
        ;; source-span-external-ref.xml drift went undetected: it fired 2
        ;; rules while declaring 1, and the missing-only check passed.
        unexpected (set/difference actual-set expected-set)]
    (when (seq missing)
      (throw (ex-info "missing expected Schematron rule"
                      {:fixture path
                       :missing (sort missing)
                       :actual (sort actual-errors)})))
    (when (seq unexpected)
      (throw (ex-info "Schematron fixture fired rules not declared in its expected set (exact-equality violation)"
                      {:fixture path
                       :unexpected (sort unexpected)
                       :expected (sort (seq expected-set))
                       :actual (sort actual-errors)})))))

(defn rule-universe
  "Return the canonical set of ABC Schematron rule ids declared in
  schemas/tei-profile.odd. The set is derived from the ODD at runtime so
  it cannot drift from the authored constraintSpec identifiers."
  []
  (->> (slurp "schemas/tei-profile.odd")
       (re-seq #"<constraintSpec[^>]*\bident=\"(abc-[a-z0-9-]+)\"")
       (map second)
       set))

(def tei-schematron-fixtures
  "Hand-coded partition of TEI Schematron fixtures. The expected rule
  sets are still maintained here, but they are now cross-checked against
  the ODD-derived rule universe by validate-tei-schematron!."
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
                      "fixtures/tei/invalid/abc-bad-layout-params.xml"
                      #{"abc-layout-params-shape"}
                      "fixtures/tei/invalid/abc-bad-preservation-record.xml"
                      #{"abc-preservation-record-shape"}
                      "fixtures/tei/invalid/abc-missing-vocab-version.xml"
                      #{"abc-vocab-version-declared"}
                      "fixtures/tei/invalid/char-empty-decl.xml"
                      #{"abc-char-resolution-form"}
                      "fixtures/tei/invalid/gaiji-dangling-ref.xml"
                      #{"abc-gaiji-chardecl-resolution"}
                      "fixtures/tei/invalid/gaiji-missing-ref.xml"
                      #{"abc-gaiji-reference"}
                      "fixtures/tei/invalid/header-no-language.xml"
                      #{"abc-header-language-declared"}
                      "fixtures/tei/invalid/missing-source-work-id.xml"
                      #{"abc-tei-header-source-work-id"}
                      "fixtures/tei/invalid/ruby-empty-base.xml"
                      #{"abc-ruby-base-non-empty"}
                      "fixtures/tei/invalid/ruby-empty-reading.xml"
                      #{"abc-ruby-reading-non-empty"}
                      "fixtures/tei/invalid/ruby-missing-reading.xml"
                      #{"abc-ruby-complete"}
                      "fixtures/tei/invalid/source-span-external-ref.xml"
                      #{"abc-source-span-reference" "abc-source-span-target-exists"}
                      "fixtures/tei/invalid/source-span-dangling-ref.xml"
                      #{"abc-source-span-target-exists"}}})

(defn validate-tei-schematron!
  [{:keys [schema-path valid-fixtures warning-fixtures invalid-fixtures]}]
  (doseq [path valid-fixtures]
    (validate-schematron-valid-fixture! schema-path path))
  (doseq [[path expected-rules] warning-fixtures]
    (validate-schematron-warning-fixture! schema-path path expected-rules))
  (doseq [[path expected-rules] invalid-fixtures]
    (validate-schematron-invalid-fixture! schema-path path expected-rules))
  (let [fixture-paths (concat (keys warning-fixtures) (keys invalid-fixtures))
        findings (map (fn [path]
                        (let [{:keys [findings]} (schematron/validate!
                                                  {:schema-path schema-path
                                                   :xml-path path
                                                   :label path})]
                          {:errors (rule-ids (filter schematron-error? findings))
                           :warnings (rule-ids (filter schematron-warning? findings))}))
                      fixture-paths)
        actual-errors (set (mapcat :errors findings))
        actual-warnings (set (mapcat :warnings findings))
        all-actual (set/union actual-errors actual-warnings)
        all-expected (set/union (apply set/union (vals warning-fixtures))
                                (apply set/union (vals invalid-fixtures)))
        universe (rule-universe)
        uncovered (set/difference universe all-actual)
        unknown (set/difference all-expected universe)]
    (when (seq uncovered)
      (throw (ex-info "Schematron rules declared in the ODD are not covered by any negative or warning fixture"
                      {:rule-universe (sort universe)
                       :uncovered (sort uncovered)})))
    (when (seq unknown)
      (throw (ex-info "Schematron fixture references rule-ids not declared in the ODD"
                      {:rule-universe (sort universe)
                       :unknown (sort unknown)})))))

(defn- load-turtle-graph [path]
  (aa/read (aa/graph :simple) (io/file path)))

(defn- validate-drift-ttl-fixture-result [{:keys [event graph]}]
  (let [event-value (files/read-json event)
        data-graph (load-turtle-graph graph)
        failures (person-drift/validate-drift-graph-failures event-value
                                                             data-graph
                                                             graph)]
    (if (seq failures)
      {:status :error :failures failures}
      {:status :ok})))

(defn- validate-drift-fixture-result [fixture]
  (if (map? fixture)
    (case (:type fixture)
      :ttl (validate-drift-ttl-fixture-result fixture)
      (throw (ex-info "unknown drift fixture type" {:fixture fixture})))
    (person-drift/validate-drift-events! {:persons-dir fixture})))

(defn- validate-drift-invalid-fixture! [fixture expected-codes]
  (let [result (validate-drift-fixture-result fixture)
        failures (:failures result)
        actual (set (map :code failures))
        unknown (set/difference actual person-drift/failure-codes)]
    (when-not (= :error (:status result))
      (throw (ex-info "expected invalid drift fixture to fail"
                      {:fixture fixture
                       :result result})))
    (when (seq unknown)
      (throw (ex-info "unknown drift validation failure codes"
                      {:fixture fixture
                       :known (sort person-drift/failure-codes)
                       :unknown (sort unknown)
                       :failures failures})))
    (when-not (= expected-codes actual)
      (throw (ex-info "unexpected drift validation failure set"
                      {:fixture fixture
                       :expected (sort expected-codes)
                       :actual (sort actual)
                       :missing (sort (set/difference expected-codes actual))
                       :unexpected (sort (set/difference actual expected-codes))})))))

(defn validate-drift-fixtures! [invalid-fixtures]
  (doseq [[path expected-codes] invalid-fixtures]
    (validate-drift-invalid-fixture! path expected-codes)))

(defn- file-bytes [path]
  (with-open [in (io/input-stream (io/file path))]
    (.readAllBytes in)))

(defn validate-publication-view!
  "Regenerate the Linked Art candidate, expanded, and validation-result
  fixtures under a temp dir and byte-compare them against the committed
  LOD fixtures. The harness internally enforces the artifact-id
  identity invariant; here we additionally enforce that the on-disk
  bytes are exactly what the harness emits, so any drift surfaces as a
  bundle failure rather than an ADR 0013 invariant breach at publish
  time."
  [{:keys [manifest-path metadata-record-path context-path
           candidate-path expanded-path result-path]}]
  (let [temp (.toFile (java.nio.file.Files/createTempDirectory
                       "abc-linked-art-bundle"
                       (make-array java.nio.file.attribute.FileAttribute 0)))]
    (try
      (let [tmp-candidate (io/file temp "linked-art-candidate.jsonld")
            tmp-expanded (io/file temp "linked-art-expanded.normalized.json")
            tmp-result (io/file temp "jsonld-context-validation-result.json")]
        (linked-art/write-publication-view!
         {:manifest-path manifest-path
          :metadata-record-path metadata-record-path
          :context-path context-path
          :candidate-path (str tmp-candidate)
          :expanded-path (str tmp-expanded)
          :result-path (str tmp-result)})
        (doseq [[label committed regen]
                [["linked-art-candidate.jsonld" candidate-path tmp-candidate]
                 ["linked-art-expanded.normalized.json" expanded-path tmp-expanded]
                 ["jsonld-context-validation-result.json" result-path tmp-result]]]
          (let [a (file-bytes committed)
                b (file-bytes regen)]
            (when-not (= (seq a) (seq b))
              (throw (ex-info (str label " drifted from harness output")
                              {:committed (str committed)
                               :regenerated (str regen)}))))))
      (finally
        (doseq [f (reverse (file-seq temp))]
          (.delete f))))))

(defn validate-git-cliff! []
  (run-command! "git-cliff" "--config" "cliff.toml" "--unreleased" "--strip" "header"
                "--output" "/tmp/abc-changelog-check.md"))

(defn validate-publication-output! [publication-output]
  (let [manifest-schema (files/read-json "schemas/manifest.schema.json")
        preservation-schema (files/read-json "schemas/parser-ir-publication-preservation.schema.json")
        validation-result-schema (files/read-json "schemas/tei-validation-result.schema.json")
        plain-file (:plaintext publication-output)
        tei-file (:tei publication-output)
        preservation-file (:preservation publication-output)
        validation-result-file (:tei-validation-result publication-output)]
    (when-not (and (.exists plain-file) (pos? (.length plain-file)))
      (throw (ex-info "parser-IR publication plain.txt must exist and be non-empty"
                      {:path (str plain-file)})))
    (run-command! "xmllint" "--noout" (str tei-file))
    ;; Parser-IR publication TEI uses the ABC namespace extension. Strict
    ;; upstream tei_all.rng does not admit project-specific foreign attributes,
    ;; so generated publication artifacts are validated against the customized
    ;; ABC profile below.
    (validate-tei! "schemas/tei-profile.rng" [tei-file])
    (let [{:keys [findings]} (schematron/validate! {:schema-path "schemas/tei-profile.sch"
                                                    :xml-path (str tei-file)
                                                    :label (str tei-file)})
          errors (filter schematron-error? findings)]
      (when (seq errors)
        (throw (ex-info "generated parser-IR publication TEI has Schematron errors"
                        {:errors (mapv render-schematron-finding errors)}))))
    (doseq [manifest-file [(:plaintext-manifest publication-output)
                           (:tei-manifest publication-output)]]
      (validate-json! manifest-schema manifest-file))
    (validate-json! preservation-schema preservation-file)
    (validate-json! validation-result-schema validation-result-file)))

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
  (let [temp-dir (.toFile (java.nio.file.Files/createTempDirectory
                           "abc-design-bundle"
                           (make-array java.nio.file.attribute.FileAttribute 0)))
        materialized-dir (io/file temp-dir "materialized-import")
        publication-dir (io/file temp-dir "publication")]
    (try
      (tel/log! :info "==> Materializing imported ab-validator output")
      (let [materialized (materialize/materialize-import!
                          {:input-dir (files/path "examples" "ab-validator-output")
                           :output-dir materialized-dir
                           :generated-at materialize/default-generated-at})
            publication-output (publication/materialize-publication!
                                {:parser-ir-path "examples/v0/example-work/parser-ir.json"
                                 :source-manifest-path "examples/v0/example-work/source.manifest.json"
                                 :metadata-record-path "examples/v0/example-work/metadata-record.json"
                                 :persons-dir "examples/v0/example-persons"
                                 :output-dir publication-dir
                                 :generated-at publication/default-generated-at})]
        (tel/log! :info "materialized import ok")
        (tel/log! :info "parser-IR publication materialization ok")
        (tel/log! :info "==> Validating JSON schemas and examples")
        (validate-json-schemas! (concat (vals materialized)
                                        [(:plaintext-manifest publication-output)
                                         (:tei-manifest publication-output)]))
        (tel/log! :info "json schema validation ok")
        (tel/log! :info "==> Checking parser-IR publication output")
        (validate-publication-output! publication-output)
        (tel/log! :info "parser-IR publication output ok")
        (tel/log! :info "==> Checking materialized manifest index")
        (let [entries (manifest-index/index-manifest-files
                       (concat (vals materialized)
                               [(:plaintext-manifest publication-output)
                                (:tei-manifest publication-output)]))]
          (manifest-index/validate-no-reproducibility-conflicts! entries)
          (manifest-index/validate-tokenized-release-guardrail! entries)
          (manifest-index/validate-analysis-copied-fields! entries))
        (tel/log! :info "materialized manifest index ok")
        (tel/log! :info "==> Checking materialized RDF views")
        (doseq [manifest-path (concat (vals materialized)
                                      [(:plaintext-manifest publication-output)
                                       (:tei-manifest publication-output)])]
          (manifest-to-rdf/manifest->ttl (files/read-json manifest-path)))
        (tel/log! :info "materialized RDF views ok")
        (tel/log! :info "==> Validating SHACL shapes")
        (let [shapes (shacl/load-shapes-graph)
              targets (concat (vals materialized)
                              [(:plaintext-manifest publication-output)
                               (:tei-manifest publication-output)]
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
        (tel/log! :info "metadata bundle ok")
        (tel/log! :info "==> Validating person drift events")
        (let [result (person-drift/validate-drift-events!
                      {:persons-dir "examples/v0/example-persons"})]
          (when (= :error (:status result))
            (throw (ex-info "person drift validation failed"
                            {:errors (:failures result)}))))
        (tel/log! :info "person drift events ok"))
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
      ;; missing-title and ruby-missing-reading are intentionally
      ;; structurally invalid TEI: they have no <title> at all, and a
      ;; <ruby> with no <rt>. Both fail the RelaxNG content model that
      ;; the ODD-derived tei-profile.rng now enforces, in addition to
      ;; tripping their corresponding ABC Schematron rule. They are
      ;; covered by the Schematron partition below; excluding them from
      ;; the project-RNG step keeps the structural-validity step honest
      ;; about which fixtures *should* pass RNG.
      (validate-tei! "schemas/tei-profile.rng"
                     ["examples/v0/example-work/tei.xml"
                      "fixtures/tei/valid/rashomon-minimal.xml"
                      "fixtures/tei/valid/source-span-local-ref.xml"
                      "fixtures/tei/valid/transcription-enrichment-declared.xml"
                      "fixtures/tei/warnings/figure-missing-desc.xml"
                      "fixtures/tei/warnings/transcription-enrichment-undeclared.xml"
                      "fixtures/tei/invalid/abc-bad-layout-params.xml"
                      "fixtures/tei/invalid/abc-missing-vocab-version.xml"
                      "fixtures/tei/invalid/char-empty-decl.xml"
                      "fixtures/tei/invalid/gaiji-dangling-ref.xml"
                      "fixtures/tei/invalid/gaiji-missing-ref.xml"
                      "fixtures/tei/invalid/header-no-language.xml"
                      "fixtures/tei/invalid/missing-source-work-id.xml"
                      "fixtures/tei/invalid/ruby-empty-base.xml"
                      "fixtures/tei/invalid/ruby-empty-reading.xml"
                      "fixtures/tei/invalid/source-span-dangling-ref.xml"
                      "fixtures/tei/invalid/source-span-external-ref.xml"])
      (tel/log! :info "tei project rng validation ok")
      (tel/log! :info "==> Validating TEI against project Schematron")
      (validate-tei-schematron! tei-schematron-fixtures)
      (tel/log! :info "tei schematron validation ok")
      (tel/log! :info "==> Validating person drift negative fixtures")
      (validate-drift-fixtures!
       {"fixtures/v0/invalid/drift/broken-index-target" #{:index-target-missing}
        "fixtures/v0/invalid/drift/asymmetric-index" #{:event-missing-from-participant-index}
        "fixtures/v0/invalid/drift/orphan-event-file"
        #{:orphan-event-file :event-missing-from-participant-index}
        "fixtures/v0/invalid/drift/unsorted-participants"
        #{:participants-not-sorted :index-target-missing
          :event-missing-from-participant-index :orphan-event-file}
        "fixtures/v0/invalid/drift/dangling-snapshot-ref"
        #{:unknown-snapshot-reference :participant-not-covered
          :index-target-missing :event-missing-from-participant-index
          :orphan-event-file}
        "fixtures/v0/invalid/drift/invalid-role"
        #{:invalid-had-role :index-target-missing
          :event-missing-from-participant-index :orphan-event-file}
        "fixtures/v0/invalid/drift/invalid-agent"
        #{:invalid-agent-iri :index-target-missing
          :event-missing-from-participant-index :orphan-event-file}
        {:type :ttl
         :event "examples/v0/example-persons/_events/sha256:550c55dbfed12ce9b6de833a75c8b03e8a01bf3047c17ced494e87db0f4ee747.json"
         :graph "fixtures/v0/invalid/drift/shacl-missing-date/graph.ttl"}
        #{:shacl-violation}
        {:type :ttl
         :event "examples/v0/example-persons/_events/sha256:550c55dbfed12ce9b6de833a75c8b03e8a01bf3047c17ced494e87db0f4ee747.json"
         :graph "fixtures/v0/invalid/drift/split-cardinality-one-successor/graph.ttl"}
        #{:shacl-violation :rdf-participant-prov-mismatch}
        {:type :ttl
         :event "fixtures/v0/invalid/drift/merge-cardinality-one-predecessor/event.json"
         :graph "fixtures/v0/invalid/drift/merge-cardinality-one-predecessor/graph.ttl"}
        #{:shacl-violation}
        {:type :ttl
         :event "examples/v0/example-persons/_events/sha256:550c55dbfed12ce9b6de833a75c8b03e8a01bf3047c17ced494e87db0f4ee747.json"
         :graph "fixtures/v0/invalid/drift/typing-missing-subclass/graph.ttl"}
        #{:missing-rdf-type :rdf-participant-prov-mismatch}
        {:type :ttl
         :event "examples/v0/example-persons/_events/sha256:550c55dbfed12ce9b6de833a75c8b03e8a01bf3047c17ced494e87db0f4ee747.json"
         :graph "fixtures/v0/invalid/drift/typing-missing-activity/graph.ttl"}
        #{:missing-rdf-type :shacl-violation :rdf-participant-prov-mismatch}
        {:type :ttl
         :event "examples/v0/example-persons/_events/sha256:550c55dbfed12ce9b6de833a75c8b03e8a01bf3047c17ced494e87db0f4ee747.json"
         :graph "fixtures/v0/invalid/drift/rdf-participant-prov-mismatch/graph.ttl"}
        #{:rdf-participant-prov-mismatch}})
      (tel/log! :info "person drift negative fixtures ok")
      (tel/log! :info "==> Validating Linked Art publication view (ADR 0013)")
      (validate-publication-view!
       {:manifest-path "examples/v0/example-work/manifest.json"
        :metadata-record-path "examples/v0/example-work/metadata-record.json"
        :context-path "contexts/abc-v0.jsonld"
        :candidate-path "examples/v0/example-work/lod/linked-art-candidate.jsonld"
        :expanded-path "examples/v0/example-work/lod/linked-art-expanded.normalized.json"
        :result-path "examples/v0/example-work/lod/jsonld-context-validation-result.json"})
      (tel/log! :info "linked art publication view ok")
      (tel/log! :info "==> Checking IIIF applicability record (ADR 0014)")
      (iiif/validate-applicability! "examples/v0/example-work/iiif/applicability.json")
      (tel/log! :info "iiif applicability record ok")
      (tel/log! :info "==> Checking git-cliff configuration")
      (validate-git-cliff!)
      (tel/log! :info "git-cliff config ok")
      (tel/log! :info "design bundle validation ok")
      (finally
        (doseq [file (reverse (file-seq temp-dir))]
          (.delete file))))))

(defn -main [& _args]
  (logging/install-cli-handler!)
  (am/install!)
  (try
    (validate-design-bundle!)
    (catch Throwable t
      (tel/log! {:level :error :error t} "design bundle validation failed")
      (when-let [errors (:errors (ex-data t))]
        (doseq [error errors]
          (tel/log! :error (str "- " error))))
      (System/exit 1))))
