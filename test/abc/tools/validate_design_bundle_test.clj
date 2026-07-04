(ns abc.tools.validate-design-bundle-test
  (:require [abc.tools.aat-parser-ir-compat :as compat]
            [abc.tools.files :as files]
            [abc.tools.malli :as am]
            [abc.tools.manifest-to-rdf :as manifest-to-rdf]
            [abc.tools.shacl :as shacl]
            [abc.tools.validate-design-bundle :as validate]
            [clojure.test :refer [deftest is testing use-fixtures]]))

(use-fixtures :once (fn [f] (am/install!) (f)))

(deftest sha256-file-test
  (let [file (java.io.File/createTempFile "abc-sha256" ".txt")]
    (try
      (spit file "abc")
      (is (= "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
             (files/sha256-file file)))
      (finally
        (.delete file)))))

(deftest read-json-lines-test
  (let [file (java.io.File/createTempFile "abc-jsonl" ".jsonl")]
    (try
      (spit file "{\"a\":1}\n\n{\"b\":2}\n")
      (is (= [{"a" 1} {"b" 2}]
             (files/read-json-lines file)))
      (finally
        (.delete file)))))

(def ^:private old-mapping-hash
  "sha256:af2aac0855b0ab42111b7a05aae7a6c337963446a7bc620d2c11790e524fbb03")

(def ^:private current-mapping-hash
  "sha256:4c0d3eb53942b4e1e14a6efc614bab99e391e90d85b817e090b42d02c05ba22e")

(def ^:private v2-mapping-hash
  "sha256:68b0868b25f3b072a47d781099178bf2a31e4b16c561814f5e13e3801714d089")

(def ^:private mapping-schema-hash
  "sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4")

(def ^:private parser-ir-schema-hash
  "sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396")

(def ^:private complete-manifest-inputs
  {"producer" "ab-validator"
   "producer_version" "0.0.0"
   "work_id" "fixture"
   "corpus_snapshot_hash" (files/example-hash "00")
   "work_content_hash" (files/example-hash "01")
   "parser_build_hash" (files/example-hash "02")
   "parser_config_hash" (files/example-hash "03")
   "mapping_hash" current-mapping-hash
   "parser_ir_schema_hash" (files/example-hash "04")
   "diagnostic_schema_hash" (files/example-hash "08")
   "warning_sidecar_hash" (files/example-hash "05")
   "run_summary_hash" (files/example-hash "06")
   "comparison_report_hash" (files/example-hash "07")})

(deftest run-summary-events-schema-test
  (testing "accepts start, work result, complete"
    (is (= :ok
           (am/explain-or-throw!
            ::am/run-summary-events
            [{"event" "run-start" "run_id" "r1"}
             {"event" "work-result" "run_id" "r1"}
             {"event" "run-complete" "run_id" "r1"}]
            "test"))))
  (testing "accepts start and complete without work results"
    (is (= :ok
           (am/explain-or-throw!
            ::am/run-summary-events
            [{"event" "run-start" "run_id" "r1"}
             {"event" "run-complete" "run_id" "r1"}]
            "test"))))
  (testing "rejects mismatched run ids"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"share one run_id"
         (am/explain-or-throw!
          ::am/run-summary-events
          [{"event" "run-start" "run_id" "r1"}
           {"event" "work-result" "run_id" "r2"}
           {"event" "run-complete" "run_id" "r1"}]
          "test"))))
  (testing "rejects extra lifecycle events"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"exactly one run-start event"
         (am/explain-or-throw!
          ::am/run-summary-events
          [{"event" "run-start" "run_id" "r1"}
           {"event" "run-start" "run_id" "r1"}
           {"event" "run-complete" "run_id" "r1"}
           {"event" "run-complete" "run_id" "r1"}]
          "test"))))
  (testing "rejects missing run id"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"must include run_id"
         (am/explain-or-throw!
          ::am/run-summary-events
          [{"event" "run-start" "run_id" "r1"}
           {"event" "work-result"}
           {"event" "run-complete" "run_id" "r1"}]
          "test"))))
  (testing "ex-data carries humanized errors and explanation"
    (try
      (am/explain-or-throw!
       ::am/run-summary-events
       [{"event" "work-result" "run_id" "r1"}]
       "test")
      (is false "expected throw")
      (catch clojure.lang.ExceptionInfo e
        (let [d (ex-data e)]
          (is (= "test" (:label d)))
          (is (every? string? (:errors-humanized d)))
          (is (some? (:explanation d))))))))

(deftest manifest-inputs-schema-test
  (testing "accepts complete manifest inputs"
    (is (= :ok (am/explain-or-throw!
                ::am/manifest-inputs complete-manifest-inputs "test"))))
  (testing "rejects missing required keys"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"missing required keys"
         (am/explain-or-throw!
          ::am/manifest-inputs {"producer" "ab-validator"} "test"))))
  (testing "requires the AAT parser-IR mapping document hash"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"missing required keys"
         (am/explain-or-throw!
          ::am/manifest-inputs
          (dissoc complete-manifest-inputs "mapping_hash")
          "test"))))
  (testing "rejects invalid hash values"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"sha256: hash"
         (am/explain-or-throw!
          ::am/manifest-inputs
          (assoc complete-manifest-inputs "work_content_hash" "nope")
          "test")))))

(deftest validate-json-schemas-includes-aat-mapping-contracts-test
  (testing "design-bundle schema pass validates the AAT mapping and divergence contracts"
    (let [checked-paths (atom [])
          registry-checked (atom nil)]
      (with-redefs [validate/schema-valid! (fn [_schema path]
                                             (swap! checked-paths conj path)
                                             nil)
                    validate/validate-json! (fn [& _args] nil)
                    validate/validate-json-lines! (fn [& _args] nil)
                    validate/validation-errors (fn [_schema value]
                                                 (if (and (map? value)
                                                          (contains? value "rule_id"))
                                                   nil
                                                   [:expected-error]))
                    compat/load-registry (fn [] {:entries []})
                    compat/validate-registry! (fn [registry]
                                                (reset! registry-checked registry)
                                                :ok)]
        (validate/validate-json-schemas! [])
        (is (every? (set @checked-paths)
                    ["schemas/aat-parser-ir-mapping.schema.json"
                     "schemas/aat-parser-ir-divergence.schema.json"
                     "schemas/aat-parser-ir-divergence-bundle.schema.json"]))
        (is (= {:entries []} @registry-checked))))))

(deftest comparison-report-schema-test
  (testing "accepts a well-formed comparison report"
    (is (= :ok (am/explain-or-throw!
                ::am/comparison-report
                {"report_schema" "abc.ab-validator-comparison.v0"
                 "parser_candidates" [{"parser_id" "fixture"}]}
                "test"))))
  (testing "rejects unexpected report_schema"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"unexpected report_schema"
         (am/explain-or-throw!
          ::am/comparison-report
          {"report_schema" "wrong"
           "parser_candidates" [{"parser_id" "fixture"}]}
          "test"))))
  (testing "rejects empty parser_candidates"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"parser_candidates"
         (am/explain-or-throw!
          ::am/comparison-report
          {"report_schema" "abc.ab-validator-comparison.v0"
           "parser_candidates" []}
          "test")))))

(deftest schema-hash-errors-test
  (is (empty?
       (validate/schema-hash-errors
        {"parser_ir_schema_hash" "sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396"
         "diagnostic_schema_hash" "sha256:e21ef2abdbf64b6fc920b4ef9a3df0e426b7bcc1cad0a6bbdd654f41e8ff302d"})))
  (is (= ["ab-validator parser_ir_schema_hash sha256:0000000000000000000000000000000000000000000000000000000000000004 does not match ABC parser IR schema hash sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396"
          "ab-validator diagnostic_schema_hash sha256:0000000000000000000000000000000000000000000000000000000000000008 does not match ABC diagnostic schema hash sha256:e21ef2abdbf64b6fc920b4ef9a3df0e426b7bcc1cad0a6bbdd654f41e8ff302d"]
         (validate/schema-hash-errors
          {"parser_ir_schema_hash" (files/example-hash "04")
           "diagnostic_schema_hash" (files/example-hash "08")}))))

(deftest parser-ir-schema-hash-errors-test
  (is (empty?
       (validate/parser-ir-schema-hash-errors
        {"schema_hash" "sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396"})))
  (is (= ["ab-validator parser IR schema_hash sha256:0000000000000000000000000000000000000000000000000000000000000004 does not match ABC parser IR schema hash sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396"]
         (validate/parser-ir-schema-hash-errors
          {"schema_hash" (files/example-hash "04")}))))

(deftest parser-ir-schema-accepts-derived-from-test
  (testing "AAT-derived parser IR may record mapping provenance"
    (let [schema (files/read-json "schemas/parser-ir.schema.json")
          parser-ir (files/read-json "examples/ab-validator-output/parser-ir.json")
          derived-from {"aat_version" 1
                        "aat_adapter" "aozora2html"
                        "aat_adapter_version" "aozora2html-adapter 0.1.0 gem-3.0.1"
                        "mapping_id" "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1"
                        "mapping_version" "1.0.0"
                        "mapping_schema_hash" (files/example-hash "09")}]
      (is (nil? (validate/validation-errors
                 schema
                 (assoc parser-ir "derived_from" derived-from)))))))

(def ^:private old-compat-query
  {:aat_version 1
   :aat_adapter "aozora-rs-adapter"
   :aat_adapter_version nil
   :mapping_id "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe"
   :mapping_version "0.1.0"
   :mapping_hash old-mapping-hash
   :mapping_schema_hash mapping-schema-hash
   :parser_ir_schema_id "https://w3id.org/abc/schemas/parser-ir.schema.json"
   :parser_ir_schema_hash parser-ir-schema-hash})

(def ^:private old-registry-entry
  (assoc old-compat-query
         :evidence_scope {:adapter "aozora-rs-adapter"
                          :evidence_type :mapping-generation
                          :corpus "aozora-rs full corpus"
                          :files_scanned 17894
                          :files_with_unsupported 0
                          :generated_rules 25}
         :compatibility "lossy"))

(def ^:private current-rs-compat-query
  {:aat_version 1
   :aat_adapter "aozora-rs"
   :aat_adapter_version "aozora-rs-adapter 0.1.0 aozora-rs-v0.6.0"
   :mapping_id "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe"
   :mapping_version "0.1.1"
   :mapping_hash current-mapping-hash
   :mapping_schema_hash mapping-schema-hash
   :parser_ir_schema_id "https://w3id.org/abc/schemas/parser-ir.schema.json"
   :parser_ir_schema_hash parser-ir-schema-hash})

(def ^:private current-rs-registry-entry
  (assoc current-rs-compat-query
         :evidence_scope {:adapter "aozora-rs"
                          :adapter_version "aozora-rs-adapter 0.1.0 aozora-rs-v0.6.0"
                          :evidence_type :conversion-audit
                          :corpus "aozora-rs full corpus"
                          :files_scanned 17894
                          :files_succeeded 17894
                          :files_failed 0
                          :parser_ir_nodes 7828615
                          :divergence_records 288039
                          :divergence_occurrences 13246894
                          :rules_total 118
                          :rules_emitted 28
                          :rules_missing 90
                          :unsupported_occurrences 0}
         :compatibility "lossy"))

(def ^:private current-html-compat-query
  {:aat_version 1
   :aat_adapter "aozora2html"
   :aat_adapter_version "aozora2html-adapter 0.1.0 gem-3.0.1"
   :mapping_id "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe"
   :mapping_version "0.1.1"
   :mapping_hash current-mapping-hash
   :mapping_schema_hash mapping-schema-hash
   :parser_ir_schema_id "https://w3id.org/abc/schemas/parser-ir.schema.json"
   :parser_ir_schema_hash parser-ir-schema-hash})

(def ^:private current-html-registry-entry
  (assoc current-html-compat-query
         :evidence_scope {:adapter "aozora2html"
                          :adapter_version "aozora2html-adapter 0.1.0 gem-3.0.1"
                          :evidence_type :conversion-audit
                          :corpus "aozora2html full corpus"
                          :files_scanned 17689
                          :files_succeeded 17689
                          :files_failed 0
                          :parser_ir_nodes 8414559
                          :divergence_records 324294
                          :divergence_occurrences 17172155
                          :rules_total 118
                          :rules_emitted 117
                          :rules_missing 1
                          :unsupported_occurrences 14230}
         :compatibility "lossy"))

(def ^:private v2-rs-compat-query
  {:aat_version 1
   :aat_adapter "aozora-rs"
   :aat_adapter_version "aozora-rs-adapter 0.1.0 2b4e8d1"
   :mapping_id "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe"
   :mapping_version "0.2.0"
   :mapping_hash v2-mapping-hash
   :mapping_schema_hash mapping-schema-hash
   :parser_ir_schema_id "https://w3id.org/abc/schemas/parser-ir.schema.json"
   :parser_ir_schema_hash parser-ir-schema-hash})

(def ^:private v2-rs-registry-entry
  (assoc v2-rs-compat-query
         :evidence_scope {:evidence_type :conversion-audit
                          :adapter "aozora-rs"
                          :adapter_version "aozora-rs-adapter 0.1.0 2b4e8d1"
                          :corpus "aozora-rs-adapter"
                          :files_scanned 17894
                          :files_succeeded 17894
                          :files_failed 0
                          :parser_ir_nodes 7828615
                          :divergence_records 252251
                          :divergence_occurrences 13211106
                          :rules_total 116
                          :rules_emitted 26
                          :rules_missing 90
                          :unsupported_occurrences 0}
         :compatibility "lossy"))

(def ^:private v2-html-compat-query
  {:aat_version 1
   :aat_adapter "aozora2html"
   :aat_adapter_version "aozora2html-adapter 0.1.0 gem-3.0.1"
   :mapping_id "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe"
   :mapping_version "0.2.0"
   :mapping_hash v2-mapping-hash
   :mapping_schema_hash mapping-schema-hash
   :parser_ir_schema_id "https://w3id.org/abc/schemas/parser-ir.schema.json"
   :parser_ir_schema_hash parser-ir-schema-hash})

(def ^:private v2-html-registry-entry
  (assoc v2-html-compat-query
         :evidence_scope {:evidence_type :conversion-audit
                          :adapter "aozora2html"
                          :adapter_version "aozora2html-adapter 0.1.0 gem-3.0.1"
                          :corpus "aozora2html-adapter"
                          :files_scanned 17689
                          :files_succeeded 17689
                          :files_failed 0
                          :parser_ir_nodes 8414559
                          :divergence_records 288916
                          :divergence_occurrences 17136777
                          :rules_total 116
                          :rules_emitted 115
                          :rules_missing 1
                          :unsupported_occurrences 14230}
         :compatibility "lossy"))

(def ^:private valid-derived-from
  {"aat_version" 1
   "aat_adapter" "aozora-rs-adapter"
   "aat_adapter_version" nil
   "mapping_id" "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe"
   "mapping_version" "0.1.0"
   "mapping_schema_hash" mapping-schema-hash})

(def ^:private valid-divergence-bundle
  {"schema_id" "https://abc.local/schemas/aat-parser-ir-divergence-bundle-v1.json"
   "schema_version" "0.1.0"
   "work_id" "fixture"
   "mapping" {"mapping_id" "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe"
              "mapping_version" "0.1.1"
              "mapping_schema_hash" mapping-schema-hash}
   "target" {"parser_ir_schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
             "parser_ir_schema_hash" parser-ir-schema-hash}
   "aat" {"version" 1
          "adapter" "aozora2html"
          "adapter_version" "aozora2html-adapter 0.1.0 gem-3.0.1"
          "source_hash" (files/example-hash "01")
          "parse_complete" true}
   "preserved_aat_meta" {"metrics" nil
                         "semantic_summary" nil}
   "summary" {"LOSS" 0
              "INVENTION" 0
              "AMBIGUITY" 0
              "UNSUPPORTED" 0
              "STRUCTURAL" 0}
   "records" []})

(defn- has-error?
  [pattern errors]
  (boolean (some #(re-find pattern %) errors)))

(deftest aat-parser-ir-registry-validation-test
  (testing "accepts the measured adapter-scoped registry shapes"
    (is (empty? (compat/registry-errors {:entries [old-registry-entry
                                                   current-rs-registry-entry
                                                   current-html-registry-entry
                                                   v2-rs-registry-entry
                                                   v2-html-registry-entry]}))))
  (testing "rejects entries without evidence scope"
    (is (has-error? #"entry 0 is missing :evidence_scope"
                    (compat/registry-errors
                     {:entries [(dissoc old-registry-entry :evidence_scope)]}))))
  (testing "rejects entries without an evidence type"
    (is (has-error? #":evidence_scope is missing :evidence_type"
                    (compat/registry-errors
                     {:entries [(update old-registry-entry
                                        :evidence_scope
                                        dissoc
                                        :evidence_type)]}))))
  (testing "rejects adapter-neutral wildcard claims"
    (is (has-error? #":aat_adapter must name a concrete adapter"
                    (compat/registry-errors
                     {:entries [(assoc old-registry-entry :aat_adapter "*")]}))))
  (testing "rejects malformed hashes"
    (is (has-error? #":mapping_hash must be a sha256 hash"
                    (compat/registry-errors
                     {:entries [(assoc old-registry-entry :mapping_hash "sha256:not-a-real-hash")]}))))
  (testing "requires evidence scope to match the adapter claim"
    (is (has-error? #":evidence_scope :adapter must equal :aat_adapter"
                    (compat/registry-errors
                     {:entries [(assoc-in old-registry-entry
                                          [:evidence_scope :adapter]
                                          "aozora2html")]}))))
  (testing "requires conversion file counts to cohere"
    (is (has-error? #"files_scanned must equal files_succeeded plus files_failed"
                    (compat/registry-errors
                     {:entries [(assoc-in current-html-registry-entry
                                          [:evidence_scope :files_succeeded]
                                          1)]}))))
  (testing "requires conversion rule counts to cohere"
    (is (has-error? #"rules_total must equal rules_emitted plus rules_missing"
                    (compat/registry-errors
                     {:entries [(assoc-in current-html-registry-entry
                                          [:evidence_scope :rules_missing]
                                          2)]}))))
  (testing "rejects duplicate compatibility match keys"
    (is (has-error? #"duplicates entry 0 compatibility keys"
                    (compat/registry-errors
                     {:entries [old-registry-entry
                                (assoc-in old-registry-entry
                                          [:evidence_scope :corpus]
                                          "duplicate corpus note")]})))))

(deftest aat-parser-ir-compatibility-test
  (let [registry (compat/load-registry)]
    (testing "matches measured adapter-scoped registry entries"
      (is (true? (compat/compatible? registry old-compat-query)))
      (is (true? (compat/compatible? registry current-rs-compat-query)))
      (is (true? (compat/compatible? registry current-html-compat-query)))
      (is (true? (compat/compatible? registry v2-rs-compat-query)))
      (is (true? (compat/compatible? registry v2-html-compat-query)))
      (let [entry-for (fn [adapter adapter-version]
                        (->> (:entries registry)
                             (filter #(and (= adapter (:aat_adapter %))
                                           (= adapter-version (:aat_adapter_version %))
                                           (= "0.2.0" (:mapping_version %))))
                             first))
            aozora-rs-entry (entry-for "aozora-rs" "aozora-rs-adapter 0.1.0 2b4e8d1")
            aozora2html-entry (entry-for "aozora2html" "aozora2html-adapter 0.1.0 gem-3.0.1")]
        (is (some? aozora-rs-entry) "missing current aozora-rs registry entry")
        (is (some? aozora2html-entry) "missing current aozora2html registry entry")
        (is (= (:mapping_hash aozora-rs-entry) (:mapping_hash aozora2html-entry))
            "both adapter entries must point at the same measured mapping document")
        (is (= v2-mapping-hash (:mapping_hash aozora-rs-entry)))
        (doseq [entry [aozora-rs-entry aozora2html-entry]]
          (is (true? (compat/compatible? registry (select-keys entry compat/match-keys))))
          (is (= :conversion-audit (get-in entry [:evidence_scope :evidence_type])))
          (is (= (get-in entry [:evidence_scope :files_scanned])
                 (+ (get-in entry [:evidence_scope :files_succeeded])
                    (get-in entry [:evidence_scope :files_failed]))))
          (is (= (get-in entry [:evidence_scope :rules_total])
                 (+ (get-in entry [:evidence_scope :rules_emitted])
                    (get-in entry [:evidence_scope :rules_missing]))))))
      (is (false? (compat/compatible?
                   registry
                   (assoc old-compat-query
                          :aat_adapter "aozora2html"
                          :aat_adapter_version "aozora2html-adapter 0.1.0 gem-3.0.1")))
          "the old aozora-rs mapping-generation evidence must not authorize current aozora2html output")
      (doseq [[k v] [[:aat_version 2]
                     [:aat_adapter "other-adapter"]
                     [:aat_adapter_version "aozora-rs-adapter 9.9.9"]
                     [:mapping_id "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/other"]
                     [:mapping_version "9.9.9"]
                     [:mapping_hash (files/example-hash "99")]
                     [:mapping_schema_hash (files/example-hash "98")]
                     [:parser_ir_schema_id "https://w3id.org/abc/schemas/other-parser-ir.schema.json"]
                     [:parser_ir_schema_hash (files/example-hash "97")]]]
        (is (false? (compat/compatible? registry (assoc current-html-compat-query k v)))
            (str "registry must reject mismatched " k))))))

(deftest aat-parser-ir-compatibility-admission-report-test
  (testing "reports when producer candidates are already admitted exactly"
    (is (= {:status :admitted
            :candidate-count 2
            :admitted [v2-rs-registry-entry v2-html-registry-entry]
            :missing []
            :conflicts []
            :registry-errors []
            :candidate-errors []}
           (compat/admission-report
            {:entries [old-registry-entry v2-rs-registry-entry v2-html-registry-entry]}
            {:entries [v2-rs-registry-entry v2-html-registry-entry]}))))
  (testing "reports missing producer candidates"
    (is (= :missing
           (:status
            (compat/admission-report
             {:entries [v2-rs-registry-entry]}
             {:entries [v2-rs-registry-entry v2-html-registry-entry]})))))
  (testing "reports same compatibility identity with changed evidence as a conflict"
    (let [candidate (assoc-in v2-rs-registry-entry
                              [:evidence_scope :corpus]
                              "re-audited corpus")
          report (compat/admission-report
                  {:entries [v2-rs-registry-entry]}
                  {:entries [candidate]})]
      (is (= :conflict (:status report)))
      (is (= [{:registry v2-rs-registry-entry
               :candidate candidate}]
             (:conflicts report)))))
  (testing "validates producer candidate files before comparing them"
    (let [report (compat/admission-report
                  {:entries [v2-rs-registry-entry]}
                  {:entries [(dissoc v2-rs-registry-entry :evidence_scope)]})]
      (is (= :invalid-candidates (:status report)))
      (is (has-error? #"missing :evidence_scope" (:candidate-errors report))))))

(deftest compatibility-errors-test
  (testing "does not check compatibility when no AAT mapping metadata is present"
    (is (empty? (validate/compatibility-errors
                 {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
                  "schema_hash" "sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396"}
                 {}))))
  (testing "requires derived_from or divergence bundle when manifest inputs carry mapping_hash"
    (is (= ["AAT parser-IR compatibility requires parser IR derived_from or divergence bundle when manifest inputs mapping_hash is present"]
           (validate/compatibility-errors
            {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
             "schema_hash" parser-ir-schema-hash}
            {"mapping_hash" old-mapping-hash}))))
  (testing "requires mapping_hash when mapping provenance is present"
    (is (= ["AAT parser-IR compatibility requires manifest inputs mapping_hash when parser IR mapping provenance is present"]
           (validate/compatibility-errors
            {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
             "schema_hash" parser-ir-schema-hash
             "derived_from" valid-derived-from}
            {}))))
  (testing "requires explicit adapter version key even when the value is null"
    (is (= ["AAT parser-IR compatibility requires parser IR derived_from.aat_adapter_version"]
           (validate/compatibility-errors
            {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
             "schema_hash" parser-ir-schema-hash
             "derived_from" (dissoc valid-derived-from "aat_adapter_version")}
            {"mapping_hash" old-mapping-hash}))))
  (testing "accepts legacy derived_from provenance"
    (is (empty? (validate/compatibility-errors
                 {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
                  "schema_hash" parser-ir-schema-hash
                  "derived_from" valid-derived-from}
                 {"mapping_hash" old-mapping-hash}))))
  (testing "accepts divergence bundle provenance"
    (is (empty? (validate/compatibility-errors
                 (compat/load-registry)
                 {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
                  "schema_hash" parser-ir-schema-hash}
                 {"mapping_hash" current-mapping-hash}
                 valid-divergence-bundle))))
  (testing "rejects divergence bundle target mismatch"
    (is (= ["AAT parser-IR divergence bundle target parser_ir_schema_hash sha256:0000000000000000000000000000000000000000000000000000000000000099 does not match parser IR schema_hash sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396"]
           (validate/compatibility-errors
            (compat/load-registry)
            {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
             "schema_hash" parser-ir-schema-hash}
            {"mapping_hash" current-mapping-hash}
            (assoc-in valid-divergence-bundle
                      ["target" "parser_ir_schema_hash"]
                      (files/example-hash "99"))))))
  (testing "rejects adapter mismatch against registry"
    (is (= ["AAT parser-IR compatibility registry has no entry for adapter aozora2, AAT version 1, mapping https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe 0.1.1, mapping hash sha256:4c0d3eb53942b4e1e14a6efc614bab99e391e90d85b817e090b42d02c05ba22e, mapping schema hash sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4, parser IR schema id https://w3id.org/abc/schemas/parser-ir.schema.json, parser IR schema hash sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396"]
           (validate/compatibility-errors
            (compat/load-registry)
            {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
             "schema_hash" parser-ir-schema-hash}
            {"mapping_hash" current-mapping-hash}
            (assoc-in valid-divergence-bundle ["aat" "adapter"] "aozora2"))))))

(deftest validate-shacl-smoke-test
  (testing "validate-design-bundle SHACL pass conforms for the example success manifest"
    (let [shapes (shacl/load-shapes-graph)
          manifest (files/read-json "examples/v0/example-work/manifest.json")
          data (manifest-to-rdf/manifest->graph manifest)]
      (is (= :ok (shacl/validate! {:shapes-graph shapes
                                   :data-graph data
                                   :label "validate_design_bundle_test"})))))

  (testing "validate-design-bundle SHACL pass conforms for the example failure manifest"
    (let [shapes (shacl/load-shapes-graph)
          manifest (files/read-json "examples/v0/example-work/failure-manifest.example.json")
          data (manifest-to-rdf/manifest->graph manifest)]
      (is (= :ok (shacl/validate! {:shapes-graph shapes
                                   :data-graph data
                                   :label "validate_design_bundle_test"}))))))

(def ^:private tei-skip-flag "ABC_TEI_SCHEMA_SKIP")

(defn- tei-schema-tests-skipped?
  []
  (= "1" (System/getenv tei-skip-flag)))

(defn- assert-tei-schema-test-skipped!
  []
  (is (tei-schema-tests-skipped?)
      (str "TEI schema-backed test skipped because " tei-skip-flag "=1")))

(deftest validate-tei-smoke-test
  (testing "validate-tei! returns nil for the example fixture when TEI_SCHEMA_PATH is set"
    (if (tei-schema-tests-skipped?)
      (assert-tei-schema-test-skipped!)
      (let [schema-path (System/getenv "TEI_SCHEMA_PATH")]
        (when-not schema-path
          (throw (ex-info "TEI_SCHEMA_PATH must be set to run validate-tei-smoke-test."
                          {:env-var "TEI_SCHEMA_PATH"})))
        (is (nil? (validate/validate-tei! schema-path
                                          ["examples/v0/example-work/tei.xml"])))))))

(deftest validate-tei-loud-fail-when-env-unset-test
  (testing "validate-tei! throws ex-info naming the schema-path problem when called with nil"
    (try
      (validate/validate-tei! nil ["examples/v0/example-work/tei.xml"])
      (is false "expected validate-tei! to throw")
      (catch clojure.lang.ExceptionInfo e
        (is (re-find #"TEI RelaxNG schema path" (ex-message e)))
        (is (= :missing-schema-path (:error (ex-data e)))
            (str "ex-data must surface the schema-path error; got: "
                 (pr-str (ex-data e))))))))

(deftest validate-tei-schematron-expected-findings-test
  (testing "expected invalid fixtures fail with the requested rule IDs"
    (is (nil? (validate/validate-tei-schematron!
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
                                   "fixtures/tei/invalid/char-empty-decl.xml"
                                   #{"abc-char-resolution-form"}
                                   "fixtures/tei/invalid/gaiji-missing-ref.xml"
                                   #{"abc-gaiji-reference"}
                                   "fixtures/tei/invalid/gaiji-dangling-ref.xml"
                                   #{"abc-gaiji-chardecl-resolution"}
                                   "fixtures/tei/invalid/header-no-language.xml"
                                   #{"abc-header-language-declared"}
                                   "fixtures/tei/invalid/ruby-missing-reading.xml"
                                   #{"abc-ruby-complete"}
                                   "fixtures/tei/invalid/ruby-empty-base.xml"
                                   #{"abc-ruby-base-non-empty"}
                                   "fixtures/tei/invalid/ruby-empty-reading.xml"
                                   #{"abc-ruby-reading-non-empty"}
                                   "fixtures/tei/invalid/source-span-external-ref.xml"
                                   #{"abc-source-span-reference"
                                     "abc-source-span-target-exists"}
                                   "fixtures/tei/invalid/source-span-dangling-ref.xml"
                                   #{"abc-source-span-target-exists"}}})))))

(deftest validate-tei-schematron-loud-fail-test
  (testing "a fixture missing its expected finding makes the harness fail"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"missing expected Schematron rule"
         (validate/validate-tei-schematron!
          {:schema-path "schemas/tei-profile.sch"
           :valid-fixtures []
           :warning-fixtures {}
           :invalid-fixtures {"fixtures/tei/valid/rashomon-minimal.xml"
                              #{"abc-tei-header-title"}}})))))

(deftest validate-tei-schematron-valid-fixture-loud-fail-test
  (testing "valid fixtures must have no error findings"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"unexpected Schematron error"
         (validate/validate-tei-schematron!
          {:schema-path "schemas/tei-profile.sch"
           :valid-fixtures ["fixtures/tei/invalid/missing-title.xml"]
           :warning-fixtures {}
           :invalid-fixtures {}})))))

(def ^:private expected-tei-schematron-fixtures-snapshot
  "Hard-coded snapshot of the production TEI Schematron fixture map as of
  the refactor. It pins the current partition so any change to the
  production map is deliberate and reviewable."
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

(deftest tei-schematron-fixtures-unchanged-test
  (testing "hand-coded Schematron fixture map is unchanged and still passes"
    (is (= expected-tei-schematron-fixtures-snapshot validate/tei-schematron-fixtures))
    (is (nil? (validate/validate-tei-schematron! validate/tei-schematron-fixtures)))))

(deftest tei-schematron-rule-universe-test
  (testing "rule-universe extracts exactly the 13 abc-* ids from the ODD"
    (is (= #{"abc-tei-header-title"
             "abc-tei-header-source-work-id"
             "abc-header-language-declared"
             "abc-ruby-complete"
             "abc-ruby-base-non-empty"
             "abc-ruby-reading-non-empty"
             "abc-gaiji-reference"
             "abc-gaiji-chardecl-resolution"
             "abc-char-resolution-form"
             "abc-figure-accessibility"
             "abc-source-span-reference"
             "abc-source-span-target-exists"
             "abc-transcription-vs-annotation"}
           (validate/rule-universe)))))

(def ^:private bundle-args
  {:record-path "examples/v0/example-work/metadata-record.json"
   :manifest-path "examples/v0/example-work/manifest.json"
   :persons-dir "examples/v0/example-persons"
   :record-schema-path "schemas/metadata-record.schema.json"
   :person-schema-path "schemas/person-record.schema.json"
   :ttl-path "examples/v0/example-work/metadata-record.ttl"})

(deftest validate-metadata-bundle-smoke-test
  (testing "validate-metadata-bundle! returns nil for the example fixture"
    (let [shapes (shacl/load-shapes-graph)]
      (is (nil? (validate/validate-metadata-bundle!
                 (assoc bundle-args :shapes-graph shapes)))))))

(deftest validate-metadata-bundle-hash-mismatch-test
  (testing "validate-metadata-bundle! throws when the manifest's metadata_record_hash is wrong"
    (let [shapes (shacl/load-shapes-graph)
          tmp-manifest (java.io.File/createTempFile "abc-bad-manifest" ".json")
          original (files/read-json "examples/v0/example-work/manifest.json")
          mutated (assoc-in original
                            ["manifest_identity_object" "metadata_record_hash"]
                            "sha256:0000000000000000000000000000000000000000000000000000000000000000")]
      (try
        ((requiring-resolve 'abc.tools.json/write-deterministic-json-file!)
         tmp-manifest mutated)
        (try
          (validate/validate-metadata-bundle!
           (assoc bundle-args :manifest-path (str tmp-manifest) :shapes-graph shapes))
          (is false "expected hash-mismatch throw")
          (catch clojure.lang.ExceptionInfo e
            (is (re-find #"metadata_record_hash mismatch" (ex-message e)))))
        (finally (.delete tmp-manifest))))))

(deftest validate-metadata-bundle-schema-hash-mismatch-test
  (testing "validate-metadata-bundle! throws when the record's schema-hash is stale"
    (let [shapes (shacl/load-shapes-graph)
          tmp-record (java.io.File/createTempFile "abc-bad-record" ".json")
          record (files/read-json "examples/v0/example-work/metadata-record.json")
          mutated (assoc record "metadata_record_schema_hash"
                         "sha256:0000000000000000000000000000000000000000000000000000000000000000")]
      (try
        ((requiring-resolve 'abc.tools.json/write-deterministic-json-file!)
         tmp-record mutated)
        (try
          (validate/validate-metadata-bundle!
           (assoc bundle-args :record-path (str tmp-record) :shapes-graph shapes))
          (is false "expected schema-hash-mismatch throw")
          (catch clojure.lang.ExceptionInfo e
            (is (re-find #"metadata_record_schema_hash mismatch" (ex-message e)))))
        (finally (.delete tmp-record))))))

(deftest validate-metadata-bundle-stale-contributor-hash-test
  (testing "mutating a person file's bytes makes the harness fail with the person_id and both hashes"
    (let [shapes (shacl/load-shapes-graph)
          persons-dir (.toFile (java.nio.file.Files/createTempDirectory
                                "abc-vdb-persons"
                                (make-array java.nio.file.attribute.FileAttribute 0)))
          orig (files/read-json "examples/v0/example-persons/000879.json")
          mutated (assoc orig "family_name_romaji" "Akutagawa-MUTATED")]
      (try
        ((requiring-resolve 'abc.tools.json/write-deterministic-json-file!)
         (java.io.File. persons-dir "000879.json") mutated)
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo #"000879"
             (validate/validate-metadata-bundle!
              (assoc bundle-args
                     :persons-dir (str persons-dir)
                     :shapes-graph shapes))))
        (finally
          (doseq [f (reverse (file-seq persons-dir))] (.delete f)))))))

(deftest validate-metadata-bundle-person-schema-hash-drift-test
  (testing "a person file with a stale embedded schema hash makes the harness fail"
    (let [shapes (shacl/load-shapes-graph)
          persons-dir (.toFile (java.nio.file.Files/createTempDirectory
                                "abc-vdb-pschema"
                                (make-array java.nio.file.attribute.FileAttribute 0)))
          orig (files/read-json "examples/v0/example-persons/000879.json")
          mutated (assoc orig "person_record_schema_hash"
                         "sha256:0000000000000000000000000000000000000000000000000000000000000000")]
      (try
        ((requiring-resolve 'abc.tools.json/write-deterministic-json-file!)
         (java.io.File. persons-dir "000879.json") mutated)
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo #"person_record_schema_hash"
             (validate/validate-metadata-bundle!
              (assoc bundle-args
                     :persons-dir (str persons-dir)
                     :shapes-graph shapes))))
        (finally
          (doseq [f (reverse (file-seq persons-dir))] (.delete f)))))))

(deftest validate-tei-warning-partition-test
  (testing "validate-tei! does not throw when only warnings are present"
    (if (tei-schema-tests-skipped?)
      (assert-tei-schema-test-skipped!)
      (let [schema-path (System/getenv "TEI_SCHEMA_PATH")
            tmp (java.io.File/createTempFile "abc-tei-warn" ".xml")]
        (try
          (spit tmp (str "<?xml version=\"1.0\"?>"
                         "<TEI xmlns=\"http://www.tei-c.org/ns/1.0\">"
                         "  <teiHeader><fileDesc>"
                         "    <titleStmt><title>t</title></titleStmt>"
                         "    <publicationStmt><p>p</p></publicationStmt>"
                         "    <sourceDesc><p>s</p></sourceDesc>"
                         "  </fileDesc></teiHeader>"
                         "  <text><body>"
                         "    <p>see <ref target=\"#missing\">link</ref></p>"
                         "  </body></text>"
                         "</TEI>"))
          (require '[abc.tools.tei :as tei])
          (let [{:keys [violations]} ((resolve 'abc.tools.tei/validate!)
                                      {:schema-path schema-path
                                       :xml-path (str tmp)
                                       :label "warn"})
                warnings (filter #(= :warning (:severity %)) violations)]
            (is (empty? (filter #(#{:error :fatal} (:severity %)) violations))
                "fixture must not contain TEI errors")
            (is (nil? (validate/validate-tei! schema-path [(str tmp)]))
                (if (seq warnings)
                  "harness must not throw when only warnings are present"
                  "harness must not throw when this fixture is clean under the current Jing version"))
            (when-not (seq warnings)
              (println "validate-tei-warning-partition-test: no warnings"
                       "in this fixture under Jing 20241231; severities seen:"
                       (vec (distinct (map :severity violations))))))
          (finally
            (.delete tmp)))))))

(deftest validate-drift-fixtures-smoke-test
  (testing "drift fixture runner returns nil when expected failures are observed"
    (is (nil? (validate/validate-drift-fixtures!
               {"fixtures/v0/invalid/drift/broken-index-target"
                #{:index-target-missing}
                "fixtures/v0/invalid/drift/asymmetric-index"
                #{:event-missing-from-participant-index}
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
                #{:rdf-participant-prov-mismatch}})))))

(deftest validate-drift-fixtures-rejects-unexpected-failure-codes-test
  (testing "drift fixture runner requires the expected code set to be exact"
    (is (thrown? clojure.lang.ExceptionInfo
                 (validate/validate-drift-fixtures!
                  {"fixtures/v0/invalid/drift/invalid-agent" #{}})))))

(deftest validate-drift-fixtures-rejects-unknown-actual-failure-codes-test
  (testing "drift fixture runner rejects emitted codes outside the registry even when expected exactly"
    (with-redefs [validate/validate-drift-fixture-result
                  (fn [_fixture]
                    {:status :error
                     :failures [{:code :not-a-registered-drift-code}]})]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"unknown drift validation failure codes"
           (validate/validate-drift-fixtures!
            {"synthetic-drift-fixture" #{:not-a-registered-drift-code}}))))))
