(ns abc.tools.validate-design-bundle-test
  (:require [abc.tools.files :as files]
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

(def ^:private complete-manifest-inputs
  {"producer" "ab-validator"
   "producer_version" "0.0.0"
   "work_id" "fixture"
   "corpus_snapshot_hash" (files/example-hash "00")
   "work_content_hash" (files/example-hash "01")
   "parser_build_hash" (files/example-hash "02")
   "parser_config_hash" (files/example-hash "03")
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
  (testing "rejects invalid hash values"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"sha256: hash"
         (am/explain-or-throw!
          ::am/manifest-inputs
          (assoc complete-manifest-inputs "work_content_hash" "nope")
          "test")))))

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
        {"parser_ir_schema_hash" "sha256:13e3127fe8eaa0649f83fd5c12e11923115810b454c6d3d22996b00e1218623f"
         "diagnostic_schema_hash" "sha256:e21ef2abdbf64b6fc920b4ef9a3df0e426b7bcc1cad0a6bbdd654f41e8ff302d"})))
  (is (= ["ab-validator parser_ir_schema_hash sha256:0000000000000000000000000000000000000000000000000000000000000004 does not match ABC parser IR schema hash sha256:13e3127fe8eaa0649f83fd5c12e11923115810b454c6d3d22996b00e1218623f"
          "ab-validator diagnostic_schema_hash sha256:0000000000000000000000000000000000000000000000000000000000000008 does not match ABC diagnostic schema hash sha256:e21ef2abdbf64b6fc920b4ef9a3df0e426b7bcc1cad0a6bbdd654f41e8ff302d"]
         (validate/schema-hash-errors
          {"parser_ir_schema_hash" (files/example-hash "04")
           "diagnostic_schema_hash" (files/example-hash "08")}))))

(deftest parser-ir-schema-hash-errors-test
  (is (empty?
       (validate/parser-ir-schema-hash-errors
        {"schema_hash" "sha256:13e3127fe8eaa0649f83fd5c12e11923115810b454c6d3d22996b00e1218623f"})))
  (is (= ["ab-validator parser IR schema_hash sha256:0000000000000000000000000000000000000000000000000000000000000004 does not match ABC parser IR schema hash sha256:13e3127fe8eaa0649f83fd5c12e11923115810b454c6d3d22996b00e1218623f"]
         (validate/parser-ir-schema-hash-errors
          {"schema_hash" (files/example-hash "04")}))))

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

(deftest validate-tei-smoke-test
  (testing "validate-tei! returns nil for the example fixture when TEI_SCHEMA_PATH is set"
    (when-not (= "1" (System/getenv tei-skip-flag))
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
    (when-not (= "1" (System/getenv tei-skip-flag))
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
            (if (seq warnings)
              (is (nil? (validate/validate-tei! schema-path [(str tmp)]))
                  "harness must not throw when only warnings are present")
              (println "validate-tei-warning-partition-test: no warnings"
                       "in this fixture under Jing 20241231; severities seen:"
                       (vec (distinct (map :severity violations))))))
          (finally
            (.delete tmp)))))))
