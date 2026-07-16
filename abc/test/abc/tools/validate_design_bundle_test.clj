(ns abc.tools.validate-design-bundle-test
  (:require [abc.tools.aat-parser-ir-compat :as compat]
            [abc.tools.adr-evidence-runtime-inputs :as runtime]
            [abc.tools.evidence-test-support :as evidence-support]
            [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.evidence-io :as evidence-io]
            [abc.tools.json :as abc-json]
            [abc.tools.malli :as am]
            [abc.tools.manifest :as manifest]
            [abc.tools.manifest-index :as manifest-index]
            [abc.tools.manifest-to-rdf :as manifest-to-rdf]
            [abc.tools.materialize-import :as materialize]
            [abc.tools.parser-evidence :as parser-evidence]
            [abc.tools.parser-ir-plaintext :as plaintext]
            [abc.tools.parser-ir-sentence-policy :as sentence-policy]
            [abc.tools.schema :as schema]
            [abc.tools.shacl :as shacl]
            [abc.tools.schematron :as schematron]
            [abc.tools.validate-design-bundle :as validate]
            [babashka.fs :as fs]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [malli.core :as m]
            [malli.generator :as mg]))

(use-fixtures :once (fn [f] (am/install!) (f)))

(defn- with-corpus-generation-ref [index]
  (assoc index "corpus_generation_ref"
         (hash/format-sha256
          (hash/sha256-json-rfc8785-safe-integer-v1
           (dissoc index "corpus_generation_ref")))))

(deftest parser-rq-source-recognition-protocols
  (let [root "test/fixtures/parser-rq/source-recognition"
        work-schema (files/read-json
                     "schemas/parser-rq-source-recognition-work.schema.json")
        index-schema (files/read-json
                      "schemas/parser-rq-source-recognition-index.schema.json")
        aggregate-schema (files/read-json
                          "schemas/parser-rq-source-recognition-aggregate.schema.json")
        work (files/read-json (str root "/work-ok.json"))
        unavailable-work (files/read-json (str root "/work-unavailable.json"))
        index (files/read-json (str root "/index-ok.json"))
        aggregate (files/read-json (str root "/aggregate-ok.json"))
        unavailable-aggregate
        (files/read-json (str root "/aggregate-unavailable.json"))]
    (testing "closed valid fixtures"
      (doseq [[contract value] [[work-schema work]
                                [work-schema unavailable-work]
                                [index-schema index]
                                [aggregate-schema aggregate]
                                [aggregate-schema unavailable-aggregate]]]
        (is (nil? (schema/validation-errors contract value))))
      (is (empty? (validate/parser-rq-source-recognition-work-errors work)))
      (is (empty? (validate/parser-rq-source-recognition-index-errors index)))
      (is (empty? (validate/parser-rq-source-recognition-aggregate-errors
                   aggregate)))
      (is (empty? (validate/parser-rq-source-recognition-coherence-errors
                   index aggregate [work]))))
    (testing "unknown fields and unavailable trusted totals are rejected"
      (is (seq (schema/validation-errors work-schema (assoc work "unknown" true))))
      (is (seq (schema/validation-errors
                work-schema (assoc unavailable-work "eligible_bytes" 10))))
      (is (seq (schema/validation-errors
                aggregate-schema
                (assoc unavailable-aggregate "recognized_bytes" 0))))
      (is (seq (schema/validation-errors
                aggregate-schema
                (assoc-in aggregate ["work_completeness" "complete"] false))))
      (is (seq (schema/validation-errors
                index-schema
                (assoc index "corpus_generation_algorithm" "legacy-jcs"))))
      (is (seq (schema/validation-errors
                aggregate-schema
                (assoc aggregate "corpus_generation_algorithm" "legacy-jcs")))))
    (testing "corpus numeric identity is safe-integer only and fails as data"
      (doseq [value [9007199254740992 1.5 Double/MIN_VALUE]]
        (let [candidate (assoc index "expected_work_count" value)]
          (is (seq (schema/validation-errors index-schema candidate)))
          (is (seq (validate/parser-rq-source-recognition-index-errors
                    candidate))))))
    (testing "projections are ordered subsets with exact byte conservation"
      (doseq [invalid [(assoc work "recognized_bytes" 11)
                       (assoc work "accounted_bytes" 11)
                       (assoc work "recognized"
                              [{"start" 0 "end" 6}
                               {"start" 4 "end" 10}])
                       (assoc work "recognized" [{"start" 0 "end" 11}])
                       (assoc work "recognized" [{"start" 0 "end" 10}])
                       (assoc work "semantic_gaps" [{"start" 5 "end" 6}])]]
        (is (seq (validate/parser-rq-source-recognition-work-errors invalid)))))
    (testing "membership and identity bindings are exact"
      (is (seq (validate/parser-rq-source-recognition-index-errors
                (assoc index "records" []))))
      (is (seq (validate/parser-rq-source-recognition-index-errors
                (assoc index "expected_work_ids" ["another-work"]))))
      (is (seq (validate/parser-rq-source-recognition-index-errors
                (assoc index
                       "expected_work_ids" ["fixture-work" "fixture-work"]
                       "expected_work_count" 2))))
      (is (seq (validate/parser-rq-source-recognition-coherence-errors
                index aggregate [])))
      (is (seq (validate/parser-rq-source-recognition-coherence-errors
                index aggregate [(assoc work "capture_generation_ref"
                                        (str "sha256:" (apply str (repeat 64 "f"))))])))
      (is (seq (validate/parser-rq-source-recognition-coherence-errors
                index
                (assoc aggregate "corpus_generation_algorithm" "legacy-jcs")
                [work]))))
    (testing "corpus identity authenticates distinct per-work capture mappings"
      (let [other-capture (str "sha256:" (apply str (repeat 64 "7")))
            other-work (assoc work
                              "work_id" "other-work"
                              "capture_generation_ref" other-capture)
            other-entry {"work_id" "other-work"
                         "capture_generation_ref" other-capture
                         "sha256" (str "sha256:" (apply str (repeat 64 "8")))
                         "bytes" 513
                         "media_type" "application/json"
                         "locator" "records/other-work/recognition.json"}
            multi-index (-> index
                            (assoc "expected_work_ids"
                                   ["fixture-work" "other-work"]
                                   "expected_work_count" 2
                                   "record_count" 2
                                   "records" [(first (get index "records")) other-entry])
                            with-corpus-generation-ref)
            multi-aggregate (-> aggregate
                                (assoc "corpus_generation_ref"
                                       (get multi-index "corpus_generation_ref")
                                       "work_completeness"
                                       {"expected" 2 "observed" 2 "complete" true}
                                       "eligible_bytes" 20
                                       "recognized_bytes" 16
                                       "accounted_bytes" 20
                                       "semantic_gap_bytes" 4
                                       "semantic_gaps"
                                       [{"work_id" "fixture-work" "start" 4 "end" 6}
                                        {"work_id" "other-work" "start" 4 "end" 6}]))]
        (is (not= (get work "capture_generation_ref")
                  (get other-work "capture_generation_ref")))
        (is (empty? (validate/parser-rq-source-recognition-index-errors
                     multi-index)))
        (is (empty? (validate/parser-rq-source-recognition-coherence-errors
                     multi-index multi-aggregate [work other-work])))
        (doseq [bad-index [(with-corpus-generation-ref
                             (update multi-index "records" pop))
                           (with-corpus-generation-ref
                             (assoc multi-index "records"
                                    [(first (get multi-index "records"))
                                     (first (get multi-index "records"))]))]]
          (is (seq (validate/parser-rq-source-recognition-index-errors
                    bad-index))))
        (let [swapped-index
              (with-corpus-generation-ref
                (assoc-in multi-index ["records" 0 "capture_generation_ref"]
                          other-capture))]
          (is (empty? (validate/parser-rq-source-recognition-index-errors
                       swapped-index)))
          (is (seq (validate/parser-rq-source-recognition-coherence-errors
                    swapped-index
                    (assoc multi-aggregate "corpus_generation_ref"
                           (get swapped-index "corpus_generation_ref"))
                    [work other-work]))))
        (is (empty? (validate/parser-rq-source-recognition-coherence-errors
                     multi-index multi-aggregate [other-work work])))))
    (testing "available aggregates require an available complete exact fold"
      (doseq [[candidate-index candidate-aggregate candidate-records]
              [[(assoc index "status" "unavailable" "errors" ["index failed"])
                aggregate [work]]
               [index (assoc-in aggregate ["work_completeness" "complete"] false)
                [work]]
               [index (assoc-in aggregate ["work_completeness" "expected"] 99)
                [work]]
               [index (assoc-in aggregate ["work_completeness" "observed"] 0)
                [work]]
               [index aggregate [work work]]
               [index aggregate []]
               [index aggregate [(assoc work "work_id" "extra-work")]]]]
        (is (seq (validate/parser-rq-source-recognition-coherence-errors
                  candidate-index candidate-aggregate candidate-records))))
      (let [empty-index (with-corpus-generation-ref
                          (assoc index
                                 "expected_work_ids" []
                                 "expected_work_count" 0
                                 "record_count" 0
                                 "records" []))
            empty-aggregate (assoc aggregate
                                   "corpus_generation_ref"
                                   (get empty-index "corpus_generation_ref")
                                   "work_completeness"
                                   {"expected" 0 "observed" 0 "complete" true}
                                   "eligible_bytes" 0
                                   "recognized_bytes" 0
                                   "accounted_bytes" 0
                                   "semantic_gap_bytes" 0
                                   "unaccounted_bytes" 0
                                   "semantic_gaps" []
                                   "unaccounted" [])]
        (is (empty? (validate/parser-rq-source-recognition-index-errors
                     empty-index)))
        (is (empty? (validate/parser-rq-source-recognition-coherence-errors
                     empty-index empty-aggregate [])))))
    (testing "aggregate totals and work witnesses are coherent"
      (is (seq (validate/parser-rq-source-recognition-aggregate-errors
                (assoc aggregate "recognized_bytes" 11))))
      (is (seq (validate/parser-rq-source-recognition-aggregate-errors
                (assoc aggregate "semantic_gaps"
                       [{"work_id" "fixture-work" "start" 4 "end" 11}]))))
      (is (seq (validate/parser-rq-source-recognition-aggregate-errors
                (assoc aggregate
                       "semantic_gap_bytes" 3
                       "semantic_gaps"
                       [{"work_id" "fixture-work" "start" 4 "end" 6}
                        {"work_id" "fixture-work" "start" 5 "end" 6}]))))
      (is (seq (validate/parser-rq-source-recognition-coherence-errors
                index (assoc aggregate "recognized_bytes" 7) [work]))))))

(deftest parser-rq-classified-source
  (let [root "test/fixtures/parser-rq/classified-source"
        policy-schema (files/read-json "schemas/parser-rq-classified-source-policy.schema.json")
        ledger-schema (files/read-json "schemas/parser-rq-classified-source-ledger.schema.json")
        generation-schema (files/read-json "schemas/parser-rq-capture-generation.schema.json")
        authority-schema (files/read-json "schemas/parser-rq-classified-source-authority.schema.json")
        policy (files/read-json "data/parser-rq-ab-aozora-classified-source-v1.json")
        ledger (files/read-json (str root "/ledger.json"))
        generation (files/read-json (str root "/generation.json"))
        authority (files/read-json
                   "data/parser-rq-classified-source-authority-v1.json")]
    (testing "closed valid fixtures and canonical hashes"
      (doseq [[contract value] [[policy-schema policy]
                                [ledger-schema ledger]
                                [generation-schema generation]]]
        (is (nil? (schema/validation-errors contract value))))
      (is (= (get policy "policy_hash")
             (hash/format-sha256
              (hash/sha256-json-jcs (dissoc policy "policy_hash")))))
      (is (= (get ledger "ledger_schema_hash")
             (hash/format-sha256 (hash/sha256-json-jcs ledger-schema)))))
    (testing "the compiled authority descriptor authenticates exact ABC bytes and identities"
      (is (nil? (schema/validation-errors authority-schema authority)))
      (is (seq (schema/validation-errors
                authority-schema
                (assoc-in authority ["generation_ref_contract" "projection"]
                          "all-fields"))))
      (doseq [[section path value identity]
              [["policy" "data/parser-rq-ab-aozora-classified-source-v1.json"
                (dissoc policy "policy_hash") (get policy "policy_hash")]
               ["ledger_schema" "schemas/parser-rq-classified-source-ledger.schema.json"
                ledger-schema (get ledger "ledger_schema_hash")]
               ["generation_schema" "schemas/parser-rq-capture-generation.schema.json"
                generation-schema
                "sha256:02a933e45f65f2bb1f1af08103de10c611fce2232addbaf754147bb9bf4dbcf5"]]]
        (is (= (get-in authority [section "raw_bytes_hash"])
               (hash/format-sha256 (hash/sha256-file path))))
        (is (= (get-in authority [section "identity_hash"])
               (hash/format-sha256 (hash/sha256-json-jcs value))))
        (is (= identity (get-in authority [section "identity_hash"])))))
    (is (= {"schemas/parser-rq-classified-source-policy.schema.json"
            "sha256:c9f68f073afcbdd2fca81e0e926c1428e7fcbb3f00307b5eff016a7c25276e60"
            "schemas/parser-rq-classified-source-authority.schema.json"
            "sha256:cfe47129b725e29c4a5a5922ccbf7e2a17e9e8c5db716a3d1baf083fbb41fe1c"
            "schemas/parser-rq-classified-source-ledger.schema.json"
            "sha256:f508dfeecb44cebbfb36ede4cfb72cee94cf44e5716041071345dae9c5e1530f"
            "schemas/parser-rq-capture-generation.schema.json"
            "sha256:02a933e45f65f2bb1f1af08103de10c611fce2232addbaf754147bb9bf4dbcf5"}
           (into {} (map (fn [path]
                           [path (hash/format-sha256
                                  (hash/sha256-json-jcs
                                   (files/read-json path)))])
                         ["schemas/parser-rq-classified-source-policy.schema.json"
                          "schemas/parser-rq-classified-source-authority.schema.json"
                          "schemas/parser-rq-classified-source-ledger.schema.json"
                          "schemas/parser-rq-capture-generation.schema.json"]))))
    (is (= ["visible_text" "structural_newline" "ruby" "typography"
            "gaiji" "layout" "break" "heading" "illustration" "kunten"
            "source_annotation" "container_syntax" "terminal_provenance"
            "publication_metadata" "unrecognized_source_form"]
           (get policy "roles")))
    (is (every? #(contains? % "construct_id") (get policy "rules")))
    (is (not-any? #(contains? % "case") (get policy "rules")))
    (is (= 114 (count (get policy "accent_mappings"))))
    (is (= #{["crlf_normalization" "structural_newline" "crlf"]
             ["bare_cr_normalization" "structural_newline" "bare_cr"]
             ["accent_normalization" "visible_text" "accent_decomposition"]}
           (set (for [rule (get policy "rules")
                      :when (= "lossless_normalization"
                               (get rule "disposition"))]
                  [(get rule "construct_id")
                   (get rule "source_role")
                   ({"crlf_normalization" "crlf"
                     "bare_cr_normalization" "bare_cr"
                     "accent_normalization" "accent_decomposition"}
                    (get rule "construct_id"))]))))
    (is (empty? (validate/parser-rq-classified-source-characterization-errors
                 policy
                 (files/read-json
                  (str root "/characterization-policy-map.json")))))
    (is (empty? (validate/parser-rq-capture-generation-errors generation)))
    (testing "production capture fixture satisfies the ABC protocols"
      (let [capture-root "test/fixtures/parser-rq/classified-source-capture"
            capture-ledger (files/read-json (str capture-root "/ledger.json"))
            capture-generation (files/read-json (str capture-root "/generation.json"))
            decoded (slurp (str capture-root "/decoded.txt"))]
        (is (nil? (schema/validation-errors ledger-schema capture-ledger)))
        (is (nil? (schema/validation-errors generation-schema capture-generation)))
        (is (empty? (validate/parser-rq-classified-source-ledger-errors
                     policy capture-ledger decoded)))
        (is (empty? (validate/parser-rq-capture-generation-errors
                     capture-generation)))))
    (testing "unknown fields and duplicate policy rows are rejected"
      (is (seq (schema/validation-errors policy-schema (assoc policy "unknown" true))))
      (is (seq (schema/validation-errors ledger-schema (assoc ledger "unknown" true))))
      (is (seq (schema/validation-errors generation-schema
                                         (assoc generation "unknown" true))))
      (is (seq (validate/parser-rq-classified-source-policy-errors
                (update policy "rules" conj (first (get policy "rules"))))))
      (is (seq (validate/parser-rq-classified-source-policy-errors
                (update policy "accent_mappings" conj
                        (assoc (first (get policy "accent_mappings"))
                               "normalized" "x"))))))
    (testing "closed role/disposition and evidence combinations"
      (let [entry (first (get ledger "entries"))]
        (is (seq (schema/validation-errors
                  ledger-schema
                  (assoc-in ledger ["entries" 0 "source_role"] "*"))))
        (is (seq (schema/validation-errors
                  ledger-schema
                  (-> ledger
                      (assoc-in ["entries" 0 "disposition"] "preserved_opaque")
                      (assoc-in ["entries" 0 "source_role"] "visible_text")))))
        (is (seq (schema/validation-errors
                  ledger-schema
                  (update-in ledger ["entries" 0] dissoc "target_identity"))))
        (is (seq (schema/validation-errors
                  ledger-schema
                  (assoc-in ledger ["entries" 0 "target_identity" "unknown"] true))))
        (is (seq (schema/validation-errors
                  ledger-schema
                  (update ledger "entries" conj entry))))
        (is (seq (schema/validation-errors
                  ledger-schema
                  (assoc ledger "entries"
                         [(-> entry
                              (assoc "source_role" "structural_newline"
                                     "disposition" "structural_control")
                              (dissoc "target_identity"))]))))))
    (testing "generation identity is acyclic and authenticates real member bytes"
      (is (not-any? #(contains? % "generation_ref")
                    (vals (get generation "members"))))
      (doseq [[_ member] (get generation "members")]
        (is (= (get member "value_hash")
               (hash/format-sha256
                (hash/sha256-file (str root "/" (get member "artifact_ref")))))))
      (is (seq (validate/parser-rq-capture-generation-errors
                (assoc-in generation ["members" "raw_diagnostics" "value_hash"]
                          (str "sha256:" (apply str (repeat 64 "f"))))))))
    (testing "lossless normalization is closed, target-bound, and reversible"
      (let [normalization (files/read-json (str root "/normalization-ledger.json"))
            invalid [(update-in normalization ["entries" 0] dissoc "target_identity")
                     (update-in normalization ["entries" 0] dissoc "normalization_proof")
                     (assoc-in normalization ["entries" 0 "normalization_proof"
                                              "inverse_rule"] "structural_newline")
                     (assoc-in normalization ["entries" 0 "normalization_proof"
                                              "source_form"] "\n")
                     (assoc-in normalization ["entries" 0 "normalization_proof"
                                              "normalized_form"] "\r\n")
                     (assoc-in normalization ["entries" 0 "target_identity"
                                              "relation"] "preserves")]]
        (is (nil? (schema/validation-errors ledger-schema normalization)))
        (is (empty? (validate/parser-rq-classified-source-ledger-errors
                     policy normalization "\r\n")))
        (doseq [ledger invalid]
          (is (seq (validate/parser-rq-classified-source-ledger-errors
                    policy ledger "\r\n"))))
        (is (seq (schema/validation-errors
                  ledger-schema
                  (-> normalization
                      (assoc-in ["entries" 0 "construct_id"] "newline")
                      (assoc-in ["entries" 0 "disposition"]
                                "structural_control")))))))
    (testing "accent normalization equals the closed live mapping"
      (let [accent (files/read-json
                    (str root "/accent-normalization-ledger.json"))
            arbitrary (-> accent
                          (assoc-in ["entries" 0 "normalization_proof"
                                     "normalized_form"] "〔arbitrary〕")
                          (assoc-in ["entries" 0 "normalization_proof"
                                     "normalized_bytes_hash"]
                                    "sha256:00354980966b90c6ebc563c850cebb14b3a3057a216383cc832113e81455dc83"))
            wrong-source (-> accent
                             (assoc-in ["entries" 0 "normalization_proof"
                                        "source_form"] "〔cafe`〕")
                             (assoc-in ["entries" 0 "normalization_proof"
                                        "source_bytes_hash"]
                                       "sha256:3d23d9c487249a3de1ffe1854e535765bd9039ff7b9a216c1a57b84b35b5f5df"))
            unsupported (-> accent
                            (assoc-in ["entries" 0 "end"] 8)
                            (assoc-in ["entries" 0 "normalization_proof"
                                       "source_form"] "〔q^〕")
                            (assoc-in ["entries" 0 "normalization_proof"
                                       "source_bytes_hash"]
                                      "sha256:36d9432b7d410d455ac3655e4a7e83d430ffe89e6dfcd5a2805795e7f8bd9236"))]
        (is (nil? (schema/validation-errors ledger-schema accent)))
        (is (empty? (validate/parser-rq-classified-source-ledger-errors
                     policy accent "〔cafe'〕")))
        (is (seq (validate/parser-rq-classified-source-ledger-errors
                  policy arbitrary "〔cafe'〕")))
        (is (seq (validate/parser-rq-classified-source-ledger-errors
                  policy wrong-source "〔cafe`〕")))
        (is (seq (validate/parser-rq-classified-source-ledger-errors
                  policy unsupported "〔q^〕")))))
    (testing "structural witnesses are role-specific and span-bound"
      (let [structural (files/read-json (str root "/structural-ledger.json"))
            forms {"newline" "\n"
                   "warichu_open" "［＃割り注］"
                   "page_break" "［＃改ページ］"
                   "section_break" "［＃改丁］"
                   "body_end" "［＃本文終わり］"
                   "forced_break" "［＃改行］"
                   "container_open" "［＃ここから］"
                   "container_close" "［＃ここで終わり］"}
            structural-rules (filter #(= "structural_control"
                                         (get % "disposition"))
                                     (get policy "rules"))]
        (is (nil? (schema/validation-errors ledger-schema structural)))
        (is (empty? (validate/parser-rq-classified-source-ledger-errors
                     policy structural "\n")))
        (doseq [rule structural-rules
                :let [source-form (get forms (get rule "witness_kind"))
                      end (alength (.getBytes ^String source-form "UTF-8"))
                      entry (-> (get-in structural ["entries" 0])
                                (assoc "end" end
                                       "construct_id" (get rule "construct_id")
                                       "source_role" (get rule "source_role")
                                       "evidence_class" (get rule "evidence_class"))
                                (assoc "construct_witness"
                                       {"construct_id" (get rule "witness_kind")
                                        "start" 0 "end" end
                                        "source_form" source-form}))
                      ledger-for-rule (assoc structural "entries" [entry])]]
          (is (empty? (validate/parser-rq-classified-source-ledger-errors
                       policy ledger-for-rule source-form)))
          (is (seq (validate/parser-rq-classified-source-ledger-errors
                    policy
                    (assoc-in ledger-for-rule ["entries" 0 "source_role"]
                              "publication_metadata")
                    source-form))))
        (doseq [invalid [(assoc-in structural ["entries" 0 "construct_witness"
                                               "construct_id"] "page_break")
                         (assoc-in structural ["entries" 0 "construct_witness"
                                               "end"] 2)
                         (assoc-in structural ["entries" 0 "construct_witness"
                                               "source_form"] "x")]]
          (is (seq (validate/parser-rq-classified-source-ledger-errors
                    policy invalid "\n"))))
        (let [opaque-entry {"start" 0 "end" 3
                            "construct_id" "recovered_verbatim"
                            "source_role" "unrecognized_source_form"
                            "disposition" "preserved_opaque"
                            "evidence_class" "recovered_verbatim"
                            "parser_evidence_code" "recovered-verbatim"
                            "construct_witness"
                            {"construct_id" "recovered_verbatim"
                             "start" 0 "end" 3 "source_form" "※"}}
              opaque (assoc structural "entries" [opaque-entry])]
          (is (empty? (validate/parser-rq-classified-source-ledger-errors
                       policy opaque "※")))
          (is (seq (validate/parser-rq-classified-source-ledger-errors
                    policy
                    (assoc-in opaque ["entries" 0 "evidence_class"]
                              "accepted_text")
                    "※")))
          (is (seq (validate/parser-rq-classified-source-ledger-errors
                    policy
                    (-> opaque
                        (assoc-in ["entries" 0 "end"] 1)
                        (assoc-in ["entries" 0 "construct_witness" "end"] 1)
                        (assoc-in ["entries" 0 "construct_witness" "source_form"]
                                  "�"))
                    "※"))))))))

(deftest parser-rq-source-accountability-schemas-are-closed-test
  (let [pairs [["schemas/parser-rq-ignored-regions.schema.json"
                "test/fixtures/parser-rq/parser-rq-ignored-regions.schema.json"]
               ["schemas/parser-rq-source-accountability-work.schema.json"
                "test/fixtures/parser-rq/parser-rq-source-accountability-work.schema.json"]
               ["schemas/parser-rq-source-accountability-index.schema.json"
                "test/fixtures/parser-rq/parser-rq-source-accountability-index.schema.json"]
               ["schemas/parser-rq-source-accountability-aggregate.schema.json"
                "test/fixtures/parser-rq/parser-rq-source-accountability-aggregate.schema.json"]]]
    (doseq [[path fixture] pairs]
      (let [contract (files/read-json path)]
        (is (= false (get contract "additionalProperties")) path)
        (is (nil? (schema/validation-errors contract
                                            (files/read-json fixture))) path)))))

(deftest parser-rq-source-accountability-schemas-reject-unknown-fields-test
  (doseq [[path fixture] [["schemas/parser-rq-ignored-regions.schema.json"
                           "test/fixtures/parser-rq/parser-rq-ignored-regions.schema.json"]
                          ["schemas/parser-rq-source-accountability-work.schema.json"
                           "test/fixtures/parser-rq/parser-rq-source-accountability-work.schema.json"]
                          ["schemas/parser-rq-source-accountability-index.schema.json"
                           "test/fixtures/parser-rq/parser-rq-source-accountability-index.schema.json"]
                          ["schemas/parser-rq-source-accountability-aggregate.schema.json"
                           "test/fixtures/parser-rq/parser-rq-source-accountability-aggregate.schema.json"]]]
    (let [contract (files/read-json path)
          document (assoc (files/read-json fixture) "unexpected" true)]
      (is (seq (schema/validation-errors contract document)) path))))

(deftest parser-rq-source-accountability-work-provenance-boundaries-test
  (let [contract (files/read-json
                  "schemas/parser-rq-source-accountability-work.schema.json")
        document (files/read-json
                  "test/fixtures/parser-rq/parser-rq-source-accountability-work.schema.json")
        unavailable-lossy (-> document
                              (assoc "status" "unavailable"
                                     "errors" ["lossy-source-decode"])
                              (assoc-in ["decoded_source" "encoding"]
                                        "windows-31j-lossy"))
        unavailable-no-diagnostics (dissoc unavailable-lossy "diagnostics")
        invalid-documents [(dissoc document "coverage_basis")
                           (assoc document "coverage_basis" "parser_ir.paragraphs[*].span")
                           (update document "diagnostics" dissoc "profile")
                           (assoc-in document ["diagnostics" "profile"]
                                     "abc/authorized-parser-diagnostics-schema-v3")
                           (assoc-in document ["decoded_source" "encoding"]
                                     "windows-31j-lossy")
                           (dissoc document "diagnostics")
                           (assoc unavailable-lossy "status" "ok" "errors" [])
                           (assoc-in document ["diagnostics" "unexpected"] true)]]
    (doseq [valid [unavailable-lossy unavailable-no-diagnostics]]
      (is (nil? (schema/validation-errors contract valid))))
    (doseq [invalid invalid-documents]
      (is (seq (schema/validation-errors contract invalid))))))

(deftest parser-rq-source-accountability-unavailable-aggregate-rejects-numeric-test
  (let [contract (files/read-json
                  "schemas/parser-rq-source-accountability-aggregate.schema.json")
        document (-> (files/read-json
                      "test/fixtures/parser-rq/parser-rq-source-accountability-aggregate.schema.json")
                     (assoc "status" "unavailable" "errors" ["record unavailable"]))]
    (is (seq (schema/validation-errors contract document)))))

(deftest parser-rq-v1-taxonomy-is-empty-test
  (let [taxonomy (files/read-json "data/parser-rq-ignored-regions-v1.json")]
    (is (= "parser-rq-ignored-regions-v1" (get taxonomy "taxonomy_version")))
    (is (= "decoded_utf8" (get taxonomy "coordinate_system")))
    (is (= [] (get taxonomy "rules")))))

(deftest design-bundle-does-not-run-repository-history-checks-test
  (let [git-cliff-var (ns-resolve 'abc.tools.validate-design-bundle
                                  'validate-git-cliff!)
        reached? (atom false)
        run! #(with-redefs [validate/validate-publication-output! (fn [_])
                            validate/validate-xml! (fn [])
                            validate/validate-tei! (fn [& _])
                            validate/validate-tei-schematron! (fn [_])]
                (validate/validate-design-bundle!))]
    (if git-cliff-var
      (with-redefs-fn {git-cliff-var
                       (fn []
                         (reset! reached? true)
                         (throw (ex-info "repository-history sentinel reached" {})))}
        run!)
      (run!))
    (is (false? @reached?))))

(defn- evidence-input-catalog-equals-the-pure-schema-validation-read-set-assertions []
  (let [traced (evidence-io/with-read-trace
                 {:identity-root "." :cwd-root "."}
                 #(validate/validate-json-schemas! []))]
    (is (= (validate/evidence-input-paths)
           (:repository-paths traced)))))

(deftest evidence-input-catalog-equals-the-pure-schema-validation-read-set-test
  (runtime/with-validated-read-trace!
    (evidence-support/focused-trace-options "adr-0008-c4-validation-read-catalog")
    (fn []
      (evidence-input-catalog-equals-the-pure-schema-validation-read-set-assertions))))

(deftest design-bundle-temporary-import-materialization-test
  (runtime/with-validated-read-trace!
    (evidence-support/focused-trace-options "adr-0009-c6-temporary-materialization")
    (fn []
      (evidence-io/with-owned-ephemeral-root
        (fn [root]
          (let [generated (materialize/materialize-import!
                           {:input-dir "examples/ab-validator-output"
                            :output-dir root
                            :generated-at materialize/default-generated-at})]
            (is (files/file? (:parser-ir generated)))
            (is (files/file? (:warnings generated)))))))))

(deftest publication-view-temp-directory-cleanup-test
  (fs/with-temp-dir [root {}]
    (let [committed (fs/file root "committed")
          paths {:manifest-path "unused"
                 :metadata-record-path "unused"
                 :context-path "unused"
                 :candidate-path (fs/file committed "candidate")
                 :expanded-path (fs/file committed "expanded")
                 :result-path (fs/file committed "result")}
          seen-temp (atom nil)
          writer (fn [{:keys [candidate-path expanded-path result-path]}]
                   (reset! seen-temp (fs/parent candidate-path))
                   (doseq [[source target] [[(:candidate-path paths) candidate-path]
                                            [(:expanded-path paths) expanded-path]
                                            [(:result-path paths) result-path]]]
                     (fs/copy source target {:replace-existing true})))]
      (fs/create-dirs committed)
      (doseq [path (map paths [:candidate-path :expanded-path :result-path])]
        (spit path "golden"))
      (with-redefs [abc.tools.linked-art/write-publication-view! writer]
        (validate/validate-publication-view! paths))
      (is (some? @seen-temp))
      (is (not (fs/exists? @seen-temp)))
      (reset! seen-temp nil)
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"forced failure"
           (with-redefs [abc.tools.linked-art/write-publication-view!
                         (fn [{:keys [candidate-path]}]
                           (reset! seen-temp (fs/parent candidate-path))
                           (throw (ex-info "forced failure" {})))]
             (validate/validate-publication-view! paths))))
      (is (some? @seen-temp))
      (is (not (fs/exists? @seen-temp))))))

(deftest design-bundle-outer-temp-directory-cleanup-test
  (testing "cleanup after successful validation"
    (let [seen-root (atom nil)
          result (#'validate/with-design-temp-dir
                  (fn [root]
                    (reset! seen-root root)
                    (spit (fs/file root "proof") "inside")
                    :validated))]
      (is (= :validated result))
      (is (some? @seen-root))
      (is (not (fs/exists? @seen-root)))))
  (testing "cleanup after thrown validation"
    (let [seen-root (atom nil)
          failure (ex-info "validation failed" {})]
      (is (identical?
           failure
           (try
             (#'validate/with-design-temp-dir
              (fn [root]
                (reset! seen-root root)
                (spit (fs/file root "proof") "inside")
                (throw failure)))
             (catch Throwable t t))))
      (is (some? @seen-root))
      (is (not (fs/exists? @seen-root))))))

(deftest run-command-retains-command-and-nonzero-exit-code-test
  (let [fixture (java.io.File/createTempFile "abc-run-command" ".sh")]
    (try
      (spit fixture "#!/bin/sh\nexit 9\n")
      (.setExecutable fixture true)
      (let [command [(.getAbsolutePath fixture)]
            exception (try
                        (apply validate/run-command! command)
                        nil
                        (catch clojure.lang.ExceptionInfo ex ex))]
        (is (some? exception))
        (is (= command (vec (:command (ex-data exception)))))
        (is (= 9 (:exit-code (ex-data exception)))))
      (finally
        (.delete fixture)))))

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

(deftest manifest-schema-accepts-analysis-result-sidecar-test
  (let [manifest-schema (files/read-json "schemas/manifest.schema.json")
        manifest {"manifest_schema_id" "https://w3id.org/abc/schemas/manifest.schema.json"
                  "artifact_id" (files/example-hash "26")
                  "artifact_kind" "analysis"
                  "validation_status" "passed"
                  "manifest_identity_object" {"manifest_schema_hash" (manifest/schema-hash "schemas/manifest.schema.json")
                                              "corpus_snapshot_hash" (files/example-hash "01")
                                              "work_content_hash" (files/example-hash "02")
                                              "metadata_record_hash" nil
                                              "parser_build_hash" (files/example-hash "03")
                                              "parser_config_hash" (files/example-hash "04")
                                              "aat_parser_ir_mapping_hash" (files/example-hash "05")
                                              "parser_ir_schema_hash" (files/example-hash "06")
                                              "tei_profile_hash" nil
                                              "tokenizer_build_hash" nil
                                              "tokenizer_dictionary_hash" nil
                                              "tokenizer_profile_hash" nil
                                              "analysis_recipe_hash" (files/example-hash "07")
                                              "annotation_policy_hash" nil
                                              "output_format_spec_hash" (manifest/schema-hash "schemas/analysis-result.schema.json")}
                  "content" {"content_hash" (files/example-hash "08")
                             "media_type" "application/json"
                             "byte_length" 10
                             "path_hint" "analysis-result.json"}
                  "sidecars" [{"role" "analysis-result"
                               "hash" (files/example-hash "08")
                               "media_type" "application/json"
                               "path_hint" "analysis-result.json"}]
                  "provenance" {"generated_at" "2026-07-07T00:00:00Z"
                                "activity_id" "https://w3id.org/abc/activity/materialize-analysis"
                                "agent" "abc.tools.materialize-analysis"
                                "plan_hash" nil
                                "used" [(files/example-hash "03")]
                                "was_derived_from" [(files/example-hash "03")]}
                  "license" nil
                  "signatures" []
                  "superseded_by" nil
                  "invalidated_at" nil
                  "replacement_reason" nil
                  "notes" nil}]
    (is (nil? (schema/validation-errors manifest-schema manifest)))))

(deftest manifest-schema-accepts-token-stream-sidecar-test
  (let [manifest-schema (files/read-json "schemas/manifest.schema.json")
        token-output-schema-hash (manifest/schema-hash
                                  "schemas/token-output.schema.json")
        token-stream-hash (files/example-hash "09")
        manifest {"manifest_schema_id" "https://w3id.org/abc/schemas/manifest.schema.json"
                  "artifact_id" (files/example-hash "27")
                  "artifact_kind" "tokenized"
                  "validation_status" "passed"
                  "manifest_identity_object" {"manifest_schema_hash" (manifest/schema-hash "schemas/manifest.schema.json")
                                              "corpus_snapshot_hash" (files/example-hash "01")
                                              "work_content_hash" (files/example-hash "02")
                                              "metadata_record_hash" nil
                                              "parser_build_hash" (files/example-hash "03")
                                              "parser_config_hash" (files/example-hash "04")
                                              "aat_parser_ir_mapping_hash" (files/example-hash "05")
                                              "parser_ir_schema_hash" (files/example-hash "06")
                                              "tei_profile_hash" nil
                                              "tokenizer_build_hash" (files/example-hash "10")
                                              "tokenizer_dictionary_hash" (files/example-hash "11")
                                              "tokenizer_profile_hash" (files/example-hash "12")
                                              "analysis_recipe_hash" nil
                                              "annotation_policy_hash" nil
                                              "output_format_spec_hash" token-output-schema-hash}
                  "content" {"content_hash" token-stream-hash
                             "media_type" "application/json"
                             "byte_length" 10
                             "path_hint" "token-stream.json"}
                  "sidecars" [{"role" "token-stream"
                               "hash" token-stream-hash
                               "media_type" "application/json"
                               "path_hint" "token-stream.json"}]
                  "provenance" {"generated_at" "2026-07-07T00:00:00Z"
                                "activity_id" "https://w3id.org/abc/activity/tokenize"
                                "agent" "abc.tools.tokenize"
                                "plan_hash" nil
                                "used" [(files/example-hash "03")
                                        (files/example-hash "12")]
                                "was_derived_from" [(files/example-hash "03")]}
                  "license" nil
                  "signatures" []
                  "superseded_by" nil
                  "invalidated_at" nil
                  "replacement_reason" nil
                  "notes" nil}]
    (is (nil? (schema/validation-errors manifest-schema manifest)))))

(def ^:private old-mapping-hash
  "sha256:af2aac0855b0ab42111b7a05aae7a6c337963446a7bc620d2c11790e524fbb03")

(def ^:private current-mapping-hash
  "sha256:4c0d3eb53942b4e1e14a6efc614bab99e391e90d85b817e090b42d02c05ba22e")

(def ^:private v2-mapping-hash
  "sha256:68b0868b25f3b072a47d781099178bf2a31e4b16c561814f5e13e3801714d089")

(def ^:private v3-mapping-hash
  "sha256:b508665af72c237fc60f00b720f80db2b16148aa64b5d1cc723a2948ee576390")

(def ^:private v4-mapping-hash
  "sha256:13734117384aede0ee484cbda1b44b29c96007a17f238a21788f567f7da8ea06")

(def ^:private v5-mapping-hash
  "sha256:feaab2d246fd17d79dc979012893400e0f5faacc0df04e260bee4f2b129299bf")

(def ^:private v6-mapping-hash
  "sha256:61d0d549bb73d45765757a739d738effc6a1979fa24cb8c878e9d2ba9d73f53e")

(def ^:private mapping-schema-hash
  "sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4")

(def ^:private current-mapping-schema-hash
  "sha256:23a2822cbae88533168121e8a09648441276d8af6484269ae666b90030eb1e06")

(def ^:private legacy-parser-ir-schema-hash
  "sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396")

(def ^:private v4-parser-ir-schema-hash
  "sha256:da916a3a92f64d985cb98f9b2ddc7f562e660fd0c3dbe0c902392d3764b0158a")

(def ^:private level3-parser-ir-schema-hash
  "sha256:c081f2365e2159e6e608733c4eb4e6fdf1fa80203ccd3d5e1f2afc533da8d411")

(def ^:private v5-parser-ir-schema-hash
  "sha256:a1fcd348bf396d8d4e6f30ffb928b76b3802b594ea773ed6fa9e1dac52edf712")

(def ^:private v6-parser-ir-schema-hash
  "sha256:0b495bb5c12c4d76482afefdaedb5464a74672ffbd5282f9c67d5f419d39a340")

(def ^:private v0-6-parser-ir-schema-hash
  "sha256:a1e1b5069fdec17cbb1f94eb5e9a582d1b109dd95c07257f4da7d9b76c82cfa2")

(def ^:private current-parser-ir-schema-hash
  "sha256:43a6a6d86ca5eca062508e6cae633d19bf5248f15c5bb46153a6d8580ea916ec")

(def ^:private parser-ir-schema-hash
  legacy-parser-ir-schema-hash)

(def ^:private valid-source-region-coverage
  {"schema_version" "aozora-source-region-coverage-v1"
   "gate_status" "SOURCE_AUTHORITY_GATE_PASS"
   "works_scanned" 17894
   "unknown_markers_total" 14886
   "unallowlisted_unknown_markers_total" 0
   "representability" {"typed_occurrences" 4570071
                       "raw_preserved_occurrences" 46382
                       "out_of_body_occurrences" 950
                       "malformed_noise_occurrences" 13936
                       "unsupported_occurrences" 0
                       "needs_research_occurrences" 0}
   "source_region_coverage" {"body_typed_occurrences" 4570071
                             "body_raw_preserved_occurrences" 46382
                             "source_apparatus_occurrences" 13920
                             "front_matter_occurrences" 14627
                             "back_matter_occurrences" 243
                             "malformed_source_occurrences" 16
                             "unsupported_body_markup_occurrences" 0
                             "unknown_region_occurrences" 0
                             "unknown_unreviewed_occurrences" 0}})

(def ^:private valid-source-region-policy
  {"policy_id" "https://w3id.org/abc/policies/source-region-publication-v0"
   "policy_version" "0.2.0"
   "dispositions" [{"source_class" "notation_legend"
                    "target_class" "tei_policy_projection"
                    "tei_target" "encodingDesc/editorialDecl"
                    "custom_sidecar" true
                    "plaintext_projection" "omit"
                    "measurement_status" "measured"}
                   {"source_class" "notation_placeholder"
                    "target_class" "custom_sidecar"
                    "tei_target" nil
                    "custom_sidecar" true
                    "plaintext_projection" "omit"
                    "measurement_status" "measured"}
                   {"source_class" "body_end_boundary"
                    "target_class" "custom_sidecar"
                    "tei_target" nil
                    "custom_sidecar" true
                    "plaintext_projection" "omit"
                    "measurement_status" "measured"}
                   {"source_class" "terminal_provenance"
                    "target_class" "tei_policy_projection"
                    "tei_target" "text/back/div[@type='source']"
                    "custom_sidecar" true
                    "plaintext_projection" "omit"
                    "measurement_status" "needs_measurement_split"}
                   {"source_class" "colophon_metadata"
                    "target_class" "tei_policy_projection"
                    "tei_target" "teiHeader/sourceDesc"
                    "custom_sidecar" true
                    "plaintext_projection" "omit"
                    "measurement_status" "needs_measurement_split"}
                   {"source_class" "letter_address_origin"
                    "target_class" "tei_policy_projection"
                    "tei_target" "teiHeader/profileDesc/correspDesc"
                    "custom_sidecar" true
                    "plaintext_projection" "omit"
                    "measurement_status" "measured"}
                   {"source_class" "malformed_source"
                    "target_class" "diagnostic"
                    "tei_target" nil
                    "custom_sidecar" true
                    "plaintext_projection" "omit"
                    "measurement_status" "measured"}]})

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
          registry-checked (atom nil)
          parser-evidence-checked (atom nil)]
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
                                                :ok)
                    parser-evidence/load-index (fn [] {:entries []})
                    parser-evidence/validate-index! (fn [index]
                                                      (reset! parser-evidence-checked index)
                                                      :ok)]
        (validate/validate-json-schemas! [])
        (is (every? (set @checked-paths)
                    ["schemas/aat-parser-ir-mapping.schema.json"
                     "schemas/aat-parser-ir-divergence.schema.json"
                     "schemas/aat-parser-ir-divergence-bundle.schema.json"]))
        (is (= {:entries []} @registry-checked))
        (is (= {:entries []} @parser-evidence-checked))))))

(deftest validate-json-schemas-includes-source-region-coverage-contract-test
  (testing "design-bundle schema pass validates the source-region coverage contract"
    (let [checked-paths (atom [])]
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
                    compat/validate-registry! (fn [_registry] :ok)
                    parser-evidence/load-index (fn [] {:entries []})
                    parser-evidence/validate-index! (fn [_index] :ok)]
        (validate/validate-json-schemas! [])
        (is (some #{"schemas/source-region-coverage.schema.json"}
                  @checked-paths))))))

(deftest validate-json-schemas-includes-source-assertion-contract-test
  (testing "design-bundle schema pass validates the shared source-assertion contract"
    (let [checked-paths (atom [])]
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
                    compat/validate-registry! (fn [_registry] :ok)
                    parser-evidence/load-index (fn [] {:entries []})
                    parser-evidence/validate-index! (fn [_index] :ok)]
        (validate/validate-json-schemas! [])
        (is (some #{"schemas/source-assertion.schema.json"}
                  @checked-paths))))))

(deftest validate-json-schemas-includes-adr-claim-migration-baseline-contract-test
  (let [checked-paths (atom [])]
    (with-redefs [validate/schema-valid! (fn [_schema path]
                                           (swap! checked-paths conj path))
                  validate/validate-json! (fn [& _args])
                  validate/validate-json-lines! (fn [& _args])
                  validate/validation-errors (fn [_schema value]
                                               (when-not (contains? value "rule_id")
                                                 [:expected-error]))
                  compat/load-registry (fn [] {:entries []})
                  compat/validate-registry! (fn [_registry])
                  parser-evidence/load-index (fn [] {:entries []})
                  parser-evidence/validate-index! (fn [_index])]
      (validate/validate-json-schemas! [])
      (is (some #{"schemas/adr-claim-migration-baseline.schema.json"}
                @checked-paths)))))

(deftest validate-json-schemas-includes-adr-evidence-contracts-test
  (let [run-file (java.io.File/createTempFile "abc-adr-evidence-run" ".json")
        external-file (java.io.File/createTempFile "abc-adr-external-evidence" ".json")
        run-value {"schema_version" "abc-adr-evidence-run-v1"
                   "producer" {"tool" "bin/kaocha"
                               "command" "bin/kaocha --focus abc.tools.adr-evidence-test"
                               "revision" "0000000000000000000000000000000000000000"}
                   "input_profile" {"kind" "clojure-test-v1"
                                    "roots" ["abc.tools.adr-evidence-test"]
                                    "explicit" []}
                   "inputs" {"test/abc/tools/adr_evidence_test.clj"
                             "sha256:0000000000000000000000000000000000000000000000000000000000000000"}
                   "observations" {"contract" {"value" true
                                               "details" {"tests" 4
                                                          "failures" 0
                                                          "errors" 0}}}}
        external-value {"schema_version" "abc-adr-external-evidence-v1"
                        "source_url" "https://example.invalid/authority"
                        "retrieved_at" "2026-07-12"
                        "review_after" "2027-07-12"
                        "summary" {"path" "docs/evidence/authority-summary.md"
                                   "hash" "sha256:1111111111111111111111111111111111111111111111111111111111111111"}
                        "input_profile" {"kind" "external-authority-v1"
                                         "explicit" []}
                        "inputs" {}
                        "observations" {"source-contract" {"value" "documented"
                                                           "details" {}}}}
        checked-schemas (atom {})]
    (try
      (abc-json/write-deterministic-json-file! run-file run-value)
      (abc-json/write-deterministic-json-file! external-file external-value)
      (with-redefs [validate/schema-valid! (fn [schema path]
                                             (swap! checked-schemas assoc path schema)
                                             nil)
                    validate/validate-json! (fn [& _args] nil)
                    validate/validate-json-lines! (fn [& _args] nil)
                    validate/validation-errors (fn [_schema value]
                                                 (if (and (map? value)
                                                          (contains? value "rule_id"))
                                                   nil
                                                   [:expected-error]))
                    compat/load-registry (fn [] {:entries []})
                    compat/validate-registry! (fn [_registry] :ok)
                    parser-evidence/load-index (fn [] {:entries []})
                    parser-evidence/validate-index! (fn [_index] :ok)]
        (validate/validate-json-schemas! []))
      (let [run-schema (get @checked-schemas "schemas/adr-evidence-run.schema.json")
            external-schema (get @checked-schemas "schemas/adr-external-evidence.schema.json")]
        (is (map? run-schema))
        (is (map? external-schema))
        (when (and run-schema external-schema)
          (is (nil? (schema/validation-errors run-schema
                                              (files/read-json run-file))))
          (is (nil? (schema/validation-errors external-schema
                                              (files/read-json external-file))))
          (doseq [invalid [(assoc run-value "schema_version" "unknown")
                           (assoc run-value "unexpected" true)
                           (assoc-in run-value ["observations" "contract" "value"] 0.5)
                           (assoc-in run-value ["observations" "contract" "value"] 9007199254740992)
                           (assoc-in run-value ["observations" "contract" "details" "nested"]
                                     [0.5])
                           (assoc-in run-value ["observations" "contract" "details" "nested"]
                                     [-9007199254740992])]]
            (is (seq (schema/validation-errors run-schema invalid))))
          (doseq [invalid [(assoc external-value "schema_version" "unknown")
                           (assoc external-value "unexpected" true)
                           (assoc external-value "retrieved_at" "2026/07/12")
                           (dissoc external-value "review_after")
                           (assoc-in external-value
                                     ["observations" "source-contract" "value"] 0.5)
                           (assoc-in external-value
                                     ["observations" "source-contract" "details" "nested"]
                                     [9007199254740992])]]
            (is (seq (schema/validation-errors external-schema invalid))))))
      (finally
        (.delete run-file)
        (.delete external-file)))))

(deftest validate-json-schemas-includes-tei-eaj-comparison-fixture-test
  (testing "design-bundle schema pass validates the TEI-EAJ comparison export contract"
    (let [checked-schemas (atom [])
          checked-json (atom [])]
      (with-redefs [validate/schema-valid! (fn [_schema path]
                                             (swap! checked-schemas conj path)
                                             nil)
                    validate/validate-json! (fn [_schema path]
                                              (swap! checked-json conj path)
                                              nil)
                    validate/validate-json-lines! (fn [& _args] nil)
                    validate/validation-errors (fn [_schema value]
                                                 (if (and (map? value)
                                                          (contains? value "rule_id"))
                                                   nil
                                                   [:expected-error]))
                    compat/load-registry (fn [] {:entries []})
                    compat/validate-registry! (fn [_registry] :ok)
                    parser-evidence/load-index (fn [] {:entries []})
                    parser-evidence/validate-index! (fn [_index] :ok)]
        (validate/validate-json-schemas! [])
        (is (some #{"schemas/tei-eaj-comparison.schema.json"}
                  @checked-schemas))
        (is (some #{"fixtures/tei-eaj-comparison/workset-export.json"}
                  @checked-json))))))

(deftest validate-json-schemas-includes-analysis-fixtures-test
  (testing "design-bundle schema pass validates analysis recipe and result contracts"
    (let [checked-schemas (atom [])
          checked-json (atom [])]
      (with-redefs [validate/schema-valid! (fn [_schema path]
                                             (swap! checked-schemas conj path)
                                             nil)
                    validate/validate-json! (fn [_schema path]
                                              (swap! checked-json conj path)
                                              nil)
                    validate/validate-json-lines! (fn [& _args] nil)
                    validate/validation-errors (fn [_schema value]
                                                 (if (and (map? value)
                                                          (contains? value "rule_id"))
                                                   nil
                                                   [:expected-error]))
                    compat/load-registry (fn [] {:entries []})
                    compat/validate-registry! (fn [_registry] :ok)
                    parser-evidence/load-index (fn [] {:entries []})
                    parser-evidence/validate-index! (fn [_index] :ok)]
        (validate/validate-json-schemas! [])
        (is (every? (set @checked-schemas)
                    ["schemas/analysis-recipe.schema.json"
                     "schemas/analysis-result.schema.json"
                     "schemas/pack-policy.schema.json"]))
        (is (every? (set @checked-json)
                    ["data/analysis-recipes/literary-basic-ja-v1.json"
                     "data/analysis-recipes/token-basic-ja-v1.json"
                     "data/pack-policies/no-pack-v1.json"
                     "data/pack-policies/parquet-basic-v1.json"
                     "examples/v0/example-work/analysis-result.json"]))))))

(deftest validate-json-schemas-includes-snapshot-index-fixture-test
  (testing "design-bundle schema pass validates the snapshot index contract"
    (let [checked-schemas (atom [])
          checked-json (atom [])]
      (with-redefs [validate/schema-valid! (fn [_schema path]
                                             (swap! checked-schemas conj path)
                                             nil)
                    validate/validate-json! (fn [_schema path]
                                              (swap! checked-json conj path)
                                              nil)
                    validate/validate-json-lines! (fn [& _args] nil)
                    validate/validation-errors (fn [_schema value]
                                                 (if (and (map? value)
                                                          (contains? value "rule_id"))
                                                   nil
                                                   [:expected-error]))
                    compat/load-registry (fn [] {:entries []})
                    compat/validate-registry! (fn [_registry] :ok)
                    parser-evidence/load-index (fn [] {:entries []})
                    parser-evidence/validate-index! (fn [_index] :ok)]
        (validate/validate-json-schemas! [])
        (is (some #{"schemas/snapshot-index.schema.json"}
                  @checked-schemas))
        (is (some #{"examples/v0/snapshot/snapshot-index.json"}
                  @checked-json))))))

(deftest validate-json-schemas-includes-request-set-fixtures-test
  (testing "design-bundle schema pass validates resolved request sets"
    (let [checked-schemas (atom [])
          checked-json (atom [])]
      (with-redefs [validate/schema-valid! (fn [_schema path]
                                             (swap! checked-schemas conj path)
                                             nil)
                    validate/validate-json! (fn [_schema path]
                                              (swap! checked-json conj path)
                                              nil)
                    validate/validate-json-lines! (fn [& _args] nil)
                    validate/validation-errors (fn [_schema value]
                                                 (if (and (map? value)
                                                          (contains? value "rule_id"))
                                                   nil
                                                   [:expected-error]))
                    compat/load-registry (fn [] {:entries []})
                    compat/validate-registry! (fn [_registry] :ok)
                    parser-evidence/load-index (fn [] {:entries []})
                    parser-evidence/validate-index! (fn [_index] :ok)]
        (validate/validate-json-schemas! [])
        (is (some #{"schemas/request-set.schema.json"}
                  @checked-schemas))
        (is (some #{"data/request-sets/smoke-basic-ja.json"}
                  @checked-json))))))

(deftest validate-json-schemas-includes-workflow-run-fixture-test
  (testing "design-bundle schema pass validates the workflow-run report contract"
    (let [checked-schemas (atom [])
          checked-json (atom [])]
      (with-redefs [validate/schema-valid! (fn [_schema path]
                                             (swap! checked-schemas conj path)
                                             nil)
                    validate/validate-json! (fn [_schema path]
                                              (swap! checked-json conj path)
                                              nil)
                    validate/validate-json-lines! (fn [& _args] nil)
                    validate/validation-errors (fn [_schema value]
                                                 (if (and (map? value)
                                                          (contains? value "rule_id"))
                                                   nil
                                                   [:expected-error]))
                    compat/load-registry (fn [] {:entries []})
                    compat/validate-registry! (fn [_registry] :ok)
                    parser-evidence/load-index (fn [] {:entries []})
                    parser-evidence/validate-index! (fn [_index] :ok)]
        (validate/validate-json-schemas! [])
        (is (some #{"schemas/workflow-run.schema.json"}
                  @checked-schemas))
        (is (some #{"examples/workflow/passed.workflow-run.json"}
                  @checked-json))))))

(deftest validate-analysis-copied-fields-gate-test
  (let [producer-id (files/example-hash "31")
        producer {"artifact_id" producer-id
                  "artifact_kind" "parser-ir"
                  "validation_status" "passed"
                  "manifest_identity_object" {"parser_build_hash" (files/example-hash "01")
                                              "parser_config_hash" (files/example-hash "02")
                                              "aat_parser_ir_mapping_hash" (files/example-hash "03")
                                              "parser_ir_schema_hash" (files/example-hash "04")}
                  "content" {"content_hash" (files/example-hash "05")
                             "media_type" "application/json"}
                  "provenance" {"used" []
                                "was_derived_from" []}}
        analysis (assoc producer
                        "artifact_id" (files/example-hash "32")
                        "artifact_kind" "analysis"
                        "manifest_identity_object" {"parser_build_hash" (files/example-hash "99")
                                                    "parser_config_hash" (files/example-hash "02")
                                                    "aat_parser_ir_mapping_hash" (files/example-hash "03")
                                                    "parser_ir_schema_hash" (files/example-hash "04")}
                        "provenance" {"used" [producer-id]
                                      "was_derived_from" [producer-id]})
        entries (manifest-index/index-entries {"parser.manifest.json" producer
                                               "analysis.manifest.json" analysis})]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Analysis manifest copied identity fields differ from producer"
                          (manifest-index/validate-analysis-copied-fields! entries)))))

(deftest source-region-coverage-errors-test
  (testing "accepts the current ab-validator source-region coverage contract"
    (is (empty? (validate/source-region-coverage-errors
                 valid-source-region-coverage
                 valid-source-region-policy))))
  (testing "requires the v1 schema version and passing source authority gate"
    (is (seq (validate/source-region-coverage-errors
              (assoc valid-source-region-coverage
                     "schema_version" "old")
              valid-source-region-policy)))
    (is (seq (validate/source-region-coverage-errors
              (assoc valid-source-region-coverage
                     "gate_status" "SOURCE_AUTHORITY_GATE_BLOCKED")
              valid-source-region-policy))))
  (testing "blocks unreviewed, unknown-region, or unsupported body source markup"
    (doseq [path [["unallowlisted_unknown_markers_total"]
                  ["source_region_coverage" "unsupported_body_markup_occurrences"]
                  ["source_region_coverage" "unknown_region_occurrences"]
                  ["source_region_coverage" "unknown_unreviewed_occurrences"]]]
      (is (seq (validate/source-region-coverage-errors
                (assoc-in valid-source-region-coverage path 1)
                valid-source-region-policy))
          (str "expected source-region gate to reject " path))))
  (testing "keeps legacy aliases honest during ABC migration"
    (is (seq (validate/source-region-coverage-errors
              (assoc-in valid-source-region-coverage
                        ["representability" "malformed_noise_occurrences"]
                        1)
              valid-source-region-policy)))
    (is (seq (validate/source-region-coverage-errors
              (assoc-in valid-source-region-coverage
                        ["representability" "unsupported_occurrences"]
                        1)
              valid-source-region-policy)))))

(deftest source-region-policy-errors-test
  (testing "requires a disposition for every source-apparatus class ABC owns"
    (is (seq (validate/source-region-policy-errors
              (update valid-source-region-policy
                      "dispositions"
                      #(remove (fn [row]
                                 (= "malformed_source" (get row "source_class")))
                               %))))))
  (testing "requires plaintext omission for source apparatus and diagnostics"
    (is (seq (validate/source-region-policy-errors
              (assoc-in valid-source-region-policy
                        ["dispositions" 0 "plaintext_projection"]
                        "include")))))
  (testing "requires measurement_status to be measured or needs_measurement_split"
    (is (seq (validate/source-region-policy-errors
              (assoc-in valid-source-region-policy
                        ["dispositions" 3 "measurement_status"]
                        "bogus"))))
    (is (empty? (validate/source-region-policy-errors
                 (assoc-in valid-source-region-policy
                           ["dispositions" 3 "measurement_status"]
                           "measured"))))
    (is (empty? (validate/source-region-policy-errors
                 (assoc-in valid-source-region-policy
                           ["dispositions" 3 "measurement_status"]
                           "needs_measurement_split"))))))

(deftest source-region-publication-fixture-test
  (testing "fixture covers front apparatus, body text, body-end boundary, and back matter plaintext omission"
    (let [fixture (files/read-json
                   "fixtures/source-region/valid/source-apparatus-publication-bundle.json")
          coverage (get fixture "source_region_coverage_report")
          policy (get fixture "source_region_publication_policy")
          parser-ir (get fixture "parser_ir")]
      (is (empty? (validate/source-region-coverage-errors coverage policy)))
      (is (= #{"notation_legend"
               "notation_placeholder"
               "body_end_boundary"
               "terminal_provenance"
               "colophon_metadata"
               "letter_address_origin"
               "malformed_source"}
             (set (map #(get % "source_class")
                       (get policy "dispositions")))))
      (is (= (get fixture "expected_plaintext")
             (plaintext/render-string parser-ir))))))

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
        {"parser_ir_schema_hash" legacy-parser-ir-schema-hash
         "diagnostic_schema_hash" "sha256:e21ef2abdbf64b6fc920b4ef9a3df0e426b7bcc1cad0a6bbdd654f41e8ff302d"})))
  (is (empty?
       (validate/schema-hash-errors
        {"parser_ir_schema_hash" level3-parser-ir-schema-hash
         "diagnostic_schema_hash" "sha256:e21ef2abdbf64b6fc920b4ef9a3df0e426b7bcc1cad0a6bbdd654f41e8ff302d"})))
  (is (empty?
       (validate/schema-hash-errors
        {"parser_ir_schema_hash" v0-6-parser-ir-schema-hash
         "diagnostic_schema_hash" "sha256:e21ef2abdbf64b6fc920b4ef9a3df0e426b7bcc1cad0a6bbdd654f41e8ff302d"})))
  (is (= [(str "ab-validator parser_ir_schema_hash sha256:0000000000000000000000000000000000000000000000000000000000000004 does not match ABC parser IR schema hash " current-parser-ir-schema-hash)
          "ab-validator diagnostic_schema_hash sha256:0000000000000000000000000000000000000000000000000000000000000008 does not match ABC diagnostic schema hash sha256:e21ef2abdbf64b6fc920b4ef9a3df0e426b7bcc1cad0a6bbdd654f41e8ff302d"]
         (validate/schema-hash-errors
          {"parser_ir_schema_hash" (files/example-hash "04")
           "diagnostic_schema_hash" (files/example-hash "08")}))))

(defn- parser-ir-schema-hash-errors-assertions []
  (is (empty?
       (validate/parser-ir-schema-hash-errors
        {"schema_hash" legacy-parser-ir-schema-hash})))
  (is (empty?
       (validate/parser-ir-schema-hash-errors
        {"schema_hash" v0-6-parser-ir-schema-hash})))
  (is (= [(str "ab-validator parser IR schema_hash sha256:0000000000000000000000000000000000000000000000000000000000000004 does not match ABC parser IR schema hash " current-parser-ir-schema-hash)]
         (validate/parser-ir-schema-hash-errors
          {"schema_hash" "sha256:0000000000000000000000000000000000000000000000000000000000000004"}))))

(deftest parser-ir-schema-hash-errors-test
  (runtime/with-validated-read-trace!
    (evidence-support/focused-trace-options "adr-0010-c4-parser-schema-mismatch")
    (fn [] (parser-ir-schema-hash-errors-assertions))))

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

(deftest parser-ir-schema-accepts-level3-paragraphs-test
  (testing "parser IR may carry paragraph ranges and source-note nodes for Level 3 publication structure"
    (let [schema (files/read-json "schemas/parser-ir.schema.json")
          parser-ir {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
                     "schema_hash" current-parser-ir-schema-hash
                     "source" {"work_content_hash" (files/example-hash "01")
                               "encoding" "Shift_JIS"
                               "normalization" "source"}
                     "nodes" [{"type" "text"
                               "span" {"start" 0 "end" 12
                                       "coordinate_system" "decoded_utf8"}
                               "text" "第一段"}
                              {"type" "line-break"
                               "span" {"start" 12 "end" 17
                                       "coordinate_system" "decoded_utf8"}
                               "marker" "［＃改行］"}
                              {"type" "source-note"
                               "span" {"start" 17 "end" 57
                                       "coordinate_system" "decoded_utf8"}
                               "text" "（古伝説と、シルレルの詩から。）"
                               "note_type" "source-attribution"
                               "placement" "back"
                               "classification" "heuristic"
                               "source_pointer" "blocks[78]"}]
                     "paragraphs" [{"id" "p000000"
                                    "span" {"start" 0 "end" 12
                                            "coordinate_system" "decoded_utf8"}
                                    "span_source" "direct"
                                    "node_range" {"start" 0 "end" 2}
                                    "role" "body"
                                    "source_pointer" "blocks[0]"
                                    "classification" "direct"}
                                   {"id" "p000001"
                                    "span" {"start" 17 "end" 57
                                            "coordinate_system" "decoded_utf8"}
                                    "span_source" "direct"
                                    "node_range" {"start" 2 "end" 3}
                                    "role" "source-note"
                                    "source_pointer" "blocks[78]"
                                    "classification" "heuristic"}]
                     "warnings" []
                     "errors" []}]
      (is (nil? (validate/validation-errors schema parser-ir))))))

(deftest parser-ir-schema-accepts-sentences-and-orthographic-annotations-test
  (testing "parser IR carries sentence segmentation plus orthographic annotation provenance"
    (let [schema (files/read-json "schemas/parser-ir.schema.json")
          parser-ir {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
                     "schema_hash" current-parser-ir-schema-hash
                     "source" {"work_content_hash" (files/example-hash "11")
                               "encoding" "UTF-8"
                               "normalization" "source"}
                     "nodes" [{"type" "text"
                               "span" {"start" 0 "end" 24
                                       "coordinate_system" "decoded_utf8"}
                               "text" "吾輩ハ猫デアル。"}
                              {"type" "text"
                               "span" {"start" 24 "end" 48
                                       "coordinate_system" "decoded_utf8"}
                               "text" "名前はまだ無い。"}]
                     "paragraphs" [{"id" "p000000"
                                    "span" {"start" 0 "end" 48
                                            "coordinate_system" "decoded_utf8"}
                                    "span_source" "direct"
                                    "node_range" {"start" 0 "end" 2}
                                    "role" "body"
                                    "source_pointer" "blocks[0]"
                                    "classification" "direct"}]
                     "sentence_segmentation" {"schema_version" "sentence-segmentation-v1"
                                              "splitter_id" "ab-plaintext-japanese-v1"
                                              "coordinate_system" "decoded_utf8"
                                              "coverage" "body-paragraphs"}
                     "sentences" [{"id" "s000000"
                                   "paragraph_id" "p000000"
                                   "span" {"start" 0 "end" 24
                                           "coordinate_system" "decoded_utf8"}
                                   "node_range" {"start" 0 "end" 1}
                                   "tags" ["orthographic-katakana"]
                                   "orthographic_annotation_indices" [0]}
                                  {"id" "s000001"
                                   "paragraph_id" "p000000"
                                   "span" {"start" 24 "end" 48
                                           "coordinate_system" "decoded_utf8"}
                                   "node_range" {"start" 1 "end" 2}
                                   "tags" []
                                   "orthographic_annotation_indices" []}]
                     "orthographic_annotations" {"work_id" "000000"
                                                 "work_content_hash" (files/example-hash "11")
                                                 "coordinate_system" "decoded_utf8"
                                                 "detector_id" "HeuristicV1"
                                                 "annotations" [{"source_byte_range" {"start" 0 "end" 24}
                                                                 "normalized_text" "吾輩は猫である。"
                                                                 "kind" "ScriptKatakanaToHiragana"
                                                                 "confidence" nil}]}
                     "warnings" []
                     "errors" []}]
      (is (nil? (schema/validation-errors schema parser-ir))))))

(deftest parser-ir-schema-accepts-paragraph-layout-test
  (testing "paragraph rows may carry Aozora layout metadata for TEI paragraph rendering"
    (let [schema (files/read-json "schemas/parser-ir.schema.json")
          parser-ir {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
                     "schema_hash" current-parser-ir-schema-hash
                     "source" {"work_content_hash" (files/example-hash "01")
                               "encoding" "Shift_JIS"
                               "normalization" "source"}
                     "nodes" [{"type" "text"
                               "span" {"start" 0 "end" 6
                                       "coordinate_system" "decoded_utf8"}
                               "text" "台詞"}
                              {"type" "text"
                               "span" {"start" 6 "end" 36
                                       "coordinate_system" "decoded_utf8"}
                               "text" "（大正十一年十二月）"}]
                     "paragraphs" [{"id" "p000000"
                                    "span" {"start" 0 "end" 6
                                            "coordinate_system" "decoded_utf8"}
                                    "span_source" "direct"
                                    "node_range" {"start" 0 "end" 1}
                                    "role" "body"
                                    "source_pointer" "blocks[0]"
                                    "classification" "direct"
                                    "layout" {"kind" "burasage"
                                              "first_line_indent" 0
                                              "continuation_indent" 1
                                              "source" "aat-style"}}
                                   {"id" "p000001"
                                    "span" {"start" 6 "end" 36
                                            "coordinate_system" "decoded_utf8"}
                                    "span_source" "direct"
                                    "node_range" {"start" 1 "end" 2}
                                    "role" "body"
                                    "source_pointer" "blocks[1]"
                                    "classification" "direct"
                                    "layout" {"kind" "chitsuki"
                                              "align" "right"
                                              "offset_from_end" 1
                                              "source" "aat-style"}}]
                     "warnings" []
                     "errors" []}]
      (is (nil? (validate/validation-errors schema parser-ir))))))

(deftest parser-ir-schema-accepts-all-paragraph-layout-kinds-test
  (testing "every supported layout kind carries enough payload for deterministic TEI p@rend"
    (let [schema (files/read-json "schemas/parser-ir.schema.json")
          parser-ir {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
                     "schema_hash" current-parser-ir-schema-hash
                     "source" {"work_content_hash" (files/example-hash "01")
                               "encoding" "Shift_JIS"
                               "normalization" "source"}
                     "nodes" [{"type" "text"
                               "span" {"start" 0 "end" 6
                                       "coordinate_system" "decoded_utf8"}
                               "text" "台詞"}]
                     "paragraphs" [{"id" "p000000"
                                    "span" {"start" 0 "end" 6
                                            "coordinate_system" "decoded_utf8"}
                                    "span_source" "direct"
                                    "node_range" {"start" 0 "end" 1}
                                    "role" "body"
                                    "source_pointer" "blocks[0]"
                                    "classification" "direct"}]
                     "warnings" []
                     "errors" []}
          layouts [{"kind" "jisage" "indent" 2 "source" "aat-block"}
                   {"kind" "burasage" "first_line_indent" 0 "continuation_indent" 1 "source" "aat-style"}
                   {"kind" "chitsuki" "align" "right" "offset_from_end" 1 "source" "aat-style"}
                   {"kind" "jizume" "width" 20 "source" "source-derived"}
                   {"kind" "line-jisage" "indent" 3 "source" "source-derived"}]]
      (doseq [layout layouts]
        (is (nil? (validate/validation-errors
                   schema
                   (assoc-in parser-ir ["paragraphs" 0 "layout"] layout)))
            (str "schema should accept complete " (get layout "kind") " layout"))))))

(deftest parser-ir-schema-rejects-incomplete-paragraph-layout-test
  (testing "layout kind-specific payload is required so TEI p@rend projection cannot silently disappear"
    (let [schema (files/read-json "schemas/parser-ir.schema.json")
          parser-ir {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
                     "schema_hash" current-parser-ir-schema-hash
                     "source" {"work_content_hash" (files/example-hash "01")
                               "encoding" "Shift_JIS"
                               "normalization" "source"}
                     "nodes" [{"type" "text"
                               "span" {"start" 0 "end" 6
                                       "coordinate_system" "decoded_utf8"}
                               "text" "台詞"}]
                     "paragraphs" [{"id" "p000000"
                                    "span" {"start" 0 "end" 6
                                            "coordinate_system" "decoded_utf8"}
                                    "span_source" "direct"
                                    "node_range" {"start" 0 "end" 1}
                                    "role" "body"
                                    "source_pointer" "blocks[0]"
                                    "classification" "direct"
                                    "layout" {"kind" "jisage"
                                              "source" "aat-style"}}]
                     "warnings" []
                     "errors" []}]
      (is (seq (validate/validation-errors schema parser-ir))))))

(deftest parser-ir-schema-accepts-emphasis-inline-children-test
  (testing "emphasis can carry recursive inline children while retaining legacy text"
    (let [schema (files/read-json "schemas/parser-ir.schema.json")
          parser-ir {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
                     "schema_hash" current-parser-ir-schema-hash
                     "source" {"work_content_hash" "sha256:0000000000000000000000000000000000000000000000000000000000000002"
                               "encoding" "Shift_JIS"
                               "normalization" "source"}
                     "nodes" [{"type" "emphasis"
                               "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                               "style" "bold"
                               "text" "東京"
                               "inline_children" [{"type" "ruby"
                                                   "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                                                   "ruby" {"base" "東京"
                                                           "reading" "とうきょう"
                                                           "scope" "explicit"
                                                           "direction" "right"}}]}]
                     "warnings" []
                     "errors" []}]
      (is (nil? (schema/validation-errors schema parser-ir)))))
  (testing "emphasis can carry structured inline children without legacy text"
    (let [schema (files/read-json "schemas/parser-ir.schema.json")
          parser-ir {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
                     "schema_hash" current-parser-ir-schema-hash
                     "source" {"work_content_hash" "sha256:0000000000000000000000000000000000000000000000000000000000000002"
                               "encoding" "Shift_JIS"
                               "normalization" "source"}
                     "nodes" [{"type" "emphasis"
                               "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                               "style" "bold"
                               "inline_children" [{"type" "ruby"
                                                   "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                                                   "ruby" {"base" "東京"
                                                           "reading" "とうきょう"
                                                           "scope" "explicit"
                                                           "direction" "right"}}]}]
                     "warnings" []
                     "errors" []}]
      (is (nil? (schema/validation-errors schema parser-ir))))))

(deftest parser-ir-schema-accepts-heading-inline-children-test
  (testing "heading nodes preserve structured inline content while retaining text"
    (let [schema (files/read-json "schemas/parser-ir.schema.json")
          parser-ir {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
                     "schema_hash" current-parser-ir-schema-hash
                     "source" {"work_content_hash" "sha256:0000000000000000000000000000000000000000000000000000000000000002"
                               "encoding" "Shift_JIS"
                               "normalization" "source"}
                     "nodes" [{"type" "heading"
                               "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                               "text" "東京"
                               "level" 2
                               "inline_children" [{"type" "ruby"
                                                   "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                                                   "ruby" {"base" "東京"
                                                           "reading" "とうきょう"
                                                           "scope" "explicit"
                                                           "direction" "right"}}]}]
                     "warnings" []
                     "errors" []}]
      (is (nil? (schema/validation-errors schema parser-ir))))))

(deftest parser-ir-schema-accepts-layout-span-test
  (testing "layout-span is a typed inline publication-layout scope"
    (let [schema (files/read-json "schemas/parser-ir.schema.json")
          parser-ir {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
                     "schema_hash" current-parser-ir-schema-hash
                     "source" {"work_content_hash" "sha256:0000000000000000000000000000000000000000000000000000000000000002"
                               "encoding" "Shift_JIS"
                               "normalization" "source"}
                     "nodes" [{"type" "layout-span"
                               "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                               "text" "12"
                               "inline_children" [{"type" "text"
                                                   "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                                                   "text" "12"}]
                               "layout" {"kind" "tcy"
                                         "source" "aat-inline"
                                         "marker" "縦中横"}}
                              {"type" "emphasis"
                               "span" {"start" 2 "end" 3 "coordinate_system" "decoded_utf8"}
                               "style" "bold"
                               "inline_children" [{"type" "layout-span"
                                                   "span" {"start" 2 "end" 3 "coordinate_system" "decoded_utf8"}
                                                   "text" "横"
                                                   "layout" {"kind" "yokogumi"
                                                             "source" "aat-inline"
                                                             "direction" "horizontal"}}]}]
                     "warnings" []
                     "errors" []}]
      (is (nil? (schema/validation-errors schema parser-ir))))))

(deftest parser-ir-schema-rejects-invalid-layout-span-test
  (testing "layout-span requires typed layout metadata"
    (let [schema (files/read-json "schemas/parser-ir.schema.json")
          parser-ir {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
                     "schema_hash" current-parser-ir-schema-hash
                     "source" {"work_content_hash" "sha256:0000000000000000000000000000000000000000000000000000000000000002"
                               "encoding" "Shift_JIS"
                               "normalization" "source"}
                     "nodes" [{"type" "layout-span"
                               "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                               "text" "12"
                               "layout" {"source" "aat-inline"}}]
                     "warnings" []
                     "errors" []}]
      (is (seq (schema/validation-errors schema parser-ir))))))

(deftest parser-ir-schema-rejects-empty-emphasis-test
  (testing "emphasis must carry either legacy text or structured inline children"
    (let [schema (files/read-json "schemas/parser-ir.schema.json")
          parser-ir {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
                     "schema_hash" current-parser-ir-schema-hash
                     "source" {"work_content_hash" "sha256:0000000000000000000000000000000000000000000000000000000000000002"
                               "encoding" "Shift_JIS"
                               "normalization" "source"}
                     "nodes" [{"type" "emphasis"
                               "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                               "style" "bold"}]
                     "warnings" []
                     "errors" []}]
      (is (seq (schema/validation-errors schema parser-ir))))))

(deftest parser-ir-schema-accepts-inline-only-emphasis-children-test
  (testing "emphasis inline_children stay restricted to inline-safe node types"
    (let [schema (files/read-json "schemas/parser-ir.schema.json")
          parser-ir {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
                     "schema_hash" current-parser-ir-schema-hash
                     "source" {"work_content_hash" "sha256:0000000000000000000000000000000000000000000000000000000000000002"
                               "encoding" "Shift_JIS"
                               "normalization" "source"}
                     "nodes" [{"type" "emphasis"
                               "span" {"start" 0 "end" 4 "coordinate_system" "decoded_utf8"}
                               "style" "bold"
                               "text" "AGB\n"
                               "inline_children" [{"type" "text"
                                                   "span" {"start" 0 "end" 1 "coordinate_system" "decoded_utf8"}
                                                   "text" "A"}
                                                  {"type" "ruby"
                                                   "span" {"start" 1 "end" 2 "coordinate_system" "decoded_utf8"}
                                                   "ruby" {"base" "東"
                                                           "reading" "ひがし"
                                                           "scope" "explicit"
                                                           "direction" "right"}}
                                                  {"type" "gaiji"
                                                   "span" {"start" 2 "end" 3 "coordinate_system" "decoded_utf8"}
                                                   "gaiji" {"raw_marker" "※［＃g］"
                                                            "unicode" "G"
                                                            "reference" nil
                                                            "ivs" nil
                                                            "image_or_glyph_fallback" nil
                                                            "resolved" true}}
                                                  {"type" "editor-note"
                                                   "span" {"start" 3 "end" 3 "coordinate_system" "decoded_utf8"}
                                                   "note" {"raw" "［＃注］"
                                                           "category" "misc"}}
                                                  {"type" "emphasis"
                                                   "span" {"start" 3 "end" 4 "coordinate_system" "decoded_utf8"}
                                                   "style" "inner"
                                                   "text" "B"}
                                                  {"type" "line-break"
                                                   "span" {"start" 4 "end" 4 "coordinate_system" "decoded_utf8"}
                                                   "marker" "［＃改行］"}]}]
                     "warnings" []
                     "errors" []}]
      (is (nil? (schema/validation-errors schema parser-ir))))))

(deftest parser-ir-schema-rejects-block-nodes-inside-inline-children-test
  (testing "emphasis inline_children reject page-break and image nodes"
    (let [schema (files/read-json "schemas/parser-ir.schema.json")
          base {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
                "schema_hash" current-parser-ir-schema-hash
                "source" {"work_content_hash" "sha256:0000000000000000000000000000000000000000000000000000000000000002"
                          "encoding" "Shift_JIS"
                          "normalization" "source"}
                "warnings" []
                "errors" []}]
      (doseq [inline-child [{"type" "page-break"
                             "span" {"start" 0 "end" 0 "coordinate_system" "decoded_utf8"}
                             "marker" "［＃改ページ］"}
                            {"type" "image"
                             "span" {"start" 0 "end" 0 "coordinate_system" "decoded_utf8"}
                             "src" "fig.png"
                             "alt" "図"}]]
        (is (seq (schema/validation-errors
                  schema
                  (assoc base
                         "nodes" [{"type" "emphasis"
                                   "span" {"start" 0 "end" 1 "coordinate_system" "decoded_utf8"}
                                   "style" "bold"
                                   "text" "x"
                                   "inline_children" [inline-child]}]))))))))

(def ^:private level3-parser-ir-fixture
  {"nodes" [{"type" "text"
             "span" {"start" 0 "end" 12}
             "text" "第一段"}
            {"type" "source-note"
             "span" {"start" 12 "end" 52}
             "text" "（古伝説と、シルレルの詩から。）"
             "note_type" "source-attribution"
             "placement" "back"
             "classification" "heuristic"
             "source_pointer" "blocks[78]"}]
   "paragraphs" [{"id" "p000000"
                  "span" {"start" 0 "end" 12}
                  "span_source" "direct"
                  "node_range" {"start" 0 "end" 1}
                  "role" "body"
                  "source_pointer" "blocks[0]"
                  "classification" "direct"}
                 {"id" "p000001"
                  "span" {"start" 12 "end" 52}
                  "span_source" "direct"
                  "node_range" {"start" 1 "end" 2}
                  "role" "source-note"
                  "source_pointer" "blocks[78]"
                  "classification" "heuristic"}]})

(def ^:private sentence-parser-ir-fixture
  {"nodes" [{"type" "text"
             "span" {"start" 0 "end" 24 "coordinate_system" "decoded_utf8"}
             "text" "吾輩ハ猫デアル。"}
            {"type" "text"
             "span" {"start" 24 "end" 48 "coordinate_system" "decoded_utf8"}
             "text" "名前はまだ無い。"}
            {"type" "text"
             "span" {"start" 48 "end" 63 "coordinate_system" "decoded_utf8"}
             "text" "後続段落。"}]
   "paragraphs" [{"id" "p000000"
                  "span" {"start" 0 "end" 48 "coordinate_system" "decoded_utf8"}
                  "span_source" "direct"
                  "node_range" {"start" 0 "end" 2}
                  "role" "body"
                  "source_pointer" "blocks[0]"
                  "classification" "direct"}
                 {"id" "p000001"
                  "span" {"start" 48 "end" 63 "coordinate_system" "decoded_utf8"}
                  "span_source" "direct"
                  "node_range" {"start" 2 "end" 3}
                  "role" "body"
                  "source_pointer" "blocks[1]"
                  "classification" "direct"}]
   "sentence_segmentation" {"schema_version" "sentence-segmentation-v1"
                            "splitter_id" "ab-plaintext-japanese-v1"
                            "coordinate_system" "decoded_utf8"
                            "coverage" "body-paragraphs"}
   "sentences" [{"id" "s000000"
                 "paragraph_id" "p000000"
                 "span" {"start" 0 "end" 24 "coordinate_system" "decoded_utf8"}
                 "node_range" {"start" 0 "end" 1}
                 "tags" ["orthographic-katakana"]
                 "orthographic_annotation_indices" [0]}
                {"id" "s000001"
                 "paragraph_id" "p000000"
                 "span" {"start" 24 "end" 48 "coordinate_system" "decoded_utf8"}
                 "node_range" {"start" 1 "end" 2}
                 "tags" []
                 "orthographic_annotation_indices" []}
                {"id" "s000002"
                 "paragraph_id" "p000001"
                 "span" {"start" 48 "end" 63 "coordinate_system" "decoded_utf8"}
                 "node_range" {"start" 2 "end" 3}
                 "tags" []
                 "orthographic_annotation_indices" []}]
   "orthographic_annotations" {"annotations" [{"source_byte_range" {"start" 0 "end" 24}
                                               "normalized_text" "吾輩は猫である。"
                                               "kind" "ScriptKatakanaToHiragana"
                                               "confidence" nil}]}})

(deftest parser-ir-paragraph-coherence-errors-test
  (testing "accepts coherent paragraph ranges"
    (is (empty? (validate/parser-ir-paragraph-coherence-errors
                 level3-parser-ir-fixture))))
  (testing "rejects duplicate paragraph ids"
    (is (= ["parser IR paragraphs[] contains duplicate id p000000"]
           (validate/parser-ir-paragraph-coherence-errors
            (assoc level3-parser-ir-fixture
                   "paragraphs"
                   [(first (get level3-parser-ir-fixture "paragraphs"))
                    (assoc (second (get level3-parser-ir-fixture "paragraphs"))
                           "id" "p000000")])))))
  (testing "rejects ranges outside nodes[]"
    (is (= ["parser IR paragraph p000000 node_range 0..3 is outside nodes[] length 2"]
           (validate/parser-ir-paragraph-coherence-errors
            (assoc-in level3-parser-ir-fixture
                      ["paragraphs" 0 "node_range"]
                      {"start" 0 "end" 3})))))
  (testing "rejects overlapping or non-monotonic ranges"
    (is (= ["parser IR paragraph p000001 node_range starts before previous paragraph end 1"]
           (validate/parser-ir-paragraph-coherence-errors
            (assoc-in level3-parser-ir-fixture
                      ["paragraphs" 1 "node_range"]
                      {"start" 0 "end" 2})))))
  (testing "direct source-note paragraphs must contain a source-note node"
    (is (= ["parser IR paragraph p000001 has role source-note but no source-note node in node_range"]
           (validate/parser-ir-paragraph-coherence-errors
            (-> level3-parser-ir-fixture
                (assoc-in ["nodes" 1 "type"] "text")
                (assoc-in ["paragraphs" 1 "classification"] "direct")))))))

(deftest parser-ir-sentence-coherence-errors-test
  (testing "accepts coherent sentence rows"
    (is (empty? (validate/parser-ir-sentence-coherence-errors
                 sentence-parser-ir-fixture))))
  (testing "rejects a sentence referencing a missing body paragraph"
    (is (= ["parser IR sentence s999999 references non-body paragraph p999999"]
           (validate/parser-ir-sentence-coherence-errors
            (update sentence-parser-ir-fixture
                    "sentences"
                    conj
                    {"id" "s999999"
                     "paragraph_id" "p999999"
                     "span" {"start" 63 "end" 66 "coordinate_system" "decoded_utf8"}
                     "node_range" {"start" 3 "end" 3}
                     "tags" []
                     "orthographic_annotation_indices" []})))))
  (testing "rejects a sentence node range outside its paragraph"
    (is (= ["parser IR sentence s000001 node_range starts at 0 but expected 1"]
           (validate/parser-ir-sentence-coherence-errors
            (assoc-in sentence-parser-ir-fixture
                      ["sentences" 1 "node_range"]
                      {"start" 0 "end" 2})))))
  (testing "rejects a sentence byte span outside its paragraph"
    (is (= ["parser IR sentence s000001 span 30..54 is outside paragraph p000000 span 0..48"
            "parser IR sentence s000001 span starts at 30 but expected 24"
            "parser IR body paragraph p000000 sentence spans end at 54 but paragraph span ends at 48"]
           (validate/parser-ir-sentence-coherence-errors
            (assoc-in sentence-parser-ir-fixture
                      ["sentences" 1 "span"]
                      {"start" 30 "end" 54 "coordinate_system" "decoded_utf8"})))))
  (testing "rejects orthographic tag without annotation index"
    (is (= ["parser IR sentence s000000 has orthographic-katakana tag without annotation indices"]
           (validate/parser-ir-sentence-coherence-errors
            (assoc-in sentence-parser-ir-fixture
                      ["sentences" 0 "orthographic_annotation_indices"]
                      [])))))
  (testing "rejects annotation index outside annotation array"
    (is (= ["parser IR sentence s000000 has orthographic annotation index outside annotations[]"]
           (validate/parser-ir-sentence-coherence-errors
            (assoc-in sentence-parser-ir-fixture
                      ["sentences" 0 "orthographic_annotation_indices"]
                      [1])))))
  (testing "rejects annotation index without orthographic tag"
    (is (= ["parser IR sentence s000000 has orthographic annotation indices without orthographic-katakana tag"]
           (validate/parser-ir-sentence-coherence-errors
            (assoc-in sentence-parser-ir-fixture
                      ["sentences" 0 "tags"]
                      [])))))
  (testing "rejects orthographic annotation outside sentence span"
    (is (= ["parser IR sentence s000001 orthographic annotation index 0 range 0..24 does not overlap sentence span 24..48"]
           (validate/parser-ir-sentence-coherence-errors
            (-> sentence-parser-ir-fixture
                (assoc-in ["sentences" 1 "tags"] ["orthographic-katakana"])
                (assoc-in ["sentences" 1 "orthographic_annotation_indices"] [0]))))))
  (testing "rejects non-empty body paragraph with no sentence rows"
    (is (= ["parser IR body paragraph p000001 has no sentence rows"]
           (validate/parser-ir-sentence-coherence-errors
            (update sentence-parser-ir-fixture
                    "sentences"
                    #(vec (remove (fn [sentence]
                                    (= "p000001" (get sentence "paragraph_id")))
                                  %)))))))
  (testing "accepts zero-span body paragraph with no sentence rows"
    (is (empty?
         (validate/parser-ir-sentence-coherence-errors
          (-> sentence-parser-ir-fixture
              (update "nodes"
                      conj
                      {"type" "page-break"
                       "span" {"start" 63
                               "end" 63
                               "coordinate_system" "decoded_utf8"}})
              (update "paragraphs"
                      conj
                      {"id" "p000002"
                       "span" {"start" 63
                               "end" 63
                               "coordinate_system" "decoded_utf8"}
                       "span_source" "direct"
                       "node_range" {"start" 3 "end" 4}
                       "role" "body"
                       "source_pointer" "blocks[2]"
                       "classification" "direct"}))))))
  (testing "rejects byte-span gaps"
    (is (= ["parser IR sentence s000001 span starts at 30 but expected 24"]
           (validate/parser-ir-sentence-coherence-errors
            (assoc-in sentence-parser-ir-fixture
                      ["sentences" 1 "span"]
                      {"start" 30 "end" 48 "coordinate_system" "decoded_utf8"})))))
  (testing "rejects node-range gaps"
    (is (= ["parser IR sentence s000001 node_range starts at 2 but expected 1"]
           (validate/parser-ir-sentence-coherence-errors
            (assoc-in sentence-parser-ir-fixture
                      ["sentences" 1 "node_range"]
                      {"start" 2 "end" 2})))))
  (testing "accepts coherent fragment I→F chain"
    (is (empty?
         (validate/parser-ir-sentence-coherence-errors
          (-> sentence-parser-ir-fixture
              (assoc-in ["sentences" 0 "part"] "I")
              (assoc-in ["sentences" 0 "fragment_group"] "fg000000")
              (assoc-in ["sentences" 0 "next_id"] "s000001")
              (assoc-in ["sentences" 1 "part"] "F")
              (assoc-in ["sentences" 1 "fragment_group"] "fg000000")
              (assoc-in ["sentences" 1 "prev_id"] "s000000"))))))
  (testing "rejects part without fragment_group"
    (is (= ["parser IR sentence s000000 has part but no fragment_group"
            "parser IR sentence s000000 part=I but no next_id"]
           (validate/parser-ir-sentence-coherence-errors
            (assoc-in sentence-parser-ir-fixture ["sentences" 0 "part"] "I")))))
  (testing "rejects fragment_group without part"
    (is (= ["parser IR sentence s000000 has fragment_group but no part"]
           (validate/parser-ir-sentence-coherence-errors
            (assoc-in sentence-parser-ir-fixture ["sentences" 0 "fragment_group"] "fg000000")))))
  (testing "rejects part=I without next_id"
    (is (= ["parser IR sentence s000000 part=I but no next_id"]
           (validate/parser-ir-sentence-coherence-errors
            (-> sentence-parser-ir-fixture
                (assoc-in ["sentences" 0 "part"] "I")
                (assoc-in ["sentences" 0 "fragment_group"] "fg000000"))))))
  (testing "rejects part=F without prev_id"
    (is (= ["parser IR sentence s000001 part=F but no prev_id"]
           (validate/parser-ir-sentence-coherence-errors
            (-> sentence-parser-ir-fixture
                (assoc-in ["sentences" 1 "part"] "F")
                (assoc-in ["sentences" 1 "fragment_group"] "fg000000")))))))

(deftest parser-ir-publication-sentence-evidence-errors-test
  (is (= ["parser IR publication requires sentence_segmentation"]
         (sentence-policy/publication-sentence-evidence-errors
          (dissoc sentence-parser-ir-fixture "sentence_segmentation")))))

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

(def ^:private v3-epub3-compat-query
  {:aat_version 1
   :aat_adapter "aozora-epub3"
   :aat_adapter_version "aozora-epub3-adapter 0.1.0 AozoraEpub3-JDK21-1.3.4-jdk21"
   :mapping_id "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe"
   :mapping_version "0.2.1"
   :mapping_hash v3-mapping-hash
   :mapping_schema_hash mapping-schema-hash
   :parser_ir_schema_id "https://w3id.org/abc/schemas/parser-ir.schema.json"
   :parser_ir_schema_hash parser-ir-schema-hash})

(def ^:private v3-epub3-registry-entry
  (assoc v3-epub3-compat-query
         :evidence_scope {:evidence_type :conversion-audit
                          :adapter "aozora-epub3"
                          :adapter_version "aozora-epub3-adapter 0.1.0 AozoraEpub3-JDK21-1.3.4-jdk21"
                          :corpus "aozora-epub3-adapter"
                          :files_scanned 17844
                          :files_succeeded 17844
                          :files_failed 0
                          :parser_ir_nodes 10670874
                          :divergence_records 243318
                          :divergence_occurrences 24679754
                          :rules_total 130
                          :rules_emitted 64
                          :rules_missing 66
                          :unsupported_occurrences 13234}
         :compatibility "lossy"))

(def ^:private v3-rs-compat-query
  {:aat_version 1
   :aat_adapter "aozora-rs"
   :aat_adapter_version "aozora-rs-adapter 0.1.0 2b4e8d1"
   :mapping_id "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe"
   :mapping_version "0.2.1"
   :mapping_hash v3-mapping-hash
   :mapping_schema_hash mapping-schema-hash
   :parser_ir_schema_id "https://w3id.org/abc/schemas/parser-ir.schema.json"
   :parser_ir_schema_hash parser-ir-schema-hash})

(def ^:private v3-rs-registry-entry
  (assoc v3-rs-compat-query
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
                          :rules_total 130
                          :rules_emitted 26
                          :rules_missing 104
                          :unsupported_occurrences 0}
         :compatibility "lossy"))

(def ^:private v3-html-compat-query
  {:aat_version 1
   :aat_adapter "aozora2html"
   :aat_adapter_version "aozora2html-adapter 0.1.0 gem-3.0.1"
   :mapping_id "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe"
   :mapping_version "0.2.1"
   :mapping_hash v3-mapping-hash
   :mapping_schema_hash mapping-schema-hash
   :parser_ir_schema_id "https://w3id.org/abc/schemas/parser-ir.schema.json"
   :parser_ir_schema_hash parser-ir-schema-hash})

(def ^:private v3-html-registry-entry
  (assoc v3-html-compat-query
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
                          :rules_total 130
                          :rules_emitted 115
                          :rules_missing 15
                          :unsupported_occurrences 14230}
         :compatibility "lossy"))

(def ^:private v4-epub3-compat-query
  {:aat_version 1
   :aat_adapter "aozora-epub3"
   :aat_adapter_version "aozora-epub3-adapter 0.1.0 AozoraEpub3-JDK21-1.3.4-jdk21"
   :mapping_id "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe"
   :mapping_version "0.2.3"
   :mapping_hash v4-mapping-hash
   :mapping_schema_hash mapping-schema-hash
   :parser_ir_schema_id "https://w3id.org/abc/schemas/parser-ir.schema.json"
   :parser_ir_schema_hash v4-parser-ir-schema-hash})

(def ^:private v4-epub3-registry-entry
  (assoc v4-epub3-compat-query
         :evidence_scope {:evidence_type :conversion-audit
                          :adapter "aozora-epub3"
                          :adapter_version "aozora-epub3-adapter 0.1.0 AozoraEpub3-JDK21-1.3.4-jdk21"
                          :corpus "aozora-epub3-adapter"
                          :files_scanned 17844
                          :files_succeeded 17844
                          :files_failed 0
                          :parser_ir_nodes 10670874
                          :divergence_records 206149
                          :divergence_occurrences 18314414
                          :rules_total 127
                          :rules_emitted 61
                          :rules_missing 66
                          :unsupported_occurrences 13234}
         :compatibility "lossy"))

(def ^:private v4-rs-compat-query
  {:aat_version 1
   :aat_adapter "aozora-rs"
   :aat_adapter_version "aozora-rs-adapter 0.1.0 2b4e8d1"
   :mapping_id "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe"
   :mapping_version "0.2.3"
   :mapping_hash v4-mapping-hash
   :mapping_schema_hash mapping-schema-hash
   :parser_ir_schema_id "https://w3id.org/abc/schemas/parser-ir.schema.json"
   :parser_ir_schema_hash v4-parser-ir-schema-hash})

(def ^:private v4-rs-registry-entry
  (assoc v4-rs-compat-query
         :evidence_scope {:evidence_type :conversion-audit
                          :adapter "aozora-rs"
                          :adapter_version "aozora-rs-adapter 0.1.0 2b4e8d1"
                          :corpus "aozora-rs-adapter"
                          :files_scanned 17894
                          :files_succeeded 17894
                          :files_failed 0
                          :parser_ir_nodes 7821839
                          :divergence_records 217665
                          :divergence_occurrences 11926731
                          :rules_total 127
                          :rules_emitted 25
                          :rules_missing 102
                          :unsupported_occurrences 0}
         :compatibility "lossy"))

(def ^:private v4-aozora2-compat-query
  {:aat_version 1
   :aat_adapter "aozora2"
   :aat_adapter_version "aozora2-adapter 0.1.0 aozora-core-0.7.1"
   :mapping_id "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe"
   :mapping_version "0.2.3"
   :mapping_hash v4-mapping-hash
   :mapping_schema_hash mapping-schema-hash
   :parser_ir_schema_id "https://w3id.org/abc/schemas/parser-ir.schema.json"
   :parser_ir_schema_hash v4-parser-ir-schema-hash})

(def ^:private v4-aozora2-registry-entry
  (assoc v4-aozora2-compat-query
         :evidence_scope {:evidence_type :conversion-audit
                          :adapter "aozora2"
                          :adapter_version "aozora2-adapter 0.1.0 aozora-core-0.7.1"
                          :corpus "aozora2-adapter"
                          :files_scanned 17874
                          :files_succeeded 13988
                          :files_failed 3886
                          :parser_ir_nodes 1582155
                          :divergence_records 155517
                          :divergence_occurrences 4329397
                          :rules_total 127
                          :rules_emitted 89
                          :rules_missing 38
                          :unsupported_occurrences 4804}
         :compatibility "lossy"))

(def ^:private v4-html-compat-query
  {:aat_version 1
   :aat_adapter "aozora2html"
   :aat_adapter_version "aozora2html-adapter 0.1.0 gem-3.0.1"
   :mapping_id "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe"
   :mapping_version "0.2.3"
   :mapping_hash v4-mapping-hash
   :mapping_schema_hash mapping-schema-hash
   :parser_ir_schema_id "https://w3id.org/abc/schemas/parser-ir.schema.json"
   :parser_ir_schema_hash v4-parser-ir-schema-hash})

(def ^:private v4-html-registry-entry
  (assoc v4-html-compat-query
         :evidence_scope {:evidence_type :conversion-audit
                          :adapter "aozora2html"
                          :adapter_version "aozora2html-adapter 0.1.0 gem-3.0.1"
                          :corpus "aozora2html-adapter"
                          :files_scanned 17689
                          :files_succeeded 17689
                          :files_failed 0
                          :parser_ir_nodes 8419185
                          :divergence_records 250329
                          :divergence_occurrences 12246124
                          :rules_total 127
                          :rules_emitted 112
                          :rules_missing 15
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
                                                   v2-html-registry-entry
                                                   v3-epub3-registry-entry
                                                   v3-rs-registry-entry
                                                   v3-html-registry-entry
                                                   v4-epub3-registry-entry
                                                   v4-rs-registry-entry
                                                   v4-aozora2-registry-entry
                                                   v4-html-registry-entry]}))))
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

(deftest aat-parser-ir-registry-generator-backed-validation-test
  (am/install!)
  (testing "generated compatibility entries pass the public validator"
    (let [entry (mg/generate ::am/aat-parser-ir-compat-entry)]
      (is (m/validate ::am/aat-parser-ir-compat-entry entry))
      (is (empty? (compat/registry-errors {:entries [entry]})))))
  (testing "mutating a generated entry to violate conversion file counts is rejected"
    (let [entry (mg/generate ::am/aat-parser-ir-compat-entry)
          invalid-entry (assoc-in entry [:evidence_scope :files_succeeded] 99)]
      (is (has-error? #"files_scanned must equal files_succeeded plus files_failed"
                      (compat/registry-errors
                       {:entries [invalid-entry]}))))))

(defn- compatibility-entry [registry adapter adapter-version]
  (->> (:entries registry)
       (filter #(and (= adapter (:aat_adapter %))
                     (= adapter-version (:aat_adapter_version %))
                     (= "0.2.0" (:mapping_version %))))
       first))

(defn aat-parser-ir-compatibility-assertions []
  (let [registry (compat/load-registry)]
    (testing "matches measured adapter-scoped registry entries"
      (is (true? (compat/compatible? registry old-compat-query)))
      (is (true? (compat/compatible? registry current-rs-compat-query)))
      (is (true? (compat/compatible? registry current-html-compat-query)))
      (is (true? (compat/compatible? registry v2-rs-compat-query)))
      (is (true? (compat/compatible? registry v2-html-compat-query)))
      (is (true? (compat/compatible? registry v3-epub3-compat-query)))
      (is (true? (compat/compatible? registry v3-rs-compat-query)))
      (is (true? (compat/compatible? registry v3-html-compat-query)))
      (is (true? (compat/compatible? registry v4-epub3-compat-query)))
      (is (true? (compat/compatible? registry v4-rs-compat-query)))
      (is (true? (compat/compatible? registry v4-aozora2-compat-query)))
      (is (true? (compat/compatible? registry v4-html-compat-query)))
      (doseq [[adapter adapter-version] [["aozora" "aozora-adapter 0.1.0 aozora 0.4.1"]
                                         ["aozora-epub3" "aozora-epub3-adapter 0.1.0 AozoraEpub3-JDK21-1.3.4-jdk21"]
                                         ["aozora-rs" "aozora-rs-adapter 0.1.0 2b4e8d1"]
                                         ["aozora2" "aozora2-adapter 0.1.0 aozora-core-0.7.1"]
                                         ["aozora2html" "aozora2html-adapter 0.1.0 gem-3.0.1"]]]
        (is (true? (compat/compatible?
                    registry
                    {:aat_version 1
                     :aat_adapter adapter
                     :aat_adapter_version adapter-version
                     :mapping_id "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe"
                     :mapping_version "0.2.3"
                     :mapping_hash v5-mapping-hash
                     :mapping_schema_hash mapping-schema-hash
                     :parser_ir_schema_id "https://w3id.org/abc/schemas/parser-ir.schema.json"
                     :parser_ir_schema_hash v5-parser-ir-schema-hash}))))
      (doseq [[adapter adapter-version] [["aozora" "aozora-adapter 0.1.0 aozora 0.4.1"]
                                         ["aozora-epub3" "aozora-epub3-adapter 0.1.0 AozoraEpub3-JDK21-1.3.4-jdk21"]
                                         ["aozora-rs" "aozora-rs-adapter 0.1.0 2b4e8d1"]
                                         ["aozora2" "aozora2-adapter 0.1.0 aozora-core-0.7.1"]
                                         ["aozora2html" "aozora2html-adapter 0.1.0 gem-3.0.1"]]]
        (is (true? (compat/compatible?
                    registry
                    {:aat_version 1
                     :aat_adapter adapter
                     :aat_adapter_version adapter-version
                     :mapping_id "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe"
                     :mapping_version "0.2.4"
                     :mapping_hash v6-mapping-hash
                     :mapping_schema_hash current-mapping-schema-hash
                     :parser_ir_schema_id "https://w3id.org/abc/schemas/parser-ir.schema.json"
                     :parser_ir_schema_hash v6-parser-ir-schema-hash}))))
      (let [aozora-rs-entry (compatibility-entry registry "aozora-rs" "aozora-rs-adapter 0.1.0 2b4e8d1")
            aozora2html-entry (compatibility-entry registry "aozora2html" "aozora2html-adapter 0.1.0 gem-3.0.1")]
        (is (some? aozora-rs-entry) "missing 0.2.0 aozora-rs registry entry")
        (is (some? aozora2html-entry) "missing 0.2.0 aozora2html registry entry")
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

(deftest aat-parser-ir-compatibility-test
  (runtime/with-validated-read-trace!
    (evidence-support/focused-trace-options "adr-0009-c5-aat-conversion-compatibility")
    (fn [] (aat-parser-ir-compatibility-assertions))))

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
  (testing "reports when current 0.2.1 producer candidates are admitted exactly"
    (is (= {:status :admitted
            :candidate-count 3
            :admitted [v3-epub3-registry-entry v3-rs-registry-entry v3-html-registry-entry]
            :missing []
            :conflicts []
            :registry-errors []
            :candidate-errors []}
           (compat/admission-report
            {:entries [v3-epub3-registry-entry v3-rs-registry-entry v3-html-registry-entry]}
            {:entries [v3-epub3-registry-entry v3-rs-registry-entry v3-html-registry-entry]}))))
  (testing "reports when latest admitted 0.2.3 Level 3 producer candidates are admitted exactly"
    (is (= {:status :admitted
            :candidate-count 4
            :admitted [v4-epub3-registry-entry
                       v4-rs-registry-entry
                       v4-aozora2-registry-entry
                       v4-html-registry-entry]
            :missing []
            :conflicts []
            :registry-errors []
            :candidate-errors []}
           (compat/admission-report
            {:entries [v4-epub3-registry-entry
                       v4-rs-registry-entry
                       v4-aozora2-registry-entry
                       v4-html-registry-entry]}
            {:entries [v4-epub3-registry-entry
                       v4-rs-registry-entry
                       v4-aozora2-registry-entry
                       v4-html-registry-entry]}))))
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

(defn- tei-schema-path!
  []
  (or (System/getenv "TEI_SCHEMA_PATH")
      (throw (ex-info "TEI_SCHEMA_PATH must be set for TEI schema-backed tests."
                      {:env-var "TEI_SCHEMA_PATH"}))))

(deftest validate-tei-smoke-test
  (testing "validate-tei! returns nil for the example fixture when TEI_SCHEMA_PATH is set"
    (is (nil? (validate/validate-tei! (tei-schema-path!)
                                      ["examples/v0/example-work/tei.xml"])))))

(deftest project-rng-valid-schematron-invalid-fixture-test
  (let [path "fixtures/tei/invalid/abc-bad-layout-params.xml"]
    (is (nil? (validate/validate-tei! "schemas/tei-profile.rng" [path])))
    (let [{:keys [findings]} (schematron/validate!
                              {:schema-path "schemas/tei-profile.sch"
                               :xml-path path
                               :label path})]
      (is (= ["abc-layout-params-shape"] (mapv :rule-id findings)))
      (is (= [:error] (mapv :severity findings))))))

(deftest valid-project-tei-fixtures-pass-rng-and-schematron-test
  (let [paths ["examples/v0/example-work/tei.xml"
               "fixtures/tei/valid/rashomon-minimal.xml"
               "fixtures/tei/valid/source-span-local-ref.xml"
               "fixtures/tei/valid/transcription-enrichment-declared.xml"]]
    (is (nil? (validate/validate-tei! "schemas/tei-profile.rng" paths)))
    (doseq [path paths]
      (let [{:keys [findings]} (schematron/validate!
                                {:schema-path "schemas/tei-profile.sch"
                                 :xml-path path
                                 :label path})]
        (is (not-any? #(= :error (:severity %)) findings) path)))))

(deftest tei-committed-manifest-references-validation-result-test
  (let [manifest (files/read-json "examples/v0/example-work/manifest.json")]
    (is (some #(and (= "validation-result" (get % "role"))
                    (= "tei-validation-result.json" (get % "path_hint")))
              (get manifest "sidecars")))))

(defn- check-expression [flake check-name]
  (re-find
   (re-pattern
    (str "(?s)\\n          " (java.util.regex.Pattern/quote check-name)
         " =.*?(?=\\n          [a-zA-Z0-9_-]+ =|\\n        };)"))
   flake))

(defn- tei-evidence-check-contract-errors [expression expected-focuses]
  (cond-> []
    (not (string/includes? expression "pkgs.clojure"))
    (conj :missing-clojure)
    (not (string/includes? expression
                           "export TEI_SCHEMA_PATH=\"${tei.teiAllSchema}\""))
    (conj :missing-tei-schema-path)
    (not-every? #(string/includes? expression %) expected-focuses)
    (conj :missing-focus)))

(deftest tei-evidence-nix-checks-are-hermetic-test
  (let [flake (slurp "flake.nix")
        contracts
        {"adr-evidence-tei-project-cross-schema-invalid"
         ["abc.tools.validate-design-bundle-test/project-rng-valid-schematron-invalid-fixture-test"]
         "adr-evidence-tei-project-valid-fixtures"
         ["abc.tools.validate-design-bundle-test/valid-project-tei-fixtures-pass-rng-and-schematron-test"]
         "adr-evidence-tei-schematron-invalid-ids"
         ["abc.tools.schematron-test/missing-title-fails-title-rule-test"
          "abc.tools.schematron-test/gaiji-missing-reference-fails-gaiji-rule-test"
          "abc.tools.schematron-test/ruby-missing-reading-fails-ruby-rule-test"]
         "adr-evidence-tei-figure-warning"
         ["abc.tools.schematron-test/figure-missing-description-reports-warning-test"]
         "adr-evidence-tei-enrichment-warning"
         ["abc.tools.schematron-test/transcription-enrichment-undeclared-reports-warning-test"]
         "adr-evidence-tei-upstream-rng"
         ["abc.tools.tei-test/validate-example-fixture-test"]
         "adr-evidence-tei-publication-sidecars"
         ["abc.tools.materialize-publication-test/tei-generated-manifest-references-validation-result-test"
          "abc.tools.validate-design-bundle-test/tei-committed-manifest-references-validation-result-test"]}]
    (doseq [[check-name focuses] contracts]
      (let [expression (check-expression flake check-name)]
        (is (string? expression) check-name)
        (is (= [] (tei-evidence-check-contract-errors expression focuses))
            check-name)))
    (let [sample (check-expression
                  flake "adr-evidence-tei-project-cross-schema-invalid")]
      (is (= [:missing-clojure]
             (tei-evidence-check-contract-errors
              (string/replace sample "pkgs.clojure" "")
              (get contracts "adr-evidence-tei-project-cross-schema-invalid"))))
      (is (= [:missing-tei-schema-path]
             (tei-evidence-check-contract-errors
              (string/replace sample
                              "export TEI_SCHEMA_PATH=\"${tei.teiAllSchema}\""
                              "")
              (get contracts "adr-evidence-tei-project-cross-schema-invalid")))))))

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
               validate/tei-schematron-fixtures)))))

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

(deftest tei-fixture-catalog-drives-validation-paths-test
  (testing "TEI fixture paths are derived from a single catalog"
    (let [fixture-paths (mapv :path validate/tei-fixture-catalog)
          schematron validate/tei-schematron-fixtures
          schematron-paths (set (concat (:valid-fixtures schematron)
                                        (keys (:warning-fixtures schematron))
                                        (keys (:invalid-fixtures schematron))))]
      (is (= fixture-paths (validate/tei-fixture-paths)))
      (is (= (into ["schemas/tei-profile.odd"
                    "schemas/tei-profile.sch"
                    "schemas/tei-profile.rng"]
                   fixture-paths)
             (validate/tei-xml-paths)))
      (is (= ["examples/v0/example-work/tei.xml"
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
              "fixtures/tei/invalid/source-span-external-ref.xml"]
             (validate/tei-project-rng-paths)))
      (is (= (set fixture-paths) schematron-paths)))))

(def ^:private expected-tei-schematron-fixtures-snapshot
  "Snapshot of the production TEI Schematron fixture map derived from
  tei-fixture-catalog. It pins the current partition so any catalog change is
  deliberate and reviewable."
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

(deftest tei-schematron-fixtures-unchanged-test
  (testing "hand-coded Schematron fixture map is unchanged and still passes"
    (is (= expected-tei-schematron-fixtures-snapshot validate/tei-schematron-fixtures))
    (is (nil? (validate/validate-tei-schematron! validate/tei-schematron-fixtures)))))

(deftest tei-schematron-rule-universe-test
  (testing "rule-universe extracts exactly the 16 abc-* ids from the ODD"
    (is (= #{"abc-tei-header-title"
             "abc-tei-header-source-work-id"
             "abc-header-language-declared"
             "abc-layout-params-shape"
             "abc-preservation-record-shape"
             "abc-ruby-complete"
             "abc-ruby-base-non-empty"
             "abc-ruby-reading-non-empty"
             "abc-gaiji-reference"
             "abc-gaiji-chardecl-resolution"
             "abc-char-resolution-form"
             "abc-figure-accessibility"
             "abc-source-span-reference"
             "abc-source-span-target-exists"
             "abc-transcription-vs-annotation"
             "abc-vocab-version-declared"}
           (validate/rule-universe)))))

(deftest rule-universe-parses-xml-instead-of-regex-shape-test
  (testing "rule-universe reads ODD XML attributes independent of quote style"
    (let [odd (java.io.File/createTempFile "abc-rule-universe" ".odd")]
      (try
        (spit odd (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                       "<TEI xmlns=\"http://www.tei-c.org/ns/1.0\">"
                       "<text><body>"
                       "<constraintSpec scheme='schematron' ident='abc-single-quoted'>"
                       "<constraint><sch:pattern xmlns:sch=\"http://purl.oclc.org/dsdl/schematron\"/></constraint>"
                       "</constraintSpec>"
                       "<constraintSpec ident=\"abc-non-schematron\" scheme=\"other\"/>"
                       "</body></text></TEI>"))
        (is (= #{"abc-single-quoted"}
               (validate/rule-universe (str odd))))
        (finally
          (.delete odd))))))

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
    (let [schema-path (tei-schema-path!)
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
          (.delete tmp))))))

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
