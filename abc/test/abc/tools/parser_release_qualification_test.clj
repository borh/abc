(ns abc.tools.parser-release-qualification-test
  (:require [abc.tools.aat-parser-ir-compat :as compat]
            [abc.tools.parser-release-qualification :as q]
            [abc.tools.parser-rq-capture :as capture]
            [clojure.test :refer [deftest is testing]]))

(def admitted-identity
  {:aat_version 2
   :aat_adapter "ab-aozora"
   :aat_adapter_version "ab-aozora 0.6.0 aat-schema 2 facade 0.3.0 wire-schema 3 (git 004deaf548f34a36abbc17d0f7a162df010a6292)"
   :mapping_id "https://w3id.org/abc/mappings/aat-v2-to-parser-ir-v1/generated-probe"
   :mapping_version "0.4.0"
   :mapping_hash "sha256:cf177bee98af086fe728cbc1942e4f631b26ed5bb55aedc7d91b21f467c41f30"
   :mapping_schema_hash "sha256:e6af01115ccdb7c5cad086eee4c458230f6b6f55e0dfee7791730b48994283e2"
   :parser_ir_schema_id "https://w3id.org/abc/schemas/parser-ir.schema.json"
   :parser_ir_schema_hash "sha256:a1e1b5069fdec17cbb1f94eb5e9a582d1b109dd95c07257f4da7d9b76c82cfa2"
   :parser_git_rev "004deaf548f34a36abbc17d0f7a162df010a6292"
   :corpus_snapshot_hash "sha256:63d8d53a9a0ef8ec80c921d7fb17d142f231fbc061066fb8056b951ffcfbe47e"
   :corpus_list_hash "sha256:ace3fa3f4fb6565d46276d8276b4a2e183c58e595f27f0e3149d7395ca6554dd"
   :predicate_set_hash "sha256:d40f5c8996f3e93165575895b579902d10ec2ad1c06107e1047fe7ac8694f15f"
   :instrument_versions {:fixture "v1"}})

(def all-pass-values
  {:fatal_failures 0 :source_span_coverage 1.0 :silent_drops 0
   :diagnostic_completeness 1.0 :parser_ir_schema_validation 1.0
   :publication_structure 1.0 :wall_time_seconds 12 :peak_cgroup_memory_bytes 1024
   :timeouts 0})

(defn envelopes [identity values]
  (let [identity-ref (q/qualification-identity-ref identity)]
    (update-vals values #(hash-map :value % :identity_ref identity-ref))))

(defn report-input [identity measurements]
  {:report_id "test-report"
   :corpus (q/load-corpus)
   :predicate-set (q/load-predicates)
   :registry (compat/load-registry)
   :identity identity
   :measurements measurements})

;; --- Pinned corpus integrity --------------------------------------------------

(deftest committed-corpus-passes-its-own-integrity-check-test
  (let [corpus (q/load-corpus)]
    (is (= [] (q/corpus-integrity-errors corpus)))
    (is (= (:corpus_snapshot_hash corpus) (q/corpus-snapshot-hash corpus)))
    (is (= (:list_hash corpus) (q/corpus-list-hash corpus)))
    (is (seq (:entries corpus)))))

(deftest corpus-list-hash-detects-tampering-test
  (testing "changing a pinned work identity is caught by the recomputed list hash"
    (let [corpus (q/load-corpus)
          tampered (assoc-in corpus [:entries 0 :expected_status] :failed)]
      (is (seq (q/corpus-integrity-errors tampered)))
      (is (thrown? clojure.lang.ExceptionInfo (q/validate-corpus! tampered))))))

;; --- Predicate declaration ----------------------------------------------------

(deftest predicate-set-declares-all-nine-dimensions-test
  (let [ids (set (map :predicate_id (:predicates (q/load-predicates))))]
    (is (= #{:fatal-failures :source-span-coverage :silent-drops
             :diagnostic-completeness :parser-ir-schema-validation
             :publication-structure :wall-time :memory :timeout-policy}
           ids))
    (doseq [p (:predicates (q/load-predicates))]
      (is (contains? (:expected p) :comparator) (str p))
      (is (contains? (:expected p) :value) (str p)))))

(deftest predicate-set-hash-is-pinned-and-recomputed
  (let [predicates (q/load-predicates)]
    (is (= (:predicate_set_hash predicates)
           (q/predicate-set-hash predicates)))))

(deftest memory-predicate-is-process-tree-cgroup-memory
  (let [predicate (first (filter #(= :memory (:predicate_id %))
                                 (:predicates (q/load-predicates))))]
    (is (= :peak_cgroup_memory_bytes (:observed_key predicate)))
    (is (= "parser-rq-resource-v1" (:instrument predicate)))
    (is (= 2147483648 (get-in predicate [:expected :value])))))

;; --- Exact numeric boundary: 0.969 FAILS a 1.0 predicate ----------------------

(deftest span-coverage-0969-fails-1_0-predicate-test
  (let [pred (first (filter #(= :source-span-coverage (:predicate_id %))
                            (:predicates (q/load-predicates))))]
    (is (not= 0.969 1.0))
    (is (= := (get-in pred [:expected :comparator])))
    (is (= 1.0 (get-in pred [:expected :value])))
    (testing "0.969 observed is a mechanical fail (exact equality, not float-fuzzy)"
      (is (= :fail (:verdict (q/evaluate-predicate pred {:source_span_coverage {:value 0.969}})))))
    (testing "only an exact 1.0 passes"
      (is (= :pass (:verdict (q/evaluate-predicate pred {:source_span_coverage {:value 1.0}})))))))

(deftest semantic-source-recognition-replaces-r1-without-hiding-node-evidence
  (let [identity-ref (q/qualification-identity-ref admitted-identity)
        node-envelope {:value 1.0M :identity_ref identity-ref}
        recognition-envelope {:value 0.9M :identity_ref identity-ref}
        migrated (q/install-source-recognition-observation
                  {:source_span_coverage node-envelope}
                  recognition-envelope)
        pred (first (filter #(= :source-span-coverage (:predicate_id %))
                            (:predicates (q/load-predicates))))]
    (is (= recognition-envelope (:source_span_coverage migrated)))
    (is (= node-envelope (:parser_ir_node_span_coverage migrated)))
    (is (= :fail (:verdict (q/evaluate-predicate pred migrated))))))

(deftest compare-observed-is-exact-test
  (is (false? (q/compare-observed := 1.0 0.969)))
  (is (true? (q/compare-observed := 1.0 1.0)))
  (is (true? (q/compare-observed :<= 0 0)))
  (is (false? (q/compare-observed :<= 0 1)))
  (is (true? (q/compare-observed :>= 1.0 1.0))))

;; --- Integrity rule: no instrument -> :unavailable, never :pass ---------------

(deftest missing-instrument-is-unavailable-not-pass-test
  (let [pred {:predicate_id :memory :dimension "d" :instrument "none"
              :observed_key :peak_cgroup_memory_bytes :unit "bytes"
              :expected {:comparator :<= :value 2147483648}}]
    (is (= :unavailable (:verdict (q/evaluate-predicate pred {}))))
    (is (= :unavailable (:verdict (q/evaluate-predicate pred {:peak_cgroup_memory_bytes {:value :unavailable}}))))
    (is (= :unavailable (:verdict (q/evaluate-predicate pred {:peak_cgroup_memory_bytes {:value :instrument-missing}}))))
    (testing "a real observation within bound still passes"
      (is (= :pass (:verdict (q/evaluate-predicate pred {:peak_cgroup_memory_bytes {:value 1024}})))))))

(deftest typed-failure-sentinels-are-available-failures-test
  (let [predicates (into {} (map (juxt :predicate_id identity)
                                 (:predicates (q/load-predicates))))]
    (is (= :fail
           (:verdict (q/evaluate-predicate
                      (:diagnostic-completeness predicates)
                      {:diagnostic_completeness
                       {:value :invalid-diagnostic-envelope}}))))
    (is (= :fail
           (:verdict (q/evaluate-predicate
                      (:parser-ir-schema-validation predicates)
                      {:parser_ir_schema_validation
                       {:value :no-parser-ir-output}}))))))

(deftest parser-ir-all-valid-double-passes-exact-comparator-test
  (let [predicate (first (filter #(= :parser-ir-schema-validation
                                     (:predicate_id %))
                                 (:predicates (q/load-predicates))))
        envelope {:value (double 1.0)
                  :identity_ref (q/qualification-identity-ref admitted-identity)}]
    (is (double? (:value envelope)))
    (is (= :pass
           (:verdict (q/evaluate-predicate
                      predicate {:parser_ir_schema_validation envelope}))))))

;; --- Gate status + ADR promotion rule -----------------------------------------

(deftest predicate-tally-and-adr-status-require-all-pass-test
  (let [preds (q/load-predicates)
        all-pass {:fatal_failures 0 :source_span_coverage 1.0 :silent_drops 0
                  :diagnostic_completeness 1.0 :parser_ir_schema_validation 1.0
                  :publication_structure 1.0 :wall_time_seconds 12 :peak_cgroup_memory_bytes 1024
                  :timeouts 0}
        one-unavailable (dissoc all-pass :peak_cgroup_memory_bytes)
        one-fail (assoc all-pass :source_span_coverage 0.969)]
    (is (= "Accepted" (q/adr-0039-status true (q/evaluate preds (envelopes admitted-identity all-pass)))))
    (is (= "Proposed" (q/adr-0039-status true (q/evaluate preds (envelopes admitted-identity one-unavailable)))))
    (is (= "Proposed" (q/adr-0039-status true (q/evaluate preds (envelopes admitted-identity one-fail)))))))

(deftest admission-query-projects-exactly-the-nine-match-keys
  (is (= (set compat/match-keys)
         (set (keys (q/admission-query admitted-identity))))))

(deftest gate-not-qualified-when-tuple-unadmitted-even-if-all-pass
  (let [unadmitted (assoc admitted-identity :mapping_version "9.9.9")
        report (q/build-report (report-input unadmitted (envelopes unadmitted all-pass-values)))]
    (is (false? (q/admitted? (compat/load-registry) unadmitted)))
    (is (= :not-qualified (:gate_status report)))))

(deftest gate-not-qualified-when-observations-incoherent
  (let [measurements (assoc-in (envelopes admitted-identity all-pass-values)
                               [:fatal_failures :identity_ref]
                               capture/sha256-schema)
        report (q/build-report (report-input admitted-identity measurements))]
    (is (seq (q/coherence-errors admitted-identity measurements)))
    (is (= :not-qualified (:gate_status report)))))

(deftest gate-not-qualified-when-pinned-contract-hashes-disagree
  (let [wrong-corpus (assoc admitted-identity :corpus_list_hash
                            "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc")
        report (q/build-report (report-input wrong-corpus
                                             (envelopes wrong-corpus all-pass-values)))]
    (is (= :not-qualified (:gate_status report)))
    (is (some #(re-find #"corpus_list_hash" %) (get-in report [:coherence :errors])))
    (is (= (:identity report) wrong-corpus))
    (is (= (q/qualification-identity-ref wrong-corpus)
           (get-in report [:coherence :identity_ref])))))

(deftest gate-not-qualified-when-full-identity-is-incomplete
  (let [incomplete (dissoc admitted-identity :parser_git_rev :instrument_versions)
        report (q/build-report (report-input incomplete
                                             (envelopes incomplete all-pass-values)))]
    (is (= :not-qualified (:gate_status report)))
    (is (some #(re-find #"parser_git_rev" %) (get-in report [:coherence :errors])))
    (is (some #(re-find #"instrument_versions" %) (get-in report [:coherence :errors])))))

(deftest gate-not-qualified-when-identity-values-are-malformed
  (doseq [malformed [(assoc admitted-identity :parser_git_rev nil)
                     (assoc admitted-identity :parser_git_rev "  ")
                     (assoc admitted-identity :instrument_versions {:fixture nil})
                     (assoc admitted-identity :instrument_versions {"" "v1"})]]
    (let [report (q/build-report
                  (report-input malformed (envelopes malformed all-pass-values)))]
      (is (= :not-qualified (:gate_status report)))
      (is (some #(re-find #"qualification identity values" %)
                (get-in report [:coherence :errors]))))))

(deftest gate-release-qualified-requires-precondition-and-nine-pass
  (let [report (q/build-report
                (report-input admitted-identity (envelopes admitted-identity all-pass-values)))]
    (is (= :release-qualified (:gate_status report)))))

;; The release evidence-class boundary itself (assert-release-evidence! /
;; entry-release-qualifying?) is exercised directly in
;; abc.tools.parser-evidence-test; this gate consumes a measurement bundle and
;; never ingests citations, so there is no wrapper to test here.

;; --- Report assembly + schema -------------------------------------------------

(deftest build-report-validates-against-schema-test
  (let [corpus (q/load-corpus)
        preds (q/load-predicates)
        values {:fatal_failures 0 :source_span_coverage :instrument-missing
                :silent_drops :instrument-missing :diagnostic_completeness 1.0
                :parser_ir_schema_validation 1.0 :publication_structure :instrument-missing
                :wall_time_seconds 8 :peak_cgroup_memory_bytes :instrument-missing :timeouts 0}
        measurements (envelopes admitted-identity values)
        report (q/build-report
                {:report_id "test-report"
                 :corpus corpus
                 :predicate-set preds
                 :registry (compat/load-registry)
                 :identity admitted-identity
                 :measurements measurements})]
    (is (q/report-valid? report) (pr-str (q/report-explain report)))
    (is (= :not-qualified (:gate_status report)))
    (is (= "Proposed" (:adr_0039_status report)))
    (is (= (:list_hash corpus) (get-in report [:identity :corpus_list_hash])))
    (is (= 9 (count (:predicate_verdicts report))))))
