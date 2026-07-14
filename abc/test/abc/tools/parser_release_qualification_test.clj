(ns abc.tools.parser-release-qualification-test
  (:require [abc.tools.parser-release-qualification :as q]
            [clojure.test :refer [deftest is testing]]))

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

;; --- Exact numeric boundary: 0.969 FAILS a 1.0 predicate ----------------------

(deftest span-coverage-0969-fails-1_0-predicate-test
  (let [pred (first (filter #(= :source-span-coverage (:predicate_id %))
                            (:predicates (q/load-predicates))))]
    (is (not= 0.969 1.0))
    (is (= := (get-in pred [:expected :comparator])))
    (is (= 1.0 (get-in pred [:expected :value])))
    (testing "0.969 observed is a mechanical fail (exact equality, not float-fuzzy)"
      (is (= :fail (:verdict (q/evaluate-predicate pred {:source_span_coverage 0.969})))))
    (testing "only an exact 1.0 passes"
      (is (= :pass (:verdict (q/evaluate-predicate pred {:source_span_coverage 1.0})))))))

(deftest compare-observed-is-exact-test
  (is (false? (q/compare-observed := 1.0 0.969)))
  (is (true? (q/compare-observed := 1.0 1.0)))
  (is (true? (q/compare-observed :<= 0 0)))
  (is (false? (q/compare-observed :<= 0 1)))
  (is (true? (q/compare-observed :>= 1.0 1.0))))

;; --- Integrity rule: no instrument -> :unavailable, never :pass ---------------

(deftest missing-instrument-is-unavailable-not-pass-test
  (let [pred {:predicate_id :memory :dimension "d" :instrument "none"
              :observed_key :peak_rss_bytes :unit "bytes"
              :expected {:comparator :<= :value 2147483648}}]
    (is (= :unavailable (:verdict (q/evaluate-predicate pred {}))))
    (is (= :unavailable (:verdict (q/evaluate-predicate pred {:peak_rss_bytes :unavailable}))))
    (is (= :unavailable (:verdict (q/evaluate-predicate pred {:peak_rss_bytes :instrument-missing}))))
    (testing "a real observation within bound still passes"
      (is (= :pass (:verdict (q/evaluate-predicate pred {:peak_rss_bytes 1024})))))))

;; --- Gate status + ADR promotion rule -----------------------------------------

(deftest gate-and-adr-status-require-all-pass-test
  (let [preds (q/load-predicates)
        all-pass {:fatal_failures 0 :source_span_coverage 1.0 :silent_drops 0
                  :diagnostic_completeness 1.0 :parser_ir_schema_validation 1.0
                  :publication_structure 1.0 :wall_time_seconds 12 :peak_rss_bytes 1024
                  :timeouts 0}
        one-unavailable (dissoc all-pass :peak_rss_bytes)
        one-fail (assoc all-pass :source_span_coverage 0.969)]
    (is (= :release-qualified (q/gate-status (q/evaluate preds all-pass))))
    (is (= "Accepted" (q/adr-0039-status (q/evaluate preds all-pass))))
    (is (= :not-qualified (q/gate-status (q/evaluate preds one-unavailable))))
    (is (= "Proposed" (q/adr-0039-status (q/evaluate preds one-unavailable))))
    (is (= :not-qualified (q/gate-status (q/evaluate preds one-fail))))
    (is (= "Proposed" (q/adr-0039-status (q/evaluate preds one-fail))))))

;; --- Release-evidence class boundary (Task 6 / ADR 0038) ----------------------

(deftest comparison-and-neutral-evidence-cannot-qualify-release-test
  (testing "a conversion-compatibility entry is a valid release evidence class"
    (is (map? (q/release-evidence-guard! {:evidence_id "x"
                                          :evidence_class :conversion-compatibility}))))
  (doseq [cls [:parser-selection :neutral-comparison :comparator-oracle]]
    (testing (str cls " is structurally rejected as release evidence")
      (is (thrown? clojure.lang.ExceptionInfo
                   (q/release-evidence-guard! {:evidence_id "x" :evidence_class cls}))))))

;; --- Report assembly + schema -------------------------------------------------

(deftest build-report-validates-against-schema-test
  (let [corpus (q/load-corpus)
        preds (q/load-predicates)
        measurements {:fatal_failures 0 :source_span_coverage :instrument-missing
                      :silent_drops :instrument-missing :diagnostic_completeness 1.0
                      :parser_ir_schema_validation 1.0 :publication_structure :instrument-missing
                      :wall_time_seconds 8 :peak_rss_bytes :instrument-missing :timeouts 0}
        report (q/build-report
                {:report_id "test-report"
                 :corpus corpus
                 :predicate-set preds
                 :identity {:parser "ab-aozora"
                            :adapter_version "ab-aozora 0.6.0 aat-schema 2 facade 0.3.0 wire-schema 3 (git deadbeef)"
                            :admitted_tuple_adapter_version "ab-aozora 0.6.0 aat-schema 2 facade 0.3.0 wire-schema 3 (git 004deaf548f34a36abbc17d0f7a162df010a6292)"
                            :admitted_tuple_matches false}
                 :measurements measurements})]
    (is (q/report-valid? report) (pr-str (q/report-explain report)))
    (is (= :not-qualified (:gate_status report)))
    (is (= "Proposed" (:adr_0039_status report)))
    (is (= (:list_hash corpus) (get-in report [:identity :corpus_list_hash])))
    (is (= 9 (count (:predicate_verdicts report))))))
