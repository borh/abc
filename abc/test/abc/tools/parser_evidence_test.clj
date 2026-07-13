(ns abc.tools.parser-evidence-test
  (:require [abc.tools.malli :as am]
            [abc.tools.hash :as hash]
            [abc.tools.parser-evidence :as parser-evidence]
            [abc.test-fs :refer [with-temp-dir]]
            [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Install (compose registry + instrument m/=> contracts) once for the whole
;; namespace so the instrumentation-activation test below actually bites.
(use-fixtures :once (fn [f] (am/install!) (f)))

(def valid-entry
  {:evidence_id "ab-validator/example"
   :evidence_class :conversion-compatibility
   :producer_component "ab-validator"
   :logical_path "ab-validator/docs/example.md"
   :current_external_path "docs/example.md"
   :sha256 "sha256:bf0910f5316efc2cd528f504c0cbd16816ca993e7ccb2b99122aab8a71359527"
   :status :citable
   :summary "Example evidence."})

(defn- has-error?
  [pattern errors]
  (boolean (some #(re-find pattern %) errors)))

(deftest parser-evidence-duplicate-keys-have-named-pure-targets-test
  (is (= (:evidence_id valid-entry)
         (parser-evidence/evidence-id-key valid-entry)))
  (is (= ((juxt :logical_path :sha256) valid-entry)
         (parser-evidence/logical-file-key valid-entry))))

(deftest parser-evidence-citation-files-are-content-addressed-test
  (with-temp-dir [base]
    (let [root (fs/file base "monorepo")
          outside (fs/file base "outside.md")
          passing (fs/file root "docs/passing.md")
          drifted (fs/file root "docs/drifted.md")
          escaped (fs/file root "docs/escaped.md")]
      (fs/create-dirs (fs/parent passing))
      (spit passing "passing bytes")
      (spit drifted "changed bytes")
      (spit outside "outside bytes")
      (fs/create-sym-link escaped outside)
      (let [index {:entries [{:evidence_id "passing"
                              :logical_path "docs/passing.md"
                              :sha256 (str "sha256:" (hash/sha256-file passing))}
                             {:evidence_id "missing"
                              :logical_path "docs/missing.md"
                              :sha256 (str "sha256:" (apply str (repeat 64 "0")))}
                             {:evidence_id "drifted"
                              :logical_path "docs/drifted.md"
                              :sha256 (str "sha256:" (apply str (repeat 64 "0")))}
                             {:evidence_id "lexical-escape"
                              :logical_path "../outside.md"
                              :sha256 (str "sha256:" (hash/sha256-file outside))}
                             {:evidence_id "real-path-escape"
                              :logical_path "docs/escaped.md"
                              :sha256 (str "sha256:" (hash/sha256-file outside))}]}]
        (is (= [(sorted-map :kind :citation-missing
                            :evidence-id "missing"
                            :logical-path "docs/missing.md")
                (sorted-map :kind :citation-hash-mismatch
                            :evidence-id "drifted"
                            :logical-path "docs/drifted.md")
                (sorted-map :kind :citation-path-traversal
                            :evidence-id "lexical-escape"
                            :logical-path "../outside.md")
                (sorted-map :kind :citation-real-path-escape
                            :evidence-id "real-path-escape"
                            :logical-path "docs/escaped.md")]
               (parser-evidence/citation-file-problems root index)))))))

(deftest historical-parser-citation-rows-are-exact-and-not-neutral-study-test
  (let [root (fs/canonicalize "..")
        ids #{"ab-validator/aozora-parser-comparison-study-2026-07-08"
              "ab-validator/parser-fork-candidacy-faithful-comparison-2026-07-08"
              "ab-validator/parser-comparison-followups-2026-07-09"}
        rows (->> (:entries (parser-evidence/load-index))
                  (filter #(contains? ids (:evidence_id %))))
        expected {"ab-validator/aozora-parser-comparison-study-2026-07-08"
                  ["ab-validator/docs/superpowers/reports/2026-07-08-aozora-parser-comparison-study.md"
                   "sha256:8f68178186c9ccad2514c833098f3ffbc725183699757d5dc8182678c5826a59"]
                  "ab-validator/parser-fork-candidacy-faithful-comparison-2026-07-08"
                  ["ab-validator/docs/superpowers/reports/2026-07-08-parser-fork-candidacy-faithful-comparison.md"
                   "sha256:ec0e5b7cdef6ea0c14b6ba59af8a35eb7b742b12a3da74683f771873330933e8"]
                  "ab-validator/parser-comparison-followups-2026-07-09"
                  ["ab-validator/docs/superpowers/specs/2026-07-09-parser-comparison-followups-handoff.md"
                   "sha256:e13c904f36ddc4180f1326cdc375e6939716487e55318349c86c64e5158d2aa0"]}]
    (is (= expected
           (into {} (map (juxt :evidence_id
                               (juxt :logical_path :sha256)) rows))))
    (is (empty? (parser-evidence/citation-file-problems root {:entries rows})))
    (is (every? #(not (contains? % :study_contract)) rows))
    (is (every? #(not (contains? % :neutral_comparison)) rows))
    (is (every? #(not= :neutral-comparison (:evidence_class %)) rows))))

(deftest parser-evidence-index-validation-test
  (testing "accepts the committed evidence index"
    (is (= :ok (parser-evidence/validate-index!
                (parser-evidence/load-index)))))
  (testing "rejects missing required keys"
    (is (has-error? #"entry 0 is missing :sha256"
                    (parser-evidence/index-errors
                     {:entries [(dissoc valid-entry :sha256)]}))))
  (testing "rejects physical relative paths as identity paths"
    (is (has-error? #":logical_path must be workspace-relative"
                    (parser-evidence/index-errors
                     {:entries [(assoc valid-entry
                                       :logical_path
                                       "../ab-validator/docs/example.md")]}))))
  (testing "rejects invalid evidence classes"
    (is (has-error? #":evidence_class must be"
                    (parser-evidence/index-errors
                     {:entries [(assoc valid-entry
                                       :evidence_class
                                       :unknown)]}))))
  (testing "rejects duplicate evidence ids"
    (is (has-error? #"duplicates :evidence_id"
                    (parser-evidence/index-errors
                     {:entries [valid-entry
                                (assoc valid-entry
                                       :logical_path
                                       "ab-validator/docs/other.md")]})))))

(deftest parser-evidence-citable-hashes-test
  (let [index {:entries [(assoc valid-entry
                                :sha256 "sha256:2222222222222222222222222222222222222222222222222222222222222222"
                                :status :provisional)
                         (assoc valid-entry
                                :evidence_id "ab-validator/citable-b"
                                :logical_path "ab-validator/docs/b.md"
                                :sha256 "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                                :status :citable)
                         (assoc valid-entry
                                :evidence_id "ab-validator/citable-a"
                                :logical_path "ab-validator/docs/a.md"
                                :sha256 "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                                :status :citable)]}]
    (is (= ["sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
            "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"]
           (parser-evidence/citable-hashes index)))))

(deftest parser-evidence-nullable-external-path-test
  (testing "current_external_path may be absent or nil but not blank"
    (is (empty? (parser-evidence/index-errors
                 {:entries [(dissoc valid-entry :current_external_path)]})))
    (is (empty? (parser-evidence/index-errors
                 {:entries [(assoc valid-entry :current_external_path nil)]})))
    (is (has-error? #":current_external_path must be null or a non-empty string"
                    (parser-evidence/index-errors
                     {:entries [(assoc valid-entry :current_external_path "")]})))))

(deftest parser-evidence-entry-round-trips-and-validates-property-test
  (testing "every generated entry validates, EDN-round-trips, and is accepted"
    (let [result (tc/quick-check
                  100
                  (prop/for-all [entry (mg/generator ::am/parser-evidence-entry)]
                                (and (m/validate ::am/parser-evidence-entry entry)
                                     (= entry (edn/read-string (pr-str entry)))
                                     (empty? (parser-evidence/index-errors
                                              {:entries [entry]})))))]
      (is (:pass? result) (pr-str result)))))

(deftest parser-evidence-mutation-rejected-property-test
  (testing "mutating any generated entry's :sha256 is rejected by the validator"
    (let [result (tc/quick-check
                  50
                  (prop/for-all [entry (mg/generator ::am/parser-evidence-entry)]
                                (has-error? #":sha256 must be a sha256 hash"
                                            (parser-evidence/index-errors
                                             {:entries [(assoc entry :sha256 "sha256:not-real")]}))))]
      (is (:pass? result) (pr-str result)))))

(deftest instrumented-citable-hashes-rejects-invalid-index-test
  (testing "install!/instrument! is active: the m/=> input contract bites"
    (is (thrown? Exception
                 (parser-evidence/citable-hashes {:entries [{:not-valid true}]}))))
  (testing "and a valid index still returns its citable hashes"
    (is (= [(:sha256 valid-entry)]
           (parser-evidence/citable-hashes {:entries [valid-entry]})))))
