(ns abc.tools.parser-evidence-test
  (:require
   [abc.tools.hash :as hash]
   [abc.tools.parser-evidence :as parser-evidence]
   [abc.test-fs :refer [with-temp-dir]]
   [babashka.fs :as fs]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(deftest adr-0038-grants-development-release-authority-test
  (let [text (slurp
              "docs/adr/0038-custom-parser-ownership-and-neutral-comparison.md")]
    (is (str/includes? text "Release authority: development"))))

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

(deftest neutral-comparison-reports-are-registered-with-study-contract-test
  (testing "the generated neutral-comparison reports register, content-address,
            and bind to their frozen study-contract hash"
    (let [root (fs/canonicalize "..")
          study-contract "sha256:8715c30f69250dbd254d38218c05a433165e86d30a9c1e42fc169f5909500fbe"
          ids #{"ab-validator/aozora-parser-neutral-comparison-result-2026-07"
                "ab-validator/aozora-parser-neutral-comparison-report-2026-07"}
          rows (->> (:entries (parser-evidence/load-index))
                    (filter #(contains? ids (:evidence_id %))))]
      (testing "the committed index validates as a whole"
        (is (= :ok (parser-evidence/validate-index!
                    (parser-evidence/load-index)))))
      (testing "both reports are present and typed neutral-comparison"
        (is (= 2 (count rows)))
        (is (every? #(= :neutral-comparison (:evidence_class %)) rows)))
      (testing "each carries the frozen study-contract hash"
        (is (every? #(= study-contract (:study_contract %)) rows)))
      (testing "each report file content-addresses to its committed hash"
        (is (empty? (parser-evidence/citation-file-problems root {:entries rows}))))
      (testing "neutral-comparison is not an admission/release evidence class"
        (is (every? #(not (contains? #{:conversion-compatibility
                                       :parser-selection
                                       :comparator-oracle}
                                     (:evidence_class %)))
                    rows))))))

(deftest min-1-study-contract-is-coupled-to-evidence-class-test
  (testing "MIN-1: an admission-class (:conversion-compatibility) entry must not
            carry :study_contract"
    (is (empty? (parser-evidence/index-errors {:entries [valid-entry]})))
    (is (has-error? #":conversion-compatibility evidence must not carry a :study_contract"
                    (parser-evidence/index-errors
                     {:entries [(assoc valid-entry
                                       :study_contract
                                       "sha256:8715c30f69250dbd254d38218c05a433165e86d30a9c1e42fc169f5909500fbe")]}))))
  (testing "MIN-1: a :neutral-comparison entry must carry :study_contract"
    (let [neutral (assoc valid-entry
                         :evidence_id "ab-validator/neutral"
                         :evidence_class :neutral-comparison
                         :logical_path "ab-validator/docs/neutral.md"
                         :study_contract "sha256:8715c30f69250dbd254d38218c05a433165e86d30a9c1e42fc169f5909500fbe")]
      (is (empty? (parser-evidence/index-errors {:entries [neutral]})))
      (is (has-error? #":neutral-comparison evidence must carry a :study_contract"
                      (parser-evidence/index-errors
                       {:entries [(dissoc neutral :study_contract)]})))))
  (testing "MIN-1 leaves the unconstrained classes (parser-selection,
            comparator-oracle) free to omit :study_contract"
    (is (empty? (parser-evidence/index-errors
                 {:entries [(assoc valid-entry :evidence_class :parser-selection
                                   :status :provisional)]})))
    (is (empty? (parser-evidence/index-errors
                 {:entries [(assoc valid-entry :evidence_class :comparator-oracle)]})))))

(deftest comparison-citations-cannot-satisfy-admission-or-release-test
  (testing "the structural allowlist boundary excludes both comparison classes"
    (is (= #{:parser-selection :neutral-comparison}
           parser-evidence/comparison-evidence-classes))
    (is (empty? (set/intersection
                 parser-evidence/comparison-evidence-classes
                 parser-evidence/admission-evidence-classes)))
    (is (empty? (set/intersection
                 parser-evidence/comparison-evidence-classes
                 parser-evidence/release-evidence-classes))))
  (let [entries (:entries (parser-evidence/load-index))
        selection-rows (filter #(= :parser-selection (:evidence_class %)) entries)
        neutral-rows (filter #(= :neutral-comparison (:evidence_class %)) entries)
        compat-rows (filter #(= :conversion-compatibility (:evidence_class %)) entries)]
    (testing "the committed index actually contains rows of each kind"
      (is (seq selection-rows))
      (is (seq neutral-rows))
      (is (seq compat-rows)))
    (testing "every historical :parser-selection row is rejected for admission
              AND release, keyed off evidence class not absence of fields"
      (doseq [row selection-rows]
        (is (false? (parser-evidence/entry-admissible? row)) (:evidence_id row))
        (is (false? (parser-evidence/entry-release-qualifying? row)) (:evidence_id row))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"cannot support an admission claim"
                              (parser-evidence/assert-admission-evidence! row)))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"cannot support a release claim"
                              (parser-evidence/assert-release-evidence! row)))))
    (testing "every new :neutral-comparison row is rejected for admission AND
              release even though it is :citable and carries a study contract"
      (doseq [row neutral-rows]
        (is (= :citable (:status row)) (:evidence_id row))
        (is (contains? row :study_contract) (:evidence_id row))
        (is (false? (parser-evidence/entry-admissible? row)) (:evidence_id row))
        (is (false? (parser-evidence/entry-release-qualifying? row)) (:evidence_id row))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"cannot support an admission claim"
                              (parser-evidence/assert-admission-evidence! row)))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"cannot support a release claim"
                              (parser-evidence/assert-release-evidence! row)))))
    (testing "an admission-class :conversion-compatibility row still passes the
              admission boundary, proving the allowlist is not vacuous"
      (is (every? parser-evidence/entry-admissible? compat-rows))
      (doseq [row compat-rows]
        (is (identical? row (parser-evidence/assert-admission-evidence! row)))))))

(deftest parser-evidence-index-validation-test
  (testing "accepts the committed evidence index"
    (is (= :ok (parser-evidence/validate-index!
                (parser-evidence/load-index)))))
  (testing "rejects missing required keys"
    (is (has-error? #"entry 0 is missing :sha256"
                    (parser-evidence/index-errors
                     {:entries [(dissoc valid-entry :sha256)]}))))
  (testing "rejects an empty :entries vector"
    (is (has-error? #"Parser evidence index :entries must not be empty"
                    (parser-evidence/index-errors {:entries []}))))
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
           (#'parser-evidence/citable-hashes-of index)))))

(deftest parser-evidence-nullable-external-path-test
  (testing "current_external_path may be absent or nil but not blank"
    (is (empty? (parser-evidence/index-errors
                 {:entries [(dissoc valid-entry :current_external_path)]})))
    (is (empty? (parser-evidence/index-errors
                 {:entries [(assoc valid-entry :current_external_path nil)]})))
    (is (has-error? #":current_external_path must be null or a non-empty string"
                    (parser-evidence/index-errors
                     {:entries [(assoc valid-entry :current_external_path "")]})))))

(deftest parser-evidence-mutation-rejected-test
  (testing "mutating a valid entry's :sha256 is rejected by the validator"
    (doseq [entry [valid-entry
                   (assoc valid-entry
                          :evidence_id "ab-validator/neutral"
                          :evidence_class :neutral-comparison
                          :logical_path "ab-validator/docs/neutral.md"
                          :study_contract (:sha256 valid-entry))]]
      (is (empty? (parser-evidence/index-errors {:entries [entry]})))
      (is (has-error? #":sha256 must be a sha256 hash"
                      (parser-evidence/index-errors
                       {:entries [(assoc entry :sha256 "sha256:not-real")]}))))))

(deftest citable-hashes-loads-and-validates-the-committed-index-test
  (is (= (#'parser-evidence/citable-hashes-of (parser-evidence/load-index))
         (parser-evidence/citable-hashes))))
