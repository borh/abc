(ns abc.tools.adr-test
  (:require [abc.tools.adr :as adr]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir []
  (.toFile (Files/createTempDirectory
            "abc-adr-test" (make-array FileAttribute 0))))

(defn- write-adr! [dir filename body]
  (let [file (io/file dir filename)]
    (spit file body)
    file))

(defn- write-path! [dir path body]
  (let [file (io/file dir path)]
    (.mkdirs (.getParentFile file))
    (spit file body)
    file))

(defn- kinds [problems]
  (set (map :kind problems)))

(defn- accepted-body [num title extra-fields]
  (str "# ADR " (format "%04d" num) ": " title "\n\n"
       "Status: Accepted\nDate: 2026-07-10\nAccepted: 2026-07-10\n"
       extra-fields
       "\n## Decision\n\nDecision.\n\n"
       "## Implementation Status\n\nImplemented.\n\n"
       "## Acceptance Criteria\n\n- `test/evidence.clj`.\n"))

(deftest parses-exact-header-relations-sections-and-evidence
  (let [dir (temp-dir)]
    (write-adr! dir "0002-next.md"
                (str "# ADR 0002: Next\n\n"
                     "Status: Accepted\n"
                     "Date: 2026-07-10\n"
                     "Accepted: 2026-07-10\n"
                     "Supersedes: ADR 0001 [scope: old rule]\n"
                     "Depends on: ADR 0001 [scope: fixture contract]\n"
                     "Source: `docs/spec.md`\n\n"
                     "## Decision\n\nUse the next rule.\n\n"
                     "## Implementation Status\n\nImplemented.\n\n"
                     "## Acceptance Criteria\n\n"
                     "- Covered by `test/abc/tools/adr_test.clj`.\n"))
    (let [parsed (adr/parse-adr (.getPath dir) "0002-next.md")]
      (is (= 2 (:num parsed)))
      (is (= "Accepted" (:status parsed)))
      (is (= [{:target 1 :scope "old rule"}]
             (get-in parsed [:relations :supersedes])))
      (is (= #{{:path "test/abc/tools/adr_test.clj"
                :section "Acceptance Criteria"
                :criterion-index 0}}
             (set (:evidence parsed))))
      (is (= #{"Decision" "Implementation Status" "Acceptance Criteria"}
             (:sections parsed)))
      (is (empty? (:parse-problems parsed))))))

(deftest preserves-decorated-status-and-rejects-continuations
  (let [dir (temp-dir)]
    (write-adr! dir "0028-bad.md"
                (str "# ADR 0028: Bad\n\n"
                     "Status: Proposed (implemented)\n"
                     "continued status prose\n"
                     "Date: 2026-07-10\n\n"
                     "## Decision\n\nProposed.\n"))
    (let [parsed (adr/parse-adr (.getPath dir) "0028-bad.md")]
      (is (= "Proposed (implemented)" (:status parsed)))
      (is (some #(= :invalid-header-line (:kind %))
                (:parse-problems parsed))))))

(deftest rejects-unknown-duplicate-empty-and-multiline-header-fields
  (let [dir (temp-dir)]
    (write-adr! dir "0001-bad.md"
                (str "# ADR 0001: Bad\n\n"
                     "Status: Draft\n"
                     "Staus: Accepted\n"
                     "Status: Proposed\n"
                     "Source:\n"
                     "wrapped source\n\n"
                     "## Decision\n\nBad.\n"))
    (let [kinds (set (map :kind (:parse-problems
                                 (adr/parse-adr (.getPath dir) "0001-bad.md"))))]
      (is (contains? kinds :unknown-header-field))
      (is (contains? kinds :duplicate-header-field))
      (is (contains? kinds :empty-header-value))
      (is (contains? kinds :invalid-header-line)))))

(deftest relation-grammar-is-closed
  (let [dir (temp-dir)]
    (write-adr! dir "0024-bad.md"
                (str "# ADR 0024: Bad relation\n\n"
                     "Status: Accepted\nDate: 2026-07-10\nAccepted: 2026-07-10\n"
                     "Depends on: ADR 0002 (old scope), `docs/probe.md`\n\n"
                     "## Decision\n\nX.\n\n"
                     "## Implementation Status\n\nX.\n\n"
                     "## Acceptance Criteria\n\n- `test/abc/tools/adr_test.clj`.\n"))
    (is (some #(= :invalid-relation-item (:kind %))
              (:parse-problems
               (adr/parse-adr (.getPath dir) "0024-bad.md"))))))

(deftest malformed-reference-width-never-normalizes
  (let [dir (temp-dir)]
    (write-adr! dir "0001-bad.md"
                "# ADR 0001: Bad\n\nStatus: Draft\nDate: 2026-07-10\nDepends on: ADR 6\n\n## Decision\n\nX.\n")
    (is (some #(= :invalid-relation-item (:kind %))
              (:parse-problems (adr/parse-adr (.getPath dir) "0001-bad.md"))))))

(deftest filename-title-number-mismatch-is-preserved
  (let [dir (temp-dir)]
    (write-adr! dir "0001-mismatch.md"
                "# ADR 0002: Mismatch\n\nStatus: Draft\nDate: 2026-07-10\n\n## Decision\n\nX.\n")
    (is (some #(= :filename-title-mismatch (:kind %))
              (:parse-problems
               (adr/parse-adr (.getPath dir) "0001-mismatch.md"))))))

(deftest supersedes-none-is-the-only-empty-relation-sentinel
  (let [dir (temp-dir)]
    (write-adr! dir "0001-none.md"
                "# ADR 0001: None\n\nStatus: Draft\nDate: 2026-07-10\nSupersedes: none\n\n## Decision\n\nX.\n")
    (is (= [] (get-in (adr/parse-adr (.getPath dir) "0001-none.md")
                      [:relations :supersedes])))))

(deftest missing-required-blank-line-preserves-first-header-field
  (let [dir (temp-dir)]
    (write-adr! dir "0001-no-blank.md"
                "# ADR 0001: No blank\nStatus: Draft\nDate: 2026-07-10\n\n## Decision\n\nX.\n")
    (let [parsed (adr/parse-adr (.getPath dir) "0001-no-blank.md")]
      (is (= "Draft" (:status parsed)))
      (is (some #(= :missing-required-blank-line (:kind %))
                (:parse-problems parsed))))))

(deftest end-of-file-is-not-a-required-blank-line
  (let [dir (temp-dir)]
    (write-adr! dir "0001-title-only.md" "# ADR 0001: Title only")
    (is (some #(= :missing-required-blank-line (:kind %))
              (:parse-problems
               (adr/parse-adr (.getPath dir) "0001-title-only.md"))))))

(deftest duplicate-field-tracks-an-empty-first-occurrence
  (let [dir (temp-dir)]
    (write-adr! dir "0001-duplicate-empty.md"
                "# ADR 0001: Duplicate empty\n\nStatus: Draft\nSource:\nSource: docs/spec.md\n\n## Decision\n\nX.\n")
    (let [parsed (adr/parse-adr (.getPath dir) "0001-duplicate-empty.md")
          kinds (set (map :kind (:parse-problems parsed)))]
      (is (nil? (get-in parsed [:fields "Source"])))
      (is (contains? kinds :empty-header-value))
      (is (contains? kinds :duplicate-header-field)))))

(deftest adr-files-excludes-directories
  (let [dir (temp-dir)]
    (.mkdir (io/file dir "0001-directory.md"))
    (write-adr! dir "0002-file.md" "")
    (is (= ["0002-file.md"] (adr/adr-files (.getPath dir))))))

(deftest status-must-be-an-exact-vocabulary-value
  (let [dir (temp-dir)]
    (write-adr! dir "0001-bad.md"
                (str "# ADR 0001: Bad\n\n"
                     "Status: Accepted now\nDate: 2026-07-10\n\n"
                     "## Decision\n\nX.\n"))
    (let [problems (adr/validate-adrs (adr/parse-all (.getPath dir)) dir)]
      (is (= #{:invalid-status} (kinds problems))))))

(deftest lifecycle-dates-are-calendar-valid-and-status-specific
  (let [dir (temp-dir)]
    (write-adr! dir "0001-bad-date.md"
                "# ADR 0001: Bad date\n\nStatus: Draft\nDate: 2026-02-30\nAccepted: 2026-02-28\n\n## Decision\n\nX.\n")
    (write-adr! dir "0002-before.md"
                (str "# ADR 0002: Before\n\nStatus: Accepted\nDate: 2026-07-10\n"
                     "Accepted: 2026-07-09\n\n## Decision\n\nX.\n\n"
                     "## Implementation Status\n\nDone.\n\n"
                     "## Acceptance Criteria\n\n- `test/evidence.clj`.\n"))
    (let [problems (adr/validate-adrs (adr/parse-all (.getPath dir)) dir)]
      (is (contains? (kinds problems) :invalid-date))
      (is (contains? (kinds problems) :forbidden-accepted-date))
      (is (contains? (kinds problems) :accepted-before-date)))))

(deftest any-present-accepted-date-must-be-calendar-valid
  (let [dir (temp-dir)]
    (write-adr! dir "0001-draft.md"
                (str "# ADR 0001: Draft\n\nStatus: Draft\nDate: 2026-07-10\n"
                     "Accepted: 2026-02-30\n\n## Decision\n\nX.\n"))
    (let [problem-kinds (kinds (adr/validate-adrs
                                (adr/parse-all (.getPath dir)) dir))]
      (is (contains? problem-kinds :invalid-date))
      (is (contains? problem-kinds :forbidden-accepted-date)))))

(deftest accepted-status-requires-date-and-sections
  (let [dir (temp-dir)]
    (write-adr! dir "0001-bad.md"
                (str "# ADR 0001: Bad\n\n"
                     "Status: Accepted\nDate: 2026-07-10\n\n"
                     "## Decision\n\nX.\n"))
    (let [problems (adr/validate-adrs (adr/parse-all (.getPath dir)) dir)]
      (is (contains? (kinds problems) :missing-accepted-date))
      (is (contains? (kinds problems) :missing-implementation-status))
      (is (contains? (kinds problems) :missing-acceptance-criteria)))))

(deftest draft-criteria-need-no-existing-evidence
  (let [dir (temp-dir)]
    (write-adr! dir "0001-draft.md"
                (str "# ADR 0001: Draft\n\nStatus: Draft\nDate: 2026-07-10\n\n"
                     "## Decision\n\nFuture.\n\n"
                     "## Acceptance Criteria\n\n- Future system exists.\n"))
    (is (empty? (adr/validate-adrs (adr/parse-all (.getPath dir)) dir)))))

(deftest accepted-evidence-must-exist
  (let [dir (temp-dir)]
    (write-adr! dir "0001-accepted.md"
                (str "# ADR 0001: Accepted\n\nStatus: Accepted\n"
                     "Date: 2026-07-10\nAccepted: 2026-07-10\n\n"
                     "## Decision\n\nX.\n\n## Implementation Status\n\nDone.\n\n"
                     "## Acceptance Criteria\n\n- `test/missing_test.clj`.\n"))
    (let [problems (adr/validate-adrs (adr/parse-all (.getPath dir)) dir)]
      (is (contains? (kinds problems) :missing-evidence-path)))))

(deftest accepted-evidence-requires-a-path-and-directory-companion
  (let [dir (temp-dir)]
    (write-path! dir "fixtures/corpus/input.txt" "input")
    (write-path! dir "test/evidence.clj" "(ns evidence)")
    (write-adr! dir "0001-none.md"
                (str "# ADR 0001: None\n\nStatus: Accepted\nDate: 2026-07-10\n"
                     "Accepted: 2026-07-10\n\n## Decision\n\nX.\n\n"
                     "## Implementation Status\n\nDone.\n\n"
                     "## Acceptance Criteria\n\n- prose only.\n"))
    (write-adr! dir "0002-dir.md"
                (str "# ADR 0002: Directory\n\nStatus: Accepted\nDate: 2026-07-10\n"
                     "Accepted: 2026-07-10\n\n## Decision\n\nX.\n\n"
                     "## Implementation Status\n\nDone.\n\n"
                     "## Acceptance Criteria\n\n- `fixtures/corpus/`.\n"
                     "- Separate `test/evidence.clj`.\n"))
    (let [problems (adr/validate-adrs (adr/parse-all (.getPath dir)) dir)]
      (is (contains? (kinds problems) :missing-evidence))
      (is (contains? (kinds problems) :unverified-evidence-directory)))))

(deftest duplicate-numbers-relations-and-missing-targets-are-rejected
  (let [dir (temp-dir)]
    (write-adr! dir "0001-first.md"
                "# ADR 0001: First\n\nStatus: Draft\nDate: 2026-07-10\nDepends on: ADR 0099, ADR 0099\n\n## Decision\n\nX.\n")
    (write-adr! dir "0002-second.md"
                "# ADR 0001: Second\n\nStatus: Draft\nDate: 2026-07-10\n\n## Decision\n\nX.\n")
    (let [problems (adr/validate-adrs (adr/parse-all (.getPath dir)) dir)]
      (is (contains? (kinds problems) :duplicate-number))
      (is (contains? (kinds problems) :duplicate-relation))
      (is (contains? (kinds problems) :missing-relation-target)))))

(deftest accepted-to-draft-requires-scope
  (let [dir (temp-dir)]
    (write-path! dir "test/evidence.clj" "(ns evidence)")
    (write-adr! dir "0001-draft.md"
                "# ADR 0001: Draft\n\nStatus: Draft\nDate: 2026-07-10\n\n## Decision\n\nX.\n")
    (write-adr! dir "0002-accepted.md"
                (accepted-body 2 "Accepted" "Depends on: ADR 0001\n"))
    (is (contains? (kinds (adr/validate-adrs
                           (adr/parse-all (.getPath dir)) dir))
                   :unscoped-nonaccepted-dependency))))

(deftest accepted-cannot-depend-on-inactive-decisions
  (let [dir (temp-dir)]
    (write-path! dir "test/evidence.clj" "(ns evidence)")
    (write-adr! dir "0001-withdrawn.md"
                "# ADR 0001: Withdrawn\n\nStatus: Withdrawn\nDate: 2026-07-10\n\n## Decision\n\nX.\n")
    (write-adr! dir "0002-accepted.md"
                (accepted-body 2 "Accepted"
                               "Depends on: ADR 0001 [scope: old contract]\n"))
    (is (contains? (kinds (adr/validate-adrs
                           (adr/parse-all (.getPath dir)) dir))
                   :inactive-dependency))))

(deftest supersession-reciprocity-and-status-are-distinct
  (let [dir (temp-dir)]
    (write-path! dir "test/evidence.clj" "(ns evidence)")
    (write-adr! dir "0001-base.md"
                (accepted-body
                 1 "Base"
                 "Superseded by: ADR 0002 [scope: old rule]\n"))
    (write-adr! dir "0002-next.md"
                (accepted-body
                 2 "Next"
                 "Supersedes: ADR 0001 [scope: old rule]\n"))
    (let [adrs (adr/parse-all (.getPath dir))]
      (is (empty? (adr/validate-adrs adrs dir)))
      (is (contains?
           (kinds (adr/validate-adrs
                   (update-in adrs [0 :relations]
                              dissoc :superseded-by)
                   dir))
           :missing-superseded-by))
      (is (contains?
           (kinds (adr/validate-adrs
                   (-> adrs
                       (assoc-in [0 :relations :superseded-by 0 :scope] nil)
                       (assoc-in [1 :relations :supersedes 0 :scope] nil))
                   dir))
           :unscoped-supersession-target-not-superseded)))
    (write-adr! dir "0003-orphan.md"
                "# ADR 0003: Orphan\n\nStatus: Superseded\nDate: 2026-07-10\n\n## Decision\n\nOld.\n")
    (is (contains?
         (kinds (adr/validate-adrs (adr/parse-all (.getPath dir)) dir))
         :superseded-without-successor))))

(deftest reciprocity-requires-the-same-scope-on-both-sides
  (let [dir (temp-dir)]
    (write-path! dir "test/evidence.clj" "(ns evidence)")
    (write-adr! dir "0001-base.md"
                (accepted-body
                 1 "Base"
                 "Superseded by: ADR 0002 [scope: replacement rule]\n"))
    (write-adr! dir "0002-next.md"
                (accepted-body
                 2 "Next"
                 "Supersedes: ADR 0001 [scope: old rule]\n"))
    (let [problem-kinds (kinds (adr/validate-adrs
                                (adr/parse-all (.getPath dir)) dir))]
      (is (contains? problem-kinds :missing-superseded-by)))))

(deftest proposed-amendment-is-pending-and-reciprocal
  (let [dir (temp-dir)]
    (write-path! dir "test/evidence.clj" "(ns evidence)")
    (write-adr! dir "0001-base.md"
                (accepted-body 1 "Base" "Amended by: ADR 0002\n"))
    (write-adr! dir "0002-pending.md"
                (str "# ADR 0002: Pending\n\nStatus: Proposed\nDate: 2026-07-10\n"
                     "Amends: ADR 0001\n\n## Decision\n\nProposed amendment.\n"))
    (let [adrs (adr/parse-all (.getPath dir))]
      (is (empty? (adr/validate-adrs adrs dir)))
      (is (contains?
           (kinds (adr/validate-adrs
                   (update-in adrs [0 :relations] dissoc :amended-by)
                   dir))
           :missing-amended-by)))))

(deftest current-repository-satisfies-governance
  (is (= [] (adr/validate-repository "."))))
