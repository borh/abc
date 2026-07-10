(ns abc.tools.adr-test
  (:require [abc.tools.adr :as adr]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]])
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
