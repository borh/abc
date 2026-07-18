(ns abc.tools.adr-test
  (:require [abc.tools.adr :as adr]
            [abc.tools.adr-evidence-runtime-inputs :as runtime]
            [abc.tools.evidence-io :as evidence-io]
            [babashka.fs :as fs]
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

(defn- problem-kind [thunk]
  (try (thunk) nil (catch Exception exception (:kind (ex-data exception)))))

(defn- accepted-body [num title extra-fields]
  (str "# ADR " (format "%04d" num) ": " title "\n\n"
       "Status: Accepted\nDate: 2026-07-10\nAccepted: 2026-07-10\n"
       "Validation scope: structural\nRelease authority: none\n"
       extra-fields
       "\n## Decision\n\nDecision.\n\n"
       "## Implementation Status\n\nImplemented.\n\n"
       "## Acceptance Criteria\n\n- **ADR-" (format "%04d" num)
       "-C1 — structural-invariant:** `test/evidence.clj`.\n"))

(defn- claim-body [num status criteria]
  (str "# ADR " (format "%04d" num) ": Claims\n\n"
       "Status: " status "\nDate: 2026-07-10\n\n"
       "## Decision\n\nClaims.\n\n"
       "## Acceptance Criteria\n\n" criteria "\n"))

(deftest parses-exact-multiline-claim-header
  (let [dir (temp-dir)]
    (write-adr! dir "0042-claims.md"
                (claim-body
                 42 "Proposed"
                 (str "- **ADR-0042-C1 — structural-invariant:** A multiline claim whose continuation\n"
                      "  remains part of the same criterion.")))
    (let [parsed (adr/parse-adr (.getPath dir) "0042-claims.md")]
      (is (= [{:criterion-index 0
               :body (str "**ADR-0042-C1 — structural-invariant:** A multiline claim whose continuation\n"
                          "remains part of the same criterion.")
               :claim-id "ADR-0042-C1"
               :claim-kind :structural-invariant}]
             (:criteria parsed)))
      (is (empty? (:claim-problems parsed))))))

(deftest typed-claim-headers-are-forbidden-only-in-exact-non-acceptance-sections
  (let [dir (temp-dir)]
    (write-adr! dir "0042-claims.md"
                (str (claim-body 42 "Proposed"
                                 "- **ADR-0042-C3 — fixture-behavior:** valid criterion.")
                     "\n## Historical Evidence\n\n"
                     "**ADR-0042-C1 — structural-invariant:** historical coordinate.\n\n"
                     "## Future Verification\n\n"
                     "**ADR-0042-C2 — operational-behavior:** future guard.\n\n"
                     "## Notes\n\n"
                     "**Ordinary bold prose:** remains ordinary prose.\n"))
    (let [problems (:claim-problems (adr/parse-adr dir "0042-claims.md"))
          outside (filter #(= :claim-header-outside-acceptance (:kind %)) problems)]
      (is (= #{{:kind :claim-header-outside-acceptance
                :file "0042-claims.md"
                :message "typed claim headers are allowed only in Acceptance Criteria"
                :section "Historical Evidence"}
               {:kind :claim-header-outside-acceptance
                :file "0042-claims.md"
                :message "typed claim headers are allowed only in Acceptance Criteria"
                :section "Future Verification"}}
             (set outside)))
      (is (= 2 (count problems))))))

(deftest duplicate-target-sections-preserve-earlier-claim-headers
  (let [dir (temp-dir)]
    (write-adr! dir "0042-duplicate-targets.md"
                (str (claim-body 42 "Proposed" "- Plain criterion.")
                     "\n## Historical Evidence\n\n"
                     "**ADR-0042-C1 — structural-invariant:** first historical occurrence.\n\n"
                     "## Historical Evidence\n\nLater plain history.\n\n"
                     "## Future Verification\n\n"
                     "**ADR-0042-C2 — operational-behavior:** first future occurrence.\n\n"
                     "## Future Verification\n\nLater plain verification.\n"))
    (let [parsed (adr/parse-adr dir "0042-duplicate-targets.md")
          outside (filter #(= :claim-header-outside-acceptance (:kind %))
                          (:claim-problems parsed))]
      (is (= ["Historical Evidence" "Future Verification"]
             (mapv :section outside)))
      (is (empty? (:parse-problems parsed)))
      (is (every? string? (vals (:section-bodies parsed)))))))

(deftest duplicate-ordinary-sections-remain-legacy-valid
  (let [dir (temp-dir)]
    (write-adr! dir "0042-duplicate-ordinary.md"
                (str (claim-body 42 "Proposed" "- Plain criterion.")
                     "\n## Notes\n\nFirst note.\n\n"
                     "## Notes\n\nSecond note.\n\n"
                     "## Consequences\n\nFirst consequence.\n\n"
                     "## Consequences\n\nSecond consequence.\n"))
    (is (empty? (adr/validate-adrs-legacy
                 (adr/parse-all dir) dir)))))

(deftest markdown-list-prefixed-claim-headers-are-linted-in-target-sections
  (let [dir (temp-dir)]
    (write-adr! dir "0042-list-claims.md"
                (str (claim-body 42 "Proposed" "- Plain criterion.")
                     "\n## Historical Evidence\n\n"
                     "- **ADR-0042-C1 — structural-invariant:** dash.\n"
                     "* **ADR-0042-C2 — structural-invariant:** star.\n"
                     "+ **ADR-0042-C3 — structural-invariant:** plus.\n"
                     "1. **ADR-0042-C4 — structural-invariant:** ordered.\n"
                     "Ordinary **ADR-0042-C5 — structural-invariant:** prose is not a header.\n"))
    (let [problems (->> (adr/parse-adr dir "0042-list-claims.md")
                        :claim-problems
                        (filter #(= :claim-header-outside-acceptance (:kind %))))]
      (is (= 4 (count problems)))
      (is (every? #(= "Historical Evidence" (:section %)) problems)))))

(deftest adr-read-adapters-preserve-values-and-are-traced-default-deny
  (let [dir (temp-dir)
        first-body (claim-body 42 "Proposed" "- First criterion.")
        second-body (claim-body 43 "Proposed" "- Second criterion.")]
    (write-adr! dir "0043-second.md" second-body)
    (write-adr! dir "0042-first.md" first-body)
    (write-adr! dir "README.md" "ignored")
    (let [expected-files ["0042-first.md" "0043-second.md"]
          expected-adr (adr/parse-adr dir "0042-first.md")
          traced (evidence-io/with-read-trace
                   {:identity-root dir :cwd-root dir}
                   #(vector (adr/adr-files dir)
                            (adr/parse-adr dir "0042-first.md")))]
      (is (= [expected-files expected-adr] (:value traced)))
      (is (= ["0042-first.md" "0043-second.md" "README.md"]
             (:repository-paths traced))))
    (let [analyzer-root (fs/file (fs/create-temp-dir {:prefix "adr-direct-read-"}))]
      (write-path! analyzer-root "src/example/core.clj"
                   (str "(ns example.core)\n"
                        "(defn direct-list [dir] (.listFiles (java.io.File. dir)))\n"
                        "(defn direct-read [path] (slurp path))\n"))
      (write-path! analyzer-root "test/.keep" "")
      (is (= :forbidden-evidence-io
             (problem-kind #(runtime/analyze-reachable-vars
                             analyzer-root ['example.core/direct-list]))))
      (is (= :forbidden-evidence-io
             (problem-kind #(runtime/analyze-reachable-vars
                             analyzer-root ['example.core/direct-read])))))))

(deftest claim-header-problems-are-specific-and-proposed-aware
  (let [dir (temp-dir)]
    (doseq [[filename num status criterion]
            [["0042-missing.md" 42 "Accepted"
              "- ADR-0042-C1 — structural-invariant: missing bold prefix"]
             ["0043-short.md" 43 "Accepted"
              "- **ADR-43-C1 — structural-invariant:** short ADR number"]
             ["0044-mismatch.md" 44 "Accepted"
              "- **ADR-0041-C1 — structural-invariant:** wrong ADR"]
             ["0045-zero.md" 45 "Accepted"
              "- **ADR-0045-C0 — structural-invariant:** zero criterion"]
             ["0046-unknown.md" 46 "Accepted"
              "- **ADR-0046-C1 — unknown-kind:** unknown kind"]
             ["0047-proposed-plain.md" 47 "Proposed"
              "- Future criterion without a header."]
             ["0048-proposed-malformed.md" 48 "Proposed"
              "- **ADR-48-C1 — structural-invariant:** malformed proposal"]]]
      (write-adr! dir filename (claim-body num status criterion)))
    (is (= #{:missing-claim-header}
           (kinds (:claim-problems
                   (adr/parse-adr dir "0042-missing.md")))))
    (is (= #{:malformed-claim-header}
           (kinds (:claim-problems
                   (adr/parse-adr dir "0043-short.md")))))
    (is (= #{:claim-adr-mismatch}
           (kinds (:claim-problems
                   (adr/parse-adr dir "0044-mismatch.md")))))
    (is (= #{:malformed-claim-header}
           (kinds (:claim-problems
                   (adr/parse-adr dir "0045-zero.md")))))
    (is (= #{:unknown-claim-kind}
           (kinds (:claim-problems
                   (adr/parse-adr dir "0046-unknown.md")))))
    (is (empty? (:claim-problems
                 (adr/parse-adr dir "0047-proposed-plain.md"))))
    (is (= #{:malformed-claim-header}
           (kinds (:claim-problems
                   (adr/parse-adr dir "0048-proposed-malformed.md")))))))

(deftest duplicate-claim-ids-are-repository-wide-problems
  (let [dir (temp-dir)]
    (write-adr! dir "0042-duplicate.md"
                (claim-body
                 42 "Proposed"
                 (str "- **ADR-0042-C1 — structural-invariant:** first.\n"
                      "- **ADR-0042-C1 — fixture-behavior:** second.")))
    (is (contains? (kinds (adr/validate-adrs (adr/parse-all dir) dir))
                   :duplicate-claim-id))))

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

(deftest repository-discovery-fails-closed
  (let [repo (temp-dir)
        missing (adr/validate-repository repo "docs/adr")]
    (is (contains? (kinds missing) :missing-adr-directory))
    (write-path! repo "docs/adr" "not a directory")
    (is (contains? (kinds (adr/validate-repository repo "docs/adr"))
                   :invalid-adr-directory))))

(deftest repository-rejects-empty-and-malformed-adr-corpora
  (let [repo (temp-dir)
        adr-dir (io/file repo "docs/adr")]
    (.mkdirs adr-dir)
    (write-adr! adr-dir "README.md" "# ADR index\n")
    (is (contains? (kinds (adr/validate-repository repo "docs/adr"))
                   :empty-adr-corpus))
    (write-adr! adr-dir "bad-name.md"
                "# ADR 0001: Bad name\n\nStatus: Draft\nDate: 2026-07-10\n\n## Decision\n\nX.\n")
    (is (contains? (kinds (adr/validate-repository repo "docs/adr"))
                   :invalid-adr-filename))))

(deftest header-fields-require-exact-colon-space-syntax
  (let [dir (temp-dir)]
    (doseq [[filename line]
            [["0001-missing-space.md" "Status:Draft"]
             ["0002-leading-space.md" "Status:  Draft"]
             ["0003-tab.md" "Status:\tDraft"]
             ["0004-trailing-space.md" "Status: Draft "]]]
      (write-adr! dir filename
                  (str "# ADR " (subs filename 0 4) ": Invalid header\n\n"
                       line "\nDate: 2026-07-10\n\n## Decision\n\nX.\n")))
    (doseq [filename (adr/adr-files dir)]
      (is (contains? (kinds (:parse-problems (adr/parse-adr dir filename)))
                     :invalid-header-line)
          filename))))

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

(deftest accepted-status-requires-validation-scope-and-release-authority
  (let [dir (temp-dir)]
    (write-path! dir "test/evidence.clj" "(ns evidence)")
    (write-adr! dir "0001-missing.md"
                (str "# ADR 0001: Missing lifecycle dimensions\n\n"
                     "Status: Accepted\nDate: 2026-07-10\nAccepted: 2026-07-10\n\n"
                     "## Decision\n\nX.\n\n"
                     "## Implementation Status\n\nDone.\n\n"
                     "## Acceptance Criteria\n\n- `test/evidence.clj`.\n"))
    (let [problem-kinds (kinds (adr/validate-adrs
                                (adr/parse-all (.getPath dir)) dir))]
      (is (contains? problem-kinds :missing-validation-scope))
      (is (contains? problem-kinds :missing-release-authority)))))

(deftest lifecycle-dimensions-use-closed-vocabularies
  (let [dir (temp-dir)]
    (write-path! dir "test/evidence.clj" "(ns evidence)")
    (write-adr! dir "0001-invalid.md"
                (str "# ADR 0001: Invalid lifecycle dimensions\n\n"
                     "Status: Accepted\nDate: 2026-07-10\nAccepted: 2026-07-10\n"
                     "Validation scope: everything\nRelease authority: automatic\n\n"
                     "## Decision\n\nX.\n\n"
                     "## Implementation Status\n\nDone.\n\n"
                     "## Acceptance Criteria\n\n- `test/evidence.clj`.\n"))
    (let [problem-kinds (kinds (adr/validate-adrs
                                (adr/parse-all (.getPath dir)) dir))]
      (is (contains? problem-kinds :invalid-validation-scope))
      (is (contains? problem-kinds :invalid-release-authority)))))

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

(deftest evidence-paths-are-contained-in-canonical-allowed-roots
  (let [repo (temp-dir)
        adr-dir (io/file repo "docs/adr")]
    (.mkdirs adr-dir)
    (write-path! repo "test/evidence.clj" "(ns evidence)")
    (write-path! repo "fixtures/evidence.txt" "fixture")
    (write-path! repo "nix/evidence.nix" "{}")
    (write-adr! adr-dir "0001-contained.md"
                (str "# ADR 0001: Contained\n\nStatus: Accepted\nDate: 2026-07-10\n"
                     "Accepted: 2026-07-10\n"
                     "Validation scope: structural\nRelease authority: none\n\n"
                     "## Decision\n\nX.\n\n"
                     "## Implementation Status\n\nDone.\n\n## Acceptance Criteria\n\n"
                     "- **ADR-0001-C1 — structural-invariant:** `test/./evidence.clj`, `fixtures/evidence.txt`, and `nix/evidence.nix`.\n"))
    (is (empty? (adr/validate-repository repo "docs/adr")))))

(deftest evidence-rejects-lexical-traversal-and-malformed-paths
  (let [repo (temp-dir)
        adr-dir (io/file repo "docs/adr")]
    (.mkdirs adr-dir)
    (write-path! repo "outside.clj" "outside")
    (write-adr! adr-dir "0001-traversal.md"
                (str "# ADR 0001: Traversal\n\nStatus: Draft\nDate: 2026-07-10\n\n"
                     "## Decision\n\nX.\n\n## Acceptance Criteria\n\n"
                     "- `test/../outside.clj` and `test/\u0000bad.clj`.\n"))
    (let [problem-kinds (kinds (adr/validate-repository repo "docs/adr"))]
      (is (contains? problem-kinds :evidence-path-traversal))
      (is (contains? problem-kinds :malformed-evidence-path)))))

(deftest evidence-rejects-real-path-symlink-escape
  (let [repo (temp-dir)
        adr-dir (io/file repo "docs/adr")
        outside (temp-dir)
        outside-file (write-path! outside "escaped.clj" "outside")]
    (.mkdirs adr-dir)
    (.mkdirs (io/file repo "test"))
    (Files/createSymbolicLink (.toPath (io/file repo "test/escaped.clj"))
                              (.toPath outside-file)
                              (make-array FileAttribute 0))
    (write-adr! adr-dir "0001-symlink.md"
                (str "# ADR 0001: Symlink\n\nStatus: Draft\nDate: 2026-07-10\n\n"
                     "## Decision\n\nX.\n\n## Acceptance Criteria\n\n"
                     "- `test/escaped.clj`.\n"))
    (is (contains? (kinds (adr/validate-repository repo "docs/adr"))
                   :evidence-real-path-escape))))

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

(deftest accepted-to-nonaccepted-is-rejected-even-with-scope
  (let [dir (temp-dir)]
    (write-path! dir "test/evidence.clj" "(ns evidence)")
    (write-adr! dir "0001-draft.md"
                "# ADR 0001: Draft\n\nStatus: Draft\nDate: 2026-07-10\n\n## Decision\n\nX.\n")
    (write-adr! dir "0002-accepted.md"
                (accepted-body 2 "Accepted"
                               "Depends on: ADR 0001 [scope: provisional contract]\n"))
    (is (contains? (kinds (adr/validate-adrs
                           (adr/parse-all (.getPath dir)) dir))
                   :noncanonical-dependency-path))))

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
                   :noncanonical-dependency-path))))

(deftest accepted-dependency-closure-reports-transitive-path
  (let [dir (temp-dir)]
    (write-path! dir "test/evidence.clj" "(ns evidence)")
    (write-adr! dir "0001-root.md"
                (accepted-body 1 "Root" "Depends on: ADR 0002\n"))
    (write-adr! dir "0002-middle.md"
                (accepted-body 2 "Middle" "Depends on: ADR 0003\n"))
    (write-adr! dir "0003-proposed.md"
                (str "# ADR 0003: Proposed\n\nStatus: Proposed\n"
                     "Date: 2026-07-10\n\n## Decision\n\nFuture.\n"))
    (let [problem (->> (adr/validate-adrs (adr/parse-all (.getPath dir)) dir)
                       (filter #(and (= :noncanonical-dependency-path (:kind %))
                                     (= "0001-root.md" (:file %))))
                       first)]
      (is (= [1 2 3] (:path problem))))))

(deftest accepted-dependency-closure-terminates-on-cycles
  (let [dir (temp-dir)]
    (write-path! dir "test/evidence.clj" "(ns evidence)")
    (write-adr! dir "0001-first.md"
                (accepted-body 1 "First" "Depends on: ADR 0002\n"))
    (write-adr! dir "0002-second.md"
                (accepted-body 2 "Second" "Depends on: ADR 0001\n"))
    (is (not (contains? (kinds (adr/validate-adrs
                                (adr/parse-all (.getPath dir)) dir))
                        :noncanonical-dependency-path)))))

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

(deftest current-repository-audit-inventory-is-explicit
  (let [adrs (adr/parse-all "docs/adr")
        accepted (filter #(= "Accepted" (:status %)) adrs)
        accepted-criteria (mapcat :criteria accepted)
        missing-scope-count (count (filter #(nil? (:validation-scope %)) accepted))
        missing-authority-count (count (filter #(nil? (:release-authority %)) accepted))
        problems (adr/validate-repository ".")
        missing-claim-count (count (filter #(= :missing-claim-header (:kind %))
                                           problems))
        missing-evidence-count (count (filter #(= :missing-evidence (:kind %))
                                              problems))
        dependency-paths (->> problems
                              (filter #(= :noncanonical-dependency-path
                                          (:kind %)))
                              (map :path)
                              vec)]
    (is (= #{} (kinds problems)))
    (is (= 30 (count accepted)))
    (is (= 148 (count accepted-criteria)))
    (is (zero? missing-claim-count))
    (is (zero? missing-evidence-count))
    (is (= (+ missing-scope-count missing-authority-count
              missing-claim-count missing-evidence-count)
           (count problems)))
    (is (= [] dependency-paths))))

(deftest legacy-policy-keeps-audit-only-rules-nonblocking
  (let [dir (temp-dir)]
    (write-path! dir "test/evidence.clj" "(ns evidence)")
    (write-adr! dir "0001-proposed.md"
                (str "# ADR 0001: Proposed\n\nStatus: Proposed\n"
                     "Date: 2026-07-10\n\n## Decision\n\nFuture.\n"))
    (write-adr! dir "0002-accepted.md"
                (str "# ADR 0002: Accepted\n\n"
                     "Status: Accepted\nDate: 2026-07-10\nAccepted: 2026-07-10\n"
                     "Depends on: ADR 0001 [scope: provisional contract]\n\n"
                     "## Decision\n\nCurrent.\n\n"
                     "## Implementation Status\n\nImplemented.\n\n"
                     "## Acceptance Criteria\n\n- `test/evidence.clj`.\n"))
    (let [adrs (adr/parse-all (.getPath dir))
          strict-kinds (kinds (adr/validate-adrs adrs dir))]
      (is (contains? strict-kinds :missing-validation-scope))
      (is (contains? strict-kinds :missing-release-authority))
      (is (contains? strict-kinds :noncanonical-dependency-path))
      (is (empty? (adr/validate-adrs-legacy adrs dir))))))

(deftest legacy-policy-retains-pre-migration-dependency-safety
  (let [dir (temp-dir)]
    (write-path! dir "test/evidence.clj" "(ns evidence)")
    (write-adr! dir "0001-proposed.md"
                (str "# ADR 0001: Proposed\n\nStatus: Proposed\n"
                     "Date: 2026-07-10\n\n## Decision\n\nFuture.\n"))
    (write-adr! dir "0002-accepted.md"
                (accepted-body 2 "Accepted" "Depends on: ADR 0001\n"))
    (is (contains? (kinds (adr/validate-adrs-legacy
                           (adr/parse-all (.getPath dir)) dir))
                   :unscoped-nonaccepted-dependency))))
