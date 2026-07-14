(ns abc.tools.adr-0034-evidence-test
  (:require [abc.tools.adr :as adr]
            [abc.tools.adr-evidence :as evidence]
            [abc.tools.adr-evidence-bundle :as bundle]
            [abc.tools.adr-evidence-runtime-inputs :as runtime]
            [abc.tools.adr-governance :as governance]
            [abc.tools.evidence-io :as evidence-io]
            [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.json :as json]
            [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def matrix
  {:structural-invariant #{:structural-test}
   :fixture-behavior #{:fixture-conformance}
   :corpus-behavior #{:corpus-measurement}
   :performance-bound #{:benchmark}
   :external-semantics #{:external-authority}
   :implementation-agreement #{:cross-implementation}
   :domain-interpretation #{:expert-assessment :external-authority}
   :operational-behavior #{:operational-observation}})

(defn- kinds [problems]
  (set (map :kind problems)))

(defn- claim [kind]
  {:claim-id "ADR-0034-C1" :claim-kind kind
   :file "0034-synthetic.md" :criterion-index 0})

(defn- entry [claim-kind evidence-kind]
  {:claim-id "ADR-0034-C1" :claim-kind claim-kind
   :evidence-kind evidence-kind
   :artifact-path "docs/evidence/run.json"
   :artifact-hash (str "sha256:" (apply str (repeat 64 "0")))
   :observation-key "contract"
   :expected {:operator := :value true}})

(defn- registry-problems [claim-value entry-value artifact as-of]
  (with-redefs [bundle/validate-bundle
                (fn [& _] {:bundle artifact :problems []})]
    (evidence/validate-registry
     {:repo-root "." :claims [claim-value]
      :registry {:entries [entry-value]}
      :matrix matrix :as-of as-of})))

(defn- boundary-options [stem]
  (let [descriptor-path (str "abc/docs/evidence/adr-capture/" stem ".edn")]
    {:identity-root ".." :cwd-root "." :repo-root "." :workspace-root ".."
     :descriptor {:path descriptor-path
                  :value (files/read-edn
                          (str "docs/evidence/adr-capture/" stem ".edn"))}}))

(deftest adr-0034-c1-contract
  (runtime/with-validated-read-trace!
    (boundary-options "adr-0034-c1")
    (fn []
      (testing "the compatibility matrix and entry shape are closed"
        (is (= matrix (evidence/load-matrix)))
        (doseq [key [:observed :inputs :verdict]]
          (is (contains?
               (kinds (registry-problems
                       (claim :structural-invariant)
                       (assoc (entry :structural-invariant :structural-test) key true)
                       {"schema_version" "abc-adr-evidence-run-v1"
                        "observations" {"contract" {"value" true}}}
                       "2026-07-12"))
               :forbidden-inline-evidence-value))))
      (testing "artifact identity and current inputs are independently bound"
        (evidence-io/with-owned-ephemeral-root
          (fn [repo]
            (let [input (fs/file repo "input.txt")
                  artifact-path "docs/evidence/run.json"
                  artifact (fs/file repo artifact-path)]
              (files/write-text! input "one")
              (let [input-hash (hash/format-sha256 (hash/sha256-file input))
                    value {"schema_version" "abc-adr-evidence-run-v1"
                           "producer" {"tool" "test" "command" "test"
                                       "revision" (apply str (repeat 40 "0"))}
                           "input_profile" {"kind" "repo-files-v1"
                                            "roots" [] "explicit" ["input.txt"]}
                           "inputs" {"input.txt" input-hash}
                           "observations" {"contract" {"value" true}}}
                    canonical (hash/format-sha256 (hash/sha256-json-jcs value))]
                (json/write-deterministic-json-file! artifact value)
                (is (= #{:artifact-hash-mismatch}
                       (kinds (:problems
                               (bundle/validate-bundle
                                repo artifact-path
                                (str "sha256:" (apply str (repeat 64 "f")))
                                ["ADR-0034-C1"])))))
                (files/write-text! input "two")
                (is (contains?
                     (kinds (:problems (bundle/validate-bundle
                                        repo artifact-path canonical ["ADR-0034-C1"])))
                     :input-hash-mismatch)))))))
      (testing "predicates derive pass/fail/type outcomes"
        (is (= {:status :pass}
               (evidence/evaluate-result {:operator :>= :value 2} 3)))
        (is (= {:status :fail}
               (evidence/evaluate-result {:operator :>= :value 3} 2)))
        (is (= {:status :type-error}
               (evidence/evaluate-result {:operator :>= :value 2} "3"))))
      (testing "external expiry uses only the explicit governance date"
        (let [c (claim :external-semantics)
              e (entry :external-semantics :external-authority)
              artifact {"schema_version" "abc-adr-external-evidence-v1"
                        "review_after" "2026-07-12"
                        "observations" {"contract" {"value" true}}}]
          (is (not (contains? (kinds (registry-problems c e artifact "2026-07-12"))
                              :expired-evidence)))
          (is (contains? (kinds (registry-problems c e artifact "2026-07-13"))
                         :expired-evidence)))))))

(def lifecycle-problem
  {:file "0001-root.md" :kind :noncanonical-dependency-path
   :message "Accepted ADR reaches Proposed ADR" :path [1 2 4]})

(def evidence-problem
  {:file "0034-synthetic.md" :criterion-index 0 :claim-id "ADR-0034-C1"
   :kind :missing-claim-evidence :message "missing"})

(deftest adr-0034-c2-contract
  (runtime/with-validated-read-trace!
    (boundary-options "adr-0034-c2")
    (fn []
      (testing "lifecycle vocabulary, complete closure, cycles, and shortest witnesses"
        (let [invalid-fixture
              (files/read-text
               "fixtures/adr-governance-invalid/docs/adr/0001-invalid.md")]
          (is (str/includes? invalid-fixture "ADR 9999"))
          (evidence-io/with-owned-ephemeral-root
            (fn [dir]
              (doseq [[name body]
                      [["0001-root.md" "# ADR 0001: Root\n\nStatus: Accepted\nDate: 2026-07-12\nAccepted: 2026-07-12\nValidation scope: structural\nRelease authority: none\nDepends on: ADR 0002, ADR 0003\n\n## Decision\n\nRoot.\n\n## Implementation Status\n\nDone.\n\n## Acceptance Criteria\n\n- **ADR-0001-C1 — structural-invariant:** `test/x.clj`.\n"]
                       ["0002-left.md" "# ADR 0002: Left\n\nStatus: Accepted\nDate: 2026-07-12\nAccepted: 2026-07-12\nValidation scope: structural\nRelease authority: none\nDepends on: ADR 0004\n\n## Decision\n\nLeft.\n\n## Implementation Status\n\nDone.\n\n## Acceptance Criteria\n\n- **ADR-0002-C1 — structural-invariant:** `test/x.clj`.\n"]
                       ["0003-right.md" "# ADR 0003: Right\n\nStatus: Accepted\nDate: 2026-07-12\nAccepted: 2026-07-12\nValidation scope: structural\nRelease authority: none\nDepends on: ADR 0004\n\n## Decision\n\nRight.\n\n## Implementation Status\n\nDone.\n\n## Acceptance Criteria\n\n- **ADR-0003-C1 — structural-invariant:** `test/x.clj`.\n"]
                       ["0004-future.md" "# ADR 0004: Future\n\nStatus: Proposed\nDate: 2026-07-12\n\n## Decision\n\nFuture.\n"]
                       ["0005-cycle.md" "# ADR 0005: Cycle\n\nStatus: Draft\nDate: 2026-07-12\nDepends on: ADR 0006\n\n## Decision\n\nCycle.\n"]
                       ["0006-cycle.md" "# ADR 0006: Cycle\n\nStatus: Draft\nDate: 2026-07-12\nDepends on: ADR 0005\n\n## Decision\n\nCycle.\n"]
                       ["0007-invalid.md" "# ADR 0007: Invalid\n\nStatus: Accepted now\nDate: 2026-07-12\n\n## Decision\n\nInvalid.\n"]]]
                (files/write-text! (fs/file dir name) body))
              (files/create-dirs! (fs/file dir "test"))
              (files/write-text! (fs/file dir "test/x.clj") "(ns x)")
              (let [problems (adr/validate-adrs (adr/parse-all dir) dir)]
                (is (contains? (kinds problems) :invalid-status))
                (is (= [1 2 4]
                       (:path (first (filter #(and (= "0001-root.md" (:file %))
                                                   (= :noncanonical-dependency-path (:kind %)))
                                             problems)))))
                (is (vector? problems)))))))
      (testing "ADR problems precede evidence and audit/enforce differ only by exit"
        (with-redefs [adr/parse-all (constantly [])
                      adr/validate-adrs (constantly [lifecycle-problem])
                      adr/validate-repository-legacy (constantly [])
                      evidence/load-registry (constantly {:entries []})
                      evidence/load-matrix (constantly {})
                      evidence/load-as-of (constantly "2026-07-12")
                      evidence/validate-registry (constantly [evidence-problem])]
          (let [audit (governance/run! "." {:mode :audit})
                enforce (governance/run! "." {:mode :enforce})]
            (is (= [lifecycle-problem evidence-problem] (:problems audit)))
            (is (= (:problems audit) (:problems enforce)))
            (is (= 0 (:exit-code audit)))
            (is (= 1 (:exit-code enforce)))
            (is (= {:ok? true :exit-code 0 :mode :legacy :problems []}
                   (governance/run! "." {:mode :legacy})))))))))
