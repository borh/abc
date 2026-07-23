(ns abc.tools.adr-governance-test
  (:require [abc.tools.adr :as adr]
            [abc.tools.adr-governance :as governance]
            [babashka.fs :as fs]
            [clojure.string :as string]
            [clojure.test :refer [deftest is]]))

(def ^:private lifecycle-problem
  {:file "0031-adr-governance-validation.md"
   :kind :noncanonical-dependency-path
   :message "Accepted ADR reaches Proposed ADR"
   :path [31 29]})

(deftest strict-problems-are-nonzero
  (with-redefs [adr/validate-repository (fn [_repo] [lifecycle-problem])]
    (is (= {:ok? false :exit-code 1 :problems [lifecycle-problem]}
           (governance/run! ".")))))

(deftest clean-corpus-is-zero
  (with-redefs [adr/validate-repository (fn [_repo] [])]
    (is (= {:ok? true :exit-code 0 :problems []}
           (governance/run! ".")))))

(deftest repository-corpus-is-strictly-valid-test
  (let [{:keys [ok? problems]} (governance/run! ".")]
    (is ok? (pr-str (take 5 problems)))))

(deftest retired-typed-evidence-apparatus-is-absent-test
  (doseq [path ["docs/adr/adr-evidence.edn"
                "docs/adr/adr-claim-migration.edn"
                "docs/adr/adr-claim-migration-baseline.json"
                "docs/adr/claim-evidence-compatibility.edn"
                "docs/adr/governance-as-of.edn"
                "docs/evidence/adr-bootstrap"
                "docs/evidence/adr-capture"
                "docs/evidence/adr-entries"
                "docs/evidence/adr-inputs"
                "docs/evidence/adr-runs"
                "data/adr-evidence"
                "src/abc/tools/adr_evidence.clj"
                "src/abc/tools/adr_evidence_runtime_inputs.clj"
                "src/abc/tools/evidence_io.clj"
                "src/abc/tools/evidence_output.clj"]]
    (is (not (fs/exists? path)) path)))

(deftest cli-runs-strict-governance-and-preserves-root-options-test
  (let [calls (atom [])
        argv ["--repo-root" "/repo/abc"]]
    (with-redefs [governance/run!
                  (fn [repo-root]
                    (swap! calls conj repo-root)
                    {:ok? true :exit-code 0 :problems []})]
      (governance/run-cli! argv)
      (governance/run-cli! (into ["--"] argv)))
    (is (= ["/repo/abc" "/repo/abc"] @calls))))

(deftest cli-rejects-invalid-arguments-test
  (let [result (governance/run-cli! ["a" "b"])]
    (is (false? (:ok? result)))
    (is (= 2 (:exit-code result)))
    (is (= :invalid-cli (:kind (first (:problems result)))))))

(deftest invalid-fixture-is-nonzero-test
  (let [{:keys [ok? exit-code problems]}
        (governance/run! "fixtures/adr-governance-invalid")]
    (is (false? ok?))
    (is (= 1 exit-code))
    (is (= #{:missing-relation-target}
           (set (map :kind problems))))))

(deftest nix-governance-check-is-strict-test
  (let [flake (slurp "flake.nix")]
    (is (string/includes? flake "clojure -M:abc/adr-governance"))
    (is (not (string/includes? flake "--mode")))
    (is (string/includes? flake "ADR corpus is strictly valid."))))

(deftest root-governance-check-is-the-component-check-test
  (let [root-flake (slurp (fs/file "../flake.nix"))]
    (is (string/includes?
         root-flake
         "monorepo-adr-governance = abc.checks.${system}.adr-governance;"))
    (is (not (string/includes? root-flake "--mode")))
    (is (not (string/includes? root-flake "adr-problem-identities")))))

(deftest shared-clojure-app-launcher-sets-user-home-quietly-test
  (let [flake (slurp "flake.nix")
        start (string/index-of flake "mkCljLauncher =")
        end (string/index-of flake "mkCljApp =" start)
        launcher (subs flake start end)]
    (is (not (string/includes? launcher "export JAVA_TOOL_OPTIONS")))
    (is (string/includes? launcher "-J-Duser.home=${cljDepsCache}"))))
