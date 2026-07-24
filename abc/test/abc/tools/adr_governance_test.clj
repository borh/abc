(ns abc.tools.adr-governance-test
  (:require [abc.tools.adr-governance :as governance]
            [abc.tools.decisions :as decisions]
            [babashka.fs :as fs]
            [clojure.string :as string]
            [clojure.test :refer [deftest is]]))

(def ^:private lifecycle-problem
  {:file "docs/adr/decisions.edn"
   :kind :noncanonical-dependency-path
   :message "Accepted dependency closure contains a non-Accepted record"
   :slug "adr-governance-validation"})

(deftest strict-problems-are-nonzero
  (with-redefs [decisions/validate-repository (fn [_repo] [lifecycle-problem])]
    (is (= {:ok? false :exit-code 1 :problems [lifecycle-problem]}
           (governance/run! ".")))))

(deftest clean-corpus-includes-index-currency
  ;; core validation clean -> INDEX currency still gates the result
  (with-redefs [decisions/validate-repository (fn [_repo] [])]
    (let [{:keys [ok? problems]} (governance/run! ".")]
      (is (or ok?
              (= [:stale-index] (map :kind problems)))))))

(deftest repository-corpus-is-strictly-valid-test
  (let [{:keys [ok? problems]} (governance/run! ".")]
    (is ok? (pr-str (take 5 problems)))))

(deftest retired-apparatus-is-absent-test
  (doseq [path [;; typed-evidence apparatus (ADR 0043)
                "docs/adr/adr-evidence.edn"
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
                "src/abc/tools/evidence_output.clj"
                ;; Markdown ADR grammar (data-driven-decision-records)
                "src/abc/tools/adr.clj"
                "test/abc/tools/adr_test.clj"
                "docs/adr/adr-relations.edn"
                "fixtures/adr-governance-invalid"
                "dev/migrate_decisions.clj"
                "test/abc/tools/decisions_migration_test.clj"
                "nix/adr-family-clean.jq"
                "data/evidence-higher-order-calls/adr-validate-repository-star.edn"]]
    (is (not (fs/exists? path)) path)))

(deftest no-number-prefixed-narrative-files-remain-test
  (is (empty? (for [f (fs/list-dir "docs/adr")
                    :let [n (fs/file-name f)]
                    :when (re-matches #"\d{4}-.*" n)]
                n))))

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

(deftest unreadable-corpus-is-nonzero-test
  (let [root (str (fs/create-temp-dir))]
    (fs/create-dirs (fs/path root "docs/adr"))
    (spit (str (fs/path root "docs/adr/decisions.edn"))
          "{:decisions []} :junk")
    (let [{:keys [ok? exit-code problems]} (governance/run! root)]
      (is (false? ok?))
      (is (= 1 exit-code))
      (is (= [:invalid-edn] (map :kind problems))))))

(deftest invalid-corpus-is-nonzero-test
  (let [root (str (fs/create-temp-dir))]
    (fs/create-dirs (fs/path root "docs/adr"))
    (spit (str (fs/path root "docs/adr/decisions.edn"))
          (pr-str {:decisions
                   [{:slug "a" :title "A" :status :draft :date "2026-07-24"
                     :topics [] :claims []
                     :relations [{:class :lifecycle :type :depends-on
                                  :to "ghost"}]}]}))
    (spit (str (fs/path root "docs/adr/a.md")) "# A\n")
    (let [{:keys [ok? exit-code problems]} (governance/run! root)]
      (is (false? ok?))
      (is (= 1 exit-code))
      (is (= #{:missing-relation-target}
             (set (map :kind problems)))))))

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
