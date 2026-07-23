(ns abc.tools.adr-governance-test
  (:require [abc.tools.adr :as adr]
            [abc.tools.adr-governance :as governance]
            [abc.tools.json :as json]
            [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]))

(def ^:private lifecycle-problem
  {:file "0031-adr-governance-validation.md"
   :kind :noncanonical-dependency-path
   :message "Accepted ADR reaches Proposed ADR"
   :path [31 29]})

(deftest audit-and-enforce-report-strict-problems
  (with-redefs [adr/validate-repository (fn [_repo] [lifecycle-problem])]
    (testing "audit reports every strict problem with a zero exit"
      (is (= {:ok? false :exit-code 0 :mode :audit
              :problems [lifecycle-problem]}
             (governance/run! "." {:mode :audit}))))
    (testing "enforcement is nonzero on the same problem set"
      (is (= {:ok? false :exit-code 1 :mode :enforce
              :problems [lifecycle-problem]}
             (governance/run! "." {:mode :enforce}))))))

(deftest legacy-mode-uses-the-legacy-validator
  (with-redefs [adr/validate-repository-legacy (fn [_repo] [])
                adr/validate-repository
                (fn [_repo] (throw (ex-info "must not run" {})))]
    (is (= {:ok? true :exit-code 0 :mode :legacy :problems []}
           (governance/run! "." {:mode :legacy})))))

(deftest audit-report-preserves-problem-coordinates
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "adr-governance-test"
                      (make-array java.nio.file.attribute.FileAttribute 0)))
        report (io/file dir "report.json")]
    (try
      (with-redefs [adr/validate-repository (fn [_repo] [lifecycle-problem])]
        (let [result (governance/run-cli!
                      ["--mode" "audit" "--report" (.getPath report) "."])
              value (json/read-json-file report)]
          (is (= 0 (:exit-code result)))
          (is (false? (get value "ok")))
          (is (= {"file" "0031-adr-governance-validation.md"
                  "kind" "noncanonical-dependency-path"}
                 (select-keys (get-in value ["problems" 0])
                              ["file" "kind"])))))
      (finally
        (doseq [file (reverse (file-seq dir))]
          (.delete file))))))

(deftest repository-corpus-is-strictly-valid-test
  (let [{:keys [ok? problems]} (governance/run! "." {:mode :enforce})]
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

(deftest nix-governance-check-selects-audit-mode-test
  (let [flake (slurp "flake.nix")]
    (is (string/includes? flake
                          "clojure -M:abc/adr-governance --mode audit"))
    (is (string/includes?
         flake
         "ADR lifecycle, dependency, claim, artifact, freshness, and evidence audit completed."))))

(deftest root-governance-check-selects-enforcement-test
  (let [root-flake (slurp (fs/file "../flake.nix"))]
    (is (string/includes? root-flake "src = self;"))
    (is (string/includes? root-flake "--mode enforce"))
    (is (not (string/includes? root-flake "--mode audit")))
    (is (not (string/includes? root-flake "adr-problem-identities")))
    (is (not (string/includes? root-flake "adr-evidence-migration")))))

(deftest shared-clojure-app-launcher-sets-user-home-quietly-test
  (let [flake (slurp "flake.nix")
        start (string/index-of flake "mkCljLauncher =")
        end (string/index-of flake "mkCljApp =" start)
        launcher (subs flake start end)]
    (is (not (string/includes? launcher "export JAVA_TOOL_OPTIONS")))
    (is (string/includes? launcher "-J-Duser.home=${cljDepsCache}"))))

(deftest cli-leading-separator-preserves-root-options-test
  (let [calls (atom [])
        argv ["--repo-root" "/repo/abc" "--mode" "audit"]]
    (with-redefs [governance/run!
                  (fn [repo-root options]
                    (swap! calls conj [repo-root options])
                    {:ok? true :exit-code 0 :mode (:mode options) :problems []})]
      (governance/run-cli! argv)
      (governance/run-cli! (into ["--"] argv)))
    (is (= [["/repo/abc" {:mode :audit :workspace-root nil}]
            ["/repo/abc" {:mode :audit :workspace-root nil}]]
           @calls))))

(deftest invalid-fixture-has-identical-audit-and-enforce-problems-test
  (let [root "fixtures/adr-governance-invalid"
        audit (governance/run! root {:mode :audit})
        enforce (governance/run! root {:mode :enforce})]
    (is (false? (:ok? audit)))
    (is (false? (:ok? enforce)))
    (is (= 0 (:exit-code audit)))
    (is (= 1 (:exit-code enforce)))
    (is (= (:problems audit) (:problems enforce)))
    (is (= #{:missing-relation-target}
           (set (map :kind (:problems audit)))))))
