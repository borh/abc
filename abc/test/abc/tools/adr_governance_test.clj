(ns abc.tools.adr-governance-test
  (:require [abc.tools.adr :as adr]
            [abc.tools.adr-evidence :as evidence]
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

(def ^:private evidence-problem
  {:file "0034-typed-evidence.md"
   :criterion-index 0
   :claim-id "ADR-0034-C1"
   :kind :missing-claim-evidence
   :message "Accepted claim has no evidence registry entry"})

(def ^:private parsed-adrs
  [{:file "0034-typed-evidence.md"
    :status "Accepted"
    :criteria [{:criterion-index 0
                :body "Claim."
                :claim-id "ADR-0034-C1"
                :claim-kind :structural-invariant}]}])

(defn- with-aggregate-stubs [f]
  (with-redefs [adr/parse-all (fn [_dir] parsed-adrs)
                adr/validate-adrs (fn [_adrs _repo] [lifecycle-problem])
                adr/validate-repository-legacy (fn [_repo] [])
                evidence/load-registry (fn [] {:entries []})
                evidence/load-matrix (fn [] {})
                evidence/load-as-of (fn [] "2026-07-12")
                evidence/validate-registry
                (fn [{:keys [claims]}]
                  (is (= [{:file "0034-typed-evidence.md"
                           :status "Accepted"
                           :criterion-index 0
                           :body "Claim."
                           :claim-id "ADR-0034-C1"
                           :claim-kind :structural-invariant}]
                         claims))
                  [evidence-problem])]
    (f)))

(deftest audit-and-enforce-aggregate-adr-and-evidence-problems
  (with-aggregate-stubs
    (fn []
      (testing "audit reports every problem in ADR-then-evidence order"
        (is (= {:ok? false :exit-code 0 :mode :audit
                :problems [lifecycle-problem evidence-problem]}
               (governance/run! "." {:mode :audit}))))
      (testing "enforcement is nonzero on the same complete problem set"
        (is (= {:ok? false :exit-code 1 :mode :enforce
                :problems [lifecycle-problem evidence-problem]}
               (governance/run! "." {:mode :enforce})))))))

(deftest legacy-never-loads-or-validates-typed-evidence
  (with-redefs [adr/validate-repository-legacy (fn [_repo] [])
                evidence/load-registry (fn [] (throw (ex-info "must not load" {})))
                evidence/validate-registry (fn [_] (throw (ex-info "must not run" {})))]
    (is (= {:ok? true :exit-code 0 :mode :legacy :problems []}
           (governance/run! "." {:mode :legacy})))))

(deftest audit-preserves-an-absent-workspace-root-test
  (let [seen (atom ::unset)]
    (with-redefs [adr/parse-all (constantly [])
                  adr/validate-adrs (constantly [])
                  evidence/load-registry (constantly {:entries []})
                  evidence/load-matrix (constantly {})
                  evidence/load-as-of (constantly "2026-07-12")
                  evidence/validate-registry
                  (fn [{:keys [workspace-root]}]
                    (reset! seen workspace-root)
                    [])]
      (governance/run! "." {:mode :audit})
      (is (nil? @seen)))))

(deftest audit-report-preserves-claim-problem-coordinates
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "adr-governance-test"
                      (make-array java.nio.file.attribute.FileAttribute 0)))
        report (io/file dir "report.json")]
    (try
      (with-aggregate-stubs
        (fn []
          (let [result (governance/run-cli!
                        ["--mode" "audit" "--report" (.getPath report) "."])
                value (json/read-json-file report)]
            (is (= 0 (:exit-code result)))
            (is (false? (get value "ok")))
            (is (= {"file" "0034-typed-evidence.md"
                    "criterion-index" 0
                    "claim-id" "ADR-0034-C1"
                    "kind" "missing-claim-evidence"}
                   (select-keys (get-in value ["problems" 1])
                                ["file" "criterion-index" "claim-id" "kind"]))))))
      (finally
        (doseq [file (reverse (file-seq dir))]
          (.delete file))))))

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
    (is (string/includes? root-flake "abc/nix/adr-problem-identities.jq"))
    (is (string/includes? root-flake
                          "cmp expected-problem-identities.json actual-problem-identities.json"))
    (is (not (string/includes? root-flake "(.problems | length) ==")))))

(deftest shared-clojure-app-launcher-sets-user-home-quietly-test
  (let [flake (slurp "flake.nix")
        start (string/index-of flake "mkCljLauncher =")
        end (string/index-of flake "mkCljApp =" start)
        launcher (subs flake start end)]
    (is (not (string/includes? launcher "export JAVA_TOOL_OPTIONS")))
    (is (string/includes? launcher "-J-Duser.home=${cljDepsCache}"))))

(deftest cli-leading-separator-preserves-root-options-test
  (let [calls (atom [])
        argv ["--repo-root" "/repo/abc" "--workspace-root" "/repo"
              "--mode" "audit"]]
    (with-redefs [governance/run!
                  (fn [repo-root options]
                    (swap! calls conj [repo-root options])
                    {:ok? true :exit-code 0 :mode (:mode options) :problems []})]
      (governance/run-cli! argv)
      (governance/run-cli! (into ["--"] argv)))
    (is (= [["/repo/abc" {:mode :audit :workspace-root "/repo"}]
            ["/repo/abc" {:mode :audit :workspace-root "/repo"}]]
           @calls))))

(deftest invalid-fixture-has-identical-audit-and-enforce-problems-test
  (let [root "fixtures/adr-governance-invalid"]
    (with-redefs [evidence/load-registry (constantly {:entries []})
                  evidence/load-matrix (constantly {})
                  evidence/load-as-of (constantly "2026-07-12")]
      (let [audit (governance/run! root {:mode :audit})
            enforce (governance/run! root {:mode :enforce})]
        (is (false? (:ok? audit)))
        (is (false? (:ok? enforce)))
        (is (= 0 (:exit-code audit)))
        (is (= 1 (:exit-code enforce)))
        (is (= (:problems audit) (:problems enforce)))
        (is (= #{:missing-relation-target}
               (set (map :kind (:problems audit)))))))))
