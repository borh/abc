(ns abc.tools.adr-evidence-register-test
  (:require [abc.tools.adr-evidence :as evidence]
            [abc.tools.adr-evidence-register :as register]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir []
  (.toFile (Files/createTempDirectory "adr-evidence-register-test"
                                      (make-array FileAttribute 0))))

(defn- write! [root path value]
  (let [file (io/file root path)]
    (.mkdirs (.getParentFile file))
    (spit file value)
    file))

(defn- artifact [observation-key value]
  (str "{\"schema_version\":\"abc-adr-evidence-run-v1\","
       "\"observations\":{\"" observation-key "\":{\"value\":" value "}}}"))

(defn- entry
  ([] (entry "ADR-0001-C1" "docs/evidence/adr-runs/example.json" "example-passes"))
  ([claim-id path observation-key]
   {:claim-id claim-id
    :claim-kind :fixture-behavior
    :evidence-kind :fixture-conformance
    :artifact-path path
    :observation-key observation-key
    :expected {:operator := :value true}}))

(defn- template [entries]
  {:schema-version :abc-adr-evidence-registration-v1 :entries entries})

(deftest materialize-template-is-closed-and-atomic
  (let [root (temp-dir)]
    (write! root "docs/evidence/adr-runs/example.json" (artifact "example-passes" "true"))
    (let [result (register/materialize-template root (template [(entry)]))]
      (is (empty? (:problems result)))
      (is (re-matches #"sha256:[0-9a-f]{64}" (get-in result [:entries 0 :artifact-hash]))))
    (doseq [[label bad]
            [[:unknown-entry-key (assoc (entry) :extra true)]
             [:missing-entry-key (dissoc (entry) :expected)]
             [:supplied-hash (assoc (entry) :artifact-hash "sha256:nope")]
             [:inline-value (assoc (entry) :observed true)]
             [:absent-bundle (assoc (entry) :artifact-path "missing.json")]
             [:missing-observation (assoc (entry) :observation-key "absent")]]]
      (testing (name label)
        (let [result (register/materialize-template root (template [(entry) bad]))]
          (is (seq (:problems result)))
          (is (nil? (:entries result))))))
    (is (seq (:problems (register/materialize-template
                         root (assoc (template [(entry)]) :unknown true)))))
    (is (seq (:problems (register/materialize-template root {:entries [(entry)]}))))))

(deftest candidate-registry-replaces-owned-claims-deterministically
  (let [old-a (assoc (entry) :artifact-hash "old-a")
        old-b (assoc (entry "ADR-0002-C1" "other.json" "other") :artifact-hash "old-b")
        new-a (assoc (entry) :artifact-hash "new-a")
        new-a2 (assoc (entry "ADR-0001-C1" "second.json" "second") :artifact-hash "new-b")]
    (is (= {:entries [new-a new-a2 old-b]}
           (register/candidate-registry {:entries [old-b old-a]} [new-a2 new-a])))
    (is (= (pr-str (register/candidate-registry {:entries [old-a old-b]} [new-a new-a2]))
           (pr-str (register/candidate-registry {:entries [old-b old-a]} [new-a2 new-a]))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"duplicate"
                          (register/candidate-registry {:entries []} [new-a new-a])))))

(deftest registration-is-atomic-idempotent-and-allows-only-unrelated-missing-debt
  (let [root (temp-dir)
        registry-path "docs/adr/adr-evidence.edn"
        registry-file (write! root registry-path "{:entries []}\n")
        template-path "docs/evidence/adr-entries/foundation.edn"
        template-file (write! root template-path (pr-str (template [(entry)])))]
    (write! root "docs/adr/claim-evidence-compatibility.edn" "{}\n")
    (write! root "docs/adr/governance-as-of.edn" "{:as-of \"2026-07-12\"}\n")
    (write! root "docs/evidence/adr-runs/example.json" (artifact "example-passes" "true"))
    (with-redefs [register/current-claims (constantly [{:claim-id "ADR-0001-C1"}
                                                       {:claim-id "ADR-9999-C1"}])
                  evidence/load-matrix (constantly {})
                  evidence/load-as-of (constantly "2026-07-12")
                  evidence/validate-registry
                  (fn [{:keys [registry]}]
                    (if (= "bad" (get-in registry [:entries 0 :artifact-hash]))
                      [{:kind :artifact-hash-mismatch :claim-id "ADR-9999-C1"}]
                      [{:kind :missing-claim-evidence :claim-id "ADR-9999-C1"}]))]
      (let [result (register/register! {:repo-root root :entries-path template-path
                                        :registry-path registry-path})
            first-bytes (slurp registry-file)]
        (is (:ok? result))
        (is (re-find #"sha256:[0-9a-f]{64}" first-bytes))
        (is (:ok? (register/register! {:repo-root root :entries-path template-path
                                       :registry-path registry-path})))
        (is (= first-bytes (slurp registry-file)))))
    (let [before (slurp registry-file)]
      (spit template-file (pr-str (template [(entry) (assoc (entry) :artifact-path "missing.json")])))
      (is (false? (:ok? (register/register! {:repo-root root :entries-path template-path
                                             :registry-path registry-path}))))
      (is (= before (slurp registry-file))))
    (spit template-file (pr-str (template [(entry)])))
    (with-redefs [register/current-claims (constantly [{:claim-id "ADR-0001-C1"}])
                  evidence/load-matrix (constantly {})
                  evidence/load-as-of (constantly "2026-07-12")
                  evidence/validate-registry
                  (constantly [{:kind :predicate-failed :claim-id "ADR-9999-C1"}])]
      (let [before (slurp registry-file)]
        (is (false? (:ok? (register/register! {:repo-root root :entries-path template-path
                                               :registry-path registry-path}))))
        (is (= before (slurp registry-file)))))))

(deftest checked-jq-family-predicate-covers-all-problem-coordinate-shapes
  (let [root (temp-dir)
        report (write! root "report.json"
                       "{\"problems\":[{\"claim-id\":\"ADR-0001-C1\"},{\"affected-claim-ids\":[\"ADR-0002-C1\"]},{\"file\":\"0003-example.md\"},{\"claim-id\":\"ADR-9999-C1\"}]}")
        jq "nix/adr-family-clean.jq"
        run (fn [family]
              (-> (ProcessBuilder. ["jq" "-e" "--arg" "family" family "-f" jq
                                    (.getPath report)]) .start .waitFor))]
    (is (not (zero? (run "ADR-0001"))))
    (is (not (zero? (run "ADR-0002"))))
    (is (not (zero? (run "ADR-0003"))))
    (is (zero? (run "ADR-7777")))))
