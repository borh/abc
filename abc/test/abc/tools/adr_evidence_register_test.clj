(ns abc.tools.adr-evidence-register-test
  (:require [abc.tools.adr-evidence :as evidence]
            [abc.tools.adr-evidence-observation-catalog :as observation-catalog]
            [abc.tools.adr-evidence-register :as register]
            [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.test :refer [deftest is testing]]))

(deftest registration-bindings-select-catalog-observations-test
  (let [catalog {:schema-version :abc-adr-evidence-observation-catalog-v1
                 :focused-observations
                 [{:observation-id :shared
                   :descriptor-stem "shared"
                   :observation-key "shared-passes"
                   :focus-var 'abc.tools.foundation-evidence-test/committed-manifest-schema-conformance-test}]
                 :operational-observations []}
        binding {:claim-id "ADR-0001-C1" :observation-id :shared}]
    (is (empty? (observation-catalog/validate-bindings
                 catalog {:entries [binding
                                    (assoc binding :claim-id "ADR-0009-C1")]})))
    (is (= :duplicate-observation-binding
           (-> (observation-catalog/validate-bindings
                catalog {:entries [binding binding]}) first :kind)))))

(defn- temp-dir []
  (fs/file (fs/create-temp-dir {:prefix "adr-evidence-register-test-"})))

(defn- write! [root path value]
  (let [file (fs/file root path)]
    (fs/create-dirs (fs/parent file))
    (spit file value)
    file))

(defn- valid-workspace []
  (let [root (temp-dir)]
    (doseq [path ["flake.nix" "justfile" "abc/flake.nix" "ab-validator/flake.nix"]]
      (write! root path "fixture\n"))
    (let [{:keys [exit]} @(process/process ["git" "init" "-q"]
                                           {:dir (str root)
                                            :out :string
                                            :err :string})]
      (when-not (zero? exit)
        (throw (ex-info "git init failed" {}))))
    root))

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
    (is (seq (:problems (register/materialize-template root {:entries [(entry)]}))))
    (is (= :invalid-registration-template
           (-> (register/materialize-template root :not-a-map) :problems first :kind)))
    (is (= :invalid-registration-entry
           (-> (register/materialize-template root (template [:not-a-map]))
               :problems first :kind)))))

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
                  evidence/validate-registry
                  (constantly [{:kind :missing-claim-evidence :claim-id "ADR-9999-C1"}])]
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
    (doseq [kind [:artifact-hash-mismatch :expired-evidence :predicate-failed]]
      (testing (name kind)
        (with-redefs [register/current-claims (constantly [{:claim-id "ADR-0001-C1"}])
                      evidence/validate-registry
                      (constantly [{:kind kind :claim-id "ADR-9999-C1"}])]
          (let [before (slurp registry-file)
                result (register/register! {:repo-root root :entries-path template-path
                                            :registry-path registry-path})]
            (is (false? (:ok? result)))
            (is (= kind (-> result :problems first :kind)))
            (is (= before (slurp registry-file)))))))))

(deftest checked-jq-family-predicate-covers-all-problem-coordinate-shapes
  (let [root (temp-dir)
        report (write! root "report.json"
                       "{\"problems\":[{\"claim-id\":\"ADR-0001-C1\"},{\"affected-claim-ids\":[\"ADR-0002-C1\"]},{\"file\":\"0003-example.md\"},{\"claim-id\":\"ADR-9999-C1\"}]}")
        jq "nix/adr-family-clean.jq"
        run (fn [family]
              (:exit @(process/process ["jq" "-e" "--arg" "family" family "-f" jq
                                        (str report)]
                                       {:out :string :err :string})))]
    (is (not (zero? (run "ADR-0001"))))
    (is (not (zero? (run "ADR-0002"))))
    (is (not (zero? (run "ADR-0003"))))
    (is (zero? (run "ADR-7777")))))

(deftest registration-threads-an-explicit-workspace-without-writing-on-failure-test
  (let [workspace (valid-workspace)
        repo (fs/file workspace "abc")
        registry-path "docs/adr/adr-evidence.edn"
        registry-file (write! repo registry-path "{:entries []}\n")
        entries-path "docs/evidence/adr-entries/example.edn"]
    (write! repo entries-path (pr-str (template [(entry)])))
    (write! repo "docs/evidence/adr-runs/example.json" (artifact "example-passes" "true"))
    (write! repo "docs/adr/claim-evidence-compatibility.edn" "{}\n")
    (write! repo "docs/adr/governance-as-of.edn" "{:as-of \"2026-07-12\"}\n")
    (with-redefs [register/current-claims (constantly [{:claim-id "ADR-0001-C1"}])
                  evidence/validate-registry
                  (fn [{:keys [workspace-root]}]
                    (if (= (fs/canonicalize workspace) (fs/canonicalize workspace-root))
                      []
                      [{:kind :missing-evidence-input :claim-id "ADR-0001-C1"}]))]
      (let [before (slurp registry-file)]
        (is (false? (:ok? (register/register!
                           {:repo-root repo :entries-path entries-path
                            :registry-path registry-path}))))
        (is (= before (slurp registry-file))))
      (is (:ok? (register/register!
                 {:repo-root repo :workspace-root workspace :entries-path entries-path
                  :registry-path registry-path})))
      (let [first-bytes (slurp registry-file)]
        (is (:ok? (register/register!
                   {:repo-root repo :workspace-root workspace :entries-path entries-path
                    :registry-path registry-path})))
        (is (= first-bytes (slurp registry-file)))))))
