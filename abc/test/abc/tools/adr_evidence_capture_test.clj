(ns abc.tools.adr-evidence-capture-test
  (:require [abc.tools.adr-evidence-capture :as capture]
            [abc.tools.adr-evidence-observation-catalog :as catalog]
            [abc.tools.adr-evidence-operational :as operational]
            [abc.tools.adr-evidence-runtime-inputs :as runtime]
            [abc.tools.files :as files]
            [abc.tools.json :as json]
            [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(def ^:private foundation-descriptor-stems
  #{"adr-0001-c1-committed-manifest-schema"
    "adr-0001-c2-canonical-null-array-order"
    "adr-0001-c3-nested-artifact-id-rejection"
    "adr-0001-c4-failure-artifact-scope"
    "adr-0001-c4-failure-coordinates"
    "adr-0001-c4-failure-semantics"
    "adr-0001-c5-reproducibility-conflict"
    "adr-0008-c3-workflow-wiring"
    "adr-0008-c4-validation-read-catalog"
    "adr-0009-c2-materialized-content-hashes"
    "adr-0009-c3-mapping-divergence-sidecar"
    "adr-0009-c4-generated-artifact-ids-vs-content-hashes"
    "adr-0009-c5-aat-conversion-compatibility"
    "adr-0009-c6-temporary-materialization"
    "adr-0010-c1-materialized-bundled-schema-jcs-hash"
    "adr-0010-c2-materialized-artifact-ids-distinct"
    "adr-0010-c3-v0-identity-json"
    "adr-0010-c4-parser-schema-mismatch"
    "adr-0011-c2-deterministic-json-writer"
    "adr-0011-c3-two-run-byte-identity"
    "adr-0033-c1-canonical-all-member-schema"
    "adr-0033-c10-admission-limits"
    "adr-0033-c11-best-effort-counted-failure"
    "adr-0033-c11-non-release-admissible"
    "adr-0033-c11-strict-atomic-abort"
    "adr-0033-c2-clojure-known-answer"
    "adr-0033-c3-repack-image-evolution"
    "adr-0033-c4-parser-identity-roles"
    "adr-0033-c5-role-specific-snapshot-validation"
    "adr-0033-c6-p16-3-ungated"
    "adr-0033-c7-d7-dated-fixed-state"
    "adr-0033-c8-complete-legacy-readability"
    "design-bundle-operational"
    "diagnostic-schema-exact-current"
    "generated-import-manifest-schema"
    "source-bundle-corpus"
    "validate-design-bundle-wrapper-delegation"})

(def ^:private schema-rdf-tei-descriptor-stems
  #{"design-bundle-operational-schema-rdf-tei"
    "iiif-committed-applicability"
    "iiif-invalid-combinations"
    "iiif-schema-contract"
    "iiif-text-only-values"
    "iiif-valid-combinations"
    "linked-art-artifact-predicate"
    "linked-art-context"
    "linked-art-current-parity-hash"
    "linked-art-full-behavior"
    "linked-art-parity"
    "linked-art-result-contract"
    "manifest-rdf-parity"
    "metadata-bundle-helper"
    "metadata-rdf-containment"
    "metadata-rdf-parity"
    "metadata-rights-shacl"
    "schema-canonicalization-mismatch"
    "schema-ci-wiring"
    "schema-empty-manifest"
    "schema-entrypoint-delegation"
    "tei-enrichment-warning"
    "tei-figure-warning"
    "tei-profile-drift"
    "tei-project-cross-schema-invalid"
    "tei-project-valid-fixtures"
    "tei-publication-sidecars"
    "tei-schematron-invalid-ids"
    "tei-upstream-rng"
    "turtle-prefix-inventory"})

(deftest checked-in-foundation-observation-boundary-inventory-is-exact-test
  (let [stems (fn [root]
                (->> (fs/list-dir root)
                     (filter fs/regular-file?)
                     (map #(str (fs/strip-ext (fs/file-name %))))
                     set))]
    (is (= 37 (count foundation-descriptor-stems)))
    (is (= (set/union foundation-descriptor-stems
                      schema-rdf-tei-descriptor-stems)
           (stems "docs/evidence/adr-capture")))
    (is (= (set/union foundation-descriptor-stems
                      (disj schema-rdf-tei-descriptor-stems "tei-profile-drift"))
           (stems "docs/evidence/adr-inputs")))
    (is (= 42 (count (:entries (files/read-edn
                                "docs/evidence/adr-entries/foundation.edn")))))))

(deftest checked-in-foundation-observation-boundaries-are-valid-test
  (let [repo-root (fs/file (fs/canonicalize "."))
        monorepo-layout? (fs/directory? "../abc")
        workspace-root (fs/file (fs/canonicalize (if monorepo-layout? ".." ".")))
        catalog-value (catalog/load-catalog!
                       repo-root "data/adr-evidence/foundation-observation-catalog.edn")
        bindings (files/read-edn "docs/evidence/adr-entries/foundation.edn")]
    (is (empty? (catalog/validate-bindings catalog-value bindings)))
    (doseq [stem (sort foundation-descriptor-stems)]
      (let [descriptor-path (str "docs/evidence/adr-capture/" stem ".edn")
            context (operational/load-descriptor-context!
                     {:repo-root repo-root
                      :workspace-root workspace-root
                      :descriptor-path descriptor-path})
            descriptor (get-in context [:descriptor :value])]
        (if (= "abc-adr-evidence-capture-operational-v1"
               (:schema-version descriptor))
          (is (some? (operational/validate-operational-manifest! context)))
          (let [runtime-descriptor
                (if monorepo-layout?
                  {:path (str "abc/" descriptor-path)
                   :value (update descriptor :runtime-input-manifest
                                  #(str "abc/" %))}
                  {:path descriptor-path
                   :value (update-in descriptor [:input-profile :explicit]
                                     #(mapv (fn [path]
                                              (str/replace path #"^abc/" ""))
                                            %))})]
            (is (map?
                 (runtime/validate-runtime-input-manifest!
                  (cond-> {:repo-root repo-root
                           :workspace-root workspace-root
                           :descriptor runtime-descriptor}
                    (not monorepo-layout?) (assoc :component-root "abc")))))))))))

(deftest checked-in-schema-rdf-tei-observation-boundaries-are-valid-test
  (let [repo-root (fs/file (fs/canonicalize "."))
        monorepo-layout? (fs/directory? "../abc")
        workspace-root (fs/file (fs/canonicalize (if monorepo-layout? ".." ".")))
        catalog-value (catalog/load-catalog!
                       repo-root
                       "data/adr-evidence/schema-rdf-tei-observation-catalog.edn")
        bindings (files/read-edn "docs/evidence/adr-entries/schema-rdf-tei.edn")
        operational-problem-kinds
        (fn [context]
          (try
            (operational/validate-operational-manifest! context)
            #{}
            (catch clojure.lang.ExceptionInfo exception
              (set (map :kind (:problems (ex-data exception)))))))]
    (is (= 30 (count schema-rdf-tei-descriptor-stems)))
    (is (= 21 (count (:focused-observations catalog-value))))
    (is (= 9 (count (:operational-observations catalog-value))))
    (is (= 35 (count (:entries bindings))))
    (is (empty? (catalog/validate-bindings catalog-value bindings)))
    (is (empty? (catalog/focused-conformance-findings repo-root catalog-value)))
    (doseq [stem (sort schema-rdf-tei-descriptor-stems)]
      (let [descriptor-path (str "docs/evidence/adr-capture/" stem ".edn")
            context (operational/load-descriptor-context!
                     {:repo-root repo-root
                      :workspace-root workspace-root
                      :descriptor-path descriptor-path})
            descriptor (get-in context [:descriptor :value])]
        (if (= "abc-adr-evidence-capture-operational-v1"
               (:schema-version descriptor))
          (let [explicit (get-in descriptor [:input-profile :explicit])
                missing (assoc-in context [:descriptor :value :input-profile :explicit]
                                  (vec (rest explicit)))
                extra (assoc-in context [:descriptor :value :input-profile :explicit]
                                (vec (sort (conj explicit "docs/adr/README.md"))))]
            (is (some? (operational/validate-operational-manifest! context)) stem)
            (is (contains? (operational-problem-kinds missing)
                           :operational-input-set-mismatch)
                (str stem " rejects a missing determinant"))
            (is (contains? (operational-problem-kinds extra)
                           :operational-input-set-mismatch)
                (str stem " rejects an extra determinant")))
          (let [runtime-descriptor
                (if monorepo-layout?
                  {:path (str "abc/" descriptor-path)
                   :value (update descriptor :runtime-input-manifest
                                  #(str "abc/" %))}
                  {:path descriptor-path
                   :value (update-in descriptor [:input-profile :explicit]
                                     #(mapv (fn [path]
                                              (str/replace path #"^abc/" ""))
                                            %))})]
            (is (map?
                 (runtime/validate-runtime-input-manifest!
                  (cond-> {:repo-root repo-root
                           :workspace-root workspace-root
                           :descriptor runtime-descriptor}
                    (not monorepo-layout?) (assoc :component-root "abc"))))
                stem)))))))

(defn- temp-dir [prefix]
  (fs/file (fs/create-temp-dir {:prefix prefix})))

(defn- exec! [dir & argv]
  (:exit @(process/process (vec argv) {:dir (str dir) :out :string :err :out})))

(defn- problem-kind [thunk]
  (try (thunk) nil (catch Exception e (:kind (ex-data e)))))

(defn- write-executable! [root path body]
  (let [file (fs/file root path)]
    (fs/create-dirs (fs/parent file))
    (spit file body)
    (fs/set-posix-file-permissions file "rwxr-xr-x")
    file))

(defn- git-repo []
  (let [repo (temp-dir "abc-evidence-capture-repo")
        source (fs/file repo "src/example/core.clj")]
    (fs/create-dirs (fs/parent source))
    (spit source "(ns example.core)\n")
    (exec! repo "git" "init" "-q")
    (exec! repo "git" "config" "user.email" "capture@example.invalid")
    (exec! repo "git" "config" "user.name" "Capture Test")
    (exec! repo "git" "add" ".")
    (exec! repo "git" "commit" "-q" "-m" "fixture")
    repo))

(defn- git-workspace []
  (let [workspace (temp-dir "abc-evidence-workspace")
        repo (fs/file workspace "abc")]
    (doseq [[path body] [["flake.nix" "{}\n"]
                         ["justfile" "default:\n\t@true\n"]
                         ["abc/flake.nix" "{}\n"]
                         ["ab-validator/flake.nix" "{}\n"]
                         ["abc/src/example/core.clj" "(ns example.core)\n"]]]
      (let [file (fs/file workspace path)]
        (fs/create-dirs (fs/parent file))
        (spit file body)))
    (exec! workspace "git" "init" "-q")
    (exec! workspace "git" "config" "user.email" "capture@example.invalid")
    (exec! workspace "git" "config" "user.name" "Capture Test")
    (exec! workspace "git" "add" ".")
    (exec! workspace "git" "commit" "-q" "-m" "fixture")
    {:workspace workspace :repo repo}))

(defn- descriptor [argv]
  {:schema-version "abc-adr-evidence-capture-v1"
   :tool "sh"
   :argv argv
   :input-profile {:kind "repo-files-v1"
                   :roots []
                   :explicit ["src/example/core.clj"]}
   :observation-key "command-passed"})

(defn- install-descriptor! [repo path value message]
  (let [file (fs/file repo path)]
    (fs/create-dirs (fs/parent file))
    (spit file (pr-str value))
    (exec! repo "git" "add" path)
    (exec! repo "git" "commit" "-q" "-m" message)
    path))

(defn- capture-options [repo staging descriptor-path output]
  {:repo-root repo
   :workspace-root repo
   :staging-root staging
   :descriptor-path descriptor-path
   :output output})

(deftest three-root-capture-loads-the-contained-descriptor-test
  (let [{:keys [workspace repo]} (git-workspace)
        staging (temp-dir "abc-evidence-staging")
        descriptor-path "docs/evidence/adr-capture/example.edn"
        output (fs/file staging "example.json")
        value (descriptor ["sh" "-c" "test \"$PWD\" = \"$1\"" "sh" (str repo)])
        descriptor-file (fs/file repo descriptor-path)]
    (fs/create-dirs (fs/parent descriptor-file))
    (spit descriptor-file (pr-str value))
    (exec! workspace "git" "add" ".")
    (exec! workspace "git" "commit" "-q" "-m" "descriptor")
    (is (= 0 (:exit-code
              (capture/capture! {:repo-root repo
                                 :workspace-root workspace
                                 :staging-root staging
                                 :descriptor-path descriptor-path
                                 :output output}))))
    (is (fs/regular-file? output))
    (is (thrown? clojure.lang.ExceptionInfo
                 (capture/capture! {:repo-root repo
                                    :workspace-root workspace
                                    :staging-root staging
                                    :descriptor-path descriptor-path
                                    :descriptor value
                                    :output (fs/file staging "forbidden.json")})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (capture/capture! {:repo-root repo
                                    :workspace-root workspace
                                    :staging-root staging
                                    :descriptor-path "../outside.edn"
                                    :output (fs/file staging "outside.json")})))))

(deftest focused-runner-failures-have-a-focused-problem-kind-test
  (let [repo (git-repo)
        staging (temp-dir "abc-invalid-focused-staging")
        runner "bin/kaocha"
        descriptor-path "docs/evidence/adr-capture/example.edn"
        invalid-descriptor {:schema-version "abc-adr-evidence-capture-v2"
                            :tool runner
                            :argv [runner]
                            :input-profile {:kind "clojure-test-v1"
                                            :roots ["example.core-test"]
                                            :explicit [runner "docs/evidence/adr-inputs/example.edn"]}
                            :runtime-input-manifest "docs/evidence/adr-inputs/example.edn"
                            :observation-key "passes"}
        _ (install-descriptor! repo descriptor-path invalid-descriptor "invalid focus")
        invalid-runner-options (capture-options repo staging descriptor-path
                                                (fs/file staging "example.json"))
        validate-runner! (ns-resolve 'abc.tools.adr-evidence-capture 'validate-runner!)
        validate-summary! (ns-resolve 'abc.tools.adr-evidence-capture
                                      'validate-v2-command-result!)]
    (is (= :invalid-focused-evidence-runner
           (problem-kind #(capture/capture! invalid-runner-options))))
    (is (= :invalid-focused-evidence-runner
           (problem-kind #(@validate-runner! (temp-dir "abc-missing-runner")
                                             invalid-descriptor))))
    (is (= :invalid-focused-evidence-runner
           (problem-kind #(@validate-summary! ['example.core-test/contract]
                                              {:exit-code 0
                                               :stdout "0 tests, 0 assertions, 0 failures."
                                               :stderr ""}))))))

(deftest run-process-captures-working-directory-output-and-status
  (let [run-process (ns-resolve 'abc.tools.adr-evidence-capture 'run-process)
        dir (temp-dir "abc-evidence-capture-process")
        result (@run-process dir
                             ["sh" "-c"
                              "printf %s \"$PWD\"; printf err >&2; exit 4"])]
    (is (= #{:exit-code :stdout :stderr} (set (keys result))))
    (is (= 4 (:exit-code result)))
    (is (= (str (fs/canonicalize dir)) (:stdout result)))
    (is (= "err" (:stderr result)))))

(deftest operational-execution-uses-the-catalog-environment-policy-test
  (let [run-operational (ns-resolve 'abc.tools.adr-evidence-capture
                                    'run-operational-process)
        run-process-var (ns-resolve 'abc.tools.adr-evidence-capture 'run-process)
        calls (atom [])
        homes (atom [])
        fake-run (fn [repo argv environment]
                   (swap! calls conj {:repo repo :argv argv :environment environment})
                   (swap! homes conj (get environment "HOME"))
                   (is (fs/directory? (get environment "HOME")))
                   (cond
                     (= ["nix" "--version"] argv)
                     {:exit-code 0 :stdout "nix 2.test\n" :stderr ""}

                     (= "nix" (first argv))
                     {:exit-code 0 :stdout "x86_64-linux\n" :stderr ""}

                     :else {:exit-code 7 :stdout "" :stderr "failed"}))
        result (with-redefs-fn {run-process-var fake-run}
                 #(@run-operational "."
                                    {:catalog-row {:environment-policy :nix-local-v1
                                                   :command-id :contract}}
                                    ["bash" "--noprofile" "--norc" "-c" "exit 7"]))]
    (is (= 7 (:exit-code result)))
    (is (= {"nix_system" "x86_64-linux"
            "nix_version" "nix 2.test"
            "command_id" "contract"
            "environment_policy" "nix-local-v1"}
           (:operational-details result)))
    (is (= 3 (count @calls)))
    (is (every? #(= #{"HOME" "LANG" "LC_ALL"}
                    (set (remove #{"PATH" "NIX_REMOTE" "NIX_SSL_CERT_FILE"
                                   "SSL_CERT_FILE"}
                                 (keys (:environment %)))))
                @calls))
    (is (every? false? (map fs/exists? @homes)))))

(deftest captures-byte-identical-clean-tree-bundles
  (let [repo (git-repo)
        output-root (temp-dir "abc-evidence-capture-output")
        descriptor-path "docs/evidence/adr-capture/example.edn"
        _ (install-descriptor! repo descriptor-path
                               (descriptor ["sh" "-c" "exit 0"])
                               "descriptor")
        first-output (fs/file output-root "first.json")
        second-output (fs/file output-root "second.json")
        first-result (capture/capture!
                      (capture-options repo output-root descriptor-path first-output))
        _ (capture/capture!
           (capture-options repo output-root descriptor-path second-output))
        value (json/read-json-file first-output)]
    (is (= 0 (:exit-code first-result)))
    (is (= (slurp first-output) (slurp second-output)))
    (is (= "abc-adr-evidence-run-v1" (get value "schema_version")))
    (is (= true (get-in value ["observations" "command-passed" "value"])))
    (is (= 0 (get-in value ["observations" "command-passed" "details" "exit_code"])))
    (is (= 40 (count (get-in value ["producer" "revision"]))))))

(deftest dirty-tree-is-rejected-before-and-after-command
  (let [repo (git-repo)
        output-root (temp-dir "abc-evidence-capture-dirty")
        descriptor-path "docs/evidence/adr-capture/example.edn"
        output (fs/file output-root "bundle.json")
        marker (fs/file output-root "executed")]
    (install-descriptor! repo descriptor-path
                         (descriptor ["sh" "-c" (str "touch " (fs/absolutize marker))])
                         "descriptor")
    (spit (fs/file repo "untracked") "dirty")
    (is (thrown? clojure.lang.ExceptionInfo
                 (capture/capture!
                  (capture-options repo output-root descriptor-path output))))
    (is (not (fs/exists? marker)))
    (fs/delete (fs/file repo "untracked"))
    (install-descriptor! repo descriptor-path
                         (descriptor ["sh" "-c" "touch command-dirtied"])
                         "dirty command")
    (is (thrown? clojure.lang.ExceptionInfo
                 (capture/capture!
                  (capture-options repo output-root descriptor-path output))))
    (is (not (fs/exists? output)))))

(deftest failing-command-is-captured-before-cli-failure
  (let [repo (git-repo)
        staging (temp-dir "abc-evidence-capture-fail")
        output (fs/file staging "bundle.json")
        descriptor-path "docs/evidence/adr-capture/example.edn"
        _ (install-descriptor! repo descriptor-path
                               (descriptor ["sh" "-c" "exit 7"])
                               "descriptor")
        result (capture/capture!
                (capture-options repo staging descriptor-path output))]
    (is (= 1 (:exit-code result)))
    (is (= 7 (get-in (json/read-json-file output)
                     ["observations" "command-passed" "details" "exit_code"])))))

(deftest descriptor-key-set-is-closed
  (let [repo (git-repo)
        staging (temp-dir "abc-evidence-capture-invalid")
        output (fs/file staging "bundle.json")
        descriptor-path "docs/evidence/adr-capture/example.edn"]
    (install-descriptor! repo descriptor-path
                         (assoc (descriptor ["true"]) :unexpected true)
                         "invalid descriptor")
    (is (thrown? clojure.lang.ExceptionInfo
                 (capture/capture!
                  (capture-options repo staging descriptor-path output))))
    (is (not (fs/exists? output)))))

(deftest descriptor-version-and-runtime-manifest-contract-is-closed
  (let [repo (git-repo)
        staging (temp-dir "abc-evidence-capture-v2")
        output (fs/file staging "bundle.json")
        profile {:kind "clojure-test-v1"
                 :roots ["example.core"]
                 :explicit ["docs/evidence/adr-capture/example.edn"
                            "docs/evidence/adr-inputs/example.edn"
                            "bin/kaocha"]}
        v2 {:schema-version "abc-adr-evidence-capture-v2"
            :tool "bin/kaocha"
            :argv ["bin/kaocha" "--focus" "example.core/contract"]
            :input-profile profile
            :runtime-input-manifest "docs/evidence/adr-inputs/example.edn"
            :observation-key "passes"}]
    (doseq [[value path]
            [[(dissoc v2 :runtime-input-manifest)
              "docs/evidence/adr-capture/example.edn"]
             [(assoc (descriptor ["true"])
                     :runtime-input-manifest "docs/evidence/adr-inputs/example.edn")
              "docs/evidence/adr-capture/example.edn"]
             [(assoc-in v2 [:input-profile :unexpected] true)
              "docs/evidence/adr-capture/example.edn"]
             [(assoc-in v2 [:input-profile :component-root] "abc")
              "docs/evidence/adr-capture/example.edn"]
             [(assoc v2 :input-profile
                     {:kind "component-clojure-test-v1"
                      :roots ["example.core"] :explicit []})
              "docs/evidence/adr-capture/example.edn"]
             [v2 "docs/evidence/adr-capture/wrong.edn"]]]
      (is (thrown? clojure.lang.ExceptionInfo
                   (do
                     (let [file (fs/file repo path)]
                       (fs/create-dirs (fs/parent file))
                       (spit file (pr-str value)))
                     (capture/capture! (capture-options repo staging path output))))))))

(deftest v2-runner-shape-is-repository-bound-and-focus-only-test
  (let [validate! (ns-resolve 'abc.tools.adr-evidence-capture 'validate-descriptor!)
        descriptor-path "docs/evidence/adr-capture/example.edn"
        manifest-path "docs/evidence/adr-inputs/example.edn"
        component-descriptor-path "abc/docs/evidence/adr-capture/example.edn"
        component-manifest-path "abc/docs/evidence/adr-inputs/example.edn"
        ordinary {:schema-version "abc-adr-evidence-capture-v2"
                  :tool "bin/kaocha"
                  :argv ["bin/kaocha" "--focus" "example.core-test/contract"]
                  :runtime-input-manifest manifest-path
                  :input-profile {:kind "clojure-test-v1"
                                  :roots ["example.core-test"]
                                  :explicit [descriptor-path manifest-path "bin/kaocha"]}
                  :observation-key "passes"}
        component (-> ordinary
                      (assoc :tool "abc/bin/kaocha"
                             :argv ["abc/bin/kaocha" "--focus"
                                    "example.core-test/contract"]
                             :runtime-input-manifest component-manifest-path)
                      (assoc :input-profile
                             {:kind "component-clojure-test-v1"
                              :component-root "abc"
                              :roots ["example.core-test"]
                              :explicit [component-descriptor-path component-manifest-path
                                         "abc/bin/kaocha"]}))
        normalized-component (assoc-in component [:input-profile :component-root] "abc/.")]
    (is (= ordinary (@validate! ordinary descriptor-path)))
    (is (= component (@validate! component component-descriptor-path)))
    (is (= normalized-component (@validate! normalized-component component-descriptor-path)))
    (doseq [[label invalid]
            [["generic true" (assoc ordinary :tool "true" :argv ["true" "--focus"
                                                                 "example.core-test/contract"])]
             ["shell" (assoc ordinary :tool "bash"
                             :argv ["bash" "-lc" "bin/kaocha --focus example.core-test/contract"])]
             ["tool mismatch" (assoc ordinary :tool "bin/kaocha"
                                     :argv ["other" "--focus" "example.core-test/contract"])]
             ["missing focus" (assoc ordinary :argv ["bin/kaocha"])]
             ["unqualified focus" (assoc ordinary :argv ["bin/kaocha" "--focus" "contract"])]
             ["duplicate focus" (update ordinary :argv into
                                        ["--focus" "example.core-test/contract"])]
             ["extra option" (update ordinary :argv into ["--randomize" "false"])]
             ["runner unbound" (update-in ordinary [:input-profile :explicit]
                                          #(vec (remove #{"bin/kaocha"} %)))]]]
      (is (thrown? clojure.lang.ExceptionInfo
                   (@validate! invalid descriptor-path))
          label))))

(deftest v2-runner-must-be-a-contained-regular-executable-test
  (let [validate-runner! (ns-resolve 'abc.tools.adr-evidence-capture 'validate-runner!)
        repo (temp-dir "abc-runner-containment")
        outside (temp-dir "abc-runner-outside")
        runner "bin/kaocha"
        descriptor {:schema-version "abc-adr-evidence-capture-v2"
                    :tool runner
                    :argv [runner "--focus" "example.core-test/contract"]
                    :input-profile {:kind "clojure-test-v1"
                                    :roots ["example.core-test"]
                                    :explicit [runner]}
                    :runtime-input-manifest "docs/evidence/adr-inputs/example.edn"
                    :observation-key "passes"}]
    (is (some? validate-runner!))
    (when validate-runner!
      (write-executable! repo runner (str "#!" (fs/which "bash") "\nexit 0\n"))
      (is (= runner (@validate-runner! repo descriptor)))
      (fs/delete (fs/file repo runner))
      (is (thrown? clojure.lang.ExceptionInfo (@validate-runner! repo descriptor)))
      (fs/create-dirs (fs/file repo runner))
      (is (thrown? clojure.lang.ExceptionInfo (@validate-runner! repo descriptor)))
      (fs/delete-tree (fs/file repo runner))
      (let [plain (fs/file repo runner)]
        (fs/create-dirs (fs/parent plain))
        (spit plain "not executable")
        (is (thrown? clojure.lang.ExceptionInfo (@validate-runner! repo descriptor)))
        (fs/delete plain))
      (let [external (write-executable! outside "kaocha" (str "#!" (fs/which "bash") "\n"))]
        (fs/create-sym-link (fs/file repo runner) external)
        (is (thrown? clojure.lang.ExceptionInfo (@validate-runner! repo descriptor)))))))

(deftest v2-command-summary-must-execute-each-unique-focus-exactly-once-test
  (let [validate-summary! (ns-resolve 'abc.tools.adr-evidence-capture
                                      'validate-v2-command-result!)
        focuses ['example.core-test/one 'example.core-test/two]]
    (is (some? validate-summary!))
    (when validate-summary!
      (is (= {:exit-code 0 :stdout "2 tests, 2 assertions, 0 failures." :stderr ""}
             (@validate-summary! focuses
                                 {:exit-code 0
                                  :stdout "2 tests, 2 assertions, 0 failures."
                                  :stderr ""})))
      (doseq [stdout ["" "0 tests, 0 assertions, 0 failures."
                      "1 tests, 1 assertions, 0 failures."
                      "2 tests skipped"]]
        (is (thrown? clojure.lang.ExceptionInfo
                     (@validate-summary! focuses
                                         {:exit-code 0 :stdout stdout :stderr ""})))))))

(deftest repository-kaocha-launcher-is-cwd-independent-test
  (let [validate-summary! (ns-resolve 'abc.tools.adr-evidence-capture
                                      'validate-v2-command-result!)
        runner (str (fs/canonicalize "bin/kaocha"))
        outside (temp-dir "abc-kaocha-outside")
        valid @(process/process
                [runner "--focus"
                 "abc.tools.files-test/delete-tree-is-a-noop-on-missing-path-test"]
                {:dir (str outside) :out :string :err :string})
        non-test @(process/process
                   [runner "--focus" "abc.tools.files/bytes->hex"]
                   {:dir (str outside) :out :string :err :string})]
    (is (zero? (:exit valid)))
    (is (str/includes? (:out valid)
                       "delete-tree-is-a-noop-on-missing-path-test"))
    (is (str/includes? (:out valid) "1 tests, 1 assertions"))
    (is (zero? (:exit non-test)))
    (is (str/blank? (:out non-test)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (@validate-summary! ['abc.tools.files/bytes->hex]
                                     {:exit-code (:exit non-test)
                                      :stdout (:out non-test)
                                      :stderr (:err non-test)})))))

(deftest v2-capture-lints-the-focused-var-and-binds-its-closed-inputs-test
  (let [repo (git-repo)
        descriptor-path "docs/evidence/adr-capture/example.edn"
        manifest-path "docs/evidence/adr-inputs/example.edn"
        runner-path "bin/kaocha"
        test-path "test/example/core_test.clj"
        closure-path "src/abc/tools/adr_evidence_runtime_inputs.clj"
        descriptor {:schema-version "abc-adr-evidence-capture-v2"
                    :tool runner-path
                    :argv [runner-path "--focus" "example.core-test/runtime-input-contract"]
                    :runtime-input-manifest manifest-path
                    :input-profile {:kind "clojure-test-v1"
                                    :roots ["example.core-test"]
                                    :explicit [descriptor-path manifest-path runner-path]}
                    :observation-key "passes"}
        output (fs/file (temp-dir "abc-evidence-capture-v2-run") "bundle.json")]
    (doseq [[path body]
            [[descriptor-path (pr-str descriptor)]
             [manifest-path (pr-str {:schema-version :abc-adr-runtime-inputs-v1 :paths []})]
             [closure-path (str "(ns abc.tools.adr-evidence-runtime-inputs)\n"
                                "(defn with-validated-read-trace! [_ thunk] (thunk))\n")]
             [test-path (str "(ns example.core-test (:require [clojure.test :refer [deftest]] "
                             "[example.core] "
                             "[abc.tools.adr-evidence-runtime-inputs :as runtime]))\n"
                             "(deftest runtime-input-contract\n"
                             "  (runtime/with-validated-read-trace! {} (fn [] true)))\n")]]]
      (let [file (fs/file repo path)]
        (fs/create-dirs (fs/parent file))
        (spit file body)))
    (write-executable! repo runner-path
                       (str "#!" (fs/which "bash") "\n"
                            "set -euo pipefail\n"
                            "test \"$#\" -eq 2\n"
                            "test \"$1\" = --focus\n"
                            "test \"$2\" = example.core-test/runtime-input-contract\n"
                            "printf '1 tests, 1 assertions, 0 failures.\\n'\n"))
    (exec! repo "git" "add" ".")
    (exec! repo "git" "commit" "-q" "-m" "v2 fixture")
    (let [staging (fs/parent output)
          result (capture/capture! (capture-options repo staging descriptor-path output))
          inputs (get-in result [:bundle "inputs"])]
      (is (= 0 (:exit-code result)))
      (is (contains? inputs test-path))
      (is (contains? inputs "src/example/core.clj"))
      (is (contains? inputs descriptor-path))
      (is (contains? inputs manifest-path)))))
