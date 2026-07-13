(ns abc.tools.adr-evidence-capture-test
  (:require [abc.tools.adr-evidence-capture :as capture]
            [abc.tools.json :as json]
            [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

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

(defn- descriptor [argv]
  {:schema-version "abc-adr-evidence-capture-v1"
   :tool "sh"
   :argv argv
   :input-profile {:kind "repo-files-v1"
                   :roots []
                   :explicit ["src/example/core.clj"]}
   :observation-key "command-passed"})

(deftest focused-runner-failures-have-a-focused-problem-kind-test
  (let [runner "bin/kaocha"
        invalid-runner-options
        {:repo-root nil
         :descriptor {:schema-version "abc-adr-evidence-capture-v2"
                      :tool runner
                      :argv [runner]
                      :input-profile {:kind "clojure-test-v1"
                                      :roots ["example.core-test"]
                                      :explicit [runner "docs/evidence/adr-inputs/example.edn"]}
                      :runtime-input-manifest "docs/evidence/adr-inputs/example.edn"
                      :observation-key "passes"}
         :descriptor-path "docs/evidence/adr-capture/example.edn"
         :output nil}
        validate-runner! (ns-resolve 'abc.tools.adr-evidence-capture 'validate-runner!)
        validate-summary! (ns-resolve 'abc.tools.adr-evidence-capture
                                      'validate-v2-command-result!)]
    (is (= :invalid-focused-evidence-runner
           (problem-kind #(capture/capture! invalid-runner-options))))
    (is (= :invalid-focused-evidence-runner
           (problem-kind #(@validate-runner! (temp-dir "abc-missing-runner")
                                             (:descriptor invalid-runner-options)))))
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

(deftest captures-byte-identical-clean-tree-bundles
  (let [repo (git-repo)
        output-root (temp-dir "abc-evidence-capture-output")
        first-output (fs/file output-root "first.json")
        second-output (fs/file output-root "second.json")
        first-result (capture/capture!
                      {:repo-root repo :descriptor (descriptor ["sh" "-c" "exit 0"])
                       :output first-output})
        _ (capture/capture!
           {:repo-root repo :descriptor (descriptor ["sh" "-c" "exit 0"])
            :output second-output})
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
        output (fs/file output-root "bundle.json")
        marker (fs/file output-root "executed")]
    (spit (fs/file repo "untracked") "dirty")
    (is (thrown? clojure.lang.ExceptionInfo
                 (capture/capture!
                  {:repo-root repo
                   :descriptor (descriptor ["sh" "-c"
                                            (str "touch " (fs/absolutize marker))])
                   :output output})))
    (is (not (fs/exists? marker)))
    (fs/delete (fs/file repo "untracked"))
    (is (thrown? clojure.lang.ExceptionInfo
                 (capture/capture!
                  {:repo-root repo
                   :descriptor (descriptor ["sh" "-c" "touch command-dirtied"])
                   :output output})))
    (is (not (fs/exists? output)))))

(deftest failing-command-is-captured-before-cli-failure
  (let [repo (git-repo)
        output (fs/file (temp-dir "abc-evidence-capture-fail") "bundle.json")
        result (capture/capture!
                {:repo-root repo :descriptor (descriptor ["sh" "-c" "exit 7"])
                 :output output})]
    (is (= 1 (:exit-code result)))
    (is (= 7 (get-in (json/read-json-file output)
                     ["observations" "command-passed" "details" "exit_code"])))))

(deftest descriptor-key-set-is-closed
  (let [repo (git-repo)
        output (fs/file (temp-dir "abc-evidence-capture-invalid") "bundle.json")]
    (is (thrown? clojure.lang.ExceptionInfo
                 (capture/capture!
                  {:repo-root repo
                   :descriptor (assoc (descriptor ["true"]) :unexpected true)
                   :output output})))
    (is (not (fs/exists? output)))))

(deftest descriptor-version-and-runtime-manifest-contract-is-closed
  (let [repo (git-repo)
        output (fs/file (temp-dir "abc-evidence-capture-v2") "bundle.json")
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
                   (capture/capture! {:repo-root repo :descriptor value
                                      :descriptor-path path :output output}))))))

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
    (let [result (capture/capture! {:repo-root repo :descriptor descriptor
                                    :descriptor-path descriptor-path :output output})
          inputs (get-in result [:bundle "inputs"])]
      (is (= 0 (:exit-code result)))
      (is (contains? inputs test-path))
      (is (contains? inputs "src/example/core.clj"))
      (is (contains? inputs descriptor-path))
      (is (contains? inputs manifest-path)))))
