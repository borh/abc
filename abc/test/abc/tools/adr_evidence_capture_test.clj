(ns abc.tools.adr-evidence-capture-test
  (:require [abc.tools.adr-evidence-capture :as capture]
            [abc.tools.json :as json]
            [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.test :refer [deftest is]]))

(defn- temp-dir [prefix]
  (fs/file (fs/create-temp-dir {:prefix prefix})))

(defn- exec! [dir & argv]
  (:exit @(process/process (vec argv) {:dir (str dir) :out :string :err :out})))

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

(deftest run-process-captures-working-directory-output-and-status
  (let [run-process (ns-resolve 'abc.tools.adr-evidence-capture 'run-process)
        dir (temp-dir "abc-evidence-capture-process")
        result (@run-process dir
                             ["sh" "-c"
                              "printf %s \"$PWD\"; printf err >&2; exit 4"])]
    (is (= #{:exit-code :stdout :stderr} (set (keys result))))
    (is (= 4 (:exit-code result)))
    (is (= (.getCanonicalPath dir) (:stdout result)))
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
                                            (str "touch " (.getAbsolutePath marker))])
                   :output output})))
    (is (not (.exists marker)))
    (fs/delete (fs/file repo "untracked"))
    (is (thrown? clojure.lang.ExceptionInfo
                 (capture/capture!
                  {:repo-root repo
                   :descriptor (descriptor ["sh" "-c" "touch command-dirtied"])
                   :output output})))
    (is (not (.exists output)))))

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
    (is (not (.exists output)))))

(deftest descriptor-version-and-runtime-manifest-contract-is-closed
  (let [repo (git-repo)
        output (fs/file (temp-dir "abc-evidence-capture-v2") "bundle.json")
        profile {:kind "clojure-test-v1"
                 :roots ["example.core"]
                 :explicit ["docs/evidence/adr-capture/example.edn"
                            "docs/evidence/adr-inputs/example.edn"]}
        v2 {:schema-version "abc-adr-evidence-capture-v2"
            :tool "true" :argv ["true"] :input-profile profile
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

(deftest v2-capture-lints-the-focused-var-and-binds-its-closed-inputs-test
  (let [repo (git-repo)
        descriptor-path "docs/evidence/adr-capture/example.edn"
        manifest-path "docs/evidence/adr-inputs/example.edn"
        test-path "test/example/core_test.clj"
        closure-path "src/abc/tools/adr_evidence_runtime_inputs.clj"
        descriptor {:schema-version "abc-adr-evidence-capture-v2"
                    :tool "true"
                    :argv ["true" "--focus" "example.core-test/runtime-input-contract"]
                    :runtime-input-manifest manifest-path
                    :input-profile {:kind "clojure-test-v1"
                                    :roots ["example.core-test"]
                                    :explicit [descriptor-path manifest-path]}
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
