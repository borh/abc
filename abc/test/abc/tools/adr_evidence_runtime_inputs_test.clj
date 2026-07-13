(ns abc.tools.adr-evidence-runtime-inputs-test
  (:require [abc.tools.adr-evidence-runtime-inputs :as runtime]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir []
  (.toFile (Files/createTempDirectory "runtime-inputs-" (make-array FileAttribute 0))))

(defn- write! [root path body]
  (let [file (io/file root path)]
    (.mkdirs (.getParentFile file))
    (spit file body)
    file))

(defn- analyzer-repo [body]
  (let [root (temp-dir)]
    (write! root "src/example/core.clj" (str "(ns example.core)\n" body "\n"))
    (write! root "test/.keep" "")
    root))

(defn- problem-kind [thunk]
  (try (thunk) nil (catch Exception e (:kind (ex-data e)))))

(deftest runtime-input-closure-is-exact-test
  (let [root (temp-dir)
        descriptor-path "docs/evidence/adr-capture/example.edn"
        manifest-path "docs/evidence/adr-inputs/example.edn"
        descriptor {:schema-version "abc-adr-evidence-capture-v2"
                    :runtime-input-manifest manifest-path
                    :input-profile {:kind "clojure-test-v1"
                                    :roots []
                                    :explicit [descriptor-path manifest-path "data/value.edn"]}}]
    (write! root descriptor-path (pr-str descriptor))
    (write! root manifest-path
            (pr-str {:schema-version :abc-adr-runtime-inputs-v1
                     :paths ["data/value.edn"]}))
    (write! root "data/value.edn" "{}")
    (is (true? (runtime/assert-runtime-input-closure!
                {:repo-root root :workspace-root root
                 :descriptor {:path descriptor-path :value descriptor}
                 :repository-paths [descriptor-path manifest-path "data/value.edn"]})))
    (is (= :undeclared-runtime-input
           (:kind (ex-data
                   (try
                     (runtime/assert-runtime-input-closure!
                      {:repo-root root :workspace-root root
                       :descriptor {:path descriptor-path :value descriptor}
                       :repository-paths [descriptor-path manifest-path]})
                     (catch Exception e e))))))))

(deftest runtime-manifest-rejects-noncanonical-vectors-test
  (let [root (temp-dir)
        descriptor-path "docs/evidence/adr-capture/example.edn"
        manifest-path "docs/evidence/adr-inputs/example.edn"
        descriptor {:schema-version "abc-adr-evidence-capture-v2"
                    :runtime-input-manifest manifest-path
                    :input-profile {:kind "clojure-test-v1" :roots []
                                    :explicit [descriptor-path manifest-path "data/a" "data/b"]}}]
    (write! root descriptor-path "descriptor")
    (write! root "data/a" "a")
    (write! root "data/b" "b")
    (write! root manifest-path
            (pr-str {:schema-version :abc-adr-runtime-inputs-v1
                     :paths ["data/b" "data/a"]}))
    (is (= :invalid-runtime-input-manifest
           (:kind (ex-data
                   (try
                     (runtime/assert-runtime-input-closure!
                      {:repo-root root :workspace-root root
                       :descriptor {:path descriptor-path :value descriptor}
                       :repository-paths []})
                     (catch Exception e e))))))))

(deftest runtime-manifest-rejects-every-noncontained-path-shape-test
  (let [root (temp-dir)
        outside (temp-dir)
        descriptor-path "docs/evidence/adr-capture/example.edn"
        manifest-path "docs/evidence/adr-inputs/example.edn"
        outside-file (write! outside "outside.txt" "outside")
        link (io/file root "linked.txt")]
    (.mkdirs (.getParentFile (io/file root manifest-path)))
    (Files/createSymbolicLink (.toPath link) (.toPath outside-file)
                              (make-array java.nio.file.attribute.FileAttribute 0))
    (doseq [[label paths]
            [["absolute" [(.getAbsolutePath outside-file)]]
             ["parent" ["../outside.txt"]]
             ["missing" ["missing.txt"]]
             ["duplicate" ["linked.txt" "linked.txt"]]
             ["symlink escape" ["linked.txt"]]]]
      (write! root manifest-path
              (pr-str {:schema-version :abc-adr-runtime-inputs-v1 :paths paths}))
      (let [descriptor {:schema-version "abc-adr-evidence-capture-v2"
                        :runtime-input-manifest manifest-path
                        :input-profile {:kind "clojure-test-v1" :roots []
                                        :explicit (into [descriptor-path manifest-path] paths)}}]
        (is (contains? #{:invalid-runtime-input-manifest :missing-runtime-input}
                       (problem-kind
                        #(runtime/validate-runtime-input-manifest!
                          {:repo-root root :workspace-root root
                           :descriptor {:path descriptor-path :value descriptor}})))
            label)))))

(deftest reachable-var-analysis-expands-finite-parameter-contract-test
  (let [result (runtime/analyze-reachable-vars
                "." ['abc.tools.manifest/content])]
    (is (some #{'abc.tools.manifest/content} (:reachable-vars result)))
    (is (some #{'abc.tools.hash/sha256-file} (:reachable-vars result)))
    (is (= ["data/evidence-higher-order-calls/manifest-content.edn"]
           (:contract-paths result)))
    (is (some #{"src/abc/tools/manifest.clj"} (:paths result)))))

(deftest reachable-var-analysis-uses-private-adr-caller-contract-test
  (let [result (runtime/analyze-reachable-vars
                "." ['abc.tools.adr/validate-repository*])]
    (is (some #{'abc.tools.adr/validate-adrs} (:reachable-vars result)))
    (is (= ["data/evidence-higher-order-calls/adr-validate-repository-star.edn"]
           (:contract-paths result)))))

(deftest reachable-var-lint-is-narrow-and-default-deny-test
  (let [root (analyzer-repo
              "(defn unsafe [] (slurp \"secret\"))\n(defn threaded-unsafe [] (-> \"secret\" slurp))\n(defn safe [] (-> 1 inc inc))\n(defn value-only [] slurp)")]
    (is (= :forbidden-evidence-io
           (problem-kind #(runtime/analyze-reachable-vars
                           root ['example.core/unsafe]))))
    (is (= ['example.core/safe]
           (:reachable-vars
            (runtime/analyze-reachable-vars root ['example.core/safe]))))
    (is (= ['example.core/value-only]
           (:reachable-vars
            (runtime/analyze-reachable-vars root ['example.core/value-only]))))
    (is (= :forbidden-evidence-io
           (problem-kind #(runtime/analyze-reachable-vars
                           root ['example.core/threaded-unsafe]))))))

(deftest reachable-var-lint-rejects-computed-and-unresolved-heads-test
  (let [computed (analyzer-repo "(defn bad [f] ((identity f) :x))")
        local-call (analyzer-repo "(defn bad [] (let [f identity] (f :x)))")]
    (is (= :unsupported-evidence-call-graph
           (problem-kind #(runtime/analyze-reachable-vars computed ['example.core/bad]))))
    (is (= :reader-kondo-span-mismatch
           (problem-kind #(runtime/analyze-reachable-vars local-call ['example.core/bad]))))))

(deftest reachable-var-lint-denies-each-physical-bypass-family-test
  (doseq [[label body]
          [["reader" "(defn bad [] (clojure.java.io/reader \"x\"))"]
           ["stream" "(defn bad [] (clojure.java.io/input-stream \"x\"))"]
           ["file-reader" "(defn bad [] (java.io.FileReader. \"x\"))"]
           ["nio" "(defn bad [] (java.nio.file.Files/readAllBytes (java.nio.file.Path/of \"x\" (make-array String 0))))"]
           ["directory" "(defn bad [] (.listFiles (java.io.File. \".\")))"]
           ["archive" "(defn bad [] (java.util.zip.ZipFile. \"x\"))"]
           ["process" "(defn bad [] (java.lang.ProcessBuilder. [\"true\"]))"]
           ["shell" "(defn bad [] (clojure.java.shell/sh \"true\"))"]
           ["network" "(defn bad [] (java.net.URL. \"https://example.invalid\"))"]]]
    (let [root (analyzer-repo body)]
      (is (= :forbidden-evidence-io
             (problem-kind #(runtime/analyze-reachable-vars root ['example.core/bad])))
          label))))

(deftest higher-order-contract-shape-and-targets-fail-closed-test
  (let [root (temp-dir)
        source "(ns abc.tools.manifest)\n(defn target [x] x)\n(defn content [f] (f :x))\n"
        path "data/evidence-higher-order-calls/manifest-content.edn"
        valid {:schema-version :abc-evidence-higher-order-call-v1
               :caller 'abc.tools.manifest/content
               :parameters {'f ['abc.tools.manifest/target]}}]
    (write! root "src/abc/tools/manifest.clj" source)
    (write! root "test/.keep" "")
    (is (= :invalid-higher-order-contract
           (problem-kind #(runtime/analyze-reachable-vars
                           root ['abc.tools.manifest/content]))))
    (doseq [[label value expected]
            [["wrong caller" (assoc valid :caller 'abc.tools.manifest/other)
              :invalid-higher-order-contract]
             ["missing target" (assoc-in valid [:parameters 'f]
                                         ['abc.tools.manifest/absent])
              :unresolved-focused-var]
             ["unknown parameter" (assoc valid :parameters {'other ['abc.tools.manifest/target]})
              :invalid-higher-order-contract]]]
      (write! root path (pr-str value))
      (is (= expected
             (problem-kind #(runtime/analyze-reachable-vars
                             root ['abc.tools.manifest/content])))
          label))
    (write! root path (pr-str valid))
    (is (= ['abc.tools.manifest/content 'abc.tools.manifest/target]
           (:reachable-vars
            (runtime/analyze-reachable-vars root ['abc.tools.manifest/content]))))))

(deftest duplicate-focused-definition-fails-closed-test
  (let [root (analyzer-repo "(defn duplicate [] true)\n(defn duplicate [] false)")]
    (is (= :duplicate-var-definition
           (problem-kind #(runtime/analyze-reachable-vars
                           root ['example.core/duplicate]))))))

(deftest nix-clojure-closure-manifest-binds-the-derived-graph-test
  (let [result (runtime/analyze-reachable-vars "." ['abc.tools.manifest/content])
        manifest-path "target/runtime-input-test-closure.edn"
        manifest-file (io/file manifest-path)
        inputs (concat ["deps.edn" "deps-lock.json" "tests.edn" manifest-path]
                       (:paths result) (:contract-paths result))]
    (.mkdirs (.getParentFile manifest-file))
    (spit manifest-file
          (pr-str {:schema-version :abc-adr-nix-clojure-closure-v1
                   :focused-vars ['abc.tools.manifest/content]
                   :paths (:paths result)}))
    (try
      (is (true? (runtime/validate-nix-clojure-closure! "." manifest-path inputs)))
      (is (= :missing-evidence-input
             (problem-kind #(runtime/validate-nix-clojure-closure!
                             "." manifest-path (remove #{"deps.edn"} inputs)))))
      (finally (.delete manifest-file)))))
