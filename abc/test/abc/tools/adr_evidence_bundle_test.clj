(ns abc.tools.adr-evidence-bundle-test
  (:require [abc.tools.adr-evidence-bundle :as bundle]
            [abc.tools.hash :as hash]
            [abc.tools.json :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir []
  (.toFile (Files/createTempDirectory
            "abc-adr-evidence-bundle-test" (make-array FileAttribute 0))))

(defn- write-path! [root path body]
  (let [file (io/file root path)]
    (.mkdirs (.getParentFile file))
    (spit file body)
    file))

(defn- file-hash [file]
  (hash/format-sha256 (hash/sha256-file file)))

(defn- run-bundle [input-path input-hash]
  {"schema_version" "abc-adr-evidence-run-v1"
   "producer" {"tool" "test"
               "command" "test"
               "revision" "0000000000000000000000000000000000000000"}
   "input_profile" {"kind" "repo-files-v1"
                    "roots" []
                    "explicit" [input-path]}
   "inputs" {input-path input-hash}
   "observations" {"contract" {"value" true
                               "details" {"tests" 1}}}})

(deftest loads-and-validates-canonical-artifact-identity
  (let [repo (temp-dir)
        input (write-path! repo "fixtures/input.txt" "input")
        value (run-bundle "fixtures/input.txt" (file-hash input))
        artifact (io/file repo "docs/evidence/run.json")
        canonical-hash (hash/format-sha256 (hash/sha256-json-jcs value))]
    (json/write-deterministic-json-file! artifact value)
    (is (= {:value value :canonical-hash canonical-hash}
           (bundle/load-bundle repo "docs/evidence/run.json")))
    (is (= {:bundle value :problems []}
           (bundle/validate-bundle repo "docs/evidence/run.json"
                                   canonical-hash ["ADR-0042-C2" "ADR-0042-C1"])))
    (is (= true (bundle/observation value "contract")))
    (is (nil? (bundle/observation value "missing")))
    (let [problems (:problems
                    (bundle/validate-bundle
                     repo "docs/evidence/run.json"
                     (hash/format-sha256 (apply str (repeat 64 "f")))
                     ["ADR-0042-C2" "ADR-0042-C1"]))]
      (is (= [:artifact-hash-mismatch] (mapv :kind problems)))
      (is (= ["ADR-0042-C1" "ADR-0042-C2"]
             (:affected-claim-ids (first problems)))))))

(deftest bundle-input-profile-shapes-are-closed-test
  (let [base (run-bundle "fixtures/input.txt" (str "sha256:" (apply str (repeat 64 "0"))))]
    (is (seq (bundle/validate-bundle-value
              (assoc-in base ["input_profile" "component_root"] "abc"))))
    (is (seq (bundle/validate-bundle-value
              (assoc base "input_profile"
                     {"kind" "component-clojure-test-v1"
                      "roots" [] "explicit" []}))))))

(deftest artifact-path-and-input-failures-are-root-causes
  (let [repo (temp-dir)
        input (write-path! repo "fixtures/input.txt" "input")
        value (run-bundle "fixtures/input.txt" (file-hash input))
        artifact (io/file repo "docs/evidence/run.json")]
    (json/write-deterministic-json-file! artifact value)
    (let [artifact-hash (hash/format-sha256 (hash/sha256-json-jcs value))]
      (is (= [:missing-evidence-artifact]
             (mapv :kind (:problems
                          (bundle/validate-bundle repo "docs/evidence/missing.json"
                                                  artifact-hash ["ADR-0042-C1"])))))
      (is (= [:evidence-path-traversal]
             (mapv :kind (:problems
                          (bundle/validate-bundle repo "../run.json"
                                                  artifact-hash ["ADR-0042-C1"])))))
      (spit input "changed")
      (let [problems (:problems
                      (bundle/validate-bundle repo "docs/evidence/run.json"
                                              artifact-hash ["ADR-0042-C1"]))]
        (is (= [:input-hash-mismatch] (mapv :kind problems)))))))

(deftest derives-static-clojure-namespace-closure-plus-explicit-inputs
  (let [repo (temp-dir)]
    (write-path! repo "test/example/core_test.clj"
                 "(ns example.core-test (:require [example.core :as core]))\n(require 'example.dynamic)\n")
    (write-path! repo "src/example/core.clj"
                 "(ns example.core (:require [example.helper :refer [value]]))\n")
    (write-path! repo "src/example/helper.clj"
                 "(ns example.helper (:require [example.core :as core]))\n(def value 1)\n")
    (write-path! repo "src/example/dynamic.clj" "(ns example.dynamic)\n")
    (write-path! repo "fixtures/example.json" "{}")
    (is (= (sorted-set "fixtures/example.json"
                       "src/example/core.clj"
                       "src/example/helper.clj"
                       "test/example/core_test.clj")
           (bundle/derive-minimum-inputs
            repo {:kind "clojure-test-v1"
                  :roots ["example.core-test"]
                  :explicit ["fixtures/example.json"]})))
    (testing "in-body require and runtime data are explicit boundaries"
      (is (not (contains?
                (bundle/derive-minimum-inputs
                 repo {:kind "clojure-test-v1"
                       :roots ["example.core-test"]
                       :explicit []})
                "src/example/dynamic.clj")))
      (is (contains?
           (bundle/derive-minimum-inputs
            repo {:kind "clojure-test-v1"
                  :roots ["example.core-test"]
                  :explicit ["src/example/dynamic.clj"]})
           "src/example/dynamic.clj")))
    (testing "a declared root must resolve"
      (let [exception (try
                        (bundle/derive-minimum-inputs
                         repo {:kind "clojure-test-v1"
                               :roots ["example.missing-test"]
                               :explicit []})
                        nil
                        (catch clojure.lang.ExceptionInfo exception exception))]
        (is (= :missing-evidence-input (:kind (ex-data exception))))))
    (testing "reader evaluation is disabled while reading the ns form"
      (let [marker (io/file repo "reader-eval-ran")]
        (write-path! repo "test/example/unsafe_test.clj"
                     (str "(ns example.unsafe-test (:require #=(do (spit \""
                          (.getAbsolutePath marker)
                          "\" \"bad\") '[example.core])))\n"))
        (is (thrown? RuntimeException
                     (bundle/derive-minimum-inputs
                      repo {:kind "clojure-test-v1"
                            :roots ["example.unsafe-test"]
                            :explicit []})))
        (is (not (.exists marker)))))))

(deftest derives-component-clojure-inputs-relative-to-workspace
  (let [repo (temp-dir)]
    (write-path! repo "abc/test/example/core_test.clj"
                 "(ns example.core-test (:require [example.core]))\n")
    (write-path! repo "abc/src/example/core.clj" "(ns example.core)\n")
    (write-path! repo "ab-validator/docs/report.md" "report\n")
    (is (= (sorted-set "abc/test/example/core_test.clj"
                       "abc/src/example/core.clj"
                       "ab-validator/docs/report.md")
           (bundle/derive-minimum-inputs
            repo {:kind "component-clojure-test-v1"
                  :component-root "abc"
                  :roots ["example.core-test"]
                  :explicit ["ab-validator/docs/report.md"]})))))

(deftest ordinary-and-component-bundles-resolve-against-separate-roots-test
  (let [workspace (temp-dir)
        abc (io/file workspace "abc")
        ordinary-input (write-path! abc "src/example/core.clj" "(ns example.core)\n")
        test-input (write-path! workspace "abc/test/example/core_test.clj"
                                "(ns example.core-test (:require [example.core]))\n")
        sibling (write-path! workspace "ab-validator/docs/report.md" "report\n")
        ordinary (run-bundle "src/example/core.clj" (file-hash ordinary-input))
        component {"schema_version" "abc-adr-evidence-run-v1"
                   "producer" {"tool" "test" "command" "test" "revision" "revision"}
                   "input_profile" {"kind" "component-clojure-test-v1"
                                    "component_root" "abc"
                                    "roots" ["example.core-test"]
                                    "explicit" ["ab-validator/docs/report.md"]}
                   "inputs" {"abc/src/example/core.clj" (file-hash ordinary-input)
                             "abc/test/example/core_test.clj" (file-hash test-input)
                             "ab-validator/docs/report.md" (file-hash sibling)}
                   "observations" {"contract" {"value" true}}}
        ordinary-path "docs/evidence/ordinary.json"
        component-path "docs/evidence/component.json"]
    (json/write-deterministic-json-file! (io/file abc ordinary-path) ordinary)
    (json/write-deterministic-json-file! (io/file abc component-path) component)
    (is (empty? (:problems
                 (bundle/validate-bundle
                  {:artifact-root abc :workspace-root workspace}
                  ordinary-path (hash/format-sha256 (hash/sha256-json-jcs ordinary)) []))))
    (is (empty? (:problems
                 (bundle/validate-bundle
                  {:artifact-root abc :workspace-root workspace}
                  component-path (hash/format-sha256 (hash/sha256-json-jcs component)) []))))
    (is (seq (:problems
              (bundle/validate-bundle
               {:artifact-root abc :workspace-root nil}
               component-path (hash/format-sha256 (hash/sha256-json-jcs component)) []))))
    (spit sibling "drift\n")
    (is (= [[:input-hash-mismatch "ab-validator/docs/report.md"]]
           (mapv (juxt :kind :input-path)
                 (:problems
                  (bundle/validate-bundle
                   {:artifact-root abc :workspace-root workspace}
                   component-path (hash/format-sha256 (hash/sha256-json-jcs component)) [])))))))

(deftest missing-derived-namespace-input-is-reported
  (let [repo (temp-dir)
        root (write-path! repo "test/example/core_test.clj"
                          "(ns example.core-test (:require [example.core]))\n")
        core (write-path! repo "src/example/core.clj"
                          "(ns example.core (:require [example.helper]))\n")
        _helper (write-path! repo "src/example/helper.clj" "(ns example.helper)\n")
        value {"schema_version" "abc-adr-evidence-run-v1"
               "producer" {"tool" "test" "command" "test" "revision" "revision"}
               "input_profile" {"kind" "clojure-test-v1"
                                "roots" ["example.core-test"]
                                "explicit" []}
               "inputs" {"test/example/core_test.clj" (file-hash root)
                         "src/example/core.clj" (file-hash core)}
               "observations" {"contract" {"value" true}}}
        artifact (io/file repo "docs/evidence/run.json")
        artifact-hash (hash/format-sha256 (hash/sha256-json-jcs value))]
    (json/write-deterministic-json-file! artifact value)
    (is (= [[:missing-evidence-input "src/example/helper.clj"]]
           (mapv (juxt :kind :input-path)
                 (:problems (bundle/validate-bundle
                             repo "docs/evidence/run.json" artifact-hash
                             ["ADR-0042-C1"])))))))

(deftest external-summary-hash-is-independently-recomputed
  (let [repo (temp-dir)
        summary (write-path! repo "docs/evidence/authority-summary.md" "summary")
        actual-hash (file-hash summary)
        wrong-hash (hash/format-sha256 (apply str (repeat 64 "f")))
        value {"schema_version" "abc-adr-external-evidence-v1"
               "source_url" "https://example.invalid/authority"
               "retrieved_at" "2026-07-12"
               "review_after" "2027-07-12"
               "summary" {"path" "docs/evidence/authority-summary.md"
                          "hash" wrong-hash}
               "input_profile" {"kind" "external-authority-v1"
                                "explicit" []}
               "inputs" {"docs/evidence/authority-summary.md" actual-hash}
               "observations" {"source-contract" {"value" "documented"}}}
        artifact (io/file repo "docs/evidence/external.json")
        artifact-hash (hash/format-sha256 (hash/sha256-json-jcs value))]
    (json/write-deterministic-json-file! artifact value)
    (is (= [[:input-hash-mismatch "docs/evidence/authority-summary.md"]]
           (mapv (juxt :kind :input-path)
                 (:problems (bundle/validate-bundle
                             repo "docs/evidence/external.json" artifact-hash
                             ["ADR-0042-C1"])))))))

(deftest external-evidence-rejects-impossible-calendar-dates
  (let [value {"schema_version" "abc-adr-external-evidence-v1"
               "source_url" "https://example.invalid/authority"
               "retrieved_at" "2026-07-12"
               "review_after" "2026-13-40"
               "summary" {"path" "docs/evidence/authority-summary.md"
                          "hash" (hash/format-sha256 (apply str (repeat 64 "0")))}
               "input_profile" {"kind" "external-authority-v1"
                                "explicit" []}
               "inputs" {}
               "observations" {"source-contract" {"value" "documented"}}}]
    (is (= [:invalid-evidence-artifact]
           (mapv :kind (bundle/validate-bundle-value value))))))
(deftest component-profile-rejects-workspace-root-as-the-component-test
  (is (thrown? clojure.lang.ExceptionInfo
               (bundle/derive-minimum-inputs
                "."
                {"kind" "component-clojure-test-v1"
                 "component_root" "."
                 "roots" []
                 "explicit" []}))))
