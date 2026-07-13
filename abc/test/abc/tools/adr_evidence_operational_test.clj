(ns abc.tools.adr-evidence-operational-test
  (:require [abc.tools.adr-evidence-observation-catalog :as catalog]
            [abc.tools.adr-evidence-operational :as operational]
            [abc.tools.hash :as hash]
            [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-root []
  (fs/file (fs/create-temp-dir {:prefix "adr-operational-test-"})))

(defn- write! [root path body]
  (let [file (fs/file root path)]
    (fs/create-dirs (fs/parent file))
    (spit file body)
    file))

(defn- sha256 [file]
  (hash/format-sha256 (hash/sha256-file file)))

(defn- problem-kinds [exception]
  (set (map :kind (:problems (ex-data exception)))))

(defn- thrown-info [thunk]
  (try
    (thunk)
    nil
    (catch clojure.lang.ExceptionInfo exception exception)))

(defn- catalog-value [row]
  {:schema-version :abc-adr-evidence-observation-catalog-v1
   :focused-observations []
   :operational-observations [row]})

(def clojure-row
  {:observation-id :operation
   :descriptor-stem "operation"
   :observation-key "operation-passes"
   :command-id :operation
   :tool "bash"
   :argv ["bash" "-c" "true"]
   :environment-policy :nix-local-v1
   :entrypoint-kind :clojure
   :entrypoint-namespaces ['abc.operation]
   :determinant-paths ["flake.lock" "flake.nix"]})

(def nix-row
  (-> clojure-row
      (assoc :observation-id :tei-profile-drift
             :descriptor-stem "tei-profile-drift"
             :observation-key "tei-profile-derived-artifacts-match"
             :command-id :tei-profile-drift
             :entrypoint-kind :nix-only
             :determinant-paths ["flake.lock" "flake.nix" "nix/tei-profile-artifacts.nix"])
      (dissoc :entrypoint-namespaces)))

(defn- operational-descriptor [row explicit]
  (cond->
   {:schema-version "abc-adr-evidence-capture-operational-v1"
    :tool (:tool row)
    :argv (:argv row)
    :entrypoint-kind (name (:entrypoint-kind row))
    :catalog-path "data/catalog.edn"
    :observation-id (:observation-id row)
    :observation-contract-sha256 (catalog/observation-contract-sha256 row)
    :input-set-mode "exact-v1"
    :input-profile {:kind "repo-files-v1" :roots [] :explicit explicit}
    :observation-key (:observation-key row)}
    (= :clojure (:entrypoint-kind row))
    (assoc :clojure-closure-manifest "docs/evidence/adr-inputs/operation.edn")))

(defn- prepare-operational-root! [row]
  (let [root (temp-root)
        descriptor-path (str "docs/evidence/adr-capture/" (:descriptor-stem row) ".edn")]
    (doseq [path (:determinant-paths row)] (write! root path path))
    (write! root "data/catalog.edn" (pr-str (catalog-value row)))
    {:root root :descriptor-path descriptor-path}))

(deftest deterministic-namespace-closure-expands-only-ns-requires-test
  (let [root (temp-root)
        marker (fs/file root "reader-eval-ran")]
    (write! root "test/abc/operation.cljc"
            (str "(ns abc.operation\n"
                 "  (:require [abc.lib [alpha :as alpha] [beta]]\n"
                 "            [abcdef.external :as external]\n"
                 "            [clojure.string :as str]))\n"
                 "#=(spit \"" marker "\" \"bad\")\n"
                 "(require 'abc.dynamic)\n"))
    (write! root "src/abc/lib/alpha.clj"
            "(ns abc.lib.alpha (:require [abc.shared :as shared]))\n")
    (write! root "src/abc/lib/beta.cljc" "(ns abc.lib.beta)\n")
    (write! root "src/abc/shared.clj" "(ns abc.shared)\n")
    (write! root "src/abc/dynamic.clj" "(ns abc.dynamic)\n")
    (is (= ["src/abc/lib/alpha.clj"
            "src/abc/lib/beta.cljc"
            "src/abc/shared.clj"
            "test/abc/operation.cljc"]
           (operational/derive-namespace-closure root ['abc.operation])))
    (is (not (fs/exists? marker)))))

(deftest owned-namespace-resolution-fails-closed-test
  (testing "every owned namespace must resolve"
    (let [error (thrown-info #(operational/derive-namespace-closure
                               (temp-root) ['abc.missing]))]
      (is (= #{:missing-operational-namespace} (problem-kinds error)))))
  (testing "all four candidates participate and duplicates are ambiguous"
    (doseq [[a b] [["src/abc/duplicate.clj" "test/abc/duplicate.clj"]
                   ["src/abc/duplicate.clj" "src/abc/duplicate.cljc"]]]
      (let [root (temp-root)]
        (write! root a "(ns abc.duplicate)\n")
        (write! root b "(ns abc.duplicate)\n")
        (is (= #{:ambiguous-operational-namespace}
               (problem-kinds
                (thrown-info #(operational/derive-namespace-closure
                               root ['abc.duplicate]))))))))
  (testing "a candidate symlink escape fails before any source is read"
    (let [root (temp-root)
          outside (Files/createTempFile "escaped-ns-" ".clj"
                                        (make-array FileAttribute 0))]
      (spit (.toFile outside) "(ns abc.escape)\n")
      (fs/create-dirs (fs/file root "src/abc"))
      (Files/createSymbolicLink (.toPath (fs/file root "src/abc/escape.clj"))
                                outside (make-array FileAttribute 0))
      (is (= #{:invalid-operational-namespace-coordinate}
             (problem-kinds
              (thrown-info #(operational/derive-namespace-closure
                             root ['abc.escape])))))))
  (testing "the declared namespace must equal the requested namespace"
    (let [root (temp-root)]
      (write! root "src/abc/requested.clj" "(ns abc.different)\n")
      (is (= #{:invalid-operational-namespace}
             (problem-kinds
              (thrown-info #(operational/derive-namespace-closure
                             root ['abc.requested]))))))))

(deftest descriptor-versions-are-closed-discriminated-shapes-test
  (let [root (temp-root)
        path "docs/evidence/adr-capture/focused.edn"
        base {:tool "bin/kaocha"
              :argv ["bin/kaocha" "--focus" "abc.example-test/focus"]
              :input-profile {:kind "clojure-test-v1" :roots ["abc.example-test"]
                              :explicit [path]}
              :observation-key "focused-passes"}]
    (doseq [descriptor [(assoc base
                               :schema-version "abc-adr-evidence-capture-v1"
                               :catalog-path "data/catalog.edn")
                        (assoc base
                               :schema-version "abc-adr-evidence-capture-v2"
                               :runtime-input-manifest
                               "docs/evidence/adr-inputs/focused.edn"
                               :input-profile
                               {:kind "clojure-test-v1"
                                :roots ["abc.example-test"]
                                :explicit [path
                                           "docs/evidence/adr-inputs/focused.edn"
                                           "bin/kaocha"]}
                               :catalog-path "data/catalog.edn")]]
      (write! root path (pr-str descriptor))
      (is (= #{:invalid-evidence-descriptor}
             (problem-kinds
              (thrown-info #(operational/load-descriptor-context!
                             {:repo-root root :descriptor-path path}))))))
    (write! root path (pr-str (assoc base
                                     :schema-version "abc-adr-evidence-capture-v3"
                                     :catalog-path "data/catalog.edn"
                                     :observation-id :focused
                                     :observation-contract-sha256
                                     (str "sha256:" (apply str (repeat 64 "0"))))))
    (is (= #{:invalid-evidence-descriptor}
           (problem-kinds
            (thrown-info #(operational/load-descriptor-context!
                           {:repo-root root :descriptor-path path})))))))

(deftest focused-v3-loads-a-closed-catalog-bound-context-test
  (let [root (temp-root)
        path "docs/evidence/adr-capture/focused.edn"
        manifest-path "docs/evidence/adr-inputs/focused.edn"
        row {:observation-id :focused
             :descriptor-stem "focused"
             :observation-key "focused-passes"
             :focus-var 'abc.example-test/focus}
        descriptor {:schema-version "abc-adr-evidence-capture-v3"
                    :tool "bin/kaocha"
                    :argv ["bin/kaocha" "--focus" "abc.example-test/focus"]
                    :input-profile {:kind "clojure-test-v1"
                                    :roots ["abc.example-test"]
                                    :explicit [path manifest-path "bin/kaocha"]}
                    :runtime-input-manifest manifest-path
                    :catalog-path "data/catalog.edn"
                    :observation-id :focused
                    :observation-contract-sha256
                    (catalog/observation-contract-sha256 row)
                    :observation-key "focused-passes"}]
    (let [runner (write! root "bin/kaocha" "#!/usr/bin/env bash\n")]
      (.setExecutable runner true))
    (write! root path (pr-str descriptor))
    (write! root manifest-path
            (pr-str {:schema-version :abc-adr-runtime-inputs-v1 :paths []}))
    (write! root "data/catalog.edn"
            (pr-str {:schema-version :abc-adr-evidence-observation-catalog-v1
                     :focused-observations [row]
                     :operational-observations []}))
    (let [context (operational/load-descriptor-context!
                   {:repo-root root :descriptor-path path})]
      (is (= "abc-adr-evidence-capture-v3" (:descriptor-version context)))
      (is (= row (:catalog-row context))))))

(deftest focused-v3-rejects-adversarial-runner-and-argv-test
  (let [root (temp-root)
        path "docs/evidence/adr-capture/focused.edn"
        manifest-path "docs/evidence/adr-inputs/focused.edn"
        row {:observation-id :focused
             :descriptor-stem "focused"
             :observation-key "focused-passes"
             :focus-var 'abc.example-test/focus}
        base {:schema-version "abc-adr-evidence-capture-v3"
              :tool "bin/kaocha"
              :argv ["bin/kaocha" "--focus" "abc.example-test/focus"]
              :input-profile {:kind "clojure-test-v1"
                              :roots ["abc.example-test"]
                              :explicit [path manifest-path "bin/kaocha"]}
              :runtime-input-manifest manifest-path
              :catalog-path "data/catalog.edn"
              :observation-id :focused
              :observation-contract-sha256
              (catalog/observation-contract-sha256 row)
              :observation-key "focused-passes"}]
    (let [runner (write! root "bin/kaocha" "#!/usr/bin/env bash\n")]
      (.setExecutable runner true))
    (write! root manifest-path
            (pr-str {:schema-version :abc-adr-runtime-inputs-v1 :paths []}))
    (write! root "data/catalog.edn"
            (pr-str {:schema-version :abc-adr-evidence-observation-catalog-v1
                     :focused-observations [row]
                     :operational-observations []}))
    (doseq [[label changed]
            [[:shell (assoc base :tool "bash"
                            :argv ["bash" "-c" "abc.example-test/focus"])]
             [:extra-prefix
              (assoc base :argv ["bin/kaocha" "--focus" "abc.other-test/other"
                                 "--focus" "abc.example-test/focus"])]
             [:extra-option
              (assoc base :argv ["bin/kaocha" "--randomize" "false"
                                 "--focus" "abc.example-test/focus"])]
             [:runner-unbound
              (update-in base [:input-profile :explicit]
                         #(vec (remove #{"bin/kaocha"} %)))]
             [:non-executable base]]]
      (testing (name label)
        (when (= label :non-executable)
          (.setExecutable (fs/file root "bin/kaocha") false))
        (write! root path (pr-str changed))
        (is (= #{:invalid-focused-evidence-runner}
               (problem-kinds
                (thrown-info #(operational/load-descriptor-context!
                               {:repo-root root :descriptor-path path})))))))))

(deftest focused-v3-runtime-manifest-coordinate-is-bound-before-context-test
  (let [root (temp-root)
        path "docs/evidence/adr-capture/focused.edn"
        manifest-path "docs/evidence/adr-inputs/focused.edn"
        row {:observation-id :focused
             :descriptor-stem "focused"
             :observation-key "focused-passes"
             :focus-var 'abc.example-test/focus}
        base {:schema-version "abc-adr-evidence-capture-v3"
              :tool "bin/kaocha"
              :argv ["bin/kaocha" "--focus" "abc.example-test/focus"]
              :input-profile {:kind "clojure-test-v1"
                              :roots ["abc.example-test"]
                              :explicit [path manifest-path "bin/kaocha"]}
              :runtime-input-manifest manifest-path
              :catalog-path "data/catalog.edn"
              :observation-id :focused
              :observation-contract-sha256
              (catalog/observation-contract-sha256 row)
              :observation-key "focused-passes"}]
    (let [runner (write! root "bin/kaocha" "#!/usr/bin/env bash\n")]
      (.setExecutable runner true))
    (write! root manifest-path
            (pr-str {:schema-version :abc-adr-runtime-inputs-v1 :paths []}))
    (write! root "docs/evidence/adr-inputs/other.edn"
            (pr-str {:schema-version :abc-adr-runtime-inputs-v1 :paths []}))
    (let [outside (fs/file (temp-root) "outside-inputs")]
      (fs/create-dirs outside)
      (write! outside "focused.edn"
              (pr-str {:schema-version :abc-adr-runtime-inputs-v1 :paths []}))
      (Files/createSymbolicLink
       (.toPath (fs/file root "docs/evidence/adr-inputs/escape"))
       (.toPath outside)
       (make-array FileAttribute 0)))
    (write! root "data/catalog.edn"
            (pr-str {:schema-version :abc-adr-evidence-observation-catalog-v1
                     :focused-observations [row]
                     :operational-observations []}))
    (doseq [[label changed]
            [[:missing (assoc base :runtime-input-manifest
                              "docs/evidence/adr-inputs/missing.edn")]
             [:traversal (assoc base :runtime-input-manifest "../focused.edn")]
             [:wrong-stem (assoc base :runtime-input-manifest
                                 "docs/evidence/adr-inputs/other.edn")]
             [:symlink-escape
              (assoc base :runtime-input-manifest
                     "docs/evidence/adr-inputs/escape/focused.edn")]
             [:unbound (update-in base [:input-profile :explicit]
                                  #(vec (remove #{manifest-path} %)))]]]
      (testing (name label)
        (write! root path (pr-str changed))
        (is (= #{:invalid-runtime-input-manifest}
               (problem-kinds
                (thrown-info #(operational/load-descriptor-context!
                               {:repo-root root :descriptor-path path})))))))))

(deftest operational-clojure-manifest-and-exact-input-policy-test
  (let [{:keys [root descriptor-path]} (prepare-operational-root! clojure-row)
        closure-path "docs/evidence/adr-inputs/operation.edn"
        required ["deps-lock.json" "deps.edn" descriptor-path closure-path
                  "flake.lock" "flake.nix" "src/abc/operation.clj" "tests.edn"]
        descriptor (operational-descriptor clojure-row required)]
    (doseq [path ["deps.edn" "deps-lock.json" "tests.edn"]] (write! root path path))
    (write! root "src/abc/operation.clj" "(ns abc.operation)\n")
    (write! root closure-path
            (pr-str {:schema-version :abc-adr-operational-closure-v1
                     :owned-namespace-prefixes ['abc]
                     :entrypoint-namespaces ['abc.operation]
                     :paths ["src/abc/operation.clj"]}))
    (write! root descriptor-path (pr-str descriptor))
    (let [context (-> (operational/load-descriptor-context!
                       {:repo-root root :descriptor-path descriptor-path})
                      operational/validate-operational-manifest!)]
      (is (= required (vec (:required-inputs context)))))
    (doseq [[label changed]
            [[:wrong-mode (assoc descriptor :input-set-mode "minimum-v1")]
             [:missing-manifest (dissoc descriptor :clojure-closure-manifest)]
             [:wrong-manifest-stem
              (assoc descriptor :clojure-closure-manifest
                     "docs/evidence/adr-inputs/other.edn")]
             [:extra-branch-field (assoc descriptor :entrypoint-namespaces ['abc.operation])]
             [:catalog-mismatch (assoc descriptor :argv ["bash" "-c" "false"])]
             [:wrong-hash (assoc descriptor :observation-contract-sha256
                                 (str "sha256:" (apply str (repeat 64 "f"))))]
             [:missing-explicit (update-in descriptor [:input-profile :explicit]
                                           #(vec (remove #{"tests.edn"} %)))]
             [:extra-explicit (update-in descriptor [:input-profile :explicit]
                                         conj "extra.txt")]]]
      (testing (name label)
        (when (= label :extra-explicit) (write! root "extra.txt" "extra"))
        (when (= label :wrong-manifest-stem)
          (write! root "docs/evidence/adr-inputs/other.edn"
                  (pr-str {:schema-version :abc-adr-operational-closure-v1
                           :owned-namespace-prefixes ['abc]
                           :entrypoint-namespaces ['abc.operation]
                           :paths ["src/abc/operation.clj"]})))
        (write! root descriptor-path (pr-str changed))
        (is (some? (thrown-info
                    #(-> (operational/load-descriptor-context!
                          {:repo-root root :descriptor-path descriptor-path})
                         operational/validate-operational-manifest!))))))))

(deftest operational-manifest-rejects-unsorted-and-unsafe-coordinates-test
  (let [{:keys [root descriptor-path]} (prepare-operational-root! clojure-row)
        closure-path "docs/evidence/adr-inputs/operation.edn"
        required ["deps-lock.json" "deps.edn" descriptor-path closure-path
                  "flake.lock" "flake.nix" "src/abc/operation.clj" "tests.edn"]]
    (doseq [path ["deps.edn" "deps-lock.json" "tests.edn"]] (write! root path path))
    (write! root "src/abc/operation.clj" "(ns abc.operation)\n")
    (write! root descriptor-path (pr-str (operational-descriptor clojure-row required)))
    (doseq [manifest [{:schema-version :abc-adr-operational-closure-v1
                       :owned-namespace-prefixes ['abc]
                       :entrypoint-namespaces ['abc.operation]
                       :paths ["z.clj" "a.clj"]}
                      {:schema-version :abc-adr-operational-closure-v1
                       :owned-namespace-prefixes ['abc]
                       :entrypoint-namespaces ['abc.operation]
                       :paths ["../operation.clj"]}]]
      (write! root closure-path (pr-str manifest))
      (is (= #{:invalid-operational-closure}
             (problem-kinds
              (thrown-info
               #(-> (operational/load-descriptor-context!
                     {:repo-root root :descriptor-path descriptor-path})
                    operational/validate-operational-manifest!))))))))

(deftest closure-manifest-coordinate-is-rejected-before-namespace-reading-test
  (let [{:keys [root descriptor-path]} (prepare-operational-root! clojure-row)
        closure-path "docs/evidence/adr-inputs/operation.edn"
        explicit ["deps-lock.json" "deps.edn" descriptor-path closure-path
                  "flake.lock" "flake.nix" "tests.edn"]]
    (doseq [path ["deps.edn" "deps-lock.json" "tests.edn"]] (write! root path path))
    (write! root descriptor-path
            (pr-str (operational-descriptor clojure-row explicit)))
    (is (= #{:invalid-operational-closure}
           (problem-kinds
            (thrown-info
             #(-> (operational/load-descriptor-context!
                   {:repo-root root :descriptor-path descriptor-path})
                  operational/validate-operational-manifest!)))))))

(deftest nix-only-exactness-has-no-invented-clojure-inputs-test
  (let [{:keys [root descriptor-path]} (prepare-operational-root! nix-row)
        required (vec (sort (conj (set (:determinant-paths nix-row)) descriptor-path)))
        descriptor (operational-descriptor nix-row required)]
    (write! root descriptor-path (pr-str descriptor))
    (let [context (-> (operational/load-descriptor-context!
                       {:repo-root root :descriptor-path descriptor-path})
                      operational/validate-operational-manifest!)]
      (is (= required (vec (:required-inputs context))))
      (is (not-any? #{"deps.edn" "deps-lock.json" "tests.edn"}
                    (:required-inputs context))))
    (doseq [changed [(assoc descriptor :clojure-closure-manifest "invented.edn")
                     (assoc descriptor :entrypoint-namespaces ['abc.invented])]]
      (write! root descriptor-path (pr-str changed))
      (is (= #{:invalid-evidence-descriptor}
             (problem-kinds
              (thrown-info #(operational/load-descriptor-context!
                             {:repo-root root :descriptor-path descriptor-path}))))))))

(deftest descriptor-and-catalog-coordinates-are-validated-before-reading-test
  (let [root (temp-root)]
    (doseq [path ["../descriptor.edn" (str (fs/file root "absolute.edn"))]]
      (is (= #{:invalid-evidence-descriptor-path}
             (problem-kinds
              (thrown-info #(operational/load-descriptor-context!
                             {:repo-root root :descriptor-path path}))))))
    (let [{:keys [root descriptor-path]} (prepare-operational-root! nix-row)
          descriptor (assoc (operational-descriptor nix-row [])
                            :catalog-path "../catalog.edn")]
      (write! root descriptor-path (pr-str descriptor))
      (is (= #{:invalid-observation-catalog-path}
             (problem-kinds
              (thrown-info #(operational/load-descriptor-context!
                             {:repo-root root :descriptor-path descriptor-path}))))))))

(deftest offline-policy-accumulates-exact-input-and-hash-problems-test
  (let [{:keys [root descriptor-path]} (prepare-operational-root! nix-row)
        required (vec (sort (conj (set (:determinant-paths nix-row)) descriptor-path)))
        descriptor (operational-descriptor nix-row required)
        _ (write! root descriptor-path (pr-str descriptor))
        inputs (into {} (map (fn [path] [path (sha256 (fs/file root path))]) required))
        bundle {"schema_version" "abc-adr-evidence-run-v1"
                "input_profile" {"kind" "repo-files-v1" "roots" []
                                 "explicit" required}
                "inputs" (-> inputs
                             (dissoc descriptor-path)
                             (assoc "extra.txt" (str "sha256:" (apply str (repeat 64 "0")))))}
        problems (operational/offline-policy-problems
                  {:repo-root root :workspace-root root
                   :artifact-path "docs/evidence/adr-run/tei-profile-drift.json"
                   :bundle bundle})]
    (is (vector? problems))
    (is (contains? (set (map :kind problems)) :operational-input-set-mismatch))
    (is (contains? (set (map :kind problems)) :missing-evidence-input))
    (is (= problems (vec (sort-by pr-str problems))))))

(deftest offline-policy-never-executes-the-catalog-command-test
  (let [marker (fs/file (temp-root) "must-not-exist")
        row (assoc nix-row :argv ["bash" "-c" (str "touch " marker)])
        {:keys [root descriptor-path]} (prepare-operational-root! row)
        required (vec (sort (conj (set (:determinant-paths row)) descriptor-path)))
        descriptor (operational-descriptor row required)
        _ (write! root descriptor-path (pr-str descriptor))
        inputs (into {} (map (fn [path] [path (sha256 (fs/file root path))]) required))]
    (is (empty?
         (operational/offline-policy-problems
          {:repo-root root :workspace-root root
           :artifact-path "docs/evidence/adr-run/tei-profile-drift.json"
           :bundle {"input_profile" {"kind" "repo-files-v1" "roots" []
                                     "explicit" required}
                    "inputs" inputs}})))
    (is (not (fs/exists? marker)))))
