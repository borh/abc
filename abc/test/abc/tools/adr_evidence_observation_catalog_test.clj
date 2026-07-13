(ns abc.tools.adr-evidence-observation-catalog-test
  (:require [abc.tools.adr-evidence-observation-catalog :as catalog]
            [abc.tools.files :as files]
            [abc.tools.jcs :as jcs]
            [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]))

(def focused-row
  {:observation-id :focused-example
   :descriptor-stem "focused-example"
   :observation-key "focused-example-passes"
   :focus-var 'abc.tools.foundation-evidence-test/committed-manifest-schema-conformance-test})

(def operational-row
  {:observation-id :operational-example
   :descriptor-stem "operational-example"
   :observation-key "operational-example-passes"
   :command-id :operational-example
   :tool "bash"
   :argv ["bash" "--noprofile" "--norc" "-c" "printf operational"]
   :environment-policy :nix-local-v1
   :entrypoint-kind :clojure
   :entrypoint-namespaces ['abc.tools.validate-design-bundle]
   :determinant-paths ["determinant.txt"]})

(def foundation-binding-identities
  [["ADR-0001-C1" :adr-0001-c1-committed-manifest-schema]
   ["ADR-0001-C1" :generated-import-manifest-schema]
   ["ADR-0001-C2" :adr-0001-c2-canonical-null-array-order]
   ["ADR-0001-C3" :adr-0001-c3-nested-artifact-id-rejection]
   ["ADR-0001-C4" :adr-0001-c4-failure-semantics]
   ["ADR-0001-C4" :adr-0001-c4-failure-coordinates]
   ["ADR-0001-C4" :adr-0001-c4-failure-artifact-scope]
   ["ADR-0001-C5" :adr-0001-c5-reproducibility-conflict]
   ["ADR-0008-C1" :validate-design-bundle-wrapper-delegation]
   ["ADR-0008-C2" :design-bundle-operational]
   ["ADR-0008-C3" :adr-0008-c3-workflow-wiring]
   ["ADR-0008-C4" :adr-0008-c4-validation-read-catalog]
   ["ADR-0009-C1" :generated-import-manifest-schema]
   ["ADR-0009-C2" :adr-0009-c2-materialized-content-hashes]
   ["ADR-0009-C3" :adr-0009-c3-mapping-divergence-sidecar]
   ["ADR-0009-C4" :adr-0009-c4-generated-artifact-ids-vs-content-hashes]
   ["ADR-0009-C5" :adr-0009-c5-aat-conversion-compatibility]
   ["ADR-0009-C5" :diagnostic-schema-exact-current]
   ["ADR-0009-C6" :adr-0009-c6-temporary-materialization]
   ["ADR-0009-C7" :validate-design-bundle-wrapper-delegation]
   ["ADR-0010-C1" :adr-0010-c1-materialized-bundled-schema-jcs-hash]
   ["ADR-0010-C2" :adr-0010-c2-materialized-artifact-ids-distinct]
   ["ADR-0010-C3" :adr-0010-c3-v0-identity-json]
   ["ADR-0010-C4" :adr-0010-c4-parser-schema-mismatch]
   ["ADR-0010-C4" :diagnostic-schema-exact-current]
   ["ADR-0010-C5" :design-bundle-operational]
   ["ADR-0011-C1" :design-bundle-operational]
   ["ADR-0011-C2" :adr-0011-c2-deterministic-json-writer]
   ["ADR-0011-C3" :adr-0011-c3-two-run-byte-identity]
   ["ADR-0033-C1" :adr-0033-c1-canonical-all-member-schema]
   ["ADR-0033-C2" :adr-0033-c2-clojure-known-answer]
   ["ADR-0033-C3" :adr-0033-c3-repack-image-evolution]
   ["ADR-0033-C4" :adr-0033-c4-parser-identity-roles]
   ["ADR-0033-C5" :adr-0033-c5-role-specific-snapshot-validation]
   ["ADR-0033-C6" :adr-0033-c6-p16-3-ungated]
   ["ADR-0033-C7" :adr-0033-c7-d7-dated-fixed-state]
   ["ADR-0033-C8" :adr-0033-c8-complete-legacy-readability]
   ["ADR-0033-C9" :source-bundle-corpus]
   ["ADR-0033-C10" :adr-0033-c10-admission-limits]
   ["ADR-0033-C11" :adr-0033-c11-strict-atomic-abort]
   ["ADR-0033-C11" :adr-0033-c11-best-effort-counted-failure]
   ["ADR-0033-C11" :adr-0033-c11-non-release-admissible]])

(defn- catalog-value
  ([focused operational]
   {:schema-version :abc-adr-evidence-observation-catalog-v1
    :focused-observations focused
    :operational-observations operational}))

(defn- temp-root []
  (fs/file (fs/create-temp-dir {:prefix "observation-catalog-test-"})))

(defn- problem-kinds [problems]
  (set (map :kind problems)))

(deftest observation-contract-known-answer-is-order-independent-test
  (let [expected-json (str "{\"descriptor_stem\":\"focused-example\","
                           "\"focus_var\":\"abc.tools.foundation-evidence-test/committed-manifest-schema-conformance-test\","
                           "\"observation_id\":\"focused-example\","
                           "\"observation_key\":\"focused-example-passes\","
                           "\"observation_type\":\"focused\","
                           "\"schema_version\":\"abc-adr-evidence-observation-contract-v1\"}")
        expected-digest "sha256:f9492de66eaa225319318694ce7ece66978fc0903f5ebaffd24eba1e82070ed1"
        a (catalog/observation-contract-json-value focused-row)
        b (catalog/observation-contract-json-value
           (into (array-map) (reverse focused-row)))]
    (is (= a b))
    (is (= (vec (jcs/rfc8785-string-domain-json-bytes a))
           (vec (jcs/rfc8785-string-domain-json-bytes b))))
    (is (= expected-json (String. (jcs/rfc8785-string-domain-json-bytes a) "UTF-8")))
    (is (= expected-digest (catalog/observation-contract-sha256 focused-row)))
    (is (re-matches #"sha256:[0-9a-f]{64}"
                    (catalog/observation-contract-sha256 focused-row)))))

(deftest operational-observation-contract-is-a-closed-string-domain-value-test
  (let [value (catalog/observation-contract-json-value operational-row)]
    (is (= "operational" (get value "observation_type")))
    (is (= "operational-example" (get value "command_id")))
    (is (= ["abc.tools.validate-design-bundle"]
           (get value "entrypoint_namespaces")))
    (is (= ["determinant.txt"] (get value "determinant_paths")))
    (is (re-matches #"sha256:[0-9a-f]{64}"
                    (catalog/observation-contract-sha256 operational-row)))))

(deftest closed-catalog-shape-uniqueness-and-ordering-test
  (let [root (temp-root)
        _ (spit (fs/file root "determinant.txt") "fixture\n")
        valid (catalog-value [focused-row] [operational-row])]
    (is (empty? (catalog/catalog-problems root valid)))
    (doseq [[label changed expected]
            [[:catalog-extra (assoc valid :extra true) :invalid-observation-catalog]
             [:catalog-missing (dissoc valid :focused-observations) :invalid-observation-catalog]
             [:focused-extra (assoc-in valid [:focused-observations 0 :extra] true)
              :invalid-focused-observation]
             [:operational-extra (assoc-in valid [:operational-observations 0 :extra] true)
              :invalid-operational-observation]
             [:missing-row-key (update-in valid [:focused-observations 0]
                                          dissoc :observation-id)
              :invalid-focused-observation]
             [:mixed-entrypoints
              (assoc-in valid [:operational-observations 0 :entrypoint-namespaces]
                        ['abc.tools.a "abc.tools.z"])
              :unsorted-operational-collection]
             [:qualified-entrypoint
              (assoc-in valid [:operational-observations 0 :entrypoint-namespaces]
                        ['abc.tools/example])
              :unsorted-operational-collection]
             [:mixed-determinants
              (assoc-in valid [:operational-observations 0 :determinant-paths]
                        ["a" :z])
              :unsorted-operational-collection]
             [:duplicate-id (update valid :operational-observations conj
                                    (assoc operational-row :descriptor-stem "other"
                                           :observation-key "other-passes"))
              :duplicate-observation-id]
             [:duplicate-stem (update valid :operational-observations conj
                                      (assoc operational-row :observation-id :other
                                             :observation-key "other-passes"))
              :duplicate-descriptor-stem]
             [:duplicate-key (update valid :operational-observations conj
                                     (assoc operational-row :observation-id :other
                                            :descriptor-stem "other"))
              :duplicate-observation-key]
             [:duplicate-focus (update valid :focused-observations conj
                                       (assoc focused-row :observation-id :other
                                              :descriptor-stem "other"
                                              :observation-key "other-passes"))
              :duplicate-focus-var]
             [:duplicate-command (update valid :operational-observations conj
                                         (assoc operational-row :observation-id :other
                                                :descriptor-stem "other"
                                                :observation-key "other-passes"))
              :duplicate-command-id]
             [:unsorted-focused (assoc valid :focused-observations
                                       [(assoc focused-row :observation-id :z
                                               :descriptor-stem "z"
                                               :observation-key "z-passes"
                                               :focus-var 'abc.tools.jcs-test/canonical-json-test)
                                        focused-row])
              :unsorted-observations]
             [:unsorted-argv (assoc-in valid [:operational-observations 0 :argv]
                                       '(["bash"]))
              :invalid-operational-observation]
             [:unsorted-entrypoints
              (assoc-in valid [:operational-observations 0 :entrypoint-namespaces]
                        ['abc.tools.z 'abc.tools.a])
              :unsorted-operational-collection]
             [:unsorted-determinants
              (assoc-in valid [:operational-observations 0 :determinant-paths]
                        ["z" "a"])
              :unsorted-operational-collection]
             [:nonnormalized-determinant
              (assoc-in valid [:operational-observations 0 :determinant-paths]
                        ["./determinant.txt"])
              :invalid-operational-determinant]]]
      (testing (name label)
        (is (contains? (problem-kinds (catalog/catalog-problems root changed)) expected))))
    (testing "generic validation has no foundation row-count policy"
      (is (empty? (catalog/catalog-problems root (catalog-value [] [operational-row])))))
    (testing "problems accumulate and have a stable order"
      (let [bad (-> valid
                    (assoc :extra true)
                    (assoc-in [:focused-observations 0 :extra] true)
                    (assoc-in [:operational-observations 0 :determinant-paths]
                              ["missing-b" "missing-a"]))
            problems (catalog/catalog-problems root bad)]
        (is (< 2 (count problems)))
        (is (= problems (vec (sort-by pr-str problems))))))))

(deftest heterogeneous-unknown-row-keys-are-accumulated-deterministically-test
  (let [root (temp-root)
        row (assoc focused-row "string-key" true 42 true)
        problems (catalog/catalog-problems root (catalog-value [row] []))
        shape-problem (first (filter #(= :invalid-focused-observation (:kind %))
                                     problems))]
    (is (vector? problems))
    (is (= (vec (sort-by pr-str ["string-key" 42]))
           (:unknown-keys shape-problem)))))

(deftest catalog-load-validates-coordinate-before-reading-test
  (let [root (temp-root)
        path "catalog.edn"
        value (catalog-value [] [])]
    (spit (fs/file root path) (pr-str value))
    (is (= value (catalog/load-catalog! root path)))
    (doseq [bad ["../catalog.edn" (str (fs/file root path)) "missing.edn"]]
      (let [error (try
                    (catalog/load-catalog! root bad)
                    nil
                    (catch clojure.lang.ExceptionInfo exception exception))]
        (is (some? error))
        (is (vector? (:problems (ex-data error))))))))

(deftest binding-join-rejects-unknown-unbound-and-duplicate-identities-test
  (let [catalog (catalog-value [focused-row] [operational-row])
        binding (fn [claim observation]
                  {:claim-id claim :observation-id observation})
        valid {:entries [(binding "ADR-0001-C1" :focused-example)
                         (binding "ADR-0002-C1" :operational-example)]}]
    (is (empty? (catalog/validate-bindings catalog valid)))
    (is (= #{:unknown-binding-observation :unbound-catalog-observation
             :duplicate-observation-binding}
           (problem-kinds
            (catalog/validate-bindings
             catalog
             {:entries [(binding "ADR-0001-C1" :focused-example)
                        (binding "ADR-0001-C1" :focused-example)
                        (binding "ADR-9999-C1" :unknown)]}))))))

(deftest normalized-conformance-findings-have-stable-semantic-identity-test
  (let [base {:kind :forbidden-evidence-capability
              :focus-var 'abc.example/focus
              :target 'clojure.core/load-string
              :path "src/abc/../abc/example.clj"
              :message "first wording" :line 10 :column 20}
        normalized (catalog/normalize-conformance-findings [base])]
    (is (= normalized
           (catalog/normalize-conformance-findings
            [(assoc base :message "new wording" :line 900 :column 901)])))
    (doseq [changed [(assoc base :kind :forbidden-evidence-io)
                     (assoc base :focus-var 'abc.example/other-focus)
                     (assoc base :target 'clojure.core/eval)
                     (assoc base :path "test/abc/example.clj")]]
      (is (not= normalized
                (catalog/normalize-conformance-findings [changed]))))
    (is (= "src/abc/example.clj" (-> normalized first :path)))
    (is (= normalized
           (catalog/normalize-conformance-findings [base base])))))

(deftest normalized-conformance-findings-reject-open-or-machine-local-identities-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo #"no stable identity projection"
       (catalog/normalize-conformance-findings
        [{:kind :new-unreviewed-problem :focus-var 'abc.example/focus}])))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo #"repository-relatively"
       (catalog/normalize-conformance-findings
        [{:kind :forbidden-evidence-io
          :focus-var 'abc.example/focus
          :target 'java.io.File
          :caller 'abc.example/focus
          :path "/tmp/machine-local.clj"}])))
  (doseq [finding [{:kind :missing-evidence-boundary-owner}
                   {:kind :unresolved-focused-var
                    :focus-var 'abc.example/focus}
                   {:kind :forbidden-evidence-capability
                    :focus-var 'abc.example/focus
                    :target "clojure.core/load-string"
                    :caller 'abc.example/focus}]]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"invalid semantic coordinates"
         (catalog/normalize-conformance-findings [finding])))))

(deftest normalized-conformance-findings-reject-cross-host-machine-paths-test
  (doseq [path ["C:\\tmp\\finding.clj"
                "\\\\server\\share\\finding.clj"]]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"repository-relatively"
         (catalog/normalize-conformance-findings
          [{:kind :forbidden-evidence-io
            :focus-var 'abc.example/focus
            :target 'java.io.File
            :caller 'abc.example/focus
            :path path}])))))

(deftest foundation-catalog-has-separate-37-observation-arithmetic-test
  (let [repo-root (fs/file (fs/canonicalize "."))
        foundation (catalog/load-catalog!
                    repo-root "data/adr-evidence/foundation-observation-catalog.edn")
        bindings {:entries (mapv (fn [[claim-id observation-id]]
                                   {:claim-id claim-id :observation-id observation-id})
                                 foundation-binding-identities)}]
    (is (= 35 (count (:focused-observations foundation))))
    (is (= 2 (count (:operational-observations foundation))))
    (is (= 37 (count (catalog/observation-rows foundation))))
    (is (= 42 (count (:entries bindings))))
    (is (= 35 (count (distinct (map :claim-id (:entries bindings))))))
    (is (= 38 (count (filter #(not (#{:design-bundle-operational
                                      :source-bundle-corpus}
                                    (:observation-id %)))
                             (:entries bindings)))))
    (is (= 4 (count (filter #(#{:design-bundle-operational
                                :source-bundle-corpus}
                              (:observation-id %))
                            (:entries bindings)))))
    (is (empty? (catalog/validate-bindings foundation bindings)))
    (is (= (files/read-edn
            (fs/file repo-root "data/adr-evidence/foundation-capture-conformance-debt.edn"))
           {:schema-version :abc-foundation-capture-conformance-debt-v1
            :findings (catalog/normalize-conformance-findings
                       (catalog/focused-conformance-findings repo-root foundation))}))))
