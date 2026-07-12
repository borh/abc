(ns abc.tools.adr-evidence-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(def ^:private expected-matrix
  {:structural-invariant #{:structural-test}
   :fixture-behavior #{:fixture-conformance}
   :corpus-behavior #{:corpus-measurement}
   :performance-bound #{:benchmark}
   :external-semantics #{:external-authority}
   :implementation-agreement #{:cross-implementation}
   :domain-interpretation #{:expert-assessment :external-authority}
   :operational-behavior #{:operational-observation}})

(def ^:private input-hash
  (str "sha256:" (apply str (repeat 64 "a"))))

(defn- api [sym]
  (try
    (requiring-resolve (symbol "abc.tools.adr-evidence" (name sym)))
    (catch java.io.FileNotFoundException _ nil)))

(defn- valid-entry []
  {:claim-id "ADR-0034-C1"
   :claim-kind :structural-invariant
   :evidence-kind :structural-test
   :evidence-path "test/abc/tools/adr_evidence_test.clj"
   :scope {:contract "typed-evidence-v1"}
   :inputs {:source input-hash}
   :expected {:operator := :value true}
   :observed {:value true :inputs {:source input-hash}}})

(defn- problem-kinds [entry]
  (if-let [f (api 'validate-entry)]
    (set (map :kind (f expected-matrix "2026-07-12" entry)))
    #{}))

(deftest checked-in-evidence-contract-test
  (testing "the complete approved compatibility matrix is loadable"
    (is (= expected-matrix
           (when-let [f (api 'load-matrix)] (f)))))
  (testing "the initial registry and deterministic reference clock are loadable"
    (is (= {:entries []}
           (when-let [f (api 'load-registry)] (f))))
    (is (= "2026-07-12"
           (when-let [f (api 'load-as-of)] (f))))))

(deftest evidence-entry-validation-test
  (testing "a compatible passing entry has no problems"
    (is (empty? (problem-kinds (valid-entry)))))
  (testing "claim/evidence types and stored verdicts are closed"
    (is (contains? (problem-kinds (assoc (valid-entry)
                                         :claim-kind :unknown-claim))
                   :unknown-claim-kind))
    (is (contains? (problem-kinds (assoc (valid-entry)
                                         :evidence-kind :benchmark))
                   :incompatible-evidence-kind))
    (is (contains? (problem-kinds (assoc (valid-entry) :verdict :pass))
                   :stored-verdict)))
  (testing "observations must pass and bind the declared inputs"
    (is (contains? (problem-kinds (assoc-in (valid-entry)
                                            [:observed :value] false))
                   :predicate-failed))
    (is (contains? (problem-kinds (assoc-in (valid-entry)
                                            [:observed :inputs :source]
                                            (str "sha256:" (apply str (repeat 64 "b")))))
                   :stale-inputs))))

(deftest external-evidence-expiry-uses-explicit-as-of-test
  (let [entry (-> (valid-entry)
                  (assoc :claim-kind :external-semantics
                         :evidence-kind :external-authority
                         :retrieved-at "2026-01-01"
                         :review-after "2026-07-11"))
        validate-entry (api 'validate-entry)]
    (is (contains? (problem-kinds entry) :expired-evidence))
    (is (some? validate-entry))
    (when validate-entry
      (is (not (contains? (set (map :kind
                                    (validate-entry expected-matrix
                                                    "2026-07-11"
                                                    entry)))
                          :expired-evidence))))))

(deftest evaluate-agrees-with-clojure-ordering-property-test
  (let [evaluate (api 'evaluate)
        result (when evaluate
                 (tc/quick-check
                  200
                  (prop/for-all [a gen/small-integer
                                 b gen/small-integer]
                                (and (= (= a b) (evaluate {:operator := :value b}
                                                          {:value a}))
                                     (= (< a b) (evaluate {:operator :< :value b}
                                                          {:value a}))
                                     (= (<= a b) (evaluate {:operator :<= :value b}
                                                           {:value a}))
                                     (= (> a b) (evaluate {:operator :> :value b}
                                                          {:value a}))
                                     (= (>= a b) (evaluate {:operator :>= :value b}
                                                           {:value a}))))))]
    (is (some? evaluate))
    (is (true? (:pass? result)) (pr-str result))))
