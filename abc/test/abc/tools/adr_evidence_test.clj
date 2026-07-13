(ns abc.tools.adr-evidence-test
  (:require [abc.tools.adr-evidence :as evidence]
            [abc.tools.adr-evidence-bundle :as bundle]
            [clojure.test :refer [deftest is]]
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

(def ^:private artifact-hash
  (str "sha256:" (apply str (repeat 64 "a"))))

(defn- claim
  ([] (claim "ADR-0034-C1" :structural-invariant))
  ([claim-id claim-kind]
   {:claim-id claim-id
    :claim-kind claim-kind
    :file "0034-typed-evidence.md"
    :criterion-index 0}))

(defn- valid-entry []
  {:claim-id "ADR-0034-C1"
   :claim-kind :structural-invariant
   :evidence-kind :structural-test
   :artifact-path "docs/evidence/adr-runs/governance.json"
   :artifact-hash artifact-hash
   :observation-key "typed-evidence-contract"
   :expected {:operator := :value true}})

(defn- valid-bundle []
  {"schema_version" "abc-adr-evidence-run-v1"
   "observations" {"typed-evidence-contract" {"value" true}}})

(defn- validate
  ([claims entries]
   (validate claims entries (valid-bundle)))
  ([claims entries artifact]
   (with-redefs [bundle/validate-bundle
                 (fn [_repo _path _hash _affected]
                   {:bundle artifact :problems []})]
     (evidence/validate-registry
      {:repo-root "."
       :claims claims
       :registry {:entries entries}
       :matrix expected-matrix
       :as-of "2026-07-12"}))))

(defn- kinds [problems]
  (set (map :kind problems)))

(deftest checked-in-evidence-contract-test
  (is (= expected-matrix (evidence/load-matrix)))
  (is (= {:entries []} (evidence/load-registry)))
  (is (= "2026-07-12" (evidence/load-as-of))))

(deftest artifact-backed-entry-contract-is-closed
  (is (empty? (validate [(claim)] [(valid-entry)])))
  (doseq [forbidden [:observed :inputs :verdict]]
    (is (contains? (kinds (validate [(claim)]
                                    [(assoc (valid-entry) forbidden true)]))
                   :forbidden-inline-evidence-value)))
  (is (contains? (kinds (validate [(claim)]
                                  [(assoc (valid-entry) :unexpected true)]))
                 :invalid-evidence-entry))
  (is (contains? (kinds (validate [(claim)]
                                  [(dissoc (valid-entry) :artifact-path)]))
                 :invalid-evidence-entry)))

(deftest registry-joins-claims-coverage-and-artifacts
  (is (contains? (kinds (validate [(claim)] []))
                 :missing-claim-evidence))
  (is (contains? (kinds (validate [(claim)]
                                  [(valid-entry) (valid-entry)]))
                 :duplicate-evidence-entry))
  (is (contains? (kinds (validate [(claim)]
                                  [(assoc (valid-entry)
                                          :claim-kind :fixture-behavior)]))
                 :claim-kind-mismatch))
  (is (contains? (kinds (validate [(claim)]
                                  [(assoc (valid-entry)
                                          :evidence-kind :unknown)]))
                 :unknown-evidence-kind))
  (is (contains? (kinds (validate [(claim)]
                                  [(assoc (valid-entry)
                                          :evidence-kind :benchmark)]))
                 :incompatible-evidence-kind))
  (is (contains? (kinds (validate [(claim)] [(valid-entry)]
                                  {"schema_version" "abc-adr-evidence-run-v1"
                                   "observations" {}}))
                 :missing-observation)))

(deftest every-corroborating-entry-must-be-valid
  (let [second-entry (assoc (valid-entry)
                            :artifact-path "docs/evidence/adr-runs/second.json"
                            :artifact-hash (str "sha256:" (apply str (repeat 64 "b"))))]
    (is (empty? (validate [(claim)] [(valid-entry) second-entry])))
    (is (contains? (kinds (validate [(claim)]
                                    [(valid-entry)
                                     (assoc second-entry :evidence-kind :benchmark)]))
                   :incompatible-evidence-kind))))

(deftest shared-artifact-root-problem-is-emitted-once
  (let [claims [(claim "ADR-0034-C1" :structural-invariant)
                (claim "ADR-0034-C2" :structural-invariant)]
        entries [(valid-entry)
                 (assoc (valid-entry) :claim-id "ADR-0034-C2")]
        problems (with-redefs [bundle/validate-bundle
                               (fn [_repo _path _hash affected]
                                 {:bundle nil
                                  :problems [{:kind :artifact-hash-mismatch
                                              :affected-claim-ids affected}]})]
                   (evidence/validate-registry
                    {:repo-root "." :claims claims
                     :registry {:entries entries}
                     :matrix expected-matrix :as-of "2026-07-12"}))]
    (is (= 1 (count (filter #(= :artifact-hash-mismatch (:kind %)) problems))))
    (is (= ["ADR-0034-C1" "ADR-0034-C2"]
           (:affected-claim-ids
            (first (filter #(= :artifact-hash-mismatch (:kind %)) problems)))))))

(deftest typed-predicate-results-distinguish-failure-from-type-error
  (is (= {:status :pass}
         (evidence/evaluate-result {:operator :> :value 1} 2)))
  (is (= {:status :fail}
         (evidence/evaluate-result {:operator :> :value 2} 1)))
  (is (= {:status :type-error}
         (evidence/evaluate-result {:operator :> :value 1} "2")))
  (is (= {:status :type-error}
         (evidence/evaluate-result {:operator :> :value 1}
                                   9007199254740992)))
  (is (= {:status :type-error}
         (evidence/evaluate-result {:operator :contains :value "x"} "x")))
  (is (= {:status :pass}
         (evidence/evaluate-result {:operator :set= :value [2 1]} [1 2]))))

(deftest benchmark-observation-requires-declared-scaled-integer-unit
  (let [benchmark-claim (claim "ADR-0034-C1" :performance-bound)
        benchmark-entry (assoc (valid-entry)
                               :claim-kind :performance-bound
                               :evidence-kind :benchmark
                               :expected {:operator :<= :value 100})
        artifact {"schema_version" "abc-adr-evidence-run-v1"
                  "observations" {"typed-evidence-contract"
                                  {"value" 50 "details" {}}}}]
    (is (contains? (kinds (validate [benchmark-claim]
                                    [benchmark-entry] artifact))
                   :invalid-evidence-artifact))))

(deftest external-expiry-uses-bundle-date-and-explicit-as-of
  (let [external-claim (claim "ADR-0034-C1" :external-semantics)
        external-entry (assoc (valid-entry)
                              :claim-kind :external-semantics
                              :evidence-kind :external-authority)
        artifact {"schema_version" "abc-adr-external-evidence-v1"
                  "review_after" "2026-07-11"
                  "observations" {"typed-evidence-contract" {"value" true}}}]
    (is (contains? (kinds (validate [external-claim]
                                    [external-entry] artifact))
                   :expired-evidence))))

(deftest evaluate-result-agrees-with-clojure-ordering-property-test
  (let [result (tc/quick-check
                200
                (prop/for-all
                 [a gen/small-integer b gen/small-integer]
                 (and (= {:status (if (= a b) :pass :fail)}
                         (evidence/evaluate-result {:operator := :value b} a))
                      (= {:status (if (< a b) :pass :fail)}
                         (evidence/evaluate-result {:operator :< :value b} a))
                      (= {:status (if (<= a b) :pass :fail)}
                         (evidence/evaluate-result {:operator :<= :value b} a))
                      (= {:status (if (> a b) :pass :fail)}
                         (evidence/evaluate-result {:operator :> :value b} a))
                      (= {:status (if (>= a b) :pass :fail)}
                         (evidence/evaluate-result {:operator :>= :value b} a)))))]
    (is (true? (:pass? result)) (pr-str result))))
