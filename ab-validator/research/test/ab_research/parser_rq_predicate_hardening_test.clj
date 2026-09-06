(ns ab-research.parser-rq-predicate-hardening-test
  (:require [ab-research.parser-release-qualification :as qualification]
            [ab-research.parser-rq-predicate-hardening :as hardening]
            [clojure.test :refer [deftest is testing]]))

(def hash-a (str "sha256:" (apply str (repeat 64 "a"))))
(def hash-b (str "sha256:" (apply str (repeat 64 "b"))))
(def hash-c (str "sha256:" (apply str (repeat 64 "c"))))
(def corpus-hash (str "sha256:" (apply str (repeat 64 "d"))))

(def qualification-identity
  {:corpus_list_hash corpus-hash
   :candidate "fixture-candidate"
   :instrument_versions
   {:diagnostic_envelope_completeness hash-a
    :parser_ir_schema_conformance hash-b}})

(defn- result
  [instrument policy-hash value]
  {:instrument instrument
   :policy_hash policy-hash
   :corpus_list_hash corpus-hash
   :expected_work_ids ["a" "b"]
   :observation {:value value
                 :identity_ref (qualification/qualification-identity-ref
                                qualification-identity)}})

(def diagnostic-result
  (result :diagnostic-envelope-completeness hash-a 1.0))

(def parser-ir-result
  (result :parser-ir-schema-conformance hash-b 1.0))

(deftest installs-two-closed-coherent-results-without-mutation
  (let [measurements {:fatal_failures {:value 0 :identity_ref hash-c}}
        installed (hardening/install-observations measurements qualification-identity
                                                  diagnostic-result parser-ir-result)]
    (is (= measurements (select-keys installed [:fatal_failures])))
    (is (= (:observation diagnostic-result)
           (:diagnostic_completeness installed)))
    (is (= (:observation parser-ir-result)
           (:parser_ir_schema_validation installed)))
    (is (not (identical? measurements installed)))))

(deftest rejects-fake-seams-and-incoherence-at-construction
  (let [install #(hardening/install-observations {} qualification-identity %1 %2)]
    (testing "scalar, unknown, unavailable, and swapped inputs"
      (is (thrown? clojure.lang.ExceptionInfo (install 1 parser-ir-result)))
      (is (thrown? clojure.lang.ExceptionInfo
                   (install (assoc diagnostic-result :unknown true)
                            parser-ir-result)))
      (is (thrown? clojure.lang.ExceptionInfo
                   (install (assoc-in diagnostic-result [:observation :value]
                                      :unavailable)
                            parser-ir-result)))
      (is (thrown? clojure.lang.ExceptionInfo
                   (install (assoc diagnostic-result :policy_hash hash-b)
                            (assoc parser-ir-result :policy_hash hash-a)))))
    (testing "overwrite, membership, candidate, and corpus mixtures"
      (is (thrown? clojure.lang.ExceptionInfo
                   (hardening/install-observations
                    {:diagnostic_completeness {:value 0}}
                    qualification-identity diagnostic-result parser-ir-result)))
      (is (thrown? clojure.lang.ExceptionInfo
                   (install diagnostic-result
                            (assoc parser-ir-result :expected_work_ids ["a"]))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (install diagnostic-result
                            (assoc parser-ir-result :corpus_list_hash hash-c))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (install diagnostic-result
                            (assoc-in parser-ir-result [:observation :identity_ref]
                                      hash-c)))))))

(deftest composer-is-an-early-guard-and-gate-coherence-is-the-authority
  (let [mixed (assoc-in {:diagnostic_completeness (:observation diagnostic-result)
                         :parser_ir_schema_validation (:observation parser-ir-result)}
                        [:parser_ir_schema_validation :identity_ref]
                        hash-c)]
    (is (false? (qualification/coherent-observations? qualification-identity mixed)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (hardening/install-observations
                  {} qualification-identity diagnostic-result
                  (assoc-in parser-ir-result [:observation :identity_ref] hash-c))))))
