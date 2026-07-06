(ns abc.owl-test
  (:require [abc.owl :refer :all]
            [tawny
             [owl :as o]
             [reasoner :as r]]
            [clojure.test :as t :refer [deftest testing is use-fixtures]]
            [clojure.spec.alpha :as s]
            [orchestra.spec.test :as st]
            [clojure.spec.test.alpha :as stest]
            [expound.alpha :as expound]
            [me.raynes.fs :as fs]))

;; (alter-var-root #'s/*explain-out* (constantly expound/printer))
(comment

  (st/instrument)

  (stest/check (stest/enumerate-namespace 'abc.owl))

  (def ^:dynamic ^:private *ontology-filename* nil)

  (defn ontology-reasoner-fixture [tests]
    ;; this should kill the reasoner factory and all reasoners which is the
    ;; safest, but slowest way to start.
    (r/reasoner-factory :hermit)

    ;; inject the pizzaontology into the current namespace, which saves the
    ;; hassle of using with ontology every where. set this up each time in case
    ;; pizzaontology has been re-evaled
    (o/ontology-to-namespace blue-skies-ontology)
    (binding [r/*reasoner-progress-monitor*
              (atom r/reasoner-progress-monitor-silent)
              *ontology-filename* (fs/temp-name "ontology" ".omn")]
      (tests)))

  (use-fixtures :once ontology-reasoner-fixture)

  (deftest consistency-test
    (is (r/coherent?))
    (is (r/consistent?)))

  (deftest serialization-test
    (is (o/save-ontology *ontology-filename* :omn))
    (is (load-ontology-from-file *ontology-filename*))))
