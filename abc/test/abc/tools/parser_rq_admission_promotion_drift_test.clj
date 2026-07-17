(ns abc.tools.parser-rq-admission-promotion-drift-test
  (:require [abc.tools.files :as files]
            [abc.tools.parser-rq-campaign :as campaign]
            [clojure.test :refer [deftest is]]))

(def fixture
  {:capture_count 1
   :core_attempt_maxima {:fatal_failures 2.0 :wall_time_seconds 12.5 :timeouts 1.0}
   :evaluation_registry_transition [:unadmitted :admitted]
   :predicate_count 9
   :replica_count 2
   :selection "sole-authorized-capture-and-unique-current-registry-evaluation"})

(deftest bounded-transaction-summary-is-byte-stable
  (is (= fixture
         (files/read-edn
          "test/fixtures/parser-rq/admission-promotion/summary.edn")))
  (is (= 9 (count campaign/predicate-ids)))
  (is (= 1 (:capture_count fixture)))
  (is (= [:unadmitted :admitted]
         (:evaluation_registry_transition fixture))))

(deftest mutations-break-the-bounded-transaction
  (doseq [mutated [(assoc fixture :capture_count 2)
                   (assoc fixture :predicate_count 8)
                   (assoc fixture :replica_count 1)
                   (assoc fixture :evaluation_registry_transition [:admitted])]]
    (is (not= fixture mutated))))
