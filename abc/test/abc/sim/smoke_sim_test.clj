(ns abc.sim.smoke-sim-test
  "Suite wiring smoke test: proves the :simulation suite runs and the
  seed harness is deterministic."
  (:require [abc.sim.harness :as harness]
            [clojure.test :refer [deftest]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(deftest harness-runs-seeded-property-sim-test
  (harness/check! "smoke" 20
                  (prop/for-all [v (gen/vector gen/small-integer)]
                                (= (count v) (count (vec v))))))
