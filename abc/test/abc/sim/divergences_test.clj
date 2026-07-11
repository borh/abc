(ns abc.sim.divergences-test
  (:require [abc.sim.divergences :as div]
            [clojure.test :refer [deftest is]]))

(deftest table-covers-spec-entries-test
  (is (= #{:D1 :D2 :D3 :D4 :D5 :D6} (set (keys div/table))))
  (is (every? #(contains? #{:open :adjudicated-bug :adjudicated-intended}
                          (:status %))
              (vals div/table))))

(deftest expected-failure-inverts-while-open-test
  ;; :D1 is open: a FALSE desired-check passes, a TRUE desired-check fails.
  (is (true? (div/expected-failure* :D1 "demo" (fn [] false))))
  (is (false? (div/expected-failure* :D1 "demo" (fn [] true)))))

(deftest harness-exceptions-and-unknown-ids-escape-test
  (is (thrown? clojure.lang.ExceptionInfo (div/open? :D99)))
  (is (thrown? clojure.lang.ExceptionInfo
               (div/expected-failure* :D99 "demo" (fn [] false))))
  ;; a throwing desired-fn is a harness defect, not an expected failure
  (is (thrown? IllegalStateException
               (div/expected-failure* :D1 "demo" #(throw (IllegalStateException. "boom"))))))
