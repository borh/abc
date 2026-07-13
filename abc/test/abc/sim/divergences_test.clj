(ns abc.sim.divergences-test
  (:require [abc.sim.divergences :as div]
            [abc.tools.adr-evidence-runtime-inputs :as runtime]
            [abc.tools.evidence-test-support :as evidence-support]
            [clojure.test :refer [deftest is]]))

(deftest table-covers-spec-entries-test
  (is (= #{:D1 :D2 :D3 :D4 :D5 :D6 :D7} (set (keys div/table))))
  (is (every? #(contains? #{:open :adjudicated-bug :adjudicated-intended :fixed}
                          (:status %))
              (vals div/table))))

(deftest d7-pin-chain-fix-is-dated-and-structural-test
  (runtime/with-validated-read-trace!
    (evidence-support/focused-trace-options "adr-0033-c7-d7-dated-fixed-state")
    (fn []
      (is (= :fixed (get-in div/table [:D7 :status])))
      (is (re-find #"2026-07-12" (get-in div/table [:D7 :notes])))
      (is (re-find #"archive, bundle, member, and primary-text identity"
                   (get-in div/table [:D7 :notes]))))))

(deftest expected-failure-inverts-while-open-test
  ;; Inversion semantics are pinned against a synthetic open entry so this
  ;; test stays valid as the real divergences get fixed.
  (with-redefs [div/table {:DX {:case "demo" :status :open}}]
    (is (true? (div/expected-failure* :DX "demo" (fn [] false))))
    (is (false? (div/expected-failure* :DX "demo" (fn [] true))))))

(deftest harness-exceptions-and-unknown-ids-escape-test
  (is (thrown? clojure.lang.ExceptionInfo (div/open? :D99)))
  (is (thrown? clojure.lang.ExceptionInfo
               (div/expected-failure* :D99 "demo" (fn [] false))))
  ;; a throwing desired-fn is a harness defect, not an expected failure
  (is (thrown? IllegalStateException
               (div/expected-failure* :D1 "demo" #(throw (IllegalStateException. "boom"))))))
