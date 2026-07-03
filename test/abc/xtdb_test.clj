(ns abc.xtdb-test
  (:require [abc.xtdb :as xtdb]
            [clojure.test :refer [deftest is testing]]))

(deftest default-node-starts-explicitly-and-retries-after-failure-test
  (testing "default XTDB node startup is explicit, memoized on success, and retryable after failure"
    (let [calls (atom 0)]
      (with-redefs [xtdb/default-node (atom nil)
                    xtdb/start! (fn [& _opts]
                                  (case (swap! calls inc)
                                    1 (throw (ex-info "transient XTDB startup failure" {}))
                                    ::node))]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"transient XTDB startup failure"
                              (xtdb/node)))
        (is (= 1 @calls))
        (is (= ::node (xtdb/node)))
        (is (= 2 @calls))
        (is (= ::node (xtdb/node)))
        (is (= 2 @calls))))))
