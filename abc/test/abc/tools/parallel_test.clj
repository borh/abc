(ns abc.tools.parallel-test
  (:require [abc.tools.parallel :as parallel]
            [clojure.test :refer [deftest is testing]]))

(def ^:dynamic *probe* :root)

(deftest ordered-pmap-preserves-input-order-test
  (testing "later items finish first, results still follow coll order"
    (let [coll (vec (range 8))
          result (parallel/ordered-pmap
                  4
                  (fn [i] (Thread/sleep (long (- 80 (* 10 i)))) (* i i))
                  coll)]
      (is (= (mapv #(* % %) coll) result))
      (is (vector? result)))))

(deftest ordered-pmap-concurrency-one-is-sequential-mapv-test
  (let [thread-names (parallel/ordered-pmap
                      1
                      (fn [_] (.getName (Thread/currentThread)))
                      (range 4))]
    (is (= (repeat 4 (.getName (Thread/currentThread)))
           (seq thread-names))
        "concurrency 1 must run on the calling thread, exactly like mapv")))

(deftest ordered-pmap-actually-runs-concurrently-test
  (testing "4 tasks of ~150ms on 4 threads finish well under 4x150ms"
    (let [start (System/nanoTime)
          _ (parallel/ordered-pmap 4 (fn [_] (Thread/sleep 150)) (range 4))
          elapsed-ms (/ (- (System/nanoTime) start) 1e6)]
      (is (< elapsed-ms 450.0)
          (str "expected concurrent execution, took " elapsed-ms "ms")))))

(deftest ordered-pmap-unwraps-execution-exception-test
  (let [thrown (try
                 (parallel/ordered-pmap
                  2
                  (fn [i] (if (= 3 i) (throw (ex-info "boom" {:i i})) i))
                  (range 5))
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
    (is (some? thrown) "the original ExceptionInfo must propagate")
    (is (= "boom" (ex-message thrown)))
    (is (= {:i 3} (ex-data thrown))
        "ex-data must survive: callers dispatch on it (source-bundle-admission-error)")))

(deftest ordered-pmap-conveys-dynamic-bindings-test
  (testing "worker threads see the caller's bindings (tests rebind *derive-parser-ir!*)"
    (binding [*probe* :bound]
      (is (= [:bound :bound :bound]
             (parallel/ordered-pmap 3 (fn [_] *probe*) (range 3)))))))

(deftest ordered-pmap-empty-coll-test
  (is (= [] (parallel/ordered-pmap 4 inc []))))

(deftest ordered-pmap-shuts-down-its-executor-test
  (testing "repeated calls do not accumulate pool threads"
    (let [live-threads #(count (Thread/getAllStackTraces))
          baseline (live-threads)]
      (dotimes [_ 3]
        (parallel/ordered-pmap 4 identity (range 8)))
      (Thread/sleep 200) ; let terminated pool threads unwind
      (is (< (- (live-threads) baseline) 4)
          "3 runs x 4 threads must not leak (a leak would add ~12 threads)"))))
