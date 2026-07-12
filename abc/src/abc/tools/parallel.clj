(ns abc.tools.parallel
  "Bounded, ordered parallel mapping for independent per-work corpus loops."
  (:import [java.util.concurrent ExecutionException Executors Future]))

(defn ordered-pmap
  "Like (mapv f coll), running f on a fixed pool of `concurrency` threads.
  Results preserve coll order. concurrency <= 1 is exactly (mapv f coll) on
  the calling thread. Dynamic bindings at the call site convey to worker
  threads (per-work loops are exercised in tests under a rebound
  *derive-parser-ir!*). When any f call throws, the original throwable
  propagates (ExecutionException unwrapped) after all tasks have completed;
  in-flight work is not cancelled."
  [concurrency f coll]
  (if (<= concurrency 1)
    (mapv f coll)
    (let [executor (Executors/newFixedThreadPool concurrency)]
      (try
        (->> (.invokeAll executor
                         ^java.util.Collection
                         (mapv (fn [item] (bound-fn* #(f item))) coll))
             (mapv (fn [^Future fut]
                     (try
                       (.get fut)
                       (catch ExecutionException e
                         (throw (or (.getCause e) e)))))))
        (finally
          (.shutdown executor))))))
