(ns abc.tools.workflow.target
  "Internal target-graph evaluator: realizes a requested target value from a
   Soranoha-owned graph data value using lazy, memoized evaluation with
   conditional (branch) dependencies. Provenance is a DAG value (node set +
   edge list), never an execution sequence. No external dataflow dependency."
  (:require [clojure.set :as set]))

;; ---- Node constructors ---------------------------------------------------

(defn value-node
  "A node whose value is supplied at evaluation time via the `inputs` map."
  []
  {:kind :value})

(defn leaf
  "A node that realizes a value from its resolved dependencies.
   `impl-fn` receives a map {dep-key dep-value} and returns a value or a
   result map (see `passed` / `failed-value` / `skipped`)."
  [deps impl-id impl-fn]
  {:kind :leaf
   :deps (vec deps)
   :impl {:impl/id impl-id :impl/fn impl-fn}})

(defn branch
  "A conditional node: realizes `then-k` when `cond-k` is truthy, else `else-k`.
   Only the taken side is realized."
  [cond-k then-k else-k]
  {:kind :branch :cond cond-k :then then-k :else else-k})

;; ---- Node-result constructors -------------------------------------------

(defn passed
  "Wrap a successful leaf value."
  [value]
  {:result :passed :value value})

(defn failed-value
  "Wrap a domain failure-as-value (admission rejection, diagnostic row, ...)."
  ([value] (failed-value value nil))
  ([value evidence] {:result :failed-value :value value :evidence evidence}))

(defn skipped
  "An explicit local skip a graph could not express as a branch."
  []
  {:result :skipped :value nil})

;; ---- Graph validation ----------------------------------------------------

(defn- node-deps
  "Declared dependency keys of a node, regardless of kind."
  [node]
  (case (:kind node)
    :value  []
    :leaf   (vec (:deps node))
    :branch [(:cond node) (:then node) (:else node)]))

(defn- find-cycle
  "Return a cycle path [k … k] if the declared dependency edges contain one,
   else nil. Deterministic depth-first three-colour walk."
  [graph]
  (let [state  (atom {})   ; k -> :visiting | :done
        result (atom nil)]
    (letfn [(visit [k path]
              (when (nil? @result)
                (case (@state k)
                  :done nil
                  :visiting (reset! result
                                    (conj (vec (drop-while #(not= % k) path)) k))
                  (do (swap! state assoc k :visiting)
                      (doseq [d (filter graph (node-deps (graph k)))]
                        (visit d (conj path k)))
                      (swap! state assoc k :done)))))]
      (doseq [k (keys graph)] (visit k []))
      @result)))

(defn validate-graph
  "Return nil when the graph is a valid DAG whose every referenced dependency
   exists; otherwise a non-empty vector of error maps."
  [graph]
  (let [missing (vec (for [[k node] graph
                           d (node-deps node)
                           :when (not (contains? graph d))]
                       {:type :missing-dependency :node k :dep d}))
        cyc     (find-cycle graph)
        errors  (cond-> missing cyc (conj {:type :cycle :cycle cyc}))]
    (seq errors)))
