(ns hooks.sim-binding
  (:require [clj-kondo.hooks-api :as api]))

(defn with-symbol-bindings
  "Teach clj-kondo that the leading vector names locals for the body.

  Simulation resource macros such as with-repo and with-sidecar accept a
  vector of symbols, rather than let-style binding pairs. Rewrite that shape
  to a let which binds each symbol to nil for static analysis only."
  [{:keys [node]}]
  (let [[bindings & body] (rest (:children node))
        let-bindings (mapcat #(vector % (api/token-node nil))
                             (:children bindings))]
    {:node (api/list-node
            (list*
             (api/token-node 'let)
             (api/vector-node let-bindings)
             body))}))
