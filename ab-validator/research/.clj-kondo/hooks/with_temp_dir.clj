(ns hooks.with-temp-dir
  (:require [clj-kondo.hooks-api :as api]))

(defn with-temp-dir
  "Teach clj-kondo that (with-temp-dir [binding & _opts] body...) binds
  `binding` (covers both abc.test-fs/with-temp-dir and
  babashka.fs/with-temp-dir). Rewrites to (let [binding nil] body...)."
  [{:keys [node]}]
  (let [[bindings & body] (rest (:children node))
        binding-sym (first (:children bindings))]
    {:node (api/list-node
            (list*
             (api/token-node 'let)
             (api/vector-node [binding-sym (api/token-node nil)])
             body))}))
