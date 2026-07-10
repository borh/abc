(ns abc.tools.diagram.registry
  "Composition: the committed-diagram registry + CLI entry for `:abc/diagrams`.
   `--check` verifies drift; no arg regenerates. See ADR 0029."
  (:require [abc.tools.diagram.core :as core]
            [abc.tools.diagram.adr-graph :as adr]
            [abc.tools.diagram.architecture-graph :as arch]))

(def committed-diagrams
  [{:id :adr-decision-map :out-path adr/out-path :regen "clojure -M:abc/diagrams"
    :build adr/build :lint adr/lint*}
   {:id :architecture :out-path arch/out-path :regen "clojure -M:abc/diagrams"
    :build arch/build :lint arch/lint*}])

(defn -main [& args]
  (let [{:keys [ok? problems drifts wrote]}
        (core/run! committed-diagrams {:check? (boolean (some #{"--check"} args))})]
    (binding [*out* *err*]
      (doseq [p problems] (println "LINT:" p))
      (doseq [d drifts] (println d)))
    (doseq [w wrote] (println "wrote" w))
    (when (and ok? (empty? wrote)) (println "all diagrams current"))
    (System/exit (if ok? 0 1))))
