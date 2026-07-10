(ns abc.tools.diagram.architecture-graph
  "Tier 2 pure builder: architecture-stages.edn -> dataflow graph value, cross-
   checked against schema-contracts.json and the ADR files. See ADR 0029."
  (:require [abc.tools.adr :as adr]
            [abc.tools.diagram.adr-graph :as adr-graph]
            [abc.tools.json :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(def stages-path "docs/architecture-stages.edn")
(def contracts-path "schemas/schema-contracts.json")
(def out-path "docs/architecture.mmd")

(defn load-stages [] (edn/read-string (slurp stages-path)))

(defn schema-paths []
  (->> (get (json/read-json-file contracts-path) "schemas")
       (map #(get % "path")) set))

(defn adr-nums []
  (set (map :num (adr/parse-all adr-graph/adr-dir))))

(defn validate [stages schema-paths adr-nums]
  (let [ids (set (map :id stages))]
    (vec
     (concat
      (for [s stages :when (and (:schema s) (not (contains? schema-paths (:schema s))))]
        (format "stage %s references unknown schema %s" (:id s) (:schema s)))
      (for [s stages n (:adr s) :when (not (contains? adr-nums n))]
        (format "stage %s references non-existent ADR %04d" (:id s) n))
      (for [s stages i (:inputs s) :when (not (contains? ids i))]
        (format "stage %s has unknown input %s" (:id s) i))))))

(defn lint* []
  (validate (:stages (load-stages)) (schema-paths) (adr-nums)))

(defn- stage-label [s]
  (str (:label s) (when (:external s) " ⟨external⟩")
       "<br/>" (str/join ", " (map #(format "ADR %04d" %) (sort (:adr s))))))

(defn graph-from [stages]
  {:direction "TD"
   :nodes (for [s stages]
            {:id (:id s) :label (stage-label s)
             :class (if (:external s) "external" "owned")})
   :edges (for [s stages i (:inputs s)]
            {:from i :to (:id s) :style :solid})
   :class-defs {:owned "fill:#0d47a1,stroke:#90caf9,color:#fff"
                :external "fill:#4e342e,stroke:#bcaaa4,color:#fff"}})

(defn build [] (graph-from (:stages (load-stages))))
