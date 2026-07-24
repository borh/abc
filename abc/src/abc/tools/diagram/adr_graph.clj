(ns abc.tools.diagram.adr-graph
  "Pure builder: decisions.edn -> decision-map graph value. See ADR 0029
   and the data-driven-decision-records record."
  (:require [abc.tools.decisions :as decisions]
            [clojure.string :as str]))

(def out-path "docs/adr/adr-graph.mmd")

(def status-class
  {:accepted "accepted" :proposed "proposed" :draft "draft"
   :superseded "superseded" :withdrawn "withdrawn"})

(def class-defs
  {:accepted   "fill:#1b5e20,stroke:#a5d6a7,color:#fff"
   :proposed   "fill:#e65100,stroke:#ffcc80,color:#fff"
   :draft      "fill:#37474f,stroke:#b0bec5,color:#fff"
   :superseded "fill:#4a148c,stroke:#ce93d8,color:#fff"
   :withdrawn  "fill:#b71c1c,stroke:#ef9a9a,color:#fff"})

(def edge-style
  {:amends [:solid "amends"]
   :supersedes [:thick "supersedes"]
   :depends-on [:dashed "depends on"]})

(defn- scoped-label [label scope]
  (if scope (str label " — " scope) label))

(defn- edge-render [{:keys [type scope]}]
  (if-let [[style label] (get edge-style type)]
    [style (scoped-label label scope)]
    [:dashed (str/replace (name type) "-" " ")]))

;; graph-from is pure (no IO) so edge construction is unit-testable with
;; synthetic corpora; build is the thin source-reading wrapper.
(defn graph-from [corpus]
  {:direction "LR"
   :nodes (for [{:keys [slug title status]} (:decisions corpus)]
            {:id slug :label title
             :class (get status-class status "draft")})
   :edges (for [{:keys [slug relations]} (:decisions corpus)
                rel relations
                :let [[style label] (edge-render rel)]]
            {:from slug :to (:to rel) :style style :label label})
   :class-defs class-defs})

(defn lint* []
  (decisions/validate-repository "."))

(defn build []
  (let [{:keys [corpus problems]} (decisions/load-corpus decisions/corpus-file)]
    (when problems
      (throw (ex-info "unreadable decisions corpus" {:problems problems})))
    (graph-from corpus)))
