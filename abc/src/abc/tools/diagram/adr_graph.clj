(ns abc.tools.diagram.adr-graph
  "Tier 1 pure builder: ADR headers (+ adr-relations.edn) -> decision-map
   graph value. lint* delegates ADR governance to abc.tools.adr and enforces
   semantic-sidecar rules locally.
   See ADR 0029."
  (:require [abc.tools.adr :as adr]
            [abc.tools.files :as files]
            [babashka.fs :as fs]))

(def adr-dir "docs/adr")
(def out-path "docs/adr/adr-graph.mmd")
(def relations-path "docs/adr/adr-relations.edn")

(def status-class
  {"Accepted" "accepted" "Proposed" "proposed" "Draft" "draft"
   "Superseded" "superseded" "Withdrawn" "withdrawn"})

(def class-defs
  {:accepted   "fill:#1b5e20,stroke:#a5d6a7,color:#fff"
   :proposed   "fill:#e65100,stroke:#ffcc80,color:#fff"
   :draft      "fill:#37474f,stroke:#b0bec5,color:#fff"
   :superseded "fill:#4a148c,stroke:#ce93d8,color:#fff"
   :withdrawn  "fill:#b71c1c,stroke:#ef9a9a,color:#fff"})

;; --- typed relationship sidecar --------------------------------------------
(def header-owned-types #{:amends :supersedes :depends-on})
(def relation-types #{:restates-hard-rule :schema-hash-cascade :harness-for :extends})

(def ^:private relation-label
  {:restates-hard-rule "restates hard rule"
   :schema-hash-cascade "schema-hash cascade"
   :harness-for "harness for"
   :extends "extends"})

(defn load-relations []
  (if (fs/exists? relations-path)
    (:relations (files/read-edn relations-path))
    []))

;; --- lint (pure core + IO wrapper) ------------------------------------------
(defn lint-adrs [adrs relations]
  (let [by-num (into {} (map (juxt :num identity)) adrs)
        exists? (set (keys by-num))]
    (vec
     (concat
      ;; sidecar: shape
      (for [r relations
            :when (not (and (integer? (:from r)) (integer? (:to r)) (keyword? (:type r))))]
        (format "adr-relations entry is malformed: %s" (pr-str r)))
      (for [r relations
            :when (and (contains? r :note) (not (string? (:note r))))]
        (format "adr-relations edge %s->%s :note must be a string"
                (:from r) (:to r)))
      ;; sidecar: single ownership — header-owned types forbidden here
      (for [r relations :when (header-owned-types (:type r))]
        (format "adr-relations edge %04d->%04d uses header-owned type %s; put it in the ADR header"
                (:from r) (:to r) (:type r)))
      ;; sidecar: unknown types
      (for [r relations
            :when (and (keyword? (:type r))
                       (not (relation-types (:type r)))
                       (not (header-owned-types (:type r))))]
        (format "adr-relations edge %04d->%04d uses unknown relation type %s"
                (:from r) (:to r) (:type r)))
      ;; sidecar: referential integrity
      (for [r relations n [(:from r) (:to r)]
            :when (and (integer? n) (not (exists? n)))]
        (format "adr-relations edge references non-existent ADR %04d" n))))))

(defn lint* []
  (let [adrs (adr/parse-all adr-dir)]
    (vec (concat (adr/validate-adrs-legacy adrs ".")
                 (lint-adrs adrs (load-relations))))))

;; --- build ------------------------------------------------------------------
(defn- node-id [n] (format "ADR%04d" n))

(defn header-edges [adrs]
  (for [a adrs
        [type items] [[:amends (get-in a [:relations :amends])]
                      [:supersedes (get-in a [:relations :supersedes])]
                      [:depends-on (get-in a [:relations :depends-on])]]
        {:keys [target scope]} items]
    {:from (:num a) :to target :type type :scope scope}))

(defn- scoped-label [label scope]
  (if scope (str label " — " scope) label))

(def edge-style
  {:amends [:solid "amends"]
   :supersedes [:thick "supersedes"]
   :depends-on [:dashed "depends on"]})

(defn all-edges [adrs relations]
  (concat (header-edges adrs)
          (for [r relations]
            {:from (:from r) :to (:to r) :type (:type r)})))

;; graph-from is pure (no IO) so edge construction is unit-testable with
;; synthetic adrs; build is the thin source-reading wrapper.
(defn graph-from [adrs relations]
  (let [nums (set (map :num adrs))
        edges (for [e (all-edges adrs relations)
                    :when (and (nums (:from e)) (nums (:to e)))
                    :let [[style label] (or (get edge-style (:type e))
                                            [:dashed (get relation-label (:type e) (name (:type e)))])]]
                {:from (node-id (:from e)) :to (node-id (:to e))
                 :style style :label (scoped-label label (:scope e))})]
    {:direction "LR"
     :nodes (for [a adrs]
              {:id (node-id (:num a))
               :label (format "%04d %s" (:num a) (:title a))
               :class (get status-class (:status a) "draft")})
     :edges edges
     :class-defs class-defs}))

(defn build [] (graph-from (adr/parse-all adr-dir) (load-relations)))
