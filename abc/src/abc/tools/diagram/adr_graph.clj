(ns abc.tools.diagram.adr-graph
  "Tier 1 pure builder: ADR headers (+ adr-relations.edn, Task 6) -> decision-map
   graph value. lint* enforces header hygiene and (Task 6) sidecar rules.
   See ADR 0029."
  (:require [abc.tools.files :as files]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def adr-dir "docs/adr")
(def out-path "docs/adr/adr-graph.mmd")
(def relations-path "docs/adr/adr-relations.edn")

(def statuses #{"Draft" "Proposed" "Accepted" "Superseded" "Withdrawn"})

(def status-class
  {"Accepted" "accepted" "Proposed" "proposed" "Draft" "draft"
   "Superseded" "superseded" "Withdrawn" "withdrawn"})

(def class-defs
  {:accepted   "fill:#1b5e20,stroke:#a5d6a7,color:#fff"
   :proposed   "fill:#e65100,stroke:#ffcc80,color:#fff"
   :draft      "fill:#37474f,stroke:#b0bec5,color:#fff"
   :superseded "fill:#4a148c,stroke:#ce93d8,color:#fff"
   :withdrawn  "fill:#b71c1c,stroke:#ef9a9a,color:#fff"})

(defn adr-files [dir]
  (->> (.listFiles (io/file dir))
       (map #(.getName %))
       (filter #(re-matches #"\d{4}-.*\.md" %))
       sort))

(defn- adr-number [filename] (Integer/parseInt (subs filename 0 4)))

(defn- header-block [content]
  (->> (str/split-lines content)
       (take-while #(not (str/starts-with? % "## ")))
       (str/join "\n")))

(defn- adr-ref-tokens [s]
  (mapv second (re-seq #"ADR\s+(\d+)" (or s ""))))

(defn- field [header field-name]
  (some (fn [l] (when (str/starts-with? l (str field-name ":"))
                  (str/trim (subs l (inc (count field-name))))))
        (str/split-lines header)))

(defn- parse-adr-ref-field [header field-name]
  (let [tokens (adr-ref-tokens (field header field-name))]
    {:refs (->> tokens
                (filter #(re-matches #"\d{4}" %))
                (map #(Integer/parseInt %))
                distinct
                vec)
     :malformed-refs (->> tokens
                          (remove #(re-matches #"\d{4}" %))
                          (mapv (fn [token] {:field field-name :token token})))}))

(defn parse-adr [dir filename]
  (let [content (slurp (io/file dir filename))
        header (header-block content)
        title (-> (first (str/split-lines content))
                  (str/replace #"^# ADR \d{4}:" "") str/trim)
        field-names ["Supersedes" "Amends" "Amended by" "Depends on"]
        parsed-fields (mapv #(parse-adr-ref-field header %) field-names)
        by-field (zipmap field-names parsed-fields)]
    {:num (adr-number filename) :file filename :title title
     :status (some-> (field header "Status") (str/split #"\s+") first)
     :supersedes (get-in by-field ["Supersedes" :refs])
     :amends (get-in by-field ["Amends" :refs])
     :amended-by (get-in by-field ["Amended by" :refs])
     :depends-on (get-in by-field ["Depends on" :refs])
     :malformed-refs (vec (mapcat :malformed-refs parsed-fields))}))

(defn parse-all [dir] (mapv #(parse-adr dir %) (adr-files dir)))

;; --- typed relationship sidecar --------------------------------------------
(def header-owned-types #{:amends :supersedes :depends-on})
(def relation-types #{:restates-hard-rule :schema-hash-cascade :harness-for :extends})

(def ^:private relation-label
  {:restates-hard-rule "restates hard rule"
   :schema-hash-cascade "schema-hash cascade"
   :harness-for "harness for"
   :extends "extends"})

(defn load-relations []
  (if (.exists (io/file relations-path))
    (:relations (files/read-edn relations-path))
    []))

;; --- lint (pure core + IO wrapper) ------------------------------------------
(defn lint-adrs [adrs relations]
  (let [by-num (into {} (map (juxt :num identity)) adrs)
        exists? (set (keys by-num))]
    (vec
     (concat
      (for [a adrs :when (not (statuses (:status a)))]
        (format "ADR %04d has non-vocabulary Status %s" (:num a) (pr-str (:status a))))
      (for [a adrs {:keys [field token]} (:malformed-refs a)]
        (format "ADR %04d %s reference ADR %s must use exactly four digits"
                (:num a) field token))
      (for [a adrs t (concat (:amends a) (:depends-on a)) :when (not (exists? t))]
        (format "ADR %04d references non-existent ADR %04d" (:num a) t))
      (for [a adrs t (:amends a)
            :when (and (exists? t) (not (some #{(:num a)} (:amended-by (by-num t)))))]
        (format "ADR %04d amends ADR %04d but ADR %04d lacks `Amended by: ADR %04d`"
                (:num a) t t (:num a)))
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

(defn lint* [] (lint-adrs (parse-all adr-dir) (load-relations)))

;; --- build ------------------------------------------------------------------
(defn- node-id [n] (format "ADR%04d" n))

(defn header-edges [adrs]
  (for [a adrs
        [t-kw ts] [[:amends (:amends a)] [:supersedes (:supersedes a)] [:depends-on (:depends-on a)]]
        t ts]
    {:from (:num a) :to t :type t-kw}))

(def edge-style
  {:amends [:solid "amends"]
   :supersedes [:thick "supersedes"]
   :depends-on [:dashed "depends on"]})

(defn all-edges [adrs relations]
  (concat (header-edges adrs)
          (for [r relations] {:from (:from r) :to (:to r) :type (:type r)})))

;; graph-from is pure (no IO) so edge construction is unit-testable with
;; synthetic adrs; build is the thin source-reading wrapper. Task 6 switches the
;; edge source from (header-edges adrs) to (all-edges adrs relations).
(defn graph-from [adrs relations]
  (let [nums (set (map :num adrs))
        edges (for [e (all-edges adrs relations)
                    :when (and (nums (:from e)) (nums (:to e)))
                    :let [[style label] (or (get edge-style (:type e))
                                            [:dashed (get relation-label (:type e) (name (:type e)))])]]
                {:from (node-id (:from e)) :to (node-id (:to e)) :style style :label label})]
    {:direction "LR"
     :nodes (for [a adrs]
              {:id (node-id (:num a))
               :label (format "%04d %s" (:num a) (:title a))
               :class (get status-class (:status a) "draft")})
     :edges edges
     :class-defs class-defs}))

(defn build [] (graph-from (parse-all adr-dir) (load-relations)))
