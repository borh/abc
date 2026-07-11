(ns abc.tools.adr-evidence
  "Typed, deterministic evidence validation for ADR acceptance claims."
  (:require [abc.tools.files :as files]
            [clojure.set :as set])
  (:import [java.time DateTimeException LocalDate]))

(def matrix-path "docs/adr/claim-evidence-compatibility.edn")
(def registry-path "docs/adr/adr-evidence.edn")
(def as-of-path "docs/adr/governance-as-of.edn")

(defn load-matrix []
  (into (sorted-map)
        (map (fn [[claim-kind evidence-kinds]]
               [claim-kind (set evidence-kinds)]))
        (files/read-edn matrix-path)))

(defn load-registry []
  (files/read-edn registry-path))

(defn load-as-of []
  (:as-of (files/read-edn as-of-path)))

(defn evaluate
  "Evaluate one expected predicate against an observation. Returns false for an
  unknown operator or incomparable values; callers turn that into a problem."
  [{:keys [operator value]} {observed :value}]
  (try
    (case operator
      := (= observed value)
      :not= (not= observed value)
      :< (< observed value)
      :<= (<= observed value)
      :> (> observed value)
      :>= (>= observed value)
      :contains (boolean (some #(= value %) observed))
      :set= (= (set observed) (set value))
      false)
    (catch RuntimeException _
      false)))

(defn- problem [kind entry message]
  {:kind kind
   :claim-id (:claim-id entry)
   :message message})

(defn- parse-date [value]
  (when (string? value)
    (try
      (LocalDate/parse value)
      (catch DateTimeException _ nil))))

(defn validate-entry
  "Validate one evidence entry against `matrix` at the explicit ISO date
  `as-of`. The process wall clock is never read."
  [matrix as-of entry]
  (let [claim-kind (:claim-kind entry)
        evidence-kind (:evidence-kind entry)
        admissible (get matrix claim-kind)
        known-evidence-kinds (apply set/union #{} (vals matrix))
        as-of-date (parse-date as-of)
        review-after (parse-date (:review-after entry))]
    (vec
     (concat
      (when-not admissible
        [(problem :unknown-claim-kind entry
                  (str "unknown claim kind: " claim-kind))])
      (when-not (contains? known-evidence-kinds evidence-kind)
        [(problem :unknown-evidence-kind entry
                  (str "unknown evidence kind: " evidence-kind))])
      (when (and admissible
                 (contains? known-evidence-kinds evidence-kind)
                 (not (contains? admissible evidence-kind)))
        [(problem :incompatible-evidence-kind entry
                  (str evidence-kind " cannot satisfy " claim-kind))])
      (when (contains? entry :verdict)
        [(problem :stored-verdict entry
                  "verdict is derived and must not be stored")])
      (when (not= (:inputs entry) (get-in entry [:observed :inputs]))
        [(problem :stale-inputs entry
                  "declared and observed input bindings differ")])
      (when-not (evaluate (:expected entry) (:observed entry))
        [(problem :predicate-failed entry
                  "observed value does not satisfy the expected predicate")])
      (when (and (= :external-authority evidence-kind)
                 as-of-date
                 review-after
                 (.isAfter ^LocalDate as-of-date ^LocalDate review-after))
        [(problem :expired-evidence entry
                  (str "review date " review-after " passed at " as-of-date))])))))

(defn validate-registry
  "Validate every entry in registry order and return deterministic problems."
  ([registry]
   (validate-registry (load-matrix) (load-as-of) registry))
  ([matrix as-of registry]
   (mapv identity
         (mapcat #(validate-entry matrix as-of %) (:entries registry)))))
