(ns abc.tools.adr-evidence
  "Typed, deterministic evidence validation for ADR acceptance claims."
  (:require [abc.tools.adr-evidence-bundle :as bundle]
            [abc.tools.files :as files]
            [clojure.set :as set])
  (:import [java.time DateTimeException LocalDate]))

(def matrix-path "docs/adr/claim-evidence-compatibility.edn")
(def registry-path "docs/adr/adr-evidence.edn")
(def as-of-path "docs/adr/governance-as-of.edn")

(def ^:private entry-keys
  #{:claim-id :claim-kind :evidence-kind :artifact-path
    :artifact-hash :observation-key :expected})
(def ^:private forbidden-inline-keys #{:observed :inputs :verdict})
(def ^:private benchmark-units
  #{"nanoseconds" "bytes" "count" "parts-per-million"})
(def ^:private safe-integer-min -9007199254740991)
(def ^:private safe-integer-max 9007199254740991)

(defn load-matrix []
  (into (sorted-map)
        (map (fn [[claim-kind evidence-kinds]]
               [claim-kind (set evidence-kinds)]))
        (files/read-edn matrix-path)))

(defn load-registry [] (files/read-edn registry-path))
(defn load-as-of [] (:as-of (files/read-edn as-of-path)))

(defn- value-type [value]
  (cond
    (nil? value) :null
    (boolean? value) :boolean
    (string? value) :string
    (integer? value) :integer
    (vector? value) :array
    (map? value) :object
    :else :unsupported))

(defn evaluate-result [{:keys [operator value]} observed]
  (let [same-type? (= (value-type value) (value-type observed))
        ordered-integers? (and (integer? observed) (integer? value)
                               (<= safe-integer-min observed safe-integer-max)
                               (<= safe-integer-min value safe-integer-max))
        pass-or-fail (fn [result] {:status (if result :pass :fail)})]
    (case operator
      := (if same-type? (pass-or-fail (= observed value)) {:status :type-error})
      :not= (if same-type? (pass-or-fail (not= observed value)) {:status :type-error})
      :< (if ordered-integers?
           (pass-or-fail (< observed value)) {:status :type-error})
      :<= (if ordered-integers?
            (pass-or-fail (<= observed value)) {:status :type-error})
      :> (if ordered-integers?
           (pass-or-fail (> observed value)) {:status :type-error})
      :>= (if ordered-integers?
            (pass-or-fail (>= observed value)) {:status :type-error})
      :contains (if (vector? observed)
                  (pass-or-fail (boolean (some #(= value %) observed)))
                  {:status :type-error})
      :set= (if (and (vector? observed) (vector? value))
              (pass-or-fail (= (set observed) (set value)))
              {:status :type-error})
      {:status :type-error})))

(defn- parse-date [value]
  (when (string? value)
    (try (LocalDate/parse value)
         (catch DateTimeException _ nil))))

(defn- claim-problem [kind claim message & {:as data}]
  (merge {:kind kind :claim-id (:claim-id claim)
          :file (:file claim) :criterion-index (:criterion-index claim)
          :message message}
         data))

(defn- entry-problem [kind entry message & {:as data}]
  (merge {:kind kind :claim-id (:claim-id entry) :message message} data))

(defn- static-entry-problems [matrix known-evidence-kinds claims-by-id entry]
  (let [claim (get claims-by-id (:claim-id entry))
        claim-kind (:claim-kind entry)
        evidence-kind (:evidence-kind entry)
        admissible (get matrix claim-kind)
        forbidden (set/intersection forbidden-inline-keys (set (keys entry)))
        unknown (set/difference (set (keys entry)) entry-keys forbidden-inline-keys)
        missing (set/difference entry-keys (set (keys entry)))]
    (vec
     (concat
      (when (seq forbidden)
        [(entry-problem :forbidden-inline-evidence-value entry
                        "observations, inputs, and verdicts belong only in evidence artifacts"
                        :keys (vec (sort forbidden)))])
      (when (seq unknown)
        [(entry-problem :invalid-evidence-entry entry
                        "evidence registry entry contains unknown keys"
                        :keys (vec (sort unknown)))])
      (when (seq missing)
        [(entry-problem :invalid-evidence-entry entry
                        "evidence registry entry is missing required keys"
                        :keys (vec (sort missing)))])
      (when-not claim
        [(entry-problem :unknown-claim-id entry "registry claim ID has no criterion")])
      (when (and claim (not= (:claim-kind claim) claim-kind))
        [(entry-problem :claim-kind-mismatch entry
                        "registry claim kind differs from the Markdown criterion")])
      (when-not admissible
        [(entry-problem :unknown-claim-kind entry "registry claim kind is unknown")])
      (when-not (contains? known-evidence-kinds evidence-kind)
        [(entry-problem :unknown-evidence-kind entry "evidence kind is unknown")])
      (when (and admissible (contains? known-evidence-kinds evidence-kind)
                 (not (contains? admissible evidence-kind)))
        [(entry-problem :incompatible-evidence-kind entry
                        "evidence kind cannot satisfy this claim kind")])))))

(defn- entry-shape-valid? [entry]
  (= entry-keys (set (keys entry))))

(defn- artifact-groups [entries]
  (->> entries
       (filter entry-shape-valid?)
       (group-by (juxt :artifact-path :artifact-hash))
       (sort-by key)))

(defn- duplicate-entry-problems [entries]
  (for [[entry frequency] (sort-by (comp pr-str key) (frequencies entries))
        :when (< 1 frequency)]
    (entry-problem :duplicate-evidence-entry entry
                   "exact evidence registry entry is duplicated")))

(defn- artifact-entry-problems [as-of artifact entry]
  (let [observation (get-in artifact ["observations" (:observation-key entry)])]
    (if-not observation
      [(entry-problem :missing-observation entry
                      "named observation is absent from the evidence artifact")]
      (let [observed (get observation "value")
            benchmark-invalid?
            (and (= :benchmark (:evidence-kind entry))
                 (or (not (integer? observed))
                     (not (contains? benchmark-units
                                     (get-in observation ["details" "unit"])))))]
        (vec
         (concat
          (when benchmark-invalid?
            [(entry-problem :invalid-evidence-artifact entry
                            "benchmark observations require a scaled integer and declared unit")])
          (when-not benchmark-invalid?
            (case (:status (evaluate-result (:expected entry) observed))
              :pass []
              :fail [(entry-problem :predicate-failed entry
                                    "observation does not satisfy the expected predicate")]
              :type-error [(entry-problem :predicate-type-mismatch entry
                                          "observation and predicate operand types are incompatible")]))
          (let [as-of-date (parse-date as-of)
                review-after (parse-date (get artifact "review_after"))]
            (when (and (= :external-authority (:evidence-kind entry))
                       as-of-date review-after
                       (.isAfter ^LocalDate as-of-date ^LocalDate review-after))
              [(entry-problem :expired-evidence entry
                              "external evidence review date has passed")]))))))))

(defn validate-registry
  [{:keys [repo-root claims registry matrix as-of]}]
  (let [entries (:entries registry)
        claims-by-id (into {} (map (juxt :claim-id identity)) claims)
        known-evidence-kinds (apply set/union #{} (vals matrix))
        static-problems (mapcat #(static-entry-problems matrix known-evidence-kinds
                                                        claims-by-id %)
                                entries)
        coverage-problems (for [claim claims
                                :when (not-any? #(= (:claim-id claim) (:claim-id %))
                                                entries)]
                            (claim-problem :missing-claim-evidence claim
                                           "Accepted claim has no evidence registry entry"))
        artifact-results
        (into (sorted-map)
              (for [[[path artifact-hash] grouped] (artifact-groups entries)
                    :let [affected (mapv :claim-id grouped)]]
                [[path artifact-hash]
                 (bundle/validate-bundle repo-root path artifact-hash affected)]))
        artifact-root-problems (mapcat :problems (vals artifact-results))
        joined-problems
        (mapcat
         (fn [entry]
           (let [result (get artifact-results [(:artifact-path entry)
                                               (:artifact-hash entry)])]
             (when (:bundle result)
               (artifact-entry-problems as-of (:bundle result) entry))))
         (filter entry-shape-valid? entries))]
    (vec (concat coverage-problems
                 (duplicate-entry-problems entries)
                 static-problems
                 artifact-root-problems
                 joined-problems))))
