(ns abc.tools.diagram.architecture-graph
  "Pure builder: architecture-stages.edn -> dataflow graph value, cross-
   checked against schema-contracts.json and the ADR files. See ADR 0029."
  (:require [abc.tools.adr :as adr]
            [abc.tools.files :as files]
            [abc.tools.json :as json]
            [clojure.set :as set]
            [clojure.string :as str]))

(def stages-path "docs/architecture-stages.edn")
(def contracts-path "schemas/schema-contracts.json")
(def manifest-schema-path "schemas/manifest.schema.json")
(def adr-dir "docs/adr")
(def out-path "docs/architecture.mmd")

(defn load-stages [] (files/read-edn stages-path))

(defn manifest-identity-required [schema]
  (set (get-in schema ["$defs" "identityObject" "required"])))

(defn documented-identity-coordinates [markdown]
  (let [[_ body]
        (re-find #"(?s)<!-- manifest-identity-coordinates:start -->\s*```text\s*(.*?)\s*```\s*<!-- manifest-identity-coordinates:end -->"
                 markdown)]
    (if body
      (->> (str/split-lines body) (remove str/blank?) set)
      #{})))

(defn schema-paths []
  (->> (get (json/read-json-file contracts-path) "schemas")
       (map #(get % "path")) set))

(defn adr-nums []
  (set (map :num (adr/parse-all adr-dir))))

(defn- valid-owner-list? [owners]
  (and (sequential? owners) (seq owners) (every? integer? owners)))

(defn identity-contract-problems [doc adr-nums manifest-schema architecture-markdown]
  (let [required (manifest-identity-required manifest-schema)
        contract (:manifest-identity-contract doc)
        coordinates (get-in doc [:manifest-identity-contract :coordinates])
        attributed (set (keys coordinates))
        documented (documented-identity-coordinates architecture-markdown)
        owners (set (mapcat (fn [[_ values]]
                              (if (valid-owner-list? values) values []))
                            coordinates))
        manifest-adrs (->> (:stages doc)
                           (filter #(= :manifest (:id %)))
                           first :adr set)]
    (cond-> []
      (not= manifest-schema-path (:schema contract))
      (conj (format "identity contract schema must be %s, got %s"
                    manifest-schema-path (pr-str (:schema contract))))
      (not= required attributed)
      (conj (format "manifest identity attribution mismatch: missing=%s extra=%s"
                    (sort (set/difference required attributed))
                    (sort (set/difference attributed required))))
      (not= required documented)
      (conj (format "architecture identity list mismatch: missing=%s extra=%s"
                    (sort (set/difference required documented))
                    (sort (set/difference documented required))))
      (not= owners manifest-adrs)
      (conj (format "manifest stage ADR owners mismatch: expected=%s actual=%s"
                    (sort owners) (sort manifest-adrs)))
      true
      (into
       (concat
        (for [[coordinate values] coordinates
              :when (not (valid-owner-list? values))]
          (format "%s owners must be a non-empty sequential collection of ADR integers"
                  coordinate))
        (for [[coordinate values] coordinates
              :when (valid-owner-list? values)
              owner values
              :when (not (contains? adr-nums owner))]
          (format "%s references non-existent ADR %04d" coordinate owner)))))))

(defn validate [doc schema-paths adr-nums manifest-schema architecture-markdown]
  (let [stages (:stages doc)
        ids (set (map :id stages))]
    (vec
     (concat
      (for [s stages :when (and (:schema s) (not (contains? schema-paths (:schema s))))]
        (format "stage %s references unknown schema %s" (:id s) (:schema s)))
      (for [s stages n (:adr s) :when (not (contains? adr-nums n))]
        (format "stage %s references non-existent ADR %04d" (:id s) n))
      (for [s stages i (:inputs s) :when (not (contains? ids i))]
        (format "stage %s has unknown input %s" (:id s) i))
      (identity-contract-problems doc adr-nums manifest-schema architecture-markdown)))))

(defn lint* []
  (validate (load-stages)
            (schema-paths)
            (adr-nums)
            (json/read-json-file manifest-schema-path)
            (slurp "docs/architecture.md")))

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
