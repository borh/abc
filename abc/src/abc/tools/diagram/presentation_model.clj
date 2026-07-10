(ns abc.tools.diagram.presentation-model
  (:require [abc.tools.adr :as adr]
            [abc.tools.diagram.architecture-graph :as architecture]
            [abc.tools.files :as files]
            [abc.tools.json :as json]
            [clojure.set :as set]))

(def metadata-path "docs/architecture-presentation.edn")
(def manifest-schema-path "schemas/manifest.schema.json")
(def adr-dir "docs/adr")
(def coordinate-families #{:source :parsing :publication :analysis :output})
(def stage-roles #{:source :evidence :contract :identity :output})

(defn load-metadata [] (files/read-edn metadata-path))

(defn- stage-map [stage-doc]
  (into {} (map (juxt :id identity)) (:stages stage-doc)))

(defn- stage-edges [stages]
  (into #{}
        (mapcat (fn [{:keys [id inputs]}]
                  (map (fn [input] [input id]) inputs)))
        (vals stages)))

(defn canonical-context []
  (let [stage-doc (architecture/load-stages)
        stages (stage-map stage-doc)]
    {:stage-doc stage-doc
     :stages stages
     :stage-edges (stage-edges stages)
     :coordinates (architecture/manifest-identity-required
                   (json/read-json-file manifest-schema-path))
     :coordinate-owners (get-in stage-doc
                                [:manifest-identity-contract :coordinates])
     :adr-nums (set (map :num (adr/parse-all adr-dir)))}))

(defn reachable? [edges from to]
  (loop [frontier [from] seen #{}]
    (cond
      (empty? frontier) false
      (= to (peek frontier)) true
      (seen (peek frontier)) (recur (pop frontier) seen)
      :else
      (let [node (peek frontier)
            nexts (for [[a b] edges :when (= a node)] b)]
        (recur (into (pop frontier) nexts) (conj seen node))))))

(defn- path-resolves? [edges path]
  (and (<= 2 (count path))
       (every? #(contains? edges %) (partition 2 1 path))))

(defn- canonical-stage-adrs [context stage-ids]
  (set (mapcat #(get-in context [:stages % :adr]) stage-ids)))

(defn backing-problems [context {:keys [stages adrs]}]
  (let [stage-ids (set (keys (:stages context)))
        stages (vec stages)
        missing (sort (remove stage-ids stages))
        allowed-adrs (canonical-stage-adrs context stages)
        invalid-adrs (sort (remove allowed-adrs adrs))]
    (cond-> []
      (empty? stages) (conj "aggregate backing must name at least one stage")
      (seq missing) (conj (format "aggregate backing references missing stages %s"
                                  missing))
      (seq invalid-adrs) (conj (format "aggregate citations are not canonical for backing stages: %s"
                                       invalid-adrs)))))

(defn- keyset-problem [label expected actual]
  (when (not= expected actual)
    (format "%s metadata mismatch: missing=%s extra=%s"
            label
            (sort (set/difference expected actual))
            (sort (set/difference actual expected)))))

(defn problems [metadata context]
  (let [coordinate-problem
        (keyset-problem "coordinate" (:coordinates context)
                        (set (keys (:coordinates metadata))))
        stage-problem
        (keyset-problem "stage" (set (keys (:stages context)))
                        (set (keys (:stages metadata))))
        aggregates (mapcat (comp vals :aggregates val) (:figures metadata))
        inset-path (get-in metadata [:figures :publication :current-inset :path])]
    (vec
     (concat
      (keep identity [coordinate-problem stage-problem])
      (for [[coordinate {:keys [family label]}] (:coordinates metadata)
            :when (or (not (coordinate-families family))
                      (not (string? label))
                      (empty? label))]
        (format "coordinate %s has invalid presentation family or label" coordinate))
      (for [[stage {:keys [role label]}] (:stages metadata)
            :when (or (not (stage-roles role))
                      (not (string? label))
                      (empty? label))]
        (format "stage %s has invalid presentation role or label" stage))
      (mapcat #(backing-problems context %) aggregates)
      (when-not (path-resolves? (:stage-edges context) inset-path)
        [(format "current parser inset path does not resolve: %s" inset-path)])))))

(defn validated-model []
  (let [metadata (load-metadata)
        context (canonical-context)
        failures (problems metadata context)]
    (when (seq failures)
      (throw (ex-info "academic presentation model is invalid"
                      {:problems failures})))
    {:metadata metadata :context context}))
