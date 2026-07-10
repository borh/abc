(ns abc.tools.diagram.presentation-figures
  (:require [abc.tools.diagram.presentation-model :as model]
            [clojure.set :as set]))

(def theme
  {:canvas "#000000"
   :text "#F5F7FA"
   :secondary "#A7B0BE"
   :identity "#48CAE4"
   :evidence "#F2B84B"
   :output "#7BC47F"
   :title-size 52
   :primary-size 34
   :secondary-size 24
   :citation-size 16
   :stroke-width 2
   :safe-margin 96})

(defn- aggregate [metadata figure id]
  (get-in metadata [:figures figure :aggregates id]))

(defn- stage-adrs [context stage-ids]
  (vec (sort (set (mapcat #(get-in context [:stages % :adr]) stage-ids)))))

(defn- aggregate-node [metadata context figure id role group subtitle]
  (let [{:keys [label stages]} (aggregate metadata figure id)]
    {:id id :label label :subtitle subtitle :role role :group group
     :backing {:stages stages :adrs (stage-adrs context stages)}}))

(defn- family-node [metadata context family]
  (let [coordinates (->> (:coordinates metadata)
                         (keep (fn [[coordinate value]]
                                 (when (= family (:family value))
                                   {:id coordinate :label (:label value)})))
                         (sort-by :id)
                         vec)
        owners (vec (sort (set (mapcat #(get (:coordinate-owners context) (:id %))
                                       coordinates))))
        family-label ({:source "Source identity"
                       :parsing "Parsing identity"
                       :publication "Publication contracts"
                       :analysis "Linguistic analysis"
                       :output "Output format"} family)]
    (cond-> {:id family
             :label family-label
             :role :coordinate-family
             :group :identity-contract
             :coordinates coordinates
             :backing {:coordinates (mapv :id coordinates) :adrs owners}}
      (#{:source :parsing :analysis} family)
      (assoc :coordinate-columns 2))))

(defn- path-valid? [context path]
  (and (sequential? path)
       (<= 2 (count path))
       (every? (fn [[from to]]
                 (model/reachable? (:stage-edges context) from to))
               (partition 2 1 path))))

(def ^:private backing-keys
  #{:coordinates :stages :path :reachable-targets :adrs})

(def ^:private relation-keys #{:coordinates :stages :path})

(def ^:private node-relations
  {:source #{:stages}
   :evidence #{:stages}
   :contract #{:stages}
   :identity #{:stages}
   :output #{:stages}
   :implementation-detail #{:stages}
   :coordinate-family #{:coordinates}
   :identity-formula #{:coordinates}})

(def ^:private edge-relations
  {:identity-input #{:coordinates}
   :identity #{:coordinates :stages}
   :derived-view #{:path}
   :evidence #{:path}
   :validation #{:path}
   :implementation-detail #{:stages :path}})

(defn- valid-list? [value predicate]
  (and (sequential? value) (seq value) (every? predicate value)))

(defn- relation-shape-problems [{:keys [coordinates stages path
                                        reachable-targets adrs]
                                 :as backing}]
  (vec
   (concat
    (for [key (sort-by pr-str
                       (set/difference (set (keys backing)) backing-keys))]
      (str "unknown backing key " key))
    (when-not (seq (set/intersection relation-keys (set (keys backing))))
      ["backing must contain at least one recognized relation"])
    (when (and (contains? backing :coordinates)
               (not (valid-list? coordinates string?)))
      ["backing coordinates must be a non-empty sequential collection of strings"])
    (when (and (contains? backing :stages)
               (not (valid-list? stages keyword?)))
      ["backing stages must be a non-empty sequential collection of keywords"])
    (when (and (contains? backing :path)
               (not (and (valid-list? path keyword?) (<= 2 (count path)))))
      ["backing path must contain at least two stage keywords"])
    (when (and (contains? backing :reachable-targets)
               (not (valid-list? reachable-targets keyword?)))
      ["reachable targets must be a non-empty sequential collection of stage keywords"])
    (when (and (contains? backing :reachable-targets)
               (not (contains? backing :path)))
      ["reachable targets require a backing path"])
    (when (and (contains? backing :adrs)
               (not (and (sequential? adrs) (every? integer? adrs))))
      ["backing ADRs must be a sequential collection of integers"]))))

(defn- item-relation-problems [item-type role backing]
  (let [actual (set/intersection relation-keys (set (keys backing)))
        allowed (get (if (= :node item-type) node-relations edge-relations)
                     role #{})]
    (when (seq (set/difference actual allowed))
      [(str (name item-type) " backing relation is invalid for role " role
            ": " (sort actual))])))

(defn- item-backing-problems [context item-type role
                              {:keys [coordinates stages path
                                      reachable-targets adrs]
                               :as backing}]
  (let [shapes (relation-shape-problems backing)
        coordinate-values (if (valid-list? coordinates string?) coordinates [])
        stage-values (if (valid-list? stages keyword?) stages [])
        target-values (if (valid-list? reachable-targets keyword?)
                        reachable-targets [])
        path-values (if (valid-list? path keyword?) path [])
        adr-values (if (and (sequential? adrs) (every? integer? adrs)) adrs [])
        coordinate-set (:coordinates context)
        stage-set (set (keys (:stages context)))
        missing-coordinates (sort (remove coordinate-set coordinate-values))
        missing-stages (sort (remove stage-set stage-values))
        missing-targets (sort (remove stage-set target-values))
        missing-path-stages (sort (remove stage-set path-values))
        zero-hop-path? (boolean (some (fn [[from to]] (= from to))
                                      (partition 2 1 path-values)))
        path-ready? (and (<= 2 (count path-values))
                         (empty? missing-path-stages)
                         (not zero-hop-path?))
        allowed-adrs (set (concat
                           (mapcat #(get-in context [:stages % :adr]) stage-values)
                           (mapcat #(get (:coordinate-owners context) %)
                                   coordinate-values)))
        invalid-adrs (sort (remove allowed-adrs adr-values))
        nonexistent-adrs (sort (remove (:adr-nums context) adr-values))
        path-source (first path-values)
        unreachable (if path-ready?
                      (sort (remove #(model/reachable? (:stage-edges context)
                                                       path-source %)
                                    target-values))
                      [])]
    (cond-> (into shapes (item-relation-problems item-type role backing))
      (seq missing-coordinates)
      (conj (str "unknown backing coordinates " missing-coordinates))
      (seq missing-stages)
      (conj (str "unknown backing stages " missing-stages))
      (seq missing-targets)
      (conj (str "unknown reachable targets " missing-targets))
      (seq missing-path-stages)
      (conj (str "unknown backing path stages " missing-path-stages))
      zero-hop-path?
      (conj (str "backing path contains a zero-hop between consecutive stages "
                 path))
      (seq invalid-adrs)
      (conj (str "citations are not canonical for backing " invalid-adrs))
      (seq nonexistent-adrs)
      (conj (str "citations reference non-existent ADRs " nonexistent-adrs))
      (and (contains? backing :path)
           path-ready?
           (not (path-valid? context path)))
      (conj (str "backing path does not resolve " path))
      (seq unreachable)
      (conj (str "backing targets are unreachable " unreachable)))))

(defn validate-graph! [context graph]
  (let [node-ids (set (map :id (:nodes graph)))]
    (doseq [{:keys [from to] :as edge} (:edges graph)
            endpoint [from to]
            :when (not (node-ids endpoint))]
      (throw (ex-info "presentation edge references undeclared node"
                      {:figure (:id graph) :edge edge
                       :endpoint endpoint :node-ids node-ids}))))
  (doseq [[item-type item]
          (concat (map #(vector :node %) (:nodes graph))
                  (map #(vector :edge %) (:edges graph)))]
    (when-not (map? (:backing item))
      (throw (ex-info "presentation item has no canonical backing"
                      {:figure (:id graph) :item item})))
    (when-let [failures (seq (item-backing-problems context item-type (:role item)
                                                    (:backing item)))]
      (throw (ex-info "presentation item backing is invalid"
                      {:figure (:id graph) :item item :problems failures}))))
  graph)

(defn reproducibility-graph [{:keys [metadata context]}]
  (let [graph
        {:id :reproducibility
         :direction "LR"
         :concentrate? true
         :title (get-in metadata [:figures :reproducibility :title])
         :subtitle (get-in metadata [:figures :reproducibility :subtitle])
         :description "Sources and evidence enter a versioned identity contract; ArtifactID identifies a validated manifest from which scholarly views derive."
         :theme theme
         :groups [{:id :inputs :label "Sources and computational evidence"
                   :cluster? false}
                  {:id :identity-contract :label "Versioned identity contract"}
                  {:id :record :label "Scholarly record"}
                  {:id :derived :label "Derived views"}]
         :nodes (vec
                 (concat
                  [(aggregate-node metadata context :reproducibility :sources
                                   :evidence :inputs
                                   "Sources · metadata · parser evidence")]
                  (map #(family-node metadata context %)
                       [:source :parsing :publication :analysis :output])
                  [{:id :artifact-id
                    :label "ArtifactID"
                    :subtitle "SHA-256 of canonical manifest identity"
                    :role :identity-formula :group :identity-contract
                    :backing {:coordinates (vec (sort (:coordinates context)))
                              :adrs (vec (sort (set (mapcat val (:coordinate-owners context)))))}}
                   (aggregate-node metadata context :reproducibility :manifest
                                   :identity :record
                                   "Identity · provenance · validation · content hash")
                   (aggregate-node metadata context :reproducibility :views
                                   :output :derived
                                   "TEI · text · RDF · Linked Art · IIIF · annotation · analysis")]))
         :edges [{:from :sources :to :source :role :identity-input :style :solid
                  :tail-port :n
                  :backing {:coordinates ["corpus_snapshot_hash" "work_content_hash"
                                          "metadata_record_hash"]}}
                 {:from :sources :to :parsing :role :identity-input :style :solid
                  :tail-port :n
                  :backing {:coordinates ["parser_build_hash" "parser_config_hash"
                                          "aat_parser_ir_mapping_hash"
                                          "parser_ir_schema_hash"]}}
                 {:from :sources :to :publication :role :identity-input :style :solid
                  :tail-port :n
                  :backing {:coordinates ["manifest_schema_hash" "tei_profile_hash"]}}
                 {:from :sources :to :analysis :role :identity-input :style :solid
                  :tail-port :n
                  :backing {:coordinates ["tokenizer_build_hash"
                                          "tokenizer_dictionary_hash"
                                          "tokenizer_profile_hash"
                                          "analysis_recipe_hash"
                                          "annotation_policy_hash"]}}
                 {:from :sources :to :output :role :identity-input :style :solid
                  :tail-port :n
                  :backing {:coordinates ["output_format_spec_hash"]}}
                 {:from :source :to :artifact-id :role :identity :style :solid
                  :head-port :w
                  :backing {:coordinates ["corpus_snapshot_hash" "work_content_hash"
                                          "metadata_record_hash"]}}
                 {:from :parsing :to :artifact-id :role :identity :style :solid
                  :head-port :w
                  :backing {:coordinates ["parser_build_hash" "parser_config_hash"
                                          "aat_parser_ir_mapping_hash"
                                          "parser_ir_schema_hash"]}}
                 {:from :publication :to :artifact-id :role :identity :style :solid
                  :head-port :w
                  :backing {:coordinates ["manifest_schema_hash" "tei_profile_hash"]}}
                 {:from :analysis :to :artifact-id :role :identity :style :solid
                  :head-port :w
                  :backing {:coordinates ["tokenizer_build_hash"
                                          "tokenizer_dictionary_hash"
                                          "tokenizer_profile_hash"
                                          "analysis_recipe_hash"
                                          "annotation_policy_hash"]}}
                 {:from :output :to :artifact-id :role :identity :style :solid
                  :head-port :w
                  :backing {:coordinates ["output_format_spec_hash"]}}
                 {:from :artifact-id :to :manifest :role :identity :style :thick
                  :backing {:stages [:manifest] :adrs (stage-adrs context [:manifest])}}
                 {:from :manifest :to :views :role :derived-view :style :solid
                  :backing {:path [:manifest :tei]
                            :reachable-targets [:tei :rdf :iiif :tokenized :analysis :annotation]}}]
         :footer "Identity-bearing inputs determine ArtifactID."
         :primary-order [:sources :artifact-id :manifest :views]}]
    (validate-graph! context graph)))

(defn publication-graph [{:keys [metadata context]}]
  (let [figure :publication
        node (fn [id role group subtitle]
               (assoc (aggregate-node metadata context figure id role group subtitle)
                      :label-wrap 18
                      :subtitle-wrap 18))
        graph
        {:id :publication
         :direction "LR"
         :title (get-in metadata [:figures figure :title])
         :subtitle (get-in metadata [:figures figure :subtitle])
         :description "A stable source-to-Parser-IR-to-manifest publication path, with the current AAT producer implementation shown as subordinate detail."
         :theme theme
         :groups [{:id :producer :label "Source and parser evidence · ab-validator"}
                  {:id :abc :label "Publication contract · ABC"}
                  {:id :scholarship :label "Scholarly outputs"}
                  {:id :current-inset :label "Current producer implementation"
                   :style :dashed}]
         :nodes [(node :source :source :producer "Text · metadata")
                 (node :parser-process :evidence :producer "Versioned evidence · configuration")
                 (node :parser-ir :contract :abc "Stable publication interchange")
                 (node :manifest :identity :abc "Identity · provenance · validation · content")
                 (node :outputs :output :scholarship "TEI · text · RDF · Linked Art · IIIF · annotation · analysis")
                 {:id :aat-detail
                  :label "AAT evidence + mapping gate"
                  :subtitle nil
                  :role :implementation-detail :group :current-inset
                  :label-wrap 18
                  :subtitle-wrap 18
                  :backing {:stages [:aat]
                            :adrs (stage-adrs context [:aat])}}]
         :edges [{:from :source :to :parser-process
                  :role :evidence :style :solid
                  :backing {:path [:aozora-snapshot :aat]}}
                 {:from :parser-process :to :parser-ir
                  :role :validation :style :solid
                  :backing {:path [:aat :parser-ir]}}
                 {:from :parser-ir :to :manifest
                  :role :validation :style :thick
                  :backing {:path [:parser-ir :manifest]}}
                 {:from :manifest :to :outputs
                  :role :derived-view :style :solid
                  :backing {:path [:manifest :tei]
                            :reachable-targets [:tei :rdf :iiif :tokenized
                                                :analysis :annotation]}}
                 {:from :parser-process :to :aat-detail
                  :role :implementation-detail
                  :style :dashed :head-port :w
                  :backing {:stages [:aat]}}
                 {:from :aat-detail :to :parser-ir
                  :role :implementation-detail
                  :style :dashed :backing {:path [:aat :parser-ir]}}]
         :current-inset (get-in metadata [:figures figure :current-inset])
         :identity-spine [:source :parser-process :parser-ir :manifest :outputs]
         :primary-order [:source :parser-process :parser-ir :manifest :outputs]
         :footer "Stable contract shown prominently; current AAT implementation shown as a dashed inset."}]
    (validate-graph! context graph)))

(defn graphs []
  (let [validated (model/validated-model)]
    [(reproducibility-graph validated)
     (publication-graph validated)]))
