(ns abc.tools.diagram.presentation-figures
  (:require [abc.tools.diagram.presentation-model :as model]))

(def theme
  {:canvas "#000000"
   :text "#F5F7FA"
   :secondary "#A7B0BE"
   :identity "#48CAE4"
   :evidence "#F2B84B"
   :output "#7BC47F"
   :title-size 52
   :primary-size 30
   :secondary-size 22
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
                                       coordinates))))]
    {:id family
     :label ({:source "Source identity"
              :parsing "Parsing identity"
              :publication "Publication contracts"
              :analysis "Linguistic analysis"
              :output "Output format"} family)
     :role :coordinate-family
     :group :identity-contract
     :coordinates coordinates
     :backing {:coordinates (mapv :id coordinates) :adrs owners}}))

(defn- path-valid? [context path]
  (and (<= 2 (count path))
       (every? (fn [[from to]]
                 (model/reachable? (:stage-edges context) from to))
               (partition 2 1 path))))

(defn- item-backing-problems [context {:keys [coordinates stages path
                                              reachable-targets adrs]}]
  (let [coordinate-set (:coordinates context)
        stage-set (set (keys (:stages context)))
        missing-coordinates (sort (remove coordinate-set coordinates))
        missing-stages (sort (remove stage-set stages))
        missing-targets (sort (remove stage-set reachable-targets))
        allowed-adrs (set (concat
                           (mapcat #(get-in context [:stages % :adr]) stages)
                           (mapcat #((:coordinate-owners context) %) coordinates)))
        invalid-adrs (sort (remove allowed-adrs adrs))
        path-source (first path)
        unreachable (sort (remove #(and path-source
                                        (model/reachable? (:stage-edges context)
                                                          path-source %))
                                  reachable-targets))]
    (cond-> []
      (seq missing-coordinates)
      (conj (str "unknown backing coordinates " missing-coordinates))
      (seq missing-stages)
      (conj (str "unknown backing stages " missing-stages))
      (seq missing-targets)
      (conj (str "unknown reachable targets " missing-targets))
      (seq invalid-adrs)
      (conj (str "citations are not canonical for backing " invalid-adrs))
      (and path (not (path-valid? context path)))
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
  (doseq [item (concat (:nodes graph) (:edges graph))]
    (when-not (map? (:backing item))
      (throw (ex-info "presentation item has no canonical backing"
                      {:figure (:id graph) :item item})))
    (when-let [failures (seq (item-backing-problems context (:backing item)))]
      (throw (ex-info "presentation item backing is invalid"
                      {:figure (:id graph) :item item :problems failures}))))
  graph)

(defn reproducibility-graph [{:keys [metadata context]}]
  (let [graph
        {:id :reproducibility
         :direction "LR"
         :title (get-in metadata [:figures :reproducibility :title])
         :subtitle (get-in metadata [:figures :reproducibility :subtitle])
         :description "Sources and computational evidence enter a canonical identity contract, producing a validated manifest and traceable scholarly views."
         :theme theme
         :groups [{:id :inputs :label "Sources and computational evidence"
                   :cluster? false}
                  {:id :identity-contract :label "15-coordinate identity contract"}
                  {:id :record :label "Durable scholarly record"}
                  {:id :derived :label "Derived scholarly views"}]
         :nodes (vec
                 (concat
                  [(aggregate-node metadata context :reproducibility :sources
                                   :evidence :inputs
                                   "Corpus, bibliographic metadata, and parser evidence")]
                  (map #(family-node metadata context %)
                       [:source :parsing :publication :analysis :output])
                  [{:id :artifact-id
                    :label "ArtifactID = SHA-256(JCS(manifest_identity_object))"
                    :subtitle "Canonical identity; distinct from the output byte hash"
                    :role :identity-formula :group :identity-contract
                    :backing {:coordinates (vec (sort (:coordinates context)))
                              :adrs (vec (sort (set (mapcat val (:coordinate-owners context)))))}}
                   (aggregate-node metadata context :reproducibility :manifest
                                   :identity :record
                                   "Identity, provenance, validation status, and content hash")
                   (aggregate-node metadata context :reproducibility :views
                                   :output :derived
                                   "TEI, visible text, RDF, Linked Art, IIIF, annotation, and analysis")]))
         :edges [{:from :sources :to :source :role :identity-input :style :solid
                  :backing {:coordinates ["corpus_snapshot_hash" "work_content_hash"
                                          "metadata_record_hash"]}}
                 {:from :sources :to :parsing :role :identity-input :style :solid
                  :backing {:coordinates ["parser_build_hash" "parser_config_hash"
                                          "aat_parser_ir_mapping_hash"
                                          "parser_ir_schema_hash"]}}
                 {:from :sources :to :publication :role :identity-input :style :solid
                  :backing {:coordinates ["manifest_schema_hash" "tei_profile_hash"]}}
                 {:from :sources :to :analysis :role :identity-input :style :solid
                  :backing {:coordinates ["tokenizer_build_hash"
                                          "tokenizer_dictionary_hash"
                                          "tokenizer_profile_hash"
                                          "analysis_recipe_hash"
                                          "annotation_policy_hash"]}}
                 {:from :sources :to :output :role :identity-input :style :solid
                  :backing {:coordinates ["output_format_spec_hash"]}}
                 {:from :source :to :artifact-id :role :identity :style :solid
                  :backing {:coordinates ["corpus_snapshot_hash" "work_content_hash"
                                          "metadata_record_hash"]}}
                 {:from :parsing :to :artifact-id :role :identity :style :solid
                  :backing {:coordinates ["parser_build_hash" "parser_config_hash"
                                          "aat_parser_ir_mapping_hash"
                                          "parser_ir_schema_hash"]}}
                 {:from :publication :to :artifact-id :role :identity :style :solid
                  :backing {:coordinates ["manifest_schema_hash" "tei_profile_hash"]}}
                 {:from :analysis :to :artifact-id :role :identity :style :solid
                  :backing {:coordinates ["tokenizer_build_hash"
                                          "tokenizer_dictionary_hash"
                                          "tokenizer_profile_hash"
                                          "analysis_recipe_hash"
                                          "annotation_policy_hash"]}}
                 {:from :output :to :artifact-id :role :identity :style :solid
                  :backing {:coordinates ["output_format_spec_hash"]}}
                 {:from :artifact-id :to :manifest :role :identity :style :thick
                  :backing {:stages [:manifest] :adrs (stage-adrs context [:manifest])}}
                 {:from :manifest :to :views :role :derived-view :style :solid
                  :backing {:path [:manifest :tei]
                            :reachable-targets [:tei :rdf :iiif :tokenized :analysis :annotation]}}]
         :footer "Changing an identity-bearing input produces a new ArtifactID and rebuilds only dependent layers."
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
         :description "A stable source-to-publication contract with the current AAT producer implementation shown as subordinate detail."
         :theme theme
         :groups [{:id :producer :label "Sources and parser evidence · ab-validator"}
                  {:id :abc :label "Publication contracts and materialization · ABC"}
                  {:id :scholarship :label "Scholarly publication and analysis"}
                  {:id :current-inset :label "Current producer implementation"
                   :style :dashed}]
         :nodes [(node :source :source :producer "Authoritative text and metadata")
                 (node :parser-process :evidence :producer "Versioned parser evidence and configuration")
                 (node :parser-ir :contract :abc "Stable publication-side interchange contract")
                 (node :manifest :identity :abc "Exact identity, provenance, validation, and content")
                 (node :outputs :output :scholarship "TEI, text, RDF, Linked Art, IIIF, annotations, and analysis")
                 {:id :aat-detail
                  :label "AAT evidence + mapping gate"
                  :subtitle "Current implementation detail"
                  :role :implementation-detail :group :current-inset
                  :label-wrap 18
                  :subtitle-wrap 18
                  :backing {:stages [:aat]
                            :adrs (stage-adrs context [:aat])}}]
         :edges [{:from :source :to :parser-process :label "parse + measure"
                  :role :evidence :style :solid
                  :backing {:path [:aozora-snapshot :aat]}}
                 {:from :parser-process :to :parser-ir :label "compatibility gate"
                  :role :validation :style :solid
                  :backing {:path [:aat :parser-ir]}}
                 {:from :parser-ir :to :manifest :label "identity + schema gate"
                  :role :validation :style :thick
                  :backing {:path [:parser-ir :manifest]}}
                 {:from :manifest :to :outputs :label "validated materialization"
                  :role :derived-view :style :solid
                  :backing {:path [:manifest :tei]
                            :reachable-targets [:tei :rdf :iiif :tokenized
                                                :analysis :annotation]}}
                 {:from :parser-process :to :aat-detail
                  :label "current detail" :role :implementation-detail
                  :style :dashed :backing {:stages [:aat]}}
                 {:from :aat-detail :to :parser-ir
                  :label "mapping compatibility" :role :implementation-detail
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
