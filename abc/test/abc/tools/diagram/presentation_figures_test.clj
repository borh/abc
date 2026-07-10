(ns abc.tools.diagram.presentation-figures-test
  (:require [abc.tools.diagram.presentation-figures :as figures]
            [abc.tools.diagram.presentation-model :as model]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn- graph [id]
  (some #(when (= id (:id %)) %) (figures/graphs)))

(defn- validation-problems [context graph]
  (try
    (figures/validate-graph! context graph)
    nil
    (catch clojure.lang.ExceptionInfo ex
      (:problems (ex-data ex)))))

(def expected-topology
  {:reproducibility
   {:nodes #{:sources :source :parsing :publication :analysis :output
             :artifact-id :manifest :views}
    :edges #{[:sources :source] [:sources :parsing]
             [:sources :publication] [:sources :analysis] [:sources :output]
             [:source :artifact-id] [:parsing :artifact-id]
             [:publication :artifact-id] [:analysis :artifact-id]
             [:output :artifact-id] [:artifact-id :manifest]
             [:manifest :views]}}
   :publication
   {:nodes #{:source :parser-process :parser-ir :manifest :outputs :aat-detail}
    :edges #{[:source :parser-process] [:parser-process :parser-ir]
             [:parser-ir :manifest] [:manifest :outputs]
             [:parser-process :aat-detail] [:aat-detail :parser-ir]}}})

(defn- semantic-topology [graph]
  {:nodes (set (map :id (:nodes graph)))
   :edges (set (map (juxt :from :to) (:edges graph)))})

(deftest editorial-revision-preserves-semantic-topology
  (doseq [[id expected] expected-topology]
    (is (= expected (semantic-topology (graph id))) (name id))))

(deftest reproducibility-editorial-contract
  (let [g (graph :reproducibility)
        group (some #(when (= :identity-contract (:id %)) %) (:groups g))
        nodes (into {} (map (juxt :id identity) (:nodes g)))]
    (is (= "Versioned identity contract" (:label group)))
    (is (= "Identity-bearing inputs determine ArtifactID." (:footer g)))
    (is (not (str/includes? (:footer g) "rebuilds only dependent layers")))
    (is (= [{:id "output_format_spec_hash" :label "Output format"}]
           (:coordinates (:output nodes))))
    (is (= 34 (get-in g [:theme :primary-size])))
    (is (= 24 (get-in g [:theme :secondary-size])))
    (is (= #{"#000000" "#F5F7FA" "#A7B0BE"
             "#48CAE4" "#F2B84B" "#7BC47F"}
           (set (map (get-in g [:theme])
                     [:canvas :text :secondary :identity :evidence :output]))))
    (is (= "Sources and evidence enter a versioned identity contract; ArtifactID identifies a validated manifest from which scholarly views derive."
           (:description g)))
    (is (= "Sources · metadata · parser evidence"
           (:subtitle (:sources nodes))))
    (is (= "SHA-256 of canonical manifest identity"
           (:subtitle (:artifact-id nodes))))))

(deftest publication-copy-is-presentation-brief
  (let [g (graph :publication)
        subtitles (into {} (map (juxt :id :subtitle) (:nodes g)))]
    (is (= "A stable source-to-Parser-IR-to-manifest publication path, with the current AAT producer implementation shown as subordinate detail."
           (:description g)))
    (is (= "Text · metadata" (:source subtitles)))
    (is (= "Versioned evidence · configuration" (:parser-process subtitles)))
    (is (= "Stable publication interchange" (:parser-ir subtitles)))
    (is (= "Identity · provenance · validation · content" (:manifest subtitles)))
    (is (= "TEI · text · RDF · Linked Art · IIIF · annotation · analysis"
           (:outputs subtitles)))
    (is (nil? (:aat-detail subtitles)))))

(deftest reproducibility-figure-has-approved-title-and-five-coordinate-families
  (let [g (graph :reproducibility)
        family-nodes (filter #(= :coordinate-family (:role %)) (:nodes g))
        artifact-id (some #(when (= :artifact-id (:id %)) %) (:nodes g))]
    (is (= "Soranoha Reproducibility Architecture" (:title g)))
    (is (false? (:cluster? (some #(when (= :inputs (:id %)) %) (:groups g)))))
    (is (= #{:source :parsing :publication :analysis :output}
           (set (map :id family-nodes))))
    (is (= 15 (reduce + (map #(count (:coordinates %)) family-nodes))))
    (is (= "ArtifactID" (:label artifact-id)))
    (is (= "SHA-256 of canonical manifest identity"
           (:subtitle artifact-id)))
    (is (nil? (:subtitle-wrap artifact-id)))))

(deftest publication-figure-keeps-stable-path-and-current-inset-separate
  (let [g (graph :publication)]
    (is (= "Soranoha Publication Pipeline" (:title g)))
    (is (= [:source :parser-process :parser-ir :manifest :outputs]
           (:primary-order g)))
    (is (= [:aozora-snapshot :aat :parser-ir]
           (get-in g [:current-inset :path])))
    (is (= "Current producer implementation"
           (get-in g [:current-inset :label])))
    (is (every? #(= 18 (:label-wrap %)) (:nodes g)))
    (is (every? #(= 18 (:subtitle-wrap %)) (:nodes g)))
    (is (some #(= :aat-detail (:id %)) (:nodes g)))
    (is (= :dashed
           (:style (some #(when (= :current-inset (:id %)) %) (:groups g)))))
    (is (= :w
           (:head-port
            (some #(when (and (= :parser-process (:from %))
                              (= :aat-detail (:to %)))
                     %)
                  (:edges g))))
        "the inset edge enters below the cluster heading")
    (is (every? (comp nil? :label) (:edges g))
        "edge xlabels must not obscure the presentation spine")))

(deftest every-node-and-semantic-edge-has-canonical-backing
  (doseq [g (figures/graphs)
          item (concat (:nodes g) (:edges g))]
    (is (map? (:backing item)) (str (:id g) " " item))))

(deftest graph-validation-rejects-phantom-edge-endpoints
  (let [context (:context (model/validated-model))]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"undeclared node"
         (figures/validate-graph!
          context
          {:id :broken
           :nodes [{:id :declared :backing {:stages [:manifest]}}]
           :edges [{:from :declared :to :typo
                    :backing {:stages [:manifest]}}]})))))

(deftest graph-validation-requires-recognized-non-empty-well-formed-backing
  (let [context (:context (model/validated-model))
        valid-node {:id :declared :role :identity
                    :backing {:stages [:manifest]}}
        valid-edge {:from :declared :to :other :role :validation
                    :backing {:path [:parser-ir :manifest]}}
        graph-with-node-backing
        (fn [backing]
          {:id :broken-node
           :nodes [(assoc valid-node :backing backing)
                   {:id :other :role :identity
                    :backing {:stages [:manifest]}}]
           :edges [valid-edge]})
        graph-with-edge-backing
        (fn [role backing]
          {:id :broken-edge
           :nodes [valid-node
                   {:id :other :role :identity
                    :backing {:stages [:manifest]}}]
           :edges [(assoc valid-edge :role role :backing backing)]})]
    (doseq [[label graph]
            [["empty map" (graph-with-node-backing {})]
             ["unknown key" (graph-with-node-backing {:unknown [:manifest]})]
             ["unknown key alongside a relation"
              (graph-with-node-backing {:stages [:manifest] :unknown true})]
             ["empty relation" (graph-with-node-backing {:stages []})]
             ["malformed relation" (graph-with-node-backing {:stages :manifest})]
             ["malformed citations"
              (graph-with-node-backing {:stages [:manifest] :adrs "0001"})]
             ["node path" (graph-with-node-backing {:path [:parser-ir :manifest]})]
             ["edge stages for a validation relation"
              (graph-with-edge-backing :validation {:stages [:manifest]})]
             ["edge coordinates for a path relation"
              (graph-with-edge-backing :validation
                                       {:coordinates ["manifest_schema_hash"]})]
             ["reachable targets without a path"
              (graph-with-edge-backing :derived-view
                                       {:reachable-targets [:tei]})]
             ["malformed path"
              (graph-with-edge-backing :validation {:path :manifest})]]]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"backing is invalid"
           (figures/validate-graph! context graph))
          label))))

(deftest graph-validation-rejects-unknown-and-zero-hop-path-members
  (let [context (:context (model/validated-model))
        graph-with-path
        (fn [path]
          {:id :path-backing
           :nodes [{:id :from :role :evidence
                    :backing {:stages [:aat]}}
                   {:id :to :role :contract
                    :backing {:stages [:parser-ir]}}]
           :edges [{:from :from :to :to :role :validation
                    :backing {:path path
                              :reachable-targets [:parser-ir]}}]})]
    (doseq [[label path expected]
            [["unknown self path" [:missing :missing]
              ["unknown backing path stages" "zero-hop"]]
             ["unknown destination" [:aat :missing]
              ["unknown backing path stages"]]
             ["consecutive identical member" [:aat :aat :parser-ir]
              ["zero-hop"]]]]
      (let [problems (validation-problems context (graph-with-path path))]
        (is (seq problems) label)
        (doseq [fragment expected]
          (is (some #(str/includes? % fragment) problems) label))
        (is (not-any? #(str/includes? % "does not resolve") problems)
            (str label " is rejected before reachability"))))
    (let [reachability-calls (atom 0)]
      (with-redefs [model/reachable? (fn [& _]
                                       (swap! reachability-calls inc)
                                       false)]
        (is (seq (validation-problems context
                                      (graph-with-path [:aat :missing])))))
      (is (zero? @reachability-calls)
          "unknown path stages prevent reachability evaluation"))
    (is (= (graph-with-path [:aat :parser-ir])
           (figures/validate-graph! context
                                    (graph-with-path [:aat :parser-ir]))))))

(deftest graph-validation-enforces-node-role-backing-relations
  (let [context (:context (model/validated-model))]
    (doseq [[label node]
            [["output node cannot use coordinate backing"
              {:id :output :role :output
               :backing {:coordinates ["manifest_schema_hash"]}}]
             ["unknown node role cannot use stage backing"
              {:id :unknown :role :imaginary
               :backing {:stages [:manifest]}}]]]
      (let [problems (validation-problems
                      context {:id :node-role :nodes [node] :edges []})]
        (is (seq problems) label)
        (is (some #(str/includes? % "node backing relation is invalid for role")
                  problems)
            label)))))

(deftest canonical-graphs-satisfy-strict-backing-validation
  (let [context (:context (model/validated-model))]
    (doseq [graph (figures/graphs)]
      (is (= graph (figures/validate-graph! context graph))))))

(deftest publication-output-summary-is-backed-by-live-output-stages
  (let [g (graph :publication)
        outputs (some #(when (= :outputs (:id %)) %) (:nodes g))]
    (is (= #{:tei :rdf :iiif :tokenized :analysis :annotation}
           (set (get-in outputs [:backing :stages]))))))

(deftest existing-mermaid-graph-shape-remains-valid
  (testing "the enriched fields are additive"
    (doseq [g (figures/graphs)]
      (is (string? (:direction g)))
      (is (sequential? (:nodes g)))
      (is (sequential? (:edges g))))))
