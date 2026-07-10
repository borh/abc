(ns abc.tools.diagram.presentation-figures-test
  (:require [abc.tools.diagram.presentation-figures :as figures]
            [abc.tools.diagram.presentation-model :as model]
            [clojure.test :refer [deftest is testing]]))

(defn- graph [id]
  (some #(when (= id (:id %)) %) (figures/graphs)))

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
    (is (= "SHA-256(JCS(manifest_identity_object)) · canonical identity; distinct from the output byte hash"
           (:subtitle artifact-id)))
    (is (= 42 (:subtitle-wrap artifact-id)))))

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
        valid-node {:id :declared :backing {:stages [:manifest]}}
        valid-edge {:from :declared :to :other :role :validation
                    :backing {:path [:parser-ir :manifest]}}
        graph-with-node-backing
        (fn [backing]
          {:id :broken-node
           :nodes [(assoc valid-node :backing backing)
                   {:id :other :backing {:stages [:manifest]}}]
           :edges [valid-edge]})
        graph-with-edge-backing
        (fn [role backing]
          {:id :broken-edge
           :nodes [valid-node
                   {:id :other :backing {:stages [:manifest]}}]
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
