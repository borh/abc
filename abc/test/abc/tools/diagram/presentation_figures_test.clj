(ns abc.tools.diagram.presentation-figures-test
  (:require [abc.tools.diagram.presentation-figures :as figures]
            [abc.tools.diagram.presentation-model :as model]
            [clojure.test :refer [deftest is testing]]))

(defn- graph [id]
  (some #(when (= id (:id %)) %) (figures/graphs)))

(deftest reproducibility-figure-has-approved-title-and-five-coordinate-families
  (let [g (graph :reproducibility)
        family-nodes (filter #(= :coordinate-family (:role %)) (:nodes g))]
    (is (= "Soranoha Reproducibility Architecture" (:title g)))
    (is (false? (:cluster? (some #(when (= :inputs (:id %)) %) (:groups g)))))
    (is (= #{:source :parsing :publication :analysis :output}
           (set (map :id family-nodes))))
    (is (= 15 (reduce + (map #(count (:coordinates %)) family-nodes))))
    (is (some #(= "ArtifactID = SHA-256(JCS(manifest_identity_object))"
                  (:label %))
              (:nodes g)))))

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
           (:style (some #(when (= :current-inset (:id %)) %) (:groups g)))))))

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
