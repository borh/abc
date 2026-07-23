(ns abc.tools.diagram.diagram-registry-evidence-test
  (:require [abc.tools.adr :as adr]
            [abc.tools.diagram.adr-graph :as adr-graph]
            [abc.tools.diagram.architecture-graph :as architecture-graph]
            [abc.tools.diagram.registry :as registry]
            [abc.tools.files :as files]
            [abc.tools.json :as json]
            [clojure.test :refer [deftest is]]))

(def accepted-files
  ["0001-manifest-identity.md" "0002-parser-evaluation.md"
   "0006-v0-design-bundle-validation.md" "0007-external-parser-validation-boundary.md"
   "0008-abc-tools-runtime.md" "0009-imported-output-materialization.md"
   "0010-manifest-identity-hardening.md" "0011-generated-fixture-policy.md"
   "0012-tei-odd-schematron-validation.md" "0013-cultural-heritage-lod-profile.md"
   "0014-iiif-applicability.md" "0015-temporal-modeling.md"
   "0016-edtf-level1-decade-century.md" "0017-vocabulary-review.md"
   "0018-predicate-rename-batch-1.md" "0020-person-identity-drift-data-model.md"
   "0021-person-identity-drift-harness.md" "0022-upstream-ingest-drift-awareness.md"
   "0023-owned-aat-parser-ir-mapping.md" "0024-parser-ir-span-and-ruby-direction.md"
   "0025-parser-ir-publication-rendering.md" "0029-diagrams-as-gated-derived-views.md"
   "0030-aozora-parser-selection.md" "0031-adr-governance-validation.md"
   "0032-parser-fork-hard-detach.md" "0033-source-bundle-identity.md"
   "0038-custom-parser-ownership-and-neutral-comparison.md"])

(deftest diagram-registry-contract
  (let [adrs (mapv #(adr/parse-adr "docs/adr" %) accepted-files)
        relations (:relations (files/read-edn "docs/adr/adr-relations.edn"))
        stages (files/read-edn "docs/architecture-stages.edn")
        contracts (json/read-json-file "schemas/schema-contracts.json")
        manifest-schema (json/read-json-file "schemas/manifest.schema.json")
        architecture-markdown (files/read-text "docs/architecture.md")
        declared-owners (set (mapcat :adr (:stages stages)))
        known-adrs (into (set (map :num adrs)) declared-owners)]
    (is (= #{:adr-decision-map :architecture}
           (set (map :id registry/committed-diagrams))))
    (is (every? #(= "Accepted" (:status %)) adrs))
    (is (empty? (adr-graph/lint-adrs adrs relations)))
    (is (seq (adr-graph/graph-from adrs relations)))
    (is (empty? (architecture-graph/validate
                 stages
                 (set (map #(get % "path") (get contracts "schemas")))
                 known-adrs manifest-schema architecture-markdown)))
    (is (seq (architecture-graph/graph-from (:stages stages))))
    (is (seq (files/read-text "docs/adr/adr-graph.mmd")))
    (is (seq (files/read-text "docs/architecture.mmd")))))
