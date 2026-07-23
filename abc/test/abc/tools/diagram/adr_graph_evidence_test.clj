(ns abc.tools.diagram.adr-graph-evidence-test
  (:require [abc.tools.adr :as adr]
            [abc.tools.diagram.adr-graph :as graph]
            [abc.tools.files :as files]
            [clojure.test :refer [deftest is]]))

(def accepted-files
  ["0001-manifest-identity.md"
   "0002-parser-evaluation.md"
   "0006-v0-design-bundle-validation.md"
   "0007-external-parser-validation-boundary.md"
   "0008-abc-tools-runtime.md"
   "0009-imported-output-materialization.md"
   "0010-manifest-identity-hardening.md"
   "0011-generated-fixture-policy.md"
   "0012-tei-odd-schematron-validation.md"
   "0013-cultural-heritage-lod-profile.md"
   "0014-iiif-applicability.md"
   "0015-temporal-modeling.md"
   "0016-edtf-level1-decade-century.md"
   "0017-vocabulary-review.md"
   "0018-predicate-rename-batch-1.md"
   "0020-person-identity-drift-data-model.md"
   "0021-person-identity-drift-harness.md"
   "0022-upstream-ingest-drift-awareness.md"
   "0023-owned-aat-parser-ir-mapping.md"
   "0024-parser-ir-span-and-ruby-direction.md"
   "0025-parser-ir-publication-rendering.md"
   "0029-diagrams-as-gated-derived-views.md"
   "0030-aozora-parser-selection.md"
   "0031-adr-governance-validation.md"
   "0032-parser-fork-hard-detach.md"
   "0033-source-bundle-identity.md"
   "0038-custom-parser-ownership-and-neutral-comparison.md"])

(deftest adr-graph-contract
  (let [adrs (mapv #(adr/parse-adr "docs/adr" %) accepted-files)
        relations (graph/load-relations)
        value (graph/graph-from adrs relations)]
    (is (every? #(= "Accepted" (:status %)) adrs))
    (is (empty? (graph/lint-adrs adrs relations)))
    (is (some #(re-find #" — " (:label % "")) (:edges value)))
    (is (seq (files/read-text "docs/adr/adr-graph.mmd")))
    (is (seq (graph/lint-adrs adrs [{:from 1 :to 9999 :type :extends}])))
    (is (seq (graph/lint-adrs adrs [{:from 1 :to 2 :type :amends}])))
    (is (seq (graph/lint-adrs adrs [{:from "bad" :to 2 :type :extends}])))
    (is (seq (graph/lint-adrs adrs [{:from 1 :to 2 :type :unknown}])))))
