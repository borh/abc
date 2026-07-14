(ns abc.tools.diagram.architecture-graph-evidence-test
  (:require [abc.tools.adr :as adr]
            [abc.tools.adr-evidence-runtime-inputs :as runtime-inputs]
            [abc.tools.diagram.architecture-graph :as arch]
            [abc.tools.files :as files]
            [clojure.string :as str]
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

(deftest architecture-graph-contract
  (runtime-inputs/with-validated-read-trace!
    {:identity-root "." :cwd-root "." :repo-root "." :workspace-root ".."
     :descriptor {:path "docs/evidence/adr-capture/architecture-graph-contract.edn"
                  :value (files/read-edn "docs/evidence/adr-capture/architecture-graph-contract.edn")}}
    (fn []
      (let [adrs (mapv #(adr/parse-adr "docs/adr" %) accepted-files)
            stages (arch/load-stages)
            contracts (files/read-json "schemas/schema-contracts.json")
            manifest (files/read-json "schemas/manifest.schema.json")
            prose (files/read-text "docs/architecture.md")
            rendered (files/read-text "docs/architecture.mmd")
            schema-paths (set (map #(get % "path") (get contracts "schemas")))
            declared-adrs (set (concat (map :num adrs)
                                       (mapcat :adr (:stages stages))))
            problems (arch/validate stages schema-paths declared-adrs manifest prose)]
        (is (every? #(= "Accepted" (:status %)) adrs))
        (is (empty? problems))
        (is (seq (:nodes (arch/graph-from (:stages stages)))))
        (is (str/starts-with? rendered "%% GENERATED"))))))
