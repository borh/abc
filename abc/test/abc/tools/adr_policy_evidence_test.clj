(ns abc.tools.adr-policy-evidence-test
  (:require [abc.tools.adr :as adr]
            [abc.tools.adr-evidence-runtime-inputs :as runtime-inputs]
            [abc.tools.evidence-io :as evidence-io]
            [abc.tools.files :as files]
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

(def malformed-accepted
  (str "# ADR 0042: Malformed policy fixture\n\n"
       "Status: Accepted\nDate: 2026-07-14\nAccepted: 2026-07-14\n"
       "Validation scope: structural\nRelease authority: none\n\n"
       "## Decision\n\nFixture.\n\n## Implementation Status\n\nFixture.\n\n"
       "## Acceptance Criteria\n\n- criterion without a typed claim header\n"))

(deftest adr-policy-fixtures-contract
  (runtime-inputs/with-validated-read-trace!
    {:identity-root "." :cwd-root "." :repo-root "." :workspace-root ".."
     :descriptor {:path "docs/evidence/adr-capture/adr-policy-fixtures.edn"
                  :value (files/read-edn "docs/evidence/adr-capture/adr-policy-fixtures.edn")}}
    (fn []
      (let [adrs (mapv #(adr/parse-adr "docs/adr" %) accepted-files)
            relations (:relations (files/read-edn "docs/adr/adr-relations.edn"))
            compatibility (files/read-edn "docs/adr/claim-evidence-compatibility.edn")
            governance (files/read-edn "docs/adr/governance-as-of.edn")]
        (is (every? #(= "Accepted" (:status %)) adrs))
        (is (every? #(empty? (:parse-problems %)) adrs))
        (is (every? #(empty? (:claim-problems %)) adrs))
        (is (seq relations))
        (is (every? set? (vals compatibility)))
        (is (string? (:as-of governance)))
        (evidence-io/with-owned-ephemeral-root
          (fn [root]
            (files/write-text! (str root "/0042-malformed.md") malformed-accepted)
            (let [fixture (adr/parse-adr root "0042-malformed.md")]
              (is (= #{:missing-claim-header}
                     (set (map :kind (:claim-problems fixture))))))))))))
