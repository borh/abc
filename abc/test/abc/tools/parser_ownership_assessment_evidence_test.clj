(ns abc.tools.parser-ownership-assessment-evidence-test
  (:require [abc.tools.adr-evidence-runtime-inputs :as runtime-inputs]
            [abc.tools.files :as files]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]])
  (:import [java.time LocalDate]))

(def ^:private assessment-path
  "docs/evidence/external/custom-parser-ownership-assessment.md")

(defn custom-parser-ownership-assessment-operation []
  (let [assessment (files/read-text assessment-path)
        adr38 (files/read-text "docs/adr/0038-custom-parser-ownership-and-neutral-comparison.md")
        governance (files/read-edn "docs/adr/governance-as-of.edn")
        reports (mapv files/read-text
                      ["../ab-validator/docs/superpowers/reports/2026-07-08-aozora-parser-comparison-study.md"
                       "../ab-validator/docs/superpowers/reports/2026-07-08-parser-fork-candidacy-faithful-comparison.md"
                       "../ab-validator/docs/superpowers/specs/2026-07-09-parser-comparison-followups-handoff.md"])
        handoff (files/read-text "../ab-validator/docs/handoffs/2026-07-10-parser-fork-provenance.md")
        owned (mapv files/read-text
                    ["schemas/parser-ir.schema.json"
                     "schemas/manifest.schema.json"
                     "schemas/parser-ir-publication-preservation.schema.json"
                     "schemas/tei-validation-result.schema.json"
                     "data/parser-ir-publication-policy-v0.json"
                     "data/publication-policy.edn"])
        review-after (LocalDate/parse "2026-10-12")
        as-of (LocalDate/parse (str (:as-of governance)))]
    (doseq [line ["Assessment date: 2026-07-12"
                  "Review after: 2026-10-12"
                  "Decision owner: Soranoha project owner"
                  "Technical reviewer: Soranoha parser maintainer"
                  "Authority: development-only"
                  "Continued maintainer availability."
                  "Continued project control of the publication contract."
                  "Retirement of the custom parser in favor of an implementation that assumes"
                  "A superseding owner decision after maintainer capacity becomes inadequate."
                  "grants no exact-tuple admission"
                  "does not complete a neutral"
                  "does not qualify a release"
                  "grants no publication authority"]]
      (is (str/includes? assessment line) line))
    (doseq [family ["aozora-pipeline" "aozora-core" "aozora-rs-core"
                    "aozora2html" "aozora-epub3" "build-fresh"]]
      (is (str/includes? assessment family)))
    (is (str/includes? assessment "surveyed external implementations"))
    (is (str/includes? assessment "did not evaluate a preregistered neutral contract"))
    (is (str/includes? assessment "1a4f864603970983719655aa4af4525958ac2d38"))
    (is (str/includes? handoff "1a4f864603970983719655aa4af4525958ac2d38"))
    (is (every? seq reports))
    (is (every? seq owned))
    (is (str/includes? adr38 "Release authority: development"))
    (is (= as-of (LocalDate/parse "2026-07-12")))
    (is (= review-after (LocalDate/parse "2026-10-12")))
    true))

(deftest custom-parser-ownership-assessment-contract
  (runtime-inputs/with-validated-read-trace!
    {:identity-root ".." :cwd-root "." :repo-root "." :workspace-root ".."
     :descriptor {:path "abc/docs/evidence/adr-capture/parser-ownership-assessment.edn"
                  :value (files/read-edn "docs/evidence/adr-capture/parser-ownership-assessment.edn")}}
    custom-parser-ownership-assessment-operation))

(deftest custom-parser-ownership-assessment-contract-test
  (custom-parser-ownership-assessment-operation))
