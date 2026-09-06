(ns ab-research.parser-evidence
  (:require [ab-research.edn-registry :as registry]
            [ab-research.files :as files]
            [ab-research.hash :as hash]
            [ab-research.malli :as am]
            [ab-research.path-containment :as path-containment]
            [clojure.string :as string]
            [malli.core :as m]
            [malli.registry :as mr]))

(def index-path
  (files/path "data" "parser-evidence-citations.edn"))

(def schemas
  "Domain-owned Malli schemas for the parser evidence index."
  {::parser-evidence-entry
   [:and
    [:map
     [:evidence_id ::am/nonblank-string]
     [:evidence_class [:enum {:error/message "must be conversion-compatibility, parser-selection, comparator-oracle, or neutral-comparison"}
                       :conversion-compatibility :parser-selection :comparator-oracle :neutral-comparison]]
     [:producer_component ::am/nonblank-string]
     [:logical_path ::am/workspace-logical-path]
     [:current_external_path {:optional true} ::am/nullable-nonblank-string]
     [:sha256 ::am/sha256-hash]
     ;; Neutral-comparison evidence binds to its frozen study contract by hash so
     ;; a report can be cited as measurement while remaining structurally distinct
     ;; from the admission/release evidence classes.
     [:study_contract {:optional true} ::am/sha256-hash]
     [:status [:enum {:error/message "must be citable, provisional, or superseded"}
               :citable :provisional :superseded]]
     [:summary ::am/nonblank-string]]
    ;; MIN-1: couple :study_contract to :evidence_class so the study-contract
    ;; binding cannot drift across evidence classes. Admission-class evidence
    ;; (:conversion-compatibility, the only class ADR 0023 admits) records an
    ;; exact registry tuple, not a research study, so it MUST NOT carry a study
    ;; contract; a :neutral-comparison report is a preregistered study
    ;; measurement, so it MUST bind to its frozen study contract by hash.
    [:fn {:error/message ":conversion-compatibility evidence must not carry a :study_contract"}
     (fn [entry]
       (or (not= :conversion-compatibility (:evidence_class entry))
           (not (contains? entry :study_contract))))]
    [:fn {:error/message ":neutral-comparison evidence must carry a :study_contract"}
     (fn [entry]
       (or (not= :neutral-comparison (:evidence_class entry))
           (contains? entry :study_contract)))]]})

(def malli-registry
  "This domain's explicit Malli registry: core schemas + shared scalars
  + the schemas owned here. Never installed globally."
  (mr/composite-registry (m/default-schemas) am/scalar-schemas schemas))

(defn- parser-evidence-malli-errors
  [idx entry]
  (if-let [explanation (m/explain ::parser-evidence-entry entry
                                  {:registry malli-registry})]
    (mapv #(str "Parser evidence index entry " idx " " %)
          (am/explanation-messages explanation))
    []))

(defn- entry-errors
  [idx entry]
  (if-not (map? entry)
    [(str "Parser evidence index entry " idx " must be a map")]
    (parser-evidence-malli-errors idx entry)))

;; --- Evidence-class admission/release boundary --------------------------------
;;
;; A structural allowlist keyed off :evidence_class, not off the absence of any
;; particular field. Admission (ADR 0023 exact-tuple registry) and release
;; qualification (separately authorized, ADR 0038) each name the exact evidence
;; classes that may support the claim. Comparison/selection research evidence —
;; the historical :parser-selection rows and the new :neutral-comparison rows —
;; is deliberately outside both allowlists, so a comparison citation is
;; structurally incapable of admitting or release-qualifying a parser regardless
;; of its :status or which fields it carries (ADR 0030 §Evidence policy,
;; ADR 0038-C2). Downstream gates (e.g. the release gate) key their predicates
;; off these sets rather than re-deriving the policy.

(def comparison-evidence-classes
  "Evidence classes that record parser comparison / selection research only.
  They are neither an admission nor a release evidence class."
  #{:parser-selection :neutral-comparison})

(def admission-evidence-classes
  "Structural allowlist of evidence classes eligible to support an exact-tuple
  admission claim. Admission is controlled by ADR 0023 exact registry tuples;
  only conversion-compatibility evidence is an admission class."
  #{:conversion-compatibility})

(def release-evidence-classes
  "Structural allowlist of evidence classes eligible to support a release /
  qualification claim. Release authority is separate (ADR 0038); no
  comparison/selection class qualifies a release, so the allowlist excludes
  every comparison class by construction."
  #{:conversion-compatibility})

(defn admission-evidence-class?
  "True when `evidence-class` is on the admission allowlist."
  [evidence-class]
  (contains? admission-evidence-classes evidence-class))

(defn release-evidence-class?
  "True when `evidence-class` is on the release-qualification allowlist."
  [evidence-class]
  (contains? release-evidence-classes evidence-class))

(defn entry-admissible?
  "True when `entry`'s evidence class may support an admission claim."
  [entry]
  (admission-evidence-class? (:evidence_class entry)))

(defn entry-release-qualifying?
  "True when `entry`'s evidence class may support a release/qualification claim."
  [entry]
  (release-evidence-class? (:evidence_class entry)))

(defn assert-admission-evidence!
  "Return `entry` when its evidence class is admission-eligible; otherwise throw.
  The rejection is structural: comparison citations never pass this boundary."
  [entry]
  (if (entry-admissible? entry)
    entry
    (throw (ex-info "evidence class cannot support an admission claim"
                    {:kind :non-admission-evidence-class
                     :evidence-id (:evidence_id entry)
                     :evidence-class (:evidence_class entry)
                     :admission-evidence-classes admission-evidence-classes}))))

(defn assert-release-evidence!
  "Return `entry` when its evidence class is release-eligible; otherwise throw.
  The rejection is structural: comparison citations never pass this boundary."
  [entry]
  (if (entry-release-qualifying? entry)
    entry
    (throw (ex-info "evidence class cannot support a release claim"
                    {:kind :non-release-evidence-class
                     :evidence-id (:evidence_id entry)
                     :evidence-class (:evidence_class entry)
                     :release-evidence-classes release-evidence-classes}))))

(defn evidence-id-key [entry] (:evidence_id entry))

(defn logical-file-key [entry] [(:logical_path entry) (:sha256 entry)])

(defn citation-file-problems
  [monorepo-root index]
  (->> (:entries index)
       (keep (fn [{:keys [evidence_id logical_path sha256]}]
               (let [{:keys [state path]}
                     (path-containment/path-state monorepo-root logical_path)
                     kind (case state
                            :missing :citation-missing
                            :path-traversal :citation-path-traversal
                            :malformed-path :citation-path-traversal
                            :real-path-escape :citation-real-path-escape
                            :ok (when-not (= sha256
                                             (str "sha256:"
                                                  (hash/sha256-file path)))
                                  :citation-hash-mismatch))]
                 (when kind
                   (sorted-map :kind kind
                               :evidence-id evidence_id
                               :logical-path logical_path)))))
       vec))

(defn duplicate-values [entries key-fn label]
  (->> entries
       (map-indexed (fn [idx entry] [(key-fn entry) idx]))
       (filter (fn [pair] (some? (first pair))))
       (group-by first)
       (keep (fn [[k pairs]]
               (when (< 1 (count pairs))
                 (str "Parser evidence index duplicates " label
                      " " k " at entries "
                      (string/join ", " (map second pairs))))))))

(defn- duplicate-errors
  [entries]
  (vec (concat (duplicate-values entries evidence-id-key ":evidence_id")
               (duplicate-values entries logical-file-key
                                 "logical_path + sha256"))))

(defn index-errors
  [index]
  (registry/registry-errors
   {:registry index
    :label "Parser evidence index"
    :entry-error-fn entry-errors
    :duplicate-error-fn duplicate-errors}))

(defn validate-index!
  [index]
  (let [errors (index-errors index)]
    (when (seq errors)
      (throw (ex-info (string/join "\n" errors)
                      {:errors errors})))
    :ok))

(defn load-index
  []
  (let [index (files/read-edn index-path)]
    (validate-index! index)
    index))

(defn- citable-hashes-of
  "Citable sha256 hashes of an already-validated index value."
  [index]
  (->> (:entries index)
       (filter #(= :citable (:status %)))
       (map :sha256)
       sort
       distinct
       vec))

(defn citable-hashes
  "Load and validate the committed evidence index, then return its
  citable sha256 hashes."
  []
  (citable-hashes-of (load-index)))
