(ns soranoha.za.assemble
  "Release assembly: the bridge from kernel outputs to the publication
  transaction's input. The kernel builds every selected work policy-blind;
  admission is decided here — the assessment snapshot commits facts or attributed reliance,
  the inclusion rule derives the total admitted/excluded/quarantined
  partition, and both evidence artifacts publish with the release. Works
  are the admitted slugs minus the chain's withdrawn set; every published
  artifact byte is read from the kura CAS; the validation summary is
  derived from the same per-work validation records the chain verifier
  re-derives it from. A failed validation is an artifact and a summary
  entry, never an exclusion."
  (:require [soranoha.core.hash :as hash]
            [soranoha.kura.cas :as cas]
            [soranoha.snh.decode :as decode]
            [soranoha.snh.admission :as admission]
            [soranoha.snh.verify :as verify]))

(defn- fail! [reason data]
  (throw (ex-info (str "release assembly failed: " (name reason))
                  (assoc data :reason reason))))

;; --- evidence artifacts -----------------------------------------------------

(defn- snapshot-value
  "Sort candidates and independent contributions, retaining reliance payloads."
  [candidates]
  {"schema" "snh-assessment-snapshot/2"
   "candidates"
   (vec (sort-by #(get % "slug")
                 (map (fn [candidate]
                        (if (contains? candidate "reliance") candidate
                            (update candidate "contributions"
                                    (fn [contributions]
                                      (vec (sort-by #(get % "contribution_id")
                                                    contributions))))))
                      candidates)))})

(defn- report-value
  [snapshot-id {:keys [admitted excluded quarantined]} policy-hash rule]
  {"schema" "snh-admission-report/1"
   "assessment_snapshot" snapshot-id
   "policy_hash" policy-hash
   "inclusion_rule_id" (get rule "id")
   "inclusion_rule_hash" (hash/sha256-canonical-json rule)
   "admitted" (vec (sort admitted))
   "excluded" (vec (sort-by #(get % "slug") excluded))
   "quarantined" (vec (sort-by #(get % "slug") quarantined))})

;; --- works ------------------------------------------------------------------

(def ^:private artifact-kinds ["plaintext" "tei" "tei-validation"])

(defn- cas-blob ^bytes [cas-dir hex]
  (or (cas/get-bytes cas-dir hex)
      (fail! :artifact-missing-from-cas {:hex hex})))

(defn- bare-hex [source-content-hash]
  (or (some->> source-content-hash (re-matches #"sha256:([0-9a-f]{64})") second)
      (fail! :malformed-source-content-hash {:value source-content-hash})))

(defn- work-entry
  "Manifest works[] entry + its blob map for one slug's kernel outputs.
  Artifact order is the schema's fixed [plaintext, tei, tei-validation]."
  [cas-dir slug outputs]
  (let [blobs (mapv (fn [kind]
                      (let [hex (or (get outputs (keyword kind))
                                    (fail! :work-output-missing
                                           {:slug slug :kind kind}))]
                        [kind hex (cas-blob cas-dir hex)]))
                    artifact-kinds)]
    {:entry {"slug" slug
             "source_content_hash" (bare-hex (:source-content-hash outputs))
             "artifacts" (mapv (fn [[kind hex ^bytes bytes]]
                                 {"type" kind
                                  "id" (str "snh:1:" kind ":" hex)
                                  "bytes" (alength bytes)})
                               blobs)}
     :blobs (into {} (map (fn [[_ hex bytes]] [hex bytes])) blobs)}))

(defn- validation-failed?
  ;; the same consumed contract the chain verifier re-derives the summary
  ;; under; a noncontractual kernel record fails assembly here rather than
  ;; at pre-push verification
  [cas-dir hex]
  (= "failed"
     (:status (verify/consumed-validation-record (cas-blob cas-dir hex)))))

;; --- release ----------------------------------------------------------------

(defn toolchain-value
  "snh-manifest/1 toolchain object from engine-shaped stages: per stage id,
  the toolchain identity and stage code version exactly as its derivation
  keys carry them. nix_closure_hash holds that derivation toolchain
  identity — the wrapper-supplied Nix closure hash for nix-provisioned
  stages, the hashed binary/profile identity for subprocess stages; it is
  provenance, never an input to artifact identity."
  [stages]
  (into (sorted-map)
        (map (fn [{:keys [stage-id stage-version toolchain-id]}]
               [stage-id {"nix_closure_hash" toolchain-id
                          "stage_code_version" stage-version}]))
        stages))

(defn- assemble-release
  "One desired release as the transaction's assemble result
  {:core :blobs :selection}.
  - :cas-dir — the kura CAS every artifact byte is read from;
  - :corpus / :toolchain / :selection-params / :policy-id / :policy-hash —
    manifest coordinates;
  - :candidates — snapshot candidate values covering the assessed
    population (the totality gate compares their slugs against
    :selection, the kernel's selected slug set, so an unassessed or
    unselected candidate blocks emission);
  - :works — slug -> {:plaintext :tei :tei-validation <cas hex>,
    :source-content-hash \"sha256:<hex>\"} kernel outputs, covering at
    least every admitted candidate;
  - :withdrawn-slugs — the chain head's withdrawn set; works = admitted
    minus withdrawn."
  [{:keys [cas-dir corpus toolchain selection-params policy-id policy-hash
           candidates works withdrawn-slugs selection]}]
  (let [snapshot-enc (decode/encode "assessment-snapshot"
                                    (snapshot-value candidates))
        rule admission/inclusion-rule
        _ (doseq [{:strs [slug reliance]} candidates
                  :when (= "relied-upon" (get reliance "status"))]
            (when-not (= (get reliance "source_content_hash") (:source-content-hash (get works slug)))
              (fail! :reliance-source-content-mismatch {:slug slug})))
        partition (admission/partition-candidates rule
                                                  (get (:value snapshot-enc) "candidates"))
        report-enc (decode/encode "admission-report"
                                  (report-value (:id snapshot-enc) partition
                                                policy-hash rule))
        published (vec (sort (remove (set withdrawn-slugs)
                                     (:admitted partition))))
        entries (mapv (fn [slug]
                        (work-entry cas-dir slug
                                    (or (get works slug)
                                        (fail! :admitted-work-not-built
                                               {:slug slug}))))
                      published)
        invalid (vec (filter #(validation-failed?
                               cas-dir (:tei-validation (get works %)))
                             published))]
    {:core {"schema" "snh-manifest/1"
            "corpus" corpus
            "toolchain" toolchain
            "selection_params" selection-params
            "admission" {"policy_id" policy-id
                         "policy_hash" policy-hash
                         "inclusion_rule_id" (get rule "id")
                         "inclusion_rule_hash" (hash/sha256-canonical-json rule)
                         "assessment_snapshot" (:id snapshot-enc)
                         "admission_report" (:id report-enc)}
            "works" (mapv :entry entries)
            "validation_summary" {"invalid_count" (count invalid)
                                  "invalid_slugs" invalid}}
     :blobs (into {(:hex snapshot-enc) (:bytes snapshot-enc)
                   (:hex report-enc) (:bytes report-enc)}
                  (map :blobs)
                  entries)
     :selection (set selection)}))

(defn release-assembler
  "Adapter to the transaction contract: a fn of the current head manifest
  value (nil at genesis) closing over everything else; the head's withdrawn
  set is subtracted from the admitted works, as the transaction requires."
  [opts]
  (fn [head-manifest]
    (assemble-release
     (assoc opts :withdrawn-slugs
            (set (map #(get % "slug") (get head-manifest "withdrawn")))))))
