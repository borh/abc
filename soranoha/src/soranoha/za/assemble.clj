(ns soranoha.za.assemble
  "Release assembly: the bridge from kernel outputs to the publication
  transaction's input. Kernel stages build requested works policy-blind;
  admission is decided here: the assessment snapshot commits facts or attributed reliance,
  the inclusion rule derives the total admitted/excluded/quarantined
  partition, and both evidence artifacts publish with the release. Works
  are the admitted slugs minus the chain's withdrawn set; every published
  artifact byte is read from the kura CAS; the validation summary is
  derived from the same per-work validation records the chain verifier
  re-derives it from. A failed validation is an artifact and a summary
  entry, never an exclusion."
  (:require [babashka.fs :as fs]
            [soranoha.core.hash :as hash]
            [soranoha.kura.cas :as cas]
            [soranoha.snh.decode :as decode]
            [soranoha.snh.admission :as admission]
            [soranoha.snh.verify :as verify]
            [soranoha.za.catalog :as catalog]
            [soranoha.za.naming :as naming]))

(defn- fail! [reason data]
  (throw (ex-info (str "release assembly failed: " (name reason))
                  (assoc data :reason reason))))

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

(defn- cas-blob ^bytes [cas-dir hex]
  (or (cas/get-bytes cas-dir hex)
      (fail! :artifact-missing-from-cas {:hex hex})))

(defn- bare-hex [source-content-hash]
  (or (hash/bare-sha256-hex source-content-hash)
      (fail! :malformed-source-content-hash {:value source-content-hash})))

(defn- work-entry
  "Manifest works[] entry + its blob map for one slug's kernel outputs.
  Artifact order is the schema's fixed
  [markdown, plaintext, tei, tei-validation]."
  [cas-dir slug outputs]
  (let [blobs (mapv (fn [kind]
                      (let [hex (or (get outputs (keyword kind))
                                    (fail! :work-output-missing
                                           {:slug slug :kind kind}))
                            path (fs/path (cas/blob-path cas-dir hex))]
                        (when-not (fs/exists? path)
                          (fail! :artifact-missing-from-cas {:hex hex}))
                        [kind hex path]))
                    naming/artifact-kinds)]
    {:entry {"slug" slug
             "source_content_hash" (bare-hex (:source-content-hash outputs))
             ;; the standing selection admitted this work under. Fail closed:
             ;; a work reaching assembly without one would otherwise be
             ;; published with no stated terms at all.
             "rights" (or (:rights outputs)
                          (fail! :work-rights-missing {:slug slug}))
             "artifacts" (mapv (fn [[kind hex path]]
                                 {"type" kind
                                  "id" (str "snh:1:" kind ":" hex)
                                  "bytes" (fs/size path)})
                               blobs)
             ;; the reserved annotation layer slot. No stage publishes one
             ;; yet, so this is empty for every work; it is here because the
             ;; schema is closed and adding it after genesis would be a wire
             ;; version every verifier of the day has to handle.
             "layers" []}
     :blobs (into {} (map (fn [[_ hex path]] [hex path])) blobs)}))

(defn- validation-failed?
  ;; the same consumed contract the chain verifier re-derives the summary
  ;; under; a noncontractual kernel record fails assembly here rather than
  ;; at pre-push verification
  [cas-dir hex]
  (= "failed"
     (:status (verify/consumed-validation-record (cas-blob cas-dir hex)))))

(defn toolchain-value
  "snh-manifest/3 toolchain object from engine-shaped stages: per stage id,
  the toolchain identity and stage code version exactly as its derivation
  keys carry them. nix_closure_hash holds that derivation toolchain
  identity (the wrapper-supplied Nix closure hash for nix-provisioned
  stages, the hashed binary/profile identity for subprocess stages); it is
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
  - :cas-dir: the kura CAS every artifact byte is read from;
  - :corpus / :toolchain / :selection-params / :policy-id / :policy-hash:
    manifest coordinates;
  - :candidates: snapshot candidate values covering the assessed
    population (the totality gate compares their slugs against
    :selection, the kernel's selected slug set, so an unassessed or
    unselected candidate blocks emission);
  - :rights: the grant published in the manifest, carried by the same
    rights policy whose hash the manifest already records;
  - :works: slug -> {:markdown :plaintext :tei :tei-validation
    :metadata-record :persons <cas hex>, :primary-text-member <path>,
    :source-content-hash \"sha256:<hex>\"} kernel outputs, covering at
    least every published candidate;
  - :source-hashes: assessed source-content hashes, independent of built artifacts;
  - :withdrawn-slugs: the chain head's withdrawn set; works = admitted
    minus withdrawn."
  [{:keys [cas-dir corpus toolchain selection-params policy-id policy-hash
           rights candidates works source-hashes withdrawn-slugs selection]}]
  (let [snapshot-enc (decode/encode "assessment-snapshot"
                                    (snapshot-value candidates))
        rule admission/inclusion-rule
        _ (doseq [{:strs [slug reliance]} candidates
                  :when (= "relied-upon" (get reliance "status"))]
            (when-not (and (= (get reliance "source_content_hash") (get source-hashes slug))
                           (or (not (contains? works slug))
                               (= (get reliance "source_content_hash") (:source-content-hash (get works slug)))))
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
                             published))
        catalog-enc (decode/encode
                     "catalog"
                     (catalog/catalog-value cas-dir published works))]
    {:core {"schema" "snh-manifest/3"
            "corpus" corpus
            "toolchain" toolchain
            "selection_params" selection-params
            "admission" {"policy_id" policy-id
                         "policy_hash" policy-hash
                         "inclusion_rule_id" (get rule "id")
                         "inclusion_rule_hash" (hash/sha256-canonical-json rule)
                         "assessment_snapshot" (:id snapshot-enc)
                         "admission_report" (:id report-enc)}
            "catalog" (:id catalog-enc)
            "rights" (or rights (fail! :missing-rights-grant {}))
            "works" (mapv :entry entries)
            "validation_summary" {"invalid_count" (count invalid)
                                  "invalid_slugs" invalid}}
     :blobs (into {(:hex snapshot-enc) (:bytes snapshot-enc)
                   (:hex report-enc) (:bytes report-enc)
                   (:hex catalog-enc) (:bytes catalog-enc)}
                  (map :blobs)
                  entries)
     :selection (set selection)}))

(defn covers-from
  "Where the range this release covers starts: the predecessor's
  `upstream_rev` when the corpus moved, the predecessor's own
  `covers_from` when it did not, and null at genesis.

  Carrying it forward unchanged is what makes a rerun at the same upstream
  revision a no-op rather than a new release. `corpus` is a projection key,
  so a `covers_from` that advanced on every attempt would make every
  projection differ from the head's and publish a release for a corpus
  nobody moved."
  [head-manifest corpus]
  (let [head-corpus (get head-manifest "corpus")]
    (cond
      (nil? head-manifest) nil
      (= (get head-corpus "upstream_rev") (get corpus "upstream_rev"))
      (get head-corpus "covers_from")
      :else (get head-corpus "upstream_rev"))))

(defn validation-regressions
  "Slugs this release would publish as invalid that its parent published as
  valid, with nothing to explain the change.

  A work is here only when its `source_content_hash` is the one the parent
  published and the `validate-tei` stage coordinate is the one the parent
  published. If the source moved, upstream reproofread it. If the stage
  coordinate moved, the TEI profile tightened and reclassifying a document
  is what that is for. With neither moved, the document is the same
  document judged by the same profile, and the only remaining explanation
  is that something between the source and the TEI got worse.

  Both facts are already on the chain and neither is taken on trust: every
  verifier re-derives `validation_summary.invalid_slugs` from the published
  validation records, and the work entry carries the source hash its
  artifacts were built from. So this compares two signed statements, and it
  needs no threshold. A work absent from either release, newly published or
  withdrawn, is not a regression and is not here."
  [head-manifest manifest]
  (let [source-of (fn [m]
                    (into {} (map (juxt #(get % "slug") #(get % "source_content_hash")))
                          (get m "works")))
        published-before (source-of head-manifest)
        published-now (source-of manifest)
        invalid-before (set (get-in head-manifest ["validation_summary" "invalid_slugs"]))
        same-profile? (= (get-in head-manifest ["toolchain" "validate-tei"])
                         (get-in manifest ["toolchain" "validate-tei"]))]
    (if-not same-profile?
      []
      (vec (for [slug (get-in manifest ["validation_summary" "invalid_slugs"])
                 :when (and (contains? published-before slug)
                            (not (contains? invalid-before slug))
                            (= (get published-before slug) (get published-now slug)))]
             slug)))))

(defn release-assembler
  "Adapter to the transaction contract: a fn of the current head manifest
  value (nil at genesis) closing over everything else; the head's withdrawn
  set is subtracted from the admitted works, as the transaction requires.

  A release that regresses validation does not get assembled. The chain is
  append-only, so a toolchain change that breaks works the last release
  handled cannot be unpublished, only withdrawn work by work through
  governance events. Refusing costs one release; publishing costs the
  history. This is the only place both manifests are in hand before
  anything is signed."
  [opts]
  (fn [head-manifest]
    (let [assembled (assemble-release
                     (-> opts
                         (assoc :withdrawn-slugs
                                (set (map #(get % "slug") (get head-manifest "withdrawn"))))
                         (assoc-in [:corpus "covers_from"]
                                   (covers-from head-manifest (:corpus opts)))))
          regressed (when head-manifest
                      (validation-regressions head-manifest (:core assembled)))]
      (when (seq regressed)
        (fail! :validation-regression
               {:count (count regressed)
                :slugs (vec (take 20 regressed))}))
      assembled)))
