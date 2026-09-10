(ns soranoha.za.release
  "The automated release driver requests kernel artifacts during release
  assembly and the publication transaction. Admission is a fail-closed input: the assessment
  snapshot arrives as protocol bytes and is boundary-decoded before
  anything else runs, so a malformed or non-canonical snapshot publishes
  nothing. The driver adds no transaction semantics of its own: outcomes
  are the transaction's (:published, :already-published for scheduled
  no-ops, :requeue to retry against a new head, and :determinism-halt)."
  (:require [soranoha.core.hash :as hash]
            [soranoha.core.rights :as rights]
            [soranoha.snh.decode :as decode]
            [soranoha.snh.admission :as admission]
            [soranoha.snh.transact :as transact]
            [soranoha.za.assemble :as assemble]))

;; The one rights-publication state that authorizes release publication.
;; Every other, missing, or malformed state is fail-closed.
(def ^:private authorizing-rights-state :assessment-required)

(defn rights-authority!
  "Fail-closed value-plus-hash rights authority over the policy bytes:
  strict UTF-8 + EDN decode, then only the authorizing rights-publication
  state releases; the value evaluated and the manifest policy hash
  derive from the same byte array, so the recorded hash can never
  disagree with what was evaluated. The grant travels back to the caller so
  the build embeds the same terms in each work's TEI that the manifest
  publishes, from this one read. The policy id is fixed here, by the
  authority, never supplied by the caller. Returns
  {:policy-id :policy-hash :rights}; throws on anything else."
  [^bytes policy-bytes]
  (let [value (rights/read-policy policy-bytes)
        state (when (map? value) (:rights-publication value))]
    (when-not (= authorizing-rights-state state)
      (throw (ex-info (str "release publication blocked by rights policy: "
                           (pr-str (or state :missing-rights-publication-policy)))
                      {:reason :rights-blocked
                       :state (or state :missing-rights-publication-policy)})))
    {:policy-id "rights-publication-policy-v1"
     :policy-hash (hash/sha256-bytes policy-bytes)
     :rights (rights/grant value)}))

(defn- report-works
  "The assembler's works map projected from a build run report. Published
  artifact hexes, plus the catalog's inputs: the metadata and person records
  stay CAS references rather than values, so the assembler reads the same
  bytes the release publishes from."
  [report]
  (into {}
        (map (fn [[slug work]]
               [slug {:markdown (get work "markdown")
                      :plaintext (get work "plaintext")
                      :tei (get work "tei")
                      :tei-validation (get work "tei-validation")
                      :metadata-record (get work "metadata-record")
                      :persons (get work "persons")
                      :primary-text-member (get work "primary_text_member")
                      :source-content-hash (get work "source_content_hash")}]))
        (get report "works")))

(defn- report-toolchain
  "The manifest toolchain object from the report's stage-coordinate table."
  [report]
  (assemble/toolchain-value
   (map (fn [[_ coordinate]]
          {:stage-id (get coordinate "stage_id")
           :stage-version (get coordinate "stage_version")
           :toolchain-id (get coordinate "toolchain_id")})
        (get report "stages"))))

(defn release!
  "Account for the full selection, then request artifacts for each verified head.
  :selection is captured independently of assessment and execution.
  :build-works! accepts the published slugs and returns their kernel run report;
  it must recheck source and assessment inputs before returning on every attempt.
  :source-hashes binds assessed source facts even when no artifact is requested.
  :verified-head is passed through to the transaction: a proof of the current
  head that THIS process computed, letting a caller that publishes a run of
  releases avoid re-verifying the whole chain once per release."
  [{:keys [selection build-works! source-hashes cas-dir upstream-origin selection-params
           policy-id policy-hash rights snapshot-bytes
           clone branch pinned-keys sign-release push-fn verified-head]}]
  (let [snapshot (:value (decode/decode "assessment-snapshot" snapshot-bytes))
        candidates (get snapshot "candidates")
        selected (set selection)
        candidate-slugs (set (map #(get % "slug") candidates))
        admitted (:admitted (admission/partition-candidates admission/inclusion-rule candidates))]
    (when-not (and (seq selected) (= selected candidate-slugs))
      (throw (ex-info "snapshot does not cover the selected population"
                      {:reason :totality-violation
                       :only-in-selection (vec (sort (remove candidate-slugs selected)))
                       :only-in-snapshot (vec (sort (remove selected candidate-slugs)))})))
    (transact/publish-build!
     (cond-> {:clone clone :branch branch :pinned-keys pinned-keys :sign-release sign-release
              :assemble
              (fn [head]
                (let [withdrawn (set (map #(get % "slug") (get head "withdrawn")))
                      demanded (vec (sort (remove withdrawn admitted)))
                      report (build-works! demanded)
                      built (set (keys (get report "works")))]
                  (when-not (= (set demanded) built (set (get report "selected_slugs")))
                    (throw (ex-info "build report does not cover the requested artifacts"
                                    {:reason :selection-works-mismatch
                                     :selected-only (vec (sort (remove built demanded)))
                                     :works-only (vec (sort (remove (set demanded) built)))})))
                  ((assemble/release-assembler
                    {:cas-dir cas-dir
                     :corpus {"upstream_origin" upstream-origin
                              "upstream_rev" (get report "aozora_git_commit")}
                     :toolchain (report-toolchain report)
                     :selection-params selection-params
                     :policy-id policy-id :policy-hash policy-hash
                     :rights rights
                     :candidates candidates :works (report-works report)
                     :source-hashes source-hashes :selection selected}) head)))}
       push-fn (assoc :push-fn push-fn)
       verified-head (assoc :verified-head verified-head)))))
