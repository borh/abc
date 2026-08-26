(ns soranoha.za.release
  "The automated release driver: one scheduled invocation carries one
  kernel build run through release assembly and the publication
  transaction. Admission is a fail-closed input — the assessment
  snapshot arrives as protocol bytes and is boundary-decoded before
  anything else runs, so a malformed or non-canonical snapshot publishes
  nothing. The driver adds no transaction semantics of its own: outcomes
  are the transaction's — :published, :already-published (the scheduled
  no-op), :requeue (the next scheduled invocation retries against the
  new head), :determinism-halt."
  (:require [soranoha.snh.decode :as decode]
            [soranoha.snh.transact :as transact]
            [soranoha.za.assemble :as assemble]))

(defn- report-works
  "The assembler's works map projected from a build run report."
  [report]
  (into {}
        (map (fn [[slug work]]
               [slug {:plaintext (get work "plaintext")
                      :tei (get work "tei")
                      :tei-validation (get work "tei-validation")
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
  "Assemble and publish one release from a build run report.
  - :report — the kernel build's run report (in-process map): supplies
    the upstream revision, the stage-coordinate table, the selected
    slug set, and every work's artifact and source-content hashes;
  - :cas-dir — the kura CAS the report's artifact hashes resolve in;
  - :snapshot-bytes — the assessment snapshot as snh protocol bytes
    (versioned admission data), strictly boundary-decoded here;
  - :upstream-origin / :selection-params / :policy-id / :policy-hash —
    manifest coordinates;
  - :clone / :branch / :pinned-keys / :sign-release / :push-fn — the
    publication transaction's origin clone, protected branch, pinned
    verifier keys, and release signer.
  Returns the transaction outcome map."
  [{:keys [report cas-dir upstream-origin selection-params
           policy-id policy-hash snapshot-bytes
           clone branch pinned-keys sign-release push-fn]}]
  (let [candidates (get (:value (decode/decode "assessment-snapshot"
                                               snapshot-bytes))
                        "candidates")]
    (transact/publish-build!
     (cond-> {:clone clone
              :branch branch
              :pinned-keys pinned-keys
              :sign-release sign-release
              :assemble (assemble/release-assembler
                         {:cas-dir cas-dir
                          :corpus {"upstream_origin" upstream-origin
                                   "upstream_rev" (get report
                                                       "aozora_git_commit")}
                          :toolchain (report-toolchain report)
                          :selection-params selection-params
                          :policy-id policy-id
                          :policy-hash policy-hash
                          :candidates candidates
                          :works (report-works report)
                          :selection (keys (get report "works"))})}
       push-fn (assoc :push-fn push-fn)))))
