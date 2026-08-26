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
  (:require [clojure.edn :as edn]
            [soranoha.core.hash :as hash]
            [soranoha.snh.decode :as decode]
            [soranoha.snh.transact :as transact]
            [soranoha.za.assemble :as assemble])
  (:import (java.nio ByteBuffer)
           (java.nio.charset CodingErrorAction StandardCharsets)))

;; --- rights authority -------------------------------------------------------

;; The one rights-publication state that authorizes release publication.
;; Every other, missing, or malformed state is fail-closed. Moving this
;; value is a deliberate governance change, not an implementation detail.
(def ^:private authorizing-rights-state :assessment-required)

(defn- strict-utf8
  "Decode bytes as UTF-8, failing (rather than substituting) on malformed
  or unmappable byte sequences."
  [^bytes bytes]
  (let [decoder (doto (.newDecoder StandardCharsets/UTF_8)
                  (.onMalformedInput CodingErrorAction/REPORT)
                  (.onUnmappableCharacter CodingErrorAction/REPORT))]
    (str (.decode decoder (ByteBuffer/wrap bytes)))))

(defn- read-one-edn
  "Read exactly one EDN value spanning the whole of `text`: an empty
  document, a second form, or trailing garbage after the value all fail —
  a reader that stops at the first value would hash bytes it never
  evaluated."
  [^String text]
  (with-open [reader (java.io.PushbackReader. (java.io.StringReader. text))]
    (let [eof (Object.)
          value (edn/read {:eof eof} reader)]
      (when (identical? value eof)
        (throw (ex-info "empty policy document" {})))
      (when-not (identical? eof (try (edn/read {:eof eof} reader)
                                     (catch Exception _ nil)))
        (throw (ex-info "trailing input after the policy value" {})))
      value)))

(defn rights-authority!
  "Fail-closed value-plus-hash rights authority over the policy bytes:
  strict UTF-8 + EDN decode, then only the authorizing rights-publication
  state releases — the value evaluated and the manifest policy hash
  derive from the same byte array, so the recorded hash can never
  disagree with what was evaluated. The policy id is fixed here, by the
  authority, never supplied by the caller. Returns
  {:policy-id :policy-hash}; throws on anything else."
  [^bytes policy-bytes]
  (let [value (try
                (read-one-edn (strict-utf8 policy-bytes))
                (catch Exception e
                  (throw (ex-info "rights policy unreadable"
                                  {:reason :policy-unreadable
                                   :cause (ex-message e)}))))
        state (when (map? value) (:rights-publication value))]
    (when-not (= authorizing-rights-state state)
      (throw (ex-info (str "release publication blocked by rights policy: "
                           (pr-str (or state :missing-rights-publication-policy)))
                      {:reason :rights-blocked
                       :state (or state :missing-rights-publication-policy)})))
    {:policy-id "rights-publication-policy-v1"
     :policy-hash (hash/sha256-bytes policy-bytes)}))

;; --- report projection ------------------------------------------------------

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
                        "candidates")
        ;; the selected population is captured from the selection join
        ;; before work execution; requiring the built works to equal it
        ;; keeps the assembler's totality comparison independent — a work
        ;; co-omitted from both the works and the snapshot cannot pass
        selected (get report "selected_slugs")
        works (set (keys (get report "works")))]
    (when-not (and (seq selected) (= (set selected) works))
      (throw (ex-info "report works do not cover the selected population"
                      {:reason :selection-works-mismatch
                       :selected-only (vec (sort (remove works selected)))
                       :works-only (vec (sort (remove (set selected) works)))})))
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
                          :selection selected})}
       push-fn (assoc :push-fn push-fn)))))
