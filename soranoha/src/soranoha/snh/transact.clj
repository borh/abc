(ns soranoha.snh.transact
  "The publication transaction. Authority is the protected publication branch
  of the origin; the fast-forward-only push is the compare-and-swap. On
  rejection (or an unknown push result whose manifest is absent from the
  accepted chain) the loser consults only the current accepted head: a build
  either requeues (projection changed), converges (desired state already
  published, whoever published it), or halts as a determinism defect (same
  projection, different derived content); a governance event is appended
  after revalidation against the current head, converges if some chain
  manifest already executes it, and halts for fresh offline authorization
  when it no longer validates. Assembled manifests are discarded on
  rejection, never rebased; events are never rewritten or re-signed."
  (:require [soranoha.core.hash :as hash]
            [soranoha.snh.decode :as decode]
            [soranoha.snh.repo :as repo]
            [soranoha.snh.sign :as sign]
            [soranoha.snh.verify :as verify]
            [soranoha.snh.view :as view]))

(defn- fail! [reason data]
  (throw (ex-info (str "transaction failed: " (name reason))
                  (assoc data :reason reason))))

(defn init-publication-branch!
  "Create and push the pre-genesis initial commit: releases/HEAD = 64 zeros,
  no parent. Returns the commit sha; the push expects the branch to be
  absent."
  [clone branch]
  (let [commit (repo/write-commit!
                clone {:parents []
                       :files {verify/head-path
                               (sign/hex64-lf-bytes sign/zero-head-hex)}
                       :message "snh pre-genesis"})]
    (when-not (= :ok (repo/push! clone branch commit nil))
      (fail! :init-rejected {:branch branch}))
    commit))

(defn- release-files
  "Repo paths for one release: every referenced blob at its sharded path,
  the manifest json + detached signature, and the advanced head."
  [{:keys [manifest-bytes manifest-hex sig blobs]}]
  (into {(verify/manifest-path manifest-hex) manifest-bytes
         (verify/manifest-sig-path manifest-hex) sig
         verify/head-path (sign/hex64-lf-bytes manifest-hex)}
        (map (fn [[hex bytes]] [(verify/blob-path hex) bytes]))
        blobs))

(defn- head-manifest-at
  "Decoded head manifest at commit `c`, or nil when the head is zero."
  [v c]
  (let [head (sign/parse-hex64-lf
              (or (view/read-at v c verify/head-path)
                  (fail! :missing-head {:commit c})))]
    (when-not (= sign/zero-head-hex head)
      {:hex head
       :value (:value (decode/decode
                       "release-manifest"
                       (or (view/read-at v c (verify/manifest-path head))
                           (fail! :missing-head-manifest {:commit c :head head}))))})))

(defn- check-totality!
  "The assembler-side totality check: the snapshot's candidate set must equal
  the selection the assembler derived before emitting."
  [blobs manifest selection]
  (let [snapshot-hex (verify/id->hex (get-in manifest ["admission" "assessment_snapshot"]))
        snapshot-bytes (or (get blobs snapshot-hex)
                           (fail! :snapshot-blob-missing {:hex snapshot-hex}))
        snapshot (:value (decode/decode "assessment-snapshot" snapshot-bytes))
        candidates (set (map #(get % "slug") (get snapshot "candidates")))]
    (when-not (= candidates (set selection))
      (fail! :totality-violation
             {:only-in-snapshot (vec (sort (remove (set selection) candidates)))
              :only-in-selection (vec (sort (remove candidates selection)))}))))

(defn- build-manifest
  "Complete an assembled core into the manifest value for the given head:
  the head's withdrawn set is inherited by construction; an ordinary build
  carries no governance event."
  [core head-manifest head-hex]
  (assoc core
         "prev_manifest" head-hex
         "withdrawn" (if head-manifest (get (:value head-manifest) "withdrawn") [])
         "governance_event" nil))

(defn- derived-content [manifest]
  (select-keys manifest ["works" "validation_summary" "withdrawn"]))

(defn publish-build!
  "Run one build publication against the origin. `assemble` is called with
  the current head manifest value (nil at genesis) and returns
  {:core <manifest without prev/withdrawn/governance_event>
   :blobs {sha256-hex -> bytes} :selection #{slug ...}}.
  `sign-release` maps a manifest hex to its 64-byte signature. `push-fn`
  (default the real compare-and-swap push) may return :ok, :rejected, or
  :unknown. Returns {:outcome :published | :already-published | :requeue
  | :determinism-halt, ...}."
  [{:keys [clone branch pinned-keys assemble sign-release push-fn]
    :or {push-fn repo/push!}}]
  (let [v (view/git-view clone)
        c (or (repo/fetch! clone branch) (fail! :no-publication-branch {:branch branch}))
        head (head-manifest-at v c)
        {:keys [core blobs selection]} (assemble (some-> head :value))
        manifest (build-manifest core head (or (:hex head) sign/zero-head-hex))
        _ (check-totality! blobs manifest selection)
        {:keys [hex bytes]} (decode/encode "release-manifest" manifest)
        commit (repo/write-commit!
                clone {:parents [c]
                       :base-tree-of c
                       :files (release-files {:manifest-bytes bytes
                                              :manifest-hex hex
                                              :sig (sign-release hex)
                                              :blobs blobs})
                       :message (str "snh release " hex)})
        outcome (push-fn clone branch commit c)
        reconcile
        (fn []
          ;; current-state reconciliation: discard the assembled manifest,
          ;; fully verify the accepted head, recompute from it alone
          (let [c2 (repo/fetch! clone branch)
                _ (verify/verify-repository-at v c2 pinned-keys)
                head2 (head-manifest-at v c2)
                desired2 (assemble (some-> head2 :value))
                manifest2 (build-manifest (:core desired2) head2
                                          (or (:hex head2) sign/zero-head-hex))]
            (check-totality! (:blobs desired2) manifest2 (:selection desired2))
            (cond
              (not= (verify/projection manifest2)
                    (verify/projection (:value head2)))
              {:outcome :requeue :head (:hex head2)}

              (= (derived-content manifest2) (derived-content (:value head2)))
              {:outcome :already-published :manifest-id (:hex head2)}

              :else
              {:outcome :determinism-halt :head (:hex head2)})))]
    (case outcome
      :ok {:outcome :published :manifest-id hex :commit commit}
      :unknown (let [c2 (repo/fetch! clone branch)
                     result (verify/verify-repository-at v c2 pinned-keys)]
                 (if (some #{hex} (:chain result))
                   {:outcome :published :manifest-id hex}
                   (reconcile)))
      :rejected (reconcile))))

(defn- successor-for-event
  "Construct the successor manifest executing `event` on `head-manifest`, or
  a halt outcome when the event no longer validates against that head."
  [head-manifest head-hex event-id {:strs [kind entries]}]
  (let [wd-map (into {} (map (fn [{:strs [slug event]}] [slug event]))
                     (get head-manifest "withdrawn"))
        entry-slugs (mapv #(get % "slug") entries)]
    (case kind
      "withdrawal"
      (let [work-slugs (set (map #(get % "slug") (get head-manifest "works")))]
        (cond
          (some wd-map entry-slugs)
          {:halt :conflicting-withdrawal
           :slugs (vec (filter wd-map entry-slugs))}

          (not-every? work-slugs entry-slugs)
          {:halt :slug-not-in-works
           :slugs (vec (remove work-slugs entry-slugs))}

          :else
          (let [gone (set entry-slugs)
                summary (get head-manifest "validation_summary")
                invalid (vec (remove gone (get summary "invalid_slugs")))]
            {:manifest
             (assoc head-manifest
                    "prev_manifest" head-hex
                    "governance_event" event-id
                    "works" (vec (remove #(gone (get % "slug"))
                                         (get head-manifest "works")))
                    "withdrawn" (vec (sort-by #(get % "slug")
                                              (concat (get head-manifest "withdrawn")
                                                      (map (fn [slug]
                                                             {"slug" slug
                                                              "event" event-id})
                                                           entry-slugs))))
                    "validation_summary" {"invalid_count" (count invalid)
                                          "invalid_slugs" invalid})})))

      "event-amendment"
      (let [stale (remove (fn [{:strs [slug amends]}] (= amends (get wd-map slug)))
                          entries)]
        (if (seq stale)
          {:halt :stale-amends :slugs (mapv #(get % "slug") stale)}
          {:manifest
           (assoc head-manifest
                  "prev_manifest" head-hex
                  "governance_event" event-id
                  "withdrawn" (mapv (fn [{:strs [slug] :as entry}]
                                      (if (some #{slug} entry-slugs)
                                        {"slug" slug "event" event-id}
                                        entry))
                                    (get head-manifest "withdrawn")))})))))

(defn publish-governance!
  "Append the already-signed governance event to the chain. The event is
  validated against the current head on every attempt and never rewritten or
  re-signed. Returns {:outcome :published | :already-applied | :halt, ...}."
  [{:keys [clone branch pinned-keys sign-release event-bytes event-sig push-fn
           max-attempts]
    :or {push-fn repo/push! max-attempts 5}}]
  (let [v (view/git-view clone)
        {event :value event-hex :hex event-id :id}
        (decode/decode "governance-event" event-bytes)]
    (when-not (sign/verify-artifact-signature? pinned-keys "governance-event"
                                               event-hex event-sig)
      (fail! :event-signature-invalid {:event event-id}))
    (loop [attempt 1]
      (when (> attempt max-attempts)
        (fail! :attempts-exhausted {:event event-id}))
      (let [c (or (repo/fetch! clone branch)
                  (fail! :no-publication-branch {:branch branch}))
            chain (verify/verify-repository-at v c pinned-keys)]
        (cond
          (:empty chain)
          {:outcome :halt :reason :no-published-release}

          (some #(= event-id (get (get (:manifests chain) %) "governance_event"))
                (:chain chain))
          {:outcome :already-applied :event event-id}

          :else
          (let [head-hex (:head chain)
                head-manifest (get (:manifests chain) head-hex)
                {:keys [halt manifest] :as attempt-result}
                (successor-for-event head-manifest head-hex event-id event)]
            (if halt
              {:outcome :halt :reason halt
               :slugs (:slugs attempt-result)}
              (let [{:keys [hex bytes]} (decode/encode "release-manifest" manifest)
                    files (merge (release-files {:manifest-bytes bytes
                                                 :manifest-hex hex
                                                 :sig (sign-release hex)
                                                 :blobs {event-hex event-bytes}})
                                 {(verify/event-path event-hex) event-bytes
                                  (verify/event-sig-path event-hex) event-sig})
                    commit (repo/write-commit!
                            clone {:parents [c]
                                   :base-tree-of c
                                   :files files
                                   :message (str "snh governance " event-hex)})
                    outcome (push-fn clone branch commit c)]
                (case outcome
                  :ok {:outcome :published :manifest-id hex :event event-id}
                  :unknown (let [c2 (repo/fetch! clone branch)
                                 r (verify/verify-repository-at v c2 pinned-keys)]
                             (if (some #(= event-id
                                           (get (get (:manifests r) %)
                                                "governance_event"))
                                       (:chain r))
                               {:outcome :published :event event-id}
                               (recur (inc attempt))))
                  :rejected (recur (inc attempt)))))))))))
