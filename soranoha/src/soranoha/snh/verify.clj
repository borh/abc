(ns soranoha.snh.verify
  "The verifier primitive: verify-repository-at(view, commit, pinned-keys).
  Every read goes through the single commit-scoped view; each artifact must
  be readable at its prescribed path from the target commit's tree. Checks,
  per publication commit from the head back to genesis: manifest boundary
  decode (structure + single-object semantics) and id agreement with
  releases/HEAD, release/governance signatures, per-work and evidence blob
  presence with hash and length agreement, validation-summary re-derivation
  from the consumed projection of each tei-validation record, admission
  partition and field bindings, event closure, and the chain transition
  rules (single parent, head linkage, monotonic withdrawals,
  withdrawal/amendment shapes, genesis form).

  Verification streams: each manifest is decoded exactly once and only the
  head manifest is retained: the result carries the ordered manifest ids,
  the executed governance-event ids, and the chain length, which is
  everything the transaction consumes. Within one pass a work artifact's
  content is verified once: an older commit reuses the younger commit's
  verification only when the tree comparison proves its entry identical
  at the same path, so every commit still proves each artifact's path
  reachability in its own tree.

  archive-verification wraps the primitive into a total report over a
  readable view: acquisition failures throw; a readable view always yields
  {:result :success | :failed}."
  (:require [soranoha.snh.admission :as admission-rule]
            [clojure.string :as str]
            [soranoha.core.hash :as hash]
            [soranoha.snh.decode :as decode]
            [soranoha.snh.sign :as sign]
            [soranoha.snh.view :as view])
  (:import (java.util Arrays)
           (tools.jackson.core StreamReadFeature)
           (tools.jackson.databind.json JsonMapper)))

(def head-path "releases/HEAD")

(defn manifest-path [hex] (str "releases/" hex ".json"))
(defn manifest-sig-path [hex] (str "releases/" hex ".sig"))
(defn event-path [hex] (str "governance/" hex ".json"))
(defn event-sig-path [hex] (str "governance/" hex ".sig"))
(defn blob-path [hex] (str "blobs/sha256/" (subs hex 0 2) "/" hex))

(defn id->hex [artifact-id]
  (last (str/split artifact-id #":")))

(defn- fail! [reason data]
  (throw (ex-info (str "verification failed: " (name reason))
                  (assoc data :reason reason))))

(defn- read-required ^bytes [v commit path reason]
  (or (view/read-at v commit path)
      (fail! reason {:commit commit :path path})))

(defn- head-at [v commit]
  (let [bytes (read-required v commit head-path :missing-head)]
    (try (sign/parse-hex64-lf bytes)
         (catch Exception e
           (fail! :malformed-head {:commit commit :cause (ex-message e)})))))

(defn- decoded-manifest
  "Read and boundary-decode the manifest stored at its prescribed
  releases/<hex>.json path; the stored bytes must hash to `hex` (the value
  releases/HEAD or a prev_manifest link names)."
  [v commit hex]
  (let [bytes (read-required v commit (manifest-path hex) :missing-manifest)
        decoded (decode/decode "release-manifest" bytes)]
    (when-not (= hex (:hex decoded))
      (fail! :manifest-id-mismatch {:commit commit :expected hex :got (:hex decoded)}))
    (:value decoded)))

(defn- decoded-artifact
  "Read, hash-check, and boundary-decode a protocol artifact referenced by
  a typed id; the stored blob's hash must equal the id's hash component."
  [v commit artifact-id expected-type]
  (let [hex (id->hex artifact-id)
        bytes (read-required v commit (blob-path hex) :missing-blob)]
    (when-not (= hex (hash/sha256-bytes bytes))
      (fail! :blob-hash-mismatch {:commit commit :id artifact-id}))
    (let [{:keys [id] :as decoded} (decode/decode expected-type bytes)]
      (when-not (= id artifact-id)
        (fail! :artifact-id-mismatch {:commit commit :expected artifact-id :got id}))
      decoded)))

(defn- check-signature! [v commit pinned-keys type subject-hex sig-path-str]
  (let [sig (read-required v commit sig-path-str :missing-signature)]
    (when-not (= 64 (alength sig))
      (fail! :malformed-signature {:commit commit :path sig-path-str
                                   :length (alength sig)}))
    (when-not (sign/verify-artifact-signature? pinned-keys type subject-hex sig)
      (fail! :signature-invalid {:commit commit :type type :subject subject-hex}))))

(defn- check-event!
  "Fetch and check one governance event's closure at `commit`: authoritative
  CAS blob decodes to the id, the convenience copy is byte-equal, and the
  detached signature verifies under the governance role. Returns the decoded
  event value."
  [v commit pinned-keys event-id]
  (let [hex (id->hex event-id)
        {:keys [value]} (decoded-artifact v commit event-id "governance-event")
        cas-bytes (view/read-at v commit (blob-path hex))
        copy (read-required v commit (event-path hex) :missing-event-copy)]
    (when-not (Arrays/equals ^bytes cas-bytes ^bytes copy)
      (fail! :event-copy-diverges {:commit commit :id event-id}))
    (check-signature! v commit pinned-keys "governance-event" hex (event-sig-path hex))
    value))

(defn- work-slugs [manifest] (mapv #(get % "slug") (get manifest "works")))
(defn- withdrawn-slugs [manifest] (mapv #(get % "slug") (get manifest "withdrawn")))
(defn- withdrawn-map [manifest]
  (into {} (map (fn [{:strs [slug event]}] [slug event])) (get manifest "withdrawn")))

(def projection-keys ["corpus" "toolchain" "selection_params" "admission" "rights"])

(defn projection [manifest] (select-keys manifest projection-keys))

(def ^:private ^JsonMapper strict-record-mapper
  (let [builder (JsonMapper/builder)]
    (.enable builder ^"[Ltools.jackson.core.StreamReadFeature;"
             (into-array StreamReadFeature
                         [StreamReadFeature/STRICT_DUPLICATE_DETECTION]))
    (.build builder)))

(def ^:private validation-statuses #{"passed" "warning" "failed"})

(defn consumed-validation-record
  "The one consumed contract of a tei-validation record, shared by the
  assembler (summary derivation) and the verifier (summary re-derivation):
  strict JSON with duplicate keys rejected at parse, status exactly
  passed | warning | failed, and a string validated_artifact. Only these
  two fields are consumed; the bytes themselves remain exact published
  bytes checked by hash. Returns {:status :validated-artifact}; throws
  ex-info with :reason :validation-record-unreadable or
  :validation-status-unknown."
  [^bytes blob]
  (let [^tools.jackson.databind.JsonNode node (try (.readTree strict-record-mapper blob)
                                                   (catch Exception e
                                                     (throw (ex-info "validation record unreadable"
                                                                     {:reason :validation-record-unreadable
                                                                      :cause (ex-message e)}))))
        field (fn [^String name] (let [f (.get node name)]
                                   (when (and (some? f) (.isTextual f)) (.textValue f))))
        status (field "status")
        validated (field "validated_artifact")]
    (when-not (and (.isObject node) status validated)
      (throw (ex-info "validation record unreadable"
                      {:reason :validation-record-unreadable})))
    (when-not (contains? validation-statuses status)
      (throw (ex-info "validation status unknown"
                      {:reason :validation-status-unknown :status status})))
    {:status status :validated-artifact validated}))

(defn- validation-record
  [commit slug ^bytes blob]
  (try (consumed-validation-record blob)
       (catch clojure.lang.ExceptionInfo e
         (fail! (:reason (ex-data e))
                (merge {:commit commit :slug slug}
                       (dissoc (ex-data e) :reason))))))

(defn- checked-artifact!
  "Verify one work artifact at `commit`; returns {:hex :record} (`:record`
  only for tei-validation bytes). `reuse?` grants that the same path
  carried the same verified hex at the already-verified younger commit
  and the tree comparison proved this commit's entry identical, so the
  bytes here ARE the bytes verified there; `facts` is the pass-level
  content cache {hex {:length :record}} those grants draw on. The
  declared length is checked against the actual bytes on either route."
  [v commit slug {:strs [type id bytes]} {:keys [reuse? facts]}]
  (let [hex (id->hex id)
        path (blob-path hex)
        known (get @facts hex)
        check-length! (fn [actual]
                        (when-not (= bytes actual)
                          (fail! :blob-length-mismatch
                                 {:commit commit :slug slug :id id
                                  :declared bytes :actual actual})))]
    (if (and known (reuse? path hex)
             (or (not= type "tei-validation") (:record known)))
      (do (check-length! (:length known))
          {:hex hex :record (:record known)})
      (let [blob (read-required v commit path :missing-blob)]
        (check-length! (alength blob))
        (when-not (= hex (hash/sha256-bytes blob))
          (fail! :blob-hash-mismatch {:commit commit :slug slug :id id}))
        (let [record (when (= type "tei-validation")
                       (validation-record commit slug blob))]
          (swap! facts assoc hex {:length (alength blob) :record record})
          {:hex hex :record record})))))

(defn- check-works-blobs!
  "Blob presence/hash/length for every per-work artifact, plus the
  validation-summary re-derivation: each tei-validation record must name
  that work's TEI bytes, and invalid_slugs must equal exactly the sorted
  slugs whose status is failed. Returns {path hex} for every verified
  artifact (the younger-commit evidence `reuse?` grants draw on when the
  predecessor is verified next)."
  [v commit manifest reuse]
  (let [failed
        (vec
         (for [{:strs [slug artifacts]} (get manifest "works")
               :let [by-type (into {}
                                   (map (fn [{:strs [type] :as a}]
                                          [type (checked-artifact!
                                                 v commit slug a reuse)]))
                                   artifacts)
                     record (:record (get by-type "tei-validation"))
                     tei-hex (:hex (get by-type "tei"))]
               :when (do (when-not (= (:validated-artifact record)
                                      (str "sha256:" tei-hex))
                           (fail! :validation-artifact-mismatch
                                  {:commit commit :slug slug
                                   :validated (:validated-artifact record)
                                   :tei tei-hex}))
                         (= "failed" (:status record)))]
           slug))]
    (when-not (= (get-in manifest ["validation_summary" "invalid_slugs"]) failed)
      (fail! :validation-summary-mismatch
             {:commit commit
              :declared (get-in manifest ["validation_summary" "invalid_slugs"])
              :derived failed}))
    (into {}
          (for [{:strs [artifacts]} (get manifest "works")
                {:strs [id]} artifacts
                :let [hex (id->hex id)]]
            [(blob-path hex) hex]))))

(defn- check-admission! [v commit manifest]
  (let [admission (get manifest "admission")
        snapshot (:value (decoded-artifact v commit (get admission "assessment_snapshot")
                                           "assessment-snapshot"))
        report (:value (decoded-artifact v commit (get admission "admission_report")
                                         "admission-report"))]
    (when-not (= (get admission "assessment_snapshot") (get report "assessment_snapshot"))
      (fail! :report-snapshot-mismatch {:commit commit}))
    (doseq [field ["policy_hash" "inclusion_rule_id" "inclusion_rule_hash"]]
      (when-not (= (get admission field) (get report field))
        (fail! :report-admission-field-mismatch {:commit commit :field field})))
    (let [rule admission-rule/inclusion-rule
          partition (admission-rule/partition-candidates rule (get snapshot "candidates"))
          works (into {} (map (juxt #(get % "slug") identity)) (get manifest "works"))]
      (when-not (and (= (get rule "id") (get report "inclusion_rule_id"))
                     (= (hash/sha256-canonical-json rule) (get report "inclusion_rule_hash"))
                     (every? (fn [[k v]] (= v (get report (name k)))) partition))
        (fail! :reliance-admission-mismatch {:commit commit}))
      (doseq [{:strs [slug reliance]} (get snapshot "candidates")
              :when (and reliance (get works slug))]
        (when-not (= (get reliance "source_content_hash")
                     (str "sha256:" (get-in works [slug "source_content_hash"])))
          (fail! :reliance-source-content-mismatch {:commit commit :slug slug}))))
    (let [candidates (mapv #(get % "slug") (get snapshot "candidates"))
          admitted (get report "admitted")
          excluded (mapv #(get % "slug") (get report "excluded"))
          quarantined (mapv #(get % "slug") (get report "quarantined"))
          partition (concat admitted excluded quarantined)]
      (when-not (= (set candidates) (set partition))
        (fail! :admission-partition-invalid {:commit commit}))
      (when-not (= (work-slugs manifest)
                   (vec (sort (remove (set (withdrawn-slugs manifest)) admitted))))
        (fail! :works-not-admitted-minus-withdrawn {:commit commit})))))

(defn- check-catalog!
  "The catalog must describe THIS release: the same works, in the same order,
  bound to the same source bytes. Without this a manifest could name any
  catalog blob, including one that still describes a withdrawn work (which
  is exactly what a takedown must remove)."
  [v commit manifest]
  (let [catalog (:value (decoded-artifact v commit (get manifest "catalog")
                                          "catalog"))
        entries (get catalog "works")
        works (get manifest "works")]
    (when-not (= (mapv #(get % "slug") entries) (work-slugs manifest))
      (fail! :catalog-works-mismatch
             {:commit commit
              :catalog (mapv #(get % "slug") entries)
              :works (work-slugs manifest)}))
    (doseq [[entry work] (map vector entries works)]
      (when-not (= (get entry "source_content_hash")
                   (get work "source_content_hash"))
        (fail! :catalog-source-content-mismatch
               {:commit commit :slug (get work "slug")})))))

(defn- check-events! [v commit pinned-keys manifest]
  (let [events (into {}
                     (map (fn [id] [id (check-event! v commit pinned-keys id)]))
                     (cond-> (set (map #(get % "event") (get manifest "withdrawn")))
                       (get manifest "governance_event")
                       (conj (get manifest "governance_event"))))]
    (doseq [{:strs [slug event]} (get manifest "withdrawn")]
      (when-not (some #(= slug (get % "slug")) (get-in events [event "entries"]))
        (fail! :withdrawn-slug-not-in-event {:commit commit :slug slug :event event})))
    events))

(defn- check-transition!
  "Chain rules between a manifest and its predecessor. `events` is the
  younger manifest's decoded event map (id -> value)."
  [commit manifest predecessor events]
  (let [wd (withdrawn-map manifest)
        pwd (withdrawn-map predecessor)
        gov (get manifest "governance_event")]
    (when-not (every? (set (keys wd)) (keys pwd))
      (fail! :withdrawn-not-monotonic {:commit commit}))
    (if (nil? gov)
      (when-not (= (get manifest "withdrawn") (get predecessor "withdrawn"))
        (fail! :withdrawn-changed-without-event {:commit commit}))
      (let [event (get events gov)
            entry-slugs (set (map #(get % "slug") (get event "entries")))]
        (when-not (= (projection manifest) (projection predecessor))
          (fail! :governance-changed-coordinates {:commit commit}))
        (case (get event "kind")
          "withdrawal"
          (let [added (set (remove (set (keys pwd)) (keys wd)))]
            (when-not (= added entry-slugs)
              (fail! :withdrawal-slugs-mismatch {:commit commit}))
            (doseq [slug added]
              (when-not (= gov (get wd slug))
                (fail! :withdrawn-entry-wrong-event {:commit commit :slug slug})))
            (doseq [[slug event-id] pwd]
              (when-not (= event-id (get wd slug))
                (fail! :withdrawn-entry-rewritten {:commit commit :slug slug})))
            (when-not (= (get manifest "works")
                         (vec (remove #(added (get % "slug"))
                                      (get predecessor "works"))))
              (fail! :withdrawal-works-mismatch {:commit commit})))

          "event-amendment"
          (do
            (when-not (= (set (keys wd)) (set (keys pwd)))
              (fail! :amendment-changed-withdrawn-set {:commit commit}))
            (when-not (= (get manifest "works") (get predecessor "works"))
              (fail! :amendment-changed-works {:commit commit}))
            (let [changed (set (for [[slug event-id] wd
                                     :when (not= event-id (get pwd slug))]
                                 slug))]
              (when-not (= changed entry-slugs)
                (fail! :amendment-slugs-mismatch {:commit commit}))
              (doseq [{:strs [slug amends]} (get event "entries")]
                (when-not (= amends (get pwd slug))
                  (fail! :amendment-not-linear {:commit commit :slug slug}))
                (when-not (= gov (get wd slug))
                  (fail! :withdrawn-entry-wrong-event
                         {:commit commit :slug slug}))))))))))

(defn- check-genesis! [commit manifest]
  (when-not (nil? (get manifest "governance_event"))
    (fail! :genesis-has-governance-event {:commit commit}))
  (when-not (= [] (get manifest "withdrawn"))
    (fail! :genesis-has-withdrawn {:commit commit})))

(defn- verify-commit!
  "Everything the chain walk establishes about one commit standing alone:
  a single parent, a head that advanced, the `prev_manifest` link to the
  parent's head, the manifest signature, every work artifact, the admission
  pair, the catalog, and any governance event. Returns
  {:parent :parent-head :genesis? :events :verified}.

  `younger` is the already-verified successor commit whose evidence grants
  blob reuse, or nil when there is none, in which case every artifact is
  read and hashed. The transition against the predecessor manifest is NOT
  checked here: it needs the predecessor decoded, which is the caller's
  next move in both directions of travel.

  This is the one definition of what a commit must satisfy. The full walk
  and the increment check both call it, so neither can drift into checking
  less than the other."
  [v c pinned-keys {:keys [m-hex m younger facts]}]
  (let [parents (view/parents-of v c)]
    (when-not (= 1 (count parents))
      (fail! (if (empty? parents) :nonzero-head-at-root :merge-commit)
             {:commit c :parents parents}))
    (let [p (first parents)
          h (head-at v p)
          genesis? (= sign/zero-head-hex h)]
      (when (= m-hex h) (fail! :head-not-advanced {:commit c}))
      (when-not (= h (get m "prev_manifest"))
        (fail! :prev-manifest-mismatch
               {:commit c :head-at-parent h :prev (get m "prev_manifest")}))
      (when genesis? (check-genesis! c m))
      (check-signature! v c pinned-keys "release-manifest" m-hex
                        (manifest-sig-path m-hex))
      (let [;; a reuse grant needs both halves of the proof: the
            ;; younger, already-verified commit held this hex at
            ;; this path, and the tree comparison shows this
            ;; commit's entry is identical
            reuse? (if younger
                     (let [changed (view/changed-paths
                                    v (:commit younger) c "blobs")]
                       (fn [path hex]
                         (and (= hex (get (:verified younger) path))
                              (not (contains? changed path)))))
                     (fn [_ _] false))
            verified (check-works-blobs! v c m {:reuse? reuse?
                                                :facts facts})]
        (check-admission! v c m)
        (check-catalog! v c m)
        (let [events (check-events! v c pinned-keys m)]
          (when genesis?
            (when (seq (view/parents-of v p))
              (fail! :zero-head-after-genesis {:commit p})))
          {:parent p :parent-head h :genesis? genesis?
           :events events :verified verified})))))

(defn- with-event [gov-ids manifest]
  (cond-> gov-ids
    (get manifest "governance_event") (conj (get manifest "governance_event"))))

(defn- pre-genesis-state!
  "The initial commit is the only one allowed to carry the zero head. A zero
  head anywhere else is a chain reset, and saying so is a rule of its own
  rather than a consequence of the walk, so both entry points share it."
  [v commit]
  (when (seq (view/parents-of v commit))
    (fail! :zero-head-after-genesis {:commit commit}))
  {:empty true})

(defn- verify-chain-from
  [v commit pinned-keys]
  (let [head (head-at v commit)]
    (if (= sign/zero-head-hex head)
      (pre-genesis-state! v commit)
      (let [head-manifest (decoded-manifest v commit head)
            facts (atom {})]
        (loop [c commit
               m-hex head
               m head-manifest
               chain []
               gov-ids #{}
               younger nil]
          (let [{:keys [parent parent-head genesis? events verified]}
                (verify-commit! v c pinned-keys
                                {:m-hex m-hex :m m :younger younger :facts facts})
                chain (conj chain m-hex)
                gov-ids (with-event gov-ids m)]
            (if genesis?
              {:head head
               :head-manifest head-manifest
               :chain chain
               :governance-events gov-ids
               :chain-length (count chain)}
              (let [pm (decoded-manifest v parent parent-head)]
                (check-transition! c m pm events)
                (recur parent parent-head pm chain gov-ids
                       {:commit c :verified verified})))))))))

(defn- verify-increment-from
  [v commit pinned-keys {parent-commit :commit prior :result}]
  (let [head (head-at v commit)]
    (when (= sign/zero-head-hex head)
      ;; the same rule the full walk applies, so the two agree on the reason;
      ;; an increment always has a parent, so this never returns
      (pre-genesis-state! v commit)
      (fail! :increment-parent-mismatch
             {:commit commit :parent nil :claimed parent-commit}))
    (let [m (decoded-manifest v commit head)
          {:keys [parent parent-head genesis? events]}
          (verify-commit! v commit pinned-keys
                          {:m-hex head :m m :younger nil :facts (atom {})})]
      ;; the prior result is only a proof of the prefix if it is a proof of
      ;; THIS parent, in the state this commit was built on
      (when-not (= parent parent-commit)
        (fail! :increment-parent-mismatch
               {:commit commit :parent parent :claimed parent-commit}))
      (when-not (= genesis? (boolean (:empty prior)))
        (fail! :increment-prior-shape-mismatch
               {:commit commit :genesis? genesis?
                :prior-empty (boolean (:empty prior))}))
      (if genesis?
        {:head head :head-manifest m :chain [head]
         :governance-events (with-event #{} m)
         :chain-length 1}
        (do
          (when-not (= parent-head (:head prior))
            (fail! :increment-prior-head-mismatch
                   {:commit commit :parent-head parent-head
                    :prior-head (:head prior)}))
          (check-transition! commit m (decoded-manifest v parent parent-head) events)
          {:head head
           :head-manifest m
           :chain (into [head] (:chain prior))
           :governance-events (with-event (:governance-events prior) m)
           :chain-length (inc (:chain-length prior))})))))

(defn verify-repository-at
  "Verify the repository state at `commit` through `v`, with `pinned-keys`
  covering the full chain. Returns {:empty true} for the valid pre-genesis
  initial commit, otherwise {:head <hex> :head-manifest <value>
  :chain [hex ... genesis] :governance-events #{event id ...}
  :chain-length n}. Throws ex-info with :reason on any violation."
  [v commit pinned-keys]
  (sign/validate-pinned-keys! pinned-keys)
  (when-not (view/commit-exists? v commit)
    (fail! :commit-missing {:commit commit}))
  ;; the chain walk reads every artifact of every manifest; one batched
  ;; reader serves the whole pass
  (view/with-batch v (fn [v] (verify-chain-from v commit pinned-keys))))

(defn verify-increment-at
  "Verify `commit` when its parent has already been verified, checking the
  one commit rather than walking the chain again. `verified` is
  {:commit <parent commit id> :result <what `verify-repository-at`
  returned for that commit>}. Returns and throws exactly as
  `verify-repository-at` does.

  This is NOT a trust primitive and must not be used as one. It proves
  nothing about the prefix; it carries forward a proof the caller already
  holds, and it is sound only when the caller computed that proof itself,
  from the same view and the same pinned keys. A publisher verifying the
  commit it has just written against the head it verified moments earlier
  is the case this exists for. A third party arriving at a repository has
  no such proof and must use `verify-repository-at`.

  What it does check is that the proof it was handed is a proof of this
  commit's actual parent, in the state this commit was built on: the
  parent id must match, and so must the head that parent carries. A stale
  or foreign result fails rather than being carried forward."
  [v commit pinned-keys verified]
  (sign/validate-pinned-keys! pinned-keys)
  (when-not (view/commit-exists? v commit)
    (fail! :commit-missing {:commit commit}))
  (view/with-batch v (fn [v] (verify-increment-from v commit pinned-keys verified))))

(def verifier-version "snh-verify/1")

(defn archive-verification
  "Total over readable views: always returns a report
  {:result :success | :failed, :commit, :pinned-fingerprints,
  :verifier-version, and on failure :reason/:detail}. The commit must be
  the concrete lowercase commit id the publication repository names;
  aliases and revision expressions are refused before any read, so the
  identity the report records is the identity that was verified. Success
  requires the commit to be present, to be a publication commit (not the
  pre-genesis empty state), and the full chain to verify with the
  archived view as the sole read source."
  [v commit pinned-keys]
  (let [base {:commit commit
              :verifier-version verifier-version}]
    (try
      (when-not (re-matches #"[0-9a-f]{40}" commit)
        (fail! :malformed-commit {:commit commit}))
      (let [base (assoc base :pinned-fingerprints
                        (into {} (map (fn [[role k]] [role (sign/fingerprint k)]))
                              (sign/validate-pinned-keys! pinned-keys)))
            result (verify-repository-at v commit pinned-keys)]
        (if (:empty result)
          (assoc base :result :failed :reason :not-a-publication-commit)
          (assoc base :result :success :head (:head result)
                 :chain-length (:chain-length result))))
      (catch clojure.lang.ExceptionInfo e
        (assoc base :result :failed
               :reason (:reason (ex-data e))
               :detail (dissoc (ex-data e) :reason))))))
