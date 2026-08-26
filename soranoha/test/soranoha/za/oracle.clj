(ns soranoha.za.oracle
  "The three-set delta acceptance oracle over two kernel runs and their
  published manifests: (a) the source/selection delta, (b) the stages
  invalidated and executed, (c) the artifact/manifest delta — with the
  explanation invariant that every executed stage of the second run is
  accounted for by a changed declared input. Manifest set-difference alone
  is not a delta oracle: source edits can preserve output bytes, and a
  catalog edit fans out to every work's metadata stage without touching
  artifacts.")

(defn source-delta
  "(a) per-slug zip-content delta between two runs."
  [run-a run-b]
  (let [a (:zip-hashes run-a)
        b (:zip-hashes run-b)]
    {:added (set (remove a (keys b)))
     :removed (set (remove b (keys a)))
     :changed (set (for [[slug hex] b
                         :when (and (contains? a slug) (not= hex (a slug)))]
                     slug))}))

(defn executed-stages
  "(b) slug -> set of stage keys the engine actually executed in `run`."
  [run]
  (into {}
        (map (fn [[slug {:keys [cached]}]]
               [slug (set (for [[stage cached?] cached
                                :when (false? cached?)]
                            stage))]))
        (:results run)))

(defn works-delta
  "(c) per-slug works-entry delta between two decoded manifest values."
  [manifest-a manifest-b]
  (let [by-slug (fn [m] (into {}
                              (map (juxt #(get % "slug") identity))
                              (get m "works")))
        a (by-slug manifest-a)
        b (by-slug manifest-b)]
    {:added (set (remove a (keys b)))
     :removed (set (remove b (keys a)))
     :changed (set (for [[slug entry] b
                         :when (and (contains? a slug)
                                    (not= entry (a slug)))]
                     slug))
     :retained (set (for [[slug entry] b
                          :when (= entry (a slug))]
                      slug))}))

(defn- incomparable! [data]
  (throw (ex-info "runs incomparable: missing or divergent stage evidence"
                  (assoc data :reason :runs-incomparable))))

(defn unexplained-executions
  "Invariant check over the engine's own derivation keys: every stage
  executed in `run-b` for a work already present in `run-a` must carry a
  changed trace key — i.e. a changed declared input (a work new to run-b
  explains all its executions; a same-key execution surfaces missing-blob
  recovery work, which is exactly what the invariant should expose).

  A changed key means a changed input only under complete, identical
  stage evidence, so the check fails closed with :runs-incomparable when
  the runs' stage-coordinate tables are absent or differ, when a compared
  execution's stage is not covered by the table, or when either run lacks
  that execution's trace key. Returns violations as
  [{:slug :stage :trace-key}]; the oracle passes when this is empty."
  [run-a run-b]
  (let [coordinates-a (:stage-coordinates run-a)
        coordinates-b (:stage-coordinates run-b)]
    (when (or (nil? coordinates-a) (nil? coordinates-b)
              (not= coordinates-a coordinates-b))
      (incomparable! {:coordinates-a coordinates-a
                      :coordinates-b coordinates-b}))
    (let [compared (for [[slug stages] (executed-stages run-b)
                         :when (contains? (:results run-a) slug)
                         stage stages]
                     {:slug slug
                      :stage stage
                      :key-a (get-in run-a [:results slug :trace-keys stage])
                      :key-b (get-in run-b [:results slug :trace-keys stage])})]
      (doseq [{:keys [slug stage key-a key-b]} compared]
        (when-not (contains? coordinates-a stage)
          (incomparable! {:slug slug :stage stage
                          :missing :stage-coordinates}))
        (when-not (and (string? key-a) (string? key-b))
          (incomparable! {:slug slug :stage stage :missing :trace-key
                          :trace-key-a key-a :trace-key-b key-b})))
      (vec (for [{:keys [slug stage key-a key-b]} compared
                 :when (= key-a key-b)]
             {:slug slug :stage stage :trace-key key-b})))))
