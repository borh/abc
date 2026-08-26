(ns soranoha.za.oracle
  "The three-set delta acceptance oracle over two kernel runs: (a) the
  source/selection delta, (b) the stages invalidated and executed, (c) the
  artifact delta — with the explanation invariant that every executed stage
  of the second run is accounted for by a changed declared input (a changed
  trace key under equal, complete stage evidence). Manifest set-difference
  alone is not a delta oracle: source edits can preserve output bytes, and
  a catalog edit fans out to every work's metadata stage without touching
  artifacts.

  Runs come either from the in-process harness or from decode-run over a
  disposable build report; both yield the same shape, so there is one
  oracle representation."
  (:import (tools.jackson.core StreamReadFeature)
           (tools.jackson.databind JsonNode)
           (tools.jackson.databind.json JsonMapper)))

;; --- deltas -----------------------------------------------------------------

(defn- entry-delta [a b]
  {:added (set (remove a (keys b)))
   :removed (set (remove b (keys a)))
   :changed (set (for [[k v] b
                       :when (and (contains? a k) (not= v (a k)))]
                   k))
   :retained (set (for [[k v] b
                        :when (= v (a k))]
                    k))})

(defn source-delta
  "(a) per-slug zip-content delta between two runs."
  [run-a run-b]
  (dissoc (entry-delta (:zip-hashes run-a) (:zip-hashes run-b)) :retained))

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
                              (get m "works")))]
    (entry-delta (by-slug manifest-a) (by-slug manifest-b))))

(def ^:private artifact-fields ["parser-ir" "plaintext" "tei" "tei-validation"])

(defn report-artifact-delta
  "(c) per-slug artifact-byte delta between two decoded run reports:
  compares only the artifact hashes, so a source repackaging that preserves
  output bytes is :retained."
  [run-a run-b]
  (let [pick (fn [run]
               (into {}
                     (map (fn [[slug work]]
                            [slug (select-keys work artifact-fields)]))
                     (:works run)))]
    (entry-delta (pick run-a) (pick run-b))))

;; --- explanation invariant --------------------------------------------------

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

;; --- report boundary decode -------------------------------------------------

(def ^:private ^JsonMapper strict-report-mapper
  (-> (JsonMapper/builder)
      (.enable (into-array StreamReadFeature
                           [StreamReadFeature/STRICT_DUPLICATE_DETECTION]))
      (.build)))

(defn- reject! [reason detail]
  (throw (ex-info (str "run report rejected: " (name reason))
                  (assoc detail :reason reason))))

(defn- node->clj [^JsonNode node]
  (cond
    (.isObject node) (into {}
                           (map (fn [e] [(key e) (node->clj (val e))]))
                           (.properties node))
    (.isArray node) (mapv node->clj node)
    (.isTextual node) (.textValue node)
    (.isBoolean node) (.booleanValue node)
    (.isIntegralNumber node) (.longValue node)
    (.isNumber node) (.doubleValue node)
    (.isNull node) nil
    :else (reject! :unsupported-json-node {:node-type (str (class node))})))

(def ^:private hex64-pattern #"[0-9a-f]{64}")

(defn- hex64! [context value]
  (when-not (and (string? value) (re-matches hex64-pattern value))
    (reject! :not-a-content-hash {:context context :value value}))
  value)

(def ^:private coordinate-keys #{"stage_id" "stage_version" "toolchain_id"})

(defn- coordinates! [stages]
  (when-not (and (map? stages) (seq stages))
    (reject! :stages-not-an-object {}))
  (into {}
        (map (fn [[stage-name coordinate]]
               (when-not (and (map? coordinate)
                              (= coordinate-keys (set (keys coordinate)))
                              (every? (fn [v] (and (string? v)
                                                   (seq v)))
                                      (vals coordinate)))
                 (reject! :malformed-stage-coordinate
                          {:stage stage-name :coordinate coordinate}))
               [(keyword stage-name) coordinate]))
        stages))

(defn- work-evidence! [known-stages slug work]
  (let [stage-map (fn [field valid?]
                    (let [m (get work field)]
                      ;; exact coverage, not subset: a stage silently absent
                      ;; from the evidence would erase its execution from the
                      ;; oracle's comparison
                      (when-not (and (map? m)
                                     (= known-stages
                                        (set (map keyword (keys m))))
                                     (every? valid? (vals m)))
                        (reject! :malformed-work-evidence
                                 {:slug slug :field field :value m}))
                      (into {} (map (fn [[k v]] [(keyword k) v])) m)))]
    {:cached (stage-map "cached" boolean?)
     :trace-keys (stage-map "trace_keys"
                            #(and (string? %)
                                  (re-matches hex64-pattern %)))}))

(defn decode-run
  "Strict boundary decode of one disposable build report into the run shape
  the oracle consumes: strict JSON with duplicate keys rejected, stage
  coordinates exactly {stage_id, stage_version, toolchain_id} non-blank
  strings, per-work cached/trace_keys maps whose stage sets equal the
  coordinate table's stages exactly, 64-hex trace keys and artifact/source
  hashes. Throws ex-info with :reason on any violation."
  [^bytes report-bytes]
  (let [report (node->clj
                (try (.readTree strict-report-mapper report-bytes)
                     (catch Exception e
                       (reject! :parse-invalid {:cause (ex-message e)}))))
        _ (when-not (map? report) (reject! :report-not-an-object {}))
        coordinates (coordinates! (get report "stages"))
        works (get report "works")]
    (when-not (map? works) (reject! :works-not-an-object {}))
    {:commit (get report "aozora_git_commit")
     :stage-coordinates coordinates
     :zip-hashes (into {}
                       (map (fn [[slug work]]
                              [slug (hex64! [slug "source_zip"]
                                            (get work "source_zip"))]))
                       works)
     :works (into {}
                  (map (fn [[slug work]]
                         [slug (into {}
                                     (map (fn [field]
                                            [field (hex64! [slug field]
                                                           (get work field))]))
                                     artifact-fields)]))
                  works)
     :results (into {}
                    (map (fn [[slug work]]
                           [slug (work-evidence! (set (keys coordinates))
                                                 slug work)]))
                    works)}))
