(ns abc.tools.aozora-replay
  "Replay abc's audit machinery over the real pinned aozorabunko history
  and pin the per-pair findings as a committed baseline. Design:
  docs/superpowers/specs/2026-07-12-aozora-replay-harness-design.md."
  (:require [abc.git :as abc-git]
            [abc.tools.aozora-csv :as ac]
            [abc.tools.aozora-history-audit :as audit]
            [abc.tools.cli :as abc-cli]
            [abc.tools.json :as abc-json]
            [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [taoensso.telemere :as tel])
  (:import [java.io ByteArrayInputStream IOException]
           [java.util.zip ZipException ZipInputStream]
           [org.eclipse.jgit.errors MissingObjectException]))

(def baseline-format 1)
(def default-remote-url "https://github.com/aozorabunko/aozorabunko.git")
(def default-zip-path "index_pages/list_person_all_extended_utf8.zip")
(def default-baseline-path "test/resources/aozora-replay-baseline.json")

(defn locked-pin
  "The aozorabunko-src locked rev from a flake.lock file."
  [lock-path]
  (let [lock (abc-json/read-json-file lock-path)
        rev (get-in lock ["nodes" "aozorabunko-src" "locked" "rev"])]
    (when-not (and (string? rev) (re-matches #"[0-9a-f]{40}" rev))
      (throw (ex-info (str "no aozorabunko-src locked rev in " lock-path)
                      {:lock-path (str lock-path)})))
    rev))

(defn- zip-signature?
  "True when the bytes begin with a ZIP local-file-header (PK\\x03\\x04) or
  empty-archive end-of-central-directory (PK\\x05\\x06) signature.
  ZipInputStream.getNextEntry silently returns nil on most non-ZIP bytes,
  which would misreport garbage as no-csv-entry — so the signature is
  checked explicitly first."
  [^bytes bs]
  (and (>= (alength bs) 4)
       (= 0x50 (bit-and 0xff (aget bs 0)))
       (= 0x4B (bit-and 0xff (aget bs 1)))
       (contains? #{[3 4] [5 6]}
                  [(bit-and 0xff (aget bs 2)) (bit-and 0xff (aget bs 3))])))

(defn catalog-bytes-fault
  "nil when the bytes are a usable catalog ZIP; otherwise the source-fact
  reason string. Only ZIP-structural problems are absorbed here; anything
  else escapes as a harness/environment concern."
  [^bytes bs]
  (if-not (zip-signature? bs)
    "unreadable-zip"
    (try
      (with-open [zin (ZipInputStream. (ByteArrayInputStream. bs))]
        (loop []
          (if-let [entry (.getNextEntry zin)]
            (if (string/ends-with? (.getName entry) ".csv")
              (let [csv (String. (.readAllBytes zin) "UTF-8")]
                (if (seq (ac/read-rows-from-string csv)) nil "no-data-rows"))
              (recur))
            "no-csv-entry")))
      (catch ZipException _ "unreadable-zip")
      (catch IOException _ "unreadable-zip"))))

(defn pair-digest
  "Digest one scan pair-report into the pinned baseline pair shape."
  [period-by-ref pair]
  (let [ingest (:current_ingest pair)]
    {"previous_ref" (:previous_ref pair)
     "current_ref" (:current_ref pair)
     "period" (get period-by-ref (:current_ref pair))
     "status" (:status pair)
     "drift_summary" (get-in pair [:drift "summary"])
     "ingest" {"works_written" (:works-written ingest)
               "works_skipped" (:works-skipped ingest)
               "skipped_work_ids" (vec (:skipped-work-ids ingest))
               "persons_written" (:persons-written ingest)
               "person_conflicts" (mapv #(get % "person_id")
                                        (:person-conflicts ingest))}}))

(defn baseline-doc
  [{:keys [remote-url pin-rev zip-path sample-period excluded pairs
           period-by-ref]}]
  {"baseline_format" baseline-format
   "remote_url" remote-url
   "pin_rev" pin-rev
   "zip_path" zip-path
   "sample_period" sample-period
   "excluded" (vec excluded)
   "pairs" (mapv #(pair-digest period-by-ref %) pairs)})

(defn- header [doc]
  (select-keys doc ["baseline_format" "zip_path" "sample_period" "remote_url"]))

(defn- pair-changes [old-pairs new-pairs]
  (let [n (max (count old-pairs) (count new-pairs))]
    (vec
     (for [i (range n)
           :let [o (get old-pairs i)
                 nw (get new-pairs i)]]
       {"index" i
        "change" (cond
                   (nil? o) "added"
                   (nil? nw) "removed"
                   (= o nw) "unchanged"
                   :else "replaced")}))))

(defn- strict-final-replacement?
  "The ONLY replacement pin-bump-shaped tolerates: same period, same
  previous_ref, different current_ref (the final period gained a later
  representative). A digest change on unchanged input refs is never
  pin-bump-shaped, and neither is a replacement that moves the pair to a
  different period. Ancestry of the new current_ref is not provable in a
  pure comparison; it is implied by the plan (representatives are sampled
  from commits reachable from the fetched pin) and by human review of the
  update diff."
  [old-pair new-pair]
  (and (= (get old-pair "period") (get new-pair "period"))
       (= (get old-pair "previous_ref") (get new-pair "previous_ref"))
       (not= (get old-pair "current_ref") (get new-pair "current_ref"))))

(defn- exclusions-only-newer? [old-doc new-doc]
  (let [old-ex (set (get old-doc "excluded"))
        new-ex (set (get new-doc "excluded"))
        last-period (get (peek (get old-doc "pairs")) "period")]
    (and (every? new-ex old-ex)
         (every? (fn [e] (and (some? (get e "period"))
                              (some? last-period)
                              (pos? (compare (get e "period") last-period))))
                 (remove old-ex new-ex)))))

(defn classify-diff
  "Compare a committed baseline doc against a freshly produced one.
  Verdict semantics per the design spec's diff-classification section."
  [old-doc new-doc]
  (let [old-pairs (vec (get old-doc "pairs"))
        new-pairs (vec (get new-doc "pairs"))
        changes (pair-changes old-pairs new-pairs)
        n (count old-pairs)]
    {:pair-changes changes
     :verdict
     (cond
       (not= (header old-doc) (header new-doc))
       :configuration-change

       (= old-doc new-doc)
       :unchanged

       (= (get old-doc "pin_rev") (get new-doc "pin_rev"))
       :behavioral-change

       (and (>= (count new-pairs) n)
            (= (subvec new-pairs 0 (max 0 (dec n)))
               (subvec old-pairs 0 (max 0 (dec n))))
            (or (zero? n)
                (let [o (peek old-pairs) nw (get new-pairs (dec n))]
                  (or (= o nw) (strict-final-replacement? o nw))))
            (exclusions-only-newer? old-doc new-doc))
       :pin-bump-shaped

       :else
       :behavioral-change)}))

;; ---------------------------------------------------------------------
;; Git effects: managed partial clone + tiered blob availability.

(defn default-cache-dir
  "Managed-clone location. Deliberately on the /db/ data volume, not
  ~/.cache: the partial clone is multi-GB-scale and ~/.cache is a small
  tmpfs on the primary workstation."
  []
  "/db/abc/cache/aozorabunko")

(defn- git*
  "Run git with argv `args` (strings), optionally in `dir`. Returns the
  babashka.process result map; never throws on nonzero exit."
  [args {:keys [dir out-enc]}]
  (process/sh (into ["git"] (map str args))
              (cond-> {:out :string :err :string}
                dir (assoc :dir (str dir))
                out-enc (assoc :out-enc out-enc))))

(defn- git!
  "Like git*, but nonzero exit throws ex-info with the command context."
  [args {:keys [dir] :as opts}]
  (let [{:keys [exit err] :as res} (git* args opts)]
    (if (zero? exit)
      res
      (throw (ex-info (str "git " (string/join " " args) " failed ("
                           exit "): " (string/trim (or err "")))
                      {:git-args (vec (map str args))
                       :dir (some-> dir str)
                       :exit exit})))))

(defn- ensure-commit!
  "Ensure `ref` resolves to a commit object in `dir`, fetching if needed."
  [dir ref]
  (when-not (zero? (:exit (git* ["cat-file" "-e" (str ref "^{commit}")]
                                {:dir dir})))
    (when-not (zero? (:exit (git* ["fetch" "origin" ref] {:dir dir})))
      (git! ["fetch" "origin"] {:dir dir}))
    (git! ["cat-file" "-e" (str ref "^{commit}")] {:dir dir})))

(defn verify-origin!
  "Verify a repo's origin URL matches the expected remote-url. The
  baseline records remote_url as the upstream identity, so a repo whose
  origin disagrees must be refused. A repo with NO origin remote (e.g. a
  locally built test repo) is allowed with a warning: the recorded
  remote_url is then a declared, not observed, upstream."
  [repo-dir remote-url]
  (let [{:keys [exit out]} (git* ["remote" "get-url" "origin"]
                                 {:dir repo-dir})]
    (if (zero? exit)
      (let [actual (string/trim out)]
        (when (not= actual remote-url)
          (throw (ex-info (str "repo at " repo-dir " has origin " actual
                               ", expected " remote-url)
                          {:cache-dir (str repo-dir)
                           :expected-url remote-url
                           :actual-url actual}))))
      (tel/log! :warn (str "repo at " repo-dir " has no origin remote; "
                           "recording remote_url as declared upstream: "
                           remote-url)))))

(defn ensure-clone!
  "Create or update the managed blobless partial clone. Verifies cache
  provenance (origin URL must equal remote-url) before fetching — the
  baseline must never record one source while replaying another.
  Returns cache-dir as a string."
  [{:keys [cache-dir remote-url to-ref]}]
  (let [dir (fs/path cache-dir)]
    (if (fs/directory? (fs/path dir ".git"))
      (verify-origin! (str dir) remote-url)
      (do (fs/create-dirs dir)
          (git! ["clone" "--filter=blob:none" "--no-checkout"
                 remote-url (str dir)]
                {})))
    (when to-ref (ensure-commit! (str dir) to-ref))
    (str dir)))

(defn blob-availability
  "Tiered read of zip-path bytes at ref (spec unit 3):
  {:bytes bs} | {:excluded \"missing-at-ref\"} | :missing-object."
  [repo ref zip-path]
  (try
    {:bytes (abc-git/blob-bytes-at repo ref zip-path)}
    (catch clojure.lang.ExceptionInfo e
      (if (= zip-path (:path (ex-data e)))
        {:excluded "missing-at-ref"}
        (throw e)))
    (catch MissingObjectException _ :missing-object)))

(defn ensure-blob-bytes
  "Bytes of zip-path at ref, attempting ONE CLI promisor fetch when the
  object is promised but locally absent. A still-missing object is an
  environment failure (loud), never an exclusion."
  [repo repo-dir ref zip-path]
  (let [r (blob-availability repo ref zip-path)]
    (if (not= :missing-object r)
      r
      (do
        (try
          (git! ["cat-file" "blob" (str ref ":" zip-path)]
                {:dir repo-dir :out-enc :bytes})
          (catch Exception e
            (throw (ex-info (str "object for " zip-path " at " ref
                                 " is unavailable locally and the promisor "
                                 "fetch failed")
                            {:ref ref :zip-path zip-path :repo (str repo-dir)
                             :cause-tier :missing-local-object}
                            e))))
        (let [r2 (blob-availability repo ref zip-path)]
          (if (= :missing-object r2)
            (throw (ex-info (str "object for " zip-path " at " ref
                                 " still missing after promisor fetch")
                            {:ref ref :zip-path zip-path :repo (str repo-dir)
                             :cause-tier :missing-local-object}))
            r2))))))

(defn prefetch-and-prevalidate!
  "Partition the plan into surviving refs and pinned source-fact
  exclusions. Runs identically for managed caches and --aozora-repo."
  [repo repo-dir plan zip-path]
  (reduce
   (fn [acc {:keys [ref period]}]
     (let [{:keys [bytes excluded]} (ensure-blob-bytes repo repo-dir ref zip-path)
           reason (or excluded (catalog-bytes-fault bytes))]
       (if reason
         (update acc :excluded conj {"ref" ref "period" period "reason" reason})
         (update acc :refs conj {:ref ref :period period}))))
   {:refs [] :excluded []}
   plan))

;; ---------------------------------------------------------------------
;; Orchestration + CLI.

(defn- log-phase!
  "Progress + timing telemetry for the long-running phases. Timing lives
  ONLY in logs (telemere timestamps), never in the baseline."
  [phase started-ms detail]
  (tel/log! :info (str "replay " phase " ("
                       (- (System/currentTimeMillis) started-ms) " ms): "
                       detail)))

(defn replay-doc!
  "Plan, prefetch/pre-validate, scan, digest. Returns the baseline doc."
  [{:keys [aozora-repo cache-dir remote-url to-ref from-ref sample-period
           zip-path work-dir]}]
  (let [t0 (System/currentTimeMillis)
        repo-dir (if aozora-repo
                   (do (verify-origin! (str aozora-repo) remote-url)
                       aozora-repo)
                   (ensure-clone! {:cache-dir cache-dir
                                   :remote-url remote-url
                                   :to-ref to-ref}))
        _ (log-phase! "clone-ready" t0 (str repo-dir))
        repo (abc-git/load-git-repo (str repo-dir))]
    (try
      (let [t1 (System/currentTimeMillis)
            pin-rev (.getName (abc-git/resolve-ref repo (or to-ref "HEAD")))
            plan (audit/scan-plan repo {:zip-path zip-path
                                        :from-ref from-ref
                                        :to-ref to-ref
                                        :sample-period sample-period})
            _ (log-phase! "plan" t1 (str (count plan) " representatives"))
            t2 (System/currentTimeMillis)
            {:keys [refs excluded]} (prefetch-and-prevalidate!
                                     repo repo-dir plan zip-path)
            _ (log-phase! "prefetch" t2 (str (count refs) " usable, "
                                             (count excluded) " excluded"))
            t3 (System/currentTimeMillis)
            scan (audit/scan-history! {:aozora-repo (str repo-dir)
                                       :refs (mapv :ref refs)
                                       :zip-path zip-path
                                       :work-dir work-dir})
            _ (log-phase! "scan" t3 (str (count (:pairs scan)) " pairs"))]
        (baseline-doc {:remote-url remote-url
                       :pin-rev pin-rev
                       :zip-path zip-path
                       :sample-period sample-period
                       :excluded excluded
                       :pairs (:pairs scan)
                       :period-by-ref (into {} (map (juxt :ref :period)) refs)}))
      (finally (.close repo)))))

(defn run-replay!
  "Run one replay in :check or :update mode. Returns a result map with
  ::exit-fail? set for CLI dispatch."
  [{:keys [baseline] update? :update :as opts}]
  (if update?
    (let [doc (replay-doc! opts)]
      (abc-json/write-deterministic-json-file! (io/file baseline) doc)
      {:mode "update" :baseline (str baseline)
       :pairs (count (get doc "pairs"))
       :excluded (count (get doc "excluded"))
       ::exit-fail? false})
    ;; check: read the committed baseline BEFORE the replay so an
    ;; unreadable file fails in milliseconds, not after the scan
    (let [old (try (abc-json/read-json-file baseline)
                   (catch Exception e
                     (throw (ex-info (str "baseline unreadable: " baseline)
                                     {:baseline (str baseline)} e))))
          doc (replay-doc! opts)
          {:keys [verdict pair-changes]} (classify-diff old doc)]
      {:mode "check" :baseline (str baseline)
       :verdict (name verdict)
       :pair_changes pair-changes
       ::exit-fail? (not= :unchanged verdict)})))

(def ^:private cli-options
  [[nil "--check" "Compare a fresh replay against the committed baseline"]
   [nil "--update" "Rewrite the baseline from a fresh replay"]
   [nil "--sample-period PERIOD" "Sampling period: month or year"
    :default "year"]
   [nil "--from-ref REF" "Optional window start (ad-hoc runs only)"]
   [nil "--to-ref REF" "Replay end ref; defaults to the abc/flake.lock pin"]
   [nil "--cache-dir DIR" "Managed partial-clone location"]
   [nil "--remote-url URL" "Upstream remote for the managed clone"]
   [nil "--aozora-repo DIR" "Use an existing clone (skips clone management only)"]
   [nil "--baseline FILE" "Baseline path"]
   [nil "--work-dir DIR" "Tool-owned scan work dir"
    :default "out/aozora-replay"]
   [nil "--zip-path PATH" "Catalog ZIP path inside the upstream repo"]
   ["-h" "--help"]])

(defn- usage [summary]
  (str "Usage: clojure -M:abc/aozora-replay -- (--check | --update) [options]\n\n"
       "Replays the pinned aozorabunko history (year-sampled) through the\n"
       "audit machinery and checks/updates the committed baseline.\n\n"
       summary))

(defn resolve-options
  "Fill defaults that need runtime context and enforce mode guards.
  Public: this is the boundary that protects the committed baseline from
  accidental ad-hoc overwrites, and it is tested directly."
  [{:keys [check baseline sample-period from-ref to-ref] update? :update :as options}]
  (when (= (boolean check) (boolean update?))
    (throw (ex-info "exactly one of --check / --update is required"
                    {:check (boolean check) :update (boolean update?)})))
  (let [pin (locked-pin "flake.lock")
        baseline (or baseline default-baseline-path)
        ;; canonical compare: the guard must not be bypassable by path
        ;; spelling (./, absolute, ..) of the committed baseline
        default-baseline? (= (fs/canonicalize baseline)
                             (fs/canonicalize default-baseline-path))]
    (when (and update? default-baseline?
               (or (not= "year" sample-period)
                   (some? from-ref)
                   (and (some? to-ref) (not= to-ref pin))))
      (throw (ex-info (str "refusing --update of the default baseline with "
                           "non-default sampling/window flags; pass --baseline "
                           "for ad-hoc runs")
                      {:sample-period sample-period :from-ref from-ref
                       :to-ref to-ref :pin pin})))
    (-> options
        (assoc :baseline baseline)
        (update :to-ref #(or % pin))
        (update :cache-dir #(or % (default-cache-dir)))
        (update :remote-url #(or % default-remote-url))
        (update :zip-path #(or % default-zip-path)))))

(defn -main [& args]
  (abc-cli/run-cli!
   args
   {:cli-options cli-options
    :usage-fn usage
    :run (fn [{:keys [options]}]
           (let [result (run-replay! (resolve-options options))]
             (println (abc-json/write-deterministic-json-str
                       (dissoc result ::exit-fail?)))
             result))
    :fail? ::exit-fail?}))
