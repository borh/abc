(ns soranoha.snh.chain-bench
  "Publication, no-op and full-chain verification wall time against chain length.

  The publication rearchitecture ledger measured cost growing with releases
  times works and made correcting it a precondition for unattended activation.
  Batching has landed since. This measures whether the growth term is still
  there, in isolation from the build: releases are synthetic, so no corpus is
  needed and the repository path is what is being timed.

  Two shapes bracket the real one. `--changed 0` moves no work between releases,
  which is what the reuse grant is for and what an upstream commit touching a
  handful of works approximates at corpus scale. `--changed all` moves every
  work, which is what a toolchain change does. A real per-commit chain sits at
  the first, because a commit changes a median of two works out of 17810.

  Run from soranoha/:
    clojure -Sdeps '{:paths [\"src\" \"test\" \"resources\"]}' -M \\
      -m soranoha.snh.chain-bench \\
      --works 2000 --releases 24 --changed 3 --tmp /data/soranoha-bench

  Pass `--tmp` a path on real storage. A chain at corpus scale is gigabytes of
  loose objects before it is packed, and the system temp directory is
  memory-backed on the machines this runs on, so the default puts the chain in
  RAM alongside the JVM heap it is being measured with.

  Results are JSON lines on stdout. `verify_ms_marginal` is the cost of the one
  additional manifest, which is the quantity that decides whether the chain can
  grow. A single marginal is noisy enough to come out negative; read the slope
  across the whole run rather than any one row.

  `--carry` threads each publication's own proof of the head it just wrote
  into the next, which is what a backfill publishing a run of releases in one
  process does. Without it every release re-verifies the whole chain, so a run
  of N is quadratic; with it `publish_ms` should stop growing. Only
  `publish_ms` changes: `noop_ms` and `verify_ms` are what a party holding no
  proof pays, which is the scheduled job and the third-party verifier.

  `noop_ms` is the one the prerequisite is stated against. The ledger's bound is
  on an invocation that publishes nothing, because a scheduled job that fires on
  every upstream commit spends most of its runs discovering it has nothing to
  do. It is measured by repeating the release with its projection unchanged,
  which is what `decide-against-head` no-ops on, so it is the cost of resolving
  the head and deciding, without an assembly.

  Timings are wall time on this machine for a synthetic chain with small blobs.
  They establish the shape of the growth curve, not a production figure."
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [charred.api :as json]
            [soranoha.snh.fixture :as fx]
            [soranoha.snh.repo :as repo]
            [soranoha.snh.transact :as transact]
            [soranoha.snh.verify :as verify]
            [soranoha.snh.view :as view]))

(defn- millis [f]
  (let [start (System/nanoTime)
        result (f)]
    {:result result :milliseconds (/ (- (System/nanoTime) start) 1e6)}))

(defn- slugs [n]
  (mapv #(format "%06d_%06d" % %) (range n)))

(defn- variant-fn
  "Which works differ at release `release`. `changed` works move; the rest keep
  the genesis variant, so the verifier's reuse grant applies to them."
  [changed release all]
  (if (= :all changed)
    (constantly (str "v" release))
    (let [moved (set (take changed (drop (* changed release) (cycle all))))]
      (fn [slug] (if (contains? moved slug) (str "v" release) "v0")))))

(defn- verify-once! [clone]
  (let [v (view/git-view clone)
        commit (repo/fetch! clone fx/branch)]
    (millis #(verify/verify-repository-at v commit (fx/pinned-keys)))))

(defn -main [& args]
  (try
    (let [{:keys [works releases changed out tmp carry]}
          (cli/parse-opts args {:coerce {:works :long :releases :long :carry :boolean}})
          ;; babashka.cli parses a bare number itself, so `changed` arrives as a
          ;; long already; "all" is the only value that stays a string
          changed (cond (= "all" changed) :all
                        (integer? changed) changed
                        :else (parse-long (or changed "3")))
          works (or works 500)
          releases (or releases 12)
          all (slugs works)
          ;; the system temp directory is memory-backed here, so the default
          ;; competes with the JVM for the RAM this is measuring; `--tmp` puts
          ;; the chain on real storage instead
          dir (if tmp
                (fs/create-temp-dir {:prefix "snh-chain-bench" :dir (fs/path tmp)})
                (fs/create-temp-dir {:prefix "snh-chain-bench"}))
          origin (repo/init-origin! (fs/path dir "origin.git"))
          clone (repo/clone! origin (fs/path dir "chain"))
          _ (transact/init-publication-branch! clone fx/branch)
          emit! (fn [row]
                  (let [line (str (json/write-json-str row) "\n")]
                    (when out (spit (str out) line :append true))
                    (print line) (flush)))]
      (emit! {"phase" "setup" "works" works "releases" releases
              "changed" (str changed) "dir" (str dir)})
      (try
        ;; the cost of verifying one more manifest is the quantity that decides
        ;; whether the chain can grow, so it is reported directly. The average
        ;; over the chain falls as the fixed setup is amortised, which reads as
        ;; an improvement while the real per-release cost is flat or rising.
        (reduce
         (fn [{:keys [previous proof]} release]
           (let [opts {:admitted all
                       :variant (variant-fn changed release all)
                       ;; the projection must move or an unchanged build is a
                       ;; no-op and publishes nothing
                       :selection-params {"config" "bench" "round" (str release)}}
                 publish (millis #(fx/publish! clone opts proof))
                 ;; the same opts again: the projection now matches the head, so
                 ;; this is the scheduled job finding nothing to do
                 noop (millis #(fx/publish! clone opts))
                 verify (:milliseconds (verify-once! clone))
                 outcome (get-in noop [:result :outcome])]
             (when-not (= :already-published outcome)
               (throw (ex-info "repeat build was not a no-op, so noop_ms is not the no-op cost"
                               {:outcome outcome :release release})))
             (emit! (cond-> {"phase" "release"
                             "chain_length" (inc release)
                             "works" works
                             "carried" (boolean carry)
                             "publish_ms" (Math/round ^double (:milliseconds publish))
                             "noop_ms" (Math/round ^double (:milliseconds noop))
                             "verify_ms" (Math/round ^double verify)}
                      previous
                      (assoc "verify_ms_marginal"
                             (Math/round ^double (- verify ^double previous)))))
             {:previous verify
              ;; only `publish_ms` is measured under the carry: `noop_ms` and
              ;; `verify_ms` are the cost of a party that holds no proof, which
              ;; is what a scheduled job and a third party respectively pay
              :proof (when carry (get-in publish [:result :verified-head]))}))
         {}
         (range releases))
        (finally (fs/delete-tree dir))))
    (finally (shutdown-agents))))
