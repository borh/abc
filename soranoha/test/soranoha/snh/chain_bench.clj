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

  `verify_ms` is one walk at every chain length, not the run count a verifier
  would choose. The column is the growth curve, and the automatic count adds a
  run every 32 releases, which would step it down at each multiple and hide
  the slope. `--segments` measures the split instead.

  `--carry` threads each publication's own proof of the head it just wrote
  into the next, which is what a backfill publishing a run of releases in one
  process does. Without it every release re-verifies the whole chain, so a run
  of N is quadratic; with it `publish_ms` should stop growing. Only
  `publish_ms` changes: `noop_ms` and `verify_ms` are what a party holding no
  proof pays, which is the scheduled job and the third-party verifier.

  `--segments 1,2,4,8,16` verifies the finished chain once at each run count
  and emits a `segmented` row for each. One walk is `segments 1`, so the rows
  are directly comparable, and all of them are taken over one chain.

  `--repo-stats` reports what an origin holding the finished chain costs to
  keep and to hand out: loose objects before maintenance, repack time, packed
  size, and the apparent against on-disk size of every manifest. That last
  pair answers whether consecutive manifests delta-compress, which decides
  whether an origin at one release per upstream commit is gigabytes or
  hundreds of them. It repacks the origin, so run it last.

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
            [babashka.process :as process]
            [clojure.string :as str]
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

(defn- verify-once!
  ([clone] (verify-once! clone nil))
  ([clone opts]
   (let [v (view/git-view clone)
         commit (repo/fetch! clone fx/branch)]
     (millis #(verify/verify-repository-at v commit (fx/pinned-keys) opts)))))

(defn- segment-counts
  "Run counts to measure the built chain at, from a comma-separated
  `--segments`. Empty when the option is absent, so a run that only wants
  the growth curve pays nothing for this."
  [segments]
  (if (nil? segments)
    []
    (mapv parse-long (str/split (str segments) #","))))

(defn- git-out
  [dir args]
  (let [{:keys [exit out err]}
        (apply process/sh {:dir (str dir) :out :string :err :string} "git" args)]
    (when-not (zero? exit)
      (throw (ex-info "git failed in the benchmark" {:args args :err err})))
    out))

(defn- count-objects
  "`git count-objects -v` as a map of its keys to longs. Loose counts are what
  an origin accumulates between maintenance runs; `size-pack` is in KiB and is
  what a clone transfers."
  [dir]
  (into {}
        (map (fn [line]
               (let [[k v] (str/split line #": ")]
                 [k (parse-long (str/trim v))])))
        (str/split-lines (str/trim (git-out dir ["count-objects" "-v"])))))

(defn- manifest-blob-sizes
  "For every manifest in the chain, its size and the bytes it occupies in the
  pack. `releases/` accumulates, so the head tree names all of them.

  The ratio between the two columns is the question: consecutive manifests
  differ in a few work entries out of tens of thousands, and whether git's
  delta compression finds that is what decides whether an origin holding one
  release per upstream commit is gigabytes or hundreds of gigabytes."
  [dir branch]
  ;; `100644 blob <sha>\t<name>`, so splitting on whitespace puts the id third
  (let [ids (into []
                  (comp (filter #(str/ends-with? % ".json"))
                        (map #(nth (str/split % #"\s+") 2)))
                  (str/split-lines (git-out dir ["ls-tree" (str branch ":releases")])))
        out (:out (process/sh {:dir (str dir) :in (str/join "\n" ids) :out :string}
                              "git" "cat-file"
                              "--batch-check=%(objectsize) %(objectsize:disk)"))]
    (reduce (fn [acc line]
              (let [[size disk] (map parse-long (str/split (str/trim line) #"\s+"))]
                (-> acc
                    (update :bytes + size)
                    (update :disk_bytes + disk)
                    (update :largest_bytes max size))))
            {:bytes 0 :disk_bytes 0 :largest_bytes 0 :count (count ids)}
            (remove str/blank? (str/split-lines out)))))

(defn- repository-stats!
  "What an origin holding this chain costs to keep and to hand out.

  Four numbers decide whether one release per upstream commit is deployable.
  Loose objects are what accumulates between maintenance runs. `pack_bytes`
  is what a fresh clone transfers, which is also what Software Heritage
  ingests. `repack_ms` is the maintenance window. `largest_object_bytes` is
  a hard limit rather than a cost: Software Heritage does not archive an
  object over 100 MB, and a manifest at corpus scale is already 10 MB.

  Measured on the bare origin rather than the clone, because the origin is
  the URL an archive is given."
  [origin branch chain-length works]
  (let [loose (count-objects origin)
        started (System/nanoTime)
        _ (git-out origin ["repack" "-ad"])
        repack-ms (/ (- (System/nanoTime) started) 1e6)
        packed (count-objects origin)
        manifests (manifest-blob-sizes origin branch)]
    {"phase" "repository"
     "chain_length" chain-length
     "works" works
     "loose_objects" (get loose "count")
     "loose_kib" (get loose "size")
     "repack_ms" (Math/round ^double repack-ms)
     "pack_kib" (get packed "size-pack")
     "manifests" (:count manifests)
     "manifest_bytes" (:bytes manifests)
     "manifest_disk_bytes" (:disk_bytes manifests)
     "largest_manifest_bytes" (:largest_bytes manifests)}))

(defn -main [& args]
  (try
    (let [{:keys [works releases changed out tmp carry segments repo-stats]}
          (cli/parse-opts args {:coerce {:works :long :releases :long
                                         :carry :boolean :repo-stats :boolean}})
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
                 ;; one walk, not the automatic run count: this column is
                 ;; the growth curve, and a policy that adds a run every 32
                 ;; releases would step it down at each multiple and make the
                 ;; slope unreadable. What a verifier actually pays is the
                 ;; `segmented` rows.
                 verify (:milliseconds (verify-once! clone {:segments 1}))
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
        ;; the built chain measured again at each requested run count, so the
        ;; split is compared against one walk over the same chain rather than
        ;; against a chain built separately. The newest commit of each run
        ;; re-reads and re-hashes every artifact of its manifest, because no
        ;; younger verified commit grants it reuse, so a count that leaves the
        ;; runs short gives back more than it wins. That is what these rows
        ;; show against `segments 1`, which is one walk.
        (when repo-stats
          (emit! (repository-stats! origin fx/branch releases works)))
        (doseq [k (segment-counts segments)]
          (emit! {"phase" "segmented"
                  "chain_length" releases
                  "works" works
                  "segments" k
                  "verify_ms" (Math/round ^double
                               (:milliseconds
                                (verify-once! clone {:segments k})))}))
        (finally (fs/delete-tree dir))))
    (finally (shutdown-agents))))
