(ns soranoha.snh.chain-bench
  "Full-chain verification wall time as a function of chain length.

  The publication rearchitecture ledger measured verification cost growing with
  releases times works and made correcting it a precondition for unattended
  activation. Batching has landed since. This measures whether the growth term
  is still there, in isolation from the build: releases are synthetic, so no
  corpus is needed and nothing but the verifier is being timed.

  Two shapes bracket the real one. `--changed 0` moves no work between releases,
  which is what the reuse grant is for and what an upstream commit touching a
  handful of works approximates at corpus scale. `--changed all` moves every
  work, which is what a toolchain change does. A real per-commit chain sits at
  the first, because a commit changes a median of two works out of 17810.

  Run from soranoha/:
    clojure -Sdeps '{:paths [\"src\" \"test\" \"resources\"]}' -M \\
      -m soranoha.snh.chain-bench \\
      --works 2000 --releases 24 --changed 3

  Results are JSON lines on stdout. `verify_ms_marginal` is the cost of the one
  additional manifest, which is the quantity that decides whether the chain can
  grow. A single marginal is noisy enough to come out negative; read the slope
  across the whole run rather than any one row.

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
    (let [{:keys [works releases changed out]}
          (cli/parse-opts args {:coerce {:works :long :releases :long}})
          ;; babashka.cli parses a bare number itself, so `changed` arrives as a
          ;; long already; "all" is the only value that stays a string
          changed (cond (= "all" changed) :all
                        (integer? changed) changed
                        :else (parse-long (or changed "3")))
          works (or works 500)
          releases (or releases 12)
          all (slugs works)
          dir (fs/create-temp-dir {:prefix "snh-chain-bench"})
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
         (fn [previous release]
           (let [publish
                 (millis
                  #(fx/publish! clone
                                {:admitted all
                                 :variant (variant-fn changed release all)
                                 ;; the projection must move or an unchanged
                                 ;; build is a no-op and publishes nothing
                                 :selection-params {"config" "bench"
                                                    "round" (str release)}}))
                 verify (:milliseconds (verify-once! clone))]
             (emit! (cond-> {"phase" "release"
                             "chain_length" (inc release)
                             "works" works
                             "publish_ms" (Math/round ^double (:milliseconds publish))
                             "verify_ms" (Math/round ^double verify)}
                      previous
                      (assoc "verify_ms_marginal"
                             (Math/round ^double (- verify ^double previous)))))
             verify))
         nil
         (range releases))
        (finally (fs/delete-tree dir))))
    (finally (shutdown-agents))))
