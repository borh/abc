(ns soranoha.bench.replay
  "Replay a first-parent source range through the real build and delta oracle.
  Owns a fresh checkout and cache beneath --out; the supplied repository is
  read-only. Timings cover checkout and build, excluding live assessment,
  signing and serving. Each revision is repeated to verify the no-op path."
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [babashka.process :as process]
            [charred.api :as wire-json]
            [clojure.string :as string]
            [soranoha.core.json :as json]
            [soranoha.main :as main]
            [soranoha.za.oracle :as oracle]))

(defn git! [repo & args]
  (let [{:keys [exit out err]} (apply process/sh {:dir (str repo)} "git" args)]
    (when-not (zero? exit)
      (throw (ex-info "Replay Git operation failed" {:arguments args :diagnostic err})))
    (string/trim out)))

(defn revisions
  "Resolve an inclusive, oldest-first range on the end revision's first-parent path.

  `endpoints-only` keeps just the two ends of that range. The range is still
  resolved and still has to be a real first-parent range, so the answer remains
  about upstream history rather than about two unrelated commits; what is
  dropped is every build in between. That is what makes a comparison across an
  arbitrary distance affordable, and it is the difference between measuring how
  the build behaves over history and asking what two revisions differ by."
  [repo from to & [endpoints-only]]
  (let [resolve! #(git! repo "rev-parse" "--verify" "--end-of-options" (str % "^{commit}"))
        first-revision (resolve! from)
        last-revision (resolve! to)
        commits (->> (git! repo "rev-list" "--first-parent" "--reverse" last-revision)
                     string/split-lines
                     (drop-while #(not= first-revision %))
                     vec)]
    (when (empty? commits)
      (throw (ex-info "Start revision is not on the end revision's first-parent path"
                      {:from first-revision :to last-revision})))
    (if endpoints-only
      (into [(first commits)] (when (< 1 (count commits)) [(peek commits)]))
      commits)))

(defn measure [f]
  (let [start (System/nanoTime)
        result (f)]
    {:milliseconds (/ (- (System/nanoTime) start) 1e6) :result result}))

(defn- decoded [report]
  (oracle/decode-run (.getBytes ^String (json/write-deterministic-json-str report) "UTF-8")))

(defn- delta [before after]
  (when before
    (let [violations (oracle/unexplained-executions before after)]
      (when (seq violations)
        (throw (ex-info "Replay executed unchanged derivations" {:violations violations})))
      {:source (update-vals (oracle/source-delta before after) count)
       :artifacts (update-vals (oracle/report-artifact-delta before after) count)})))

(defn prepare!
  "Create an owned source checkout beneath a fresh output directory."
  [{:keys [repo from to out endpoints-only]}]
  (let [repo (fs/real-path repo)
        commits (revisions repo from to endpoints-only)
        out (fs/absolutize out)
        out (fs/path (fs/real-path (fs/parent out)) (fs/file-name out))
        _ (when (fs/starts-with? out repo)
            (throw (ex-info "Replay output must be outside the source repository" {:out (str out)})))
        checkout (fs/path out "checkout")
        setup (measure #(do (fs/create-dir out)
                            (git! out "clone" "--no-hardlinks" "--no-checkout"
                                  (str repo) (str checkout))))]
    {:out out :checkout checkout :commits commits :setup setup}))

(defn replay!
  "Retain reports and a streaming measurements.jsonl in a new output directory.
  No state outside that directory is changed; failures retain diagnostic outputs."
  [{:keys [repo from to out concurrency limit clj-toolchain-id assets-root
           endpoints-only]}]
  (doseq [[option value] {:repo repo :from from :to to :out out
                          :clj-toolchain-id clj-toolchain-id :assets-root assets-root}]
    (when (or (nil? value) (string/blank? (str value)))
      (throw (ex-info "Missing replay option" {:option option}))))
  (doseq [[option value] {:concurrency concurrency :limit limit}
          :when (some? value)]
    (when-not (and (integer? value) (pos? value))
      (throw (ex-info "Replay count must be positive" {:option option :value value}))))
  (let [{:keys [out checkout commits setup]} (prepare! {:repo repo :from from :to to :out out
                                                        :endpoints-only endpoints-only})
        opts (cond-> {:root (str (fs/path out "build")) :aozora-root (str checkout)
                      :concurrency (or concurrency 1) :clj-toolchain-id clj-toolchain-id
                      :assets-root assets-root}
               limit (assoc :limit limit))
        emit! (fn [row]
                (let [line (str (wire-json/write-json-str row) "\n")]
                  (spit (str (fs/path out "measurements.jsonl")) line :append true)
                  (print line)
                  (flush)))]
    (emit! {:phase "setup" :milliseconds (:milliseconds setup)
            :commits commits :endpoints-only (boolean endpoints-only)
            :limit limit :concurrency (:concurrency opts)
            :clj-toolchain-id clj-toolchain-id
            :scope "build-only; no live assessment, signing or serving"})
    (reduce
     (fn [previous commit]
       (let [checkout-run (measure #(git! checkout "checkout" "--detach" commit))
             run (measure #(main/build! opts))
             report (:result run)
             current (decoded report)
             changes (delta previous current)
             repeat-run (measure #(main/build! opts))
             repeated (decoded (:result repeat-run))
             repeat-changes (delta current repeated)
             counts (frequencies (mapcat val (oracle/executed-stages current)))]
         (when-not (and (zero? (get (:result repeat-run) "executed_stage_count"))
                        (every? zero? (vals (:source repeat-changes)))
                        (every? zero? (vals (dissoc (:artifacts repeat-changes) :retained))))
           (throw (ex-info "Unchanged revision did not produce an artifact-preserving no-op"
                           {:commit commit :delta repeat-changes})))
         (emit! {:phase (if previous "incremental" "cold-build") :commit commit
                 :checkout-ms (:milliseconds checkout-run) :build-ms (:milliseconds run)
                 :noop-ms (:milliseconds repeat-run) :executions counts
                 :selected (count (get report "selected_slugs")) :delta changes})
         current))
     nil commits)
    {:out (str out) :revisions (count commits)}))

(defn -main [& args]
  (try
    (let [opts (cli/parse-opts args {:coerce {:concurrency :long :limit :long
                                              :endpoints-only :boolean}})]
      (if (:help opts)
        (println "soranoha-replay --repo PATH --from REV --to REV --out NEW-DIRECTORY [--endpoints-only] [--concurrency N] [--limit N]")
        (replay! opts)))
    (finally (shutdown-agents))))
