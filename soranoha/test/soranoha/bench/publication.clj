(ns soranoha.bench.publication
  "Publication simulation over source history and explicitly recorded HTTP rounds.
  All mutable state and public fixture keys live beneath a fresh output directory."
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [charred.api :as json]
            [clojure.string :as string]
            [soranoha.bench.replay :as replay]
            [soranoha.core.hash :as hash]
            [soranoha.main :as main]
            [soranoha.snh.fixture :as fixture]
            [soranoha.snh.repo :as repo]
            [soranoha.snh.transact :as transact]
            [soranoha.za.oracle :as oracle])
  (:import [java.time LocalDate]))

(defn recorded-provider
  "Resolve URL-keyed responses from retained bodies. assert-complete! distinguishes
  missing experiment data from intentionally recorded HTTP failures, even when the
  acquisition layer catches the fetch exception and reports an unavailable work."
  [evidence-root responses]
  (when-not (map? responses)
    (throw (ex-info "Recorded round requires a response map" {})))
  (let [missing (atom #{})
        bodies (into {}
                     (map (fn [[url response]]
                            (let [digest (get response "sha256")
                                  status (get response "status")]
                              (when-not (and (string? url)
                                             (or (and (= #{"sha256"} (set (keys response)))
                                                      (string? digest) (re-matches #"[0-9a-f]{64}" digest))
                                                 (and (= #{"status"} (set (keys response)))
                                                      (integer? status) (<= 100 status 599) (not= 200 status))))
                                (throw (ex-info "Invalid recorded response" {:url url})))
                              [url (if digest
                                     (let [path (fs/path evidence-root digest)]
                                       (when (or (fs/sym-link? path)
                                                 (> (fs/size path) (* 32 1024 1024))
                                                 (not= digest (hash/sha256-file (str path))))
                                         (throw (ex-info "Invalid recorded body" {:url url})))
                                       {:path path :digest digest})
                                     {:status status})])))
                     responses)]
    {:fetch (fn [url]
              (if-let [{:keys [path digest status]} (get bodies url)]
                (if status
                  (throw (ex-info "Recorded HTTP failure"
                                  {:reason :aozora/http-status :status status}))
                  (try
                    (let [body (fs/read-all-bytes path)]
                      (when-not (= digest (hash/sha256-bytes body))
                        (throw (ex-info "Recorded body changed" {:url url})))
                      body)
                    (catch Exception e
                      (swap! missing conj url)
                      (throw e))))
                (do (swap! missing conj url)
                    (throw (ex-info "Missing recorded response" {:url url})))))
     :assert-complete! (fn []
                         (when (seq @missing)
                           (throw (ex-info "Missing or changed recorded responses"
                                           {:urls (vec (sort @missing))}))))}))

(defn- commit-review! [review]
  (replay/git! review "add" "source.json" "snapshot.json" "policy.edn")
  (replay/git! review "-c" "user.name=publication-replay" "-c"
               "user.email=publication-replay@localhost" "commit" "--allow-empty"
               "-qm" "Recorded publication simulation inputs"))

(defn- measured-release! [opts]
  (let [runs (fs/path (:root opts) "runs")
        before (set (fs/glob runs "*.json"))
        run (replay/measure #(main/release! opts))
        reports (mapv #(json/read-json (slurp (str %)))
                      (remove before (fs/glob runs "*.json")))]
    (assoc run :executed-stages (reduce + 0 (map #(get % "executed_stage_count") reports))
           :executions (frequencies
                        (for [report reports work (vals (get report "works"))
                              [stage cached?] (get work "cached") :when (false? cached?)]
                          stage)))))

(defn replay!
  "Run complete releases and unchanged repeats using one explicit round per source
  revision. Input policy and assessments are copied; origins and keys are fixture-owned.
  Timings describe simulation with supplied observations, not historical live state."
  [{:keys [recording assessment-source policy evidence-root assets-root clj-toolchain-id concurrency]
    :as input}]
  (doseq [option [:repo :from :to :out :recording :assessment-source :policy
                  :evidence-root :assets-root :clj-toolchain-id]]
    (when (string/blank? (str (get input option)))
      (throw (ex-info "Missing publication replay option" {:option option}))))
  (when (:limit input)
    (throw (ex-info "Publication replay covers the full selection" {:option :limit})))
  (when (and concurrency (not (and (integer? concurrency) (pos? concurrency))))
    (throw (ex-info "Replay concurrency must be positive" {:concurrency concurrency})))
  (let [recording (json/read-json (slurp recording))
        rounds (get recording "rounds")
        round-for (fn [commit] (get rounds (get-in recording ["revisions" commit])))
        commits (replay/revisions (:repo input) (:from input) (:to input))
        _ (doseq [commit commits]
            (let [round (round-for commit)]
              (when-not (map? round)
                (throw (ex-info "Missing recorded observation round" {:commit commit})))
              (LocalDate/parse (get round "as_of"))))
        {:keys [out checkout setup]} (replay/prepare! input)
        review (fs/create-dir (fs/path out "review"))
        _ (fs/copy assessment-source (fs/path review "source.json"))
        _ (fs/copy policy (fs/path review "policy.edn"))
        _ (replay/git! review "init" "-q")
        origin (repo/init-origin! (fs/path out "origin.git"))
        clone (repo/clone! origin (fs/path out "chain"))
        _ (transact/init-publication-branch! clone "main")
        serve-root (fs/path out "serve")
        _ (fs/create-dirs (fs/path serve-root "trees"))
        key-file! (fn [role field]
                    (let [path (str (fs/path out (str role "." field)))]
                      (spit path (str (get-in @fixture/keys* [role field]) "\n")) path))
        opts {:root (str (fs/path out "build")) :aozora-root (str checkout)
              :assessment-source (str (fs/path review "source.json"))
              :assessment (str (fs/path review "snapshot.json"))
              :policy (str (fs/path review "policy.edn"))
              :evidence-root evidence-root :assets-root assets-root
              :clj-toolchain-id clj-toolchain-id :concurrency (or concurrency 1)
              :chain-clone clone :branch "main" :serve-root (str serve-root)
              :upstream-origin "https://example.invalid/publication-replay/source.git"
              :release-key (key-file! "release" "seed")
              :release-pub (key-file! "release" "pub")
              :governance-pub (key-file! "governance" "pub")}
        emit! (fn [row]
                (let [line (str (json/write-json-str row) "\n")]
                  (spit (str (fs/path out "measurements.jsonl")) line :append true)
                  (print line) (flush)))]
    (emit! {:phase :setup :scope "publication simulation with recorded observations"
            :milliseconds (:milliseconds setup) :commits commits})
    (reduce
     (fn [previous commit]
       (let [round (round-for commit)
             observation-run (replay/measure #(recorded-provider evidence-root (get round "responses")))
             provider (:result observation-run)
             opts (assoc opts :as-of (get round "as_of") :aozora-fetch (:fetch provider))
             checkout-run (replay/measure #(replay/git! checkout "checkout" "--detach" commit))
             assessment-run (replay/measure
                             #(main/assessment-evaluate! (assoc opts :out (:assessment opts))))
             _ ((:assert-complete! provider))
             review-run (replay/measure #(commit-review! review))
             release-run (measured-release! opts)
             _ ((:assert-complete! provider))
             serving-run (replay/measure #(main/serving-activate! opts))
             repeat-run (measured-release! opts)
             _ ((:assert-complete! provider))
             repeat-serving (replay/measure #(main/serving-activate! opts))
             manifest (json/read-json (slurp (str (fs/path serve-root "current" "releases" "latest"))))]
         (when-not (and (= :already-published (get-in repeat-run [:result :outcome]))
                        (zero? (:executed-stages repeat-run))
                        (= (:head (:result serving-run)) (:head (:result repeat-serving)))
                        (= (:commit (:result serving-run)) (:commit (:result repeat-serving)))
                        (:reused? (:result repeat-serving)))
           (throw (ex-info "Unchanged publication was not an artifact-preserving no-op"
                           {:commit commit})))
         (emit! {:phase :publication :commit commit :round (get-in recording ["revisions" commit]) :as-of (:as-of opts)
                 :checkout-ms (:milliseconds checkout-run)
                 :observation-ms (:milliseconds observation-run)
                 :review-ms (:milliseconds review-run)
                 :assessment-ms (:milliseconds assessment-run)
                 :release-ms (:milliseconds release-run)
                 :serving-ms (:milliseconds serving-run)
                 :repeat-release-ms (:milliseconds repeat-run)
                 :repeat-serving-ms (:milliseconds repeat-serving)
                 :executed-stages (:executed-stages release-run)
                 :executions (:executions release-run)
                 :works-delta (update-vals (oracle/works-delta previous manifest) count)
                 :result (:result release-run) :served (:result serving-run)})
         manifest))
     {"works" []} commits)
    {:out (str out) :revisions (count commits)}))

(defn -main [& args]
  (try
    (replay! (cli/parse-opts args {:coerce {:concurrency :long :limit :long}}))
    (finally (shutdown-agents))))
