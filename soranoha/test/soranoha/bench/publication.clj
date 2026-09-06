(ns soranoha.bench.publication
  "Publication simulation over source history and explicitly recorded HTTP rounds.
  All mutable state and public fixture keys live beneath a fresh output directory."
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [babashka.process :as process]
            [soranoha.assessment.source :as source]
            [soranoha.assessment.aozora :as aozora]
            [soranoha.assessment.evaluate :as evaluation]
            [soranoha.snh.verify :as verify]
            [charred.api :as json]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [soranoha.bench.replay :as replay]
            [soranoha.core.hash :as hash]
            [soranoha.kura.engine :as engine]
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

(defn- phase-times [f]
  (let [totals (atom {})
        executions (atom {})
        run-stage engine/run-stage!
        phases {#'main/release-preflight! :preflight-ms
                #'main/execute-build! :build-ms
                #'main/evaluate-assessment! :assessment-ms
                #'source/capture-checkout :capture-ms
                #'aozora/check! :aozora-ms
                #'evaluation/evaluate! :evaluate-ms
                #'verify/verify-repository-at :verify-ms}
        wrappers (into {} (map (fn [[v phase]]
                                 [v (let [original @v]
                                      (fn [& args]
                                        (let [start (System/nanoTime)]
                                          (try (apply original args)
                                               (finally
                                                 (swap! totals update phase (fnil + 0)
                                                        (/ (- (System/nanoTime) start) 1e6)))))))])) phases)
        result (with-redefs-fn
                 (assoc wrappers #'engine/run-stage!
                        (fn [store stage inputs]
                          (let [result (run-stage store stage inputs)]
                            (when-not (:cached? result)
                              (swap! executions update (:stage-id stage) (fnil inc 0)))
                            result))) f)]
    {:result result :phases @totals :executions @executions}))

(defn- measured-release! [opts]
  (let [timed (phase-times (fn [] (replay/measure #(main/release! opts))))]
    (assoc (:result timed) :phases (:phases timed)
           :executed-stages (reduce + 0 (vals (:executions timed)))
           :executions (:executions timed))))

(defn- fixture-options [out {:keys [evidence-root assets-root clj-toolchain-id concurrency]}]
  {:root (str (fs/path out "build")) :aozora-root (str (fs/path out "checkout"))
   :assessment-source (str (fs/path out "review" "source.json"))
   :assessment (str (fs/path out "review" "snapshot.json"))
   :policy (str (fs/path out "review" "policy.edn"))
   :evidence-root evidence-root :assets-root assets-root
   :clj-toolchain-id clj-toolchain-id :concurrency (or concurrency 1)
   :chain-clone (str (fs/path out "chain")) :branch "main" :serve-root (str (fs/path out "serve"))
   :upstream-origin "https://example.invalid/publication-replay/source.git"
   :release-key (str (fs/path out "release.seed"))
   :release-pub (str (fs/path out "release.pub"))
   :governance-pub (str (fs/path out "governance.pub"))})

(defn replay!
  "Run complete releases and unchanged repeats using one explicit round per source
  revision. Input policy and assessments are copied; origins and keys are fixture-owned.
  Timings describe simulation with supplied observations, not historical live state."
  [{:keys [recording assessment-source policy evidence-root concurrency]
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
        _ (doseq [[role field] [["release" "seed"] ["release" "pub"] ["governance" "pub"]]]
            (key-file! role field))
        opts (fixture-options out input)
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
             assessment-run (phase-times
                             (fn [] (:milliseconds
                                     (replay/measure
                                      #(main/assessment-evaluate! (assoc opts :out (:assessment opts)))))))
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
                 :assessment-ms (:result assessment-run)
                 :assessment-executions (:executions assessment-run)
                 :release-phases (:phases release-run)
                 :repeat-phases (:phases repeat-run)
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

(defn repeat!
  "Measure an unchanged release in a completed replay's owned fixture. Phase times
  are inclusive and may nest; this times calls, never profiles a signing JVM."
  [{:keys [repeat-run recording evidence-root] :as input}]
  (let [out (fs/real-path repeat-run)
        opts (fixture-options out input)
        setup (with-open [reader (io/reader (str (fs/path out "measurements.jsonl")))]
                (json/read-json (.readLine ^java.io.BufferedReader reader)))]
    (when-not (= "publication simulation with recorded observations" (get setup "scope"))
      (throw (ex-info "Repeat requires a publication replay fixture" {})))
    (doseq [child ["build" "checkout" "review" "chain" "origin.git" "serve"]]
      (when-not (fs/starts-with? (fs/real-path (fs/path out child)) out)
        (throw (ex-info "Replay state escapes its output directory" {:child child}))))
    (when-not (= (str (fs/path out "origin.git"))
                 (replay/git! (:chain-clone opts) "remote" "get-url" "origin"))
      (throw (ex-info "Repeat requires the replay's local origin" {})))
    (doseq [role ["release" "governance"]]
      (when-not (= (get-in @fixture/keys* [role "pub"])
                   (string/trim (slurp (str (fs/path out (str role ".pub"))))))
        (throw (ex-info "Repeat requires public fixture keys" {:role role}))))
    (let [commit (main/source-provenance! (:aozora-root opts))
          recording (json/read-json (slurp recording))
          round (get-in recording ["rounds" (get-in recording ["revisions" commit])])
          provider (recorded-provider evidence-root (get round "responses"))
          opts (assoc opts :as-of (get round "as_of") :aozora-fetch (:fetch provider))
          before (replay/git! (fs/path out "origin.git") "rev-parse" "refs/heads/main")
          run (measured-release! opts)
          _ ((:assert-complete! provider))
          served (replay/measure #(main/serving-activate! opts))
          result {:release (:result run) :served (:result served)}]
      (when-not (and (= :already-published (get-in run [:result :outcome]))
                     (zero? (:executed-stages run))
                     (= before (replay/git! (fs/path out "origin.git") "rev-parse" "refs/heads/main"))
                     (= before (get-in served [:result :commit]))
                     (get-in served [:result :reused?]))
        (throw (ex-info "Repeated publication changed the fixture" {:result result})))
      (let [row {:release-ms (:milliseconds run) :phases (:phases run)
                 :serving-ms (:milliseconds served) :executed-stages (:executed-stages run)
                 :result result}]
        (println (json/write-json-str row))
        row))))

(defn compare!
  "Compare installed replay programs in ABBA/BAAB order on one completed fixture.
  Each child has a fresh JVM. Results and GNU time measurements remain under --out."
  [{:keys [baseline candidate time-bin out repeat-run recording evidence-root concurrency]}]
  (fs/create-dir out)
  (let [rows
        (mapv
         (fn [index variant]
           (let [program (if (= variant "A") baseline candidate)
                 timing (str (fs/path out (str index ".time")))
                 child (process/sh time-bin "--output" timing "--format" "%e %U %S %M"
                                   program "--repeat-run" repeat-run "--recording" recording
                                   "--evidence-root" evidence-root "--concurrency" (str (or concurrency 1)))]
             (spit (str (fs/path out (str index ".stdout"))) (:out child))
             (spit (str (fs/path out (str index ".stderr"))) (:err child))
             (when-not (zero? (:exit child))
               (throw (ex-info "Publication comparison failed" {:index index :exit (:exit child)})))
             (let [[elapsed user system rss] (mapv parse-double
                                                   (string/split (string/trim (slurp timing)) #"\s+"))
                   row (assoc (json/read-json (last (string/split-lines (:out child))))
                              "index" index "variant" variant "program" program
                              "elapsed-seconds" elapsed "user-seconds" user "system-seconds" system
                              "peak-rss-kib" rss)]
               (spit (str (fs/path out "measurements.jsonl"))
                     (str (json/write-json-str row) "\n") :append true)
               (println (json/write-json-str row)) (flush)
               row)))
         (range) ["A" "B" "B" "A" "B" "A" "A" "B"])]
    (when-not (apply = (map #(get % "result") rows))
      (throw (ex-info "Publication variants produced different results" {:out out})))
    rows))

(defn -main [& args]
  (try
    (let [opts (cli/parse-opts args {:coerce {:concurrency :long :limit :long}})]
      (cond (:baseline opts) (compare! opts)
            (:repeat-run opts) (repeat! opts)
            :else (replay! opts)))
    (finally (shutdown-agents))))
