(ns soranoha.bench.serving
  "Balanced comparisons of installed serving activators on an isolated export.
  Each child gets its own JVM; GNU time records elapsed time and peak RSS."
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [babashka.process :as process]
            [charred.api :as json]
            [clojure.string :as string]))

(defn compare!
  "Run ABBA/BAAB and retain each result, timing and stderr under a new --out.
  Use an already-exported isolated chain/tree so both variants measure reuse."
  [{:keys [baseline candidate time-bin out chain-clone serve-root release-pub governance-pub]}]
  (doseq [[option value] {:baseline baseline :candidate candidate :time-bin time-bin :out out
                          :chain-clone chain-clone :serve-root serve-root
                          :release-pub release-pub :governance-pub governance-pub}]
    (when (string/blank? value)
      (throw (ex-info "Missing comparison option" {:option option}))))
  (fs/create-dir out)
  (let [rows
        (mapv
         (fn [index variant]
           (let [program (if (= variant "A") baseline candidate)
                 timing (str (fs/path out (str index ".time")))
                 start (System/nanoTime)
                 child (process/sh time-bin "--output" timing "--format" "%e %U %S %M"
                                   program "serving-activate" "--chain-clone" chain-clone
                                   "--serve-root" serve-root "--branch" "main"
                                   "--release-pub" release-pub "--governance-pub" governance-pub)
                 wall-ms (/ (- (System/nanoTime) start) 1e6)]
             (spit (str (fs/path out (str index ".stderr"))) (:err child))
             (spit (str (fs/path out (str index ".json"))) (:out child))
             (when-not (zero? (:exit child))
               (throw (ex-info "Serving comparison child failed"
                               {:index index :variant variant :exit (:exit child)})))
             (let [[elapsed user system rss] (mapv parse-double
                                                   (string/split (string/trim (slurp timing)) #"\s+"))
                   result (json/read-json (:out child))
                   row {:index index :variant variant :program program :wall-ms wall-ms
                        :elapsed-seconds elapsed :user-seconds user :system-seconds system
                        :peak-rss-kib rss :result result}]
               (when-not (true? (get result "reused"))
                 (throw (ex-info "Comparison requires an existing export" {:result result})))
               (spit (str (fs/path out "measurements.jsonl"))
                     (str (json/write-json-str row) "\n") :append true)
               (println (json/write-json-str row))
               (flush)
               row)))
         (range) ["A" "B" "B" "A" "B" "A" "A" "B"])]
    (when-not (apply = (map :result rows))
      (throw (ex-info "Serving variants produced different results" {:out out})))
    rows))

(defn -main [& args]
  (try
    (compare! (cli/parse-opts args))
    (finally (shutdown-agents))))
