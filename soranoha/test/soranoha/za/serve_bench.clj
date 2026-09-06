(ns soranoha.za.serve-bench
  "Seed-free serving read/write benchmark against one published commit.
  Run with src and test on the classpath: -m soranoha.za.serve-bench
  baseline|compare CLONE COMMIT MANIFEST FILE-COUNT PAIRS.
  Reads never fetch or update refs; outputs use disposable directories.
  Each measured pass is checked against the manifest's exact hashes and sizes."
  (:require [babashka.fs :as fs]
            [charred.api :as json]
            [soranoha.core.hash :as hash]
            [soranoha.snh.verify :as verify]
            [soranoha.snh.view :as view]))

(defn- measure [v commit artifacts batched?]
  (let [dir (fs/create-temp-dir {:prefix "serving-read-benchmark"})
        reading (atom 0.0)
        writing (atom 0.0)
        read-write! (fn [reader]
                      (doseq [{:keys [path]} artifacts]
                        (let [start (System/nanoTime)
                              bytes (or (view/read-at reader commit path)
                                        (throw (ex-info "published path missing" {:path path})))
                              read-end (System/nanoTime)
                              target (fs/path dir path)]
                          (fs/create-dirs (fs/parent target))
                          (fs/write-bytes target bytes)
                          (swap! reading + (/ (- read-end start) 1e6))
                          (swap! writing + (/ (- (System/nanoTime) read-end) 1e6)))))]
    (try
      (let [start (System/nanoTime)
            _ (if batched? (view/with-batch v read-write!) (read-write! v))
            elapsed (/ (- (System/nanoTime) start) 1e6)
            tree (into (sorted-map)
                       (for [{:keys [path expected-hash expected-bytes]} artifacts
                             :let [bytes (fs/read-all-bytes (fs/path dir path))
                                   digest (hash/sha256-bytes bytes)]]
                         (do (assert (= expected-hash digest) (str "wrong blob bytes: " path))
                             (assert (= expected-bytes (alength bytes)) (str "wrong blob length: " path))
                             [path digest])))]
        {:milliseconds elapsed :read-ms @reading :write-ms @writing
         :tree (hash/sha256-bytes (.getBytes (pr-str tree) "UTF-8"))})
      (finally (fs/delete-tree dir)))))

(defn -main [mode clone commit manifest-path file-count pair-count]
  (let [manifest (json/read-json (slurp manifest-path))
        works (get manifest "works")
        n (parse-long file-count)
        work-count (long (Math/ceil (/ n 3.0)))
        artifacts (->> (range work-count)
                       (mapcat #(get (nth works (quot (* % (count works)) work-count)) "artifacts"))
                       (take n)
                       (mapv (fn [artifact]
                               (let [hex (verify/id->hex (get artifact "id"))]
                                 {:path (verify/blob-path hex) :expected-hash hex
                                  :expected-bytes (get artifact "bytes")}))))
        v (view/git-view clone)]
    (assert (contains? #{"baseline" "compare"} mode))
    (assert (= n (count artifacts) (count (distinct (map :path artifacts)))))
    (println (json/write-json-str {:files n :bytes (reduce + (map :expected-bytes artifacts))
                                   :commit commit :mode mode}))
    (when (= mode "compare")
      (measure v commit (take 20 artifacts) false)
      (measure v commit (take 20 artifacts) true))
    (dotimes [i (parse-long pair-count)]
      (let [order (if (= mode "baseline") [:spawned]
                      (if (even? i) [:spawned :batch] [:batch :spawned]))
            results (into {} (for [kind order]
                               [kind (measure v commit artifacts (= kind :batch))]))]
        (when (= mode "compare")
          (assert (= (get-in results [:spawned :tree]) (get-in results [:batch :tree]))))
        (println (json/write-json-str {:pair i :order order :results results}))
        (flush)))))
