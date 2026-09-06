(ns soranoha.snh.repo-bench
  "Paired commit-writer benchmark over a directory of actual artifact bytes.
  Run from soranoha/: clojure -Sdeps '{:paths [\"src\" \"test\"]}' -M
  -m soranoha.snh.repo-bench baseline|compare ARTIFACT-DIR FILE-COUNT PAIRS.
  Each write uses a fresh disposable Git repository; no signing seed or live ref
  is read. The reference retains the original per-file index algorithm solely
  to measure the effect of batching. Results are JSON on stdout."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [charred.api :as json]
            [clojure.string :as str]
            [soranoha.snh.repo :as repo]))

(defn- reference-write-commit! [dir files]
  (let [index (str (fs/create-temp-file {:prefix "writer-reference-index"}))]
    (try
      (fs/delete-if-exists index)
      (let [env {"GIT_INDEX_FILE" index}]
        (doseq [[path bytes] (sort-by key files)]
          (let [blob (#'repo/hash-blob! dir bytes)]
            (#'repo/git! dir {:extra-env env} "update-index" "--add"
                         "--cacheinfo" (str "100644," blob "," path))))
        (let [tree (str/trim (:out (#'repo/git! dir {:extra-env env} "write-tree")))]
          (str/trim (:out (#'repo/git! dir {:extra-env (merge env @#'repo/ident-env)}
                                       "commit-tree" tree "-m" "writer benchmark")))))
      (finally (fs/delete-if-exists index)))))

(defn- measure [files writer]
  (let [dir (fs/create-temp-dir {:prefix "writer-benchmark"})]
    (try
      (process/sh {:dir (str dir)} "git" "init" "-q")
      (let [calls (atom {})
            sh process/sh
            start (System/nanoTime)
            commit (with-redefs [process/sh
                                 (fn [opts executable command & args]
                                   (let [before (System/nanoTime)
                                         result (apply sh opts executable command args)]
                                     (swap! calls update command
                                            (fn [old] {:count (inc (:count old 0))
                                                       :milliseconds (+ (:milliseconds old 0.0)
                                                                        (/ (- (System/nanoTime) before) 1e6))}))
                                     result))]
                     (writer dir files))
            elapsed (/ (- (System/nanoTime) start) 1e6)
            tree (str/trim (:out (process/sh {:dir (str dir) :out :string}
                                             "git" "rev-parse" (str commit "^{tree}"))))]
        {:milliseconds elapsed :tree tree :commands @calls})
      (finally (fs/delete-tree dir)))))

(defn -main [mode artifact-dir file-count pair-count]
  (let [n (parse-long file-count)
        pairs (parse-long pair-count)
        paths (->> (fs/glob artifact-dir "**" {:hidden true})
                   (filter fs/regular-file?)
                   (sort-by str)
                   (take n))
        files (into {} (map (fn [path] [(str (fs/relativize artifact-dir path))
                                        (java.nio.file.Files/readAllBytes path)]) paths))
        current (fn [dir inputs] (repo/write-commit! dir {:parents [] :files inputs :message "writer benchmark"}))]
    (assert (= n (count files)) "Artifact directory contains fewer files than requested")
    (assert (contains? #{"baseline" "compare"} mode))
    (println (json/write-json-str {:files n :bytes (reduce + (map alength (vals files)))
                                   :mode mode :git (str/trim (:out (process/sh {:out :string} "git" "--version")))}))
    (when (= mode "compare")
      (let [warmup (into {} (take 100 files))]
        (measure warmup reference-write-commit!)
        (measure warmup current)))
    (dotimes [i pairs]
      (let [order (if (= mode "baseline") [:current]
                      (if (even? i) [:reference :current] [:current :reference]))
            results (into {} (map (fn [kind] [kind (measure files (if (= kind :reference) reference-write-commit! current))]) order))]
        (when (= mode "compare")
          (assert (= (get-in results [:reference :tree]) (get-in results [:current :tree])) "Tree identity differs"))
        (println (json/write-json-str {:pair i :order order :results results}))
        (flush)))))
