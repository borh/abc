;; Operator-side one-off for the 2026-07-10 annotation-join-stats corpus run:
;; renders each sampled parser-IR to the plaintext view (the same
;; render-with-annotations text the join-stats CLI uses) so the tokenizer
;; wrapper can feed vibrato-tokenize. Disposable; recorded in the handoff.
(require '[abc.tools.files :as files]
         '[abc.tools.parser-ir-plaintext :as plaintext]
         '[clojure.java.io :as io])

(let [[stats-in-dir plaintext-dir] *command-line-args*
      dirs (->> (.listFiles (io/file stats-in-dir))
                (filter #(.isDirectory %))
                (filter #(.isFile (io/file % "parser-ir.json")))
                (sort-by #(.getName %)))]
  (.mkdirs (io/file plaintext-dir))
  (doseq [dir dirs]
    (let [work-id (.getName dir)
          parser-ir (files/read-json (io/file dir "parser-ir.json"))
          {:keys [text]} (plaintext/render-with-annotations parser-ir)]
      (spit (io/file plaintext-dir (str work-id ".txt")) text)))
  (println "rendered" (count dirs) "works"))
