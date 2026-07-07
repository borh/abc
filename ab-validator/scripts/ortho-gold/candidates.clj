#!/usr/bin/env bb
;; Orchestrator: walks the Aozora fiction selection and shells out to
;; bootstrap_label.py per work. Concatenates JSONL output.
(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[babashka.process :as p])

(def plain-dir
  (or (System/getenv "AOZORA_FICTION_PLAIN")
      "/home/bor/Projects/aozora-corpus-generator/Aozora-Bunko-Fiction-Selection-2022-05-30/Plain"))

(def script-dir
  (or (System/getenv "ORTHO_GOLD_SCRIPTS")
      "scripts/ortho-gold"))

(def out-path
  (or (first *command-line-args*) "data/ortho-gold/candidates.jsonl"))

(io/make-parents out-path)
(spit out-path "" :append false)

(doseq [^java.io.File f (filter #(.isFile %) (file-seq (io/file plain-dir)))
        :when (str/ends-with? (.getName f) ".txt")
        :let [work-id (str/replace-first (.getName f) #"\.txt$" "")]]
  (let [res (p/shell {:out :string :err :string}
                     "python" (str script-dir "/bootstrap_label.py")
                     "--work-id" work-id
                     "--text-file" (.getPath f))]
    (when-not (zero? (:exit res))
      (binding [*out* *err*]
        (println {:error work-id :stderr (:err res)})))
    (spit out-path (:out res) :append true)))

(println "wrote" out-path)
