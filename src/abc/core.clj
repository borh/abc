(ns abc.core
  (:require [abc
             [aozora :as aozora]
             [tei :as tei]
             [load :as load]
             [db :as db]
             [git :as git]
             [config :as config]
             [owl :as owl]]
            [clojure.string :as string]
            [me.raynes.fs :as fs]
            [clojure.tools.cli :refer [parse-opts]]))

(def cli-options
  [["-i" "--input AOZORA_BUNKO_GIT_REPO" "Path to locally cloned https://github.com/aozorabunko/aozorabunko repo"
    :default "../aozorabunko"]
   ["-o" "--output OUTPUT_DIR" "Path to repository to commit data to"
    :default "../aozora-bunko-tei"]
   ["-f" "--format RDF_FORMAT" "Specify output format for RDF metadata (default is turtle pretty)"]
   ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["Natsume Server"
        ""
        "Usage: clojure -m abc.core [options]"
        ""
        "Options:"
        options-summary]
       (string/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn validate-args
  "Validate command line arguments. Either return a map indicating the program
  should exit (with a error message, and optional ok status), or a map
  indicating the action the program should take and the options provided."
  [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options) ; help => exit OK with usage summary
      {:exit-message (usage summary) :ok? true}

      errors ; errors => exit with description of errors
      {:exit-message (error-msg errors)}

      :else {:options options})))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn run [{:keys [input output]}]
  (println "Running...")
  (when-not (fs/directory? output)
    (fs/mkdir output))
  (let [ttl-file-path (fs/file output "aozora-bunko.ttl")]
    (db/save-graph! ttl-file-path
                    (->> input
                         load/aozora-bunko-db
                         load/aozora-bunko-db-coll
                         db/to-triples
                         db/to-graph))))

(defn -main [& args]
  (let [{:keys [options exit-message ok?]} (validate-args args)]
    (println options)
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (run options))))
