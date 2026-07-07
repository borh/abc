(ns abc.tools.soranoha
  (:refer-clojure :exclude [run!])
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.files :as files]
            [abc.tools.snapshot-index :as snapshot-index]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def request-sets-dir "data/request-sets")

(defn- json-file? [file]
  (and (.isFile file)
       (string/ends-with? (.getName file) ".json")))

(defn request-set-labels []
  (let [dir (io/file request-sets-dir)]
    (->> (file-seq dir)
         (filter json-file?)
         (map #(.getName %))
         (map #(subs % 0 (- (count %) (count ".json"))))
         sort
         vec)))

(defn request-set-path [label]
  (str request-sets-dir "/" label ".json"))

(defn read-request-set [label]
  (let [path (request-set-path label)
        file (io/file path)]
    (when-not (.isFile file)
      (throw (ex-info "Unknown request set"
                      {:label label
                       :path path})))
    (files/read-json path)))

(defn list-request-sets! []
  (doseq [label (request-set-labels)]
    (println label))
  0)

(defn explain-request-set! [label]
  (let [request-set (read-request-set label)
        computed-id (analysis-identity/request-set-id request-set)]
    (println "label:" (get request-set "label"))
    (println "request_set_id:" (get request-set "request_set_id"))
    (println "computed_request_set_id:" computed-id)
    (println "fixture_role:" (get request-set "fixture_role"))
    (if (= computed-id (get request-set "request_set_id"))
      0
      (do
        (binding [*out* *err*]
          (println "request_set_id does not match request_set_identity_object"))
        1))))

(defn snapshot-index! []
  (let [fixture (files/read-json "examples/v0/snapshot/snapshot-index.json")]
    (snapshot-index/validate-snapshot-index! fixture)
    (println "snapshot_label:" (get fixture "snapshot_label"))
    (println "snapshot_identity_hash:" (get fixture "snapshot_identity_hash"))
    (println "request_set_label:" (get fixture "request_set_label"))
    0))

(defn usage []
  (string/join
   "\n"
   ["usage: soranoha <command> [args]"
    ""
    "commands:"
    "  list-request-sets"
    "  explain-request-set <label>"
    "  snapshot-index"]))

(def commands
  {"list-request-sets" {:args 0
                        :run (fn [] (list-request-sets!))}
   "explain-request-set" {:args 1
                          :run explain-request-set!}
   "snapshot-index" {:args 0
                     :run (fn [] (snapshot-index!))}})

(defn run! [args]
  (let [[command & rest-args] args]
    (cond
      (or (nil? command) (= command "help") (= command "--help"))
      (do (println (usage)) 0)

      (not (contains? commands command))
      (do (binding [*out* *err*]
            (println "unknown command:" command)
            (println (usage)))
          2)

      (not= (:args (get commands command)) (count rest-args))
      (do (binding [*out* *err*]
            (println "wrong arity for command:" command)
            (println (usage)))
          2)

      :else
      (try
        (apply (:run (get commands command)) rest-args)
        (catch clojure.lang.ExceptionInfo e
          (binding [*out* *err*]
            (println (ex-message e)))
          1)))))

(defn -main [& args]
  (System/exit (run! args)))
