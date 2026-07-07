(ns abc.tools.soranoha
  (:refer-clojure :exclude [run!])
  (:require [abc.tools.files :as files]
            [abc.tools.request-set-resolver :as request-set-resolver]
            [abc.tools.snapshot-index :as snapshot-index]
            [clojure.string :as string]))

(defn request-set-labels []
  (request-set-resolver/request-set-labels))

(defn read-request-set [label]
  (request-set-resolver/resolve-request-set label))

(defn list-request-sets! []
  (doseq [label (request-set-labels)]
    (println label))
  0)

(defn explain-request-set! [label]
  (let [request-set (read-request-set label)
        computed-id (get request-set "request_set_id")]
    (println "label:" (get request-set "label"))
    (println "request_set_id:" (get request-set "request_set_id"))
    (println "computed_request_set_id:" computed-id)
    (println "source_definition_path:" (get-in request-set ["resolution"
                                                            "source_definition_path"]))
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
