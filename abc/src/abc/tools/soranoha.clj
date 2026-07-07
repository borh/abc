(ns abc.tools.soranoha
  (:refer-clojure :exclude [run!])
  (:require [abc.tools.files :as files]
            [abc.tools.request-set-resolver :as request-set-resolver]
            [abc.tools.schema :as schema]
            [abc.tools.snapshot-index :as snapshot-index]
            [clojure.java.io :as io]
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

(defn build-snapshot-index [label]
  (let [request-set (read-request-set label)
        plan (snapshot-index/read-snapshot-plan label)]
    (snapshot-index/build-snapshot-index-from-plan request-set plan)))

(defn snapshot-index! [label output-path]
  (let [snapshot (build-snapshot-index label)
        output-file (snapshot-index/write-snapshot-index! snapshot output-path)]
    (println "snapshot_index:" (str output-file))
    (println "snapshot_label:" (get snapshot "snapshot_label"))
    (println "snapshot_identity_hash:" (get snapshot "snapshot_identity_hash"))
    (println "request_set_label:" (get snapshot "request_set_label"))
    0))

(defn default-snapshot-root [label]
  (io/file "target" "soranoha" label))

(defn reproduce! [label]
  (snapshot-index! label
                   (str (io/file (default-snapshot-root label)
                                 "snapshot-index.json"))))

(defn snapshot-index-path [path]
  (let [file (io/file path)]
    (if (.isDirectory file)
      (io/file file "snapshot-index.json")
      file)))

(defn read-valid-snapshot-index [path]
  (let [snapshot-path (snapshot-index-path path)
        snapshot (files/read-json snapshot-path)
        snapshot-schema (files/read-json "schemas/snapshot-index.schema.json")]
    (when-let [errors (schema/validation-errors snapshot-schema snapshot)]
      (throw (ex-info "Snapshot index schema validation failed"
                      {:path (str snapshot-path)
                       :errors errors})))
    (snapshot-index/validate-snapshot-index! snapshot)
    snapshot))

(defn explain-snapshot! [path]
  (let [snapshot (read-valid-snapshot-index path)
        summary (get snapshot "summary")]
    (println "snapshot_label:" (get snapshot "snapshot_label"))
    (println "snapshot_identity_hash:" (get snapshot "snapshot_identity_hash"))
    (println "request_set_label:" (get snapshot "request_set_label"))
    (println "request_set_id:" (get-in snapshot ["snapshot_index_identity_object"
                                                 "request_set_id"]))
    (println "total_artifacts:" (get summary "total_artifacts"))
    (println "success_count:" (get summary "success_count"))
    (println "failure_count:" (get summary "failure_count"))
    (println "failure_rate:" (get summary "failure_rate"))
    0))

(defn validate! [snapshot-root]
  (let [snapshot (read-valid-snapshot-index snapshot-root)]
    (println "snapshot_valid: true")
    (println "snapshot_label:" (get snapshot "snapshot_label"))
    (println "snapshot_identity_hash:" (get snapshot "snapshot_identity_hash"))
    0))

(defn usage []
  (string/join
   "\n"
   ["usage: soranoha <command> [args]"
    ""
    "commands:"
    "  list-request-sets"
    "  explain-request-set <label>"
    "  snapshot-index <request-set-label> <output-path>"
    "  reproduce <request-set-label>"
    "  validate <snapshot-root-or-index>"
    "  explain-snapshot <snapshot-index>"]))

(def commands
  {"list-request-sets" {:args 0
                        :run (fn [] (list-request-sets!))}
   "explain-request-set" {:args 1
                          :run explain-request-set!}
   "snapshot-index" {:args 2
                     :run snapshot-index!}
   "reproduce" {:args 1
                :run reproduce!}
   "validate" {:args 1
               :run validate!}
   "explain-snapshot" {:args 1
                       :run explain-snapshot!}})

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
