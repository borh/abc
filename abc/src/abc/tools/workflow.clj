(ns abc.tools.workflow
  (:require [abc.tools.manifest :as manifest]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def workflow-run-schema-id
  "https://w3id.org/abc/schemas/workflow-run.schema.json")

(defn- key-name [k]
  (if (keyword? k) (name k) (str k)))

(defn- now-utc []
  (str (java.time.Instant/now)))

(defn- duration-ms [start end]
  (let [s (java.time.Instant/parse start)
        e (java.time.Instant/parse end)]
    (.toMillis (java.time.Duration/between s e))))

(defn- duplicate-values [xs]
  (->> xs
       frequencies
       (filter (fn [[_ n]] (> n 1)))
       (map first)
       vec))

(defn validate-plan! [{:keys [steps] :as plan} initial-state]
  (let [ids (map :id steps)
        duplicate-ids (duplicate-values ids)
        produced (mapcat :produces steps)
        duplicate-produced (duplicate-values produced)]
    (when (seq duplicate-ids)
      (throw (ex-info "duplicate workflow step ids"
                      {:duplicate_step_ids duplicate-ids})))
    (when (seq duplicate-produced)
      (throw (ex-info "duplicate workflow produced keys"
                      {:duplicate_produced_keys duplicate-produced})))
    (loop [available (set (keys initial-state))
           remaining (vec steps)
           ordered []]
      (if (empty? remaining)
        (assoc plan :steps ordered)
        (let [{ready true blocked false}
              (group-by (fn [step]
                          (set/subset? (set (:requires step)) available))
                        remaining)]
          (when (empty? ready)
            (throw (ex-info "missing workflow dependencies"
                            {:available (vec (sort-by key-name available))
                             :blocked (mapv (fn [step]
                                              {:id (key-name (:id step))
                                               :missing
                                               (vec
                                                (sort-by key-name
                                                         (set/difference
                                                          (set (:requires step))
                                                          available)))})
                                            blocked)})))
          (recur (into available (mapcat :produces ready))
                 (vec blocked)
                 (into ordered ready)))))))

(defn- path-record [record]
  (into {}
        (for [[k v] record
              :when (some? v)]
          [(key-name k) v])))

(defn- json-step-plan [step]
  {"id" (key-name (:id step))
   "requires" (mapv key-name (:requires step))
   "produces" (mapv key-name (:produces step))})

(defn- json-plan [workflow-id steps]
  {"schema_version" "soranoha-workflow-plan-v1"
   "workflow_id" workflow-id
   "steps" (mapv json-step-plan steps)})

(defn- summarize-run [workflow-id run-id started-at ended-at steps]
  (let [failed (count (filter #(= "failed" (get % "status")) steps))
        partial (count (filter #(= "partial" (get % "status")) steps))
        passed (count (filter #(= "passed" (get % "status")) steps))]
    {"schema_id" workflow-run-schema-id
     "schema_version" "soranoha-workflow-run-v1"
     "workflow_id" workflow-id
     "run_id" run-id
     "status" (cond
                (pos? failed) "failed"
                (pos? partial) "partial"
                :else "passed")
     "started_at" started-at
     "ended_at" ended-at
     "duration_ms" (duration-ms started-at ended-at)
     "step_count" (count steps)
     "steps_passed" passed
     "steps_failed" failed
     "steps" steps}))

(defn- write-run! [output-root run]
  (manifest/write-json-file! (io/file output-root "workflow-run.json") run))

(defn- step-record [{:keys [step status started-at ended-at result error]}]
  (cond-> {"id" (key-name (:id step))
           "status" status
           "started_at" started-at
           "ended_at" ended-at
           "duration_ms" (duration-ms started-at ended-at)
           "requires" (mapv key-name (:requires step))
           "produces" (mapv key-name (:produces step))
           "inputs" (mapv path-record (:inputs result []))
           "outputs" (mapv path-record (:outputs result []))
           "messages" (mapv path-record (:messages result []))}
    error
    (assoc "error" {"error_class" (.getName (class error))
                    "message" (.getMessage error)
                    "data" (or (ex-data error) {})})))

(defn run-workflow!
  [{:keys [workflow-id run-id output-root initial-state steps clock]
    :or {clock now-utc
         run-id "local-run"}}]
  (let [output-root (io/file output-root)
        _ (.mkdirs output-root)
        plan (validate-plan! {:steps steps} initial-state)
        ordered-steps (:steps plan)
        started-at (clock)]
    (manifest/write-json-file! (io/file output-root "workflow-plan.json")
                               (json-plan workflow-id ordered-steps))
    (loop [state initial-state
           remaining ordered-steps
           records []]
      (if (empty? remaining)
        (let [ended-at (clock)
              run (summarize-run workflow-id run-id started-at ended-at records)]
          (write-run! output-root run)
          {:state state :run run :plan plan})
        (let [step (first remaining)
              step-start (clock)
              result (try
                       ((:run step) state)
                       (catch Throwable t
                         (let [step-end (clock)
                               record (step-record {:step step
                                                    :status "failed"
                                                    :started-at step-start
                                                    :ended-at step-end
                                                    :result {}
                                                    :error t})
                               run (summarize-run workflow-id
                                                  run-id
                                                  started-at
                                                  step-end
                                                  (conj records record))]
                           (write-run! output-root run)
                           (throw t))))
              status (name (or (:status result) :passed))
              step-end (clock)
              record (step-record {:step step
                                   :status status
                                   :started-at step-start
                                   :ended-at step-end
                                   :result result})
              records' (conj records record)
              interim (summarize-run workflow-id
                                     run-id
                                     started-at
                                     step-end
                                     records')]
          (write-run! output-root interim)
          (recur (merge state (:state-updates result))
                 (rest remaining)
                 records'))))))
