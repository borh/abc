(ns abc.tools.workflow
  (:require [abc.tools.manifest :as manifest]
            [abc.tools.files :as files]
            [babashka.fs :as fs]
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

(defn- duration-ms-or-nil [start end]
  (try
    (duration-ms start end)
    (catch java.time.format.DateTimeParseException _
      nil)
    (catch NullPointerException _
      nil)))

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

(defn- run-value
  "Build a workflow-run JSON value from rendered step records."
  [workflow-id run-id started-at ended-at steps]
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
  (manifest/write-json-file! (fs/file (fs/path output-root "workflow-run.json"))
                             run))

(defn- semantic-error [path message expected actual]
  (cond-> {:path path
           :message message}
    (some? expected) (assoc :expected expected)
    (some? actual) (assoc :actual actual)))

(defn- expected-run-status [steps]
  (let [statuses (map #(get % "status") steps)]
    (cond
      (some #{"failed"} statuses) "failed"
      (some #{"partial"} statuses) "partial"
      (and (seq statuses) (every? #{"skipped"} statuses)) "skipped"
      :else "passed")))

(defn- duration-errors [path value message]
  (let [expected (duration-ms-or-nil (get value "started_at")
                                     (get value "ended_at"))
        actual (get value "duration_ms")]
    (when (and (some? expected) (not= expected actual))
      [(semantic-error path message expected actual)])))

(defn- step-order-errors [steps]
  (let [produced-by-step
        (reduce-kv (fn [m idx step]
                     (reduce (fn [m' produced]
                               (assoc m' produced idx))
                             m
                             (get step "produces")))
                   {}
                   (vec steps))]
    (->> steps
         (map-indexed
          (fn [idx step]
            (keep (fn [required]
                    (let [producer-idx (get produced-by-step required)]
                      (when (and (some? producer-idx)
                                 (>= producer-idx idx))
                        (semantic-error ["steps" idx "requires"]
                                        "step requires a value before it is produced"
                                        (str "producer step before " idx)
                                        required))))
                  (get step "requires"))))
         (apply concat)
         vec)))

(defn validate-run
  "Return semantic workflow-run invariant violations. JSON Schema validation
  remains the shape contract; this checks counters, status, durations, and
  dependency order among produced keys."
  [run]
  (let [steps (vec (get run "steps" []))
        passed (count (filter #(= "passed" (get % "status")) steps))
        failed (count (filter #(= "failed" (get % "status")) steps))
        expected-status (expected-run-status steps)
        errors (concat
                (when (not= (count steps) (get run "step_count"))
                  [(semantic-error ["step_count"]
                                   "step_count must equal number of steps"
                                   (count steps)
                                   (get run "step_count"))])
                (when (not= passed (get run "steps_passed"))
                  [(semantic-error ["steps_passed"]
                                   "steps_passed must equal passed step count"
                                   passed
                                   (get run "steps_passed"))])
                (when (not= failed (get run "steps_failed"))
                  [(semantic-error ["steps_failed"]
                                   "steps_failed must equal failed step count"
                                   failed
                                   (get run "steps_failed"))])
                (when (not= expected-status (get run "status"))
                  [(semantic-error ["status"]
                                   "status must match step statuses"
                                   expected-status
                                   (get run "status"))])
                (duration-errors ["duration_ms"]
                                 run
                                 "duration_ms must match started_at and ended_at")
                (mapcat (fn [[idx step]]
                          (duration-errors ["steps" idx "duration_ms"]
                                           step
                                           "step duration_ms must match started_at and ended_at"))
                        (map-indexed vector steps))
                (step-order-errors steps))]
    (vec errors)))

(defn- step-value
  "Build a workflow step-record JSON value."
  [{:keys [step status started-at ended-at result error]}]
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

(defn invoke-step-run [run state]
  (run state))

(defn invoke-clock [clock]
  (clock))

(defn run-workflow!
  [{:keys [workflow-id run-id output-root initial-state steps clock]
    :or {clock now-utc
         run-id "local-run"}}]
  (let [output-root (fs/path output-root)
        _ (files/create-dirs! output-root)
        plan (validate-plan! {:steps steps} initial-state)
        ordered-steps (:steps plan)
        started-at (invoke-clock clock)]
    (manifest/write-json-file! (fs/file (fs/path output-root "workflow-plan.json"))
                               (json-plan workflow-id ordered-steps))
    (loop [state initial-state
           remaining ordered-steps
           records []]
      (if (empty? remaining)
        (let [ended-at (invoke-clock clock)
              run (run-value workflow-id run-id started-at ended-at records)]
          (write-run! output-root run)
          {:state state :run run :plan plan})
        (let [step (first remaining)
              step-start (invoke-clock clock)
              result (try
                       (invoke-step-run (:run step) state)
                       (catch Throwable t
                         (let [step-end (invoke-clock clock)
                               record (step-value {:step step
                                                   :status "failed"
                                                   :started-at step-start
                                                   :ended-at step-end
                                                   :result {}
                                                   :error t})
                               run (run-value workflow-id
                                              run-id
                                              started-at
                                              step-end
                                              (conj records record))]
                           (write-run! output-root run)
                           (throw t))))
              status (name (or (:status result) :passed))
              step-end (invoke-clock clock)
              record (step-value {:step step
                                  :status status
                                  :started-at step-start
                                  :ended-at step-end
                                  :result result})
              records' (conj records record)
              interim (run-value workflow-id
                                 run-id
                                 started-at
                                 step-end
                                 records')]
          (write-run! output-root interim)
          (recur (merge state (:state-updates result))
                 (rest remaining)
                 records'))))))
