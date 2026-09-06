;; SQLite (WAL) constructive-trace store. One trace row per derivation key
;; (stage-id, stage-version, toolchain-id, input-hashes) -> output-hashes,
;; plus an append-only history ledger recording every actual execution as the
;; determinism monitor: two history rows for the same key with different
;; outputs is the violation the verifier queries for.
(ns soranoha.kura.trace
  (:require [charred.api :as json]
            [next.jdbc :as jdbc]
            [soranoha.core.canonical :as canonical]
            [soranoha.core.hash :as hash])
  (:import [java.time Instant]))

(def ^:private schema-statements
  ["CREATE TABLE IF NOT EXISTS trace (
      trace_key     TEXT PRIMARY KEY,
      stage_id      TEXT NOT NULL,
      stage_version TEXT NOT NULL,
      toolchain_id  TEXT NOT NULL,
      inputs_json   TEXT NOT NULL,
      outputs_json  TEXT NOT NULL)"
   "CREATE TABLE IF NOT EXISTS history (
      seq          INTEGER PRIMARY KEY AUTOINCREMENT,
      trace_key    TEXT NOT NULL,
      outputs_json TEXT NOT NULL,
      recorded_at  TEXT NOT NULL)"
   "CREATE INDEX IF NOT EXISTS history_trace_key ON history (trace_key)"])

(defn open!
  "Open (creating if needed) the trace database. Returns a store handle;
  serialize writes through it with `locking` on the handle itself."
  [db-path]
  (let [ds (jdbc/get-datasource {:dbtype "sqlite" :dbname db-path})
        conn (jdbc/get-connection ds)]
    (jdbc/execute-one! conn ["PRAGMA journal_mode=WAL"])
    (jdbc/execute-one! conn ["PRAGMA synchronous=NORMAL"])
    (doseq [statement schema-statements]
      (jdbc/execute-one! conn [statement]))
    {:conn conn :db-path db-path}))

(defn close! [{:keys [conn]}]
  (.close ^java.sql.Connection conn))

(defn stage-coordinate
  "The one encoding of a stage's non-input derivation coordinates: the
  exact object every derivation key hashes (with \"inputs\" added) and the
  delta oracle compares. A coordinate added here is automatically both
  hashed and compared — there is no second encoding to keep in sync."
  [{:keys [stage-id stage-version toolchain-id]}]
  {"stage_id" stage-id
   "stage_version" stage-version
   "toolchain_id" toolchain-id})

(defn derivation-key
  "The trace key: sha256 over the canonical bytes of the full derivation
  coordinates, including stage-version and toolchain-id."
  [stage inputs]
  (hash/sha256-canonical-json (assoc (stage-coordinate stage)
                                     "inputs" inputs)))

(defn stage-coordinates
  "Logical stage key -> stage-coordinate for a stage set. Two runs' trace
  keys are comparable as input-equality evidence only when their tables
  from this projection are equal."
  [stages]
  (into {}
        (map (fn [[k stage]] [k (stage-coordinate stage)]))
        stages))

(defn lookup
  "Cached outputs map for a trace key, or nil."
  [{:keys [conn] :as store} trace-key]
  (locking store
    (when-let [row (jdbc/execute-one!
                    conn ["SELECT outputs_json FROM trace WHERE trace_key = ?"
                          trace-key])]
      (json/read-json (:trace/outputs_json row)))))

(defn record-execution!
  "Record an executed derivation: history append first (the observation),
  then the trace row (INSERT OR IGNORE keeps the first winner under races —
  divergent losers remain visible in history)."
  [{:keys [conn] :as store} stage inputs outputs]
  (let [trace-key (derivation-key stage inputs)
        inputs-json (canonical/rfc8785-safe-integer-json-string-v1 inputs)
        outputs-json (canonical/rfc8785-safe-integer-json-string-v1 outputs)]
    (locking store
      (jdbc/execute-one!
       conn ["INSERT INTO history (trace_key, outputs_json, recorded_at)
              VALUES (?, ?, ?)"
             trace-key outputs-json (str (Instant/now))])
      (jdbc/execute-one!
       conn ["INSERT OR IGNORE INTO trace
              (trace_key, stage_id, stage_version, toolchain_id,
               inputs_json, outputs_json)
              VALUES (?, ?, ?, ?, ?, ?)"
             trace-key (:stage-id stage) (:stage-version stage)
             (:toolchain-id stage) inputs-json outputs-json]))
    trace-key))

(defn determinism-violations
  "Trace keys whose execution history contains conflicting output maps."
  [{:keys [conn] :as store}]
  (locking store
    (mapv :history/trace_key
          (jdbc/execute!
           conn ["SELECT trace_key FROM history
                  GROUP BY trace_key
                  HAVING COUNT(DISTINCT outputs_json) > 1"]))))

(defn all-output-hashes
  "Every blob hash referenced by any trace outputs value (for fixity sweep)."
  [{:keys [conn] :as store}]
  (locking store
    (into #{}
          (mapcat (fn [row]
                    (vals (json/read-json (:trace/outputs_json row)))))
          (jdbc/execute! conn ["SELECT outputs_json FROM trace"]))))
