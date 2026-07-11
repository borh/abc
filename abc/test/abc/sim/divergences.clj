(ns abc.sim.divergences
  "Known-divergences table (source of record: the spec §Known Divergences).
  Properties assert DESIRED behavior; while an entry is open the linked
  case runs as an expected failure and alerts when it starts passing."
  (:require [clojure.test :refer [is]]))

(def table
  {:D1 {:case "P6.divergent-work-fields" :status :open
        :notes "aozora_csv.clj:284-325 first-row-wins on divergent work fields"}
   :D2 {:case "P12.selection" :status :open
        :notes "aozora_history_audit.clj:188-193 partition-by over log order"}
   :D3 {:case "P13.ragged-row" :status :open
        :notes "aozora_csv.clj:22-32 ragged rows silently truncated"}
   :D4 {:case "P8.atomicity, P8.order-independence" :status :open
        :notes "aozora_ingest.clj:151-213 person writes precede work failure"}
   :D5 {:case "P13.empty-csv" :status :open
        :notes "confirmed 2026-07-11 (P13): empty/header-only CSV yields a silent zero-row corpus, no throw, no skip"}
   :D6 {:case "P13.non-zip-bytes" :status :open
        :notes "confirmed 2026-07-11 (P13): ZipFile ctor escapes as raw java.util.zip.ZipException, not ex-info"}})

(defn- entry [id]
  (or (get table id)
      (throw (ex-info "unknown divergence id" {:id id :known (vec (keys table))}))))

(defn open? [id]
  (contains? #{:open :adjudicated-bug} (:status (entry id))))

(defn expected-failure*
  "Returns true when the outcome matches the table's expectation. desired-fn
  must return a boolean derived from an already-captured SUT outcome; it must
  not perform SUT calls or I/O. Exceptions ESCAPE (harness defect), they are
  never treated as 'desired behavior absent'."
  [id _desc desired-fn]
  (let [holds? (boolean (desired-fn))]
    (if (open? id) (not holds?) holds?)))

(defmacro expected-failure
  [id desc & body]
  `(is (expected-failure* ~id ~desc (fn [] ~@body))
       (if (open? ~id)
         (str ~desc ": divergence " ~id " now passes — adjudicate the table")
         (str ~desc " (" ~id ") regressed"))))
