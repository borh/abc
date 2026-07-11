(ns abc.sim.divergences
  "Known-divergences table (source of record: the spec §Known Divergences).
  Properties assert DESIRED behavior; while an entry is open the linked
  case runs as an expected failure and alerts when it starts passing."
  (:require [clojure.test :refer [is]]))

(def table
  {:D1 {:case "P6.divergent-work-fields" :status :fixed
        :notes "fixed 2026-07-11: build-record-fragment-from-rows throws ex-info on divergent work fields; corpus ingest skips + counts the work"}
   :D2 {:case "P12.selection" :status :fixed
        :notes "fixed 2026-07-11: sample-commits-by-period groups globally by period key (was contiguous partition-by), keeping the last commit in log order per period"}
   :D3 {:case "P13.ragged-row" :status :fixed
        :notes "fixed 2026-07-11: read-rows* marks ragged rows (ragged-key); work assembly rejects them, corpus ingest skips + counts the work"}
   :D4 {:case "P8.atomicity, P8.order-independence" :status :fixed
        :notes "fixed 2026-07-11: run-corpus! builds+validates all works before writing; shared-person conflicts resolve to the smallest work_id deterministically and are reported in :person-conflicts"}
   :D5 {:case "P13.empty-csv" :status :fixed
        :notes "fixed 2026-07-11: zip entry points throw ex-info {:zip-path :row-count} when the CSV has no data rows"}
   :D6 {:case "P13.non-zip-bytes" :status :fixed
        :notes "fixed 2026-07-11: read-zip-csv wraps ZipException as ex-info {:zip-path} with cause chained"}})

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
