(ns soranoha.annotations.layer
  "Separate analysis claims bound to the exact text and inputs their producer used."
  (:require [charred.api :as json]
            [clojure.string :as str]
            [soranoha.annotations.view :as view]
            [soranoha.core.hash :as hash]
            [soranoha.core.json :as record-json]))

(def ^:private statuses
  {"completed" :layer/completed "partial" :layer/partial
   "failed" :layer/failed "not-run" :layer/not-run})

(defn- require! [pred message data]
  (when-not pred (throw (ex-info message data))))

(defn- exact-keys! [value required optional]
  (require! (and (map? value)
                 (every? #(contains? value %) required)
                 (every? (into (set required) optional) (keys value)))
            "Invalid annotation object fields" {:required required :fields (when (map? value) (keys value))}))

(defn- nonblank? [value]
  (and (string? value) (not (str/blank? value))))

(defn- identity? [value]
  (and (string? value) (boolean (re-matches hash/hash-pattern value))))

(defn- identities? [value]
  (and (map? value) (every? nonblank? (keys value)) (every? identity? (vals value))))

(defn- span? [boundaries span]
  (and (vector? span) (= 2 (count span))
       (every? integer? span) (apply <= span)
       (every? #(contains? boundaries %) span)))

(defn- eligibility? [boundaries spans]
  (and (vector? spans) (every? #(span? boundaries %) spans)
       (every? (fn [[a b]] (< a b)) spans)
       (every? (fn [[[_ end] [start _]]] (<= end start)) (partition 2 1 spans))))

(defn validate-layer
  "Reject mismatched views, invalid UTF-8 ranges, undeclared inputs and inconsistent outcomes.
  Vocabulary identifiers bind labels/features to the producer's interpretation."
  [text-view layer]
  (exact-keys! layer [:layer/view-id :layer/vocabulary :layer/producer :layer/dependencies
                      :layer/eligible-spans :layer/status :layer/records] [])
  (let [{:layer/keys [view-id vocabulary producer dependencies eligible-spans status records]} layer
        _ (require! (and (vector? records) (vector? eligible-spans) (every? vector? eligible-spans))
                    "Annotation records and eligible spans must be vectors" {})
        boundaries (view/utf8-offsets (:view/text text-view)
                                      (concat (mapcat identity eligible-spans)
                                              (mapcat (juxt :annotation/start :annotation/end) records)))]
    (require! (= view-id (:view/id text-view)) "Annotation layer targets a different text view" {:view-id view-id})
    (require! (identity? vocabulary) "Expected vocabulary content identity" {:vocabulary vocabulary})
    (exact-keys! producer [:producer/name :producer/inputs] [])
    (require! (and (nonblank? (:producer/name producer))
                   (identities? (:producer/inputs producer))
                   (seq (:producer/inputs producer)))
              "Producer requires named content-addressed inputs" {})
    (require! (identities? dependencies) "Invalid annotation dependency identities" {})
    (require! (eligibility? boundaries eligible-spans) "Invalid eligible UTF-8 spans" {})
    (require! (every? (fn [[start end]]
                        (some (fn [[a b]] (<= a start end b)) (:view/eligible-spans text-view)))
                      eligible-spans)
              "Producer eligibility exceeds the current text view mask" {})
    (require! (contains? (set (vals statuses)) status) "Invalid annotation execution status" {:status status})
    (require! (vector? records) "Annotation records must be a vector" {})
    (require! (or (#{:layer/completed :layer/partial} status) (empty? records))
              "Unsuccessful execution cannot carry annotation claims" {:status status})
    (require! (= (count records) (count (set (map :annotation/id records)))) "Duplicate annotation record id" {})
    (doseq [{:annotation/keys [id start end label features] :as record} records]
      (exact-keys! record [:annotation/id :annotation/start :annotation/end :annotation/label :annotation/features] [])
      (require! (and (string? id) (boolean (re-matches #"[A-Za-z_][A-Za-z0-9_.-]*" id)))
                "Invalid annotation record id" {:id id})
      (require! (and (span? boundaries [start end]) (< start end)) "Invalid annotation UTF-8 range" {:id id})
      (require! (some (fn [[a b]] (<= a start end b)) eligible-spans)
                "Annotation is outside the producer's eligible spans" {:id id})
      (require! (nonblank? label) "Annotation label must be nonempty" {:id id})
      (require! (and (map? features) (every? nonblank? (keys features)) (every? string? (vals features)))
                "Annotation features must map names to strings" {:id id})))
  layer)

(defn- record->wire [{:annotation/keys [id start end label features]}]
  {"id" id "start" start "end" end "label" label "features" features})

(defn- inputs->wire [{:layer/keys [view-id vocabulary producer dependencies eligible-spans]}]
  {"view" view-id "vocabulary" vocabulary
   "producer" {"name" (:producer/name producer) "inputs" (:producer/inputs producer)}
   "dependencies" dependencies "eligible_spans" eligible-spans})

(defn- ->wire [layer]
  (assoc (inputs->wire layer)
         "schema" "soranoha-annotation-layer/1"
         "status" (or (some (fn [[wire internal]] (when (= internal (:layer/status layer)) wire)) statuses)
                      (throw (ex-info "Invalid annotation execution status" {:status (:layer/status layer)})))
         "records" (mapv record->wire (:layer/records layer))))

(defn input-id [text-view layer]
  (validate-layer text-view layer)
  (hash/format-sha256 (hash/sha256-canonical-json (inputs->wire layer))))

(defn layer-id [text-view layer]
  (validate-layer text-view layer)
  (hash/format-sha256 (hash/sha256-canonical-json (->wire layer))))

(defn write-layer [text-view layer]
  (validate-layer text-view layer)
  (record-json/write-deterministic-json-str (->wire layer)))

(defn read-layer [text-view text]
  (let [wire (json/read-json text)]
    (exact-keys! wire ["schema" "view" "vocabulary" "producer" "dependencies" "eligible_spans" "status" "records"] [])
    (require! (= "soranoha-annotation-layer/1" (get wire "schema")) "Unknown annotation layer schema" {})
    (require! (contains? statuses (get wire "status")) "Unknown annotation execution status" {})
    (require! (vector? (get wire "records")) "Annotation records must be a vector" {})
    (let [producer (get wire "producer")]
      (exact-keys! producer ["name" "inputs"] [])
      (validate-layer
       text-view
       {:layer/view-id (get wire "view")
        :layer/vocabulary (get wire "vocabulary")
        :layer/producer {:producer/name (get producer "name") :producer/inputs (get producer "inputs")}
        :layer/dependencies (get wire "dependencies")
        :layer/eligible-spans (get wire "eligible_spans")
        :layer/status (get statuses (get wire "status"))
        :layer/records
        (mapv (fn [record]
                (exact-keys! record ["id" "start" "end" "label" "features"] [])
                {:annotation/id (get record "id")
                 :annotation/start (get record "start") :annotation/end (get record "end")
                 :annotation/label (get record "label") :annotation/features (get record "features")})
              (get wire "records"))}))))
