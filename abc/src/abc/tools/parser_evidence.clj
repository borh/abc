(ns abc.tools.parser-evidence
  (:require [abc.tools.files :as files]
            [clojure.edn :as edn]
            [clojure.string :as string]))

(def index-path
  (files/path "data" "parser-evidence-citations.edn"))

(def evidence-classes
  #{:conversion-compatibility
    :parser-selection
    :comparator-oracle})

(def statuses
  #{:citable
    :provisional
    :superseded})

(def required-entry-keys
  [:evidence_id
   :evidence_class
   :producer_component
   :logical_path
   :sha256
   :status
   :summary])

(defn- sha256-hash?
  [value]
  (and (string? value)
       (boolean (re-matches #"^sha256:[0-9a-f]{64}$" value))))

(defn- nonblank-string?
  [value]
  (and (string? value)
       (not (string/blank? value))))

(defn- logical-path?
  [value]
  (and (nonblank-string? value)
       (not (string/starts-with? value "../"))
       (not (string/starts-with? value "/"))))

(defn- missing-key-errors
  [idx entry]
  (->> required-entry-keys
       (remove #(contains? entry %))
       (mapv #(str "Parser evidence index entry " idx " is missing " %))))

(defn- entry-errors
  [idx entry]
  (if-not (map? entry)
    [(str "Parser evidence index entry " idx " must be a map")]
    (vec
     (concat
      (missing-key-errors idx entry)
      (for [k [:evidence_id :producer_component :summary]
            :when (and (contains? entry k)
                       (not (nonblank-string? (get entry k))))]
        (str "Parser evidence index entry " idx " " k
             " must be a non-empty string"))
      (when (and (contains? entry :evidence_class)
                 (not (contains? evidence-classes (:evidence_class entry))))
        [(str "Parser evidence index entry " idx
              " :evidence_class must be conversion-compatibility, parser-selection, or comparator-oracle")])
      (when (and (contains? entry :status)
                 (not (contains? statuses (:status entry))))
        [(str "Parser evidence index entry " idx
              " :status must be citable, provisional, or superseded")])
      (when (and (contains? entry :logical_path)
                 (not (logical-path? (:logical_path entry))))
        [(str "Parser evidence index entry " idx
              " :logical_path must be workspace-relative and must not start with ../ or /")])
      (when (and (contains? entry :current_external_path)
                 (some? (:current_external_path entry))
                 (not (nonblank-string? (:current_external_path entry))))
        [(str "Parser evidence index entry " idx
              " :current_external_path must be null or a non-empty string")])
      (when (and (contains? entry :sha256)
                 (not (sha256-hash? (:sha256 entry))))
        [(str "Parser evidence index entry " idx
              " :sha256 must be a sha256 hash")])))))

(defn- duplicate-errors
  [entries]
  (let [dupes (fn [key-fn label]
                (->> entries
                     (map-indexed (fn [idx entry] [(key-fn entry) idx]))
                     (filter (comp some? first))
                     (group-by first)
                     (keep (fn [[k pairs]]
                             (when (< 1 (count pairs))
                               (str "Parser evidence index duplicates " label
                                    " " k " at entries "
                                    (string/join ", " (map second pairs))))))))]
    (vec (concat (dupes :evidence_id ":evidence_id")
                 (dupes (juxt :logical_path :sha256)
                        "logical_path + sha256")))))

(defn index-errors
  [index]
  (vec
   (cond
     (not (map? index))
     ["Parser evidence index must be an EDN map"]

     (not (contains? index :entries))
     ["Parser evidence index is missing :entries"]

     (not (vector? (:entries index)))
     ["Parser evidence index :entries must be a vector"]

     (empty? (:entries index))
     ["Parser evidence index :entries must not be empty"]

     :else
     (concat
      (mapcat (fn [[idx entry]] (entry-errors idx entry))
              (map-indexed vector (:entries index)))
      (duplicate-errors (:entries index))))))

(defn validate-index!
  [index]
  (let [errors (index-errors index)]
    (when (seq errors)
      (throw (ex-info (string/join "\n" errors)
                      {:errors errors})))
    :ok))

(defn load-index
  []
  (let [index (edn/read-string (slurp index-path))]
    (validate-index! index)
    index))
