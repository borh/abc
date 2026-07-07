(ns abc.tools.parser-evidence
  (:require [abc.tools.edn-registry :as registry]
            [abc.tools.files :as files]
            [abc.tools.malli :as am]
            [clojure.edn :as edn]
            [clojure.string :as string]))

(def index-path
  (files/path "data" "parser-evidence-citations.edn"))

(def required-entry-keys
  [:evidence_id
   :evidence_class
   :producer_component
   :logical_path
   :sha256
   :status
   :summary])

(defn- parser-evidence-malli-errors
  [idx entry]
  (if-let [explanation (am/explain-contract ::am/parser-evidence-entry entry)]
    (mapv #(str "Parser evidence index entry " idx " " %)
          (am/explanation-messages explanation))
    []))

(defn- entry-errors
  [idx entry]
  (if-not (map? entry)
    [(str "Parser evidence index entry " idx " must be a map")]
    (vec
     (concat
      (registry/missing-entry-key-errors
       "Parser evidence index entry"
       idx
       required-entry-keys
       entry)
      (parser-evidence-malli-errors idx entry)))))

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
  (registry/registry-errors
   {:registry index
    :label "Parser evidence index"
    :entry-error-fn entry-errors
    :duplicate-error-fn duplicate-errors}))

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
