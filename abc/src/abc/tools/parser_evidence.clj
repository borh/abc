(ns abc.tools.parser-evidence
  (:require [abc.tools.edn-registry :as registry]
            [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.malli :as am]
            [abc.tools.path-containment :as path-containment]
            [clojure.string :as string]
            [malli.core :as m]))

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

(defn evidence-id-key [entry] (:evidence_id entry))

(defn logical-file-key [entry] [(:logical_path entry) (:sha256 entry)])

(defn citation-file-problems
  [monorepo-root index]
  (->> (:entries index)
       (keep (fn [{:keys [evidence_id logical_path sha256]}]
               (let [{:keys [state path]}
                     (path-containment/path-state monorepo-root logical_path)
                     kind (case state
                            :missing :citation-missing
                            :path-traversal :citation-path-traversal
                            :malformed-path :citation-path-traversal
                            :real-path-escape :citation-real-path-escape
                            :ok (when-not (= sha256
                                             (str "sha256:"
                                                  (hash/sha256-file path)))
                                  :citation-hash-mismatch))]
                 (when kind
                   (sorted-map :kind kind
                               :evidence-id evidence_id
                               :logical-path logical_path)))))
       vec))

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
    (vec (concat (dupes evidence-id-key ":evidence_id")
                 (dupes logical-file-key
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
  (let [index (files/read-edn index-path)]
    (validate-index! index)
    index))

(defn citable-hashes
  ([]
   (citable-hashes (load-index)))
  ([index]
   (->> (:entries index)
        (filter #(= :citable (:status %)))
        (map :sha256)
        sort
        distinct
        vec)))

;; Instrumented contract (bites once abc.tools.malli/install! runs): the
;; 1-arity is only ever called with a validated index, so this documents the
;; invariant and gives mi/instrument! a real function schema to wrap. Schemas
;; are inlined (core schemas only) rather than registry ::refs so the m/=>
;; resolves at load time — registry refs are not populated until install!.
(m/=> citable-hashes
      [:function
       [:=> [:cat] [:vector :string]]
       [:=> [:cat [:map [:entries [:vector [:map
                                            [:sha256 :string]
                                            [:status :keyword]]]]]]
        [:vector :string]]])
