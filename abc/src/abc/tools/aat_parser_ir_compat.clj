(ns abc.tools.aat-parser-ir-compat
  (:require [abc.tools.edn-registry :as registry]
            [abc.tools.files :as files]
            [abc.tools.malli :as am]
            [clojure.string :as string]
            [clojure.tools.cli :as cli]))

(def registry-path
  (files/path "data" "aat-parser-ir-compatibility.edn"))

(def match-keys
  [:aat_version
   :aat_adapter
   :aat_adapter_version
   :mapping_id
   :mapping_version
   :mapping_hash
   :mapping_schema_hash
   :parser_ir_schema_id
   :parser_ir_schema_hash])

(def required-entry-keys
  (conj match-keys :evidence_scope :compatibility))

(def required-evidence-scope-common-keys
  [:adapter
   :corpus
   :evidence_type
   :files_scanned])

(def required-mapping-generation-evidence-keys
  [:files_with_unsupported
   :generated_rules])

(def required-conversion-audit-evidence-keys
  [:files_succeeded
   :files_failed
   :parser_ir_nodes
   :divergence_records
   :divergence_occurrences
   :rules_total
   :rules_emitted
   :rules_missing
   :unsupported_occurrences])

(defn- missing-evidence-scope-key-errors
  [idx entry]
  (let [scope (:evidence_scope entry)]
    (if-not (map? scope)
      [(str "AAT parser-IR compatibility registry entry " idx
            " :evidence_scope must be a map")]
      (let [evidence-type (:evidence_type scope)
            mode-required-keys (case evidence-type
                                 :mapping-generation required-mapping-generation-evidence-keys
                                 :conversion-audit required-conversion-audit-evidence-keys
                                 [])]
        (->> (concat required-evidence-scope-common-keys mode-required-keys)
             (remove #(contains? scope %))
             (mapv #(str "AAT parser-IR compatibility registry entry " idx
                         " :evidence_scope is missing " %)))))))

(defn- compatibility-malli-errors
  [idx entry]
  (if-let [explanation (am/explain-contract ::am/aat-parser-ir-compat-entry entry)]
    (mapv #(str "AAT parser-IR compatibility registry entry " idx " " %)
          (am/explanation-messages explanation))
    []))

(defn- entry-errors
  [idx entry]
  (if-not (map? entry)
    [(str "AAT parser-IR compatibility registry entry " idx
          " must be a map")]
    (vec
     (concat
      (registry/missing-entry-key-errors
       "AAT parser-IR compatibility registry entry"
       idx
       required-entry-keys
       entry)
      (when (contains? entry :evidence_scope)
        (missing-evidence-scope-key-errors idx entry))
      (compatibility-malli-errors idx entry)))))

(defn- duplicate-key-errors
  [entries]
  (loop [remaining (map-indexed vector entries)
         seen {}
         errors []]
    (if-let [[idx entry] (first remaining)]
      (let [k (select-keys entry match-keys)]
        (if-let [prior-idx (get seen k)]
          (recur (rest remaining)
                 seen
                 (conj errors
                       (str "AAT parser-IR compatibility registry entry " idx
                            " duplicates entry " prior-idx
                            " compatibility keys")))
          (recur (rest remaining)
                 (assoc seen k idx)
                 errors)))
      errors)))

(defn registry-errors
  [registry]
  (registry/registry-errors
   {:registry registry
    :label "AAT parser-IR compatibility registry"
    :entry-error-fn entry-errors
    :duplicate-error-fn duplicate-key-errors}))

(defn validate-registry!
  [registry]
  (let [errors (registry-errors registry)]
    (when (seq errors)
      (throw (ex-info (string/join "\n" errors)
                      {:errors errors})))
    :ok))

(defn load-registry
  []
  (let [registry (files/read-edn registry-path)]
    (validate-registry! registry)
    registry))

(defn compatible?
  [registry query]
  (boolean
   (some #(= (select-keys % match-keys)
             (select-keys query match-keys))
         (:entries registry))))

(defn- match-key
  [entry]
  (select-keys entry match-keys))

(defn- entries-by-match-key
  [entries]
  (into {} (map (juxt match-key identity)) entries))

(defn admission-report
  "Compare producer compatibility candidates against an ABC registry.

  This is stricter than `compatible?`: a candidate with already-admitted
  match keys but changed evidence is reported as a conflict, not as admitted.
  That keeps registry admission exact while preserving exact adapter-version
  matching as the compatibility identity."
  [registry candidates]
  (let [registry-validation-errors (registry-errors registry)
        candidate-validation-errors (registry-errors candidates)
        candidate-entries (if (vector? (:entries candidates))
                            (:entries candidates)
                            [])
        base {:candidate-count (count candidate-entries)
              :admitted []
              :missing []
              :conflicts []
              :registry-errors registry-validation-errors
              :candidate-errors candidate-validation-errors}]
    (cond
      (seq registry-validation-errors)
      (assoc base :status :invalid-registry)

      (seq candidate-validation-errors)
      (assoc base :status :invalid-candidates)

      :else
      (let [registry-by-key (entries-by-match-key (:entries registry))
            report (reduce
                    (fn [acc candidate]
                      (if-let [registry-entry (get registry-by-key
                                                   (match-key candidate))]
                        (if (= registry-entry candidate)
                          (update acc :admitted conj candidate)
                          (update acc :conflicts conj {:registry registry-entry
                                                       :candidate candidate}))
                        (update acc :missing conj candidate)))
                    base
                    candidate-entries)
            status (cond
                     (seq (:conflicts report)) :conflict
                     (seq (:missing report)) :missing
                     :else :admitted)]
        (assoc report :status status)))))

(def cli-options
  [[nil "--registry PATH" "Registry EDN path"
    :default registry-path]
   [nil "--candidates PATH" "Producer compatibility candidates EDN path"]])

(defn- usage
  [summary]
  (binding [*out* *err*]
    (println "Usage: clojure -M:abc/aat-compat-admission -- --candidates <path> [--registry <path>]")
    (println)
    (println summary)))

(defn -main
  [& args]
  (let [args (if (= "--" (first args)) (rest args) args)
        {:keys [options errors summary]} (cli/parse-opts args cli-options)]
    (cond
      (seq errors)
      (do
        (binding [*out* *err*]
          (doseq [error errors]
            (println error)))
        (usage summary)
        (System/exit 2))

      (nil? (:candidates options))
      (do
        (usage summary)
        (System/exit 2))

      :else
      (let [report (admission-report
                    (files/read-edn (:registry options))
                    (files/read-edn (:candidates options)))]
        (prn report)
        (when-not (= :admitted (:status report))
          (System/exit 1))))))
