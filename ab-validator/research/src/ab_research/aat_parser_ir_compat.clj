(ns ab-research.aat-parser-ir-compat
  (:require [ab-research.cli :as abc-cli]
            [ab-research.edn-registry :as registry]
            [ab-research.files :as files]
            [ab-research.malli :as am]
            [clojure.string :as string]
            [malli.core :as m]
            [malli.registry :as mr]))

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

(def schemas
  "Domain-owned Malli schemas for the AAT parser-IR compatibility
  registry."
  {::aat-parser-ir-evidence-scope
   ;; A missing or unknown :evidence_type fails the :multi dispatch; the
   ;; error's :in ends with :evidence_type, so the rendered message reads
   ;; ":evidence_scope :evidence_type must be mapping-generation or
   ;; conversion-audit" for both the missing and the invalid case.
   [:multi {:dispatch :evidence_type
            :error/message "must be mapping-generation or conversion-audit"}
    [:mapping-generation
     [:map
      [:adapter ::am/concrete-adapter]
      [:adapter_version {:optional true} ::am/nullable-nonblank-string]
      [:corpus ::am/nonblank-string]
      [:evidence_type [:= {:error/message "must be mapping-generation or conversion-audit"}
                       :mapping-generation]]
      [:files_scanned ::am/positive-int]
      [:files_with_unsupported ::am/nonnegative-int]
      [:generated_rules ::am/positive-int]]]
    [:conversion-audit
     [:map
      [:adapter ::am/concrete-adapter]
      [:adapter_version {:optional true} ::am/nullable-nonblank-string]
      [:corpus ::am/nonblank-string]
      [:evidence_type [:= {:error/message "must be mapping-generation or conversion-audit"}
                       :conversion-audit]]
      [:files_scanned ::am/positive-int]
      [:files_succeeded ::am/nonnegative-int]
      [:files_failed ::am/nonnegative-int]
      [:parser_ir_nodes ::am/nonnegative-int]
      [:divergence_records ::am/nonnegative-int]
      [:divergence_occurrences ::am/nonnegative-int]
      [:rules_total ::am/nonnegative-int]
      [:rules_emitted ::am/nonnegative-int]
      [:rules_missing ::am/nonnegative-int]
      [:unsupported_occurrences ::am/nonnegative-int]]]]

   ::aat-parser-ir-compat-entry
   [:and
    [:map
     [:aat_version ::am/positive-int]
     [:aat_adapter ::am/concrete-adapter]
     ;; Presence is required (it is a compatibility match key); the value
     ;; itself may be null.
     [:aat_adapter_version ::am/nullable-nonblank-string]
     [:mapping_id ::am/nonblank-string]
     [:mapping_version ::am/semver]
     [:mapping_hash ::am/sha256-hash]
     [:mapping_schema_hash ::am/sha256-hash]
     [:parser_ir_schema_id ::am/nonblank-string]
     [:parser_ir_schema_hash ::am/sha256-hash]
     [:compatibility [:enum {:error/message "must be lossy or lossless"}
                      "lossy" "lossless"]]
     [:evidence_scope ::aat-parser-ir-evidence-scope]]
    [:fn {:error/message ":evidence_scope :adapter must equal :aat_adapter"}
     (fn [entry] (= (:aat_adapter entry)
                    (get-in entry [:evidence_scope :adapter])))]
    [:fn {:error/message ":evidence_scope :adapter_version must equal :aat_adapter_version"}
     (fn [entry] (= (:aat_adapter_version entry)
                    (get-in entry [:evidence_scope :adapter_version])))]
    [:fn {:error/message "files_scanned must equal files_succeeded plus files_failed"}
     (fn [entry]
       (let [scope (:evidence_scope entry)]
         (or (not= :conversion-audit (:evidence_type scope))
             (= (:files_scanned scope)
                (+ (:files_succeeded scope) (:files_failed scope))))))]
    [:fn {:error/message "rules_total must equal rules_emitted plus rules_missing"}
     (fn [entry]
       (let [scope (:evidence_scope entry)]
         (or (not= :conversion-audit (:evidence_type scope))
             (= (:rules_total scope)
                (+ (:rules_emitted scope) (:rules_missing scope))))))]]})

(def malli-registry
  "This domain's explicit Malli registry: core schemas + shared scalars
  + the schemas owned here. Never installed globally."
  (mr/composite-registry (m/default-schemas) am/scalar-schemas schemas))

(defn- compatibility-malli-errors
  [idx entry]
  (if-let [explanation (m/explain ::aat-parser-ir-compat-entry entry
                                  {:registry malli-registry})]
    (mapv #(str "AAT parser-IR compatibility registry entry " idx " " %)
          (am/explanation-messages explanation))
    []))

(defn- entry-errors
  [idx entry]
  (if-not (map? entry)
    [(str "AAT parser-IR compatibility registry entry " idx
          " must be a map")]
    (compatibility-malli-errors idx entry)))

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

(defn append-missing
  "Return a new registry containing exactly the missing candidates.

  The input registry is never mutated. Already-admitted, conflicting, or
  invalid candidate sets are refused so an operator cannot use this function
  as a general-purpose registry editor."
  [registry candidates]
  (let [report (admission-report registry candidates)]
    (when-not (= :missing (:status report))
      (throw (ex-info "compatibility append requires only missing candidates"
                      {:status (:status report)})))
    (let [missing (sort-by (comp pr-str match-key) (:missing report))]
      (assoc registry :entries (into (vec (:entries registry)) missing)))))

(def cli-options
  [[nil "--registry PATH" "Registry EDN path"
    :default registry-path]
   [nil "--candidates PATH" "Producer compatibility candidates EDN path"]
   [nil "--append-out PATH" "Write an append-only registry value to a new path"]])

(defn usage
  [summary]
  (str "Usage: clojure -M:abc/aat-compat-admission -- --candidates <path> [--registry <path>] [--append-out <path>]\n\n"
       summary))

(defn -main
  [& args]
  (abc-cli/run-cli!
   args
   {:cli-options cli-options
    :required    [:candidates]
    :usage-fn    usage
    :run         (fn [{:keys [options]}]
                   (let [registry (files/read-edn (:registry options))
                         candidates (files/read-edn (:candidates options))
                         report (admission-report registry candidates)]
                     (when-let [out (:append-out options)]
                       (files/write-text! out
                                          (str (pr-str (append-missing registry candidates))
                                               "\n")))
                     ;; report is the primary tool output on stdout
                     (prn report)
                     report))
    :fail?       (fn [report]
                   (not (contains? #{:admitted :missing} (:status report))))}))
