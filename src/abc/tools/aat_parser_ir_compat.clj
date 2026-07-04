(ns abc.tools.aat-parser-ir-compat
  (:require [abc.tools.files :as files]
            [clojure.edn :as edn]
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

(def evidence-types
  #{:mapping-generation :conversion-audit})

(def hash-keys
  [:mapping_hash
   :mapping_schema_hash
   :parser_ir_schema_hash])

(def concrete-adapter-placeholders
  #{"*" "all" "any" "<any>" "adapter-neutral"})

(defn- sha256-hash?
  [value]
  (and (string? value)
       (boolean (re-matches #"^sha256:[0-9a-f]{64}$" value))))

(defn- nonblank-string?
  [value]
  (and (string? value)
       (not (string/blank? value))))

(defn- concrete-adapter?
  [value]
  (and (nonblank-string? value)
       (not (contains? concrete-adapter-placeholders
                       (string/lower-case value)))))

(defn- nonnegative-int?
  [value]
  (and (integer? value) (not (neg? value))))

(defn- positive-int?
  [value]
  (and (integer? value) (pos? value)))

(defn- missing-key-errors
  [idx entry]
  (->> required-entry-keys
       (remove #(contains? entry %))
       (mapv #(str "AAT parser-IR compatibility registry entry " idx
                   " is missing " %))))

(defn- evidence-scope-errors
  [idx entry]
  (let [scope (:evidence_scope entry)]
    (cond
      (not (map? scope))
      [(str "AAT parser-IR compatibility registry entry " idx
            " :evidence_scope must be a map")]

      :else
      (let [evidence-type (:evidence_type scope)
            mode-required-keys (case evidence-type
                                 :mapping-generation required-mapping-generation-evidence-keys
                                 :conversion-audit required-conversion-audit-evidence-keys
                                 [])]
        (vec
         (concat
          (->> (concat required-evidence-scope-common-keys mode-required-keys)
               (remove #(contains? scope %))
               (map #(str "AAT parser-IR compatibility registry entry " idx
                          " :evidence_scope is missing " %)))
          (when (and (contains? scope :evidence_type)
                     (not (contains? evidence-types evidence-type)))
            [(str "AAT parser-IR compatibility registry entry " idx
                  " :evidence_scope :evidence_type must be mapping-generation or conversion-audit")])
          (when (and (contains? scope :adapter)
                     (not= (:aat_adapter entry) (:adapter scope)))
            [(str "AAT parser-IR compatibility registry entry " idx
                  " :evidence_scope :adapter must equal :aat_adapter")])
          (when (and (contains? scope :adapter_version)
                     (not= (:aat_adapter_version entry) (:adapter_version scope)))
            [(str "AAT parser-IR compatibility registry entry " idx
                  " :evidence_scope :adapter_version must equal :aat_adapter_version")])
          (when (and (contains? scope :adapter)
                     (not (concrete-adapter? (:adapter scope))))
            [(str "AAT parser-IR compatibility registry entry " idx
                  " :evidence_scope :adapter must name a concrete adapter")])
          (when (and (contains? scope :adapter_version)
                     (some? (:adapter_version scope))
                     (not (concrete-adapter? (:adapter_version scope))))
            [(str "AAT parser-IR compatibility registry entry " idx
                  " :evidence_scope :adapter_version must be null or a concrete adapter version")])
          (when (and (contains? scope :corpus)
                     (not (nonblank-string? (:corpus scope))))
            [(str "AAT parser-IR compatibility registry entry " idx
                  " :evidence_scope :corpus must be a non-empty string")])
          (when (and (contains? scope :files_scanned)
                     (not (positive-int? (:files_scanned scope))))
            [(str "AAT parser-IR compatibility registry entry " idx
                  " :evidence_scope :files_scanned must be a positive integer")])
          (when (and (contains? scope :files_with_unsupported)
                     (not (nonnegative-int? (:files_with_unsupported scope))))
            [(str "AAT parser-IR compatibility registry entry " idx
                  " :evidence_scope :files_with_unsupported must be a non-negative integer")])
          (when (and (contains? scope :generated_rules)
                     (not (positive-int? (:generated_rules scope))))
            [(str "AAT parser-IR compatibility registry entry " idx
                  " :evidence_scope :generated_rules must be a positive integer")])
          (for [k required-conversion-audit-evidence-keys
                :when (and (contains? scope k)
                           (not (nonnegative-int? (get scope k))))]
            (str "AAT parser-IR compatibility registry entry " idx
                 " :evidence_scope " k " must be a non-negative integer"))
          (when (and (= :conversion-audit evidence-type)
                     (every? #(contains? scope %)
                             [:files_scanned :files_succeeded :files_failed])
                     (not= (:files_scanned scope)
                           (+ (:files_succeeded scope) (:files_failed scope))))
            [(str "AAT parser-IR compatibility registry entry " idx
                  " :evidence_scope files_scanned must equal files_succeeded plus files_failed")])
          (when (and (= :conversion-audit evidence-type)
                     (every? #(contains? scope %)
                             [:rules_total :rules_emitted :rules_missing])
                     (not= (:rules_total scope)
                           (+ (:rules_emitted scope) (:rules_missing scope))))
            [(str "AAT parser-IR compatibility registry entry " idx
                  " :evidence_scope rules_total must equal rules_emitted plus rules_missing")])))))))

(defn- entry-errors
  [idx entry]
  (if-not (map? entry)
    [(str "AAT parser-IR compatibility registry entry " idx
          " must be a map")]
    (vec
     (concat
      (missing-key-errors idx entry)
      (when (and (contains? entry :aat_version)
                 (not (positive-int? (:aat_version entry))))
        [(str "AAT parser-IR compatibility registry entry " idx
              " :aat_version must be a positive integer")])
      (when (and (contains? entry :aat_adapter)
                 (not (concrete-adapter? (:aat_adapter entry))))
        [(str "AAT parser-IR compatibility registry entry " idx
              " :aat_adapter must name a concrete adapter")])
      (when (and (contains? entry :aat_adapter_version)
                 (some? (:aat_adapter_version entry))
                 (not (concrete-adapter? (:aat_adapter_version entry))))
        [(str "AAT parser-IR compatibility registry entry " idx
              " :aat_adapter_version must be null or a concrete adapter version")])
      (for [k [:mapping_id :mapping_version :parser_ir_schema_id :compatibility]
            :when (and (contains? entry k)
                       (not (nonblank-string? (get entry k))))]
        (str "AAT parser-IR compatibility registry entry " idx
             " " k " must be a non-empty string"))
      (when (and (contains? entry :mapping_version)
                 (nonblank-string? (:mapping_version entry))
                 (not (re-matches #"^[0-9]+\.[0-9]+\.[0-9]+$"
                                  (:mapping_version entry))))
        [(str "AAT parser-IR compatibility registry entry " idx
              " :mapping_version must be semver")])
      (when (and (contains? entry :compatibility)
                 (not (#{"lossy" "lossless"} (:compatibility entry))))
        [(str "AAT parser-IR compatibility registry entry " idx
              " :compatibility must be lossy or lossless")])
      (for [k hash-keys
            :when (and (contains? entry k)
                       (not (sha256-hash? (get entry k))))]
        (str "AAT parser-IR compatibility registry entry " idx
             " " k " must be a sha256 hash"))
      (when (contains? entry :evidence_scope)
        (evidence-scope-errors idx entry))))))

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
  (vec
   (cond
     (not (map? registry))
     ["AAT parser-IR compatibility registry must be an EDN map"]

     (not (contains? registry :entries))
     ["AAT parser-IR compatibility registry is missing :entries"]

     (not (vector? (:entries registry)))
     ["AAT parser-IR compatibility registry :entries must be a vector"]

     (empty? (:entries registry))
     ["AAT parser-IR compatibility registry :entries must not be empty"]

     :else
     (concat
      (mapcat (fn [[idx entry]] (entry-errors idx entry))
              (map-indexed vector (:entries registry)))
      (duplicate-key-errors (:entries registry))))))

(defn validate-registry!
  [registry]
  (let [errors (registry-errors registry)]
    (when (seq errors)
      (throw (ex-info (string/join "\n" errors)
                      {:errors errors})))
    :ok))

(defn load-registry
  []
  (let [registry (edn/read-string (slurp registry-path))]
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

(defn- read-edn-file
  [path]
  (edn/read-string (slurp path)))

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
                    (read-edn-file (:registry options))
                    (read-edn-file (:candidates options)))]
        (prn report)
        (when-not (= :admitted (:status report))
          (System/exit 1))))))
