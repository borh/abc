(ns abc.tools.aat-parser-ir-compat
  (:require [abc.tools.files :as files]
            [clojure.edn :as edn]
            [clojure.string :as string]))

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

(def required-evidence-scope-keys
  [:adapter
   :corpus
   :files_scanned
   :files_with_unsupported
   :generated_rules])

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
      (vec
       (concat
        (->> required-evidence-scope-keys
             (remove #(contains? scope %))
             (map #(str "AAT parser-IR compatibility registry entry " idx
                        " :evidence_scope is missing " %)))
        (when (and (contains? scope :adapter)
                   (not= (:aat_adapter entry) (:adapter scope)))
          [(str "AAT parser-IR compatibility registry entry " idx
                " :evidence_scope :adapter must equal :aat_adapter")])
        (when (and (contains? scope :adapter)
                   (not (concrete-adapter? (:adapter scope))))
          [(str "AAT parser-IR compatibility registry entry " idx
                " :evidence_scope :adapter must name a concrete adapter")])
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
                " :evidence_scope :generated_rules must be a positive integer")]))))))

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
