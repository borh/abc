(ns abc.tools.malli
  "Single-place validation foundation. `install!` composes registries
  from the project's registry-owning namespaces, publishes the result
  as malli's default registry, and instruments every `m/=>` and
  `mx/defn` contract.

  Tests and the focused-test alias call `install!` exactly once, *after*
  every namespace that declares schemas has been loaded. Namespaces
  themselves stay side-effect-free at load time."
  (:require [abc.tools.json :as abc-json]
            [clojure.string :as string]
            [malli.core :as m]
            [malli.error :as me]
            [malli.instrument :as mi]
            [malli.registry :as mr]))

(def ^:private project-namespaces
  "Registry-owning namespaces, in declared merge order."
  '[abc.annotation.schema])

(defn- compose-project-registry []
  (reduce
   (fn [acc ns-sym]
     (require ns-sym)
     (let [v (some-> (resolve (symbol (name ns-sym) "registry")) deref)]
       (cond-> acc (map? v) (merge v))))
   {}
   project-namespaces))

(def ^:private example-hash
  "sha256:bf0910f5316efc2cd528f504c0cbd16816ca993e7ccb2b99122aab8a71359527")

(def ^:private concrete-adapter-placeholders
  #{"*" "all" "any" "<any>" "adapter-neutral"})

(defn- nonblank-string? [value]
  (and (string? value)
       (not (string/blank? value))))

(defn- concrete-adapter? [value]
  (and (nonblank-string? value)
       (not (contains? concrete-adapter-placeholders
                       (string/lower-case value)))))

(defn- workspace-logical-path? [value]
  (and (nonblank-string? value)
       (not (string/starts-with? value "../"))
       (not (string/starts-with? value "/"))))

(def ^:private parser-evidence-examples
  "Several valid entries covering each evidence_class / status enum and the
  present / absent / nil variants of :current_external_path, so the schema's
  generator produces varied (not constant) values."
  [{:evidence_id "ab-validator/example"
    :evidence_class :conversion-compatibility
    :producer_component "ab-validator"
    :logical_path "ab-validator/docs/example.md"
    :current_external_path "docs/example.md"
    :sha256 example-hash
    :status :citable
    :summary "Example evidence."}
   {:evidence_id "ab-validator/selection"
    :evidence_class :parser-selection
    :producer_component "ab-validator"
    :logical_path "ab-validator/docs/selection.md"
    :sha256 example-hash
    :status :provisional
    :summary "Parser selection evidence."}
   {:evidence_id "ab-validator/oracle"
    :evidence_class :comparator-oracle
    :producer_component "ab-validator"
    :logical_path "ab-validator/docs/oracle.md"
    :current_external_path nil
    :sha256 example-hash
    :status :superseded
    :summary "Comparator oracle evidence."}])

(def ^:private compat-entry-example
  {:aat_version 1
   :aat_adapter "aozora2html"
   :aat_adapter_version "aozora2html-adapter 0.1.0 gem-3.0.1"
   :mapping_id "https://w3id.org/abc/mappings/aat-to-parser-ir/v1"
   :mapping_version "0.2.3"
   :mapping_hash example-hash
   :mapping_schema_hash example-hash
   :parser_ir_schema_id "https://w3id.org/abc/schemas/parser-ir.schema.json"
   :parser_ir_schema_hash example-hash
   :compatibility "lossy"
   :evidence_scope {:adapter "aozora2html"
                    :adapter_version "aozora2html-adapter 0.1.0 gem-3.0.1"
                    :corpus "fixture"
                    :evidence_type :conversion-audit
                    :files_scanned 2
                    :files_succeeded 1
                    :files_failed 1
                    :parser_ir_nodes 10
                    :divergence_records 3
                    :divergence_occurrences 4
                    :rules_total 5
                    :rules_emitted 2
                    :rules_missing 3
                    :unsupported_occurrences 0}})

(def contract-schemas
  {::nonblank-string
   [:fn {:error/message "must be a non-empty string"
         :gen/elements ["abc"]}
    nonblank-string?]

   ::nullable-nonblank-string
   [:fn {:error/message "must be null or a non-empty string"
         :gen/elements [nil "abc"]}
    (fn [value]
      (or (nil? value) (nonblank-string? value)))]

   ::sha256-hash
   [:re {:error/message "must be a sha256 hash"
         :gen/elements [example-hash]}
    #"^sha256:[0-9a-f]{64}$"]

   ::semver
   [:re {:error/message "must be semver"
         :gen/elements ["0.2.3"]}
    #"^[0-9]+\.[0-9]+\.[0-9]+$"]

   ::positive-int
   [:int {:error/message "must be a positive integer"
          :min 1
          :gen/elements [1]}]

   ::nonnegative-int
   [:int {:error/message "must be a non-negative integer"
          :min 0
          :gen/elements [0 1]}]

   ::workspace-logical-path
   [:fn {:error/message "must be workspace-relative and must not start with ../ or /"
         :gen/elements ["ab-validator/docs/example.md"]}
    workspace-logical-path?]

   ::concrete-adapter
   [:fn {:error/message "must name a concrete adapter"
         :gen/elements ["aozora2html"]}
    concrete-adapter?]

   ::parser-evidence-entry
   [:map {:gen/elements parser-evidence-examples}
    [:evidence_id ::nonblank-string]
    [:evidence_class [:enum {:error/message "must be conversion-compatibility, parser-selection, or comparator-oracle"}
                      :conversion-compatibility :parser-selection :comparator-oracle]]
    [:producer_component ::nonblank-string]
    [:logical_path ::workspace-logical-path]
    [:current_external_path {:optional true} ::nullable-nonblank-string]
    [:sha256 ::sha256-hash]
    [:status [:enum {:error/message "must be citable, provisional, or superseded"}
              :citable :provisional :superseded]]
    [:summary ::nonblank-string]]

   ::aat-parser-ir-evidence-scope
   [:multi {:dispatch :evidence_type}
    [:mapping-generation
     [:map
      [:adapter ::concrete-adapter]
      [:adapter_version {:optional true} ::nullable-nonblank-string]
      [:corpus ::nonblank-string]
      [:evidence_type [:= {:error/message "must be mapping-generation or conversion-audit"}
                       :mapping-generation]]
      [:files_scanned ::positive-int]
      [:files_with_unsupported ::nonnegative-int]
      [:generated_rules ::positive-int]]]
    [:conversion-audit
     [:map
      [:adapter ::concrete-adapter]
      [:adapter_version {:optional true} ::nullable-nonblank-string]
      [:corpus ::nonblank-string]
      [:evidence_type [:= {:error/message "must be mapping-generation or conversion-audit"}
                       :conversion-audit]]
      [:files_scanned ::positive-int]
      [:files_succeeded ::nonnegative-int]
      [:files_failed ::nonnegative-int]
      [:parser_ir_nodes ::nonnegative-int]
      [:divergence_records ::nonnegative-int]
      [:divergence_occurrences ::nonnegative-int]
      [:rules_total ::nonnegative-int]
      [:rules_emitted ::nonnegative-int]
      [:rules_missing ::nonnegative-int]
      [:unsupported_occurrences ::nonnegative-int]]]]

   ::aat-parser-ir-compat-entry
   [:and {:gen/elements [compat-entry-example]}
    [:map
     [:aat_version ::positive-int]
     [:aat_adapter ::concrete-adapter]
     [:aat_adapter_version {:optional true} ::nullable-nonblank-string]
     [:mapping_id ::nonblank-string]
     [:mapping_version ::semver]
     [:mapping_hash ::sha256-hash]
     [:mapping_schema_hash ::sha256-hash]
     [:parser_ir_schema_id ::nonblank-string]
     [:parser_ir_schema_hash ::sha256-hash]
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

(def design-bundle-schemas
  "Cross-event invariants for the design-bundle artifacts that JSON
  Schema cannot express. Composed into the default registry by
  `install!` so any caller can drive validation through
  `explain-or-throw!`."
  {::manifest-inputs
   [:and
    [:map-of :string :any]
    [:fn {:error/message "manifest inputs missing required keys"}
     (fn [m]
       (every? #(contains? m %)
               ["producer" "producer_version" "work_id"
                "corpus_snapshot_hash" "work_content_hash"
                "parser_build_hash" "parser_config_hash"
                "mapping_hash"
                "parser_ir_schema_hash" "diagnostic_schema_hash"
                "warning_sidecar_hash" "run_summary_hash"
                "comparison_report_hash"]))]
    [:fn {:error/message "every *_hash key must be a sha256: hash"}
     (fn [m]
       (every? (fn [[k v]]
                 (or (not (string/ends-with? (str k) "_hash"))
                     (and (string? v)
                          (re-matches #"^sha256:[0-9a-f]{64}$" v))))
               m))]]

   ::run-summary-event
   [:map-of :string :any]

   ::run-summary-events
   [:and
    [:vector ::run-summary-event]
    [:fn {:error/message "run summary must contain exactly one run-start event"}
     (fn [es] (= 1 (count (filter #(= "run-start" (get % "event")) es))))]
    [:fn {:error/message "run summary must contain exactly one run-complete event"}
     (fn [es] (= 1 (count (filter #(= "run-complete" (get % "event")) es))))]
    [:fn {:error/message "run summary must start with run-start"}
     (fn [es] (= "run-start" (get (first es) "event")))]
    [:fn {:error/message "run summary must end with run-complete"}
     (fn [es] (= "run-complete" (get (last es) "event")))]
    [:fn {:error/message "every run summary event must include run_id"}
     (fn [es] (every? #(contains? % "run_id") es))]
    [:fn {:error/message "all run summary events must share one run_id"}
     (fn [es] (<= (count (set (keep #(get % "run_id") es))) 1))]]

   ::comparison-report
   [:and
    [:map-of :string :any]
    [:fn {:error/message "comparison report has unexpected report_schema"}
     (fn [r] (= "abc.ab-validator-comparison.v0" (get r "report_schema")))]
    [:fn {:error/message "comparison report must list parser_candidates"}
     (fn [r] (seq (get r "parser_candidates")))]]})

(defn install!
  "Idempotent. Requires the project's registry-owning namespaces in
  declared order, composes their `registry` values plus contract
  schemas and design-bundle :fn schemas, publishes the composite as
  malli's default registry, then instruments every registered function
  schema. Returns the composite map."
  []
  (let [composite (merge (compose-project-registry)
                         contract-schemas
                         design-bundle-schemas)]
    (mr/set-default-registry!
     (mr/composite-registry (m/default-schemas) composite))
    (mi/instrument!)
    composite))

(defn explain-contract
  "Explain `value` against a schema owned by `contract-schemas` without
  requiring callers to install the global Malli registry."
  [schema-key value]
  (m/explain schema-key
             value
             {:registry (mr/composite-registry (m/default-schemas)
                                               contract-schemas)}))

(let [cache (atom {})]
  (defn cached-schema
    "Read and parse the JSON Schema at `path` exactly once per JVM.
    Identity-stable: callers can compare with `identical?`."
    [path]
    (or (get @cache path)
        (let [v (abc-json/read-json-file path)]
          (swap! cache assoc path v)
          v)))

  (defn cached-schema-hash
    "Compute and cache the schema-bytes hash for `path`. Delegates to
    `abc.tools.manifest/schema-hash` so the on-disk hash contract is
    preserved exactly."
    [path]
    (or (get @cache [::hash path])
        (let [schema-hash-fn (requiring-resolve 'abc.tools.manifest/schema-hash)
              v (schema-hash-fn path)]
          (swap! cache assoc [::hash path] v)
          v))))

(defn- m3-leaf-errors
  "Walk an m3 error tree. m3 nests errors via `:errors`; leaves carry
  `:document-path`, `:schema-path`, and `:message`. Yields a flat seq
  of leaf maps (descending into `:errors` when present, ignoring
  intermediate composite-schema messages)."
  [node]
  (cond
    (sequential? node) (mapcat m3-leaf-errors node)
    (and (map? node) (seq (:errors node))) (mapcat m3-leaf-errors (:errors node))
    (map? node) [(select-keys node [:document-path :schema-path :message])]
    :else nil))

(defn humanize-validation-errors
  "Format an m3 error vector into a flat sequence of readable strings.
  Returns an empty vector when `errors` is nil or empty."
  [errors]
  (->> (m3-leaf-errors errors)
       (mapv (fn [{:keys [document-path message]}]
               (let [path (when (seq document-path)
                            (string/join "/" (map str document-path)))]
                 (cond
                   (and path message) (str path ": " message)
                   message message
                   path path
                   :else (pr-str document-path)))))))

(defn explanation-messages
  "Return stable message strings from a Malli explanation. Prefer explicit
  :error/message values from schemas; fall back to the path when a Malli
  primitive emits no custom message."
  [explanation]
  (->> (:errors explanation)
       (mapv (fn [{:keys [in message path properties schema]}]
               (let [schema-message (or message
                                        (:error/message properties)
                                        (some-> schema m/properties :error/message))
                     location (seq (or in path))
                     location-label (when location
                                      (string/join " " (map str location)))]
                 (cond
                   (and location-label schema-message)
                   (str location-label " " schema-message)

                   schema-message
                   schema-message

                   location-label
                   (str location-label " is invalid")

                   :else
                   "value is invalid"))))))

(defn explain-or-throw!
  "Validate `value` against `schema-key` using malli's default registry.
  On failure, throws ex-info whose **message embeds the humanized
  errors** (so `thrown-with-msg?` regexes match) and whose ex-data
  carries `:errors-humanized` (vector of strings), `:label`, and the
  raw `:explanation`. Returns `:ok` on success."
  [schema-key value label]
  (if-let [explanation (m/explain schema-key value)]
    (let [humanized (->> (me/humanize explanation)
                         (tree-seq coll? seq)
                         (filter string?)
                         vec)]
      (throw (ex-info (str label " failed malli validation: "
                           (string/join "; " humanized))
                      {:label label
                       :errors-humanized humanized
                       :explanation explanation})))
    :ok))
