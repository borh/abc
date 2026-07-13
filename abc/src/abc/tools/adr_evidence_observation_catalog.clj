(ns abc.tools.adr-evidence-observation-catalog
  (:require [abc.tools.adr-evidence-runtime-inputs :as runtime-inputs]
            [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.jcs :as jcs]
            [abc.tools.path-containment :as containment]
            [babashka.fs :as fs]
            [clojure.set :as set]
            [clojure.string :as str]))

(def ^:private catalog-keys
  #{:schema-version :focused-observations :operational-observations})
(def ^:private focused-keys
  #{:observation-id :descriptor-stem :observation-key :focus-var})
(def ^:private operational-base-keys
  #{:observation-id :descriptor-stem :observation-key :command-id :tool :argv
    :environment-policy :entrypoint-kind :determinant-paths})
(def ^:private clojure-operational-keys
  (conj operational-base-keys :entrypoint-namespaces))
(def ^:private binding-keys #{:claim-id :observation-id})

(defn- problem [kind message & {:as data}]
  (merge {:kind kind :message message} data))

(defn- sorted-problems [problems]
  (->> problems (sort-by pr-str) vec))

(defn- unqualified-keyword? [value]
  (and (keyword? value) (nil? (namespace value))))

(defn- nonempty-string? [value]
  (and (string? value) (not (str/blank? value))))

(defn- qualified-var-symbol? [value]
  (and (symbol? value) (some? (namespace value))))

(defn- namespace-symbol? [value]
  (and (symbol? value) (nil? (namespace value))))

(defn- sorted-unique-vector? [value predicate]
  (and (vector? value)
       (every? predicate value)
       (= (count value) (count (distinct value)))
       (try
         (= value (vec (sort value)))
         (catch ClassCastException _ false))))

(defn- row-order [rows]
  (mapv (comp pr-str :observation-id) rows))

(defn- normalized-coordinate [path]
  (when (string? path)
    (try
      (-> (fs/path path) fs/normalize str (str/replace "\\" "/"))
      (catch java.nio.file.InvalidPathException _ nil))))

(defn observation-rows [catalog]
  (vec (concat (:focused-observations catalog)
               (:operational-observations catalog))))

(defn- exact-keys-problems [row expected kind index]
  (when-not (= expected (set (keys row)))
    [(problem kind "observation row must have the exact key set"
              :index index
              :missing-keys (vec (sort-by pr-str
                                          (set/difference expected (set (keys row)))))
              :unknown-keys (vec (sort-by pr-str
                                          (set/difference (set (keys row)) expected))))]))

(defn- focused-row-problems [row index]
  (if-not (map? row)
    [(problem :invalid-focused-observation "focused observation must be a map"
              :index index)]
    (concat
     (exact-keys-problems row focused-keys :invalid-focused-observation index)
     (when-not (unqualified-keyword? (:observation-id row))
       [(problem :invalid-focused-observation "observation ID must be an unqualified keyword"
                 :index index)])
     (when-not (nonempty-string? (:descriptor-stem row))
       [(problem :invalid-focused-observation "descriptor stem must be a nonempty string"
                 :index index)])
     (when-not (nonempty-string? (:observation-key row))
       [(problem :invalid-focused-observation "observation key must be a nonempty string"
                 :index index)])
     (when-not (qualified-var-symbol? (:focus-var row))
       [(problem :invalid-focused-observation "focus Var must be a qualified symbol"
                 :index index)]))))

(defn- determinant-problems [repo-root row index]
  (mapcat
   (fn [path]
     (let [state (when (nonempty-string? path)
                   (containment/path-state repo-root path))]
       (when-not (and state
                      (= path (normalized-coordinate path))
                      (= :ok (:state state))
                      (fs/regular-file? (:path state)))
         [(problem :invalid-operational-determinant
                   "operational determinant must be a contained regular file"
                   :index index :path path :state (:state state))])))
   (if (vector? (:determinant-paths row)) (:determinant-paths row) [])))

(defn- operational-row-problems [repo-root row index]
  (if-not (map? row)
    [(problem :invalid-operational-observation "operational observation must be a map"
              :index index)]
    (let [clojure? (= :clojure (:entrypoint-kind row))
          expected (if clojure? clojure-operational-keys operational-base-keys)]
      (concat
       (exact-keys-problems row expected :invalid-operational-observation index)
       (when-not (unqualified-keyword? (:observation-id row))
         [(problem :invalid-operational-observation
                   "observation ID must be an unqualified keyword" :index index)])
       (when-not (nonempty-string? (:descriptor-stem row))
         [(problem :invalid-operational-observation
                   "descriptor stem must be a nonempty string" :index index)])
       (when-not (nonempty-string? (:observation-key row))
         [(problem :invalid-operational-observation
                   "observation key must be a nonempty string" :index index)])
       (when-not (unqualified-keyword? (:command-id row))
         [(problem :invalid-operational-observation
                   "command ID must be an unqualified keyword" :index index)])
       (when-not (and (nonempty-string? (:tool row))
                      (vector? (:argv row))
                      (seq (:argv row))
                      (every? nonempty-string? (:argv row))
                      (= (:tool row) (first (:argv row))))
         [(problem :invalid-operational-observation
                   "argv must be a nonempty string vector beginning with tool"
                   :index index)])
       (when-not (= :nix-local-v1 (:environment-policy row))
         [(problem :invalid-operational-observation
                   "environment policy is unsupported" :index index)])
       (when-not (#{:clojure :nix-only} (:entrypoint-kind row))
         [(problem :invalid-operational-observation
                   "entrypoint kind is unsupported" :index index)])
       (when (and clojure?
                  (not (and (sorted-unique-vector? (:entrypoint-namespaces row)
                                                   namespace-symbol?)
                            (seq (:entrypoint-namespaces row)))))
         [(problem :unsorted-operational-collection
                   "entrypoint namespaces must be a nonempty sorted unique symbol vector"
                   :index index :field :entrypoint-namespaces)])
       (when-not (sorted-unique-vector? (:determinant-paths row) nonempty-string?)
         [(problem :unsorted-operational-collection
                   "determinant paths must be a sorted unique string vector"
                   :index index :field :determinant-paths)])
       (determinant-problems repo-root row index)))))

(defn- duplicate-problems [rows field kind]
  (for [[value count] (frequencies (keep field rows))
        :when (> count 1)]
    (problem kind "catalog identity must be unique" field value :count count)))

(defn catalog-problems [repo-root catalog]
  (if-not (map? catalog)
    [(problem :invalid-observation-catalog "observation catalog must be a map")]
    (let [focused (if (vector? (:focused-observations catalog))
                    (:focused-observations catalog) [])
          operational (if (vector? (:operational-observations catalog))
                        (:operational-observations catalog) [])
          rows (vec (concat focused operational))]
      (sorted-problems
       (concat
        (when-not (= catalog-keys (set (keys catalog)))
          [(problem :invalid-observation-catalog
                    "observation catalog must have the exact key set")])
        (when-not (= :abc-adr-evidence-observation-catalog-v1
                     (:schema-version catalog))
          [(problem :invalid-observation-catalog
                    "observation catalog schema version is unsupported")])
        (when-not (vector? (:focused-observations catalog))
          [(problem :invalid-observation-catalog
                    "focused observations must be a vector")])
        (when-not (vector? (:operational-observations catalog))
          [(problem :invalid-observation-catalog
                    "operational observations must be a vector")])
        (mapcat #(focused-row-problems %1 %2) focused (range))
        (mapcat #(operational-row-problems repo-root %1 %2) operational (range))
        (when-not (= (row-order focused) (vec (sort (row-order focused))))
          [(problem :unsorted-observations "focused observations must be sorted by ID")])
        (when-not (= (row-order operational) (vec (sort (row-order operational))))
          [(problem :unsorted-observations "operational observations must be sorted by ID")])
        (duplicate-problems rows :observation-id :duplicate-observation-id)
        (duplicate-problems rows :descriptor-stem :duplicate-descriptor-stem)
        (duplicate-problems rows :observation-key :duplicate-observation-key)
        (duplicate-problems focused :focus-var :duplicate-focus-var)
        (duplicate-problems operational :command-id :duplicate-command-id))))))

(defn load-catalog! [repo-root catalog-path]
  (let [state (when (nonempty-string? catalog-path)
                (containment/path-state repo-root catalog-path))]
    (when-not (and state (= :ok (:state state)) (fs/regular-file? (:path state)))
      (throw (ex-info "observation catalog path is not a contained regular file"
                      {:problems [(problem :invalid-observation-catalog-path
                                           "catalog path is missing or escapes the repository"
                                           :path catalog-path :state (:state state))]})))
    (let [catalog (try
                    (files/read-edn (:path state))
                    (catch Exception exception
                      (throw (ex-info "observation catalog is unreadable"
                                      {:problems [(problem :invalid-observation-catalog
                                                           "catalog EDN is unreadable"
                                                           :path catalog-path)]}
                                      exception))))
          problems (catalog-problems repo-root catalog)]
      (when (seq problems)
        (throw (ex-info "observation catalog is invalid" {:problems problems})))
      catalog)))

(defn observation-contract-json-value [row]
  (if (:focus-var row)
    {"schema_version" "abc-adr-evidence-observation-contract-v1"
     "observation_type" "focused"
     "observation_id" (name (:observation-id row))
     "descriptor_stem" (:descriptor-stem row)
     "observation_key" (:observation-key row)
     "focus_var" (str (:focus-var row))}
    (cond->
     {"schema_version" "abc-adr-evidence-observation-contract-v1"
      "observation_type" "operational"
      "observation_id" (name (:observation-id row))
      "descriptor_stem" (:descriptor-stem row)
      "observation_key" (:observation-key row)
      "command_id" (name (:command-id row))
      "tool" (:tool row)
      "argv" (:argv row)
      "environment_policy" (name (:environment-policy row))
      "entrypoint_kind" (name (:entrypoint-kind row))
      "determinant_paths" (:determinant-paths row)}
      (= :clojure (:entrypoint-kind row))
      (assoc "entrypoint_namespaces" (mapv str (:entrypoint-namespaces row))))))

(defn observation-contract-sha256 [row]
  (-> row
      observation-contract-json-value
      jcs/rfc8785-string-domain-json-bytes
      hash/sha256-bytes
      hash/format-sha256))

(defn validate-bindings [catalog template]
  (let [rows (observation-rows catalog)
        known (set (map :observation-id rows))
        entries (if (vector? (:entries template)) (:entries template) [])
        identities (mapv (juxt :claim-id :observation-id) entries)
        selected (set (keep :observation-id entries))]
    (sorted-problems
     (concat
      (when-not (and (map? template) (vector? (:entries template)))
        [(problem :invalid-observation-bindings "binding template must contain an entries vector")])
      (mapcat
       (fn [[index entry]]
         (concat
          (when-not (and (map? entry)
                         (set/subset? binding-keys (set (keys entry)))
                         (nonempty-string? (:claim-id entry))
                         (unqualified-keyword? (:observation-id entry)))
            [(problem :invalid-observation-binding "binding has invalid join coordinates"
                      :index index)])
          (when (and (map? entry)
                     (unqualified-keyword? (:observation-id entry))
                     (not (contains? known (:observation-id entry))))
            [(problem :unknown-binding-observation "binding selects an unknown observation"
                      :index index :observation-id (:observation-id entry))])))
       (map-indexed vector entries))
      (for [[identity count] (frequencies identities) :when (> count 1)]
        (problem :duplicate-observation-binding
                 "claim and observation binding identity must be unique"
                 :identity identity :count count))
      (for [observation-id (sort (set/difference known selected))]
        (problem :unbound-catalog-observation "catalog observation has no claim binding"
                 :observation-id observation-id))))))

(def ^:private finding-identity-keys
  {:duplicate-var-definition
   {:allowed #{:kind :focus-var :target :path}
    :required #{:kind :focus-var :target}}
   :focused-var-not-deftest
   {:allowed #{:kind :focus-var :target :path}
    :required #{:kind :focus-var :target}}
   :forbidden-evidence-capability
   {:allowed #{:kind :focus-var :target :caller :path}
    :required #{:kind :focus-var :target}}
   :forbidden-evidence-io
   {:allowed #{:kind :focus-var :target :caller :path}
    :required #{:kind :focus-var :target}}
   :invalid-focused-evidence-analysis
   {:allowed #{:kind :focus-var :path}
    :required #{:kind :focus-var}}
   :invalid-higher-order-contract
   {:allowed #{:kind :focus-var :target :caller :parameter :path}
    :required #{:kind :focus-var :caller}}
   :missing-evidence-boundary-owner
   {:allowed #{:kind :focus-var}
    :required #{:kind :focus-var}}
   :reader-kondo-span-mismatch
   {:allowed #{:kind :focus-var :caller :parameter :head :matches :path}
    :required #{:kind :focus-var :caller}
    :required-any #{:parameter :head}}
   :unregistered-higher-order-call
   {:allowed #{:kind :focus-var :caller :path}
    :required #{:kind :focus-var :caller}}
   :unresolved-call-edge
   {:allowed #{:kind :focus-var :target :path}
    :required #{:kind :focus-var :target}}
   :unresolved-focused-var
   {:allowed #{:kind :focus-var :target :path}
    :required #{:kind :focus-var :target}}
   :unsupported-evidence-call-graph
   {:allowed #{:kind :focus-var :target :caller :parameter :head :path}
    :required #{:kind :focus-var}
    :required-any #{:target :caller :parameter :head}}})

(def ^:private finding-key-order
  [:kind :focus-var :target :caller :parameter :head :matches :path])

(defn- normalized-finding-path [path]
  (let [machine-local? (and (string? path)
                            (or (re-find #"(?i)^[a-z]:" path)
                                (str/starts-with? path "\\\\")
                                (str/starts-with? path "//")))
        supplied (when (and (string? path) (not machine-local?)) (fs/path path))
        normalized (when-not machine-local? (normalized-coordinate path))]
    (when-not (and (not machine-local?)
                   supplied
                   (not (fs/absolute? supplied))
                   (nonempty-string? normalized)
                   (not (or (= ".." normalized)
                            (str/starts-with? normalized "../"))))
      (throw (ex-info "conformance finding path must normalize repository-relatively"
                      {:kind :invalid-conformance-finding :path path})))
    normalized))

(defn- valid-finding-coordinate? [key value]
  (case key
    :kind (keyword? value)
    :focus-var (qualified-var-symbol? value)
    :target (symbol? value)
    :caller (qualified-var-symbol? value)
    :parameter (symbol? value)
    :head (symbol? value)
    :matches (nat-int? value)
    :path (nonempty-string? value)
    false))

(defn- normalize-finding [finding]
  (let [kind (:kind finding)
        schema (get finding-identity-keys kind)]
    (when-not schema
      (throw (ex-info "conformance finding kind has no stable identity projection"
                      {:kind :invalid-conformance-finding
                       :finding-kind kind})))
    (let [finding (cond-> finding
                    (:var finding) (assoc :target (:var finding))
                    (:path finding) (update :path normalized-finding-path))
          present (set (keys finding))
          required (:required schema)
          required-any (:required-any schema)
          selected (select-keys finding (:allowed schema))]
      (when-not (and (set/subset? required present)
                     (or (nil? required-any)
                         (seq (set/intersection required-any present)))
                     (every? (fn [[key value]]
                               (valid-finding-coordinate? key value))
                             selected))
        (throw (ex-info "conformance finding has invalid semantic coordinates"
                        {:kind :invalid-conformance-finding
                         :finding-kind kind})))
      (into (array-map)
            (keep (fn [key]
                    (when (contains? selected key)
                      [key (get finding key)])))
            finding-key-order))))

(defn normalize-conformance-findings [findings]
  (->> findings
       (mapcat (fn [finding]
                 (if (seq (:vars finding))
                   (map #(-> finding
                             (dissoc :vars)
                             (assoc :focus-var %))
                        (:vars finding))
                   [finding])))
       (map normalize-finding)
       distinct
       (sort-by pr-str)
       vec))

(def ^:private focused-boundary-owner
  'abc.tools.adr-evidence-runtime-inputs/with-validated-read-trace!)

(defn- qvar [definition]
  (symbol (str (:ns definition)) (str (:name definition))))

(defn- direct-boundary-owner? [repo-root analysis focus-var]
  (let [definitions (group-by qvar (get-in analysis [:analysis :var-definitions]))
        definition (first (get definitions focus-var))
        forms (runtime-inputs/read-source-forms! (fs/file repo-root (:filename definition)))
        form (some #(when (and (seq? %) (= 'deftest (first %))
                               (= (:name definition) (second %))) %)
                   forms)
        usages (get-in analysis [:analysis :var-usages])]
    (boolean
     (some
      (fn [body-form]
        (when (and (seq? body-form) (symbol? (first body-form)))
          (let [head (first body-form)]
            (some #(and (= (:filename definition) (:filename %))
                        (= (:name definition) (:from-var %))
                        (= (namespace focused-boundary-owner) (str (:to %)))
                        (= (name focused-boundary-owner) (str (:name %)))
                        (= [(:line (meta head)) (:column (meta head))]
                           [(:name-row %) (:name-col %)]))
                  usages))))
      (drop 2 form)))))

(defn focused-conformance-findings [repo-root catalog]
  (let [analysis-var (ns-resolve 'abc.tools.adr-evidence-runtime-inputs
                                 'kondo-analysis!)
        original-analysis @analysis-var
        analysis (original-analysis (fs/file (fs/canonicalize repo-root)))]
    (with-redefs-fn
      {analysis-var (constantly analysis)}
      #(->> (:focused-observations catalog)
            (mapcat
             (fn [{:keys [focus-var]}]
               (try
                 (runtime-inputs/validate-focused-deftests! repo-root [focus-var])
                 (let [ownership (when-not (direct-boundary-owner?
                                            repo-root analysis focus-var)
                                   {:kind :missing-evidence-boundary-owner
                                    :focus-var focus-var})
                       analysis-finding
                       (try
                         (runtime-inputs/analyze-reachable-vars repo-root [focus-var])
                         nil
                         (catch clojure.lang.ExceptionInfo exception
                           (assoc (ex-data exception) :focus-var focus-var)))]
                   (remove nil? [ownership analysis-finding]))
                 (catch clojure.lang.ExceptionInfo exception
                   [(assoc (ex-data exception) :focus-var focus-var)]))))
            vec))))
