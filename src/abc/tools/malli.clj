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
  "Registry-owning namespaces, in declared merge order. The Nix
  focused-test sandbox deliberately omits the deps for some of these
  (e.g. java-time for abc.aozora); `install!` skips a namespace whose
  require fails so the foundation still works there."
  '[abc.annotation.schema abc.aozora abc.tei])

(defn- ns-loadable? [ns-sym]
  ;; A transitive `:require` failure inside the loaded namespace is
  ;; wrapped as Compiler$CompilerException whose cause is the inner
  ;; FileNotFoundException, so checking the cause chain is the
  ;; reliable way to skip Nix-sandbox-omitted namespaces.
  (try (require ns-sym) true
       (catch Throwable t
         (loop [cause t]
           (cond
             (nil? cause) (throw t)
             (instance? java.io.FileNotFoundException cause) false
             :else (recur (.getCause cause)))))))

(defn- compose-project-registry []
  (reduce
   (fn [acc ns-sym]
     (let [v (when (ns-loadable? ns-sym)
               (some-> (resolve (symbol (name ns-sym) "registry")) deref))]
       (cond-> acc (map? v) (merge v))))
   {}
   project-namespaces))

;; Placeholder; Task 6 replaces this `def` with the design-bundle :fn
;; schemas. Defined here so `install!` is loadable on its own.
(def design-bundle-schemas {})

(defn install!
  "Idempotent. Requires the project's registry-owning namespaces in
  declared order, composes their `registry` values plus the
  design-bundle :fn schemas, publishes the composite as malli's default
  registry, then instruments every registered function schema. Returns
  the composite map."
  []
  (let [composite (merge (compose-project-registry) design-bundle-schemas)]
    (mr/set-default-registry!
     (mr/composite-registry (m/default-schemas) composite))
    (mi/instrument!)
    composite))

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
