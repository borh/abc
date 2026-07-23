(ns abc.tools.malli
  "Shared Malli values: scalar schemas that domain namespaces compose
  into their own explicit registries, plus explanation-formatting
  helpers. This namespace never depends on a domain, never mutates
  malli's default registry, and never instruments anything —
  validation call sites pass a registry explicitly."
  (:require [clojure.string :as string]
            [malli.core :as m]
            [malli.error :as me]))

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

(def scalar-schemas
  "Shared scalar schemas only. Domain namespaces compose these into
  their own registries; this namespace never depends on a domain."
  {::nonblank-string
   [:fn {:error/message "must be a non-empty string"}
    nonblank-string?]

   ::nullable-nonblank-string
   [:fn {:error/message "must be null or a non-empty string"}
    (fn [value]
      (or (nil? value) (nonblank-string? value)))]

   ::sha256-hash
   [:re {:error/message "must be a sha256 hash"}
    #"^sha256:[0-9a-f]{64}$"]

   ::semver
   [:re {:error/message "must be semver"}
    #"^[0-9]+\.[0-9]+\.[0-9]+$"]

   ::positive-int
   [:int {:error/message "must be a positive integer"
          :min 1}]

   ::nonnegative-int
   [:int {:error/message "must be a non-negative integer"
          :min 0}]

   ::workspace-logical-path
   [:fn {:error/message "must be workspace-relative and must not start with ../ or /"}
    workspace-logical-path?]

   ::concrete-adapter
   [:fn {:error/message "must name a concrete adapter"}
    concrete-adapter?]})

(defn- missing-key-message
  "Message for a ::m/missing-key error at `in`: the missing key is the
  last path segment, any enclosing keys prefix the message (so a
  top-level miss reads \"is missing :sha256\" and a nested miss reads
  \":evidence_scope is missing :adapter\")."
  [in]
  (let [segments (map str in)
        parent (butlast segments)]
    (str (when (seq parent)
           (str (string/join " " parent) " "))
         "is missing " (last segments))))

(defn explanation-messages
  "Return stable message strings from a Malli explanation. Prefer explicit
  :error/message values from schemas; fall back to the path when a Malli
  primitive emits no custom message. Missing map keys render as
  \"is missing <key>\"."
  [explanation]
  (->> (:errors explanation)
       (mapv (fn [{:keys [in message path properties schema type]}]
               (let [schema-message (or message
                                        (:error/message properties)
                                        (some-> schema m/properties :error/message))
                     location (seq (or in path))
                     location-label (when location
                                      (string/join " " (map str location)))]
                 (cond
                   (and (= ::m/missing-key type)
                        (nil? message)
                        (seq in))
                   (missing-key-message in)

                   (and location-label schema-message)
                   (str location-label " " schema-message)

                   schema-message
                   schema-message

                   location-label
                   (str location-label " is invalid")

                   :else
                   "value is invalid"))))))

(defn explain-or-throw!
  "Validate `value` against `schema-key` resolved in the caller's
  explicit `registry`. On failure, throws ex-info whose **message
  embeds the humanized errors** (so `thrown-with-msg?` regexes match)
  and whose ex-data carries `:errors-humanized` (vector of strings),
  `:label`, and the raw `:explanation`. Returns `:ok` on success."
  [schema-key value label registry]
  (if-let [explanation (m/explain schema-key value {:registry registry})]
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
