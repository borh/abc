(ns abc.tools.adr-evidence-bundle
  (:require [abc.tools.hash :as hash]
            [abc.tools.json :as json]
            [abc.tools.path-containment :as containment]
            [abc.tools.schema :as schema]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [clojure.lang LineNumberingPushbackReader]
           [java.time LocalDate]))

(def ^:private safe-integer-min -9007199254740991)
(def ^:private safe-integer-max 9007199254740991)

(def ^:private schemas
  (delay
    {"abc-adr-evidence-run-v1"
     (json/read-json-file "schemas/adr-evidence-run.schema.json")
     "abc-adr-external-evidence-v1"
     (json/read-json-file "schemas/adr-external-evidence.schema.json")}))

(defn- problem [kind message affected-claim-ids & {:as data}]
  (merge {:kind kind
          :message message
          :affected-claim-ids (vec (sort (distinct affected-claim-ids)))}
         data))

(defn- value-at [m k]
  (or (get m k) (get m (name k))))

(defn- artifact-path-problems [artifact-path state affected-claim-ids]
  (case (:state state)
    :ok []
    :missing [(problem :missing-evidence-artifact
                       "evidence artifact does not exist"
                       affected-claim-ids :artifact-path artifact-path)]
    :real-path-escape [(problem :evidence-real-path-escape
                                "evidence artifact real path escapes the repository"
                                affected-claim-ids :artifact-path artifact-path)]
    [(problem :evidence-path-traversal
              "evidence artifact path is not a contained repository-relative path"
              affected-claim-ids :artifact-path artifact-path)]))

(defn load-bundle [repo-root artifact-path]
  (let [state (containment/path-state repo-root artifact-path)]
    (if (not= :ok (:state state))
      {:problems (artifact-path-problems artifact-path state [])}
      (try
        (let [value (json/read-json-file (:path state))]
          {:value value
           :canonical-hash (hash/format-sha256 (hash/sha256-json-jcs value))})
        (catch Exception exception
          {:problems [(problem :invalid-evidence-artifact
                               "evidence artifact is not readable JSON"
                               [] :artifact-path artifact-path
                               :detail (.getMessage exception))]})))))

(defn- safe-value? [value]
  (cond
    (or (nil? value) (boolean? value) (string? value)) true
    (integer? value) (<= safe-integer-min value safe-integer-max)
    (sequential? value) (every? safe-value? value)
    (map? value) (and (every? string? (keys value))
                      (every? safe-value? (vals value)))
    :else false))

(defn- calendar-date? [value]
  (and (string? value)
       (try
         (LocalDate/parse value)
         true
         (catch Exception _ false))))

(defn- artifact-validation-problems [artifact-path value affected-claim-ids]
  (let [schema-version (get value "schema_version")
        contract (get @schemas schema-version)]
    (cond
      (nil? contract)
      [(problem :invalid-evidence-artifact
                "evidence artifact schema_version is unknown"
                affected-claim-ids :artifact-path artifact-path)]

      (not (safe-value? value))
      [(problem :invalid-evidence-artifact
                "evidence artifact contains an unsafe JSON value"
                affected-claim-ids :artifact-path artifact-path)]

      :else
      (let [[errors humanized] (schema/validation-errors-humanized contract value)
            invalid-date? (and (= "abc-adr-external-evidence-v1" schema-version)
                               (or (not (calendar-date? (get value "retrieved_at")))
                                   (not (calendar-date? (get value "review_after")))))]
        (cond
          (seq errors)
          [(problem :invalid-evidence-artifact
                    "evidence artifact does not satisfy its schema"
                    affected-claim-ids :artifact-path artifact-path
                    :errors errors :errors-humanized humanized)]

          invalid-date?
          [(problem :invalid-evidence-artifact
                    "external evidence dates must be real calendar dates"
                    affected-claim-ids :artifact-path artifact-path)]

          :else [])))))

(defn- namespace-relative-path [namespace-symbol]
  (str (-> (str namespace-symbol)
           (str/replace "." "/")
           (str/replace "-" "_"))
       ".clj"))

(defn- resolve-namespace [repo-root namespace-symbol required?]
  (let [relative (namespace-relative-path namespace-symbol)
        candidates [(str "test/" relative) (str "src/" relative)]
        match (some (fn [candidate]
                      (when (= :ok (:state (containment/path-state repo-root candidate)))
                        candidate))
                    candidates)]
    (if (or match (not required?))
      match
      (throw (ex-info "required evidence namespace is missing"
                      {:kind :missing-evidence-input
                       :namespace (str namespace-symbol)})))))

(defn- ns-requires [file]
  (with-open [reader (LineNumberingPushbackReader. (io/reader file))]
    (let [form (binding [*read-eval* false]
                 (read {:eof nil} reader))]
      (if (and (seq? form) (= 'ns (first form)))
        (->> (drop 2 form)
             (filter #(and (seq? %) (= :require (first %))))
             (mapcat rest)
             (keep (fn [libspec]
                     (cond
                       (symbol? libspec) libspec
                       (and (sequential? libspec) (symbol? (first libspec)))
                       (first libspec)
                       :else nil)))
             set
             sort)
        []))))

(defn- clojure-namespace-inputs [repo-root roots]
  (loop [queue (into (sorted-set) (map symbol roots))
         visited (sorted-set)
         paths (sorted-set)]
    (if-let [namespace-symbol (first queue)]
      (let [queue (disj queue namespace-symbol)]
        (if (contains? visited namespace-symbol)
          (recur queue visited paths)
          (let [required? (contains? (set (map symbol roots)) namespace-symbol)
                relative (resolve-namespace repo-root namespace-symbol required?)]
            (if-not relative
              (recur queue (conj visited namespace-symbol) paths)
              (let [dependencies (ns-requires (io/file repo-root relative))]
                (recur (into queue dependencies)
                       (conj visited namespace-symbol)
                       (conj paths relative)))))))
      paths)))

(defn derive-minimum-inputs [repo-root input-profile]
  (let [kind (value-at input-profile :kind)
        roots (or (value-at input-profile :roots) [])
        explicit (or (value-at input-profile :explicit) [])]
    (case kind
      "clojure-test-v1"
      (into (sorted-set) (concat explicit
                                 (clojure-namespace-inputs repo-root roots)))

      ("repo-files-v1" "external-authority-v1")
      (into (sorted-set) explicit)

      (throw (ex-info "unknown evidence input profile"
                      {:kind :invalid-evidence-artifact
                       :input-profile kind})))))

(defn- dedupe-root-problems [problems]
  (:problems
   (reduce (fn [{:keys [seen] :as result} item]
             (let [identity [(:kind item) (:input-path item)]]
               (if (contains? seen identity)
                 result
                 (-> result
                     (update :seen conj identity)
                     (update :problems conj item)))))
           {:seen #{} :problems []}
           problems)))

(defn- input-problems [repo-root artifact-path value affected-claim-ids]
  (let [profile (get value "input_profile")
        inputs (get value "inputs")
        summary (when (= "abc-adr-external-evidence-v1"
                         (get value "schema_version"))
                  (get value "summary"))
        required (try
                   (cond-> (derive-minimum-inputs repo-root profile)
                     (= "abc-adr-external-evidence-v1" (get value "schema_version"))
                     (conj (get-in value ["summary" "path"])))
                   (catch Exception exception exception))]
    (if (instance? Exception required)
      [(problem (or (:kind (ex-data required)) :invalid-evidence-artifact)
                (.getMessage ^Exception required)
                affected-claim-ids :artifact-path artifact-path)]
      (dedupe-root-problems
       (concat
        (for [path required
              :when (not (contains? inputs path))]
          (problem :missing-evidence-input
                   "derived evidence input is not bound by the artifact"
                   affected-claim-ids :artifact-path artifact-path :input-path path))
        (for [[path expected-hash] (sort-by key inputs)
              :let [state (containment/path-state repo-root path)]
              :when (not= :ok (:state state))]
          (problem (case (:state state)
                     :missing :missing-evidence-input
                     :real-path-escape :evidence-real-path-escape
                     :evidence-path-traversal)
                   "evidence input path is missing or escapes the repository"
                   affected-claim-ids :artifact-path artifact-path :input-path path))
        (for [[path expected-hash] (sort-by key inputs)
              :let [state (containment/path-state repo-root path)]
              :when (= :ok (:state state))
              :let [actual-hash (hash/format-sha256 (hash/sha256-file (:path state)))]
              :when (not= expected-hash actual-hash)]
          (problem :input-hash-mismatch
                   "current evidence input hash does not match the artifact"
                   affected-claim-ids :artifact-path artifact-path
                   :input-path path :expected expected-hash :actual actual-hash))
        (when summary
          (let [path (get summary "path")
                expected-hash (get summary "hash")
                state (containment/path-state repo-root path)]
            (when (= :ok (:state state))
              (let [actual-hash
                    (hash/format-sha256 (hash/sha256-file (:path state)))]
                (when (not= expected-hash actual-hash)
                  [(problem :input-hash-mismatch
                            "bounded external summary hash does not match its artifact"
                            affected-claim-ids :artifact-path artifact-path
                            :input-path path :expected expected-hash
                            :actual actual-hash)]))))))))))

(defn validate-bundle [repo-root artifact-path expected-hash affected-claim-ids]
  (let [affected-claim-ids (vec (sort (distinct affected-claim-ids)))
        loaded (load-bundle repo-root artifact-path)]
    (if-let [load-problems (:problems loaded)]
      {:bundle nil
       :problems (mapv #(assoc % :affected-claim-ids affected-claim-ids)
                       load-problems)}
      (let [{:keys [value canonical-hash]} loaded
            artifact-problems
            (artifact-validation-problems artifact-path value affected-claim-ids)
            hash-problems
            (when (not= expected-hash canonical-hash)
              [(problem :artifact-hash-mismatch
                        "canonical evidence artifact hash does not match the registry"
                        affected-claim-ids :artifact-path artifact-path
                        :expected expected-hash :actual canonical-hash)])
            problems (vec (concat hash-problems artifact-problems
                                  (when (empty? artifact-problems)
                                    (input-problems repo-root artifact-path value
                                                    affected-claim-ids))))]
        {:bundle (when (empty? problems) value)
         :problems problems}))))

(defn observation [bundle observation-key]
  (get-in bundle ["observations" observation-key "value"]))
