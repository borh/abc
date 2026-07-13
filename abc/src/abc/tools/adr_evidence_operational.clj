(ns abc.tools.adr-evidence-operational
  (:require [abc.tools.adr-evidence-observation-catalog :as catalog]
            [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.path-containment :as containment]
            [babashka.fs :as fs]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as reader-types]))

(def ^:private descriptor-v1-keys
  #{:schema-version :tool :argv :input-profile :observation-key})
(def ^:private descriptor-v2-keys
  (conj descriptor-v1-keys :runtime-input-manifest))
(def ^:private descriptor-v3-keys
  (into descriptor-v2-keys
        [:catalog-path :observation-id :observation-contract-sha256]))
(def ^:private operational-base-keys
  #{:schema-version :tool :argv :entrypoint-kind :catalog-path :observation-id
    :observation-contract-sha256 :input-set-mode :input-profile :observation-key})
(def ^:private operational-clojure-keys
  (conj operational-base-keys :clojure-closure-manifest))
(def ^:private profile-keys
  {"clojure-test-v1" #{:kind :roots :explicit}
   "repo-files-v1" #{:kind :roots :explicit}
   "component-clojure-test-v1" #{:kind :component-root :roots :explicit}})
(def ^:private operational-manifest-keys
  #{:schema-version :owned-namespace-prefixes :entrypoint-namespaces :paths})
(def ^:private owned-prefix 'abc)

(defn- problem [kind message & {:as data}]
  (merge {:kind kind :message message} data))

(defn- fail! [message problems]
  (throw (ex-info message {:problems (->> problems (sort-by pr-str) vec)})))

(defn- nonempty-string? [value]
  (and (string? value) (not (str/blank? value))))

(defn- sorted-unique-vector? [value predicate]
  (and (vector? value)
       (every? predicate value)
       (= (count value) (count (distinct value)))
       (try
         (= value (vec (sort value)))
         (catch ClassCastException _ false))))

(defn- namespace-symbol? [value]
  (and (symbol? value) (nil? (namespace value))))

(defn- owned-namespace? [namespace-symbol]
  (let [segments (str/split (str namespace-symbol) #"\.")]
    (= (name owned-prefix) (first segments))))

(defn- namespace-base-path [namespace-symbol]
  (-> (str namespace-symbol)
      (str/replace "." "/")
      (str/replace "-" "_")))

(defn- namespace-candidates [namespace-symbol]
  (let [base (namespace-base-path namespace-symbol)]
    (vec (for [root ["src" "test"]
               extension [".clj" ".cljc"]]
           (str root "/" base extension)))))

(defn- resolve-owned-namespace! [repo-root namespace-symbol]
  (let [states (mapv (fn [path] [path (containment/path-state repo-root path)])
                     (namespace-candidates namespace-symbol))
        unsafe (for [[path state] states
                     :when (not (#{:ok :missing} (:state state)))]
                 (problem :invalid-operational-namespace-coordinate
                          "namespace candidate escapes the repository"
                          :namespace namespace-symbol :path path :state (:state state)))
        matches (for [[path state] states
                      :when (= :ok (:state state))]
                  [path state])]
    (when (seq unsafe)
      (fail! "operational namespace candidate is unsafe" unsafe))
    (when (some (fn [[_ state]]
                  (and (= :ok (:state state))
                       (not (fs/regular-file? (:path state)))))
                states)
      (fail! "operational namespace candidate is not a regular file"
             [(problem :invalid-operational-namespace-coordinate
                       "namespace candidate must be a regular file"
                       :namespace namespace-symbol)]))
    (case (count matches)
      0 (fail! "owned operational namespace is missing"
               [(problem :missing-operational-namespace
                         "owned operational namespace must resolve"
                         :namespace namespace-symbol)])
      1 (first matches)
      (fail! "owned operational namespace is ambiguous"
             [(problem :ambiguous-operational-namespace
                       "owned operational namespace has multiple candidates"
                       :namespace namespace-symbol
                       :paths (mapv first matches))]))))

(defn- prefix-libspec? [libspec]
  (and (sequential? libspec)
       (symbol? (first libspec))
       (seq (rest libspec))
       (not-any? keyword? (rest libspec))))

(defn- libspec-namespaces [libspec]
  (cond
    (symbol? libspec) [libspec]
    (not (and (sequential? libspec) (symbol? (first libspec)))) []
    (prefix-libspec? libspec)
    (let [prefix (first libspec)]
      (keep (fn [suffix]
              (let [suffix (if (sequential? suffix) (first suffix) suffix)]
                (when (symbol? suffix)
                  (symbol (str prefix "." suffix)))))
            (rest libspec)))
    :else [(first libspec)]))

(defn- ns-declaration [file]
  (with-open [input (files/reader file)]
    (let [pushback (reader-types/indexing-push-back-reader input)
          form (binding [reader/*read-eval* false
                         *read-eval* false]
                 (reader/read {:eof nil} pushback))]
      (when-not (and (seq? form) (= 'ns (first form)))
        (fail! "operational source must begin with an ns form"
               [(problem :invalid-operational-namespace
                         "operational source must begin with an ns form"
                         :path (str file))]))
      {:namespace (second form)
       :requires (->> (drop 2 form)
                      (filter #(and (seq? %) (= :require (first %))))
                      (mapcat rest)
                      (mapcat libspec-namespaces)
                      distinct
                      sort
                      vec)})))

(defn derive-namespace-closure
  "Derive the exact repository-local namespace closure for owned `abc` roots."
  [repo-root entrypoint-namespaces]
  (when-not (and (vector? entrypoint-namespaces)
                 (sorted-unique-vector? entrypoint-namespaces namespace-symbol?)
                 (seq entrypoint-namespaces)
                 (every? owned-namespace? entrypoint-namespaces))
    (fail! "operational entrypoints must be nonempty owned namespaces"
           [(problem :invalid-operational-entrypoints
                     "operational entrypoints must be nonempty owned namespaces")]))
  (loop [pending (into (sorted-set) entrypoint-namespaces)
         visited (sorted-set)
         paths (sorted-set)]
    (if-let [namespace-symbol (first pending)]
      (let [pending (disj pending namespace-symbol)]
        (if (contains? visited namespace-symbol)
          (recur pending visited paths)
          (if-not (owned-namespace? namespace-symbol)
            (recur pending (conj visited namespace-symbol) paths)
            (let [[path state] (resolve-owned-namespace! repo-root namespace-symbol)
                  declaration (ns-declaration (:path state))
                  declared (:namespace declaration)
                  requirements (:requires declaration)]
              (when-not (= namespace-symbol declared)
                (fail! "operational source declares a different namespace"
                       [(problem :invalid-operational-namespace
                                 "declared namespace must equal the requested namespace"
                                 :namespace namespace-symbol
                                 :declared-namespace declared
                                 :path path)]))
              (recur (into pending requirements)
                     (conj visited namespace-symbol)
                     (conj paths path))))))
      (vec paths))))

(defn- valid-coordinate-state [root path]
  (when (nonempty-string? path)
    (let [state (containment/path-state root path)]
      (when (and (= :ok (:state state)) (fs/regular-file? (:path state))) state))))

(defn- read-contained-edn! [root path kind]
  (let [state (valid-coordinate-state root path)]
    (when-not state
      (let [raw-state (when (nonempty-string? path)
                        (containment/path-state root path))]
        (fail! "EDN coordinate is not a contained regular file"
               [(problem kind "EDN coordinate is not a contained regular file"
                         :path path :state (:state raw-state))])))
    (try
      (files/read-edn (:path state))
      (catch Exception exception
        (fail! "contained EDN value is unreadable"
               [(problem kind "contained EDN value is unreadable"
                         :path path :detail (.getMessage exception))])))))

(defn- profile-problems [profile]
  (let [expected (when (map? profile) (get profile-keys (:kind profile)))]
    (concat
     (when-not (and expected (= expected (set (keys profile))))
       [(problem :invalid-evidence-descriptor "input profile has an invalid closed shape")])
     (when-not (and (vector? (:roots profile))
                    (every? nonempty-string? (:roots profile))
                    (= (count (:roots profile)) (count (distinct (:roots profile))))
                    (vector? (:explicit profile))
                    (every? nonempty-string? (:explicit profile))
                    (= (count (:explicit profile)) (count (distinct (:explicit profile)))))
       [(problem :invalid-evidence-descriptor "input profile collections are invalid")]))))

(defn- descriptor-shape-problems [descriptor]
  (if-not (map? descriptor)
    [(problem :invalid-evidence-descriptor "capture descriptor must be a map")]
    (let [version (:schema-version descriptor)
          kind (:entrypoint-kind descriptor)
          expected (case version
                     "abc-adr-evidence-capture-v1" descriptor-v1-keys
                     "abc-adr-evidence-capture-v2" descriptor-v2-keys
                     "abc-adr-evidence-capture-v3" descriptor-v3-keys
                     "abc-adr-evidence-capture-operational-v1"
                     (case kind
                       "clojure" operational-clojure-keys
                       "nix-only" operational-base-keys
                       nil)
                     nil)]
      (concat
       (when-not (and expected (= expected (set (keys descriptor))))
         [(problem :invalid-evidence-descriptor
                   "descriptor version and branch must have the exact key set")])
       (profile-problems (:input-profile descriptor))
       (when-not (and (nonempty-string? (:tool descriptor))
                      (vector? (:argv descriptor))
                      (seq (:argv descriptor))
                      (every? nonempty-string? (:argv descriptor))
                      (= (:tool descriptor) (first (:argv descriptor)))
                      (nonempty-string? (:observation-key descriptor)))
         [(problem :invalid-evidence-descriptor "descriptor command fields are invalid")])
       (when (and (= version "abc-adr-evidence-capture-v2")
                  (not (nonempty-string? (:runtime-input-manifest descriptor))))
         [(problem :invalid-evidence-descriptor "version 2 requires a runtime manifest")])
       (when (= version "abc-adr-evidence-capture-v3")
         (concat
          (when-not (nonempty-string? (:runtime-input-manifest descriptor))
            [(problem :invalid-evidence-descriptor "focused version 3 requires a runtime manifest")])
          (when-not (and (nonempty-string? (:catalog-path descriptor))
                         (keyword? (:observation-id descriptor))
                         (string? (:observation-contract-sha256 descriptor))
                         (re-matches hash/hash-pattern
                                     (:observation-contract-sha256 descriptor)))
            [(problem :invalid-evidence-descriptor "focused version 3 catalog binding is invalid")])))
       (when (= version "abc-adr-evidence-capture-operational-v1")
         (concat
          (when-not (= "repo-files-v1" (get-in descriptor [:input-profile :kind]))
            [(problem :invalid-evidence-descriptor "operational input profile must be repo-files-v1")])
          (when-not (= "exact-v1" (:input-set-mode descriptor))
            [(problem :invalid-evidence-descriptor "operational input set mode must be exact-v1")])
          (when-not (and (nonempty-string? (:catalog-path descriptor))
                         (keyword? (:observation-id descriptor))
                         (string? (:observation-contract-sha256 descriptor))
                         (re-matches hash/hash-pattern
                                     (:observation-contract-sha256 descriptor)))
            [(problem :invalid-evidence-descriptor "operational catalog binding is invalid")])
          (when (and (= "clojure" kind)
                     (not (nonempty-string? (:clojure-closure-manifest descriptor))))
            [(problem :invalid-evidence-descriptor "Clojure operation requires a closure manifest")])))))))

(defn- basename-stem [path]
  (some-> path fs/file-name str (str/replace #"\.[^.]+$" "")))

(defn- normalized-component-root [profile]
  (when (nonempty-string? (:component-root profile))
    (-> (:component-root profile)
        fs/path
        fs/normalize
        str
        (str/replace "\\" "/"))))

(defn- focused-runner [_profile]
  "bin/kaocha")

(defn- focused-input-key [profile repo-relative-path]
  (if (= "component-clojure-test-v1" (:kind profile))
    (some-> (normalized-component-root profile)
            (fs/path repo-relative-path)
            str
            (str/replace "\\" "/"))
    repo-relative-path))

(defn- focused-v3-policy-problems [repo-root descriptor descriptor-path row]
  (let [profile (:input-profile descriptor)
        profile-kind (:kind profile)
        runner (focused-runner profile)
        manifest-path (:runtime-input-manifest descriptor)
        explicit (set (:explicit profile))
        manifest-prefix "docs/evidence/adr-inputs/"
        runner-input (focused-input-key profile runner)
        manifest-input (focused-input-key profile manifest-path)
        descriptor-input (focused-input-key profile descriptor-path)
        runner-state (when (nonempty-string? runner)
                       (containment/path-state repo-root runner))
        manifest-state (when (nonempty-string? manifest-path)
                         (containment/path-state repo-root manifest-path))]
    (concat
     (when-not (contains? #{"clojure-test-v1" "component-clojure-test-v1"}
                          profile-kind)
       [(problem :invalid-focused-evidence-runner
                 "focused version 3 requires a Clojure test input profile")])
     (when-not (and (nonempty-string? runner)
                    (= runner (:tool descriptor))
                    (= [runner "--focus" (str (:focus-var row))]
                       (:argv descriptor))
                    (contains? explicit runner-input)
                    (= :ok (:state runner-state))
                    (fs/regular-file? (:path runner-state))
                    (fs/executable? (:path runner-state)))
       [(problem :invalid-focused-evidence-runner
                 "focused version 3 requires one bound contained executable Kaocha focus"
                 :runner runner :state (:state runner-state))])
     (when-not (and (nonempty-string? manifest-path)
                    (nonempty-string? manifest-prefix)
                    (str/starts-with? manifest-path manifest-prefix)
                    (= (some-> descriptor-path fs/file-name str)
                       (some-> manifest-path fs/file-name str))
                    (contains? explicit manifest-input)
                    (= :ok (:state manifest-state))
                    (fs/regular-file? (:path manifest-state)))
       [(problem :invalid-runtime-input-manifest
                 "focused version 3 runtime manifest must be same-stem, bound, and contained"
                 :path manifest-path :state (:state manifest-state))])
     (when-not (contains? explicit descriptor-input)
       [(problem :invalid-evidence-descriptor
                 "focused version 3 must bind its descriptor"
                 :descriptor-input descriptor-input)]))))

(defn- catalog-binding-problems [descriptor descriptor-path row family]
  (let [expected-hash (catalog/observation-contract-sha256 row)]
    (concat
     (when-not (= (basename-stem descriptor-path) (:descriptor-stem row))
       [(problem :invalid-evidence-descriptor "descriptor stem does not match catalog row")])
     (when-not (= (:observation-key descriptor) (:observation-key row))
       [(problem :invalid-evidence-descriptor "observation key does not match catalog row")])
     (when-not (= (:observation-contract-sha256 descriptor) expected-hash)
       [(problem :invalid-evidence-descriptor "observation contract hash does not match catalog row")])
     (if (= :operational family)
       (concat
        (when-not (= (:tool descriptor) (:tool row))
          [(problem :invalid-evidence-descriptor "tool does not match catalog row")])
        (when-not (= (:argv descriptor) (:argv row))
          [(problem :invalid-evidence-descriptor "argv does not match catalog row")])
        (when-not (= (:entrypoint-kind descriptor) (name (:entrypoint-kind row)))
          [(problem :invalid-evidence-descriptor "entrypoint kind does not match catalog row")]))
       (let [focus (some-> (:argv descriptor) last symbol)]
         (when-not (= focus (:focus-var row))
           [(problem :invalid-evidence-descriptor "focus Var does not match catalog row")]))))))

(defn load-descriptor-context!
  [{:keys [repo-root workspace-root descriptor-path]}]
  (let [descriptor (read-contained-edn! repo-root descriptor-path
                                        :invalid-evidence-descriptor-path)
        shape-problems (descriptor-shape-problems descriptor)]
    (when (seq shape-problems)
      (fail! "capture descriptor is invalid" shape-problems))
    (let [version (:schema-version descriptor)]
      (if-not (contains? #{"abc-adr-evidence-capture-v3"
                           "abc-adr-evidence-capture-operational-v1"}
                         version)
        {:repo-root repo-root
         :descriptor {:path descriptor-path :value descriptor}
         :descriptor-version version}
        (let [catalog-value (catalog/load-catalog! repo-root (:catalog-path descriptor))
              family (if (= version "abc-adr-evidence-capture-v3")
                       :focused :operational)
              row (catalog/select-observation! catalog-value
                                               (:observation-id descriptor) family)
              binding-problems (concat
                                (catalog-binding-problems descriptor descriptor-path row family)
                                (when (= :focused family)
                                  (focused-v3-policy-problems repo-root descriptor
                                                              descriptor-path row)))]
          (when (seq binding-problems)
            (fail! "descriptor does not match its selected observation" binding-problems))
          {:repo-root repo-root
           :descriptor {:path descriptor-path :value descriptor}
           :descriptor-version version
           :workspace-root workspace-root
           :catalog catalog-value
           :catalog-row row})))))

(defn- coordinate-problems [repo-root paths kind]
  (mapcat
   (fn [path]
     (let [state (when (nonempty-string? path)
                   (containment/path-state repo-root path))]
       (when-not (and state (= :ok (:state state))
                      (fs/regular-file? (:path state)))
         [(problem kind "input coordinate must be a contained regular file"
                   :path path :state (:state state))])))
   paths))

(defn- manifest-problems [repo-root manifest row expected-paths]
  (let [paths (:paths manifest)]
    (concat
     (when-not (= operational-manifest-keys (set (keys manifest)))
       [(problem :invalid-operational-closure "closure manifest must have the exact key set")])
     (when-not (= :abc-adr-operational-closure-v1 (:schema-version manifest))
       [(problem :invalid-operational-closure "closure manifest schema is unsupported")])
     (when-not (= ['abc] (:owned-namespace-prefixes manifest))
       [(problem :invalid-operational-closure "owned namespace prefixes must be exactly [abc]")])
     (when-not (= (:entrypoint-namespaces row) (:entrypoint-namespaces manifest))
       [(problem :invalid-operational-closure "manifest entrypoints do not match catalog policy")])
     (when-not (sorted-unique-vector? paths nonempty-string?)
       [(problem :invalid-operational-closure "closure paths must be a sorted unique string vector")])
     (when-not (= expected-paths paths)
       [(problem :invalid-operational-closure "closure paths do not match the derived namespace closure")])
     (coordinate-problems repo-root (if (vector? paths) paths [])
                          :invalid-operational-closure))))

(defn validate-operational-manifest! [context]
  (let [{:keys [repo-root descriptor-version catalog-row]} context
        {descriptor-path :path descriptor :value} (:descriptor context)]
    (if-not (= descriptor-version "abc-adr-evidence-capture-operational-v1")
      context
      (let [clojure? (= "clojure" (:entrypoint-kind descriptor))
            manifest-path (:clojure-closure-manifest descriptor)
            manifest (when clojure?
                       (read-contained-edn! repo-root manifest-path
                                            :invalid-operational-closure))
            closure (if clojure?
                      (derive-namespace-closure repo-root
                                                (:entrypoint-namespaces catalog-row))
                      [])
            manifest-errors (when clojure?
                              (concat
                               (when-not (= (basename-stem descriptor-path)
                                            (basename-stem manifest-path))
                                 [(problem :invalid-operational-closure
                                           "closure manifest stem must match descriptor stem")])
                               (manifest-problems repo-root manifest catalog-row closure)))
            required (into (sorted-set)
                           (concat [descriptor-path]
                                   (when clojure?
                                     [manifest-path "deps.edn" "deps-lock.json" "tests.edn"])
                                   closure
                                   (:determinant-paths catalog-row)))
            explicit (set (get-in descriptor [:input-profile :explicit]))
            coordinate-errors (coordinate-problems repo-root
                                                   (set/union required explicit)
                                                   :invalid-operational-input)
            equality-errors (when-not (= required explicit)
                              [(problem :operational-input-set-mismatch
                                        "operational explicit inputs must equal the derived exact set"
                                        :missing (vec (sort (set/difference required explicit)))
                                        :extra (vec (sort (set/difference explicit required))))])
            problems (concat manifest-errors coordinate-errors equality-errors)]
        (when (seq problems)
          (fail! "operational manifest or exact input set is invalid" problems))
        (assoc context :closure-manifest manifest :required-inputs required)))))

(defn- artifact-descriptor-path [artifact-path]
  (str "docs/evidence/adr-capture/" (basename-stem artifact-path) ".edn"))

(defn- bundle-value [bundle key]
  (or (get bundle key) (get bundle (name key))))

(defn offline-policy-problems
  [{:keys [repo-root workspace-root artifact-path bundle]}]
  (let [descriptor-path (artifact-descriptor-path artifact-path)
        descriptor-state (containment/path-state repo-root descriptor-path)
        validated (when (= :ok (:state descriptor-state))
                    (try
                      (-> (load-descriptor-context! {:repo-root repo-root
                                                     :workspace-root workspace-root
                                                     :descriptor-path descriptor-path})
                          validate-operational-manifest!)
                      (catch clojure.lang.ExceptionInfo exception exception)))]
    (cond
      (nil? validated) []
      (instance? clojure.lang.ExceptionInfo validated)
      (->> (:problems (ex-data validated)) (sort-by pr-str) vec)
      :else
      (let [version (:descriptor-version validated)
            descriptor (get-in validated [:descriptor :value])
            profile (:input-profile descriptor)
            inputs (or (bundle-value bundle :inputs) {})
            bundle-profile (or (bundle-value bundle :input_profile) {})
            bundle-explicit (set (or (bundle-value bundle-profile :explicit) []))
            input-keys (set (keys inputs))
            observations (or (bundle-value bundle :observations) {})
            observation-key (:observation-key descriptor)]
        (if (= version "abc-adr-evidence-capture-v3")
          (let [required (set (map #(focused-input-key profile %)
                                   [descriptor-path (:runtime-input-manifest descriptor)
                                    (:tool descriptor)]))
                missing-required (set/difference required input-keys)
                missing-explicit (set/difference required bundle-explicit)
                missing-observation? (not (contains? observations observation-key))]
            (->> (concat
                  (when (seq missing-required)
                    [(problem :missing-evidence-input
                              "focused bundle omits required protocol inputs"
                              :missing (vec (sort missing-required)))])
                  (when (seq missing-explicit)
                    [(problem :missing-evidence-input
                              "focused bundle profile omits required protocol inputs"
                              :missing (vec (sort missing-explicit)))])
                  (when missing-observation?
                    [(problem :missing-observation
                              "focused bundle omits its selected observation"
                              :observation-key observation-key)]))
                 (sort-by pr-str) vec))
          (let [required (:required-inputs validated)
                descriptor (get-in validated [:descriptor :value])
                expected (set required)
                mismatch (concat
                          (when-not (= expected bundle-explicit)
                            [(problem :operational-input-set-mismatch
                                      "bundle explicit inputs do not equal operational required inputs")])
                          (when-not (= expected input-keys)
                            [(problem :operational-input-set-mismatch
                                      "bundle input keys do not equal operational required inputs")]))
                missing (for [path required :when (not (contains? inputs path))]
                          (problem :missing-evidence-input
                                   "required operational input is absent from the bundle"
                                   :input-path path :artifact-path artifact-path))
                hashes (for [path required
                             :let [state (containment/path-state repo-root path)]
                             :when (and (= :ok (:state state)) (contains? inputs path))
                             :let [actual (hash/format-sha256
                                           (hash/sha256-file (:path state)))]
                             :when (not= actual (get inputs path))]
                         (problem :input-hash-mismatch
                                  "operational input hash does not match current bytes"
                                  :input-path path :artifact-path artifact-path
                                  :expected (get inputs path) :actual actual))
                selected-observation (when-not (contains? observations observation-key)
                                       [(problem :missing-observation
                                                 "operational bundle omits its selected observation"
                                                 :observation-key observation-key)])]
            (if (= "exact-v1" (:input-set-mode descriptor))
              (->> (concat mismatch missing hashes selected-observation)
                   (sort-by pr-str) vec)
              [])))))))
