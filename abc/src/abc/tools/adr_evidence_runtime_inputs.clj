(ns abc.tools.adr-evidence-runtime-inputs
  (:require [abc.tools.files :as files]
            [abc.tools.path-containment :as containment]
            [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as reader-types]))

(defn- fail! [kind message & {:as data}]
  (throw (ex-info message (assoc data :kind kind))))

(def ^:private workspace-sentinels
  ["flake.nix" "justfile" "abc/flake.nix" "ab-validator/flake.nix"])

(defn validate-workspace-root!
  "Require the monorepo identity root, not merely a containing directory.
  The Git identity and the four owned entry-point sentinels must agree."
  ([workspace-root] (validate-workspace-root! nil workspace-root))
  ([repo-root workspace-root]
   (let [root (fs/file (fs/canonicalize workspace-root))
         {:keys [exit out]} @(process/process
                              ["git" "-C" (str root) "rev-parse" "--show-toplevel"]
                              {:out :string :err :out})
         output (str/trim out)
         git-root (when (zero? exit) (fs/file (fs/canonicalize output)))
         missing (->> workspace-sentinels
                      (remove #(fs/regular-file? (fs/path root %)))
                      vec)
         expected-abc (fs/file (fs/canonicalize (fs/path root "abc")))
         actual-abc (some-> repo-root fs/canonicalize fs/file)]
     (when-not (and (zero? exit) (= root git-root) (empty? missing)
                    (or (nil? actual-abc) (= expected-abc actual-abc)))
       (fail! :invalid-workspace-root
              "workspace root must be the exact Soranoha Git root"
              :workspace-root (str root)
              :git-root (some-> git-root str)
              :abc-root (some-> actual-abc str)
              :missing missing))
     root)))

(defn- validate-path! [root path]
  (when-not (and (string? path) (seq path))
    (fail! :invalid-runtime-input-manifest "runtime input path must be non-empty" :path path))
  (let [state (containment/path-state root path)]
    (when-not (and (= :ok (:state state)) (fs/regular-file? (:path state)))
      (fail! (if (= :missing (:state state))
               :missing-runtime-input
               :invalid-runtime-input-manifest)
             "runtime input path is missing or not contained"
             :path path :state (:state state)))))

(defn- load-manifest! [root path]
  (validate-path! root path)
  (let [manifest (try (files/read-edn (fs/file root path))
                      (catch Exception e
                        (fail! :invalid-runtime-input-manifest
                               "runtime input manifest is unreadable"
                               :path path :detail (.getMessage e))))
        paths (:paths manifest)]
    (when-not (= #{:schema-version :paths} (set (keys manifest)))
      (fail! :invalid-runtime-input-manifest "runtime input manifest must have exactly two keys"))
    (when-not (= :abc-adr-runtime-inputs-v1 (:schema-version manifest))
      (fail! :invalid-runtime-input-manifest "runtime input manifest schema is unsupported"))
    (when-not (and (vector? paths)
                   (= paths (vec (sort paths)))
                   (= (count paths) (count (distinct paths))))
      (fail! :invalid-runtime-input-manifest "runtime input paths must be sorted and unique"))
    (doseq [input paths] (validate-path! root input))
    manifest))

(defn assert-runtime-input-closure!
  [{:keys [repo-root workspace-root descriptor repository-paths]}]
  (let [{descriptor-path :path value :value} descriptor
        profile (:input-profile value)
        component? (= "component-clojure-test-v1" (:kind profile))
        input-root (if component? workspace-root repo-root)]
    (when (and component? (nil? workspace-root))
      (fail! :missing-runtime-input "component evidence requires a workspace root"))
    (let [manifest-path (:runtime-input-manifest value)
          manifest (load-manifest! input-root manifest-path)
          ;; Runtime equality is deliberately only the data-read boundary.
          ;; Descriptor, manifest, and statically-derived source inputs are
          ;; independently hash-bound, but are not required to be contrived
          ;; physical reads by the focused test.
          expected (into (sorted-set) (:paths manifest))
          explicit (set (get-in value [:input-profile :explicit]))
          observed (into (sorted-set) repository-paths)
          missing (set/difference expected observed)
          extra (set/difference observed expected)]
      (when-not (set/subset? (conj expected descriptor-path manifest-path) explicit)
        (fail! :invalid-runtime-input-manifest
               "descriptor must bind its runtime manifest and every runtime-data input"
               :paths (vec (sort (set/difference
                                  (conj expected descriptor-path manifest-path)
                                  explicit)))))
      (when (seq missing)
        (fail! :undeclared-runtime-input "declared runtime inputs were not observed"
               :paths (vec (sort missing))))
      (when (seq extra)
        (fail! :missing-runtime-input "observed repository reads are absent from the manifest"
               :paths (vec (sort extra))))
      true)))

(defn validate-runtime-input-manifest!
  "Validate the static v2 descriptor/manifest binding before executing it."
  [{:keys [repo-root workspace-root descriptor]}]
  (let [{descriptor-path :path value :value} descriptor
        component? (= "component-clojure-test-v1" (get-in value [:input-profile :kind]))
        input-root (if component? workspace-root repo-root)]
    (when (and component? (nil? workspace-root))
      (fail! :missing-runtime-input "component evidence requires a workspace root"))
    (let [manifest-path (:runtime-input-manifest value)
          manifest (load-manifest! input-root manifest-path)
          required (into #{descriptor-path manifest-path} (:paths manifest))
          explicit (set (get-in value [:input-profile :explicit]))]
      (when-not (set/subset? required explicit)
        (fail! :invalid-runtime-input-manifest
               "descriptor omits its manifest or a runtime-data path"
               :paths (vec (sort (set/difference required explicit)))))
      manifest)))

(def ^:private contract-basenames
  {'abc.tools.manifest/content "manifest-content.edn"
   'abc.tools.adr/validate-repository* "adr-validate-repository-star.edn"})

(def ^:private forbidden-vars
  '#{clojure.core/slurp clojure.core/line-seq clojure.core/file-seq
     clojure.core/eval clojure.core/read-string clojure.core/load-string
     clojure.core/load-file clojure.core/resolve clojure.core/ns-resolve
     clojure.core/requiring-resolve
     clojure.edn/read clojure.java.io/reader clojure.java.io/input-stream
     clojure.java.shell/sh babashka.process/process babashka.process/shell})

(def ^:private code-loading-vars
  '#{clojure.core/eval clojure.core/read-string clojure.core/load-string
     clojure.core/load-file clojure.core/resolve clojure.core/ns-resolve
     clojure.core/requiring-resolve})

(def ^:private forbidden-simple
  '#{FileReader ProcessBuilder ZipFile readAllBytes readString newInputStream
     listFiles loadModel readDataset inputStream input-stream reader URL openStream
     sh process shell})

(def ^:private trusted-adapter-vars
  '#{abc.tools.evidence-io/with-read-trace
     abc.tools.evidence-io/record-read!
     abc.tools.adr-evidence-runtime-inputs/assert-runtime-input-closure!
     abc.tools.files/read-text abc.tools.files/read-bytes
     abc.tools.files/input-stream abc.tools.files/reader abc.tools.files/list-files
     abc.tools.files/exists? abc.tools.files/directory? abc.tools.files/file?
     abc.tools.files/read-edn abc.tools.files/read-jsonl abc.tools.files/read-zip-entry
     abc.tools.files/read-jena-model abc.tools.files/read-tei-document
     abc.tools.json/read-json-file abc.tools.hash/sha256-file
     abc.tools.hash/byte-length})

(def ^:private audited-structural-leaf-vars
  '#{abc.tools.path-containment/path-state})

(defn- trusted-leaf-var? [var]
  (or (contains? trusted-adapter-vars var)
      (contains? audited-structural-leaf-vars var)))

;; External executable Vars are capabilities, not harmless names. This is an
;; exact reviewed inventory of pure operations needed by current evidence
;; boundaries; additions require a focused positive test and review.
(def ^:private audited-safe-external-vars
  '#{clojure.core/= clojure.core/not= clojure.core/not clojure.core/< clojure.core/<=
     clojure.core/> clojure.core/>= clojure.core/+ clojure.core/-
     clojure.core/* clojure.core// clojure.core/inc clojure.core/dec
     clojure.core/identity clojure.core/constantly clojure.core/comp
     clojure.core/complement clojure.core/partial clojure.core/apply clojure.core/juxt
     clojure.core/ex-info clojure.core/iterator-seq clojure.core/make-array
     clojure.core/str clojure.core/pr-str clojure.core/name clojure.core/namespace
     clojure.core/symbol clojure.core/keyword clojure.core/boolean
     clojure.core/count clojure.core/empty? clojure.core/seq clojure.core/first
     clojure.core/second clojure.core/rest clojure.core/next clojure.core/nth
     clojure.core/peek clojure.core/pop
     clojure.core/get clojure.core/get-in clojure.core/find clojure.core/contains?
     clojure.core/keys clojure.core/vals clojure.core/select-keys
     clojure.core/assoc clojure.core/assoc-in clojure.core/dissoc
     clojure.core/update clojure.core/update-in clojure.core/conj clojure.core/disj
     clojure.core/into clojure.core/merge clojure.core/merge-with
     clojure.core/map clojure.core/mapv clojure.core/mapcat clojure.core/map-indexed clojure.core/filter
     clojure.core/remove clojure.core/keep clojure.core/keep-indexed
     clojure.core/reduce clojure.core/reduce-kv clojure.core/sort clojure.core/sort-by
     clojure.core/distinct clojure.core/dedupe clojure.core/group-by
     clojure.core/frequencies
     clojure.core/some clojure.core/every? clojure.core/not-any?
     clojure.core/vector clojure.core/vec clojure.core/set clojure.core/hash-map
     clojure.core/sorted-map clojure.core/sorted-set clojure.core/range
     clojure.core/repeatedly clojure.core/take clojure.core/drop
     clojure.core/take-nth clojure.core/concat clojure.core/reverse
     clojure.core/string? clojure.core/symbol? clojure.core/keyword?
     clojure.core/map? clojure.core/set? clojure.core/vector? clojure.core/seq?
     clojure.core/coll? clojure.core/integer? clojure.core/qualified-symbol?
     clojure.core/nil? clojure.core/some? clojure.core/true? clojure.core/false?
     clojure.core/zero? clojure.core/pos? clojure.core/neg?
     clojure.core/re-find clojure.core/re-matches clojure.core/re-seq
     clojure.core/subs clojure.core/swap!
     clojure.test/is clojure.string/includes? clojure.string/starts-with?
     clojure.string/ends-with? clojure.string/blank? clojure.string/split
     clojure.string/split-lines clojure.string/replace clojure.string/join clojure.string/trim
     clojure.set/union clojure.set/difference clojure.set/intersection
     clojure.set/subset? clojure.java.io/file
     babashka.fs/absolute? babashka.fs/absolutize babashka.fs/canonicalize babashka.fs/file
     babashka.fs/normalize babashka.fs/path babashka.fs/relativize})

(def ^:private audited-special-heads
  '#{fn* fn if let* let loop* loop recur do throw try catch finally
     -> ->> some-> some->> cond-> cond->>
     set! monitor-enter monitor-exit case* deftype* reify*})

(def ^:private audited-core-macros
  '#{and or when when-not if-let when-let if-some when-some cond condp case
     doseq for dotimes letfn binding with-open lazy-seq doto assert})

(def ^:private audited-noncore-macros
  '#{clojure.test/is})

;; Exact object-pure operations only. Filesystem predicates and metadata,
;; constructors, static JVM I/O, network, and process APIs go through named
;; adapters instead.
(def ^:private audited-safe-jvm-heads
  '#{.getName .isBefore .length})

(def ^:private audited-safe-jvm-vars
  '#{Integer/parseInt LocalDate/parse})

(defn- forbidden-head? [head]
  (let [simple (-> (name head)
                   (str/replace #"^\." "")
                   (str/replace #"\.$" "")
                   (str/split #"\.")
                   last
                   symbol)]
    (contains? forbidden-simple simple)))

(defn- kondo-analysis! [repo-root]
  (let [config (pr-str {:output {:format :edn}
                        :analysis {:var-definitions true :var-usages true
                                   :locals true :local-usages true}})
        {:keys [exit out]} @(process/process
                             ["clj-kondo" "--fail-level" "error"
                              "--lint" "src" "test" "--config" config]
                             {:dir (str repo-root) :out :string :err :out})
        output out]
    (when-not (zero? exit)
      (fail! :invalid-nix-clojure-closure "clj-kondo analysis failed" :output output))
    (edn/read-string output)))

(defn- read-forms! [file]
  (with-open [r (files/reader file)]
    (let [r (reader-types/indexing-push-back-reader r)]
      (loop [forms []]
        (let [form (reader/read {:eof ::eof :read-cond :allow :features #{:clj}} r)]
          (if (= ::eof form) forms (recur (conj forms form))))))))

(defn- qvar [m] (symbol (str (:ns m)) (str (:name m))))
(defn- span [x] [(:line (meta x)) (:column (meta x))])
(defn- usage-span [x] [(:name-row x) (:name-col x)])

(defn- defn-form [forms name]
  (some (fn [form]
          (when (and (seq? form) (#{'defn 'defn- 'deftest} (first form)) (= name (second form))) form))
        forms))

(defn- parse-defn [form]
  (if (= 'deftest (first form))
    {:params [] :body (drop 2 form)}
    (let [[_ _ & tail] form
          tail (cond-> tail (string? (first tail)) rest (map? (first tail)) rest)
          arities (if (vector? (first tail)) [(cons (first tail) (rest tail))] tail)]
      (when-not (= 1 (count arities))
        (fail! :unsupported-evidence-call-graph "multi-arity defn is outside the audited subset"))
      {:params (first (first arities)) :body (rest (first arities))})))

(defn- contract-path [caller]
  (when-let [basename (get contract-basenames caller)]
    (str "data/evidence-higher-order-calls/" basename)))

(defn- load-contract! [repo-root caller]
  (let [path (contract-path caller)]
    (when-not path
      (fail! :unregistered-higher-order-call "function parameter invocation has no contract"
             :caller caller))
    (when-not (= :ok (:state (containment/path-state repo-root path)))
      (fail! :invalid-higher-order-contract "higher-order contract is missing"
             :caller caller :path path))
    (let [value (try (files/read-edn (fs/file repo-root path))
                     (catch Exception e
                       (fail! :invalid-higher-order-contract "higher-order contract is unreadable"
                              :caller caller :path path :detail (.getMessage e))))]
      (when-not (and (= #{:schema-version :caller :parameters} (set (keys value)))
                     (= :abc-evidence-higher-order-call-v1 (:schema-version value))
                     (= caller (:caller value))
                     (map? (:parameters value))
                     (every? symbol? (keys (:parameters value)))
                     (every? #(and (vector? %) (seq %) (= (count %) (count (distinct %)))
                                   (every? qualified-symbol? %))
                             (vals (:parameters value))))
        (fail! :invalid-higher-order-contract "higher-order contract is malformed"
               :caller caller :path path))
      [path value])))

(defn analyze-reachable-vars
  "Return the exact call graph admitted by the evidence closed subset. clj-kondo
  resolves Vars; tools.reader identifies list-head and defn-parameter spans."
  [repo-root focused-vars]
  (let [repo-root (fs/file (fs/canonicalize repo-root))
        kondo (kondo-analysis! repo-root)
        analysis (:analysis kondo)
        definitions-grouped (group-by qvar (:var-definitions analysis))
        definitions (into {} (map (fn [[var items]] [var (first items)])) definitions-grouped)
        usages (group-by (juxt :filename :from-var) (:var-usages analysis))
        locals (group-by :filename (:local-usages analysis))
        findings (group-by :filename (:findings kondo))
        forms-cache (atom {})
        consulted (atom (sorted-set))
        reachable (atom (sorted-set))
        resolved-calls (atom (sorted-set))]
    (letfn [(definition! [var]
              (let [items (get definitions-grouped var)]
                (cond
                  (nil? items)
                  (fail! :unresolved-focused-var "focused or target Var does not resolve" :var var)
                  (not= 1 (count items))
                  (fail! :duplicate-var-definition "reachable Var has duplicate definitions" :var var)
                  :else (first items))))
            (forms! [filename]
              (or (get @forms-cache filename)
                  (let [forms (read-forms! (fs/file repo-root filename))]
                    (swap! forms-cache assoc filename forms) forms)))
            (walk! [caller definition params form pending]
              (cond
                (seq? form)
                (let [head (first form)]
                  (when-not (or (symbol? head) (keyword? head) (set? head) (map? head))
                    (fail! :unsupported-evidence-call-graph "computed invocation is forbidden"
                           :caller caller :form (pr-str form)))
                  (cond
                    (#{'quote 'var} head) nil
                    (or (keyword? head) (set? head) (map? head)) nil
                    (contains? audited-special-heads head)
                    (do
                      (when (#{'-> '->> 'some-> 'some->>} head)
                        (doseq [step (drop 2 form)
                                :when (symbol? step)]
                          (walk! caller definition params (list step) pending)))
                      (when (#{'cond-> 'cond->>} head)
                        (doseq [step (take-nth 2 (drop 3 form))
                                :when (symbol? step)]
                          (walk! caller definition params (list step) pending))))
                    (or (= 'new head)
                        (= '. head)
                        (str/starts-with? (name head) ".")
                        (and (namespace head)
                             (re-matches #"[A-Z].*" (last (str/split (namespace head) #"\.")))))
                    (when-not (or (contains? audited-safe-jvm-heads head)
                                  (contains? audited-safe-jvm-vars head))
                      (fail! :forbidden-evidence-io
                             "reachable JVM invocation is outside a named traced adapter"
                             :caller caller :target head))
                    (forbidden-head? head)
                    (fail! :forbidden-evidence-io "reachable raw I/O, network, or process call"
                           :caller caller :target head)
                    (contains? params head)
                    (let [[path contract] (load-contract! repo-root caller)
                          targets (get-in contract [:parameters head])]
                      (when-not targets
                        (fail! :invalid-higher-order-contract "invoked parameter is absent from contract"
                               :caller caller :parameter head))
                      (when-not (some #(= (span head) (usage-span %))
                                      (get locals (:filename definition)))
                        (fail! :reader-kondo-span-mismatch "parameter head does not match clj-kondo local usage"
                               :caller caller :parameter head))
                      (swap! consulted conj path)
                      (doseq [target targets]
                        (definition! target)
                        (swap! resolved-calls conj target)
                        (if (trusted-leaf-var? target)
                          (swap! reachable conj target)
                          (swap! pending conj target))))
                    :else
                    (let [matches (filter #(= (span head) (usage-span %))
                                          (get usages [(:filename definition) (:name definition)]))]
                      (when-not (= 1 (count matches))
                        (fail! :reader-kondo-span-mismatch "list head must match exactly one resolved Var"
                               :caller caller :head head :matches (count matches)))
                      (let [usage (first matches)
                            target (symbol (str (:to usage)) (str (:name usage)))]
                        (swap! resolved-calls conj target)
                        (when (and (:macro usage)
                                   (not (or (and (= 'clojure.core (:to usage))
                                                 (contains? audited-core-macros (:name usage)))
                                            (contains? audited-noncore-macros target))))
                          (fail! :unsupported-evidence-call-graph
                                 "reachable macro is outside the audited subset"
                                 :caller caller :target target))
                        (when (or (contains? forbidden-vars target)
                                  (forbidden-head? (:name usage)))
                          (fail! (if (contains? code-loading-vars target)
                                   :forbidden-evidence-capability
                                   :forbidden-evidence-io)
                                 "reachable code-loading, I/O, network, or process capability"
                                 :caller caller :target target))
                        (cond
                          (trusted-leaf-var? target)
                          (swap! reachable conj target)
                          (contains? definitions target)
                          (do
                            (when (and (:macro (get definitions target))
                                       (not= 'clojure.core (:to usage)))
                              (fail! :unsupported-evidence-call-graph
                                     "user macro is forbidden" :target target))
                            (swap! pending conj target))

                          (or (:macro usage) (contains? audited-safe-external-vars target)) nil

                          :else
                          (fail! :forbidden-evidence-capability
                                 "resolved external executable Var is not in the reviewed pure allowlist"
                                 :caller caller :target target)))))
                  ;; A forbidden Var passed as data is still an executable capability
                  ;; (for example `(apply slurp args)`). Resolve every symbol argument
                  ;; at its reader span and reject it before descending.
                  (doseq [argument (rest form)
                          :when (symbol? argument)
                          usage (filter #(= (span argument) (usage-span %))
                                        (get usages [(:filename definition) (:name definition)]))
                          :let [target (symbol (str (:to usage)) (str (:name usage)))]]
                    (cond
                      (contains? forbidden-vars target)
                      (fail! (if (contains? code-loading-vars target)
                               :forbidden-evidence-capability
                               :forbidden-evidence-io)
                             "raw capability Var passed as a higher-order argument"
                             :caller caller :target target)

                      (and (contains? definitions target)
                           (let [target-definition (get definitions target)]
                             (and (not (:macro target-definition))
                                  (or (:fixed-arities target-definition)
                                      (:varargs-min-arity target-definition))))
                           (not (trusted-leaf-var? target)))
                      ;; A directly resolved Var is already a finite target: add
                      ;; its graph. Only function-valued parameters need the
                      ;; caller-scoped contract handled above.
                      (swap! pending conj target)))
                  (doseq [x (rest form)] (walk! caller definition params x pending)))
                (coll? form) (doseq [x form] (walk! caller definition params x pending))))
            (visit! [var pending]
              (when-not (contains? @reachable var)
                (let [definition (definition! var)
                      filename (:filename definition)
                      form (defn-form (forms! filename) (:name definition))]
                  (when-not form
                    (fail! :unsupported-evidence-call-graph "reachable Var is not a direct defn" :var var))
                  (when (some #(and (<= (:row definition) (:row %))
                                    (<= (:row %) (:end-row definition))
                                    (= :error (:level %)))
                              (get findings filename))
                    (fail! :unresolved-call-edge "clj-kondo found an error in reachable Var" :var var))
                  (swap! reachable conj var)
                  (let [{:keys [params body]} (parse-defn form)]
                    (doseq [x body]
                      (walk! var definition (set (filter symbol? params)) x pending))))))]
      (let [pending (atom (into (sorted-set) focused-vars))]
        (loop []
          (when-let [var (first @pending)]
            (swap! pending disj var)
            (visit! var pending)
            (recur))))
      {:focused-vars (vec (sort focused-vars))
       :reachable-vars (vec @reachable)
       :resolved-call-vars (vec @resolved-calls)
       :paths (->> @reachable (map #(-> definitions (get %) :filename)) distinct sort vec)
       :contract-paths (vec @consulted)})))

(def ^:private required-v2-boundary-vars
  '#{abc.tools.evidence-io/with-read-trace
     abc.tools.adr-evidence-runtime-inputs/assert-runtime-input-closure!})

(defn assert-v2-boundary-ownership!
  "Require a focused v2 wrapper to own both the physical trace and its exact
  manifest comparison. Merely reaching a test suite is not evidence."
  [{:keys [resolved-call-vars] :as analysis}]
  (let [missing (set/difference required-v2-boundary-vars (set resolved-call-vars))]
    (when (seq missing)
      (fail! :missing-evidence-boundary-owner
             "v2 focus must call the exact trace and runtime-closure owners"
             :vars (vec (sort missing))))
    analysis))

(defn derive-nix-clojure-source-closure [repo-root focused-vars]
  (:paths (analyze-reachable-vars repo-root focused-vars)))

(defn validate-nix-clojure-closure!
  "Validate a checked Nix/Clojure closure manifest and its descriptor binding."
  [repo-root manifest-path descriptor-inputs]
  (validate-path! repo-root manifest-path)
  (let [manifest (files/read-edn (fs/file repo-root manifest-path))
        focused (:focused-vars manifest)
        paths (:paths manifest)]
    (when-not (= #{:schema-version :focused-vars :paths} (set (keys manifest)))
      (fail! :invalid-nix-clojure-closure "closure manifest has an invalid key set"))
    (when-not (= :abc-adr-nix-clojure-closure-v1 (:schema-version manifest))
      (fail! :invalid-nix-clojure-closure "closure manifest schema is unsupported"))
    (when-not (and (vector? focused) (seq focused)
                   (every? qualified-symbol? focused)
                   (= focused (vec (sort focused)))
                   (= (count focused) (count (distinct focused)))
                   (vector? paths) (= paths (vec (sort paths)))
                   (= (count paths) (count (distinct paths))))
      (fail! :invalid-nix-clojure-closure "closure coordinates must be sorted and unique"))
    (let [{actual-paths :paths contract-paths :contract-paths}
          (analyze-reachable-vars repo-root focused)
          expected-paths (vec (sort actual-paths))
          required (into #{"deps.edn" "deps-lock.json" "tests.edn" manifest-path}
                         (concat expected-paths contract-paths))
          inputs (set descriptor-inputs)]
      (when-not (= paths expected-paths)
        (fail! :invalid-nix-clojure-closure "closure manifest does not equal derived reachable paths"
               :expected expected-paths :actual paths))
      (when-let [missing (seq (sort (set/difference required inputs)))]
        (fail! :missing-evidence-input "descriptor omits a Nix/Clojure closure determinant"
               :paths (vec missing)))
      true)))
