(ns ab-research.parser-release-qualification
  "Release-qualification gate for the project-owned custom parser.

  Admission (exact compatibility tuple) and release qualification (this gate) are
  separate. This namespace owns the release side: a pinned qualification corpus,
  a predeclared predicate set with exact thresholds fixed BEFORE any run, and a
  measurement-agnostic evaluator that emits, per predicate, the exact observed
  and expected values plus a derived verdict.

  Integrity rules (non-negotiable):
  - A predicate whose observation is absent or explicitly `:unavailable` /
    `:instrument-missing` has verdict `:unavailable`. It is NEVER `:pass` and
    NEVER a fabricated number.
  - Numeric comparison is exact. A `0.969` observation FAILS a `1.0` predicate.
  - This gate consumes a captured measurement bundle (`:measurements`), never
    citation evidence, so comparison / neutral citations cannot reach it at all.
    The release evidence-class boundary itself lives in `ab-research.parser-evidence`
    (`assert-release-evidence!`) for any path that does ingest
    citations."
  (:require [ab-research.files :as files]
            [ab-research.aat-parser-ir-compat :as compat]
            [ab-research.hash :as hash]
            [ab-research.json :as json]
            [ab-research.parser-rq-capture :as capture]
            [clojure.set :as set]
            [clojure.walk :as walk]
            [clojure.string :as string]
            [malli.core :as m]))

(def corpus-path
  (files/path "data" "parser-release-qualification-corpus.edn"))

(def predicates-path
  (files/path "data" "parser-release-qualification-predicates.edn"))

(def corpus-entry-identity-keys
  "Fields that make a corpus entry's contract identity. The list hash is taken
  over exactly these fields, in file order, so a reason edit does not silently
  change corpus identity but a work/path/hash/category/expected-status edit
  does.

  `:expected_diagnostics` participates because the diagnostic-completeness
  instrument compares observations against it."
  [:work_id :source_path :source_sha256 :category :expected_status
   :expected_diagnostics])

(defn corpus-snapshot-hash
  "Deterministic identity of the exact source bytes the corpus pins: the
  sha256 of the newline-joined, sorted member `:source_sha256` values."
  [corpus]
  (->> (:entries corpus)
       (map :source_sha256)
       sort
       (string/join "\n")
       hash/sha256-string
       hash/format-sha256))

(defn corpus-list-hash
  "Deterministic identity of the corpus list contract: the sha256 of the
  canonical pr-str of every entry projected onto its identity keys, in file
  order."
  [corpus]
  (->> (:entries corpus)
       (mapv #(into (sorted-map) (select-keys % corpus-entry-identity-keys)))
       pr-str
       hash/sha256-string
       hash/format-sha256))

(defn corpus-integrity-errors
  "Return the list of integrity errors for `corpus`: recomputed snapshot / list
  hashes that do not equal the pinned values, or a structurally empty corpus."
  [corpus]
  (let [entries (:entries corpus)]
    (cond-> []
      (empty? entries)
      (conj "qualification corpus has no entries")

      (and (seq entries)
           (not= (:corpus_snapshot_hash corpus) (corpus-snapshot-hash corpus)))
      (conj (str "corpus_snapshot_hash mismatch: pinned "
                 (:corpus_snapshot_hash corpus) " recomputed "
                 (corpus-snapshot-hash corpus)))

      (and (seq entries)
           (not= (:list_hash corpus) (corpus-list-hash corpus)))
      (conj (str "list_hash mismatch: pinned " (:list_hash corpus)
                 " recomputed " (corpus-list-hash corpus))))))

(defn validate-corpus!
  "Throw when the pinned corpus fails its own integrity check; otherwise return
  the corpus."
  [corpus]
  (let [errors (corpus-integrity-errors corpus)]
    (when (seq errors)
      (throw (ex-info (string/join "\n" errors) {:errors errors})))
    corpus))

(defn load-corpus
  ([] (load-corpus (files/read-edn corpus-path)))
  ([corpus] (validate-corpus! corpus)))

(declare predicate-set-hash)

(defn load-predicates
  ([] (load-predicates (files/read-edn predicates-path)))
  ([predicates]
   (let [actual (predicate-set-hash predicates)]
     (when-not (= (:predicate_set_hash predicates) actual)
       (throw (ex-info "predicate_set_hash mismatch"
                       {:pinned (:predicate_set_hash predicates)
                        :recomputed actual})))
     predicates)))

(defn- canonical-json-value
  [value]
  (walk/postwalk
   (fn [node]
     (cond
       (map? node) (into {} (map (fn [[k v]] [(if (keyword? k) (name k) k) v])) node)
       (keyword? node) (name node)
       :else node))
   value))

(defn predicate-set-hash
  "Hash the predicate contract without its pinned hash field."
  [predicate-set]
  (-> predicate-set
      (dissoc :predicate_set_hash)
      canonical-json-value
      hash/sha256-json-jcs
      hash/format-sha256))

(def unavailable-observations
  "Observation sentinels that mean the instrument produced no real value. They
  map to verdict `:unavailable`, never `:pass`."
  #{:unavailable :instrument-missing})

(defn observation-available?
  "True only when `observed` is a real captured value (not nil and not an
  explicit unavailability sentinel)."
  [observed]
  (and (some? observed)
       (not (contains? unavailable-observations observed))))

(defn compare-observed
  "Exact predicate comparison. `:=` uses value equality (so `0.969` fails a
  `1.0` predicate); `:<=` / `:>=` are numeric bounds. Returns a boolean."
  [comparator expected observed]
  (case comparator
    :=  (= expected observed)
    :<= (<= observed expected)
    :>= (>= observed expected)
    (throw (ex-info "unknown predicate comparator"
                    {:comparator comparator}))))

(defn evaluate-predicate
  "Evaluate one predeclared predicate against a `measurements` map keyed by the
  predicate's `:observed_key`. Emits exact observed + expected and a derived
  verdict. No instrument -> `:unavailable`."
  [{:keys [predicate_id dimension instrument observed_key expected unit]}
   measurements]
  (let [observed (some-> (get measurements observed_key)
                         capture/observation-value)
        base {:predicate_id predicate_id
              :dimension dimension
              :instrument instrument
              :unit unit
              :expected expected
              :observed observed}]
    (assoc base :verdict
           (if (observation-available? observed)
             (if (compare-observed (:comparator expected) (:value expected) observed)
               :pass
               :fail)
             :unavailable))))

(defn evaluate
  "Evaluate every predicate in `predicate-set` against `measurements`."
  [predicate-set measurements]
  (mapv #(evaluate-predicate % measurements) (:predicates predicate-set)))

(defn install-publication-observation
  "Install the authenticated publication envelope; no scalar bypass is accepted."
  [measurements envelope]
  (assoc measurements :publication_structure envelope))

(defn install-resource-observation
  "Install predicate 8's authenticated process-tree memory envelope."
  [measurements envelope]
  (assoc measurements :peak_cgroup_memory_bytes envelope))

(defn verdict-tally
  [results]
  (frequencies (map :verdict results)))

(defn gate-status
  "The gate is `:release-qualified` only when EVERY predicate verdict is
  `:pass` and the coherence/admission precondition holds."
  [precondition-ok? results]
  (if (and precondition-ok?
           (seq results)
           (every? #(= :pass (:verdict %)) results))
    :release-qualified
    :not-qualified))

(def admission-identity-keys
  "The exact compatibility projection used for admission. This is a
  strict subset of qualification identity; corpus, predicate, and instrument
  coordinates participate in observation coherence but not registry admission."
  [:aat_version :aat_adapter :aat_adapter_version
   :mapping_id :mapping_version :mapping_hash :mapping_schema_hash
   :parser_ir_schema_id :parser_ir_schema_hash])

(def qualification-identity-keys
  "The closed identity tuple to which every observation envelope is bound."
  (into admission-identity-keys
        [:parser_git_rev :corpus_snapshot_hash :corpus_list_hash
         :predicate_set_hash :instrument_versions :instrument_policy_hashes]))

(def qualification-hash-keys
  [:mapping_hash :mapping_schema_hash :parser_ir_schema_hash
   :corpus_snapshot_hash :corpus_list_hash :predicate_set_hash])

(def qualification-string-keys
  [:aat_adapter :aat_adapter_version :mapping_id :mapping_version
   :parser_ir_schema_id :parser_git_rev])

(defn- nonblank-string?
  [value]
  (and (string? value) (not (string/blank? value))))

(defn- valid-instrument-versions?
  [versions]
  (and (map? versions)
       (seq versions)
       (every? (fn [[instrument version]]
                 (and (or (keyword? instrument) (nonblank-string? instrument))
                      (nonblank-string? version)))
               versions)))

(defn- valid-instrument-policy-hashes?
  "Shape only. Which instruments must appear is a campaign question, decided
  by the closed `instrument-policy-paths` membership that builds this map;
  this gate owns the contract that every entry names an instrument and binds
  a real content hash rather than a version string or a placeholder."
  [hashes]
  (and (map? hashes)
       (seq hashes)
       (every? (fn [[instrument value]]
                 (and (or (keyword? instrument) (nonblank-string? instrument))
                      (string? value)
                      (boolean (re-matches hash/hash-pattern value))))
               hashes)))

(defn identity-values-valid?
  "The typed, nonblank value contract every qualification identity must
  satisfy, independent of whether the key set itself is complete. Public
  because the source-accountability instrument authenticates the same
  identity and must apply the same rule rather than a copy of it."
  [identity]
  (and (pos-int? (:aat_version identity))
       (every? #(nonblank-string? (get identity %)) qualification-string-keys)
       (every? #(and (string? (get identity %))
                     (re-matches hash/hash-pattern (get identity %)))
               qualification-hash-keys)
       (valid-instrument-versions? (:instrument_versions identity))
       (valid-instrument-policy-hashes? (:instrument_policy_hashes identity))))

(defn admission-query
  [identity]
  (into (array-map)
        (map (fn [key] [key (get identity key)]))
        admission-identity-keys))

(defn admitted?
  [registry identity]
  (and (= admission-identity-keys compat/match-keys)
       (= (set admission-identity-keys)
          (set (keys (admission-query identity))))
       (compat/compatible? registry (admission-query identity))))

(defn admission-resolution
  "Resolve gate membership and full-evidence admission without conflating them.

  Nine-field compatibility is necessary, but a full-entry conflict or invalid
  candidate takes precedence and prevents qualification."
  [registry identity admission-candidate]
  (let [query (admission-query identity)
        report (compat/admission-report registry admission-candidate)
        candidates (:entries admission-candidate)
        candidate-query (when (= 1 (count candidates))
                          (select-keys (first candidates) compat/match-keys))
        membership? (and (= admission-identity-keys compat/match-keys)
                         (compat/compatible? registry query))
        status (cond
                 (contains? #{:invalid-registry :invalid-candidates} (:status report)) :invalid
                 (not= candidate-query query) :invalid
                 (= :conflict (:status report)) :conflict
                 (and membership? (= :admitted (:status report))) :admitted
                 :else :unadmitted)]
    {:status status :query query :report report}))

(defn qualification-identity-ref
  "Canonical content identity of the full qualification identity value."
  [identity]
  (let [json-value (canonical-json-value identity)]
    (hash/format-sha256 (hash/sha256-json-jcs json-value))))

(defn coherence-errors
  [identity observations]
  (let [expected (qualification-identity-ref identity)]
    (->> observations
         (keep (fn [[observed-key envelope]]
                 (cond
                   (seq (capture/envelope-errors envelope))
                   (str (name observed-key) " has an invalid observation envelope")

                   (not= expected (:identity_ref envelope))
                   (str (name observed-key) " identity_ref does not match qualification identity")

                   :else nil)))
         vec)))

(defn coherent-observations?
  [identity observations]
  (empty? (coherence-errors identity observations)))

(defn- pinned-contract-errors
  [identity corpus predicate-set]
  (let [present (set (keys identity))
        required (set qualification-identity-keys)
        missing (sort (map name (set/difference required present)))
        unexpected (sort (map name (set/difference present required)))]
    (cond-> []
      (seq missing)
      (conj (str "qualification identity is missing required fields: "
                 (string/join ", " missing)))

      (seq unexpected)
      (conj (str "qualification identity has unexpected fields: "
                 (string/join ", " unexpected)))

      (not (identity-values-valid? identity))
      (conj "qualification identity values violate the typed, nonblank identity contract")

      (not= (:corpus_list_hash identity) (:list_hash corpus))
      (conj "qualification identity corpus_list_hash does not match the pinned corpus")

      (not= (:corpus_snapshot_hash identity) (:corpus_snapshot_hash corpus))
      (conj "qualification identity corpus_snapshot_hash does not match the pinned corpus")

      (not= (:predicate_set_hash identity) (:predicate_set_hash predicate-set))
      (conj "qualification identity predicate_set_hash does not match the pinned predicate set"))))

(def predicate-result-schema
  [:map
   [:predicate_id :keyword]
   [:dimension :string]
   [:instrument :string]
   [:unit :string]
   [:expected [:map [:comparator [:enum := :<= :>=]] [:value :any]]]
   [:observed :any]
   [:verdict [:enum :pass :fail :unavailable]]])

(def report-schema
  [:map
   [:report_schema_version [:= "abc/parser-release-qualification-report/v3"]]
   [:report_id :string]
   [:gate_status [:enum :release-qualified :not-qualified]]
   [:identity [:map-of :keyword :any]]
   [:coherence [:map
                [:status [:enum :ok :error]]
                [:identity_ref :string]
                [:errors [:vector :string]]]]
   [:admission [:map
                [:status [:enum :admitted :unadmitted :conflict :invalid]]
                [:query [:map-of :keyword :any]]
                [:report :any]]]
   [:predicate_verdicts [:vector predicate-result-schema]]
   [:verdict_tally [:map-of :keyword :int]]])

(defn report-valid?
  [report]
  (m/validate report-schema report))

(defn report-explain
  [report]
  (m/explain report-schema report))

(defn build-report
  "Assemble the machine-readable qualification report from a captured
  measurement bundle. `identity` records the running parser tuple and the
  admitted-tuple comparison; `measurements` supplies the observed values."
  [{:keys [report_id corpus predicate-set registry identity measurements
           admission_candidate]}]
  (let [registry (or registry (compat/load-registry))
        results (evaluate predicate-set measurements)
        errors (into (coherence-errors identity measurements)
                     (pinned-contract-errors identity corpus predicate-set))
        admission (admission-resolution registry identity admission_candidate)
        precondition-ok? (and (empty? errors) (= :admitted (:status admission)))]
    {:report_schema_version "abc/parser-release-qualification-report/v3"
     :report_id report_id
     :gate_status (gate-status precondition-ok? results)
     :identity identity
     :coherence {:status (if (empty? errors) :ok :error)
                 :identity_ref (qualification-identity-ref identity)
                 :errors errors}
     :admission admission
     :predicate_verdicts results
     :verdict_tally (verdict-tally results)}))

;; Reads a captured measurement bundle EDN (produced by running the parser over
;; the pinned corpus on the measurement host) and writes the deterministic
;; report JSON. The bundle carries `:report_id`, `:identity`, and
;; `:measurements`; the corpus and predicate set are loaded from their pinned
;; artifacts. No observation is synthesized here; absent keys become
;; `:unavailable` verdicts by construction.

(defn report-from-bundle
  [bundle]
  (build-report {:report_id (:report_id bundle)
                 :corpus (load-corpus)
                 :predicate-set (load-predicates)
                 :identity (:identity bundle)
                 :admission_candidate (:admission_candidate bundle)
                 :measurements (:measurements bundle)}))

(defn -main
  [& [bundle-path out-path]]
  (when-not (and bundle-path out-path)
    (throw (ex-info "usage: -m ab-research.parser-release-qualification <bundle.edn> <report.json>"
                    {:args [bundle-path out-path]})))
  (let [report (report-from-bundle (files/read-edn bundle-path))]
    (json/write-deterministic-json-file! out-path report)
    (binding [*out* *err*]
      (println "gate_status" (name (:gate_status report))
               "tally" (pr-str (:verdict_tally report))))
    report))
