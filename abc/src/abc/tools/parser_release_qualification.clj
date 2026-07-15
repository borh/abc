(ns abc.tools.parser-release-qualification
  "Release-qualification gate for the project-owned custom parser.

  Admission (ADR 0023 exact tuple) and release qualification (this gate) are
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
    The release evidence-class boundary itself lives in `abc.tools.parser-evidence`
    (`assert-release-evidence!`, ADR 0038 / Task 6) for any path that does ingest
    citations."
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.json :as json]
            [clojure.string :as string]
            [malli.core :as m]))

;; --- Artifact locations -------------------------------------------------------

(def corpus-path
  (files/path "data" "parser-release-qualification-corpus.edn"))

(def predicates-path
  (files/path "data" "parser-release-qualification-predicates.edn"))

;; --- Pinned-corpus identity ---------------------------------------------------

(def corpus-entry-identity-keys
  "Fields that make a corpus entry's contract identity. The list hash is taken
  over exactly these fields, in file order, so a reason edit does not silently
  change corpus identity but a work/path/hash/category/expected-status edit
  does."
  [:work_id :source_path :source_sha256 :category :expected_status])

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

(defn load-predicates
  ([] (load-predicates (files/read-edn predicates-path)))
  ([predicates] predicates))

;; --- Predicate evaluation -----------------------------------------------------

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
  (let [observed (get measurements observed_key)
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

(defn verdict-tally
  [results]
  (frequencies (map :verdict results)))

(defn gate-status
  "The gate is `:release-qualified` only when EVERY predicate verdict is
  `:pass`. Any `:fail` or `:unavailable` verdict yields `:not-qualified`."
  [results]
  (if (and (seq results) (every? #(= :pass (:verdict %)) results))
    :release-qualified
    :not-qualified))

(defn adr-0039-status
  "ADR 0039 promotion rule: `Accepted` only on a fully passing gate; otherwise
  it stays `Proposed`."
  [results]
  (if (= :release-qualified (gate-status results))
    "Accepted"
    "Proposed"))

;; --- Report schema + assembly -------------------------------------------------

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
   [:report_id :string]
   [:gate_status [:enum :release-qualified :not-qualified]]
   [:adr_0039_status [:enum "Accepted" "Proposed"]]
   [:identity [:map
               [:parser :string]
               [:adapter_version :string]
               [:admitted_tuple_adapter_version :string]
               [:admitted_tuple_matches :boolean]
               [:corpus_list_hash :string]
               [:corpus_snapshot_hash :string]]]
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
  [{:keys [report_id corpus predicate-set identity measurements]}]
  (let [results (evaluate predicate-set measurements)]
    {:report_id report_id
     :gate_status (gate-status results)
     :adr_0039_status (adr-0039-status results)
     :identity (assoc identity
                      :corpus_list_hash (:list_hash corpus)
                      :corpus_snapshot_hash (:corpus_snapshot_hash corpus))
     :predicate_verdicts results
     :verdict_tally (verdict-tally results)}))

;; --- Capture entrypoint -------------------------------------------------------
;;
;; Reads a captured measurement bundle EDN (produced by running the parser over
;; the pinned corpus on the measurement host) and writes the deterministic
;; report JSON. The bundle carries `:report_id`, `:identity`, and
;; `:measurements`; the corpus and predicate set are loaded from their pinned
;; artifacts. No observation is ever synthesized here — absent keys become
;; `:unavailable` verdicts by construction.

(defn report-from-bundle
  [bundle]
  (build-report {:report_id (:report_id bundle)
                 :corpus (load-corpus)
                 :predicate-set (load-predicates)
                 :identity (:identity bundle)
                 :measurements (:measurements bundle)}))

(defn -main
  [& [bundle-path out-path]]
  (when-not (and bundle-path out-path)
    (throw (ex-info "usage: -m abc.tools.parser-release-qualification <bundle.edn> <report.json>"
                    {:args [bundle-path out-path]})))
  (let [report (report-from-bundle (files/read-edn bundle-path))]
    (json/write-deterministic-json-file! out-path report)
    (binding [*out* *err*]
      (println "gate_status" (name (:gate_status report))
               "adr_0039_status" (:adr_0039_status report)
               "tally" (pr-str (:verdict_tally report))))
    report))
