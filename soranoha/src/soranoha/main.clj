;; Kernel CLI. `build` runs the full per-work stage graph at an aozorabunko
;; checkout revision into the kura store; output is CAS + trace results plus
;; a disposable run report (a query/export over the trace store — no
;; independent identity, no schema, no retention promise). The build itself
;; carries no manifest, signing, or publishing: those operate on admission
;; evidence the kernel never sees. `release` composes the scheduled release
;; pipeline — build, then the za driver's assembly and publication
;; transaction, with the assessment snapshot, policy value, signing seed,
;; reviewed assessment source records, and pinned verifier keys as fail-closed
;; file inputs. `assessment-evaluate` regenerates a snapshot and an optional
;; internal RDF view. `governance`
;; appends one offline-signed event; `serving-tree` exports the verified
;; chain's serving tree; `archive-verify` runs the archival observation
;; over a sole archived view. `delta` runs
;; the three-set delta oracle over two run reports; `verify` runs the
;; kura determinism + fixity report.
(ns soranoha.main
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [babashka.process :as process]
            [charred.api :as json]
            [clojure.string :as string]
            [soranoha.assessment.aozora :as aozora]
            [soranoha.assessment.evaluate :as assessment-evaluator]
            [soranoha.assessment.records :as assessment-records]
            [soranoha.assessment.rdf :as assessment-rdf]
            [soranoha.assessment.snapshot :as assessment-snapshot]
            [soranoha.assessment.source :as assessment-source]
            [soranoha.core.config :as config]
            [soranoha.core.hash :as hash]
            [soranoha.core.canonical :as canonical]
            [soranoha.kura.engine :as engine]
            [soranoha.kura.cas :as cas]
            [soranoha.kura.trace :as trace]
            [soranoha.kura.verify :as kura-verify]
            [soranoha.ori.stages :as stages]
            [soranoha.ori.validate :as validate]
            [soranoha.core.json :as record-json]
            [soranoha.core.parallel :as parallel]
            [soranoha.snh.decode :as decode]
            [soranoha.snh.semantic :as semantic]
            [soranoha.snh.sign :as sign]
            [soranoha.snh.transact :as transact]
            [soranoha.snh.verify :as verify]
            [soranoha.snh.view :as view]
            [soranoha.aozora.csv :as csv]
            [soranoha.yomi.catalog :as catalog]
            [soranoha.yomi.select :as select]
            [soranoha.za.oracle :as oracle]
            [soranoha.za.release :as za-release]
            [soranoha.za.scaffold :as scaffold]
            [soranoha.za.serve :as serve])
  (:gen-class))

(defn- git! [aozora-root & args]
  (try
    (let [{:keys [exit out]} (process/sh (into ["git" "-C" (str aozora-root)] args))]
      (when (zero? exit) (string/trim out)))
    (catch java.io.IOException _ nil)))

(defn source-provenance!
  "Fail-closed source trust gate, before any store write: the checkout must
  answer rev-parse and be clean under cards/ and index_pages/."
  [aozora-root]
  (let [commit (git! aozora-root "rev-parse" "HEAD")
        status (git! aozora-root "status" "--porcelain" "--" "cards" "index_pages")]
    (when-not commit
      (throw (ex-info "source git unavailable" {:aozora-root (str aozora-root)})))
    (when-not (some-> status string/blank?)
      (throw (ex-info "source git dirty under cards/index_pages"
                      {:aozora-root (str aozora-root) :status status})))
    commit))

(defn- build-stages [{:keys [clj-toolchain-id assets-root]}]
  (let [adapter (stages/resolve-adapter)
        profile (validate/profile-paths assets-root)]
    {:extract (stages/extract-stage clj-toolchain-id)
     :metadata (stages/metadata-stage clj-toolchain-id assets-root)
     :parse (stages/parse-stage adapter)
     :convert (stages/convert-stage adapter)
     :render (stages/render-stage clj-toolchain-id)
     :validate (stages/validate-tei-stage clj-toolchain-id profile)
     :fidelity (stages/source-fidelity-stage clj-toolchain-id)}))

(defn- read-cas-json [store hex]
  (json/read-json (String. ^bytes (cas/get-bytes (:cas-dir store) hex) "UTF-8")))

(defn run-work!
  "Execute (or trace-skip) the supplied stages for one selected work.
  Returns {:slug :zip-hex :source-facts
  :outputs {stage-key {name hex}} :cached {stage-key bool}
  :trace-keys {stage-key derivation-key-hex}}."
  [store {:keys [extract metadata parse convert render validate fidelity]}
   {:keys [slug row file]} catalog-rows]
  (let [zip-hex (cas/put-file! (:cas-dir store) file)
        extract-r (engine/run-stage! store extract {"zip" zip-hex})
        facts (read-cas-json store (get (:outputs extract-r) "source-facts"))
        metadata-r (engine/run-stage! store metadata
                                      {"catalog-rows" (cas/put-bytes! (:cas-dir store)
                                                                      (canonical/rfc8785-safe-integer-json-bytes-v1 catalog-rows))
                                       "work_id" (catalog/row-work-id row)})
        parse-r (engine/run-stage! store parse
                                   {"source" (get (:outputs extract-r)
                                                  "primary-text")})
        convert-r (engine/run-stage! store convert
                                     {"aat" (get (:outputs parse-r) "aat")
                                      "work_content_hash"
                                      (get facts "work_content_hash")})
        render-r (engine/run-stage! store render
                                    {"parser-ir" (get (:outputs convert-r)
                                                      "parser-ir")
                                     "metadata-record" (get (:outputs metadata-r)
                                                            "metadata-record")
                                     "persons" (get (:outputs metadata-r)
                                                    "persons")})
        validate-r (engine/run-stage! store validate
                                      {"tei" (get (:outputs render-r) "tei")})
        fidelity-r (when fidelity
                     (engine/run-stage! store fidelity
                                        {"source" (get (:outputs extract-r) "primary-text")
                                         "tei" (get (:outputs render-r) "tei")
                                         "plaintext" (get (:outputs render-r) "plaintext")}))
        results (cond-> {:extract extract-r :metadata metadata-r :parse parse-r
                         :convert convert-r :render render-r :validate validate-r}
                  fidelity-r (assoc :fidelity fidelity-r))
        project (fn [field] (into {} (map (fn [[stage result]] [stage (get result field)])) results))]
    {:slug slug :zip-hex zip-hex :source-facts facts
     :outputs (project :outputs) :cached (project :cached?) :trace-keys (project :trace-key)}))

(defn- export-build!
  "Write reviewable outputs to a new directory, outside the computation cache."
  [root out report]
  (let [out (fs/absolutize out)]
    (fs/create-dirs (fs/parent out))
    (fs/create-dir out)
    (doseq [[slug work] (get report "works")]
      (when-not (= slug (str (fs/file-name slug)))
        (throw (ex-info "invalid export slug" {:slug slug})))
      (let [dir (fs/path out slug)]
        (fs/create-dir dir)
        (doseq [[kind filename] [["tei" "tei.xml"] ["plaintext" "plain.txt"]
                                 ["tei-validation" "tei-validation.json"]
                                 ["source-fidelity" "source-fidelity.json"]]
                :when (contains? work kind)]
          (fs/write-bytes (fs/path dir filename)
                          (cas/get-bytes (config/cas-dir root) (get work kind))))))
    (spit (str (fs/path out "build.json"))
          (record-json/write-deterministic-json-str report))))

(defn- capture-build!
  [{:keys [root aozora-root clj-toolchain-id limit out]}]
  (when (string/blank? clj-toolchain-id)
    ;; fail closed: the toolchain identity keys every pure-Clojure stage's
    ;; derivations and lands in release provenance; a constant default
    ;; would let dependency or runtime changes retain stale derivations
    (throw (ex-info "clj toolchain identity required; the build wrapper must pass --clj-toolchain-id"
                    {:option "--clj-toolchain-id"})))
  (when (and out (fs/exists? out))
    (throw (ex-info "build export directory already exists" {:reason :export-output-exists})))
  (let [root (config/ensure-layout! (config/root root))
        commit (source-provenance! aozora-root)
        {:keys [csv-text catalog-csv-hash]} (catalog/read-catalog-zip aozora-root)
        rows (csv/read-rows-from-string csv-text)
        {:keys [candidates rejected]} (select/select-candidates aozora-root rows)
        candidates (if (and limit (pos? limit))
                     (vec (take limit candidates))
                     candidates)]
    {:root root :commit commit :catalog-csv-hash catalog-csv-hash
     :rows rows :candidates candidates :rejected rejected}))

(defn- selected-metadata! [{:keys [rows candidates]}]
  (let [selected-work-ids (set (map #(catalog/row-work-id (:row %)) candidates))]
    (doseq [row rows
            :when (and (selected-work-ids (catalog/row-work-id row))
                       (get row csv/ragged-key))]
      (throw (ex-info "selected work has a ragged catalog row"
                      {:reason :ragged-metadata-row
                       :work-id (catalog/row-work-id row)})))))

(defn execute-build!
  "Run the supplied candidates with the supplied stages and export their build report."
  [{:keys [concurrency clj-toolchain-id out]}
   {:keys [root commit catalog-csv-hash rows candidates rejected]} stage-set]
  (let [store (engine/open-store! {:cas-dir (config/cas-dir root)
                                   :db-path (config/trace-db-path root)})]
    (try
      (let [rows-by-work (group-by catalog/row-work-id rows)
            n (if (pos? concurrency)
                concurrency
                (.availableProcessors (Runtime/getRuntime)))
            started (System/currentTimeMillis)
            results (parallel/ordered-pmap
                     n
                     (fn [candidate]
                       (try
                         (run-work! store stage-set candidate
                                    (get rows-by-work (catalog/row-work-id (:row candidate))))
                         (catch Exception e
                           (throw (ex-info (ex-message e)
                                           (assoc (ex-data e) :slug (:slug candidate)) e)))))
                     candidates)
            relpath-of (into {} (map (juxt :slug :relpath)) candidates)
          ;; the report is a disposable trace-store export, but it must
          ;; carry everything the second-revision delta oracle consumes:
          ;; per-work source identity, per-stage cache decisions, the
          ;; selection join, and the stage coordinates
            report {"aozora_git_commit" commit
                    "catalog_csv_hash" catalog-csv-hash
                  ;; Captured before execution, independently of result rows.
                    "selected_slugs" (vec (sort (map :slug candidates)))
                    "rejected_count" (count rejected)
                    "executed_stage_count" (count (filter false?
                                                          (mapcat (comp vals :cached)
                                                                  results)))
                    "clj_toolchain_id" clj-toolchain-id
                  ;; coordinate values pass through unchanged; only the
                  ;; outer logical stage keys become strings
                    "stages" (into (sorted-map)
                                   (map (fn [[stage coordinate]]
                                          [(name stage) coordinate]))
                                   (trace/stage-coordinates stage-set))
                    "works" (into (sorted-map)
                                  (map (fn [{:keys [slug outputs cached zip-hex
                                                    source-facts trace-keys]}]
                                         [slug (cond-> {"tei" (get-in outputs [:render "tei"])
                                                        "plaintext" (get-in outputs
                                                                            [:render "plaintext"])
                                                        "tei-validation"
                                                        (get-in outputs
                                                                [:validate "tei-validation"])
                                                        "parser-ir" (get-in outputs
                                                                            [:convert "parser-ir"])
                                                        "source_zip" zip-hex
                                                        "source_relpath" (get relpath-of slug)
                                                        "source_content_hash"
                                                        (get source-facts "work_content_hash")
                                                        "cached" (into (sorted-map)
                                                                       (map (fn [[stage hit?]]
                                                                              [(name stage)
                                                                               hit?]))
                                                                       cached)
                                                      ;; The delta oracle explains execution using
                                                      ;; changed derivation keys at equal coordinates.
                                                        "trace_keys"
                                                        (into (sorted-map)
                                                              (map (fn [[stage k]]
                                                                     [(name stage) k]))
                                                              trace-keys)}
                                                 (:fidelity outputs)
                                                 (assoc "source-fidelity" (get-in outputs [:fidelity "source-fidelity"])))]))
                                  results)}
            report-path (str (fs/path root "runs" (str "run-" started ".json")))]
        (fs/create-dirs (fs/parent report-path))
        (spit report-path (record-json/write-deterministic-json-str report))
        (when out (export-build! root out report))
        (println (str "run_report: " report-path))
        (println (str "selected: " (count candidates)))
        (when out (println (str "exports: " (fs/absolutize out))))
        report)
      (finally (engine/close-store! store)))))

(defn build! [opts]
  (let [captured (capture-build! opts)]
    (selected-metadata! captured)
    (execute-build! opts captured
                    (build-stages opts))))

(defn delta!
  "Upstream-revision qualification: the three-set delta oracle over two
  build run reports (strictly decoded), printed as deterministic JSON.
  Run against each candidate aozorabunko revision's report and the
  previous qualified run's; comparison requires matching
  stage-coordinate tables — differing coordinates fail as incomparable
  and require a new baseline run. The reports themselves stay
  disposable. `ok` requires zero unexplained executions; the source and
  artifact deltas are descriptive (content hashes already establish
  what changed, and no executed stage is evidence for or against an
  artifact change — a warm cache can produce changed bytes without
  executing anything)."
  [{:keys [report-a report-b]}]
  (let [run-a (oracle/decode-run (fs/read-all-bytes (str report-a)))
        run-b (oracle/decode-run (fs/read-all-bytes (str report-b)))
        violations (oracle/unexplained-executions run-a run-b)
        source (oracle/source-delta run-a run-b)
        artifacts (oracle/report-artifact-delta run-a run-b)
        executed (oracle/executed-stages run-b)
        sorted-slugs (fn [slugs] (vec (sort slugs)))
        delta-json (fn [d] (into (sorted-map)
                                 (map (fn [[k v]] [(name k) (sorted-slugs v)]))
                                 d))
        stage-counts (into (sorted-map)
                           (map (fn [[stage runs]]
                                  [(name stage) (count runs)]))
                           (group-by identity (mapcat val executed)))
        touched (into (:added source) (:changed source))
        result {"commit_a" (:commit run-a)
                "commit_b" (:commit run-b)
                "source_delta" (delta-json source)
                "artifact_delta" (delta-json artifacts)
                "executed_stage_counts" stage-counts
                "executed_for_touched_sources"
                (into (sorted-map)
                      (keep (fn [slug]
                              (when-let [stages (seq (get executed slug))]
                                [slug (vec (sort (map name stages)))])))
                      (sorted-slugs touched))
                "unexplained_executions"
                (mapv (fn [{:keys [slug stage trace-key]}]
                        {"slug" slug "stage" (name stage)
                         "trace_key" trace-key})
                      (sort-by (juxt :slug #(name (:stage %))) violations))
                "ok" (empty? violations)}]
    (println (record-json/write-deterministic-json-str result))
    result))

(defn- require-flags! [command flags]
  (doseq [[flag value] flags]
    (when (string/blank? (str value))
      (throw (ex-info (str flag " is required for " command) {:option flag})))))

(defn- pinned-keys-from-files
  "The pinned verifier configuration from the protocol's role-named
  65-byte hex+LF public-key files."
  [release-pub governance-pub]
  (sign/validate-pinned-keys!
   {:release (sign/parse-hex64-lf (fs/read-all-bytes (str release-pub)))
    :governance (sign/parse-hex64-lf (fs/read-all-bytes (str governance-pub)))}))

(defn- read-signing-seed
  "Parse a signing-seed file (64 lowercase hex + optional surrounding
  whitespace) into 32 bytes. The file content is a secret: rejection
  names only the file, never the content — hex->bytes would otherwise
  carry the rejected text into exception data, which the CLI prints."
  ^bytes [path]
  (let [text (string/trim (slurp (str path)))]
    (when-not (re-matches #"[0-9a-f]{64}" text)
      (throw (ex-info "release key file must contain exactly 64 lowercase hex characters"
                      {:reason :malformed-release-key :file (str path)})))
    (sign/hex->bytes text)))

(defn- assessment-inputs!
  [{:keys [assessment-source evidence-root as-of]}]
  (require-flags! "assessment evaluation"
                  {"--assessment-source" assessment-source "--as-of" as-of})
  (try
    (when-not (= as-of (str (java.time.LocalDate/parse as-of)))
      (throw (java.time.DateTimeException. "noncanonical date")))
    (catch java.time.DateTimeException _
      (throw (ex-info "--as-of must be a real YYYY-MM-DD date"
                      {:reason :assessment-invalid-date :value as-of}))))
  (let [source-bytes (fs/read-all-bytes (str assessment-source))
        source (:value (assessment-records/decode source-bytes))]
    (when (and (seq (get source "reliances")) (string/blank? evidence-root))
      (throw (ex-info "Aozora reliance requires --evidence-root"
                      {:reason :missing-evidence-root})))
    {:source source :source-bytes source-bytes
     :retained (assessment-source/retained-observations source evidence-root)}))

(defn- committed-assessment-inputs!
  "Require the specific reviewed files to match one checkout's committed
  bytes. Unrelated working-tree edits and later unrelated commits do not
  affect the authority of these records."
  [opts source-bytes snapshot-bytes]
  (let [revisions
        (mapv
         (fn [[path expected]]
           (let [file (fs/real-path (str path))
                 directory (str (fs/parent file))
                 git (fn [& args] (apply process/sh {:dir directory} "git" args))
                 top (git "rev-parse" "--show-toplevel")
                 head (git "rev-parse" "HEAD")]
             (when-not (and (zero? (:exit top)) (zero? (:exit head)))
               (throw (ex-info "assessment input must be committed"
                               {:reason :uncommitted-assessment-input :file (str path)})))
             (let [repo (fs/real-path (string/trim (:out top)))
                   revision (string/trim (:out head))
                   relative (str (fs/relativize repo file))
                   blob (process/sh {:dir (str repo) :out :bytes}
                                    "git" "show" (str revision ":" relative))]
               (when-not (and (zero? (:exit blob))
                              (java.util.Arrays/equals ^bytes expected ^bytes (:out blob))
                              (java.util.Arrays/equals ^bytes expected
                                                       ^bytes (fs/read-all-bytes file)))
                 (throw (ex-info "assessment input differs from committed reviewed bytes"
                                 {:reason :uncommitted-assessment-input :file (str path)})))
               [(str repo) revision])))
         [[(:assessment-source opts) source-bytes] [(:assessment opts) snapshot-bytes]])]
    (when-not (apply = revisions)
      (throw (ex-info "assessment source and snapshot must share a reviewed revision"
                      {:reason :assessment-input-revisions-differ})))
    true))

(defn- cached-source-hash [store clj-toolchain-id]
  (let [stage (stages/extract-stage clj-toolchain-id)]
    (fn [file]
      (let [zip (cas/put-file! (:cas-dir store) file)
            run (engine/run-stage! store stage {"zip" zip})
            digest (get-in run [:outputs "source-facts"])
            bytes (cas/get-bytes (:cas-dir store) digest)]
        (when-not (and bytes (= digest (hash/sha256-bytes bytes)))
          (throw (ex-info "Cached source facts failed fixity verification"
                          {:reason :source-facts-corrupt :digest digest})))
        (let [facts (json/read-json (String. ^bytes bytes "UTF-8"))
              content-hash (get facts "work_content_hash")]
          (when-not (and (= (str "sha256:" zip) (get facts "archive_hash"))
                         (string? content-hash) (re-matches hash/hash-pattern content-hash))
            (throw (ex-info "Cached source facts do not describe the current archive"
                            {:reason :source-facts-mismatch :digest digest})))
          content-hash)))))

(defn- evaluate-assessment!
  [{:keys [root aozora-root evidence-root as-of clj-toolchain-id rdf-out rdf-base aozora-fetch]}
   {:keys [source retained]}]
  (require-flags! "assessment evaluation"
                  {"--root" (config/root root)
                   "--aozora-root" aozora-root
                   "--clj-toolchain-id" clj-toolchain-id})
  (when rdf-out
    (require-flags! "internal RDF export" {"--rdf-base" rdf-base}))
  (let [root (config/ensure-layout! (config/root root))
        store (engine/open-store! {:cas-dir (config/cas-dir root)
                                   :db-path (config/trace-db-path root)})]
    (try
      (let [commit (source-provenance! aozora-root)
            source-hash (cached-source-hash store clj-toolchain-id)
            captured (assessment-source/capture-checkout aozora-root source retained source-hash)
            reliance-observations (when (seq (get source "reliances"))
                                    (aozora/check! aozora-root evidence-root (get source "reliances")
                                                   {:fetch aozora-fetch :source-hash source-hash}))
            _ (when-not (= commit (source-provenance! aozora-root))
                (throw (ex-info "source checkout changed during assessment capture"
                                {:reason :assessment-source-changed})))
            evaluation (assessment-evaluator/evaluate!
                        store source
                        (assoc captured :as-of as-of :toolchain-id clj-toolchain-id
                               :reliance-observations reliance-observations))
            snapshot (assessment-snapshot/encode evaluation)]
        (when rdf-out
          (let [rdf (assessment-rdf/project!
                     store evaluation {:base-iri rdf-base
                                       :mapping-profile assessment-rdf/default-mapping-profile
                                       :toolchain-id clj-toolchain-id})]
            (fs/write-bytes (str rdf-out)
                            (cas/get-bytes (:cas-dir store)
                                           (get-in rdf [:outputs "nquads"])))))
        {:snapshot snapshot :evaluation evaluation
         :source-commit commit :source-hashes (:source-hashes captured)})
      (finally (engine/close-store! store)))))

(defn aozora-reliance-prepare!
  "Capture official edition evidence and write a draft owner source file.
  Refresh one edition or the full selection, preserving recorded exceptions.
  Preparing evidence does not publish or commit an acceptance."
  [{:keys [aozora-root evidence-root slug all out assessment-source as-of]}]
  (require-flags! "aozora-reliance-prepare"
                  {"--aozora-root" aozora-root "--evidence-root" evidence-root
                   "--out" out})
  (when (= (boolean slug) (boolean all))
    (throw (ex-info "choose exactly one of --slug or --all"
                    {:reason :invalid-preparation-selection})))
  (let [before (source-provenance! aozora-root)
        source (if assessment-source
                 (:value (assessment-records/decode (fs/read-all-bytes assessment-source)))
                 assessment-records/empty-source)
        today (str (java.time.LocalDate/now java.time.ZoneOffset/UTC))
        prior (into {} (map (juxt #(get % "slug") identity)) (get source "reliances"))
        options {:observed-at today :decision-date (or as-of today)}
        {:keys [records unavailable]}
        (if all
          (aozora/prepare-batch! aozora-root evidence-root nil options)
          {:records [(aozora/prepare! aozora-root evidence-root slug options)]
           :unavailable []})
        _ (when-not (and (= before (source-provenance! aozora-root))
                         (every? #(= before (get % "source_revision")) records))
            (throw (ex-info "source checkout changed during reliance preparation"
                            {:reason :assessment-source-changed})))
        refreshed (remove #(some? (get (prior (get % "slug")) "exception")) records)
        result (assessment-records/encode
                (assoc source "reliances"
                       (->> refreshed
                            (reduce #(assoc %1 (get %2 "slug") %2) prior)
                            vals (sort-by #(get % "slug")) vec)))]
    (fs/write-bytes out (:bytes result))
    (println (record-json/write-deterministic-json-str
              {"assessment_source" out
               "attempted" (+ (count records) (count unavailable))
               "refreshed" (count refreshed)
               "exceptions_preserved" (- (count records) (count refreshed))
               "unavailable" (mapv (fn [entry]
                                     {:slug (:slug entry) :reason (aozora/reason->wire entry)})
                                   unavailable)}))
    result))

(defn publication-init!
  "Initialize an absent publication branch using the normal pre-genesis commit."
  [{:keys [chain-clone branch]}]
  (require-flags! "publication-init" {"--chain-clone" chain-clone "--branch" branch})
  (let [commit (transact/init-publication-branch! chain-clone branch)]
    (println (record-json/write-deterministic-json-str {"commit" commit "branch" branch}))
    commit))

(defn assessment-evaluate!
  "Evaluate versioned assessment inputs against the current checkout and
  optionally write a snapshot and an internal RDF view. Does not publish."
  [{:keys [out] :as opts}]
  (let [result (evaluate-assessment! opts (assessment-inputs! opts))
        snapshot (:snapshot result)]
    (when out (fs/write-bytes (str out) (:bytes snapshot)))
    (println (record-json/write-deterministic-json-str
              {"assessment_snapshot" (:id snapshot)
               "candidates" (count (get-in snapshot [:value "candidates"]))
               "source_commit" (:source-commit result)}))
    result))

(defn- snapshot-drift [expected supplied]
  (let [by-slug #(into {} (map (juxt (fn [c] (get c "slug")) identity))
                       (get % "candidates"))
        expected-works (by-slug expected)
        supplied-works (by-slug supplied)
        changed (vec (sort (for [[slug work] expected-works
                                 :when (not= work (get supplied-works slug))]
                             slug)))]
    (merge (scaffold/projection-drift
            (scaffold/snapshot-projection expected)
            (scaffold/snapshot-projection supplied))
           {:changed-work-count (count changed)
            :changed-work-sample (vec (take 20 changed))})))

(defn release-preflight-drift
  "Regenerate the committed snapshot from reviewed source records and
  freshly captured observations. nil means canonical bytes agree."
  ([opts]
   (release-preflight-drift
    opts (:value (decode/decode "assessment-snapshot"
                                (fs/read-all-bytes (str (:assessment opts)))))))
  ([opts supplied]
   (let [expected (:snapshot (evaluate-assessment! opts (assessment-inputs! opts)))
         actual (decode/encode "assessment-snapshot" supplied)]
     (when-not (java.util.Arrays/equals ^bytes (:bytes expected) ^bytes (:bytes actual))
       (snapshot-drift (:value expected) supplied)))))

(defn release-preflight!
  "Validate file inputs and capture local sources before building.
  Live assessment applicability is checked at the publication boundary."
  [{:keys [aozora-root chain-clone upstream-origin assessment policy
           release-pub governance-pub release-key limit]
    :as opts}]
  (require-flags! "release"
                  {"--aozora-root" aozora-root
                   "--chain-clone" chain-clone
                   "--upstream-origin" upstream-origin
                   "--assessment" assessment
                   "--policy" policy
                   "--release-pub" release-pub
                   "--governance-pub" governance-pub
                   "--release-key" release-key})
  (when limit
    (throw (ex-info "--limit is refused for release; a release covers the full selection"
                    {:option "--limit"})))
  (when-not (semantic/absolute-origin? upstream-origin)
    (throw (ex-info "upstream origin must be an absolute URI"
                    {:reason :invalid-upstream-origin :origin upstream-origin})))
  (let [snapshot-bytes (fs/read-all-bytes (str assessment))
        _ (decode/decode "assessment-snapshot" snapshot-bytes)
        assessment-inputs (assessment-inputs! opts)
        authority (za-release/rights-authority!
                   (fs/read-all-bytes (str policy)))
        pinned (pinned-keys-from-files release-pub governance-pub)
        seed (read-signing-seed release-key)]
    (when-not (sign/seed-signs-for? seed (:release pinned))
      (throw (ex-info "release signing seed does not correspond to the pinned release key"
                      {:reason :seed-key-mismatch})))
    (committed-assessment-inputs! opts (:source-bytes assessment-inputs) snapshot-bytes)
    (require-flags! "assessment evaluation"
                    {"--root" (config/root (:root opts))
                     "--clj-toolchain-id" (:clj-toolchain-id opts)})
    (let [source-commit (source-provenance! aozora-root)
          {:keys [source-hashes]}
          (let [root (config/ensure-layout! (config/root (:root opts)))
                store (engine/open-store! {:cas-dir (config/cas-dir root)
                                           :db-path (config/trace-db-path root)})]
            (try
              (assessment-source/capture-checkout
               aozora-root (:source assessment-inputs) (:retained assessment-inputs)
               (cached-source-hash store (:clj-toolchain-id opts)))
              (finally (engine/close-store! store))))]
      (when-not (= source-commit (source-provenance! aozora-root))
        (throw (ex-info "source checkout changed during assessment capture"
                        {:reason :assessment-source-changed})))
      (merge authority
             {:snapshot-bytes snapshot-bytes
              :assessment-source-bytes (:source-bytes assessment-inputs)
              :assessment-inputs assessment-inputs
              :source-commit source-commit :source-hashes source-hashes
              :pinned pinned
              :sign-release (fn [manifest-hex]
                              (sign/sign seed
                                         (sign/manifest-message manifest-hex)))}))))

(defn release!
  "Preflight the full selection, then build the artifacts requested by verified-head assembly."
  [{:keys [root chain-clone branch upstream-origin out] :as opts}]
  (let [{:keys [snapshot-bytes policy-id policy-hash pinned sign-release
                source-commit source-hashes assessment-source-bytes assessment-inputs]}
        (release-preflight! opts)
        captured (capture-build! opts)
        last-report (volatile! nil)
        outcome
        (za-release/release!
         {:selection (mapv :slug (:candidates captured))
          :source-hashes source-hashes
          :build-works!
          (fn [slugs]
            (let [requested (set slugs)
                  inputs (update captured :candidates #(filterv (comp requested :slug) %))
                  _ (selected-metadata! inputs)
                  stage-set (if (seq requested) (dissoc (build-stages opts) :fidelity) {})
                  report (execute-build! (dissoc opts :out) inputs stage-set)]
              (when-not (and (= source-commit (get report "aozora_git_commit")
                                (source-provenance! (:aozora-root opts)))
                             (every? (fn [[slug digest]]
                                       (= digest (get-in report ["works" slug "source_content_hash"])))
                                     (select-keys source-hashes slugs)))
                (throw (ex-info "built sources differ from assessment inputs"
                                {:reason :assessment-source-changed-during-build})))
              (committed-assessment-inputs! opts assessment-source-bytes snapshot-bytes)
              (let [current (evaluate-assessment! (dissoc opts :rdf-out) assessment-inputs)]
                (when-not (and (= source-commit (:source-commit current))
                               (= source-hashes (:source-hashes current)))
                  (throw (ex-info "assessed sources changed during the build"
                                  {:reason :assessment-source-changed-during-build})))
                (when-not (java.util.Arrays/equals ^bytes snapshot-bytes
                                                   ^bytes (get-in current [:snapshot :bytes]))
                  (throw (ex-info "committed assessment snapshot differs from current evaluation"
                                  (assoc (snapshot-drift
                                          (get-in current [:snapshot :value])
                                          (:value (decode/decode "assessment-snapshot" snapshot-bytes)))
                                         :reason :snapshot-regeneration-drift)))))
              (committed-assessment-inputs! opts assessment-source-bytes snapshot-bytes)
              (vreset! last-report report)
              report))
          :cas-dir (config/cas-dir (config/root root))
          :upstream-origin upstream-origin
          :selection-params {}
          :policy-id policy-id :policy-hash policy-hash
          :snapshot-bytes snapshot-bytes
          :clone (str chain-clone) :branch branch
          :pinned-keys pinned :sign-release sign-release})]
    (when out (export-build! (config/root root) out @last-report))
    (println (record-json/write-deterministic-json-str
              (into (sorted-map)
                    (keep (fn [[k v]] (when v [k v])))
                    {"outcome" (name (:outcome outcome))
                     "manifest_id" (:manifest-id outcome)
                     "head" (:head outcome)
                     "commit" (:commit outcome)})))
    outcome))

(defn governance!
  "Append one offline-signed governance event to the chain. The event and
  its detached 64-byte signature arrive as files and pass unchanged to the
  publication transaction, which boundary-decodes the event, verifies the
  governance signature, and never rewrites or re-signs it; the release
  seed here signs only the successor manifest. Preflight covers the file
  inputs and the seed's correspondence with the pinned release key."
  [{:keys [chain-clone branch event event-sig
           release-pub governance-pub release-key]}]
  (require-flags! "governance"
                  {"--chain-clone" chain-clone
                   "--event" event
                   "--event-sig" event-sig
                   "--release-pub" release-pub
                   "--governance-pub" governance-pub
                   "--release-key" release-key})
  (let [pinned (pinned-keys-from-files release-pub governance-pub)
        seed (read-signing-seed release-key)
        _ (when-not (sign/seed-signs-for? seed (:release pinned))
            (throw (ex-info "release signing seed does not correspond to the pinned release key"
                            {:reason :seed-key-mismatch})))
        outcome (transact/publish-governance!
                 {:clone (str chain-clone)
                  :branch branch
                  :pinned-keys pinned
                  :sign-release (fn [manifest-hex]
                                  (sign/sign seed
                                             (sign/manifest-message manifest-hex)))
                  :event-bytes (fs/read-all-bytes (str event))
                  :event-sig (fs/read-all-bytes (str event-sig))})]
    (println (record-json/write-deterministic-json-str
              (into (sorted-map)
                    (keep (fn [[k v]] (when v [k v])))
                    {"outcome" (name (:outcome outcome))
                     "event" (:event outcome)
                     "manifest_id" (:manifest-id outcome)
                     "reason" (some-> (:reason outcome) name)})))
    outcome))

(defn serving-tree!
  "Export the serving tree (blobs/, releases/, governance/, plus the
  work-facing symlink layer works/, withdrawn/, releases/latest) from
  the verified chain into --out, which must not yet exist."
  [{:keys [chain-clone branch out release-pub governance-pub]}]
  (require-flags! "serving-tree"
                  {"--chain-clone" chain-clone
                   "--out" out
                   "--release-pub" release-pub
                   "--governance-pub" governance-pub})
  (let [result (serve/export-tree!
                {:clone (str chain-clone)
                 :branch branch
                 :pinned-keys (pinned-keys-from-files release-pub
                                                      governance-pub)
                 :out-dir (str out)})]
    (println (record-json/write-deterministic-json-str
              {"head" (:head result)
               "releases" (:releases result)
               "blobs" (:blobs result)}))
    result))

(defn serving-activate!
  "Activate a verified export under the deployment's provisioned serving root."
  [{:keys [chain-clone branch serve-root release-pub governance-pub]}]
  (require-flags! "serving-activate"
                  {"--chain-clone" chain-clone "--serve-root" serve-root
                   "--release-pub" release-pub "--governance-pub" governance-pub})
  (let [result (serve/activate!
                {:clone chain-clone :branch branch :serve-root serve-root
                 :pinned-keys (pinned-keys-from-files release-pub governance-pub)})]
    (println (record-json/write-deterministic-json-str
              {"head" (:head result) "commit" (:commit result)
               "releases" (:releases result) "blobs" (:blobs result)
               "reused" (:reused? result)}))
    result))

(defn deployment-options
  "Read deployment-owned CLI defaults. Explicit invocation options take precedence.
  Only paths and repository coordinates belong in this non-secret configuration."
  [opts]
  (if-let [path (:deployment opts)]
    (let [allowed #{"root" "aozora-root" "chain-clone" "branch" "upstream-origin"
                    "evidence-root" "serve-root" "release-pub" "governance-pub"}
          values (json/read-json (slurp path))]
      (when-not (and (map? values)
                     (every? (fn [[k v]] (and (contains? allowed k)
                                              (string? v) (not (string/blank? v))))
                             values))
        (throw (ex-info "Invalid deployment configuration"
                        {:reason :invalid-deployment-configuration :path path})))
      (merge (update-keys values keyword) opts))
    opts))

(defn archive-verify!
  "One archival observation: run the chain verifier with the archived
  copy as the sole repository view and print the disposable report,
  identified by the normalized local path of the observed view plus the
  commit; a materializer that binds a view to an external identifier
  replaces that locator with the bound identity. A view that cannot be
  constructed throws — a failure to perform the observation, never an
  observation; a readable view always yields a report, success or
  failed."
  [{:keys [archive commit release-pub governance-pub]}]
  (require-flags! "archive-verify"
                  {"--archive" archive
                   "--commit" commit
                   "--release-pub" release-pub
                   "--governance-pub" governance-pub})
  (let [archive-view (str (fs/canonicalize (str archive)))
        v (view/git-view archive-view)
        report (assoc (verify/archive-verification
                       v (str commit)
                       (pinned-keys-from-files release-pub governance-pub))
                      :archive-view archive-view)]
    (println (record-json/write-deterministic-json-str
              (into (sorted-map)
                    (keep (fn [[k v]] (when v [k v])))
                    {"result" (name (:result report))
                     "archive_view" (:archive-view report)
                     "commit" (:commit report)
                     "verifier_version" (:verifier-version report)
                     "key_fingerprints"
                     (into (sorted-map)
                           (map (fn [[role fp]] [(name role) fp]))
                           (:pinned-fingerprints report))
                     "head" (:head report)
                     "chain_length" (:chain-length report)
                     "reason" (some-> (:reason report) name)})))
    report))

(defn verify!
  [{:keys [root]}]
  (let [root (config/root root)
        store (engine/open-store! {:cas-dir (config/cas-dir root)
                                   :db-path (config/trace-db-path root)})
        report (kura-verify/verify store)]
    (engine/close-store! store)
    (println (str "determinism_violations: "
                  (count (:determinism-violations report))))
    (println (str "fixity: " (pr-str (:fixity report))))
    (println (str "ok: " (:ok? report)))
    report))

(def ^:private cli-spec
  {:root {:coerce :string}
   :aozora-root {:coerce :string}
   :assets-root {:coerce :string}
   :report-a {:coerce :string}
   :report-b {:coerce :string}
   :chain-clone {:coerce :string}
   :branch {:coerce :string}
   :upstream-origin {:coerce :string}
   :assessment {:coerce :string}
   :assessment-source {:coerce :string}
   :evidence-root {:coerce :string}
   :slug {:coerce :string}
   :all {:coerce :boolean}
   :as-of {:coerce :string}
   :rdf-out {:coerce :string}
   :rdf-base {:coerce :string}
   :policy {:coerce :string}
   :release-pub {:coerce :string}
   :governance-pub {:coerce :string}
   :release-key {:coerce :string}
   :event {:coerce :string}
   :event-sig {:coerce :string}
   :out {:coerce :string}
   :serve-root {:coerce :string}
   :deployment {:coerce :string}
   :archive {:coerce :string}
   :commit {:coerce :string}
   ;; default pinned to the measured resource envelope (peak RSS < 8 GiB
   ;; with -Xmx4g); 0 = one worker per available processor
   :concurrency {:coerce :long :default 16}
   :limit {:coerce :long}
   ;; no default: build! fails closed without a wrapper-supplied identity
   :clj-toolchain-id {:coerce :string}})

(defn -main [& args]
  (let [[command & rest-args] args
        parsed (cli/parse-opts rest-args {:spec cli-spec})]
    (try
      (let [opts (merge {:branch "main"} (deployment-options parsed))]
        (case command
          "build" (build! opts)
          "delta" (when-not (get (delta! opts) "ok")
                    (System/exit 1))
        ;; scheduled-runner exit contract: success covers the no-op; a
        ;; requeue asks the next scheduled run to retry from the new head;
        ;; a determinism halt is an ordinary failure
          "release" (let [{:keys [outcome]} (release! opts)]
                      (when-not (#{:published :already-published} outcome)
                        (System/exit (if (= :requeue outcome) 3 1))))
          "governance" (let [{:keys [outcome]} (governance! opts)]
                         (when-not (#{:published :already-applied} outcome)
                           (System/exit 1)))
          "serving-tree" (serving-tree! opts)
          "serving-activate" (serving-activate! opts)
          "publication-init" (publication-init! opts)
          "assessment-evaluate" (assessment-evaluate! opts)
          "aozora-reliance-prepare" (aozora-reliance-prepare! opts)
          "archive-verify" (when-not (= :success (:result (archive-verify! opts)))
                             (System/exit 1))
          "verify" (when-not (:ok? (verify! opts))
                     (System/exit 1))
          (do (binding [*out* *err*]
                (println "usage: build|delta|release|governance|serving-tree|serving-activate|publication-init|assessment-evaluate|aozora-reliance-prepare|archive-verify|verify [--root R --aozora-root A --assets-root S ...]"))
              (System/exit 2))))
      (System/exit 0)
      (catch clojure.lang.ExceptionInfo e
        (binding [*out* *err*]
          (println (str "error: " (.getMessage e) " " (pr-str (ex-data e)))))
        (System/exit 1))
      (catch java.io.IOException e
        (binding [*out* *err*]
          (println (str "error: input/output operation failed "
                        (pr-str {:reason :io-failure :detail (.getMessage e)}))))
        (System/exit 1)))))
