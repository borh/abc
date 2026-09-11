(ns soranoha.main
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [babashka.process :as process]
            [charred.api :as json]
            [clojure.string :as string]
            [soranoha.assessment.aozora :as aozora]
            [soranoha.assessment.evaluate :as assessment-evaluator]
            [soranoha.assessment.records :as assessment-records]
            [soranoha.assessment.snapshot :as assessment-snapshot]
            [soranoha.assessment.source :as assessment-source]
            [soranoha.core.config :as config]
            [soranoha.core.hash :as hash]
            [soranoha.core.rights :as rights]
            [soranoha.core.canonical :as canonical]
            [soranoha.kura.engine :as engine]
            [soranoha.kura.cas :as cas]
            [soranoha.kura.trace :as trace]
            [soranoha.ori.stages :as stages]
            [soranoha.ori.accountability :as accountability]
            [soranoha.ori.validate :as validate]
            [soranoha.core.json :as record-json]
            [soranoha.core.parallel :as parallel]
            [soranoha.snh.corpus-delta :as corpus-delta]
            [soranoha.snh.decode :as decode]
            [soranoha.snh.release-delta :as release-delta]
            [soranoha.snh.repo :as repo]
            [soranoha.snh.semantic :as semantic]
            [soranoha.snh.sign :as sign]
            [soranoha.snh.transact :as transact]
            [soranoha.snh.verify :as verify]
            [soranoha.snh.view :as view]
            [soranoha.aozora.csv :as csv]
            [soranoha.yomi.catalog :as catalog]
            [soranoha.yomi.select :as select]
            [soranoha.za.maturity :as maturity]
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

(defn- build-rights-grant
  "The rights grant the build embeds in every TEI header. Release supplies the
  grant it already read for the manifest, so one policy read serves both and
  the two published statements cannot diverge. Other build paths read the same
  document, by default the one under --assets-root, so a private export shows
  the terms a release would publish rather than none."
  [{:keys [rights assets-root policy]}]
  (or rights
      (rights/grant-from-bytes
       (fs/read-all-bytes (str (or policy
                                   (fs/path assets-root "data/publication-policy.edn")))))))

(defn- publication-stages
  "The stages that produce what a release publishes. Every artifact a
  manifest carries comes from one of these."
  [{:keys [clj-toolchain-id assets-root] :as opts}]
  (let [adapter (stages/resolve-adapter)
        profile (validate/profile-paths assets-root)]
    {:extract (stages/extract-stage clj-toolchain-id)
     :metadata (stages/metadata-stage clj-toolchain-id assets-root
                                      (stages/catalog-text-reader adapter))
     :parse (stages/parse-stage adapter)
     :convert (stages/convert-stage adapter)
     :render (stages/render-stage clj-toolchain-id (build-rights-grant opts))
     :plaintext (stages/plaintext-stage clj-toolchain-id)
     :markdown (stages/markdown-stage clj-toolchain-id)
     :validate (stages/validate-tei-stage clj-toolchain-id profile)}))

(def ^:private research-stages
  "The stages that measure how much of each source the parser accounted
  for, held as constructors rather than stages so a release never builds
  one: `source-stage` resolves an external scanner binary that a release
  has no reason to require.

  Nothing they produce is an artifact kind a manifest carries, so a
  release must not run them. `build` adds exactly these names and
  `release` removes exactly these names, from this one list, so the two
  cannot drift apart and a research stage cannot reach a manifest even
  if one is later added to the publication set by mistake."
  {:accountability (fn [_] (accountability/source-stage (accountability/resolve-tool)))
   :coverage (fn [opts] (accountability/coverage-stage (:clj-toolchain-id opts)))})

(defn- build-stages
  "The publication stages plus the research stages. This is the set
  `build` runs, and it is where the parser is measured today."
  [opts]
  (into (publication-stages opts)
        (map (fn [[stage make]] [stage (make opts)]))
        research-stages))

(defn- read-cas-json [store hex]
  (record-json/read-json-bytes (cas/get-bytes (:cas-dir store) hex)))

(defn run-work!
  "Execute (or trace-skip) the supplied stages for one selected work.
  Returns {:slug :zip-hex :source-facts
  :outputs {stage-key {name hex}} :cached {stage-key bool}
  :trace-keys {stage-key derivation-key-hex}}."
  [store {:keys [extract metadata parse convert render plaintext markdown validate accountability coverage]}
   {:keys [slug row file rights]} catalog-rows]
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
                                                    "persons")
                                     ;; a scalar, so the identifier a work was
                                     ;; rendered under is part of its derivation
                                     ;; key rather than invisible to the cache
                                     "slug" slug
                                     ;; a scalar for the same reason: the TEI
                                     ;; states this work's terms, so a work
                                     ;; whose standing changes has to be
                                     ;; re-rendered and not served from cache
                                     ;; under the terms it used to carry
                                     "works-standing" rights})
        plaintext-r (engine/run-stage! store plaintext
                                       {"tei" (get (:outputs render-r) "tei")})
        markdown-r (engine/run-stage! store markdown
                                      {"tei" (get (:outputs render-r) "tei")})
        validate-r (engine/run-stage! store validate
                                      {"tei" (get (:outputs render-r) "tei")})
        accountability-r (when accountability
                           (engine/run-stage! store accountability
                                              {"source" (get (:outputs extract-r) "primary-text")}))
        coverage-r (when coverage
                     (engine/run-stage! store coverage
                                        {"source-accountability" (get (:outputs accountability-r) "source-accountability")
                                         "parser-ir" (get (:outputs convert-r) "parser-ir")}))
        results (cond-> {:extract extract-r :metadata metadata-r :parse parse-r
                         :convert convert-r :render render-r :plaintext plaintext-r
                         :markdown markdown-r :validate validate-r}
                  accountability-r (assoc :accountability accountability-r)
                  coverage-r (assoc :coverage coverage-r))
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
                                 ["markdown" "text.md"]
                                 ["plaintext-projection" "plaintext-projection.json"]
                                 ["markdown-projection" "markdown-projection.json"]
                                 ["tei-validation" "tei-validation.json"]
                                 ["source-accountability" "source-accountability.json"]
                                 ["interpretation-coverage" "interpretation-coverage.json"]]
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

(defn- with-store
  "Call `f` with the run store under `root` open, closing it afterwards."
  [root f]
  (let [store (engine/open-store! {:cas-dir (config/cas-dir root)
                                   :db-path (config/trace-db-path root)})]
    (try (f store)
         (finally (engine/close-store! store)))))

(defn execute-build!
  "Run the supplied candidates with the supplied stages and export their build report."
  [{:keys [concurrency clj-toolchain-id out]}
   {:keys [root commit catalog-csv-hash rows candidates rejected]} stage-set]
  (with-store root
    (fn [store]
      (let [rows-by-work (group-by catalog/row-work-id rows)
            n (if (pos? concurrency)
                concurrency
                (.availableProcessors (Runtime/getRuntime)))
            started (System/currentTimeMillis)
            outcomes (parallel/ordered-pmap
                      n
                      (fn [candidate]
                        (try
                          {:work (run-work! store stage-set candidate
                                            (get rows-by-work (catalog/row-work-id (:row candidate))))}
                          (catch Exception e
                            {:failure {:slug (:slug candidate)
                                       :message (ex-message e)
                                       :data (ex-data e)}})))
                      candidates)
            failures (into [] (keep :failure) outcomes)
            _ (when (seq failures)
                (throw (ex-info (str (count failures) " work builds failed")
                                {:reason :build/work-failures :failures failures})))
            results (mapv :work outcomes)
            relpath-of (into {} (map (juxt :slug :relpath)) candidates)
            ;; selection decided this and the manifest publishes it; carrying
            ;; it through the report keeps the terms a work is published under
            ;; the same ones it was admitted under
            rights-of (into {} (map (juxt :slug :rights)) candidates)
          ;; the report is a disposable trace-store export, but it must
          ;; carry everything the second-revision delta oracle consumes:
          ;; per-work source identity, per-stage cache decisions, the
          ;; selection join, and the stage coordinates
            report {"aozora_git_commit" commit
                    "catalog_csv_hash" catalog-csv-hash
                    "selected_slugs" (vec (sort (map :slug candidates)))
                    "rejected_count" (count rejected)
                    ;; and the rejections themselves. A count alone says how
                    ;; many candidates the build declined and nothing about
                    ;; which, so a work excluded on its rights left no trace an
                    ;; operator could read. Each entry carries the path, the
                    ;; reason, and for a rights refusal the licence that was
                    ;; declined.
                    "rejected" (vec rejected)
                    "executed_stage_count" (count (filter false?
                                                          (mapcat (comp vals :cached)
                                                                  results)))
                    "clj_toolchain_id" clj-toolchain-id
                    "stages" (into (sorted-map)
                                   (map (fn [[stage coordinate]]
                                          [(name stage) coordinate]))
                                   (trace/stage-coordinates stage-set))
                    "works" (into (sorted-map)
                                  (map (fn [{:keys [slug outputs cached zip-hex
                                                    source-facts trace-keys]}]
                                         [slug (cond-> {"tei" (get-in outputs [:render "tei"])
                                                        "plaintext" (get-in outputs
                                                                            [:plaintext "plaintext"])
                                                        "markdown" (get-in outputs [:markdown "markdown"])
                                                        "plaintext-projection" (get-in outputs [:plaintext "plaintext-projection"])
                                                        "markdown-projection" (get-in outputs [:markdown "markdown-projection"])
                                                        "tei-validation"
                                                        (get-in outputs
                                                                [:validate "tei-validation"])
                                                        "parser-ir" (get-in outputs
                                                                            [:convert "parser-ir"])
                                                        ;; catalog inputs: the release
                                                        ;; assembler reads these back out
                                                        ;; of the CAS to build the signed
                                                        ;; bibliographic catalog
                                                        "metadata-record" (get-in outputs [:metadata "metadata-record"])
                                                        "persons" (get-in outputs [:metadata "persons"])
                                                        "primary_text_member"
                                                        (get source-facts "primary_text_member")
                                                        "source_zip" zip-hex
                                                        "source_relpath" (get relpath-of slug)
                                                        "source_content_hash"
                                                        (get source-facts "work_content_hash")
                                                        "rights" (get rights-of slug)
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
                                                 (:accountability outputs)
                                                 (assoc "source-accountability" (get-in outputs [:accountability "source-accountability"]))
                                                 (:coverage outputs)
                                                 (assoc "interpretation-coverage" (get-in outputs [:coverage "interpretation-coverage"]))
                                                 (get source-facts "trailing_bytes_after_archive")
                                                 (assoc "trailing_bytes_after_archive"
                                                        (get source-facts "trailing_bytes_after_archive")))]))
                                  results)}
            report-path (str (fs/path root "runs" (str "run-" started ".json")))]
        (fs/create-dirs (fs/parent report-path))
        (spit report-path (record-json/write-deterministic-json-str report))
        (when out (export-build! root out report))
        (println (str "run_report: " report-path))
        (println (str "selected: " (count candidates)))
        (when out (println (str "exports: " (fs/absolutize out))))
        report))))

(defn build! [opts]
  (let [captured (capture-build! opts)]
    (selected-metadata! captured)
    (execute-build! opts captured
                    (build-stages opts))))

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
  names only the file, never the content: hex->bytes would otherwise
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
      (throw (ex-info "Aozora Bunko reliance requires --evidence-root"
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
        (let [facts (record-json/read-json-bytes bytes)
              content-hash (get facts "work_content_hash")]
          (when-not (and (= (str "sha256:" zip) (get facts "archive_hash"))
                         (string? content-hash) (re-matches hash/hash-pattern content-hash))
            (throw (ex-info "Cached source facts do not describe the current archive"
                            {:reason :source-facts-mismatch :digest digest})))
          content-hash)))))

(defn- evaluate-assessment!
  [{:keys [root aozora-root evidence-root as-of clj-toolchain-id aozora-fetch]}
   {:keys [source retained]}]
  (require-flags! "assessment evaluation"
                  {"--root" (config/root root)
                   "--aozora-root" aozora-root
                   "--clj-toolchain-id" clj-toolchain-id})
  (let [root (config/ensure-layout! (config/root root))]
    (with-store root
      (fn [store]
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
          {:snapshot snapshot :evaluation evaluation
           :source-commit commit :source-hashes (:source-hashes captured)})))))

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

(defn- publication-init!
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
  [opts]
  (let [supplied (:value (decode/decode "assessment-snapshot"
                                        (fs/read-all-bytes (str (:assessment opts)))))
        expected (:snapshot (evaluate-assessment! opts (assessment-inputs! opts)))
        actual (decode/encode "assessment-snapshot" supplied)]
    (when-not (java.util.Arrays/equals ^bytes (:bytes expected) ^bytes (:bytes actual))
      (snapshot-drift (:value expected) supplied))))

(defn- assessment-drift!
  "Compare the committed snapshot with one regenerated now, and say how they
  differ. This is the check the release runs before it builds, exposed so an
  operator can run it after changing selection or assessment code and before
  dispatching a release. Prints the drift report, or a null drift when the
  bytes agree."
  [{:keys [assessment] :as opts}]
  (require-flags! "assessment-drift" {"--assessment" assessment})
  (let [drift (release-preflight-drift opts)]
    (println (record-json/write-deterministic-json-str
              (into (sorted-map) {"assessment" (str assessment) "drift" drift})))
    drift))

(defn- release-signer
  "Read the release seed at `release-key`, confirm it signs for the pinned
  release key, and return the function that signs a manifest hex with it."
  [release-key pinned]
  (let [seed (read-signing-seed release-key)]
    (when-not (sign/seed-signs-for? seed (:release pinned))
      (throw (ex-info "release signing seed does not correspond to the pinned release key"
                      {:reason :seed-key-mismatch})))
    (fn [manifest-hex]
      (sign/sign seed (sign/manifest-message manifest-hex)))))

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
        sign-release (release-signer release-key pinned)]
    (committed-assessment-inputs! opts (:source-bytes assessment-inputs) snapshot-bytes)
    (require-flags! "assessment evaluation"
                    {"--root" (config/root (:root opts))
                     "--clj-toolchain-id" (:clj-toolchain-id opts)})
    (let [source-commit (source-provenance! aozora-root)
          {:keys [source-hashes]}
          (with-store (config/ensure-layout! (config/root (:root opts)))
            (fn [store]
              (assessment-source/capture-checkout
               aozora-root (:source assessment-inputs) (:retained assessment-inputs)
               (cached-source-hash store (:clj-toolchain-id opts)))))]
      (when-not (= source-commit (source-provenance! aozora-root))
        (throw (ex-info "source checkout changed during assessment capture"
                        {:reason :assessment-source-changed})))
      (merge authority
             {:snapshot-bytes snapshot-bytes
              :assessment-source-bytes (:source-bytes assessment-inputs)
              :assessment-inputs assessment-inputs
              :source-commit source-commit :source-hashes source-hashes
              :pinned pinned
              :sign-release sign-release}))))

(defn release!
  "Preflight the full selection, then build the artifacts requested by verified-head assembly."
  [{:keys [root chain-clone branch upstream-origin out] :as opts}]
  (let [{:keys [snapshot-bytes policy-id policy-hash rights pinned sign-release
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
                  stage-set (if (seq requested)
                              (apply dissoc
                                     (publication-stages (assoc opts :rights rights))
                                     (keys research-stages))
                              {})
                  report (execute-build! (dissoc opts :out) inputs stage-set)]
              (when-not (and (= source-commit (get report "aozora_git_commit")
                                (source-provenance! (:aozora-root opts)))
                             (every? (fn [[slug digest]]
                                       (= digest (get-in report ["works" slug "source_content_hash"])))
                                     (select-keys source-hashes slugs)))
                (throw (ex-info "built sources differ from assessment inputs"
                                {:reason :assessment-source-changed-during-build})))
              (committed-assessment-inputs! opts assessment-source-bytes snapshot-bytes)
              (let [current (evaluate-assessment! opts assessment-inputs)]
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
          :policy-id policy-id :policy-hash policy-hash :rights rights
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

(defn- published-catalog-fields
  "What `rows` contribute to a work's published bytes: the work fields, the
  person fields and the contributor relation of each row, as the record
  builder reads them. A column the record does not read can change without
  a published byte changing, and this is how the predicate knows."
  [rows]
  (set (map (juxt csv/parse-work-fields-from-row
                  (comp :fields csv/parse-person-fields-from-row)
                  csv/parse-contributor-from-row)
            rows)))

(defn- catalog-rows-at
  "The catalog's rows at `rev`, read out of git history, or nil when git
  cannot produce them."
  [aozora-root rev]
  (try
    (let [{:keys [exit out]}
          (process/sh {:out :bytes} "git" "-C" (str aozora-root) "show"
                      (str rev ":index_pages/list_person_all_extended_utf8.zip"))]
      (when (zero? exit)
        (some-> (catalog/csv-text-from-zip-bytes out) csv/read-rows-from-string)))
    (catch java.io.IOException _ nil)))

(defn- published-works-with-changed-rows
  "How many works selected in the checkout have different published catalog
  fields at `head-rev` than at `current`, or nil when either catalog cannot
  be read. Selection is taken from the checkout: no work archive moved, so
  the set of works is the same at both revisions, and a row that changed a
  work's standing is a changed row."
  [aozora-root head-rev current]
  (let [before (catalog-rows-at aozora-root head-rev)
        after (catalog-rows-at aozora-root current)]
    (when (and before after)
      (let [selected (set (map #(catalog/row-work-id (:row %))
                               (:candidates (select/select-candidates aozora-root after))))
            by-work (fn [rows]
                      (group-by catalog/row-work-id
                                (filter #(selected (catalog/row-work-id %)) rows)))
            before (by-work before)
            after (by-work after)]
        (count (filter (fn [work-id]
                         (not= (published-catalog-fields (get before work-id))
                               (published-catalog-fields (get after work-id))))
                       selected))))))

(defn release-needed!
  "Whether an upstream revision is worth a release, decided before anything
  is assembled or built.

  A release is minted when a published byte would change, not on every
  upstream commit: 796 of aozorabunko's 5,476 first-parent commits touch no
  work archive, and a release for one of those would carry a 9.1 MB manifest
  and a 14.45 MB catalog to record a 40-hex string. `corpus.covers_from` is
  what lets a reader map those revisions to the release that covers them.

  Two things decide it. A work archive that moved is a changed text. When
  none moved, the catalog rows of the selected works are compared as the
  record builder reads them: a corrected title or a changed date reaches the
  catalog artifact, the TEI header and every citation, so it is a release,
  while a column no record reads is not. Of the catalog-only commits about
  one in ten changes a published work's row, roughly five releases a year.

  Decided here rather than inside the transaction because the transaction has
  no way to say `no release`: `corpus.upstream_rev` is a projection key
  precisely so that a corpus change is a projection change and not a
  determinism halt, and the expensive part being skipped is the corpus build,
  not the chain walk.

  The head is fully verified before its revision is read, so a forged origin
  cannot talk this into skipping. Fail-open on every uncertainty: an
  unreachable predecessor revision, a diff that errors, an empty chain, all
  report `needed`. Refusing to release is the outcome that loses work, so it
  is taken only when the comparison actually succeeded.

  Exits 0 when a release is wanted and 10 when it is not, so a runner reads
  an exit code rather than parsing the report."
  [{:keys [chain-clone branch aozora-root release-pub governance-pub]}]
  (require-flags! "release-needed"
                  {"--chain-clone" chain-clone
                   "--aozora-root" aozora-root
                   "--release-pub" release-pub
                   "--governance-pub" governance-pub})
  (let [current (source-provenance! aozora-root)
        v (view/git-view (str chain-clone))
        commit (repo/fetch! (str chain-clone) branch)
        head (when commit
               (verify/verify-repository-at v commit
                                            (pinned-keys-from-files release-pub governance-pub)))
        head-rev (when (and head (not (:empty head)))
                   (get-in (:head-manifest head) ["corpus" "upstream_rev"]))
        ;; The archive diff first, because it is a path match and needs no
        ;; parse; the catalog comparison only when no archive moved. Watching
        ;; the CSV as a path would skip nothing: every commit that touches a
        ;; work archive touches the CSV in the same commit, all 4,673 of
        ;; them, so the path would fire on 5,469 of 5,476 revisions. Reading
        ;; the rows is what separates a correction to a published work from
        ;; a card-page edit the records never see.
        changed (when (and head-rev (not= head-rev current))
                  (git! aozora-root "diff" "--name-only" (str head-rev ".." current)
                        "--" "cards/*/files/*.zip"))
        rows-changed (when (and changed (string/blank? changed))
                       (published-works-with-changed-rows aozora-root head-rev current))
        report (cond
                 (nil? head-rev) {"needed" true "reason" "no-published-release"}
                 (= head-rev current) {"needed" false "reason" "revision-already-published"}
                 (nil? changed) {"needed" true "reason" "revisions-not-comparable"}
                 (not (string/blank? changed))
                 {"needed" true "reason" "work-archives-changed"
                  "changed_archives" (count (string/split-lines changed))}
                 (nil? rows-changed) {"needed" true "reason" "revisions-not-comparable"}
                 (pos? rows-changed) {"needed" true "reason" "catalog-rows-changed"
                                      "changed_works" rows-changed}
                 :else {"needed" false "reason" "nothing-published-changed"})]
    (println (record-json/write-deterministic-json-str
              (into (sorted-map)
                    (cond-> report
                      head-rev (assoc "head_rev" head-rev)
                      true (assoc "current_rev" current)))))
    report))

(defn- verified-manifest
  "The decoded manifest for `hex`, read from the tree of the verified commit.
  Every release's manifest accumulates in that tree, because each release
  commit is written on its parent's, so one verified commit serves the whole
  chain and no second walk is needed to reach an older release."
  [v commit hex]
  (let [bytes (or (view/read-at v commit (verify/manifest-path hex))
                  (throw (ex-info "manifest is absent from the verified tree"
                                  {:reason :manifest-absent :manifest hex})))]
    (:value (decode/decode "release-manifest" bytes))))

(defn release-delta!
  "What changed between two published releases, as deterministic JSON.

  Derived from the two manifests and nothing else, which is what makes the
  answer checkable rather than announced: anyone holding a clone runs this
  and gets the same bytes, with no corpus checkout and no rebuild. The chain
  is verified against the pinned keys before either manifest is read, so the
  comparison is over bytes the keys establish and not over whatever the
  origin happened to serve.

  `--to` defaults to the head and `--from` to the release before it, so the
  usual question, what the newest release changed, needs neither flag.

  A source change and a toolchain change are reported apart and never merged,
  and the catalog and rights the release declared are reported beside them
  because a document depends on those too; see `soranoha.snh.release-delta`
  for why the distinction is load-bearing and what `unexplained` means."
  [{:keys [chain-clone branch release-pub governance-pub from to]}]
  (require-flags! "release-delta"
                  {"--chain-clone" chain-clone
                   "--release-pub" release-pub
                   "--governance-pub" governance-pub})
  (let [v (view/git-view (str chain-clone))
        commit (or (repo/fetch! (str chain-clone) branch)
                   (throw (ex-info "no publication branch"
                                   {:reason :no-publication-branch :branch branch})))
        verified (verify/verify-repository-at
                  v commit (pinned-keys-from-files release-pub governance-pub))
        _ (when (:empty verified)
            (throw (ex-info "the chain has no release to compare"
                            {:reason :no-published-release})))
        ;; newest first, so the release before `to` is the next element
        chain (:chain verified)
        position (into {} (map-indexed (fn [i hex] [hex i])) chain)
        to-hex (or (not-empty (str to)) (first chain))
        to-index (or (position to-hex)
                     (throw (ex-info "manifest is not in this chain"
                                     {:reason :manifest-not-in-chain :manifest to-hex})))
        from-hex (or (not-empty (str from)) (nth chain (inc to-index) nil))
        _ (when-not from-hex
            (throw (ex-info "genesis has no predecessor to compare against"
                            {:reason :no-predecessor :manifest to-hex})))
        _ (when-not (position from-hex)
            (throw (ex-info "manifest is not in this chain"
                            {:reason :manifest-not-in-chain :manifest from-hex})))
        report (assoc (release-delta/report (verified-manifest v commit from-hex)
                                            (verified-manifest v commit to-hex))
                      "from_manifest" from-hex
                      "to_manifest" to-hex)]
    (println (record-json/write-deterministic-json-str report))
    report))

(defn corpus-delta!
  "How the corpus in a local aozorabunko checkout differs from what a release
  published, as deterministic JSON.

  `release-delta` answers this between two releases from published bytes
  alone. This answers it for a revision no release was ever cut at, which the
  chain cannot reach: the archives at that revision are not in it. So the
  reader supplies them, from their own checkout, rather than asking a server to
  rebuild history on demand. The chain is still verified against the pinned
  keys first, so the only unsigned side is the one the reader supplies.

  No parser runs. `source_content_hash` is produced before parsing, so the
  cost is reading the corpus rather than converting it. The answer is
  therefore about sources: under one toolchain that is the whole question,
  since nothing else can move a document, and across a toolchain change it is
  not. See `soranoha.snh.corpus-delta`.

  `--to` defaults to the head, so comparing a checkout against the newest
  release needs no flag."
  [{:keys [chain-clone branch release-pub governance-pub to aozora-root
           concurrency limit]}]
  (require-flags! "corpus-delta"
                  {"--chain-clone" chain-clone
                   "--release-pub" release-pub
                   "--governance-pub" governance-pub
                   "--aozora-root" aozora-root})
  (let [;; before the scan, so a checkout that cannot name its own revision
        ;; costs nothing rather than a full corpus read
        checkout-rev (source-provenance! aozora-root)
        v (view/git-view (str chain-clone))
        commit (or (repo/fetch! (str chain-clone) branch)
                   (throw (ex-info "no publication branch"
                                   {:reason :no-publication-branch :branch branch})))
        verified (verify/verify-repository-at
                  v commit (pinned-keys-from-files release-pub governance-pub))
        _ (when (:empty verified)
            (throw (ex-info "the chain has no release to compare"
                            {:reason :no-published-release})))
        chain (:chain verified)
        to-hex (or (not-empty (str to)) (first chain))
        _ (when-not (some #{to-hex} chain)
            (throw (ex-info "manifest is not in this chain"
                            {:reason :manifest-not-in-chain :manifest to-hex})))
        manifest (verified-manifest v commit to-hex)
        scanned (corpus-delta/scan aozora-root {:concurrency concurrency :limit limit})
        report (assoc (corpus-delta/report manifest scanned checkout-rev)
                      "to_manifest" to-hex)]
    (println (record-json/write-deterministic-json-str report))
    report))

(defn governance-event-prepare!
  "Turn candidate governance-event content into the exact bytes the offline
  governance key will sign, and print their sha256.

  This is the online half of the ceremony in docs/key-ceremony.md. It needs
  no chain clone and no key material, so it is safe to run anywhere, and its
  output is the one file the ceremony carries offline.

  The candidate need not already be canonical: key order and surrounding
  whitespace are decided by the canonicalization, not by the author, and
  `--out` receives the canonical bytes. Everything the boundary decode can
  reject about a lone event is rejected here instead of after an offline
  signing session: a duplicate key, a non-integral number, a schema
  violation, and entries that are not sorted by slug or repeat one.

  Two conditions are not checked, because both need the chain: whether each
  slug names a work the current release admits, and whether an amendment's
  `amends` names an event the chain carries. The `governance` subcommand
  checks those when it appends the signed event.

  `input_was_canonical` reports whether the candidate file was already the
  bytes to sign. When it is false the candidate's own sha256 is not the one
  to sign, so carry `--out` rather than the file that produced it."
  [{:keys [event out]}]
  (require-flags! "governance-event-prepare" {"--event" event "--out" out})
  (let [candidate (fs/read-all-bytes (str event))
        ;; parse-value first so a duplicate key or a non-integral number is
        ;; reported against the author's own bytes; encode then canonicalizes
        ;; the parsed value and round-trips it through the full boundary decode.
        value (decode/parse-value "governance-event" candidate)
        {:keys [bytes hex id]} (decode/encode "governance-event" value)
        report {"event" id
                "sha256" hex
                "kind" (get value "kind")
                "entries" (count (get value "entries"))
                "input_was_canonical" (= hex (hash/sha256-bytes candidate))}]
    (fs/write-bytes (str out) bytes)
    (println (record-json/write-deterministic-json-str report))
    report))

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
        outcome (transact/publish-governance!
                 {:clone (str chain-clone)
                  :branch branch
                  :pinned-keys pinned
                  :sign-release (release-signer release-key pinned)
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

(defn- release-doi!
  "The release's Zenodo version DOI, checked for shape before it reaches
  every citation of the release. Optional: there is none until the release
  has been deposited, and the first release is exported before it has been.

  Checked rather than trusted because a malformed DOI is not a partial
  failure. It is rendered into the citation record of every work, and a
  reader who follows it lands nowhere; refusing at activation costs one
  corrected flag, and the alternative costs a release."
  [value]
  (when-let [doi (some-> value string/trim not-empty)]
    (when-not (re-matches #"10\.\d{4,9}/[^\s]+" doi)
      (throw (ex-info "Invalid release DOI"
                      {:reason :invalid-release-doi :release-doi doi})))
    doi))

(defn serving-activate!
  "Activate a verified export under the deployment's provisioned serving root."
  [{:keys [chain-clone branch serve-root release-pub governance-pub release-doi
           release-name release-maturity]}]
  (require-flags! "serving-activate"
                  {"--chain-clone" chain-clone "--serve-root" serve-root
                   "--release-pub" release-pub "--governance-pub" governance-pub})
  (let [result (serve/activate!
                {:clone chain-clone :branch branch :serve-root serve-root
                 :release-doi (release-doi! release-doi)
                 :release-name (maturity/release-name! release-name)
                 :maturity (do (maturity/label! release-maturity) release-maturity)
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
                    "evidence-root" "serve-root" "release-pub" "governance-pub"
                    "release-doi" "release-name" "release-maturity"}
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
  constructed throws: a failure to perform the observation, never an
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

(def ^:private cli-spec
  {:root {:coerce :string}
   :aozora-root {:coerce :string}
   :assets-root {:coerce :string}
   :chain-clone {:coerce :string}
   :branch {:coerce :string}
   :upstream-origin {:coerce :string}
   :assessment {:coerce :string}
   :assessment-source {:coerce :string}
   :evidence-root {:coerce :string}
   :slug {:coerce :string}
   :all {:coerce :boolean}
   :as-of {:coerce :string}
   :policy {:coerce :string}
   :release-pub {:coerce :string}
   :governance-pub {:coerce :string}
   :release-key {:coerce :string}
   :event {:coerce :string}
   :event-sig {:coerce :string}
   :out {:coerce :string}
   :serve-root {:coerce :string}
   :release-doi {:coerce :string}
   :release-name {:coerce :string}
   :release-maturity {:coerce :string}
   :deployment {:coerce :string}
   :archive {:coerce :string}
   :commit {:coerce :string}
   :from {:coerce :string}
   :to {:coerce :string}
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
          ;; scheduled-runner exit contract: success covers the no-op; a
        ;; requeue asks the next scheduled run to retry from the new head;
        ;; a determinism halt is an ordinary failure
          "release" (let [{:keys [outcome]} (release! opts)]
                      (when-not (#{:published :already-published} outcome)
                        (System/exit (if (= :requeue outcome) 3 1))))
          ;; scheduled-runner exit contract: 0 asks for a release, 10 says
          ;; the corpus did not move. An exit code rather than parsed output,
          ;; so the runner needs no JSON tool on its path; the report still
          ;; prints, and carries the reason.
          "release-needed" (when-not (get (release-needed! opts) "needed")
                             (System/exit 10))
          "release-delta" (release-delta! opts)
          "corpus-delta" (corpus-delta! opts)
          "governance-event-prepare" (governance-event-prepare! opts)
          "governance" (let [{:keys [outcome]} (governance! opts)]
                         (when-not (#{:published :already-applied} outcome)
                           (System/exit 1)))
          "serving-activate" (serving-activate! opts)
          "publication-init" (publication-init! opts)
          "assessment-evaluate" (assessment-evaluate! opts)
          ;; exit 1 on drift: the committed snapshot would stop a release, so
          ;; this stops the dispatch first
          "assessment-drift" (when (assessment-drift! opts)
                               (System/exit 1))
          "aozora-reliance-prepare" (aozora-reliance-prepare! opts)
          "archive-verify" (when-not (= :success (:result (archive-verify! opts)))
                             (System/exit 1))
          (do (binding [*out* *err*]
                (println "usage: build|release|release-needed|release-delta|corpus-delta|governance-event-prepare|governance|serving-activate|publication-init|assessment-evaluate|assessment-drift|aozora-reliance-prepare|archive-verify [--root R --aozora-root A --assets-root S ...]"))
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
