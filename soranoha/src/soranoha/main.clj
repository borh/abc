;; Kernel CLI. `build` runs the full per-work stage graph at an aozorabunko
;; checkout revision into the kura store; output is CAS + trace results plus
;; a disposable run report (a query/export over the trace store — no
;; independent identity, no schema, no retention promise). The build itself
;; carries no manifest, signing, or publishing: those operate on admission
;; evidence the kernel never sees. `release` composes the scheduled release
;; pipeline — build, then the za driver's assembly and publication
;; transaction, with the assessment snapshot, policy value, signing seed,
;; and pinned verifier keys as fail-closed file inputs. `governance`
;; appends one offline-signed event; `serving-tree` exports the verified
;; chain's serving tree; `archive-verify` runs the archival observation
;; over a sole archived view. `compare` checks per-work TEI/plaintext
;; bytes against a reference tree through the trace store; `delta` runs
;; the three-set delta oracle over two run reports; `verify` runs the
;; kura determinism + fixity report.
(ns soranoha.main
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [babashka.process :as process]
            [charred.api :as json]
            [clojure.string :as string]
            [soranoha.core.config :as config]
            [soranoha.core.hash :as core-hash]
            [soranoha.kura.engine :as engine]
            [soranoha.kura.cas :as cas]
            [soranoha.kura.trace :as trace]
            [soranoha.kura.verify :as kura-verify]
            [soranoha.ori.stages :as stages]
            [soranoha.ori.validate :as validate]
            [soranoha.ported.assets :as assets]
            [soranoha.ported.json :as abc-json]
            [soranoha.ported.parallel :as parallel]
            [soranoha.snh.decode :as decode]
            [soranoha.snh.semantic :as semantic]
            [soranoha.snh.sign :as sign]
            [soranoha.snh.transact :as transact]
            [soranoha.snh.verify :as verify]
            [soranoha.snh.view :as view]
            [soranoha.yomi.catalog :as catalog]
            [soranoha.yomi.select :as select]
            [soranoha.za.oracle :as oracle]
            [soranoha.za.release :as za-release]
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

(defn- build-stages [{:keys [clj-toolchain-id rows-for catalog-provenance
                             adapter profile]}]
  {:extract (stages/extract-stage clj-toolchain-id)
   :metadata (stages/metadata-stage clj-toolchain-id rows-for catalog-provenance)
   :parse (stages/parse-stage adapter)
   :convert (stages/convert-stage adapter)
   :render (stages/render-stage clj-toolchain-id)
   :validate (stages/validate-tei-stage clj-toolchain-id profile)})

(defn- read-cas-json [store hex]
  (json/read-json (String. ^bytes (cas/get-bytes (:cas-dir store) hex) "UTF-8")))

(defn run-work!
  "Execute (or trace-skip) the full chain for one selected work.
  Returns {:slug :zip-hex :source-facts
  :outputs {stage-key {name hex}} :cached {stage-key bool}
  :trace-keys {stage-key derivation-key-hex}}."
  [store {:keys [extract metadata parse convert render validate]}
   {:keys [slug row file]} catalog-hex]
  (let [zip-hex (cas/put-file! (:cas-dir store) file)
        extract-r (engine/run-stage! store extract {"zip" zip-hex})
        facts (read-cas-json store (get (:outputs extract-r) "source-facts"))
        metadata-r (engine/run-stage! store metadata
                                      {"catalog" catalog-hex
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
                                      {"tei" (get (:outputs render-r) "tei")})]
    {:slug slug
     :zip-hex zip-hex
     :source-facts facts
     :outputs {:extract (:outputs extract-r)
               :metadata (:outputs metadata-r)
               :parse (:outputs parse-r)
               :convert (:outputs convert-r)
               :render (:outputs render-r)
               :validate (:outputs validate-r)}
     :cached {:extract (:cached? extract-r)
              :metadata (:cached? metadata-r)
              :parse (:cached? parse-r)
              :convert (:cached? convert-r)
              :render (:cached? render-r)
              :validate (:cached? validate-r)}
     :trace-keys {:extract (:trace-key extract-r)
                  :metadata (:trace-key metadata-r)
                  :parse (:trace-key parse-r)
                  :convert (:trace-key convert-r)
                  :render (:trace-key render-r)
                  :validate (:trace-key validate-r)}}))

(defn build!
  [{:keys [root aozora-root assets-root concurrency clj-toolchain-id limit]}]
  (when (string/blank? clj-toolchain-id)
    ;; fail closed: the toolchain identity keys every pure-Clojure stage's
    ;; derivations and lands in release provenance; a constant default
    ;; would let dependency or runtime changes retain stale derivations
    (throw (ex-info "clj toolchain identity required; the build wrapper must pass --clj-toolchain-id"
                    {:option "--clj-toolchain-id"})))
  (binding [assets/*root* (str assets-root)]
    (let [root (config/ensure-layout! (config/root root))
          commit (source-provenance! aozora-root)
          {:keys [csv-text catalog-csv-hash]} (catalog/read-catalog-zip aozora-root)
          rows (catalog/read-rows-from-string csv-text)
          {:keys [candidates rejected]} (select/select-candidates aozora-root rows)
          candidates (if (and limit (pos? limit))
                       (vec (take limit candidates))
                       candidates)
          store (engine/open-store! {:cas-dir (config/cas-dir root)
                                     :db-path (config/trace-db-path root)})
          catalog-hex (cas/put-bytes! (:cas-dir store)
                                      (.getBytes ^String csv-text "UTF-8"))
          rows-cache (atom {})
          rows-for (fn [hex]
                     (or (get @rows-cache hex)
                         (let [parsed (if (= hex catalog-hex)
                                        rows
                                        (catalog/read-rows-from-string
                                         (String. ^bytes (cas/get-bytes
                                                          (:cas-dir store) hex)
                                                  "UTF-8")))]
                           (swap! rows-cache assoc hex parsed)
                           parsed)))
          stage-set (build-stages
                     {:clj-toolchain-id clj-toolchain-id
                      :rows-for rows-for
                      :catalog-provenance {"source_url" nil
                                           "retrieved_at" nil
                                           "original_file_hash"
                                           (str "sha256:" catalog-csv-hash)}
                      :adapter (stages/resolve-adapter)
                      :profile (validate/profile-paths assets-root)})
          n (if (pos? concurrency)
              concurrency
              (.availableProcessors (Runtime/getRuntime)))
          started (System/currentTimeMillis)
          results (parallel/ordered-pmap
                   n
                   (fn [candidate] (run-work! store stage-set candidate catalog-hex))
                   candidates)
          relpath-of (into {} (map (juxt :slug :relpath)) candidates)
          ;; the report is a disposable trace-store export, but it must
          ;; carry everything the second-revision delta oracle consumes:
          ;; per-work source identity, per-stage cache decisions, the
          ;; selection join, and the stage coordinates
          report {"aozora_git_commit" commit
                  "catalog_csv_hash" catalog-csv-hash
                  ;; captured from the selection join, before work
                  ;; execution and independent of the works projection:
                  ;; the release driver's totality comparison runs
                  ;; against this set, never against the works keys
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
                                       [slug {"tei" (get-in outputs [:render "tei"])
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
                                              ;; with equal stage-coordinate
                                              ;; tables across two runs, an
                                              ;; executed stage must carry a
                                              ;; changed derivation key — the
                                              ;; delta oracle's explanation
                                              ;; invariant runs on these
                                              "trace_keys"
                                              (into (sorted-map)
                                                    (map (fn [[stage k]]
                                                           [(name stage) k]))
                                                    trace-keys)}]))
                                results)}
          report-path (str (fs/path root "runs" (str "run-" started ".json")))]
      (fs/create-dirs (fs/parent report-path))
      (spit report-path (abc-json/write-deterministic-json-str report))
      (engine/close-store! store)
      (println (str "run_report: " report-path))
      (println (str "selected: " (count candidates)))
      report)))

(defn compare!
  "Acceptance: per-work byte equality of TEI + plaintext against a
  reference tree, counted through the trace store — artifact hashes come
  from the run report (a trace-store export) and bytes from the CAS."
  [{:keys [root reference report]}]
  (let [_ (config/root root)
        run-report (json/read-json (slurp report))
        works (get run-report "works")
        results
        (mapv (fn [[slug artifacts]]
                (let [ref-dir (fs/path reference "publications" slug)
                      check (fn [name file]
                              (let [ref-file (fs/path ref-dir file)
                                    hex (get artifacts name)]
                                (cond
                                  (not (fs/exists? ref-file)) "missing-reference"
                                  (nil? hex) "missing-kernel-artifact"
                                  (= (core-hash/sha256-file (fs/file ref-file)) hex)
                                  "equal"
                                  :else "different")))]
                  {:slug slug
                   :tei (check "tei" "tei.xml")
                   :plaintext (check "plaintext" "plain.txt")}))
              works)
        equal (count (filter #(and (= "equal" (:tei %))
                                   (= "equal" (:plaintext %)))
                             results))
        problems (remove #(and (= "equal" (:tei %)) (= "equal" (:plaintext %)))
                         results)]
    (println (str "equal: " equal "/" (count results)))
    (doseq [p (take 20 problems)]
      (println (str "PROBLEM " (:slug p) " tei=" (:tei p)
                    " plaintext=" (:plaintext p))))
    {:equal equal :total (count results) :problems (vec problems)}))

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
    (println (abc-json/write-deterministic-json-str result))
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

(defn release-preflight!
  "Read and validate every release input before the build runs, returning
  the already-read values the release consumes. Fail-closed: the
  assessment snapshot must boundary-decode, the rights policy must
  authorize (value plus hash from the same bytes; the authority fixes the
  policy id), the pinned role keys must form a valid two-role
  configuration, the 32-byte signing seed must correspond to the pinned
  release key, and the upstream origin must be an absolute URI. --limit
  is refused: it is a build diagnostic, and a limited selection would
  claim the same projection as the full one."
  [{:keys [chain-clone upstream-origin assessment policy
           release-pub governance-pub release-key limit]}]
  (require-flags! "release"
                  {"--chain-clone" chain-clone
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
        authority (za-release/rights-authority!
                   (fs/read-all-bytes (str policy)))
        pinned (pinned-keys-from-files release-pub governance-pub)
        seed (read-signing-seed release-key)]
    (when-not (sign/seed-signs-for? seed (:release pinned))
      (throw (ex-info "release signing seed does not correspond to the pinned release key"
                      {:reason :seed-key-mismatch})))
    (merge authority
           {:snapshot-bytes snapshot-bytes
            :pinned pinned
            :sign-release (fn [manifest-hex]
                            (sign/sign seed
                                       (sign/manifest-message manifest-hex)))})))

(defn release!
  "One scheduled release invocation: preflight every fail-closed file
  input, then the kernel build at the current checkout, then the za
  driver's release assembly and publication transaction."
  [{:keys [root chain-clone branch upstream-origin] :as opts}]
  (let [{:keys [snapshot-bytes policy-id policy-hash pinned sign-release]}
        (release-preflight! opts)
        report (build! opts)
        outcome (za-release/release!
                 {:report report
                  :cas-dir (config/cas-dir (config/root root))
                  :upstream-origin upstream-origin
                  ;; v1's selector has no production parameters
                  :selection-params {}
                  :policy-id policy-id
                  :policy-hash policy-hash
                  :snapshot-bytes snapshot-bytes
                  :clone (str chain-clone)
                  :branch branch
                  :pinned-keys pinned
                  :sign-release sign-release})]
    (println (abc-json/write-deterministic-json-str
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
    (println (abc-json/write-deterministic-json-str
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
    (println (abc-json/write-deterministic-json-str
              {"head" (:head result)
               "releases" (:releases result)
               "blobs" (:blobs result)
               "works" (:works result)}))
    result))

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
    (println (abc-json/write-deterministic-json-str
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
   :reference {:coerce :string}
   :report {:coerce :string}
   :report-a {:coerce :string}
   :report-b {:coerce :string}
   :chain-clone {:coerce :string}
   :branch {:coerce :string :default "main"}
   :upstream-origin {:coerce :string}
   :assessment {:coerce :string}
   :policy {:coerce :string}
   :release-pub {:coerce :string}
   :governance-pub {:coerce :string}
   :release-key {:coerce :string}
   :event {:coerce :string}
   :event-sig {:coerce :string}
   :out {:coerce :string}
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
        opts (cli/parse-opts rest-args {:spec cli-spec})]
    (try
      (case command
        "build" (build! opts)
        "compare" (compare! opts)
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
        "archive-verify" (when-not (= :success (:result (archive-verify! opts)))
                           (System/exit 1))
        "verify" (when-not (:ok? (verify! opts))
                   (System/exit 1))
        (do (binding [*out* *err*]
              (println "usage: build|compare|delta|release|governance|serving-tree|archive-verify|verify [--root R --aozora-root A --assets-root S ...]"))
            (System/exit 2)))
      (System/exit 0)
      (catch clojure.lang.ExceptionInfo e
        (binding [*out* *err*]
          (println (str "error: " (.getMessage e) " " (pr-str (ex-data e)))))
        (System/exit 1)))))
