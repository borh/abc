;; Kernel CLI. `build` runs the full per-work stage graph at an aozorabunko
;; checkout revision into the kura store; output is CAS + trace results plus
;; a disposable run report (a query/export over the trace store — no
;; independent identity, no schema, no retention promise). No manifest,
;; signing, or publishing here: those operate on admission evidence this
;; kernel never sees. `compare` checks per-work TEI/plaintext bytes against
;; a reference tree through the trace store; `verify` runs the kura
;; determinism + fixity report.
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
            [soranoha.yomi.catalog :as catalog]
            [soranoha.yomi.select :as select])
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
                  "selected_count" (count candidates)
                  "rejected_count" (count rejected)
                  "executed_stage_count" (count (filter false?
                                                        (mapcat (comp vals :cached)
                                                                results)))
                  "clj_toolchain_id" clj-toolchain-id
                  "stages" (into (sorted-map)
                                 (map (fn [[_ {:keys [stage-id stage-version
                                                      toolchain-id]}]]
                                        [stage-id
                                         {"stage_version" stage-version
                                          "toolchain_id" toolchain-id}]))
                                 stage-set)
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
        "verify" (do (when-not (:ok? (verify! opts))
                       (System/exit 1)))
        (do (binding [*out* *err*]
              (println "usage: build|compare|verify [--root R --aozora-root A --assets-root S ...]"))
            (System/exit 2)))
      (System/exit 0)
      (catch clojure.lang.ExceptionInfo e
        (binding [*out* *err*]
          (println (str "error: " (.getMessage e) " " (pr-str (ex-data e)))))
        (System/exit 1)))))
