(ns abc.tools.annotation-join-stats-run
  "Workflow-backed corpus run for annotation join statistics: samples an AAT
  dump, converts each sampled work to parser-IR with the pinned ab-validator
  converter, renders the annotation plaintext view, tokenizes it with the
  pinned `ab-morph-run tokenize-plaintext` tool (all tokenization and token
  parsing stay in the Rust analyzers; this side only consumes per-work
  tokens.jsonl), and computes join statistics via
  abc.tools.annotation-join-stats. Every stage is a step of
  abc.tools.workflow, so the run leaves schema-valid workflow-plan.json and
  workflow-run.json provenance records beside its outputs.

  The stats core stays free of cross-project process coupling; this
  namespace is the orchestration seam where the external binaries live, and
  both binaries arrive as explicit inputs (CLI env), never as discovered
  PATH entries."
  (:require [abc.tools.annotation-join-stats :as stats]
            [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [soranoha.ported.parser-ir-plaintext :as plaintext]
            [abc.tools.workflow :as workflow]
            [babashka.fs :as fs]
            [babashka.process :as process]
            [charred.api :as charred]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(defn work-id-from-aat-filename
  "AAT dump files are named <work-id>-<hash12>.json; return the full file
  stem (work id + content-hash suffix). A dump can contain several files
  for one Aozora work (multi-file works, fragments) with distinct hashes,
  so the bare work id is not a unique pipeline key — a full-corpus repin
  dump has 236 work ids spanning 281 extra files. File-stem identity keeps
  every dump entry distinct; work-level aggregation stays a follow-up."
  [filename]
  (if-let [[_ stem] (re-matches #"((.+)-[0-9a-f]{12})\.json" filename)]
    stem
    (throw (ex-info "AAT filename does not match <work-id>-<hash12>.json"
                    {:filename filename}))))

(defn sample-aat-files
  "Deterministic stride sample over the sorted AAT file list."
  [aat-dir stride]
  (let [all (->> (fs/list-dir aat-dir)
                 (filter #(and (fs/regular-file? %)
                               (string/ends-with? (str (fs/file-name %))
                                                  ".json")))
                 (sort-by #(str (fs/file-name %)))
                 (map fs/file)
                 vec)]
    (when (empty? all)
      (throw (ex-info "AAT directory contains no .json files"
                      {:aat-dir (str aat-dir)})))
    {:total (count all)
     :sampled (vec (take-nth stride all))}))

(defn- run-process!
  "Run cmd to completion, feeding stdin (when given) and draining stdout and
  stderr concurrently so neither pipe can deadlock. A broken pipe while
  writing stdin (the child died early) is tolerated so the child's exit code
  and stderr — the actual diagnosis — survive to the caller. Returns
  {:exit :out :err}."
  [{:keys [cmd env stdin]}]
  (let [proc (process/process cmd {:extra-env env
                                   :out :string
                                   :out-enc "UTF-8"
                                   :err :string
                                   :err-enc "UTF-8"})]
    (try
      (with-open [w (io/writer (:in proc) :encoding "UTF-8")]
        (when stdin
          (.write w ^String stdin)))
      (catch java.io.IOException _))
    (let [{:keys [exit out err]} @proc]
      {:exit exit :out out :err err})))

(defn- tokenize-summary
  "Parse the tokenize-plaintext summary JSON line from the tool's stdout."
  [stdout]
  (charred/read-json (string/trim stdout)))

(defn- annotation-join-stats-steps []
  [{:id :sample-aat
    :requires [:aat-dir :stride :plan-file]
    :produces [:aat-files]
    :run (fn [{:keys [aat-dir stride plan-file]}]
           (let [{:keys [total sampled]} (sample-aat-files aat-dir stride)]
             {:state-updates {:aat-files sampled}
              :inputs [{:role "join-stats-plan"
                        :path (str plan-file)
                        :content_hash (manifest/file-hash plan-file)}
                       {:role "aat-dir" :path (str aat-dir)}]
              :messages [{:level "info"
                          :message (str (count sampled) " of " total
                                        " AAT files sampled (stride "
                                        stride ")")}]}))}
   {:id :convert-parser-ir
    :requires [:aat-files :mapping-file :converter-bin :parser-ir-dir]
    :produces [:work-ids]
    :run (fn [{:keys [aat-files mapping-file converter-bin parser-ir-dir]}]
           (let [work-ids
                 (mapv
                  (fn [aat-file]
                    (let [work-id (work-id-from-aat-filename
                                   (str (fs/file-name aat-file)))
                          work-dir (io/file parser-ir-dir work-id)]
                      (fs/create-dirs work-dir)
                      (let [{:keys [exit err]}
                            (run-process!
                             {:cmd [converter-bin "convert"
                                    "--aat" (str aat-file)
                                    "--mapping" (str mapping-file)
                                    "--parser-ir-out"
                                    (str (io/file work-dir "parser-ir.json"))
                                    "--divergence-out"
                                    (str (io/file work-dir "divergence.json"))]})]
                        (when-not (zero? exit)
                          (throw (ex-info "AAT to parser-IR conversion failed"
                                          {:work-id work-id
                                           :exit exit
                                           :stderr err})))
                        work-id)))
                  aat-files)]
             {:state-updates {:work-ids work-ids}
              :inputs [{:role "converter-bin" :path (str converter-bin)}
                       {:role "mapping"
                        :path (str mapping-file)
                        :content_hash (manifest/file-hash mapping-file)}]
              :outputs [{:role "parser-ir-dir" :path (str parser-ir-dir)}]
              :messages [{:level "info"
                          :message (str (count work-ids)
                                        " works converted to parser-IR")}]}))}
   {:id :render-plaintexts
    :requires [:work-ids :parser-ir-dir :plaintext-dir]
    :produces [:rendered-work-ids]
    :run (fn [{:keys [work-ids parser-ir-dir plaintext-dir]}]
           (fs/create-dirs plaintext-dir)
           (doseq [work-id work-ids]
             (let [parser-ir (files/read-json
                              (io/file parser-ir-dir work-id "parser-ir.json"))
                   {:keys [text]} (plaintext/render-with-annotations parser-ir)]
               (spit (io/file plaintext-dir (str work-id ".txt")) text)))
           {:state-updates {:rendered-work-ids work-ids}
            :outputs [{:role "plaintext-dir" :path (str plaintext-dir)}]
            :messages [{:level "info"
                        :message (str (count work-ids)
                                      " annotation plaintexts rendered")}]})}
   {:id :tokenize
    :requires [:rendered-work-ids :plaintext-dir :tokenizer-bin
               :tokenizer-dict :tokens-dir]
    :produces [:tokenized-work-ids]
    :run (fn [{:keys [rendered-work-ids plaintext-dir tokenizer-bin
                      tokenizer-dict tokens-dir]}]
           (fs/create-dirs tokens-dir)
           (let [{:keys [exit out err]}
                 (run-process!
                  {:cmd (cond-> [tokenizer-bin "tokenize-plaintext"
                                 "--analyzer" tokenizer-dict
                                 "--plaintext-dir" (str plaintext-dir)
                                 "--out-dir" (str tokens-dir)]
                          (System/getenv "AB_MORPH_TOKENIZE_JOBS")
                          (into ["--jobs"
                                 (System/getenv "AB_MORPH_TOKENIZE_JOBS")]))})]
             (when-not (zero? exit)
               (throw (ex-info "Tokenizer failed"
                               {:exit exit :stderr err})))
             (let [summary (tokenize-summary out)
                   errors-file (io/file tokens-dir "tokenize-errors.jsonl")
                   errored (if (fs/regular-file? errors-file)
                             (mapv #(get % "work_id")
                                   (files/read-json-lines errors-file))
                             [])
                   errored-set (set errored)
                   missing (vec (remove #(or (contains? errored-set %)
                                             (fs/regular-file? (fs/file
                                                                tokens-dir
                                                                (str % ".tokens.jsonl"))))
                                        rendered-work-ids))]
               (when (seq missing)
                 (throw (ex-info "Tokenizer output missing for works"
                                 {:missing missing})))
               (when-not (= (count rendered-work-ids)
                            (+ (get summary "works")
                               (get summary "errors" 0)))
                 (throw (ex-info "Tokenizer work accounting mismatch"
                                 {:expected-works (count rendered-work-ids)
                                  :tokenized-works (get summary "works")
                                  :errored-works (get summary "errors" 0)})))
               {:state-updates {:tokenized-work-ids
                                (vec (remove errored-set rendered-work-ids))}
                :inputs [{:role "tokenizer-bin" :path (str tokenizer-bin)}]
                :outputs [{:role "tokens-dir" :path (str tokens-dir)}]
                :messages (cond-> [{:level "info"
                                    :message (str (get summary "works")
                                                  " works tokenized ("
                                                  tokenizer-dict ", "
                                                  (get summary "tokens")
                                                  " tokens)")}]
                            (seq errored)
                            (conj {:level "warn"
                                   :message (str (count errored)
                                                 " works failed tokenization"
                                                 " and join-stats will skip"
                                                 " them (tokenize-errors"
                                                 ".jsonl)")}))})))}
   {:id :join-stats
    :requires [:tokenized-work-ids :parser-ir-dir :tokens-dir :stats-dir]
    :produces [:aggregate]
    :run (fn [{:keys [parser-ir-dir tokens-dir stats-dir]}]
           (stats/run-join-stats! (str parser-ir-dir)
                                  (str tokens-dir)
                                  (str stats-dir))
           (let [aggregate-file (io/file stats-dir "aggregate.json")
                 aggregate (files/read-json aggregate-file)]
             {:state-updates {:aggregate aggregate}
              :outputs [{:role "aggregate"
                         :path (str aggregate-file)
                         :content_hash (manifest/file-hash aggregate-file)}
                        {:role "per-work"
                         :path (str (io/file stats-dir "per-work.jsonl"))}
                        {:role "work-level"
                         :path (str (io/file stats-dir "work-level.jsonl"))}
                        {:role "report"
                         :path (str (io/file stats-dir "report.md"))}]
              :messages [{:level "info"
                          :message (str (get aggregate "work_count")
                                        " file entries ("
                                        (get aggregate "distinct_work_count")
                                        " works) in aggregate, "
                                        (count (get aggregate
                                                    "skipped_work_ids"))
                                        " skipped")}]}))}])

(def ^:private required-plan-keys
  ["label" "aat_dir" "mapping" "stride" "tokenizer_dict"])

(defn- read-plan! [plan-file]
  (let [plan (files/read-json plan-file)
        missing (vec (remove #(contains? plan %) required-plan-keys))]
    (when (seq missing)
      (throw (ex-info "Join-stats plan is missing required keys"
                      {:plan (str plan-file) :missing missing})))
    (when-not (and (integer? (get plan "stride")) (pos? (get plan "stride")))
      (throw (ex-info "Join-stats plan stride must be a positive integer"
                      {:plan (str plan-file) :stride (get plan "stride")})))
    plan))

(defn run-annotation-join-stats-run!
  "Execute the full join-stats corpus run described by plan-file into
  out-root. External binary paths must be explicit; the CLI wrapper reads
  them from AB_AAT_TO_PARSER_IR_BIN and AB_MORPH_RUN_BIN. The plan's
  tokenizer_dict is passed verbatim as the tool's --analyzer spec (e.g.
  \"vibrato:unidic-novel-202512\")."
  [{:keys [plan-file out-root converter-bin tokenizer-bin]}]
  (doseq [[label value] {"converter-bin" converter-bin
                         "tokenizer-bin" tokenizer-bin}]
    (when (string/blank? (str value))
      (throw (ex-info (str "Missing required binary: " label)
                      {:binary label}))))
  (let [plan (read-plan! plan-file)
        out-root-file (io/file out-root)
        parser-ir-dir (io/file out-root-file "parser-ir")
        plaintext-dir (io/file out-root-file "plaintext")
        tokens-dir (io/file out-root-file "tokens")
        stats-dir (io/file out-root-file "stats")]
    (files/delete-tree! out-root-file)
    (fs/create-dirs out-root-file)
    (files/copy-file! (io/file plan-file)
                      (io/file out-root-file "join-stats-plan.json"))
    (let [{:keys [state run]}
          (workflow/run-workflow!
           {:workflow-id "soranoha.annotation-join-stats.v1"
            :run-id (str "annotation-join-stats:" (get plan "label"))
            :output-root out-root-file
            :initial-state {:plan-file (io/file plan-file)
                            :aat-dir (io/file (get plan "aat_dir"))
                            :stride (get plan "stride")
                            :mapping-file (io/file (get plan "mapping"))
                            :tokenizer-dict (get plan "tokenizer_dict")
                            :converter-bin converter-bin
                            :tokenizer-bin tokenizer-bin
                            :parser-ir-dir parser-ir-dir
                            :plaintext-dir plaintext-dir
                            :tokens-dir tokens-dir
                            :stats-dir stats-dir}
            :steps (annotation-join-stats-steps)})]
      (println "workflow_run:" (str (io/file out-root-file
                                             "workflow-run.json")))
      (println "workflow_status:" (get run "status"))
      (println "aggregate:" (str (io/file stats-dir "aggregate.json")))
      (println "work_count:" (get-in state [:aggregate "work_count"]))
      (println "distinct_work_count:" (get-in state
                                              [:aggregate
                                               "distinct_work_count"]))
      (println "skipped_count:" (count (get-in state
                                               [:aggregate
                                                "skipped_work_ids"])))
      0)))
