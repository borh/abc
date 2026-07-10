(ns abc.tools.annotation-join-stats-run
  "Workflow-backed corpus run for annotation join statistics: samples an AAT
  dump, converts each sampled work to parser-IR with the pinned ab-validator
  converter, renders the annotation plaintext view, tokenizes it with the
  pinned tokenizer, and computes join statistics via
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
            [abc.tools.parser-ir-plaintext :as plaintext]
            [abc.tools.workflow :as workflow]
            [charred.api :as charred]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(defn work-id-from-aat-filename
  "AAT dump files are named <work-id>-<hash12>.json; return the work id."
  [filename]
  (if-let [[_ work-id] (re-matches #"(.+)-[0-9a-f]{12}\.json" filename)]
    work-id
    (throw (ex-info "AAT filename does not match <work-id>-<hash12>.json"
                    {:filename filename}))))

(defn sample-aat-files
  "Deterministic stride sample over the sorted AAT file list."
  [aat-dir stride]
  (let [all (->> (.listFiles (io/file aat-dir))
                 (filter #(and (.isFile ^java.io.File %)
                               (string/ends-with? (.getName ^java.io.File %)
                                                  ".json")))
                 (sort-by #(.getName ^java.io.File %))
                 vec)]
    (when (empty? all)
      (throw (ex-info "AAT directory contains no .json files"
                      {:aat-dir (str aat-dir)})))
    {:total (count all)
     :sampled (vec (take-nth stride all))}))

(defn- run-process!
  "Run cmd to completion, feeding stdin (when given) and draining stdout and
  stderr concurrently so neither pipe can deadlock. Returns
  {:exit :out :err}."
  [{:keys [cmd env stdin]}]
  (let [pb (ProcessBuilder. ^java.util.List (mapv str cmd))]
    (doseq [[k v] env]
      (.put (.environment pb) (str k) (str v)))
    (let [proc (.start pb)
          out-fut (future (slurp (io/reader (.getInputStream proc)
                                            :encoding "UTF-8")))
          err-fut (future (slurp (io/reader (.getErrorStream proc)
                                            :encoding "UTF-8")))]
      (with-open [w (io/writer (.getOutputStream proc) :encoding "UTF-8")]
        (when stdin
          (.write w ^String stdin)))
      (let [exit (.waitFor proc)]
        {:exit exit :out @out-fut :err @err-fut}))))

(defn split-eos-groups
  "Split MeCab-format tokenizer output into per-input-line surface groups.
  The tokenizer emits one EOS line per input line (including blank input
  lines); each token line is <surface>TAB<features>."
  [tokenizer-output]
  (loop [lines (string/split tokenizer-output #"\n" -1)
         current []
         groups []]
    (if-let [line (first lines)]
      (cond
        (= line "EOS") (recur (next lines) [] (conj groups current))
        (string/blank? line) (recur (next lines) current groups)
        :else (recur (next lines)
                     (conj current (first (string/split line #"\t" 2)))
                     groups))
      groups)))

(defn- surface-json-line [surface]
  (str (charred/write-json-str {"surface" surface}) "\n"))

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
                                   (.getName ^java.io.File aat-file))
                          work-dir (io/file parser-ir-dir work-id)]
                      (.mkdirs work-dir)
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
           (.mkdirs (io/file plaintext-dir))
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
           (.mkdirs (io/file tokens-dir))
           (let [works (mapv (fn [work-id]
                               (let [text (slurp (io/file plaintext-dir
                                                          (str work-id ".txt")))]
                                 {:work-id work-id
                                  :lines (string/split text #"\n" -1)}))
                             rendered-work-ids)
                 full-input (str (string/join "\n" (mapcat :lines works)) "\n")
                 env (cond-> {"AB_VIBRATO_DICT" tokenizer-dict}
                       (System/getenv "AB_VIBRATO_CACHE_DIR")
                       (assoc "AB_VIBRATO_CACHE_DIR"
                              (System/getenv "AB_VIBRATO_CACHE_DIR")))
                 {:keys [exit out err]} (run-process! {:cmd [tokenizer-bin]
                                                       :env env
                                                       :stdin full-input})
                 _ (when-not (zero? exit)
                     (throw (ex-info "Tokenizer failed"
                                     {:exit exit :stderr err})))
                 groups (split-eos-groups out)
                 expected (reduce + (map #(count (:lines %)) works))]
             (when-not (= expected (count groups))
               (throw (ex-info "Tokenizer line accounting mismatch"
                               {:expected-lines expected
                                :eos-groups (count groups)})))
             (loop [remaining works
                    groups groups]
               (when-let [{:keys [work-id lines]} (first remaining)]
                 (let [[work-groups rest-groups] (split-at (count lines)
                                                           groups)]
                   (spit (io/file tokens-dir (str work-id ".tokens.jsonl"))
                         (apply str (map surface-json-line
                                         (apply concat work-groups))))
                   (recur (next remaining) rest-groups))))
             {:state-updates {:tokenized-work-ids (mapv :work-id works)}
              :inputs [{:role "tokenizer-bin" :path (str tokenizer-bin)}]
              :outputs [{:role "tokens-dir" :path (str tokens-dir)}]
              :messages [{:level "info"
                          :message (str (count works) " works tokenized ("
                                        tokenizer-dict ")")}]}))}
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
                        {:role "report"
                         :path (str (io/file stats-dir "report.md"))}]
              :messages [{:level "info"
                          :message (str (get aggregate "work_count")
                                        " works in aggregate, "
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
  them from AB_AAT_TO_PARSER_IR_BIN and AB_VIBRATO_TOKENIZE_BIN."
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
    (.mkdirs out-root-file)
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
      (println "skipped_count:" (count (get-in state
                                               [:aggregate
                                                "skipped_work_ids"])))
      0)))
