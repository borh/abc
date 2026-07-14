(ns abc.tools.adr-evidence-bootstrap
  (:require [abc.tools.adr :as adr]
            [abc.tools.adr-claim-migration :as migration]
            [abc.tools.adr-evidence :as evidence]
            [abc.tools.adr-evidence-bundle :as bundle]
            [abc.tools.adr-governance :as governance]
            [abc.tools.cli :as cli]
            [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.json :as json]
            [abc.tools.path-containment :as containment]
            [abc.tools.schema :as schema]
            [babashka.fs :as fs]
            [clojure.set :as set]
            [clojure.java.shell :as shell]
            [clojure.string :as string])
  (:import [java.time LocalDate]
           [java.util Base64]))

(def schema-path "schemas/adr-evidence-bootstrap.schema.json")

(def required-governance-paths
  #{"abc/docs/adr/adr-evidence.edn"
    "abc/docs/adr/claim-evidence-compatibility.edn"
    "abc/docs/adr/governance-as-of.edn"
    "abc/docs/adr/adr-claim-migration-baseline.json"
    "abc/docs/adr/adr-claim-migration.edn"
    "abc/src/abc/tools/adr.clj"
    "abc/src/abc/tools/adr_governance.clj"
    "abc/src/abc/tools/adr_evidence.clj"
    "abc/src/abc/tools/adr_evidence_bundle.clj"
    "abc/src/abc/tools/adr_claim_migration.clj"
    "abc/schemas/adr-evidence-run.schema.json"
    "abc/schemas/adr-external-evidence.schema.json"})

(def ^:private additional-governance-paths
  #{"abc/docs/adr/adr-relations.edn"
    "abc/docs/adr/adr-graph.mmd"
    "abc/docs/architecture-stages.edn"
    "abc/docs/architecture.md"
    "abc/docs/architecture.mmd"
    "abc/schemas/adr-evidence-bootstrap.schema.json"
    "abc/schemas/schema-contracts.json"})

(defn- problem [kind message & {:as data}]
  (merge {:kind kind :message message} data))

(defn- schema-problems [value]
  (when-let [errors (schema/validation-errors (files/read-json schema-path) value)]
    [(problem :invalid-bootstrap-schema
              "bootstrap snapshot does not satisfy its closed schema"
              :errors errors)]))

(defn- real-date? [value]
  (and (string? value)
       (try
         (LocalDate/parse value)
         true
         (catch Exception _ false))))

(defn- safe-relative-path? [path]
  (and (string? path)
       (not (string/blank? path))
       (not (string/starts-with? path "/"))
       (not (string/includes? path "\\"))
       (not-any? #{"" "." ".."} (string/split path #"/" -1))))

(defn- decoded-bytes [encoded]
  (when (string? encoded)
    (try
      (.decode (Base64/getDecoder) encoded)
      (catch IllegalArgumentException _ nil))))

(defn- manifest-problems [files-value]
  (if-not (map? files-value)
    []
    (vec
     (concat
      (for [path (sort (set/difference required-governance-paths
                                       (set (keys files-value))))]
        (problem :missing-bootstrap-file
                 "bootstrap manifest omits a required governance file"
                 :path path))
      (mapcat
       (fn [[path entry]]
         (let [bytes (decoded-bytes (get entry "content_base64"))]
           (concat
            (when-not (safe-relative-path? path)
              [(problem :unsafe-bootstrap-path
                        "bootstrap manifest path is not safe and repository-relative"
                        :path path)])
            (when (and (map? entry) (string? (get entry "content_base64"))
                       (nil? bytes))
              [(problem :invalid-bootstrap-base64
                        "bootstrap file payload is not canonical basic Base64"
                        :path path)])
            (when (and bytes (string? (get entry "sha256"))
                       (not= (get entry "sha256")
                             (hash/format-sha256 (hash/sha256-bytes bytes))))
              [(problem :bootstrap-file-hash-mismatch
                        "bootstrap payload hash does not match decoded bytes"
                        :path path)]))))
       (sort-by key files-value))))))

(defn validate-snapshot-value [value]
  (let [schema-errors (schema-problems value)
        numbers (get value "accepted_adr_numbers")]
    (vec
     (concat
      schema-errors
      (when-not (real-date? (get value "governance_as_of"))
        [(problem :invalid-bootstrap-date
                  "bootstrap governance_as_of is not a real calendar date")])
      (when (and (vector? numbers)
                 (integer? (get value "accepted_adr_count"))
                 (not= (count numbers) (get value "accepted_adr_count")))
        [(problem :bootstrap-accepted-count-mismatch
                  "accepted_adr_count does not match accepted_adr_numbers")])
      (manifest-problems (get value "files"))))))

(defn validate-snapshot-file [path]
  (try
    (validate-snapshot-value (files/read-json path))
    (catch Exception exception
      [(problem :invalid-bootstrap-file
                "bootstrap snapshot is not readable JSON"
                :detail (.getMessage exception))])))

(defn- workspace-path [workspace-root path]
  (string/replace (str (fs/relativize workspace-root path)) "\\" "/"))

(defn- accepted-adr-paths [abc-root workspace-root adrs]
  (for [{:keys [file status]} adrs
        :when (= "Accepted" status)]
    (workspace-path workspace-root (fs/file abc-root "docs/adr" file))))

(defn- governance-source-paths [abc-root workspace-root]
  (let [tools-root (fs/path abc-root "src/abc/tools")]
    (concat
     (for [path (fs/glob tools-root "adr*.clj")]
       (workspace-path workspace-root path))
     (map #(str "abc/src/abc/tools/" %)
          ["path_containment.clj" "hash.clj" "json.clj" "files.clj"]))))

(defn- artifact-input-paths [abc-root workspace-root registry]
  (mapcat
   (fn [artifact-path]
     (let [{:keys [value problems]} (bundle/load-bundle abc-root artifact-path)]
       (when (seq problems)
         (throw (ex-info "bootstrap registry artifact is unavailable"
                         {:artifact-path artifact-path :problems problems})))
       (let [component? (= "component-clojure-test-v1"
                           (get-in value ["input_profile" "kind"]))]
         (cons
          (workspace-path workspace-root (fs/file abc-root artifact-path))
          (map (fn [path]
                 (if component?
                   path
                   (workspace-path workspace-root (fs/file abc-root path))))
               (keys (get value "inputs")))))))
   (sort (distinct (map :artifact-path (:entries registry))))))

(defn snapshot-input-paths
  [abc-root workspace-root adrs registry]
  (let [workspace-root (fs/canonicalize workspace-root)
        abc-root (fs/canonicalize abc-root)]
    (when-not (= abc-root (fs/canonicalize (fs/path workspace-root "abc")))
      (throw (ex-info "ABC root must be the workspace abc directory"
                      {:abc-root (str abc-root)
                       :workspace-root (str workspace-root)})))
    (->> (concat required-governance-paths
                 additional-governance-paths
                 (accepted-adr-paths abc-root workspace-root adrs)
                 (governance-source-paths abc-root workspace-root)
                 (artifact-input-paths abc-root workspace-root registry))
         distinct
         sort
         vec)))

(defn git-command [workspace-root & args]
  (apply shell/sh "git" "-C" (str workspace-root) args))

(defn- successful-git-output [workspace-root & args]
  (let [{:keys [exit out err]} (apply git-command workspace-root args)]
    (when-not (zero? exit)
      (throw (ex-info "Git command failed while creating bootstrap snapshot"
                      {:args args :exit exit :error err})))
    (string/trim out)))

(defn- assert-clean-git! [workspace-root phase]
  (let [status (successful-git-output workspace-root
                                      "status" "--porcelain"
                                      "--untracked-files=all")]
    (when-not (string/blank? status)
      (throw (ex-info "bootstrap snapshot requires a clean Git worktree"
                      {:kind :dirty-bootstrap-worktree
                       :phase phase :status status})))))

(defn- assert-root-layout! [abc-root workspace-root]
  (let [workspace-root (fs/canonicalize workspace-root)
        abc-root (fs/canonicalize abc-root)
        git-root (fs/canonicalize
                  (successful-git-output workspace-root
                                         "rev-parse" "--show-toplevel"))]
    (when-not (= workspace-root git-root)
      (throw (ex-info "workspace root is not the Git root"
                      {:workspace-root (str workspace-root)
                       :git-root (str git-root)})))
    (when-not (= abc-root (fs/canonicalize (fs/path workspace-root "abc")))
      (throw (ex-info "ABC root is not the workspace abc directory"
                      {:abc-root (str abc-root)
                       :workspace-root (str workspace-root)})))
    {:abc-root abc-root :workspace-root workspace-root}))

(defn- strict-report [abc-root workspace-root]
  (let [{:keys [ok? mode problems] :as result}
        (governance/run! abc-root {:mode :enforce
                                   :workspace-root workspace-root})]
    (when-not (and ok? (= :enforce mode) (empty? problems))
      (throw (ex-info "strict ADR governance is not clean"
                      {:kind :bootstrap-governance-failed
                       :result result})))
    {"mode" "enforce" "ok" true "problems" []}))

(defn- snapshot-file-entry [workspace-root path]
  (let [state (containment/path-state workspace-root path)]
    (when-not (and (= :ok (:state state)) (fs/regular-file? (:path state)))
      (throw (ex-info "bootstrap input is missing or escapes the workspace"
                      {:kind :missing-bootstrap-input
                       :path path :state (:state state)})))
    (let [bytes (files/read-bytes (:path state))]
      {"sha256" (hash/format-sha256 (hash/sha256-bytes bytes))
       "content_base64" (.encodeToString (Base64/getEncoder) bytes)})))

(defn snapshot-value [abc-root workspace-root]
  (let [{:keys [abc-root workspace-root]}
        (assert-root-layout! abc-root workspace-root)]
    (assert-clean-git! workspace-root :before-snapshot)
    (let [adrs (adr/parse-all (fs/file abc-root "docs/adr"))
          adr-0034 (first (filter #(= 34 (:num %)) adrs))
          accepted (filterv #(= "Accepted" (:status %)) adrs)
          migration-state (migration/load-migration-state abc-root {})
          registry (files/read-edn (fs/file abc-root evidence/registry-path))
          report-before (strict-report abc-root workspace-root)]
      (when-not (= "Proposed" (:status adr-0034))
        (throw (ex-info "ADR 0034 must remain Proposed while snapshotting"
                        {:kind :adr-0034-not-proposed
                         :status (:status adr-0034)})))
      (when (seq (:problems migration-state))
        (throw (ex-info "ADR claim migration ledger is incomplete"
                        {:kind :incomplete-migration-ledger
                         :problems (:problems migration-state)})))
      (let [paths (snapshot-input-paths abc-root workspace-root adrs registry)
            embedded-files (into (sorted-map)
                                 (map (fn [path]
                                        [path (snapshot-file-entry workspace-root path)]))
                                 paths)
            report-after (strict-report abc-root workspace-root)
            value {"schema_version" "abc-adr-evidence-bootstrap-v1"
                   "subject" "pre-promotion Accepted ADR corpus excluding ADR 0034"
                   "producer_revision" (successful-git-output
                                        workspace-root "rev-parse" "HEAD")
                   "governance_as_of" (str (evidence/load-as-of))
                   "accepted_adr_numbers" (mapv :num (sort-by :num accepted))
                   "accepted_adr_count" (count accepted)
                   "accepted_criterion_count"
                   (count (filter :claim-id (mapcat :criteria accepted)))
                   "audit_report" report-before
                   "files" embedded-files}]
        (when-not (= report-before report-after)
          (throw (ex-info "strict governance changed while snapshotting"
                          {:kind :bootstrap-governance-race
                           :before report-before :after report-after})))
        (assert-clean-git! workspace-root :after-snapshot)
        (when-let [problems (seq (validate-snapshot-value value))]
          (throw (ex-info "generated bootstrap snapshot is invalid"
                          {:kind :invalid-generated-bootstrap
                           :problems problems})))
        value))))

(defn- accepted-adrs [adrs]
  (filterv #(= "Accepted" (:status %)) adrs))

(defn- accepted-criterion-count [adrs]
  (count (filter :claim-id (mapcat :criteria (accepted-adrs adrs)))))

(defn- governance-enforced? [workspace-root]
  (let [flake (files/read-text (fs/file workspace-root "flake.nix"))
        start (string/index-of flake "monorepo-adr-governance")
        tail (when start (subs flake start))
        end (when tail (string/index-of tail "''" 2))
        block (if end (subs tail 0 end) tail)]
    (and block
         (string/includes? block "--mode enforce")
         (not (string/includes? block "--mode audit")))))

(defn final-transition-problems [abc-root workspace-root snapshot]
  (try
    (let [{:keys [abc-root workspace-root]}
          (assert-root-layout! abc-root workspace-root)
          snapshot-problems (validate-snapshot-value snapshot)
          adrs (adr/parse-all (fs/file abc-root "docs/adr"))
          accepted (accepted-adrs adrs)
          adr-0034 (first (filter #(= 34 (:num %)) adrs))
          migration-state (migration/load-migration-state abc-root {})
          strict-result (governance/run! abc-root {:mode :enforce
                                                   :workspace-root workspace-root})
          pre-count (get snapshot "accepted_adr_count")
          pre-criteria (get snapshot "accepted_criterion_count")]
      (vec
       (concat
        snapshot-problems
        (when-not (= "Accepted" (:status adr-0034))
          [(problem :adr-0034-not-accepted
                    "ADR 0034 must be Accepted in the final tree")])
        (when-not (= "full-corpus" (:validation-scope adr-0034))
          [(problem :adr-0034-validation-scope-mismatch
                    "ADR 0034 must declare full-corpus validation scope")])
        (when-not (= "none" (:release-authority adr-0034))
          [(problem :adr-0034-release-authority-mismatch
                    "ADR 0034 must declare no release authority")])
        (when (and (integer? pre-count)
                   (not= (inc pre-count) (count accepted)))
          [(problem :accepted-adr-count-mismatch
                    "final tree must add exactly ADR 0034 to the Accepted set")])
        (when (and (integer? pre-criteria)
                   (not= (+ 3 pre-criteria) (accepted-criterion-count adrs)))
          [(problem :accepted-criterion-count-mismatch
                    "final tree must add exactly three binding criteria")])
        (when (seq (:problems migration-state))
          [(problem :incomplete-migration-ledger
                    "final tree has migration ledger problems"
                    :problems (:problems migration-state))])
        (when-not (and (:ok? strict-result)
                       (= :enforce (:mode strict-result))
                       (empty? (:problems strict-result)))
          [(problem :strict-governance-failed
                    "final tree does not pass strict governance"
                    :problems (:problems strict-result))])
        (when-not (governance-enforced? workspace-root)
          [(problem :governance-gate-not-enforced
                    "root Nix governance check must statically select enforce mode")]))))
    (catch Exception exception
      [(problem :final-transition-validation-failed
                "final transition validation could not complete"
                :detail (.getMessage exception))])))

(def cli-options
  [[nil "--write PATH"]
   [nil "--verify PATH"]
   [nil "--verify-final PATH"]
   [nil "--repo-root PATH"]
   [nil "--workspace-root PATH"]])

(defn usage [_]
  "Usage: clojure -M:abc/adr-evidence-bootstrap (--write|--verify|--verify-final) PATH --repo-root PATH --workspace-root PATH")

(defn -main [& args]
  (cli/run-cli!
   args
   {:cli-options cli-options
    :required [:repo-root :workspace-root]
    :max-args 0
    :usage-fn usage
    :run (fn [{:keys [options]}]
           (let [modes (filter #(some? (get options %))
                               [:write :verify :verify-final])]
             (when-not (= 1 (count modes))
               (throw (ex-info "exactly one bootstrap mode is required" {})))
             (case (first modes)
               :write (let [value (snapshot-value (:repo-root options)
                                                  (:workspace-root options))]
                        (json/write-deterministic-json-file! (:write options) value)
                        {:ok? true})
               :verify (let [problems (validate-snapshot-file (:verify options))]
                         (when (seq problems)
                           (throw (ex-info "bootstrap snapshot validation failed"
                                           {:problems problems})))
                         {:ok? true})
               :verify-final
               (let [snapshot (files/read-json (:verify-final options))
                     problems (final-transition-problems (:repo-root options)
                                                         (:workspace-root options)
                                                         snapshot)]
                 (when (seq problems)
                   (throw (ex-info "final transition validation failed"
                                   {:problems problems})))
                 {:ok? true}))))
    :fail? (complement :ok?)}))
