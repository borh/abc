(ns abc.tools.adr-governance
  (:refer-clojure :exclude [run!])
  (:require [abc.tools.adr :as adr]
            [abc.tools.adr-evidence :as evidence]
            [abc.tools.json :as json]
            [clojure.java.io :as io]
            [clojure.tools.cli :as cli]))

(def modes #{:legacy :audit :enforce})

(def cli-options
  [[nil "--mode MODE" "Governance mode: legacy, audit, or enforce."
    :default "legacy"]
   [nil "--report PATH" "Write a deterministic JSON problem report."]
   [nil "--repo-root PATH" "ABC artifact and ADR root."]
   [nil "--workspace-root PATH" "Monorepo input root."]])

(defn- accepted-claims [adrs]
  (vec
   (for [{:keys [file status criteria]} adrs
         :when (= "Accepted" status)
         {:keys [claim-id] :as criterion} criteria
         :when claim-id]
     (assoc criterion :file file :status status))))

(defn- strict-problems [repo-root workspace-root]
  (let [adrs (adr/parse-all (io/file repo-root "docs/adr"))
        adr-problems (adr/validate-adrs adrs repo-root)
        evidence-problems
        (evidence/validate-registry
         {:repo-root repo-root
          :workspace-root workspace-root
          :claims (accepted-claims adrs)
          :registry (evidence/load-registry)
          :matrix (evidence/load-matrix)
          :as-of (evidence/load-as-of)})]
    (vec (concat adr-problems evidence-problems))))

(defn run!
  ([repo-root]
   (let [problems (adr/validate-repository-legacy repo-root)]
     {:ok? (empty? problems) :problems problems}))
  ([repo-root {:keys [mode workspace-root] :or {mode :legacy}}]
   (let [problems (if (= :legacy mode)
                    (adr/validate-repository-legacy repo-root)
                    (strict-problems repo-root (or workspace-root repo-root)))
         ok? (empty? problems)]
     {:ok? ok?
      :exit-code (if (or ok? (= :audit mode)) 0 1)
      :mode mode
      :problems problems})))

(defn- report-value [{:keys [ok? mode problems]}]
  {"mode" (name mode)
   "ok" ok?
   "problems" problems})

(defn run-cli!
  "Parse CLI arguments, run governance, optionally write a report, and return
  the result value without exiting the process."
  [args]
  (let [args (if (= "--" (first args)) (rest args) args)
        {:keys [options arguments errors]} (cli/parse-opts args cli-options)
        mode (keyword (:mode options))]
    (if (or (seq errors) (not (contains? modes mode)) (> (count arguments) 1))
      {:ok? false
       :exit-code 2
       :mode mode
       :problems [{:kind :invalid-cli
                   :message (or (first errors)
                                (str "invalid ADR governance arguments: " args))}]}
      (let [repo-root (or (:repo-root options) (first arguments) ".")
            result (run! repo-root {:mode mode
                                    :workspace-root (or (:workspace-root options) repo-root)})]
        (when-let [path (:report options)]
          (json/write-deterministic-json-file! path (report-value result)))
        result))))

(defn -main [& args]
  (let [{:keys [ok? problems exit-code]} (run-cli! args)]
    (binding [*out* *err*]
      (doseq [{:keys [file kind message]} problems]
        (println "ADR-LINT" file (str (name kind) ":") message)))
    (when ok? (println "ADR governance valid"))
    (System/exit exit-code)))
