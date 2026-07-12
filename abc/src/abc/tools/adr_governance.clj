(ns abc.tools.adr-governance
  (:refer-clojure :exclude [run!])
  (:require [abc.tools.adr :as adr]
            [abc.tools.json :as json]
            [clojure.tools.cli :as cli]))

(def modes #{:legacy :audit :enforce})

(def cli-options
  [[nil "--mode MODE" "Governance mode: legacy, audit, or enforce."
    :default "legacy"]
   [nil "--report PATH" "Write a deterministic JSON problem report."]])

(defn run!
  ([repo-root]
   (let [problems (adr/validate-repository-legacy repo-root)]
     {:ok? (empty? problems) :problems problems}))
  ([repo-root {:keys [mode] :or {mode :legacy}}]
   (let [problems ((if (= :legacy mode)
                     adr/validate-repository-legacy
                     adr/validate-repository)
                   repo-root)
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
  (let [{:keys [options arguments errors]} (cli/parse-opts args cli-options)
        mode (keyword (:mode options))]
    (if (or (seq errors) (not (contains? modes mode)) (> (count arguments) 1))
      {:ok? false
       :exit-code 2
       :mode mode
       :problems [{:kind :invalid-cli
                   :message (or (first errors)
                                (str "invalid ADR governance arguments: " args))}]}
      (let [result (run! (or (first arguments) ".") {:mode mode})]
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
