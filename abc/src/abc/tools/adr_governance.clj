(ns abc.tools.adr-governance
  (:refer-clojure :exclude [run!])
  (:require [abc.tools.cli :as abc-cli]
            [abc.tools.decisions :as decisions]
            [abc.tools.decisions-index :as index]
            [babashka.fs :as fs]))

(def cli-options
  [[nil "--repo-root PATH" "ABC artifact and ADR root."]
   [nil "--write-index" "Regenerate docs/adr/INDEX.md from decisions.edn."]])

(defn run!
  "Strictly validate the decisions corpus under repo-root, including INDEX
  byte currency. INDEX currency lives here (not in
  decisions/validate-repository) so decisions-index may depend on decisions
  without a require cycle."
  [repo-root]
  (let [core (decisions/validate-repository repo-root)
        currency (if (seq core)
                   [] ; unreadable/invalid corpus is already reported
                   (let [{:keys [corpus]}
                         (decisions/load-corpus
                          (fs/path repo-root decisions/corpus-file))]
                     (index/currency-problems corpus repo-root)))
        problems (vec (concat core currency))
        ok? (empty? problems)]
    {:ok? ok?
     :exit-code (if ok? 0 1)
     :problems problems}))

(defn run-cli!
  "Parse CLI arguments, run strict governance, and return the result value
  without exiting the process."
  [args]
  (let [{:keys [options arguments errors]}
        (abc-cli/parse args {:cli-options cli-options})]
    (if (or (seq errors) (> (count arguments) 1))
      {:ok? false
       :exit-code 2
       :problems [{:kind :invalid-cli
                   :message (or (first errors)
                                (str "invalid ADR governance arguments: " args))}]}
      (let [root (or (:repo-root options) (first arguments) ".")]
        (when (:write-index options) (index/write! root))
        (run! root)))))

(defn usage [_]
  "Usage: clojure -M:abc/adr-governance [--repo-root PATH] [--write-index] [REPO_ROOT]")

(defn- emit-result! [{:keys [ok? problems] :as result}]
  (binding [*out* *err*]
    (doseq [{:keys [file kind message]} problems]
      (println "ADR-LINT" file (str (name kind) ":") message)))
  (when ok? (println "ADR governance valid"))
  result)

(defn -main [& args]
  (abc-cli/run-cli!
   args
   {:cli-options cli-options
    :max-args 1
    :usage-fn usage
    :run (fn [{:keys [options arguments]}]
           (let [root (or (:repo-root options) (first arguments) ".")]
             (when (:write-index options) (index/write! root))
             (emit-result! (run! root))))
    :fail? (comp pos? :exit-code)}))
