(ns abc.tools.adr-governance
  (:refer-clojure :exclude [run!])
  (:require [abc.tools.adr :as adr]
            [abc.tools.cli :as abc-cli]))

(def cli-options
  [[nil "--repo-root PATH" "ABC artifact and ADR root."]])

(defn run!
  "Strictly validate the complete ADR corpus under repo-root."
  [repo-root]
  (let [problems (adr/validate-repository repo-root)
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
      (run! (or (:repo-root options) (first arguments) ".")))))

(defn usage [_]
  "Usage: clojure -M:abc/adr-governance [--repo-root PATH] [REPO_ROOT]")

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
           (emit-result! (run! (or (:repo-root options) (first arguments) "."))))
    :fail? (comp pos? :exit-code)}))
