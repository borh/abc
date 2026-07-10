(ns abc.tools.adr-governance
  (:require [abc.tools.adr :as adr]))

(defn run! [repo-root]
  (let [problems (adr/validate-repository repo-root)]
    {:ok? (empty? problems) :problems problems}))

(defn -main [& [repo-root]]
  (let [{:keys [ok? problems]} (run! (or repo-root "."))]
    (binding [*out* *err*]
      (doseq [{:keys [file kind message]} problems]
        (println "ADR-LINT" file (str (name kind) ":") message)))
    (when ok? (println "ADR governance valid"))
    (System/exit (if ok? 0 1))))
