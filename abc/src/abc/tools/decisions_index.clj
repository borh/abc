(ns abc.tools.decisions-index
  "Generated, byte-current derived view of decisions.edn (ADR 0029
   discipline: derived views are gated, never identity)."
  (:require [abc.tools.decisions :as decisions]
            [babashka.fs :as fs]
            [clojure.string :as str]))

(def out-path "docs/adr/INDEX.md")

(defn- inverse-note [corpus slug]
  (let [incoming (for [rec (:decisions corpus)
                       r (:relations rec)
                       :when (and (= :lifecycle (:class r)) (= slug (:to r)))]
                   [(:type r) (:slug rec) (:scope r)])]
    (str/join "; "
              (for [[type from scope] (sort incoming)]
                (str (case type
                       :supersedes "superseded by"
                       :amends "amended by"
                       :depends-on "depended on by")
                     " [" from "](" from ".md)"
                     (when scope (str " [scope: " scope "]")))))))

(defn- record-row [corpus {:keys [slug title status date topics]}]
  (str "| [" slug "](" slug ".md) | " title " | " (name status) " | " date
       " | " (str/join ", " (map name topics))
       " | " (inverse-note corpus slug) " |\n"))

(defn render [corpus]
  (let [decisions (sort-by :slug (:decisions corpus))
        header (str "| Record | Title | Status | Date | Topics | Derived links |\n"
                    "| --- | --- | --- | --- | --- | --- |\n")]
    (str "# Decision Records Index\n\n"
         "GENERATED from `decisions.edn` — do not edit. Regenerate: "
         "`clojure -M:abc/adr-governance --write-index`.\n\n"
         "## All records\n\n" header
         (apply str (map #(record-row corpus %) decisions))
         (apply str
                (for [topic (->> decisions (mapcat :topics) distinct sort)]
                  (str "\n## Topic: " (name topic) "\n\n" header
                       (apply str (for [d decisions
                                        :when (some #{topic} (:topics d))]
                                    (record-row corpus d))))))
         "\n## Legacy numbers\n\n| Legacy | Record |\n| --- | --- |\n"
         (apply str (for [d (sort-by :legacy-number
                                     (filter :legacy-number decisions))]
                      (str "| " (:legacy-number d) " | [" (:slug d) "]("
                           (:slug d) ".md) |\n"))))))

(defn currency-problems [corpus repo-root]
  (let [path (fs/path repo-root out-path)
        expected (render corpus)]
    (if (and (fs/exists? path) (= expected (slurp (fs/file path))))
      []
      [(decisions/problem :stale-index out-path
                          "INDEX.md is not byte-current with decisions.edn; regenerate with --write-index")])))

(defn write! [repo-root]
  (let [{:keys [corpus problems]} (decisions/load-corpus
                                   (fs/path repo-root decisions/corpus-file))]
    (when problems
      (throw (ex-info "cannot index an unreadable corpus" {:problems problems})))
    (spit (str (fs/path repo-root out-path)) (render corpus))))
