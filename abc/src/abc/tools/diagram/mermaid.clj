(ns abc.tools.diagram.mermaid
  "Deterministic Mermaid flowchart renderer. Sorted ordering keeps a regenerated
   diagram byte-stable and safe to commit + drift-check. See ADR 0029."
  (:require [clojure.string :as str]))

(defn sanitize-id [k]
  (str/replace (name k) #"[^A-Za-z0-9_]" "_"))

(defn quote-label [s]
  (str "\"" (str/replace (str s) "\"" "&quot;") "\""))

(defn- edge-arrow [style]
  (case style :dashed " -.->" :thick " ==>" " -->"))

(defn flowchart [{:keys [direction nodes edges class-defs]}]
  (let [nodes (vec nodes)
        collisions (->> nodes
                        (map :id)
                        (group-by sanitize-id)
                        (keep (fn [[rendered ids]]
                                (when (< 1 (count ids)) [rendered (vec ids)])))
                        (into {}))]
    (when (seq collisions)
      (throw (ex-info "node ids collide after sanitize-id"
                      {:collisions collisions})))
    (let [dir (or direction "TD")
          node-lines (->> nodes
                          (sort-by (comp sanitize-id :id))
                          (map (fn [{:keys [id label class]}]
                                 (str "  " (sanitize-id id) "[" (quote-label label) "]"
                                      (when class (str ":::" (name class)))))))
          edge-lines (->> edges
                          (sort-by (fn [{:keys [from to label]}]
                                     [(sanitize-id from) (sanitize-id to) (str label)]))
                          (map (fn [{:keys [from to label style]}]
                                 (str "  " (sanitize-id from) (edge-arrow style)
                                      (when label (str "|" (quote-label label) "|"))
                                      " " (sanitize-id to)))))
          class-lines (->> class-defs
                           (sort-by (comp name key))
                           (map (fn [[k v]] (str "  classDef " (name k) " " v))))]
      (str (str/join "\n" (concat [(str "flowchart " dir)]
                                    node-lines edge-lines class-lines))
           "\n"))))
