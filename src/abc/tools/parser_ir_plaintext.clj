(ns abc.tools.parser-ir-plaintext)

(defn- append-text [acc node-text]
  (update acc :text str (or node-text "")))

(defn- mark-omitted [acc node-type]
  (update acc :omitted conj {:type node-type :policy "omitted"}))

(defn- render-node [acc node]
  (let [node-type (get node "type")]
    (let [acc (update acc :node_counts update node-type (fnil inc 0))]
      (case node-type
        "text" (append-text acc (get node "text"))
        "ruby" (append-text acc (get-in node ["ruby" "base"]))
        "gaiji" (append-text acc (or (get-in node ["gaiji" "unicode"])
                                     (get-in node ["gaiji" "raw_marker"])))
        "editor-note" (mark-omitted acc node-type)
        "emphasis" (append-text acc (get node "text"))
        "heading" (append-text acc (str "\n" (or (get node "text") "") "\n"))
        "indentation" (if-let [text (get node "text")]
                        (append-text acc text)
                        (mark-omitted acc node-type))
        "page-break" (append-text acc "\n")
        "image" (if-let [alt (get node "alt")]
                  (append-text acc alt)
                  (mark-omitted acc node-type))
        "caption" (append-text acc (get node "text"))
        "quote" (if-let [text (get node "text")]
                  (append-text acc text)
                  (mark-omitted acc node-type))
        acc))))

(defn render [parser-ir]
  (reduce render-node
          {:text ""
           :node_counts {}
           :omitted []}
          (get parser-ir "nodes")))

(defn render-string [parser-ir]
  (:text (render parser-ir)))

(def covered-node-types
  #{"text" "ruby" "gaiji" "editor-note" "emphasis" "heading"
    "indentation" "page-break" "image" "caption" "quote"})
