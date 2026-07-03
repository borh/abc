(ns abc.tools.parser-ir-plaintext)

(defn- append-text [acc node-text]
  (update acc :text str (or node-text "")))

(defn- mark-omitted [acc node-type]
  (update acc :omitted conj {:type node-type :policy "omitted"}))

(defn- present-text? [text]
  (seq text))

(defn- render-text-node [acc node]
  (append-text acc (get node "text")))

(defn- render-ruby-node [acc node]
  (append-text acc (get-in node ["ruby" "base"])))

(defn- render-gaiji-node [acc node]
  (append-text acc (or (get-in node ["gaiji" "unicode"])
                       (get-in node ["gaiji" "raw_marker"]))))

(defn- render-editor-note-node [acc node]
  (mark-omitted acc "editor-note"))

(defn- render-emphasis-node [acc node]
  (append-text acc (get node "text")))

(defn- render-heading-node [acc node]
  (append-text acc (str "\n" (or (get node "text") "") "\n")))

(defn- render-indentation-node [acc node]
  (if (present-text? (get node "text"))
    (append-text acc (get node "text"))
    (mark-omitted acc "indentation")))

(defn- render-page-break-node [acc node]
  (append-text acc "\n"))

(defn- render-image-node [acc node]
  (if (present-text? (get node "alt"))
    (append-text acc (get node "alt"))
    (mark-omitted acc "image")))

(defn- render-caption-node [acc node]
  (append-text acc (get node "text")))

(defn- render-quote-node [acc node]
  (if (present-text? (get node "text"))
    (append-text acc (get node "text"))
    (mark-omitted acc "quote")))

(def ^:private node-renderers
  {"text" render-text-node
   "ruby" render-ruby-node
   "gaiji" render-gaiji-node
   "editor-note" render-editor-note-node
   "emphasis" render-emphasis-node
   "heading" render-heading-node
   "indentation" render-indentation-node
   "page-break" render-page-break-node
   "image" render-image-node
   "caption" render-caption-node
   "quote" render-quote-node})

(defn- render-node [acc node]
  (let [node-type (get node "type")]
    (let [acc (update acc :node_counts update node-type (fnil inc 0))]
      (if-let [render-node-fn (get node-renderers node-type)]
        (render-node-fn acc node)
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
  (set (keys node-renderers)))
