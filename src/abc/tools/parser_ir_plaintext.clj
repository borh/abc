(ns abc.tools.parser-ir-plaintext
  (:require [clojure.string :as string]))

(defn- append-text [acc node-text]
  (update acc :text str (or node-text "")))

(defn- mark-omitted [acc node-type]
  (update acc :omitted conj {:type node-type :policy "omitted"}))

(defn- present-text? [text]
  (seq text))

(defn- append-separated-note [acc key node-text]
  (if (present-text? node-text)
    (update acc key conj node-text)
    acc))

(declare render-node)

(def ^:private max-inline-depth 64)

(defn- render-inline-children [acc children depth]
  (if (>= depth max-inline-depth)
    (mark-omitted acc "emphasis-inline-depth")
    (reduce (fn [state child]
              (render-node state child (inc depth)))
            acc
            children)))

(defn- render-text-node
  ([acc node] (render-text-node acc node 0))
  ([acc node _depth]
   (append-text acc (get node "text"))))

(defn- render-ruby-node
  ([acc node] (render-ruby-node acc node 0))
  ([acc node _depth]
   (append-text acc (get-in node ["ruby" "base"]))))

(defn- render-gaiji-node
  ([acc node] (render-gaiji-node acc node 0))
  ([acc node _depth]
   (append-text acc (or (get-in node ["gaiji" "unicode"])
                        (get-in node ["gaiji" "raw_marker"])))))

(defn- render-editor-note-node
  ([acc node] (render-editor-note-node acc node 0))
  ([acc _node _depth]
   (mark-omitted acc "editor-note")))

(defn- render-emphasis-node
  ([acc node] (render-emphasis-node acc node 0))
  ([acc node depth]
   (if-let [children (seq (get node "inline_children"))]
     (render-inline-children acc children depth)
     (append-text acc (get node "text")))))

(defn- render-heading-node
  ([acc node] (render-heading-node acc node 0))
  ([acc node _depth]
   (append-text acc (str "\n" (or (get node "text") "") "\n"))))

(defn- render-indentation-node
  ([acc node] (render-indentation-node acc node 0))
  ([acc node _depth]
   (if (present-text? (get node "text"))
     (append-text acc (get node "text"))
     (mark-omitted acc "indentation"))))

(defn- render-page-break-node
  ([acc node] (render-page-break-node acc node 0))
  ([acc _node _depth]
   (append-text acc "\n")))

(defn- render-line-break-node
  ([acc node] (render-line-break-node acc node 0))
  ([acc _node _depth]
   (append-text acc "\n")))

(defn- render-image-node
  ([acc node] (render-image-node acc node 0))
  ([acc node _depth]
   (if (present-text? (get node "alt"))
     (append-text acc (get node "alt"))
     (mark-omitted acc "image"))))

(defn- render-caption-node
  ([acc node] (render-caption-node acc node 0))
  ([acc node _depth]
   (append-text acc (get node "text"))))

(defn- render-quote-node
  ([acc node] (render-quote-node acc node 0))
  ([acc node _depth]
   (if (present-text? (get node "text"))
     (append-text acc (get node "text"))
     (mark-omitted acc "quote"))))

(defn- render-source-note-node
  ([acc node] (render-source-note-node acc node 0))
  ([acc node _depth]
   (if-not (present-text? (get node "text"))
     (mark-omitted acc "source-note")
     (case (get node "placement")
       "front" (append-separated-note acc :front_notes (get node "text"))
       "body" (append-text acc (get node "text"))
       "back" (append-separated-note acc :source_notes (get node "text"))
       (mark-omitted acc "source-note")))))

(def ^:private node-renderers
  {"text" render-text-node
   "ruby" render-ruby-node
   "gaiji" render-gaiji-node
   "editor-note" render-editor-note-node
   "emphasis" render-emphasis-node
   "heading" render-heading-node
   "indentation" render-indentation-node
   "page-break" render-page-break-node
   "line-break" render-line-break-node
   "image" render-image-node
   "caption" render-caption-node
   "quote" render-quote-node
   "source-note" render-source-note-node})

(defn- render-node
  ([acc node] (render-node acc node 0))
  ([acc node depth]
   (let [node-type (get node "type")
         acc (update acc :node_counts update node-type (fnil inc 0))]
     (if-let [render-node-fn (get node-renderers node-type)]
       (render-node-fn acc node depth)
       acc))))

(defn render [parser-ir]
  (let [{:keys [text front_notes source_notes node_counts omitted]}
        (reduce render-node
                {:text ""
                 :front_notes []
                 :source_notes []
                 :node_counts {}
                 :omitted []}
                (get parser-ir "nodes"))
        text (str (when (seq front_notes)
                    (str (string/join "\n" front_notes) "\n\n"))
                  text
                  (when (seq source_notes)
                    (str "\n\n" (string/join "\n" source_notes))))]
    {:text text
     :node_counts node_counts
     :omitted omitted}))

(defn render-string [parser-ir]
  (:text (render parser-ir)))

(def covered-node-types
  (set (keys node-renderers)))
