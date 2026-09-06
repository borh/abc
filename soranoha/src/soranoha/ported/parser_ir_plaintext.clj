(ns soranoha.ported.parser-ir-plaintext
  (:require [soranoha.ported.parser-ir-publication-whitespace :as whitespace]
            [clojure.string :as string]))

(defn- scalar-count [^String s]
  (.codePointCount s 0 (.length s)))

(defn- append-text [acc node-text]
  (let [s (or node-text "")]
    (-> acc
        (update :text str s)
        (update :offset (fnil + 0) (scalar-count s))
        (assoc :source-text-ended-with-newline? false))))

(defn- append-source-text [acc node-text]
  (let [s (whitespace/source-text->plaintext node-text (empty? (:text acc)))]
    (-> acc
        (update :text str s)
        (update :offset (fnil + 0) (scalar-count s))
        (assoc :source-text-ended-with-newline?
               (whitespace/source-text-ends-with-newline? node-text)))))

(defn- mark-omitted [acc node-type]
  (update acc :omitted conj {:type node-type :policy "omitted"}))

(defn- present-text? [text]
  (seq text))

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
   (append-source-text acc (get node "text"))))

(defn- render-ruby-node
  ([acc node] (render-ruby-node acc node 0))
  ([acc node _depth]
   (let [start (:offset acc 0)
         acc (append-text acc (get-in node ["ruby" "base"]))]
     (update acc :annotations (fnil conj [])
             (cond-> {"annotation_kind" "ruby"
                      "span" {"start" start "end" (:offset acc)}
                      "ruby" {"base" (or (get-in node ["ruby" "base"]) "")
                              "reading" (or (get-in node ["ruby" "reading"]) "")
                              "direction" (get-in node ["ruby" "direction"])}}
               (get node "span") (assoc "source_span"
                                        (select-keys (get node "span") ["start" "end"])))))))

(defn- render-gaiji-node
  ([acc node] (render-gaiji-node acc node 0))
  ([acc node _depth]
   (let [start (:offset acc 0)
         acc (append-text acc (or (get-in node ["gaiji" "unicode"])
                                  (get-in node ["gaiji" "raw_marker"])))]
     (update acc :annotations (fnil conj [])
             (cond-> {"annotation_kind" "gaiji"
                      "span" {"start" start "end" (:offset acc)}
                      "gaiji" {"raw_marker" (or (get-in node ["gaiji" "raw_marker"]) "")
                               "unicode" (get-in node ["gaiji" "unicode"])
                               "resolved" (boolean (get-in node ["gaiji" "resolved"]))}}
               (get node "span") (assoc "source_span"
                                        (select-keys (get node "span") ["start" "end"])))))))

(defn- inline-children-need-visible-text-fallback? [children]
  (boolean
   (some (fn [child]
           (let [node-type (get child "type")]
             (or (and (= "gaiji" node-type)
                      (true? (get-in child ["gaiji" "resolved"]))
                      (nil? (get-in child ["gaiji" "unicode"])))
                 (and (= "emphasis" node-type)
                      (inline-children-need-visible-text-fallback?
                       (get child "inline_children"))))))
         children)))

(defn- render-editor-note-node
  ([acc node] (render-editor-note-node acc node 0))
  ([acc _node _depth]
   (mark-omitted acc "editor-note")))

(defn- render-emphasis-node
  ([acc node] (render-emphasis-node acc node 0))
  ([acc node depth]
   (if-let [children (seq (get node "inline_children"))]
     (if (and (present-text? (get node "text"))
              (inline-children-need-visible-text-fallback? children))
       (append-text acc (get node "text"))
       (render-inline-children acc children depth))
     (append-text acc (get node "text")))))

(defn- render-layout-span-node
  ([acc node] (render-layout-span-node acc node 0))
  ([acc node depth]
   (if-let [children (seq (get node "inline_children"))]
     (if (and (present-text? (get node "text"))
              (inline-children-need-visible-text-fallback? children))
       (append-text acc (get node "text"))
       (render-inline-children acc children depth))
     (append-text acc (get node "text")))))

(defn- render-heading-node
  ([acc node] (render-heading-node acc node 0))
  ([acc node _depth]
   (append-text acc (str (when (seq (:text acc)) "\n")
                         (or (get node "text") "") "\n"))))

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
  ([acc _node _depth]
   (mark-omitted acc "source-note")))

(def ^:private node-renderers
  {"text" render-text-node
   "ruby" render-ruby-node
   "gaiji" render-gaiji-node
   "editor-note" render-editor-note-node
   "emphasis" render-emphasis-node
   "layout-span" render-layout-span-node
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

(defn render-with-annotations [parser-ir]
  (let [{:keys [text front_notes source_notes node_counts omitted annotations
                source-text-ended-with-newline?]}
        (reduce render-node
                {:text "" :offset 0 :annotations []
                 :front_notes [] :source_notes []
                 :node_counts {} :omitted []
                 :source-text-ended-with-newline? false}
                (get parser-ir "nodes"))
        text (if source-text-ended-with-newline?
               (whitespace/trim-trailing-newlines text)
               text)
        prefix (when (seq front_notes)
                 (str (string/join "\n" front_notes) "\n\n"))
        shift (if prefix (scalar-count prefix) 0)
        text (str prefix text
                  (when (seq source_notes)
                    (str "\n\n" (string/join "\n" source_notes))))]
    {:text text
     :annotations (mapv (fn [ann]
                          (update ann "span"
                                  (fn [{:strs [start end]}]
                                    {"start" (+ start shift)
                                     "end" (+ end shift)})))
                        annotations)
     :node_counts node_counts
     :omitted omitted}))

(defn render [parser-ir]
  (dissoc (render-with-annotations parser-ir) :annotations))
