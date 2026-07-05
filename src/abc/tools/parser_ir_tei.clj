(ns abc.tools.parser-ir-tei
  (:require [clojure.string :as string]))

(defn- present-text? [text]
  (seq text))

(defn- mark-omitted [acc node-type]
  (update acc :omitted conj {:type node-type :policy "omitted"}))

(defn- append-inline [acc node]
  (update acc :current-paragraph conj node))

(defn- append-structural-child [acc node]
  (if (seq (:current-division acc))
    (update acc :current-division conj node)
    (update acc :body-children conj node)))

(defn- layout-rend [layout]
  (case (get layout "kind")
    "jisage" (when-some [indent (get layout "indent")]
               (str "jisage indent(" indent ")"))
    "burasage" (let [first-line (get layout "first_line_indent")
                     continuation (get layout "continuation_indent")]
                 (when (and (some? first-line) (some? continuation))
                   (str "burasage first(" first-line ") rest(" continuation ")")))
    "chitsuki" (let [align (get layout "align")
                     offset (get layout "offset_from_end")]
                 (when (and align (some? offset))
                   (str "chitsuki align(" align ") offset-from-end(" offset ")")))
    "jizume" (when-some [width (get layout "width")]
               (str "jizume width(" width ")"))
    "line-jisage" (when-some [indent (get layout "indent")]
                    (str "line-jisage indent(" indent ")"))
    nil))

(defn- paragraph-attrs [paragraph]
  (when-let [rend (some-> (get paragraph "layout") layout-rend)]
    {:rend rend}))

(defn- source-note-hiccup [node]
  [:note {:type (get node "note_type")}
   (get node "text")])

(defn- flush-paragraph [acc]
  (if (seq (:current-paragraph acc))
    (let [paragraph-node (into (cond-> [:p]
                                 (:current-paragraph-attrs acc)
                                 (conj (:current-paragraph-attrs acc)))
                               (:current-paragraph acc))]
      (assoc (append-structural-child acc paragraph-node)
             :current-paragraph []
             :current-paragraph-attrs nil))
    (assoc acc :current-paragraph-attrs nil)))

(defn- flush-division [acc]
  (if (seq (:current-division acc))
    (-> acc
        (update :body-children conj (into [:div] (:current-division acc)))
        (assoc :current-division []))
    acc))

(defn- append-block [acc node]
  (append-structural-child (flush-paragraph acc) node))

(defn- count-node [acc node-type]
  (update acc :node_counts update node-type (fnil inc 0)))

(def ^:private xml-safe-id-pattern #"^[A-Za-z_][A-Za-z0-9_.-]*$")

(defn- fallback-gaiji-id [span]
  (str "gaiji-" (get span "start") "-" (get span "end")))

(defn- sanitize-gaiji-reference-id [reference]
  (when-let [reference (some-> reference
                               (string/replace #"^#" "")
                               string/trim
                               (string/replace #"[^A-Za-z0-9_.-]+" "-")
                               (string/replace #"-+" "-")
                               (string/replace #"^-|-$" ""))]
    (when (seq reference)
      (if (re-matches xml-safe-id-pattern reference)
        reference
        (str "gaiji-" reference)))))

(defn- normalize-gaiji-id [reference span]
  (or (sanitize-gaiji-reference-id reference)
      (fallback-gaiji-id span)))

(defn- register-char-declaration [acc declaration]
  (if (contains? (:char-declaration-ids acc) (:xml-id declaration))
    acc
    (-> acc
        (update :char_declarations conj declaration)
        (update :char-declaration-ids conj (:xml-id declaration)))))

(defn- gaiji-declaration [node]
  (let [gaiji (get node "gaiji")
        span (get node "span")]
    (cond-> {:xml-id (normalize-gaiji-id (get gaiji "reference") span)}
      (contains? gaiji "unicode")
      (assoc :unicode (get gaiji "unicode"))

      (contains? gaiji "raw_marker")
      (assoc :raw-marker (get gaiji "raw_marker")))))

(defn- render-text-node [acc node]
  (append-inline acc (get node "text")))

(defn- render-ruby-node [acc node]
  (let [ruby (get node "ruby")]
    (if (and (present-text? (get ruby "base"))
             (present-text? (get ruby "reading")))
      (let [attrs (cond-> {:type "furigana"}
                    (get ruby "direction")
                    (assoc :rend (get ruby "direction")))]
        (append-inline acc
                       [:ruby attrs
                        [:rb (get ruby "base")]
                        [:rt (get ruby "reading")]]))
      (mark-omitted acc "ruby"))))

(defn- render-gaiji-node [acc node]
  (let [declaration (gaiji-declaration node)]
    (-> acc
        (register-char-declaration declaration)
        (append-inline [:g {:ref (str "#" (:xml-id declaration))}]))))

(defn- render-editor-note-node [acc node]
  (let [note (get node "note")]
    (append-inline acc
                   [:note {:type (get note "category")}
                    (get note "raw")])))

(defn- render-emphasis-node [acc node]
  (append-inline acc
                 [:hi {:rend (get node "style")}
                  (get node "text")]))

(defn- render-heading-node [acc node]
  (-> acc
      flush-paragraph
      flush-division
      (update :current-division conj
              [:head {:n (str (get node "level"))}
               (get node "text")])))

(defn- render-indentation-node [acc node]
  (if (present-text? (get node "text"))
    (append-inline acc
                   [:seg {:type "indentation"
                          :n (str (get node "depth"))}
                    (get node "text")])
    (mark-omitted acc "indentation")))

(defn- render-page-break-node [acc node]
  (append-block acc
                (cond-> [:pb]
                  (some? (get node "page_number"))
                  (conj {:n (get node "page_number")}))))

(defn- render-line-break-node [acc _node]
  (append-inline acc [:lb]))

(defn- render-image-node [acc node]
  (append-block acc
                (cond-> [:figure
                         [:graphic {:url (get node "src")}]]
                  (present-text? (get node "alt"))
                  (conj [:figDesc (get node "alt")]))))

(defn- render-caption-node [acc node]
  (append-block acc [:figDesc (get node "text")]))

(defn- render-quote-node [acc node]
  (if (present-text? (get node "text"))
    (append-inline acc [:quote (get node "text")])
    (mark-omitted acc "quote")))

(defn- render-source-note-node [acc node]
  (if-not (present-text? (get node "text"))
    (mark-omitted acc "source-note")
    (case (get node "placement")
      "front" (-> acc
                  flush-paragraph
                  flush-division
                  (update :front-notes conj (source-note-hiccup node)))
      "body" (append-inline acc (source-note-hiccup node))
      "back" (-> acc
                 flush-paragraph
                 flush-division
                 (update :back-notes conj (source-note-hiccup node)))
      (mark-omitted acc "source-note"))))

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

(defn- render-node [acc node]
  (let [node-type (get node "type")]
    (if-let [render-node-fn (get node-renderers node-type)]
      (render-node-fn acc node)
      (throw (ex-info "Unsupported TEI parser-IR node type"
                      {:node-type node-type})))))

(defn- initial-acc []
  {:body-children []
   :current-division []
   :current-paragraph []
   :current-paragraph-attrs nil
   :front-notes []
   :back-notes []
   :char_declarations []
   :char-declaration-ids #{}
   :node_counts {}
   :omitted []})

(defn- render-node-seq [acc nodes]
  (reduce (fn [acc node]
            (-> acc
                (count-node (get node "type"))
                (render-node node)))
          acc
          nodes))

(defn- tei-source-div [notes]
  (into [:div {:type "source"}] notes))

(defn- tei-text [result]
  (into [:text]
        (concat
         (when (seq (:front-notes result))
           [[:front (tei-source-div (:front-notes result))]])
         [(into [:body] (:body-children result))]
         (when (seq (:back-notes result))
           [[:back (tei-source-div (:back-notes result))]]))))

(defn- finalize-result [result]
  (let [result (-> result
                   flush-paragraph
                   flush-division)]
    {:body (tei-text result)
     :char_declarations (:char_declarations result)
     :node_counts (:node_counts result)
     :omitted (:omitted result)}))

(defn- paragraph-range [paragraph]
  (get paragraph "node_range"))

(defn- validate-paragraph-ranges! [nodes paragraphs]
  (loop [remaining paragraphs
         prior-end 0]
    (when-let [paragraph (first remaining)]
      (let [{start "start" end "end"} (paragraph-range paragraph)]
        (when-not (and (integer? start)
                       (integer? end)
                       (<= 0 start end (count nodes))
                       (<= prior-end start))
          (throw (ex-info "Invalid parser-IR paragraph node_range"
                          {:paragraph paragraph
                           :nodes-count (count nodes)
                           :prior-end prior-end})))
        (recur (rest remaining) end)))))

(defn- render-paragraph-row [nodes acc paragraph]
  (let [{start "start" end "end"} (paragraph-range paragraph)
        node-slice (subvec nodes start end)]
    (case (get paragraph "role")
      "body" (-> acc
                 (assoc :current-paragraph-attrs (paragraph-attrs paragraph))
                 (render-node-seq node-slice)
                 flush-paragraph)
      "source-note" (-> acc
                        flush-paragraph
                        (render-node-seq node-slice)
                        flush-paragraph)
      (-> acc
          (assoc :current-paragraph-attrs (paragraph-attrs paragraph))
          (render-node-seq node-slice)
          flush-paragraph))))

(defn- render-with-paragraphs [nodes paragraphs]
  (validate-paragraph-ranges! nodes paragraphs)
  (finalize-result
   (reduce (partial render-paragraph-row nodes)
           (initial-acc)
           paragraphs)))

(defn- render-flat [nodes]
  (finalize-result
   (render-node-seq (initial-acc) nodes)))

(defn render [parser-ir]
  (let [nodes (vec (get parser-ir "nodes"))
        paragraphs (seq (get parser-ir "paragraphs"))]
    (if paragraphs
      (render-with-paragraphs nodes (vec paragraphs))
      (render-flat nodes))))

(def covered-node-types
  (set (keys node-renderers)))
