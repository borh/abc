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

(defn- layout-params [layout]
  (case (get layout "kind")
    "jisage" (when-some [indent (get layout "indent")]
               (str "indent=" indent))
    "burasage" (let [first-line (get layout "first_line_indent")
                     continuation (get layout "continuation_indent")]
                 (when (and (some? first-line) (some? continuation))
                   (str "first-line-indent=" first-line
                        ";continuation-indent=" continuation)))
    "chitsuki" (let [align (get layout "align")
                     offset (get layout "offset_from_end")]
                 (when (and align (some? offset))
                   (str "align=" align ";offset-from-end=" offset)))
    "jizume" (when-some [width (get layout "width")]
               (str "width=" width))
    "line-jisage" (when-some [indent (get layout "indent")]
                    (str "indent=" indent))
    "font-size" (let [size-type (get layout "size_type")
                      level (get layout "level")]
                  (when (and size-type (some? level))
                    (str "size-type=" size-type ";level=" level)))
    "tcy" (when-let [marker (get layout "marker")]
            (str "marker=" marker))
    "keigakomi" (when-let [border (get layout "border")]
                  (str "border=" border))
    "yokogumi" "direction=horizontal"
    nil))

(defn- inline-layout-rend [layout]
  (case (get layout "kind")
    "font-size" (let [size-type (get layout "size_type")
                      level (get layout "level")]
                  (when (and size-type (some? level))
                    (str "font-size " size-type "(" level ")")))
    "tcy" "text-combine-upright"
    "keigakomi" (if-let [border (get layout "border")]
                  (str "keigakomi border(" border ")")
                  "keigakomi")
    "yokogumi" "yokogumi horizontal"
    nil))

(defn- paragraph-attrs [paragraph]
  (when-let [layout (get paragraph "layout")]
    (let [rend (layout-rend layout)]
      (cond-> {}
        rend
        (assoc :rend rend)

        (get layout "kind")
        (assoc :abc/layout-kind (get layout "kind"))

        (layout-params layout)
        (assoc :abc/layout-params (layout-params layout))))))

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

(declare render-node)

(def ^:private max-inline-depth 64)

(defn- render-inline-children [acc children depth]
  (if (>= depth max-inline-depth)
    (mark-omitted acc "emphasis-inline-depth")
    (reduce (fn [state child]
              (-> state
                  (count-node (get child "type"))
                  (render-node child (inc depth))))
            acc
            children)))

(defn- render-text-node
  ([acc node] (render-text-node acc node 0))
  ([acc node _depth]
   (append-inline acc (get node "text"))))

(defn- render-ruby-node
  ([acc node] (render-ruby-node acc node 0))
  ([acc node _depth]
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
       (mark-omitted acc "ruby")))))

(defn- render-gaiji-node
  ([acc node] (render-gaiji-node acc node 0))
  ([acc node _depth]
   (let [declaration (gaiji-declaration node)]
     (-> acc
         (register-char-declaration declaration)
         (append-inline [:g {:ref (str "#" (:xml-id declaration))}])))))

(defn- render-editor-note-node
  ([acc node] (render-editor-note-node acc node 0))
  ([acc node _depth]
   (let [note (get node "note")]
     (append-inline acc
                    [:note {:type (get note "category")}
                     (get note "raw")]))))

(defn- render-inline-wrapper [acc children text depth wrapper]
  (if (seq children)
    (let [before-count (count (:current-paragraph acc))
          rendered (render-inline-children acc children depth)
          inline-fragment (subvec (vec (:current-paragraph rendered)) before-count)]
      (assoc rendered
             :current-paragraph
             (conj (subvec (vec (:current-paragraph acc)) 0 before-count)
                   (into wrapper inline-fragment))))
    (append-inline acc (conj wrapper text))))

(defn- render-emphasis-node
  ([acc node] (render-emphasis-node acc node 0))
  ([acc node depth]
   (render-inline-wrapper acc
                          (seq (get node "inline_children"))
                          (get node "text")
                          depth
                          [:hi {:rend (get node "style")}])))

(defn- render-layout-span-node
  ([acc node] (render-layout-span-node acc node 0))
  ([acc node depth]
   (let [layout (get node "layout")]
     (if-let [rend (some-> layout inline-layout-rend)]
       (render-inline-wrapper acc
                              (seq (get node "inline_children"))
                              (get node "text")
                              depth
                              [:hi (cond-> {:rend rend}
                                     (get layout "kind")
                                     (assoc :abc/layout-kind (get layout "kind"))

                                     (layout-params layout)
                                     (assoc :abc/layout-params (layout-params layout)))])
       (mark-omitted acc "layout-span")))))

(defn- render-heading-node
  ([acc node] (render-heading-node acc node 0))
  ([acc node depth]
   (let [children (seq (get node "inline_children"))
         base (-> acc flush-paragraph flush-division)]
     (if children
       (let [scratch (assoc base :current-paragraph [])
             rendered (render-inline-children scratch children depth)
             head-fragment (:current-paragraph rendered)]
         (-> rendered
             (assoc :current-paragraph [])
             (update :current-division conj
                     (into [:head {:n (str (get node "level"))}]
                           head-fragment))))
       (update base :current-division conj
               [:head {:n (str (get node "level"))}
                (get node "text")])))))

(defn- render-indentation-node
  ([acc node] (render-indentation-node acc node 0))
  ([acc node _depth]
   (if (present-text? (get node "text"))
     (append-inline acc
                    [:seg {:type "indentation"
                           :n (str (get node "depth"))}
                     (get node "text")])
     (mark-omitted acc "indentation"))))

(defn- render-page-break-node
  ([acc node] (render-page-break-node acc node 0))
  ([acc node _depth]
   (append-block acc
                 (cond-> [:pb]
                   (some? (get node "page_number"))
                   (conj {:n (get node "page_number")})))))

(defn- render-line-break-node
  ([acc node] (render-line-break-node acc node 0))
  ([acc _node _depth]
   (append-inline acc [:lb])))

(defn- render-image-node
  ([acc node] (render-image-node acc node 0))
  ([acc node _depth]
   (append-block acc
                 (cond-> [:figure
                          [:graphic {:url (get node "src")}]]
                   (present-text? (get node "alt"))
                   (conj [:figDesc (get node "alt")])))))

(defn- render-caption-node
  ([acc node] (render-caption-node acc node 0))
  ([acc node _depth]
   (append-block acc [:figDesc (get node "text")])))

(defn- render-quote-node
  ([acc node] (render-quote-node acc node 0))
  ([acc node _depth]
   (if (present-text? (get node "text"))
     (append-inline acc [:quote (get node "text")])
     (mark-omitted acc "quote"))))

(defn- render-source-note-node
  ([acc node] (render-source-note-node acc node 0))
  ([acc node _depth]
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
       (mark-omitted acc "source-note")))))

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
   (let [node-type (get node "type")]
     (if-let [render-node-fn (get node-renderers node-type)]
       (render-node-fn acc node depth)
       (throw (ex-info "Unsupported TEI parser-IR node type"
                       {:node-type node-type}))))))

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
  (let [body-children (if (seq (:body-children result))
                        (:body-children result)
                        [[:p [:gap {:reason "missing"}]]])]
    (into [:text]
          (concat
           (when (seq (:front-notes result))
             [[:front (tei-source-div (:front-notes result))]])
           [(into [:body] body-children)]
           (when (seq (:back-notes result))
             [[:back (tei-source-div (:back-notes result))]])))))

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

(defn- sentence-attrs [sentence]
  (when (some #{"orthographic-katakana"} (get sentence "tags" []))
    {:type "orthographic-katakana"}))

(defn- sentence-node [sentence fragment]
  (if-let [attrs (sentence-attrs sentence)]
    (into [:s attrs] fragment)
    (into [:s] fragment)))

(defn- render-sentence-row [nodes acc sentence]
  (let [{start "start" end "end"} (get sentence "node_range")
        before-count (count (:current-paragraph acc))
        rendered (render-node-seq acc (subvec nodes start end))
        rendered-paragraph (vec (:current-paragraph rendered))
        prefix (subvec rendered-paragraph 0 before-count)
        fragment (subvec rendered-paragraph before-count)]
    (assoc rendered
           :current-paragraph
           (conj prefix (sentence-node sentence fragment)))))

(defn- sentences-by-paragraph [sentences]
  (group-by #(get % "paragraph_id") sentences))

(defn- render-paragraph-row-with-sentences [nodes sentences-by-pid acc paragraph]
  (let [paragraph-sentences (get sentences-by-pid (get paragraph "id"))]
    (if (and (= "body" (get paragraph "role"))
             (seq paragraph-sentences))
      (-> (assoc acc :current-paragraph-attrs (paragraph-attrs paragraph))
          (as-> state
                (reduce (partial render-sentence-row nodes)
                        state
                        paragraph-sentences))
          flush-paragraph)
      (render-paragraph-row nodes acc paragraph))))

(defn- render-with-paragraphs [nodes paragraphs sentences]
  (validate-paragraph-ranges! nodes paragraphs)
  (let [sentences-by-pid (sentences-by-paragraph sentences)]
    (finalize-result
     (reduce (partial render-paragraph-row-with-sentences nodes sentences-by-pid)
             (initial-acc)
             paragraphs))))

(defn- render-flat [nodes]
  (finalize-result
   (render-node-seq (initial-acc) nodes)))

(defn render [parser-ir]
  (let [nodes (vec (get parser-ir "nodes"))
        paragraphs (seq (get parser-ir "paragraphs"))
        sentences (vec (get parser-ir "sentences" []))]
    (if paragraphs
      (render-with-paragraphs nodes (vec paragraphs) sentences)
      (render-flat nodes))))

(def covered-node-types
  (set (keys node-renderers)))
