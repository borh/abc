(ns soranoha.ported.parser-ir-tei
  (:require [soranoha.ported.parser-ir-publication-whitespace :as whitespace]
            [clojure.string :as string]))

(defn- present-text? [text]
  (seq text))

(defn- present-ruby-text? [text]
  (and (string? text)
       (not (string/blank? text))))

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
    (let [rend (layout-rend layout)
          params (layout-params layout)]
      (cond-> {}
        rend
        (assoc :rend rend)

        (get layout "kind")
        (assoc :abc/layout-kind (get layout "kind"))

        params
        (assoc :abc/layout-params params)))))

(defn- leading-indent [text]
  (count (re-find #"^　+" (or text ""))))

(defn- source-note-hiccup [node]
  (into [:note {:type (get node "note_type")}]
        (interpose [:lb]
                   (map (fn [line]
                          (let [indent (leading-indent line)]
                            [:seg (cond-> {:type "source-line"}
                                    (pos? indent)
                                    (assoc :style (str "padding-inline-start: " indent "em")))
                             (subs line indent)]))
                        (string/split-lines (get node "text"))))))

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
  ([acc node _depth]
   (update acc :current-paragraph into
           (whitespace/source-text->tei-inline (get node "text")))))

(defn- render-ruby-node
  ([acc node depth]
   (let [ruby (get node "ruby")]
     (if (and (present-ruby-text? (get ruby "base"))
              (present-ruby-text? (get ruby "reading")))
       (let [attrs (cond-> {:type "furigana"}
                     (get ruby "direction")
                     (assoc :rend (get ruby "direction")))]
         (let [before (:current-paragraph acc)
               rendered (if (seq (get node "inline_children"))
                          (render-inline-children (assoc acc :current-paragraph [])
                                                  (get node "inline_children") depth)
                          (assoc acc :current-paragraph [(get ruby "base")]))]
           (append-inline (assoc rendered :current-paragraph before)
                          [:ruby attrs
                           (into [:rb] (:current-paragraph rendered))
                           [:rt (get ruby "reading")]])))
       (mark-omitted acc "ruby")))))

(defn- render-gaiji-node
  ([acc node _depth]
   (let [identity (gaiji-declaration node)
         base-id (:xml-id identity)
         id (or (get (:gaiji-identities acc) identity)
                (if-not (contains? (:char-declaration-ids acc) base-id)
                  base-id
                  (loop [ordinal 0]
                    (let [candidate (str base-id "-at-" (get-in node ["span" "start"] 0)
                                         (when (pos? ordinal) (str "-" ordinal)))]
                      (if (contains? (:char-declaration-ids acc) candidate)
                        (recur (inc ordinal)) candidate)))))
         declaration (assoc identity :xml-id id)]
     (-> acc
         (assoc-in [:gaiji-identities identity] id)
         (register-char-declaration declaration)
         (append-inline (cond-> [:g {:ref (str "#" id)}]
                          (seq (:unicode declaration)) (conj (:unicode declaration))))))))

(defn- render-editor-note-node
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
  ([acc node depth]
   (render-inline-wrapper acc
                          (seq (get node "inline_children"))
                          (get node "text")
                          depth
                          [:hi {:rend (get node "style")}])))

(defn- render-layout-span-node
  ([acc node depth]
   (let [layout (get node "layout")]
     (if-let [rend (some-> layout inline-layout-rend)]
       (let [params (layout-params layout)]
         (render-inline-wrapper acc
                                (seq (get node "inline_children"))
                                (get node "text")
                                depth
                                [:hi (cond-> {:rend rend}
                                       (get layout "kind")
                                       (assoc :abc/layout-kind (get layout "kind"))

                                       params
                                       (assoc :abc/layout-params params))]))
       (mark-omitted acc "layout-span")))))

(defn- heading-attrs [node]
  (cond-> {:n (str (get node "level"))}
    (some? (get node "indent"))
    (assoc :style (str "padding-inline-start: " (get node "indent") "em"))))

(defn- render-heading-node
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
                     (into [:head (heading-attrs node)]
                           head-fragment))))
       (update base :current-division conj
               [:head (heading-attrs node)
                (get node "text")])))))

(defn- render-indentation-node
  ([acc node _depth]
   (if (present-text? (get node "text"))
     (append-inline acc
                    [:seg {:type "indentation"
                           :n (str (get node "depth"))}
                     (get node "text")])
     (mark-omitted acc "indentation"))))

(defn- render-page-break-node
  ([acc node _depth]
   (append-block acc
                 (cond-> [:pb]
                   (some? (get node "page_number"))
                   (conj {:n (get node "page_number")})))))

(defn- render-line-break-node
  ([acc _node _depth]
   (append-inline acc [:lb])))

(defn- render-image-node
  ([acc node _depth]
   (append-block acc
                 (cond-> [:figure
                          [:graphic {:url (get node "src")}]]
                   (present-text? (get node "alt"))
                   (conj [:figDesc (get node "alt")])))))

(defn- render-caption-node
  ([acc node _depth]
   (append-block acc [:figDesc (get node "text")])))

(defn- render-quote-node
  ([acc node _depth]
   (if (present-text? (get node "text"))
     (append-inline acc [:quote (get node "text")])
     (mark-omitted acc "quote"))))

(defn- render-source-note-node
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
   :gaiji-identities {}
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

(defn- paragraph-render-inputs [nodes paragraph]
  (let [{start "start" end "end"} (paragraph-range paragraph)
        first-node (when (< start end) (nth nodes start))
        indent (if (and (= "body" (get paragraph "role"))
                        (= "text" (get first-node "type")))
                 (leading-indent (get first-node "text"))
                 0)]
    [(if (pos? indent)
       (update-in nodes [start "text"] subs indent)
       nodes)
     (cond-> (paragraph-attrs paragraph)
       (pos? indent) (assoc :style (str "text-indent: " indent "em")))]))

(defn- render-paragraph-row [nodes acc paragraph]
  (let [[nodes attrs] (paragraph-render-inputs nodes paragraph)
        {start "start" end "end"} (paragraph-range paragraph)
        node-slice (subvec nodes start end)]
    (case (get paragraph "role")
      "body" (-> acc
                 (assoc :current-paragraph-attrs attrs)
                 (render-node-seq node-slice)
                 flush-paragraph)
      "source-note" (-> acc
                        flush-paragraph
                        (render-node-seq node-slice)
                        flush-paragraph)
      (-> acc
          (assoc :current-paragraph-attrs attrs)
          (render-node-seq node-slice)
          flush-paragraph))))

(defn- sentence-attrs [sentence]
  (let [tags (get sentence "tags" [])
        base-attrs (cond-> {:xml:id (get sentence "id")}
                     (some #{"orthographic-katakana"} tags)
                     (assoc :type "orthographic-katakana"))
        part (get sentence "part")
        next-id (get sentence "next_id")
        prev-id (get sentence "prev_id")]
    (cond-> base-attrs
      part
      (assoc :part part)

      next-id
      (assoc :next (str "#" next-id))

      prev-id
      (assoc :prev (str "#" prev-id)))))

(defn- sentence-node [sentence fragment]
  (into [:s (sentence-attrs sentence)] fragment))

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

(defn- sentence-inline-node? [node]
  (case (get node "type")
    ("text" "ruby" "gaiji" "editor-note" "emphasis" "layout-span"
            "indentation" "line-break" "quote") true
    "source-note" (= "body" (get node "placement"))
    false))

(defn- sentence-wrappable-paragraph? [node-slice paragraph]
  (and (= "body" (get paragraph "role"))
       (every? sentence-inline-node? node-slice)))

(defn- render-paragraph-row-with-sentences [nodes sentences-by-pid acc paragraph]
  (let [{start "start" end "end"} (paragraph-range paragraph)
        node-slice (subvec nodes start end)
        paragraph-sentences (get sentences-by-pid (get paragraph "id"))]
    (if (and (seq paragraph-sentences)
             (sentence-wrappable-paragraph? node-slice paragraph))
      (let [[nodes attrs] (paragraph-render-inputs nodes paragraph)]
        (-> (assoc acc :current-paragraph-attrs attrs)
            (as-> state
                  (reduce (partial render-sentence-row nodes)
                          state
                          paragraph-sentences))
            flush-paragraph))
      (render-paragraph-row nodes acc paragraph))))

(defn- validate-layout-blocks! [nodes paragraphs blocks]
  (doseq [block blocks]
    (let [{start "start" end "end"} (get block "paragraph_range")]
      (when-not (and (integer? start) (integer? end) (<= 0 start) (< start end)
                     (<= end (count paragraphs))
                     (every? #(= "body" (get % "role")) (subvec paragraphs start end))
                     (every? sentence-inline-node?
                             (subvec nodes (get-in paragraphs [start "node_range" "start"])
                                     (get-in paragraphs [(dec end) "node_range" "end"]))))
        (throw (ex-info "Invalid layout block paragraph range" {:block block})))))
  (doseq [a blocks b blocks
          :let [{as "start" ae "end"} (get a "paragraph_range")
                {bs "start" be "end"} (get b "paragraph_range")]
          :when (< as bs ae be)]
    (throw (ex-info "Crossing layout block ranges" {:blocks [a b]}))))

(defn- layout-block-attrs [block]
  {:type "layout"
   :style (str "padding-inline-start: " (get block "indent") "em; "
               "writing-mode: horizontal-tb; text-align: center; border-style: solid")})

(defn- close-layout-blocks [acc frames paragraph-end]
  (loop [acc acc frames frames]
    (if-let [{:keys [block target start]} (peek frames)]
      (if (= paragraph-end (get-in block ["paragraph_range" "end"]))
        (let [children (get acc target)
              wrapped (into [:div (layout-block-attrs block)] (subvec children start))]
          (recur (assoc acc target (conj (subvec children 0 start) wrapped)) (pop frames)))
        [acc frames])
      [acc frames])))

(defn- render-with-paragraphs [nodes paragraphs sentences layout-blocks]
  (validate-paragraph-ranges! nodes paragraphs)
  (validate-layout-blocks! nodes paragraphs layout-blocks)
  (let [sentences-by-pid (sentences-by-paragraph sentences)
        starts (group-by #(get-in % ["paragraph_range" "start"]) layout-blocks)
        [result end _] (reduce
                        (fn [[acc prior-end frames] [index paragraph]]
                          (let [{start "start" end "end"} (paragraph-range paragraph)
                                acc (-> acc (render-node-seq (subvec nodes prior-end start)) flush-paragraph)
                                target (if (seq (:current-division acc)) :current-division :body-children)
                                frames (into frames (map (fn [block] {:block block :target target :start (count (get acc target))})
                                                         (sort-by #(get-in % ["paragraph_range" "end"]) > (get starts index))))
                                rendered (render-paragraph-row-with-sentences nodes sentences-by-pid acc paragraph)
                                [closed frames] (close-layout-blocks rendered frames (inc index))]
                            [closed end frames]))
                        [(initial-acc) 0 []]
                        (map-indexed vector paragraphs))]
    (finalize-result (render-node-seq result (subvec nodes end)))))

(defn- render-flat [nodes]
  (finalize-result
   (render-node-seq (initial-acc) nodes)))

(defn render [parser-ir]
  (let [nodes (vec (get parser-ir "nodes"))
        paragraphs (seq (get parser-ir "paragraphs"))
        sentences (vec (get parser-ir "sentences" []))]
    (when (and (seq (get parser-ir "layout_blocks")) (not paragraphs))
      (throw (ex-info "Layout blocks require paragraph ranges" {})))
    (if paragraphs
      (render-with-paragraphs nodes (vec paragraphs) sentences (get parser-ir "layout_blocks" []))
      (render-flat nodes))))
