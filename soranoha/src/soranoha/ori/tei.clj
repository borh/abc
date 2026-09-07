(ns soranoha.ori.tei
  (:require [soranoha.ori.publication-whitespace :as whitespace]
            [soranoha.core.json :as record-json]
            [clojure.string :as string]))

(defn- present-text? [text]
  (seq text))

(defn- source-reference [node]
  (when-let [span (get node "source_span")]
    (when-not (and (= "decoded_utf8" (get span "coordinate_system"))
                   (integer? (get span "start")) (integer? (get span "end"))
                   (<= 0 (get span "start") (get span "end")))
      (throw (ex-info "TEI source correspondence requires decoded UTF-8 coordinates" {:span span})))
    (str "source-" (get span "start") "-" (get span "end"))))

(defn- sourced [node element]
  (if-let [reference (::source-reference node)]
    (if (map? (second element))
      (assoc-in element [1 :source] (str "#" reference))
      (into [(first element) {:source (str "#" reference)}] (rest element)))
    element))

(defn- register-source [acc node]
  (let [reference (source-reference node)
        span (get node "source_span")
        existing (get-in acc [:source-spans reference])]
    (when (and existing (not= existing span))
      (throw (ex-info "Conflicting metadata for one decoded source extent"
                      {:reference reference :existing existing :span span})))
    [(cond-> acc reference (assoc-in [:source-spans reference] span))
     (assoc node ::source-reference reference)]))

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
    "font-size" (case (get layout "size_type")
                  "absolute" (str "size-type=absolute;size=" (get layout "size"))
                  "qualitative" (str "size-type=qualitative;direction=" (get layout "direction")
                                     (when-let [qualifier (get layout "qualifier")]
                                       (str ";qualifier=" qualifier)))
                  (let [size-type (get layout "size_type")
                        level (get layout "level")]
                    (when (and size-type (some? level))
                      (str "size-type=" size-type ";level=" level))))
    "baseline-position" (str "position=" (get layout "position"))
    "small-script" (str "position=" (get layout "position"))
    "tcy" (when-let [marker (get layout "marker")]
            (str "marker=" marker))
    "keigakomi" (when-let [border (get layout "border")]
                  (str "border=" border))
    "yokogumi" "direction=horizontal"
    nil))

(defn- layout-attributes [layout]
  (if (vector? layout) layout [layout]))

(defn- inline-layout-rend [layout]
  (case (get layout "kind")
    "emphasis" (string/join " " (remove nil? [(get layout "style")
                                              (get-in layout ["decoration" "kind"])
                                              (get-in layout ["decoration" "position"])]))
    "font-size" (case (get layout "size_type")
                  "absolute" (str "font-size absolute(" (get layout "size") ")")
                  "qualitative" (str "font-size qualitative(" (get layout "direction") ")"
                                     (when-let [qualifier (get layout "qualifier")]
                                       (str " qualifier(" qualifier ")")))
                  (let [size-type (get layout "size_type")
                        level (get layout "level")]
                    (when (and size-type (some? level))
                      (str "font-size " size-type "(" level ")"))))
    "baseline-position" (str "baseline-" (get layout "position"))
    "exponent" "exponent"
    "small-script" (str "small-script " (get layout "position"))
    "tcy" "text-combine-upright"
    "keigakomi" (if-let [border (get layout "border")]
                  (str "keigakomi border(" border ")")
                  "keigakomi")
    "yokogumi" "yokogumi horizontal"
    "fraction" "fraction"
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
  (into (sourced node [:note {:type (get node "note_type")}])
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

(defn- normalize-layout-siblings [children]
  ;; TEI permits prose before divisions, but later prose must belong to a
  ;; division. Neutral wrappers keep those later paragraphs outside the
  ;; source's indented division without inventing an embedded text.
  (let [[before remaining] (split-with #(not= :div (first %)) children)]
    (into (vec before)
          (mapcat (fn [run]
                    (if (= :div (ffirst run)) run [(into [:div] run)])))
          (partition-by #(= :div (first %)) remaining))))

(defn- flush-division [acc]
  (if (seq (:current-division acc))
    (-> acc
        (update :body-children conj (into [:div] (normalize-layout-siblings (:current-division acc))))
        (assoc :current-division []))
    acc))

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
        span (or (get node "source_span") (get node "span"))]
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
   (let [content (whitespace/source-text->tei-inline (get node "text"))]
     (if (and (seq content) (::source-reference node))
       (append-inline acc (into (sourced node [:seg]) content))
       (update acc :current-paragraph into content)))))

(defn- render-ruby-node
  ([acc node depth]
   (let [ruby (get node "ruby")]
     (if (and (present-ruby-text? (get ruby "base"))
              (or (present-ruby-text? (get ruby "reading"))
                  (seq (get node "reading_children"))))
       (let [attrs (cond-> {:type "furigana"}
                     (get ruby "direction")
                     (assoc :rend (get ruby "direction")))
             before (:current-paragraph acc)
             base (if (seq (get node "inline_children"))
                    (render-inline-children (assoc acc :current-paragraph [])
                                            (get node "inline_children") depth)
                    (assoc acc :current-paragraph [(get ruby "base")]))
             reading (if (seq (get node "reading_children"))
                       (render-inline-children (assoc base :current-paragraph [])
                                               (get node "reading_children") depth)
                       (assoc base :current-paragraph [(get ruby "reading")]))]
         (append-inline (assoc reading :current-paragraph before)
                        (sourced node [:ruby attrs
                                       (into [:rb] (:current-paragraph base))
                                       (into [:rt] (:current-paragraph reading))])))
       (mark-omitted acc "ruby")))))

(defn- render-gaiji-node
  ([acc node _depth]
   (let [identity (gaiji-declaration node)
         base-id (:xml-id identity)
         id (or (get (:gaiji-identities acc) identity)
                (if-not (contains? (:char-declaration-ids acc) base-id)
                  base-id
                  (loop [ordinal 0]
                    (let [candidate (str base-id "-at-" (or (get-in node ["source_span" "start"])
                                                            (get-in node ["span" "start"]) 0)
                                         (when (pos? ordinal) (str "-" ordinal)))]
                      (if (contains? (:char-declaration-ids acc) candidate)
                        (recur (inc ordinal)) candidate)))))
         declaration (assoc identity :xml-id id)]
     (-> acc
         (assoc-in [:gaiji-identities identity] id)
         (register-char-declaration declaration)
         (append-inline (sourced node (cond-> [:g {:ref (str "#" id)}]
                                        (seq (:unicode declaration)) (conj (:unicode declaration)))))))))

(defn- render-editor-note-node [acc node depth]
  (let [before (:current-paragraph acc)
        annotation (when-let [children (get node "annotation_children")]
                     (render-inline-children (assoc acc :current-paragraph []) children depth))
        acc (if annotation (assoc annotation :current-paragraph before) acc)
        element (sourced node
                         (if-let [kind (get node "note_kind")]
                           (into [:note {:type kind}] (if annotation (:current-paragraph annotation) [(get node "text")]))
                           (let [note (get node "note")]
                             [:note (cond-> {:type (get note "category")}
                                      (get note "resolution") (assoc :subtype (get note "resolution")))
                              (get note "raw")])))]
    (if-let [targets (get node "target_source_spans")]
      (let [[acc references] (reduce (fn [[acc references] span]
                                       (let [[acc target] (register-source acc {"source_span" span})]
                                         [acc (conj references (str "#" (::source-reference target)))]))
                                     [acc []] targets)
            element (assoc-in element [1 :target] (string/join " " references))]
        (if-let [closing-span (get node "closing_source_span")]
          (let [[acc closing] (register-source acc {"source_span" closing-span})]
            (append-inline acc (update-in element [1 :source] str " #" (::source-reference closing))))
          (append-inline acc element)))
      (append-inline acc element))))

(defn- render-kunten-node [acc node _depth]
  (let [kind (get node "kunten_kind")]
    (append-inline acc (sourced node [:note {:type "kunten" :subtype kind
                                             :rend (case kind "return-mark" "subscript" "okurigana" "superscript")}
                                      (get node "text")]))))

(defn- render-gap-node [acc node _depth]
  (append-inline acc
                 [:gap (cond-> {:reason (get node "reason")}
                         (get node "quantity") (assoc :quantity (get node "quantity")
                                                      :unit (get node "unit"))
                         (get node "extent") (assoc :extent (get node "extent")))]))

(defn- render-annotated-text-node [acc node depth]
  (let [before (:current-paragraph acc)
        principal (render-inline-children (assoc acc :current-paragraph [])
                                          (get node "inline_children") depth)
        annotation (render-inline-children (assoc principal :current-paragraph [])
                                           (get node "annotation_children") depth)
        kind (get node "note_kind")
        attributes (if (contains? #{"annotation-number" "author-note"} kind)
                     {:type "source-role" :subtype kind}
                     (cond-> {:type kind}
                       (get node "position") (assoc :place (get node "position"))))
        note (into [:note attributes]
                   (:current-paragraph annotation))]
    (append-inline (assoc annotation :current-paragraph before)
                   (sourced node (conj (into [:seg {:type "annotated-text"}]
                                             (:current-paragraph principal)) note)))))

(defn- render-source-realization-node [acc node _depth]
  (let [[acc annotation] (register-source acc {"source_span" (get node "annotation_span")})
        reference (::source-reference annotation)]
    (append-inline acc (sourced node [:choice (cond-> {} reference (assoc :corresp (str "#" reference)))
                                      [:orig (get node "source")]
                                      [:reg (get node "text")]]))))

(defn- render-base-text-variant-node [acc node depth]
  (let [before (:current-paragraph acc)
        rendered (if (seq (get node "inline_children"))
                   (render-inline-children (assoc acc :current-paragraph [])
                                           (get node "inline_children") depth)
                   (assoc acc :current-paragraph [(get node "text")]))
        witness (render-inline-children (assoc rendered :current-paragraph [])
                                        (get-in node ["variant" "base_children"]) depth)]
    (append-inline (assoc witness :current-paragraph before)
                   (sourced node [:app {:type "base-text-variant"}
                                  (into [:lem] (:current-paragraph rendered))
                                  (into [:rdg (cond-> {:type "base-text"}
                                                (= "" (get-in node ["variant" "base_text"]))
                                                (assoc :subtype "omission"))]
                                        (:current-paragraph witness))]))))

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

(defn- render-warichu-node [acc node depth]
  (let [wrapper (sourced node [:seg {:type "warichu" :rend "two-line"}])]
    (if (contains? node "inline_children")
      (render-inline-wrapper acc (get node "inline_children") (get node "text") depth wrapper)
      (let [before (:current-paragraph acc)
            upper (render-inline-wrapper (assoc acc :current-paragraph [])
                                         (get node "upper_children") "" depth [:seg {:type "upper"}])
            lower (render-inline-wrapper upper (get node "lower_children") "" depth [:seg {:type "lower"}])]
        (append-inline (assoc lower :current-paragraph before)
                       (into wrapper (:current-paragraph lower)))))))

(defn- render-emphasis-node
  ([acc node depth]
   (render-inline-wrapper acc
                          (seq (get node "inline_children"))
                          (get node "text")
                          depth
                          (sourced node [:hi {:rend (string/join " " (remove nil? [(get node "style")
                                                                                   (get-in node ["decoration" "kind"])
                                                                                   (get-in node ["decoration" "position"])]))}]))))

(defn- render-layout-span-node
  ([acc node depth]
   (let [layout (get node "layout")
         layouts (layout-attributes layout)
         rends (mapv inline-layout-rend layouts)]
     (if (every? some? rends)
       (let [rend (string/join " " rends)
             params (if (vector? layout)
                      (not-empty (string/join ";" (mapcat (fn [attribute]
                                                            (when-let [params (layout-params attribute)]
                                                              (map #(str (get attribute "kind") "." %)
                                                                   (string/split params #";"))))
                                                          layouts)))
                      (layout-params layout))]
         (render-inline-wrapper acc
                                (seq (get node "inline_children"))
                                (get node "text")
                                depth
                                (sourced node [:hi (cond-> {:rend rend}
                                                     (seq layouts)
                                                     (assoc :abc/layout-kind (string/join " " (map #(get % "kind") layouts)))

                                                     params
                                                     (assoc :abc/layout-params params))])))
       (mark-omitted acc "layout-span")))))

(defn- heading-attrs [node]
  (cond-> {:n (str (get node "level"))}
    (some? (get node "indent"))
    (assoc :style (str "padding-inline-start: " (get node "indent") "em"))))

(defn- render-heading-node
  ([acc node depth]
   (if (#{"dogyo" "mado"} (get node "style"))
     (render-inline-wrapper acc
                            (seq (get node "inline_children"))
                            (get node "text")
                            depth
                            (sourced node [:seg (assoc (heading-attrs node)
                                                       :type "heading"
                                                       :rend (get node "style"))]))
     (let [children (seq (get node "inline_children"))
           base (-> acc flush-paragraph flush-division)]
       (if children
         (let [scratch (assoc base :current-paragraph [])
               rendered (render-inline-children scratch children depth)
               head-fragment (:current-paragraph rendered)]
           (-> rendered
               (assoc :current-paragraph [])
               (update :current-division conj
                       (into (sourced node [:head (heading-attrs node)])
                             head-fragment))))
         (update base :current-division conj
                 (sourced node [:head (heading-attrs node)
                                (get node "text")])))))))

(defn- render-indentation-node
  ([acc node _depth]
   (if (present-text? (get node "text"))
     (append-inline acc
                    (sourced node [:seg {:type "indentation"
                                         :n (str (get node "depth"))}
                                   (get node "text")]))
     (mark-omitted acc "indentation"))))

(defn- render-page-break-node
  ([acc node _depth]
   (append-inline acc
                  (sourced node (cond-> [(if (= "column-break" (get node "type")) :cb :pb) {}]
                                  (#{"kaicho" "kaimihiraki"} (get node "marker"))
                                  (assoc-in [1 :rend] (get node "marker"))
                                  (some? (get node "page_number"))
                                  (assoc-in [1 :n] (get node "page_number")))))))

(defn- render-line-break-node
  ([acc node _depth]
   (append-inline acc (sourced node [:lb]))))

(defn- render-image-node [acc node depth]
  (let [before (:current-paragraph acc)
        rendered (render-inline-children (assoc acc :current-paragraph [])
                                         (get node "caption_reference_children") depth)
        description (render-inline-children (assoc rendered :current-paragraph [])
                                            (get node "description_children") depth)
        annotations (render-inline-children (assoc description :current-paragraph [])
                                            (get node "annotation_children") depth)
        graphic (cond-> {:url (get node "src")}
                  (some? (get node "width")) (assoc :width (str (get node "width") "px"))
                  (some? (get node "height")) (assoc :height (str (get node "height") "px")))
        figure (cond-> [:figure (cond-> {} (get node "number") (assoc :n (get node "number")))
                        [:graphic graphic]]
                 (and (present-text? (get node "alt")) (empty? (:current-paragraph description))) (conj [:figDesc (get node "alt")])
                 (seq (:current-paragraph description)) (conj (into [:note {:type "image-description"}] (:current-paragraph description)))
                 (seq (:current-paragraph rendered)) (conj (into [:note {:type "caption-reference"}] (:current-paragraph rendered)))
                 (get node "description_source") (conj [:note {:type "uninterpreted-image-description"} (get node "description_source")])
                 (get node "caption_source") (conj [:note {:type "uninterpreted-caption-reference"} (get node "caption_source")])
                 (and (get node "dimensions_source") (nil? (get node "width")))
                 (conj [:note {:type "image-dimensions"} (get node "dimensions_source")]))]
    (append-inline (assoc annotations :current-paragraph before)
                   (sourced node (into figure (:current-paragraph annotations))))))

(defn- render-caption-node
  ([acc node depth]
   (render-inline-wrapper acc (get node "inline_children") (get node "text") depth
                          (sourced node [:seg {:type "caption"}]))))

(defn- render-quote-node
  ([acc node _depth]
   (if (present-text? (get node "text"))
     (append-inline acc (sourced node [:quote (get node "text")]))
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
   "base-text-variant" render-base-text-variant-node
   "annotated-text" render-annotated-text-node
   "gap" render-gap-node
   "emphasis" render-emphasis-node
   "layout-span" render-layout-span-node
   "heading" render-heading-node
   "indentation" render-indentation-node
   "page-break" render-page-break-node
   "column-break" render-page-break-node
   "line-break" render-line-break-node
   "image" render-image-node
   "caption" render-caption-node
   "quote" render-quote-node
   "source-note" render-source-note-node
   "kunten" render-kunten-node
   "iteration-mark" render-source-realization-node
   "supplied-diacritic" render-source-realization-node
   "warichu" render-warichu-node})

(defn- render-node
  ([acc node] (render-node acc node 0))
  ([acc node depth]
   (let [node-type (get node "type")
         [acc node] (register-source acc node)]
     (if-let [render-node-fn (get node-renderers node-type)]
       (render-node-fn acc node depth)
       (throw (ex-info "Unsupported TEI parser-IR node type"
                       {:node-type node-type}))))))

(defn- initial-acc [primary-text-hash]
  {:body-children []
   :current-division []
   :current-paragraph []
   :current-paragraph-attrs nil
   :front-notes []
   :back-notes []
   :source-spans (sorted-map)
   :primary-text-hash primary-text-hash
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
                   flush-division)
        result (update result :back-notes into
                       (map (fn [[id span]]
                              [:note (cond-> {:type "source-span" :xml/id id}
                                       (:primary-text-hash result)
                                       (assoc :corresp (str "urn:" (:primary-text-hash result))))
                               (record-json/write-deterministic-json-str span)])
                            (:source-spans result)))]
    {:body (tei-text result)
     :char_declarations (:char_declarations result)
     :node_counts (:node_counts result)
     :omitted (:omitted result)}))

(defn- paragraph-range [paragraph]
  (get paragraph "node_range"))

(defn- validate-paragraph-ranges! [nodes paragraphs]
  (loop [remaining paragraphs
         prior-end (Long/valueOf 0)]
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

(defn- paragraph-render-inputs [nodes paragraph hanging-offset]
  (let [{start "start" end "end"} (paragraph-range paragraph)
        first-index (first (drop-while
                            (fn [index]
                              (let [node (nth nodes index)]
                                (or (= "editor-note" (get node "type"))
                                    (and (= "text" (get node "type"))
                                         (empty? (get node "text"))))))
                            (range start end)))
        first-node (when first-index (nth nodes first-index))
        indent (if (and (= "body" (get paragraph "role"))
                        (= "text" (get first-node "type")))
                 (leading-indent (get first-node "text"))
                 0)]
    [(if (pos? indent)
       (update-in nodes [first-index "text"] subs indent)
       nodes)
     (cond-> (paragraph-attrs paragraph)
       (pos? indent) (assoc :style (str "text-indent: " (+ hanging-offset indent) "em"))
       (pos? indent) (update :rend #(string/join " " (remove string/blank? [% (str "first-line-indent(" indent ")")]))))]))

(defn- layout-block-attrs [block]
  (let [indent (get block "indent")
        placement (get block "relative_placement")
        anchor (get placement "anchor_span")
        attributes (layout-attributes (get block "typography"))
        frame-style (some (fn [attribute]
                            (when (= "keigakomi" (get attribute "kind"))
                              (case (get attribute "border")
                                ("rule" "box" "circle") "border-style: solid"
                                "dashed-rule" "border-style: dashed"
                                "dotted-circle" "border-style: dotted"
                                "double-rule" "border-style: double"
                                nil))) attributes)
        continuation (get block "continuation_indent")
        start-padding (or continuation indent)
        styles (cond-> []
                 (some? start-padding) (conj (str "padding-inline-start: " start-padding "em"))
                 (and (some? indent) (some? continuation)) (conj (str "text-indent: " (- indent continuation) "em"))
                 (contains? block "offset_from_end") (conj (str "padding-inline-end: " (get block "offset_from_end") "em"))
                 (contains? block "width") (conj (str "inline-size: " (get block "width") "em"))
                 (get block "column_count") (conj (str "column-count: " (get block "column_count")))
                 (get block "direction") (conj (str "writing-mode: " (case (get block "direction")
                                                                       "horizontal" "horizontal-tb"
                                                                       "vertical" "vertical-rl")))
                 (get block "align") (conj (str "text-align: " (get block "align")))
                 frame-style (conj frame-style))
        rend (cond-> []
               (get block "placement") (conj (get block "placement"))
               placement (conj "placement-below" (str "anchor-kind(" (get placement "anchor_kind") ")"))
               (contains? placement "offset_chars") (conj (str "anchor-offset-chars(" (get placement "offset_chars") ")"))
               (get block "column_rule") (conj "column-rule")
               (get block "typography") (into (keep inline-layout-rend) attributes)
               (get block "page_placement") (conj "page-horizontal-center")
               (get block "line_count") (conj (str "line-count(" (get block "line_count") ")")))]
    (cond-> {:type (get block "role" "layout")}
      (get block "source_kind") (assoc :subtype (get block "source_kind"))
      anchor (assoc :corresp (str "#" (source-reference {"source_span" anchor})))
      (source-reference block) (assoc :source (str "#" (source-reference block)))
      (seq styles) (assoc :style (string/join "; " styles))
      (seq rend) (assoc :rend (string/join " " rend))
      (= "warichu" (get block "role")) (assoc :rend "two-line"))))

(defn- wrap-scope-intersections [content scopes]
  (reduce (fn [children scope]
            [(into [(if (get scope "typography") :hi :seg)
                    (cond-> (layout-block-attrs scope)
                      (get scope "typography") (dissoc :type))]
                   children)])
          content (reverse scopes)))

(defn- render-paragraph-intersections [acc nodes start end scopes]
  (let [boundaries (into (sorted-set start end)
                         (mapcat (fn [scope]
                                   [(max start (get-in scope ["node_range" "start"]))
                                    (min end (get-in scope ["node_range" "end"]))])) scopes)]
    (reduce (fn [acc [from to]]
              (let [active (filterv #(<= (get-in % ["node_range" "start"]) from
                                         (dec to) (dec (get-in % ["node_range" "end"]))) scopes)
                    prefix (:current-paragraph acc)
                    rendered (render-node-seq (assoc acc :current-paragraph []) (subvec nodes from to))]
                (assoc rendered :current-paragraph
                       (into prefix (wrap-scope-intersections (:current-paragraph rendered) active)))))
            acc (partition 2 1 boundaries))))

(defn- render-paragraph-row [nodes acc paragraph scopes frames]
  (let [hanging-offset (or (some (fn [{:keys [block]}]
                                   (when (and (contains? block "indent") (contains? block "continuation_indent"))
                                     (- (get block "indent") (get block "continuation_indent"))))
                                 (rseq frames)) 0)
        [nodes attrs] (paragraph-render-inputs nodes paragraph hanging-offset)
        {start "start" end "end"} (paragraph-range paragraph)
        node-slice (subvec nodes start end)]
    (case (get paragraph "role")
      "body" (-> acc
                 (assoc :current-paragraph-attrs attrs)
                 (render-paragraph-intersections nodes start end scopes)
                 flush-paragraph)
      "source-note" (-> acc
                        flush-paragraph
                        (render-node-seq node-slice)
                        flush-paragraph)
      (-> acc
          (assoc :current-paragraph-attrs attrs)
          (render-node-seq node-slice)
          flush-paragraph))))

(defn- paragraph-boundary-predicate [paragraphs]
  (let [ends (into (sorted-map)
                   (map (fn [p] [(get-in p ["node_range" "start"]) (get-in p ["node_range" "end"])]))
                   paragraphs)]
    (fn [index]
      (let [[start end] (first (rsubseq ends <= index))]
        (not (and start (< start index end)))))))

(defn- validate-layout-blocks! [nodes paragraphs blocks]
  (let [boundary? (paragraph-boundary-predicate paragraphs)
        external-notes (into (sorted-set)
                             (keep-indexed (fn [index node]
                                             (when (and (= "source-note" (get node "type"))
                                                        (not= "body" (get node "placement"))) index))) nodes)]
    (doseq [block blocks]
      (let [{start "start" end "end"} (get block "node_range")]
        (when-not (and (integer? start) (integer? end) (<= 0 start) (< start end)
                       (<= end (count nodes))
                       (or (and (boundary? start) (boundary? end))
                           (and (or (get block "typography") (get block "role")
                                    (some #(contains? block %) ["indent" "continuation_indent" "offset_from_end" "width" "line_count" "column_count" "align" "direction" "border"]))
                                (source-reference block)))
                       (empty? (subseq external-notes >= start < end)))
          (throw (ex-info "Invalid layout block node range" {:block block}))))))
  (reduce (fn [stack block]
            (let [{start "start" end "end"} (get block "node_range")
                  stack (loop [stack stack]
                          (if (and (seq stack) (<= (get-in (peek stack) ["node_range" "end"]) start))
                            (recur (pop stack)) stack))]
              (when (and (seq stack) (> end (get-in (peek stack) ["node_range" "end"])))
                (throw (ex-info "Crossing layout block ranges" {:blocks [(peek stack) block]})))
              (conj stack block)))
          [] (sort-by (juxt #(get-in % ["node_range" "start"])
                            #(- (get-in % ["node_range" "end"]))) blocks)))

(defn- scope-intersections [nodes paragraphs scopes]
  (let [runs (into (sorted-map)
                   (concat (map (fn [p] [(get-in p ["node_range" "start"]) (get-in p ["node_range" "end"])]) paragraphs)
                           (keep-indexed (fn [index node] (when (and (= "heading" (get node "type")) (= "normal" (get node "style"))) [index (inc index)])) nodes)))]
    (reduce (fn [result scope]
              (let [{start "start" end "end"} (get scope "node_range")
                    first-start (or (ffirst (rsubseq runs <= start)) start)]
                (reduce (fn [result [from to]]
                          (if (> to start) (update result from (fnil conj []) scope) result))
                        result (take-while (fn [[from _]] (< from end)) (subseq runs >= first-start)))))
            {} (sort-by (juxt #(get-in % ["node_range" "start"]) #(- (get-in % ["node_range" "end"]))) scopes))))

(defn- close-layout-blocks [acc frames node-end]
  (loop [acc acc frames frames]
    (if-let [{:keys [block parent-content]} (peek frames)]
      (if (= node-end (get-in block ["node_range" "end"]))
        (let [acc (-> acc flush-paragraph flush-division)
              content (normalize-layout-siblings (:body-children acc))
              wrapped (into [:div (layout-block-attrs block)] content)]
          (recur (append-structural-child (merge acc parent-content) wrapped) (pop frames)))
        [acc frames])
      [acc frames])))

(defn- render-with-paragraphs [nodes paragraphs layout-blocks primary-text-hash]
  (validate-paragraph-ranges! nodes paragraphs)
  (validate-layout-blocks! nodes paragraphs layout-blocks)
  (let [boundary? (paragraph-boundary-predicate paragraphs)
        grouped (group-by #(and (boundary? (get-in % ["node_range" "start"]))
                                (boundary? (get-in % ["node_range" "end"]))) layout-blocks)
        intersections (scope-intersections nodes paragraphs (get grouped false))
        starts (group-by #(get-in % ["node_range" "start"]) (reverse (get grouped true)))
        paragraph-starts (into {} (keep (fn [paragraph]
                                          (let [{start "start" end "end"} (paragraph-range paragraph)]
                                            (when (< start end) [start paragraph])))) paragraphs)]
    (loop [acc (reduce (fn [acc block]
                         (reduce (fn [acc span]
                                   (if-let [reference (source-reference {"source_span" span})]
                                     (assoc-in acc [:source-spans reference] span) acc))
                                 acc [(get block "source_span") (get-in block ["relative_placement" "anchor_span"])]))
                       (initial-acc primary-text-hash) layout-blocks)
           index (Long/valueOf 0) frames []]
      (let [[acc frames] (close-layout-blocks acc frames index)]
        (if (= index (count nodes))
          (finalize-result (cond-> (-> acc flush-paragraph flush-division)
                             (seq layout-blocks) (update :body-children normalize-layout-siblings)))
          (let [[acc frames] (reduce
                              (fn [[acc frames] block]
                                (let [acc (flush-paragraph acc)]
                                  [(assoc acc :body-children [] :current-division [])
                                   (conj frames {:block block :parent-content
                                                 (select-keys acc [:body-children :current-division])})]))
                              [acc frames]
                              (sort-by #(get-in % ["node_range" "end"]) > (get starts index)))
                paragraph (get paragraph-starts index)]
            (if paragraph
              (recur (render-paragraph-row nodes (flush-paragraph acc) paragraph (get intersections index) frames)
                     (get-in paragraph ["node_range" "end"]) frames)
              (let [rendered (render-node-seq acc [(nth nodes index)])
                    scopes (get intersections index)
                    rendered (if (and (seq scopes) (= "heading" (get (nth nodes index) "type")))
                               (update-in rendered [:current-division 0]
                                          (fn [head] (into (subvec head 0 2) (wrap-scope-intersections (subvec head 2) scopes))))
                               rendered)]
                (recur rendered (inc index) frames)))))))))

(defn- render-flat [nodes primary-text-hash]
  (finalize-result
   (render-node-seq (initial-acc primary-text-hash) nodes)))

(defn render [parser-ir]
  (let [nodes (vec (get parser-ir "nodes"))
        paragraphs (seq (get parser-ir "paragraphs"))]
    (if (or paragraphs (seq (get parser-ir "layout_blocks")))
      (render-with-paragraphs nodes (vec paragraphs) (get parser-ir "layout_blocks" [])
                              (get-in parser-ir ["source" "primary_text_hash"]))
      (render-flat nodes (get-in parser-ir ["source" "primary_text_hash"])))))
