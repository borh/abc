(ns soranoha.za.reading
  "Published TEI to readable hiccup, for the serving layer's reading view.

  A projection, never a replacement. The TEI artifact is what the release
  signs and what a citation names; this renders it so the encoding is legible
  to a reader who does not read TEI source. Every reading page names the
  release it came from and links the artifact itself, so a reader who doubts
  the rendering can check it against the bytes.

  Why this lives in the serving layer rather than in the published bytes: an
  `<?xml-stylesheet?>` processing instruction would change every TEI
  artifact's bytes, and so would move presentation into the signed record,
  into the conformance vectors and into genesis. Presentation is exactly the
  thing that will need correcting after publication, and a rendering that
  lives here can be corrected without republishing anything.

  What the reading shows is the same text the plaintext projection publishes:
  one lemma from an apparatus, the first supported branch of a choice, the
  same order. `soranoha.annotations.view/selected-children` is the authority
  for that policy and this namespace mirrors it. Where the two differ they
  differ deliberately: `view` drops ruby readings and notes because analysis
  wants base text, and a reader wants both, so they are shown rather than
  removed.

  Where the encoding records something HTML cannot carry, the fact becomes a
  class or a title rather than disappearing: an apparatus's rejected reading,
  a gaiji's original Aozora marker, the Japanese name of an emphasis mark
  this stylesheet renders only approximately."
  (:require [clojure.string :as string]
            [soranoha.annotations.view :as view])
  (:import (org.w3c.dom Document Element Node)))

(def ^:private snh-namespace "https://w3id.org/soranoha/ns/tei")

(def ^:private indented-containers
  "Containers `hiccup->pretty-xml-string` indents. Their whitespace children
  are layout the emitter added, not text the source had."
  #{"TEI" "text" "front" "body" "back" "div" "charDecl" "char"})

(def ^:private audit-note-types
  "Notes that record what the parser established rather than what the source
  says. They belong to the validation report a reader can fetch, and putting
  a JSON diagnostic in the middle of a story would be worse than useless."
  #{"interpretation-problem" "parser-diagnostic" "source-span"
    "parser-completion" "source-decoding"})

(def ^:private choice-preference
  ;; the same order as soranoha.annotations.view/selected-children, so the
  ;; reading and the published plaintext cannot select different branches
  ["corr" "reg" "expan" "sic" "orig" "abbr"])

(def ^:private decoration-classes
  "Aozora's emphasis-mark names, as they reach `@rend` from the parser's
  `decoration.kind`, mapped onto ASCII class names. The source keyword is the
  identifier; this table only says which of them this stylesheet can draw and
  which mark it draws for each. A name absent here still reaches the reader,
  as the element's title."
  {"傍点" "dots"
   "白ゴマ傍点" "dots-open"
   "丸傍点" "dots-circle"
   "白丸傍点" "dots-circle-open"
   "二重丸傍点" "dots-double-circle"
   "蛇の目傍点" "dots-double-circle"
   "ばつ傍点" "dots-cross"
   "白三角傍点" "dots-triangle-open"
   "黒三角傍点" "dots-triangle"
   "傍線" "line"
   "二重傍線" "line-double"
   "波線" "line-wavy"
   "鎖線" "line-dashed"
   "破線" "line-dashed"})

(def ^:private font-sizes
  "`snh:layout-params` for a font-size scope onto one CSS length. Levels are
  clamped: Aozora records `［＃５段階大きな文字］` and a reader gains nothing
  from a font that leaves the page."
  {"absolute" {"extra-large" "1.7em" "large" "1.35em" "medium" "1em" "small" ".85em"}
   "qualitative" {"larger" "1.3em" "smaller" ".8em"}
   "large" ["1.15em" "1.3em" "1.5em" "1.7em" "1.9em"]
   "small" [".9em" ".8em" ".7em" ".62em" ".55em"]})

(def ^:private safe-declaration
  "The TEI emitter writes `@style` for the source-derived geometry it cannot
  express in TEI attributes — indents, measures, column counts, alignment.
  Every value in it comes from the parser's own bounded vocabulary, but this
  rendering re-checks the shape rather than trusting the check upstream: a
  declaration that does not match is dropped, not passed through."
  #"[a-z-]+: -?[0-9]+(?:\.[0-9]+)?(?:em|px)?|[a-z-]+: [a-z][a-z-]*")

(def ^:private ignored-properties
  ;; `writing-mode` states a run's direction relative to the source's vertical
  ;; setting, and this view is horizontal until the reader says otherwise. The
  ;; layout-kind classes carry the same fact in a form the toggle can honour.
  #{"writing-mode"})

;; --------------------------------------------------------------- reading

(defn- attr [^Element element ^String name]
  (let [value (.getAttribute element name)]
    (when-not (string/blank? value) value)))

(defn- snh-attr [^Element element ^String name]
  (let [value (.getAttributeNS element snh-namespace name)]
    (when-not (string/blank? value) value)))

(defn- ascii-token? [token]
  (and (seq token) (nil? (re-find #"[^0-9A-Za-z-]" token))))

(defn- rend-tokens
  "`@rend` is a space-separated list whose tokens may carry parenthesised
  arguments, as in `jisage indent(2)`. The arguments are read from
  `snh:layout-params` instead, which states them as data; only the bare names
  become classes, and a name this stylesheet cannot draw becomes nothing."
  [^Element element]
  (->> (string/split (or (attr element "rend") "") #"\s+")
       (keep (fn [token]
               (let [bare (string/replace token #"\(.*\)$" "")]
                 (cond
                   (ascii-token? bare) bare
                   (decoration-classes bare) (decoration-classes bare)))))
       distinct))

(defn- layout-kinds [^Element element]
  (into #{} (remove string/blank?)
        (string/split (or (snh-attr element "layout-kind") "") #"\s+")))

(defn- layout-params
  "`snh:layout-params` as a map. A span carrying several layout scopes at
  once prefixes each key with its kind, so both spellings are accepted and
  the bare key is what the geometry rules look up."
  [^Element element]
  (into {}
        (keep (fn [pair]
                (let [[k v] (string/split pair #"=" 2)]
                  (when (and k v)
                    [(string/replace k #"^[a-z-]+\." "") v]))))
        (string/split (or (snh-attr element "layout-params") "") #";")))

(defn- em [value]
  (when (re-matches #"[0-9]{1,3}" (str value)) (str value "em")))

(defn- level [value]
  (when-let [n (re-matches #"[0-9]{1,2}" (str value))]
    (min 4 (max 0 (dec (parse-long n))))))

(defn- font-size [params]
  (let [size-type (get params "size-type")]
    (case size-type
      "absolute" (get-in font-sizes ["absolute" (get params "size")])
      "qualitative" (get-in font-sizes ["qualitative" (get params "direction")])
      ("large" "small") (some->> (level (get params "level"))
                                 (nth (get font-sizes size-type)))
      nil)))

(defn- geometry
  "The layout facts that are geometry rather than appearance, as inline CSS.
  Indents and measures are recorded in source characters, and a character is
  one `em` in either writing direction — the one place this rendering can be
  exact rather than approximate, which is why it is computed here instead of
  guessed at in a stylesheet."
  [^Element element]
  (let [kinds (layout-kinds element)
        params (layout-params element)
        indent (em (get params "indent"))
        declarations
        (cond-> []
          (and (some kinds ["jisage" "line-jisage"]) indent)
          (conj (str "padding-inline-start:" indent))

          (and (kinds "burasage") (em (get params "first-line-indent")))
          (conj (str "text-indent:" (em (get params "first-line-indent"))))

          (and (kinds "burasage") (em (get params "continuation-indent")))
          (conj (str "padding-inline-start:" (em (get params "continuation-indent"))))

          (and (kinds "jizume") (em (get params "width")))
          (conj (str "inline-size:" (em (get params "width"))))

          (and (kinds "chitsuki") (em (get params "offset-from-end")))
          (conj (str "padding-inline-end:" (em (get params "offset-from-end"))))

          (and (kinds "font-size") (font-size params))
          (conj (str "font-size:" (font-size params))))
        own (->> (string/split (or (attr element "style") "") #";")
                 (map string/trim)
                 (filter #(re-matches safe-declaration %))
                 (remove #(contains? ignored-properties
                                     (first (string/split % #":" 2)))))]
    (not-empty (string/join ";" (concat declarations own)))))

(defn- classes [^Element element extra]
  (not-empty
   (string/join
    " "
    (distinct
     (concat extra
             (map #(str "rend-" %) (rend-tokens element))
             (map #(str "layout-" %) (sort (layout-kinds element)))
             (when-let [type (attr element "type")]
               (when (ascii-token? type) [(str "type-" type)])))))))

(defn- element-attrs
  ([element extra] (element-attrs element extra nil))
  ([element extra title]
   (let [class (classes element extra)
         style (geometry element)]
     (cond-> {}
       class (assoc :class class)
       style (assoc :style style)
       (not (string/blank? title)) (assoc :title title)))))

(defn- text-content
  "Reading text of a subtree, for the places where HTML offers only an
  attribute to put it in: an apparatus's rejected witness, a gaiji's name."
  [^Node node]
  (string/join (map (fn [^Node child]
                      (case (.getNodeType child)
                        3 (.getNodeValue child)
                        1 (text-content child)
                        ""))
                    (view/children node))))

(defn- child-named [node name]
  (first (filter #(= name (view/local-name %)) (view/children node))))

(defn- required-child
  "The child an element's reading depends on. `soranoha.annotations.view`
  refuses a document without it and the profile forbids one, so reaching this
  means the published TEI is not what the pipeline says it is — which is worth
  a named failure rather than a null pointer somewhere downstream."
  [element names]
  (or (some #(child-named element %) names)
      (throw (ex-info "TEI element has no readable child"
                      {:reason :unreadable-tei-element
                       :element (view/local-name element)
                       :expected (vec names)}))))

(defn- unrendered-rend
  "The `@rend` tokens no class was made for, so an emphasis mark this
  stylesheet cannot draw still reaches the reader by name."
  [^Element element]
  (->> (string/split (or (attr element "rend") "") #"\s+")
       (remove string/blank?)
       (remove #(let [bare (string/replace % #"\(.*\)$" "")]
                  (or (ascii-token? bare) (decoration-classes bare))))
       (string/join " ")))

(defn- char-declarations
  "The header's gaiji declarations, as xml:id -> {:unicode :marker :desc}."
  [^Document document]
  (let [chars (.getElementsByTagNameNS document view/tei-namespace "char")]
    (into {}
          (map (fn [index]
                 (let [^Element c (.item chars index)]
                   [(.getAttributeNS c view/xml-namespace "id")
                    {:unicode (some-> (child-named c "mapping") text-content not-empty)
                     :marker (some (fn [^Element part]
                                     (when (and (= "localProp" (view/local-name part))
                                                (= "rawMarker" (.getAttribute part "name")))
                                       (not-empty (.getAttribute part "value"))))
                                   (view/children c))
                     :desc (some-> (child-named c "desc") text-content not-empty)}])))
          (range (.getLength chars)))))

(declare render-nodes)

(defn- gaiji
  "A `<g>` shows its mapped character where the encoding found one, and its
  Aozora marker either way. The marker is what identifies the character in
  the source, so it stays available even when a substitute is displayed."
  [declarations ^Element element]
  (let [id (string/replace (or (attr element "ref") "") #"^#" "")
        {:keys [unicode marker desc]} (get declarations id)
        shown (or (not-empty (text-content element)) unicode)
        label (string/join " " (remove string/blank? [marker desc]))]
    [:span (cond-> {:class (if shown "gaiji" "gaiji gaiji-unmapped")}
             (seq label) (assoc :title label))
     (or shown (str "〔" (if (seq label) label "外字") "〕"))]))

(defn- render-element [declarations ^Element element]
  (let [tag (view/local-name element)
        kids #(render-nodes declarations (view/children element))]
    (case tag
      "div" (into [:section (element-attrs element ["div"])] (kids))
      ;; the page's own h1 is the work's title, so a source heading starts at
      ;; h2 and its recorded level places it under that
      "head" (into [(case (attr element "n") "1" :h2 "2" :h3 :h4)
                    (element-attrs element ["head"])]
                   (kids))
      "p" (into [:p (element-attrs element nil)] (kids))
      "lb" [:br]
      ("pb" "cb") [:span (element-attrs element ["pb"] (attr element "n"))]
      "gap" [:span (element-attrs element ["gap"] (attr element "reason")) "〔欠〕"]
      "quote" (into [:blockquote (element-attrs element ["quote"])] (kids))
      "g" (gaiji declarations element)

      ("hi" "seg")
      (into [:span (element-attrs element [tag] (unrendered-rend element))] (kids))

      "ruby"
      ;; the acceptance criterion for this view: a reading above its base
      ;; characters, not beside them in brackets
      (if-let [base (child-named element "rb")]
        [:ruby (element-attrs element ["ruby"])
         (render-nodes declarations (view/children base))
         [:rt (some->> (child-named element "rt")
                       view/children
                       (render-nodes declarations))]]
        (into [:span (element-attrs element ["ruby"])] (kids)))

      "app"
      ;; one lemma is the reading, exactly as the plaintext projection has
      ;; it; the witness the editor rejected is kept as the element's title
      (let [rdgs (filter #(= "rdg" (view/local-name %)) (view/children element))]
        [:span (element-attrs element ["app"]
                              (when (seq rdgs)
                                (str "底本: " (string/join " / " (map text-content rdgs)))))
         (render-nodes declarations (view/children (required-child element ["lem"])))])

      "choice"
      (let [parts (view/children element)
            chosen (required-child element choice-preference)
            others (remove #(identical? chosen %) parts)]
        [:span (element-attrs element ["choice"]
                              (string/join " / " (remove string/blank?
                                                         (map text-content others))))
         (render-nodes declarations (view/children chosen))])

      "figure"
      ;; the image itself is not published — Aozora's illustrations are not
      ;; part of the grant — so the figure is a placeholder with whatever
      ;; the source said about it
      (into [:figure (element-attrs element ["figure"])
             [:span {:class "figure-mark"} "〔図〕"]]
            (kids))

      "figDesc" (into [:figcaption (element-attrs element ["fig-desc"])] (kids))
      "graphic" [:span {:class "graphic-url"} (or (attr element "url") "")]

      "note"
      (when-not (contains? audit-note-types (attr element "type"))
        (case (attr element "rend")
          "subscript" (into [:sub (element-attrs element ["note"])] (kids))
          "superscript" (into [:sup (element-attrs element ["note"])] (kids))
          (into [:span (element-attrs element ["note"] (attr element "type"))] (kids))))

      "ref" (into [:a (cond-> (element-attrs element ["ref"])
                        (attr element "target") (assoc :href (attr element "target")))]
                  (kids))

      ;; the profile can admit an element before this stylesheet has an
      ;; opinion about it; showing its text unstyled is wrong in a way a
      ;; reader can see, and dropping it is wrong in a way they cannot
      (into [:span (element-attrs element [(str "tei-" tag)])] (kids)))))

(defn- render-nodes [declarations nodes]
  (keep (fn [^Node node]
          (condp = (.getNodeType node)
            Node/ELEMENT_NODE (render-element declarations node)
            Node/TEXT_NODE (let [text (.getNodeValue node)]
                             (when-not (and (string/blank? text)
                                            (contains? indented-containers
                                                       (view/local-name (.getParentNode node))))
                               text))
            nil))
        nodes))

(defn render
  "Published TEI to `{:front :body :back}`, each a vector of hiccup nodes.

  `:body` is the work. `:front` and `:back` are what the source itself says
  around it — Aozora's notes on the text, and the colophon naming the printed
  edition the transcription came from. The colophon is the one piece of
  bibliographic evidence a reader cannot reconstruct from the catalog, so it
  is rendered rather than dropped; the parser's own audit notes are dropped,
  because they are published in full as the work's validation report."
  [^String tei]
  (let [document (view/read-document tei)
        declarations (char-declarations document)
        text (child-named (.getDocumentElement document) "text")
        section (fn [name]
                  (when-let [element (child-named text name)]
                    (not-empty (vec (render-nodes declarations (view/children element))))))]
    {:front (section "front")
     :body (or (section "body") [])
     :back (section "back")}))
