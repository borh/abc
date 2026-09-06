(ns soranoha.ported.tei-header
  "Pure metadata-record → TEI <teiHeader> hiccup builder + XML emitter.

  build returns nested-vector data; emit-xml serialises to a string.
  The data shape is the API: any consumer can serialise the same
  hiccup tree without re-deriving the TEI mapping. No xml/alias-uri
  global side effect; the adapter wraps all unprefixed tag keywords
  in the TEI namespace and translates :xml/lang / :xml/id to the
  XML-namespaced attribute qnames clojure.data.xml expects.

  Hiccup contract (the protocol the data shape conveys):

    [:teiHeader
      [:fileDesc
        [:titleStmt
          [:title {:type \"main\" :xml/lang \"ja\"} \"...\"]
          [:title {:type \"reading\" :xml/lang \"ja-Hira\"} \"...\"]
          [:author <persName>+ <idno>]
          [:respStmt [:resp \"...\"] <persName>+]?]
        [:publicationStmt [:idno {:type \"...\"} \"...\"] [:publisher \"...\"] [:date {:when \"...\"} \"...\"]]
        [:sourceDesc [:bibl ...]+]]
      [:encodingDesc
        [:styleDefDecl {:scheme \"css\"}]
        [:charDecl [:char {:xml/id \"...\"} ...]?]
        [:editorialDecl
          [:normalization {:method \"markup\"} ...]]?]
      [:profileDesc
        [:langUsage [:language {:ident \"ja\"} \"...\"]]
        [:textClass [:classCode {:scheme \"NDC\"} \"...\"]]]]

  Tag keywords with no namespace are TEI elements. :xml/lang and
  :xml/id route to the XML namespace. Strings are text content."
  (:require [clojure.data.xml :as xml]
            [clojure.string :as string]))

(def ^:private tei-ns "http://www.tei-c.org/ns/1.0")
(def ^:private xml-ns "http://www.w3.org/XML/1998/namespace")

;; ---------------------------------------------------------------------------
;; Hiccup builder (pure data, no XML library coupling beyond keyword names)
;; ---------------------------------------------------------------------------

(defn- person-name-block
  "Three <persName> elements per TEI-EAJ: kanji, hiragana, romaji.
  Each carries an <idno type=\"aozora-person-id\"> for the kanji form;
  the reading and romaji forms repeat surname/forename only."
  [person]
  (let [pid (get person "person_id")
        pers-name (fn [lang surname forename & extras]
                    (cond-> [:persName {:xml/lang lang} [:surname surname]]
                      forename (conj [:forename forename])
                      true (into extras)))]
    (cond-> [(pers-name "ja"
                        (get person "family_name")
                        (get person "given_name")
                        [:idno {:type "aozora-person-id"} pid])]
      (get person "family_name_reading")
      (conj (pers-name "ja-Hira"
                       (get person "family_name_reading")
                       (get person "given_name_reading")))
      (get person "family_name_romaji")
      (conj (pers-name "ja-Latn"
                       (get person "family_name_romaji")
                       (get person "given_name_romaji"))))))

(defn- author-block [person]
  (into [:author] (person-name-block person)))

(defn- resp-stmt
  "TEI <respStmt> with the role string and the person's name block."
  [role person]
  (into [:respStmt
         [:resp role]]
        (person-name-block person)))

(defn- title-stmt
  "Build <titleStmt>. `contributors` is a vector of
  {:relation-to-work <role-string> :person <person-body-map>}."
  [work contributors]
  (let [authors (filter #(= "著者" (:relation-to-work %)) contributors)
        others (remove #(= "著者" (:relation-to-work %)) contributors)
        title (get work "title")
        title-r (get work "title_reading")]
    (-> [:titleStmt
         [:title {:type "main" :xml/lang "ja"} title]]
        (cond-> title-r
          (conj [:title {:type "reading" :xml/lang "ja-Hira"} title-r]))
        (into (mapv #(author-block (:person %)) authors))
        (into (mapv #(resp-stmt (:relation-to-work %) (:person %)) others)))))

(defn- publication-stmt [work]
  [:publicationStmt
   [:publisher "ABC"]
   [:idno {:type "aozora-work-id"} (get work "work_id")]
   [:date {:when (get work "aozora_modified")} (get work "aozora_modified")]])

(defn- bibl-edition [edition]
  (cond-> [:bibl
           [:title (get edition "title")]
           [:publisher (get edition "publisher")]]
    (get edition "first_edition_year")
    (conj [:date (get edition "first_edition_year")])
    (get edition "input_edition")
    (conj [:note {:type "input-edition"} (get edition "input_edition")])
    (get edition "proof_edition")
    (conj [:note {:type "proof-edition"} (get edition "proof_edition")])))

(defn- fallback-source-bibl [work]
  (if-let [card-url (get work "card_url")]
    (cond-> [:bibl]
      (get work "title")
      (conj [:title (get work "title")])
      true
      (conj [:idno {:type "aozora-card-url"} card-url]))
    [:p "Aozora Bunko source edition metadata is not available."]))

(defn- source-desc [work]
  (let [editions (get work "source_editions")]
    (if (seq editions)
      (into [:sourceDesc] (mapv bibl-edition editions))
      [:sourceDesc (fallback-source-bibl work)])))

(defn- file-desc [work contributors]
  [:fileDesc
   (title-stmt work contributors)
   (publication-stmt work)
   (source-desc work)])

(defn- declaration->char [declaration]
  (cond-> [:char {:xml/id (:xml-id declaration)}]
    (:unicode declaration)
    (conj [:mapping {:type "unicode"} (:unicode declaration)])

    (:raw-marker declaration)
    (conj [:localProp {:name "rawMarker"
                       :value (:raw-marker declaration)}])

    (:name declaration)
    (conj [:localProp {:name "charName"
                       :value (:name declaration)}])

    (:desc declaration)
    (conj [:desc (:desc declaration)])))

(defn- char-decl [declarations]
  (when (seq declarations)
    (into [:charDecl]
          (map declaration->char declarations))))

(defn- orthographic-normalization-decl []
  [:editorialDecl
   [:normalization {:method "markup"}
    [:p
     (str "Parser-IR sentence elements with "
          "type=\"orthographic-katakana\" mark spans where the "
          "ab-validator orthographic detector identified katakana-dominant "
          "prose for tokenizer-facing normalization. The source text is "
          "preserved in the TEI body.")]]])

(defn- encoding-desc [declarations orthographic-sentence-normalization?]
  (let [children (keep identity
                       [[:styleDefDecl {:scheme "css"}]
                        (char-decl declarations)
                        (when orthographic-sentence-normalization?
                          (orthographic-normalization-decl))])]
    (when (seq children)
      (into [:encodingDesc] children))))

(defn- text-class [work]
  (when-let [ndc (get work "ndc")]
    [:textClass
     [:classCode {:scheme "NDC"}
      (string/replace ndc #"^NDC " "")]]))

(defn- profile-desc [work]
  (let [classification (text-class work)]
    (cond-> [:profileDesc
             [:langUsage [:language {:ident "ja"} "日本語"]]]
      classification
      (conj classification))))

(defn build
  "Return a TEI <teiHeader> as hiccup-style nested vectors. Pure;
  no clojure.data.xml coupling at this boundary.

  Input shape:
    {:work         <work map, string keys>
     :contributors [{:relation-to-work \"...\" :person <person map, string keys>} ...]}

  Role and person are kept separate at every level inside this builder;
  the relation_to_work value never enters the person body."
  [{:keys [work contributors char-declarations
           orthographic-sentence-normalization?]}]
  [:teiHeader
   (file-desc work contributors)
   (encoding-desc char-declarations orthographic-sentence-normalization?)
   (profile-desc work)])

;; ---------------------------------------------------------------------------
;; Hiccup → clojure.data.xml adapter
;; ---------------------------------------------------------------------------

(defn- tei-qname [tag]
  (xml/qname tei-ns (name tag)))

(defn- attr-key [k]
  (cond
    (and (keyword? k) (= "xml" (namespace k)))
    (xml/qname xml-ns (name k))

    (and (keyword? k) (= "abc" (namespace k)))
    (str "abc:" (name k))

    (and (keyword? k) (= "xmlns" (namespace k)))
    (str "xmlns:" (name k))

    (keyword? k) (keyword (name k))

    :else k))

(defn- attrs->xml [attrs]
  (into {}
        (for [[k v] attrs]
          [(attr-key k) (str v)])))

(defn- ->xml-element
  "Convert hiccup vector to a clojure.data.xml element. Tags are TEI
  by default; :xml/lang and :xml/id route to the XML namespace."
  [v]
  (cond
    (vector? v)
    (let [[tag attrs-or-child & rest-children] v
          [attrs children] (if (and (map? attrs-or-child)
                                    (not (record? attrs-or-child)))
                             [attrs-or-child rest-children]
                             [{} (cons attrs-or-child rest-children)])]
      (apply xml/element
             (tei-qname tag)
             (attrs->xml attrs)
             (keep ->xml-element children)))

    (nil? v) nil
    :else (str v)))

(defn hiccup->xml-string
  "Serialise TEI hiccup to an XML string. The TEI namespace is the
  default; xml: prefix is bound to the XML namespace."
  [hiccup]
  (xml/emit-str (->xml-element hiccup)))

(def ^:private element-only-containers
  #{"TEI" "teiHeader" "fileDesc" "titleStmt" "publicationStmt" "sourceDesc"
    "encodingDesc" "profileDesc" "langUsage" "textClass" "keywords" "revisionDesc"
    "charDecl" "char" "biblStruct" "analytic" "monogr" "imprint"
    "text" "front" "body" "back" "div"})

(defn- indent-structural-elements [element depth]
  (let [children (:content element)]
    (if (and (contains? element-only-containers (name (:tag element)))
             (not= "preserve" (get (:attrs element) (xml/qname xml-ns "space")))
             (seq children)
             (every? #(and (map? %) (:tag %)) children))
      (let [padding #(str "\n" (apply str (repeat (* 2 %) " ")))]
        (assoc element :content
               (concat
                (mapcat (fn [child]
                          [(padding (inc depth))
                           (indent-structural-elements child (inc depth))])
                        children)
                [(padding depth)])))
      element)))

(defn hiccup->pretty-xml-string
  "Indent structural TEI containers while leaving mixed content and preserved
  whitespace unchanged. Unknown content models remain unformatted."
  [hiccup]
  (xml/emit-str (indent-structural-elements (->xml-element hiccup) 0)))

(defn emit-xml
  "Serialise a hiccup TEI header to an XML string. The TEI namespace
  is the default; xml: prefix is bound to the XML namespace."
  [hiccup]
  (hiccup->xml-string hiccup))
