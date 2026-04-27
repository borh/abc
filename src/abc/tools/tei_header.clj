(ns abc.tools.tei-header
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
      [:encodingDesc [:charDecl [:char {:xml/id \"...\"} ...]?]]
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
        family (get person "family_name")
        given (get person "given_name")
        family-r (get person "family_name_reading")
        given-r (get person "given_name_reading")
        family-l (get person "family_name_romaji")
        given-l (get person "given_name_romaji")]
    (cond-> [[:persName {:xml/lang "ja"}
              [:surname family]
              [:forename given]
              [:idno {:type "aozora-person-id"} pid]]]
      (and family-r given-r)
      (conj [:persName {:xml/lang "ja-Hira"}
             [:surname family-r]
             [:forename given-r]])
      (and family-l given-l)
      (conj [:persName {:xml/lang "ja-Latn"}
             [:surname family-l]
             [:forename given-l]]))))

(defn- author-block [person]
  (into [:author] (person-name-block person)))

(defn- resp-stmt [person]
  (into [:respStmt
         [:resp (get person "relation_to_work")]]
        (person-name-block person)))

(defn- title-stmt [work persons]
  (let [authors (filter #(= "著者" (get % "relation_to_work")) persons)
        contributors (remove #(= "著者" (get % "relation_to_work")) persons)
        title (get work "title")
        title-r (get work "title_reading")]
    (-> [:titleStmt
         [:title {:type "main" :xml/lang "ja"} title]]
        (cond-> title-r
          (conj [:title {:type "reading" :xml/lang "ja-Hira"} title-r]))
        (into (mapv author-block authors))
        (into (mapv resp-stmt contributors)))))

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

(defn- source-desc [work]
  (let [editions (get work "source_editions")]
    (into [:sourceDesc] (mapv bibl-edition editions))))

(defn- file-desc [work persons]
  [:fileDesc
   (title-stmt work persons)
   (publication-stmt work)
   (source-desc work)])

(defn- encoding-desc []
  [:encodingDesc
   [:charDecl
    [:char {:xml/id "example-gaiji"}
     [:localProp {:name "charName"
                  :value "Example unresolved Aozora gaiji fixture"}]
     [:desc "Design fixture for preserving an unresolved gaiji marker."]]]])

(defn- profile-desc [work]
  [:profileDesc
   [:langUsage [:language {:ident "ja"} "日本語"]]
   [:textClass
    [:classCode {:scheme "NDC"}
     (string/replace (get work "ndc") #"^NDC " "")]]])

(defn build
  "Return a TEI <teiHeader> as hiccup-style nested vectors. Pure;
  no clojure.data.xml coupling at this boundary."
  [{:strs [work persons]}]
  [:teiHeader
   (file-desc work persons)
   (encoding-desc)
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

(defn emit-xml
  "Serialise a hiccup TEI header to an XML string. The TEI namespace
  is the default; xml: prefix is bound to the XML namespace."
  [hiccup]
  (xml/emit-str (->xml-element hiccup)))
