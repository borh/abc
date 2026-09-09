(ns soranoha.ori.tei-header
  "Pure metadata-record → TEI <teiHeader> hiccup builder + XML emitter.

  build returns nested-vector data; the XML emitters serialize that tree.
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
          [:title {:type \"reading\" :xml/lang \"ja-Hira\"} \"...\"]?
          [:title {:type \"sub\" :xml/lang \"ja\"} \"...\"]?
          [:title {:type \"sub-reading\" :xml/lang \"ja-Hira\"} \"...\"]?
          [:title {:type \"original\"} \"...\"]?
          [:author <persName>+ <idno>]
          [:respStmt [:resp \"...\"] <persName>+]?]
        [:publicationStmt [:publisher \"...\"] [:idno {:type \"...\"} \"...\"]+ [:date {:when \"...\"} \"...\"] <availability>?]
        [:sourceDesc [:bibl ...]+]]
      [:encodingDesc
        [:styleDefDecl {:scheme \"css\"}]
        [:classDecl [:taxonomy {:xml/id \"...\"} [:bibl ...]]+]
        [:charDecl [:char {:xml/id \"...\"} ...]?]
        [:editorialDecl
          [:normalization {:method \"markup\"} ...]]?]
      [:profileDesc
        [:langUsage [:language {:ident \"ja\"} \"...\"]]
        [:textClass [:classCode {:scheme \"#...\"} \"...\"]+]]]

  Tag keywords with no namespace are TEI elements. :xml/lang and
  :xml/id route to the XML namespace. Strings are text content."
  (:require [clojure.data.xml :as xml]
            [clojure.string :as string]
            [soranoha.core.rights :as rights]))

(def ^:private tei-ns "http://www.tei-c.org/ns/1.0")
(def ^:private xml-ns "http://www.w3.org/XML/1998/namespace")

;; Hiccup builder (pure data, no XML library coupling beyond keyword names)

(defn- person-idno-type
  "Who issued this person identifier.

  A six-digit id is Aozora's 人物ID, taken from the catalog's 人物ID column.
  Anything else is minted locally, and the record schemas permit one such
  form. Publishing a locally minted id as `aozora-person-id` would claim a
  provenance Aozora did not grant, which is the one thing an idno type
  exists to state."
  [person-id]
  (if (re-matches #"[0-9]{6}" (str person-id))
    "aozora-person-id"
    "soranoha-person-id"))

(defn- person-name-block
  "Kanji name with the person ID; reading and romaji names when supplied."
  [person]
  (let [pid (get person "person_id")
        pers-name (fn [lang surname forename & extras]
                    (cond-> [:persName {:xml/lang lang} [:surname surname]]
                      forename (conj [:forename forename])
                      true (into extras)))]
    (cond-> [(pers-name "ja"
                        (get person "family_name")
                        (get person "given_name")
                        [:idno {:type (person-idno-type pid)} pid])]
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
  {:relation-to-work <role-string> :person <person-body-map>}.

  Four title forms where the catalog carries them. The subtitle matters for
  identification, not decoration: author and title alone leave 1966 works in
  the Aozora catalog ambiguous, and adding 副題 cuts that to 562. The original
  title of a translated work carries no language attribute because the catalog
  records no source language, and guessing one from the string would be an
  assertion this project cannot support."
  [work contributors]
  (let [authors (filter #(= "著者" (:relation-to-work %)) contributors)
        others (remove #(= "著者" (:relation-to-work %)) contributors)
        title (get work "title")
        title-r (get work "title_reading")
        subtitle (get work "subtitle")
        subtitle-r (get work "subtitle_reading")
        original (get work "original_title")]
    (-> [:titleStmt
         [:title {:type "main" :xml/lang "ja"} title]]
        (cond-> title-r
          (conj [:title {:type "reading" :xml/lang "ja-Hira"} title-r]))
        (cond-> subtitle
          (conj [:title {:type "sub" :xml/lang "ja"} subtitle]))
        (cond-> subtitle-r
          (conj [:title {:type "sub-reading" :xml/lang "ja-Hira"} subtitle-r]))
        (cond-> original
          (conj [:title {:type "original"} original]))
        (into (mapv #(author-block (:person %)) authors))
        (into (mapv #(resp-stmt (:relation-to-work %) (:person %)) others)))))

(defn- availability
  "TEI <availability> for the release's rights grant. Two layers, stated
  separately because they differ: the underlying work's standing, which
  Soranoha inherits and does not create, and the licence over Soranoha's own
  encoding. A TEI file is normally read detached from the site it came from,
  so the terms have to travel inside it."
  [{:strs [works encoding statement_url]}]
  ;; Two <licence> elements rather than <licence> plus <p>: a <p> here would
  ;; be a paragraph of the document, and the reading view and its projections
  ;; select paragraphs, so header prose would surface as body text.
  [:availability {:status "free"}
   [:licence {:target (rights/works-uri works)}
    (rights/works-statement works)]
   [:licence {:target (rights/licence-uri encoding)}
    (rights/licence-statement encoding)
    " Full rights statement: "
    [:ptr {:target statement_url}]]])

(defn- publication-stmt
  "Who published these bytes, under what identifier, and on what terms.

  Three identifiers, and the type names say who issued which. Soranoha issues
  the publication identifier — the `<work-id>_<card-directory>` form the site
  serves works under — so it is not labelled `aozora-*`: Aozora issues the
  work id and the card, not the pair. Without it a downloaded file cannot say
  what to cite it as, which is the property the citation projections rest on.

  The card URL moves here from the source description. It identifies Aozora's
  record for the work, which is what the other two identifiers beside it do;
  the source description is about the printed edition transcribed, which is a
  different thing and is often not the one the card links.

  The publisher is `Soranoha`, the entity, not `Soranoha Aozora TEI Corpus`,
  which is the corpus title and is already carried by `titleStmt`. `abc`
  survives in the w3id schema namespaces and the source-bundle construction
  id, where it identifies a schema rather than naming a publisher.

  `@ref` rather than a `pubPlace`: a downloaded file should resolve to its
  publisher without a lookup, and a corpus published only on the web has no
  place of publication that would not be invented."
  [work slug rights-grant]
  (cond-> [:publicationStmt
           [:publisher {:ref "https://soranoha.org"} "Soranoha"]]
    slug (conj [:idno {:type "soranoha-work-identifier"} slug])
    true (conj [:idno {:type "aozora-work-id"} (get work "work_id")])
    (get work "card_url") (conj [:idno {:type "aozora-card-url"} (get work "card_url")])
    true (conj [:date {:when (get work "aozora_modified")} (get work "aozora_modified")])
    rights-grant (conj (availability rights-grant))))

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

(defn- fallback-source-bibl
  "What can be said about the source when the catalog records no edition. The
  Aozora card URL is no longer part of this: it is published unconditionally
  in the publication statement, so a work with no edition metadata keeps the
  link back to Aozora instead of being the only kind of work that has one."
  [work]
  (if-let [title (get work "title")]
    [:bibl [:title title]]
    [:bibl [:note "Aozora Bunko source edition metadata is not available."]]))

(defn- source-desc
  "The editions this text was transcribed from, plus where it first appeared.

  初出 is a `bibl` rather than a `note` because `sourceDesc` does not admit
  notes — its content is bibliographic — and because a first-publication
  statement is a reference to another appearance of the work, which is what a
  `bibl` is for. It is distinguished from the transcribed editions by its
  type: it describes where the text was first printed, not what was keyed."
  [work source-content-hash primary-text-hash]
  (let [editions (get work "source_editions")]
    (cond-> (if (seq editions)
              (into [:sourceDesc] (mapv bibl-edition editions))
              [:sourceDesc (fallback-source-bibl work)])
      (get work "first_published")
      (conj [:bibl {:type "first-publication"} (get work "first_published")])
      source-content-hash (conj [:bibl [:idno {:type "source-content-hash"} source-content-hash]])
      primary-text-hash (conj [:bibl [:idno {:type "primary-text-hash"} primary-text-hash]]))))

(defn- file-desc [work contributors slug source-content-hash primary-text-hash rights-grant]
  [:fileDesc
   (title-stmt work contributors)
   (publication-stmt work slug rights-grant)
   (source-desc work source-content-hash primary-text-hash)])

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

(def ^:private class-decl
  "The taxonomies the text classification points at, declared rather than
  named by a bare string, so a reader can tell who classified the work.

  Aozora assigns both the NDC code and the orthographic style. Declaring the
  taxonomies is what carries that provenance to a reader, and it matters most
  for 文字遣い種別, which is populated for every work and partitions the corpus
  into 新字新仮名, 新字旧仮名, 旧字旧仮名, 旧字新仮名 and その他. A
  historical-kana study that silently mixes those is invalid, and the value
  varies within one series: 銭形平次捕物控 001 to 004 are 旧字旧仮名 while 005
  is 新字新仮名."
  [:classDecl
   [:taxonomy {:xml/id "ndc"}
    [:bibl "日本十進分類法 (Nippon Decimal Classification), as recorded by "
     [:title "青空文庫"] " in its 分類番号 field."]]
   [:taxonomy {:xml/id "aozora-orthography"}
    [:bibl [:title "青空文庫"]
     " 文字遣い種別: the orthographic style Aozora Bunko records for the "
     "transcription."]]])

(def ^:private source-refs-decl
  "Where a `source-span` reference's two numbers are measured from.

  That is the one fact a reader cannot recover from the document itself: the
  extent is in the note's own id, the line is in its `n`, and the text is the
  one `sourceDesc` already names. What remains is the unit, and a pointer to
  the vocabulary for anything further. The convention is documented at the
  namespace IRI rather than restated in each of tens of thousands of files;
  a published file is the text and its evidence, not a manual."
  [:refsDecl {:xml/id "source-spans"}
   [:p "A note of type source-span has an xml:id of the form source-START-END, "
    "giving the extent in UTF-8 bytes of the decoded primary text identified in "
    "sourceDesc. "
    [:ptr {:target "https://w3id.org/soranoha/ns/tei"}]]])

(defn- encoding-desc [declarations]
  (cond-> [:encodingDesc [:styleDefDecl {:scheme "css"}] class-decl source-refs-decl]
    (seq declarations) (conj (char-decl declarations))))

(defn- text-class
  "NDC class and orthographic style, each pointing at its declared taxonomy."
  [work]
  ;; `ndc` is already nil unless it matched the catalog's documented form.
  ;; `orthographic_style` is not run through the same nullable guard upstream,
  ;; so a row that carries the column empty would otherwise publish an empty
  ;; classification, which claims less than nothing.
  (let [ndc (get work "ndc")
        orthography (not-empty (some-> (get work "orthographic_style") string/trim))]
    (when (or ndc orthography)
      (cond-> [:textClass]
        ndc (conj [:classCode {:scheme "#ndc"} (string/replace ndc #"^NDC " "")])
        orthography (conj [:classCode {:scheme "#aozora-orthography"} orthography])))))

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
     :contributors [{:relation-to-work \"...\" :person <person map, string keys>} ...]
     :slug          <the publication identifier, when the caller has one>
     :rights        <the release rights grant, manifest key spelling>}

  Role and person are kept separate at every level inside this builder;
  the relation_to_work value never enters the person body.

  :rights is optional here so a header can be built for inspection without a
  policy in hand; the render stage supplies it for every published work and
  the profile's snh-publication-licence rule rejects a published file without
  it."
  [{:keys [work contributors slug char-declarations source-content-hash
           primary-text-hash rights]}]
  [:teiHeader
   (file-desc work contributors slug source-content-hash primary-text-hash rights)
   (encoding-desc char-declarations)
   (profile-desc work)])

;; Hiccup → clojure.data.xml adapter

(defn- tei-qname [tag]
  (xml/qname tei-ns (name tag)))

(defn- attr-key [k]
  (cond
    (and (keyword? k) (= "xml" (namespace k)))
    (xml/qname xml-ns (name k))

    (and (keyword? k) (= "snh" (namespace k)))
    (str "snh:" (name k))

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

(defn- with-default-tei-ns
  "Declare the TEI namespace as the document default on the root element.

  Without a declaration clojure.data.xml invents a prefix for the namespace
  and emits `<a:TEI xmlns:a=\"...\">`. That is namespace-correct and every
  namespace-aware processor reads it, but it is not the shape TEI corpora are
  published in, and researcher-written XPath (copied from teaching material,
  or run through tooling that ignores namespaces) is written against
  unprefixed element names. A prefixed serialisation returns empty node sets
  there rather than failing visibly."
  [element]
  (update element :attrs assoc :xmlns tei-ns))

(defn hiccup->xml-string
  "Serialise TEI hiccup to an XML string. The TEI namespace is the
  default; xml: prefix is bound to the XML namespace."
  [hiccup]
  (xml/emit-str (with-default-tei-ns (->xml-element hiccup))))

(def ^:private element-only-containers
  #{"TEI" "teiHeader" "fileDesc" "titleStmt" "publicationStmt" "availability" "sourceDesc"
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
  (xml/emit-str (indent-structural-elements (with-default-tei-ns (->xml-element hiccup)) 0)))
