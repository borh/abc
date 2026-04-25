(ns abc.tei
  "Conversion utilities from Aozora Bunko text format to TEI P5, with reference to the Japanese TEI Guidelines from
  https://github.com/TEI-EAJ/jp_guidelines/wiki."
  (:require [malli.core :as m]
            [malli.experimental :as mx]
            [clojure.java.io :as io]
            [clojure.data.xml :as xml]
            [java-time.format :as time-format]
            [abc.aozora :as aozora]
            [abc.annotation :as a]
            [java-time :as time]
            [clojure.spec.gen.alpha :as gen])
  (:import [clojure.data.xml.node Element]
           (java.time LocalDate)))

;; Check:
;; https://github.com/shibusawa-dlab/lab1

(def registry
  (merge abc.aozora/registry
         {:tei/title              :string
          :tei/authors            [:vector :string]
          :tei/editors            [:vector :string]
          :tei/extent             :document/stats #_[:map-of [:enum :character-count :suw-count :sentence-count :paragraph-count :section-count]
                                                     :int]
          :tei/publications       [:vector
                                   [:map
                                    [:tei/publisher :string] [:tei/distributor :string] [:tei/availability :string]
                                    [:tei/date {:optional true} :abc.aozora/date] [:tei/idno {:optional true} :string]]]
          :tei/source-description :string
          :tei/responsibilities   [:vector [:map [:resp :string] [:names :string]]]
          :tei/factuality-type    [:enum "fictional" "non-fictional" "mixed"]
          :tei/ndc                [:schema [:vector [:map
                                                     [:ndc/category #_[:vector :string] (into [:enum] abc.aozora/ndc-strings)]
                                                     [:ndc/children :boolean]]]]
          :tei/text-type          [:enum "novel" "letter"]
          :tei/derivation-type    [:enum "original" "translation"]
          :tei/constitution-type  [:enum "single"]
          :tei/purpose-type       :string
          :tei/purpose-degree     :string
          :tei/changes            [:vector [:map [:changes/when :abc.aozora/date] [:changes/who :string]]]
          :tei/header             [:map
                                   :tei/title
                                   :tei/authors
                                   :tei/editors
                                   :tei/extent
                                   :tei/publications
                                   :tei/source-description
                                   :tei/responsibilities
                                   :tei/factuality-type
                                   :tei/ndc
                                   :tei/text-type
                                   :tei/derivation-type
                                   :tei/constitution-type
                                   :tei/purpose-type
                                   :tei/purpose-degree
                                   :tei/changes]}))

(defn metadata-to-tei [m]
  #:tei{:title              (::aozora/title m)
        :authors            (mapv (fn [a] (str (::aozora/family-name a) " " (::aozora/given-name a)))
                                  (::aozora/authors m))
        :editors            (::aozora/editors m)
        :extent             (::aozora/stats m)
        :publications       (mapv (fn [x]
                                    ;; TODO
                                    #:tei{:publisher (::aozora/publisher x)
                                          :date      (::aozora/first-published x)})
                                  (::aozora/reference m))
        :source-description "TODO"
        :responsibilities   [{:resp "" :names [""]}]
        :ndc                (::aozora/NDC m)
        :changes            [(::aozora/aozora-publishing-date m)
                             (::aozora/aozora-last-modified-date m)]})

(defn header [m]
  [:teiHeader
   (into
    [:fileDesc
     (concat
      [:titleStmt
       [:title (:tei/title m)]]
      (mapv (fn [author] [:author author]) (:tei/authors m))
      (mapv (fn [editor] [:editor editor]) (:tei/editors m)))
     [:editionStmt [:p]]
     (let [{:keys [abc.aozora.stats/characters abc.aozora.stats/tokens abc.aozora.stats/sentences
                   abc.aozora.stats/paragraphs abc.aozora.stats/types abc.aozora.stats/sentence-lengths-median
                   abc.aozora.stats/hapax-legomenon abc.aozora.stats/yules-k abc.aozora.stats/sttr-500]} (::aozora/stats m)]
       [:extent
        [:measure {:unit "characters" :quantity characters} (str characters " characters")]
        [:measure {:unit "SUWs" :quantity tokens} (str tokens " Short Unit Words")]
        [:measure {:unit "paragraphs" :quantity paragraphs} (str paragraphs " paragraphs")]
        [:measure {:unit "sentences" :quantity sentences} (str sentences " sentences")]
        [:measure {:unit "types" :quantity types} (str types " SUW orthographic types")]
        [:measure {:unit "sentence-lengths-median" :quantity sentence-lengths-median} (str sentence-lengths-median " median sentence SUW length")]
        [:measure {:unit "hapax-legomenon" :quantity hapax-legomenon} (str hapax-legomenon " hapax legomenon")]
        [:measure {:unit "yules-k" :quantity yules-k} (str yules-k " Yule's K")]
        [:measure {:unit "STTR" :quantity sttr-500} (str sttr-500 " STTR (for window length: 500 SUW)")]])
     (into [:publicationStmt]
           (first (mapv (fn [{:keys [tei/publisher tei/distributor tei/availability tei/date tei/idno]}]
                          [[:publisher publisher]
                           [:distributor distributor]
                           [:idno {:type "ISBN"} idno]
                           [:availability {:status "free"} [:p availability]]
                           (when date
                             [:date {:when (time-format/format "yyyy-MM-dd" date)}
                              (time-format/format "MMMM d, YYY" date)])])
                        (:tei/publications m))))
     #_[:seriesStmt [:p]]
     [:sourceDesc [:p (:tei/source-description m)]]
     [:encodingDesc                                        ;; correction normalization quotation
      [:p "This document is an automated conversion of the original document from the Aozora Bunko by the [[Project Name]] with additional encoding of word, sentence and paragraph boundaries. Some non-linguistic formatting has been elided from the original. FIXME"]]
     [:projectDesc [:p "Text converted from the Aozora Bunko collection. FIXME"]]]
    (mapv (fn [{:keys [resp names]}]
            (into [:respStmt [:resp resp]]
                  (mapv (fn [n] [:name n]) names)))
          (:tei/responsibilities m)))
   [:profileDesc
    [:langUsage {:ident "ja"} "Japanese"]
    #_[:creation "Original written in " [:date {:when ""} ""] "."]
    (into [:textClass]
          (mapv (fn [{:keys [category childrens]}]
                  [:classCode {:scheme "#NDC"} (str (if childrens "K") category)]) (:tei/ndc m)))
    [:textDesc {:n (:tei/text-type m)}
     [:channel {:mode "w"} (:tei/channel-description m)]
     [:constitution {:type (:tei/constitution-type m)}]
     [:derivation {:type (:tei/derivation-type m)}]
     [:factuality {:type (:tei/factuality-type m)}]
     [:purpose {:type (:tei/purpose-type m) :degree (:tei/purpose-degree m)}]]]
   (into
    [:revisionDesc
     {:status "published"}]
    (mapv (fn [{:keys [when who]}] [:change {:when when :who who} "change msg"]) (:tei/changes m)))
   [:classDecl
    [:taxonomy {:xml:id "NDC"}
     [:bibl
      [:title "TODO"]
      [:edition "9"]
      [:ptr {:target "http://..."}]]]]])

#_(m/=> header [:=>
                [:cat [:schema {:registry registry} :tei/header]]
                [:vector :any]])

;; <purpose type="entertain" degree="high"/>
;; <purpose type="inform" degree="medium"/>

#_(s/fdef tei-quotation
    :args (s/cat :m ::a/quotation :s string?)
    :ret (s/tuple #{:quote :q} map?))
(def Quotation
  [:schema [:map
            [:quotation/type [:enum :quote :TODO]]
            [:quotation/direct :boolean]
            [:quotation/aloud :boolean]]])
(mx/defn tei-quotation :- [:tuple [:enum :quote :q] map?]
  [m :- Quotation s :- :string]
  [(if (:quotation/outside-referer m)
     :quote
     :q)
   (cond-> {}
     (:quotation/type m) (assoc :type (:quotation/type m))
     (:quotation/direct m) (assoc :direct true)
     (:quotation/aloud m) (assoc :aloud true))
   s])

#_(m/=> tei-quotation [:=> [:catn [:m Quotation :s :string]] [:tuple [:enum :quote :q] map?]])

#_(s/fdef tei-tags
    :args (s/cat :tags (s/alt :p :paragraph/tags :s :sentence/tags))
    :ret map?)
(defn tei-tags [tags]
  {:tags tags})

(def ParagraphTags [:enum :speech :TODO])
(def SentenceTags [:enum :speech :TODO])

(m/=> tei-tags [:=> [:cat [:alt ParagraphTags SentenceTags]] :map])

#_(s/fdef body
    :args (s/cat :text :document/body)
    :ret vector?)

;; TODO Should this be a multimetod or should we already create TEI compatible tags at the parsing stage?
(defmulti render-tei :annotation/type)

(defmethod render-tei :ruby
  [m]
  nil)

(defn body [text]
  [:body
   (for [p (:document/paragraphs text)]
     [:p (tei-tags (:paragraph/tags p))
      (for [s (:paragraph/sentences p)
            ;; TODO use annotated/tokenized information here
            :let [sentence-text (:sentence/text s)]]
        [:s (tei-tags (:sentence/tags s))
         sentence-text])])])

#_(m/=> body
        [:=>
         [:cat [:schema {:registry abc.annotation/registry} :document/body]]
         :vector])

#_(s/fdef doc
    :args (s/cat :metadata :tei/header :text :document/body)
    :ret #(instance? Element %))

(xml/alias-uri 'TEI "http://www.tei-c.org/ns/1.0")
;; TODO Have interoperable EDN and XML versions. Need to merge the metadata and text below into a standard structure.
(defn doc [metadata doc]
  (xml/sexp-as-element
   [:TEI {:xmlns "http://www.tei-c.org/ns/1.0"}
    (header (assoc metadata ::aozora/stats (-> doc :document/metadata)))
    (body doc)]))

#_(m/=> doc
        [:=>
         [:catn
          [:metadata [:schema {:registry registry} :tei/header]]
          [:text [:schema {:registry abc.annotation/registry} :document/body]]]
         #(instance? Element %)])

(defn save! [path data]
  ;; FIXME pretty-print
  (with-open [f (io/writer path)]
    (xml/emit data f)))

(defn load-tei [path]
  (xml/parse (io/reader path)))

(defn export-all! [])
