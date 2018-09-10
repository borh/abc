(ns abc.tei
  "Conversion utilities from Aozora Bunko text format to TEI P5."
  (:require [clojure.spec.alpha :as s]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.data.xml :as xml]
            [java-time.format :as time-format]
            [abc.aozora]
            [abc.annotation :as a])
  (:import [clojure.data.xml.node Element]))

(s/def :tei/title string?)
(s/def :tei/authors (s/coll-of string?)) ;; xml:id
(s/def :tei/editors (s/coll-of string?)) ;; xml:id
(s/def :tei/extent (s/map-of #{:character-count :suw-count :sentence-count :paragraph-count} int?))
(s/def :tei/publisher string?)
(s/def :tei/distributor string?)
(s/def :tei/availability string?)
(s/def :tei/date :abc.aozora/date-time)
(s/def :tei/idno string?)
(s/def :tei/publications (s/coll-of (s/keys :req [:tei/publisher :tei/distributor :tei/availability :tei/date]
                                            :opt [:tei/idno])))
(s/def :tei/source-description string?)
(s/def :tei.responsibilities/resp string?)
(s/def :tei.responsibilities/names (s/coll-of string?))
(s/def :tei/responsibilities (s/coll-of (s/keys :req [:tei.responsibilities/resp :tei.responsibilities/names])))
(s/def :tei/factuality-type #{"fictional" "non-fictional" "mixed"})
(s/def :tei/ndc :abc.aozora/NDC)
(s/def :tei/text-type #{"novel" "letter"})
(s/def :tei/derivation-type #{"original" "translation"})
(s/def :tei/consitution-type #{"single"})
(s/def :tei/purpose-type string?)
(s/def :tei/purpose-degree string?)
(s/def :tei.changes/when :abc.aozora/date-time)
(s/def :tei.changes/who (s/and string? #_#(string/starts-with? % "#")))
(s/def :tei/changes (s/coll-of (s/keys :req [:tei.changes/when :tei.changes/who]))) ;; More recent changes should appear first. The who attribute may be used to point to any other element, but will typically specify a respStmt or person element elsewhere in the header, identifying the person responsible for the change and their role in making it. (-> use xml:id to map to existing ids)

(s/def :tei/header
  (s/keys
   :req
   [:tei/title
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
    :tei/consitution-type
    :tei/purpose-type
    :tei/purpose-degree
    :tei/changes]))

(s/fdef header
  :args (s/cat :m :tei/header)
  :ret vector?)

(defn header [m]
  [:teiHeader
   (concat
    [:fileDesc
     (concat
      [:titleStmt
       [:title (:tei/title m)]]
      (mapv (fn [author] [:author author]) (:tei/authors m))
      (mapv (fn [editor] [:editor editor]) (:tei/editors m)))
     [:editionStmt [:p ]]
     (let [{:keys [character-count suw-count sentence-count paragraph-count]} (:tei/extent m)]
       [:extent
        [:measure {:unit "characters" :quantity character-count} (str character-count " characters")]
        [:measure {:unit "SUWs" :quantity suw-count} (str suw-count " Short Unit Words")]
        [:measure {:unit "paragraphs" :quantity paragraph-count} (str paragraph-count " paragraphs")]
        [:measure {:unit "sentences" :quantity sentence-count} (str sentence-count " sentences")]])
     (into [:publicationStmt]
           (first (mapv (fn [{:keys [tei/publisher tei/distributor tei/availability tei/date tei/idno]}]
                          [[:publisher publisher]
                           [:distributor distributor]
                           [:idno {:type "ISBN"} idno]
                           [:availability {:status "free"} [:p availability]]
                           [:date {:when (time-format/format "yyyy-MM-dd" date)}
                            (time-format/format "MMMM d, YYY" date)]])
                        (:tei/publications m))))
     #_[:seriesStmt [:p]]
     [:sourceDesc [:p (:tei/source-description m)]]
     [:encodingDesc ;; correction normalization quotation
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
     [:consititution {:type (:tei/constitution-type m)}]
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
      [:title ""]
      [:edition "9"]
      [:ptr {:target "http://..."}]]]]])

;; <purpose type="entertain" degree="high"/>
;; <purpose type="inform" degree="medium"/>

(s/fdef tei-quotation
  :args (s/cat :m ::a/quotation :s string?)
  :ret (s/tuple #{:quote :q} map?))
(defn tei-quotation [m s]
  [(if (:quotation/outside-referer m)
     :quote
     :q)
   (cond-> {}
     (:quotation/type m) (assoc :type (:quotation/type m))
     (:quotation/direct m) (assoc :direct true)
     (:quotation/aloud m) (assoc :aloud true))
   s])

(s/fdef tei-tags
  :args (s/cat :tags (s/or :p :paragraph/tags :s :sentence/tags))
  :ret map?)
(defn tei-tags [tags]
  {:tags tags})

(s/fdef body
  :args (s/cat :text :document/text)
  :ret vector?)

(defn body [text]
  [:body
   (for [p text]
     [:p (tei-tags (:paragraph/tags p))
      (for [s (:document/sentences p)]
        [:s (tei-tags (:sentence/tags s))
         (:document/sentence s)])])])

(s/fdef doc
  :args (s/cat :metadata :tei/header :text :document/text)
  :ret #(instance? Element %))

(defn doc [metadata text]
  (xml/sexp-as-element
   [:TEI {:xmlns "http://www.tei-c.org/ns/1.0"}
    (header metadata)
    (body text)]))

(defn save! [path data]
  ;; FIXME pretty-print
  (with-open [f (io/writer path)]
    (xml/emit data f)))

(defn load-tei [path]
  (xml/parse (io/reader path)))
