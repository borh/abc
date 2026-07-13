(ns abc.annotation.schema)

;; att.linguistics: <w> element for morphological information
;; http://www.tei-c.org/release/doc/tei-p5-doc/en/html/ref-att.linguistic.html
;; Morphosyntactic fields: msd, lemma, pos, join (for non-independent morphemes).

(def registry
  {:quotation/type            [:enum :spoken :thought]
   :quotation/direct          boolean?
   :quotation/aloud           boolean?
   :quotation/outside-referer boolean?
   ::quotation                [:map
                               [:quotation/type {:optional true}]
                               [:quotation/direct {:optional true}]
                               [:quotation/aloud {:optional true}]
                               [:quotation/outside-referer {:optional true}]]

   :annotation/type           :keyword
   :annotation/content        [:or :string ::quotation]
   :annotation/target         :string
   :annotation/replace?       :boolean
   :ruby/reading              :string
   ::annotation               [:map
                               :annotation/type
                               [:annotation/content {:optional true}]
                               [:annotation/target {:optional true}]
                               [:ruby/reading {:optional true}]]
   :fragment/annotation       ::annotation

   ::tags                     [:set ::annotation]
   ::annotated-fragment       [:or
                               :string
                               ::annotation
                               [:map
                                [:sentence/fragment {:optional true} :string]
                                [:fragment/annotation {:optional true} ::annotation]]
                               [:vector [:ref ::annotated-fragment]]]

   :sentence/text             :string
   :sentence/annotated-text   [:or
                               :string
                               [:vector ::annotated-fragment]]
   :mecab.morpheme            [:map
                               [:mecab.features/pos-1 :string]
                               [:mecab.features/pos-2 :string]
                               [:mecab.features/pos-3 :string]
                               [:mecab.features/pos-4 :string]
                               [:mecab.features/c-type :string]
                               [:mecab.features/c-form :string]
                               [:mecab.features/orth :string]
                               [:mecab.features/orth-base :string]
                               [:mecab.features/pron {:optional true} :string]
                               [:mecab.features/l-form {:optional true} :string]
                               [:mecab.features/lemma {:optional true} :string]
                               [:mecab.features/kana {:optional true} :string]
                               [:mecab.features/goshu {:optional true} :string]
                               [:mecab.features/pron-base {:optional true} :string]
                               [:mecab.features/kana-base {:optional true} :string]
                               [:mecab.features/form {:optional true} :string]
                               [:mecab.features/form-base {:optional true} :string]
                               [:mecab.features/i-type {:optional true} :string]
                               [:mecab.features/i-form {:optional true} :string]
                               [:mecab.features/i-con-type {:optional true} :string]
                               [:mecab.features/f-type {:optional true} :string]
                               [:mecab.features/f-form {:optional true} :string]
                               [:mecab.features/f-con-type {:optional true} :string]
                               [:mecab.features/type {:optional true} :string]
                               [:mecab.features/a-type {:optional true} :string]
                               [:mecab.features/a-con-type {:optional true} :string]
                               [:mecab.features/a-mod-type {:optional true} :string]
                               [:mecab.features/lid {:optional true} :string]
                               [:mecab.features/lemma-id {:optional true} :string]
                               [:mecab.features/position {:optional true} :int]]
   :sentence/tokens           [:vector :mecab.morpheme]
   :sentence/tags             [:set :keyword]

   :text/tags                 ::tags
   :text/fragment             :string
   ::fragments                [:vector [:map
                                        [:text/fragment {:optional true}]
                                        [:text/tags {:optional true}]]]

   :paragraph/text            [:vector [:map :text/fragment]]
   :paragraph/sentences       [:vector [:map
                                        [:sentence/tags {:optional true}]
                                        :sentence/annotated-text
                                        :sentence/text
                                        [:sentence/tokens {:optional true}]]]
   :paragraph/tags            [:maybe [:set [:map-of :keyword :string]]]
   :document/paragraph        [:map
                               [:paragraph/tags {:optional true}]
                               :paragraph/sentences]
   :document/paragraphs       [:vector :document/paragraph]
   :document/stats            [:map
                               [:abc.stats/characters :int]
                               [:abc.stats/tokens :int]
                               [:abc.stats/types :int]
                               [:abc.stats/paragraphs :int]
                               [:abc.stats/sentences :int]
                               [:abc.stats/sentence-lengths-median :double]
                               [:abc.stats/hapax-legomenon :int]
                               [:abc.stats/yules-k :double]
                               [:abc.stats/sttr-500 [:maybe :double]]]
   :document/metadata         :document/stats
   :document/body             [:map
                               :document/paragraphs
                               :document/metadata]
   :document                  [:map
                               :document/paragraphs
                               :document/metadata]

   :annotation/span           [:map
                               [:span/sentence-start {:optional true} :int]
                               [:span/sentence-end {:optional true} :int]
                               [:span/paragraph-start {:optional true} :int]
                               [:span/paragraph-end {:optional true} :int]
                               [:span/token-start {:optional true} :int]
                               [:span/token-end {:optional true} :int]
                               [:span/char-start {:optional true} :int]
                               [:span/char-end {:optional true} :int]]})
