(ns abc.aozora
  (:require [malli.core :as m]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.data]
            [java-time :as time]
            [corpus-utils.ndc :refer [ndc-map]]
            [lambdaisland.regal :as regal])
  (:import [java.time LocalDate]
           [java.net URL]))

(defn url? [u] (instance? URL u))

(defonce ndc-strings (set (vals ndc-map)))

(def registry
  {::date                      (m/-simple-schema
                                 {:type            ::date
                                  :pred            (fn [d] (instance? LocalDate d))
                                  :type-properties {:gen/elements [(time/local-date "2010-01-01")
                                                                   (time/local-date "2020-12-12")]}})
   ::url                       (m/-simple-schema
                                 {:type            ::url
                                  :pred            (fn [u] (instance? URL u))
                                  :type-properties {:gen/elements [(URL. "http://some.url.com/")
                                                                   (URL. "https://some.other.url.com/with/path")]}})
   :dcterms/format             [:enum "text/plain" "text/html" "text/xml"]
   ::character-set             [:enum "JIS X 0208" "Unicode"]
   ::encoding                  [:enum "SJIS" "EUC" "UTF-8"]
   :abc.aozora.ndc/category    (into [:enum] ndc-strings)
   :abc.aozora.ndc/children    :boolean
   ::NDC                       [:set [:map :abc.aozora.ndc/category [:abc.aozora.ndc/children {:optional true}]]]
   ::orthographic-style        [:enum
                                "新字新仮名"
                                "新字旧仮名"
                                "旧字新仮名"
                                "旧字旧仮名"
                                "その他"]
   ::relation-to-work          [:enum "著者" "翻訳者" "編者" "校訂者" "その他"]
   ::revision-count            [:int {:min 0}]
   ::last-modified-date        ::date
   ::source                    [:map :dcterms/format :abc.aozora/url :abc.aozora/revision-count :abc.aozora/character-set :abc.aozora/last-modified-date :abc.aozora/encoding]
   ::sources                   [:vector ::source]
   ::text-resource             ::source
   ::html-resource             ::source
   ::work-id                   :qualified-keyword
   ::person-id                 :qualified-keyword
   ::title                     [:string {:min 1}]
   ::original-title            [:string {:min 1}]
   ::reviser                   [:string {:min 1}]
   ::publisher                 [:vector :string]
   ::transcriber               [:string {:min 1}]
   ::transcription             [:string {:min 1}]
   ::subtitle                  [:string {:min 1}]
   ::subtitle-reading          [:string {:min 1}]
   ::sort-reading              [:string {:min 1}]
   ::copyright-expired         :boolean
   ::person-copyright-expired  :boolean
   ::revision-source           [:vector :string]
   ::transcription-source      [:vector :string]
   ::bib-resource              ::url

   ::author                    [:vector ::person-id]
   ::translator                [:vector ::person-id]
   ::editor                    [:vector ::person-id]
   ::proofreader               [:vector ::person-id]
   ::other-author              [:vector ::person-id #_[:string {:min 1}]]

   ::author-of                 [:vector ::work-id #_[:string {:min 1}]]
   ::translator-of             [:vector ::work-id]
   ::editor-of                 [:vector ::work-id]
   ::proofreader-of            [:vector ::work-id]
   ::other-author-of           [:vector ::work-id]

   ::work                      [:map ::work-id
                                [::author {:optional true}]
                                [::translator {:optional true}]
                                [::editor {:optional true}]
                                [::proofreader {:optional true}]
                                [::other-author {:optional true}]
                                [::NDC {:optional true}]
                                [::aozora-last-modified-date {:optional true}]
                                [::aozora-publishing-date {:optional true}]
                                [::bib-resource {:optional true}]
                                [::copyright-expired {:optional true}]
                                [::first-published {:optional true}]
                                [::original-title {:optional true}]
                                [::orthographic-style {:optional true}]
                                [::revision-source {:optional true}]
                                [::reviser {:optional true}]
                                [::sort-reading {:optional true}]
                                [::subtitle {:optional true}]
                                [::subtitle-reading {:optional true}]
                                [::title {:optional true}]
                                [::transcriber {:optional true}]
                                [::transcription-source {:optional true}]]
   ::publishing-span           ::date
   ::first-published           ::date
   ::aozora-publishing-date    ::date
   ::aozora-last-modified-date ::date
   ::date-of-birth             ::date
   ::date-of-death             ::date
   ::reference                 [:map
                                [::publisher {:optional true}]
                                [::first-published {:optional true}]
                                [::publishing-span {:optional true}]
                                [::title {:optional true}]]
   ::reference-1               ::reference
   ::reference-2               ::reference
   ::references                [:vector ::reference]
   ::given-name                [:string {:min 1}]
   ::given-name-romaji         [:string {:min 1}]
   ::given-name-reading        [:string {:min 1}]
   ::family-name               [:string {:min 1}]
   ::family-name-romaji        [:string {:min 1}]
   ::family-name-reading       [:string {:min 1}]
   ::person                    [:map ::person-id
                                [::author-of {:optional true}]
                                [::translator-of {:optional true}]
                                [::editor-of {:optional true}]
                                [::proofreader-of {:optional true}]
                                [::other-author-of {:optional true}]
                                [::date-of-birth {:optional true}]
                                [::date-of-death {:optional true}]
                                [::family-name {:optional true}]
                                [::family-name-reading {:optional true}]
                                [::family-name-romaji {:optional true}]
                                [::given-name {:optional true}]
                                [::given-name-reading {:optional true}]
                                [::given-name-romaji {:optional true}]
                                [::person-copyright-expired {:optional true}]]
   ::db-entry                  [:map ::work ::person]
   ::db-entries                [:map
                                [:works [:map-of :keyword ::work]]
                                [:persons [:map-of :keyword ::person]]]
   ::entity-map                [:altn
                                [:work ::work]
                                [:person ::person]
                                [:sources ::sources]
                                [:references ::references]]})

(defn inverse-relation [kw]
  (keyword (namespace kw) (str (name kw) "-of")))

(defn relation-to-work-rdf [s]
  (case s
    "著者" ::author
    "翻訳者" ::translator
    "編者" ::editor
    "校訂者" ::proofreader
    "その他" ::other-author))

(defn to-integer [s]
  (Integer/parseInt s))

(defn to-uri
  "Converts string to URL (uri name kept to align with uri?)."
  [s]
  (if s (URL. s)))

(def japan-modern-period-map
  {"明治" 1868
   "大正" 1912
   "昭和" 1926
   "平成" 1989
   "令和" 2019})
(def year-rx [:repeat :digit 3 4])
(def day-or-month-rx [:repeat :digit 1 2])
(def simple-date-rx
  (regal/regex [:cat
                [:capture year-rx]
                [:? "-"]
                [:? [:capture day-or-month-rx]]
                [:? "-"]
                [:? [:capture day-or-month-rx]]]))
(def japanese-date-rx
  (regal/regex [:cat
                [:capture (into [:alt] (keys japan-modern-period-map))]
                [:capture day-or-month-rx]
                [:? "）"]
                \年
                [:? [:capture day-or-month-rx]]
                [:? "月"]]))

(defn to-date [s]
  (if-let [[_ year month day] (first (re-seq simple-date-rx s
                                             #_#"(\d{3,4})-?(\d{1,2})?-?(\d{1,2})?" #_s))]
    (try
      (cond
        (and year month day) (time/local-date (to-integer year) (to-integer month) (to-integer day))
        (and year month) (time/local-date (to-integer year) (to-integer month))
        year (time/local-date (to-integer year))
        #_:else #_[s _ year month day])
      (catch Exception e
        (throw (Exception. (format "%s" [(ex-data e) (seq s) (re-seq simple-date-rx s) s])))))))

#_(s/fdef string-date-helper
    :args (s/cat :year string? :month (s/nilable string?) :day (s/nilable string?))
    :ret ::date-time)

(defn string-date-helper [year month day]
  (cond
    (and year month day) (time/local-date (to-integer year) (to-integer month) (to-integer day))
    (and year month) (time/local-date (to-integer year) (to-integer month))
    year (time/local-date (to-integer year))))

;; FIXME: Example below list several dates, meaning we should change this to be optionally a collection of dates...
;; 推古時代における仏教受容の仕方について「思想」1922（大正11）年7月<br>仏像の相好についての一考察「思想」1922（大正11）年5月<br>『万葉集』の歌と『古今集』の歌との相違について「思想」1922（大正11）年8月<br>お伽噺としての『竹取物語』「思想」1922（大正11）年11月<br>『枕草紙』について「思想」1922（大正11）年9月<br>『枕草紙』の原典批評についての提案「思想」1922（大正11）年9月<br>『源氏物語』について「思想」1922（大正11）年12月<br>「もののあはれ」について「思想」1922（大正11）年10月<br>歌舞伎劇についての一考察「思想」1922（大正11）年4月
;; FIXME アーヴィング -> p001257 -> dateOfBirth/Death missing month and day!? maybe a serialization problem. Also, it should be a date and not date-time object in the first place.
#_(s/fdef aozora-to-date
    :args (s/cat :s string?)
    :ret (s/nilable ::date-time))                           ;; also (s/coll-of ::date-time)
(defn aozora-to-date [s]
  (try
    (cond
      (or (= "不詳" s) (re-seq #"^\s*$" s)) nil

      (= "「太陽　創刊号」" s) (time/local-date 1963 7)

      (= "紀元前6世紀初" s) (time/local-date -600)

      (not (re-seq #"\d{4}" s))
      (let [match (first (re-seq japanese-date-rx s))]
        (if (seq match)
          (let [[_ period period-year month] match
                year (+ (japan-modern-period-map period) (dec (to-integer period-year)))]
            (string-date-helper (str year) month nil))))

      :else
      (let [match (first (re-seq #"(\d{3,4})年?([\(（][^)）]+[）\)])?年?\s?((\d{1,2})|(\d{1,2})[～、]\d{1,2})?月?((\d{1,2})日)?" s))]
        (if (seq match)
          (let [[_ year _ _ month _ _ day] match]
            (string-date-helper year month day))
          (throw (Exception. (format "%s :: %s :: %s :: %s" s match (seq s) (re-seq #"\d{3,4}.+年" s)))))))
    (catch
      Exception
      e
      (println (ex-data e)
               s
               (re-seq japanese-date-rx s)
               (re-seq #"(\d{3,4})年?([\(（][^)）]+[）\)])?年?\s?((\d{1,2})|(\d{1,2})[～、]\d{1,2})?月?((\d{1,2})日)?" s)))))


(defn flag-to-boolean [s]
  (case s
    "なし" false
    "あり" true))

(defn to-ndc [s]
  (when-let [match (seq (first (re-seq #"NDC (K)?(\d{3})\s?(K)?(\d{3})?" s)))]
    (let [[_ a-child? a b-child? b] match]
      (if (and a b)
        (set/union                                          ;; TODO dcndl:NDC9 (Aozora Bunko is not updated to NDC10, but maybe we could replace with LOD from Web NDL Authorities)
          #{(cond-> {:abc.aozora.ndc/category (ndc-map a)}
                    a-child? (assoc :abc.aozora.ndc/children true))}
          #{(cond-> {:abc.aozora.ndc/category (ndc-map b)}
                    b-child? (assoc :abc.aozora.ndc/children true))})
        #{(cond-> {:abc.aozora.ndc/category (ndc-map a)}
                  a-child? (assoc :abc.aozora.ndc/children true))}))))

(m/=> to-ndc
      [:=>
       [:cat :string]
       [:schema {:registry registry} [:maybe ::NDC]]])

(defn to-encoding [s]
  (case s
    "ShiftJIS" "SJIS"
    "EUC" "EUC"
    "UTF-8" "UTF-8"))

(defn to-multiple
  ([s] (string/split s #"、"))
  ([prefix s]
   (mapv (fn [p] {prefix p}) (string/split s #"、"))))

(defn as-subject [t id]
  (assoc t :rdf/about id))

(defn as-object [t id]
  (assoc t :rdf/resource id))

(defn compact
  [coll]
  (cond
    (map? coll)
    (let [x (into {} (filter (comp not nil? second) coll))]
      (if (seq x) x))
    :else coll))

(defn remove-nils [coll]
  (clojure.walk/postwalk compact coll))

(defn remove-nils-vec [v]
  (filterv (complement nil?) v))

(defn to-id [s]
  (if (url? s)
    s
    (keyword "abc.aozora" s)))

(defn to-xtdb-id [s]
  (if (url? s)
    s
    (keyword "abc.aozora" s)))

(defn to-subject [subj m]
  (remove-nils
    (assoc m :rdf/about (to-id subj)
             :xt/id (to-xtdb-id subj))))

(defn record-to-entities [m]
  (let [g (partial get m)

        work-id (str "w" (g "作品ID"))                        ;; -> (g "図書カードURL")
        person-id (str "p" (g "人物ID"))                      ;; ->
        text-resource-id (to-uri (g "テキストファイルURL"))
        html-resource-id (to-uri (g "XHTML/HTMLファイルURL"))
        reference-1-id (g "底本名1")
        reference-2-id (g "底本名2")

        relation (g "役割フラグ")
        relation-kw (relation-to-work-rdf relation)

        text-resource
        (if text-resource-id
          (to-subject
            text-resource-id
            {::url                text-resource-id
             :dcterms/format      "text/plain"
             ::revision-count     (to-integer (g "テキストファイル修正回数"))
             ::character-set      (g "テキストファイル文字集合")
             ::last-modified-date (to-date (g "テキストファイル最終更新日"))
             ::encoding           (to-encoding (g "テキストファイル符号化方式"))}))

        html-resource
        (if html-resource-id
          (to-subject
            html-resource-id
            {::url                html-resource-id
             :dcterms/format      "text/html"
             ::revision-count     (to-integer (g "XHTML/HTMLファイル修正回数"))
             ::character-set      (g "XHTML/HTMLファイル文字集合")
             ::last-modified-date (to-date (g "XHTML/HTMLファイル最終更新日"))
             ::encoding           (to-encoding (g "XHTML/HTMLファイル符号化方式"))}))

        reference-1
        (if reference-1-id
          (to-subject
            reference-1-id
            {::title           reference-1-id
             ::first-published (some-> (g "底本初版発行年1") aozora-to-date)
             ::publisher       (some->> (g "底本出版社名1") to-multiple)
             ::parent          {::title           (g "底本の親本名1")
                                ::publishing-span (some-> (g "底本の親本初版発行年1") aozora-to-date) #_TODO
                                ::publisher       (some->> (g "底本の親本出版社名1") to-multiple)}}))

        reference-2
        (if reference-2-id
          (to-subject
            reference-2-id
            {::title           reference-2-id
             ::first-published (some-> (g "底本初版発行年2") aozora-to-date)
             ::publisher       (some->> (g "底本出版社名2") to-multiple)
             ::parent          {::title           (g "底本の親本名2")
                                ::publishing-span (some-> (g "底本の親本初版発行年2") aozora-to-date) #_TODO
                                ::publisher       (some->> (g "底本の親本出版社名2") to-multiple)}}))

        work
        (if work-id
          (to-subject
            work-id
            {::work-id                   (to-id work-id)
             relation-kw                 [(to-id person-id)]
             ::sources                   (cond-> []
                                                 text-resource (conj text-resource)
                                                 html-resource (conj html-resource))
             ::references                (cond-> []
                                                 reference-1 (conj reference-1)
                                                 reference-2 (conj reference-2))
             ::title                     (g "作品名")
             ::transcription             (g "作品名読み")
             ::subtitle                  (g "作品名読み")
             ::subtitle-transcription    (g "副題読み")
             ::original-title            (g "原題")
             ::copyright-expired         (flag-to-boolean (g "作品著作権フラグ"))
             ::transcriber               (g "入力者")
             ::aozora-publishing-date    (to-date (g "公開日"))
             ::NDC                       (some-> (g "分類番号") to-ndc)
             ::first-published           (some-> (g "初出") aozora-to-date)
             ::bib-resource              (to-uri (g "図書カードURL"))
             ::orthographic-style        (g "文字遣い種別")
             ::aozora-last-modified-date (to-date (g "最終更新日"))
             ::revisor                   (g "校正者")
             ::revision-source           (remove-nils-vec [(g "校正に使用した版1") (g "校正に使用した版2")])
             ::transcription-source      (remove-nils-vec [(g "入力に使用した版1") (g "入力に使用した版2")])}))

        person
        (if person-id
          (to-subject
            person-id
            {::person-id                    (to-id person-id)
             (inverse-relation relation-kw) [(to-id work-id)]
             ::person-copyright-expired     (flag-to-boolean (g "人物著作権フラグ")) ;; FIXME How to deal with Tsurayuki being set to false? (Because of recently published word/rendition...) Should we rather set this to a rule-based (date of death) value?
             ::given-name                   (g "名")
             ::given-name-romaji            (g "名ローマ字")
             ::given-name-transcription     (g "名読み")
             ::family-name                  (g "姓")
             ::family-name-romaji           (g "姓ローマ字")
             ::family-name-transcription    (g "姓読み")
             ::date-of-death                (some-> (g "没年月日") aozora-to-date)
             ::date-of-birth                (some-> (g "生年月日") aozora-to-date)}))]
    (remove-nils
      {::work   work
       ::person person})))

(m/=> record-to-entities
      [:=>
       [:cat [:map-of :string :string]]
       [:schema {:registry registry} ::db-entry]])

(defn merge-entities
  "Joins a sequence of works and person records on work and person ids, and returns a map of each, keyed to work-ids and
  person-ids, respectively."
  [entities]
  (reduce
    (fn [a {:keys [abc.aozora/work abc.aozora/person]}]
      (let [{:keys [#_abc.aozora/author
                    #_abc.aozora/translator
                    #_abc.aozora/editor
                    #_abc.aozora/proofreader
                    #_abc.aozora/other-author
                    abc.aozora/work-id]} work
            {:keys [#_abc.aozora/author-of
                    #_abc.aozora/translator-of
                    #_abc.aozora/editor-of
                    #_abc.aozora/proofreader-of
                    #_abc.aozora/other-author-of
                    abc.aozora/person-id]} person]
        (-> a
            (update-in [:works work-id]
                       (partial
                         merge-with
                         (fn [w _]
                           (cond
                             (nil? w) work
                             (= w _) w
                             :else ((fnil conj []) w))))
                       work)
            (update-in [:persons person-id]
                       (partial
                         merge-with
                         (fn [p _]
                           (cond
                             (nil? p) person
                             (= p _) p
                             :else ((fnil conj []) p))))
                       person))))
    {:works   {}
     :persons {}}
    entities))

(m/=> merge-entities
      [:=>
       [:cat [:schema {:registry registry} ::db-entry]]
       [:schema {:registry registry} ::db-entries]])
