(ns abc.aozora
  (:require [clojure.spec.alpha :as s]
            [expound.alpha :as expound]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.data]
            [java-time :as time]
            [corpus-utils.kokken :refer [ndc-map]])
  (:import [java.time LocalDate]
           [java.net URL]))

(defn url? [u] (instance? URL u))
(s/def ::url (s/with-gen
               #(instance? URL %)
               #(s/gen #{(URL. "http://some.url.com/")})))
(s/def :dcterms/format #{"text/plain" "text/html" "text/xml"})
(s/def ::character-set #{"JIS X 0208" "Unicode"})
(s/def ::encoding #{"SJIS" "EUC" "UTF-8"})
(s/def ::revisionCount (s/and int? #(>= % 0)))
(s/def ::non-negative-integer (s/and int? #(>= % 0)))
(s/def ::non-empty-string (s/and string? not-empty))
(s/def ::date-time (s/with-gen
                     (s/nilable #(instance? LocalDate %))
                     #(s/gen #{(time/local-date "2010-01-01")})))
(defonce ndc-strings (set (vals ndc-map)))
(s/def :abc.aozora.ndc/category
  (s/with-gen
    (s/and string? #(contains? ndc-strings #_ndc-map %))
    #(s/gen ndc-strings #_(set (keys ndc-map)))))
(s/def :abc.aozora.ndc/childrens boolean?)
(s/def ::NDC (s/coll-of (s/keys :req [:abc.aozora.ndc/category] :opt [:abc.aozora.ndc/childrens])))
(s/def ::orthographic-style #{"新字新仮名"
                              "新字旧仮名"
                              "旧字新仮名"
                              "旧字旧仮名"
                              "その他"})
(s/def ::relation-to-work #{"著者" "翻訳者" "編者" "校訂者" "その他"})

(defn inverse-relation [kw]
  (keyword (namespace kw) (str (name kw) "-of")))

(defn relation-to-work-rdf [s]
  (case s
    "著者"   ::author
    "翻訳者" ::translator
    "編者"   ::editor
    "校訂者" ::proofreader
    "その他" ::other-author))

(defn to-integer [s]
  (Integer/parseInt s))

(defn to-uri
  "Converts string to URL (uri name kept to align with uri?)."
  [s]
  (if s (java.net.URL. s)))

(s/fdef to-date
  :args (s/cat :s string?)
  :ret ::date-time)

(defn to-date [s]
  (if-let [[_ year month day] (first (re-seq #"(\d{3,4})-?(\d{1,2})?-?(\d{1,2})?" s))]
    (cond
      (and year month day) (time/local-date (to-integer year) (to-integer month) (to-integer day))
      (and year month)     (time/local-date (to-integer year) (to-integer month))
      year                 (time/local-date (to-integer year))
      #_:else #_[s _ year month day])
    (throw (Exception. (format "%s" [(seq s) (re-seq #"(\d{3,4})-?(\d{1,2})?-?(\d{1,2})?" s) s])))))

(s/fdef string-date-helper
  :args (s/cat :year string? :month (s/nilable string?) :day (s/nilable string?))
  :ret ::date-time)

(defn string-date-helper [year month day]
  (cond
    (and year month day) (time/local-date (to-integer year) (to-integer month) (to-integer day))
    (and year month) (time/local-date (to-integer year) (to-integer month))
    year (time/local-date (to-integer year))))

(def japan-modern-period-map
  {"明治" 1868
   "大正" 1912
   "昭和" 1926
   "平成" 1989})

(s/fdef aozora-to-date
  :args (s/cat :s string?)
  :ret (s/nilable ::date-time))

(defn aozora-to-date [s]
  (cond
    (or (= "不詳" s) (re-seq #"^\s*$" s)) nil

    (= "「太陽　創刊号」" s) (time/local-date 1963 7)

    (= "紀元前6世紀初" s) (time/local-date -600)

    (not (re-seq #"\d{4}" s))
    (let [match (first (re-seq #"(明治|大正|昭和|平成)(\d{1,2})年(\d{1,2})?月?" s))]
      (if (seq match)
        (let [[_ period period-year month] match
              year (+ (japan-modern-period-map period) (dec (to-integer period-year)))]
          (string-date-helper (str year) month nil))))

    :else
    (let [match (first (re-seq #"(\d{3,4})年?([\(（][^)）]+[）\)])?年?\s?((\d{1,2})|(\d{1,2})[～、]\d{1,2})?月?((\d{1,2})日)?" s))]
      (if (seq match)
        (let [[_ year _ _ month _ _ day] match]
          (string-date-helper year month day))
        (throw (Exception. (format "%s :: %s :: %s :: %s" s match (seq s) (re-seq #"\d{3,4}.+年" s))))))))

(defn flag-to-boolean [s]
  (case s
    "なし" false
    "あり" true))

(s/fdef to-ndc
  :args (s/cat :s ::non-empty-string)
  :ret ::NDC)

(defn to-ndc [s]
  (if-let [match (seq (first (re-seq #"NDC (K)?(\d{3})\s?(K)?(\d{3})?" s)))]
    (let [[_ a-child? a b-child? b] match]
      (if (and a b)
        (set/union ;; TODO dcndl:NDC9 (Aozora Bunko is not updated to NDC10, but maybe we could replace with LOD from Web NDL Authorities)
         #{(cond-> {:abc.aozora.ndc/category (ndc-map a)}
             a-child? (assoc :abc.aozora.ndc/childrens true))}
         #{(cond-> {:abc.aozora.ndc/category (ndc-map b)}
             b-child? (assoc :abc.aozora.ndc/childrens true))})
        #{(cond-> {:abc.aozora.ndc/category (ndc-map a)}
            a-child? (assoc :abc.aozora.ndc/childrens true))}))))

(defn to-encoding [s]
  (case s
    "ShiftJIS" "SJIS"
    "EUC" "EUC"
    "UTF-8" "UTF-8"))

(defn to-multiple
  ([s] (string/split s #"、"))
  ([prefix s]
   (mapv (fn [p] {prefix p}) (string/split s #"、"))))

(s/def ::csv-string-map (s/map-of string? string?))

(s/def ::revision-count (s/and int? #(>= % 0)))
(s/def ::last-modified-date ::date-time)
(s/def ::source (s/keys :req [:dcterms/format :abc.aozora/url :abc.aozora/revision-count :abc.aozora/character-set :abc.aozora/last-modified-date :abc.aozora/encoding]))
(s/def ::sources (s/coll-of ::source))
(s/def ::text-resource ::source)
(s/def ::html-resource ::source)
(s/def ::work-id string?)
(s/def ::person-id string?)
(s/def ::title string?)
(s/def ::original-title string?)
(s/def ::revisor string?)
(s/def ::publisher (s/coll-of string?))
(s/def ::transcriber string?)
(s/def ::transcription string?)
(s/def ::subtitle string?)
(s/def ::subtitle-reading string?)
(s/def ::sort-reading string?)
(s/def ::copyright-expired boolean?)
(s/def ::person-copyright-expired boolean?)
(s/def ::revision-source (s/coll-of string?))
(s/def ::transcription-source (s/coll-of string?))
(s/def ::bib-resource ::url)

(s/def ::author (s/coll-of string?))
(s/def ::translator (s/coll-of string?))
(s/def ::editor (s/coll-of string?))
(s/def ::proofreader (s/coll-of string?))
(s/def ::other-author (s/coll-of string?))

(s/def ::author-of (s/coll-of string?))
(s/def ::translator-of (s/coll-of string?))
(s/def ::editor-of (s/coll-of string?))
(s/def ::proofreader-of (s/coll-of string?))
(s/def ::other-author-of (s/coll-of string?))

(s/def ::work (s/keys :req [::work-id] :opt [::author ::translator ::editor ::proofreader ::other-author ::NDC ::aozora-last-modified-date ::aozora-publishing-date ::bib-resource ::copyright-expired ::first-published ::original-title ::orthographic-style ::revision-source ::revisor ::sort-reading ::subtitle ::subtitle-reading ::title ::transcriber ::transcription-source]))
(s/def ::publishing-span ::date-time) ;; FIXME
(s/def ::first-published ::date-time)
(s/def ::aozora-publishing-date ::date-time)
(s/def ::aozora-last-modified-date ::date-time)
(s/def ::date-of-birth ::date-time)
(s/def ::date-of-death ::date-time)
(s/def ::reference (s/keys :opt [::publisher ::first-published ::publishing-span ::title]))
(s/def ::reference-1 ::reference)
(s/def ::reference-2 ::reference)
(s/def ::references (s/coll-of ::reference))
(s/def ::given-name string?)
(s/def ::given-name-romaji string?)
(s/def ::given-name-reading string?)
(s/def ::family-name string?)
(s/def ::family-name-romaji string?)
(s/def ::family-name-reading string?)
(s/def ::person (s/keys :req [::person-id] :opt [::author-of ::translator-of ::editor-of ::proofreader-of ::other-author-of ::date-of-birth ::date-of-death ::family-name ::family-name-reading ::family-name-romaji ::given-name ::given-name-reading ::given-name-romaji ::person-copyright-expired]))

(s/def ::entity-map
  (s/keys :opt [::work ::person ::sources ::references]))

#_(s/def ::indexed-entity-map (s/map-of #{}))

(s/def ::entity-coll (s/coll-of ::entity-map))

(s/def ::db-entry (s/keys :req [::work ::person]))

(s/def ::db-map (s/map-of #{:works :persons} (s/map-of string? ::entity-map)))

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

(defn to-subject [subj m]
  (remove-nils
   (assoc m :rdf/about
          (if (url? subj)
            subj
            (keyword "abc.aozora" subj)))))

(s/fdef record-to-entities
  :args (s/cat :m (s/map-of string? string?))
  :ret ::db-entry)
(defn record-to-entities [m]
  (let [g (partial get m)

        work-id          (str "w" (g "作品ID")) ;; -> (g "図書カードURL")
        person-id        (str "p" (g "人物ID")) ;; ->
        text-resource-id (to-uri (g "テキストファイルURL"))
        html-resource-id (to-uri (g "XHTML/HTMLファイルURL"))
        reference-1-id   (g "底本名1")
        reference-2-id   (g "底本名2")

        relation    (g "役割フラグ")
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
            :dcterms/format      "text/plain"
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
           {::work-id                   work-id
            relation-kw                 [person-id]
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
            ::copyright-expired            (flag-to-boolean (g "作品著作権フラグ"))
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
           {::person-id                    person-id
            (inverse-relation relation-kw) [work-id]
            ::person-copyright-expired     (flag-to-boolean (g "人物著作権フラグ"))
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

(s/fdef merge-entities
  :args (s/cat :entities ::entity-coll)
  :ret ::db-map)
(defn merge-entities [entities]
  (reduce
   (fn [a
        {:keys [abc.aozora/work
                abc.aozora/person]
         :as   m}]
     (let [{:keys [abc.aozora/author
                   abc.aozora/translator
                   abc.aozora/editor
                   abc.aozora/proofreader
                   abc.aozora/other-author
                   abc.aozora/work-id]}   work
           {:keys [abc.aozora/author-of
                   abc.aozora/translator-of
                   abc.aozora/editor-of
                   abc.aozora/proofreader-of
                   abc.aozora/other-author-of
                   abc.aozora/person-id]} person]
       (-> a
           (update-in [:works work-id]
                      (partial
                       merge-with
                       (fn [w _]
                         (cond
                           (nil? w) work
                           (= w _)  w
                           :else    ((fnil conj []) w))))
                      work)
           (update-in [:persons person-id]
                      (partial
                       merge-with
                       (fn [p _]
                         (cond
                           (nil? p) person
                           (= p _)  p
                           :else    ((fnil conj []) p))))
                      person))))
   {:works   {}
    :persons {}}
   entities))
