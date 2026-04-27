(ns abc.annotation
  (:require [malli.core :as m]
            [abc.annotation.schema :as annotation-schema]
            [abc.text :as text]
            [clj-mecab.parse :as mecab]
            [clojure.string :as str]
            [abc.aozora :as aozora]
            [abc.stats :as stats]
            [clojure.java.io :as io]
            [net.cgrand.xforms :as x]
            [net.cgrand.xforms.rfs :as rf]
            [clojure.core.match :refer [match]]
            [lambdaisland.regal :as regal]
            [clojure.string :as string]
            [taoensso.timbre :as timbre]
            [clojure.tools.logging :as log])
  (:import [java.lang StringBuilder]))

(def registry annotation-schema/registry)

(defn oov? [m]                                              ;; FIXME
  (= "未知" (:mecab.features/goshu m)))

(defn has-quotation? [s]
  (or false))

(defn katakana-sentence? [s]
  (or true))

(defn- resource-lines
  [resource-name & {:keys [required?] :or {required? true}}]
  (if-let [resource (io/resource resource-name)]
    (with-open [f (io/reader resource)]
      (doall (line-seq f)))
    (if required?
      (throw (ex-info (str "Required annotation resource is missing: " resource-name)
                      {:resource resource-name}))
      (do
        (timbre/debug "Optional annotation resource is missing:" resource-name)
        []))))

;; TODO make this deferable so only created when really needed.
(def jis-unicode-map
  (let [hex-to-code (zipmap (map #(format "%X" %) (range 33 (+ 33 95)))
                            (map #(format "%02d" %) (range 1 95)))
        jis-map (into {}
                      (comp (remove (fn [s] (= \# (first s))))
                            (map (fn [s]
                                   (let [[jis-field unicode-field & _] (string/split s #"\t")
                                         [jis-standard jis-code] (string/split jis-field #"-")
                                         men (case jis-standard "3" 1 "4" 2)
                                         ku (hex-to-code (subs jis-code 0 2))
                                         ten (hex-to-code (subs jis-code 2 4))
                                         unicode-point (string/replace unicode-field "U+" "")]
                                     (if (empty? unicode-point)
                                       nil
                                       (let [->unicode (fn [s] (String. (Character/toChars (Integer/parseInt s 16))))
                                             unicode-points (string/split unicode-point #"\+")
                                             unicode-char (apply str (map ->unicode unicode-points))
                                             jis-string (format "%s-%s-%s" men ku ten)]
                                         [jis-string unicode-char])))))
                            (map identity)) ; Remove nil unicode-points.
                      (resource-lines "jisx0213-2004-std.txt"))]
    jis-map
    #_(merge jis-map
             (reduce
              (fn [a [jis-string unicode-char]]
                (let [[men ku ten] (string/split jis-string #"-")]
                  (if (= men "1")
                    (assoc a (format "%s-%s" ku ten) unicode-char)
                    a)))
              {}
              jis-map))))

;; We need a second mapping for gaiji not covered by above jis-map
(def gaiji-map
  ;; TODO This file is a bit outdated, and we should just add the missing mappings where possible.
  ;; https://raw.githubusercontent.com/cjkvi/cjkvi-data/master/aozora_gaiji_chuki.txt
  ;; Note that there is a formatting error on line containing "!!!" (spaces should be replaced with \tab).
  (into {}
        (comp (remove (fn [s] (= \# (first s))))
              (remove empty?)
              (map (fn [s]
                     (let [[category-type ucs ivs c gaiji-text comment] (string/split s #"\t")
                           ;; gaiji-text includes annotation markers, so we exclude them if present to match against TEXT
                           gaiji-text (-> gaiji-text (string/replace "※［＃" "") (string/replace "［＃" ""))
                           gaiji-text (if (= \］ (last gaiji-text)) (subs gaiji-text 0 (dec (count gaiji-text))))]
                       #_(when-not gaiji-text
                           (timbre/error "gaiji-text" [category-type ucs ivs c gaiji-text comment]))
                       (when (empty? c)
                         (timbre/error "no replacement offered" [category-type ucs ivs c gaiji-text comment]))
                       [gaiji-text
                        (if-not (empty? c)
                          c
                          (cond (re-seq #"ローマ数字" gaiji-text)
                                (let [numeral (first (re-seq #"\d+" gaiji-text))]
                                  (case numeral
                                    "13" "ⅩⅢ"
                                    "14" "ⅩⅣ"
                                    "15" "ⅩⅤ"))

                                (re-seq #"小書き" gaiji-text)
                                (subs gaiji-text (dec (dec (count gaiji-text))) (dec (count gaiji-text)))

                                (re-seq #"黒丸Ａ" gaiji-text) "🅐"
                                (re-seq #"黒丸Ｚ" gaiji-text) "🅩"

                                :else nil))]))))
        (resource-lines "aozora_gaiji_chuki.txt" :required? false)))

;; Custom Aozora Bunko parser

(defn- append
  ([^StringBuilder sb c] (.append sb (char c)))
  ([] (StringBuilder.)))

(defn parse-annotation-string [s]
  (let [lhs (subs s 0 (str/index-of s \=))]
    (transduce (comp (filter #(Character/isAlphabetic (int %)))
                     (map #(if (Character/isUpperCase (char %))
                             (Character/toLowerCase (char %))
                             (Character/toUpperCase (char %)))))
               (completing append str)
               lhs)))

;; https://www.aozora.gr.jp/annotation/etc.html

(defn kanji? [c]
  (let [cp (.codePointAt ^String c 0)]
    (or (<= 0x4e00 cp 0x9fff)
        ;; "仝々〆〇ヶ"
        (= 20189 cp)
        (= 12293 cp)
        (= 12294 cp)
        (= 12295 cp)
        (= 12534 cp))))

(defn katakana? [c]
  (<= 0x30a0 (.codePointAt ^String c 0) 0x30ff))

(defn hiragana? [c]
  (<= 0x3041 (.codePointAt ^String c 0) 0x309f))

(defn romaji? [c]
  (let [cp (.codePointAt ^String c 0)]
    (or (<= 65 cp 122)                                      ; half-width alphabet (A-Za-z)
        (<= 65313 cp 65370)                                 ; full-width alphabet (Ａ-Ｚａ-ｚ)
        (<= 48 cp 57)                                       ; half-width numbers  (0-9)
        (<= 65296 cp 65305))))                              ; full-width numbers  (０-９)

(defn parse-annotation [lhs ruby prefix s]
  (let [prefix-map {"※［＃" :gaiji
                    "《"   :ruby
                    "［＃"  :annotation-2
                    "［"   :annotation}]
    #_(println {:lhs lhs :ruby ruby :prefix prefix :s s})
    (case (prefix-map prefix)
      :ruby (let [ruby (if-not ruby lhs ruby)]
              (when (empty? ruby)
                (throw (Exception. (pr-str ["RUBY EXCEPTION:" lhs ruby prefix s]))))
              {:annotation/type   :ruby
               :ruby/reading      s
               :annotation/target (let [last-char (subs ruby (dec (count ruby)))
                                        test-fn (cond (kanji? last-char) kanji?
                                                      (hiragana? last-char) hiragana?
                                                      (katakana? last-char) katakana?
                                                      (romaji? last-char) romaji?
                                                      ;; FIXME
                                                      :else identity)]
                                    (->> ruby reverse (map str) (take-while test-fn) reverse string/join))})
      :gaiji (jis-unicode-map (second (first (re-seq #"(\d-\d+-\d+)" s))))
      {:annotation/type    (prefix-map prefix)
       :annotation/content s})))

(def annotation-rx
  (let [non-annotation-rx [:capture [:* (into [:not] (map str "《》［］※｜"))]]
        ruby-rx [:? [:capture "｜" [:* (into [:not] (map str "《》［］※"))]]]
        opening-rx [:capture [:? "※"] [:class "《" "［"] [:? "＃"]]
        annotation-content-rx [:capture [:+ [:not "］" "》"]]]
        closing-rx [:capture [:class "］" "》"]]]
    (regal/regex [:cat
                  non-annotation-rx                         ; left
                  ruby-rx                                   ; ruby
                  opening-rx                                ; open-sym
                  annotation-content-rx                     ; tag
                  closing-rx                                ; close-sym
                  non-annotation-rx])))                     ; right

(def matching-metachars
  {"［"   "］"
   "《"   "》"
   "［＃"  "］"
   "※［＃" "］"})

(comment
  ;; Why is this slower?
  )
(defn re-split
  [^java.util.regex.Pattern re s]
  (let [m (re-matcher re s)]
    (loop [r (transient [])]
      (if-not (.find m)
        (persistent! r)
        (recur (conj! r (.group m)))))))

(def ^:const open-gaiji "※［＃")
(def ^:const open-anno "［＃")
(def ^:const open-ruby "《")
(def ^:const open-quot "「")
(def ^:const open-bracket "［")
(def ^:const ruby-bar "｜")

(def matching-meta
  {open-gaiji   "］"
   open-anno    "］"
   open-ruby    "》"
   open-quot    "」"
   open-bracket "］"
   ruby-bar     "》"})

(defn annotation-lexer [s]
  (re-split #"(?:※［＃|［＃|《|［|］|》|｜|※|＃|「|」|[^《》［］「」※｜＃]+)" s))

(defn parse-ruby [xs prev]
  #_(timbre/debug [prev (->> xs (remove (partial = ruby-bar)) (take-while (partial not= open-ruby)) string/join)])
  (let [reading (->> xs (drop-while (partial not= open-ruby)) (drop 1) drop-last string/join)
        fragment (let [target (->> xs (remove (partial = ruby-bar)) (take-while (partial not= open-ruby)) string/join)]
                   (if (or (not target) (empty? target))
                     (let [last-char (subs prev (dec (count prev)))
                           test-fn (cond (kanji? last-char) kanji?
                                         (hiragana? last-char) hiragana?
                                         (katakana? last-char) katakana?
                                         (romaji? last-char) romaji?
                                         ;; FIXME
                                         :else identity)]
                       (->> prev reverse (map str) (take-while test-fn) reverse string/join))
                     target))]
    {:sentence/fragment   fragment
     :fragment/annotation {:annotation/type :ruby
                           :ruby/reading    reading
                           #_(let [last-char (subs ruby (dec (count ruby)))
                                   test-fn (cond (kanji? last-char) kanji?
                                                 (hiragana? last-char) hiragana?
                                                 (katakana? last-char) katakana?
                                                 (romaji? last-char) romaji?
                                                 ;; FIXME
                                                 :else identity)]
                               (->> ruby reverse (map str) (take-while test-fn) reverse string/join))}}))

(defn reformat-jis [s]
  (when s
    (let [fields (string/split s #"-")]
      (if (= (count fields) 2)
        (apply format "%s-%s" (map (fn [x] (if (= (count x) 1) (str "0" x) x)) fields))
        s))))

(defn parse-gaiji [xs]
  (let [gaiji (jis-unicode-map
               (some (fn [s]
                       (->> s (re-seq #"(\d-\d+-\d+|\d+-\d+)") first second #_reformat-jis))
                     (flatten xs)))]
    (if-not gaiji
      (timbre/error "Gaiji not found:" xs)
      [gaiji])))

(defn parse-other [xs tag]
  #:annotation{:type    (if (= tag open-anno) :annotation-2 :annotation)
               :content (string/join (->> xs rest drop-last))})

(defn parse-quotation [xs]
  {:sentence/fragment   (string/join xs)
   :fragment/annotation #:annotation{:type :quotation}})

(defn annotation-parser [xs & {:keys [nested?] :or {nested? false}}]
  (loop [input xs
         stack []
         output []]
    (let [c (first input)
          s (first stack)
          end? (get matching-meta s)
          begin? (get matching-meta c)]
      (timbre/debug begin? end? {:c c :s-end s :output output :stack stack})
      (cond
        ;; Termination condition
        (nil? c)
        (if (empty? stack) output (conj output stack))

        ;; FIXME temporary fix; we always want to return after first nested block processed
        (and nested? (> (count output) 1)) output

        ;; With empty stack we always append to stack
        (nil? s)
        (recur (subvec input 1) (conj stack c) output)

        ;; Termination of stack based on input matching stack end string
        (= c end?)
        (recur (subvec input 1) [] (conj output (conj stack c)))

        ;; Ruby bar symbol is an exception where we do not want to move to a new stack until ruby end is found
        (and (= s ruby-bar) begin?)
        (recur (subvec input 1) (conj stack c) output)

        ;; Recur function when nested annotation found, appending to the stack
        (and begin? end?) (let [nested-annotations (first (annotation-parser input :nested? true))]
                            #_(timbre/debug nested-annotations)
                            (recur (subvec input (count nested-annotations))
                                   (conj stack nested-annotations)
                                   output))

        ;; New stack based on input string (begin? indicates opening match)
        begin? (recur (subvec input 1) [c] (conj output stack))

        ;; Otherwise append to stack
        :else (recur (subvec input 1) (conj stack c) output)))))

(defn find-text [xs]
  (->> xs reverse (some (fn [x] (string? x) x))))

(defn tag-parser [ast]
  (->> ast
       (reduce
        (fn [a x]
          (let [tag (first x)
                parsed-tag (condp = tag
                             open-gaiji (parse-gaiji x)
                             open-anno (parse-other x tag)
                             open-ruby (parse-ruby x (some-> a peek peek))
                             open-quot (parse-quotation x)
                             open-bracket (parse-other x tag)
                             ruby-bar (parse-ruby x nil #_(some-> a peek peek))
                             x)]
            (cond
              (= tag open-ruby) (conj (pop a) (assoc (peek a) (dec (count (peek a))) parsed-tag))
              (map? parsed-tag) (conj a parsed-tag)
              (nil? parsed-tag) a
              (not-empty a) (if (string? (first (peek a)))
                              (conj (pop a) (into (peek a) parsed-tag))
                              (conj a parsed-tag))
              :else (conj a parsed-tag))))
        [])
       (mapv (fn [x]
               (if (every? string? x)
                 (string/join x)
                 x)))))

(defn aozora-annotation->tags [s]
  (tag-parser (annotation-parser (annotation-lexer s))))

#_(defn aozora-annotation->tags [s]
    (let [annotations (let [matches (map (comp vec rest) (re-seq annotation-rx s))]
                        (if (= (count matches) 1)
                          matches
                          (let [n (count matches)]
                            (loop [i 1                      ; Needed to keep last rhs
                                   ms matches
                                   rhs nil
                                   r []]
                              (if-let [m (first ms)]
                                (recur (inc i)
                                       (rest ms)
                                       (nth m 5)
                                       (conj r (cond-> m
                                                 (< i n) (assoc 5 "")
                                                 rhs (update 0 str rhs))))
                                r)))))]
      (when-not (empty? annotations)
        (->> annotations
             (reduce
              (fn [v [left ruby open-sym tag close-sym right]]
                (if-not (= close-sym (get matching-metachars open-sym))
                   ;; If metachars do not match, discard this match
                  (do (println :1 [open-sym close-sym (get matching-metachars open-sym)] left ruby open-sym tag close-sym right) v)
                  (when-let [a (try (parse-annotation left ruby open-sym tag)
                                    (catch Exception e (do (timbre/debug "Annotation parse failure: " e "in string:" s)
                                                           nil)))]
                    (if (= :ruby (:annotation/type a))
                      (conj v
                            (if ruby left (string/replace left (:annotation/target a) ""))
                            {:sentence/fragment   (:annotation/target a)
                             :fragment/annotation (dissoc a :annotation/target)}
                            right)
                      (conj v left a right)))))
              [])
             (into [] (remove empty?))
             (reduce
              (fn [a x]
                (let [prev (peek a)]
                  (if (and (string? prev) (string? x))
                    (conj (pop a) (str prev x))
                    (conj a x))))
              [])))))

(defn remove-aozora-formatting
  "Removes content added by the Aozora Bunko project that is not present in the original.
  This information is already available in a more structured manner within the bibliographic data of the project."
  [s]
  (-> s
      ;; Front matter
      (str/replace #"(?s)^.+-{10,}[^-]*-{10,}\n+" "")
      ;; Back matter
      (str/replace #"(?s)\n[　【]?(?:底本：|訳者あとがき|この翻訳は|この作品.*翻訳|この翻訳.*全訳).*$" "")))

(defn sentence->plaintext
  [text-fragment]
  ;; :sentence/text is either a string or vector
  (if (string? text-fragment)
    text-fragment
    (string/join
     (map (fn [x]
            (if (map? x)
               ;; Only annotations with a sentence fragment are extracted
              (if-let [y (:sentence/fragment x)]
                y)
              x)) text-fragment))))

(defn doc->plaintext
  [doc]
  (transduce
   (comp (mapcat :document/paragraphs)
         (interpose "\n\n")
         (mapcat :paragraph/sentences)
         (interpose "\n\n")
         (map :sentence/text)
         (interpose "\n")
         (map sentence->plaintext))
   rf/str
   doc))

(defn parse-with-tags [sentence]
  (let [annotated-text (if-let [tagged-sentence (try (aozora-annotation->tags sentence)
                                                     (catch Exception e (do (timbre/error sentence e)
                                                                            (throw e))))]
                         tagged-sentence
                         sentence)
        plaintext (sentence->plaintext annotated-text)]
    {:sentence/tags           #{}
     :sentence/annotated-text annotated-text
     :sentence/text           plaintext
     :sentence/tokens         (mapv (fn [m position] (assoc m :mecab.features/position position))
                                    (mecab/parse-sentence plaintext)
                                    (range))}))

(defn add-tags [paragraphs]
  (into []
        (map (fn [paragraph]
               (let [sentences (mapv parse-with-tags paragraph)]
                 (timbre/debug (count sentences))
                 {:paragraph/tags      #{}
                  :paragraph/sentences sentences}))
             paragraphs)))

;; FIXME validate this.
(defn lines->paragraph-sentences
  "Splits string into paragraphs and sentences.
   Paragraphs are detected using the passed in `paragraph-split-fn` and are by default defined as:
   1) one or more non-empty lines delimited by one empty line or BOF/EOF
   2) lines prefixed with fullwidth unicode space '　'"
  ([lines]
   (lines->paragraph-sentences lines #(or (nil? %) (empty? %) (= (subs % 0 1) "　"))))
  ([lines paragraph-split-fn]
   (into []
         (comp
           ;; Partition by paragraph (empty line or indented line (common in BCCWJ)).
          (partition-by paragraph-split-fn)
          (map (fn [paragraphs]
                 (into []
                       (comp (filter identity)
                             (remove empty?)
                             (map text/split-japanese-sentence)
                             (map #(mapv string/trim %)))
                       paragraphs)))
          (remove (partial every? empty?))
          (mapcat identity))                               ; Remove paragraph boundaries.
         lines)))

(defn parse-paragraphs [s]
  (->> s
       ;; Aozora Bunko is already normalized.
       ;; Validated as having no effect:
       #_text/normalize-nfkc
       #_text/convert-half-to-fullwidth
       str/split-lines
       lines->paragraph-sentences
       add-tags))                                           ;; TODO Might need to define split-fn

(defn parse-text
  ([s]
   (let [paragraphs (-> s
                        remove-aozora-formatting
                        parse-paragraphs)
         stats (stats/stylometric-measures paragraphs)]
     (timbre/debug paragraphs stats)
     {:document/paragraphs paragraphs
      :document/metadata   stats}))
  ([s work-id]
   (let [doc (parse-text s)]
     (assoc-in doc [:document/metadata ::aozora/work-id] work-id))))

(m/=> parse-text
      [:=>
       [:cat :string]
       [:schema {:registry registry} :document]])
