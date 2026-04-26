(ns abc.text
  (:require [clojure.core.reducers :as r]
            [clojure.string :as string])
  (:import [com.ibm.icu.text Normalizer Transliterator]))

(defonce half-to-fullwidth
  (Transliterator/getInstance "Halfwidth-Fullwidth"))

(defn convert-half-to-fullwidth
  [^String s]
  (.transliterate ^Transliterator half-to-fullwidth s))

(defn normalize-nfkc
  [^String s]
  (Normalizer/normalize s Normalizer/NFKC))

(def delimiter #"[\.!\?．。！？]")
(def closing-quotation #"[\)）」』】］〕〉》\]]")
(def alphanumerics #"[\d０-９a-zA-Zａ-ｚＡ-Ｚ]")

(defn codepoint-range->string [codepoints]
  (string/join (for [codepoint codepoints] (char codepoint))))

(def delimiter-set (set (vec (str delimiter))))
(def alphanumerics-set (set (vec (str "0123456789"
                                      (codepoint-range->string (range 65 123))
                                      (codepoint-range->string (range 65313 65371))
                                      (codepoint-range->string (range 65296 65306))))))
(def number-set (set (vec (str "0123456789" (codepoint-range->string (range 65296 65306))))))
(def closing-quotation-set (set (vec (str closing-quotation))))

(defn split-japanese-sentence
  "Splits a string on Japanese sentence boundaries."
  [s]
  (->> s
       reverse
       vec
       (r/reduce
        (fn
          ([] [])
          ([a x]
           (let [y (peek a)
                 z (and y (peek (pop a)))]
             (if (and y z
                      (delimiter-set y)
                      (not (or (and (alphanumerics-set x)
                                    (not= \。 y)
                                    (alphanumerics-set z))
                               (and (number-set x)
                                    (not= \。 y))
                               (closing-quotation-set z)
                               (delimiter-set z))))
               (conj (pop (pop a)) z \newline y x)
               (conj a x))))))
       reverse
       string/join
       string/split-lines))
