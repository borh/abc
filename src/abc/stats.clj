(ns abc.stats
  (:require
    [clojure.string :as string]
    [net.cgrand.xforms :as x]))

(defn yules-k
  "Measures distribution of tokens across types."
  [xs]
  (let [freqs (frequencies xs)
        spect (frequencies (vals freqs))
        N (count xs)]
    (* 10000.0 (- (reduce + (for [[freq freq-size] spect]
                              (* freq-size (Math/pow (/ freq N) 2.0))))
                  (/ 1 N)))))

(defn hapax [xs]
  (count (filter (fn [[_ f]] (= 1 f)) (frequencies xs))))

(defn ttr [xs]
  (double (/ (count (distinct xs))
             (count xs))))

(defn median [ns]
  (double
    (let [ns (sort ns)
          cnt (count ns)
          mid (bit-shift-right cnt 1)]
      (if (odd? cnt)
        (nth ns mid)
        (/ (+ (nth ns mid) (nth ns (dec mid))) 2)))))

(defn average [coll]
  (double (/ (reduce + coll) (count coll))))

(defn sttr [xs window-size]
  (if (> (count xs) window-size)
    (average (map ttr (partition window-size xs)))))

(defn- parse-sentence [s]
  ((requiring-resolve 'clj-mecab.parse/parse-sentence) s))

(defn compute-text
  "Tokenizes input text `s` and returns a map containing words "
  [s]
  (into {}
        (comp
          (map parse-sentence)
          (x/transjuxt {:tokens           (x/reduce (fn ([] []) ([a] a) ([a x] (x/into a (:mecab.features/orth x)))))
                        :sentence-lengths (x/reduce (fn ([] []) ([a] a) ([a x] (conj a (count x)))))}))
        (string/split s #"\n+")))

(defn doc-to-token-map [doc]
  (into {}
        (comp
          (mapcat :paragraph/sentences)
          (map :sentence/tokens)
          (x/transjuxt {:tokens           (x/reduce (fn ([] []) ([a] a) ([a x] (x/into a (map :mecab.features/orth) x))))
                        :sentence-lengths (x/reduce (fn ([] []) ([a] a) ([a x] (conj a (count x)))))}))
        doc))

(defn stylometric-measures [doc]
  (let [{:keys [tokens sentence-lengths]} (doc-to-token-map doc)
        sentence-lengths-median (when (pos? (count sentence-lengths))
                                  (median sentence-lengths))
        n (count tokens)
        characters (reduce + (map count tokens))]
    #:abc.stats{:characters              characters
                :tokens                  n
                :types                   (count (set tokens))
                :paragraphs              (count doc)
                :sentences               (count (mapcat :paragraph/sentences doc))
                :sentence-lengths-median sentence-lengths-median
                :hapax-legomenon         (hapax tokens)
                :yules-k                 (yules-k tokens)
                :sttr-500                (sttr tokens 500)}))
