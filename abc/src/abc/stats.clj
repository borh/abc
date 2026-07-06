(ns abc.stats
  (:require
   [clojure.string :as string]
   [net.cgrand.xforms :as x]))

(defn yules-k
  "Measures distribution of tokens across types."
  [xs]
  (let [N (count xs)]
    (if (zero? N)
      0.0
      (let [freqs (frequencies xs)
            spect (frequencies (vals freqs))]
        (* 10000.0 (- (reduce + (for [[freq freq-size] spect]
                                  (* freq-size (Math/pow (/ freq N) 2.0))))
                      (/ 1 N)))))))

(defn hapax [xs]
  (count (filter (fn [[_ f]] (= 1 f)) (frequencies xs))))

(defn ttr [xs]
  (let [c (count xs)]
    (if (zero? c)
      0.0
      (double (/ (count (distinct xs)) c)))))

(defn median [ns]
  (let [cnt (count ns)]
    (if (zero? cnt)
      0.0
      (double
       (let [ns (sort ns)
             mid (bit-shift-right cnt 1)]
         (if (odd? cnt)
           (nth ns mid)
           (/ (+ (nth ns mid) (nth ns (dec mid))) 2)))))))

(defn average [coll]
  (let [c (count coll)]
    (if (zero? c)
      0.0
      (double (/ (reduce + coll) c)))))

(defn sttr [xs window-size]
  (if (> (count xs) window-size)
    (average (map ttr (partition window-size xs)))
    0.0))

;; Tokenization (mecab morpheme parsing) is done by the Rust ab-validator
;; parser, not in Clojure. compute-text now takes per-line token sequences and
;; computes sentence-length statistics over them; :tokens is the caller's
;; responsibility. See :mecab.features/* schema in abc.annotation.schema.
(defn compute-text
  "Given a map of line -> token sequence, returns sentence-length statistics."
  [lines->tokens]
  (into {}
        (comp
         (map (fn [[line tokens]]
                [line {:sentence-lengths (count tokens)}])))
        lines->tokens))

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
