(ns abc.tools.annotation-join-stats
  "Join-statistics core for the corpus-scale ruby/gaiji annotation run
  (design spec 2026-07-10, slice B). Pure: token-span reconstruction from a
  surface sequence, per-work classification statistics via
  abc.tools.annotation-join, and aggregation. Reconstructed spans are sorted
  and non-overlapping by construction (contiguous left-to-right walk), which
  is annotation-join/join's documented precondition."
  (:require [abc.tools.annotation-join :as annotation-join]
            [clojure.string :as string]))

(defn- scalar-count [^String s]
  (.codePointCount s 0 (.length s)))

(defn reconstruct-token-spans
  "Walks tokenizer surface forms over the rendered plaintext, assigning
  unicode-scalar input spans. Whitespace (incl. newlines) between tokens is
  skipped. Returns {:tokens [...]} on success or {:failure {:token-index i
  :offset scalar-offset :surface s}} on the first mismatch — failures are
  recorded, never papered over (spec B3)."
  [^String text surfaces]
  (let [scalars (vec (map #(String. (Character/toChars %))
                          (iterator-seq (.iterator (.codePoints text)))))
        total (count scalars)]
    (loop [offset 0
           token-index 0
           remaining (seq surfaces)
           tokens []]
      (if-not remaining
        {:tokens tokens}
        (let [surface (first remaining)
              width (scalar-count surface)
              slice (when (<= (+ offset width) total)
                      (apply str (subvec scalars offset (+ offset width))))]
          (cond
            (= slice surface)
            (recur (+ offset width)
                   (inc token-index)
                   (next remaining)
                   (conj tokens {"token_index" token-index
                                 "input_span" {"start" offset
                                               "end" (+ offset width)}
                                 "text" surface}))

            (and (< offset total)
                 (string/blank? (nth scalars offset)))
            (recur (inc offset) token-index remaining tokens)

            :else
            {:failure {:token-index token-index
                       :offset offset
                       :surface surface}}))))))

(defn work-stats
  "Classification statistics for one work: joins every annotation against
  the token spans and counts classifications per annotation kind."
  [{:keys [annotations tokens]}]
  (let [joined (annotation-join/join tokens annotations)
        by-kind (group-by #(get-in % ["annotation" "annotation_kind"]) joined)]
    {:annotation_counts (into {} (map (fn [[k v]] [k (count v)])) by-kind)
     :classifications (into {}
                            (map (fn [[k v]]
                                   [k (frequencies
                                       (map #(get % "classification") v))]))
                            by-kind)}))

(defn- merge-counts [maps]
  (apply merge-with + {} maps))

(defn aggregate
  "Aggregates per-work stats: summed counts plus per-kind classification
  rates (fractions of that kind's corpus-wide total, so a kind's rates sum
  to 1)."
  [work-stats-seq]
  (let [annotation-counts (merge-counts (map :annotation_counts work-stats-seq))
        kinds (keys annotation-counts)
        classifications (into {}
                              (map (fn [kind]
                                     [kind (merge-counts
                                            (keep #(get-in % [:classifications kind])
                                                  work-stats-seq))]))
                              kinds)
        rates (into {}
                    (map (fn [kind]
                           (let [total (get annotation-counts kind)]
                             [kind (into {}
                                         (map (fn [[c n]]
                                                [c (double (/ n total))]))
                                         (get classifications kind))])))
                    kinds)]
    {:work_count (count work-stats-seq)
     :annotation_counts annotation-counts
     :classifications classifications
     :classification_rates rates}))
