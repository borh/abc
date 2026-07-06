#!/usr/bin/env bb
;; Reads detect_sentences output JSONL, computes recall + precision, prints report.
(require '[cheshire.core :as json])

(defn -main [in-path]
  (let [recs (doall
               (for [line (line-seq (clojure.java.io/reader in-path))
                     :let [r (json/parse-string line true)]
                     :when r]
                 r))
        n (count recs)
        gold-pos (filter #(= "accept" (:gold %)) recs)
        gold-neg (filter #(= "reject" (:gold %)) recs)
        tp (count (filter :agree gold-pos))
        fn_ (count (filter #(not (:agree %)) gold-pos))
        fp (count (filter #(not (:agree %)) gold-neg))
        tn (count (filter :agree gold-neg))
        recall (if (zero? (count gold-pos)) 0.0 (/ tp (double (count gold-pos))))
        precision (let [pred-pos (filter #(= "accept" (:heuristic %)) recs)]
                    (if (zero? (count pred-pos)) 0.0 (/ tp (double (count pred-pos)))))
        f1 (if (zero? (+ recall precision)) 0.0
               (/ (* 2.0 recall precision) (+ recall precision)))]
    (println "=== Recall-floor report ===")
    (println "n:" n "gold_pos:" (count gold-pos) "gold_neg:" (count gold-neg))
    (println "tp:" tp "fn:" fn_ "fp:" fp "tn:" tn)
    (println "recall:" recall)
    (println "precision:" precision)
    (println "f1:" f1)
    {:n n :tp tp :fn fn_ :fp fp :tn tn :recall recall :precision precision :f1 f1}))

(when-let [in-path (first *command-line-args*)]
  (-main in-path))
