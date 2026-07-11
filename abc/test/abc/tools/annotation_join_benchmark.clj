(ns abc.tools.annotation-join-benchmark
  (:require [abc.tools.annotation-join :as join]
            [abc.tools.annotation-join-stats :as stats]
            [abc.tools.files :as files]
            [abc.tools.parser-ir-plaintext :as plaintext]
            [clojure.string :as string]))

(defn- overlapping-exhaustive [tokens start end]
  (filterv (fn [token]
             (let [{token-start "start" token-end "end"}
                   (get token "input_span")]
               (and (< token-start end) (< start token-end))))
           tokens))

(defn- classify [cover start end]
  (let [first-span (get (first cover) "input_span")
        last-span (get (last cover) "input_span")
        start-aligned? (= start (get first-span "start"))
        end-aligned? (= end (get last-span "end"))]
    (cond
      (and start-aligned? end-aligned? (= 1 (count cover))) "aligned-single"
      (and start-aligned? end-aligned?) "aligned-multi"
      (and start-aligned? (not end-aligned?)) "stem-prefix"
      :else "conflict")))

(defn- join-exhaustive [tokens annotations]
  (mapv (fn [annotation]
          (let [{:strs [start end]} (get annotation "span")
                cover (if (>= start end)
                        []
                        (overlapping-exhaustive tokens start end))]
            {"annotation" annotation
             "token_indexes" (mapv #(get % "token_index") cover)
             "classification" (if (empty? cover)
                                "conflict"
                                (classify cover start end))}))
        annotations))

(defn- elapsed-ms [f]
  (let [started (System/nanoTime)
        result (f)]
    {:result result
     :elapsed-ms (/ (double (- (System/nanoTime) started)) 1e6)}))

(defn- median [values]
  (let [ordered (vec (sort values))]
    (nth ordered (quot (count ordered) 2))))

(defn- scalar-count [^String text]
  (.codePointCount text 0 (.length text)))

(defn- reconstruct-legacy-token-spans [^String text token-rows]
  (let [scalars (vec (map #(String. (Character/toChars %))
                          (iterator-seq (.iterator (.codePoints text)))))
        total (count scalars)]
    (loop [offset 0
           token-index 0
           remaining (seq token-rows)
           tokens []]
      (if-let [row (first remaining)]
        (let [surface (get row "surface")
              width (scalar-count surface)
              slice (when (<= (+ offset width) total)
                      (apply str (subvec scalars offset (+ offset width))))]
          (cond
            (= slice surface)
            (recur (+ offset width)
                   (inc token-index)
                   (next remaining)
                   (conj tokens {"token_index" token-index
                                 "input_span" {"start" offset "end" (+ offset width)}
                                 "text" surface}))

            (and (< offset total) (string/blank? (nth scalars offset)))
            (recur (inc offset) token-index remaining tokens)

            :else
            {:failure {:token-index token-index :offset offset :surface surface}}))
        {:tokens tokens}))))

(defn- load-work [parser-ir-path tokens-path]
  (let [parser-ir (files/read-json parser-ir-path)
        {:keys [text annotations]} (plaintext/render-with-annotations parser-ir)
        token-rows (vec (files/read-json-lines tokens-path))
        {:keys [tokens failure]}
        (if (contains? (first token-rows) "char_start")
          (stats/token-spans text token-rows)
          (reconstruct-legacy-token-spans text token-rows))]
    (when failure
      (throw (ex-info "Invalid benchmark token spans" failure)))
    {:work-id (.getName (.getParentFile (java.io.File. parser-ir-path)))
     :tokens tokens
     :annotations annotations}))

(defn- benchmark-work [{:keys [work-id tokens annotations]}]
  (let [optimized #(join/join tokens annotations)
        exhaustive #(join-exhaustive tokens annotations)
        exhaustive-run (elapsed-ms exhaustive)
        optimized-run (elapsed-ms optimized)]
    (when-not (= (:result exhaustive-run) (:result optimized-run))
      (throw (ex-info "Benchmark implementations disagree" {:work-id work-id})))
    (optimized)
    (let [optimized-ms (into [(:elapsed-ms optimized-run)]
                             (map (fn [_] (:elapsed-ms (elapsed-ms optimized))) (range 4)))
          exhaustive-ms (:elapsed-ms exhaustive-run)
          optimized-median (median optimized-ms)
          speedup (/ exhaustive-ms optimized-median)]
      {:work-id work-id
       :token-count (count tokens)
       :annotation-count (count annotations)
       :optimized-ms optimized-ms
       :exhaustive-ms exhaustive-ms
       :optimized-median-ms optimized-median
       :speedup speedup})))

(defn -main [& paths]
  (when (or (< (count paths) 4) (odd? (count paths)))
    (throw (ex-info "Expected at least two PARSER_IR TOKENS path pairs"
                    {:paths paths})))
  (doseq [[parser-ir-path tokens-path] (partition 2 paths)]
    (prn (benchmark-work (load-work parser-ir-path tokens-path)))))
