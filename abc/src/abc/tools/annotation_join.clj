(ns abc.tools.annotation-join)

(defn- validated-token-vector [tokens]
  (loop [remaining (seq tokens)
         token-index 0
         prev-end 0
         result (transient [])]
    (if-let [token (first remaining)]
      (let [{:strs [start end]} (get token "input_span")]
        (if (and (integer? start)
                 (integer? end)
                 (<= prev-end start)
                 (< start end))
          (recur (next remaining)
                 (inc token-index)
                 end
                 (conj! result token))
          (throw (ex-info "Invalid token span"
                          {:token-index token-index
                           :start start
                           :end end
                           :prev-end prev-end}))))
      (persistent! result))))

(defn- first-ending-after [tokens offset]
  (loop [low 0
         high (count tokens)]
    (if (< low high)
      (let [mid (quot (+ low high) 2)
            end (get-in tokens [mid "input_span" "end"])]
        (if (<= end offset)
          (recur (inc mid) high)
          (recur low mid)))
      low)))

(defn- overlapping [tokens s e]
  (loop [index (first-ending-after tokens s)
         result (transient [])]
    (if (< index (count tokens))
      (let [token (nth tokens index)
            start (get-in token ["input_span" "start"])]
        (if (< start e)
          (recur (inc index) (conj! result token))
          (persistent! result)))
      (persistent! result))))

(defn- classify [cover s e]
  (let [first-span (get (first cover) "input_span")
        last-span (get (last cover) "input_span")
        start-aligned? (= s (get first-span "start"))
        end-aligned? (= e (get last-span "end"))]
    (cond
      (and start-aligned? end-aligned? (= 1 (count cover))) "aligned-single"
      (and start-aligned? end-aligned?) "aligned-multi"
      ;; end-only straddle = stem ruby: base covers the token's leading kanji,
      ;; the token continues into okurigana (probe: 92.6% of straddles).
      (and start-aligned? (not end-aligned?)) "stem-prefix"
      :else "conflict")))

(defn join
  "Join annotations to tokens by span intersection in the shared plaintext
  unicode-scalar coordinate system.

  Token input is materialized as a vector and validated before joining. Every
  token span must contain integer start/end offsets, have positive width, and
  be sorted and non-overlapping; violation throws ExceptionInfo before any
  result is produced. Classification reads the first/last covering token
  positionally. Degenerate annotation spans
  (start >= end) cover no text: zero-width annotation spans (D3 allows
  empty gaiji spans) and inverted spans alike yield token_indexes [] and
  classification \"conflict\"; a fifth classification for
  invisible-vs-misaligned is deliberately deferred until a consumer needs
  the distinction (ADR 0028 records four probe classifications).

  Returns a generated view; never a canonical artifact (ADR 0028)."
  [tokens annotations]
  (let [tokens (validated-token-vector tokens)]
    (mapv (fn [ann]
            (let [{:strs [start end]} (get ann "span")
                  cover (if (>= start end)
                          []
                          (overlapping tokens start end))]
              {"annotation" ann
               "token_indexes" (mapv #(get % "token_index") cover)
               "classification" (if (empty? cover)
                                  "conflict"
                                  (classify cover start end))}))
          annotations)))
