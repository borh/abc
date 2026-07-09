(ns abc.tools.annotation-join)

(defn- overlapping [tokens s e]
  (filterv (fn [t]
             (let [{:strs [start end]} (get t "input_span")]
               (and (< start e) (< s end))))
           tokens))

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
  unicode-scalar coordinate system. Returns a generated view; never a
  canonical artifact (ADR 0028)."
  [tokens annotations]
  (mapv (fn [ann]
          (let [{:strs [start end]} (get ann "span")
                cover (overlapping tokens start end)]
            {"annotation" ann
             "token_indexes" (mapv #(get % "token_index") cover)
             "classification" (if (empty? cover)
                                "conflict"
                                (classify cover start end))}))
        annotations))
