(ns abc.tools.parser-rq-decoded-utf8
  "The decoded_utf8 coordinate system shared by parser-RQ instruments:
  strict UTF-8 byte slices, Unicode private-use scalars, and canonical
  interval algebra over eligible decoded bytes.")

(defn decoded-slice
  "Strictly decode the byte interval [start, end) of `source-bytes`.
  Nil unless the interval is a nonempty in-bounds slice that decodes as
  valid UTF-8 on both boundaries."
  [^bytes source-bytes start end]
  (when (and source-bytes (int? start) (int? end)
             (<= 0 start) (< start end) (<= end (alength source-bytes)))
    (try
      (let [decoder (doto (.newDecoder java.nio.charset.StandardCharsets/UTF_8)
                      (.onMalformedInput java.nio.charset.CodingErrorAction/REPORT)
                      (.onUnmappableCharacter java.nio.charset.CodingErrorAction/REPORT))]
        (str (.decode decoder
                      (java.nio.ByteBuffer/wrap source-bytes start (- end start)))))
      (catch Exception _ nil))))

(defn unicode-private-use?
  "True when `value` is exactly one Unicode private-use scalar."
  [value]
  (when (and (string? value)
             (= 1 (.codePointCount ^String value 0 (.length ^String value))))
    (let [codepoint (.codePointAt ^String value 0)]
      (or (<= 0xE000 codepoint 0xF8FF)
          (<= 0xF0000 codepoint 0xFFFFD)
          (<= 0x100000 codepoint 0x10FFFD)))))

(defn interval-bytes
  "Total bytes covered by canonical `intervals`."
  [intervals]
  (reduce + 0 (map #(- (:end %) (:start %)) intervals)))

(defn canonical-interval-errors
  "Itemized violations of the canonical-interval contract for `intervals`
  within [0, eligible-bytes): integer bounds per interval and strictly
  increasing, non-adjacent neighbours. Producers emit maximal, canonical
  intervals, so adjacent intervals are as invalid as overlapping
  intervals at this trust boundary."
  [label intervals eligible-bytes]
  (vec
   (concat
    (keep-indexed
     (fn [index interval]
       (let [start (:start interval) end (:end interval)]
         (when (or (not (int? start)) (not (int? end))
                   (>= start end) (neg? start) (> end eligible-bytes))
           (str label " interval " index " is outside eligible bytes"))))
     intervals)
    (keep-indexed
     (fn [index [left right]]
       (when (>= (:end left) (:start right))
         (str label " intervals " index " and " (inc index)
              " overlap or are not maximally normalized")))
     (partition 2 1 intervals)))))

(defn canonical-intervals?
  "Boolean form of canonical-interval-errors, additionally requiring a
  vector."
  [intervals eligible-bytes]
  (and (vector? intervals)
       (empty? (canonical-interval-errors "intervals" intervals eligible-bytes))))

(defn interval-subset?
  "True when every byte in canonical `subset` occurs in canonical `superset`."
  [subset superset]
  (loop [remaining-subset subset
         remaining-superset superset]
    (if-let [{sub-start :start sub-end :end} (first remaining-subset)]
      (if-let [{super-start :start super-end :end} (first remaining-superset)]
        (cond
          (<= super-end sub-start)
          (recur remaining-subset (next remaining-superset))

          (and (<= super-start sub-start) (<= sub-end super-end))
          (recur (next remaining-subset) remaining-superset)

          :else false)
        false)
      true)))

(defn interval-complement
  "Canonical complement of canonical `intervals` within [0, eligible)."
  [intervals eligible]
  (loop [cursor 0
         remaining intervals
         complement []]
    (if-let [{:keys [start end]} (first remaining)]
      (recur end
             (next remaining)
             (cond-> complement (< cursor start)
                     (conj {:start cursor :end start})))
      (cond-> complement (< cursor eligible)
              (conj {:start cursor :end eligible})))))

(defn normalized-intervals
  "Sort and merge possibly overlapping `intervals` into canonical form."
  [intervals]
  (reduce (fn [result interval]
            (if-let [previous (peek result)]
              (if (<= (:start interval) (:end previous))
                (conj (pop result)
                      (assoc previous :end (max (:end previous) (:end interval))))
                (conj result interval))
              [interval]))
          [] (sort-by (juxt :start :end) intervals)))

(defn subtract-interval
  "Subtract the canonical `cuts` intervals from the `source` interval."
  [source cuts]
  (loop [cursor (:start source), remaining cuts, result []]
    (if-let [cut (first remaining)]
      (let [result (if (< cursor (:start cut))
                     (conj result {:start cursor :end (:start cut)}) result)]
        (recur (max cursor (:end cut)) (next remaining) result))
      (cond-> result (< cursor (:end source))
              (conj {:start cursor :end (:end source)})))))
