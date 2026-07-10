(ns abc.tools.annotation-join-stats
  "Join-statistics core for the corpus-scale ruby/gaiji annotation run
  (design spec 2026-07-10, slice B). Pure: token-span reconstruction from a
  surface sequence, per-work classification statistics via
  abc.tools.annotation-join, and aggregation. Reconstructed spans are sorted
  and non-overlapping by construction (contiguous left-to-right walk), which
  is annotation-join/join's documented precondition."
  (:require [abc.tools.annotation-join :as annotation-join]
            [abc.tools.files :as files]
            [abc.tools.json :as abc-json]
            [abc.tools.manifest :as manifest]
            [abc.tools.parser-ir-plaintext :as plaintext]
            [clojure.java.io :as io]
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
              _ (when-not (string? surface)
                  (throw (ex-info "Token surface must be a string"
                                  {:token-index token-index :surface surface})))
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

(defn- work-dirs [parser-ir-dir]
  (->> (.listFiles (io/file parser-ir-dir))
       (filter #(.isDirectory %))
       (sort-by #(.getName %))))

(defn- work-tokens [tokens-dir work-id]
  (let [file (io/file tokens-dir (str work-id ".tokens.jsonl"))]
    (when (.isFile file)
      (mapv #(get % "surface") (files/read-json-lines file)))))

(defn- stats-row [work-id text work-stats-value]
  (let [{:keys [annotation_counts classifications]} work-stats-value]
    {"work_id" work-id
     "text_scalar_count" (scalar-count text)
     "annotation_counts" annotation_counts
     "classifications" classifications}))

(defn- report-md [aggregate-value]
  (str "# Annotation join statistics\n\n"
       "- works: " (get aggregate-value "work_count")
       " (skipped: " (count (get aggregate-value "skipped_work_ids")) ")\n"
       "- annotation counts: "
       (pr-str (into (sorted-map) (get aggregate-value "annotation_counts")))
       "\n\n"
       "## Classification rates\n\n"
       (apply str
              (for [[kind rates] (sort-by key (get aggregate-value "classification_rates"))]
                (str "- " kind ": "
                     (pr-str (into (sorted-map) rates))
                     "\n")))))

(defn- row-json-str [row]
  (abc-json/write-deterministic-jsonl-line row))

(defn run-join-stats!
  "CLI body for `soranoha annotation-join-stats` (spec B4/B5). Reads
  <parser-ir-dir>/<work-id>/parser-ir.json and
  <tokens-dir>/<work-id>.tokens.jsonl, writes per-work.jsonl,
  aggregate.json, and report.md to out-dir. Works without a token file or
  with a failed span walk are recorded in skipped_work_ids. Returns 0."
  [parser-ir-dir tokens-dir out-dir]
  (let [out (io/file out-dir)]
    (.mkdirs out)
    (loop [dirs (work-dirs parser-ir-dir)
           rows []
           work-stats-acc []
           skipped []]
      (if-let [dir (first dirs)]
        (let [work-id (.getName dir)
              has-parser-ir (.isFile (io/file dir "parser-ir.json"))
              surfaces (and has-parser-ir (work-tokens tokens-dir work-id))]
          (if-not surfaces
            (recur (next dirs) rows work-stats-acc (conj skipped work-id))
            (let [parser-ir (files/read-json (io/file dir "parser-ir.json"))
                  {:keys [text annotations]} (plaintext/render-with-annotations
                                              parser-ir)
                  {:keys [tokens failure]} (reconstruct-token-spans text surfaces)]
              (if failure
                (recur (next dirs) rows work-stats-acc (conj skipped work-id))
                (let [work-stats-value (work-stats {:annotations annotations
                                                    :tokens tokens})]
                  (recur (next dirs)
                         (conj rows (stats-row work-id text work-stats-value))
                         (conj work-stats-acc work-stats-value)
                         skipped))))))
        (let [aggregate-value
              (-> (aggregate work-stats-acc)
                  (update-keys name)
                  (assoc "skipped_work_ids" (vec (sort skipped))))]
          (spit (io/file out "per-work.jsonl")
                (apply str (map #(str (row-json-str %) "\n") rows)))
          (manifest/write-json-file! (io/file out "aggregate.json")
                                     aggregate-value)
          (spit (io/file out "report.md") (report-md aggregate-value))
          0)))))
