(ns abc.tools.annotation-join-stats
  "Join-statistics core for the corpus-scale ruby/gaiji annotation run
  (design spec 2026-07-10, slice B). Pure: token spans taken from the
  tokenizer's reported char offsets (all tokenization/parsing lives in the
  Rust tools; abc only validates the reported spans), per-work
  classification statistics via abc.tools.annotation-join, and aggregation.
  Validated spans are sorted and non-overlapping, which is
  annotation-join/join's documented precondition."
  (:require [abc.tools.annotation-join :as annotation-join]
            [abc.tools.files :as files]
            [abc.tools.json :as abc-json]
            [abc.tools.manifest :as manifest]
            [abc.tools.parser-ir-plaintext :as plaintext]
            [clojure.java.io :as io]))

(defn- scalar-count [^String s]
  (.codePointCount s 0 (.length s)))

(defn token-spans
  "Builds annotation-join token spans from the tokenizer-reported
  `char_start`/`char_end` offsets in tokens.jsonl (unicode-scalar offsets
  into the rendered plaintext). The tokenizer is authoritative — abc never
  re-derives spans from surfaces. Validates the annotation-join
  precondition (ascending, non-overlapping, in-bounds spans); returns
  {:tokens [...]} on success or {:failure {...}} on the first invalid
  row — failures are recorded, never papered over (spec B3)."
  [^String text token-rows]
  (let [total (scalar-count text)]
    (loop [rows (seq token-rows)
           prev-end 0
           token-index 0
           tokens []]
      (if-not rows
        {:tokens tokens}
        (let [row (first rows)
              surface (get row "surface")
              start (get row "char_start")
              end (get row "char_end")]
          (when-not (string? surface)
            (throw (ex-info "Token surface must be a string"
                            {:token-index token-index :surface surface})))
          (if (and (integer? start)
                   (integer? end)
                   (<= prev-end start)
                   (< start end)
                   (<= end total))
            (recur (next rows)
                   (long end)
                   (inc token-index)
                   (conj tokens {"token_index" token-index
                                 "input_span" {"start" start "end" end}
                                 "text" surface}))
            {:failure {:token-index token-index
                       :char-start start
                       :char-end end
                       :prev-end prev-end
                       :text-scalar-count total}}))))))

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

(defn- merge-nested-counts [maps]
  (apply merge-with (partial merge-with +) {} maps))

(defn bare-work-id
  "Bare work id for an AAT file stem `<work-id>-<hash12>`. The corpus dump
  holds several AAT files for some works (fragments with distinct content
  hashes), so the pipeline unit is the file stem; this recovers the work.
  Inputs without a content-hash suffix are returned unchanged."
  [stem]
  (if-let [[_ bare] (re-matches #"(.+)-[0-9a-f]{12}" stem)]
    bare
    stem))

(defn work-level-rows
  "Rolls per-file-stem stats rows up to bare work ids, summing counts
  across a work's file stems. Sorted by work id."
  [rows]
  (->> rows
       (group-by #(bare-work-id (get % "work_id")))
       (sort-by key)
       (mapv (fn [[bare group]]
               {"work_id" bare
                "file_count" (count group)
                "file_stems" (vec (sort (map #(get % "work_id") group)))
                "text_scalar_count" (reduce + (map #(get % "text_scalar_count")
                                                   group))
                "annotation_counts" (merge-counts
                                     (map #(get % "annotation_counts") group))
                "classifications" (merge-nested-counts
                                   (map #(get % "classifications") group))}))))

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
      (vec (files/read-json-lines file)))))

(defn- stats-row [work-id text work-stats-value]
  (let [{:keys [annotation_counts classifications]} work-stats-value]
    {"work_id" work-id
     "text_scalar_count" (scalar-count text)
     "annotation_counts" annotation_counts
     "classifications" classifications}))

(defn- report-md [aggregate-value]
  (str "# Annotation join statistics\n\n"
       "- works: " (get aggregate-value "work_count")
       " file entries across " (get aggregate-value "distinct_work_count")
       " works (skipped: " (count (get aggregate-value "skipped_work_ids")) ")\n"
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
  <tokens-dir>/<work-id>.tokens.jsonl, writes per-work.jsonl (one row per
  file stem), work-level.jsonl (rolled up to bare work ids),
  aggregate.json, and report.md to out-dir. Works without a token file or
  with invalid reported spans are recorded in skipped_work_ids. Returns 0."
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
              token-rows (and has-parser-ir (work-tokens tokens-dir work-id))]
          (if-not token-rows
            (recur (next dirs) rows work-stats-acc (conj skipped work-id))
            (let [parser-ir (files/read-json (io/file dir "parser-ir.json"))
                  {:keys [text annotations]} (plaintext/render-with-annotations
                                              parser-ir)
                  {:keys [tokens failure]} (token-spans text token-rows)]
              (if failure
                (recur (next dirs) rows work-stats-acc (conj skipped work-id))
                (let [work-stats-value (work-stats {:annotations annotations
                                                    :tokens tokens})]
                  (recur (next dirs)
                         (conj rows (stats-row work-id text work-stats-value))
                         (conj work-stats-acc work-stats-value)
                         skipped))))))
        (let [work-rows (work-level-rows rows)
              aggregate-value
              (-> (aggregate work-stats-acc)
                  (update-keys name)
                  (assoc "distinct_work_count" (count work-rows))
                  (assoc "skipped_work_ids" (vec (sort skipped))))]
          (spit (io/file out "per-work.jsonl")
                (apply str (map #(str (row-json-str %) "\n") rows)))
          (spit (io/file out "work-level.jsonl")
                (apply str (map #(str (row-json-str %) "\n") work-rows)))
          (manifest/write-json-file! (io/file out "aggregate.json")
                                     aggregate-value)
          (spit (io/file out "report.md") (report-md aggregate-value))
          0)))))
