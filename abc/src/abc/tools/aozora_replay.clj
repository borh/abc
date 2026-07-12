(ns abc.tools.aozora-replay
  "Replay abc's audit machinery over the real pinned aozorabunko history
  and pin the per-pair findings as a committed baseline. Design:
  docs/superpowers/specs/2026-07-12-aozora-replay-harness-design.md."
  (:require [abc.tools.aozora-csv :as ac]
            [abc.tools.json :as abc-json]
            [clojure.string :as string])
  (:import [java.io ByteArrayInputStream IOException]
           [java.util.zip ZipException ZipInputStream]))

(def baseline-format 1)
(def default-remote-url "https://github.com/aozorabunko/aozorabunko.git")
(def default-zip-path "index_pages/list_person_all_extended_utf8.zip")
(def default-baseline-path "test/resources/aozora-replay-baseline.json")

(defn locked-pin
  "The aozorabunko-src locked rev from a flake.lock file."
  [lock-path]
  (let [lock (abc-json/read-json-file lock-path)
        rev (get-in lock ["nodes" "aozorabunko-src" "locked" "rev"])]
    (when-not (and (string? rev) (re-matches #"[0-9a-f]{40}" rev))
      (throw (ex-info (str "no aozorabunko-src locked rev in " lock-path)
                      {:lock-path (str lock-path)})))
    rev))

(defn- zip-signature?
  "True when the bytes begin with a ZIP local-file-header (PK\\x03\\x04) or
  empty-archive end-of-central-directory (PK\\x05\\x06) signature.
  ZipInputStream.getNextEntry silently returns nil on most non-ZIP bytes,
  which would misreport garbage as no-csv-entry — so the signature is
  checked explicitly first."
  [^bytes bs]
  (and (>= (alength bs) 4)
       (= 0x50 (bit-and 0xff (aget bs 0)))
       (= 0x4B (bit-and 0xff (aget bs 1)))
       (contains? #{[3 4] [5 6]}
                  [(bit-and 0xff (aget bs 2)) (bit-and 0xff (aget bs 3))])))

(defn catalog-bytes-fault
  "nil when the bytes are a usable catalog ZIP; otherwise the source-fact
  reason string. Only ZIP-structural problems are absorbed here; anything
  else escapes as a harness/environment concern."
  [^bytes bs]
  (if-not (zip-signature? bs)
    "unreadable-zip"
    (try
      (with-open [zin (ZipInputStream. (ByteArrayInputStream. bs))]
        (loop []
          (if-let [entry (.getNextEntry zin)]
            (if (string/ends-with? (.getName entry) ".csv")
              (let [csv (String. (.readAllBytes zin) "UTF-8")]
                (if (seq (ac/read-rows-from-string csv)) nil "no-data-rows"))
              (recur))
            "no-csv-entry")))
      (catch ZipException _ "unreadable-zip")
      (catch IOException _ "unreadable-zip"))))

(defn pair-digest
  "Digest one scan pair-report into the pinned baseline pair shape."
  [period-by-ref pair]
  (let [ingest (:current_ingest pair)]
    {"previous_ref" (:previous_ref pair)
     "current_ref" (:current_ref pair)
     "period" (get period-by-ref (:current_ref pair))
     "status" (:status pair)
     "drift_summary" (get-in pair [:drift "summary"])
     "ingest" {"works_written" (:works-written ingest)
               "works_skipped" (:works-skipped ingest)
               "skipped_work_ids" (vec (:skipped-work-ids ingest))
               "persons_written" (:persons-written ingest)
               "person_conflicts" (mapv #(get % "person_id")
                                        (:person-conflicts ingest))}}))

(defn baseline-doc
  [{:keys [remote-url pin-rev zip-path sample-period excluded pairs
           period-by-ref]}]
  {"baseline_format" baseline-format
   "remote_url" remote-url
   "pin_rev" pin-rev
   "zip_path" zip-path
   "sample_period" sample-period
   "excluded" (vec excluded)
   "pairs" (mapv #(pair-digest period-by-ref %) pairs)})

(defn- header [doc]
  (select-keys doc ["baseline_format" "zip_path" "sample_period" "remote_url"]))

(defn- pair-changes [old-pairs new-pairs]
  (let [n (max (count old-pairs) (count new-pairs))]
    (vec
     (for [i (range n)
           :let [o (get old-pairs i)
                 nw (get new-pairs i)]]
       {"index" i
        "change" (cond
                   (nil? o) "added"
                   (nil? nw) "removed"
                   (= o nw) "unchanged"
                   :else "replaced")}))))

(defn- strict-final-replacement?
  "The ONLY replacement pin-bump-shaped tolerates: same period, same
  previous_ref, different current_ref (the final period gained a later
  representative). A digest change on unchanged input refs is never
  pin-bump-shaped, and neither is a replacement that moves the pair to a
  different period. Ancestry of the new current_ref is not provable in a
  pure comparison; it is implied by the plan (representatives are sampled
  from commits reachable from the fetched pin) and by human review of the
  update diff."
  [old-pair new-pair]
  (and (= (get old-pair "period") (get new-pair "period"))
       (= (get old-pair "previous_ref") (get new-pair "previous_ref"))
       (not= (get old-pair "current_ref") (get new-pair "current_ref"))))

(defn- exclusions-only-newer? [old-doc new-doc]
  (let [old-ex (set (get old-doc "excluded"))
        new-ex (set (get new-doc "excluded"))
        last-period (get (peek (get old-doc "pairs")) "period")]
    (and (every? new-ex old-ex)
         (every? (fn [e] (and (some? (get e "period"))
                              (some? last-period)
                              (pos? (compare (get e "period") last-period))))
                 (remove old-ex new-ex)))))

(defn classify-diff
  "Compare a committed baseline doc against a freshly produced one.
  Verdict semantics per the design spec's diff-classification section."
  [old-doc new-doc]
  (let [old-pairs (vec (get old-doc "pairs"))
        new-pairs (vec (get new-doc "pairs"))
        changes (pair-changes old-pairs new-pairs)
        n (count old-pairs)]
    {:pair-changes changes
     :verdict
     (cond
       (not= (header old-doc) (header new-doc))
       :configuration-change

       (= old-doc new-doc)
       :unchanged

       (= (get old-doc "pin_rev") (get new-doc "pin_rev"))
       :behavioral-change

       (and (>= (count new-pairs) n)
            (= (subvec new-pairs 0 (max 0 (dec n)))
               (subvec old-pairs 0 (max 0 (dec n))))
            (or (zero? n)
                (let [o (peek old-pairs) nw (get new-pairs (dec n))]
                  (or (= o nw) (strict-final-replacement? o nw))))
            (exclusions-only-newer? old-doc new-doc))
       :pin-bump-shaped

       :else
       :behavioral-change)}))
