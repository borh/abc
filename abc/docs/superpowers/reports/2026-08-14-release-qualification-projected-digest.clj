;; Projected per-work digests and family aggregates for the release-qualification
;; design, computed with the canonicalizer the spec actually names:
;; abc.tools.jcs/rfc8785-safe-integer-json-string-v1 (via
;; abc.tools.hash/sha256-json-rfc8785-safe-integer-v1).
;;
;; This exists to close Q10. The companion Python harness
;; (2026-08-14-release-qualification-measurement.py) canonicalizes with
;; `json.dumps(sort_keys=True)`, which is adequate evidence for cost and
;; determinism and is NOT an approvable identity: it sorts object keys by
;; Unicode code point rather than UTF-16 code unit, and its escaping rules are
;; its own. The values this script prints are the approvable ones.
;;
;; Not a production instrument: the production projection op is Phase 1 of the
;; design. This reads the three output trees the measurement harness left on
;; disk and reports what the named canonicalizer makes of them.
;;
;; Definitions (both fixed here, both permutation-invariant):
;;   per-work digest = sha256(rfc8785-safe-integer-json-string-v1(projected doc))
;;   family aggregate = sha256(rfc8785-safe-integer-json-string-v1(
;;                        {slug -> per-work digest}))
;; The aggregate folds a map, not a byte concatenation, so row order cannot
;; enter the value (design D10).
;;
;; Usage:
;;   cd abc && clojure -M -i docs/superpowers/reports/<this file> \
;;       -e '(release-qualification-projected-digest/report!)'
;; Inputs come from the environment:
;;   QUAL_DIR   directory holding aat/, ir1/, div1/ and population.jsonl
;;   QUAL_JOBS  thread count (default 32)
;;   QUAL_OUT   optional path for the JSON summary (stdout always gets it)

;; The namespace deliberately does not match the file name: report artifacts are
;; date-prefixed, and a namespace cannot start with a digit. This file is loaded
;; with `-i`, never required, and lives outside the linted src/ and test/ trees.
(ns release-qualification-projected-digest
  (:require [abc.tools.hash :as hash]
            [abc.tools.json :as json]
            [abc.tools.parallel :as parallel]
            [clojure.java.io :as io]
            [clojure.string :as string]))

;; Each entry is [family, subdirectory, projection id, excluded path]. The
;; excluded paths are the spec's three projection contracts: each is the
;; parser's self-declaration of build identity, copied forward by the converter
;; (design E4, E9, E15).
(def families
  [["aat" "aat" "aat-behavior-v1" ["meta" "adapter_version"]]
   ["parser_ir" "ir1" "parser-ir-behavior-v1" ["derived_from" "aat_adapter_version"]]
   ["divergence" "div1" "divergence-behavior-v1" ["aat" "adapter_version"]]])

(defn- project
  "Remove exactly one path from the document, reporting whether it was there.
  An absent path is not an error — the projection declares what may not be
  digested, not what must exist — but a projection that never fires on any
  document is a wiring mistake, so the caller counts the hits."
  [doc path]
  (let [parent (vec (butlast path))
        leaf (last path)
        node (if (seq parent) (get-in doc parent) doc)]
    (if (and (map? node) (contains? node leaf))
      [(if (seq parent) (update-in doc parent dissoc leaf) (dissoc doc leaf))
       true]
      [doc false])))

(defn- slugs [dir]
  (->> (io/file dir "population.jsonl")
       slurp
       string/split-lines
       (remove string/blank?)
       (mapv #(get (json/read-json-str %) "slug"))))

(defn- per-work-digest [file path]
  (let [[projected excluded?] (project (json/read-json-file file) path)]
    [(hash/format-sha256
      (hash/sha256-json-rfc8785-safe-integer-v1 projected))
     excluded?]))

(defn- digest-family [dir jobs [family subdir _projection path] slugs]
  (let [start (System/nanoTime)
        rows (parallel/ordered-pmap
              jobs
              (fn [slug]
                (let [[digest excluded?]
                      (per-work-digest (io/file dir subdir (str slug ".json")) path)]
                  [slug digest excluded?]))
              slugs)
        elapsed (/ (- (System/nanoTime) start) 1e9)
        per-work (into {} (map (juxt first second)) rows)]
    (when-not (= (count per-work) (count slugs))
      (throw (ex-info "duplicate slug in population" {:family family})))
    ;; A projection that fires on no document would digest the excluded field
    ;; everywhere and silently defeat the whole design, so it fails closed.
    (when (zero? (count (filter #(nth % 2) rows)))
      (throw (ex-info "projection matched no document"
                      {:family family :path path})))
    {:family family
     :works (count per-work)
     :projected (count (filter #(nth % 2) rows))
     :seconds (Double/parseDouble (format "%.1f" elapsed))
     :aggregate (hash/format-sha256
                 (hash/sha256-json-rfc8785-safe-integer-v1 per-work))
     :per-work per-work}))

(defn report! []
  (let [dir (or (System/getenv "QUAL_DIR")
                (throw (ex-info "QUAL_DIR is required" {})))
        jobs (Integer/parseInt (or (System/getenv "QUAL_JOBS") "32"))
        slugs (slugs dir)
        results (mapv #(digest-family dir jobs % slugs) families)
        summary {"schema_version" "release-qualification-projected-digest-v1"
                 "canonicalization" "rfc8785-safe-integer-json-string-v1"
                 "aggregate_definition"
                 (str "sha256(rfc8785-safe-integer-json-string-v1({slug -> "
                      "sha256(rfc8785-safe-integer-json-string-v1(projected "
                      "document))}))")
                 "qualified_works" (count slugs)
                 "families"
                 (into {}
                       (map (fn [[[family _ projection excluded] result]]
                              [family {"projection" projection
                                       "excludes" (string/join "." excluded)
                                       "works" (:works result)
                                       "documents_projected" (:projected result)
                                       "digest_seconds" (:seconds result)
                                       "aggregate" (:aggregate result)}]))
                       (map vector families results))}]
    (doseq [result results]
      (binding [*out* *err*]
        (println (format "%-12s n=%-6d projected=%-6d %7.1f s  %s"
                         (:family result) (:works result) (:projected result)
                         (:seconds result) (:aggregate result)))))
    (when-let [out (System/getenv "QUAL_OUT")]
      (spit out (str (json/write-deterministic-json-str summary) "\n"))
      (doseq [result results]
        (spit (str out "." (:family result) ".per-work.jsonl")
              (->> (sort (:per-work result))
                   (map (fn [[slug digest]]
                          (json/write-deterministic-jsonl-line
                           {"slug" slug "digest" digest})))
                   (string/join "\n"))))
      (doseq [result results]
        (spit (str out "." (:family result) ".per-work.jsonl") "\n" :append true)))
    (println (json/write-deterministic-json-str summary))))
