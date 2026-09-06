(ns soranoha.assessment.source
  "Capture the observation inputs an assessment consumes. Checkout-backed
  observations are read from the selected edition; retained evidence is
  read and digest-verified outside the disposable computation store."
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [soranoha.core.hash :as hash]
            [soranoha.aozora.source-bundle :as bundle]
            [soranoha.aozora.csv :as csv]
            [soranoha.yomi.catalog :as catalog]
            [soranoha.yomi.select :as select]
            [soranoha.za.scaffold :as scaffold]))

(defn retained-observations
  "Read declared evidence files below evidence-root, refusing escapes and
  digest mismatches before any evaluator stage can execute."
  [source evidence-root]
  (into {}
        (for [{:strs [id selector path sha256]} (get source "observations")
              :when (= selector "retained-evidence")]
          (let [_ (when-not evidence-root
                    (throw (ex-info "retained evidence requires --evidence-root"
                                    {:reason :missing-evidence-root :observation id})))
                root (fs/real-path evidence-root)
                file (fs/path root path)
                _ (when (or (fs/absolute? path)
                            (not (fs/starts-with? (fs/normalize file) root)))
                    (throw (ex-info "retained evidence path escapes its root"
                                    {:reason :evidence-path-escape :observation id})))
                _ (when-not (fs/regular-file? file)
                    (throw (ex-info "retained evidence is missing"
                                    {:reason :missing-evidence :observation id})))
                actual (fs/real-path file)
                _ (when-not (fs/starts-with? actual root)
                    (throw (ex-info "retained evidence symlink escapes its root"
                                    {:reason :evidence-path-escape :observation id})))
                digest (hash/sha256-file (str actual))]
            (when-not (= sha256 digest)
              (throw (ex-info "retained evidence digest mismatch"
                              {:reason :evidence-digest-mismatch :observation id
                               :expected sha256 :actual digest})))
            [id (str "sha256:" digest)]))))

(defn capture-checkout
  "Resolve declared observations against the current selected population.
  Catalog values are observations, never the assessed contribution set.
  Only source bundles consumed by an observation or reliance are inspected."
  [aozora-root source retained-values]
  (let [rows (csv/read-rows-from-string
              (:csv-text (catalog/read-catalog-zip aozora-root)))
        selected (:candidates (select/select-candidates aozora-root rows))
        by-slug (into {} (map (juxt :slug identity)) selected)
        candidates (scaffold/projection rows selected)
        source-slugs (into (set (map #(get % "slug") (get source "reliances")))
                           (keep (fn [{:strs [selector slug]}]
                                   (when (= selector "canonical-source-bundle") slug)))
                           (get source "observations"))
        source-hashes (into {}
                            (for [slug source-slugs
                                  :let [candidate (get by-slug slug)]
                                  :when candidate]
                              [slug (:bundle-hash (bundle/inspect-zip
                                                   (io/file (:file candidate))))]))
        unavailable {"state" "unavailable" "reason" "missing-selected-work"}
        observations
        (into {}
              (for [{:strs [id selector slug]} (get source "observations")]
                [id (case selector
                      "catalog-contributors" (get candidates slug unavailable)
                      "canonical-source-bundle" (get source-hashes slug unavailable)
                      "retained-evidence" (get retained-values id))]))]
    {:candidates candidates :observations observations :source-hashes source-hashes}))
