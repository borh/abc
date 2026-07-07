(ns abc.tools.source-region-contract
  (:require [clojure.set :as set]))

(def schema-version
  "aozora-source-region-coverage-v1")

(def required-classes
  #{"notation_legend"
    "notation_placeholder"
    "body_end_boundary"
    "terminal_provenance"
    "colophon_metadata"
    "letter_address_origin"
    "malformed_source"})

(def classes-needing-measurement-split
  #{"terminal_provenance" "colophon_metadata"})

(def allowed-target-classes
  #{"tei_policy_projection"
    "tei_plus_abc_extension"
    "custom_sidecar"
    "diagnostic"
    "unsupported_gap"})

(defn- policy-dispositions-by-class [policy]
  (->> (get policy "dispositions")
       (map (fn [row] [(get row "source_class") row]))
       (into {})))

(defn policy-errors [policy]
  (let [dispositions (policy-dispositions-by-class policy)
        disposition-classes (set (keys dispositions))
        missing (set/difference required-classes
                                disposition-classes)
        duplicate-classes (->> (get policy "dispositions")
                               (map #(get % "source_class"))
                               frequencies
                               (keep (fn [[source-class count]]
                                       (when (> count 1) source-class)))
                               set)
        row-errors (mapcat
                    (fn [[source-class row]]
                      (let [target-class (get row "target_class")
                            plaintext-projection (get row "plaintext_projection")
                            measurement-status (get row "measurement_status")]
                        (concat
                         (when-not (contains? allowed-target-classes
                                              target-class)
                           [(str "source-region policy class " source-class
                                 " has unsupported target_class "
                                 target-class)])
                         (when-not (= "omit" plaintext-projection)
                           [(str "source-region policy class " source-class
                                 " must omit from plaintext, got "
                                 plaintext-projection)])
                         (when (and (contains? classes-needing-measurement-split
                                               source-class)
                                    (not= "needs_measurement_split"
                                          measurement-status))
                           [(str "source-region policy class " source-class
                                 " must declare measurement_status "
                                 "needs_measurement_split until ab-validator "
                                 "separately measures terminal provenance and "
                                 "colophon prevalence")]))))
                    dispositions)]
    (vec
     (concat
      (for [source-class (sort missing)]
        (str "source-region policy is missing disposition for " source-class))
      (for [source-class (sort duplicate-classes)]
        (str "source-region policy has duplicate disposition for " source-class))
      row-errors))))

(defn coverage-errors [coverage policy]
  (let [region (get coverage "source_region_coverage")
        representability (get coverage "representability")
        source-apparatus (get region "source_apparatus_occurrences")
        malformed-source (get region "malformed_source_occurrences")
        malformed-noise (get representability "malformed_noise_occurrences")
        unsupported (get representability "unsupported_occurrences")
        unsupported-body (get region "unsupported_body_markup_occurrences")]
    (vec
     (concat
      (when-not (= schema-version
                   (get coverage "schema_version"))
        [(str "source-region coverage schema_version must be "
              schema-version
              ", got "
              (get coverage "schema_version"))])
      (when-not (= "SOURCE_AUTHORITY_GATE_PASS"
                   (get coverage "gate_status"))
        [(str "source-region coverage requires SOURCE_AUTHORITY_GATE_PASS, got "
              (get coverage "gate_status"))])
      (when-not (zero? (or (get coverage "unallowlisted_unknown_markers_total")
                           0))
        [(str "source-region coverage has unallowlisted unknown markers: "
              (get coverage "unallowlisted_unknown_markers_total"))])
      (mapcat (fn [[field value]]
                (when-not (zero? (or value 0))
                  [(str "source-region coverage requires " field
                        " == 0, got " value)]))
              [["unsupported_body_markup_occurrences" unsupported-body]
               ["unknown_region_occurrences" (get region "unknown_region_occurrences")]
               ["unknown_unreviewed_occurrences" (get region "unknown_unreviewed_occurrences")]])
      (when (and (some? malformed-noise)
                 (some? source-apparatus)
                 (some? malformed-source)
                 (not= malformed-noise (+ source-apparatus malformed-source)))
        [(str "legacy malformed_noise_occurrences must equal "
              "source_apparatus_occurrences + malformed_source_occurrences"
              ", got "
              malformed-noise
              " vs "
              source-apparatus
              " + "
              malformed-source)])
      (when (and (some? unsupported)
                 (some? unsupported-body)
                 (not= unsupported unsupported-body))
        [(str "legacy unsupported_occurrences must equal "
              "unsupported_body_markup_occurrences, got "
              unsupported
              " vs "
              unsupported-body)])
      (policy-errors policy)))))
