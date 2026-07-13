(ns abc.tools.adr-claim-migration
  (:require [abc.tools.adr :as adr]
            [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.json :as json]
            [abc.tools.schema :as schema]
            [babashka.fs :as fs]
            [clojure.set :as set]
            [clojure.tools.cli :as cli]))

(def ^:private baseline-schema-version "abc-adr-claim-migration-baseline-v1")
(def ^:private ledger-schema-version "abc-adr-claim-migration-v1")

(defn- normative-coordinate-inventory []
  (let [decision-adrs [1 2 6 7 8 9 10 11 12 13 14 15 16 17 18 20 21 22
                       23 24 25 29 30 31 32 33]
        hard-rule-adrs [13 14 15 17 18 20 21 29]]
    (vec (concat (map #(vector % "Decision") decision-adrs)
                 [[14 "Decision Matrix"]]
                 (map #(vector % "Hard Rule") hard-rule-adrs)
                 [[16 "Hard Rule (carried forward from ADR 0015)"]]))))

(defn criterion-text-hash [body]
  (hash/format-sha256 (hash/sha256-string body)))

(defn- accepted-adrs [adrs]
  (filter #(= "Accepted" (:status %)) adrs))

(defn- criterion-row [{:keys [num file]} {:keys [criterion-index body]}]
  {"adr" num
   "file" file
   "original_criterion_index" criterion-index
   "original_text" body
   "original_text_hash" (criterion-text-hash body)})

(defn- normative-row [{:keys [num file section-bodies]} section]
  (when-let [body (get section-bodies section)]
    {"adr" num
     "file" file
     "section" section
     "original_text" body
     "original_text_hash" (criterion-text-hash body)}))

(defn baseline-value [revision adrs]
  (let [normative-coordinates (normative-coordinate-inventory)
        duplicate-coordinates (->> normative-coordinates
                                   frequencies
                                   (keep (fn [[coordinate count]]
                                           (when (< 1 count) coordinate))))
        accepted (vec (accepted-adrs adrs))
        accepted-by-num (into {} (map (juxt :num identity) accepted))
        required-coordinates (filter (fn [[adr-number _]]
                                       (contains? accepted-by-num adr-number))
                                     normative-coordinates)
        missing-coordinates (remove (fn [[adr-number section]]
                                      (contains? (:section-bodies (get accepted-by-num adr-number))
                                                 section))
                                    required-coordinates)
        criteria (->> accepted
                      (mapcat (fn [item] (map #(criterion-row item %) (:criteria item))))
                      (sort-by (juxt #(get % "adr") #(get % "original_criterion_index")))
                      vec)
        sections (->> accepted
                      (mapcat (fn [{:keys [num] :as item}]
                                (for [[adr-number section] normative-coordinates
                                      :when (= adr-number num)
                                      :let [row (normative-row item section)]
                                      :when row]
                                  row)))
                      (sort-by (juxt #(get % "adr") #(get % "section")))
                      vec)]
    (when (seq duplicate-coordinates)
      (throw (ex-info "Explicit inventory contains a duplicate normative coordinate"
                      {:coordinates (vec duplicate-coordinates)})))
    (when (seq missing-coordinates)
      (throw (ex-info "Accepted ADR is missing an explicit normative coordinate"
                      {:coordinates (vec (sort missing-coordinates))})))
    {"schema_version" baseline-schema-version
     "baseline_revision" revision
     "accepted_adr_count" (count accepted)
     "criterion_count" (count criteria)
     "criteria" criteria
     "normative_section_count" (count sections)
     "normative_sections" sections}))

(defn baseline-hash [baseline]
  (hash/format-sha256 (hash/sha256-json-jcs baseline)))

(defn- problem [kind message & {:as data}]
  (merge {:kind kind :message message} data))

(defn- duplicates [key-fn values]
  (->> values (group-by key-fn) (keep (fn [[key rows]] (when (< 1 (count rows)) key)))))

(defn validate-baseline [baseline]
  (let [schema-value (files/read-json "schemas/adr-claim-migration-baseline.schema.json")
        criteria (get baseline "criteria" [])
        sections (get baseline "normative_sections" [])
        invalid-hashes (concat
                        (filter #(not= (get % "original_text_hash")
                                       (criterion-text-hash (get % "original_text" ""))) criteria)
                        (filter #(not= (get % "original_text_hash")
                                       (criterion-text-hash (get % "original_text" ""))) sections))
        invalid? (or (seq (schema/validation-errors schema-value baseline))
                     (not= (count criteria) (get baseline "criterion_count"))
                     (not= (count (set (map #(get % "adr") criteria)))
                           (get baseline "accepted_adr_count"))
                     (not= (count sections) (get baseline "normative_section_count"))
                     (not= criteria (vec (sort-by (juxt #(get % "adr") #(get % "original_criterion_index")) criteria)))
                     (not= sections (vec (sort-by (juxt #(get % "adr") #(get % "section")) sections)))
                     (seq invalid-hashes)
                     (seq (duplicates (juxt #(get % "adr") #(get % "original_text_hash")) criteria))
                     (seq (duplicates (juxt #(get % "adr") #(get % "section")) sections)))]
    (if invalid?
      [(problem :invalid-migration-baseline "migration baseline violates its closed contract")]
      [])))

(defn normative-section-problems [baseline current-adrs]
  (let [normative-coordinates (normative-coordinate-inventory)
        by-adr (into {} (map (juxt :num identity) current-adrs))
        expected (into {} (map (fn [row] [[(get row "adr") (get row "section")] row])
                               (get baseline "normative_sections")))
        original-adrs (set (map #(get % "adr") (get baseline "criteria")))]
    (->> normative-coordinates
         (filter (fn [[adr-number _]] (contains? original-adrs adr-number)))
         (keep (fn [[adr-number section :as coordinate]]
                 (let [row (get expected coordinate)
                       current (get-in by-adr [adr-number :section-bodies section])]
                   (when (or (nil? row)
                             (nil? current)
                             (not= (get row "original_text") current)
                             (not= (get row "original_text_hash")
                                   (some-> current criterion-text-hash)))
                     (problem :accepted-normative-section-drift
                              "an original Accepted ADR normative section changed"
                              :adr adr-number :section section)))))
         vec)))

(def ^:private dispositions
  #{:retain :correct :move-out-of-acceptance :demote-adr})

(defn validate-ledger [baseline ledger claims-by-id {:keys [require-complete?]
                                                     :or {require-complete? true}}]
  (let [baseline-keys (set (map (juxt #(get % "adr") #(get % "original_text_hash"))
                                (get baseline "criteria")))
        entries (:entries ledger {})
        entry-keys (set (keys entries))
        resulting-ids (mapcat :resulting-claim-ids (vals entries))]
    (vec
     (concat
      (when-not (= ledger-schema-version (:schema-version ledger))
        [(problem :invalid-disposition "migration ledger schema version is invalid")])
      (when-not (= (get baseline "baseline_revision") (:baseline-revision ledger))
        [(problem :baseline-revision-mismatch "ledger baseline revision does not match")])
      (when-not (= (baseline-hash baseline) (:baseline-manifest-hash ledger))
        [(problem :baseline-hash-mismatch "ledger baseline manifest hash does not match")])
      (map #(problem :unknown-baseline-key "ledger key is absent from baseline" :key %)
           (sort (set/difference entry-keys baseline-keys)))
      (when require-complete?
        (map #(problem :unresolved-baseline-key "baseline row has no ledger disposition" :key %)
             (sort (set/difference baseline-keys entry-keys))))
      (mapcat
       (fn [[key {:keys [disposition rationale planned-evidence-boundaries resulting-claim-ids]}]]
         (concat
          (when-not (contains? dispositions disposition)
            [(problem :invalid-disposition "ledger disposition is invalid" :key key)])
          (when (and (not= :retain disposition) (not (seq rationale)))
            [(problem :missing-disposition-rationale "non-retained row requires rationale" :key key)])
          (when (and (contains? #{:retain :correct} disposition)
                     (not (seq planned-evidence-boundaries)))
            [(problem :missing-evidence-boundary "live row requires an evidence boundary" :key key)])
          (when (and (contains? #{:move-out-of-acceptance :demote-adr} disposition)
                     (seq resulting-claim-ids))
            [(problem :invalid-disposition "non-live row cannot have resulting claim IDs" :key key)])
          (for [claim-id resulting-claim-ids
                :when (not (contains? claims-by-id claim-id))]
            (problem :missing-resulting-claim-id "resulting claim ID is absent from current ADRs"
                     :key key :claim-id claim-id))))
       entries)
      (map #(problem :duplicate-resulting-claim-id "resulting claim ID occurs more than once"
                     :claim-id %)
           (duplicates identity resulting-ids))))))

(defn load-migration-state [repo-root options]
  (let [baseline (files/read-json (fs/file repo-root "docs/adr/adr-claim-migration-baseline.json"))
        ledger (files/read-edn (fs/file repo-root "docs/adr/adr-claim-migration.edn"))
        adrs (adr/parse-all (fs/file repo-root "docs/adr"))
        claims (into {} (keep (fn [criterion]
                                (when-let [claim-id (:claim-id criterion)] [claim-id criterion])))
                     (mapcat :criteria adrs))
        problems (vec (concat (validate-baseline baseline)
                              (normative-section-problems baseline adrs)
                              (validate-ledger baseline ledger claims options)))]
    {:baseline baseline :ledger ledger :by-key (:entries ledger) :problems problems}))

(def cli-options
  [[nil "--write-baseline PATH"]
   [nil "--revision REV"]])

(defn -main [& args]
  (let [args (if (= "--" (first args)) (rest args) args)
        {:keys [options errors]} (cli/parse-opts args cli-options)
        output (:write-baseline options)
        revision (:revision options)]
    (cond
      (or (seq errors) (nil? output) (nil? revision))
      (System/exit 2)

      (fs/exists? output)
      (do (binding [*out* *err*] (println "Refusing to replace immutable baseline:" output))
          (System/exit 1))

      :else
      (let [value (baseline-value revision (adr/parse-all "docs/adr"))]
        (json/write-deterministic-json-file! output value)
        (println (str "Wrote baseline: " (get value "accepted_adr_count") " ADRs, "
                      (get value "criterion_count") " criteria, "
                      (get value "normative_section_count") " normative sections"))))))
