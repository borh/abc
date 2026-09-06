(ns ab-research.parser-phase5-frozen-tuple
  (:require [ab-research.files :as files]
            [ab-research.hash :as hash]
            [babashka.fs :as fs]
            [clojure.string :as string]
            [clojure.walk :as walk]))

(def reports-prefix (str "ab-validator/docs/superpowers/" "reports/"))

(def compat-path
  (str reports-prefix "2026-07-12-ab-aozora-phase5-c5-compat.edn"))

(def expected
  {:aat-version 2
   :aat-adapter "ab-aozora"
   :aat-adapter-version "ab-aozora 0.6.0 aat-schema 2 facade 0.3.0 wire-schema 3 (git 004deaf548f34a36abbc17d0f7a162df010a6292)"
   :candidate-commit "004deaf548f34a36abbc17d0f7a162df010a6292"
   :candidate-bin-sha256 "00066926b8cb035c2b2a55691cfcb99d26c8450b87d960d058855a69afedd24d"
   :converter-bin-sha256 "74f507726f352cd5749b056e46fcdbfb6801b85ffa280ebc8b11a627528b7481"
   :mapping-id "https://w3id.org/abc/mappings/aat-v2-to-parser-ir-v1/generated-probe"
   :mapping-version "0.4.0"
   :mapping-hash "sha256:cf177bee98af086fe728cbc1942e4f631b26ed5bb55aedc7d91b21f467c41f30"
   :mapping-byte-sha256 "8ebd74dcd0f29973e375a9c89654ffbccc9f15fb2f5c5b7a5c97a0bef6206a56"
   :mapping-schema-hash "sha256:e6af01115ccdb7c5cad086eee4c458230f6b6f55e0dfee7791730b48994283e2"
   :parser-ir-schema-id "https://w3id.org/abc/schemas/parser-ir.schema.json"
   :parser-ir-schema-hash "sha256:a1e1b5069fdec17cbb1f94eb5e9a582d1b109dd95c07257f4da7d9b76c82cfa2"
   :compatibility "lossy"
   :evidence-scope
   {:evidence-type "conversion-audit" :adapter "ab-aozora"
    :adapter-version "ab-aozora 0.6.0 aat-schema 2 facade 0.3.0 wire-schema 3 (git 004deaf548f34a36abbc17d0f7a162df010a6292)"
    :corpus "ab-aozora" :files-scanned 17886 :files-succeeded 17886
    :files-failed 0 :parser-ir-nodes 18803174 :divergence-records 231683
    :divergence-occurrences 4938800 :rules-total 689 :rules-emitted 54
    :rules-missing 635 :unsupported-occurrences 195437}})

(def row-keys
  [:aat-version :aat-adapter :aat-adapter-version :mapping-id :mapping-version
   :mapping-hash :mapping-schema-hash :parser-ir-schema-id
   :parser-ir-schema-hash :evidence-scope :compatibility])

(def match-keys (vec (remove #{:evidence-scope :compatibility} row-keys)))

(defn- path [root relative] (fs/file root relative))

(defn- kebab-key [key]
  (if (keyword? key)
    (keyword (string/replace (name key) "_" "-"))
    key))

(defn- normalize [value]
  (walk/postwalk
   (fn [item]
     (cond
       (map? item) (into {} (map (fn [[key child]]
                                   [(kebab-key (if (string? key) (keyword key) key)) child])) item)
       (keyword? item) (name item)
       :else item))
   value))

(defn- mismatch [relative key-path expected-value actual]
  {:problem :phase5-coordinate-mismatch
   :path relative
   :key key-path
   :expected expected-value
   :actual actual})

(defn- compare-coordinate [relative key-path expected-value actual]
  (when (not= expected-value actual)
    (mismatch relative key-path expected-value actual)))

(defn- leaf-paths
  ([value] (leaf-paths [] value))
  ([prefix value]
   (if (map? value)
     (mapcat (fn [[key child]] (leaf-paths (conj prefix key) child)) value)
     [prefix])))

(defn- compare-map [relative expected-value actual]
  (keep (fn [key-path]
          (compare-coordinate relative key-path
                              (get-in expected-value key-path)
                              (get-in actual key-path)))
        (->> (concat (leaf-paths expected-value) (leaf-paths actual))
             distinct
             (sort-by pr-str))))

(defn- gate-problems [root relative gate]
  (let [summary (files/read-json (path root relative))
        expected-gate {:stage "c5" :gate gate :verdict "PASS"
                       :candidate {:commit (:candidate-commit expected)
                                   :bin_sha256 (:candidate-bin-sha256 expected)
                                   :version (:aat-adapter-version expected)}}
        actual {:stage (get summary "stage") :gate (get summary "gate")
                :verdict (get summary "verdict")
                :candidate (get summary "candidate")}]
    (compare-map relative (normalize expected-gate) (normalize actual))))

(defn- exactly-one [relative key-path values]
  (if (= 1 (count values))
    {:value (nth values 0) :problems []}
    {:value nil :problems [(mismatch relative key-path 1 (count values))]}))

(defn- observed-prefix [text expected-prefix]
  (if (string/starts-with? text expected-prefix)
    expected-prefix
    (subs text 0 (min (count text) (count expected-prefix)))))

(defn frozen-tuple-problems [monorepo-root]
  (let [mapping-relative "ab-validator/data/aat-to-parser-ir-mapping-v2-0.4.0.json"
        mapping-file (path monorepo-root mapping-relative)
        mapping (files/read-json mapping-file)
        producer-selection (exactly-one compat-path [:entries]
                                        (:entries (files/read-edn
                                                   (path monorepo-root compat-path))))
        producer (some-> (:value producer-selection) normalize)
        expected-row (select-keys expected row-keys)
        registry-relative "ab-validator/research/data/aat-parser-ir-compatibility.edn"
        registry-rows (mapv normalize
                            (:entries (files/read-edn (path monorepo-root registry-relative))))
        matches (filterv #(= (select-keys expected match-keys)
                             (select-keys % match-keys))
                         registry-rows)
        audit-relative (str reports-prefix "2026-07-12-ab-aozora-phase5-c5-conversion-audit.summary.json")
        audit (files/read-json (path monorepo-root audit-relative))
        audit-selection (exactly-one audit-relative [:compatibility-candidates]
                                     (get audit "compatibility_candidates"))
        audit-row (some-> (:value audit-selection) normalize)
        schema-relative "ab-validator/research/schemas/parser-ir.schema.json"
        schema (files/read-json (path monorepo-root schema-relative))
        schema-hash (str "sha256:" (hash/sha256-json-jcs schema))
        schema-alternatives (->> (get-in schema ["$defs" "orthographicAnnotations" "anyOf"])
                                 (mapcat #(get % "required"))
                                 set)
        conversion-relative (str reports-prefix "2026-07-12-phase5-c5-conversion-gate.summary.json")
        conversion (files/read-json (path monorepo-root conversion-relative))]
    (vec
     (remove
      nil?
      (concat
       (:problems producer-selection)
       (when producer (compare-map compat-path expected-row producer))
       [(compare-coordinate mapping-relative [:mapping-byte-sha256]
                            (:mapping-byte-sha256 expected) (hash/sha256-file mapping-file))
        (compare-coordinate mapping-relative [:mapping-hash]
                            (:mapping-hash expected)
                            (hash/sha256-json-abc-legacy-v0 mapping))
        (when (= (:parser-ir-schema-hash expected) schema-hash)
          (mismatch schema-relative [:parser-ir-schema-hash]
                    :not-historical schema-hash))
        (compare-coordinate schema-relative [:source-role-alternatives]
                            #{"primary_text_hash" "work_content_hash"} schema-alternatives)]
       (:problems audit-selection)
       (when audit-row (compare-map audit-relative expected-row audit-row))
       (compare-map audit-relative
                    {:totals {:files-attempted (get-in expected [:evidence-scope :files-scanned])
                              :files-succeeded (get-in expected [:evidence-scope :files-succeeded])
                              :files-failed (get-in expected [:evidence-scope :files-failed])}
                     :mapping {:mapping-id (:mapping-id expected)
                               :mapping-version (:mapping-version expected)
                               :mapping-hash (:mapping-hash expected)
                               :mapping-schema-hash (:mapping-schema-hash expected)
                               :parser-ir-schema-id (:parser-ir-schema-id expected)
                               :parser-ir-schema-hash (:parser-ir-schema-hash expected)
                               :rules-total (get-in expected [:evidence-scope :rules-total])}}
                    {:totals (select-keys (normalize (get audit "totals"))
                                          [:files-attempted :files-succeeded :files-failed])
                     :mapping (let [mapping-summary (normalize (get audit "mapping"))]
                                {:mapping-id (:mapping-id mapping-summary)
                                 :mapping-version (:mapping-version mapping-summary)
                                 :mapping-hash (:mapping-hash mapping-summary)
                                 :mapping-schema-hash (:mapping-schema-hash mapping-summary)
                                 :parser-ir-schema-id (:target-parser-ir-schema-id mapping-summary)
                                 :parser-ir-schema-hash (:target-parser-ir-schema-hash mapping-summary)
                                 :rules-total (:rules-total mapping-summary)})})
       (if (= 1 (count matches))
         (when producer (compare-map registry-relative producer (nth matches 0)))
         [(mismatch registry-relative [:entries] 1 (count matches))])
       (mapcat (fn [[relative gate]] (gate-problems monorepo-root relative gate))
               [[(str reports-prefix "2026-07-12-phase5-c5-delta.summary.json") "delta"]
                [(str reports-prefix "2026-07-12-phase5-c5-conformance-gate.summary.json") "conformance"]
                [(str reports-prefix "2026-07-12-phase5-c5-perf.summary.json") "perf"]
                [conversion-relative "conversion"]])
       (compare-map conversion-relative
                    {:details {:files-attempted (get-in expected [:evidence-scope :files-scanned])
                               :files-succeeded (get-in expected [:evidence-scope :files-succeeded])
                               :files-failed (get-in expected [:evidence-scope :files-failed])
                               :mapping-version (:mapping-version expected)
                               :mapping-hash (:mapping-hash expected)}}
                    {:details (select-keys (normalize (get conversion "details"))
                                           [:files-attempted :files-succeeded :files-failed
                                            :mapping-version :mapping-hash])})
       [(compare-coordinate conversion-relative [:converter-bin-sha256]
                            (:converter-bin-sha256 expected)
                            (get-in conversion ["details" "converter_bin_sha256"]))
        (compare-coordinate (str reports-prefix "2026-07-12-phase5-checkpoint.txt")
                            [:prefix] "CHECKPOINT OK"
                            (observed-prefix
                             (files/read-text
                              (path monorepo-root (str reports-prefix "2026-07-12-phase5-checkpoint.txt")))
                             "CHECKPOINT OK"))])))))

(defn assert-frozen-tuple! [monorepo-root]
  (let [problems (frozen-tuple-problems monorepo-root)]
    (if (empty? problems)
      :ok
      (throw (ex-info "Historical parser tuple mismatch" {:problems problems})))))
