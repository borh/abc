(ns ab-research.parser-phase5-frozen-tuple-test
  (:require [ab-research.files :as files]
            [ab-research.hash :as hash]
            [ab-research.parser-phase5-frozen-tuple :as frozen]
            [babashka.fs :as fs]
            [charred.api :as json]
            [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]))

(def historical-schema-hash
  "sha256:a1e1b5069fdec17cbb1f94eb5e9a582d1b109dd95c07257f4da7d9b76c82cfa2")

(def evidence-files
  ["ab-validator/data/aat-to-parser-ir-mapping-v2-0.4.0.json"
   "ab-validator/docs/superpowers/reports/2026-07-12-ab-aozora-phase5-c5-compat.edn"
   "ab-validator/docs/superpowers/reports/2026-07-12-ab-aozora-phase5-c5-conversion-audit.summary.json"
   "ab-validator/docs/superpowers/reports/2026-07-12-phase5-c5-delta.summary.json"
   "ab-validator/docs/superpowers/reports/2026-07-12-phase5-c5-conformance-gate.summary.json"
   "ab-validator/docs/superpowers/reports/2026-07-12-phase5-c5-perf.summary.json"
   "ab-validator/docs/superpowers/reports/2026-07-12-phase5-c5-conversion-gate.summary.json"
   "ab-validator/docs/superpowers/reports/2026-07-12-phase5-checkpoint.txt"
   "ab-validator/research/data/aat-parser-ir-compatibility.edn"
   "ab-validator/research/schemas/parser-ir.schema.json"])

(def compat-path (second evidence-files))

(def audit-path (nth evidence-files 2))
(def delta-path (nth evidence-files 3))
(def conformance-path (nth evidence-files 4))
(def perf-path (nth evidence-files 5))
(def conversion-path (nth evidence-files 6))
(def checkpoint-path (nth evidence-files 7))
(def registry-path (nth evidence-files 8))
(def schema-path (nth evidence-files 9))

(def full-key-paths
  [[:aat_version] [:aat_adapter] [:aat_adapter_version]
   [:mapping_id] [:mapping_version] [:mapping_hash] [:mapping_schema_hash]
   [:parser_ir_schema_id] [:parser_ir_schema_hash] [:compatibility]
   [:evidence_scope :evidence_type] [:evidence_scope :adapter]
   [:evidence_scope :adapter_version] [:evidence_scope :corpus]
   [:evidence_scope :files_scanned] [:evidence_scope :files_succeeded]
   [:evidence_scope :files_failed] [:evidence_scope :parser_ir_nodes]
   [:evidence_scope :divergence_records] [:evidence_scope :divergence_occurrences]
   [:evidence_scope :rules_total] [:evidence_scope :rules_emitted]
   [:evidence_scope :rules_missing] [:evidence_scope :unsupported_occurrences]])

(defn- temp-root []
  (let [root (fs/create-temp-dir {:prefix "phase5-frozen-"})]
    (doseq [relative evidence-files]
      (files/copy-file! (fs/file "../.." relative) (fs/file root relative)))
    root))

(defn- mutate-edn! [root relative f]
  (let [path (fs/file root relative)]
    (spit path (pr-str (f (edn/read-string (slurp path)))))))

(defn- mutate-json! [root relative f]
  (let [path (fs/file root relative)]
    (spit path (json/write-json-str (f (files/read-json path))))))

(defn- changed [value]
  (if (number? value) (inc value) (str value "-changed")))

(defn- problem-key [key-path]
  (mapv #(keyword (string/replace (name %) "_" "-")) key-path))

(defn- expected-problem [relative key-path expected actual]
  {:problem :phase5-coordinate-mismatch
   :path relative
   :key key-path
   :expected expected
   :actual actual})

(defn- legacy-canonical-json [value]
  (hash/abc-legacy-json-c14n-v0 value))

(defn- assert-problem! [relative mutate problem]
  (let [root (temp-root)]
    (try
      (cond
        (.endsWith relative ".edn") (mutate-edn! root relative mutate)
        (.endsWith relative ".json") (mutate-json! root relative mutate)
        :else (spit (fs/file root relative) (mutate (slurp (fs/file root relative)))))
      (is (some #{problem} (frozen/frozen-tuple-problems root)))
      (finally (fs/delete-tree root)))))

(deftest checked-in-phase5-tuple-is-exact-test
  (let [root (fs/canonicalize "../..")
        mapping (files/read-json
                 (fs/file root "ab-validator/data/aat-to-parser-ir-mapping-v2-0.4.0.json"))
        schema (files/read-json (fs/file root "ab-validator/research/schemas/parser-ir.schema.json"))]
    (is (= [] (frozen/frozen-tuple-problems root)))
    (is (= :ok (frozen/assert-frozen-tuple! root)))
    (is (= "38a5497c231947af05aa2ffd3c80ab0d9f5ab41d2fcca39a00435a0967a5da2b"
           (hash/sha256-json-jcs mapping)))
    (is (not= "sha256:cf177bee98af086fe728cbc1942e4f631b26ed5bb55aedc7d91b21f467c41f30"
              (str "sha256:" (hash/sha256-json-jcs mapping))))
    (is (not= historical-schema-hash
              (str "sha256:" (hash/sha256-json-jcs schema))))
    (is (= #{"primary_text_hash" "work_content_hash"}
           (->> (get-in schema ["$defs" "orthographicAnnotations" "anyOf"])
                (mapcat #(get % "required"))
                set)))))

(deftest every-compatibility-coordinate-is-checked-test
  (doseq [key-path full-key-paths]
    (testing (pr-str key-path)
      (let [root (temp-root)]
        (try
          (mutate-edn! root compat-path
                       #(update-in % (into [:entries 0] key-path) changed))
          (is (some (fn [problem]
                      (and (= :phase5-coordinate-mismatch (:problem problem))
                           (= compat-path (:path problem))
                           (= (problem-key key-path) (:key problem))))
                    (frozen/frozen-tuple-problems root)))
          (finally (fs/delete-tree root)))))))

(deftest producer-selection-requires-exactly-one-entry-test
  (doseq [[label entries expected-count]
          [["zero" [] 0]
           ["duplicate" (let [entry (first (:entries (files/read-edn (fs/file "../.." compat-path))))]
                          [entry entry]) 2]]]
    (testing label
      (assert-problem!
       compat-path #(assoc % :entries entries)
       (expected-problem compat-path [:entries] 1 expected-count)))))

(deftest derived-bytes-schema-registry-and-gates-are-checked-test
  (doseq [[relative mutate key-path]
          [["ab-validator/data/aat-to-parser-ir-mapping-v2-0.4.0.json"
            #(assoc % "mapping_version" "live") [:mapping-byte-sha256]]
           ["ab-validator/research/data/aat-parser-ir-compatibility.edn"
            #(update % :entries
                     (fn [entries]
                       (mapv (fn [entry]
                               (if (and (= "ab-aozora" (:aat_adapter entry))
                                        (= "0.4.0" (:mapping_version entry)))
                                 (update-in entry [:evidence_scope :files_scanned] inc)
                                 entry))
                             entries)))
            [:evidence-scope :files-scanned]]
           ["ab-validator/docs/superpowers/reports/2026-07-12-phase5-c5-perf.summary.json"
            #(assoc-in % ["candidate" "commit"] "changed") [:candidate :commit]]]]
    (testing relative
      (let [root (temp-root)]
        (try
          (if (.endsWith relative ".edn")
            (mutate-edn! root relative mutate)
            (mutate-json! root relative mutate))
          (is (some (fn [problem]
                      (and (= :phase5-coordinate-mismatch (:problem problem))
                           (= relative (:path problem))
                           (= key-path (:key problem))))
                    (frozen/frozen-tuple-problems root)))
          (finally (fs/delete-tree root)))))))

(deftest remaining-frozen-binary-coordinates-are-checked-test
  (doseq [[key-path json-path]
          [[[:candidate-bin-sha256] ["candidate" "bin_sha256"]]
           [[:converter-bin-sha256] ["details" "converter_bin_sha256"]]]]
    (let [relative "ab-validator/docs/superpowers/reports/2026-07-12-phase5-c5-conversion-gate.summary.json"
          root (temp-root)]
      (try
        (mutate-json! root relative #(assoc-in % json-path "changed"))
        (is (some (fn [problem]
                    (and (= :phase5-coordinate-mismatch (:problem problem))
                         (= relative (:path problem))
                         (= (if (= key-path [:candidate-bin-sha256])
                              [:candidate :bin-sha256]
                              key-path)
                            (:key problem))))
                  (frozen/frozen-tuple-problems root)))
        (finally (fs/delete-tree root))))))

(deftest every-gate-coordinate-is-checked-test
  (doseq [relative [delta-path conformance-path perf-path conversion-path]
          [key-path replacement]
          [[[:stage] "changed"] [[:gate] "changed"] [[:verdict] "changed"]
           [[:candidate :commit] "changed"] [[:candidate :bin-sha256] "changed"]
           [[:candidate :version] "changed"]]]
    (testing (str relative " " key-path)
      (let [expected-value (case key-path
                             [:stage] "c5"
                             [:gate] ({delta-path "delta" conformance-path "conformance"
                                       perf-path "perf" conversion-path "conversion"} relative)
                             [:verdict] "PASS"
                             [:candidate :commit] (:candidate-commit frozen/expected)
                             [:candidate :bin-sha256] (:candidate-bin-sha256 frozen/expected)
                             [:candidate :version] (:aat-adapter-version frozen/expected))]
        (assert-problem!
         relative #(assoc-in % (mapv (fn [key] (string/replace (name key) "-" "_"))
                                     key-path)
                             replacement)
         (expected-problem relative key-path expected-value replacement))))))

(deftest audit-candidate-counts-and-mapping-joins-are-checked-test
  (doseq [[json-path key-path expected-value]
          [["compatibility_candidates" [:compatibility-candidates] 1]
           [["totals" "files_attempted"] [:totals :files-attempted] 17886]
           [["totals" "files_succeeded"] [:totals :files-succeeded] 17886]
           [["totals" "files_failed"] [:totals :files-failed] 0]
           [["mapping" "mapping_id"] [:mapping :mapping-id]
            "https://w3id.org/abc/mappings/aat-v2-to-parser-ir-v1/generated-probe"]
           [["mapping" "mapping_version"] [:mapping :mapping-version] "0.4.0"]
           [["mapping" "mapping_hash"] [:mapping :mapping-hash]
            "sha256:cf177bee98af086fe728cbc1942e4f631b26ed5bb55aedc7d91b21f467c41f30"]
           [["mapping" "mapping_schema_hash"] [:mapping :mapping-schema-hash]
            "sha256:e6af01115ccdb7c5cad086eee4c458230f6b6f55e0dfee7791730b48994283e2"]
           [["mapping" "target_parser_ir_schema_id"] [:mapping :parser-ir-schema-id]
            "https://w3id.org/abc/schemas/parser-ir.schema.json"]
           [["mapping" "target_parser_ir_schema_hash"] [:mapping :parser-ir-schema-hash]
            historical-schema-hash]
           [["mapping" "rules_total"] [:mapping :rules-total] 689]]]
    (let [replacement (if (number? expected-value) (inc expected-value) "changed")]
      (assert-problem!
       audit-path
       (if (= json-path "compatibility_candidates")
         #(assoc % json-path [])
         #(assoc-in % json-path replacement))
       (expected-problem audit-path key-path expected-value
                         (if (= json-path "compatibility_candidates") 0 replacement)))))
  (assert-problem!
   audit-path #(update % "compatibility_candidates" (fn [rows] (vec (concat rows rows))))
   (expected-problem audit-path [:compatibility-candidates] 1 2)))

(deftest conversion-details-and-checkpoint-are-checked-test
  (doseq [[json-path key-path expected-value]
          [["files_attempted" [:details :files-attempted] 17886]
           ["files_succeeded" [:details :files-succeeded] 17886]
           ["files_failed" [:details :files-failed] 0]
           ["mapping_version" [:details :mapping-version] "0.4.0"]
           ["mapping_hash" [:details :mapping-hash]
            "sha256:cf177bee98af086fe728cbc1942e4f631b26ed5bb55aedc7d91b21f467c41f30"]
           ["converter_bin_sha256" [:converter-bin-sha256]
            "74f507726f352cd5749b056e46fcdbfb6801b85ffa280ebc8b11a627528b7481"]]]
    (let [replacement (if (number? expected-value) (inc expected-value) "changed")]
      (assert-problem! conversion-path #(assoc-in % ["details" json-path] replacement)
                       (expected-problem conversion-path key-path expected-value replacement))))
  (assert-problem! checkpoint-path #(str "BROKEN" (subs % 6))
                   (expected-problem checkpoint-path [:prefix] "CHECKPOINT OK" "BROKENOINT OK")))

(deftest registry-cardinality-and-whole-row-are-checked-test
  (let [matches? #(= "0.4.0" (:mapping_version %))]
    (assert-problem! registry-path #(update % :entries (fn [rows] (vec (remove matches? rows))))
                     (expected-problem registry-path [:entries] 1 0))
    (assert-problem! registry-path
                     #(update % :entries (fn [rows]
                                           (let [row (first (filter matches? rows))]
                                             (conj rows row))))
                     (expected-problem registry-path [:entries] 1 2))
    (assert-problem! registry-path
                     #(update % :entries (fn [rows]
                                           (mapv (fn [row]
                                                   (if (matches? row) (assoc row :extra "x") row))
                                                 rows)))
                     (expected-problem registry-path [:extra] nil "x"))))

(deftest live-schema-rule-is-relative-and-mutation-sensitive-test
  (let [root (temp-root)]
    (try
      (with-redefs [hash/sha256-json-jcs (constantly (subs historical-schema-hash 7))]
        (is (some #{(expected-problem schema-path [:parser-ir-schema-hash]
                                      :not-historical historical-schema-hash)}
                  (frozen/frozen-tuple-problems root))))
      (finally (fs/delete-tree root))))
  (doseq [required ["work_content_hash" "primary_text_hash"]]
    (assert-problem!
     schema-path
     #(update-in % ["$defs" "orthographicAnnotations" "anyOf"]
                 (fn [alternatives]
                   (mapv (fn [alternative]
                           (update alternative "required"
                                   (fn [fields] (vec (remove #{required} fields)))))
                         alternatives)))
     (expected-problem schema-path [:source-role-alternatives]
                       #{"primary_text_hash" "work_content_hash"}
                       (disj #{"primary_text_hash" "work_content_hash"} required)))))

(deftest legacy-canonicalization-authoritative-golden-test
  (doseq [[value expected]
          [[{"b" 1 "a" 2} "{\"a\":2,\"b\":1}"]
           [{"path" "a/b/c"} "{\"path\":\"a\\/b\\/c\"}"]
           [{"z" [3 1 2] "y" "x/y"} "{\"y\":\"x\\/y\",\"z\":[3,1,2]}"]
           [{"u" "café"} "{\"u\":\"café\"}"]
           [{"nested" {"d/e" {"f" "/root"}}}
            "{\"nested\":{\"d\\/e\":{\"f\":\"\\/root\"}}}"]
           [[] "[]"] [{} "{}"]]]
    (is (= expected (legacy-canonical-json value)))
    (is (= (vec (.getBytes expected java.nio.charset.StandardCharsets/UTF_8))
           (vec (.getBytes (legacy-canonical-json value)
                           java.nio.charset.StandardCharsets/UTF_8))))))

(deftest multiple-mismatches-have-complete-deterministic-order-test
  (let [root (temp-root)]
    (try
      (mutate-edn! root compat-path #(assoc-in % [:entries 0 :aat_version] 3))
      (mutate-json! root perf-path #(assoc % "stage" "changed"))
      (spit (fs/file root checkpoint-path) "BROKENOINT OK\n")
      (is (= [(expected-problem compat-path [:aat-version] 2 3)
              (expected-problem registry-path [:aat-version] 3 2)
              (expected-problem perf-path [:stage] "c5" "changed")
              (expected-problem checkpoint-path [:prefix] "CHECKPOINT OK" "BROKENOINT OK")]
             (frozen/frozen-tuple-problems root)))
      (finally (fs/delete-tree root)))))

(deftest short-checkpoint-preserves-all-independent-problems-test
  (doseq [checkpoint ["" "SHORT"]]
    (let [root (temp-root)]
      (try
        (mutate-json! root perf-path #(assoc % "verdict" "FAIL"))
        (spit (fs/file root checkpoint-path) checkpoint)
        (is (= [(expected-problem perf-path [:verdict] "PASS" "FAIL")
                (expected-problem checkpoint-path [:prefix] "CHECKPOINT OK" checkpoint)]
               (frozen/frozen-tuple-problems root)))
        (finally (fs/delete-tree root))))))
