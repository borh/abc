(ns abc.tools.parser-phase5-frozen-tuple-test
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.parser-phase5-frozen-tuple :as frozen]
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
   "abc/data/aat-parser-ir-compatibility.edn"
   "abc/schemas/parser-ir.schema.json"])

(def compat-path (second evidence-files))

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
      (files/copy-file! (fs/file ".." relative) (fs/file root relative)))
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

(deftest checked-in-phase5-tuple-is-exact-test
  (let [root (fs/canonicalize "..")
        mapping (files/read-json
                 (fs/file root "ab-validator/data/aat-to-parser-ir-mapping-v2-0.4.0.json"))
        schema (files/read-json (fs/file root "abc/schemas/parser-ir.schema.json"))]
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

(deftest derived-bytes-schema-registry-and-gates-are-checked-test
  (doseq [[relative mutate key-path]
          [["ab-validator/data/aat-to-parser-ir-mapping-v2-0.4.0.json"
            #(assoc % "mapping_version" "live") [:mapping-byte-sha256]]
           ["abc/schemas/parser-ir.schema.json"
            #(assoc % "title" "substituted live schema") [:parser-ir-schema-hash]]
           ["abc/data/aat-parser-ir-compatibility.edn"
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
