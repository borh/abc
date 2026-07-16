(ns abc.tools.validate-design-bundle
  (:require ;; logging first so its load-time SLF4J filter is in place
            ;; before deps that emit chatty INFO logs (Aristotle/Jena/
            ;; Apache SSHD) get pulled in by other requires.
   [abc.tools.logging :as logging]
   [abc.tools.aat-parser-ir-compat :as compat]
   [abc.tools.analysis-identity :as analysis-identity]
   [abc.tools.evidence-io :as evidence-io]
   [abc.tools.files :as files]
   [abc.tools.hash :as hash]
   [abc.tools.iiif :as iiif]
   [abc.tools.jcs :as jcs]
   [abc.tools.linked-art :as linked-art]
   [abc.tools.malli :as am]
   [abc.tools.manifest-index :as manifest-index]
   [abc.tools.manifest-to-rdf :as manifest-to-rdf]
   [abc.tools.manifest :as manifest]
   [abc.tools.materialize-analysis :as analysis]
   [abc.tools.materialize-annotations :as annotations]
   [abc.tools.materialize-import :as materialize]
   [abc.tools.materialize-publication :as publication]
   [abc.tools.materialize-tokenized :as tokenized]
   [abc.tools.schema :as schema]
   [abc.tools.snapshot-index :as snapshot-index]
   [abc.tools.metadata-record :as metadata-record]
   [abc.tools.parser-evidence :as parser-evidence]
   [abc.tools.parser-maintenance-evidence :as parser-maintenance]
   [abc.tools.parser-ir-sentence-policy :as sentence-policy]
   [abc.tools.person-drift :as person-drift]
   [abc.tools.person-record :as person-record]
   [abc.tools.shacl :as shacl]
   [abc.tools.schematron :as schematron]
   [abc.tools.source-region-contract :as source-region]
   [abc.tools.tei :as tei]
   [arachne.aristotle :as aa]
   [babashka.fs :as fs]
   [babashka.process :as process]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as string]
   [taoensso.telemere :as tel]))

(def legacy-parser-ir-schema-hashes
  #{"sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396"
    "sha256:8e56871965e647e40ade08fd9dd580a3516d33905be17957cc79750bd42ea64d"
    "sha256:da916a3a92f64d985cb98f9b2ddc7f562e660fd0c3dbe0c902392d3764b0158a"
    "sha256:c081f2365e2159e6e608733c4eb4e6fdf1fa80203ccd3d5e1f2afc533da8d411"
    "sha256:0ab6f07e681b7adb14b9cacb14e4f406ef122151df4d1554503e77a3f1faf8c2"
    "sha256:0b495bb5c12c4d76482afefdaedb5464a74672ffbd5282f9c67d5f419d39a340"
    "sha256:40d7ff6683a395e8727de55574c3af1fd70475cfa325ae5b67cc19fdb3eb32b6"
    "sha256:a1e1b5069fdec17cbb1f94eb5e9a582d1b109dd95c07257f4da7d9b76c82cfa2"})

(defn accepted-parser-ir-schema-hashes []
  (conj legacy-parser-ir-schema-hashes
        (manifest/schema-hash "schemas/parser-ir.schema.json")))

(defn tokenizer-profiles-by-hash []
  (->> (fs/glob "data/tokenizer-profiles" "*.json")
       (map (comp files/read-json fs/file))
       (map (juxt analysis-identity/tokenizer-profile-hash identity))
       (into {})))

(def ^:private fixture-tokenized-tokens
  [{"token_index" 0
    "input_span" {"start" 0
                  "end" 2}
    "text" "吾輩"}
   {"token_index" 1
    "input_span" {"start" 2
                  "end" 3}
    "text" "猫"}])

(def ^:private fixture-analysis-subject
  {"source_id" "aozora:example-work"
   "logical_path" "aozora/example-work.txt"
   "git_ref" "refs/heads/fixture"
   "work_id" "aozora:example-work"})

(def ^:private fixture-token-analysis-metrics
  [{"metric_id" "fixture-token-count"
    "value" 2
    "value_type" "integer"
    "denominator" nil
    "unit" "token"
    "status" "passed"}])

(defn- parser-ir-schema-hash-accepted? [value]
  (contains? (accepted-parser-ir-schema-hashes) value))

(defn schema-hash-errors [manifest-inputs]
  (let [expected-parser-ir (manifest/schema-hash "schemas/parser-ir.schema.json")
        expected-diagnostic (manifest/schema-hash "schemas/diagnostic.schema.json")
        actual-parser-ir (get manifest-inputs "parser_ir_schema_hash")
        actual-diagnostic (get manifest-inputs "diagnostic_schema_hash")]
    (vec
     (concat
      (when (and actual-parser-ir
                 (not (parser-ir-schema-hash-accepted? actual-parser-ir)))
        [(str "ab-validator parser_ir_schema_hash " actual-parser-ir
              " does not match ABC parser IR schema hash " expected-parser-ir)])
      (when (and actual-diagnostic (not= expected-diagnostic actual-diagnostic))
        [(str "ab-validator diagnostic_schema_hash " actual-diagnostic
              " does not match ABC diagnostic schema hash " expected-diagnostic)])))))

(defn parser-ir-schema-hash-errors [parser-ir]
  (let [expected-parser-ir (manifest/schema-hash "schemas/parser-ir.schema.json")
        actual-parser-ir (get parser-ir "schema_hash")]
    (vec
     (when-not (parser-ir-schema-hash-accepted? actual-parser-ir)
       [(str "ab-validator parser IR schema_hash " actual-parser-ir
             " does not match ABC parser IR schema hash " expected-parser-ir)]))))

(defn- duplicate-paragraph-id-errors [paragraphs]
  (->> paragraphs
       (map #(get % "id"))
       frequencies
       (keep (fn [[paragraph-id count]]
               (when (> count 1)
                 (str "parser IR paragraphs[] contains duplicate id " paragraph-id))))
       sort))

(defn- node-range-label [start end]
  (str start ".." end))

(defn- source-note-node-in-range? [nodes start end]
  (boolean
   (some #(= "source-note" (get % "type"))
         (subvec nodes start end))))

(defn parser-ir-paragraph-coherence-errors [parser-ir]
  (let [nodes (vec (get parser-ir "nodes" []))
        paragraphs (vec (get parser-ir "paragraphs" []))]
    (vec
     (concat
      (duplicate-paragraph-id-errors paragraphs)
      (loop [remaining paragraphs
             previous-end 0
             errors []]
        (if-let [paragraph (first remaining)]
          (let [paragraph-id (get paragraph "id")
                node-range (get paragraph "node_range")
                start (get node-range "start")
                end (get node-range "end")
                outside? (not (and (integer? start)
                                   (integer? end)
                                   (<= 0 start end (count nodes))))
                non-monotonic? (and (integer? start)
                                    (< start previous-end))
                source-note-missing? (and (not outside?)
                                          (= "source-note" (get paragraph "role"))
                                          (= "direct" (get paragraph "classification"))
                                          (not (source-note-node-in-range?
                                                nodes start end)))
                errors (cond-> errors
                         outside?
                         (conj (str "parser IR paragraph " paragraph-id
                                    " node_range " (node-range-label start end)
                                    " is outside nodes[] length " (count nodes)))

                         (and (not outside?) non-monotonic?)
                         (conj (str "parser IR paragraph " paragraph-id
                                    " node_range starts before previous paragraph end "
                                    previous-end))

                         source-note-missing?
                         (conj (str "parser IR paragraph " paragraph-id
                                    " has role source-note but no source-note node in node_range")))]
            (recur (rest remaining)
                   (if (and (integer? end) (not outside?))
                     end
                     previous-end)
                   errors))
          errors))))))

(defn parser-ir-sentence-coherence-errors [parser-ir]
  (sentence-policy/sentence-coherence-errors parser-ir))

(defn derived-from-compatibility-query [parser-ir manifest-inputs]
  (let [derived-from (get parser-ir "derived_from")]
    {:aat_version (get derived-from "aat_version")
     :aat_adapter (get derived-from "aat_adapter")
     :aat_adapter_version (get derived-from "aat_adapter_version")
     :mapping_id (get derived-from "mapping_id")
     :mapping_version (get derived-from "mapping_version")
     :mapping_hash (get manifest-inputs "mapping_hash")
     :mapping_schema_hash (get derived-from "mapping_schema_hash")
     :parser_ir_schema_id (get parser-ir "schema_id")
     :parser_ir_schema_hash (get parser-ir "schema_hash")}))

(defn divergence-bundle-compatibility-query [parser-ir manifest-inputs divergence-bundle]
  (let [mapping (get divergence-bundle "mapping")
        target (get divergence-bundle "target")
        aat (get divergence-bundle "aat")]
    {:aat_version (get aat "version")
     :aat_adapter (get aat "adapter")
     :aat_adapter_version (get aat "adapter_version")
     :mapping_id (get mapping "mapping_id")
     :mapping_version (get mapping "mapping_version")
     :mapping_hash (get manifest-inputs "mapping_hash")
     :mapping_schema_hash (get mapping "mapping_schema_hash")
     :parser_ir_schema_id (or (get target "parser_ir_schema_id")
                              (get parser-ir "schema_id"))
     :parser_ir_schema_hash (or (get target "parser_ir_schema_hash")
                                (get parser-ir "schema_hash"))}))

(def compatibility-derived-from-keys
  ["aat_version"
   "aat_adapter"
   "aat_adapter_version"
   "mapping_id"
   "mapping_version"
   "mapping_schema_hash"])

(defn missing-derived-from-key-errors [derived-from]
  (->> compatibility-derived-from-keys
       (remove #(contains? derived-from %))
       (mapv #(str "AAT parser-IR compatibility requires parser IR derived_from." %))))

(defn divergence-bundle-target-errors [parser-ir divergence-bundle]
  (let [target (get divergence-bundle "target")]
    (vec
     (concat
      (when (not= (get parser-ir "schema_id")
                  (get target "parser_ir_schema_id"))
        [(str "AAT parser-IR divergence bundle target parser_ir_schema_id "
              (get target "parser_ir_schema_id")
              " does not match parser IR schema_id "
              (get parser-ir "schema_id"))])
      (when (not= (get parser-ir "schema_hash")
                  (get target "parser_ir_schema_hash"))
        [(str "AAT parser-IR divergence bundle target parser_ir_schema_hash "
              (get target "parser_ir_schema_hash")
              " does not match parser IR schema_hash "
              (get parser-ir "schema_hash"))])))))

(defn compatibility-mismatch-error [{:keys [aat_adapter
                                            aat_version
                                            mapping_id
                                            mapping_version
                                            mapping_hash
                                            mapping_schema_hash
                                            parser_ir_schema_id
                                            parser_ir_schema_hash]}]
  (str "AAT parser-IR compatibility registry has no entry for adapter " aat_adapter
       ", AAT version " aat_version
       ", mapping " mapping_id " " mapping_version
       ", mapping hash " mapping_hash
       ", mapping schema hash " mapping_schema_hash
       ", parser IR schema id " parser_ir_schema_id
       ", parser IR schema hash " parser_ir_schema_hash))

(defn compatibility-errors
  ([parser-ir manifest-inputs]
   (compatibility-errors (compat/load-registry) parser-ir manifest-inputs nil))
  ([registry parser-ir manifest-inputs]
   (compatibility-errors registry parser-ir manifest-inputs nil))
  ([registry parser-ir manifest-inputs divergence-bundle]
   (let [derived-from (get parser-ir "derived_from")
         mapping-hash (get manifest-inputs "mapping_hash")]
     (vec
      (cond
        (and (nil? derived-from) (nil? divergence-bundle) (nil? mapping-hash))
        []

        (nil? mapping-hash)
        ["AAT parser-IR compatibility requires manifest inputs mapping_hash when parser IR mapping provenance is present"]

        (and (nil? derived-from) (nil? divergence-bundle))
        ["AAT parser-IR compatibility requires parser IR derived_from or divergence bundle when manifest inputs mapping_hash is present"]

        :else
        (let [metadata-errors (concat
                               (when derived-from
                                 (missing-derived-from-key-errors derived-from))
                               (when divergence-bundle
                                 (divergence-bundle-target-errors parser-ir divergence-bundle)))]
          (if (seq metadata-errors)
            metadata-errors
            (let [queries (cond-> []
                            derived-from
                            (conj (derived-from-compatibility-query parser-ir manifest-inputs))
                            divergence-bundle
                            (conj (divergence-bundle-compatibility-query
                                   parser-ir manifest-inputs divergence-bundle)))]
              (->> queries
                   (remove #(compat/compatible? registry %))
                   (mapv compatibility-mismatch-error))))))))))

(defn source-region-policy-errors [policy]
  (source-region/policy-errors policy))

(defn source-region-coverage-errors [coverage policy]
  (source-region/coverage-errors coverage policy))

(defn validation-errors [schema value]
  (schema/validation-errors schema value))

(defn validate-json! [schema path]
  (schema/validate-json! schema path))

(defn validate-json-lines! [schema path {:keys [require-nonempty]}]
  (let [values (files/read-json-lines path)]
    (when (and require-nonempty (empty? values))
      (throw (ex-info (str path " must contain at least one JSON object")
                      {:path (str path)})))
    (schema/validate-jsonl! schema values path)))

(defn schema-valid! [schema path]
  (schema/schema-valid! schema path))

(defn run-command! [& command]
  (try
    (process/shell (vec command) {:in :inherit :out :inherit :err :inherit})
    nil
    (catch clojure.lang.ExceptionInfo ex
      (throw (ex-info (str "Command failed: " (string/join " " command))
                      {:command command :exit-code (:exit (ex-data ex))}
                      ex)))))

(defn check-errors! [errors]
  (when (seq errors)
    (throw (ex-info (string/join "\n" errors)
                    {:errors errors}))))

(defn validate-maintenance-evidence!
  ([record as-of]
   (validate-maintenance-evidence! "." record as-of))
  ([repo-root record as-of]
   (check-errors! (concat (parser-maintenance/problems record as-of)
                          (parser-maintenance/benchmark-artifact-problems
                           repo-root record)))))

(defn- classified-source-duplicate-errors [label values]
  (->> values frequencies
       (keep (fn [[value count]]
               (when (> count 1)
                 (str "classified-source policy contains ambiguous "
                      label " " value))))))

(defn parser-rq-classified-source-policy-errors [policy]
  (->> (concat
        (classified-source-duplicate-errors
         "selector"
         (map #(select-keys % ["construct_id"])
              (get policy "rules" [])))
        (classified-source-duplicate-errors
         "accent source"
         (map #(get % "source")
              (get policy "accent_mappings" []))))
       sort vec))

(def ^:private parser-rq-diagnostic-gap-vocabulary
  [["source-contains-pua" "source_contains_pua" "warning" "source" "authorize_exact_span"]
   ["unclosed-bracket" "unclosed_bracket" "error" "source" "observe_only"]
   ["unmatched-close" "unmatched_close" "error" "source" "observe_only"]
   ["accent-decomposition-applied" "accent_decomposition_applied" "note" "source" "observe_only"]
   ["unresolved-gaiji" "unresolved_gaiji" "warning" "source" "observe_only"]
   ["mismatched-container-close" "mismatched_container_close" "error" "source" "observe_only"]
   ["empty-ruby-reading" "empty_ruby_reading" "error" "source" "observe_only"]
   ["nested-ruby" "nested_ruby" "error" "source" "observe_only"]
   ["unrecognised-container-directive" "unrecognised_container_directive" "warning" "source" "observe_only"]
   ["tcy-target-not-found" "tcy_target_not_found" "warning" "source" "observe_only"]
   ["bouten-target-ambiguous" "bouten_target_ambiguous" "warning" "source" "observe_only"]
   ["forward-referent-not-stylable" "forward_referent_not_stylable" "warning" "source" "observe_only"]
   ["break-in-single-line-container" "break_in_single_line_container" "warning" "source" "observe_only"]
   ["bracketed-kaeriten-no-pair" "bracketed_kaeriten_no_pair" "error" "source" "observe_only"]
   ["kaeriten-outside-kanbun" "kaeriten_outside_kanbun" "warning" "source" "observe_only"]
   ["mismatched-bouten-container" "mismatched_bouten_container" "error" "source" "observe_only"]
   ["non-canonical-directive" "non_canonical_directive" "warning" "source" "observe_only"]
   ["residual-annotation-marker" "residual_annotation_marker" "error" "internal" "reject_internal"]
   ["unregistered-sentinel" "unregistered_sentinel" "error" "internal" "reject_internal"]
   ["registry-out-of-order" "registry_out_of_order" "error" "internal" "reject_internal"]
   ["registry-position-mismatch" "registry_position_mismatch" "error" "internal" "reject_internal"]])

(defn parser-rq-diagnostic-gap-policy-errors [policy]
  (let [rules (get policy "rules" [])
        tuples (mapv (fn [rule]
                       (mapv #(get rule %)
                             ["code" "kind" "severity" "source" "disposition"]))
                     rules)
        raw-schema (files/read-json
                    "schemas/parser-rq-ab-aozora-diagnostics-v3.schema.json")
        expected-schema-hash (hash/format-sha256
                              (hash/sha256-json-jcs raw-schema))
        expected-policy-hash (hash/format-sha256
                              (hash/sha256-json-jcs
                               (dissoc policy "policy_hash")))]
    (vec
     (concat
      (when-not (= (count tuples) (count (distinct tuples)))
        ["diagnostic-gap policy contains duplicate or ambiguous selectors"])
      (when-not (= (count rules) (count (distinct (map #(get % "code") rules))))
        ["diagnostic-gap policy contains duplicate selector codes"])
      (when-not (= parser-rq-diagnostic-gap-vocabulary tuples)
        ["diagnostic-gap policy does not equal the closed ABC vocabulary"])
      (when-not (= expected-schema-hash
                   (get policy "raw_diagnostic_schema_hash"))
        ["diagnostic-gap policy raw diagnostic schema hash mismatch"])
      (when-not (= expected-policy-hash (get policy "policy_hash"))
        ["diagnostic-gap policy identity hash mismatch"])))))

(defn- unicode-private-use? [value]
  (when (and (string? value)
             (= 1 (.codePointCount ^String value 0 (.length ^String value))))
    (let [codepoint (.codePointAt ^String value 0)]
      (or (<= 0xE000 codepoint 0xF8FF)
          (<= 0xF0000 codepoint 0xFFFFD)
          (<= 0x100000 codepoint 0x10FFFD)))))

(declare decoded-slice)

(defn parser-rq-raw-diagnostics-errors [policy capture decoded]
  (let [raw-schema (files/read-json
                    "schemas/parser-rq-ab-aozora-diagnostics-v3.schema.json")
        policy-by-code (into {}
                             (map (fn [rule] [(get rule "code") rule])
                                  (get policy "rules" [])))]
    (vec
     (concat
      (when-let [errors (schema/validation-errors raw-schema capture)]
        (map #(str "raw diagnostics schema: " %) errors))
      (parser-rq-diagnostic-gap-policy-errors policy)
      (when-not (= (count (get capture "data" []))
                   (count (distinct (get capture "data" []))))
        ["raw diagnostics contain duplicate complete diagnostic identities"])
      (mapcat
       (fn [diagnostic]
         (let [code (get diagnostic "code")
               rule (get policy-by-code code)
               span (get diagnostic "span")
               source-slice (when (and (map? span)
                                       (integer? (get span "start"))
                                       (integer? (get span "end"))
                                       (string? decoded))
                              (decoded-slice decoded
                                             (get span "start")
                                             (get span "end")))
               selector (select-keys diagnostic
                                     ["code" "kind" "severity" "source"])
               expected (select-keys rule
                                     ["code" "kind" "severity" "source"])]
           (concat
            (when-not (= selector expected)
              [(str "raw diagnostic selector does not match policy for " code)])
            (when (= "internal" (get diagnostic "source"))
              [(str "raw diagnostic uses rejected internal source for " code)])
            (when-not source-slice
              [(str "raw diagnostic span is not a nonempty decoded UTF-8 interval for " code)])
            (when (= code "source-contains-pua")
              (let [codepoint (get diagnostic "codepoint")]
                (concat
                 (when-not (unicode-private-use? codepoint)
                   ["source-contains-pua codepoint is not one Unicode private-use scalar"])
                 (when-not (= source-slice codepoint)
                   ["source-contains-pua span does not equal its codepoint"])))))))
       (get capture "data" []))))))

(defn parser-rq-diagnostic-gap-result-coherence-errors
  [result source-recognition-work source-recognition-artifact-ref]
  (if (not= "ok" (get result "status"))
    []
    (let [evidence (get result "source_recognition_evidence")
          expected-value-hash
          (hash/format-sha256 (hash/sha256-json-jcs source-recognition-work))]
      (vec
       (concat
        (when-not (= source-recognition-artifact-ref
                     (get evidence "artifact_ref"))
          ["diagnostic-gap result source-recognition artifact reference mismatch"])
        (when-not (= expected-value-hash (get evidence "value_hash"))
          ["diagnostic-gap result source-recognition value hash mismatch"])
        (for [[field outer-value]
              [["work_id" (get result "work_id")]
               ["capture_generation_ref" (get result "capture_generation_ref")]
               ["qualification_identity_ref"
                (get source-recognition-work "qualification_identity_ref")]]
              :when (or (not= outer-value (get evidence field))
                        (not= outer-value (get source-recognition-work field)))]
          (str "diagnostic-gap result source-recognition " field
               " is not coherent")))))))

(defn parser-rq-classified-source-characterization-errors [policy mapping]
  (let [constructs (set (map #(get % "construct_id") (get policy "rules" [])))
        observations (get mapping "observations" [])]
    (vec
     (concat
      (when-not (= 36 (count observations))
        [(str "characterization map must contain 36 observations, found "
              (count observations))])
      (->> observations
           (keep (fn [observation]
                   (when-not (contains? constructs (get observation "construct_id"))
                     (str "characterization case " (get observation "case")
                          " maps to unknown construct "
                          (get observation "construct_id")))))
           sort)))))

(defn- decoded-slice [decoded start end]
  (let [bytes (.getBytes ^String decoded "UTF-8")]
    (when (and (<= 0 start) (< start end) (<= end (alength bytes))
               (or (zero? start)
                   (not (<= -128 (aget bytes start) -65)))
               (or (= end (alength bytes))
                   (not (<= -128 (aget bytes end) -65))))
      (String. bytes start (- end start) "UTF-8"))))

(defn- utf8-value-hash [value]
  (hash/format-sha256 (hash/sha256-bytes (.getBytes ^String value "UTF-8"))))

(defn- accent-normalized-form [policy source-form]
  (when (and (string/starts-with? source-form "〔")
             (string/ends-with? source-form "〕"))
    (let [body (subs source-form 1 (dec (count source-form)))
          mappings (into {} (map (juxt #(get % "source")
                                       #(get % "normalized"))
                                 (get policy "accent_mappings" [])))]
      (str "〔"
           (loop [index 0 output (StringBuilder.)]
             (if (= index (count body))
               (str output)
               (let [remaining (- (count body) index)
                     source (or (when (<= 3 remaining)
                                  (let [candidate (subs body index (+ index 3))]
                                    (when (contains? mappings candidate) candidate)))
                                (when (<= 2 remaining)
                                  (let [candidate (subs body index (+ index 2))]
                                    (when (contains? mappings candidate) candidate))))]
                 (if source
                   (recur (+ index (count source))
                          (.append output ^String (get mappings source)))
                   (recur (inc index) (.append output (.charAt body index)))))))
           "〕"))))

(defn- normalization-errors [policy construct proof source-slice]
  (let [source-form (get proof "source_form")
        normalized-form (get proof "normalized_form")
        inverse-rule (get proof "inverse_rule")
        expected-rule {"crlf_normalization" "crlf"
                       "bare_cr_normalization" "bare_cr"
                       "accent_normalization" "accent_decomposition"}
        exact-forms {"crlf" ["\r\n" "\n"]
                     "bare_cr" ["\r" "\n"]}]
    (concat
     (when-not (= source-slice source-form)
       [(str "normalization source form does not match decoded bytes for "
             construct)])
     (when-not (= (get expected-rule construct) inverse-rule)
       [(str "normalization inverse rule does not match policy for " construct)])
     (when (and (contains? exact-forms inverse-rule)
                (not= (get exact-forms inverse-rule)
                      [source-form normalized-form]))
       [(str "normalization forms do not match inverse rule for " construct)])
     (when (and (= "accent_decomposition" inverse-rule)
                (or (= source-form normalized-form)
                    (not= normalized-form
                          (accent-normalized-form policy source-form))))
       [(str "accent normalization proof does not match the closed mapping for "
             construct)])
     (when (and (string? source-form)
                (not= (get proof "source_bytes_hash")
                      (utf8-value-hash source-form)))
       [(str "normalization source hash does not match proof for " construct)])
     (when (and (string? normalized-form)
                (not= (get proof "normalized_bytes_hash")
                      (utf8-value-hash normalized-form)))
       [(str "normalization target hash does not match proof for " construct)]))))

(defn parser-rq-classified-source-ledger-errors [policy ledger decoded]
  (let [rules (into {} (map (juxt #(get % "construct_id") identity)
                            (get policy "rules" [])))]
    (->> (get ledger "entries" [])
         (mapcat
          (fn [entry]
            (let [construct (get entry "construct_id")
                  rule (get rules construct)
                  witness (get entry "construct_witness")
                  proof (get entry "normalization_proof")
                  target (get entry "target_identity")
                  expected (select-keys rule ["source_role" "disposition"
                                              "evidence_class"])
                  actual (select-keys entry ["source_role" "disposition"
                                             "evidence_class"])]
              (concat
               (when-not rule
                 [(str "ledger entry uses unknown construct " construct)])
               (when (and rule (not= expected actual))
                 [(str "ledger entry does not match policy for " construct)])
               (when (and rule (contains? rule "target_relation")
                          (not= (get rule "target_relation")
                                (get target "relation")))
                 [(str "ledger target relation does not match policy for " construct)])
               (when (and rule (contains? rule "witness_kind")
                          (not= (get rule "witness_kind")
                                (get witness "construct_id")))
                 [(str "ledger witness does not match policy for " construct)])
               (when (and (= "lossless_normalization" (get rule "disposition"))
                          (nil? proof))
                 [(str "ledger normalization proof is missing for " construct)])
               (when (and witness
                          (not= [(get entry "start") (get entry "end")]
                                [(get witness "start") (get witness "end")]))
                 [(str "ledger witness span does not match entry for " construct)])
               (when (and witness
                          (not= (get witness "source_form")
                                (decoded-slice decoded (get entry "start")
                                               (get entry "end"))))
                 [(str "ledger witness source form does not match decoded bytes for "
                       construct)])
               (when proof
                 (normalization-errors
                  policy construct proof
                  (decoded-slice decoded (get entry "start")
                                 (get entry "end"))))))))
         sort
         vec)))

(defn parser-rq-capture-generation-errors [generation]
  (let [expected (get generation "generation_ref")
        identity (dissoc generation "generation_ref")
        computed (hash/format-sha256
                  (hash/sha256-bytes
                   (jcs/rfc8785-string-domain-json-bytes identity)))]
    (vec
     (when (not= expected computed)
       [(str "capture generation_ref does not match canonical identity: "
             expected " != " computed)]))))

(defn- interval-bytes [intervals]
  (reduce + 0 (map #(- (get % "end") (get % "start")) intervals)))

(defn- canonical-interval-errors [label intervals eligible-bytes]
  (let [pairs (partition 2 1 intervals)]
    (vec
     (concat
      (keep-indexed
       (fn [index interval]
         (let [start (get interval "start") end (get interval "end")]
           (when (or (not (integer? start)) (not (integer? end))
                     (>= start end) (neg? start) (> end eligible-bytes))
             (str label " interval " index " is outside eligible bytes"))))
       intervals)
      (keep-indexed
       (fn [index [left right]]
         (when (>= (get left "end") (get right "start"))
           (str label " intervals " index " and " (inc index)
                " overlap or are not maximally normalized")))
       pairs)))))

(defn- interval-subset? [inner outer]
  (every? (fn [interval]
            (some #(and (<= (get % "start") (get interval "start"))
                        (>= (get % "end") (get interval "end")))
                  outer))
          inner))

(defn- interval-complement [intervals eligible-bytes]
  (loop [cursor 0 remaining intervals result []]
    (if-let [interval (first remaining)]
      (recur (get interval "end") (next remaining)
             (cond-> result
               (< cursor (get interval "start"))
               (conj {"start" cursor "end" (get interval "start")})))
      (cond-> result
        (< cursor eligible-bytes)
        (conj {"start" cursor "end" eligible-bytes})))))

(defn parser-rq-source-recognition-work-errors [record]
  (if (not= "ok" (get record "status"))
    []
    (let [eligible (get record "eligible_bytes")
          recognized (get record "recognized")
          accounted (get record "accounted")
          semantic-gaps (get record "semantic_gaps")
          unaccounted (get record "unaccounted")]
      (vec
       (concat
        (mapcat #(canonical-interval-errors (first %) (second %) eligible)
                [["recognized" recognized] ["accounted" accounted]
                 ["semantic_gaps" semantic-gaps]
                 ["unaccounted" unaccounted]])
        (when-not (interval-subset? recognized accounted)
          ["recognized intervals are not a subset of accounted intervals"])
        (when-not (= recognized
                     (sort-by (fn [interval]
                                [(get interval "start") (get interval "end")])
                              recognized))
          ["recognized intervals are not in canonical order"])
        (when-not (= (get record "recognized_bytes")
                     (interval-bytes recognized))
          ["recognized_bytes does not equal recognized intervals"])
        (when-not (= (get record "accounted_bytes")
                     (interval-bytes accounted))
          ["accounted_bytes does not equal accounted intervals"])
        (when-not (= (get record "semantic_gap_bytes")
                     (interval-bytes semantic-gaps))
          ["semantic_gap_bytes does not equal semantic gaps"])
        (when-not (= (get record "unaccounted_bytes")
                     (interval-bytes unaccounted))
          ["unaccounted_bytes does not equal unaccounted intervals"])
        (when-not (= semantic-gaps (interval-complement recognized eligible))
          ["semantic gaps are not the exact recognized complement"])
        (when-not (= unaccounted (interval-complement accounted eligible))
          ["unaccounted intervals are not the exact accounted complement"])
        (when-not (= eligible (+ (get record "recognized_bytes")
                                 (get record "semantic_gap_bytes")))
          ["recognized byte conservation does not hold"])
        (when-not (= eligible (+ (get record "accounted_bytes")
                                 (get record "unaccounted_bytes")))
          ["accounted byte conservation does not hold"]))))))

(defn parser-rq-source-recognition-index-errors [index]
  (let [expected-ids (get index "expected_work_ids" [])
        record-ids (mapv #(get % "work_id") (get index "records" []))
        record-id-set (set record-ids)
        asserted-ref (get index "corpus_generation_ref")
        canonical-result
        (try
          {:computed-ref
           (hash/format-sha256
            (hash/sha256-json-rfc8785-safe-integer-v1
             (dissoc index "corpus_generation_ref")))}
          (catch clojure.lang.ExceptionInfo error
            {:canonical-error (ex-data error)}))]
    (vec
     (concat
      (when (:canonical-error canonical-result)
        ["corpus generation index is outside the authenticated safe-integer JSON domain"])
      (when (and (:computed-ref canonical-result)
                 (not= asserted-ref (:computed-ref canonical-result)))
        ["corpus_generation_ref does not authenticate the closed record index"])
      (when-not (= (count expected-ids) (get index "expected_work_count"))
        ["expected_work_count does not equal expected membership"])
      (when-not (= (count record-ids) (get index "record_count"))
        ["record_count does not equal records"])
      (when-not (= (count expected-ids) (count (distinct expected-ids)))
        ["expected membership contains duplicate work IDs"])
      (when-not (= (count record-ids) (count (distinct record-ids)))
        ["record index contains duplicate work IDs"])
      (when-not (= record-ids
                   (filterv #(contains? record-id-set %) expected-ids))
        ["record index is not an ordered subset of expected membership"])
      (when (and (= "ok" (get index "status"))
                 (not= (set expected-ids) record-id-set))
        ["available record index does not exactly match expected membership"])))))

(defn- work-interval-errors [label intervals]
  (mapcat
   (fn [[work-id work-intervals]]
     (canonical-interval-errors
      (str label " for " work-id)
      (mapv #(select-keys % ["start" "end"]) work-intervals)
      (reduce max 0 (map #(get % "end") work-intervals))))
   (group-by #(get % "work_id") intervals)))

(defn parser-rq-source-recognition-aggregate-errors [aggregate]
  (if (not= "ok" (get aggregate "status"))
    []
    (let [eligible (get aggregate "eligible_bytes")
          recognized (get aggregate "recognized_bytes")
          accounted (get aggregate "accounted_bytes")
          semantic-gap (get aggregate "semantic_gap_bytes")
          unaccounted-bytes (get aggregate "unaccounted_bytes")
          semantic-gaps (get aggregate "semantic_gaps")
          unaccounted (get aggregate "unaccounted")]
      (vec
       (concat
        (work-interval-errors "semantic gaps" semantic-gaps)
        (work-interval-errors "unaccounted" unaccounted)
        (when (> recognized accounted)
          ["aggregate recognized_bytes exceeds accounted_bytes"])
        (when (> accounted eligible)
          ["aggregate accounted_bytes exceeds eligible_bytes"])
        (when-not (= semantic-gap (interval-bytes semantic-gaps))
          ["aggregate semantic_gap_bytes does not equal witnesses"])
        (when-not (= unaccounted-bytes (interval-bytes unaccounted))
          ["aggregate unaccounted_bytes does not equal witnesses"])
        (when-not (= eligible (+ recognized semantic-gap))
          ["aggregate recognized byte conservation does not hold"])
        (when-not (= eligible (+ accounted unaccounted-bytes))
          ["aggregate accounted byte conservation does not hold"]))))))

(defn- source-recognition-aggregate-gaps [records key]
  (vec (mapcat (fn [record]
                 (map #(assoc % "work_id" (get record "work_id"))
                      (get record key)))
               records)))

(defn parser-rq-source-recognition-coherence-errors [index aggregate records]
  (let [identity-keys ["qualification_identity_ref" "policy_hash"
                       "coordinate_system"]
        completeness (get aggregate "work_completeness")
        indexed-ids (set (map #(get % "work_id") (get index "records" [])))
        entries-by-id (group-by #(get % "work_id") (get index "records" []))
        records-by-id (group-by #(get % "work_id") records)
        ok-records (->> (get index "records" [])
                        (map #(first (get records-by-id (get % "work_id"))))
                        (filter #(= "ok" (get % "status"))))]
    (vec
     (concat
      (when-not (= indexed-ids (set (keys records-by-id)))
        ["loaded records do not exactly match indexed membership"])
      (when-not (every? #(= 1 (count %)) (vals records-by-id))
        ["loaded records contain duplicate work IDs"])
      (mapcat
       (fn [record]
         (let [work-id (get record "work_id")
               entry (first (get entries-by-id work-id))]
           (concat
            (keep (fn [key]
                    (when-not (= (get index key) (get record key))
                      (str "work " work-id " has mismatched " key)))
                  identity-keys)
            (when-not (= (get entry "capture_generation_ref")
                         (get record "capture_generation_ref"))
              [(str "work " work-id
                    " does not match its indexed capture_generation_ref")]))))
       records)
      (keep (fn [key]
              (when-not (= (get index key) (get aggregate key))
                (str "aggregate has mismatched " key)))
            identity-keys)
      (when-not (= (get index "corpus_generation_ref")
                   (get aggregate "corpus_generation_ref"))
        ["aggregate has mismatched corpus_generation_ref"])
      (when-not (= (get index "corpus_generation_algorithm")
                   (get aggregate "corpus_generation_algorithm"))
        ["aggregate has mismatched corpus_generation_algorithm"])
      (when-not (= (get index "membership_ref")
                   (get aggregate "membership_ref"))
        ["aggregate has mismatched membership_ref"])
      (when (= "ok" (get aggregate "status"))
        (concat
         (when-not (= "ok" (get index "status"))
           ["available aggregate requires an available index"])
         (when-not (true? (get completeness "complete"))
           ["available aggregate requires complete work membership"])
         (when-not (= (get completeness "expected")
                      (get index "expected_work_count"))
           ["aggregate expected count does not equal index membership"])
         (when-not (= (get completeness "observed")
                      (get index "record_count"))
           ["aggregate observed count does not equal index records"])
         (when-not (= (get completeness "observed") (count records))
           ["aggregate observed count does not equal loaded records"])
         (when-not (= (get completeness "expected")
                      (get completeness "observed"))
           ["available aggregate expected and observed counts differ"])
         (when-not (every? #(= "ok" (get % "status")) records)
           ["available aggregate contains unavailable records"])
         (for [[field key] [["eligible_bytes" "eligible_bytes"]
                            ["recognized_bytes" "recognized_bytes"]
                            ["accounted_bytes" "accounted_bytes"]
                            ["semantic_gap_bytes" "semantic_gap_bytes"]
                            ["unaccounted_bytes" "unaccounted_bytes"]]
               :let [expected (reduce + 0 (map #(get % key) ok-records))]
               :when (not= expected (get aggregate field))]
           (str "aggregate " field " does not equal work records"))
         (when-not (= (get aggregate "semantic_gaps")
                      (source-recognition-aggregate-gaps
                       ok-records "semantic_gaps"))
           ["aggregate semantic gaps do not equal work records"])
         (when-not (= (get aggregate "unaccounted")
                      (source-recognition-aggregate-gaps
                       ok-records "unaccounted"))
           ["aggregate unaccounted intervals do not equal work records"])))))))

(def ^:private design-schema-inputs
  ["schemas/aat-parser-ir-divergence-bundle.schema.json"
   "schemas/aat-parser-ir-divergence.schema.json"
   "schemas/aat-parser-ir-mapping.schema.json"
   "schemas/adr-claim-migration-baseline.schema.json"
   "schemas/adr-evidence-run.schema.json"
   "schemas/adr-external-evidence.schema.json"
   "schemas/analysis-recipe.schema.json"
   "schemas/analysis-result.schema.json"
   "schemas/annotation-output.schema.json"
   "schemas/comparison-report.schema.json"
   "schemas/custom-parser-maintenance-evidence.schema.json"
   "schemas/diagnostic.schema.json"
   "schemas/iiif-applicability.schema.json"
   "schemas/manifest-inputs.schema.json"
   "schemas/manifest.schema.json"
   "schemas/pack-policy.schema.json"
   "schemas/parser-rq-ignored-regions.schema.json"
   "schemas/parser-rq-capture-generation.schema.json"
   "schemas/parser-rq-ab-aozora-diagnostics-v3.schema.json"
   "schemas/parser-rq-classified-source-ledger.schema.json"
   "schemas/parser-rq-classified-source-policy.schema.json"
   "schemas/parser-rq-diagnostic-gap-policy.schema.json"
   "schemas/parser-rq-diagnostic-gap-aggregate.schema.json"
   "schemas/parser-rq-diagnostic-gap-result.schema.json"
   "schemas/parser-rq-source-accountability-aggregate.schema.json"
   "schemas/parser-rq-source-accountability-index.schema.json"
   "schemas/parser-rq-source-accountability-work.schema.json"
   "schemas/parser-rq-source-recognition-aggregate.schema.json"
   "schemas/parser-rq-source-recognition-index.schema.json"
   "schemas/parser-rq-source-recognition-work.schema.json"
   "schemas/parser-rq-publication-aggregate.schema.json"
   "schemas/parser-rq-publication-index.schema.json"
   "schemas/parser-rq-publication-policy.schema.json"
   "schemas/parser-rq-publication-work.schema.json"
   "schemas/parser-ir-publication-preservation.schema.json"
   "schemas/parser-ir.schema.json"
   "schemas/person-drift-event.schema.json"
   "schemas/person-drift-index.schema.json"
   "schemas/request-set.schema.json"
   "schemas/run-summary.schema.json"
   "schemas/snapshot-index.schema.json"
   "schemas/source-assertion.schema.json"
   "schemas/source-region-coverage.schema.json"
   "schemas/tei-eaj-comparison.schema.json"
   "schemas/tei-validation-result.schema.json"
   "schemas/token-output.schema.json"
   "schemas/workflow-run.schema.json"])

(def ^:private design-data-inputs
  ["docs/evidence/external/custom-parser-maintenance-as-of.edn"
   "data/aat-parser-ir-compatibility.edn"
   "data/analysis-recipes/literary-basic-ja-v1.json"
   "data/analysis-recipes/token-basic-ja-v1.json"
   "data/pack-policies/no-pack-v1.json"
   "data/pack-policies/parquet-basic-v1.json"
   "data/parser-evidence-citations.edn"
   "data/parser-rq-ab-aozora-classified-source-v1.json"
   "data/parser-rq-ab-aozora-diagnostic-gap-v1.json"
   "data/request-sets/demo-basic-ja.json"
   "data/request-sets/full-corpus-analysis-basic-ja.json"
   "data/request-sets/full-corpus-basic-ja.json"
   "data/request-sets/full-corpus-publication-basic-ja.json"
   "data/request-sets/smoke-basic-ja.json"])

(def ^:private design-fixture-inputs
  ["examples/ab-validator-output/comparison-report.json"
   "examples/ab-validator-output/divergence.json"
   "examples/ab-validator-output/manifest-inputs.json"
   "examples/ab-validator-output/parser-ir.json"
   "examples/ab-validator-output/run-summary.jsonl"
   "examples/ab-validator-output/source-region-coverage.json"
   "examples/ab-validator-output/warnings.jsonl"
   "examples/v0/example-work/analysis-result.json"
   "examples/v0/example-work/body-annotations.json"
   "examples/v0/example-work/failure-manifest.example.json"
   "examples/v0/example-work/manifest.json"
   "examples/v0/example-work/parser-ir.json"
   "examples/v0/example-work/source.manifest.json"
   "examples/v0/example-work/tei-validation-result.json"
   "examples/v0/example-work/token-stream.json"
   "examples/v0/example-work/warnings.jsonl"
   "examples/v0/snapshot/snapshot-index.json"
   "examples/workflow/passed.workflow-run.json"
   "docs/evidence/external/custom-parser-maintenance-2026-q3.json"
   "fixtures/tei-eaj-comparison/workset-export.json"
   "test/fixtures/parser-rq/classified-source/generation.json"
   "test/fixtures/parser-rq/classified-source/ledger.json"
   "test/fixtures/parser-rq/diagnostic-gap/raw-diagnostics-valid.json"
   "test/fixtures/parser-rq/diagnostic-gap/result-available.json"
   "test/fixtures/parser-rq/diagnostic-gap/result-unavailable.json"
   "test/fixtures/parser-rq/source-recognition/aggregate-ok.json"
   "test/fixtures/parser-rq/source-recognition/aggregate-unavailable.json"
   "test/fixtures/parser-rq/source-recognition/index-ok.json"
   "test/fixtures/parser-rq/source-recognition/work-ok.json"
   "test/fixtures/parser-rq/source-recognition/work-unavailable.json"])

(defn evidence-input-paths []
  (->> (concat design-schema-inputs design-data-inputs design-fixture-inputs)
       distinct
       sort
       vec))

(defn validate-json-schemas! [extra-manifest-paths]
  (let [manifest-schema (files/read-json "schemas/manifest.schema.json")
        adr-claim-migration-baseline-schema (files/read-json "schemas/adr-claim-migration-baseline.schema.json")
        adr-evidence-run-schema (files/read-json "schemas/adr-evidence-run.schema.json")
        adr-external-evidence-schema (files/read-json "schemas/adr-external-evidence.schema.json")
        parser-ir-schema (files/read-json "schemas/parser-ir.schema.json")
        diagnostic-schema (files/read-json "schemas/diagnostic.schema.json")
        run-summary-schema (files/read-json "schemas/run-summary.schema.json")
        workflow-run-schema (files/read-json "schemas/workflow-run.schema.json")
        manifest-inputs-schema (files/read-json "schemas/manifest-inputs.schema.json")
        comparison-report-schema (files/read-json "schemas/comparison-report.schema.json")
        custom-parser-maintenance-evidence-schema (files/read-json "schemas/custom-parser-maintenance-evidence.schema.json")
        aat-parser-ir-mapping-schema (files/read-json "schemas/aat-parser-ir-mapping.schema.json")
        aat-parser-ir-divergence-schema (files/read-json "schemas/aat-parser-ir-divergence.schema.json")
        aat-parser-ir-divergence-bundle-schema (files/read-json "schemas/aat-parser-ir-divergence-bundle.schema.json")
        parser-ir-publication-preservation-schema (files/read-json "schemas/parser-ir-publication-preservation.schema.json")
        analysis-recipe-schema (files/read-json "schemas/analysis-recipe.schema.json")
        analysis-result-schema (files/read-json "schemas/analysis-result.schema.json")
        request-set-schema (files/read-json "schemas/request-set.schema.json")
        snapshot-index-schema (files/read-json "schemas/snapshot-index.schema.json")
        pack-policy-schema (files/read-json "schemas/pack-policy.schema.json")
        parser-rq-capture-generation-schema (files/read-json "schemas/parser-rq-capture-generation.schema.json")
        parser-rq-raw-diagnostics-schema (files/read-json "schemas/parser-rq-ab-aozora-diagnostics-v3.schema.json")
        parser-rq-classified-source-ledger-schema (files/read-json "schemas/parser-rq-classified-source-ledger.schema.json")
        parser-rq-classified-source-policy-schema (files/read-json "schemas/parser-rq-classified-source-policy.schema.json")
        parser-rq-diagnostic-gap-policy-schema (files/read-json "schemas/parser-rq-diagnostic-gap-policy.schema.json")
        parser-rq-diagnostic-gap-aggregate-schema (files/read-json "schemas/parser-rq-diagnostic-gap-aggregate.schema.json")
        parser-rq-diagnostic-gap-result-schema (files/read-json "schemas/parser-rq-diagnostic-gap-result.schema.json")
        parser-rq-ignored-regions-schema (files/read-json "schemas/parser-rq-ignored-regions.schema.json")
        parser-rq-source-accountability-aggregate-schema (files/read-json "schemas/parser-rq-source-accountability-aggregate.schema.json")
        parser-rq-source-accountability-index-schema (files/read-json "schemas/parser-rq-source-accountability-index.schema.json")
        parser-rq-source-accountability-work-schema (files/read-json "schemas/parser-rq-source-accountability-work.schema.json")
        parser-rq-source-recognition-aggregate-schema (files/read-json "schemas/parser-rq-source-recognition-aggregate.schema.json")
        parser-rq-source-recognition-index-schema (files/read-json "schemas/parser-rq-source-recognition-index.schema.json")
        parser-rq-source-recognition-work-schema (files/read-json "schemas/parser-rq-source-recognition-work.schema.json")
        parser-rq-publication-aggregate-schema (files/read-json "schemas/parser-rq-publication-aggregate.schema.json")
        parser-rq-publication-index-schema (files/read-json "schemas/parser-rq-publication-index.schema.json")
        parser-rq-publication-policy-schema (files/read-json "schemas/parser-rq-publication-policy.schema.json")
        parser-rq-publication-work-schema (files/read-json "schemas/parser-rq-publication-work.schema.json")
        source-assertion-schema (files/read-json "schemas/source-assertion.schema.json")
        source-region-coverage-schema (files/read-json "schemas/source-region-coverage.schema.json")
        tei-eaj-comparison-schema (files/read-json "schemas/tei-eaj-comparison.schema.json")
        token-output-schema (files/read-json "schemas/token-output.schema.json")
        annotation-output-schema (files/read-json "schemas/annotation-output.schema.json")
        tei-validation-result-schema (files/read-json "schemas/tei-validation-result.schema.json")
        iiif-applicability-schema (files/read-json "schemas/iiif-applicability.schema.json")
        person-drift-event-schema (files/read-json "schemas/person-drift-event.schema.json")
        person-drift-index-schema (files/read-json "schemas/person-drift-index.schema.json")]
    (doseq [[path schema] [["schemas/manifest.schema.json" manifest-schema]
                           ["schemas/adr-claim-migration-baseline.schema.json" adr-claim-migration-baseline-schema]
                           ["schemas/adr-evidence-run.schema.json" adr-evidence-run-schema]
                           ["schemas/adr-external-evidence.schema.json" adr-external-evidence-schema]
                           ["schemas/parser-ir.schema.json" parser-ir-schema]
                           ["schemas/diagnostic.schema.json" diagnostic-schema]
                           ["schemas/run-summary.schema.json" run-summary-schema]
                           ["schemas/workflow-run.schema.json" workflow-run-schema]
                           ["schemas/manifest-inputs.schema.json" manifest-inputs-schema]
                           ["schemas/comparison-report.schema.json" comparison-report-schema]
                           ["schemas/custom-parser-maintenance-evidence.schema.json" custom-parser-maintenance-evidence-schema]
                           ["schemas/aat-parser-ir-mapping.schema.json" aat-parser-ir-mapping-schema]
                           ["schemas/aat-parser-ir-divergence.schema.json" aat-parser-ir-divergence-schema]
                           ["schemas/aat-parser-ir-divergence-bundle.schema.json" aat-parser-ir-divergence-bundle-schema]
                           ["schemas/parser-ir-publication-preservation.schema.json" parser-ir-publication-preservation-schema]
                           ["schemas/analysis-recipe.schema.json" analysis-recipe-schema]
                           ["schemas/analysis-result.schema.json" analysis-result-schema]
                           ["schemas/request-set.schema.json" request-set-schema]
                           ["schemas/snapshot-index.schema.json" snapshot-index-schema]
                           ["schemas/pack-policy.schema.json" pack-policy-schema]
                           ["schemas/parser-rq-capture-generation.schema.json" parser-rq-capture-generation-schema]
                           ["schemas/parser-rq-ab-aozora-diagnostics-v3.schema.json" parser-rq-raw-diagnostics-schema]
                           ["schemas/parser-rq-classified-source-ledger.schema.json" parser-rq-classified-source-ledger-schema]
                           ["schemas/parser-rq-classified-source-policy.schema.json" parser-rq-classified-source-policy-schema]
                           ["schemas/parser-rq-diagnostic-gap-policy.schema.json" parser-rq-diagnostic-gap-policy-schema]
                           ["schemas/parser-rq-diagnostic-gap-aggregate.schema.json" parser-rq-diagnostic-gap-aggregate-schema]
                           ["schemas/parser-rq-diagnostic-gap-result.schema.json" parser-rq-diagnostic-gap-result-schema]
                           ["schemas/parser-rq-ignored-regions.schema.json" parser-rq-ignored-regions-schema]
                           ["schemas/parser-rq-source-accountability-aggregate.schema.json" parser-rq-source-accountability-aggregate-schema]
                           ["schemas/parser-rq-source-accountability-index.schema.json" parser-rq-source-accountability-index-schema]
                           ["schemas/parser-rq-source-accountability-work.schema.json" parser-rq-source-accountability-work-schema]
                           ["schemas/parser-rq-source-recognition-aggregate.schema.json" parser-rq-source-recognition-aggregate-schema]
                           ["schemas/parser-rq-source-recognition-index.schema.json" parser-rq-source-recognition-index-schema]
                           ["schemas/parser-rq-source-recognition-work.schema.json" parser-rq-source-recognition-work-schema]
                           ["schemas/parser-rq-publication-aggregate.schema.json" parser-rq-publication-aggregate-schema]
                           ["schemas/parser-rq-publication-index.schema.json" parser-rq-publication-index-schema]
                           ["schemas/parser-rq-publication-policy.schema.json" parser-rq-publication-policy-schema]
                           ["schemas/parser-rq-publication-work.schema.json" parser-rq-publication-work-schema]
                           ["schemas/source-assertion.schema.json" source-assertion-schema]
                           ["schemas/source-region-coverage.schema.json" source-region-coverage-schema]
                           ["schemas/tei-eaj-comparison.schema.json" tei-eaj-comparison-schema]
                           ["schemas/token-output.schema.json" token-output-schema]
                           ["schemas/annotation-output.schema.json" annotation-output-schema]
                           ["schemas/tei-validation-result.schema.json" tei-validation-result-schema]
                           ["schemas/iiif-applicability.schema.json" iiif-applicability-schema]
                           ["schemas/person-drift-event.schema.json" person-drift-event-schema]
                           ["schemas/person-drift-index.schema.json" person-drift-index-schema]]]
      (schema-valid! schema path))
    (let [policy (files/read-json "data/parser-rq-ab-aozora-classified-source-v1.json")
          generation-path "test/fixtures/parser-rq/classified-source/generation.json"
          generation (files/read-json generation-path)]
      (validate-json! parser-rq-classified-source-policy-schema
                      "data/parser-rq-ab-aozora-classified-source-v1.json")
      (check-errors! (parser-rq-classified-source-policy-errors policy))
      (validate-json! parser-rq-classified-source-ledger-schema
                      "test/fixtures/parser-rq/classified-source/ledger.json")
      (validate-json! parser-rq-capture-generation-schema generation-path)
      (check-errors! (parser-rq-capture-generation-errors generation)))
    (let [root "test/fixtures/parser-rq/diagnostic-gap"
          policy-path "data/parser-rq-ab-aozora-diagnostic-gap-v1.json"
          policy (files/read-json policy-path)
          capture-path (str root "/raw-diagnostics-valid.json")
          capture (files/read-json capture-path)]
      (validate-json! parser-rq-diagnostic-gap-policy-schema policy-path)
      (validate-json! parser-rq-raw-diagnostics-schema capture-path)
      (doseq [path ["result-available.json" "result-unavailable.json"]]
        (validate-json! parser-rq-diagnostic-gap-result-schema
                        (str root "/" path)))
      (check-errors! (parser-rq-diagnostic-gap-policy-errors policy))
      (check-errors! (parser-rq-raw-diagnostics-errors
                      policy capture "本文\uE001終わり")))
    (let [root "test/fixtures/parser-rq/source-recognition"
          work (files/read-json (str root "/work-ok.json"))
          index (files/read-json (str root "/index-ok.json"))
          aggregate (files/read-json (str root "/aggregate-ok.json"))]
      (doseq [path ["work-ok.json" "work-unavailable.json"]]
        (validate-json! parser-rq-source-recognition-work-schema
                        (str root "/" path)))
      (validate-json! parser-rq-source-recognition-index-schema
                      (str root "/index-ok.json"))
      (doseq [path ["aggregate-ok.json" "aggregate-unavailable.json"]]
        (validate-json! parser-rq-source-recognition-aggregate-schema
                        (str root "/" path)))
      (check-errors! (parser-rq-source-recognition-work-errors work))
      (check-errors! (parser-rq-source-recognition-index-errors index))
      (check-errors! (parser-rq-source-recognition-aggregate-errors aggregate))
      (check-errors! (parser-rq-source-recognition-coherence-errors
                      index aggregate [work])))
    (let [maintenance-path
          "docs/evidence/external/custom-parser-maintenance-2026-q3.json"
          maintenance-record (files/read-json maintenance-path)
          governance-as-of
          (str (:as-of
                (files/read-edn
                 "docs/evidence/external/custom-parser-maintenance-as-of.edn")))]
      (validate-json! custom-parser-maintenance-evidence-schema maintenance-path)
      (validate-maintenance-evidence! "." maintenance-record governance-as-of))
    (doseq [path (concat ["examples/v0/example-work/source.manifest.json"
                          "examples/v0/example-work/manifest.json"
                          "examples/v0/example-work/failure-manifest.example.json"]
                         extra-manifest-paths)]
      (validate-json! manifest-schema path))
    (doseq [path ["examples/v0/example-work/parser-ir.json"
                  "examples/ab-validator-output/parser-ir.json"]]
      (validate-json! parser-ir-schema path))
    (doseq [path ["examples/v0/example-work/warnings.jsonl"
                  "examples/ab-validator-output/warnings.jsonl"]]
      (validate-json-lines! diagnostic-schema path {:require-nonempty true}))
    (validate-json-lines! run-summary-schema
                          "examples/ab-validator-output/run-summary.jsonl"
                          {:require-nonempty false})
    (validate-json! workflow-run-schema
                    "examples/workflow/passed.workflow-run.json")
    (validate-json! manifest-inputs-schema
                    "examples/ab-validator-output/manifest-inputs.json")
    (validate-json! comparison-report-schema
                    "examples/ab-validator-output/comparison-report.json")
    (validate-json! source-region-coverage-schema
                    "examples/ab-validator-output/source-region-coverage.json")
    (validate-json! tei-eaj-comparison-schema
                    "fixtures/tei-eaj-comparison/workset-export.json")
    (validate-json! aat-parser-ir-divergence-bundle-schema
                    "examples/ab-validator-output/divergence.json")
    (doseq [path ["data/analysis-recipes/literary-basic-ja-v1.json"
                  "data/analysis-recipes/token-basic-ja-v1.json"]]
      (validate-json! analysis-recipe-schema path))
    (doseq [path ["data/pack-policies/no-pack-v1.json"
                  "data/pack-policies/parquet-basic-v1.json"]]
      (validate-json! pack-policy-schema path))
    (validate-json! analysis-result-schema
                    "examples/v0/example-work/analysis-result.json")
    (validate-json! token-output-schema
                    "examples/v0/example-work/token-stream.json")
    (validate-json! annotation-output-schema
                    "examples/v0/example-work/body-annotations.json")
    (doseq [label ["smoke-basic-ja"
                   "demo-basic-ja"
                   "full-corpus-publication-basic-ja"
                   "full-corpus-analysis-basic-ja"
                   "full-corpus-basic-ja"]]
      (validate-json! request-set-schema
                      (str "data/request-sets/" label ".json")))
    (validate-json! snapshot-index-schema
                    "examples/v0/snapshot/snapshot-index.json")
    (snapshot-index/validate-snapshot-index!
     (files/read-json "examples/v0/snapshot/snapshot-index.json"))
    (doseq [record (get (files/read-json "examples/ab-validator-output/divergence.json") "records")]
      (check-errors! (validation-errors aat-parser-ir-divergence-schema record)))
    (validate-json! tei-validation-result-schema
                    "examples/v0/example-work/tei-validation-result.json")
    (let [manifest (files/read-json "examples/v0/example-work/manifest.json")
          validation-sidecars (filter #(= "validation-result" (get % "role"))
                                      (get manifest "sidecars"))]
      (when-not (some #(= "tei-validation-result.json" (get % "path_hint"))
                      validation-sidecars)
        (throw (ex-info "example TEI manifest must reference tei-validation-result.json"
                        {:manifest "examples/v0/example-work/manifest.json"}))))
    (compat/validate-registry! (compat/load-registry))
    (parser-evidence/validate-index! (parser-evidence/load-index))
    (when-not (validation-errors manifest-schema {})
      (throw (ex-info "manifest schema accepted an empty object"
                      {:schema "schemas/manifest.schema.json"})))))

(defn validate-ab-validator-output! []
  (let [manifest-inputs (files/read-json (files/path "examples" "ab-validator-output" "manifest-inputs.json"))
        parser-ir (files/read-json (files/path "examples" "ab-validator-output" "parser-ir.json"))
        source-region-coverage (files/read-json
                                (files/path "examples" "ab-validator-output"
                                            "source-region-coverage.json"))
        source-region-policy (files/read-json
                              (files/path "data"
                                          "source-region-publication-policy-v0.json"))
        divergence-file (files/path "examples" "ab-validator-output" "divergence.json")
        divergence-bundle (when (fs/exists? divergence-file)
                            (files/read-json divergence-file))]
    (am/explain-or-throw! ::am/manifest-inputs manifest-inputs
                          "ab-validator manifest inputs")
    (check-errors! (schema-hash-errors manifest-inputs))
    (check-errors! (parser-ir-schema-hash-errors parser-ir))
    (check-errors! (parser-ir-paragraph-coherence-errors parser-ir))
    (check-errors! (parser-ir-sentence-coherence-errors parser-ir))
    (check-errors! (source-region-coverage-errors source-region-coverage
                                                  source-region-policy))
    (check-errors! (compatibility-errors (compat/load-registry)
                                         parser-ir
                                         manifest-inputs
                                         divergence-bundle)))
  (am/explain-or-throw! ::am/run-summary-events
                        (files/read-json-lines (files/path "examples" "ab-validator-output" "run-summary.jsonl"))
                        "ab-validator run summary")
  (am/explain-or-throw! ::am/comparison-report
                        (files/read-json (files/path "examples" "ab-validator-output" "comparison-report.json"))
                        "ab-validator comparison report"))

(def ^:private canonicalization-defaults
  {:expected "667a3bfa5ab9a5e52a88e2e7de15506936a13c5d6c33825b8983861787bbcdea"
   :identity-json "fixtures/canonicalization/manifest-identity-object.canonical.json"
   :array-a "fixtures/canonicalization/array-ordering-negative-a.json"
   :array-b "fixtures/canonicalization/array-ordering-negative-b.json"})

(defn validate-canonicalization!
  ([] (validate-canonicalization! canonicalization-defaults))
  ([options]
   (let [{:keys [expected identity-json array-a array-b]} options
         actual (files/sha256-file identity-json)
         array-a (files/sha256-file array-a)
         array-b (files/sha256-file array-b)]
     (when-not (= expected actual)
       (throw (ex-info "canonical identity fixture hash mismatch"
                       {:expected expected
                        :actual actual})))
     (when (= array-a array-b)
       (throw (ex-info "array-ordering negative fixtures produced the same digest"
                       {:digest array-a}))))))

(def ^:private tei-ns "http://www.tei-c.org/ns/1.0")

(defn- tei-fixture
  [path schematron-kind & {:keys [expected-rules project-rng?]
                           :or {project-rng? true}}]
  (cond-> {:path path
           :schematron-kind schematron-kind
           :project-rng? project-rng?}
    expected-rules (assoc :expected-rules expected-rules)))

(def tei-fixture-catalog
  [(tei-fixture "examples/v0/example-work/tei.xml" :valid)
   (tei-fixture "fixtures/tei/valid/rashomon-minimal.xml" :valid)
   (tei-fixture "fixtures/tei/valid/source-span-local-ref.xml" :valid)
   (tei-fixture "fixtures/tei/valid/transcription-enrichment-declared.xml" :valid)
   (tei-fixture "fixtures/tei/warnings/figure-missing-desc.xml" :warning
                :expected-rules #{"abc-figure-accessibility"})
   (tei-fixture "fixtures/tei/warnings/transcription-enrichment-undeclared.xml" :warning
                :expected-rules #{"abc-transcription-vs-annotation"})
   (tei-fixture "fixtures/tei/invalid/missing-title.xml" :invalid
                :expected-rules #{"abc-tei-header-title"}
                :project-rng? false)
   (tei-fixture "fixtures/tei/invalid/abc-bad-layout-params.xml" :invalid
                :expected-rules #{"abc-layout-params-shape"})
   (tei-fixture "fixtures/tei/invalid/abc-bad-preservation-record.xml" :invalid
                :expected-rules #{"abc-preservation-record-shape"}
                :project-rng? false)
   (tei-fixture "fixtures/tei/invalid/abc-missing-vocab-version.xml" :invalid
                :expected-rules #{"abc-vocab-version-declared"})
   (tei-fixture "fixtures/tei/invalid/char-empty-decl.xml" :invalid
                :expected-rules #{"abc-char-resolution-form"})
   (tei-fixture "fixtures/tei/invalid/gaiji-dangling-ref.xml" :invalid
                :expected-rules #{"abc-gaiji-chardecl-resolution"})
   (tei-fixture "fixtures/tei/invalid/gaiji-missing-ref.xml" :invalid
                :expected-rules #{"abc-gaiji-reference"})
   (tei-fixture "fixtures/tei/invalid/header-no-language.xml" :invalid
                :expected-rules #{"abc-header-language-declared"})
   (tei-fixture "fixtures/tei/invalid/missing-source-work-id.xml" :invalid
                :expected-rules #{"abc-tei-header-source-work-id"})
   (tei-fixture "fixtures/tei/invalid/ruby-empty-base.xml" :invalid
                :expected-rules #{"abc-ruby-base-non-empty"})
   (tei-fixture "fixtures/tei/invalid/ruby-empty-reading.xml" :invalid
                :expected-rules #{"abc-ruby-reading-non-empty"})
   (tei-fixture "fixtures/tei/invalid/ruby-missing-reading.xml" :invalid
                :expected-rules #{"abc-ruby-complete"}
                :project-rng? false)
   (tei-fixture "fixtures/tei/invalid/source-span-dangling-ref.xml" :invalid
                :expected-rules #{"abc-source-span-target-exists"})
   (tei-fixture "fixtures/tei/invalid/source-span-external-ref.xml" :invalid
                :expected-rules #{"abc-source-span-reference"
                                  "abc-source-span-target-exists"})])

(defn tei-fixture-paths []
  (mapv :path tei-fixture-catalog))

(defn tei-xml-paths []
  (into ["schemas/tei-profile.odd"
         "schemas/tei-profile.sch"
         "schemas/tei-profile.rng"]
        (tei-fixture-paths)))

(defn tei-project-rng-paths []
  (->> tei-fixture-catalog
       (filter :project-rng?)
       (mapv :path)))

(defn validate-xml! []
  (apply run-command! "xmllint" "--noout" (tei-xml-paths)))

(defn- render-tei-violation
  "Render a violation map to a single line. Label is supplied by the
  harness pass per-file rather than copied into every violation map."
  [label {:keys [severity line column message]}]
  (let [sev (cond
              (keyword? severity) (string/upper-case (name severity))
              (string? severity) (string/upper-case severity)
              :else "VIOLATION")]
    (str sev ": " label " " (or line "?") ":" (or column "?")
         " — " (or message "(no message)"))))

(defn validate-tei!
  "Validate every TEI document in `xml-paths` against the schema at
  `schema-path`. Warnings are logged via Telemere but do not fail the
  step; errors and fatals are aggregated and thrown at the end."
  [^String schema-path xml-paths]
  (when (or (nil? schema-path) (= "" schema-path))
    (throw (ex-info "TEI RelaxNG schema path must be set."
                    {:error :missing-schema-path})))
  (let [per-file-results
        (mapv (fn [path]
                (let [{:keys [violations]}
                      (tei/validate! {:schema-path schema-path
                                      :xml-path (str path)
                                      :label (str path)})]
                  {:label (str path) :violations violations}))
              xml-paths)
        all-warnings (mapcat (fn [{:keys [label violations]}]
                               (->> violations
                                    (filter #(= :warning (:severity %)))
                                    (map #(vector label %))))
                             per-file-results)
        all-failures (mapcat (fn [{:keys [label violations]}]
                               (->> violations
                                    (filter #(#{:error :fatal} (:severity %)))
                                    (map #(vector label %))))
                             per-file-results)]
    (doseq [[label v] all-warnings]
      (tel/log! :warn (render-tei-violation label v)))
    (when (seq all-failures)
      (throw (ex-info "TEI RelaxNG validation failed"
                      {:errors (mapv (fn [[label v]] (render-tei-violation label v))
                                     all-failures)})))))

(defn- schematron-error? [finding]
  (= :error (:severity finding)))

(defn- schematron-warning? [finding]
  (= :warning (:severity finding)))

(defn- rule-ids [findings]
  (set (map :rule-id findings)))

(defn- render-schematron-finding [{:keys [label rule-id severity message]}]
  (str (string/upper-case (name severity)) ": "
       label " " rule-id " — " message))

(defn- validate-schematron-valid-fixture! [schema-path path]
  (let [{:keys [findings]} (schematron/validate! {:schema-path schema-path
                                                  :xml-path path
                                                  :label path})
        errors (filter schematron-error? findings)]
    (when (seq errors)
      (throw (ex-info "unexpected Schematron error in valid TEI fixture"
                      {:fixture path
                       :errors (mapv render-schematron-finding errors)})))))

(defn- validate-schematron-warning-fixture! [schema-path path expected-rules]
  (let [{:keys [findings]} (schematron/validate! {:schema-path schema-path
                                                  :xml-path path
                                                  :label path})
        actual-warnings (rule-ids (filter schematron-warning? findings))
        missing (set/difference expected-rules actual-warnings)
        errors (filter schematron-error? findings)]
    (when (seq errors)
      (throw (ex-info "unexpected Schematron error in warning TEI fixture"
                      {:fixture path
                       :errors (mapv render-schematron-finding errors)})))
    (when (seq missing)
      (throw (ex-info "missing expected Schematron warning rule"
                      {:fixture path
                       :missing (sort missing)
                       :actual (sort actual-warnings)})))))

(defn- validate-schematron-invalid-fixture! [schema-path path expected-rules]
  (let [{:keys [findings]} (schematron/validate! {:schema-path schema-path
                                                  :xml-path path
                                                  :label path})
        actual-errors (rule-ids (filter schematron-error? findings))
        expected-set (set expected-rules)
        actual-set (set actual-errors)
        missing (set/difference expected-set actual-set)
        ;; Exact-equality coverage (review §3 root-cause fix): a fixture
        ;; must fire EXACTLY its declared rule set, not a superset. The
        ;; prior asymmetry (missing-only) is how the
        ;; source-span-external-ref.xml drift went undetected: it fired 2
        ;; rules while declaring 1, and the missing-only check passed.
        unexpected (set/difference actual-set expected-set)]
    (when (seq missing)
      (throw (ex-info "missing expected Schematron rule"
                      {:fixture path
                       :missing (sort missing)
                       :actual (sort actual-errors)})))
    (when (seq unexpected)
      (throw (ex-info "Schematron fixture fired rules not declared in its expected set (exact-equality violation)"
                      {:fixture path
                       :unexpected (sort unexpected)
                       :expected (sort (seq expected-set))
                       :actual (sort actual-errors)})))))

(defn- namespace-aware-document [path]
  (let [factory (doto (javax.xml.parsers.DocumentBuilderFactory/newInstance)
                  (.setNamespaceAware true))]
    (.. factory newDocumentBuilder (parse (io/file path)))))

(defn rule-universe
  "Return the canonical set of ABC Schematron rule ids declared in
  schemas/tei-profile.odd. The set is derived from the ODD at runtime so
  it cannot drift from the authored constraintSpec identifiers."
  ([] (rule-universe "schemas/tei-profile.odd"))
  ([odd-path]
   (let [nodes (.getElementsByTagNameNS (namespace-aware-document odd-path)
                                        tei-ns
                                        "constraintSpec")]
     (into #{}
           (keep (fn [i]
                   (let [node (.item nodes i)
                         scheme (.getAttribute node "scheme")
                         ident (.getAttribute node "ident")]
                     (when (and (= "schematron" scheme)
                                (re-matches #"abc-[a-z0-9-]+" ident))
                       ident))))
           (range (.getLength nodes))))))

(defn- tei-schematron-fixtures-for [kind]
  (->> tei-fixture-catalog
       (filter #(= kind (:schematron-kind %)))
       (map (fn [{:keys [path expected-rules]}]
              (if (= :valid kind)
                path
                [path expected-rules])))))

(def tei-schematron-fixtures
  "TEI Schematron fixture partition derived from tei-fixture-catalog. Expected
  rule sets are cross-checked against the ODD-derived rule universe by
  validate-tei-schematron!."
  {:schema-path "schemas/tei-profile.sch"
   :valid-fixtures (vec (tei-schematron-fixtures-for :valid))
   :warning-fixtures (into {} (tei-schematron-fixtures-for :warning))
   :invalid-fixtures (into {} (tei-schematron-fixtures-for :invalid))})

(defn validate-tei-schematron!
  [{:keys [schema-path valid-fixtures warning-fixtures invalid-fixtures]}]
  (doseq [path valid-fixtures]
    (validate-schematron-valid-fixture! schema-path path))
  (doseq [[path expected-rules] warning-fixtures]
    (validate-schematron-warning-fixture! schema-path path expected-rules))
  (doseq [[path expected-rules] invalid-fixtures]
    (validate-schematron-invalid-fixture! schema-path path expected-rules))
  (let [fixture-paths (concat (keys warning-fixtures) (keys invalid-fixtures))
        findings (map (fn [path]
                        (let [{:keys [findings]} (schematron/validate!
                                                  {:schema-path schema-path
                                                   :xml-path path
                                                   :label path})]
                          {:errors (rule-ids (filter schematron-error? findings))
                           :warnings (rule-ids (filter schematron-warning? findings))}))
                      fixture-paths)
        actual-errors (set (mapcat :errors findings))
        actual-warnings (set (mapcat :warnings findings))
        all-actual (set/union actual-errors actual-warnings)
        all-expected (set/union (apply set/union (vals warning-fixtures))
                                (apply set/union (vals invalid-fixtures)))
        universe (rule-universe)
        uncovered (set/difference universe all-actual)
        unknown (set/difference all-expected universe)]
    (when (seq uncovered)
      (throw (ex-info "Schematron rules declared in the ODD are not covered by any negative or warning fixture"
                      {:rule-universe (sort universe)
                       :uncovered (sort uncovered)})))
    (when (seq unknown)
      (throw (ex-info "Schematron fixture references rule-ids not declared in the ODD"
                      {:rule-universe (sort universe)
                       :unknown (sort unknown)})))))

(defn- load-turtle-graph [path]
  (aa/read (aa/graph :simple) (io/file (evidence-io/record-read! path))))

(defn- validate-drift-ttl-fixture-result [{:keys [event graph]}]
  (let [event-value (files/read-json event)
        data-graph (load-turtle-graph graph)
        failures (person-drift/validate-drift-graph-failures event-value
                                                             data-graph
                                                             graph)]
    (if (seq failures)
      {:status :error :failures failures}
      {:status :ok})))

(defn- validate-drift-fixture-result [fixture]
  (if (map? fixture)
    (case (:type fixture)
      :ttl (validate-drift-ttl-fixture-result fixture)
      (throw (ex-info "unknown drift fixture type" {:fixture fixture})))
    (person-drift/validate-drift-events! {:persons-dir fixture})))

(defn- validate-drift-invalid-fixture! [fixture expected-codes]
  (let [result (validate-drift-fixture-result fixture)
        failures (:failures result)
        actual (set (map :code failures))
        unknown (set/difference actual person-drift/failure-codes)]
    (when-not (= :error (:status result))
      (throw (ex-info "expected invalid drift fixture to fail"
                      {:fixture fixture
                       :result result})))
    (when (seq unknown)
      (throw (ex-info "unknown drift validation failure codes"
                      {:fixture fixture
                       :known (sort person-drift/failure-codes)
                       :unknown (sort unknown)
                       :failures failures})))
    (when-not (= expected-codes actual)
      (throw (ex-info "unexpected drift validation failure set"
                      {:fixture fixture
                       :expected (sort expected-codes)
                       :actual (sort actual)
                       :missing (sort (set/difference expected-codes actual))
                       :unexpected (sort (set/difference actual expected-codes))})))))

(defn validate-drift-fixtures! [invalid-fixtures]
  (doseq [[path expected-codes] invalid-fixtures]
    (validate-drift-invalid-fixture! path expected-codes)))

(defn- file-bytes [path]
  (with-open [in (io/input-stream (io/file path))]
    (.readAllBytes in)))

(defn validate-publication-view!
  "Regenerate the Linked Art candidate, expanded, and validation-result
  fixtures under a temp dir and byte-compare them against the committed
  LOD fixtures. The harness internally enforces the artifact-id
  identity invariant; here we additionally enforce that the on-disk
  bytes are exactly what the harness emits, so any drift surfaces as a
  bundle failure rather than an ADR 0013 invariant breach at publish
  time."
  [{:keys [manifest-path metadata-record-path context-path
           candidate-path expanded-path result-path]}]
  (fs/with-temp-dir [temp {:prefix "abc-linked-art-bundle"}]
    (let [tmp-candidate (fs/file temp "linked-art-candidate.jsonld")
          tmp-expanded (fs/file temp "linked-art-expanded.normalized.json")
          tmp-result (fs/file temp "jsonld-context-validation-result.json")]
      (linked-art/write-publication-view!
       {:manifest-path manifest-path
        :metadata-record-path metadata-record-path
        :context-path context-path
        :candidate-path (str tmp-candidate)
        :expanded-path (str tmp-expanded)
        :result-path (str tmp-result)})
      (doseq [[label committed regen]
              [["linked-art-candidate.jsonld" candidate-path tmp-candidate]
               ["linked-art-expanded.normalized.json" expanded-path tmp-expanded]
               ["jsonld-context-validation-result.json" result-path tmp-result]]]
        (let [a (file-bytes committed)
              b (file-bytes regen)]
          (when-not (= (seq a) (seq b))
            (throw (ex-info (str label " drifted from harness output")
                            {:committed (str committed)
                             :regenerated (str regen)}))))))))

(defn validate-publication-output! [publication-output]
  (let [manifest-schema (files/read-json "schemas/manifest.schema.json")
        preservation-schema (files/read-json "schemas/parser-ir-publication-preservation.schema.json")
        validation-result-schema (files/read-json "schemas/tei-validation-result.schema.json")
        plain-file (:plaintext publication-output)
        tei-file (:tei publication-output)
        preservation-file (:preservation publication-output)
        validation-result-file (:tei-validation-result publication-output)]
    (when-not (and (fs/exists? plain-file) (pos? (fs/size plain-file)))
      (throw (ex-info "parser-IR publication plain.txt must exist and be non-empty"
                      {:path (str plain-file)})))
    (run-command! "xmllint" "--noout" (str tei-file))
    ;; Parser-IR publication TEI uses the ABC namespace extension. Strict
    ;; upstream tei_all.rng does not admit project-specific foreign attributes,
    ;; so generated publication artifacts are validated against the customized
    ;; ABC profile below.
    (validate-tei! "schemas/tei-profile.rng" [tei-file])
    (let [{:keys [findings]} (schematron/validate! {:schema-path "schemas/tei-profile.sch"
                                                    :xml-path (str tei-file)
                                                    :label (str tei-file)})
          errors (filter schematron-error? findings)]
      (when (seq errors)
        (throw (ex-info "generated parser-IR publication TEI has Schematron errors"
                        {:errors (mapv render-schematron-finding errors)}))))
    (doseq [manifest-file [(:plaintext-manifest publication-output)
                           (:tei-manifest publication-output)]]
      (validate-json! manifest-schema manifest-file))
    (validate-json! preservation-schema preservation-file)
    (validate-json! validation-result-schema validation-result-file)))

;; Spec format: "<severity>: <focus> <path> — <message> (<label>)".
;; We append "[<source>]" because the source shape IRI is high-signal
;; for debugging and the spec did not pin punctuation, only fields.
(defn- render-violation [{:keys [severity focus-node path message label source]}]
  (str (or severity "Violation") ": "
       (or focus-node "?") " "
       (or path "")
       (when message (str " — " message))
       (when label (str " (" label ")"))
       (when source (str " [" source "]"))))

(defn validate-shacl!
  "Validate every manifest in `manifest-paths` against the shapes graph.
  Aggregates all violations and throws once at the end if any are found."
  [shapes-graph manifest-paths]
  (let [violations
        (reduce
         (fn [acc path]
           (try
             (let [m (files/read-json path)
                   data (manifest-to-rdf/manifest->graph m)]
               (shacl/validate! {:shapes-graph shapes-graph
                                 :data-graph data
                                 :label (str path)})
               acc)
             (catch clojure.lang.ExceptionInfo e
               (into acc (:errors (ex-data e))))))
         []
         manifest-paths)]
    (when (seq violations)
      (throw (ex-info "SHACL validation failed"
                      {:errors (mapv render-violation violations)})))))

(defn- validate-persons-directory!
  "For every JSON file under `persons-dir`: load, schema-validate,
  and verify the embedded person_record_schema_hash matches the live
  schema's JCS hash. Returns a map person_id → person-record map."
  [persons-dir person-schema-path]
  (let [live-schema-hash (manifest/schema-hash person-schema-path)
        files (->> (files/list-files persons-dir)
                   (filter #(string/ends-with? (str (fs/file-name %)) ".json"))
                   (sort-by str))]
    (into {}
          (for [^java.io.File f files]
            (let [record (files/read-json (str f))]
              (person-record/validate! record)
              (let [embedded (get record "person_record_schema_hash")]
                (when-not (= embedded live-schema-hash)
                  (throw (ex-info
                          (str "person_record_schema_hash mismatch in " f
                               ": record has " embedded
                               ", live schema hash is " live-schema-hash)
                          {:path (str f)
                           :embedded embedded
                           :live live-schema-hash}))))
              [(get record "person_id") record])))))

(defn validate-metadata-bundle!
  "Validate the example-work metadata-record bundle:
  1. Every person file in `persons-dir` validates against
     person-record.schema.json + schema-hash precondition.
  2. The work's metadata-record.json validates against
     metadata-record.schema.json + schema-hash precondition.
  3. Recompute metadata_record_hash; compare against the
     `manifest_identity_object.metadata_record_hash` in manifest.json.
  4. For every contributors[i]: recompute the referenced person's
     person_record_hash from the on-disk file and fail if it does
     not match contributors[i].person_record_hash.
  5. Compose the work + persons graph; SHACL validate against shapes.
  6. Compose work + persons → ttl; byte-equal to ttl-path."
  [{:keys [record-path manifest-path persons-dir
           record-schema-path person-schema-path
           ttl-path shapes-graph]}]
  (let [persons-by-id (validate-persons-directory! persons-dir person-schema-path)
        record (files/read-json record-path)]
    (metadata-record/validate! record)
    (let [computed-schema-hash (manifest/schema-hash record-schema-path)
          expected-schema-hash (get record "metadata_record_schema_hash")]
      (when-not (= computed-schema-hash expected-schema-hash)
        (throw (ex-info (str "metadata_record_schema_hash mismatch: "
                             "record has " expected-schema-hash
                             ", live schema hash is " computed-schema-hash)
                        {:record-path record-path
                         :computed computed-schema-hash
                         :expected expected-schema-hash}))))
    (let [computed (metadata-record/record-hash record)
          expected (get-in (files/read-json manifest-path)
                           ["manifest_identity_object" "metadata_record_hash"])]
      (when-not (= computed expected)
        (throw (ex-info (str "metadata_record_hash mismatch: manifest has "
                             expected ", record-hash computed " computed)
                        {:record-path record-path
                         :manifest-path manifest-path
                         :computed computed
                         :expected expected}))))
    (doseq [contributor (get record "contributors")]
      (let [pid (get contributor "person_id")
            referenced (get contributor "person_record_hash")
            body (get persons-by-id pid)]
        (when-not body
          (throw (ex-info (str "contributor " pid " has no matching file in " persons-dir)
                          {:person-id pid
                           :persons-dir persons-dir})))
        (let [recomputed (person-record/record-hash body)]
          (when-not (= referenced recomputed)
            (throw (ex-info
                    (str "contributor reference for person_id " pid
                         " is stale: metadata-record references " referenced
                         ", recomputed from " persons-dir "/" pid ".json is "
                         recomputed)
                    {:person-id pid
                     :referenced referenced
                     :recomputed recomputed
                     :persons-dir persons-dir}))))))
    (shacl/validate! {:shapes-graph shapes-graph
                      :data-graph (metadata-record/record+persons->graph
                                   record persons-by-id)
                      :label record-path})
    (let [generated (metadata-record/record+persons->ttl record persons-by-id)
          expected (files/read-text ttl-path)]
      (when-not (= expected generated)
        (throw (ex-info (str "metadata-record.ttl parity mismatch with " ttl-path)
                        {:record-path record-path
                         :ttl-path ttl-path}))))))

(defn- with-design-temp-dir [f]
  (fs/with-temp-dir [temp-dir {:prefix "abc-design-bundle"}]
    (f temp-dir)))

(defn validate-design-bundle! []
  (with-design-temp-dir
    (fn [temp-dir]
      (let [materialized-dir (fs/file temp-dir "materialized-import")
            publication-dir (fs/file temp-dir "publication")
            tokenized-dir (fs/file temp-dir "tokenized")
            token-analysis-dir (fs/file temp-dir "token-analysis")
            annotation-dir (fs/file temp-dir "annotation")]
        (tel/log! :info "==> Materializing imported ab-validator output")
        (let [materialized (materialize/materialize-import!
                            {:input-dir (files/path "examples" "ab-validator-output")
                             :output-dir materialized-dir
                             :generated-at materialize/default-generated-at})
              publication-output (publication/materialize-publication!
                                  {:parser-ir-path "examples/v0/example-work/parser-ir.json"
                                   :source-manifest-path "examples/v0/example-work/source.manifest.json"
                                   :metadata-record-path "examples/v0/example-work/metadata-record.json"
                                   :persons-dir "examples/v0/example-persons"
                                   :output-dir publication-dir
                                   :generated-at publication/default-generated-at})
              tokenized-output (tokenized/materialize-tokenized!
                                {:producer-manifest (files/read-json
                                                     (:parser-ir materialized))
                                 :tokenizer-profile (files/read-json
                                                     "data/tokenizer-profiles/fixture-tokenizer-ja-v1.json")
                                 :input-plaintext-policy-hash (files/example-hash "13")
                                 :tokens fixture-tokenized-tokens
                                 :output-dir tokenized-dir
                                 :generated-at materialize/default-generated-at})
              token-analysis-output (analysis/materialize-token-backed-analysis!
                                     {:producer-manifest (files/read-json
                                                          (:manifest
                                                           tokenized-output))
                                      :recipe (files/read-json
                                               "data/analysis-recipes/token-basic-ja-v1.json")
                                      :subject fixture-analysis-subject
                                      :metrics fixture-token-analysis-metrics
                                      :output-dir token-analysis-dir
                                      :generated-at materialize/default-generated-at})
              annotation-output (annotations/materialize-annotations!
                                 {:producer-manifest (files/read-json
                                                      (:parser-ir materialized))
                                  :parser-ir (files/read-json
                                              "examples/ab-validator-output/parser-ir.json")
                                  :annotation-policy (files/read-json
                                                      "data/annotation-policies/ruby-gaiji-v1.json")
                                  :input-plaintext-policy-hash (files/example-hash "13")
                                  :output-dir annotation-dir
                                  :generated-at materialize/default-generated-at})
              manifest-paths (concat (vals materialized)
                                     [(:plaintext-manifest publication-output)
                                      (:tei-manifest publication-output)
                                      (:manifest tokenized-output)
                                      (:manifest token-analysis-output)
                                      (:manifest annotation-output)])]
          (tel/log! :info "materialized import ok")
          (tel/log! :info "parser-IR publication materialization ok")
          (tel/log! :info "tokenized fixture materialization ok")
          (tel/log! :info "token-backed analysis fixture materialization ok")
          (tel/log! :info "annotation fixture materialization ok")
          (tel/log! :info "==> Validating JSON schemas and examples")
          (validate-json-schemas! manifest-paths)
          (validate-json! (files/read-json "schemas/token-output.schema.json")
                          (:token-stream tokenized-output))
          (validate-json! (files/read-json "schemas/analysis-result.schema.json")
                          (:analysis-result token-analysis-output))
          (validate-json! (files/read-json "schemas/annotation-output.schema.json")
                          (:annotations annotation-output))
          (tel/log! :info "json schema validation ok")
          (tel/log! :info "==> Checking parser-IR publication output")
          (validate-publication-output! publication-output)
          (tel/log! :info "parser-IR publication output ok")
          (tel/log! :info "==> Checking materialized manifest index")
          (let [entries (manifest-index/index-manifest-files manifest-paths)
                tokenizer-profiles (tokenizer-profiles-by-hash)]
            (manifest-index/validate-no-reproducibility-conflicts! entries)
            (manifest-index/validate-tokenized-release-guardrail! entries)
            (manifest-index/validate-tokenized-copied-fields! entries
                                                              tokenizer-profiles)
            (manifest-index/validate-analysis-copied-fields! entries)
            (manifest-index/validate-annotation-release-guardrail! entries)
            (manifest-index/validate-annotation-copied-fields! entries))
          (tel/log! :info "materialized manifest index ok")
          (tel/log! :info "==> Checking materialized RDF views")
          (doseq [manifest-path manifest-paths]
            (manifest-to-rdf/manifest->ttl (files/read-json manifest-path)))
          (tel/log! :info "materialized RDF views ok")
          (tel/log! :info "==> Validating SHACL shapes")
          (let [shapes (shacl/load-shapes-graph)
                targets (concat manifest-paths
                                ["examples/v0/example-work/manifest.json"
                                 "examples/v0/example-work/failure-manifest.example.json"])]
            (validate-shacl! shapes targets))
          (tel/log! :info "shacl shapes ok")
          (tel/log! :info "==> Validating metadata record + persons bundle")
          (let [shapes (shacl/load-shapes-graph)]
            (validate-metadata-bundle!
             {:record-path "examples/v0/example-work/metadata-record.json"
              :manifest-path "examples/v0/example-work/manifest.json"
              :persons-dir "examples/v0/example-persons"
              :record-schema-path "schemas/metadata-record.schema.json"
              :person-schema-path "schemas/person-record.schema.json"
              :ttl-path "examples/v0/example-work/metadata-record.ttl"
              :shapes-graph shapes}))
          (tel/log! :info "metadata bundle ok")
          (tel/log! :info "==> Validating person drift events")
          (let [result (person-drift/validate-drift-events!
                        {:persons-dir "examples/v0/example-persons"})]
            (when (= :error (:status result))
              (throw (ex-info "person drift validation failed"
                              {:errors (:failures result)}))))
          (tel/log! :info "person drift events ok"))
        (tel/log! :info "==> Checking imported ab-validator output")
        (validate-ab-validator-output!)
        (tel/log! :info "ab-validator output ok")
        (tel/log! :info "==> Checking canonicalization fixtures")
        (validate-canonicalization!)
        (tel/log! :info "canonicalization fixtures ok")
        (tel/log! :info "==> Checking XML fixtures")
        (validate-xml!)
        (tel/log! :info "xml fixtures ok")
        (tel/log! :info "==> Validating TEI against P5 RelaxNG")
        (validate-tei! (System/getenv "TEI_SCHEMA_PATH")
                       ["examples/v0/example-work/tei.xml"])
        (tel/log! :info "tei rng validation ok")
        (tel/log! :info "==> Validating TEI against project RelaxNG")
      ;; Some negative Schematron fixtures are intentionally structurally
      ;; invalid too. tei-fixture-catalog marks which fixtures should pass the
      ;; ODD-derived Relax NG layer so this list does not drift independently.
        (validate-tei! "schemas/tei-profile.rng" (tei-project-rng-paths))
        (tel/log! :info "tei project rng validation ok")
        (tel/log! :info "==> Validating TEI against project Schematron")
        (validate-tei-schematron! tei-schematron-fixtures)
        (tel/log! :info "tei schematron validation ok")
        (tel/log! :info "==> Validating person drift negative fixtures")
        (validate-drift-fixtures!
         {"fixtures/v0/invalid/drift/broken-index-target" #{:index-target-missing}
          "fixtures/v0/invalid/drift/asymmetric-index" #{:event-missing-from-participant-index}
          "fixtures/v0/invalid/drift/orphan-event-file"
          #{:orphan-event-file :event-missing-from-participant-index}
          "fixtures/v0/invalid/drift/unsorted-participants"
          #{:participants-not-sorted :index-target-missing
            :event-missing-from-participant-index :orphan-event-file}
          "fixtures/v0/invalid/drift/dangling-snapshot-ref"
          #{:unknown-snapshot-reference :participant-not-covered
            :index-target-missing :event-missing-from-participant-index
            :orphan-event-file}
          "fixtures/v0/invalid/drift/invalid-role"
          #{:invalid-had-role :index-target-missing
            :event-missing-from-participant-index :orphan-event-file}
          "fixtures/v0/invalid/drift/invalid-agent"
          #{:invalid-agent-iri :index-target-missing
            :event-missing-from-participant-index :orphan-event-file}
          {:type :ttl
           :event "examples/v0/example-persons/_events/sha256:550c55dbfed12ce9b6de833a75c8b03e8a01bf3047c17ced494e87db0f4ee747.json"
           :graph "fixtures/v0/invalid/drift/shacl-missing-date/graph.ttl"}
          #{:shacl-violation}
          {:type :ttl
           :event "examples/v0/example-persons/_events/sha256:550c55dbfed12ce9b6de833a75c8b03e8a01bf3047c17ced494e87db0f4ee747.json"
           :graph "fixtures/v0/invalid/drift/split-cardinality-one-successor/graph.ttl"}
          #{:shacl-violation :rdf-participant-prov-mismatch}
          {:type :ttl
           :event "fixtures/v0/invalid/drift/merge-cardinality-one-predecessor/event.json"
           :graph "fixtures/v0/invalid/drift/merge-cardinality-one-predecessor/graph.ttl"}
          #{:shacl-violation}
          {:type :ttl
           :event "examples/v0/example-persons/_events/sha256:550c55dbfed12ce9b6de833a75c8b03e8a01bf3047c17ced494e87db0f4ee747.json"
           :graph "fixtures/v0/invalid/drift/typing-missing-subclass/graph.ttl"}
          #{:missing-rdf-type :rdf-participant-prov-mismatch}
          {:type :ttl
           :event "examples/v0/example-persons/_events/sha256:550c55dbfed12ce9b6de833a75c8b03e8a01bf3047c17ced494e87db0f4ee747.json"
           :graph "fixtures/v0/invalid/drift/typing-missing-activity/graph.ttl"}
          #{:missing-rdf-type :shacl-violation :rdf-participant-prov-mismatch}
          {:type :ttl
           :event "examples/v0/example-persons/_events/sha256:550c55dbfed12ce9b6de833a75c8b03e8a01bf3047c17ced494e87db0f4ee747.json"
           :graph "fixtures/v0/invalid/drift/rdf-participant-prov-mismatch/graph.ttl"}
          #{:rdf-participant-prov-mismatch}})
        (tel/log! :info "person drift negative fixtures ok")
        (tel/log! :info "==> Validating Linked Art publication view (ADR 0013)")
        (validate-publication-view!
         {:manifest-path "examples/v0/example-work/manifest.json"
          :metadata-record-path "examples/v0/example-work/metadata-record.json"
          :context-path "contexts/abc-v0.jsonld"
          :candidate-path "examples/v0/example-work/lod/linked-art-candidate.jsonld"
          :expanded-path "examples/v0/example-work/lod/linked-art-expanded.normalized.json"
          :result-path "examples/v0/example-work/lod/jsonld-context-validation-result.json"})
        (tel/log! :info "linked art publication view ok")
        (tel/log! :info "==> Checking IIIF applicability record (ADR 0014)")
        (iiif/validate-applicability! "examples/v0/example-work/iiif/applicability.json")
        (tel/log! :info "iiif applicability record ok")
        (tel/log! :info "design bundle validation ok")))))

(defn -main [& _args]
  (logging/install-cli-handler!)
  (am/install!)
  (try
    (validate-design-bundle!)
    (catch Throwable t
      (tel/log! {:level :error :error t} "design bundle validation failed")
      (when-let [errors (:errors (ex-data t))]
        (doseq [error errors]
          (tel/log! :error (str "- " error))))
      (System/exit 1))))
