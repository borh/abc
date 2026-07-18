(ns abc.tools.parser-rq-campaign-test
  (:require [abc.tools.hash :as hash]
            [abc.tools.jcs :as jcs]
            [abc.tools.parser-release-qualification :as qualification]
            [abc.tools.parser-rq-campaign :as campaign]
            [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]))

(def sha (hash/format-sha256 (apply str (repeat 64 "a"))))
(def sha-b (hash/format-sha256 (apply str (repeat 64 "b"))))

(def qualification-identity
  {:aat_version 2
   :aat_adapter "ab-aozora"
   :aat_adapter_version "candidate"
   :mapping_id "mapping"
   :mapping_version "1"
   :mapping_hash sha
   :mapping_schema_hash sha
   :parser_ir_schema_id "parser-ir"
   :parser_ir_schema_hash sha
   :parser_git_rev (apply str (repeat 40 "a"))
   :corpus_snapshot_hash sha
   :corpus_list_hash sha
   :predicate_set_hash sha
   :instrument_versions {:core "v1"}})

(defn with-ref [value field ref-fn]
  (assoc value field (ref-fn value)))

(def candidate
  (with-ref {:schema_id "https://w3id.org/abc/schemas/parser-rq-candidate.schema.json"
             :schema_version "1.0.0"
             :qualification_identity_ref (qualification/qualification-identity-ref qualification-identity)
             :qualification_identity qualification-identity
             :executable_provenance_ref sha}
    :candidate_ref campaign/candidate-ref))

(def provenance
  (with-ref
    {:schema_id "https://w3id.org/abc/schemas/parser-rq-executable-provenance.schema.json"
     :schema_version "2.0.0"
     :candidate_ref (:candidate_ref candidate)
     :qualification_identity_ref (:qualification_identity_ref candidate)
     :status :reproducible}
    :provenance_core_ref campaign/provenance-core-ref))

(def graph
  (with-ref {:schema_version "abc/parser-rq-production-graph/v1"}
    :policy_hash campaign/production-graph-ref))

(def receipt
  (with-ref
    {:schema_id "https://w3id.org/abc/schemas/parser-rq-readiness-receipt.schema.json"
     :schema_version "1.0.0"
     :site_preflight_report_ref sha
     :candidate_ref (:candidate_ref candidate)
     :qualification_identity_ref (:qualification_identity_ref candidate)
     :provenance_core_ref (:provenance_core_ref provenance)
     :production_graph_hash (:policy_hash graph)
     :production_graph_version (:schema_version graph)
     :candidate_git_rev (:parser_git_rev qualification-identity)
     :candidate_tree_clean true
     :evidence_base_git_rev (apply str (repeat 40 "b"))
     :evidence_tree_clean true
     :corpus_snapshot_hash (:corpus_snapshot_hash qualification-identity)
     :corpus_list_hash (:corpus_list_hash qualification-identity)
     :site_facts {:clock_synchronized true}}
    :readiness_receipt_ref campaign/readiness-receipt-ref))

(def authorization
  (with-ref {:schema_id "https://w3id.org/abc/schemas/parser-rq-capture-authorization.schema.json"
             :schema_version "2.0.0"
             :authorization_ordinal 1
             :candidate_ref (:candidate_ref candidate)
             :qualification_identity_ref (:qualification_identity_ref candidate)
             :readiness_receipt_ref (:readiness_receipt_ref receipt)
             :not_before_utc "2026-07-17T00:00:00Z"
             :not_after_utc "2026-07-17T01:00:00Z"
             :repetitions 3
             :reduction "maximum"
             :host_policy_ref sha}
    :authorization_ref campaign/authorization-ref))

(deftest qualification-identity-ref-cli-authenticates-the-candidate-value
  (let [root (fs/create-temp-dir {:prefix "parser-rq-candidate-ref"})
        path (fs/file root "candidate.edn")]
    (spit (str path) (pr-str (assoc candidate :qualification_identity_ref sha-b)))
    (is (= (str (:qualification_identity_ref candidate) "\n")
           (with-out-str
             (campaign/-main "qualification-identity-ref" "--candidate" (str path)))))))

(deftest runtime-inputs-are-a-closed-projection-of-authoritative-edn
  (let [corpus {:entries [{:work_id "work" :source_path "source.txt"
                           :source_sha256 sha}]}]
    (is (= {:schema_version "abc/parser-rq-runtime-inputs/v1"
            :candidate {:candidate_ref (:candidate_ref candidate)
                        :qualification_identity_ref
                        (:qualification_identity_ref candidate)
                        :qualification_identity qualification-identity}
            :corpus corpus
            :source_accountability_corpus
            [{:work_id "work" :source_path "source.txt"
              :original_sha256 sha}]
            :authorization
            {:authorization_ref (:authorization_ref authorization)
             :authorization_ordinal 1
             :candidate_ref (:candidate_ref candidate)
             :qualification_identity_ref
             (:qualification_identity_ref candidate)
             :not_before_utc "2026-07-17T00:00:00Z"
             :not_after_utc "2026-07-17T01:00:00Z"
             :repetitions 3
             :reduction "maximum"}}
           (campaign/runtime-inputs candidate authorization corpus)))))

(def observed-values
  {:fatal_failures 0.0
   :source_span_coverage 1.0
   :silent_drops 0.0
   :diagnostic_completeness 1.0
   :parser_ir_schema_validation 1.0
   :publication_structure 1.0
   :wall_time_seconds 10.0
   :peak_cgroup_memory_bytes 1024.0
   :timeouts 0.0})

(def envelopes
  (update-vals observed-values
               #(hash-map :value % :identity_ref (:qualification_identity_ref candidate))))

(defn json-value [value]
  (walk/postwalk
   (fn [item]
     (cond
       (keyword? item) (name item)
       (map? item) (into {} (map (fn [[key nested]]
                                   [(if (keyword? key) (name key) key)
                                    nested])) item)
       :else item))
   value))

(defn canonical-bytes [value]
  (jcs/canonical-json-bytes (json-value value)))

(defn blob-for [locator value]
  (let [bytes (canonical-bytes value)]
    {:sha256 (hash/format-sha256 (hash/sha256-bytes bytes))
     :bytes (alength bytes)
     :media_type "application/json"
     :locator locator}))

(defn capture-for
  ([members measurements]
   (capture-for candidate authorization members measurements))
  ([candidate-value authorization-value members measurements]
   (with-ref
     {:schema_id "https://w3id.org/abc/schemas/parser-rq-capture-index.schema.json"
      :schema_version "1.0.0"
      :capture_started_at_utc "2026-07-17T00:30:00Z"
      :authorization_ref (:authorization_ref authorization-value)
      :candidate_ref (:candidate_ref candidate-value)
      :qualification_identity_ref (:qualification_identity_ref candidate-value)
      :members (assoc (reduce-kv (fn [refs member value]
                                   (assoc refs member
                                          (blob-for (str (name member) ".json") value)))
                                 {} members)
                      :measurements (blob-for "measurements.json" measurements))}
     :capture_generation_ref campaign/capture-generation-ref)))

(declare write-edn! write-canonical-json!)

(deftest content-references-ignore-only-their-self-field
  (doseq [[value field ref-fn] [[candidate :candidate_ref campaign/candidate-ref]
                                [receipt :readiness_receipt_ref
                                 campaign/readiness-receipt-ref]
                                [authorization :authorization_ref campaign/authorization-ref]]]
    (is (= (get value field) (ref-fn value)))
    (is (not= (get value field) (ref-fn (assoc value :schema_version "changed"))))))

(deftest authorization-is-one-shot-candidate-bound-and-time-bounded
  (is (= [] (campaign/verify-authorization candidate provenance graph receipt authorization
                                           "2026-07-17T00:30:00Z" true)))
  (is (= [] (campaign/verify-authorization-record
             candidate provenance graph receipt authorization)))
  (doseq [bad [(assoc authorization :authorization_ordinal 2)
               (assoc authorization :candidate_ref sha-b)
               (assoc authorization :not_after_utc "2026-07-16T23:59:59Z")
               (assoc authorization :extra true)]]
    (is (seq (campaign/verify-authorization candidate provenance graph receipt bad
                                            "2026-07-17T00:30:00Z" true)))))

(deftest authorization-structure-is-separate-from-clock-permission
  (let [future (campaign/build-authorization
                candidate receipt 1 "2026-07-18T01:00:00Z"
                "2026-07-18T02:00:00Z" sha)]
    (is (empty? (campaign/verify-authorization-record
                 candidate provenance graph receipt future)))
    (is (some #(re-find #"outside" %)
              (campaign/verify-authorization
               candidate provenance graph receipt future
               "2026-07-18T00:00:00Z" true)))
    (is (some #(re-find #"clock is not synchronized" %)
              (campaign/verify-authorization
               candidate provenance graph receipt future
               "2026-07-18T01:30:00Z" false)))))

(deftest authorization-rejects-resealed-readiness-drift
  (doseq [changed [(assoc receipt :candidate_ref sha-b)
                   (assoc receipt :qualification_identity_ref sha-b)
                   (assoc receipt :provenance_core_ref sha-b)
                   (assoc receipt :site_preflight_report_ref sha-b)
                   (assoc receipt :production_graph_version "changed")]
          :let [resealed (assoc changed :readiness_receipt_ref
                                (campaign/readiness-receipt-ref changed))]]
    (is (seq (campaign/verify-authorization-record
              candidate provenance graph resealed authorization))))
  (is (seq (campaign/verify-authorization-record
            candidate provenance graph receipt
            (assoc authorization :readiness_receipt_ref sha-b)))))

(deftest authorization-cli-requires-explicit-clock-and-supports-structural-check
  (let [root (fs/create-temp-dir {:prefix "parser-rq-authorization-cli"})
        candidate-path (fs/file root "candidate.edn")
        provenance-path (fs/file root "provenance.json")
        graph-path (fs/file root "graph.json")
        receipt-path (fs/file root "receipt.json")
        authorization-path (fs/file root "authorization.edn")]
    (write-edn! candidate-path candidate)
    (write-canonical-json! provenance-path provenance)
    (write-canonical-json! graph-path graph)
    (write-canonical-json! receipt-path receipt)
    (write-edn! authorization-path authorization)
    (is (= "ok\n"
           (with-out-str
             (campaign/-main
              "verify-authorization-record"
              "--candidate" (str candidate-path)
              "--provenance" (str provenance-path)
              "--graph" (str graph-path)
              "--receipt" (str receipt-path)
              "--authorization" (str authorization-path)))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (campaign/-main
                  "verify-authorization"
                  "--candidate" (str candidate-path)
                  "--provenance" (str provenance-path)
                  "--graph" (str graph-path)
                  "--receipt" (str receipt-path)
                  "--authorization" (str authorization-path)
                  "--clock-synchronized" "true")))))

(deftest candidate-is-derived-from-live-contracts-and-reproducible-provenance
  (let [root (fs/create-temp-dir {:prefix "parser-rq-candidate"})
        revision (apply str (repeat 40 "a"))
        executable {:name "ab-aozora" :nix_output "/nix/store/parser"
                    :nar_hash sha :sha256 sha :bytes 1 :adapter "ab-aozora"
                    :adapter_version "v1" :parser_git_rev revision
                    :argv_template ["{executable}"]}
        provenance {:status :reproducible
                    :builds [{:build_id "build-a" :output_ref sha}
                             {:build_id "build-b" :output_ref sha}]
                    :executables [executable]}
        _ (write-edn! (fs/file root "abc/data/parser-release-qualification-corpus.edn")
                      (edn/read-string
                       (slurp "data/parser-release-qualification-corpus.edn")))
        _ (write-edn! (fs/file root "abc/data/parser-release-qualification-predicates.edn")
                      (edn/read-string
                       (slurp "data/parser-release-qualification-predicates.edn")))
        _ (write-canonical-json!
           (fs/file root "ab-validator/data/aat-to-parser-ir-mapping-v2.json")
           {:source_aat_version 2
            :mapping_id "mapping"
            :mapping_version "1"
            :mapping_schema_hash sha
            :target_parser_ir_schema_id "parser-ir"
            :target_parser_ir_schema_hash sha})
        value (campaign/build-candidate root revision provenance)
        receipt-value (-> receipt
                          (assoc :candidate_ref (:candidate_ref value)
                                 :qualification_identity_ref
                                 (:qualification_identity_ref value)
                                 :provenance_core_ref
                                 (campaign/provenance-core-ref provenance)
                                 :candidate_git_rev revision
                                 :corpus_snapshot_hash
                                 (get-in value [:qualification_identity
                                                :corpus_snapshot_hash])
                                 :corpus_list_hash
                                 (get-in value [:qualification_identity
                                                :corpus_list_hash]))
                          (dissoc :readiness_receipt_ref)
                          (with-ref :readiness_receipt_ref
                            campaign/readiness-receipt-ref))
        provenance-value (assoc provenance
                                :candidate_ref (:candidate_ref value)
                                :qualification_identity_ref
                                (:qualification_identity_ref value)
                                :provenance_core_ref
                                (campaign/provenance-core-ref provenance))
        authorization-value (campaign/build-authorization
                             value receipt-value 1 "2026-07-17T00:00:00Z"
                             "2026-07-17T01:00:00Z" sha)]
    (is (= (:candidate_ref value) (campaign/candidate-ref value)))
    (is (= (:executable_provenance_ref value)
           (campaign/executable-provenance-ref provenance)))
    (is (= "sha256:bec4fff7ab46003667df6115accf16da88260e02a003a07ab5537e8f5851c203"
           (get-in value [:qualification_identity :predicate_set_hash])))
    (is (= [] (campaign/verify-authorization
               value provenance-value graph receipt-value authorization-value
               "2026-07-17T00:30:00Z" true)))))

(deftest composition-installs-exactly-nine-authenticated-envelopes
  (let [members {:core_attempt (select-keys envelopes
                                            [:fatal_failures :wall_time_seconds :timeouts])
                 :source_recognition (select-keys envelopes [:source_span_coverage])
                 :diagnostic_gap (select-keys envelopes [:silent_drops])
                 :diagnostic_completeness (select-keys envelopes [:diagnostic_completeness])
                 :parser_ir_conformance (select-keys envelopes [:parser_ir_schema_validation])
                 :publication_structure (select-keys envelopes [:publication_structure])
                 :resource (select-keys envelopes [:peak_cgroup_memory_bytes])}
        capture (capture-for members envelopes)
        composed (campaign/compose-measurements candidate capture members)]
    (is (= envelopes composed))
    (is (thrown? clojure.lang.ExceptionInfo
                 (campaign/compose-measurements candidate capture (dissoc members :resource))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (campaign/compose-measurements
                  candidate capture
                  (assoc-in members [:resource :peak_cgroup_memory_bytes :value] 1))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (campaign/compose-measurements
                  candidate capture
                  (assoc-in members [:resource :peak_cgroup_memory_bytes :identity_ref] sha-b))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (campaign/compose-measurements
                  candidate (assoc capture :capture_generation_ref sha-b) members)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (campaign/compose-measurements
                  candidate (assoc capture :candidate_ref sha-b) members)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (campaign/compose-measurements
                  candidate (assoc-in capture [:members :measurements :sha256] sha-b) members)))))

(deftest generation-resolution-requires-uniqueness-and-current-registry
  (let [capture {:authorization_ref (:authorization_ref authorization)}
        evaluation {:registry_ref sha}]
    (is (= capture (campaign/resolve-capture-values authorization [capture])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (campaign/resolve-capture-values authorization [capture capture])))
    (is (= evaluation (campaign/resolve-current-evaluation-values sha [evaluation])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (campaign/resolve-current-evaluation-values sha [])))))

(deftest registry-reader-closure-is-exact
  (is (= []
         (campaign/registry-closure-errors
          "docs/reports/adr-claim-migration-inventory.json"))))

(defn write-edn! [path value]
  (io/make-parents (io/file path))
  (spit path (str (pr-str value) "\n")))

(defn write-canonical-json! [path value]
  (io/make-parents (io/file path))
  (with-open [output (io/output-stream path)]
    (.write output ^bytes (canonical-bytes value))))

(deftest promotion-is-derived-from-committed-generations-and-fails-closed
  (let [root (fs/create-temp-dir {:prefix "parser-rq-promotion"})
        executable {:name "ab-check" :nix_output "/nix/store/parser"
                    :nar_hash sha :sha256 sha :bytes 1 :adapter "ab-aozora"
                    :adapter_version "v1" :parser_git_rev (apply str (repeat 40 "a"))
                    :argv_template ["{executable}"]}
        provenance {:status :reproducible
                    :builds [{:build_id "build-a" :output_ref sha}
                             {:build_id "build-b" :output_ref sha}]
                    :executables [executable]}
        provenance-ref (hash/format-sha256
                        (hash/sha256-json-jcs (json-value provenance)))
        candidate-value (with-ref (assoc candidate :executable_provenance_ref provenance-ref)
                          :candidate_ref campaign/candidate-ref)
        authorization-value
        (with-ref (assoc authorization
                         :candidate_ref (:candidate_ref candidate-value)
                         :qualification_identity_ref
                         (:qualification_identity_ref candidate-value))
          :authorization_ref campaign/authorization-ref)
        candidate-dir (fs/file root (subs (:candidate_ref candidate-value) 7))
        envelope-values (update-vals observed-values
                                     #(hash-map :value %
                                                :identity_ref
                                                (:qualification_identity_ref candidate-value)))
        members {:core_attempt (select-keys envelope-values
                                            [:fatal_failures :wall_time_seconds :timeouts])
                 :source_recognition (select-keys envelope-values [:source_span_coverage])
                 :diagnostic_gap (select-keys envelope-values [:silent_drops])
                 :diagnostic_completeness (select-keys envelope-values [:diagnostic_completeness])
                 :parser_ir_conformance (select-keys envelope-values [:parser_ir_schema_validation])
                 :publication_structure (select-keys envelope-values [:publication_structure])
                 :resource (select-keys envelope-values [:peak_cgroup_memory_bytes])}
        capture-index (capture-for candidate-value authorization-value members envelope-values)
        capture-root (fs/file candidate-dir "captures"
                              (subs (:capture_generation_ref capture-index) 7))
        registry {:entries []}
        registry-ref (hash/format-sha256 (hash/sha256-json-jcs (json-value registry)))
        report {:gate_status :release-qualified
                :admission {:status :admitted}
                :predicate_verdicts
                (mapv #(hash-map :predicate_id % :verdict :pass)
                      campaign/predicate-ids)}
        evaluation-members {:admission_candidate {:candidate true}
                            :admission_report {:status :admitted}
                            :qualification_report report}
        evaluation-index
        (with-ref
          {:schema_id "https://w3id.org/abc/schemas/parser-rq-evaluation-index.schema.json"
           :schema_version "1.0.0"
           :candidate_ref (:candidate_ref candidate-value)
           :qualification_identity_ref (:qualification_identity_ref candidate-value)
           :capture_generation_ref (:capture_generation_ref capture-index)
           :registry_ref registry-ref
           :members (reduce-kv (fn [refs member value]
                                 (assoc refs member
                                        (blob-for (str (name member) ".json") value)))
                               {} evaluation-members)}
          :evaluation_generation_ref campaign/evaluation-generation-ref)
        evaluation-root (fs/file candidate-dir "evaluations"
                                 (subs (:evaluation_generation_ref evaluation-index) 7))
        replication-base
        {:schema_id "https://w3id.org/abc/schemas/parser-rq-replication-receipt.schema.json"
         :schema_version "1.0.0"
         :candidate_ref (:candidate_ref candidate-value)
         :capture_generation_ref (:capture_generation_ref capture-index)
         :primary_failure_domain "primary"
         :replica_failure_domain "replica"
         :status :replicated
         :blobs (mapv (fn [blob]
                        {:blob blob :primary_rehash (:sha256 blob)
                         :replica_rehash (:sha256 blob)})
                      (vals (:members capture-index)))}
        replication (with-ref replication-base :receipt_ref
                      #(campaign/receipt-ref %))
        registry-path (fs/file root "registry.edn")
        measurements-path (fs/file root "measurements.json")
        report-path (fs/file root "report.json")
        provenance-path (fs/file root "provenance.edn")
        adr-0040-path (fs/file root "0040.md")
        adr-0041-path (fs/file root "0041.md")
        options {:runs_root (str root)
                 :candidate_ref (:candidate_ref candidate-value)
                 :registry_path (str registry-path)
                 :measurements_path (str measurements-path)
                 :report_path (str report-path)
                 :provenance_path (str provenance-path)
                 :adr_0040_path (str adr-0040-path)
                 :adr_0041_path (str adr-0041-path)}]
    (write-edn! (fs/file candidate-dir "candidate.edn") candidate-value)
    (write-edn! (fs/file candidate-dir "authorizations" "authorization.edn")
                authorization-value)
    (write-edn! (fs/file capture-root "capture-index.edn") capture-index)
    (doseq [[member value] members]
      (write-canonical-json! (fs/file capture-root
                                      (get-in capture-index [:members member :locator])) value))
    (write-canonical-json! (fs/file capture-root
                                    (get-in capture-index [:members :measurements :locator]))
                           envelope-values)
    (write-edn! (fs/file capture-root "replication-receipt.edn") replication)
    (write-edn! (fs/file evaluation-root "evaluation-index.edn") evaluation-index)
    (doseq [[member value] evaluation-members]
      (write-canonical-json! (fs/file evaluation-root
                                      (get-in evaluation-index [:members member :locator])) value))
    (write-edn! registry-path registry)
    (write-canonical-json! measurements-path envelope-values)
    (write-canonical-json! report-path report)
    (write-edn! provenance-path provenance)
    (spit adr-0040-path "Status: Accepted\n")
    (spit adr-0041-path "Status: Accepted\n")
    (is (= [] (campaign/promotion-errors options)))
    (write-canonical-json! report-path (assoc report :gate_status :not-qualified))
    (is (some #(re-find #"canonical projections" %)
              (campaign/promotion-errors options)))
    (write-canonical-json! report-path report)
    (write-edn! (fs/file candidate-dir "authorizations" "authorization.edn")
                (assoc authorization-value :not_after_utc "2026-07-17T00:15:00Z"))
    (is (some #(re-find #"authorization is invalid" %)
              (campaign/promotion-errors options)))
    (write-edn! (fs/file candidate-dir "authorizations" "authorization.edn")
                authorization-value)
    (write-edn! (fs/file candidate-dir "captures" "sibling" "capture-index.edn")
                capture-index)
    (is (some #(re-find #"capture resolution requires exactly one" %)
              (campaign/promotion-errors options)))))
