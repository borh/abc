(ns abc.tools.parser-rq-campaign-test
  (:require [abc.tools.hash :as hash]
            [abc.tools.parser-release-qualification :as qualification]
            [abc.tools.parser-rq-campaign :as campaign]
            [clojure.test :refer [deftest is testing]]))

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

(def authorization
  (with-ref {:schema_id "https://w3id.org/abc/schemas/parser-rq-capture-authorization.schema.json"
             :schema_version "1.0.0"
             :authorization_ordinal 1
             :candidate_ref (:candidate_ref candidate)
             :qualification_identity_ref (:qualification_identity_ref candidate)
             :not_before_utc "2026-07-17T00:00:00Z"
             :not_after_utc "2026-07-17T01:00:00Z"
             :repetitions 3
             :reduction "maximum"
             :host_policy_ref sha}
    :authorization_ref campaign/authorization-ref))

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

(deftest content-references-ignore-only-their-self-field
  (doseq [[value field ref-fn] [[candidate :candidate_ref campaign/candidate-ref]
                                [authorization :authorization_ref campaign/authorization-ref]]]
    (is (= (get value field) (ref-fn value)))
    (is (not= (get value field) (ref-fn (assoc value :schema_version "changed"))))))

(deftest authorization-is-one-shot-candidate-bound-and-time-bounded
  (is (= [] (campaign/verify-authorization candidate authorization
                                           "2026-07-17T00:30:00Z")))
  (doseq [bad [(assoc authorization :authorization_ordinal 2)
               (assoc authorization :candidate_ref sha-b)
               (assoc authorization :not_after_utc "2026-07-16T23:59:59Z")
               (assoc authorization :extra true)]]
    (is (seq (campaign/verify-authorization candidate bad
                                            "2026-07-17T00:30:00Z")))))

(deftest composition-installs-exactly-nine-authenticated-envelopes
  (let [members {:core_attempt (select-keys envelopes
                                            [:fatal_failures :wall_time_seconds :timeouts])
                 :source_recognition (select-keys envelopes [:source_span_coverage])
                 :diagnostic_gap (select-keys envelopes [:silent_drops])
                 :diagnostic_completeness (select-keys envelopes [:diagnostic_completeness])
                 :parser_ir_conformance (select-keys envelopes [:parser_ir_schema_validation])
                 :publication_structure (select-keys envelopes [:publication_structure])
                 :resource (select-keys envelopes [:peak_cgroup_memory_bytes])}
        composed (campaign/compose-measurements candidate {} members)]
    (is (= envelopes composed))
    (is (thrown? clojure.lang.ExceptionInfo
                 (campaign/compose-measurements candidate {} (dissoc members :resource))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (campaign/compose-measurements
                  candidate {} (assoc-in members [:resource :peak_cgroup_memory_bytes] 1))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (campaign/compose-measurements
                  candidate {} (assoc-in members [:resource :peak_cgroup_memory_bytes :identity_ref]
                                         sha-b))))))

(deftest generation-resolution-requires-uniqueness-and-current-registry
  (let [capture {:authorization_ref (:authorization_ref authorization)}
        evaluation {:registry_ref sha}]
    (is (= capture (campaign/resolve-capture-values authorization [capture])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (campaign/resolve-capture-values authorization [capture capture])))
    (is (= evaluation (campaign/resolve-current-evaluation-values sha [evaluation])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (campaign/resolve-current-evaluation-values sha [])))))

(deftest promotion-is-fail-closed
  (let [blob {:sha256 sha :bytes 1 :media_type "application/json" :locator "blob"}
        executable {:name "ab-check" :nix_output "/nix/store/parser"
                    :nar_hash sha :sha256 sha :bytes 1 :adapter "ab-aozora"
                    :adapter_version "v1" :parser_git_rev (apply str (repeat 40 "a"))
                    :argv_template ["{executable}"]}
        positive {:candidate candidate
                  :authorization authorization
                  :capture {:candidate_ref (:candidate_ref candidate)
                            :authorization_ref (:authorization_ref authorization)}
                  :evaluation {:candidate_ref (:candidate_ref candidate)
                               :registry_ref sha}
                  :current_registry_ref sha
                  :qualification_report {:gate_status :release-qualified
                                         :adr_0039_status "Accepted"
                                         :admission {:status :admitted}
                                         :predicate_verdicts
                                         (mapv #(hash-map :predicate_id % :verdict :pass)
                                               campaign/predicate-ids)}
                  :provenance {:status :reproducible
                               :builds [{:build_id "build-a" :output_ref sha}
                                        {:build_id "build-b" :output_ref sha}]
                               :executables [executable]}
                  :replication {:status :replicated
                                :blobs [{:blob blob :primary_rehash sha
                                         :replica_rehash sha}]}
                  :manifest_blobs [blob]
                  :adr_0040_status "Accepted"
                  :adr_0041_status "Accepted"
                  :capture_count 1
                  :canonical_equal true}]
    (is (= [] (campaign/promotion-errors positive)))
    (doseq [bad [(assoc-in positive [:qualification_report :gate_status] :not-qualified)
                 (assoc-in positive [:qualification_report :admission :status] :conflict)
                 (assoc positive :current_registry_ref sha-b)
                 (assoc-in positive [:replication :status] :unavailable)
                 (assoc positive :capture_count 2)
                 (assoc positive :canonical_equal false)]]
      (is (seq (campaign/promotion-errors bad))))))
