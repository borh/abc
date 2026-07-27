(ns abc.tools.parser-rq-campaign-test
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.jcs :as jcs]
            [abc.tools.parser-release-qualification :as qualification]
            [abc.tools.parser-rq-campaign :as campaign]
            [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is]]
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
   :instrument_versions {:core "v1"}
   :instrument_policy_hashes {:core sha}})

(defn with-ref [value field ref-fn]
  (assoc value field (ref-fn value)))

(def provenance-proof
  (with-ref
    {:status :reproducible
     :builds [{:build_id "build-a"
               :store_uri "local?root=/tmp/build-a"
               :output_ref sha
               :build_record_ref sha}
              {:build_id "build-b"
               :store_uri "local?root=/tmp/build-b"
               :output_ref sha
               :build_record_ref sha-b}]
     :executables
     [{:name "ab-aozora"
       :nix_output "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-parser-rq"
       :nar_hash sha
       :sha256 sha
       :bytes 1
       :adapter "ab-aozora"
       :adapter_version "candidate"
       :parser_git_rev (:parser_git_rev qualification-identity)
       :argv_template ["{executable}" "{source}"]}]}
    :provenance_core_ref campaign/provenance-core-ref))

(def candidate
  (with-ref {:schema_id "https://w3id.org/abc/schemas/parser-rq-candidate.schema.json"
             :schema_version "1.0.0"
             :qualification_identity_ref
             (qualification/qualification-identity-ref qualification-identity)
             :qualification_identity qualification-identity
             :executable_provenance_ref
             (campaign/executable-provenance-ref provenance-proof)}
    :candidate_ref campaign/candidate-ref))

(def provenance
  (assoc provenance-proof
         :schema_id
         "https://w3id.org/abc/schemas/parser-rq-executable-provenance.schema.json"
         :schema_version "2.0.0"
         :candidate_ref (:candidate_ref candidate)
         :qualification_identity_ref (:qualification_identity_ref candidate)))

(def graph
  (with-ref {:schema_version "abc/parser-rq-production-graph/v1"}
    :policy_hash campaign/production-graph-ref))

(def receipt
  (with-ref
    {:schema_id "https://w3id.org/abc/schemas/parser-rq-readiness-receipt.schema.json"
     :schema_version "2.0.0"
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
     :corpus_list_hash (:corpus_list_hash qualification-identity)}
    :readiness_receipt_ref campaign/readiness-receipt-ref))

(def authorization
  (with-ref {:schema_id "https://w3id.org/abc/schemas/parser-rq-capture-authorization.schema.json"
             :schema_version "3.0.0"
             :authorization_ordinal 1
             :candidate_ref (:candidate_ref candidate)
             :qualification_identity_ref (:qualification_identity_ref candidate)
             :readiness_receipt_ref (:readiness_receipt_ref receipt)
             :not_before_utc "2026-07-17T00:00:00Z"
             :not_after_utc "2026-07-17T01:00:00Z"
             :repetitions 3
             :reduction "maximum"}
    :authorization_ref campaign/authorization-ref))

(deftest qualification-identity-ref-cli-authenticates-the-candidate-value
  (let [root (fs/create-temp-dir {:prefix "parser-rq-candidate-ref"})
        path (fs/file root "candidate.edn")]
    (spit (str path) (pr-str (assoc candidate :qualification_identity_ref sha-b)))
    (is (= (str (:qualification_identity_ref candidate) "\n")
           (with-out-str
             (campaign/-main "qualification-identity-ref" "--candidate" (str path)))))))

(deftest candidate-cli-decodes-json-provenance-before-derivation
  (let [root (fs/create-temp-dir {:prefix "parser-rq-candidate-json"})
        provenance-path (fs/file root "provenance.json")
        output-path (fs/file root "candidate.edn")
        received (atom nil)]
    (spit (str provenance-path) "{\"status\":\"reproducible\"}")
    (with-redefs [campaign/build-candidate
                  (fn [repo revision value]
                    (reset! received [repo revision value])
                    candidate)]
      (campaign/-main "candidate" "--repo" (str root)
                      "--parser-git-rev" (apply str (repeat 40 "a"))
                      "--provenance" (str provenance-path)
                      "--out" (str output-path)))
    (is (= {:status "reproducible"} (nth @received 2)))
    (is (= candidate (edn/read-string (slurp (str output-path)))))))

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

(defn write-edn! [path value]
  (io/make-parents (io/file path))
  (spit path (str (pr-str value) "\n")))

(defn write-canonical-json! [path value]
  (io/make-parents (io/file path))
  (with-open [output (io/output-stream path)]
    (.write output ^bytes (canonical-bytes value))))

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

(defn- workspace-root []
  (loop [path (fs/absolutize ".")]
    (if (and (fs/exists? (fs/file path "justfile"))
             (fs/directory? (fs/file path "abc"))
             (fs/directory? (fs/file path "ab-validator")))
      path
      (if-let [parent (fs/parent path)]
        (recur parent)
        (throw (ex-info "workspace root is not reachable" {:start (str (fs/absolutize "."))}))))))

(defn- provenance-script []
  (if-let [configured (System/getenv "PARSER_RQ_PROVENANCE_SCRIPT")]
    (fs/path configured)
    (fs/file (workspace-root) "ab-validator" "reports" "parser-ir"
             "parser-rq-campaign-provenance.py")))

(deftest content-references-ignore-only-their-self-field
  (doseq [[value field ref-fn] [[candidate :candidate_ref campaign/candidate-ref]
                                [receipt :readiness_receipt_ref
                                 campaign/readiness-receipt-ref]
                                [authorization :authorization_ref campaign/authorization-ref]]]
    (is (= (get value field) (ref-fn value)))
    (is (not= (get value field) (ref-fn (assoc value :schema_version "changed"))))))

(deftest candidate-provenance-projection-unifies-proof-and-bound-record
  (is (= provenance-proof
         (campaign/candidate-provenance-value provenance-proof)))
  (is (= (:executable_provenance_ref candidate)
         (campaign/executable-provenance-ref provenance)))
  (is (not= (:executable_provenance_ref candidate)
            (campaign/executable-provenance-ref
             (assoc-in provenance [:executables 0 :bytes] 2)))))

(deftest bound-provenance-envelope-and-core-are-authenticated
  (is (= [] (campaign/verify-provenance-errors candidate provenance)))
  (doseq [changed [(assoc provenance :schema_version "changed")
                   (assoc provenance :schema_id "https://example.invalid/provenance")
                   (assoc provenance :extra true)]]
    (is (some #(re-find #"bound provenance envelope" %)
              (campaign/verify-provenance-errors candidate changed))))
  (is (some #(re-find #"provenance core" %)
            (campaign/verify-provenance-errors
             candidate (assoc-in provenance [:executables 0 :bytes] 2)))))

(deftest python-bound-provenance-authenticates-as-the-unbound-proof
  (let [root (fs/create-temp-dir {:prefix "parser-rq-bound-provenance"})
        proof-path (fs/file root "proof.json")
        bound-path (fs/file root "bound.json")
        script (provenance-script)]
    (write-canonical-json! proof-path provenance-proof)
    (let [{:keys [exit err]}
          (shell/sh "python3" (str script) "bind-provenance"
                    "--proof" (str proof-path)
                    "--candidate-ref" (:candidate_ref candidate)
                    "--qualification-identity-ref"
                    (:qualification_identity_ref candidate)
                    "--out" (str bound-path))
          bound (walk/keywordize-keys (files/read-json bound-path))]
      (is (= 0 exit) err)
      (is (= (:executable_provenance_ref candidate)
             (campaign/executable-provenance-ref bound)))
      (is (= [] (campaign/verify-provenance-errors candidate bound))))))

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

(deftest authorization-rejects-candidate-provenance-reference-mismatch
  (let [errors (campaign/verify-authorization-record
                (assoc candidate :executable_provenance_ref sha-b)
                provenance graph receipt authorization)]
    (is (some #{"executable provenance does not authenticate the candidate"}
              errors))))

(deftest readiness-and-authorization-bind-no-runtime-place
  (is (empty? (campaign/verify-authorization-record
               candidate provenance graph receipt authorization)))
  (doseq [removed (mapv keyword [(str "site" "_preflight_report_ref")
                                 (str "site" "_facts")
                                 (str "host" "_policy_ref")])]
    (is (not (contains? receipt removed)))
    (is (not (contains? authorization removed)))))

(deftest predicate-roster-is-the-closed-nine
  ;; promotion-errors and the campaign fixtures both derive from this literal,
  ;; so shrinking the roster would otherwise pass every generated-report test.
  (is (= 9 (count campaign/predicate-ids))))

(deftest evidence-integrity-authenticates-closed-manifest-membership
  (let [blob {:sha256 sha :bytes 10 :media_type "application/json"
              :locator "aa/blob"}
        verified {:schema_id
                  "https://w3id.org/abc/schemas/parser-rq-evidence-integrity-receipt.schema.json"
                  :schema_version "1.0.0"
                  :receipt_ref sha
                  :candidate_ref (:candidate_ref candidate)
                  :capture_generation_ref sha-b
                  :status :verified
                  :blobs [{:blob blob :rehash sha :observed_bytes 10}]}]
    (is (= [] (campaign/evidence-integrity-errors verified [blob])))
    (is (seq (campaign/evidence-integrity-errors
              verified [(assoc blob :bytes 11)])))
    (is (seq (campaign/evidence-integrity-errors
              (assoc-in verified [:blobs 0 :rehash] sha-b) [blob])))
    (is (seq (campaign/evidence-integrity-errors
              verified [blob (assoc blob :locator "bb/extra")])))
    (is (seq (campaign/evidence-integrity-errors
              (update verified :blobs conj
                      {:blob (assoc blob :locator "bb/extra")
                       :rehash sha :observed_bytes 10})
              [blob])))))

(deftest python-evidence-receipt-authenticates-in-clojure
  (let [root (fs/create-temp-dir {:prefix "parser-rq-evidence-integrity"})
        payload (.getBytes "immutable evidence" java.nio.charset.StandardCharsets/UTF_8)
        digest (hash/format-sha256 (hash/sha256-bytes payload))
        blob {:sha256 digest :bytes (alength payload)
              :media_type "application/json" :locator "blob"}
        blobs-path (fs/file root "blobs.json")
        receipt-path (fs/file root "receipt.json")
        script (provenance-script)]
    (spit (str (fs/file root "blob")) "immutable evidence")
    (write-canonical-json! blobs-path [blob])
    (let [{:keys [exit err]}
          (shell/sh "python3" (str script) "verify-evidence"
                    "--blobs" (str blobs-path)
                    "--evidence-root" (str root)
                    "--candidate-ref" (:candidate_ref candidate)
                    "--capture-generation-ref" sha-b
                    "--out" (str receipt-path))
          receipt-value (walk/keywordize-keys (files/read-json receipt-path))]
      (is (= 0 exit) err)
      (is (= [] (campaign/evidence-integrity-receipt-errors
                 receipt-value (:candidate_ref candidate) sha-b [blob])))
      (is (some #(re-find #"self-reference" %)
                (campaign/evidence-integrity-receipt-errors
                 (assoc receipt-value :receipt_ref sha)
                 (:candidate_ref candidate) sha-b [blob]))))))

(deftest authorization-structure-is-separate-from-clock-permission
  (let [future (campaign/build-authorization
                candidate receipt 1 "2026-07-18T01:00:00Z"
                "2026-07-18T02:00:00Z")]
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

(defn- write-live-contracts!
  "Stage the governed authorities build-candidate reads into a synthetic repo
  root, copied from the live abc tree so the test binds what the campaign
  actually ships rather than a restatement of it."
  [root]
  (write-edn! (fs/file root "abc/data/parser-release-qualification-corpus.edn")
              (edn/read-string (slurp "data/parser-release-qualification-corpus.edn")))
  (write-edn! (fs/file root "abc/data/parser-release-qualification-predicates.edn")
              (edn/read-string (slurp "data/parser-release-qualification-predicates.edn")))
  (doseq [relative (mapcat identity (vals campaign/instrument-policy-paths))
          :let [target (fs/file root "abc" relative)]]
    (files/create-parent-dirs! target)
    (fs/copy (fs/file relative) target {:replace-existing true}))
  (write-canonical-json!
   (fs/file root "ab-validator/data/aat-to-parser-ir-mapping-v2.json")
   {:source_aat_version 2
    :mapping_id "mapping"
    :mapping_version "1"
    :mapping_schema_hash sha
    :target_parser_ir_schema_id "parser-ir"
    :target_parser_ir_schema_hash sha}))

(deftest instrument-policy-hashes-close-the-membership-they-claim-to-bind
  (let [root (fs/create-temp-dir {:prefix "parser-rq-instrument-policy"})
        revision (apply str (repeat 40 "a"))
        executable {:name "ab-aozora" :nix_output "/nix/store/parser"
                    :nar_hash sha :sha256 sha :bytes 1 :adapter "ab-aozora"
                    :adapter_version "v1" :parser_git_rev revision
                    :argv_template ["{executable}"]}
        provenance (with-ref
                     {:status :reproducible
                      :builds [{:build_id "build-a" :output_ref sha}
                               {:build_id "build-b" :output_ref sha}]
                      :executables [executable]}
                     :provenance_core_ref campaign/provenance-core-ref)
        _ (write-live-contracts! root)
        value (campaign/build-candidate root revision provenance)
        bound (get-in value [:qualification_identity :instrument_policy_hashes])]
    ;; Every capture member that contributes an observation must name a
    ;; governed authority. A member missing here is exactly the gap this key
    ;; exists to close, so the two sets are compared rather than sampled.
    (is (= (into (sorted-set) (map name) (keys campaign/member-observed-keys))
           (into (sorted-set) (keys bound))))
    ;; A single-document member carries that document's own content hash,
    ;; byte-for-byte -- widening the map to vectors must not rotate members
    ;; whose governance did not change.
    (is (= (hash/format-sha256
            (hash/sha256-json-jcs
             (files/read-json
              (fs/file root "abc"
                       (first (:resource campaign/instrument-policy-paths))))))
           (get bound "resource")))
    ;; A multi-document member folds its documents' hashes in declared order.
    (is (= (hash/format-sha256
            (hash/sha256-json-jcs
             (mapv #(hash/format-sha256
                     (hash/sha256-json-jcs
                      (files/read-json (fs/file root "abc" %))))
                   (:source_recognition campaign/instrument-policy-paths))))
           (get bound "source_recognition")))
    ;; A policy that is named but absent must fail the build rather than
    ;; producing an identity with a hole in it.
    (fs/delete (fs/file root "abc"
                        (first (:resource campaign/instrument-policy-paths))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (campaign/build-candidate root revision provenance)))))

(deftest classified-source-policy-edits-rotate-the-qualification-identity
  ;; Q16: the classified-source policy decides which eligible bytes are
  ;; RECOGNIZED, and so decides `source_span_coverage`. Until 2026-07-27 only
  ;; the ignored-regions taxonomy was bound for `:source_recognition`, so this
  ;; policy entered the identity only as a side effect of `parser_git_rev` --
  ;; the identity rotated on every commit and never named what changed.
  (let [root (fs/create-temp-dir {:prefix "parser-rq-classified-source"})
        revision (apply str (repeat 40 "a"))
        executable {:name "ab-aozora" :nix_output "/nix/store/parser"
                    :nar_hash sha :sha256 sha :bytes 1 :adapter "ab-aozora"
                    :adapter_version "v1" :parser_git_rev revision
                    :argv_template ["{executable}"]}
        provenance (with-ref
                     {:status :reproducible
                      :builds [{:build_id "build-a" :output_ref sha}
                               {:build_id "build-b" :output_ref sha}]
                      :executables [executable]}
                     :provenance_core_ref campaign/provenance-core-ref)
        _ (write-live-contracts! root)
        before (campaign/build-candidate root revision provenance)
        policy-path (fs/file root "abc"
                             "data/parser-rq-ab-aozora-classified-source-v1.json")
        policy (walk/keywordize-keys (files/read-json policy-path))
        ;; A representative policy edit: implement the declared but
        ;; unimplemented `publication_metadata` role. This stands in for any
        ;; Q15 amendment; it is NOT the Q15 fix itself. The frame gap is a
        ;; coordinate mismatch -- the ledger lexes the body projection while
        ;; `eligible_bytes` counts the whole file -- so this rule would not
        ;; change any coverage measurement. What it exercises is rotation.
        _ (write-canonical-json!
           policy-path
           (update policy :rules conj
                   {:construct_id "publication_metadata"
                    :source_role "publication_metadata"
                    :disposition "emitted_semantic_value"
                    :evidence_class "accepted_text"
                    :target_relation "emits"}))
        after (campaign/build-candidate root revision provenance)]
    ;; The rotation the decision requires: same commit, same parser, and the
    ;; identity still moves because the recognition policy moved.
    (is (not= (get-in before [:qualification_identity :instrument_policy_hashes
                              "source_recognition"])
              (get-in after [:qualification_identity :instrument_policy_hashes
                             "source_recognition"])))
    (is (not= (:qualification_identity_ref before)
              (:qualification_identity_ref after)))
    ;; ... and it moves without touching the predicate contract, which is a
    ;; separate rotation route.
    (is (= (get-in before [:qualification_identity :predicate_set_hash])
           (get-in after [:qualification_identity :predicate_set_hash])))))

(deftest instrument-policy-edits-rotate-identity-without-touching-the-predicate-contract
  (let [root (fs/create-temp-dir {:prefix "parser-rq-instrument-rotation"})
        revision (apply str (repeat 40 "a"))
        executable {:name "ab-aozora" :nix_output "/nix/store/parser"
                    :nar_hash sha :sha256 sha :bytes 1 :adapter "ab-aozora"
                    :adapter_version "v1" :parser_git_rev revision
                    :argv_template ["{executable}"]}
        provenance (with-ref
                     {:status :reproducible
                      :builds [{:build_id "build-a" :output_ref sha}
                               {:build_id "build-b" :output_ref sha}]
                      :executables [executable]}
                     :provenance_core_ref campaign/provenance-core-ref)
        _ (write-live-contracts! root)
        before (campaign/build-candidate root revision provenance)
        policy-path (fs/file root "abc"
                             (first (:diagnostic_completeness
                                     campaign/instrument-policy-paths)))
        _ (write-canonical-json!
           policy-path
           (assoc (walk/keywordize-keys (files/read-json policy-path))
                  :algorithm_version "edited-for-this-test"))
        after (campaign/build-candidate root revision provenance)]
    ;; The whole point of the coordinate: instrument semantics move the
    ;; identity the admission and promotion chain binds ...
    (is (not= (:qualification_identity_ref before) (:qualification_identity_ref after)))
    (is (not= (:candidate_ref before) (:candidate_ref after)))
    ;; ... without braiding themselves into what the release must prove.
    (is (= (get-in before [:qualification_identity :predicate_set_hash])
           (get-in after [:qualification_identity :predicate_set_hash])))))

(deftest candidate-is-derived-from-live-contracts-and-reproducible-provenance
  (let [root (fs/create-temp-dir {:prefix "parser-rq-candidate"})
        revision (apply str (repeat 40 "a"))
        executable {:name "ab-aozora" :nix_output "/nix/store/parser"
                    :nar_hash sha :sha256 sha :bytes 1 :adapter "ab-aozora"
                    :adapter_version "v1" :parser_git_rev revision
                    :argv_template ["{executable}"]}
        provenance (with-ref
                     {:status :reproducible
                      :builds [{:build_id "build-a" :output_ref sha}
                               {:build_id "build-b" :output_ref sha}]
                      :executables [executable]}
                     :provenance_core_ref campaign/provenance-core-ref)
        _ (write-live-contracts! root)
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
                                :schema_id campaign/executable-provenance-schema-id
                                :schema_version "2.0.0"
                                :candidate_ref (:candidate_ref value)
                                :qualification_identity_ref
                                (:qualification_identity_ref value)
                                :provenance_core_ref (:provenance_core_ref provenance))
        authorization-value (campaign/build-authorization
                             value receipt-value 1 "2026-07-17T00:00:00Z"
                             "2026-07-17T01:00:00Z")]
    (is (= (:candidate_ref value) (campaign/candidate-ref value)))
    (is (= (:executable_provenance_ref value)
           (campaign/executable-provenance-ref provenance)))
    (is (= "sha256:bec4fff7ab46003667df6115accf16da88260e02a003a07ab5537e8f5851c203"
           (get-in value [:qualification_identity :predicate_set_hash])))
    (is (= (hash/sha256-json-abc-legacy-v0
            (files/read-json
             (fs/file root "ab-validator/data/aat-to-parser-ir-mapping-v2.json")))
           (get-in value [:qualification_identity :mapping_hash])))
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

(deftest measurement-projection-does-not-require-an-evaluation
  (let [root (fs/create-temp-dir {:prefix "parser-rq-measurement-projection"})
        candidate-dir (fs/file root (subs (:candidate_ref candidate) 7))
        members {:core_attempt (select-keys envelopes
                                            [:fatal_failures :wall_time_seconds :timeouts])
                 :source_recognition (select-keys envelopes [:source_span_coverage])
                 :diagnostic_gap (select-keys envelopes [:silent_drops])
                 :diagnostic_completeness (select-keys envelopes [:diagnostic_completeness])
                 :parser_ir_conformance (select-keys envelopes [:parser_ir_schema_validation])
                 :publication_structure (select-keys envelopes [:publication_structure])
                 :resource (select-keys envelopes [:peak_cgroup_memory_bytes])}
        capture (capture-for members envelopes)
        capture-root (fs/file candidate-dir "captures"
                              (subs (:capture_generation_ref capture) 7))
        output (fs/file root "measurements.json")]
    (write-edn! (fs/file candidate-dir "candidate.edn") candidate)
    (write-edn! (fs/file candidate-dir "authorizations" "authorization.edn")
                authorization)
    (write-edn! (fs/file capture-root "capture-index.edn") capture)
    (doseq [[member value] members]
      (write-canonical-json! (fs/file capture-root
                                      (get-in capture [:members member :locator]))
                             value))
    (write-canonical-json! (fs/file capture-root
                                    (get-in capture [:members :measurements :locator]))
                           envelopes)
    (#'abc.tools.parser-rq-campaign/project!
     {:runs_root (str root)
      :candidate_ref (:candidate_ref candidate)
      :measurements_out (str output)})
    (is (= (hash/sha256-bytes (canonical-bytes envelopes))
           (hash/sha256-file output)))))

(deftest promotion-is-derived-from-committed-generations-and-fails-closed
  (let [root (fs/create-temp-dir {:prefix "parser-rq-promotion"})
        executable {:name "ab-check" :nix_output "/nix/store/parser"
                    :nar_hash sha :sha256 sha :bytes 1 :adapter "ab-aozora"
                    :adapter_version "v1" :parser_git_rev (apply str (repeat 40 "a"))
                    :argv_template ["{executable}"]}
        provenance-proof (with-ref
                           {:status :reproducible
                            :builds [{:build_id "build-a" :output_ref sha}
                                     {:build_id "build-b" :output_ref sha}]
                            :executables [executable]}
                           :provenance_core_ref campaign/provenance-core-ref)
        candidate-value (with-ref (assoc candidate :executable_provenance_ref
                                         (campaign/executable-provenance-ref
                                          provenance-proof))
                          :candidate_ref campaign/candidate-ref)
        provenance (assoc provenance-proof
                          :schema_id campaign/executable-provenance-schema-id
                          :schema_version "2.0.0"
                          :candidate_ref (:candidate_ref candidate-value)
                          :qualification_identity_ref
                          (:qualification_identity_ref candidate-value))
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
        evidence-integrity-base
        {:schema_id
         "https://w3id.org/abc/schemas/parser-rq-evidence-integrity-receipt.schema.json"
         :schema_version "1.0.0"
         :candidate_ref (:candidate_ref candidate-value)
         :capture_generation_ref (:capture_generation_ref capture-index)
         :status :verified
         :blobs (mapv (fn [blob]
                        {:blob blob :rehash (:sha256 blob)
                         :observed_bytes (:bytes blob)})
                      (vals (:members capture-index)))}
        evidence-integrity (with-ref evidence-integrity-base :receipt_ref
                             #(campaign/receipt-ref %))
        registry-path (fs/file root "registry.edn")
        measurements-path (fs/file root "measurements.json")
        report-path (fs/file root "report.json")
        provenance-path (fs/file root "provenance.json")
        decisions-path (fs/file root "decisions.edn")
        options {:runs_root (str root)
                 :candidate_ref (:candidate_ref candidate-value)
                 :registry_path (str registry-path)
                 :measurements_path (str measurements-path)
                 :report_path (str report-path)
                 :provenance_path (str provenance-path)
                 :decisions_path (str decisions-path)}]
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
    (write-canonical-json! (fs/file capture-root "evidence-integrity-receipt.json")
                           evidence-integrity)
    (write-edn! (fs/file evaluation-root "evaluation-index.edn") evaluation-index)
    (doseq [[member value] evaluation-members]
      (write-canonical-json! (fs/file evaluation-root
                                      (get-in evaluation-index [:members member :locator])) value))
    (write-edn! registry-path registry)
    (write-canonical-json! measurements-path envelope-values)
    (write-canonical-json! report-path report)
    (write-canonical-json! provenance-path provenance)
    (let [decision (fn [slug status]
                     (cond-> {:slug slug
                              :title slug
                              :status status
                              :date "2026-07-20"
                              :topics [:parser]
                              :relations []
                              :claims []}
                       (= :accepted status)
                       (assoc :accepted "2026-07-20"
                              :validation-scope :smoke-corpus
                              :release-authority :publication)))
          accepted-corpus
          {:decisions
           [(decision "process-tree-memory-qualification" :accepted)
            (decision "parser-release-instrument-bindings" :accepted)]}]
      (write-edn! decisions-path accepted-corpus)
      (is (= [] (campaign/promotion-errors options)))
      (let [verification (campaign/promotion-verification options)]
        (is (= [] (:problems verification))
            "promotion-errors and promotion-verification agree on success")
        (is (= (:candidate_ref candidate-value)
               (:candidate_ref (:candidate verification))))
        (is (= (walk/keywordize-keys (files/read-json report-path))
               (:qualification-report verification)))
        (is (= (:evaluation_generation_ref evaluation-index)
               (:evaluation_generation_ref (:evaluation verification))))
        (is (= registry-ref (:registry-ref verification)))
        (is (= (hash/format-sha256 (hash/sha256-file registry-path))
               (:registry-file-hash verification))
            "registry-file-hash authenticates the exact registry bytes read")
        (is (= (walk/keywordize-keys (files/read-json provenance-path))
               (:provenance verification))))
      (write-edn! decisions-path
                  {:decisions
                   [(decision "process-tree-memory-qualification" :proposed)
                    (decision "parser-release-instrument-bindings" :accepted)]})
      (is (some #{"decision process-tree-memory-qualification (ADR 0040) is not accepted"}
                (campaign/promotion-errors options))
          "a dependency decision that is not :accepted blocks promotion")
      (write-edn! decisions-path
                  {:decisions
                   [(decision "parser-release-instrument-bindings" :accepted)]})
      (is (some #{"decision process-tree-memory-qualification (ADR 0040) is not accepted"}
                (campaign/promotion-errors options))
          "a missing dependency decision blocks promotion")
      (spit decisions-path "{:decisions []} {:extra true}")
      (is (some #(re-find #"exactly one EDN form" %)
                (campaign/promotion-errors options))
          "a malformed decisions corpus is reported, never treated as accepted")
      (write-edn! decisions-path
                  {:decisions
                   [{:slug "process-tree-memory-qualification" :status :accepted}
                    {:slug "parser-release-instrument-bindings" :status :accepted}]})
      (is (some #(re-find #"invalid-shape|missing required key|:title" %)
                (campaign/promotion-errors options))
          "a shape-invalid corpus never authorizes promotion, even with accepted statuses")
      (write-edn! decisions-path accepted-corpus)
      (is (= [] (campaign/promotion-errors options))))
    (write-canonical-json! provenance-path
                           (assoc provenance :schema_version "changed"))
    (is (some #{"bound provenance envelope is invalid"}
              (campaign/promotion-errors options)))
    (write-canonical-json! provenance-path provenance)
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
    (write-canonical-json! (fs/file capture-root "evidence-integrity-receipt.json")
                           (assoc evidence-integrity :status :unverified))
    (is (some #{"evidence integrity status is not verified"}
              (campaign/promotion-errors options)))
    (fs/delete (fs/file capture-root "evidence-integrity-receipt.json"))
    (is (seq (campaign/promotion-errors options)))
    (write-canonical-json! (fs/file capture-root "evidence-integrity-receipt.json")
                           evidence-integrity)
    (is (= [] (campaign/promotion-errors options)))
    (write-edn! (fs/file candidate-dir "captures" "sibling" "capture-index.edn")
                capture-index)
    (is (some #(re-find #"capture resolution requires exactly one" %)
              (campaign/promotion-errors options)))))
