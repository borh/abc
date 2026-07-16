(ns abc.tools.parser-rq-source-accountability-test
  (:require [abc.tools.hash :as hash]
            [abc.tools.json :as json]
            [abc.tools.parser-release-qualification :as qualification]
            [abc.tools.parser-rq-capture :as capture]
            [abc.tools.parser-rq-source-accountability :as rq-source]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [clojure.walk :as walk]))

(def qualification-identity
  {:aat_version 2
   :aat_adapter "fixture-adapter"
   :aat_adapter_version "1.0.0"
   :mapping_id "https://example.test/mapping"
   :mapping_version "1"
   :mapping_hash (str "sha256:" (apply str (repeat 64 "1")))
   :mapping_schema_hash (str "sha256:" (apply str (repeat 64 "2")))
   :parser_ir_schema_id "https://example.test/parser-ir"
   :parser_ir_schema_hash (str "sha256:" (apply str (repeat 64 "3")))
   :parser_git_rev "fixture-revision"
   :corpus_snapshot_hash (str "sha256:" (apply str (repeat 64 "4")))
   :corpus_list_hash (str "sha256:" (apply str (repeat 64 "5")))
   :predicate_set_hash (str "sha256:" (apply str (repeat 64 "6")))
   :instrument_versions {:source_accountability "parser-rq-source-accountability-v1"}})

(def identity-ref
  (qualification/qualification-identity-ref qualification-identity))

(deftest qualification-identity-ref-matches-rust-golden
  (is (= "sha256:8823c4600a7b9cff9b03728247dbd991474a8bbdf528cec8752b63219e68ae85"
         identity-ref)))

(def taxonomy-text
  "{\"$schema\":\"https://w3id.org/abc/schemas/parser-rq-ignored-regions.schema.json\",\"coordinate_system\":\"decoded_utf8\",\"rules\":[],\"schema_version\":\"abc/parser-rq-ignored-regions/v1\",\"taxonomy_version\":\"parser-rq-ignored-regions-v1\"}")

(def taxonomy-hash
  (hash/format-sha256 (hash/sha256-string taxonomy-text)))

(defn- aggregate-value
  []
  {:schema_version "abc/parser-rq-source-accountability-aggregate/v1"
   :identity_ref identity-ref
   :taxonomy_version "parser-rq-ignored-regions-v1"
   :taxonomy_hash taxonomy-hash
   :coordinate_system "decoded_utf8"
   :status "ok"
   :work_completeness {:expected 1 :observed 1 :complete true}
   :eligible_bytes 10
   :covered_eligible_bytes 9
   :uncovered_eligible_bytes 1
   :uncovered [{:work_id "fixture" :start 9 :end 10}]})

(defn- write-blob!
  [root locator value]
  (let [file (io/file root locator)]
    (json/write-deterministic-json-file! file value)
    {:locator locator
     :ref {:sha256 (hash/format-sha256 (hash/sha256-file file))
           :bytes (hash/byte-length file)
           :media_type "application/json"}}))

(def fixture-root
  (io/file "test/fixtures/parser-rq/source-accountability"))

(def fixture-names
  ["aggregate.json" "identity.json" "index.json" "manifest.json" "taxonomy.json"])

(defn- blob-ref
  [file]
  {:sha256 (hash/format-sha256 (hash/sha256-file file))
   :bytes (hash/byte-length file)
   :media_type "application/json"})

(defn- stage-committed-capture
  []
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "parser-rq-committed"
                       (make-array java.nio.file.attribute.FileAttribute 0)))]
    (doseq [name fixture-names]
      (java.nio.file.Files/copy (.toPath (io/file fixture-root name))
                                (.toPath (io/file root name))
                                (into-array java.nio.file.CopyOption [])))
    root))

(defn- capture
  [aggregate denominator]
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "parser-rq-source"
                       (make-array java.nio.file.attribute.FileAttribute 0)))
        aggregate-blob (write-blob! root "aggregate.json" aggregate)
        identity-blob (write-blob! root "identity.json" qualification-identity)
        taxonomy-file (io/file root "taxonomy.json")
        _ (spit taxonomy-file taxonomy-text)
        taxonomy-blob {:locator "taxonomy.json"
                       :ref {:sha256 taxonomy-hash
                             :bytes (hash/byte-length taxonomy-file)
                             :media_type "application/json"}}
        manifest {:blobs [aggregate-blob identity-blob taxonomy-blob]
                  :denominator denominator}]
    {:root root
     :store {:root (.getPath root)}
     :manifest manifest}))

(defn- with-capture
  [aggregate denominator f]
  (let [{:keys [root] :as captured} (capture aggregate denominator)]
    (try
      (f captured)
      (finally
        (doseq [file (reverse (file-seq root))]
          (.delete file))))))

(defn- derive-envelope
  [aggregate denominator]
  (with-capture aggregate denominator
    (fn [{:keys [store manifest]}]
      (rq-source/derive-source-span-envelope
       store manifest aggregate qualification-identity))))

(deftest byte-denominator-is-required-not-work-count
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "parser-rq-source"
                       (make-array java.nio.file.attribute.FileAttribute 0)))
        aggregate (aggregate-value)
        aggregate-file (io/file root "aggregate.json")
        _ (json/write-deterministic-json-file! aggregate-file aggregate)
        aggregate-blob {:locator "aggregate.json"
                        :ref {:sha256 (hash/format-sha256
                                       (hash/sha256-file aggregate-file))
                              :bytes (hash/byte-length aggregate-file)
                              :media_type "application/json"}}
        store {:root (.getPath root)}
        manifest {:blobs [aggregate-blob]
                  :denominator {:value 1 :unit "works"}}]
    (try
      (is (= :unavailable
             (:status (rq-source/derive-source-span-envelope
                       store manifest aggregate qualification-identity))))
      (finally
        (.delete aggregate-file)
        (.delete root)))))

(deftest exact-byte-ratio-is-derived-from-integer-counters
  (let [complete (assoc (aggregate-value)
                        :covered_eligible_bytes 10
                        :uncovered_eligible_bytes 0
                        :uncovered [])]
    (is (= {:value 1.0M :identity_ref identity-ref}
           (derive-envelope complete {:value 10 :unit "decoded_utf8_bytes"})))
    (is (= {:value 0.9M :identity_ref identity-ref}
           (derive-envelope (aggregate-value)
                            {:value 10 :unit "decoded_utf8_bytes"})))))

(deftest one-byte-deficit-cannot-round-to-a-pass
  (let [eligible 1000000
        aggregate (assoc (aggregate-value)
                         :eligible_bytes eligible
                         :covered_eligible_bytes (dec eligible)
                         :uncovered_eligible_bytes 1
                         :uncovered [{:work_id "fixture"
                                      :start (dec eligible)
                                      :end eligible}])
        envelope (derive-envelope aggregate {:value eligible
                                             :unit "decoded_utf8_bytes"})]
    (is (< (:value envelope) 1M))))

(deftest aggregate-must-be-schema-valid-complete-and-conservative
  (let [denominator {:value 10 :unit "decoded_utf8_bytes"}]
    (doseq [aggregate [(assoc (aggregate-value) :identity_ref taxonomy-hash)
                       (assoc (aggregate-value) :taxonomy_version "wrong")
                       (assoc (aggregate-value) :taxonomy_hash identity-ref)
                       (assoc (aggregate-value) :covered_eligible_bytes 8)
                       (assoc (aggregate-value) :uncovered [])
                       (assoc (aggregate-value)
                              :uncovered [{:work_id "fixture" :start 10 :end 9}])
                       (assoc (aggregate-value)
                              :uncovered [{:work_id "fixture" :start 8 :end 10}])
                       (assoc-in (aggregate-value) [:work_completeness :complete] false)
                       (dissoc (aggregate-value) :work_completeness)]]
      (is (= :unavailable
             (:status (derive-envelope aggregate denominator)))))))

(deftest aggregate-argument-must-match-reverified-blob-bytes
  (with-capture (aggregate-value) {:value 10 :unit "decoded_utf8_bytes"}
    (fn [{:keys [store manifest]}]
      (is (= :unavailable
             (:status (rq-source/derive-source-span-envelope
                       store manifest
                       (assoc (aggregate-value) :covered_eligible_bytes 8)
                       qualification-identity)))))))

(deftest aggregate-reread-is-rehashed-after-manifest-verification
  (with-capture (aggregate-value) {:value 10 :unit "decoded_utf8_bytes"}
    (fn [{:keys [store manifest]}]
      (let [verify capture/verify-manifest]
        (with-redefs [capture/verify-manifest
                      (fn [actual-store actual-manifest]
                        (let [result (verify actual-store actual-manifest)]
                          (spit (io/file (:root actual-store) "aggregate.json")
                                (json/write-deterministic-json-str
                                 (aggregate-value)))
                          result))]
          (is (= :unavailable
                 (:status (rq-source/derive-source-span-envelope
                           store manifest (aggregate-value)
                           qualification-identity)))))))))

(deftest manifest-blob-mismatch-is-unavailable
  (with-capture (aggregate-value) {:value 10 :unit "decoded_utf8_bytes"}
    (fn [{:keys [store manifest]}]
      (spit (io/file (:root store) "aggregate.json") "tampered")
      (is (= :unavailable
             (:status (rq-source/derive-source-span-envelope
                       store manifest (aggregate-value)
                       qualification-identity)))))))

(deftest committed-capture-derives-and-authenticated-mutation-is-rejected
  (let [root (stage-committed-capture)
        store {:root (.getPath root)}
        manifest (-> (io/file root "manifest.json") json/read-json-file walk/keywordize-keys)
        aggregate (-> (io/file root "aggregate.json") json/read-json-file walk/keywordize-keys)
        identity (-> (io/file root "identity.json") json/read-json-file walk/keywordize-keys)]
    (try
      (is (= {:value 1.0M :identity_ref identity-ref}
             (rq-source/derive-source-span-envelope store manifest aggregate identity)))
      (spit (io/file root "aggregate.json") "\n" :append true)
      (is (= :unavailable (:status (capture/verify-manifest store manifest))))
      (is (= :unavailable
             (:status (rq-source/derive-source-span-envelope
                       store manifest aggregate identity))))
      (finally
        (doseq [file (reverse (file-seq root))] (.delete file))))))

(deftest taxonomy-member-is-schema-validated-and-unambiguous
  (let [root (stage-committed-capture)
        store {:root (.getPath root)}
        manifest (-> (io/file root "manifest.json") json/read-json-file walk/keywordize-keys)
        aggregate-file (io/file root "aggregate.json")
        aggregate (-> aggregate-file json/read-json-file walk/keywordize-keys)
        identity (-> (io/file root "identity.json") json/read-json-file walk/keywordize-keys)
        taxonomy-file (io/file root "taxonomy.json")]
    (try
      (let [taxonomy-blob (first (filter #(= "taxonomy.json" (:locator %))
                                         (:blobs manifest)))
            taxonomy-copy (io/file root "taxonomy-copy.json")]
        (java.nio.file.Files/copy (.toPath taxonomy-file)
                                  (.toPath taxonomy-copy)
                                  (into-array java.nio.file.CopyOption []))
        (doseq [ambiguous-manifest
                [(update manifest :blobs conj taxonomy-blob)
                 (update manifest :blobs conj
                         (-> taxonomy-blob
                             (assoc :locator "taxonomy-copy.json")
                             (assoc-in [:ref :media_type]
                                       "application/vnd.example+json")))]]
          (is (= :unavailable
                 (:status (rq-source/derive-source-span-envelope
                           store ambiguous-manifest aggregate identity))))))
      (spit taxonomy-file
            "{\"coordinate_system\":\"decoded_utf8\",\"rules\":[],\"schema_version\":\"abc/parser-rq-ignored-regions/v1\",\"taxonomy_version\":\"parser-rq-ignored-regions-v1\"}")
      (let [taxonomy-ref (blob-ref taxonomy-file)
            changed-aggregate (assoc aggregate :taxonomy_hash (:sha256 taxonomy-ref))
            _ (spit aggregate-file (json/write-deterministic-json-str changed-aggregate))
            aggregate-ref (blob-ref aggregate-file)
            changed-manifest
            (update manifest :blobs
                    (fn [blobs]
                      (mapv (fn [blob]
                              (case (:locator blob)
                                "aggregate.json" (assoc blob :ref aggregate-ref)
                                "taxonomy.json" (assoc blob :ref taxonomy-ref)
                                blob))
                            blobs)))]
        (is (= :ok (:status (capture/verify-manifest store changed-manifest))))
        (is (= :unavailable
               (:status (rq-source/derive-source-span-envelope
                         store changed-manifest changed-aggregate identity)))))
      (finally
        (doseq [file (reverse (file-seq root))] (.delete file))))))
