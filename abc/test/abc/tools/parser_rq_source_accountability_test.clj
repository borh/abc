(ns abc.tools.parser-rq-source-accountability-test
  (:require [abc.tools.hash :as hash]
            [abc.tools.json :as json]
            [abc.tools.parser-release-qualification :as qualification]
            [abc.tools.parser-rq-capture :as capture]
            [abc.tools.parser-rq-source-accountability :as rq-source]
            [abc.tools.parser-rq-source-recognition :as source-recognition]
            [clojure.java.io :as io]
            [clojure.string :as string]
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
   :instrument_versions {:source_accountability "parser-rq-source-accountability-v1"}
   :instrument_policy_hashes
   {:source_recognition (str "sha256:" (apply str (repeat 64 "7")))}})

(def identity-ref
  (qualification/qualification-identity-ref qualification-identity))

(def recognition-identity
  (assoc-in qualification-identity
            [:instrument_versions :source_span_coverage]
            "parser-rq-source-recognition-v1"))

(def recognition-identity-ref
  (qualification/qualification-identity-ref recognition-identity))

(deftest production-recognition-identity-needs-only-the-predicate-instrument
  (let [production-identity
        (update recognition-identity :instrument_versions dissoc :source_accountability)]
    (is (#'rq-source/recognition-identity-valid? production-identity))))

(def production-recognition-fixture-root
  (io/file "test/fixtures/parser-rq/source-recognition-capture"))

(def diagnostic-gap-capture-fixture-root
  (io/file "test/fixtures/parser-rq/diagnostic-gap-capture"))

(defn- read-keyword-json
  [file]
  (-> file slurp json/read-json-str walk/keywordize-keys))

(defn- copy-tree!
  [source destination]
  (doseq [file (file-seq source)
          :let [relative (.relativize (.toPath source) (.toPath file))
                target (.toFile (.resolve (.toPath destination) relative))]]
    (if (.isDirectory file)
      (.mkdirs target)
      (do
        (.mkdirs (.getParentFile target))
        (java.nio.file.Files/copy (.toPath file) (.toPath target)
                                  (into-array java.nio.file.CopyOption []))))))

(defn- staged-production-recognition
  []
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "parser-rq-recognition-production"
                       (make-array java.nio.file.attribute.FileAttribute 0)))]
    (copy-tree! production-recognition-fixture-root root)
    {:root root
     :store {:root (.getPath (io/file root "store"))}
     :manifest (read-keyword-json (io/file root "manifest.json"))
     :identity (read-keyword-json (io/file root "store/identity.json"))
     :aggregate (read-keyword-json (io/file root "store/recognition-aggregate.json"))}))

(defn- staged-diagnostic-gap-capture
  []
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "parser-rq-diagnostic-gap-production"
                       (make-array java.nio.file.attribute.FileAttribute 0)))]
    (copy-tree! diagnostic-gap-capture-fixture-root root)
    {:root root
     :store {:root (.getPath (io/file root "store"))}
     :manifest (read-keyword-json (io/file root "manifest.json"))
     :identity (read-keyword-json (io/file root "store/identity.json"))}))

(defn- delete-tree!
  [root]
  (doseq [file (reverse (file-seq root))] (.delete file)))

(defn- blob-ref-for
  [file media-type]
  {:sha256 (hash/format-sha256 (hash/sha256-file file))
   :bytes (hash/byte-length file)
   :media_type media-type})

(defn- replace-manifest-member
  [manifest locator ref]
  (update manifest :blobs
          (fn [blobs]
            (mapv #(if (= locator (:locator %)) (assoc % :ref ref) %) blobs))))

(defn- reseal-existing-json!
  [root manifest locator value]
  (let [file (io/file root "store" locator)]
    (json/write-deterministic-json-file! file value)
    (replace-manifest-member manifest locator
                             (blob-ref-for file "application/json"))))

(defn- publish-json!
  [root manifest value]
  (let [text (json/write-deterministic-json-str value)
        sha256 (hash/format-sha256 (hash/sha256-string text))
        digest (subs sha256 7)
        locator (str "sha256/" (subs digest 0 2) "/" digest ".json")
        file (io/file root "store" locator)
        _ (.mkdirs (.getParentFile file))
        _ (spit file text)
        member {:locator locator :ref (blob-ref-for file "application/json")}]
    {:manifest (update manifest :blobs conj member)
     :member member}))

(defn- generation-for
  [root capture-ref]
  (->> (file-seq (io/file root "store"))
       (filter #(.isFile %))
       (filter #(string/ends-with? (.getName %) ".json"))
       (keep #(try (read-keyword-json %) (catch Exception _ nil)))
       (filter #(= capture-ref (:generation_ref %)))
       first))

(defn- reseal-chain!
  [{:keys [root manifest aggregate identity] :as staged}
   {:keys [ledger generation generation-ref record membership index corpus-ref aggregate-value]
    entry-mutation :entry}]
  (let [index-value (read-keyword-json (io/file root "store/recognition-index.json"))
        entry (first (:records index-value))
        record-value (read-keyword-json (io/file root "store" (:locator entry)))
        generation-value (generation-for root (:capture_generation_ref record-value))
        ledger-value (read-keyword-json
                      (io/file root "store" (get-in generation-value
                                                    [:members :classified_source_ledger
                                                     :artifact_ref])))
        ledger-value (if ledger (ledger ledger-value) ledger-value)
        ledger-published (publish-json! root manifest ledger-value)
        manifest (:manifest ledger-published)
        generation-value (assoc-in generation-value
                                   [:members :classified_source_ledger]
                                   {:artifact_ref (get-in ledger-published [:member :locator])
                                    :value_hash (get-in ledger-published
                                                        [:member :ref :sha256])})
        generation-value (if generation (generation generation-value) generation-value)
        generation-value (assoc generation-value :generation_ref
                                (#'rq-source/projected-ref generation-value :generation_ref))
        generation-value (if generation-ref
                           (update generation-value :generation_ref generation-ref)
                           generation-value)
        generation-published (publish-json! root manifest generation-value)
        manifest (:manifest generation-published)
        record-value (-> record-value
                         (assoc :capture_generation_ref (:generation_ref generation-value)
                                :ledger {:locator (get-in ledger-published [:member :locator])
                                         :sha256 (get-in ledger-published [:member :ref :sha256])
                                         :bytes (get-in ledger-published [:member :ref :bytes])
                                         :media_type "application/json"})
                         (cond-> record (record)))
        record-published (publish-json! root manifest record-value)
        manifest (:manifest record-published)
        new-entry (assoc entry
                         :capture_generation_ref (:generation_ref generation-value)
                         :locator (get-in record-published [:member :locator])
                         :sha256 (get-in record-published [:member :ref :sha256])
                         :bytes (get-in record-published [:member :ref :bytes]))
        new-entry (if entry-mutation (entry-mutation new-entry) new-entry)
        index-value (assoc-in index-value [:records 0] new-entry)
        membership-value (read-keyword-json
                          (io/file root "store"
                                   (#'rq-source/content-locator
                                    (:membership_ref index-value) "json")))
        membership-published (when membership
                               (publish-json! root manifest
                                              (membership membership-value)))
        manifest (if membership-published
                   (:manifest membership-published)
                   manifest)
        index-value (if membership-published
                      (assoc index-value :membership_ref
                             (get-in membership-published [:member :ref :sha256]))
                      index-value)
        index-value (if index (index index-value) index-value)
        index-value (assoc index-value :corpus_generation_ref
                           (source-recognition/corpus-generation-ref index-value))
        index-value (if corpus-ref
                      (update index-value :corpus_generation_ref corpus-ref)
                      index-value)
        aggregate (merge aggregate
                         (select-keys index-value
                                      [:qualification_identity_ref :corpus_generation_ref
                                       :corpus_generation_algorithm :policy_hash
                                       :membership_ref :coordinate_system]))
        aggregate (if aggregate-value (aggregate-value aggregate) aggregate)
        manifest (reseal-existing-json! root manifest "recognition-index.json" index-value)
        manifest (reseal-existing-json! root manifest "recognition-aggregate.json" aggregate)]
    (assoc staged :manifest manifest :aggregate aggregate :identity identity)))

(defn- unavailable-recognition?
  [{:keys [store manifest aggregate identity]}]
  (= :unavailable
     (:status (rq-source/derive-source-recognition-envelope
               store manifest aggregate identity))))

(deftest qualification-identity-ref-matches-rust-golden
  (is (= "sha256:45b662893c840cdb68647baa9c2af48fdbc7dae14cb064f903d35c939e220088"
         identity-ref)))

(deftest production-recognition-fixture-drives-the-real-envelope-and-gate
  (let [{:keys [root store manifest identity aggregate]}
        (staged-production-recognition)]
    (try
      (let [envelope (rq-source/derive-source-recognition-envelope
                      store manifest aggregate identity)]
        (is (= recognition-identity-ref (:identity_ref envelope)))
        (is (= 0.461M (:value envelope)))
        (is (= :not-qualified
               (qualification/gate-status
                true
                [(qualification/evaluate-predicate
                  {:id :source_span_coverage :operator :eq :threshold 1.0}
                  {:source_span_coverage envelope})]))))
      (finally (delete-tree! root)))))

(deftest recognition-derivation-rejects-a-symlinked-manifest-destination
  (let [{:keys [root store manifest identity aggregate]}
        (staged-production-recognition)
        identity-path (.toPath (io/file root "store/identity.json"))
        real-path (.toPath (io/file root "store/identity-real.json"))]
    (try
      (java.nio.file.Files/move identity-path real-path
                                (into-array java.nio.file.CopyOption []))
      (java.nio.file.Files/createSymbolicLink
       identity-path real-path (make-array java.nio.file.attribute.FileAttribute 0))
      (is (= :unavailable
             (:status (rq-source/derive-source-recognition-envelope
                       store manifest aggregate identity))))
      (finally (java.nio.file.Files/deleteIfExists identity-path)
               (java.nio.file.Files/deleteIfExists real-path)
               (delete-tree! root)))))

(deftest production-recognition-fixture-identity-edges-fail-closed
  (doseq [[label mutation]
          [["ledger policy authority"
            {:ledger #(assoc % :policy_hash (str "sha256:" (apply str (repeat 64 "8"))))}]
           ["ledger schema authority"
            {:ledger #(assoc % :ledger_schema_hash
                             (str "sha256:" (apply str (repeat 64 "8"))))}]
           ["ledger qualification"
            {:ledger #(assoc % :qualification_identity_ref
                             (str "sha256:" (apply str (repeat 64 "8"))))}]
           ["ledger coordinate"
            {:ledger #(assoc % :coordinate_system "body_relative_utf8")}]
           ["ledger original-source work identity"
            {:ledger #(assoc-in % [:original_source :value_hash]
                                (str "sha256:" (apply str (repeat 64 "8"))))}]
           ["ledger original-source locator"
            {:ledger #(assoc-in % [:original_source :artifact_ref]
                                "source/sha256:8888888888888888888888888888888888888888888888888888888888888888")}]
           ["ledger target identity"
            {:ledger #(update % :entries
                              (fn [entries]
                                (mapv (fn [entry]
                                        (if (:target_identity entry)
                                          (assoc-in entry [:target_identity :value_hash]
                                                    (str "sha256:"
                                                         (apply str (repeat 64 "8"))))
                                          entry))
                                      entries)))}]
           ["capture decoded-source hash"
            {:generation #(assoc-in % [:members :decoded_source :value_hash]
                                    (str "sha256:" (apply str (repeat 64 "8"))))}]
           ["capture decoded-source locator"
            {:generation #(assoc-in % [:members :decoded_source :artifact_ref]
                                    "sha256/88/8888888888888888888888888888888888888888888888888888888888888888.txt")}]
           ["capture generation work identity"
            {:generation #(assoc % :work_id
                                 (str "sha256:" (apply str (repeat 64 "8"))))}]
           ["capture generation ref"
            {:generation-ref (constantly
                              (str "sha256:" (apply str (repeat 64 "8"))))}]
           ["capture generation unknown top-level field"
            {:generation #(assoc % :attacker_field "resealed")}]
           ["capture generation unknown member"
            {:generation #(assoc-in % [:members :attacker_member]
                                    (get-in % [:members :decoded_source]))}]
           ["record qualification identity"
            {:record #(assoc % :qualification_identity_ref
                             (str "sha256:" (apply str (repeat 64 "8"))))}]
           ["record parser instrument"
            {:record #(assoc % :instrument_version "parser-rq-source-recognition-v2")}]
           ["record coordinate"
            {:record #(assoc % :coordinate_system "body_relative_utf8")}]
           ["record work identity"
            {:record #(assoc % :work_id
                             (str "sha256:" (apply str (repeat 64 "8"))))}]
           ["record totals"
            {:record #(update % :recognized_bytes inc)}]
           ["record ledger identity"
            {:record #(assoc-in % [:ledger :sha256]
                                (str "sha256:" (apply str (repeat 64 "8"))))}]
           ["record locator"
            {:entry #(assoc % :locator
                            "sha256/88/8888888888888888888888888888888888888888888888888888888888888888.json")}]
           ["record hash"
            {:entry #(assoc % :sha256
                            (str "sha256:" (apply str (repeat 64 "8"))))}]
           ["corpus algorithm"
            {:index #(assoc % :corpus_generation_algorithm "sha256-other-v1")}]
           ["corpus membership"
            {:index #(assoc % :membership_ref
                            (str "sha256:" (apply str (repeat 64 "8"))))}]
           ["membership wrong expected count"
            {:membership #(update % :expected_work_count inc)}]
           ["membership duplicate work identity"
            {:membership #(assoc-in % [:records 1 :work_id]
                                    (get-in % [:records 0 :work_id]))}]
           ["corpus capture mapping"
            {:index #(assoc-in % [:records 0 :capture_generation_ref]
                               (str "sha256:" (apply str (repeat 64 "8"))))}]
           ["corpus work membership"
            {:index #(assoc % :expected_work_ids
                            (vec (reverse (:expected_work_ids %))))}]
           ["corpus generation ref"
            {:corpus-ref (constantly
                          (str "sha256:" (apply str (repeat 64 "8"))))}]
           ["aggregate totals"
            {:aggregate-value #(update % :recognized_bytes inc)}]
           ["aggregate membership ref"
            {:aggregate-value #(assoc % :membership_ref
                                      (str "sha256:" (apply str (repeat 64 "8"))))}]
           ["aggregate corpus ref"
            {:aggregate-value #(assoc % :corpus_generation_ref
                                      (str "sha256:"
                                           (apply str (repeat 64 "8"))))}]]]
    (let [{:keys [root store manifest identity aggregate]}
          (staged-production-recognition)]
      (try
        (is (unavailable-recognition?
             (reseal-chain! {:root root :store store :manifest manifest
                             :identity identity :aggregate aggregate}
                            mutation))
            label)
        (finally (delete-tree! root)))))
  (let [{:keys [root manifest] :as staged} (staged-production-recognition)]
    (try
      (let [index (read-keyword-json (io/file root "store/recognition-index.json"))
            generation (generation-for root
                                       (:capture_generation_ref (first (:records index))))
            locator (get-in generation [:members :decoded_source :artifact_ref])
            file (io/file root "store" locator)
            _ (spit file (str (slurp file) "attacker-byte"))
            manifest (replace-manifest-member
                      manifest locator (blob-ref-for file "text/plain"))]
        (is (unavailable-recognition? (assoc staged :manifest manifest))
            "source blob bytes, with its P0 binding resealed"))
      (finally (delete-tree! root))))
  (let [{:keys [root manifest identity] :as staged} (staged-production-recognition)]
    (try
      (let [identity (assoc identity :parser_git_rev "attacker-revision")
            manifest (reseal-existing-json! root manifest "identity.json" identity)]
        (is (unavailable-recognition?
             (assoc staged :manifest manifest :identity identity))
            "P0 qualification identity binding"))
      (finally (delete-tree! root))))
  (doseq [[label path]
          [["P0 locator binding" [:blobs 0 :locator]]
           ["P0 qualification artifact hash" [:blobs 0 :ref :sha256]]]]
    (let [{:keys [root] :as staged} (staged-production-recognition)]
      (try
        (is (unavailable-recognition?
             (assoc staged :manifest
                    (assoc-in (:manifest staged) path
                              (if (= :locator (last path))
                                "../outside.json"
                                (str "sha256:" (apply str (repeat 64 "8")))))))
            label)
        (finally (delete-tree! root)))))
  (let [{:keys [root store identity aggregate] :as staged}
        (staged-production-recognition)]
    (try
      (let [manifest (assoc-in (:manifest staged) [:blobs 0 :locator]
                               "../outside.json")]
        (is (= :unavailable
               (:status (rq-source/derive-source-recognition-envelope
                         store manifest aggregate identity)))
            "P0 manifest binding"))
      (finally (delete-tree! root)))))

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
    (.mkdirs (.getParentFile file))
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

(deftest aggregate-consumption-uses-the-bytes-authenticated-by-manifest-verification
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
          (is (= identity-ref
                 (:identity_ref (rq-source/derive-source-span-envelope
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

(defn- recognition-values
  [recognized accounted eligible]
  (let [work {:schema_version "abc/parser-rq-source-recognition-work/v1"
              :qualification_identity_ref recognition-identity-ref
              :capture_generation_ref (str "sha256:" (apply str (repeat 64 "2")))
              :policy_hash (str "sha256:" (apply str (repeat 64 "3")))
              :instrument_version "parser-rq-source-recognition-v1"
              :work_id "fixture-work"
              :coordinate_system "decoded_utf8"
              :ledger {:sha256 (str "sha256:" (apply str (repeat 64 "4")))
                       :bytes 1
                       :media_type "application/json"
                       :locator "unused-ledger.json"}
              :status "ok"
              :eligible_bytes eligible
              :recognized_bytes recognized
              :accounted_bytes accounted
              :semantic_gap_bytes (- eligible recognized)
              :unaccounted_bytes (- eligible accounted)
              :recognized (if (pos? recognized) [{:start 0 :end recognized}] [])
              :accounted (if (pos? accounted) [{:start 0 :end accounted}] [])
              :semantic_gaps (if (< recognized eligible)
                               [{:start recognized :end eligible}]
                               [])
              :unaccounted (if (< accounted eligible)
                             [{:start accounted :end eligible}]
                             [])}
        policy-hash (:policy_hash work)
        membership-ref (str "sha256:" (apply str (repeat 64 "5")))
        base-index {:schema_version "abc/parser-rq-source-recognition-index/v1"
                    :qualification_identity_ref recognition-identity-ref
                    :corpus_generation_ref capture/sha256-schema
                    :corpus_generation_algorithm "sha256-rfc8785-safe-integer-domain-abc-v1"
                    :policy_hash policy-hash
                    :membership_ref membership-ref
                    :coordinate_system "decoded_utf8"
                    :status "ok"
                    :expected_work_ids ["fixture-work"]
                    :expected_work_count 1
                    :record_count 1
                    :records []
                    :errors []}]
    {:work work
     :base-index base-index
     :aggregate-base {:schema_version "abc/parser-rq-source-recognition-aggregate/v1"
                      :qualification_identity_ref recognition-identity-ref
                      :corpus_generation_algorithm "sha256-rfc8785-safe-integer-domain-abc-v1"
                      :policy_hash policy-hash
                      :membership_ref membership-ref
                      :coordinate_system "decoded_utf8"
                      :status "ok"
                      :work_completeness {:expected 1 :observed 1 :complete true}
                      :eligible_bytes eligible
                      :recognized_bytes recognized
                      :accounted_bytes accounted
                      :semantic_gap_bytes (- eligible recognized)
                      :unaccounted_bytes (- eligible accounted)
                      :semantic_gaps (mapv #(assoc % :work_id "fixture-work")
                                           (:semantic_gaps work))
                      :unaccounted (mapv #(assoc % :work_id "fixture-work")
                                         (:unaccounted work))}}))

(defn- recognition-capture
  [recognized accounted eligible]
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "parser-rq-recognition"
                       (make-array java.nio.file.attribute.FileAttribute 0)))
        {:keys [work base-index aggregate-base]}
        (recognition-values recognized accounted eligible)
        work-blob (write-blob! root "records/fixture-work/recognition.json" work)
        entry (merge {:work_id "fixture-work"
                      :capture_generation_ref (:capture_generation_ref work)}
                     (:ref work-blob)
                     {:locator (:locator work-blob)})
        index-with-entry (assoc base-index :records [entry])
        index (assoc index-with-entry :corpus_generation_ref
                     (source-recognition/corpus-generation-ref index-with-entry))
        aggregate (assoc aggregate-base :corpus_generation_ref
                         (:corpus_generation_ref index))
        aggregate-blob (write-blob! root "recognition-aggregate.json" aggregate)
        index-blob (write-blob! root "recognition-index.json" index)
        identity-blob (write-blob! root "identity.json" recognition-identity)
        manifest {:blobs [aggregate-blob index-blob identity-blob work-blob]
                  :denominator {:value eligible :unit "decoded_utf8_bytes"}}]
    {:root root
     :store {:root (.getPath root)}
     :manifest manifest
     :aggregate aggregate
     :index index}))

(defn- with-recognition-capture
  [recognized accounted eligible f]
  (let [{:keys [root] :as captured}
        (recognition-capture recognized accounted eligible)]
    (try
      (f captured)
      (finally
        (doseq [file (reverse (file-seq root))]
          (.delete file))))))

(defn- replace-manifest-blob
  [manifest blob]
  (update manifest :blobs
          (fn [blobs]
            (mapv #(if (= (:locator blob) (:locator %)) blob %) blobs))))

(defn- reseal-recognition-work!
  [{:keys [root manifest index aggregate]} work aggregate-f]
  (let [work-blob (write-blob! root "records/fixture-work/recognition.json" work)
        entry (merge {:work_id "fixture-work"
                      :capture_generation_ref (:capture_generation_ref work)}
                     (:ref work-blob)
                     {:locator (:locator work-blob)})
        index-with-entry (assoc index :records [entry])
        new-index (assoc index-with-entry :corpus_generation_ref
                         (source-recognition/corpus-generation-ref index-with-entry))
        new-aggregate (-> aggregate
                          (assoc :corpus_generation_ref
                                 (:corpus_generation_ref new-index))
                          aggregate-f)
        index-blob (write-blob! root "recognition-index.json" new-index)
        aggregate-blob (write-blob! root "recognition-aggregate.json" new-aggregate)]
    {:manifest (-> manifest
                   (replace-manifest-blob work-blob)
                   (replace-manifest-blob index-blob)
                   (replace-manifest-blob aggregate-blob))
     :aggregate new-aggregate}))

(deftest semantic-recognition-not-accountability-drives-r1
  (is (= 0.9M (#'rq-source/exact-display-ratio 9 10))))

(deftest complete-and-empty-semantic-domains-pass-exactly
  (is (= 1.0M (#'rq-source/exact-display-ratio 10 10)))
  (is (= 1.0M (if (zero? 0)
                1.0M
                (#'rq-source/exact-display-ratio 0 0)))))

(deftest recognition-unavailability-and-resealed-summaries-fail-closed
  (with-recognition-capture 9 10 10
    (fn [{:keys [store manifest aggregate]}]
      (doseq [candidate [(assoc aggregate :status "unavailable")
                         (assoc aggregate :recognized_bytes 10
                                :semantic_gap_bytes 0
                                :semantic_gaps [])]]
        (let [aggregate-blob (write-blob! (:root store)
                                          "recognition-aggregate.json"
                                          candidate)
              resealed (update manifest :blobs
                               (fn [blobs]
                                 (mapv #(if (= "recognition-aggregate.json"
                                               (:locator %))
                                          aggregate-blob
                                          %)
                                       blobs)))]
          (is (= :unavailable
                 (:status (rq-source/derive-source-recognition-envelope
                           store resealed candidate recognition-identity)))))))))

(deftest malformed-or-missing-recognition-evidence-cannot-fall-back-to-node-spans
  (with-recognition-capture 9 10 10
    (fn [{:keys [store manifest aggregate]}]
      (let [malformed (update manifest :blobs
                              (fn [blobs]
                                (mapv #(if (= "recognition-index.json" (:locator %))
                                         (assoc-in % [:ref :bytes]
                                                   (inc (get-in % [:ref :bytes])))
                                         %)
                                      blobs)))]
        (is (= :unavailable
               (:status (rq-source/derive-source-recognition-envelope
                         store malformed aggregate recognition-identity)))))))
  (let [legacy-envelope (derive-envelope (aggregate-value)
                                         {:value 10 :unit "decoded_utf8_bytes"})]
    (with-capture (aggregate-value) {:value 10 :unit "decoded_utf8_bytes"}
      (fn [{:keys [store manifest]}]
        (is (= {:value :instrument-missing
                :identity_ref recognition-identity-ref}
               (rq-source/derive-source-recognition-envelope
                store manifest nil recognition-identity)))))
    (is (= 0.9M (:value legacy-envelope)))))

(deftest authenticated-recognition-declaration-distinguishes-history-from-deletion
  (with-recognition-capture 9 10 10
    (fn [{:keys [store manifest aggregate]}]
      (doseq [removed-locators [#{"recognition-aggregate.json"}
                                #{"recognition-index.json"}
                                #{"recognition-aggregate.json"
                                  "recognition-index.json"}]]
        (let [changed (update manifest :blobs
                              #(filterv (comp not removed-locators :locator) %))]
          (is (= :unavailable
                 (:status (rq-source/derive-source-recognition-envelope
                           store changed aggregate recognition-identity))))))
      (let [changed (update manifest :blobs
                            (fn [blobs]
                              (mapv #(if (= "recognition-index.json" (:locator %))
                                       (dissoc % :locator)
                                       %)
                                    blobs)))]
        (is (= :unavailable
               (:status (rq-source/derive-source-recognition-envelope
                         store changed aggregate recognition-identity))))))))

(deftest recognition-projections-must-be-canonical-nested-sets
  (with-recognition-capture 5 5 10
    (fn [{:keys [store] :as captured}]
      (let [{:keys [work]} (recognition-values 5 5 10)
            disjoint (assoc work
                            :recognized [{:start 5 :end 10}]
                            :accounted [{:start 0 :end 5}]
                            :semantic_gaps [{:start 0 :end 5}]
                            :unaccounted [{:start 5 :end 10}])
            {:keys [manifest aggregate]}
            (reseal-recognition-work!
             captured disjoint
             #(assoc % :semantic_gaps [{:work_id "fixture-work"
                                        :start 0 :end 5}]))]
        (is (= :unavailable
               (:status (rq-source/derive-source-recognition-envelope
                         store manifest aggregate recognition-identity)))))))
  (doseq [recognized [[{:start 0 :end 2} {:start 2 :end 5}]
                      [{:start 0 :end 3} {:start 2 :end 4}]]]
    (with-recognition-capture 5 5 10
      (fn [{:keys [store] :as captured}]
        (let [{:keys [work]} (recognition-values 5 5 10)
              mutated (assoc work :recognized recognized)
              {:keys [manifest aggregate]}
              (reseal-recognition-work! captured mutated identity)]
          (is (= :unavailable
                 (:status (rq-source/derive-source-recognition-envelope
                           store manifest aggregate recognition-identity)))))))))

(deftest ambiguous-recognition-binding-is-unavailable-not-instrument-missing
  (with-recognition-capture 9 10 10
    (fn [{:keys [store manifest aggregate]}]
      (let [aggregate-member (first (filter #(= "recognition-aggregate.json"
                                                (:locator %))
                                            (:blobs manifest)))
            ambiguous (update manifest :blobs conj aggregate-member)]
        (is (= :unavailable
               (:status (rq-source/derive-source-recognition-envelope
                         store ambiguous aggregate recognition-identity))))))))

(deftest recognition-ratio-keeps-a-one-byte-deficit-visible
  (let [value (#'rq-source/exact-display-ratio 999999 1000000)]
    (is (= 0.9999990M value))
    (is (< value 1M))))

(defn- diagnostic-gap-aggregate
  []
  {:status "ok"
   :qualification_identity_ref recognition-identity-ref
   :corpus_generation_ref (str "sha256:" (apply str (repeat 64 "a")))
   :policy_hash (str "sha256:" (apply str (repeat 64 "b")))
   :expected_work_ids ["work-a" "work-b"]
   :observed_work_ids ["work-a" "work-b"]
   :authorized_bytes 3
   :silent_bytes 1
   :semantic_gap_bytes 4
   :silent_drop_count 1})

(deftest caller-supplied-diagnostic-gap-maps-cannot-derive-r2
  (let [index {:qualification_identity_ref recognition-identity-ref
               :corpus_generation_ref (str "sha256:" (apply str (repeat 64 "a")))
               :expected_work_ids ["work-a" "work-b"]}
        r1 {:qualification_identity_ref recognition-identity-ref
            :corpus_generation_ref (:corpus_generation_ref index)
            :semantic_gap_bytes 4}
        before [index r1]
        envelope (rq-source/silent-drops-envelope
                  recognition-identity (diagnostic-gap-aggregate) index r1)]
    (is (= :unavailable (:status envelope)))
    (is (= before [index r1]) "R2 derivation leaves R1 evidence byte-values unchanged")))

(deftest empty-diagnostics-do-not-bypass-whole-source-utf8-validation
  (is (nil? (#'rq-source/strict-utf8 (byte-array [(unchecked-byte 0xff)]))))
  (is (= "" (#'rq-source/strict-utf8 (byte-array 0)))))

(deftest disposition-audit-counts-are-derived-not-merely-summed
  (let [honest {:diagnostic_count 2 :authorizing_diagnostic_count 1
                :observe_only_diagnostic_count 1}
        swapped {:diagnostic_count 2 :authorizing_diagnostic_count 0
                 :observe_only_diagnostic_count 2}
        dispositions ["authorize_exact_span" "observe_only"]]
    (is (#'rq-source/disposition-counts-coherent? honest dispositions))
    (is (not (#'rq-source/disposition-counts-coherent? swapped dispositions)))))

(deftest diagnostic-gap-aggregate-fails-closed-without-mutating-r1
  (let [index {:qualification_identity_ref recognition-identity-ref
               :corpus_generation_ref (str "sha256:" (apply str (repeat 64 "a")))
               :expected_work_ids ["work-a" "work-b"]}
        r1 {:qualification_identity_ref recognition-identity-ref
            :corpus_generation_ref (:corpus_generation_ref index)
            :semantic_gap_bytes 4}
        base (diagnostic-gap-aggregate)]
    (doseq [candidate [(assoc base :status "unavailable")
                       (assoc base :observed_work_ids ["work-a"])
                       (assoc base :corpus_generation_ref
                              (str "sha256:" (apply str (repeat 64 "c"))))
                       (assoc base :silent_bytes 0)
                       (assoc base :silent_drop_count -1)]]
      (is (= :unavailable
             (:status (rq-source/silent-drops-envelope
                       recognition-identity candidate index r1)))))))

(deftest detached-diagnostic-gap-artifacts-cannot-derive-r2-even-when-resealed
  (let [{:keys [root store manifest identity]} (staged-production-recognition)]
    (try
      (let [index (read-keyword-json (io/file root "store/recognition-index.json"))
            r1 (read-keyword-json (io/file root "store/recognition-aggregate.json"))
            raw {:schemaVersion 3 :data []}
            raw-published (publish-json! root manifest raw)
            policy (read-keyword-json (io/file "data/parser-rq-ab-aozora-diagnostic-gap-v1.json"))
            policy-published (publish-json! root (:manifest raw-published) policy)
            policy-artifact-hash (get-in policy-published [:member :ref :sha256])
            policy-artifact-bytes (get-in policy-published [:member :ref :bytes])
            policy-hash (:policy_hash policy)
            raw-hash (get-in raw-published [:member :ref :sha256])
            raw-bytes (get-in raw-published [:member :ref :bytes])
            state (reduce
                   (fn [{:keys [manifest results]} entry]
                     (let [record (read-keyword-json (io/file root "store" (:locator entry)))
                           result {:schema_version "abc/parser-rq-diagnostic-gap-result/v1"
                                   :status "ok"
                                   :work_id (:work_id entry)
                                   :capture_generation_ref (:capture_generation_ref entry)
                                   :qualification_identity_ref recognition-identity-ref
                                   :policy_hash policy-hash
                                   :source_recognition_evidence
                                   {:relation "partitions-semantic-gaps-of"
                                    :artifact_ref (select-keys entry [:sha256 :bytes :media_type :locator])
                                    :value_hash (:sha256 entry)
                                    :qualification_identity_ref recognition-identity-ref
                                    :capture_generation_ref (:capture_generation_ref entry)
                                    :work_id (:work_id entry)}
                                   :diagnostic_authorization_evidence
                                   {:decoded_source_hash (:work_id entry)
                                    :raw_diagnostics_hash raw-hash
                                    :raw_diagnostics_bytes raw-bytes
                                    :policy_hash policy-hash
                                    :policy_artifact_hash policy-artifact-hash
                                    :policy_artifact_bytes policy-artifact-bytes
                                    :source_recognition_hash (:sha256 entry)}
                                   :authorized_intervals []
                                   :silent_intervals (:semantic_gaps record)
                                   :authorized_bytes 0
                                   :silent_bytes (:semantic_gap_bytes record)
                                   :silent_drop_count (count (:semantic_gaps record))
                                   :diagnostic_count 0
                                   :authorizing_diagnostic_count 0
                                   :observe_only_diagnostic_count 0
                                   :vacuous true}
                           published (publish-json! root manifest result)]
                       {:manifest (:manifest published) :results (conj results result)}))
                   {:manifest (:manifest policy-published) :results []}
                   (:records index))
            aggregate {:schema_version "abc/parser-rq-diagnostic-gap-aggregate/v1"
                       :status "ok"
                       :qualification_identity_ref recognition-identity-ref
                       :corpus_generation_ref (:corpus_generation_ref index)
                       :policy_hash policy-hash
                       :policy_artifact_hash policy-artifact-hash
                       :expected_work_ids (:expected_work_ids index)
                       :observed_work_ids (:expected_work_ids index)
                       :authorized_bytes 0
                       :silent_bytes (:semantic_gap_bytes r1)
                       :silent_drop_count (reduce + (map :silent_drop_count (:results state)))
                       :diagnostic_count 0
                       :authorizing_diagnostic_count 0
                       :observe_only_diagnostic_count 0
                       :authorized_interval_count 0
                       :vacuous true}
            aggregate-file (io/file root "store/diagnostic-gap-aggregate.json")
            _ (json/write-deterministic-json-file! aggregate-file aggregate)
            aggregate-member {:locator "diagnostic-gap-aggregate.json"
                              :ref (blob-ref-for aggregate-file "application/json")}
            manifest (update (:manifest state) :blobs conj aggregate-member)
            last-result-member (some #(when (= (last (:results state))
                                               (#'rq-source/authenticated-json-value store %)) %)
                                     (:blobs manifest))]
        (is (= :unavailable
               (:status (rq-source/silent-drops-envelope store manifest identity)))
            "a raw artifact absent from each asserted generation is detached evidence")
        (let [original (last (:results state))
              observe-raw {:schemaVersion 3
                           :data [{:kind "unclosed_bracket" :code "unclosed-bracket"
                                   :severity "error" :source "source"
                                   :span {:start 0 :end 3}}]}
              raw-published (publish-json! root manifest observe-raw)
              observed-result (-> original
                                  (assoc-in [:diagnostic_authorization_evidence :raw_diagnostics_hash]
                                            (get-in raw-published [:member :ref :sha256]))
                                  (assoc-in [:diagnostic_authorization_evidence :raw_diagnostics_bytes]
                                            (get-in raw-published [:member :ref :bytes]))
                                  (assoc :diagnostic_count 1
                                         :authorizing_diagnostic_count 0
                                         :observe_only_diagnostic_count 1
                                         :vacuous false))
              result-member last-result-member
              result-resealed (reseal-existing-json! root (:manifest raw-published)
                                                     (:locator result-member) observed-result)
              observed-aggregate (assoc aggregate :diagnostic_count 1
                                        :observe_only_diagnostic_count 1 :vacuous false)
              observed-manifest (reseal-existing-json! root result-resealed
                                                       "diagnostic-gap-aggregate.json"
                                                       observed-aggregate)]
          (is (= :unavailable
                 (:status (rq-source/silent-drops-envelope
                           store observed-manifest identity))))
          (let [swapped-result (assoc observed-result :authorizing_diagnostic_count 1
                                      :observe_only_diagnostic_count 0)
                swapped-result-manifest (reseal-existing-json!
                                         root observed-manifest (:locator result-member)
                                         swapped-result)
                swapped-aggregate (assoc observed-aggregate
                                         :authorizing_diagnostic_count 1
                                         :observe_only_diagnostic_count 0)
                swapped-manifest (reseal-existing-json!
                                  root swapped-result-manifest
                                  "diagnostic-gap-aggregate.json" swapped-aggregate)]
            (is (= :unavailable
                   (:status (rq-source/silent-drops-envelope store swapped-manifest identity)))
                "resealed disposition counts are derived from diagnostics")))
        (let [original (last (:results state))
              forged-raw {:schemaVersion 3
                          :data [{:kind "source_contains_pua"
                                  :code "source-contains-pua"
                                  :severity "warning" :source "source"
                                  :span {:start 0 :end 3} :codepoint "\uE001"}]}
              raw-published (publish-json! root manifest forged-raw)
              forged-result (-> original
                                (assoc-in [:diagnostic_authorization_evidence :raw_diagnostics_hash]
                                          (get-in raw-published [:member :ref :sha256]))
                                (assoc-in [:diagnostic_authorization_evidence :raw_diagnostics_bytes]
                                          (get-in raw-published [:member :ref :bytes]))
                                (assoc :authorized_intervals [{:start 0 :end 3}]
                                       :silent_intervals [{:start 3 :end 6}]
                                       :authorized_bytes 3 :silent_bytes 3
                                       :diagnostic_count 1 :authorizing_diagnostic_count 1
                                       :vacuous false))
              result-member last-result-member
              result-resealed (reseal-existing-json! root (:manifest raw-published)
                                                     (:locator result-member) forged-result)
              forged-aggregate (assoc aggregate :authorized_bytes 3 :silent_bytes 18
                                      :diagnostic_count 1 :authorizing_diagnostic_count 1
                                      :authorized_interval_count 1 :vacuous false)
              forged-manifest (reseal-existing-json! root result-resealed
                                                     "diagnostic-gap-aggregate.json"
                                                     forged-aggregate)]
          (is (= :unavailable
                 (:status (rq-source/silent-drops-envelope store forged-manifest identity)))
              "ordinary decoded bytes cannot be resealed as a PUA authorization"))
        (let [result-member (last (:blobs manifest))
              policy-hash policy-artifact-hash]
          (doseq [mutated [(update manifest :blobs pop)
                           (update manifest :blobs conj result-member)
                           (update manifest :blobs
                                   #(vec (concat (butlast (butlast %)) [(last %)])))
                           (update manifest :blobs
                                   #(vec (remove (fn [member]
                                                   (= policy-hash (get-in member [:ref :sha256])))
                                                 %)))]]
            (is (= :unavailable
                   (:status (rq-source/silent-drops-envelope store mutated identity))))))
        (let [forged (assoc aggregate :silent_drop_count 0)
              forged-file (io/file root "store/diagnostic-gap-aggregate.json")
              _ (json/write-deterministic-json-file! forged-file forged)
              resealed (replace-manifest-member
                        manifest "diagnostic-gap-aggregate.json"
                        (blob-ref-for forged-file "application/json"))]
          (is (= :unavailable
                 (:status (rq-source/silent-drops-envelope store resealed identity))))))
      (finally (delete-tree! root)))))

(deftest production-diagnostic-gap-fixture-drives-the-real-p0-r2-boundary
  (let [root diagnostic-gap-capture-fixture-root
        store {:root (.getPath (io/file root "store"))}
        manifest (read-keyword-json (io/file root "manifest.json"))
        identity (read-keyword-json (io/file root "store/identity.json"))
        expected (read-keyword-json (io/file root "expected-outcomes.json"))
        before (slurp (io/file root "store/recognition-aggregate.json"))]
    (is (= {:value 3 :identity_ref recognition-identity-ref}
           (rq-source/silent-drops-envelope store manifest identity)))
    (is (= #{:clean-vacuous :authorized-pua :observe-only :opaque-unknown :silent-gap}
           (set (keys (:outcomes expected)))))
    (is (= 5 (count (set (vals (:outcomes expected))))))
    (is (= before (slurp (io/file root "store/recognition-aggregate.json")))
        "R2 leaves the exact R1 artifact byte-identical")))

(deftest r2-rejects-a-fully-resealed-cross-generation-raw-substitution
  (let [{:keys [root store manifest identity]} (staged-diagnostic-gap-capture)]
    (try
      (let [values (keep (fn [member]
                           (when-let [value (#'rq-source/authenticated-json-value store member)]
                             [member value]))
                         (:blobs manifest))
            [result-member result]
            (some (fn [[member value]]
                    (when (and (= "abc/parser-rq-diagnostic-gap-result/v1"
                                  (:schema_version value))
                               (= 1 (:authorizing_diagnostic_count value)))
                      [member value])) values)
            alternate-raw-member
            (some (fn [[member value]]
                    (when (and (= 3 (:schemaVersion value))
                               (= "unclosed-bracket" (get-in value [:data 0 :code])))
                      member)) values)
            substituted (-> result
                            (assoc-in [:diagnostic_authorization_evidence
                                       :raw_diagnostics_hash]
                                      (get-in alternate-raw-member [:ref :sha256]))
                            (assoc-in [:diagnostic_authorization_evidence
                                       :raw_diagnostics_bytes]
                                      (get-in alternate-raw-member [:ref :bytes]))
                            (assoc :authorized_intervals []
                                   :silent_intervals [{:start 0 :end 3}]
                                   :authorized_bytes 0 :silent_bytes 3
                                   :silent_drop_count 1
                                   :authorizing_diagnostic_count 0
                                   :observe_only_diagnostic_count 1))
            manifest (reseal-existing-json! root manifest (:locator result-member)
                                            substituted)
            aggregate (read-keyword-json
                       (io/file root "store/diagnostic-gap-aggregate.json"))
            aggregate (-> aggregate
                          (update :authorized_bytes - 3)
                          (update :silent_bytes + 3)
                          (update :silent_drop_count inc)
                          (update :authorizing_diagnostic_count dec)
                          (update :observe_only_diagnostic_count inc)
                          (update :authorized_interval_count dec))
            manifest (reseal-existing-json! root manifest
                                            "diagnostic-gap-aggregate.json" aggregate)]
        (is (= :unavailable
               (:status (rq-source/silent-drops-envelope store manifest identity)))
            "the result/raw/aggregate/P0 seals cannot replace generation membership"))
      (finally (delete-tree! root)))))

(deftest production-diagnostic-gap-fixture-rejects-outer-edge-tampering
  (let [root diagnostic-gap-capture-fixture-root
        store {:root (.getPath (io/file root "store"))}
        manifest (read-keyword-json (io/file root "manifest.json"))
        identity (read-keyword-json (io/file root "store/identity.json"))
        members (:blobs manifest)
        raw-member (some #(when (and (= "application/json" (get-in % [:ref :media_type]))
                                     (string/includes? (:locator %) "/"))
                            (let [value (#'rq-source/authenticated-json-value store %)]
                              (when (= 3 (:schemaVersion value)) %))) members)
        aggregate-member (some #(when (= "diagnostic-gap-aggregate.json" (:locator %)) %) members)]
    (doseq [[label changed-manifest changed-identity]
            [["raw diagnostic member removed"
              (update manifest :blobs #(vec (remove #{raw-member} %))) identity]
             ["aggregate member duplicated"
              (update manifest :blobs conj aggregate-member) identity]
             ["qualification identity changed"
              manifest (assoc-in identity [:instrument_versions :source_recognition] "forged")]]]
      (is (= :unavailable
             (:status (rq-source/silent-drops-envelope store changed-manifest changed-identity)))
          label))))
