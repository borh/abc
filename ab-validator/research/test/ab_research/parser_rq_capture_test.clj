(ns ab-research.parser-rq-capture-test
  (:require [ab-research.hash :as hash]
            [ab-research.parser-rq-capture :as capture]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(def valid-sha256
  "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

(deftest manifest-requires-logical-blob-identity-not-a-path
  (let [ok {:blobs [{:locator "run-7/out.json"
                     :ref {:sha256 valid-sha256
                           :bytes 12
                           :media_type "application/json"}}]}
        bad {:blobs [{:locator "run-7/out.json"
                      :ref {:artifact_root "/db/hinoki/run-7/out.json"}}]}]
    (is (empty? (capture/manifest-errors ok)))
    (is (seq (capture/manifest-errors bad)))
    (is (some #(re-find #"sha256|logical blob identity" %)
              (capture/manifest-errors bad)))))

(deftest manifest-denominator-carries-explicit-units
  (let [bad {:blobs [{:locator "capture.bin"
                      :ref {:sha256 valid-sha256
                            :bytes 1
                            :media_type "application/octet-stream"}}]
             :denominator {:value 17886}}]
    (is (seq (capture/manifest-errors bad)))))

(deftest observation-is-an-identity-bound-envelope
  (let [env {:value 1.0 :identity_ref valid-sha256}]
    (is (= 1.0 (capture/observation-value env)))
    (is (empty? (capture/envelope-errors env)))
    (is (seq (capture/envelope-errors {:value 1.0})))
    (is (seq (capture/envelope-errors {:identity_ref valid-sha256})))))

(deftest closed-membership-is-order-independent-test
  (is (empty? (capture/closed-membership-errors
               ["a" "b"]
               [{:work_id "b"} {:work_id "a"}]))))

(deftest closed-membership-rejects-omission-extra-and-duplicate-test
  (is (seq (capture/closed-membership-errors
            ["a" "b"]
            [{:work_id "a"}])))
  (is (seq (capture/closed-membership-errors
            ["a" "b"]
            [{:work_id "a"} {:work_id "b"} {:work_id "c"}])))
  (is (seq (capture/closed-membership-errors
            ["a" "b"]
            [{:work_id "a"} {:work_id "a"}])))
  (is (seq (capture/closed-membership-errors
            ["a"]
            [{}]))))

(deftest closed-membership-permutation-property-test
  (let [result
        (tc/quick-check
         100
         (prop/for-all [work-ids (gen/vector-distinct
                                  (gen/such-that seq gen/string-alphanumeric 100)
                                  {:min-elements 1
                                   :max-elements 20})
                        order gen/nat]
                       (let [records (mapv (fn [work-id] {:work_id work-id}) work-ids)
                             rotated (if (seq records)
                                       (let [n (mod order (count records))]
                                         (vec (concat (drop n records) (take n records))))
                                       records)]
                         (empty? (capture/closed-membership-errors work-ids rotated)))))]
    (is (:pass? result) (pr-str result))))

(deftest generation-identity-is-canonical-and-content-addressed
  (let [left {"candidate" {"rev" "abc" "schema" "v1"}
              "blobs" [{"sha256" valid-sha256 "bytes" 2}]}
        reordered {"blobs" [{"bytes" 2 "sha256" valid-sha256}]
                   "candidate" {"schema" "v1" "rev" "abc"}}
        mutated (assoc-in left ["candidate" "rev"] "def")]
    (is (= (capture/capture-generation-ref left)
           (capture/capture-generation-ref reordered)))
    (is (not= (capture/capture-generation-ref left)
              (capture/capture-generation-ref mutated)))))

(deftest observation-envelope-may-carry-closed-details
  (let [envelope (capture/observation-envelope
                  valid-sha256 1.0
                  {:diagnostic_count 0 :vacuous true})]
    (is (= {:value 1.0
            :identity_ref valid-sha256
            :details {:diagnostic_count 0 :vacuous true}}
           envelope))
    (is (empty? (capture/envelope-errors envelope)))
    (is (seq (capture/envelope-errors (assoc envelope :unexpected true))))))

(deftest wire-status-mapping-is-total-injective-and-closed
  (let [mapping {:allowed_statuses ["measured" "invalid"]
                 :values {"measured" 1.0
                          "invalid" :invalid-envelope}}]
    (is (= 1.0 (capture/map-wire-status mapping "measured")))
    (is (= :invalid-envelope (capture/map-wire-status mapping "invalid")))
    (is (= {:status :unavailable :reason :status-mapping-invalid}
           (capture/map-wire-status mapping "unknown")))
    (doseq [bad-mapping [(assoc-in mapping [:values "measured"] :invalid-envelope)
                         (update mapping :values dissoc "invalid")
                         (update mapping :allowed_statuses conj "missing")]
            status ["measured" "unknown"]
            :when (or (= status "unknown") (not= bad-mapping mapping))]
      (is (= {:status :unavailable :reason :status-mapping-invalid}
             (capture/map-wire-status bad-mapping status))))))

(deftest verifier-streams-and-rehashes-never-trusts-metadata
  (let [f (doto (java.io.File/createTempFile "blob" ".bin")
            (spit "hello"))
        real (hash/format-sha256 (hash/sha256-string "hello"))
        ref {:sha256 real :bytes 5 :media_type "application/octet-stream"}]
    (try
      (is (= :ok (:status (capture/verify-blob {:root (.getParent f)}
                                               ref
                                               (.getName f)))))
      (is (= :unavailable
             (:status (capture/verify-blob {:root (.getParent f)}
                                           (assoc ref :sha256 valid-sha256)
                                           (.getName f)))))
      (is (= :unavailable
             (:status (capture/verify-blob {:root (.getParent f)}
                                           ref
                                           "missing.bin"))))
      (finally
        (.delete f)))))

(deftest manifest-verification-fails-closed-when-any-blob-is-unavailable
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "parser-rq-store"
                       (make-array java.nio.file.attribute.FileAttribute 0)))
        present (io/file root "present.bin")
        _ (spit present "hello")
        real (hash/format-sha256 (hash/sha256-string "hello"))
        manifest {:blobs [{:locator "present.bin"
                           :ref {:sha256 real :bytes 5 :media_type "text/plain"}}
                          {:locator "missing.bin"
                           :ref {:sha256 real :bytes 5 :media_type "text/plain"}}]}]
    (try
      (is (= :unavailable
             (:status (capture/verify-manifest {:root (.getPath root)} manifest))))
      (finally
        (.delete present)
        (.delete root)))))

(deftest authenticated-read-rejects-symlink-components-and-destination
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "parser-rq-store-links"
                       (make-array java.nio.file.attribute.FileAttribute 0)))
        real-dir (doto (io/file root "real") .mkdirs)
        blob (doto (io/file real-dir "blob.json") (spit "{}"))
        ref {:sha256 (hash/format-sha256 (hash/sha256-string "{}"))
             :bytes 2 :media_type "application/json"}
        ancestor-link (.resolve (.toPath root) "alias")
        destination-link (.resolve (.toPath root) "blob-link.json")]
    (try
      (java.nio.file.Files/createSymbolicLink ancestor-link (.toPath real-dir)
                                              (make-array java.nio.file.attribute.FileAttribute 0))
      (java.nio.file.Files/createSymbolicLink destination-link (.toPath blob)
                                              (make-array java.nio.file.attribute.FileAttribute 0))
      (is (= :unavailable (:status (capture/authenticated-read
                                    {:root (.getPath root)} ref "alias/blob.json"))))
      (is (= :unavailable (:status (capture/authenticated-read
                                    {:root (.getPath root)} ref "blob-link.json"))))
      (is (= :ok (:status (capture/authenticated-read
                           {:root (.getPath root)} ref "real/blob.json"))))
      (finally (doseq [path [destination-link ancestor-link]]
                 (java.nio.file.Files/deleteIfExists path))
               (.delete blob) (.delete real-dir) (.delete root)))))

(deftest authenticated-read-rejects-absolute-and-traversing-locators
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "parser-rq-store-locators"
                       (make-array java.nio.file.attribute.FileAttribute 0)))
        real-dir (doto (io/file root "real") .mkdirs)
        blob (doto (io/file real-dir "blob") (spit "ok"))
        encoded-dir (doto (io/file root "%2e%2e") .mkdirs)
        encoded-blob (doto (io/file encoded-dir "blob") (spit "ok"))
        ref {:sha256 (hash/format-sha256 (hash/sha256-string "ok"))
             :bytes 2 :media_type "application/octet-stream"}]
    (try
      (is (= :unavailable (:status (capture/authenticated-read
                                    {:root (.getPath root)} ref "../outside"))))
      (is (= :unavailable (:status (capture/authenticated-read
                                    {:root (.getPath root)} ref
                                    "real/../real/blob"))))
      (is (= :unavailable (:status (capture/authenticated-read
                                    {:root (.getPath root)} ref "/tmp/outside"))))
      (is (= :unavailable (:status (capture/authenticated-read
                                    {:root (.getPath root)} ref ""))))
      (is (= :ok (:status (capture/authenticated-read
                           {:root (.getPath root)} ref "real/./blob"))))
      (is (= :ok (:status (capture/authenticated-read
                           {:root (.getPath root)} ref "%2e%2e/blob")))
          "locators are filesystem paths, not URL-decoded strings")
      (finally (.delete encoded-blob) (.delete encoded-dir)
               (.delete blob) (.delete real-dir) (.delete root)))))

(deftest manifest-authentication-retains-the-exact-bytes-that-were-hashed
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "parser-rq-store-single-read"
                       (make-array java.nio.file.attribute.FileAttribute 0)))
        blob (doto (io/file root "blob.json") (spit "{\"version\":1}"))
        ref {:sha256 (hash/format-sha256 (hash/sha256-string "{\"version\":1}"))
             :bytes 13 :media_type "application/json"}
        manifest {:blobs [{:locator "blob.json" :ref ref}]}]
    (try
      (binding [capture/*after-authenticated-read*
                (fn [_] (spit blob "{\"version\":2}"))]
        (let [verified (capture/verify-manifest {:root (.getPath root)} manifest)]
          (is (= :ok (:status verified)))
          (is (= "{\"version\":1}"
                 (String. ^bytes (get-in verified [:authenticated_blobs "blob.json"])
                          java.nio.charset.StandardCharsets/UTF_8)))))
      (finally (.delete blob) (.delete root)))))
