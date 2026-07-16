(ns abc.tools.parser-rq-capture-test
  (:require [abc.tools.hash :as hash]
            [abc.tools.parser-rq-capture :as capture]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

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
        ref {:sha256 valid-sha256 :bytes 0 :media_type "application/octet-stream"}]
    (try
      (is (= :unavailable (:status (capture/authenticated-read
                                    {:root (.getPath root)} ref "../outside"))))
      (is (= :unavailable (:status (capture/authenticated-read
                                    {:root (.getPath root)} ref "/tmp/outside"))))
      (finally (.delete root)))))

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
