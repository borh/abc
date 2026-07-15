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
