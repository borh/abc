(ns abc.tools.parser-rq-capture-test
  (:require [abc.tools.hash :as hash]
            [abc.tools.parser-rq-capture :as capture]
            [clojure.test :refer [deftest is]]))

(def valid-sha256
  "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

(deftest manifest-requires-logical-blob-identity-not-a-path
  (let [ok {:blob {:sha256 valid-sha256
                   :bytes 12
                   :media_type "application/json"}}
        bad {:blob {:artifact_root "/db/hinoki/run-7/out.json"}}]
    (is (empty? (capture/manifest-errors ok)))
    (is (seq (capture/manifest-errors bad)))
    (is (some #(re-find #"sha256|logical blob identity" %)
              (capture/manifest-errors bad)))))

(deftest manifest-denominator-carries-explicit-units
  (let [bad {:blob {:sha256 valid-sha256
                    :bytes 1
                    :media_type "application/octet-stream"}
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
