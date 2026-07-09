(ns abc.tools.workflow.cache-test
  (:require [abc.tools.workflow.cache :as cache]
            [abc.tools.hash :as hash]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(defn with-temp-dir [f]
  (let [dir (io/file (System/getProperty "java.io.tmpdir") (str "cache-test-" (System/nanoTime)))]
    (.mkdirs dir)
    (try
      (f dir)
      (finally
        (doseq [file (file-seq dir)]
          (.delete file))))))

(def comps
  {:workflow-id "wf" :target-key :bundle :node-key :admission
   :node-kind "leaf" :graph-version "g1"
   :impl-id :admit :impl-hash "sha256:0000000000000000000000000000000000000000000000000000000000000000"
   :input-value-hashes {"request-set" "sha256:1111111111111111111111111111111111111111111111111111111111111111"}
   :path-content-hashes {"snapshot.json" "sha256:2222222222222222222222222222222222222222222222222222222222222222"}
   :policy-hashes {"pack-policy" "sha256:3333333333333333333333333333333333333333333333333333333333333333"}})

(deftest node-cache-key-is-valid-and-deterministic
  (testing "node-cache-key returns a valid sha256 hash"
    (let [key (cache/node-cache-key comps)]
      (is (re-matches hash/hash-pattern key))))

  (testing "node-cache-key is deterministic"
    (let [key1 (cache/node-cache-key comps)
          key2 (cache/node-cache-key comps)]
      (is (= key1 key2)))))

(deftest key-changes-when-components-change
  (testing "changing node-key yields a different key"
    (let [key1 (cache/node-cache-key comps)
          key2 (cache/node-cache-key (assoc comps :node-key :other))]
      (is (not= key1 key2))))

  (testing "changing workflow-id yields a different key"
    (let [key1 (cache/node-cache-key comps)
          key2 (cache/node-cache-key (assoc comps :workflow-id "other"))]
      (is (not= key1 key2))))

  (testing "reordering input-value-hashes yields the same key (canonical)"
    (let [key1 (cache/node-cache-key comps)
          reordered (assoc comps :input-value-hashes
                           {"request-set" "sha256:1111111111111111111111111111111111111111111111111111111111111111"
                            "other-input" "sha256:4444444444444444444444444444444444444444444444444444444444444444"})
          key2 (cache/node-cache-key reordered)
          reordered2 (assoc comps :input-value-hashes
                            {"other-input" "sha256:4444444444444444444444444444444444444444444444444444444444444444"
                             "request-set" "sha256:1111111111111111111111111111111111111111111111111111111111111111"})
          key3 (cache/node-cache-key reordered2)]
      (is (= key2 key3)))))

(deftest valid-cached-node-result-ok
  (testing "returns :ok for valid cached result"
    (with-temp-dir
      (fn [dir]
        (let [out-file (io/file dir "out.txt")
              _ (spit out-file "hello world")
              recorded (hash/format-sha256 (hash/sha256-file out-file))
              cached {:outputs [{:path "out.txt" :content_hash recorded}]
                      :config {"profile" "default"}}
              env {:base-dir dir :config {"profile" "default"}}
              result (cache/valid-cached-node-result cached env)]
          (is (= (:status result) :ok)))))))

(deftest valid-cached-node-result-stale-content-hash
  (testing "returns :stale on content-hash mismatch"
    (with-temp-dir
      (fn [dir]
        (let [out-file (io/file dir "out.txt")
              _ (spit out-file "hello world")
              recorded (hash/format-sha256 (hash/sha256-file out-file))
              _ (spit out-file "different content")
              cached {:outputs [{:path "out.txt" :content_hash recorded}]
                      :config {"profile" "default"}}
              env {:base-dir dir :config {"profile" "default"}}
              result (cache/valid-cached-node-result cached env)]
          (is (= (:status result) :stale))
          (is (contains? result :reason))
          (is (contains? result :evidence)))))))

(deftest valid-cached-node-result-invalid-missing-path
  (testing "returns :invalid on missing path"
    (with-temp-dir
      (fn [dir]
        (let [cached {:outputs [{:path "nonexistent.txt"
                                 :content_hash "sha256:1111111111111111111111111111111111111111111111111111111111111111"}]
                      :config {"profile" "default"}}
              env {:base-dir dir :config {"profile" "default"}}
              result (cache/valid-cached-node-result cached env)]
          (is (= (:status result) :invalid)))))))

(deftest valid-cached-node-result-stale-config-drift
  (testing "returns :stale on identity-relevant config drift"
    (with-temp-dir
      (fn [dir]
        (let [out-file (io/file dir "out.txt")
              _ (spit out-file "hello world")
              recorded (hash/format-sha256 (hash/sha256-file out-file))
              cached {:outputs [{:path "out.txt" :content_hash recorded}]
                      :config {"profile" "default"}}
              env {:base-dir dir :config {"profile" "other"}}
              result (cache/valid-cached-node-result cached env)]
          (is (= (:status result) :stale)))))))

(deftest valid-cached-node-result-invalid-malformed
  (testing "returns :invalid on missing outputs"
    (let [cached {:config {"profile" "default"}}
          env {:base-dir "/tmp" :config {"profile" "default"}}
          result (cache/valid-cached-node-result cached env)]
      (is (= (:status result) :invalid))))

  (testing "returns :invalid on output missing content_hash"
    (with-temp-dir
      (fn [dir]
        (let [cached {:outputs [{:path "out.txt"}]
                      :config {"profile" "default"}}
              env {:base-dir dir :config {"profile" "default"}}
              result (cache/valid-cached-node-result cached env)]
          (is (= (:status result) :invalid))))))

  (testing "returns :invalid on output missing path"
    (with-temp-dir
      (fn [dir]
        (let [cached {:outputs [{:content_hash "sha256:1111111111111111111111111111111111111111111111111111111111111111"}]
                      :config {"profile" "default"}}
              env {:base-dir dir :config {"profile" "default"}}
              result (cache/valid-cached-node-result cached env)]
          (is (= (:status result) :invalid)))))))
