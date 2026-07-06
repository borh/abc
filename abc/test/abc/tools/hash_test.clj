(ns abc.tools.hash-test
  (:require [abc.tools.hash :as hash]
            [clojure.test :refer [deftest is testing]]))

(deftest sha256-helpers-test
  (testing "known byte and string hashes"
    (is (= "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
           (hash/sha256-bytes (byte-array 0))))
    (is (= "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
           (hash/sha256-string "abc"))))
  (testing "file hash matches byte hash"
    (let [file (java.io.File/createTempFile "abc-sha256" ".txt")]
      (try
        (spit file "abc")
        (is (= (hash/sha256-string "abc")
               (hash/sha256-file file)))
        (finally
          (.delete file)))))
  (testing "sha256 formatter and parser"
    (is (= "sha256:ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
           (hash/format-sha256 (hash/sha256-string "abc"))))
    (is (= (hash/sha256-string "abc")
           (hash/parse-sha256 "sha256:ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Invalid sha256 hash"
                          (hash/parse-sha256 "sha256:BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Invalid sha256 hash"
                          (hash/parse-sha256 "nope")))))
