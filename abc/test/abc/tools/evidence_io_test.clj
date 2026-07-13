(ns abc.tools.evidence-io-test
  (:require [abc.tools.evidence-io :as evidence-io]
            [abc.tools.files :as files]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(deftest read-trace-canonicalizes-and-classifies-test
  (let [root (.toFile (java.nio.file.Files/createTempDirectory "evidence-io-" (make-array java.nio.file.attribute.FileAttribute 0)))
        file (io/file root "data/value.txt")]
    (.mkdirs (.getParentFile file))
    (spit file "value")
    (let [result (evidence-io/with-read-trace
                   {:identity-root root :cwd-root root}
                   #(do (evidence-io/record-read! "data/value.txt")
                        (evidence-io/record-read! file)
                        :ok))]
      (is (= :ok (:value result)))
      (is (= ["data/value.txt"] (:repository-paths result)))
      (is (= [] (:ephemeral-paths result))))))

(deftest ephemeral-root-is-a-narrow-capability-test
  (let [identity (.toFile (java.nio.file.Files/createTempDirectory "identity-" (make-array java.nio.file.attribute.FileAttribute 0)))
        temp (.toFile (java.nio.file.Files/createTempDirectory "ephemeral-" (make-array java.nio.file.attribute.FileAttribute 0)))
        generated (io/file temp "generated.txt")]
    (spit generated "generated")
    (let [result (evidence-io/with-read-trace
                   {:identity-root identity :cwd-root identity}
                   (fn [] (evidence-io/with-ephemeral-root
                            temp (fn [] (evidence-io/record-read! generated)))))]
      (is (= [(.getCanonicalPath generated)] (:ephemeral-paths result))))
    (testing "external reads and identity-contained ephemeral roots fail closed"
      (is (= :external-read-denied
             (:kind (ex-data (try
                               (evidence-io/with-read-trace
                                 {:identity-root identity :cwd-root identity}
                                 #(evidence-io/record-read! generated))
                               (catch Exception e e))))))
      (is (= :invalid-ephemeral-root
             (:kind (ex-data (try
                               (evidence-io/with-read-trace
                                 {:identity-root identity :cwd-root identity}
                                 #(evidence-io/with-ephemeral-root identity identity))
                               (catch Exception e e)))))))))

(deftest ephemeral-copy-cannot-launder-a-repository-read-test
  (let [identity (.toFile (java.nio.file.Files/createTempDirectory
                           "identity-copy-" (make-array java.nio.file.attribute.FileAttribute 0)))
        temp (.toFile (java.nio.file.Files/createTempDirectory
                       "ephemeral-copy-" (make-array java.nio.file.attribute.FileAttribute 0)))
        source (io/file identity "source.txt")
        generated (io/file temp "generated.txt")]
    (spit source "source")
    (let [result
          (evidence-io/with-read-trace
            {:identity-root identity :cwd-root identity}
            #(evidence-io/with-ephemeral-root
               temp
               (fn []
                 (files/copy-file! source generated)
                 (files/read-text generated))))]
      (is (= ["source.txt"] (:repository-paths result)))
      (is (= [(.getCanonicalPath generated)] (:ephemeral-paths result))))))
