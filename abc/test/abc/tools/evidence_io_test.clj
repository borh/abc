(ns abc.tools.evidence-io-test
  (:require [abc.tools.evidence-io :as evidence-io]
            [abc.tools.files :as files]
            [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]])
  (:import [java.util.zip ZipEntry ZipOutputStream]))

(deftest read-trace-canonicalizes-and-classifies-test
  (let [root (fs/file (fs/create-temp-dir {:prefix "evidence-io-"}))
        file (fs/file root "data/value.txt")]
    (fs/create-dirs (fs/parent file))
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
  (let [identity (fs/file (fs/create-temp-dir {:prefix "identity-"}))
        temp (fs/file (fs/create-temp-dir {:prefix "ephemeral-"}))
        generated (fs/file temp "generated.txt")]
    (spit generated "generated")
    (let [result (evidence-io/with-read-trace
                   {:identity-root identity :cwd-root identity}
                   (fn [] (evidence-io/with-ephemeral-root
                            temp (fn [] (evidence-io/record-read! generated)))))]
      (is (= [(str (fs/canonicalize generated))] (:ephemeral-paths result))))
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

(deftest ephemeral-root-requires-trace-and-external-workspace-test
  (let [identity (fs/file (fs/create-temp-dir {:prefix "identity-auth-"}))
        workspace (fs/file (fs/create-temp-dir {:prefix "workspace-auth-"}))
        workspace-alias (fs/file workspace "nested" "..")]
    (fs/create-dirs (fs/file workspace "nested"))
    (is (= :invalid-ephemeral-root
           (:kind (ex-data (try
                             (evidence-io/with-ephemeral-root workspace identity)
                             (catch Exception e e))))))
    (is (= :invalid-ephemeral-root
           (:kind (ex-data (try
                             (evidence-io/with-read-trace
                               {:identity-root identity
                                :cwd-root identity
                                :workspace-root workspace}
                               #(evidence-io/with-ephemeral-root
                                  workspace-alias identity))
                             (catch Exception e e))))))))

(deftest read-trace-workspace-defaults-to-canonical-identity-test
  (let [identity (fs/file (fs/create-temp-dir {:prefix "identity-default-"}))]
    (is (= (fs/file (fs/canonicalize identity))
           (:value
            (evidence-io/with-read-trace
              {:identity-root identity :cwd-root identity}
              #(:workspace-root evidence-io/*read-trace*)))))))

(deftest owned-ephemeral-root-owns-lifecycle-and-traces-rereads-test
  (let [identity (fs/file (fs/create-temp-dir {:prefix "identity-owned-"}))
        workspace (fs/file (fs/create-temp-dir {:prefix "workspace-owned-"}))
        seen (atom nil)
        result (evidence-io/with-read-trace
                 {:identity-root identity
                  :cwd-root identity
                  :workspace-root workspace}
                 #(evidence-io/with-owned-ephemeral-root
                    (fn [root]
                      (reset! seen root)
                      (is (fs/directory? root))
                      (is (not (fs/starts-with? (fs/canonicalize root)
                                                (fs/canonicalize identity))))
                      (is (not (fs/starts-with? (fs/canonicalize root)
                                                (fs/canonicalize workspace))))
                      (let [generated (fs/file root "generated.txt")]
                        (spit generated "generated")
                        (files/read-text generated)
                        (files/read-text generated)
                        :ok))))]
    (is (= :ok (:value result)))
    (is (= [(str (fs/canonicalize (fs/file @seen "generated.txt")))]
           (:ephemeral-paths result)))
    (is (false? (fs/exists? @seen)))))

(deftest owned-ephemeral-root-cleans-up-after-throw-test
  (let [identity (fs/file (fs/create-temp-dir {:prefix "identity-throw-"}))
        workspace (fs/file (fs/create-temp-dir {:prefix "workspace-throw-"}))
        seen (atom nil)]
    (is (thrown? Exception
                 (evidence-io/with-read-trace
                   {:identity-root identity
                    :cwd-root identity
                    :workspace-root workspace}
                   #(evidence-io/with-owned-ephemeral-root
                      (fn [root]
                        (reset! seen root)
                        (throw (Exception.)))))))
    (is (false? (fs/exists? @seen)))))

(deftest owned-ephemeral-root-requires-active-trace-test
  (is (= :invalid-ephemeral-root
         (:kind (ex-data (try
                           (evidence-io/with-owned-ephemeral-root identity)
                           (catch Exception e e)))))))

(deftest ephemeral-copy-cannot-launder-a-repository-read-test
  (let [identity (fs/file (fs/create-temp-dir {:prefix "identity-copy-"}))
        temp (fs/file (fs/create-temp-dir {:prefix "ephemeral-copy-"}))
        source (fs/file identity "source.txt")
        generated (fs/file temp "generated.txt")]
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
      (is (= [(str (fs/canonicalize generated))] (:ephemeral-paths result))))))

(deftest metadata-adapters-are-traced-and-missing-state-fails-closed-test
  (let [root (fs/file (fs/create-temp-dir {:prefix "metadata-trace-"}))
        file (fs/file root "present.txt")]
    (fs/create-dirs (fs/file root "data"))
    (spit file "present")
    (let [result (evidence-io/with-read-trace
                   {:identity-root root :cwd-root root}
                   #(vector (files/exists? file)
                            (files/file? file)
                            (files/directory? (fs/file root "data"))))]
      (is (= [true true true] (:value result)))
      (is (= ["data" "present.txt"] (:repository-paths result))))
    (is (false? (files/exists? (fs/file root "missing.txt"))))
    (is (= :missing-runtime-input
           (:kind
            (ex-data
             (try
               (evidence-io/with-read-trace
                 {:identity-root root :cwd-root root}
                 #(files/exists? "missing.txt"))
               (catch Exception exception exception))))))))

(deftest structured-read-adapters-record-the-paths-they-load-test
  (let [root (fs/file (fs/create-temp-dir {:prefix "structured-trace-"}))
        jsonl (fs/file root "rows.jsonl")
        archive (fs/file root "fixture.zip")
        ttl (fs/file root "model.ttl")
        xml (fs/file root "document.xml")]
    (spit jsonl "{\"value\":1}\n")
    (with-open [out (ZipOutputStream. (io/output-stream archive))]
      (.putNextEntry out (ZipEntry. "entry.txt"))
      (.write out (.getBytes "value" "UTF-8"))
      (.closeEntry out))
    (spit ttl "@prefix ex: <https://example.invalid/> . ex:s ex:p ex:o .\n")
    (spit xml "<root/>")
    (let [result
          (evidence-io/with-read-trace
            {:identity-root root :cwd-root root}
            #(vector (files/read-json-lines jsonl)
                     (files/with-zip-file archive
                       (fn [zip] (some? (.getEntry zip "entry.txt"))))
                     (.size (files/load-jena-model ttl))
                     (.getDocumentElement (files/parse-xml-document xml))))]
      (is (= [{"value" 1}] (first (:value result))))
      (is (= true (second (:value result))))
      (is (= 1 (nth (:value result) 2)))
      (is (= "root" (.getTagName (nth (:value result) 3))))
      (is (= ["document.xml" "fixture.zip" "model.ttl" "rows.jsonl"]
             (:repository-paths result))))))
