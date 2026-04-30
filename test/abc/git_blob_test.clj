(ns abc.git-blob-test
  (:require [abc.git :as abc-git]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [org.eclipse.jgit.api Git]))

(defn- temp-dir [prefix]
  (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- delete-recursive [^java.io.File f]
  (when (.isDirectory f)
    (doseq [child (.listFiles f)] (delete-recursive child)))
  (.delete f))

(defn- commit-file! [^Git git root rel content message]
  (let [file (io/file root rel)]
    (io/make-parents file)
    (spit file content)
    (-> git .add (.addFilepattern rel) .call)
    (-> git
        .commit
        (.setMessage message)
        (.setAuthor "ABC Test" "abc@example.test")
        (.setCommitter "ABC Test" "abc@example.test")
        .call)))

(deftest blob-bytes-at-reads-file-content-at-ref-test
  (testing "abc.git exposes ref/path blob extraction without shelling out"
    (let [root (temp-dir "abc-git-blob")
          git (-> (Git/init) (.setDirectory root) .call)]
      (try
        (let [old-commit (commit-file! git root "index_pages/list_person_all_extended_utf8.zip"
                                       "old bytes" "old")
              new-commit (commit-file! git root "index_pages/list_person_all_extended_utf8.zip"
                                       "new bytes" "new")]
          (is (= "old bytes"
                 (String. (abc-git/blob-bytes-at git
                                                 (.getName old-commit)
                                                 "index_pages/list_person_all_extended_utf8.zip")
                          StandardCharsets/UTF_8)))
          (is (= "new bytes"
                 (String. (abc-git/blob-bytes-at git
                                                 (.getName new-commit)
                                                 "index_pages/list_person_all_extended_utf8.zip")
                          StandardCharsets/UTF_8))))
        (finally
          (.close git)
          (delete-recursive root))))))
