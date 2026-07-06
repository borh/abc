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

(deftest update-aozora-bunko-repo-pulls-aozora-repo-test
  (testing "update-aozora-bunko-repo uses the supplied Aozora repo path"
    (let [remote-dir (temp-dir "abc-git-remote")
          local-dir (temp-dir "abc-git-local")
          seed-dir (temp-dir "abc-git-seed")
          remote-git (-> (Git/init) (.setDirectory remote-dir) (.setBare true) .call)
          seed-git (-> (Git/init) (.setDirectory seed-dir) .call)]
      (try
        (commit-file! seed-git seed-dir "index.txt" "v1" "v1")
        (-> seed-git .remoteAdd
            (.setName "origin")
            (.setUri (org.eclipse.jgit.transport.URIish. (.toString (.toURI remote-dir))))
            .call)
        (-> seed-git .push (.setRemote "origin") .call)
        (let [local-git (Git/cloneRepository)]
          (-> local-git
              (.setURI (.toString (.toURI remote-dir)))
              (.setDirectory local-dir)
              .call
              .close))
        (commit-file! seed-git seed-dir "index.txt" "v2" "v2")
        (-> seed-git .push (.setRemote "origin") .call)
        (abc-git/update-aozora-bunko-repo (str local-dir))
        (is (= "v2" (slurp (io/file local-dir "index.txt"))))
        (finally
          (.close remote-git)
          (.close seed-git)
          (delete-recursive remote-dir)
          (delete-recursive local-dir)
          (delete-recursive seed-dir))))))

(deftest commits-touching-path-returns-chronological-path-history-test
  (testing "abc.git can discover the commits that changed one path"
    (let [root (temp-dir "abc-git-path-history")
          git (-> (Git/init) (.setDirectory root) .call)]
      (try
        (let [first-target (commit-file! git root "index_pages/list_person_all_extended_utf8.zip"
                                         "v1" "target v1")
              other-path (commit-file! git root "README.md" "notes" "other")
              second-target (commit-file! git root "index_pages/list_person_all_extended_utf8.zip"
                                          "v2" "target v2")
              third-target (commit-file! git root "index_pages/list_person_all_extended_utf8.zip"
                                         "v3" "target v3")]
          (is (= [(.getName first-target)
                  (.getName second-target)
                  (.getName third-target)]
                 (mapv #(.getName %)
                       (abc-git/commits-touching-path
                        git "index_pages/list_person_all_extended_utf8.zip"))))
          (is (= [(.getName second-target)]
                 (mapv #(.getName %)
                       (abc-git/commits-touching-path
                        git "index_pages/list_person_all_extended_utf8.zip"
                        {:from-ref (.getName first-target)
                         :to-ref (.getName second-target)}))))
          (is (not-any? #{(.getName other-path)}
                        (map #(.getName %)
                             (abc-git/commits-touching-path
                              git "index_pages/list_person_all_extended_utf8.zip")))))
        (finally
          (.close git)
          (delete-recursive root))))))
