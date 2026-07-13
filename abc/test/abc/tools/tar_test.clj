(ns abc.tools.tar-test
  (:require [abc.tools.tar :as tar]
            [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- delete-tree! [root]
  (doseq [file (reverse (file-seq root))]
    (.delete file)))

(deftest write-tar-supports-bare-filename-and-preserves-member-order-test
  (let [dir (.toFile (Files/createTempDirectory
                      "abc-tar" (make-array FileAttribute 0)))
        archive-name (str ".abc-tar-" (java.util.UUID/randomUUID) ".tar")
        archive-file (io/file archive-name)]
    (try
      (spit (io/file dir "a.txt") "alpha")
      (spit (io/file dir "z.txt") "zeta")
      (let [archive (tar/write-tar!
                     archive-name
                     [{:member-path "z.txt" :source-file (io/file dir "z.txt")}
                      {:member-path "a.txt" :source-file (io/file dir "a.txt")}])]
        (is (nil? (fs/parent archive-name)))
        (is (= archive-file archive))
        (is (= "zeta" (String. (tar/member-bytes archive "z.txt"))))
        (is (= "alpha" (String. (tar/member-bytes archive "a.txt"))))
        (let [bytes (String. (Files/readAllBytes (.toPath archive))
                             java.nio.charset.StandardCharsets/ISO_8859_1)]
          (is (< (.indexOf bytes "z.txt") (.indexOf bytes "a.txt")))))
      (finally
        (delete-tree! dir)
        (.delete archive-file)))))
