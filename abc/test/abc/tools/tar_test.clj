(ns abc.tools.tar-test
  (:require [abc.tools.tar :as tar]
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
        original-user-dir (System/getProperty "user.dir")]
    (try
      (spit (io/file dir "a.txt") "alpha")
      (spit (io/file dir "z.txt") "zeta")
      (System/setProperty "user.dir" (.getPath dir))
      (let [archive (tar/write-tar!
                     "ordered.tar"
                     [{:member-path "z.txt" :source-file (io/file dir "z.txt")}
                      {:member-path "a.txt" :source-file (io/file dir "a.txt")}])]
        (is (= (io/file "ordered.tar") archive))
        (is (= "zeta" (String. (tar/member-bytes archive "z.txt"))))
        (is (= "alpha" (String. (tar/member-bytes archive "a.txt"))))
        (let [bytes (String. (Files/readAllBytes (.toPath archive))
                             java.nio.charset.StandardCharsets/ISO_8859_1)]
          (is (< (.indexOf bytes "z.txt") (.indexOf bytes "a.txt")))))
      (finally
        (System/setProperty "user.dir" original-user-dir)
        (delete-tree! dir)
        (.delete (io/file "ordered.tar"))))))
