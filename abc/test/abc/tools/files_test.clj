(ns abc.tools.files-test
  (:require [abc.tools.files :as files]
            [babashka.fs :as fs]
            [clojure.test :refer [deftest is]]))

(deftest delete-tree-is-a-noop-on-missing-path-test
  (is (nil? (files/delete-tree!
             (str (fs/path (fs/temp-dir) "abc-files-missing-xyz-should-not-exist"))))))

(deftest delete-tree-removes-a-populated-tree-test
  (fs/with-temp-dir [d {}]
    (let [nested (fs/file d "a" "b")]
      (fs/create-dirs nested)
      (spit (fs/file nested "f.txt") "x")
      (files/delete-tree! (fs/file d "a"))
      (is (not (fs/exists? (fs/file d "a")))))))

(deftest copy-file-creates-missing-parents-test
  (fs/with-temp-dir [d {}]
    (let [src (fs/file d "src.txt")
          dst (fs/file d "nested" "deep" "dst.txt")]
      (spit src "hello")
      (files/copy-file! src dst)
      (is (= "hello" (slurp dst))))))

(deftest copy-file-handles-parentless-target-test
  ;; regression guard: a bare-filename target must not NPE
  (fs/with-temp-dir [d {}]
    (let [src (fs/file d "src.txt")
          cwd-target "abc-copy-parentless-target-test.txt"]
      (spit src "hi")
      (try
        (is (some? (files/copy-file! src cwd-target)))
        (is (= "hi" (slurp cwd-target)))
        (finally
          (fs/delete-if-exists cwd-target))))))

(deftest read-edn-parses-a-file-test
  (fs/with-temp-dir [d {}]
    (let [f (fs/file d "x.edn")]
      (spit f "{:a 1 :b [2 3]}")
      (is (= {:a 1 :b [2 3]} (files/read-edn f))))))

(deftest relative-path-uses-forward-slashes-test
  (is (= "a/b/c.txt"
         (files/relative-path (fs/file "/root") (fs/file "/root/a/b/c.txt")))))
