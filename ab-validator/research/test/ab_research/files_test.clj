(ns ab-research.files-test
  (:require [ab-research.files :as files]
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

(deftest sorted-path-seq-is-deterministic-depth-first-preorder-test
  (fs/with-temp-dir [base {}]
    (let [root (fs/path base "root")]
      (fs/create-dirs (fs/path root "b"))
      (fs/create-dirs (fs/path root "a"))
      (spit (fs/file root "b" "z.txt") "z")
      (spit (fs/file root "a" "y.txt") "y")
      (is (= ["" "a" "a/y.txt" "b" "b/z.txt"]
             (mapv #(files/relative-path root %)
                   (files/sorted-path-seq root)))))))

(deftest sorted-path-seq-yields-a-missing-root-once-test
  (fs/with-temp-dir [base {}]
    (let [missing (fs/path base "missing")]
      (is (= [missing] (vec (files/sorted-path-seq missing)))))))

(deftest sorted-path-seq-traverses-a-symlinked-root-test
  (fs/with-temp-dir [base {}]
    (let [target (fs/path base "target")
          root (fs/path base "root-link")]
      (fs/create-dirs target)
      (spit (fs/file target "inside.txt") "inside")
      (fs/create-sym-link root (fs/path "target"))
      (is (= ["root-link" "inside.txt"]
             (mapv #(str (fs/file-name %))
                   (files/sorted-path-seq root)))))))

(deftest sorted-path-seq-yields-but-does-not-descend-descendant-directory-links-test
  (fs/with-temp-dir [base {}]
    (let [root (fs/path base "root")
          external (fs/path base "external")
          link (fs/path root "linked")]
      (fs/create-dirs root)
      (fs/create-dirs external)
      (spit (fs/file external "outside.txt") "outside")
      (fs/create-sym-link link external)
      (is (= [root link] (vec (files/sorted-path-seq root)))))))

(deftest sorted-path-seq-yields-file-links-test
  (fs/with-temp-dir [base {}]
    (let [root (fs/path base "root")
          target (fs/path base "target.txt")
          link (fs/path root "linked.txt")]
      (fs/create-dirs root)
      (spit (fs/file target) "target")
      (fs/create-sym-link link target)
      (is (= [root link] (vec (files/sorted-path-seq root)))))))

(deftest sorted-path-seq-propagates-listing-errors-on-realization-test
  (fs/with-temp-dir [root {}]
    (let [failure (java.io.IOException. "listing failed")
          paths (#'files/sorted-path-seq* root (fn [_] (throw failure)))]
      (is (= root (first paths)))
      (is (identical? failure
                      (try
                        (doall (rest paths))
                        nil
                        (catch java.io.IOException e e)))))))

(deftest sorted-path-seq-propagates-security-errors-on-realization-test
  (fs/with-temp-dir [root {}]
    (let [failure (SecurityException. "listing denied")
          paths (#'files/sorted-path-seq* root (fn [_] (throw failure)))]
      (is (= root (first paths)))
      (is (identical? failure
                      (try
                        (doall (rest paths))
                        nil
                        (catch SecurityException e e)))))))
