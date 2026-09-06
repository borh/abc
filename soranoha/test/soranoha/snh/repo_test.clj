(ns soranoha.snh.repo-test
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [soranoha.snh.repo :as repo])
  (:import [java.io ByteArrayOutputStream]))

(defn- git [dir & args]
  (let [result (apply process/sh {:dir (str dir) :out :string :err :string} "git" args)]
    (assert (zero? (:exit result)) (:err result))
    (str/trim (:out result))))

(defn- bytes-at [path]
  (vec (java.nio.file.Files/readAllBytes (fs/path path))))

(defn- read-blob [dir oid]
  (let [out (ByteArrayOutputStream.)
        result (process/sh {:dir (str dir) :out out :err :string} "git" "cat-file" "blob" oid)]
    (assert (zero? (:exit result)) (:err result))
    (vec (.toByteArray out))))

(deftest commit-writes-preserve-paths-bytes-parents-and-user-state
  (let [dir (fs/create-temp-dir {:prefix "repo-writer"})]
    (try
      (git dir "init" "-q")
      (let [tracked (fs/path dir "tracked.txt")
            _ (spit (str tracked) "user index bytes")
            _ (git dir "add" "tracked.txt")
            index (bytes-at (fs/path dir ".git/index"))
            _ (spit (str tracked) "unstaged user bytes")
            base (repo/write-commit! dir {:parents [] :files {"retained" (.getBytes "base" "UTF-8")}})
            other (repo/write-commit! dir {:parents [] :files {}})
            files {"日本語/名,前.txt" (.getBytes "本文" "UTF-8")
                   "tab\tname" (byte-array [(unchecked-byte 255) 0 42])
                   "line\nname" (byte-array 0)
                   "quote\"back\\slash" (.getBytes "quoted" "UTF-8")
                   "-option" (.getBytes "option" "UTF-8")}
            sh process/sh
            updates (atom 0)
            commit (with-redefs [process/sh (fn [opts executable command & args]
                                              (when (= command "update-index") (swap! updates inc))
                                              (apply sh opts executable command args))]
                     (repo/write-commit! dir {:parents [base other] :base-tree-of base
                                              :files files :message "exact parents and files"}))
            tree-entries (->> (str/split (:out (process/sh {:dir (str dir) :out :string}
                                                           "git" "ls-tree" "-rz" commit)) #"\u0000")
                              (map (fn [entry]
                                     (let [[metadata path] (str/split entry #"\t" 2)]
                                       [path (last (str/split metadata #" "))])))
                              (into {}))]
        (is (= 1 @updates) "index work is one batch, independent of file count")
        (is (= (conj (set (keys files)) "retained") (set (keys tree-entries))))
        (doseq [[path expected] (assoc files "retained" (.getBytes "base" "UTF-8"))]
          (is (= (vec expected) (read-blob dir (get tree-entries path))) path))
        (is (= [commit base other] (str/split (git dir "rev-list" "--parents" "-n" "1" commit) #" ")))
        (is (= index (bytes-at (fs/path dir ".git/index"))))
        (is (= "unstaged user bytes" (slurp (str tracked))))
        (is (= 1 (:exit (process/sh {:dir (str dir) :out :string :err :string} "git" "rev-parse" "--verify" "--quiet" "HEAD"))))
        (testing "an empty overlay preserves the base tree"
          (let [unchanged (repo/write-commit! dir {:parents [commit] :base-tree-of commit :files {}})]
            (is (= (git dir "rev-parse" (str commit "^{tree}"))
                   (git dir "rev-parse" (str unchanged "^{tree}")))))))
      (finally (fs/delete-tree dir)))))

(deftest invalid-paths-refuse-the-write-and-clean-the-private-index
  (let [dir (fs/create-temp-dir {:prefix "repo-writer-invalid"})]
    (try
      (git dir "init" "-q")
      (let [blob (str/trim (:out (process/sh {:dir (str dir) :in (byte-array 0) :out :string}
                                             "git" "hash-object" "-w" "--stdin")))
            invalid [(str "safe\u0000100644 " blob "\tinjected")
                     "../outside" ".git/config" "/absolute" "trailing/"]]
        (doseq [path invalid]
          (let [created (atom [])
                create fs/create-temp-file]
            (with-redefs [fs/create-temp-file (fn [opts]
                                                (let [file (create opts)]
                                                  (swap! created conj file)
                                                  file))]
              (is (thrown? Exception
                           (repo/write-commit! dir {:parents [] :files {path (.getBytes "data" "UTF-8")}}))
                  (pr-str path)))
            (is (seq @created))
            (doseq [index @created]
              (is (not (fs/exists? index)))
              (is (not (fs/exists? (str index ".lock"))))))))
      (finally (fs/delete-tree dir)))))
