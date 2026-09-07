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

(deftest file-backed-blobs-match-byte-backed-git-objects
  (let [dir (fs/create-temp-dir {:prefix "repo-file-blobs"})]
    (try
      (git dir "init" "-q")
      (let [path (fs/path dir "payload")
            payload (byte-array [0 10 (unchecked-byte 255) 42])
            _ (with-open [out (java.io.FileOutputStream. (str path))] (.write out ^bytes payload))
            empty-path (fs/create-temp-file {:dir dir})
            files {"payload" payload "empty" (byte-array 0) "inline" (.getBytes "inline" "UTF-8")}
            bytes-commit (repo/write-commit! dir {:parents [] :files files})
            file-commit (repo/write-commit! dir {:parents [] :files (assoc files "payload" path "empty" empty-path)})]
        (is (= (git dir "rev-parse" (str bytes-commit "^{tree}"))
               (git dir "rev-parse" (str file-commit "^{tree}"))))
        (is (= (vec payload) (read-blob dir (str file-commit ":payload")))))
      (finally (fs/delete-tree dir)))))

(deftest commit-writes-preserve-paths-bytes-parents-and-user-state
  (let [dir (fs/create-temp-dir {:prefix "repo-writer"})]
    (try
      (git dir "init" "-q")
      (let [tracked (fs/path dir "tracked.txt")
            _ (spit (str tracked) "user index bytes")
            _ (git dir "add" "tracked.txt")
            index (bytes-at (fs/path dir ".git/index"))
            _ (spit (str tracked) "unstaged user bytes")
            base (repo/write-commit! dir {:parents [] :files {"retained" (.getBytes "base" "UTF-8")
                                                              "overwritten" (.getBytes "old" "UTF-8")}})
            other (repo/write-commit! dir {:parents [] :files {}})
            _ (git dir "update-ref" "refs/heads/existing" base)
            refs (git dir "for-each-ref")
            files {"日本語/名,前.txt" (.getBytes "本文" "UTF-8")
                   "duplicate.txt" (.getBytes "本文" "UTF-8")
                   "overwritten" (.getBytes "new" "UTF-8")
                   "tab\tname" (byte-array [(unchecked-byte 255) 0 42])
                   "line\nname" (byte-array 0)
                   "quote\"back\\slash" (.getBytes "quoted" "UTF-8")
                   "-option" (.getBytes "option" "UTF-8")}
            sh process/sh
            launch process/process
            updates (atom 0)
            blob-commands (atom [])
            commit (with-redefs [process/sh (fn [opts executable command & args]
                                              (when (= command "update-index") (swap! updates inc))
                                              (apply sh opts executable command args))
                                 process/process (fn [& args]
                                                   (doseq [command ["fast-import" "hash-object"]]
                                                     (when (some #{command} (flatten args))
                                                       (swap! blob-commands conj command)))
                                                   (apply launch args))]
                     (repo/write-commit! dir {:parents [base other] :base-tree-of base
                                              :files files :message "exact parents and files"}))
            tree-entries (->> (str/split (:out (process/sh {:dir (str dir) :out :string}
                                                           "git" "ls-tree" "-rz" commit)) #"\u0000")
                              (map (fn [entry]
                                     (let [[metadata path] (str/split entry #"\t" 2)]
                                       [path (last (str/split metadata #" "))])))
                              (into {}))]
        (is (= 1 @updates) "index work is one batch, independent of file count")
        (is (= ["fast-import"] @blob-commands) "blob writes use one importer and no per-blob processes")
        (is (= (conj (set (keys files)) "retained") (set (keys tree-entries))))
        (doseq [[path expected] (assoc files "retained" (.getBytes "base" "UTF-8"))]
          (is (= (vec expected) (read-blob dir (get tree-entries path))) path))
        (is (= [commit base other] (str/split (git dir "rev-list" "--parents" "-n" "1" commit) #" ")))
        (is (= index (bytes-at (fs/path dir ".git/index"))))
        (is (= "unstaged user bytes" (slurp (str tracked))))
        (is (= refs (git dir "for-each-ref")))
        (is (= 1 (:exit (process/sh {:dir (str dir) :out :string :err :string} "git" "rev-parse" "--verify" "--quiet" "HEAD"))))
        (testing "an empty overlay preserves the base tree"
          (let [unchanged (repo/write-commit! dir {:parents [commit] :base-tree-of commit :files {}})]
            (is (= (git dir "rev-parse" (str commit "^{tree}"))
                   (git dir "rev-parse" (str unchanged "^{tree}")))))))
      (finally (fs/delete-tree dir)))))

(deftest failed-blob-streams-never-write-a-commit-and-clean-temporary-files
  (let [dir (fs/create-temp-dir {:prefix "repo-writer-failure"})]
    (try
      (git dir "init" "-q")
      (doseq [failure [:producer :importer :missing-file]]
        (let [created (atom [])
              commands (atom [])
              create fs/create-temp-file
              launch process/process
              sh process/sh]
          (with-redefs [fs/create-temp-file (fn [opts]
                                              (let [file (create opts)]
                                                (swap! created conj file)
                                                file))
                        process/process (fn [args opts]
                                          (launch (if (and (= failure :importer)
                                                           (some #{"fast-import"} args))
                                                    (conj (vec args) "--invalid-import-option")
                                                    args)
                                                  opts))
                        process/sh (fn [opts executable command & args]
                                     (swap! commands conj command)
                                     (apply sh opts executable command args))]
            (is (thrown? Exception
                         (repo/write-commit! dir {:parents []
                                                  :files (cond-> {"first" (.getBytes "valid" "UTF-8")}
                                                           (= failure :producer) (assoc "second" nil)
                                                           (= failure :missing-file) (assoc "second" (fs/path dir "absent")))}))
                (name failure)))
          (is (not-any? #{"update-index" "commit-tree"} @commands))
          (is (seq @created))
          (doseq [path @created]
            (is (not (fs/exists? path)))
            (is (not (fs/exists? (str path ".lock")))))
          (is (str/blank? (git dir "for-each-ref")))))
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
