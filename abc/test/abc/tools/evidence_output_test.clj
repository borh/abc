(ns abc.tools.evidence-output-test
  (:require [abc.tools.evidence-output :as output]
            [abc.tools.json :as json]
            [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio ByteBuffer]
           [java.nio.channels FileChannel]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util UUID]))

(defn- temp-dir [prefix]
  (fs/file (fs/create-temp-dir {:prefix prefix})))

(defn- layout []
  (let [root (temp-dir "evidence-output-test-")
        workspace (fs/path root "workspace")
        repo (fs/path workspace "abc")
        staging (fs/path root "staging")]
    (doseq [directory [repo staging]]
      (fs/create-dirs directory))
    {:root root
     :repo-root repo
     :workspace-root workspace
     :staging-root staging
     :output (fs/path staging "bundle.json")}))

(defn- sibling-names [staging]
  (->> (fs/list-dir staging)
       (map (comp str fs/file-name))
       set))

(defn- problem-kind [thunk]
  (try
    (thunk)
    nil
    (catch clojure.lang.ExceptionInfo exception
      (:kind (ex-data exception)))))

(deftest validated-destination-is-pure-and-canonical-test
  (let [{:keys [staging-root output] :as paths} (layout)
        destination (output/validated-destination paths)]
    (is (= (fs/file (fs/canonicalize staging-root))
           (:staging-root destination)))
    (is (= (fs/file (fs/path (fs/canonicalize staging-root) "bundle.json"))
           (:output destination)))
    (is (not (fs/exists? output)))
    (is (empty? (sibling-names staging-root)))))

(deftest staging-root-must-not-overlap-identity-trees-test
  (let [{:keys [root repo-root workspace-root] :as paths} (layout)]
    (doseq [staging [repo-root
                     (fs/path repo-root "generated")
                     workspace-root
                     (fs/path workspace-root "generated")
                     root]]
      (fs/create-dirs staging)
      (testing (str staging)
        (is (= :invalid-evidence-destination
               (problem-kind
                #(output/validated-destination
                  (assoc paths
                         :staging-root staging
                         :output (fs/path staging "bundle.json"))))))))))

(deftest output-must-be-a-direct-missing-child-test
  (let [{:keys [staging-root output] :as paths} (layout)
        indirect (fs/path staging-root "nested" "bundle.json")]
    (fs/create-dirs (fs/parent indirect))
    (is (= :invalid-evidence-destination
           (problem-kind #(output/validated-destination
                           (assoc paths :output indirect)))))
    (spit (fs/file output) "existing")
    (is (= :evidence-output-exists
           (problem-kind #(output/validated-destination paths))))))

(deftest output-symlink-escape-is-rejected-test
  (let [{:keys [staging-root] :as paths} (layout)
        outside (temp-dir "evidence-output-outside-")
        link (fs/path staging-root "escape")]
    (Files/createSymbolicLink link (.toPath outside)
                              (make-array FileAttribute 0))
    (is (= :invalid-evidence-destination
           (problem-kind
            #(output/validated-destination
              (assoc paths :output (fs/path link "bundle.json"))))))))

(deftest staging-symlink-cannot-escape-from-an-identity-tree-test
  (let [{:keys [repo-root staging-root] :as paths} (layout)
        link (fs/path repo-root "escaped-staging")]
    (Files/createSymbolicLink link (fs/path staging-root)
                              (make-array FileAttribute 0))
    (is (= :invalid-evidence-destination
           (problem-kind
            #(output/validated-destination
              (assoc paths
                     :staging-root link
                     :output (fs/path link "bundle.json"))))))))

(deftest serialization-failure-creates-no-output-or-sibling-test
  (let [{:keys [staging-root output] :as paths} (layout)
        destination (output/validated-destination paths)]
    (is (thrown-with-msg?
         Exception #"serialization failed"
         (with-redefs [json/write-deterministic-json-str
                       (fn [_] (throw (Exception. "serialization failed")))]
           (output/write-json-exclusive! destination {:bad true}))))
    (is (not (fs/exists? output)))
    (is (empty? (sibling-names staging-root)))))

(deftest sibling-collision-is-retried-without-replacing-collision-test
  (let [{:keys [staging-root output] :as paths} (layout)
        destination (output/validated-destination paths)
        first-id (UUID/fromString "00000000-0000-0000-0000-000000000001")
        second-id (UUID/fromString "00000000-0000-0000-0000-000000000002")
        collision (fs/path staging-root (str ".bundle.json." first-id ".tmp"))
        ids (atom [first-id second-id])]
    (spit (fs/file collision) "owned by another writer")
    (with-redefs-fn {#'output/fresh-uuid
                     (fn [] (let [id (first @ids)]
                              (swap! ids rest)
                              id))}
      #(is (= (fs/file output)
              (output/write-json-exclusive! destination {"value" 1}))))
    (is (= "owned by another writer" (slurp (fs/file collision))))
    (is (= {"value" 1} (json/read-json-file (fs/file output))))
    (is (= #{(str (fs/file-name collision)) "bundle.json"}
           (sibling-names staging-root)))))

(deftest unsupported-hard-link-fails-closed-and-cleans-sibling-test
  (let [{:keys [staging-root output] :as paths} (layout)
        destination (output/validated-destination paths)
        publication-attempts (atom 0)]
    (is (thrown? UnsupportedOperationException
                 (with-redefs-fn
                   {#'output/create-link!
                    (fn [_output _temp]
                      (swap! publication-attempts inc)
                      (throw (UnsupportedOperationException.
                              "hard links unsupported")))}
                   #(output/write-json-exclusive! destination {"value" 1}))))
    (is (= 1 @publication-attempts))
    (is (not (fs/exists? output)))
    (is (empty? (sibling-names staging-root)))))

(deftest prefix-write-failure-closes-channel-and-removes-only-owned-sibling-test
  (let [{:keys [staging-root output] :as paths} (layout)
        destination (output/validated-destination paths)
        collision-id (UUID/fromString "00000000-0000-0000-0000-000000000003")
        owned-id (UUID/fromString "00000000-0000-0000-0000-000000000004")
        collision-name (str ".bundle.json." collision-id ".tmp")
        collision (fs/path staging-root collision-name)
        ids (atom [collision-id owned-id])
        seen-channel (atom nil)
        total-bytes (atom 0)
        written (atom 0)]
    (spit (fs/file collision) "unrelated collision")
    (is (thrown-with-msg?
         Exception #"prefix write failed"
         (with-redefs-fn
           {#'output/fresh-uuid
            (fn [] (let [id (first @ids)]
                     (swap! ids rest)
                     id))
            #'output/write-buffer!
            (fn [^FileChannel channel ^ByteBuffer buffer]
              (reset! seen-channel channel)
              (reset! total-bytes (.remaining buffer))
              (.limit buffer (+ (.position buffer) 3))
              (reset! written (.write channel buffer))
              (throw (Exception. "prefix write failed")))}
           #(output/write-json-exclusive! destination {"value" "long enough"}))))
    (is (pos? @written))
    (is (< @written @total-bytes))
    (is (false? (.isOpen ^FileChannel @seen-channel)))
    (is (not (fs/exists? output)))
    (is (= #{collision-name} (sibling-names staging-root)))
    (is (= "unrelated collision" (slurp (fs/file collision))))))

(deftest force-failure-removes-complete-unpublished-sibling-test
  (let [{:keys [staging-root output] :as paths} (layout)
        destination (output/validated-destination paths)]
    (is (thrown-with-msg?
         Exception #"force failed"
         (with-redefs-fn {#'output/force-channel!
                          (fn [_channel _metadata?]
                            (throw (Exception. "force failed")))}
           #(output/write-json-exclusive! destination {"value" 1}))))
    (is (not (fs/exists? output)))
    (is (empty? (sibling-names staging-root)))))

(deftest successful-publication-is-forced-and-exclusive-test
  (let [{:keys [staging-root output] :as paths} (layout)
        destination (output/validated-destination paths)
        forced (atom [])
        original-force @#'output/force-channel!]
    (with-redefs-fn {#'output/force-channel!
                     (fn [^FileChannel channel metadata?]
                       (swap! forced conj metadata?)
                       (original-force channel metadata?))}
      #(is (= (fs/file output)
              (output/write-json-exclusive! destination {"b" 2 "a" 1}))))
    (is (= [true] @forced))
    (is (= (json/write-deterministic-json-str {"b" 2 "a" 1})
           (slurp (fs/file output))))
    (is (= #{"bundle.json"} (sibling-names staging-root)))
    (is (thrown? java.nio.file.FileAlreadyExistsException
                 (output/write-json-exclusive! destination {"replacement" true})))
    (is (= {"a" 1 "b" 2} (json/read-json-file (fs/file output))))
    (is (= #{"bundle.json"} (sibling-names staging-root)))))

(deftest writer-rejects-an-unvalidated-map-test
  (let [{:keys [staging-root output]} (layout)]
    (is (= :unvalidated-evidence-destination
           (problem-kind
            #(output/write-json-exclusive!
              {:staging-root staging-root :output output}
              {"value" 1}))))
    (is (not (fs/exists? output)))
    (is (empty? (sibling-names staging-root)))))
