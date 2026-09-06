(ns soranoha.za.serve-test
  "Serving-tree acceptance: the exported tree derives only from a fully
  verified chain, is self-verifying (every file's bytes hash to its
  name), keeps withdrawn works served under their historical manifests,
  and exports byte-identically for the same chain. The destination must
  not yet exist, so a served path can never mix chain content with
  pre-existing bytes; an unverifiable chain exports nothing."
  (:require [babashka.fs :as fs]
            [charred.api :as json]
            [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [soranoha.snh.view :as view]
            [soranoha.core.hash :as hash]
            [soranoha.snh.fixture :as fx]
            [soranoha.snh.verify :as verify]
            [soranoha.za.serve :as serve]))

(def ^:private slug-a "hashire_merosu_000035_1567")
(def ^:private slug-b "kumo_no_ito_000879_92")

(defn- chain-with-withdrawal!
  "Two releases then a withdrawal of slug-b; returns {:clone :withdrawal}."
  []
  (let [{:keys [clone]} (fx/make-repos!)]
    (fx/publish! clone {:admitted [slug-a slug-b]})
    (fx/publish! clone {:admitted [slug-a slug-b]
                        :selection-params {"config" "fixture" "round" 2}})
    (let [withdrawal (fx/publish-event!
                      clone (fx/event-value
                             "withdrawal"
                             [{"slug" slug-b
                               "reason_code" "takedown-request"
                               "statement" "Documented request."}]))]
      {:clone clone :withdrawal withdrawal})))

(defn- export! [clone out]
  (serve/export-tree! {:clone clone
                       :branch fx/branch
                       :pinned-keys (fx/pinned-keys)
                       :out-dir (str out)}))

(defn- tree-out []
  (fs/path (fs/create-temp-dir {:prefix "za-serve"}) "tree"))

(defn- tree-bytes ^bytes [out rel]
  (fs/read-all-bytes (fs/path out rel)))

(defn- tree-map
  "Every regular file in the tree as relative-path -> byte vector."
  [out]
  (into (sorted-map)
        (for [p (fs/glob out "**")
              :when (fs/regular-file? p)]
          [(str (fs/relativize out p)) (vec (fs/read-all-bytes p))])))

(deftest serving-tree-derives-from-the-verified-chain
  (let [{:keys [clone withdrawal]} (chain-with-withdrawal!)
        out (tree-out)
        result (export! clone out)
        manifest-at (fn [hex]
                      (json/read-json
                       (String. (tree-bytes out (verify/manifest-path hex))
                                "UTF-8")))
        zero-genesis (apply str (repeat 64 "0"))
        chain (loop [hex (:head result) acc []]
                (if (= zero-genesis hex)
                  acc
                  (recur (get (manifest-at hex) "prev_manifest")
                         (conj acc hex))))]
    (is (= 3 (:releases result)))

    (testing "releases/HEAD names the head"
      (is (= (str (:head result) "\n")
             (String. (tree-bytes out verify/head-path) "UTF-8"))))

    (testing "the served manifests link head-first back to genesis"
      (is (= 3 (count chain)))
      (is (= [{"works" 1 "withdrawn" 1} {"works" 2 "withdrawn" 0}
              {"works" 2 "withdrawn" 0}]
             (mapv (fn [hex]
                     (let [manifest (manifest-at hex)]
                       {"works" (count (get manifest "works"))
                        "withdrawn" (count (get manifest "withdrawn"))}))
                   chain)))
      (is (some? (get (manifest-at (:head result)) "governance_event"))))

    (testing "every exported manifest and blob is self-verifying"
      (doseq [rel (map str (fs/glob out "releases/*.json"))
              :let [hex (fs/strip-ext (fs/file-name rel))]]
        (is (= hex (hash/sha256-bytes (fs/read-all-bytes rel)))))
      (doseq [rel (map str (fs/glob out "blobs/sha256/**"))
              :when (fs/regular-file? rel)]
        (is (= (fs/file-name rel)
               (hash/sha256-bytes (fs/read-all-bytes rel))))))

    (testing "the withdrawn work stays served under its historical manifest"
      (let [genesis (manifest-at (last chain))
            withdrawn-artifacts (for [work (get genesis "works")
                                      :when (= slug-b (get work "slug"))
                                      artifact (get work "artifacts")]
                                  (verify/id->hex (get artifact "id")))]
        (is (= 3 (count withdrawn-artifacts)))
        (doseq [hex withdrawn-artifacts]
          (is (fs/exists? (fs/path out (verify/blob-path hex)))))))

    (testing "the work-facing layer names only the current corpus"
      (is (fs/sym-link? (fs/path out "releases/latest")))
      (is (= [slug-a]
             (mapv fs/file-name (fs/list-dir (fs/path out "works")))))
      (is (fs/sym-link? (fs/path out "withdrawn" (str slug-b ".json")))))

    (testing "the governing event and its signature are served"
      (let [event-hex (verify/id->hex (:event withdrawal))]
        (is (fs/exists? (fs/path out (verify/event-path event-hex))))
        (is (= 64 (count (tree-bytes out (verify/event-sig-path
                                          event-hex)))))))

    (testing "an export of the same chain is byte-identical"
      (let [out-2 (tree-out)]
        (is (= result (export! clone out-2)))
        (is (= (tree-map out) (tree-map out-2)))))

    (testing "an existing destination is refused, never merged into"
      (let [reason (try (export! clone out)
                        nil
                        (catch clojure.lang.ExceptionInfo e
                          (:reason (ex-data e))))]
        (is (= :destination-exists reason))))))

(deftest a-preexisting-directory-cannot-contribute-bytes-to-a-served-tree
  ;; closure: everything under a served tree is chain content, so a
  ;; destination that already holds bytes is refused outright rather
  ;; than exported over
  (let [{:keys [clone]} (chain-with-withdrawal!)
        out (tree-out)
        stray (fs/path out (verify/blob-path (apply str (repeat 64 "e"))))]
    (fs/create-dirs (fs/parent stray))
    (fs/write-bytes stray (.getBytes "not chain content" "UTF-8"))
    (let [reason (try (export! clone out)
                      nil
                      (catch clojure.lang.ExceptionInfo e
                        (:reason (ex-data e))))]
      (is (= :destination-exists reason))
      (is (not (fs/exists? (fs/path out verify/head-path))))
      (is (= "not chain content" (String. ^bytes (fs/read-all-bytes stray) "UTF-8")))
      (is (= [(str (fs/file-name out))]
             (map fs/file-name (fs/list-dir (fs/parent out))))
          "the refused export leaves no staging residue"))))

(deftest an-export-owns-only-its-own-staging-directory
  ;; sibling directories — including one that happens to carry a
  ;; staging-like name — belong to whoever created them, not to the
  ;; exporter's cleanup
  (let [{:keys [clone]} (chain-with-withdrawal!)
        out (tree-out)
        foreign (fs/path (str out ".staging") "valuable")]
    (fs/create-dirs (fs/parent foreign))
    (fs/write-bytes foreign (.getBytes "irreplaceable" "UTF-8"))
    (let [result (export! clone out)]
      (is (= 3 (:releases result)))
      (is (fs/exists? (fs/path out verify/head-path)))
      (is (= "irreplaceable" (String. ^bytes (fs/read-all-bytes foreign) "UTF-8")))
      (is (= #{(str (fs/file-name out)) (str (fs/file-name out) ".staging")}
             (set (map fs/file-name (fs/list-dir (fs/parent out)))))
          "the export's own staging directory is gone; the foreign one stays"))))

(deftest an-unverifiable-chain-exports-nothing
  (let [{:keys [clone]} (chain-with-withdrawal!)
        out (fs/path (fs/create-temp-dir {:prefix "za-serve-refuse"})
                     "tree")
        pinned (fx/pinned-keys)
        reason (try (serve/export-tree!
                     {:clone clone
                      :branch fx/branch
                      ;; swapped roles form a valid configuration whose
                      ;; signatures cannot verify
                      :pinned-keys {:release (:governance pinned)
                                    :governance (:release pinned)}
                      :out-dir (str out)})
                    nil
                    (catch clojure.lang.ExceptionInfo e
                      (:reason (ex-data e))))]
    (is (= :signature-invalid reason))
    (is (not (fs/exists? out)))
    (is (empty? (fs/list-dir (fs/parent out))))))

(deftest exporter-batch-failures-install-nothing-and-remove-owned-staging
  (let [{:keys [clone]} (chain-with-withdrawal!)
        verify-at verify/verify-repository-at
        batch view/with-batch
        read-at view/read-at]
    (doseq [failure [:first-manifest-read :blob-read :batch-termination]]
      (let [out (tree-out)
            exporting? (atom false)
            batch-starts (atom 0)]
        (with-redefs [verify/verify-repository-at
                      (fn [v commit pins]
                        (let [result (verify-at v commit pins)]
                          (reset! exporting? true)
                          result))
                      view/with-batch
                      (fn [v f]
                        (if @exporting?
                          (do (swap! batch-starts inc)
                              (let [result (batch v f)]
                                (is (not (fs/exists? out)) "batch must terminate before installation")
                                (when (= failure :batch-termination)
                                  (throw (ex-info "batch did not terminate cleanly" {:exit 1})))
                                result))
                          (batch v f)))
                      view/read-at
                      (fn [v commit path]
                        (when @exporting?
                          (is (some? (:batch v)) "every export read uses the hardened batch view")
                          (when (or (= failure :first-manifest-read)
                                    (and (= failure :blob-read) (str/starts-with? path "blobs/")))
                            (throw (ex-info "injected export read failure" {:path path}))))
                        (read-at v commit path))]
          (is (thrown? clojure.lang.ExceptionInfo (export! clone out)) (name failure)))
        (is (= 1 @batch-starts))
        (is (not (fs/exists? out)))
        (is (empty? (fs/list-dir (fs/parent out))) "failed export leaves no owned staging residue")))))

(deftest failed-install-preserves-a-concurrently-created-destination
  (let [{:keys [clone]} (chain-with-withdrawal!)
        out (tree-out)
        verified? (atom false)
        verify-at verify/verify-repository-at
        batch view/with-batch]
    (with-redefs [verify/verify-repository-at
                  (fn [v commit pins]
                    (let [result (verify-at v commit pins)]
                      (reset! verified? true)
                      result))
                  view/with-batch
                  (fn [v f]
                    (let [exporting? @verified?
                          result (batch v f)]
                      (when exporting?
                        (fs/create-dirs out)
                        (spit (str (fs/path out "foreign")) "foreign bytes"))
                      result))]
      (is (thrown? java.nio.file.FileSystemException (export! clone out))))
    (is (= "foreign bytes" (slurp (str (fs/path out "foreign")))))
    (is (= ["tree"] (mapv fs/file-name (fs/list-dir (fs/parent out)))))))
