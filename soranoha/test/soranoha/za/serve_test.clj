(ns soranoha.za.serve-test
  "Serving-tree acceptance: the exported tree derives only from a fully
  verified chain, is self-verifying (every file's bytes hash to its
  name), keeps withdrawn works served under their historical manifests,
  and re-exports byte-identically. An unverifiable chain exports
  nothing."
  (:require [babashka.fs :as fs]
            [charred.api :as json]
            [clojure.test :refer [deftest is testing]]
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

(defn- tree-bytes ^bytes [out rel]
  (fs/read-all-bytes (fs/path out rel)))

(deftest serving-tree-derives-from-the-verified-chain
  (let [{:keys [clone withdrawal]} (chain-with-withdrawal!)
        out (fs/create-temp-dir {:prefix "za-serve"})
        result (export! clone out)
        history (json/read-json (String. (tree-bytes out "history.json")
                                         "UTF-8"))]
    (is (= 3 (:releases result)))
    (is (= (:head result) (get history "head")))

    (testing "history is the verified chain, head-first, with prev links"
      (let [releases (get history "releases")]
        (is (= 3 (count releases)))
        (is (= (:head result) (get (first releases) "manifest_id")))
        (is (= (mapv #(get % "manifest_id") (rest releases))
               (mapv #(get % "prev_manifest") (butlast releases))))
        (is (= [{"works" 1 "withdrawn" 1} {"works" 2 "withdrawn" 0}
                {"works" 2 "withdrawn" 0}]
               (mapv (fn [r] {"works" (get r "work_count")
                              "withdrawn" (get r "withdrawn_count")})
                     releases)))
        (is (some? (get (first releases) "governance_event")))))

    (testing "releases/HEAD names the head"
      (is (= (str (:head result) "\n")
             (String. (tree-bytes out verify/head-path) "UTF-8"))))

    (testing "every exported manifest and blob is self-verifying"
      (doseq [rel (map str (fs/glob out "releases/*.json"))
              :let [hex (fs/strip-ext (fs/file-name rel))]]
        (is (= hex (hash/sha256-bytes (fs/read-all-bytes rel)))))
      (doseq [rel (map str (fs/glob out "blobs/sha256/**"))
              :when (fs/regular-file? rel)]
        (is (= (fs/file-name rel)
               (hash/sha256-bytes (fs/read-all-bytes rel))))))

    (testing "the withdrawn work stays served under its historical manifest"
      (let [genesis-hex (get (last (get history "releases")) "manifest_id")
            genesis (json/read-json
                     (String. (tree-bytes out (verify/manifest-path
                                               genesis-hex))
                              "UTF-8"))
            withdrawn-artifacts (for [work (get genesis "works")
                                      :when (= slug-b (get work "slug"))
                                      artifact (get work "artifacts")]
                                  (verify/id->hex (get artifact "id")))]
        (is (= 3 (count withdrawn-artifacts)))
        (doseq [hex withdrawn-artifacts]
          (is (fs/exists? (fs/path out (verify/blob-path hex)))))))

    (testing "the governing event and its signature are served"
      (let [event-hex (verify/id->hex (:event withdrawal))]
        (is (fs/exists? (fs/path out (verify/event-path event-hex))))
        (is (= 64 (count (tree-bytes out (verify/event-sig-path
                                          event-hex)))))))

    (testing "a re-export writes identical bytes"
      (let [before (vec (tree-bytes out "history.json"))]
        (is (= result (export! clone out)))
        (is (= before (vec (tree-bytes out "history.json"))))))))

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
    (is (not (fs/exists? out)))))
