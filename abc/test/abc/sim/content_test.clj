(ns abc.sim.content-test
  "Unit tests for the content axis (model events, generators, rendering,
  selection/status oracles) added by the content-side evolution spec."
  (:require [abc.sim.gen :as sgen]
            [abc.sim.model :as model]
            [abc.sim.oracle :as oracle]
            [abc.sim.render :as render]
            [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]))

(deftest content-events-test
  (let [m0 (model/bootstrap 2)]
    (testing "bootstrap has empty contents"
      (is (= {} (:contents m0))))
    (testing "add-content applies once to an existing work"
      (let [{m1 :model a1 :applied}
            (model/apply-event m0 {:event/type :add-content :wid "000101"
                                   :text "作品000101 本文"})]
        (is (= :add-content (:intent a1)))
        (is (= "作品000101 本文" (get-in m1 [:contents "000101" :text])))
        (is (nil? (:applied (model/apply-event
                             m1 {:event/type :add-content :wid "000101"
                                 :text "作品000101 別文"}))))
        (testing "edit-content changes text; identical text no-ops"
          (let [{m2 :model a2 :applied}
                (model/apply-event m1 {:event/type :edit-content :wid "000101"
                                       :text "作品000101 改"})]
            (is (= :edit-content (:intent a2)))
            (is (= "作品000101 改" (get-in m2 [:contents "000101" :text])))
            (is (nil? (:applied (model/apply-event
                                 m2 {:event/type :edit-content :wid "000101"
                                     :text "作品000101 改"}))))))
        (testing "remove-content applies once"
          (let [{m3 :model a3 :applied}
                (model/apply-event m1 {:event/type :remove-content :wid "000101"})]
            (is (= :remove-content (:intent a3)))
            (is (not (contains? (:contents m3) "000101")))
            (is (nil? (:applied (model/apply-event
                                 m3 {:event/type :remove-content :wid "000101"}))))))
        (testing "remove-work drops the work's content"
          (let [{m4 :model} (model/apply-event
                             m1 {:event/type :remove-work :wid "000101"})]
            (is (not (contains? (:contents m4) "000101")))))))
    (testing "no-ops: missing work, blank text, text without wid, edit without content"
      (is (nil? (:applied (model/apply-event
                           m0 {:event/type :add-content :wid "999999"
                               :text "作品999999 本文"}))))
      (is (nil? (:applied (model/apply-event
                           m0 {:event/type :add-content :wid "000101" :text "  "}))))
      (is (nil? (:applied (model/apply-event
                           m0 {:event/type :add-content :wid "000101"
                               :text "本文のみ"}))))
      (is (nil? (:applied (model/apply-event
                           m0 {:event/type :edit-content :wid "000101"
                               :text "作品000101 改"})))))))

(deftest content-history-gen-shape-test
  (testing "structure: two leading adds on the two lowest bootstrap works, one edit"
    (let [{:keys [initial events]} (gen/generate (sgen/content-history-gen {}) 30 42)
          [e1 e2] events
          [w1 w2] (vec (take 2 (keys (:works initial))))
          edits (filterv #(= :edit-content (:event/type %)) events)]
      (is (= :add-content (:event/type e1)))
      (is (= :add-content (:event/type e2)))
      (is (= [w1 w2] [(:wid e1) (:wid e2)]))
      (is (= 1 (count edits)))
      (is (= w1 (:wid (first edits))))
      (is (not= (:text e1) (:text (first edits))))))
  (testing "with no benign events the seeded edit always applies"
    (let [hist (gen/generate (sgen/content-history-gen {:length [0 0] :works [2 2]}) 30 7)
          fold (model/fold-history hist)]
      (is (= 3 (count (:events hist))))
      (is (some? (sgen/find-applied fold :edit-content)))))
  (testing "the cap includes the two seeded content works"
    (doseq [seed (range 20)]
      (let [hist (gen/generate (sgen/content-history-gen
                                {:content-cap 4 :works [6 6] :length [10 10]})
                               30 seed)
            fold (model/fold-history hist)]
        (is (every? #(<= (count (:contents %)) 4) (:states fold)))))))

(defn- two-work-content-state []
  (-> (model/bootstrap 2)
      (assoc-in [:contents "000101"] {:text "作品000101 本文 春"})))

(deftest card-pid-test
  (let [m (-> (model/bootstrap 2)
              (assoc-in [:edges ["000101" "翻訳者"]] #{"000002"}))
        proj (oracle/projection m)]
    (is (= "000001" (oracle/card-pid proj "000101")))
    (is (= "000002" (oracle/card-pid proj "000102")))))

(deftest content-render-test
  (let [m (two-work-content-state)
        rows (render/model->rows m)
        row101 (first (filter #(= "000101" (get % "作品ID")) rows))
        row102 (first (filter #(= "000102" (get % "作品ID")) rows))]
    (is (some #(= "テキストファイルURL" %) render/headers))
    (is (= "https://www.aozora.gr.jp/cards/000001/files/000101_t.zip"
           (get row101 "テキストファイルURL")))
    (is (= "" (get row102 "テキストファイルURL")))))

(deftest text->zip-bytes-deterministic-test
  (let [a (render/text->zip-bytes "作品000101 本文" "000101")
        b (render/text->zip-bytes "作品000101 本文" "000101")
        c (render/text->zip-bytes "作品000101 改" "000101")]
    (is (java.util.Arrays/equals ^bytes a ^bytes b))
    (is (not (java.util.Arrays/equals ^bytes a ^bytes c)))))

(deftest write-aozora-root-test
  (let [dir (render/temp-dir "sim-aroot")]
    (try
      (let [m (two-work-content-state)]
        (render/write-aozora-root! dir m)
        (is (.isFile (io/file dir render/zip-path)))
        (is (.isFile (io/file dir "cards/000001/files/000101_t.zip")))
        (is (.isFile (io/file dir "cards/999999/files/decoy.zip")))
        (is (.isFile (io/file dir "support/tools.zip")))
        (is (= "sim-fixture-head\n" (slurp (io/file dir ".git/HEAD"))))
        (let [srcs (render/content-sources m)]
          (is (= ["000101"] (vec (keys srcs))))
          (is (= "cards/000001/files/000101_t.zip"
                 (get-in srcs ["000101" :relpath])))
          ;; the oracle pin equals the sha256 of the file actually written
          (is (= (hash/format-sha256
                  (files/sha256-file (io/file dir "cards/000001/files/000101_t.zip")))
                 (get-in srcs ["000101" :source-hash])))))
      (finally (render/delete-tree! dir)))))
