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

(deftest expected-selection-test
  ;; person_id follows last-row-wins over the shared row projection; the
  ;; work's rows sort by [wid relation pid] and 著者 (U+8457) sorts after
  ;; 翻訳者 (U+7FFB), so the 著者 row wins here — pid 000002 — while
  ;; card-pid stays the edge-minimum 000001.
  (let [m (-> (model/bootstrap 2)
              (assoc-in [:contents "000101"] {:text "作品000101 本文 春"})
              (assoc-in [:edges ["000101" "著者"]] #{"000002"})
              (assoc-in [:edges ["000101" "翻訳者"]] #{"000001"}))
        sources (render/content-sources m)
        sel (oracle/expected-selection (render/model->rows m) sources)]
    (is (= [{:work_id "000101"
             :person_id "000002"
             :slug "000101_000002_000101_t"
             :text_zip_relpath "cards/000001/files/000101_t.zip"
             :source_hash (get-in sources ["000101" :source-hash])}]
           (:selected sel)))
    (is (= #{["cards/999999/files/decoy.zip" "not-catalog-text-zip"]
             ["support/tools.zip" "not-under-cards-files"]
             ["index_pages/list_person_all_extended_utf8.zip" "not-under-cards-files"]}
           (:rejected sel)))))

(deftest expected-selection-order-test
  ;; selected is sorted by relpath (card-pid dir), not by wid
  (let [m (-> (model/bootstrap 3)
              (assoc-in [:contents "000101"] {:text "作品000101 本文 春"})
              (assoc-in [:contents "000103"] {:text "作品000103 本文 冬"})
              ;; move 000101's only author to pid 000003 → card dir 000003
              (assoc-in [:edges ["000101" "著者"]] #{"000003"}))
        sel (oracle/expected-selection (render/model->rows m) (render/content-sources m))]
    (is (= ["cards/000003/files/000101_t.zip" "cards/000003/files/000103_t.zip"]
           (mapv :text_zip_relpath (:selected sel))))))

(deftest expected-statuses-test
  (let [m1 (-> (model/bootstrap 3)
               (assoc-in [:contents "000101"] {:text "作品000101 本文 春"})
               (assoc-in [:contents "000102"] {:text "作品000102 本文 秋"}))
        m2 (-> m1
               (assoc-in [:contents "000101" :text] "作品000101 本文 改")
               (assoc-in [:contents "000103"] {:text "作品000103 本文 冬"}))
        sel1 (oracle/expected-selection (render/model->rows m1) (render/content-sources m1))
        sel2 (oracle/expected-selection (render/model->rows m2) (render/content-sources m2))]
    (is (= {"000101_000001_000101_t" "passed"   ;; text changed
            "000102_000002_000102_t" "reused"   ;; untouched
            "000103_000003_000103_t" "passed"}  ;; new
           (oracle/expected-statuses sel1 sel2)))
    (testing "identical states are all reused; slug change forces passed"
      (is (every? #(= "reused" %) (vals (oracle/expected-statuses sel1 sel1))))
      (let [m2' (assoc-in m1 [:edges ["000102" "著者"]] #{"000003"})
            sel2' (oracle/expected-selection (render/model->rows m2')
                                             (render/content-sources m2'))]
        ;; same bytes, but 000102's winning row pid changed → new slug → passed
        (is (= "passed" (get (oracle/expected-statuses sel1 sel2')
                             "000102_000003_000102_t")))))))
