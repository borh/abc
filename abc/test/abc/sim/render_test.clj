(ns abc.sim.render-test
  (:require [abc.sim.model :as model]
            [abc.sim.render :as render]
            [abc.tools.aozora-csv :as ac]
            [abc.tools.person-drift-history :as drift-history]
            [clojure.test :refer [deftest is testing]]))

(deftest model-rows-roundtrip-through-parser-test
  (let [m (model/bootstrap 2)
        csv (render/rows->csv (render/model->rows m))
        rows (ac/read-rows-from-string csv)]
    (is (= 2 (count rows)))
    (is (= #{"000101" "000102"} (set (map #(get % "作品ID") rows))))
    (let [{:keys [work persons-by-id contributors]}
          (ac/build-record-fragment-from-rows
           (filter #(= "000101" (get % "作品ID")) rows))]
      (is (= "作品000101" (get work "title")))
      (is (= ["000001"] (keys persons-by-id)))
      (is (= [{"person_id" "000001" "relation_to_work" "著者"}] contributors)))))

(deftest csv-quoting-test
  (let [m (model/bootstrap 1)
        m' (:model (model/apply-event m {:event/type :edit-work :wid "000101"
                                         :field :title :value "旅,\"新\"\n行"}))
        rows (ac/read-rows-from-string (render/rows->csv (render/model->rows m')))]
    (is (= "旅,\"新\"\n行" (get (first rows) "作品名")))))

(deftest corpus-dirs-feed-drift-history-test
  (let [m (model/bootstrap 2)
        prev (render/temp-dir "sim-prev")
        cur (render/temp-dir "sim-cur")]
    (try
      (render/write-corpus-dirs! prev m)
      (render/write-corpus-dirs! cur m)
      (let [r (drift-history/report {:previous-dir (str prev) :current-dir (str cur)})]
        (is (= 0 (get-in r ["summary" "split_candidates"])))
        (is (= 2 (get-in r ["summary" "persons_previous"]))))
      (finally
        (render/delete-tree! prev)
        (render/delete-tree! cur)))))

(deftest corruption-shapes-test
  (let [m (model/bootstrap 1)
        rows (render/model->rows m)]
    (testing "cell corruption replaces a single cell"
      (let [[r] (render/corrupt-rows rows [{:corrupt/type :cell :wid "000101"
                                            :column "生年月日" :value "1900. 1. 1"}])]
        (is (= "1900. 1. 1" (get r "生年月日")))))
    (testing "ragged-short renders a cell vector shorter than the header"
      (let [[r] (render/corrupt-rows rows [{:corrupt/type :ragged-short :wid "000101"}])]
        (is (vector? r))
        (is (< (count r) (count render/headers)))))
    (testing "duplicate+divergent yields two distinct bodies for ONE person id"
      ;; proves the fault shape P8/P13 rely on actually reaches the parser
      (let [corrupted (render/corrupt-rows rows
                                           [{:corrupt/type :duplicate-row
                                             :wid "000101" :pid "000001"}
                                            {:corrupt/type :divergent-person
                                             :wid "000101" :pid "000001"
                                             :column "姓" :value "×"}])
            parsed (ac/read-rows-from-string (render/rows->csv corrupted))
            same-pid (filter #(= "000001" (get % "人物ID")) parsed)]
        (is (= 2 (count same-pid)))
        (is (= #{(get (first same-pid) "姓") "×"}
               (set (map #(get % "姓") same-pid))))
        (is (apply not= (map #(get % "姓") same-pid)))))))
