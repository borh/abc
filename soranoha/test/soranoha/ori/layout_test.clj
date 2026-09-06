(ns soranoha.ori.layout-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is]]
            [soranoha.ori.render :as render]
            [soranoha.ported.parser-ir-tei :as tei]
            [soranoha.ported.tei :as rng]))

(defn- document [blocks]
  {"nodes" (mapv #(hash-map "type" "text" "text" %) ["前" "RESTAURANT" "西洋料理店" "WILDCAT HOUSE" "山猫軒" "後"])
   "paragraphs" (mapv #(hash-map "role" "body" "node_range" {"start" % "end" (inc %)}) (range 6))
   "layout_blocks" blocks})

(defn- block [start end]
  {"paragraph_range" {"start" start "end" end} "indent" 4
   "direction" "horizontal" "align" "center" "border" "solid" "source_pointer" "blocks[1]"})

(deftest one-layout-scope-encloses-all-and-only-its-paragraphs
  (let [body (:body (tei/render (document [(block 1 5)])))]
    (is (= :text (first body)))
    (is (= [:p "前"] (get-in body [1 1])))
    (is (= :floatingText (get-in body [1 2 0])))
    (is (= [[:p "RESTAURANT"] [:p "西洋料理店"] [:p "WILDCAT HOUSE"] [:p "山猫軒"]]
           (subvec (get-in body [1 2 2]) 1)))
    (is (= [:p "後"] (get-in body [1 3])))))

(deftest nested-scopes-remain-nested-and-crossing-scopes-are-refused
  (let [body (:body (tei/render (document [(block 1 5) (block 2 4)])))]
    (is (= :floatingText (get-in body [1 2 0])))
    (is (= :floatingText (get-in body [1 2 2 2 0])))
    (is (= [[:p "西洋料理店"] [:p "WILDCAT HOUSE"]] (subvec (get-in body [1 2 2 2 2]) 1))))
  (doseq [blocks [[(block 1 4) (block 2 5)] [(block 2 7)] [(block 3 3)]]]
    (is (thrown? clojure.lang.ExceptionInfo (tei/render (document blocks))))))

(deftest embedded-sign-allows-surrounding-prose-in-the-publication-profile
  (let [dir (fs/create-temp-dir {:prefix "embedded-sign"})
        xml-path (str (fs/path dir "tei.xml"))
        result (render/render-work
                {:parser-ir (assoc (document [(block 1 5)]) "sentence_segmentation" {})
                 :metadata-record {"work" {"title" "試験" "aozora_modified" "2026-09-06"} "contributors" []}
                 :persons-by-id {}})]
    (try
      (spit xml-path (:tei result))
      (is (empty? (:violations (rng/validate! {:schema-path "../abc/schemas/tei-profile.rng"
                                               :xml-path xml-path :label "embedded sign"}))))
      (finally (fs/delete-tree dir)))))

(deftest layout-block-cannot-cross-a-division-changing-gap
  (let [ir {"nodes" [{"type" "heading" "text" "A" "level" 2}
                     {"type" "text" "text" "前"} {"type" "text" "text" "札1"}
                     {"type" "heading" "text" "B" "level" 2}
                     {"type" "text" "text" "札2"} {"type" "text" "text" "後"}]
            "paragraphs" (mapv #(hash-map "role" "body" "node_range" {"start" % "end" (inc %)}) [1 2 4 5])}]
    (is (thrown? clojure.lang.ExceptionInfo (tei/render (assoc ir "layout_blocks" [(block 1 3)])))))
  (let [ir (document [(block 1 5)])
        nodes (into [{"type" "heading" "text" "章" "level" 2}] (get ir "nodes"))
        paragraphs (mapv #(update % "node_range" (fn [r] (update (update r "start" inc) "end" inc)))
                         (get ir "paragraphs"))
        body (:body (tei/render (assoc ir "nodes" nodes "paragraphs" paragraphs)))]
    (is (= :floatingText (get-in body [1 1 3 0])))
    (is (= [[:p "RESTAURANT"] [:p "西洋料理店"] [:p "WILDCAT HOUSE"] [:p "山猫軒"]]
           (subvec (get-in body [1 1 3 2]) 1)))))

(deftest same-gaiji-reference-cannot-alias-different-source-glyphs
  (let [known {"type" "gaiji" "span" {"start" 15308 "end" 15311}
               "gaiji" {"reference" "264-7" "unicode" "糸" "raw_marker" "糸＋率"}}
        unresolved {"type" "gaiji" "span" {"start" 15314 "end" 15329}
                    "gaiji" {"reference" "264-7" "unicode" nil "raw_marker" "てへん＋全"}}
        ir {"nodes" [known unresolved (assoc unresolved "span" {"start" 15330 "end" 15345})]}
        result (tei/render ir)]
    (is (= [{:xml-id "gaiji-264-7" :unicode "糸" :raw-marker "糸＋率"}
            {:xml-id "gaiji-264-7-at-15314" :unicode nil :raw-marker "てへん＋全"}]
           (:char_declarations result)))
    (is (= [:p [:g {:ref "#gaiji-264-7"} "糸"]
            [:g {:ref "#gaiji-264-7-at-15314"}] [:g {:ref "#gaiji-264-7-at-15314"}]]
           (get-in result [:body 1 1])))
    (is (= [{:xml-id "gaiji-264-7" :unicode "糸" :raw-marker "糸＋率"}]
           (:char_declarations (tei/render {"nodes" [known known]}))))))
