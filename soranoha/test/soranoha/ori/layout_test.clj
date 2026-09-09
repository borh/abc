(ns soranoha.ori.layout-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is]]
            [clojure.string :as string]
            [soranoha.ori.fixture :as fixture]
            [soranoha.ori.render :as render]
            [soranoha.ori.tei :as tei]
            [soranoha.ori.relaxng :as rng]))

(defn- document [blocks]
  {"nodes" (mapv #(hash-map "type" "text" "text" %) ["前" "RESTAURANT" "西洋料理店" "WILDCAT HOUSE" "山猫軒" "後"])
   "paragraphs" (mapv #(hash-map "role" "body" "node_range" {"start" % "end" (inc %)}) (range 6))
   "layout_blocks" blocks})

(defn- block [start end]
  {"node_range" {"start" start "end" end} "indent" 4
   "direction" "horizontal" "align" "center"
   "typography" {"kind" "keigakomi" "border" "rule" "source" "aat-block"}
   "source_pointer" "blocks[1]"})

(defn- framed-scopes [body]
  (filter #(and (vector? %) (= :div (first %))
                (string/includes? (get-in % [1 :rend] "") "keigakomi"))
          (tree-seq vector? seq body)))

(defn- rendered-paragraphs [body]
  (filter #(and (vector? %) (= :p (first %))) (tree-seq vector? seq body)))

(deftest one-layout-scope-encloses-all-and-only-its-paragraphs
  (let [body (:body (tei/render (document [(block 1 5)])))
        scope (first (framed-scopes body))]
    (is (= :text (first body)))
    (is (= [:p "前"] (first (rendered-paragraphs body))))
    (is (not-any? #(and (vector? %) (= :floatingText (first %))) (tree-seq vector? seq body)))
    (is (= [[:p "RESTAURANT"] [:p "西洋料理店"] [:p "WILDCAT HOUSE"] [:p "山猫軒"]]
           (rendered-paragraphs scope)))
    (is (= [:p "後"] (last (rendered-paragraphs body))))))

(deftest nested-scopes-remain-nested-and-crossing-scopes-are-refused
  (let [body (:body (tei/render (document [(block 1 5) (block 2 4)])))
        [outer inner] (framed-scopes body)]
    (is (= 2 (count (framed-scopes body))))
    (is (some #{inner} (tree-seq vector? seq outer)))
    (is (= [[:p "西洋料理店"] [:p "WILDCAT HOUSE"]] (rendered-paragraphs inner))))
  (doseq [blocks [[(block 1 4) (block 2 5)] [(block 2 7)] [(block 3 3)]]]
    (is (thrown? clojure.lang.ExceptionInfo (tei/render (document blocks))))))

(deftest coincident-ranges-retain-source-containment-order
  (let [inner (dissoc (assoc (block 1 5) "indent" 2) "typography" "direction" "align")
        outer (dissoc (assoc (block 1 5) "indent" 4) "typography" "direction" "align")
        body (:body (tei/render (document [inner outer])))]
    (is (= "padding-inline-start: 4em" (get-in body [1 2 1 :style])))
    (is (= "padding-inline-start: 2em" (get-in body [1 2 2 1 :style])))))

(deftest layout-scope-cannot-bisect-a-source-paragraph
  (let [ir (assoc (document [(block 1 3)])
                  "paragraphs" [{"role" "body" "node_range" {"start" 0 "end" 2}}
                                {"role" "body" "node_range" {"start" 2 "end" 6}}])]
    (is (thrown? clojure.lang.ExceptionInfo (tei/render ir)))))

(deftest layout-scopes-allow-surrounding-prose-in-the-publication-profile
  (doseq [scope [(block 1 5) (dissoc (block 1 5) "direction" "align" "typography")]
          heading? [false true]
          boundary [nil {"type" "heading" "text" "次章" "level" 2}
                    {"type" "source-note" "note_type" "source-attribution" "text" "底本：本" "placement" "back"}
                    {"type" "source-note" "note_type" "source-attribution" "text" "序文" "placement" "front"}]]
    (let [dir (fs/create-temp-dir {:prefix "embedded-sign"})
          xml-path (str (fs/path dir "tei.xml"))
          ir (cond-> (document [scope])
               heading? (update "nodes" #(into [{"type" "heading" "text" "章" "level" 2}] %))
               heading? (update "paragraphs" #(mapv (fn [paragraph]
                                                      (-> paragraph
                                                          (update-in ["node_range" "start"] inc)
                                                          (update-in ["node_range" "end"] inc))) %))
               heading? (update "layout_blocks" #(mapv (fn [block]
                                                         (-> block
                                                             (update-in ["node_range" "start"] inc)
                                                             (update-in ["node_range" "end"] inc))) %))
               boundary (update "nodes" conj boundary))
          result (render/render-work
                  {:rights @fixture/grant
                   :parser-ir ir
                   :metadata-record {"work" {"title" "試験" "aozora_modified" "2026-09-06"} "contributors" []}
                   :persons-by-id {}})]
      (try
        (spit xml-path (:tei result))
        (is (empty? (:violations (rng/validate! {:schema-path "schemas/tei-profile.rng"
                                                 :xml-path xml-path :label "embedded sign"}))))
        (finally (fs/delete-tree dir))))))

(deftest layout-block-preserves-normal-headings-within-its-source-extent
  (let [ir {"nodes" [{"type" "heading" "text" "A" "level" 2}
                     {"type" "text" "text" "前"} {"type" "text" "text" "札1"}
                     {"type" "heading" "text" "B" "level" 2}
                     {"type" "text" "text" "札2"} {"type" "text" "text" "後"}]
            "paragraphs" (mapv #(hash-map "role" "body" "node_range" {"start" % "end" (inc %)}) [1 2 4 5])}
        body (:body (tei/render (assoc ir "layout_blocks" [(block 2 5)])))
        scope (first (framed-scopes body))]
    (is (some #{[:head {:n "2"} "B"]} (tree-seq vector? seq scope)))
    (is (not-any? #{[:head {:n "2"} "A"]} (tree-seq vector? seq scope)))
    (is (some #{[:p "札1"]} (tree-seq vector? seq scope)))
    (is (some #{[:p "札2"]} (tree-seq vector? seq scope))))
  (let [ir (document [(block 2 6)])
        nodes (into [{"type" "heading" "text" "章" "level" 2}] (get ir "nodes"))
        paragraphs (mapv #(update % "node_range" (fn [r] (update (update r "start" inc) "end" inc)))
                         (get ir "paragraphs"))
        body (:body (tei/render (assoc ir "nodes" nodes "paragraphs" paragraphs)))]
    (is (= 1 (count (framed-scopes body))))
    (is (= [[:p "RESTAURANT"] [:p "西洋料理店"] [:p "WILDCAT HOUSE"] [:p "山猫軒"]]
           (rendered-paragraphs (first (framed-scopes body)))))))

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

(deftest enclosing-indent-and-local-closing-alignment-remain-independent
  (let [scope {"node_range" {"start" 1 "end" 3} "indent" 2 "source_pointer" "blocks[1]"}
        ir (-> (document [scope])
               (assoc-in ["paragraphs" 2 "layout"]
                         {"kind" "chitsuki" "source" "aat-style" "align" "right" "offset_from_end" 2}))
        body (:body (tei/render ir))
        enclosure (get-in body [1 2])]
    (is (= [:p "前"] (get-in body [1 1])))
    (is (= :div (first enclosure)))
    (is (= {:type "layout" :style "padding-inline-start: 2em"} (second enclosure)))
    (is (= [:p "RESTAURANT"] (get enclosure 2)))
    (is (= "chitsuki align(right) offset-from-end(2)" (get-in enclosure [3 1 :rend])))
    (is (= [:p "WILDCAT HOUSE"] (get-in body [1 3 1])))
    (let [changed (:body (tei/render (assoc-in ir ["paragraphs" 2 "layout" "offset_from_end"] 3)))]
      (is (= (second enclosure) (get-in changed [1 2 1])))
      (is (= "chitsuki align(right) offset-from-end(3)" (get-in changed [1 2 3 1 :rend]))))))

(deftest supplied-line-geometry-keeps-independent-axes
  (let [scope {"node_range" {"start" 1 "end" 5} "source_pointer" "blocks[1]"
               "indent" 6 "continuation_indent" 7 "width" 21 "line_count" 2
               "offset_from_end" 3 "page_placement" "horizontal-center"}
        body (:body (tei/render (document [scope])))
        attrs (get-in body [1 2 1])]
    (is (= "padding-inline-start: 7em; text-indent: -1em; padding-inline-end: 3em; inline-size: 21em"
           (:style attrs)))
    (is (= "page-horizontal-center line-count(2)" (:rend attrs)))
    (is (= [:p "前"] (get-in body [1 1])))
    (is (some #{[:p "後"]} (tree-seq vector? seq (get-in body [1 3]))))))

(deftest supplied-frame-kind-and-typeface-share-a-scope
  (doseq [[border style] [["rule" "solid"] ["dashed-rule" "dashed"] ["unspecified" nil]]]
    (let [scope (assoc (block 1 5) "typography"
                       [{"kind" "keigakomi" "border" border "source" "aat-block"}
                        {"kind" "emphasis" "style" "gothic" "source" "aat-block"}])
          body (:body (tei/render (document [scope])))
          frame (first (framed-scopes body))]
      (is (if style
            (string/includes? (get-in frame [1 :style]) (str "border-style: " style))
            (not (string/includes? (get-in frame [1 :style]) "border-style"))))
      (is (string/includes? (get-in frame [1 :rend]) (str "border(" border ")")))
      (is (string/includes? (get-in frame [1 :rend]) "gothic"))
      (is (= 4 (count (rendered-paragraphs frame)))))))

(deftest a-layout-scope-over-only-a-heading-keeps-the-heading-inside-it
  ;; The shape a heading produces when the source opens a width and a hanging
  ;; indent inside the heading scope and closes them inside it: two scopes over
  ;; the heading node alone, written innermost first, as the converter emits
  ;; them from the nested containers. Both endpoints fall on paragraph
  ;; boundaries, so these are structural scopes and render as divisions around
  ;; the head rather than as segments inside it.
  (let [ir {"nodes" [{"type" "text" "text" "前"}
                     {"type" "heading" "text" "章" "level" 2 "style" "normal"}
                     {"type" "text" "text" "後"}]
            "paragraphs" [{"role" "body" "node_range" {"start" 0 "end" 1}}
                          {"role" "body" "node_range" {"start" 2 "end" 3}}]
            "layout_blocks" [{"node_range" {"start" 1 "end" 2} "indent" 2
                              "continuation_indent" 3
                              "source_pointer" "blocks[0].children[0]"}
                             {"node_range" {"start" 1 "end" 2} "width" 12
                              "source_pointer" "blocks[0]"}]}
        body (:body (tei/render ir))
        scopes (filterv #(and (vector? %) (= :div (first %))
                              (= "layout" (get-in % [1 :type])))
                        (tree-seq vector? seq body))]
    ;; Source containment order is kept: the width the source opened first is
    ;; the outer scope, and the indent it opened inside that is the inner one.
    (is (= 2 (count scopes)) (pr-str body))
    (is (= "inline-size: 12em" (get-in scopes [0 1 :style])))
    (is (= "padding-inline-start: 3em; text-indent: -1em" (get-in scopes [1 1 :style])))
    ;; The heading is inside both and is still a head, not a paragraph.
    (is (some #{[:head {:n "2"} "章"]} (tree-seq vector? seq (second scopes))))
    (is (not-any? #{[:p "章"]} (tree-seq vector? seq body)))
    ;; The prose on either side stays outside the scopes.
    (is (not-any? #{[:p "前"] [:p "後"]} (tree-seq vector? seq (first scopes))))))

(deftest a-layout-scope-over-only-a-heading-satisfies-the-publication-profile
  (let [ir {"nodes" [{"type" "text" "text" "前"}
                     {"type" "heading" "text" "章" "level" 2 "style" "normal"}
                     {"type" "text" "text" "後"}]
            "paragraphs" [{"role" "body" "node_range" {"start" 0 "end" 1}}
                          {"role" "body" "node_range" {"start" 2 "end" 3}}]
            "layout_blocks" [{"node_range" {"start" 1 "end" 2} "indent" 2
                              "continuation_indent" 3
                              "source_pointer" "blocks[0].children[0]"}
                             {"node_range" {"start" 1 "end" 2} "width" 12
                              "source_pointer" "blocks[0]"}]}
        dir (fs/create-temp-dir {:prefix "heading-scope"})
        xml-path (str (fs/path dir "tei.xml"))
        result (render/render-work
                {:rights @fixture/grant
                 :parser-ir ir
                 :metadata-record {"work" {"title" "試験" "aozora_modified" "2026-09-06"} "contributors" []}
                 :persons-by-id {}})]
    (try
      (spit xml-path (:tei result))
      (is (empty? (:violations (rng/validate! {:schema-path "schemas/tei-profile.rng"
                                               :xml-path xml-path :label "heading scope"}))))
      (finally (fs/delete-tree dir)))))
