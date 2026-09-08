(ns soranoha.tei-fidelity-test
  (:require [babashka.fs :as fs]
            [clojure.string :as string]
            [soranoha.ori.fixture :as fixture]
            [soranoha.ori.render :as render]
            [soranoha.ori.relaxng :as rng]
            [soranoha.ori.schematron :as schematron]
            [clojure.test :refer [deftest is testing]]
            [soranoha.ori.tei :as tei]))

(defn- elements [tree tag]
  (filter #(and (vector? %) (= tag (first %)))
          (tree-seq coll? seq tree)))

(deftest paragraph-ranges-do-not-discard-intervening-structure
  (let [rendered (tei/render
                  {"nodes" [{"type" "heading" "level" 2 "text" "一"}
                            {"type" "text" "text" "本文"}
                            {"type" "heading" "level" 2 "text" "二"}
                            {"type" "text" "text" "続き"}
                            {"type" "heading" "level" 2 "text" "三"}]
                   "paragraphs" [{"id" "p1" "role" "body" "node_range" {"start" 1 "end" 2}}
                                 {"id" "p2" "role" "body" "node_range" {"start" 3 "end" 4}}]})]
    (is (= ["一" "二" "三"] (map last (elements (:body rendered) :head))))
    (is (= 3 (get-in rendered [:node_counts "heading"])))))

(deftest layout-indentation-is-not-lexical-whitespace
  (let [rendered (tei/render
                  {"nodes" [{"type" "text" "text" "　最初。"}
                            {"type" "text" "text" "　続く　本文。"}
                            {"type" "source-note" "placement" "back"
                             "note_type" "source-attribution"
                             "text" "底本：書名　巻数\n　　　刊行日\n"}]
                   "paragraphs" [{"id" "p1" "role" "body" "node_range" {"start" 0 "end" 2}}]})
        body (:body rendered)
        paragraph (first (elements body :p))
        note (first (elements body :note))]
    (is (= "text-indent: 1em" (:style (second paragraph))))
    (is (= ["最初。" "　続く　本文。"] (drop 2 paragraph)))
    (is (= [[:seg {:type "source-line"} "底本：書名　巻数"]
            [:seg {:type "source-line" :style "padding-inline-start: 3em"} "刊行日"]]
           (vec (elements note :seg))))
    (is (= 1 (count (elements note :lb))))))

(deftest indentation-follows-empty-text-and-apparatus
  (doseq [prefix [[{"type" "text" "text" ""}]
                  [{"type" "editor-note" "note" {"category" "correction" "raw" "注記"}}
                   {"type" "text" "text" ""}]]]
    (let [nodes (into prefix [{"type" "text" "text" "　本文　中。"}])
          body (:body (tei/render
                       {"nodes" nodes
                        "paragraphs" [{"id" "p1" "role" "body"
                                       "node_range" {"start" 0 "end" (count nodes)}}]}))
          paragraph (first (elements body :p))]
      (is (= "text-indent: 1em" (:style (second paragraph))))
      (is (= "本文　中。" (last paragraph)))
      (is (= (count (filter #(= "editor-note" (get % "type")) prefix))
             (count (elements body :note)))))))

(deftest visible-nodes-stop-paragraph-indentation-search
  (doseq [prefix [{"type" "text" "text" "前"}
                  {"type" "ruby" "ruby" {"base" "前" "reading" "まえ"}}
                  {"type" "emphasis" "style" "sesame-dot"
                   "inline_children" [{"type" "text" "text" "前"}]}]]
    (let [body (:body (tei/render
                       {"nodes" [prefix {"type" "text" "text" "　本文。"}]
                        "paragraphs" [{"id" "p1" "role" "body"
                                       "node_range" {"start" 0 "end" 2}}]}))
          paragraph (first (elements body :p))]
      (is (nil? (:style (second paragraph))))
      (is (= "　本文。" (last paragraph))))))

(deftest heading-indentation-is-preserved
  (let [body (:body (tei/render {"nodes" [{"type" "heading" "level" 2 "indent" 8 "text" "一"}]}))]
    (is (= {:n "2" :style "padding-inline-start: 8em"}
           (second (first (elements body :head)))))))

(deftest gaiji-remains-inside-ruby-base
  (let [rendered (tei/render
                  {"nodes" [{"type" "ruby"
                             "ruby" {"base" "犍陀多" "reading" "かんだた"}
                             "inline_children" [{"type" "gaiji"
                                                 "gaiji" {"reference" "1-87-71" "unicode" "犍"}}
                                                {"type" "text" "text" "陀多"}]}]})
        rb (first (elements (:body rendered) :rb))]
    (is (= [:rb [:g {:ref "#gaiji-1-87-71"} "犍"] "陀多"] rb))
    (is (= "犍" (:unicode (first (:char_declarations rendered)))))))

(deftest source-layout-validates-against-the-publication-profile
  (let [dir (fs/create-temp-dir {:prefix "tei-layout-profile"})
        xml-path (str (fs/path dir "tei.xml"))
        result (render/render-work
                {:rights @fixture/grant
                 :parser-ir {"nodes" [{"type" "heading" "level" 2 "indent" 8 "text" "一"}
                                      {"type" "text" "text" "　本文。"}
                                      {"type" "source-note" "placement" "back"
                                       "note_type" "source-attribution" "text" "底本：書名\n　　　刊行日\n"}]
                             "paragraphs" [{"id" "p1" "role" "body" "node_range" {"start" 1 "end" 2}}]}
                 :metadata-record {"work" {"title" "試験" "aozora_modified" "2026-09-06"} "contributors" []}
                 :persons-by-id {}})]
    (try
      (spit xml-path (:tei result))
      (is (empty? (:violations (rng/validate! {:schema-path "schemas/tei-profile.rng"
                                               :xml-path xml-path :label "source layout"}))))
      (finally (fs/delete-tree dir)))))

(def ^:private fully-described-work
  "A work carrying every catalog field the header can express. Measured over
  the Aozora catalog, these are what make a work identifiable: author plus
  title leaves 1966 works ambiguous, and 副題, 文字遣い種別, 底本名 and 初出
  narrow that to 16."
  {"work_id" "000092"
   "title" "蜘蛛の糸"
   "title_reading" "くものいと"
   "subtitle" "副題"
   "subtitle_reading" "ふくだい"
   "original_title" "The Spider's Thread"
   "first_published" "「赤い鳥」1918(大正7)年7月"
   "ndc" "NDC 913"
   "orthographic_style" "新字新仮名"
   "aozora_modified" "2026-09-06"
   "card_url" "https://www.aozora.gr.jp/cards/000879/card92.html"
   "source_editions" [{"title" "芥川龍之介全集　第三巻"
                       "publisher" "筑摩書房"
                       "first_edition_year" "1971"}]})

(defn- render-fully-described []
  (render/render-work
   {:rights @fixture/grant
    :slug "000092_000879"
    :parser-ir {"nodes" [{"type" "text" "text" "　本文。"}]
                "paragraphs" [{"id" "p1" "role" "body" "node_range" {"start" 0 "end" 1}}]}
    :metadata-record {"work" fully-described-work "contributors" []}
    :persons-by-id {}}))

(deftest the-header-carries-what-distinguishes-one-work-from-another
  (let [tei (:tei (render-fully-described))
        has? (fn [fragment] (string/includes? tei fragment))]
    (testing "the titles that separate same-titled works"
      (is (has? "<title type=\"main\" xml:lang=\"ja\">蜘蛛の糸</title>"))
      (is (has? "<title type=\"sub\" xml:lang=\"ja\">副題</title>"))
      (is (has? "<title type=\"sub-reading\" xml:lang=\"ja-Hira\">ふくだい</title>"))
      (is (has? "<title type=\"original\">The Spider's Thread</title>")
          "no xml:lang: the catalog records no source language to assert"))

    (testing "the identifier a downloaded file has to be citable by"
      (is (has? "<idno type=\"soranoha-work-identifier\">000092_000879</idno>"))
      (is (has? "<idno type=\"aozora-work-id\">000092</idno>"))
      (is (not (string/includes? tei "aozora-work-identifier"))
          "Aozora issues the work id and the card, not the pair"))

    (testing "the Aozora card, on every work rather than only the fallback path"
      (is (has? "<idno type=\"aozora-card-url\">https://www.aozora.gr.jp/cards/000879/card92.html</idno>")))

    (testing "where the text first appeared, kept apart from what was keyed"
      (is (has? "<bibl type=\"first-publication\">「赤い鳥」1918(大正7)年7月</bibl>")))

    (testing "both classifications point at a declared taxonomy"
      (is (has? "<taxonomy xml:id=\"aozora-orthography\">"))
      (is (has? "<classCode scheme=\"#ndc\">913</classCode>"))
      (is (has? "<classCode scheme=\"#aozora-orthography\">新字新仮名</classCode>")))))

(deftest a-fully-described-header-validates-against-the-publication-profile
  (let [dir (fs/create-temp-dir {:prefix "tei-header-profile"})
        xml-path (str (fs/path dir "tei.xml"))]
    (try
      (spit xml-path (:tei (render-fully-described)))
      (is (empty? (:violations (rng/validate! {:schema-path "schemas/tei-profile.rng"
                                               :xml-path xml-path :label "full header"}))))
      (is (empty? (remove #(= :warning (:severity %))
                          (:findings (schematron/validate!
                                      {:schema-path "schemas/tei-profile.sch"
                                       :xml-path xml-path :label "full header"})))))
      (finally (fs/delete-tree dir)))))

(deftest a-header-with-only-subordinate-titles-is-refused
  ;; the main-title rule has to exclude every subordinate form, or a file
  ;; carrying only a reading or only a subtitle satisfies it
  (let [dir (fs/create-temp-dir {:prefix "tei-header-subordinate"})
        xml-path (str (fs/path dir "tei.xml"))
        tei (string/replace (:tei (render-fully-described))
                            #"<title type=\"main\"[^>]*>[^<]*</title>" "")]
    (try
      (spit xml-path tei)
      (is (= ["snh-tei-header-title"]
             (mapv :rule-id
                   (remove #(= :warning (:severity %))
                           (:findings (schematron/validate!
                                       {:schema-path "schemas/tei-profile.sch"
                                        :xml-path xml-path :label "no main title"}))))))
      (finally (fs/delete-tree dir)))))
