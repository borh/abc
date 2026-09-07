(ns soranoha.annotations.tei-test
  (:require [babashka.fs :as fs]
            [charred.api :as json]
            [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [soranoha.annotations.layer :as layer]
            [soranoha.annotations.main :as main]
            [soranoha.annotations.stages :as stages]
            [soranoha.annotations.tei :as tei]
            [soranoha.annotations.view :as view]
            [soranoha.core.hash :as hash]
            [soranoha.kura.cas :as cas]
            [soranoha.kura.engine :as engine]
            [soranoha.ori.stages :as publication-stages]
            [soranoha.ori.validate :as validation])
  (:import [java.nio.file FileAlreadyExistsException]
           [org.w3c.dom Document Node NodeList]))

(defn document [body]
  (str "<a:TEI xmlns:a='http://www.tei-c.org/ns/1.0'><a:teiHeader><a:fileDesc>"
       "<a:titleStmt><a:title>試験</a:title></a:titleStmt><a:publicationStmt><a:p>試験</a:p></a:publicationStmt>"
       "<a:sourceDesc><a:p><a:idno type='aozora-work-id'>1</a:idno></a:p></a:sourceDesc></a:fileDesc>"
       "<a:encodingDesc><a:charDecl><a:char xml:id='g1'><a:desc>外字</a:desc>"
       "<a:mapping type='unicode'>犍</a:mapping></a:char></a:charDecl></a:encodingDesc>"
       "<a:profileDesc><a:langUsage><a:language ident='ja'>Japanese</a:language></a:langUsage></a:profileDesc>"
       "</a:teiHeader><a:text><a:body>" body "</a:body></a:text></a:TEI>"))

(def rich-body
  (str "\n<a:p xml:id='original'><a:ruby><a:rb><a:g ref='#g1'>犍</a:g>陀多</a:rb><a:rt>かんだた</a:rt></a:ruby>は"
       "<a:ruby><a:rb>籠</a:rb><a:rt><a:choice><a:sic>さる</a:sic><a:corr>ざる</a:corr></a:choice></a:rt></a:ruby>をさげ"
       "<a:note type='source-span'>原文の位置</a:note></a:p>\n"
       "<a:p>前　後𠮷<a:lb/>続<a:pb/>末</a:p>\n"
       "<a:floatingText><a:body>\n<a:p>看板</a:p>\n</a:body></a:floatingText>\n"
       "<a:figure><a:graphic url='image.png'/><a:figDesc>挿絵</a:figDesc></a:figure>\n"
       "<a:note type='interpretation-uncertainty' subtype='layout' n='document'>未確定</a:note>"))

(defn content-id [value]
  (hash/format-sha256 (hash/sha256-string value)))

(defn analysis [text-view producer records]
  {:layer/view-id (:view/id text-view)
   :layer/vocabulary (content-id "vocabulary")
   :layer/producer {:producer/name producer :producer/inputs {"code" (content-id producer)}}
   :layer/dependencies {}
   :layer/eligible-spans (:view/eligible-spans text-view)
   :layer/status :layer/completed
   :layer/records (mapv (fn [[id start end label]]
                          {:annotation/id id :annotation/start start :annotation/end end
                           :annotation/label label :annotation/features {"lemma" "籠"}}) records)})

(deftest body-view-has-explicit-reading-and-whitespace-policy
  (let [base (document rich-body)
        text-view (view/from-tei base)]
    (is (= "犍陀多は籠をさげ\n前　後𠮷\n続\n末\n看板\n挿絵" (:view/text text-view)))
    (is (= (:view/id text-view)
           (:view/id (view/from-tei (document (str/replace rich-body "\n" "\n    "))))))
    (is (= "ざる" (view/visible-text (-> ^Document (:view/document text-view)
                                       (.getElementsByTagNameNS view/tei-namespace "rt") (.item 1)))))
    (is (= "正形" (:view/text (view/from-tei (document "<a:p><a:choice><a:orig>原形</a:orig><a:reg>正形</a:reg></a:choice></a:p>"))))))
  (is (= "D\n\nALT\nCAP\nQ"
         (:view/text (view/from-tei (document "<a:p>D</a:p><a:pb/><a:p><a:lb/></a:p><a:figure><a:figDesc>ALT</a:figDesc></a:figure><a:figDesc>CAP</a:figDesc><a:p>Q</a:p>")))))
  (is (thrown? clojure.lang.ExceptionInfo (view/from-tei "<TEI><text><body/></text></TEI>"))))

(deftest unresolved-glyphs-are-present-and-ineligible
  (let [text-view (view/from-tei (document "<a:p>前<a:g ref='#g1'/>後</a:p>"))
        successful (analysis text-view "empty" [])]
    (is (= "前\uFFFC後" (:view/text text-view)))
    (is (= [[0 3] [6 9]] (:view/eligible-spans text-view)))
    (is (= [{:view/problem :view/unresolved-glyph :view/start 3 :view/end 6}] (:view/problems text-view)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (layer/validate-layer text-view (assoc successful :layer/eligible-spans [[0 9]]))))))

(deftest layers-bind-inputs-and-reject-invalid-ranges-and-domains
  (let [text-view (view/from-tei (document "<a:p>犍𠮷陀多</a:p>"))
        original (analysis text-view "tokenizer" [["t1" 0 7 "token"]])]
    (is (= original (layer/read-layer text-view (layer/write-layer text-view original))))
    (doseq [[name changed] [["view" (assoc original :layer/view-id (content-id "wrong"))]
                            ["split UTF-8" (assoc-in original [:layer/records 0 :annotation/end] 5)]
                            ["other domain" (assoc original :layer/status :assessment/completed)]
                            ["failed claims" (assoc original :layer/status :layer/failed)]
                            ["outside mask" (assoc original :layer/eligible-spans [[7 13]])]
                            ["duplicate record" (update original :layer/records into (:layer/records original))]]]
      (testing name (is (thrown? clojure.lang.ExceptionInfo (layer/validate-layer text-view changed)))))
    (let [new-result (assoc-in original [:layer/records 0 :annotation/label] "proper-noun")
          changed-dependency (assoc original :layer/dependencies {"sentences" (content-id "new-sentences")})
          changed-mask (assoc original :layer/eligible-spans [[0 7]])]
      (is (= (layer/input-id text-view original) (layer/input-id text-view new-result)))
      (is (not= (layer/layer-id text-view original) (layer/layer-id text-view new-result)))
      (is (not= (layer/input-id text-view original) (layer/input-id text-view changed-dependency)))
      (is (not= (layer/input-id text-view original) (layer/input-id text-view changed-mask))))))

(defn- transcription [^Node node]
  (when-not (#{"anchor" "standOff"} (view/local-name node))
    (if (= Node/TEXT_NODE (.getNodeType node))
      (.getNodeValue node)
      (let [attrs (.getAttributes node)
            attributes (when attrs (into (sorted-map)
                                         (map (fn [i] (let [attr (.item attrs i)] [(.getNodeName attr) (.getNodeValue attr)])))
                                         (range (.getLength attrs))))
            content (reduce (fn [result child]
                              (if (and (string? child) (string? (peek result)))
                                (conj (pop result) (str (peek result) child))
                                (conj result child)))
                            [] (keep transcription (view/children node)))]
        [(.getNodeName node) attributes content]))))

(deftest enrichment-preserves-rich-transcription-and-competing-layers
  (let [base (document rich-body)
        text-view (view/from-tei base)
        tokens-a (analysis text-view "segmenter"
                           (mapv (fn [index {:view/keys [start end]}]
                                   [(str "s" index) start end "segment"])
                                 (range) (:view/segments text-view)))
        tokens-b (analysis text-view "tokenizer-b" [["t1" 0 3 "token"] ["t2" 3 9 "token"]])
        entities (analysis text-view "ner" [["e1" 0 9 "person"]
                                            ["e2" (- (view/utf8-size (:view/text text-view)) 3)
                                             (view/utf8-size (:view/text text-view)) "description-token"]])
        enriched (tei/enrich base [tokens-a tokens-b entities])
        replacement (tei/enrich base [tokens-a tokens-b (assoc-in entities [:layer/records 0 :annotation/label] "character")])
        ^Document doc (:view/document (view/from-tei enriched))
        base-body (-> ^Document (:view/document text-view) (.getElementsByTagNameNS view/tei-namespace "body") (.item 0))
        body (-> doc (.getElementsByTagNameNS view/tei-namespace "body") (.item 0))
        ^NodeList groups (.getElementsByTagNameNS doc view/tei-namespace "spanGrp")]
    (is (= base (tei/enrich base [])))
    (is (= (:view/id text-view) (:view/id (view/from-tei enriched))))
    (is (= (transcription base-body) (transcription body)))
    (is (= 3 (.getLength groups)))
    (is (not= enriched replacement))
    (doseq [i [0 1]]
      (is (= (transcription (.item groups i))
             (transcription (-> (view/read-document replacement)
                                (.getElementsByTagNameNS view/tei-namespace "spanGrp") (.item i))))))
    (is (thrown? clojure.lang.ExceptionInfo (tei/enrich (document "<a:p>別の本文</a:p>") [entities])))
    (let [dir (fs/create-temp-dir {:prefix "analysis-tei"})
          file (str (fs/path dir "tei.xml"))]
      (try
        (spit file enriched)
        (let [result (validation/tei-validation-result (validation/profile-paths ".") file)]
          (is (= "passed" (get result "status")) (pr-str (get result "findings"))))
        (finally (fs/delete-tree dir))))))

(deftest file-boundary-validates-layers-and-refuses-overwrite
  (let [dir (fs/create-temp-dir {:prefix "analysis-files"})
        path #(str (fs/path dir %))
        base (document "<a:p>本文</a:p>")
        text-view (view/from-tei base)]
    (try
      (spit (path "base.xml") base)
      (spit (path "layer.json") (layer/write-layer text-view (analysis text-view "sentences" [["s1" 0 6 "sentence"]])))
      (is (= "written" (get (main/export-view! (path "base.xml") (path "view.json")) "result")))
      (is (= "本文" (get (json/read-json (slurp (path "view.json"))) "text")))
      (is (= "passed" (get (main/validate-files (path "base.xml") [(path "layer.json")]) "result")))
      (is (= "written" (get (main/enrich-files! (path "base.xml") [(path "layer.json")] (path "enriched.xml")) "result")))
      (is (thrown? FileAlreadyExistsException (main/export-view! (path "base.xml") (path "view.json"))))
      (is (thrown? FileAlreadyExistsException (main/enrich-files! (path "base.xml") [(path "layer.json")] (path "enriched.xml"))))
      (finally (fs/delete-tree dir)))))

(deftest changed-analysis-reruns-enrichment-and-reuses-base-stages
  (let [dir (fs/create-temp-dir {:prefix "analysis-dag"})
        store (engine/open-store! {:cas-dir (str (fs/path dir "objects")) :db-path (str (fs/path dir "traces.sqlite"))})
        put-text #(cas/put-bytes! (:cas-dir store) (.getBytes ^String % "UTF-8"))
        put-json #(put-text (json/write-json-str %))
        inputs {"parser-ir" (put-json {"nodes" [{"type" "text" "text" "本文"}]
                                       "sentence_segmentation" {"coordinate_system" "parser_text_utf8"}})
                "metadata-record" (put-json {"work" {"title" "試験" "work_id" "1" "aozora_modified" "2026-09-07"} "contributors" []})
                "persons" (put-json {})}
        render-stage (publication-stages/render-stage "test-runtime")
        plain-stage (publication-stages/plaintext-stage "test-runtime")
        enrich-stage (stages/enrichment-stage "test-runtime")]
    (try
      (let [rendered (engine/run-stage! store render-stage inputs)
            tei-hash (get-in rendered [:outputs "tei"])
            base (String. (cas/get-bytes (:cas-dir store) tei-hash) "UTF-8")
            text-view (view/from-tei base)
            original (analysis text-view "ner" [["n1" 0 6 "place"]])
            updated (assoc-in original [:layer/records 0 :annotation/label] "person")
            original-hash (put-text (layer/write-layer text-view original))
            updated-hash (put-text (layer/write-layer text-view updated))
            plain (engine/run-stage! store plain-stage {"tei" tei-hash})
            first-enriched (engine/run-stage! store enrich-stage {"tei" tei-hash "layers" [original-hash]})
            second-enriched (engine/run-stage! store enrich-stage {"tei" tei-hash "layers" [updated-hash]})]
        (is (every? false? (map :cached? [rendered plain first-enriched second-enriched])))
        (is (not= (:outputs first-enriched) (:outputs second-enriched)))
        (is (:cached? (engine/run-stage! store render-stage inputs)))
        (is (:cached? (engine/run-stage! store plain-stage {"tei" tei-hash})))
        (is (:cached? (engine/run-stage! store enrich-stage {"tei" tei-hash "layers" [updated-hash]}))))
      (finally (engine/close-store! store) (fs/delete-tree dir)))))

(deftest interpretation-influence-controls-eligibility-without-changing-reading
  (let [base (document "<a:p>本文</a:p>")
        with-problem (fn [aspects influence]
                       (let [problem {"kind" "unknown-notation" "code" "unknown-notation"
                                      "raw" "［＃未知］" "aspects" aspects "influence" influence
                                      "source_span" {"coordinate_system" "decoded_utf8" "start" 100 "end" 115}}]
                         (str/replace base "</a:text>"
                                      (str "<a:back><a:div><a:note type='interpretation-problem'>"
                                           (json/write-json-str problem)
                                           "</a:note></a:div></a:back></a:text>"))))
        ordinary (view/from-tei base)
        unknown (view/from-tei (with-problem ["content" "structure" "layout"] {"kind" "document"}))
        layout (view/from-tei (with-problem ["layout"] {"kind" "document"}))]
    (is (= (:view/id ordinary) (:view/id unknown) (:view/id layout)))
    (is (= [] (:view/eligible-spans unknown)))
    (is (= [[0 6]] (:view/eligible-spans layout)))
    (is (= "［＃未知］" (get-in unknown [:view/problems 0 :view/evidence "raw"])))
    (is (= [] (:view/eligible-spans (view/from-tei (with-problem ["content"] {})))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (layer/validate-layer unknown (analysis ordinary "tokens" [["t" 0 6 "token"]]))))))
