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
            [soranoha.ori.projection :as projection]
            [soranoha.ori.validate :as validation])
  (:import [java.nio.file FileAlreadyExistsException]
           [org.w3c.dom Document Node NodeList]))

(defn document [body]
  (str "<TEI xmlns='http://www.tei-c.org/ns/1.0'><teiHeader><fileDesc>"
       "<titleStmt><title>試験</title></titleStmt><publicationStmt><publisher>試験</publisher><availability status='free'><licence target='https://creativecommons.org/publicdomain/zero/1.0/'>CC0-1.0</licence></availability></publicationStmt>"
       "<sourceDesc><p><idno type='aozora-work-id'>1</idno></p></sourceDesc></fileDesc>"
       "<encodingDesc><charDecl><char xml:id='g1'><desc>外字</desc>"
       "<mapping type='unicode'>犍</mapping></char></charDecl></encodingDesc>"
       "<profileDesc><langUsage><language ident='ja'>Japanese</language></langUsage></profileDesc>"
       "</teiHeader><text><body>" body "</body></text></TEI>"))

(def rich-body
  (str "\n<p xml:id='original'><ruby><rb><g ref='#g1'>犍</g>陀多</rb><rt>かんだた</rt></ruby>は"
       "<ruby><rb>籠</rb><rt><choice><sic>さる</sic><corr>ざる</corr></choice></rt></ruby>をさげ"
       "<note type='source-span'>原文の位置</note></p>\n"
       "<p>前　後𠮷<lb/>続<pb/>末</p>\n"
       "<floatingText><body>\n<p>看板</p>\n</body></floatingText>\n"
       "<figure><graphic url='image.png'/><figDesc>挿絵</figDesc></figure>\n"
       "<note type='interpretation-uncertainty' subtype='layout' n='document'>未確定</note>"))

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
    (is (= "犍陀多は籠をさげ\n前　後𠮷\n続\n末\n看板" (:view/text text-view)))
    (is (= (:view/id text-view)
           (:view/id (view/from-tei (document (str/replace rich-body "\n" "\n    "))))))
    (is (= "ざる" (view/visible-text (-> ^Document (:view/document text-view)
                                       (.getElementsByTagNameNS view/tei-namespace "rt") (.item 1)))))
    (is (= "正形" (:view/text (view/from-tei (document "<p><choice><orig>原形</orig><reg>正形</reg></choice></p>"))))))
  (is (= "D\n\nCAP\nQ"
         (:view/text (view/from-tei (document "<p>D</p><pb/><p><lb/></p><figure><figDesc>ALT</figDesc></figure><p><seg type='caption'>CAP</seg></p><p>Q</p>")))))
  (is (thrown? clojure.lang.ExceptionInfo (view/from-tei "<TEI><text><body/></text></TEI>"))))

(deftest unresolved-glyphs-are-present-and-ineligible
  (let [text-view (view/from-tei (document "<p>前<g ref='#g1'/>後</p>"))
        successful (analysis text-view "empty" [])]
    (is (= "前\uFFFC後" (:view/text text-view)))
    (is (= [[0 3] [6 9]] (:view/eligible-spans text-view)))
    (is (= [{:view/problem :view/unresolved-glyph :view/start 3 :view/end 6}] (:view/problems text-view)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (layer/validate-layer text-view (assoc successful :layer/eligible-spans [[0 9]]))))))

(deftest layers-bind-inputs-and-reject-invalid-ranges-and-domains
  (let [text-view (view/from-tei (document "<p>犍𠮷陀多</p>"))
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
    (is (thrown? clojure.lang.ExceptionInfo (tei/enrich (document "<p>別の本文</p>") [entities])))
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
        base (document "<p>本文</p>")
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
        inputs {"parser-ir" (put-json {"nodes" [{"type" "text" "text" "本文"}]})
                "metadata-record" (put-json {"work" {"title" "試験" "work_id" "1" "aozora_modified" "2026-09-07"} "contributors" []})
                "persons" (put-json {})
                ;; a scalar input, so a work rendered under one publication
                ;; identifier is never served from cache under another
                "slug" "000001_000001"}
        render-stage (publication-stages/render-stage
                      "test-runtime"
                      {"works" "public-domain"
                       "encoding" "CC0-1.0"
                       "statement_url" "https://w3id.org/soranoha/rights"})
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
  (let [base (document "<p>本文</p>")
        with-problem (fn [aspects influence]
                       (let [problem {"kind" "unknown-notation" "code" "unknown-notation"
                                      "raw" "［＃未知］" "aspects" aspects "influence" influence
                                      "source_span" {"coordinate_system" "decoded_utf8" "start" 100 "end" 115}}]
                         (str/replace base "</text>"
                                      (str "<back><div><note type='interpretation-problem'>"
                                           (json/write-json-str problem)
                                           "</note></div></back></text>"))))
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

(deftest external-content-slot-limits-projections-without-invalidating-retained-prose
  (let [base (document "<p>本文</p>")
        problem {"kind" "content-outside-primary-input" "code" "content-outside-primary-input"
                 "raw" "［＃ここに表組入る、別ファイル（table.txt）参照］"
                 "aspects" ["content"] "influence" {"kind" "source-location"}
                 "source_span" {"coordinate_system" "decoded_utf8" "start" 100 "end" 175}}
        reading (fn [problem]
                  (view/from-tei
                   (str/replace base "</text>"
                                (str "<back><div><note type='interpretation-problem'>"
                                     (json/write-json-str problem)
                                     "</note></div></back></text>"))))
        external (reading problem)]
    (is (= (:view/id (view/from-tei base)) (:view/id external)))
    (is (= [[0 6]] (:view/eligible-spans external)))
    (doseq [profile [:projection/plaintext :projection/markdown]]
      (let [report (projection/report profile external)]
        (is (= "limited" (get report "status")))
        (is (some #{{"family" "external-content" "disposition" "unresolved" "count" 1}}
                  (get report "counts")))))
    (is (= [] (:view/eligible-spans (reading (dissoc problem "source_span")))))))
