(ns soranoha.ori.projection-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is]]
            [soranoha.annotations.view :as view]
            [soranoha.kura.cas :as cas]
            [soranoha.kura.engine :as engine]
            [soranoha.ori.projection :as projection]
            [soranoha.ori.stages :as stages]))

(defn- tei [body]
  (str "<TEI xmlns=\"http://www.tei-c.org/ns/1.0\"><teiHeader/><text><body>"
       body "</body></text></TEI>"))

(deftest projections-share-the-established-reading
  (let [reading (view/from-tei
                 (tei (str "<p style=\"text-indent: 1em\">前<ruby><rb><g ref=\"#g\">犍</g></rb>"
                           "<rt>かん</rt></ruby><choice><sic>誤</sic><corr>正</corr></choice>"
                           "<note>注</note><hi rend=\"bold\">強</hi><lb/>後</p><p>次</p>")))]
    (is (= "前犍正強\n後\n次" (:view/text reading)))
    (is (= "　前犍正強\n後\n次" (projection/plaintext reading)))
    (is (= "前<ruby><rb>犍</rb><rt>かん</rt></ruby>正<strong>強</strong>  \n後\n\n次"
           (projection/markdown reading)))))

(deftest markdown-source-text-cannot-inject-markup
  (let [reading (view/from-tei (tei "<p>*本文* &lt;script&gt;<ruby><rb>字</rb><rt>&lt;b&gt;</rt></ruby></p>"))]
    (is (= "\\*本文\\* &lt;script&gt;<ruby><rb>字</rb><rt>&lt;b&gt;</rt></ruby>"
           (projection/markdown reading)))))

(deftest projection-policy-change-does-not-rerun-its-sibling
  (let [dir (fs/create-temp-dir {:prefix "projection-cache"})
        store (engine/open-store! {:cas-dir (str (fs/path dir "objects"))
                                   :db-path (str (fs/path dir "trace.sqlite"))})
        inputs {"tei" (cas/put-bytes! (:cas-dir store) (.getBytes ^String (tei "<p>本文</p>") "UTF-8"))}
        plain (stages/plaintext-stage "runtime")
        markdown (stages/markdown-stage "runtime")]
    (try
      (engine/run-stage! store plain inputs)
      (engine/run-stage! store markdown inputs)
      (is (false? (:cached? (engine/run-stage! store (assoc markdown :stage-version "next-policy") inputs))))
      (is (:cached? (engine/run-stage! store plain inputs)))
      (finally (engine/close-store! store) (fs/delete-tree dir)))))
