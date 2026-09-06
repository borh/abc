(ns abc.tools.parser-ir-annotations-test
  (:require [abc.tools.parser-ir-plaintext :as plaintext]
            [clojure.test :refer [deftest is testing]]))

(defn- scalar-subs
  "Substring by unicode scalar offsets (NOT UTF-16 indices)."
  [^String s start end]
  (let [i (.offsetByCodePoints s 0 start)
        j (.offsetByCodePoints s 0 end)]
    (subs s i j)))

(def ^:private ir
  {"nodes"
   [{"type" "text" "text" "冒頭"}
    {"type" "ruby" "span" {"start" 6 "end" 12 "coordinate_system" "parser_text_utf8"}
     "source_span" {"start" 6 "end" 12 "coordinate_system" "decoded_utf8"}
     "ruby" {"base" "吾輩" "reading" "わがはい" "scope" "explicit" "direction" "right"}}
    {"type" "text" "text" "は"}
    ;; astral-plane gaiji: U+20B9F 𠮟 — 2 UTF-16 units, 1 unicode scalar
    {"type" "gaiji" "span" {"start" 15 "end" 23}
     "gaiji" {"raw_marker" "※［＃「口＋七」］" "unicode" "𠮟" "resolved" true}}
    {"type" "text" "text" "る"}]})

(deftest render-with-annotations-spans-test
  (let [{:keys [text annotations]} (plaintext/render-with-annotations ir)]
    (testing "plaintext unchanged from render-string"
      (is (= (plaintext/render-string ir) text))
      (is (= "冒頭吾輩は𠮟る" text)))
    (testing "ruby annotation carries plaintext scalar span over the base text"
      (let [ruby (first (filter #(= "ruby" (get % "annotation_kind")) annotations))]
        (is (= "吾輩" (scalar-subs text
                                 (get-in ruby ["span" "start"])
                                 (get-in ruby ["span" "end"]))))
        (is (= "わがはい" (get-in ruby ["ruby" "reading"])))
        (is (= "right" (get-in ruby ["ruby" "direction"])))
        (is (nil? (get-in ruby ["ruby" "scope"])) "scope withheld (D4)")
        (is (= {"start" 6 "end" 12} (get ruby "source_span")))))
    (testing "gaiji span counts astral chars as one scalar"
      (let [gaiji (first (filter #(= "gaiji" (get % "annotation_kind")) annotations))]
        (is (= {"start" 5 "end" 6} (get gaiji "span")))
        (is (= "𠮟" (scalar-subs text 5 6)))))))
