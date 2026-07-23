(ns abc.tools.parser-publication-evidence-test
  (:require [abc.tools.files :as files]
            [abc.tools.materialize-publication :as materialize]
            [abc.tools.parser-ir-plaintext :as plaintext]
            [abc.tools.parser-ir-publication-policy :as policy]
            [abc.tools.parser-ir-tei :as parser-ir-tei]
            [abc.tools.parser-ir-vocabulary :as vocab]
            [abc.tools.schema :as schema]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def ^:private generated-at "2026-07-03T00:00:00Z")

(defn- temp-dir [prefix]
  (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- delete-tree! [dir]
  (when dir
    (doseq [file (reverse (file-seq dir))]
      (.delete file))))

(defn- hiccup-nodes [node]
  (tree-seq vector? rest node))

(deftest parser-publication-rendering-contract
  (let [work-dir (temp-dir "abc-parser-publication-evidence")
        output-dir (io/file work-dir "publication")]
    (try
      (testing "schema-derived renderer coverage is exact and detects both drift directions"
        (let [node-types (vocab/node-types
                          (files/read-json "schemas/parser-ir.schema.json"))
              publication-policy (policy/load-policy
                                  "data/parser-ir-publication-policy-v0.json")]
          (doseq [[renderer covered] [["plaintext" plaintext/covered-node-types]
                                      ["tei" parser-ir-tei/covered-node-types]]]
            (is (empty? (vocab/coverage-errors node-types renderer covered)))
            (is (= covered
                   (policy/renderer-covered-node-types publication-policy renderer))))
          (is (seq (vocab/coverage-errors
                    node-types "plaintext"
                    (disj (policy/renderer-covered-node-types
                           publication-policy "plaintext") "ruby"))))
          (is (seq (vocab/coverage-errors
                    node-types "tei"
                    (conj (policy/renderer-covered-node-types
                           publication-policy "tei") "future-node"))))))

      (testing "Aozora ruby defaults to furigana and direction uses profile-valid rend"
        (let [rendered (parser-ir-tei/render
                        {"nodes" [{"type" "ruby"
                                   "span" {"start" 0 "end" 3
                                           "coordinate_system" "decoded_utf8"}
                                   "ruby" {"base" "猫" "reading" "ねこ"
                                           "scope" "inferred"
                                           "direction" "right"}}]})
              ruby (some #(when (= :ruby (first %)) %)
                         (hiccup-nodes (:body rendered)))]
          (is (= "furigana" (:type (second ruby))))
          (is (= "right" (:rend (second ruby))))
          (is (not (contains? (second ruby) :place))))
        (let [rendered (parser-ir-tei/render
                        {"nodes" [{"type" "ruby"
                                   "span" {"start" 0 "end" 3
                                           "coordinate_system" "decoded_utf8"}
                                   "ruby" {"base" "猫" "reading" "ねこ"
                                           "scope" "inferred"
                                           "direction" "left"}}]})
              ruby (some #(when (= :ruby (first %)) %)
                         (hiccup-nodes (:body rendered)))]
          (is (= "left" (:rend (second ruby))))))

      (testing "plaintext contains visible body text only"
        (let [text (plaintext/render-string
                    {"source" {"source_path" "cards/work.txt"
                               "encoding" "Shift_JIS"}
                     "nodes" [{"type" "ruby" "span" {"start" 0 "end" 3}
                               "ruby" {"base" "猫" "reading" "ねこ"
                                       "scope" "explicit" "direction" "right"}}
                              {"type" "editor-note" "span" {"start" 3 "end" 9}
                               "note" {"raw" "［＃注］" "category" "apparatus"}}
                              {"type" "layout-span" "span" {"start" 9 "end" 12}
                               "text" "本文"
                               "layout" {"kind" "tcy" "marker" "縦中横"}}
                              {"type" "source-note" "span" {"start" 12 "end" 20}
                               "text" "底本注" "note_type" "source-attribution"
                               "placement" "back" "classification" "direct"
                               "source_pointer" "blocks[9]"}]})]
          (is (= "猫本文" text))
          (doseq [excluded ["ねこ" "［＃注］" "cards/work.txt" "Shift_JIS"
                            "tcy" "縦中横" "底本注" "blocks[9]"]]
            (is (not (string/includes? text excluded)) excluded))))

      (testing "materialization writes and validates the complete publication artifact set"
        (let [result (materialize/materialize-publication!
                      {:parser-ir-path "examples/v0/example-work/parser-ir.json"
                       :source-manifest-path "examples/v0/example-work/source.manifest.json"
                       :metadata-record-path "examples/v0/example-work/metadata-record.json"
                       :persons-dir "examples/v0/example-persons"
                       :output-dir output-dir
                       :generated-at generated-at})
              expected {:plaintext "plain.txt"
                        :tei "tei.xml"
                        :plaintext-manifest "plaintext.manifest.json"
                        :tei-manifest "tei.manifest.json"
                        :preservation "preservation.json"
                        :tei-validation-result "tei-validation-result.json"}
              validation (files/read-json (:tei-validation-result result))
              pinned-rng (System/getenv "TEI_SCHEMA_PATH")]
          (doseq [[key name] expected]
            (is (= (.getCanonicalFile (io/file output-dir name))
                   (.getCanonicalFile (io/file (get result key)))))
            (is (.isFile (io/file output-dir name))))
          (is (= "passed" (get validation "status")))
          (is (= "passed" (get-in validation ["layers" "relax_ng" "status"])))
          (is (= "passed" (get-in validation ["layers" "schematron" "status"])))
          (is (empty? (get validation "findings")))
          (is (not (string/blank? pinned-rng)))
          (is (nil? (schema/validation-errors
                     (files/read-json "schemas/tei-validation-result.schema.json")
                     validation)))))
      (finally
        (delete-tree! work-dir)))))
