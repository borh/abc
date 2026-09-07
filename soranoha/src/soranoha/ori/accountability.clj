(ns soranoha.ori.accountability
  "Independent source-marker evidence. Recognition does not certify interpretation."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [soranoha.core.hash :as hash]
            [soranoha.core.json :as record-json]))

(defn resolve-tool
  "Resolve the independent source scanner and its source-authority matrix."
  []
  (into {} (map (fn [[key variable]]
                  (let [value (System/getenv variable)]
                    (when (str/blank? value)
                      (throw (ex-info (str "Source accountability requires " variable)
                                      {:env_var variable})))
                    [key value])))
        [[:bin "AB_SOURCE_INVENTORY_BIN"] [:matrix "AB_AOZORA_SYNTAX_MATRIX"]]))

(defn source-stage
  "Source bytes -> lexical accountability, independently cached from interpretation."
  [{:keys [bin matrix]}]
  {:stage-id "source-accountability"
   :stage-version "1"
   :toolchain-id (hash/sha256-canonical-json
                  {"binary" (hash/sha256-file bin)
                   "matrix" (hash/sha256-file matrix)})
   :f (fn [{:keys [blob]} inputs]
        (let [dir (fs/create-temp-dir {:prefix "soranoha-source-accountability"})
              source (str (fs/path dir "source.txt"))
              output (str (fs/path dir "accountability.json"))]
          (try
            (io/copy ^bytes (blob (get inputs "source")) (io/file source))
            (let [{:keys [exit err]}
                  @(process/process [bin "--source" source "--matrix" matrix
                                     "--output-json" output]
                                    {:out :string :err :string})]
              (when-not (zero? exit)
                (throw (ex-info "Source accountability scanner failed"
                                {:exit exit :stderr err})))
              {"source-accountability" (java.nio.file.Files/readAllBytes (fs/path output))})
            (finally (fs/delete-tree dir)))))})

(def ^:private compatible-families
  {"kunten" {:markers #{"CommandFullwidth" "CommandAscii"}
             :families #{"kunten.kaeriten" "kunten.okurigana"} :aspects #{"content" "structure" "layout"}}
   "heading" {:markers #{"CommandFullwidth" "CommandAscii"}
              :families #{"heading.basic" "heading.dogyo" "heading.mado"} :aspects #{"structure" "layout"}}
   "ruby" {:markers #{"RubyExplicit" "RubyImplicit"}
           :families #{"ruby.basic"} :aspects #{"content" "structure"}}
   "gaiji" {:markers #{"GaijiFullwidth" "GaijiAscii"}
            :families #{"gaiji.marker" "gaiji.jis_code" "gaiji.unicode_codepoint"}
            :aspects #{"content"}}
   "gaiji-ruby" {:markers #{"GaijiFullwidth" "GaijiAscii" "RubyExplicit"}
                 :families #{"gaiji_ruby.inline_base"} :aspects #{"content" "structure"}}
   "emphasis" {:markers #{"CommandFullwidth" "CommandAscii"}
               :families #{"emphasis.basic" "decoration.boten" "decoration.bousen"
                           "decoration.bold_italic" "decoration.typeface" "decoration.font_size" "layout.tcy"} :aspects #{"layout"}}
   "caption" {:markers #{"CommandFullwidth" "CommandAscii"}
              :families #{"figure.image_caption" "caption.block" "caption.inline"} :aspects #{"structure" "layout"}}
   "text-variant" {:markers #{"CommandFullwidth" "CommandAscii"}
                   :families #{"annotation.chuuki"} :aspects #{"content" "structure"}}
   "editorial-note" {:markers #{"CommandFullwidth" "CommandAscii"}
                     :families #{"annotation.chuuki"} :aspects #{"content" "structure"}}
   "layout" {:markers #{"CommandFullwidth" "CommandAscii"}
             :families #{"decoration.keigakomi" "layout.yokogumi" "glyph.variant_note"} :aspects #{"layout"}}
   "line-layout" {:markers #{"CommandFullwidth" "CommandAscii"}
                  :families #{"indentation.basic" "indentation.jisage_block" "indentation.jisage_oneline"
                              "indentation.chitsuki" "indentation.jizume" "indentation.burasage" "layout.center_page" "layout.multicolumn"}
                  :aspects #{"layout"}}
   "table" {:markers #{"CommandFullwidth" "CommandAscii"}
            :families #{"structure.table"} :aspects #{"structure" "layout"}}
   "layout-break" {:markers #{"CommandFullwidth" "CommandAscii"}
                   :families #{"break.page_line" "break.line_explicit"} :aspects #{"structure" "layout"}}
   "warichu" {:markers #{"CommandFullwidth" "CommandAscii"}
              :families #{"warichu.basic"} :aspects #{"structure" "layout"}}})

(defn- require-evidence [condition message]
  (when-not condition (throw (ex-info message {:type :accountability/invalid-evidence}))))

(defn- valid-span? [{:strs [start end coordinate_system]}]
  (and (= "decoded_utf8" coordinate_system)
       (integer? start) (integer? end) (<= 0 start) (< start end)))

(defn- validate-fact! [{:strs [kind outcome aspects source_span] :as fact}]
  (let [allowed (get compatible-families kind)]
    (require-evidence
     (and allowed (= #{"kind" "outcome" "aspects" "source_span"} (set (keys fact)))
          (= "established" outcome) (vector? aspects) (seq aspects)
          (= (count aspects) (count (set aspects)))
          (every? (:aspects allowed) aspects) (valid-span? source_span))
     "Invalid native interpretation fact")))

(defn- marker-key [value]
  (let [span (get value "source_span")]
    [(get span "start") (get span "end")]))

(defn- occurrence-claims [occurrence facts]
  (into []
        (keep (fn [fact]
                (let [{:keys [markers families]} (get compatible-families (get fact "kind"))
                      matched (filterv families (get occurrence "families"))]
                  (when (and (markers (get occurrence "kind")) (seq matched))
                    {"kind" (get fact "kind") "families" matched
                     "aspects" (get fact "aspects")
                     "source_span" (get fact "source_span")}))))
        facts))

(defn- located-claims [occurrence facts-by-marker]
  (let [components (mapv #(assoc % "claims" (occurrence-claims % (get facts-by-marker (marker-key %))))
                         (get occurrence "components" []))
        component-families (into #{} (mapcat #(get % "families")) components)
        complete-families (into #{}
                                (filter (fn [family]
                                          (every? (fn [component]
                                                    (or (not (some #{family} (get component "families")))
                                                        (some #(some #{family} (get % "families"))
                                                              (get component "claims"))))
                                                  components)))
                                component-families)
        restrict-claims (fn [allowed claims]
                          (keep (fn [claim]
                                  (let [families (filterv allowed (get claim "families"))]
                                    (when (seq families) (assoc claim "families" families))))
                                claims))
        claims (into (vec (restrict-claims #(not (component-families %))
                                           (occurrence-claims occurrence (get facts-by-marker (marker-key occurrence)))))
                     (restrict-claims complete-families (mapcat #(get % "claims") components)))]
    [components claims]))

(defn coverage-report
  "Join lexical evidence to explicit native claims; no source/export certification.
  Negative problems retain their declared influence, independently of positive claims."
  [oracle parser-ir]
  (let [facts (get parser-ir "interpretation_facts")
        problems (get parser-ir "interpretation_problems")
        occurrences (get oracle "occurrences")]
    (require-evidence (= "aozora-source-accountability/2" (get oracle "schema"))
                      "Unsupported source accountability schema")
    (require-evidence (and (string? (get oracle "source_sha256"))
                           (= (get oracle "source_sha256")
                              (get-in parser-ir ["source" "primary_text_hash"]))
                           (= (get oracle "encoding")
                              (get-in parser-ir ["source" "decode_outcome"])))
                      "Source accountability and interpretation have different inputs")
    (require-evidence (and (vector? facts) (vector? problems) (vector? occurrences))
                      "Interpretation evidence requires explicit occurrence, fact and problem arrays")
    (doseq [fact facts] (validate-fact! fact))
    (doseq [occurrence occurrences]
      (require-evidence (and (valid-span? (get occurrence "source_span"))
                             (vector? (get occurrence "families")))
                        "Invalid lexical occurrence")
      (doseq [component (get occurrence "components")]
        (let [parent (get occurrence "source_span") child (get component "source_span")]
          (require-evidence (and (valid-span? child) (vector? (get component "families"))
                                 (<= (get parent "start") (get child "start"))
                                 (<= (get child "end") (get parent "end"))
                                 (not= (marker-key occurrence) (marker-key component)))
                            "Invalid lexical component"))))
    (let [facts-by-marker (group-by marker-key facts)
          results
          (into []
                (map (fn [occurrence]
                       (let [apparatus? (#{"front-matter" "body-end-boundary" "back-matter"}
                                         (get occurrence "region"))
                             [components claims] (located-claims occurrence
                                                                 (if (or apparatus? (not= "lossless" (get oracle "decode_outcome")))
                                                                   {} facts-by-marker))
                             claimed (into #{} (mapcat #(get % "families")) claims)]
                         (assoc occurrence "claims" claims "components" components
                                "disposition" (if apparatus? "source-apparatus" "interpretation-evidence")
                                "unaccounted_families" (if apparatus? []
                                                           (filterv #(not (claimed %)) (get occurrence "families")))
                                "unclassified" (and (not apparatus?) (empty? (get occurrence "families")))))))
                (sort-by #(get-in % ["source_span" "start"]) occurrences))
          families
          (reduce (fn [counts occurrence]
                    (let [claimed (into #{} (mapcat #(get % "families")) (get occurrence "claims"))]
                      (reduce (fn [counts family]
                                (update-in counts [family (cond
                                                            (= "source-apparatus" (get occurrence "disposition")) "source_apparatus"
                                                            (claimed family) "interpreter_claimed"
                                                            :else "unaccounted")] (fnil inc 0)))
                              counts (get occurrence "families"))))
                  (sorted-map) results)]
      {"schema" "soranoha-interpretation-coverage/1"
       "source_sha256" (get oracle "source_sha256")
       "decode_outcome" (get oracle "decode_outcome")
       "semantic_certification" "not-assessed"
       "families" families
       "unclassified_occurrences" (count (filter #(get % "unclassified") results))
       "occurrences" results
       "interpretation_problems" problems})))

(defn coverage-stage
  "Independent lexical oracle + parser IR -> explicit claim accounting."
  [clj-toolchain-id]
  {:stage-id "interpretation-coverage" :stage-version "8" :toolchain-id clj-toolchain-id
   :f (fn [{:keys [blob]} inputs]
        (let [input-bytes (into {} (map (fn [name] [name (blob (get inputs name))]))
                                ["source-accountability" "parser-ir"])
              read-input #(json/read-json (String. ^bytes (get input-bytes %) "UTF-8"))
              report (assoc (coverage-report (read-input "source-accountability") (read-input "parser-ir"))
                            "artifacts" (into {} (map (fn [[name bytes]]
                                                        [name (str "sha256:" (hash/sha256-bytes bytes))]))
                                              input-bytes))]
          {"interpretation-coverage"
           (.getBytes ^String (record-json/write-deterministic-json-str report) "UTF-8")}))})
