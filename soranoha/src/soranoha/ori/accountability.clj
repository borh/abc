(ns soranoha.ori.accountability
  "Independent source-marker evidence. Recognition does not certify interpretation."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.java.io :as io]
            [soranoha.core.config :as config]
            [soranoha.core.hash :as hash]
            [soranoha.core.json :as record-json]))

(defn resolve-tool
  "Resolve the independent source scanner and its source-authority matrix."
  []
  {:bin (config/require-env "AB_SOURCE_INVENTORY_BIN" "source accountability scanner")
   :matrix (config/require-env "AB_AOZORA_SYNTAX_MATRIX" "source authority matrix")})

(defn source-stage
  "Source bytes -> lexical accountability, independently cached from interpretation."
  [{:keys [bin matrix]}]
  {:stage-id "source-accountability"
   :stage-version "1"
   :toolchain-id (hash/sha256-canonical-json
                  {"binary" (hash/sha256-file bin)
                   "matrix" (hash/sha256-file matrix)})
   :f (fn [{:keys [blob]} inputs]
        (config/with-temp-dir
          "soranoha-source-accountability"
          (fn [dir]
            (let [source (str (fs/path dir "source.txt"))
                  output (str (fs/path dir "accountability.json"))]
              (io/copy ^bytes (blob (get inputs "source")) (io/file source))
              (let [{:keys [exit err]}
                    @(process/process [bin "--source" source "--matrix" matrix
                                       "--output-json" output]
                                      {:out :string :err :string})]
                (when-not (zero? exit)
                  (throw (ex-info "Source accountability scanner failed"
                                  {:exit exit :stderr err})))
                {"source-accountability" (java.nio.file.Files/readAllBytes (fs/path output))})))))})

(def ^:private compatible-families
  {"kunten" {:markers #{"CommandFullwidth" "CommandAscii"}
             :families #{"kunten.kaeriten" "kunten.okurigana"} :aspects #{"content" "structure" "layout"}}
   "iteration-mark" {:markers #{"IterationNotation"}
                     :families #{"iteration.kunoji"} :aspects #{"content"}}
   "supplied-diacritic" {:markers #{"CommandFullwidth" "CommandAscii"}
                         :families #{"accent.dotted_letter" "glyph.variant_note"}
                         :aspects #{"content"}}
   "heading" {:markers #{"CommandFullwidth" "CommandAscii"}
              :families #{"heading.basic" "heading.dogyo" "heading.mado"} :aspects #{"structure" "layout"}}
   "ruby" {:markers #{"RubyExplicit" "RubyImplicit"}
           :families #{"ruby.basic"} :aspects #{"content" "structure"}}
   "gaiji" {:markers #{"GaijiFullwidth" "GaijiAscii"}
            :families #{"gaiji.marker" "gaiji.jis_code" "gaiji.unicode_codepoint" "glyph.variant_note"}
            :aspects #{"content"}}
   "gaiji-ruby" {:markers #{"GaijiFullwidth" "GaijiAscii" "RubyExplicit"}
                 :families #{"gaiji_ruby.inline_base"} :aspects #{"content" "structure"}}
   "baseline-position" {:markers #{"CommandFullwidth" "CommandAscii"}
                        :families #{"decoration.font_size"} :aspects #{"layout"}}
   "exponent" {:markers #{"CommandFullwidth" "CommandAscii"}
               :families #{"glyph.variant_note"} :aspects #{"structure"}}
   "emphasis" {:markers #{"CommandFullwidth" "CommandAscii"}
               :families #{"emphasis.basic" "decoration.boten" "decoration.bousen"
                           "decoration.bold_italic" "decoration.typeface" "decoration.font_size" "layout.tcy"} :aspects #{"layout"}}
   "illustration" {:markers #{"CommandFullwidth" "CommandAscii"}
                   :families #{"figure.image_inline" "figure.image_caption" "caption.inline"}
                   :aspects #{"content" "structure" "layout"}}
   "translation" {:markers #{"CommandFullwidth" "CommandAscii"}
                  :families #{"source.translation_scope"} :aspects #{"structure"}}
   "caption" {:markers #{"CommandFullwidth" "CommandAscii"}
              :families #{"figure.image_caption" "caption.block" "caption.inline"} :aspects #{"structure" "layout"}}
   "text-variant" {:markers #{"CommandFullwidth" "CommandAscii"}
                   :families #{"annotation.chuuki"} :aspects #{"content" "structure"}}
   "glyph-shape-assertion" {:markers #{"CommandFullwidth" "CommandAscii"}
                            :families #{"glyph.variant_note"} :aspects #{"structure"}}
   ;; A mark the source places between two characters is spelled with the
   ;; emphasis vocabulary, so the coverage row that scans for it is the
   ;; emphasis one even though what it publishes is a note and not emphasis.
   "supplied-mark" {:markers #{"CommandFullwidth" "CommandAscii"}
                    :families #{"glyph.variant_note" "emphasis.basic"}
                    :aspects #{"structure" "layout"}}
   "annotated-text" {:markers #{"CommandFullwidth" "CommandAscii"}
                     :families #{"annotation.chuuki" "annotation.bouki" "reference.frontref" "source.page_reference"} :aspects #{"content" "structure" "layout"}}
   "external-table-reference" {:markers #{"CommandFullwidth" "CommandAscii"}
                               :families #{"structure.table"} :aspects #{"structure"}}
   "editorial-note" {:markers #{"CommandFullwidth" "CommandAscii"}
                     :families #{"annotation.chuuki" "source.note_label" "figure.insertion_declaration"}
                     :aspects #{"content" "structure"}}
   "layout" {:markers #{"CommandFullwidth" "CommandAscii"}
             :families #{"decoration.keigakomi" "layout.yokogumi" "glyph.variant_note"} :aspects #{"layout"}}
   "line-layout" {:markers #{"CommandFullwidth" "CommandAscii"}
                  :families #{"indentation.basic" "indentation.jisage_block" "indentation.jisage_oneline"
                              "indentation.chitsuki" "indentation.jizume" "indentation.burasage" "layout.center_page" "layout.multicolumn"}
                  :aspects #{"layout"}}
   "formula" {:markers #{"CommandFullwidth" "CommandAscii"}
              :families #{"structure.formula"} :aspects #{"structure"}}
   "table" {:markers #{"CommandFullwidth" "CommandAscii"}
            :families #{"structure.table"} :aspects #{"structure" "layout"}}
   ;; One family covers both roles: the source spells them apart (引用文 vs
   ;; 手紙文) and the claims stay apart, but the coverage row is the one that
   ;; scans for either marker.
   "quotation" {:markers #{"CommandFullwidth" "CommandAscii"}
                :families #{"structure.quote_block"} :aspects #{"structure"}}
   "letter" {:markers #{"CommandFullwidth" "CommandAscii"}
             :families #{"structure.quote_block"} :aspects #{"structure"}}
   "layout-break" {:markers #{"CommandFullwidth" "CommandAscii"}
                   :families #{"break.page_line" "break.line_explicit"} :aspects #{"structure" "layout"}}
   "warichu" {:markers #{"CommandFullwidth" "CommandAscii"}
              :families #{"warichu.basic"} :aspects #{"structure" "layout"}}})

(def claimable-families
  "Every family some fact kind can claim, on any marker kind it admits.

  Public because it is one half of a pair that has to agree: the coverage
  matrix names the families the scanner assigns, this table names the families
  a claim can carry, and nothing compared them until the two had drifted far
  enough that most reported coverage gaps were families no claim could reach."
  (into (sorted-set) (mapcat :families) (vals compatible-families)))

(def ^:private reachable-families
  "For each marker kind the scanner emits, the families a claim could carry on an
  occurrence of that kind.

  A claim counts against an occurrence only when the fact kind's marker set
  admits the occurrence's own kind, so a family outside this set is unreachable
  however the interpreter behaved. Calling such a family unaccounted would read
  as a coverage gap and be none: the scanner and the fact table name marker kinds
  from two vocabularies, and the scanner emits kinds no fact kind mentions. The
  set is derived from the fact table rather than restated, so a fact kind that
  gains a marker widens it in step."
  (reduce (fn [reachable {:keys [markers families]}]
            (reduce #(update %1 %2 (fnil into #{}) families) reachable markers))
          {} (vals compatible-families)))

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
                             claimed (into #{} (mapcat #(get % "families")) claims)
                             ;; a nested component carries its own marker kind and
                             ;; its claims are promoted to the occurrence, so what
                             ;; is reachable here is reachable through any of them
                             reachable (transduce (map #(get reachable-families (get % "kind")))
                                                  into
                                                  (get reachable-families (get occurrence "kind") #{})
                                                  components)]
                         (assoc occurrence "claims" claims "components" components
                                "disposition" (if apparatus? "source-apparatus" "interpretation-evidence")
                                "unaccounted_families" (if apparatus? []
                                                           (filterv #(and (reachable %) (not (claimed %)))
                                                                    (get occurrence "families")))
                                "unreachable_families" (if apparatus? []
                                                           (filterv #(not (reachable %)) (get occurrence "families")))
                                "unclassified" (and (not apparatus?) (empty? (get occurrence "families")))))))
                (sort-by #(get-in % ["source_span" "start"]) occurrences))
          families
          (reduce (fn [counts occurrence]
                    (let [claimed (into #{} (mapcat #(get % "families")) (get occurrence "claims"))
                          unreachable (set (get occurrence "unreachable_families"))]
                      (reduce (fn [counts family]
                                (update-in counts [family (cond
                                                            (= "source-apparatus" (get occurrence "disposition")) "source_apparatus"
                                                            (claimed family) "interpreter_claimed"
                                                            (unreachable family) "outside_claim_vocabulary"
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

(defn reported-coverage
  "The coverage report in the form the stage writes it: the family counts, the
  interpretation problems, and the occurrences that were not fully accounted
  for.

  An occurrence the interpreter claimed is already visible from the other
  direction, as the `source` attribute on the element the claim produced, so
  listing every one of them here said the same thing twice. It was not a small
  duplication: measured over a full review export, this report was 38% of the
  bytes written and larger than the TEI it accounts for, while 99.7% of its
  occurrences were cleanly claimed and said nothing a reader could act on.

  What is not recoverable from anything else the build writes is which
  occurrences went unaccounted, so those are kept whole, and the total they
  were drawn from is stated so their number can be read as a proportion.

  An occurrence whose families are all outside the claim vocabulary is not one
  of them. It is counted in `families` under `outside_claim_vocabulary` and left
  off the list, because what explains it is its marker kind rather than anything
  about the occurrence, and one count per family says that as completely as
  thousands of records would. Measured over the full corpus, those were 69% of
  the entries this list used to carry."
  [report]
  (let [occurrences (get report "occurrences")]
    (-> report
        (assoc "schema" "soranoha-interpretation-coverage/3"
               "occurrence_count" (count occurrences)
               "unclassified_occurrence_count" (get report "unclassified_occurrences")
               "unaccounted_occurrences"
               (filterv #(or (seq (get % "unaccounted_families")) (get % "unclassified"))
                        occurrences))
        (dissoc "occurrences" "unclassified_occurrences"))))

(defn coverage-stage
  "Independent lexical oracle + parser IR -> explicit claim accounting."
  [clj-toolchain-id]
  {:stage-id "interpretation-coverage" :stage-version "22" :toolchain-id clj-toolchain-id
   :f (fn [{:keys [blob]} inputs]
        (let [input-bytes (into {} (map (fn [name] [name (blob (get inputs name))]))
                                ["source-accountability" "parser-ir"])
              read-input #(record-json/read-json-bytes (get input-bytes %))
              report (assoc (coverage-report (read-input "source-accountability") (read-input "parser-ir"))
                            "artifacts" (into {} (map (fn [[name bytes]]
                                                        [name (str "sha256:" (hash/sha256-bytes bytes))]))
                                              input-bytes))]
          {"interpretation-coverage"
           (.getBytes ^String (record-json/write-deterministic-json-str
                               (reported-coverage report))
                      "UTF-8")}))})
