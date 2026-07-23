(ns abc.tools.temporal-evidence-input-test
  (:require [abc.tools.aozora-csv :as aozora-csv]
            [abc.tools.aozora-history-audit :as history-audit]
            [abc.tools.aozora-ingest :as ingest]
            [abc.tools.files :as files]
            [abc.tools.json :as json]
            [abc.tools.malli :as malli]
            [abc.tools.manifest :as manifest]
            [abc.tools.person-drift :as person-drift]
            [abc.tools.person-record :as person-record]
            [abc.tools.schema :as schema]
            [abc.tools.shacl :as shacl]
            [abc.tools.validate-design-bundle :as validate]
            [abc.sim.render :as sim-render]
            [babashka.fs :as fs]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [arachne.aristotle :as aa])
  (:import [org.apache.jena.datatypes BaseDatatype]
           [org.apache.jena.datatypes.xsd XSDDatatype]
           [org.apache.jena.graph Graph Node NodeFactory Triple]))

(use-fixtures :once (fn [test-fn] (malli/install!) (test-fn)))

(def ^:private event-path
  "examples/v0/example-persons/_events/sha256:550c55dbfed12ce9b6de833a75c8b03e8a01bf3047c17ced494e87db0f4ee747.json")
(def ^:private index-paths
  ["examples/v0/example-persons/_indexes/000879.json"
   "examples/v0/example-persons/_indexes/abc-000000000001.json"
   "examples/v0/example-persons/_indexes/abc-000000000002.json"])

(defn- base-event []
  (person-drift/materialize-event-id
   {"schema_id" person-drift/event-schema-id
    "schema_hash" (manifest/schema-hash person-drift/event-schema-path)
    "drift_event_type" "split"
    "date" "2026-04-30"
    "participants" [{"snapshot_id" "post-abc-000000000001"
                     "person_id" "abc-000000000001"
                     "person_record_hash" (files/example-hash "01")}
                    {"snapshot_id" "post-abc-000000000002"
                     "person_id" "abc-000000000002"
                     "person_record_hash" (files/example-hash "02")}
                    {"snapshot_id" "pre-000879"
                     "person_id" "000879"
                     "person_record_hash" (files/example-hash "03")}]
    "evidence" ["https://example.org/abc/drift-evidence/fictional-split-2026"]
    "prov" {"used" ["pre-000879"]
            "was_generated_by" ["post-abc-000000000001"
                                "post-abc-000000000002"]
            "qualified_association"
            {"agent" "https://w3id.org/abc/agents/editorial-board"
             "had_role" "abc:DriftEditor"}}}))

(defn- base-merge-event []
  (person-drift/materialize-event-id
   {"schema_id" person-drift/event-schema-id
    "schema_hash" (manifest/schema-hash person-drift/event-schema-path)
    "drift_event_type" "merge"
    "date" "2026-04-30"
    "participants" [{"snapshot_id" "post-000879"
                     "person_id" "000879"
                     "person_record_hash" (files/example-hash "03")}
                    {"snapshot_id" "pre-abc-000000000001"
                     "person_id" "abc-000000000001"
                     "person_record_hash" (files/example-hash "01")}
                    {"snapshot_id" "pre-abc-000000000002"
                     "person_id" "abc-000000000002"
                     "person_record_hash" (files/example-hash "02")}]
    "evidence" ["https://example.org/abc/drift-evidence/fictional-merge-2026"]
    "prov" {"used" ["pre-abc-000000000001" "pre-abc-000000000002"]
            "was_generated_by" ["post-000879"]
            "qualified_association"
            {"agent" "https://w3id.org/abc/agents/editorial-board"
             "had_role" "abc:DriftEditor"}}}))

(defn- coherence-codes [event]
  (set (map :code (person-drift/event-json-coherence-failures event))))

(defn- write-sidecars! [root event]
  (let [event-id (get event "drift_event_id")]
    (json/write-deterministic-json-file!
     (fs/file (fs/path root "_events" (str event-id ".json"))) event)
    (doseq [participant (get event "participants")]
      (json/write-deterministic-json-file!
       (fs/file (fs/path root "_indexes" (str (get participant "person_id") ".json")))
       {"schema_id" person-drift/index-schema-id
        "schema_hash" (manifest/schema-hash person-drift/index-schema-path)
        "person_id" (get participant "person_id")
        "drift_event_ids" [event-id]}))))

(defn- literal-values [graph predicate]
  (->> (iterator-seq (.find graph))
       (filter #(= predicate (.getURI (.getPredicate %))))
       (mapv (fn [triple]
               (let [node (.getObject triple)]
                 [(.getLiteralLexicalForm node) (.getLiteralDatatypeURI node)])))))

(def ^:private material-clause-keys
  {:temporal-date-normalization
   #{:pad-month :pad-day :pad-year :strip-whitespace :collapse-multi-dash
     :normalize-separator :bce-astronomical :unknown-不詳 :unknown-未詳
     :calendar-impossible-uncorrected :range-invalid-uncorrected
     :decade-verbatim-no-correction :century-seven-source-phrase
     :century-six-source-phrase}
   :temporal-person-record-contract
   #{:date_of_birth-schema-table :date_of_death-schema-table
     :date_of_birth-xsd-and-edtf-dispatch :date_of_death-xsd-and-edtf-dispatch
     :date_of_birth-level-one-edtf-only :date_of_death-level-one-edtf-only
     :fixture-valid :fixture-live-schema-hash :fixture-exact-birth
     :fixture-exact-death :fixture-complete-temporal-rdf}
   :temporal-person-shacl-contract
   #{:rda-dateOfBirth-cardinality-and-datatypes
     :rda-dateOfDeath-cardinality-and-datatypes
     :edtfDateOfBirth-cardinality-datatype-pattern
     :edtfDateOfDeath-cardinality-datatype-pattern
     :malformed-edtf-rejected-with-path-and-shape}
   :person-drift-contract
   #{:event-schema-meta-valid :index-schema-meta-valid :event-schema-accepts-split
     :index-schema-accepts-sidecars :participant-required-fields
     :split-merge-cardinality-bounds :coherence-rejection-codes
     :snapshot-iri-embedded-hash :committed-exact-layout
     :committed-validation-counts :committed-shacl-ok :committed-typing-ok
     :shape-PersonDriftEventShape :shape-PersonDriftSplitEventShape
     :shape-PersonDriftMergeEventShape
     :subclass-drift-event-activity :subclass-split-event-drift-event
     :subclass-merge-event-drift-event
     :event-rdf-type :event-prov-activity :split-rdf-type :prov-used
     :prov-invalidated :prov-associated :prov-derived}
   :person-drift-negative-fixtures #{:exact-invalid-fixture-code-sets}
   :aozora-ingest-drift-invariants
   #{:sidecars-person-bytes :sidecars-metadata-bytes :sidecars-manifest-bytes
     :sidecars-manifest-identity :schema-rotation-exact-four-paths
     :schema-rotation-person-semantics-unchanged
     :schema-rotation-metadata-semantics-unchanged}
   :aozora-history-audit
   #{:bounded-update-exact :empty-sidecars-empty-report :invalid-sidecars-abort
     :integrated-two-ref-status :integrated-two-ref-updates
     :integrated-matched-event-ids}})

(defn- assert-material-clauses! [boundary clauses]
  (is (= (get material-clause-keys boundary) (set (keys clauses)))
      (str (name boundary) " clause set"))
  (doseq [[clause passes?] clauses]
    (is (true? passes?) (name clause)))
  true)

(defn- changed-json-paths* [path before after]
  (cond
    (and (map? before) (map? after))
    (into #{}
          (mapcat (fn [key]
                    (changed-json-paths* (conj path key)
                                         (get before key ::missing)
                                         (get after key ::missing)))
                  (set/union (set (keys before)) (set (keys after)))))

    (and (vector? before) (vector? after))
    (into #{}
          (mapcat (fn [index]
                    (changed-json-paths* (conj path index)
                                         (get before index ::missing)
                                         (get after index ::missing)))
                  (range (max (count before) (count after)))))

    (= before after) #{}
    :else #{path}))

(defn- changed-json-paths [before after]
  (changed-json-paths* [] before after))

(defn- graph-objects [^Graph graph subject predicate]
  (mapv #(.getObject %)
        (iterator-seq (.find graph subject predicate Node/ANY))))

(defn- unique-graph-object [^Graph graph subject predicate]
  (let [values (graph-objects graph subject predicate)]
    (when-not (= 1 (count values))
      (throw (ex-info "expected one graph object"
                      {:subject subject :predicate predicate :values values})))
    (first values)))

(defn- rdf-list-values [^Graph graph head]
  (let [rdf-first (NodeFactory/createURI "http://www.w3.org/1999/02/22-rdf-syntax-ns#first")
        rdf-rest (NodeFactory/createURI "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest")
        rdf-nil (NodeFactory/createURI "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil")]
    (loop [node head seen #{} values []]
      (cond
        (= rdf-nil node) values
        (contains? seen node) (throw (ex-info "cyclic RDF list" {:head head}))
        :else (recur (unique-graph-object graph node rdf-rest)
                     (conj seen node)
                     (conj values (unique-graph-object graph node rdf-first)))))))

(defn- property-shape [^Graph graph path]
  (let [shape (NodeFactory/createURI "https://w3id.org/abc/PersonRecordShape")
        property (NodeFactory/createURI "http://www.w3.org/ns/shacl#property")
        sh-path (NodeFactory/createURI "http://www.w3.org/ns/shacl#path")
        matches (filter #(some #{path} (graph-objects graph % sh-path))
                        (graph-objects graph shape property))]
    (when-not (= 1 (count matches))
      (throw (ex-info "expected one attached property shape" {:path path})))
    (first matches)))

(defn- graph-triple-uris [graph]
  (set (map (fn [triple]
              [(when (.isURI (.getSubject triple)) (.getURI (.getSubject triple)))
               (.getURI (.getPredicate triple))
               (cond
                 (.isURI (.getObject triple)) (.getURI (.getObject triple))
                 (.isLiteral (.getObject triple)) (.getLiteralLexicalForm (.getObject triple))
                 :else (str (.getObject triple)))])
            (iterator-seq (.find graph)))))

(defn- example-row []
  {"作品ID" "000127" "人物ID" "000879" "役割フラグ" "著者"
   "作品名" "羅生門" "作品名読み" "らしょうもん" "ソート用読み" "らしようもん"
   "副題" "" "副題読み" "" "原題" "" "初出" "" "分類番号" "NDC 913"
   "文字遣い種別" "新字新仮名" "作品著作権フラグ" "なし"
   "公開日" "1997-10-29" "最終更新日" "2022-07-16"
   "図書カードURL" "https://www.aozora.gr.jp/cards/000879/card127.html"
   "底本名1" "羅生門" "底本出版社名1" "テスト出版社" "底本名2" ""
   "底本出版社名2" "" "底本初版発行年1" "" "底本初版発行年2" ""
   "入力に使用した版1" "" "入力に使用した版2" "" "校正に使用した版1" ""
   "校正に使用した版2" "" "底本の親本名1" "" "底本の親本名2" ""
   "底本の親本出版社名1" "" "底本の親本出版社名2" ""
   "底本の親本初版発行年1" "" "底本の親本初版発行年2" ""
   "姓" "芥川" "名" "竜之介" "姓読み" "あくたがわ" "名読み" "りゅうのすけ"
   "姓読みソート用" "あくたかわ" "名読みソート用" "りゆうのすけ"
   "姓ローマ字" "Akutagawa" "名ローマ字" "Ryunosuke"
   "生年月日" "1892-03-01" "没年月日" "1927-07-24" "人物著作権フラグ" "なし"})

(defn- run-ingest [root with-sidecars?]
  (let [work-dir (fs/path root (if with-sidecars? "with/work" "without/work"))
        persons-dir (fs/path root (if with-sidecars? "with/persons" "without/persons"))
        manifest-path (fs/path work-dir "manifest.json")
        options {:rows [(example-row)] :work-id "000127"
                 :output (str (fs/path work-dir "metadata-record.json"))
                 :persons-output-dir (str persons-dir)
                 :refresh-manifest (str manifest-path)}]
    (manifest/write-json-file! (fs/file manifest-path)
                               (files/read-json "examples/v0/example-work/manifest.json"))
    (ingest/run-from-rows! options)
    (when with-sidecars?
      (doseq [source (conj index-paths event-path)
              :let [target (fs/path persons-dir
                                    (fs/file-name (fs/parent source))
                                    (fs/file-name source))]]
        (files/copy-file! source target))
      (ingest/run-from-rows! options))
    {:person (vec (files/read-bytes (fs/file (fs/path persons-dir "000879.json"))))
     :metadata (vec (files/read-bytes (fs/file (fs/path work-dir "metadata-record.json"))))
     :manifest (vec (files/read-bytes (fs/file manifest-path)))
     :identity (get (files/read-json (fs/file manifest-path)) "manifest_identity_object")
     :json {"person" (files/read-json (fs/file (fs/path persons-dir "000879.json")))
            "metadata" (files/read-json (fs/file (fs/path work-dir "metadata-record.json")))
            "manifest" (files/read-json (fs/file manifest-path))}}))

(defn- run-ingest-with-person-schema-hash [root suffix person-schema-hash]
  (with-redefs [malli/cached-schema-hash
                (fn [path]
                  (if (= path ingest/person-schema-path)
                    person-schema-hash
                    (manifest/schema-hash path)))]
    (:json (run-ingest (fs/path root suffix) false))))

(defn- write-person! [root family-name]
  (let [record {"person_record_schema_id" person-record/schema-id
                "person_record_schema_hash"
                (manifest/schema-hash "schemas/person-record.schema.json")
                "person_id" "000879" "family_name" family-name
                "given_name" "竜之介" "family_name_reading" "あくたがわ"
                "given_name_reading" "りゅうのすけ" "family_name_sort" "あくたかわ"
                "given_name_sort" "りゆうのすけ" "family_name_romaji" "Akutagawa"
                "given_name_romaji" "Ryunosuke" "date_of_birth" "1892-03-01"
                "date_of_death" "1927-07-24" "person_copyright_expired" true
                "external_links" []}]
    (json/write-deterministic-json-file!
     (fs/file (fs/path root "persons/000879.json")) record)
    record))

(defn- history-row [person-id family-name]
  (merge (example-row)
         {"作品ID" "000100" "人物ID" person-id
          "作品名" "テスト作品" "作品名読み" "てすとさくひん"
          "ソート用読み" "てすとさくひん"
          "図書カードURL" "https://www.aozora.gr.jp/cards/000001/card100.html"
          "姓" family-name "名" "人" "姓読み" "せい" "名読み" "ひと"
          "姓読みソート用" "せい" "名読みソート用" "ひと"
          "姓ローマ字" "Sei" "名ローマ字" "Hito"
          "生年月日" "1900-01-01" "没年月日" "1970-01-01"}))

(defn- csv-text [rows]
  (let [headers (vec (sort (keys (first rows))))]
    (->> (cons headers (map (fn [row] (mapv #(get row % "") headers)) rows))
         (map #(str/join "," %))
         (str/join "\n"))))

(defn- commit-csv-zip! [git root rows message]
  (sim-render/commit-zip-at!
   git (fs/file root) (sim-render/csv->zip-bytes (csv-text rows))
   message "2022-01-01T00:00:00Z"))

(defn- indexed-audit-event [previous-record]
  (let [successor-a (assoc previous-record
                           "person_id" "abc-000000000001" "family_name" "芥川一")
        successor-b (assoc previous-record
                           "person_id" "abc-000000000002" "family_name" "芥川二")]
    (person-drift/materialize-event-id
     {"schema_id" person-drift/event-schema-id
      "schema_hash" (manifest/schema-hash person-drift/event-schema-path)
      "drift_event_type" "split" "date" "2026-04-30"
      "participants" [{"snapshot_id" "post-abc-000000000001"
                       "person_id" "abc-000000000001"
                       "person_record_hash" (person-record/record-hash successor-a)}
                      {"snapshot_id" "post-abc-000000000002"
                       "person_id" "abc-000000000002"
                       "person_record_hash" (person-record/record-hash successor-b)}
                      {"snapshot_id" "pre-000879" "person_id" "000879"
                       "person_record_hash" (person-record/record-hash previous-record)}]
      "evidence" ["https://example.org/drift-evidence"]
      "prov" {"used" ["pre-000879"]
              "was_generated_by" ["post-abc-000000000001"
                                  "post-abc-000000000002"]
              "qualified_association"
              {"agent" "https://w3id.org/abc/agents/test"
               "had_role" "abc:DriftEditor"}}})))

(deftest temporal-date-normalization-contract
  (let [expected-cases
        {:pad-month ["1888-6-12" "1888-06-12" ["pad-month"]]
         :pad-day ["1888-06-1" "1888-06-01" ["pad-day"]]
         :pad-year ["723-08-15" "0723-08-15" ["pad-year"]]
         :strip-whitespace ["1869- 02-22" "1869-02-22" ["strip-whitespace"]]
         :collapse-multi-dash ["1850-08--18" "1850-08-18" ["collapse-multi-dash"]]
         :normalize-separator ["1839.1.1" "1839-01-01"
                               ["normalize-date-separator" "pad-month" "pad-day"]]
         :bce-astronomical ["前347" "-0346" ["bce-astronomical"]]}
        correction-clauses
        (into {}
              (map (fn [[clause [raw corrected rules]]]
                     (let [[actual corrections] (aozora-csv/parse-date raw)]
                       [clause
                        (and (= corrected actual)
                             (= rules (mapv #(get % "rule") corrections))
                             (every? #(= raw (get % "raw")) corrections)
                             (every? #(= corrected (get % "corrected")) corrections))]))
                   expected-cases))
        unknown-clauses
        (into {}
              (map (fn [raw]
                     (let [[actual corrections] (aozora-csv/parse-date raw)]
                       [(keyword (str "unknown-" raw))
                        (= [nil [{"raw" raw "corrected" nil
                                  "rule" "unknown-marker"}]]
                           [actual corrections])]))
                   ["不詳" "未詳"]))
        passthrough-clauses
        {:calendar-impossible-uncorrected
         (= ["2020-02-31" []] (aozora-csv/parse-date "2020-02-31"))
         :range-invalid-uncorrected
         (= ["1892-13" []] (aozora-csv/parse-date "1892-13"))
         :decade-verbatim-no-correction
         (every? #(= [% []] (aozora-csv/parse-date %)) ["192X" "-019X"])
         :century-seven-source-phrase
         (= ["-06XX" [{"raw" "紀元前7世紀末" "corrected" "-06XX"
                       "rule" "century-prose"}]]
            (aozora-csv/parse-date "紀元前7世紀末"))
         :century-six-source-phrase
         (= ["-05XX" [{"raw" "紀元前6世紀初" "corrected" "-05XX"
                       "rule" "century-prose"}]]
            (aozora-csv/parse-date "紀元前6世紀初"))}]
    (assert-material-clauses! :temporal-date-normalization
                              (merge correction-clauses unknown-clauses passthrough-clauses))))

(deftest temporal-person-record-contract
  (let [schema-value (files/read-json "schemas/person-record.schema.json")
        record (files/read-json "examples/v0/example-persons/000879.json")
        adr-0015-accepted
        [nil "1892-03-01" "1904-01" "1941"
         "-0426-01-15" "-0426-01" "-0426"]
        adr-0016-accepted ["192X" "-019X" "-06XX"]
        accepted (into adr-0015-accepted adr-0016-accepted)
        rejected ["1892?" "1984/1999" "{1984, 1986}"
                  "1892-00" "1892-13" "1892-01-00" "1892-01-32"]
        field-cases [["date_of_birth" "dateOfBirth" "edtfDateOfBirth"]
                     ["date_of_death" "dateOfDeath" "edtfDateOfDeath"]]
        xsd-cases [["1892-03-01" "http://www.w3.org/2001/XMLSchema#date"]
                   ["1904-01" "http://www.w3.org/2001/XMLSchema#gYearMonth"]
                   ["1941" "http://www.w3.org/2001/XMLSchema#gYear"]]
        schema-clauses
        (into {}
              (for [[field _ _] field-cases]
                [(keyword (str field "-schema-table"))
                 (and (every? #(empty? (schema/validation-errors
                                        schema-value (assoc record field %)))
                              accepted)
                      (every? #(seq (schema/validation-errors
                                     schema-value (assoc record field %)))
                              rejected))]))
        rdf-clauses
        (into {}
              (for [[field rda-local edtf-local] field-cases]
                [(keyword (str field "-xsd-and-edtf-dispatch"))
                 (every?
                  (fn [[value datatype]]
                    (let [graph (person-record/record->graph
                                 (assoc record field value))]
                      (and (= [[value datatype]]
                              (literal-values graph
                                              (str "http://RDVocab.info/ElementsGr2/"
                                                   rda-local)))
                           (= [[value "https://w3id.org/abc/EDTF"]]
                              (literal-values graph
                                              (str "https://w3id.org/abc/"
                                                   edtf-local))))))
                  xsd-cases)]))
        level-one-clauses
        (into {}
              (for [[field rda-local edtf-local] field-cases]
                [(keyword (str field "-level-one-edtf-only"))
                 (every?
                  (fn [value]
                    (let [graph (person-record/record->graph
                                 (assoc record field value))]
                      (and (empty? (literal-values
                                    graph (str "http://RDVocab.info/ElementsGr2/"
                                               rda-local)))
                           (= [[value "https://w3id.org/abc/EDTF"]]
                              (literal-values graph
                                              (str "https://w3id.org/abc/"
                                                   edtf-local))))))
                  ["192X" "-019X" "-06XX"])]))
        fixture-graph (person-record/record->graph record)
        fixture-clauses
        {:fixture-valid (empty? (schema/validation-errors schema-value record))
         :fixture-live-schema-hash
         (= (manifest/schema-hash "schemas/person-record.schema.json")
            (get record "person_record_schema_hash"))
         :fixture-exact-birth (= "1892-03-01" (get record "date_of_birth"))
         :fixture-exact-death (= "1927-07-24" (get record "date_of_death"))
         :fixture-complete-temporal-rdf
         (and (= [["1892-03-01" "http://www.w3.org/2001/XMLSchema#date"]]
                 (literal-values fixture-graph
                                 "http://RDVocab.info/ElementsGr2/dateOfBirth"))
              (= [["1927-07-24" "http://www.w3.org/2001/XMLSchema#date"]]
                 (literal-values fixture-graph
                                 "http://RDVocab.info/ElementsGr2/dateOfDeath"))
              (= [["1892-03-01" "https://w3id.org/abc/EDTF"]]
                 (literal-values fixture-graph
                                 "https://w3id.org/abc/edtfDateOfBirth"))
              (= [["1927-07-24" "https://w3id.org/abc/EDTF"]]
                 (literal-values fixture-graph
                                 "https://w3id.org/abc/edtfDateOfDeath")))}]
    (assert-material-clauses! :temporal-person-record-contract
                              (merge schema-clauses rdf-clauses level-one-clauses fixture-clauses))))

(deftest temporal-person-shacl-contract
  (let [shapes (shacl/load-shapes-graph)
        sh-or (NodeFactory/createURI "http://www.w3.org/ns/shacl#or")
        sh-datatype (NodeFactory/createURI "http://www.w3.org/ns/shacl#datatype")
        sh-max-count (NodeFactory/createURI "http://www.w3.org/ns/shacl#maxCount")
        sh-pattern (NodeFactory/createURI "http://www.w3.org/ns/shacl#pattern")
        abc-edtf (NodeFactory/createURI "https://w3id.org/abc/EDTF")
        expected-xsd #{(NodeFactory/createURI "http://www.w3.org/2001/XMLSchema#date")
                       (NodeFactory/createURI "http://www.w3.org/2001/XMLSchema#gYearMonth")
                       (NodeFactory/createURI "http://www.w3.org/2001/XMLSchema#gYear")}
        expected-max #{(NodeFactory/createLiteral "1" XSDDatatype/XSDinteger)}
        expected-pattern
        "^(-?\\d{4}(-(0[1-9]|1[0-2])(-(0[1-9]|[12][0-9]|3[01]))?)?|-?\\d{3}X|-?\\d{2}XX)$"
        paths [["dateOfBirth" "edtfDateOfBirth"]
               ["dateOfDeath" "edtfDateOfDeath"]]
        rda-clauses
        (into {}
              (for [[local _] paths
                    :let [property (property-shape
                                    shapes
                                    (NodeFactory/createURI
                                     (str "http://RDVocab.info/ElementsGr2/" local)))
                          alternatives
                          (->> (rdf-list-values
                                shapes (unique-graph-object shapes property sh-or))
                               (map #(unique-graph-object shapes % sh-datatype))
                               set)]]
                [(keyword (str "rda-" local "-cardinality-and-datatypes"))
                 (and (= expected-max (set (graph-objects shapes property sh-max-count)))
                      (= expected-xsd alternatives))]))
        edtf-clauses
        (into {}
              (for [[_ edtf-local] paths
                    :let [property (property-shape
                                    shapes
                                    (NodeFactory/createURI
                                     (str "https://w3id.org/abc/" edtf-local)))]]
                [(keyword (str edtf-local "-cardinality-datatype-pattern"))
                 (and (= expected-max (set (graph-objects shapes property sh-max-count)))
                      (= #{abc-edtf} (set (graph-objects shapes property sh-datatype)))
                      (= expected-pattern
                         (.getLiteralLexicalForm
                          (unique-graph-object shapes property sh-pattern))))]))
        datatype (BaseDatatype. "https://w3id.org/abc/EDTF")
        person {:rdf/about "<http://www.aozora.gr.jp/index_pages/person001234.html>"
                :rdf/type [:foaf/Person]
                :dcterms/identifier (NodeFactory/createLiteral "1234" XSDDatatype/XSDint)
                :foaf/familyName "Test" :foaf/givenName "Person"
                :foaf/name "Test Person"
                :abc/edtfDateOfBirth (NodeFactory/createLiteral "1892?" datatype)}
        data (-> (aa/graph :simple) (aa/add person))
        diagnostic-path-uri "https://w3id.org/abc/edtfDateOfBirth"
        expected-diagnostic-path (str "<" diagnostic-path-uri ">")
        expected-diagnostic-source
        (str (property-shape shapes
                             (NodeFactory/createURI diagnostic-path-uri)))
        diagnostics
        (try
          (shacl/validate! {:shapes-graph shapes :data-graph data
                            :label "bad-edtf-dob"})
          []
          (catch clojure.lang.ExceptionInfo exception
            (:errors (ex-data exception))))
        diagnostic-clause
        {:malformed-edtf-rejected-with-path-and-shape
         (boolean
          (and (seq diagnostics)
               (some (fn [violation]
                       (and (= expected-diagnostic-path (:path violation))
                            (= expected-diagnostic-source (:source violation))
                            (re-find #"(?i)pattern"
                                     (str (:message violation)))))
                     diagnostics)))}]
    (assert-material-clauses! :temporal-person-shacl-contract
                              (merge rda-clauses edtf-clauses diagnostic-clause))))

(deftest person-drift-contract
  (let [event-schema (files/read-json person-drift/event-schema-path)
        index-schema (files/read-json person-drift/index-schema-path)
        event (files/read-json event-path)
        indexes (mapv files/read-json index-paths)
        graph (person-drift/event->graph event)
        triples (graph-triple-uris graph)
        event-iri (person-drift/event-iri (get event "drift_event_id"))
        first-participant (first (get event "participants"))
        first-snapshot (person-drift/snapshot-iri first-participant)
        expected-first-snapshot
        (str "https://w3id.org/abc/persons/"
             (get first-participant "person_id")
             "#snapshot-"
             (subs (get first-participant "person_record_hash")
                   (count "sha256:") (+ (count "sha256:") 12)))
        mutated-participant
        (assoc first-participant "person_record_hash"
               (str "sha256:f"
                    (subs (get first-participant "person_record_hash")
                          (inc (count "sha256:")))))
        expected-mutated-snapshot
        (str "https://w3id.org/abc/persons/"
             (get first-participant "person_id") "#snapshot-f"
             (subs (get first-participant "person_record_hash")
                   (inc (count "sha256:"))
                   (+ (count "sha256:") 12)))
        participants-by-id
        (into {} (map (fn [participant]
                        [(get participant "snapshot_id") participant])
                      (get event "participants")))
        used-snapshots
        (mapv #(person-drift/snapshot-iri (get participants-by-id %))
              (get-in event ["prov" "used"]))
        generated-snapshots
        (mapv #(person-drift/snapshot-iri (get participants-by-id %))
              (get-in event ["prov" "was_generated_by"]))
        agent-iri (get-in event ["prov" "qualified_association" "agent"])
        split (base-event)
        merge-event (base-merge-event)
        cardinality-mutations
        [(assoc-in split ["prov" "used"] [])
         (assoc-in split ["prov" "used"] ["pre-000879" "pre-other"])
         (assoc-in split ["prov" "was_generated_by"]
                   ["post-abc-000000000001"])
         (assoc-in merge-event ["prov" "used"] ["pre-abc-000000000001"])
         (assoc-in merge-event ["prov" "was_generated_by"] [])
         (assoc-in merge-event ["prov" "was_generated_by"]
                   ["post-000879" "post-other"])]
        coherence-mutations
        {:duplicate-snapshot
         [(update split "participants" conj (first (get split "participants")))
          :duplicate-snapshot-id]
         :interleaved
         [(assoc-in split ["prov" "used"]
                    ["pre-000879" "post-abc-000000000001"])
          :participant-in-both-used-and-generated]
         :prefix-usage
         [(-> split
              (assoc-in ["prov" "used"] ["post-abc-000000000001"])
              (assoc-in ["prov" "was_generated_by"]
                        ["post-abc-000000000002" "pre-000879"]))
          :snapshot-prefix-usage-mismatch]
         :participants-order
         [(update split "participants" #(vec (reverse %)))
          :participants-not-sorted]
         :edge-order
         [(update-in split ["prov" "was_generated_by"] #(vec (reverse %)))
          :generated-not-sorted]
         :used-edge-order
         [(update-in merge-event ["prov" "used"] #(vec (reverse %)))
          :used-not-sorted]
         :unknown-snapshot
         [(assoc-in split ["prov" "used"] ["pre-missing"])
          :unknown-snapshot-reference]
         :uncovered-participant
         [(update split "participants" conj
                  {"snapshot_id" "post-abc-000000000003"
                   "person_id" "abc-000000000003"
                   "person_record_hash" (files/example-hash "04")})
          :participant-not-covered]
         :invalid-role
         [(assoc-in split ["prov" "qualified_association" "had_role"]
                    "abc:DriftReviewer")
          :invalid-had-role]
         :unknown-role-prefix
         [(assoc-in split ["prov" "qualified_association" "had_role"]
                    "unknown:DriftEditor")
          :unresolved-curie-prefix]
         :invalid-agent
         [(assoc-in split ["prov" "qualified_association" "agent"] "not an iri")
          :invalid-agent-iri]}
        shapes (shacl/load-shapes-graph)
        rdf-type (NodeFactory/createURI
                  "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
        node-shape (NodeFactory/createURI "http://www.w3.org/ns/shacl#NodeShape")
        subclass (NodeFactory/createURI
                  "http://www.w3.org/2000/01/rdf-schema#subClassOf")
        resource-clauses
        (into {}
              (concat
               (for [name ["PersonDriftEventShape" "PersonDriftSplitEventShape"
                           "PersonDriftMergeEventShape"]]
                 [(keyword (str "shape-" name))
                  (.contains shapes
                             (Triple/create
                              (NodeFactory/createURI (str "https://w3id.org/abc/" name))
                              rdf-type node-shape))])
               (for [[label child parent]
                     [[:subclass-drift-event-activity
                       "https://w3id.org/abc/DriftEvent"
                       "http://www.w3.org/ns/prov#Activity"]
                      [:subclass-split-event-drift-event
                       "https://w3id.org/abc/DriftSplitEvent"
                       "https://w3id.org/abc/DriftEvent"]
                      [:subclass-merge-event-drift-event
                       "https://w3id.org/abc/DriftMergeEvent"
                       "https://w3id.org/abc/DriftEvent"]]]
                 [label
                  (.contains shapes
                             (Triple/create (NodeFactory/createURI child)
                                            subclass
                                            (NodeFactory/createURI parent)))])))
        graph-clauses
        {:event-rdf-type
         (contains? triples [event-iri
                             "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
                             "https://w3id.org/abc/DriftEvent"])
         :event-prov-activity
         (contains? triples [event-iri
                             "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
                             "http://www.w3.org/ns/prov#Activity"])
         :split-rdf-type
         (contains? triples [event-iri
                             "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
                             "https://w3id.org/abc/DriftSplitEvent"])
         :prov-used
         (every? #(contains? triples
                             [event-iri "http://www.w3.org/ns/prov#used" %])
                 used-snapshots)
         :prov-invalidated
         (every? #(contains? triples
                             [% "http://www.w3.org/ns/prov#wasInvalidatedBy"
                              event-iri])
                 used-snapshots)
         :prov-associated
         (contains? triples [event-iri
                             "http://www.w3.org/ns/prov#wasAssociatedWith"
                             agent-iri])
         :prov-derived
         (and
          (every? #(contains? triples
                              [% "http://www.w3.org/ns/prov#wasGeneratedBy"
                               event-iri])
                  generated-snapshots)
          (every? (fn [[generated used]]
                    (contains? triples
                               [generated
                                "http://www.w3.org/ns/prov#wasDerivedFrom"
                                used]))
                  (for [generated generated-snapshots
                        used used-snapshots]
                    [generated used])))}
        contract-clauses
        {:event-schema-meta-valid (nil? (schema/schema-valid! event-schema "event"))
         :index-schema-meta-valid (nil? (schema/schema-valid! index-schema "index"))
         :event-schema-accepts-split (empty? (schema/validation-errors event-schema split))
         :index-schema-accepts-sidecars
         (every? #(empty? (schema/validation-errors index-schema %)) indexes)
         :participant-required-fields
         (every? (fn [field]
                   (seq (schema/validation-errors
                         event-schema
                         (update event "participants"
                                 #(mapv (fn [participant] (dissoc participant field)) %)))))
                 ["snapshot_id" "person_id" "person_record_hash"])
         :split-merge-cardinality-bounds
         (every? #(seq (schema/validation-errors event-schema %))
                 cardinality-mutations)
         :coherence-rejection-codes
         (every? (fn [[mutated expected]]
                   (contains? (coherence-codes mutated) expected))
                 (vals coherence-mutations))
         :snapshot-iri-embedded-hash
         (and (= expected-first-snapshot first-snapshot)
              (= (dissoc first-participant "person_record_hash")
                 (dissoc mutated-participant "person_record_hash"))
              (= expected-mutated-snapshot
                 (person-drift/snapshot-iri mutated-participant))
              (not= first-snapshot expected-mutated-snapshot))
         :committed-exact-layout
         (and (= 1 (count (files/list-files
                           "examples/v0/example-persons/_events")))
              (= 3 (count (files/list-files
                           "examples/v0/example-persons/_indexes"))))
         :committed-validation-counts
         (= {:status :ok :events 1 :indexes 3}
            (person-drift/validate-drift-events!
             {:persons-dir "examples/v0/example-persons"}))
         :committed-shacl-ok
         (= :ok (person-drift/validate-event-shacl! graph "committed split"))
         :committed-typing-ok
         (empty? (person-drift/typing-coherence-failures event graph))}]
    (assert-material-clauses! :person-drift-contract
                              (merge contract-clauses resource-clauses graph-clauses))))

(def ^:private negative-fixtures
  {"fixtures/v0/invalid/drift/broken-index-target" #{:index-target-missing}
   "fixtures/v0/invalid/drift/asymmetric-index" #{:event-missing-from-participant-index}
   "fixtures/v0/invalid/drift/orphan-event-file"
   #{:orphan-event-file :event-missing-from-participant-index}
   "fixtures/v0/invalid/drift/unsorted-participants"
   #{:participants-not-sorted :index-target-missing
     :event-missing-from-participant-index :orphan-event-file}
   "fixtures/v0/invalid/drift/dangling-snapshot-ref"
   #{:unknown-snapshot-reference :participant-not-covered :index-target-missing
     :event-missing-from-participant-index :orphan-event-file}
   "fixtures/v0/invalid/drift/invalid-role"
   #{:invalid-had-role :index-target-missing :event-missing-from-participant-index
     :orphan-event-file}
   "fixtures/v0/invalid/drift/invalid-agent"
   #{:invalid-agent-iri :index-target-missing :event-missing-from-participant-index
     :orphan-event-file}
   {:type :ttl :event event-path
    :graph "fixtures/v0/invalid/drift/shacl-missing-date/graph.ttl"}
   #{:shacl-violation}
   {:type :ttl :event event-path
    :graph "fixtures/v0/invalid/drift/split-cardinality-one-successor/graph.ttl"}
   #{:shacl-violation :rdf-participant-prov-mismatch}
   {:type :ttl :event "fixtures/v0/invalid/drift/merge-cardinality-one-predecessor/event.json"
    :graph "fixtures/v0/invalid/drift/merge-cardinality-one-predecessor/graph.ttl"}
   #{:shacl-violation}
   {:type :ttl :event event-path
    :graph "fixtures/v0/invalid/drift/typing-missing-subclass/graph.ttl"}
   #{:missing-rdf-type :rdf-participant-prov-mismatch}
   {:type :ttl :event event-path
    :graph "fixtures/v0/invalid/drift/typing-missing-activity/graph.ttl"}
   #{:missing-rdf-type :shacl-violation :rdf-participant-prov-mismatch}
   {:type :ttl :event event-path
    :graph "fixtures/v0/invalid/drift/rdf-participant-prov-mismatch/graph.ttl"}
   #{:rdf-participant-prov-mismatch}})

(deftest person-drift-negative-fixtures-contract
  (fs/with-temp-dir [root {:prefix "abc-test-"}]
    (files/read-json person-drift/event-schema-path)
    (files/read-json person-drift/index-schema-path)
    (shacl/load-shapes-graph)
    (doseq [path (->> (keys negative-fixtures)
                      (filter map?)
                      (map (fn [fixture] (:graph fixture)))
                      distinct)]
      (files/read-bytes path))
    (let [ephemeral-event (fs/path root "event.json")
          fixtures (reduce-kv
                    (fn [result fixture expected]
                      (assoc result
                             (if (and (map? fixture)
                                      (= event-path (:event fixture)))
                               (assoc fixture :event (str ephemeral-event))
                               fixture)
                             expected))
                    {}
                    negative-fixtures)]
      (json/write-deterministic-json-file! (fs/file ephemeral-event) (base-event))
      (assert-material-clauses!
       :person-drift-negative-fixtures
       {:exact-invalid-fixture-code-sets
        (nil? (validate/validate-drift-fixtures! fixtures))}))))

(deftest aozora-ingest-drift-invariants-contract
  (fs/with-temp-dir [root {:prefix "abc-test-"}]
    (files/read-json "schemas/person-record.schema.json")
    (files/read-json "schemas/metadata-record.schema.json")
    (files/read-json "schemas/manifest.schema.json")
    (let [without (run-ingest root false)
          with (run-ingest root true)
          baseline (run-ingest-with-person-schema-hash
                    root "schema-live"
                    (manifest/schema-hash "schemas/person-record.schema.json"))
          rotated (run-ingest-with-person-schema-hash
                   root "schema-rotated"
                   "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
          expected-paths
          #{["person" "person_record_schema_hash"]
            ["metadata" "contributors" 0 "person_record_hash"]
            ["manifest" "manifest_identity_object" "metadata_record_hash"]
            ["manifest" "artifact_id"]}
          clauses
          {:sidecars-person-bytes (= (:person without) (:person with))
           :sidecars-metadata-bytes (= (:metadata without) (:metadata with))
           :sidecars-manifest-bytes (= (:manifest without) (:manifest with))
           :sidecars-manifest-identity (= (:identity without) (:identity with))
           :schema-rotation-exact-four-paths
           (= expected-paths (changed-json-paths baseline rotated))
           :schema-rotation-person-semantics-unchanged
           (= (dissoc (get baseline "person") "person_record_schema_hash")
              (dissoc (get rotated "person") "person_record_schema_hash"))
           :schema-rotation-metadata-semantics-unchanged
           (= (get baseline "metadata")
              (assoc-in (get rotated "metadata")
                        ["contributors" 0 "person_record_hash"]
                        (get-in baseline
                                ["metadata" "contributors" 0
                                 "person_record_hash"])))}]
      (assert-material-clauses! :aozora-ingest-drift-invariants clauses))))

(deftest aozora-history-audit-contract
  (fs/with-temp-dir [root {:prefix "abc-test-"}]
    (doseq [path ["schemas/person-record.schema.json"
                  "schemas/metadata-record.schema.json"
                  person-drift/event-schema-path person-drift/index-schema-path]]
      (files/read-json path))
    (shacl/load-shapes-graph)
    (let [previous (fs/path root "previous")
          current (fs/path root "current")
          drift-root (fs/path root "drift")
          before (write-person! previous "芥川")
          after (write-person! current "芥川改")
          event (indexed-audit-event before)]
      (write-sidecars! drift-root event)
      (let [updates (history-audit/drift-participant-updates
                     {:previous-dir (str previous)
                      :current-dir (str current)
                      :drift-persons-dir (str drift-root)})
            empty-drift (fs/path root "empty-drift")
            invalid-drift (fs/path root "invalid-drift")
            invalid-index (fs/path invalid-drift "_indexes/000879.json")
            _ (json/write-deterministic-json-file!
               (fs/file invalid-index)
               {"schema_id" person-drift/index-schema-id
                "schema_hash" (manifest/schema-hash person-drift/index-schema-path)
                "person_id" "000879"
                "drift_event_ids"
                ["sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"]})
            repo-root (fs/path root "audit-repo")
            work-root (fs/path root "audit-work")
            integrated-drift (fs/path root "integrated-drift")
            git (sim-render/init-repo! repo-root)]
        (try
          (let [previous-ref (.getName
                              (commit-csv-zip!
                               git repo-root
                               [(history-row "000879" "芥川")]
                               "previous corpus"))
                current-ref (.getName
                             (commit-csv-zip!
                              git repo-root
                              [(history-row "abc-000000000001" "芥川一")
                               (history-row "abc-000000000002" "芥川二")]
                              "current corpus"))
                integrated-event (base-event)
                _ (write-sidecars! integrated-drift integrated-event)
                report (history-audit/audit!
                        {:aozora-repo (str repo-root)
                         :previous-ref previous-ref
                         :current-ref current-ref
                         :drift-persons-dir (str integrated-drift)
                         :work-dir (str work-root)})
                empty-report
                (history-audit/audit!
                 {:aozora-repo (str repo-root)
                  :previous-ref previous-ref
                  :current-ref previous-ref
                  :drift-persons-dir (str empty-drift)
                  :work-dir (str (fs/path root "empty-audit-work"))})
                invalid-aborts?
                (try
                  (history-audit/audit!
                   {:aozora-repo (str repo-root)
                    :previous-ref previous-ref
                    :current-ref current-ref
                    :drift-persons-dir (str invalid-drift)
                    :work-dir (str (fs/path root "invalid-audit-work"))})
                  false
                  (catch clojure.lang.ExceptionInfo exception
                    (boolean (re-find #"drift sidecars failed validation"
                                      (.getMessage exception)))))
                report-updates (:drift_participant_updates report)
                clauses
                {:bounded-update-exact
                 (= [{"person_id" "000879"
                      "change_type" "hash_changed"
                      "previous_hash" (person-record/record-hash before)
                      "current_hash" (person-record/record-hash after)
                      "drift_event_ids" [(get event "drift_event_id")]}]
                    updates)
                 :empty-sidecars-empty-report
                 (and (= "ok" (:status empty-report))
                      (= [] (:drift_participant_updates empty-report))
                      (zero? (get-in empty-report [:validation :current :failed]))
                      (zero? (get-in empty-report
                                     [:drift "summary" "split_candidates"]))
                      (zero? (get-in empty-report
                                     [:drift "summary" "merge_candidates"])))
                 :invalid-sidecars-abort invalid-aborts?
                 :integrated-two-ref-status (= "ok" (:status report))
                 :integrated-two-ref-updates
                 (= #{"000879" "abc-000000000001" "abc-000000000002"}
                    (set (map #(get % "person_id") report-updates)))
                 :integrated-matched-event-ids
                 (every? #(= [(get integrated-event "drift_event_id")]
                             (get % "drift_event_ids"))
                         report-updates)}]
            (assert-material-clauses! :aozora-history-audit clauses))
          (finally
            (.close git)))))))

(deftest material-boundaries-recompute-from-mutated-domain-values-test
  (let [schema-value (files/read-json "schemas/person-record.schema.json")
        record (files/read-json "examples/v0/example-persons/000879.json")
        signed-date (assoc record "date_of_birth" "-0426-01-15")
        invalid-date (assoc record "date_of_birth" "1892-01-00")
        participant (first (get (base-event) "participants"))
        original (person-drift/snapshot-iri participant)
        mutated (person-drift/snapshot-iri
                 (assoc participant "person_record_hash"
                        (str "sha256:f"
                             (subs (get participant "person_record_hash")
                                   (inc (count "sha256:"))))))
        malformed-date (aozora-csv/parse-date "1892-13-40")
        normalized-date (aozora-csv/parse-date " 1892-1-2 ")
        coherent (base-event)
        ordered-merge (base-merge-event)
        unordered-merge (update-in ordered-merge ["prov" "used"]
                                   #(vec (reverse %)))
        incoherent (assoc-in coherent ["prov" "used"] ["post-missing"])]
    (is (empty? (schema/validation-errors schema-value signed-date)))
    (is (seq (schema/validation-errors schema-value invalid-date)))
    (is (not= original mutated))
    (is (= ["1892-13-40" []] malformed-date))
    (is (= "1892-01-02" (first normalized-date)))
    (is (not= malformed-date normalized-date))
    (is (empty? (coherence-codes coherent)))
    (is (empty? (coherence-codes ordered-merge)))
    (is (contains? (coherence-codes unordered-merge) :used-not-sorted))
    (is (contains? (coherence-codes incoherent) :unknown-snapshot-reference))))
