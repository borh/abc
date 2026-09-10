(ns soranoha.assessment.rdf-test
  (:require [babashka.fs :as fs]
            [charred.api :as json]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [soranoha.assessment.evaluate :as evaluate]
            [soranoha.assessment.rdf :as rdf]
            [soranoha.assessment.records :as records]
            [soranoha.assessment.snapshot :as snapshot]
            [soranoha.kura.cas :as cas]
            [soranoha.kura.engine :as engine]))

(def ^:private options
  {:base-iri "https://example.invalid/assessment/"
   :mapping-profile rdf/default-mapping-profile
   :toolchain-id "rdf-test"})

(def ^:private awkward-text "根拠 \"quoted\" \\ path\nline\rreturn\ttab\bback\fform\u0000null 😀")

(def ^:private captured-evidence (str "sha256:" (records/fingerprint "captured evidence")))

(defn- finding [id subject value]
  {"id" id "fact" (records/fact-key subject "death-year") "value" value
   "effective_date" "2020-01-01" "reviewed_at" "2020-01-02"
   "assessor" "審査 者/#" "method" "document-review" "basis" awkward-text
   "premises" [{"kind" "observation" "ref" "evidence"
                "fingerprint" (records/fingerprint captured-evidence)}]})

(defn- source []
  (assoc records/empty-source
         "observations" [{"id" "evidence" "selector" "canonical-source-bundle" "slug" "work"}]
         "findings" [(finding "current" "person:000001" 1902)
                     (finding "historical" "person:000001" 1901)
                     (finding "rejected" "person:000002" 1900)
                     (assoc-in (finding "stale" "person:000003" 1903)
                               ["premises" 0 "fingerprint"] (records/fingerprint "old evidence"))]
         "controls" [{"id" "replace-old" "kind" "supersession"
                      "target" "historical" "replacement" "current"}
                     {"id" "reject" "kind" "withdrawal" "target" "rejected"}]))

(defn- evaluate-view [store source]
  (evaluate/evaluate! store source
                      {:observations {"evidence" captured-evidence}
                       :candidates {} :as-of "2026-09-05" :toolchain-id "evaluator-test"}))

(defn- with-store [f]
  (let [dir (fs/create-temp-dir {:prefix "assessment-rdf-test-"})
        store (engine/open-store! {:cas-dir (str (fs/path dir "cas"))
                                   :db-path (str (fs/path dir "trace.sqlite"))})]
    (try (f store)
         (finally (engine/close-store! store) (fs/delete-tree dir)))))

(def ^:private query-probe
  (str/join
   "\n"
   ["import json, sys"
    "from rdflib import Dataset, URIRef, Literal, Namespace"
    "from rdflib.namespace import XSD"
    "payload = json.load(sys.stdin)"
    "dataset = Dataset(default_union=False)"
    "dataset.parse(data=payload['nquads'], format='nquads')"
    "v = Namespace('urn:soranoha:assessment:')"
    "prov = Namespace('http://www.w3.org/ns/prov#')"
    "accepted = URIRef('https://example.invalid/assessment/accepted')"
    "prefix = 'PREFIX a: <urn:soranoha:assessment:> PREFIX prov: <http://www.w3.org/ns/prov#> '"
    "def query(body): return list(dataset.query(prefix + body))"
    "rows = query('SELECT ?value ?date ?jurisdiction WHERE { GRAPH <https://example.invalid/assessment/accepted> { ?fact a:value ?value; a:effectiveDate ?date; a:jurisdiction ?jurisdiction } }')"
    "assert len(rows) == 1, rows"
    "assert rows[0][0] == Literal(1902) and rows[0][0].datatype == XSD.integer, rows"
    "assert str(rows[0][1]) == '2020-01-01' and rows[0][1].datatype == XSD.date, rows"
    "assert str(rows[0][2]) == 'jp', rows"
    "claims = query('SELECT ?id ?state ?value WHERE { ?graph a:recordId ?id; a:applicability ?state . GRAPH ?graph { ?fact a:value ?value } }')"
    "assert {(str(i),str(s),int(v)) for i,s,v in claims} == {('current','available',1902),('historical','unavailable',1901),('rejected','unavailable',1900),('stale','unavailable',1903)}, claims"
    "disagreement = query('SELECT ?left ?right WHERE { ?g1 a:recordId ?left . ?g2 a:recordId ?right . GRAPH ?g1 { ?fact a:value ?v1 } GRAPH ?g2 { ?fact a:value ?v2 } FILTER (?v1 != ?v2) }')"
    "assert {(str(a),str(b)) for a,b in disagreement} == {('current','historical'),('historical','current')}, disagreement"
    "reasons = query('SELECT ?id ?reason WHERE { ?graph a:recordId ?id; a:unavailabilityReason ?reason }')"
    "assert {(str(i),str(r)) for i,r in reasons} == {('historical','revoked-support'),('rejected','revoked-support'),('stale','stale-premise')}, reasons"
    "controls = query('SELECT ?kind ?id WHERE { ?control a ?kind; a:target ?graph . ?graph a:recordId ?id }')"
    "assert {(str(k),str(i)) for k,i in controls} == {(str(v.supersession),'historical'),(str(v.withdrawal),'rejected')}, controls"
    "subjects = query('SELECT ?subject WHERE { GRAPH <https://example.invalid/assessment/accepted> { ?fact a:subject ?subject } }')"
    "assert subjects[0][0] == URIRef('https://example.invalid/assessment/subject/person%3A000001'), subjects"
    "profiles = query('SELECT ?profile WHERE { ?view a:mappingProfile ?profile }')"
    "assert profiles[0][0].datatype == URIRef('http://www.w3.org/1999/02/22-rdf-syntax-ns#JSON') and json.loads(str(profiles[0][0])) == {'vocabulary':'urn:soranoha:assessment:'}, profiles"
    "basis = query('SELECT ?basis WHERE { ?graph a:recordId ?id; a:basis ?basis }')"
    "assert len(basis) == 4 and all(str(row[0]) == payload['basis'] for row in basis), basis"
    "roles = query('SELECT ?role WHERE { ?activity prov:qualifiedUsage ?usage . ?usage prov:entity ?entity; prov:hadRole ?role }')"
    "assert len(roles) >= 4 and all(row[0] == v['evidence-version'] for row in roles), roles"
    "dates = query('SELECT ?date WHERE { ?activity a:reviewedAt ?date }')"
    "assert len(dates) == 4 and all(row[0].datatype == XSD.date for row in dates), dates"
    "assert not list(dataset.default_graph.triples((None,v.value,None)))"
    "again = Dataset(default_union=False)"
    "again.parse(data=dataset.serialize(format='nquads'), format='nquads')"
    "assert set(dataset.quads()) == set(again.quads())"
    "print('RDF parser, named-graph queries, datatypes and round trip passed')"]))

(deftest independent-rdf-parser-and-query-contract
  (with-store
    (fn [store]
      (let [view (evaluate-view store (source))
            projected (rdf/project! store view options)
            nq (String. (cas/get-bytes (:cas-dir store) (get-in projected [:outputs "nquads"])) "UTF-8")
            result (shell/sh "python3" "-c" query-probe
                             :in (json/write-json-str {"nquads" nq "basis" awkward-text}))]
        (is (= 0 (:exit result)) (str (:out result) (:err result)))
        (is (= nq (rdf/nquads (evaluate-view store (update (source) "findings" #(vec (reverse %))))
                              options)))))))

(deftest an-identity-premise-is-traceable-to-the-evidence-it-was-minted-over
  (with-store
    (fn [store]
      (let [identity {"id" "soranoha-example" "assessor" "Synthetic reviewer"
                      "basis" "Synthetic identity evidence" "evidence" ["evidence"]}
            minted (-> (source)
                       (assoc "identities" [identity])
                       (update-in ["findings" 0 "premises"] conj
                                  {"kind" "identity" "ref" "soranoha-example"
                                   "fingerprint" (evaluate/identity-fingerprint
                                                  identity {"evidence" captured-evidence})}))
            nq (rdf/nquads (evaluate-view store minted) options)
            plain (rdf/nquads (evaluate-view store (source)) options)]
        (testing "an identity premise names the identity, and nothing else names what it was minted from"
          (is (str/includes? nq "\"soranoha-example\"")))
        (testing "so the review carries an edge to each evidence observation"
          (is (str/includes? nq "<https://example.invalid/assessment/observation/evidence>"))
          (is (str/includes? nq "<urn:soranoha:assessment:groundedIn>")))
        (testing "and a premise with no such grounding gains no edge"
          (is (not (str/includes? plain "groundedIn"))))))))

(deftest projection-inputs-do-not-change-snapshot
  (with-store
    (fn [store]
      (let [view (evaluate-view store (source))
            before (:hex (snapshot/encode view))
            first-run (rdf/project! store view options)
            warm (rdf/project! store view options)
            new-base (rdf/project! store view (assoc options :base-iri "urn:internal:assessment/"))
            new-profile (rdf/project! store view (assoc options :mapping-profile {"vocabulary" "urn:other:"}))]
        (is (false? (:cached? first-run)))
        (is (true? (:cached? warm)))
        (is (= 3 (count (set (map :trace-key [first-run new-base new-profile])))))
        (is (= 3 (count (set (map #(get-in % [:outputs "nquads"]) [first-run new-base new-profile])))))
        (is (= before (:hex (snapshot/encode view))))
        (doseq [[projection-options result]
                [[options first-run]
                 [(assoc options :base-iri "urn:internal:assessment/") new-base]
                 [(assoc options :mapping-profile {"vocabulary" "urn:other:"}) new-profile]]]
          (is (= (rdf/nquads view projection-options)
                 (String. (cas/get-bytes (:cas-dir store) (get-in result [:outputs "nquads"])) "UTF-8"))))))))

(deftest invalid-inputs-never-produce-an-accepted-dataset
  (with-store
    (fn [store]
      (testing "contradictory applicable support fails before export"
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"conflicting-assessment-support"
                              (rdf/nquads (evaluate-view store (assoc (source) "controls" [])) options))))
      (testing "IRI injection and unknown mapping fields fail"
        (let [view (evaluate-view store (source))]
          (doseq [bad [(assoc options :base-iri "https://example.invalid/> <urn:inject>/")
                       (assoc options :mapping-profile {"vocabulary" "urn:v:" "extra" true})]]
            (is (thrown? clojure.lang.ExceptionInfo (rdf/nquads view bad)))))))))

(deftest fragments-follow-only-their-consumed-facts
  (with-store
    (fn [store]
      (let [source (update (source) "findings" conj (finding "independent" "person:000004" 1910))
            original (rdf/project! store (evaluate-view store source) options)
            revised-source (assoc-in source ["findings" 0 "basis"] "Revised support description")
            revised-view (evaluate-view store revised-source)
            revised (rdf/project! store revised-view options)
            fact (records/fact-key "person:000001" "death-year")
            withdrawn-view (evaluate-view store (update revised-source "controls" conj
                                                        {"id" "withdraw-current" "kind" "withdrawal"
                                                         "target" "current"}))
            withdrawn (rdf/project! store withdrawn-view options)]
        (is (every? #(false? (:cached? %)) (:fragments original)))
        (is (= #{["finding" "current"] ["conclusion" fact]}
               (set (for [fragment (:fragments revised) :when (not (:cached? fragment))]
                      [(:kind fragment) (:id fragment)]))))
        (is (not-any? #(and (= "conclusion" (:kind %)) (= fact (:id %))) (:fragments withdrawn)))
        (doseq [[view result] [[revised-view revised] [withdrawn-view withdrawn]]]
          (is (= (rdf/nquads view options)
                 (String. (cas/get-bytes (:cas-dir store) (get-in result [:outputs "nquads"])) "UTF-8"))))))))
