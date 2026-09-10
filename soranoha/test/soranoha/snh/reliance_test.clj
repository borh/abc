(ns soranoha.snh.reliance-test
  (:require [babashka.fs :as fs]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is]]
            [soranoha.assessment.rdf :as rdf]
            [soranoha.core.hash :as hash]
            [soranoha.kura.cas :as cas]
            [soranoha.kura.engine :as engine]
            [soranoha.core.schema :as validator]
            [soranoha.snh.decode :as decode]
            [soranoha.snh.fixture :as fx]
            [soranoha.snh.schema :as schema]
            [soranoha.snh.transact :as transact]
            [soranoha.snh.verify :as verify]
            [soranoha.snh.view :as view]
            [soranoha.za.assemble :as assemble]
            [soranoha.za.serve :as serve]))

(defn- snapshot []
  (:value (decode/decode-string "assessment-snapshot"
                                (slurp (io/resource "snh/vectors/assessment-snapshot-v2-mixed-valid.json")))))

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest versioned-boundary-preserves-the-independent-arm
  (let [value (snapshot)]
    (is (= value (:value (decode/encode "assessment-snapshot" value))))
    (is (nil? (validator/validation-errors (schema/schema-for "assessment-snapshot") value)))
    (doseq [[mutation expected]
            [[#(assoc % "schema" "snh-assessment-snapshot/1" "candidates" [(first (get % "candidates"))]) :schema-invalid]
             [#(assoc % "schema" "snh-assessment-snapshot/3") :schema-invalid]
             [#(assoc-in % ["candidates" 1 "reliance" "observed_at"] "2026-02-30") :invalid-reliance-date]
             [#(assoc-in % ["candidates" 1 "reliance" "decision_date"] "2026-09-04") :reliance-observed-after-decision]
             [#(assoc-in % ["candidates" 1 "contributions"] []) :schema-invalid]
             [#(assoc-in % ["candidates" 1 "reliance" "exception"] "review needed") :schema-invalid]
             [#(assoc-in % ["candidates" 2 "reliance" "reason"] nil) :schema-invalid]
             [#(assoc-in % ["candidates" 1 "reliance" "slug"] "other") :schema-invalid]]]
      (is (= expected (reason #(decode/encode "assessment-snapshot" (mutation value))))))))

(defn- work! [cas-dir slug]
  (let [text (cas/put-bytes! cas-dir (fx/work-blob-bytes "plaintext" slug "v1"))
        markdown (cas/put-bytes! cas-dir (fx/work-blob-bytes "markdown" slug "v1"))
        tei (cas/put-bytes! cas-dir (fx/work-blob-bytes "tei" slug "v1"))
        validation (cas/put-bytes! cas-dir (fx/validation-blob-bytes tei false))
        metadata (cas/put-bytes!
                  cas-dir
                  (.getBytes ^String
                   (json/write-json-str
                    {"work" {"title" (str "fixture:" slug)
                             "title_reading" nil "subtitle" nil
                             "original_title" nil "first_published" nil
                             "orthographic_style" "新字新仮名" "ndc" nil
                             "card_url" "https://www.aozora.gr.jp/cards/000001/card1.html"
                             "source_editions" []}
                     "contributors" [{"person_id" "000001"
                                      "relation_to_work" "著者"}]})
                             "UTF-8"))
        persons (cas/put-bytes!
                 cas-dir
                 (.getBytes ^String
                  (json/write-json-str
                   {"000001" {"family_name" "試験" "given_name" nil
                              "family_name_romaji" "Shiken"
                              "given_name_romaji" nil}})
                            "UTF-8"))]
    {:rights "public-domain"
     :plaintext text :markdown markdown :tei tei :tei-validation validation
     :metadata-record metadata :persons persons
     :primary-text-member (str slug ".txt")
     :source-content-hash (str "sha256:" (apply str (repeat 64 "1")))}))

(defn- assembly-inputs [cas-dir]
  {:cas-dir cas-dir
   :corpus {"upstream_origin" "https://example.invalid/corpus.git"
            "upstream_rev" (apply str (repeat 40 "2"))}
   :toolchain {} :selection-params {} :policy-id "synthetic-reliance-test"
   :policy-hash (hash/sha256-string "synthetic-reliance-policy")
   :rights {"encoding" "CC0-1.0"
            "statement_url" "https://soranoha.example/rights"}
   :candidates (get (snapshot) "candidates")
   :works (into {} (map (fn [slug] [slug (work! cas-dir slug)])) ["independent" "relied"])
   :source-hashes {"relied" (str "sha256:" (apply str (repeat 64 "1")))}
   :selection ["independent" "relied" "unavailable"]})

(defn- assembler [cas-dir]
  (assemble/release-assembler (assembly-inputs cas-dir)))

(deftest withdrawn-reliance-keeps-source-binding-without-demanding-artifacts
  (let [dir (fs/create-temp-dir {:prefix "withdrawn-reliance"})]
    (try
      (let [inputs (assembly-inputs (str (fs/path dir "cas")))
            withdrawn {"withdrawn" [{"slug" "relied"}]}
            unbuilt (update inputs :works dissoc "relied")
            wrong-hash (str "sha256:" (apply str (repeat 64 "2")))]
        (is (= ["independent"]
               (mapv #(get % "slug")
                     (get-in ((assemble/release-assembler unbuilt) withdrawn) [:core "works"]))))
        (doseq [bad [(dissoc unbuilt :source-hashes)
                     (assoc-in unbuilt [:source-hashes "relied"] wrong-hash)
                     (assoc-in inputs [:works "relied" :source-content-hash] wrong-hash)]]
          (is (= :reliance-source-content-mismatch
                 (reason #((assemble/release-assembler bad) withdrawn)))))
        (is (= :admitted-work-not-built
               (reason #((assemble/release-assembler unbuilt) nil)))))
      (finally (fs/delete-tree dir)))))

(defn- artifact [clone commit id]
  (:value (decode/decode (second (re-find #"snh:1:([^:]+):" id))
                         (view/read-at (view/git-view clone) commit (verify/blob-path (verify/id->hex id))))))

(deftest signed-mixed-release-verifies-and-exports
  (let [{:keys [clone dir]} (fx/make-repos!)
        cas-dir (str (fs/path dir "cas"))]
    (try
      (let [result (transact/publish-build! {:clone clone :branch fx/branch :pinned-keys (fx/pinned-keys)
                                             :sign-release fx/sign-release :assemble (assembler cas-dir)})
            commit (fx/head-of clone)
            chain (verify/verify-repository-at (view/git-view clone) commit (fx/pinned-keys))
            manifest (:head-manifest chain)
            snapshot-id (get-in manifest ["admission" "assessment_snapshot"])
            out (fs/path dir "served")]
        (is (= :published (:outcome result)))
        (is (= ["independent" "relied"] (mapv #(get % "slug") (get manifest "works"))))
        (is (= (snapshot) (artifact clone commit snapshot-id)))
        (is (= 1 (:releases (serve/export-tree! {:clone clone :branch fx/branch :pinned-keys (fx/pinned-keys)
                                                 :out-dir (str out)}))))
        (is (= (snapshot)
               (:value (decode/decode "assessment-snapshot"
                                      (fs/read-all-bytes (fs/path out (verify/blob-path (verify/id->hex snapshot-id)))))))))
      (finally (fs/delete-tree dir)))))

(deftest signed-tampered-reliance-is-rejected-by-verifier
  (let [{:keys [clone dir]} (fx/make-repos!)]
    (try
      (transact/publish-build! {:clone clone :branch fx/branch :pinned-keys (fx/pinned-keys)
                                :sign-release fx/sign-release :assemble (assembler (str (fs/path dir "cas")))})
      (let [commit (fx/head-of clone)
            {:keys [hex value]} (fx/manifest-at clone commit)
            old-report (artifact clone commit (get-in value ["admission" "admission_report"]))]
        (doseq [[changed expected]
                [[(-> (snapshot)
                      (assoc-in ["candidates" 1 "reliance" "status"] "unavailable")
                      (assoc-in ["candidates" 1 "reliance" "reason"] "source-content-changed"))
                  :reliance-admission-mismatch]
                 [(assoc-in (snapshot) ["candidates" 1 "reliance" "source_content_hash"]
                            (str "sha256:" (apply str (repeat 64 "3"))))
                  :reliance-source-content-mismatch]]]
          (let [snapshot-enc (decode/encode "assessment-snapshot" changed)
                report-enc (decode/encode "admission-report" (assoc old-report "assessment_snapshot" (:id snapshot-enc)))
                next-value (-> value
                               (assoc "prev_manifest" hex)
                               (assoc-in ["admission" "assessment_snapshot"] (:id snapshot-enc))
                               (assoc-in ["admission" "admission_report"] (:id report-enc)))
                crafted (fx/craft-release! clone
                                           {:parents [commit] :base-tree-of commit :manifest-value next-value
                                            :extra-files {(verify/blob-path (:hex snapshot-enc)) (:bytes snapshot-enc)
                                                          (verify/blob-path (:hex report-enc)) (:bytes report-enc)}})]
            (is (= expected (reason #(verify/verify-repository-at (view/git-view clone)
                                                                  (:commit crafted) (fx/pinned-keys))))))))
      (finally (fs/delete-tree dir)))))

(deftest explicit-v2-schema-survives-an-independent-only-selection
  (let [dir (fs/create-temp-dir)
        value (update (snapshot) "candidates" #(subvec % 0 1))]
    (try
      (with-redefs [snapshot (constantly value)]
        (let [result ((assembler (str (fs/path dir "cas"))) nil)
              id (get-in result [:core "admission" "assessment_snapshot"])]
          (is (= value (:value (decode/decode "assessment-snapshot"
                                              (get (:blobs result) (verify/id->hex id))))))))
      (finally (fs/delete-tree dir)))))

(deftest wrong-edition-reliance-fails-before-assembly
  (let [dir (fs/create-temp-dir)]
    (try
      (let [value (snapshot)]
        (with-redefs [snapshot #(assoc-in value ["candidates" 1 "reliance" "source_content_hash"]
                                          (str "sha256:" (apply str (repeat 64 "3"))))]
          (is (= :reliance-source-content-mismatch
                 (reason #((assembler (str (fs/path dir "cas"))) nil))))))
      (finally (fs/delete-tree dir)))))

(deftest reliance-rdf-is-attributed-and-separate-from-independent-facts
  (let [dir (fs/create-temp-dir)
        store (engine/open-store! {:cas-dir (str (fs/path dir "cas")) :db-path (str (fs/path dir "trace.sqlite"))})
        payloads (into {} (keep (fn [{:strs [slug reliance]}] (when reliance [slug reliance])))
                       (get (snapshot) "candidates"))
        view {:facts {} :findings [] :source {"controls" []} :reliances payloads}
        options {:base-iri "urn:test/" :mapping-profile rdf/default-mapping-profile :toolchain-id "test"}]
    (try
      (let [result (rdf/project! store view options)
            text (String. (cas/get-bytes (:cas-dir store) (get-in result [:outputs "nquads"])) "UTF-8")
            parsed (shell/sh "python3" "-c"
                             (str "import sys\nfrom rdflib import Dataset\n"
                                  "d=Dataset(); d.parse(data=sys.stdin.read(),format='nquads')\n"
                                  "q='PREFIX a: <urn:soranoha:assessment:> PREFIX prov: <http://www.w3.org/ns/prov#> '\n"
                                  "assert len(list(d.query(q+'SELECT ?x WHERE { GRAPH <urn:test/accepted-reliance> { ?x a:sourceClassificationReliance ?r } }'))) == 1\n"
                                  "assert len(list(d.query(q+'SELECT ?x WHERE { GRAPH ?g { ?x a a:ScopedFact } }'))) == 0\n"
                                  "assert len(list(d.query(q+'SELECT ?x WHERE { GRAPH ?g { ?x prov:wasAttributedTo <https://www.aozora.gr.jp/> } }'))) == 2\n")
                             :in text)]
        (is (= 0 (:exit parsed)) (:err parsed))
        (is (= text (rdf/nquads view options)))
        (is (every? :cached? (:fragments (rdf/project! store view options)))))
      (finally (engine/close-store! store) (fs/delete-tree dir)))))
