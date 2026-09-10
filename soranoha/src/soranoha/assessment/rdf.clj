(ns soranoha.assessment.rdf
  "Internal RDF dataset projection. Only the accepted named graph expresses
  current findings. Other graphs describe scoped historical claims; consumers
  must not treat their union as an accepted graph. Source JSON defines identity."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [soranoha.core.rdf :refer [iri component literal value-term quad]]
            [soranoha.assessment.evaluate :as evaluate]
            [soranoha.core.hash :as hash]
            [soranoha.kura.engine :as engine]))

(def ^:private fragment-versions {"finding" "3" "conclusion" "1" "reliance" "1"})
(def ^:private assembly-version "2")

(def default-mapping-profile {"vocabulary" "urn:soranoha:assessment:"})

(def ^:private rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(def ^:private prov "http://www.w3.org/ns/prov#")
(def ^:private xsd "http://www.w3.org/2001/XMLSchema#")

(defn- fact-iri [base fact]
  (str base "fact/" (hash/sha256-canonical-json fact)))

(defn- claim-iri [base record]
  (str base "claim/" (hash/sha256-canonical-json record)))

(defn- assertion [base vocabulary graph fact value date]
  (let [id (fact-iri base fact)]
    [(quad id (str rdf "type") (iri (str vocabulary "ScopedFact")) graph)
     (quad id (str vocabulary "subject")
           (iri (str base "subject/" (component (get fact "subject")))) graph)
     (quad id (str vocabulary "predicate")
           (iri (str vocabulary (component (get fact "predicate")))) graph)
     (quad id (str vocabulary "jurisdiction") (value-term (get fact "jurisdiction")) graph)
     (quad id (str vocabulary "value") (value-term value) graph)
     (quad id (str vocabulary "effectiveDate") (literal date (str xsd "date")) graph)]))

(defn- grounding-quads
  "The observations a premise's match consumed, as edges from the review that
  made it.

  A premise names one thing in `ref`, and for most kinds that is also the
  thing the match read: an observation premise refers to its observation, a
  fact premise to a fact whose own review is in the graph already. An identity
  premise is the exception. Its fingerprint is minted over several evidence
  observations and it matches only when every one of them is available, so
  `ref` names the identity and nothing names what the identity was minted
  from. Those are the premises whose grounding is least visible, and without
  these edges a finding resting on one could not be traced through the graph
  to the evidence that carried it."
  [base vocabulary activity usage observation-ids]
  (mapcat (fn [observation-id]
            (let [entity (str base "observation/" (component observation-id))]
              [(quad activity (str prov "used") (iri entity) nil)
               (quad usage (str vocabulary "groundedIn") (iri entity) nil)
               (quad entity (str rdf "type") (iri (str prov "Entity")) nil)
               (quad entity (str vocabulary "observationId") (value-term observation-id) nil)]))
          observation-ids))

(defn- finding-quads [base vocabulary {:keys [record state reason grounding]}]
  (let [id (claim-iri base record)
        activity (str id "/review")
        assessor (str base "agent/" (component (get record "assessor")))]
    (concat
     (assertion base vocabulary id (get record "fact") (get record "value")
                (get record "effective_date"))
     [(quad id (str rdf "type") (iri (str prov "Bundle")) nil)
      (quad id (str vocabulary "recordId") (value-term (get record "id")) nil)
      (quad id (str vocabulary "applicability") (value-term state) nil)
      (quad id (str vocabulary "basis") (value-term (get record "basis")) nil)
      (quad id (str prov "wasGeneratedBy") (iri activity) nil)
      (quad activity (str rdf "type") (iri (str prov "Activity")) nil)
      (quad activity (str prov "wasAssociatedWith") (iri assessor) nil)
      (quad assessor (str rdf "type") (iri (str prov "Agent")) nil)
      (quad activity (str vocabulary "method") (value-term (get record "method")) nil)
      (quad activity (str vocabulary "reviewedAt") (literal (get record "reviewed_at") (str xsd "date")) nil)]
     (when reason [(quad id (str vocabulary "unavailabilityReason") (value-term reason) nil)])
     (mapcat
      (fn [[index premise]]
        (let [usage (str activity "/usage/" index)
              evidence (str "urn:sha256:" (get premise "fingerprint"))
              projection (get premise "projection" "evidence-version")]
          (concat
           [(quad activity (str prov "used") (iri evidence) nil)
            (quad activity (str prov "qualifiedUsage") (iri usage) nil)
            (quad usage (str rdf "type") (iri (str prov "Usage")) nil)
            (quad usage (str prov "entity") (iri evidence) nil)
            (quad usage (str prov "hadRole") (iri (str vocabulary projection)) nil)
            (quad usage (str vocabulary "premiseKind") (value-term (get premise "kind")) nil)
            (quad usage (str vocabulary "premiseReference") (value-term (get premise "ref")) nil)
            (quad evidence (str rdf "type") (iri (str prov "Entity")) nil)]
           (grounding-quads base vocabulary activity usage (get grounding index)))))
      (map-indexed vector (get record "premises"))))))

(defn- dependency-input [edge]
  (cond-> edge
    (contains? edge :state) (update :state evaluate/state->wire)
    (contains? edge :reason) (update :reason evaluate/reason->wire)
    (contains? edge :dependencies) (update :dependencies #(mapv dependency-input %))))

(defn- projection-input [view]
  (when-not (and (map? (:facts view)) (vector? (:findings view)))
    (throw (ex-info "RDF projection requires an evaluated assessment view"
                    {:reason :invalid-assessment-view})))
  {"reliances" (mapv (fn [[slug payload]] {"slug" slug "payload" payload}) (sort-by key (:reliances view)))
   "controls" (get-in view [:source "controls"])
   "findings" (mapv (fn [{:keys [record state reason dependencies]}]
                      ;; `dependencies` is one edge per premise, in the order
                      ;; the record lists them, so the observations each
                      ;; premise's match consumed stay aligned with it by index
                      {"record" record "state" (evaluate/state->wire state) "reason" (evaluate/reason->wire reason)
                       "grounding" (mapv #(vec (keep :observation (:dependencies %))) dependencies)})
                    (:findings view))
   "facts" (mapv (fn [[fact result]]
                   {"fact" fact "state" (evaluate/state->wire (:state result)) "value" (:value result)
                    "effective-date" (:effective-date result)
                    "semantic-id" (:semantic-id result) "basis-id" (:basis-id result)
                    "basis" (:basis result)
                    "dependencies" (mapv (fn [edge]
                                           (walk/stringify-keys
                                            (cond-> (dependency-input edge)
                                              (:fact edge)
                                              (assoc :semantic-id (get-in view [:facts (:fact edge) :semantic-id])))))
                                         (:dependencies result))})
                 (sort-by (comp hash/sha256-canonical-json key) (:facts view)))})

(defn- conclusion-quads [base vocabulary accepted by-id result]
  (when (= "available" (get result "state"))
    (let [fact (get result "fact")
          conclusion (str base "conclusion/" (hash/sha256-canonical-json result))
          activity (str conclusion "/evaluation")
          assertion-quads (fn [graph]
                            (assertion base vocabulary graph fact (get result "value")
                                       (get result "effective-date")))
          support-iris (map (fn [basis]
                              (if-let [record (get by-id (get basis "id"))]
                                (claim-iri base record)
                                (str base "rule/" (component (get basis "id")))))
                            (get result "basis"))]
      (concat
       (assertion-quads accepted)
       (assertion-quads conclusion)
       [(quad conclusion (str rdf "type") (iri (str prov "Bundle")) nil)
        (quad conclusion (str prov "wasGeneratedBy") (iri activity) nil)
        (quad activity (str rdf "type") (iri (str prov "Activity")) nil)
        (quad (fact-iri base fact) (str prov "wasDerivedFrom") (iri conclusion) accepted)]
       (mapcat (fn [support]
                 [(quad conclusion (str prov "wasDerivedFrom") (iri support) nil)
                  (quad activity (str prov "used") (iri support) nil)]) support-iris)
       (mapcat
        (fn [[index edge]]
          (let [usage (str activity "/usage/" index)
                premise (get edge "premise")
                consumed (if premise
                           (str "urn:sha256:" (get premise "fingerprint"))
                           (str "urn:sha256:" (get edge "semantic-id")))
                role (if premise (get premise "projection" "evidence-version") "value")]
            [(quad activity (str prov "qualifiedUsage") (iri usage) nil)
             (quad usage (str rdf "type") (iri (str prov "Usage")) nil)
             (quad usage (str prov "entity") (iri consumed) nil)
             (quad usage (str prov "hadRole") (iri (str vocabulary role)) nil)
             (quad usage (str vocabulary "premiseReference")
                   (value-term (if premise (get premise "ref") (get edge "fact"))) nil)]))
        (map-indexed vector (get result "dependencies")))))))

(defn- control-quads [base vocabulary by-id control]
  (let [id (str base "control/" (hash/sha256-canonical-json control))]
    (concat
     [(quad id (str rdf "type") (iri (str vocabulary (get control "kind"))) nil)
      (quad id (str vocabulary "target")
            (iri (claim-iri base (get by-id (get control "target")))) nil)]
     (when-let [replacement (get control "replacement")]
       [(quad id (str vocabulary "replacement")
              (iri (claim-iri base (get by-id replacement))) nil)]))))

(defn- reliance-quads [base vocabulary {:strs [slug payload] :as record}]
  (let [id (str base "reliance/" (hash/sha256-canonical-json record))
        source (str id "/source-classification")
        activity (str id "/decision")
        graph id
        subject (str base "subject/" (component slug))]
    (concat
     [(quad id (str rdf "type") (iri (str vocabulary "SourceClassificationReliance")) graph)
      (quad id (str vocabulary "subject") (iri subject) graph)
      (quad id (str prov "wasGeneratedBy") (iri activity) graph)
      (quad activity (str rdf "type") (iri (str prov "Activity")) graph)
      (quad activity (str prov "used") (iri source) graph)
      (quad source (str rdf "type") (iri (str vocabulary "SourceClassification")) graph)
      (quad source (str prov "wasAttributedTo") (iri "https://www.aozora.gr.jp/") graph)
      (quad source (str vocabulary "classification") (value-term (get payload "classification")) graph)
      (quad source (str vocabulary "source_content_hash") (value-term (get payload "source_content_hash")) graph)]
     (map (fn [[field value]]
            (quad id (str vocabulary field)
                  (if (#{"observed_at" "decision_date"} field)
                    (literal value (str xsd "date")) (value-term value)) graph)) payload)
     (when (= "relied-upon" (get payload "status"))
       [(quad subject (str vocabulary "sourceClassificationReliance") (iri id)
              (str base "accepted-reliance"))]))))

(defn- metadata-quads [base profile by-id controls]
  (let [vocabulary (get profile "vocabulary") accepted (str base "accepted")]
    (concat
     [(quad accepted (str rdf "type") (iri (str vocabulary "AcceptedView")) nil)
      (quad accepted (str vocabulary "mappingProfile") (value-term profile) nil)]
     (mapcat #(control-quads base vocabulary by-id %) controls))))

(defn- render [input base profile]
  (let [vocabulary (get profile "vocabulary")
        accepted (str base "accepted")
        findings (map #(hash-map :record (get % "record") :state (get % "state")
                                 :reason (get % "reason") :grounding (get % "grounding"))
                      (get input "findings"))
        by-id (into {} (map (fn [{:keys [record]}] [(get record "id") record]) findings))]
    (apply str
           (sort
            (distinct
             (concat
              (metadata-quads base profile by-id (get input "controls"))
              (mapcat #(finding-quads base vocabulary %) findings)
              (mapcat #(reliance-quads base vocabulary %) (get input "reliances"))
              (mapcat #(conclusion-quads base vocabulary accepted by-id %) (get input "facts"))))))))

(defn- options! [{:keys [base-iri mapping-profile]}]
  (iri base-iri)
  (when-not (and (str/ends-with? base-iri "/")
                 (= #{"vocabulary"} (set (keys mapping-profile))))
    (throw (ex-info "RDF requires a slash-terminated base and vocabulary mapping"
                    {:reason :invalid-rdf-options})))
  (iri (get mapping-profile "vocabulary")))

(defn nquads
  "Serialize a successfully evaluated view as deterministic UTF-8 N-Quads text."
  [view {:keys [base-iri mapping-profile] :as options}]
  (options! options)
  (render (projection-input view) base-iri mapping-profile))

(defn- fragment! [store options kind id payload]
  (let [{:keys [base-iri mapping-profile toolchain-id]} options
        result
        (engine/run-stage!
         store
         {:stage-id (str "assessment-rdf-" kind) :stage-version (get fragment-versions kind) :toolchain-id toolchain-id
          :f (fn [_ inputs]
               (let [base (get inputs "base") vocabulary (get-in inputs ["mapping" "vocabulary"])
                     payload (get inputs "payload")
                     quads (case kind
                             "reliance" (reliance-quads base vocabulary payload)
                             "finding" (finding-quads base vocabulary
                                                      {:record (get payload "record")
                                                       :state (get payload "state")
                                                       :reason (get payload "reason")
                                                       :grounding (get payload "grounding")})
                             "conclusion" (conclusion-quads base vocabulary (str base "accepted")
                                                            (get payload "supports") (get payload "result")))]
                 {"fragment" (.getBytes ^String (apply str (sort (distinct quads))) "UTF-8")}))}
         {"base" base-iri "mapping" mapping-profile "payload" payload})]
    (assoc result :kind kind :id id)))

(defn project!
  "Cache findings and available conclusions separately, then assemble the dataset.
  Inactive conclusions never contribute a cached accepted assertion."
  [store view {:keys [base-iri mapping-profile toolchain-id] :as options}]
  (options! options)
  (let [input (projection-input view)
        by-id (into {} (map (fn [finding]
                              [(get-in finding ["record" "id"]) (get finding "record")]))
                    (get input "findings"))
        fragments
        (into (mapv #(fragment! store options "finding" (get-in % ["record" "id"]) %)
                    (get input "findings"))
              (for [result (get input "facts") :when (= "available" (get result "state"))]
                (fragment! store options "conclusion" (get result "fact")
                           {"result" result
                            "supports" (select-keys by-id (map #(get % "id") (get result "basis")))})))
        fragments (into fragments (map #(fragment! store options "reliance" (get % "slug") %) (get input "reliances")))
        control-targets (mapcat #(keep % ["target" "replacement"]) (get input "controls"))
        metadata {"targets" (select-keys by-id control-targets) "controls" (get input "controls")}
        result
        (engine/run-stage!
         store
         {:stage-id "assessment-rdf" :stage-version assembly-version :toolchain-id toolchain-id
          :f (fn [{:keys [blob]} inputs]
               (let [fragments (map #(String. ^bytes (blob %) "UTF-8") (get inputs "fragments"))
                     metadata (metadata-quads (get inputs "base") (get inputs "mapping")
                                              (get-in inputs ["metadata" "targets"])
                                              (get-in inputs ["metadata" "controls"]))
                     lines (mapcat str/split-lines (concat metadata fragments))]
                 {"nquads" (.getBytes (str (str/join "\n" (sort (distinct lines))) "\n") "UTF-8")}))}
         {"base" base-iri "mapping" mapping-profile "metadata" metadata
          "fragments" (vec (sort (distinct (map #(get-in % [:outputs "fragment"]) fragments))))})]
    (assoc result :fragments fragments)))
