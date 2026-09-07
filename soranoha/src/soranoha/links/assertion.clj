(ns soranoha.links.assertion
  "Attributed external relationships over explicitly typed local entities."
  (:require [charred.api :as json]
            [clojure.string :as str]
            [soranoha.core.hash :as hash]
            [soranoha.core.rdf :as rdf]))

(def ^:private kinds
  {"document" :entity/document "work" :entity/work
   "edition" :entity/edition "person" :entity/person})
(def ^:private kind-names (into {} (map (fn [[wire internal]] [internal wire])) kinds))
(def ^:private rdf-ns "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(def ^:private prov "http://www.w3.org/ns/prov#")
(def ^:private vocabulary "urn:soranoha:links:")

(defn- require! [condition message]
  (when-not condition (throw (ex-info message {:reason :invalid-link-assertion}))))

(defn- fields! [value fields]
  (require! (and (map? value) (= (set fields) (set (keys value))))
            "Unexpected or missing external-link fields"))

(defn- nonblank! [value]
  (require! (and (string? value) (not (str/blank? value))) "Expected nonempty text")
  (rdf/value-term value))

(defn- entity! [entity field]
  (fields! entity [:entity/kind field])
  (require! (contains? kind-names (:entity/kind entity)) "Invalid entity kind")
  (nonblank! (get entity field))
  (when (= field :entity/id)
    (require! (not (contains? #{"." ".."} (get entity field))) "Local identifier cannot be a dot segment"))
  (when (= field :entity/iri) (rdf/iri (get entity field))))

(defn validate-assertion [assertion]
  (fields! assertion [:link/target :link/relation :link/external :link/attribution])
  (let [{:link/keys [target relation external attribution]} assertion
        {:attribution/keys [agent method evidence]} attribution]
    (entity! target :entity/id)
    (entity! external :entity/iri)
    (rdf/iri relation)
    (fields! attribution [:attribution/agent :attribution/method :attribution/evidence])
    (rdf/iri agent)
    (nonblank! method)
    (require! (and (vector? evidence) (seq evidence)) "Attribution requires evidence IRIs")
    (doseq [iri evidence] (rdf/iri iri)))
  assertion)

(defn- entity-wire [entity field]
  {"kind" (get kind-names (:entity/kind entity))
   (name field) (get entity field)})

(defn- wire [assertion]
  (let [{:link/keys [target relation external attribution]} assertion
        {:attribution/keys [agent method evidence]} attribution]
    {"schema" "soranoha-external-link/1"
     "target" (entity-wire target :entity/id)
     "relation" relation "external" (entity-wire external :entity/iri)
     "attribution" {"agent" agent "method" method "evidence" (vec (sort (distinct evidence)))}}))

(defn assertion-id [assertion]
  (validate-assertion assertion)
  (hash/format-sha256 (hash/sha256-canonical-json (wire assertion))))

(defn- decode-entity [entity field]
  (fields! entity ["kind" (name field)])
  {:entity/kind (get kinds (get entity "kind")) field (get entity (name field))})

(defn- decode [value]
  (fields! value ["target" "relation" "external" "attribution"])
  (let [attribution (get value "attribution")]
    (fields! attribution ["agent" "method" "evidence"])
    (validate-assertion
     {:link/target (decode-entity (get value "target") :entity/id)
      :link/relation (get value "relation")
      :link/external (decode-entity (get value "external") :entity/iri)
      :link/attribution {:attribution/agent (get attribution "agent")
                         :attribution/method (get attribution "method")
                         :attribution/evidence (get attribution "evidence")}})))

(defn read-assertions [text]
  (let [document (json/read-json text)]
    (fields! document ["schema" "assertions"])
    (require! (= "soranoha-external-links/1" (get document "schema")) "Unsupported link schema")
    (require! (vector? (get document "assertions")) "Assertions must be an array")
    (mapv decode (get document "assertions"))))

(defn- base! [base]
  (rdf/iri base)
  (require! (and (str/ends-with? base "/") (not (re-find #"[?#]" base)))
            "Local entity base must end in slash and have no query or fragment"))

(defn local-iri
  "The entity kind and caller-owned identifier remain stable when claims change."
  [base target]
  (base! base)
  (entity! target :entity/id)
  (str base "entities/" (get kind-names (:entity/kind target)) "/"
       (rdf/component (:entity/id target))))

(defn- quads [base assertion]
  (let [{:link/keys [target relation external attribution]} assertion
        {:attribution/keys [agent method evidence]} attribution
        id (str base "assertions/" (subs (assertion-id assertion) 7))
        subject (local-iri base target)
        object (:entity/iri external)]
    (concat
     [(rdf/quad id (str rdf-ns "type") (rdf/iri (str rdf-ns "Statement")) id)
      (rdf/quad id (str rdf-ns "subject") (rdf/iri subject) id)
      (rdf/quad id (str rdf-ns "predicate") (rdf/iri relation) id)
      (rdf/quad id (str rdf-ns "object") (rdf/iri object) id)
      (rdf/quad id (str prov "wasAttributedTo") (rdf/iri agent) id)
      (rdf/quad id (str vocabulary "method") (rdf/value-term method) id)
      (rdf/quad id (str vocabulary "targetKind") (rdf/iri (str vocabulary (get kind-names (:entity/kind target)))) id)
      (rdf/quad id (str vocabulary "externalKind") (rdf/iri (str vocabulary (get kind-names (:entity/kind external)))) id)]
     (map #(rdf/quad id (str prov "wasDerivedFrom") (rdf/iri %) id) evidence))))

(defn nquads
  "Export attributed statement descriptions; do not assert the described relation."
  [base assertions]
  (base! base)
  (apply str (sort (distinct (mapcat #(quads base %) assertions)))))
