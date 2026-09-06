(ns soranoha.assessment.records
  "Owner-authored assessment inputs. Assessor names describe attribution;
  authority comes from the versioned source, not in-band authentication."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [soranoha.core.canonical :as canonical]
            [soranoha.core.hash :as hash]
            [soranoha.core.schema :as schema]
            [soranoha.snh.decode :as decode])
  (:import [java.time LocalDate]
           [java.util Arrays]))

(def empty-source
  {"schema" "soranoha-assessment-source/1"
   "observations" [] "findings" [] "controls" [] "identities" []})

(defn fail! [reason data]
  (throw (ex-info (str "Assessment source rejected: " (name reason))
                  (assoc data :reason reason))))

(defn fingerprint [value]
  (hash/sha256-canonical-json value))

(defn fact-key [subject predicate]
  {"subject" subject "predicate" predicate "jurisdiction" "jp"})

(defn contribution-subject [slug contribution-id]
  (str slug "/" contribution-id))

(defn date! [value]
  (try (LocalDate/parse value)
       (catch Exception _ (fail! :invalid-assessment-date {:date value}))))

(def ^:private source-schema
  (delay (json/read-json (slurp (io/resource "assessment/source-1.schema.json")))))

(defn validate!
  "Check closed record structure and references before evaluation."
  [source]
  (when-let [errors (schema/validation-errors @source-schema source)]
    (fail! :assessment-schema-invalid {:errors errors}))
  (let [findings (get source "findings")
        by-id (into {} (map (juxt #(get % "id") identity)) findings)
        observations (into {} (map (juxt #(get % "id") identity))
                           (get source "observations"))
        ids (map #(get % "id") (mapcat #(get source %)
                                       ["findings" "observations" "controls" "identities"]))]
    (when-not (= (count ids) (count (set ids)))
      (fail! :duplicate-assessment-id {}))
    (doseq [control (get source "controls")
            target (keep #(get control %) ["target" "replacement"])]
      (when-not (contains? by-id target)
        (fail! :dangling-assessment-control {:target target})))
    (doseq [control (get source "controls")
            :when (= "supersession" (get control "kind"))]
      (when (or (= (get control "target") (get control "replacement"))
                (not= (get-in by-id [(get control "target") "fact"])
                      (get-in by-id [(get control "replacement") "fact"])))
        (fail! :invalid-supersession {:control control})))
    (let [targets (frequencies (map #(get % "target")
                                    (filter #(= "supersession" (get % "kind"))
                                            (get source "controls"))))
          duplicates (sort (keep (fn [[target n]] (when (> n 1) target)) targets))]
      (when (seq duplicates)
        (fail! :duplicate-supersession-target {:targets (vec duplicates)})))
    (let [successor (into {} (keep (fn [control]
                                     (when (= "supersession" (get control "kind"))
                                       [(get control "target") (get control "replacement")])))
                          (get source "controls"))]
      (doseq [id (keys successor)]
        (loop [current id seen #{}]
          (when (seen current) (fail! :supersession-cycle {:finding current}))
          (when-let [next-id (get successor current)]
            (recur next-id (conj seen current))))))
    (doseq [finding findings]
      (doseq [field ["effective_date" "reviewed_at"]]
        (date! (get finding field)))
      (when (pos? (compare (get finding "effective_date") (get finding "reviewed_at")))
        (fail! :effective-date-after-review {:finding (get finding "id")}))
      (when (and (#{"contribution-set" "derivative-chain"}
                  (get-in finding ["fact" "predicate"]))
                 (not= (get finding "value") (vec (sort (get finding "value")))))
        (fail! :noncanonical-assessment-set {:finding (get finding "id")}))
      (doseq [premise (get finding "premises")]
        (let [kind (get premise "kind") ref (get premise "ref")
              projection (get premise "projection" "evidence-version")]
          (when-not (if (= kind "fact") (map? ref) (string? ref))
            (fail! :invalid-premise-reference {:premise premise}))
          (when (and (= kind "observation") (not (contains? observations ref)))
            (fail! :unknown-observation {:id ref}))
          (when (and (= kind "identity")
                     (or (not= projection "evidence-version")
                         (not (some #(= ref (get % "id")) (get source "identities")))))
            (fail! :invalid-identity-premise {:premise premise}))
          (when (and (not= projection "evidence-version")
                     (not (seq (get premise "rationale"))))
            (fail! :unjustified-semantic-premise {:finding (get finding "id")})))))
    (let [reliances (get source "reliances")
          slugs (map #(get % "slug") reliances)]
      (when-not (= (count slugs) (count (set slugs)))
        (fail! :duplicate-reliance-slug {}))
      (doseq [reliance reliances]
        (doseq [field ["observed_at" "decision_date"]]
          (date! (get reliance field)))
        (when (pos? (compare (get reliance "observed_at") (get reliance "decision_date")))
          (fail! :reliance-decision-before-observation {:slug (get reliance "slug")}))))
    (doseq [identity (get source "identities") id (get identity "evidence")]
      (when-not (contains? observations id)
        (fail! :unknown-identity-evidence {:id id})))
    source))

(defn decode [^bytes bytes]
  (let [value (validate! (decode/parse-value "assessment-source" bytes))
        canonical (canonical/rfc8785-safe-integer-json-bytes-v1 value)]
    (when-not (Arrays/equals bytes canonical)
      (fail! :noncanonical {}))
    {:value value :bytes bytes :hex (hash/sha256-bytes bytes)}))

(defn encode [source]
  (decode (canonical/rfc8785-safe-integer-json-bytes-v1 source)))
