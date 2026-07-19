(ns abc.tools.parser-rq-capture
  "Contracts for committed parser-qualification capture metadata and rebinding
  logical blob identities to an explicitly configured external store."
  (:require [abc.tools.hash :as hash]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as string]
            [malli.core :as m]))

(def sha256-schema
  [:fn {:error/message "must be a sha256 logical blob identity"}
   #(and (string? %) (re-matches hash/hash-pattern %))])

(def blob-reference-schema
  [:map {:closed true}
   [:sha256 sha256-schema]
   [:bytes [:int {:min 0}]]
   [:media_type [:string {:min 1}]]])

(def denominator-schema
  [:map {:closed true}
   [:value [:int {:min 0}]]
   [:unit [:string {:min 1}]]])

(def manifest-schema
  [:map {:closed true}
   [:blobs [:vector {:min 1}
            [:map {:closed true}
             [:locator [:string {:min 1}]]
             [:ref blob-reference-schema]]]]
   [:denominator {:optional true} denominator-schema]])

(def envelope-schema
  [:map {:closed true}
   [:value :any]
   [:identity_ref sha256-schema]
   [:details {:optional true} [:map-of :keyword :any]]])

(defn- validation-errors
  [schema value label]
  (if (m/validate schema value)
    []
    [(str label " violates its closed contract: "
          (pr-str (m/explain schema value)))]))

(defn manifest-errors
  [manifest]
  (validation-errors manifest-schema manifest "capture manifest; expected sha256 logical blob identity"))

(defn envelope-errors
  [envelope]
  (validation-errors envelope-schema envelope "observation envelope"))

(defn observation-value
  [envelope]
  (:value envelope))

(defn closed-membership-errors
  "Compare explicit expected work IDs with record work IDs as closed sets.

  Ordering has no meaning. Duplicate expected IDs, duplicate records, missing
  IDs, extra IDs, and malformed IDs are all errors."
  [expected-work-ids records]
  (let [record-work-ids (mapv :work_id records)
        expected-frequencies (frequencies expected-work-ids)
        record-frequencies (frequencies record-work-ids)
        expected-set (set expected-work-ids)
        record-set (set record-work-ids)
        duplicate-expected (->> expected-frequencies
                                (keep (fn [[work-id n]] (when (> n 1) work-id)))
                                sort)
        duplicate-records (->> record-frequencies
                               (keep (fn [[work-id n]] (when (> n 1) work-id)))
                               sort)
        missing (sort (set/difference expected-set record-set))
        extra (sort (set/difference record-set expected-set))]
    (cond-> []
      (or (some #(not (and (string? %) (not (string/blank? %)))) expected-work-ids)
          (some #(not (and (string? %) (not (string/blank? %)))) record-work-ids))
      (conj "work IDs must be nonblank strings")

      (seq duplicate-expected)
      (conj (str "expected membership contains duplicate work IDs: "
                 (string/join ", " duplicate-expected)))

      (seq duplicate-records)
      (conj (str "record index contains duplicate work IDs: "
                 (string/join ", " duplicate-records)))

      (seq missing)
      (conj (str "record index is missing expected work IDs: "
                 (string/join ", " missing)))

      (seq extra)
      (conj (str "record index contains extra work IDs: "
                 (string/join ", " extra))))))

(defn capture-generation-ref
  "Canonical content identity for a complete capture-generation value."
  [generation-value]
  (hash/format-sha256 (hash/sha256-json-jcs generation-value)))

(defn observation-envelope
  "Construct an identity-bound observation with optional typed disclosure."
  [identity-ref value details]
  (cond-> {:value value :identity_ref identity-ref}
    (some? details) (assoc :details details)))

(defn normalize-status-map
  "Normalize a JSON-derived status map after either shallow or recursive
  keywordization. Wire statuses remain strings because policies name JSON
  values, not Clojure implementation keys."
  [status-map]
  (let [allowed (or (:allowed_statuses status-map)
                    (get status-map "allowed_statuses"))
        values (or (:values status-map) (get status-map "values"))]
    {:allowed_statuses allowed
     :values (into {}
                   (map (fn [[status value]]
                          [(if (keyword? status) (name status) status) value]))
                   values)}))

(defn- valid-status-map?
  [{:keys [allowed_statuses values] :as status-map}]
  (and (= #{:allowed_statuses :values} (set (keys status-map)))
       (vector? allowed_statuses)
       (seq allowed_statuses)
       (every? #(and (string? %) (not (string/blank? %))) allowed_statuses)
       (= (count allowed_statuses) (count (set allowed_statuses)))
       (map? values)
       (= (set allowed_statuses) (set (keys values)))
       (every? some? (vals values))
       (= (count values) (count (set (vals values))))))

(defn map-wire-status
  "Map one closed JSON status to its instrument observation value.

  The authenticated policy supplies the complete allowed status vector and an
  injective value map. Invalid policy shape or an unknown status fails closed."
  [status-map json-status]
  (if (and (valid-status-map? status-map)
           (contains? (:values status-map) json-status))
    (get-in status-map [:values json-status])
    {:status :unavailable :reason :status-mapping-invalid}))

(defn- unavailable
  [reason]
  {:status :unavailable :reason reason})

(def ^:dynamic *after-authenticated-read*
  "Test seam invoked after a locator's bytes have been read and authenticated."
  nil)

(defn- no-symlink-path?
  [root-path blob-path]
  (every? (fn [path]
            (not (java.nio.file.Files/isSymbolicLink path)))
          (rest (reductions #(.resolve ^java.nio.file.Path %1 ^java.nio.file.Path %2)
                            root-path (iterator-seq
                                       (.iterator (.relativize root-path blob-path)))))))

(defn- parent-component?
  [path]
  (some #(= ".." (str %)) (iterator-seq (.iterator path))))

(defn authenticated-read
  "Read and authenticate one immutable-store value exactly once.

  Locators are relative, remain beneath the canonical store root, and may not
  contain symlink components. The returned bytes are the bytes whose length and
  digest were authenticated; consumers must not reopen the locator."
  [{:keys [root]} blob-ref locator]
  (try
    (let [root-path (.toPath (.getCanonicalFile (io/file root)))
          locator-path (java.nio.file.Paths/get locator (make-array String 0))
          blob-path (.normalize (.resolve root-path locator-path))]
      (cond
        (.isAbsolute locator-path)
        (unavailable "blob locator must be relative to the configured store root")

        (parent-component? locator-path)
        (unavailable "blob locator contains a parent-directory component")

        (not (.startsWith blob-path root-path))
        (unavailable "blob locator escapes the configured store root")

        (not (no-symlink-path? root-path blob-path))
        (unavailable "blob locator contains a symbolic link")

        (not (java.nio.file.Files/isRegularFile
              blob-path (into-array java.nio.file.LinkOption
                                    [java.nio.file.LinkOption/NOFOLLOW_LINKS])))
        (unavailable "blob is absent from the configured store")

        :else
        (let [bytes (java.nio.file.Files/readAllBytes blob-path)]
          (cond
            (not= (:bytes blob-ref) (alength bytes))
            (unavailable "blob byte length does not match its logical identity")

            (not= (:sha256 blob-ref)
                  (hash/format-sha256 (hash/sha256-bytes bytes)))
            (unavailable "blob hash does not match its logical identity")

            :else
            (do
              (when *after-authenticated-read* (*after-authenticated-read* blob-path))
              {:status :ok :bytes bytes})))))
    (catch Exception error
      (unavailable (.getMessage error)))))

(defn verify-blob
  "Resolve `locator` below the runtime-configured store root, then stream and
  re-hash it. Store metadata is never trusted. Missing, escaping, or mismatched
  blobs are unavailable and cannot yield an observation."
  [{:keys [root]} blob-ref locator]
  (dissoc (authenticated-read {:root root} blob-ref locator) :bytes))

(defn read-blob
  "Read one locator-bearing logical blob through the shared authenticated store
  boundary. The returned bytes are present only when identity verification
  succeeds."
  [store blob-ref]
  (authenticated-read store blob-ref (:locator blob-ref)))

(defn verify-manifest
  "Verify every logical blob in a closed capture manifest against the runtime
  store. One invalid, absent, or mismatched blob makes the whole capture
  unavailable; partial verification never yields observations."
  [store manifest]
  (let [errors (manifest-errors manifest)]
    (if (seq errors)
      (unavailable (string/join "; " errors))
      (let [results (mapv (fn [{:keys [ref locator]}]
                            (assoc (authenticated-read store ref locator)
                                   :locator locator))
                          (:blobs manifest))]
        (if (every? #(= :ok (:status %)) results)
          {:status :ok
           :blob_count (count results)
           :authenticated_blobs (into {} (map (juxt :locator :bytes) results))}
          {:status :unavailable
           :reason "one or more manifest blobs are unavailable"
           :blob_results results})))))
