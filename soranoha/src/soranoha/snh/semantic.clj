(ns soranoha.snh.semantic
  "Single-object semantic rules for the four protocol JSON objects — every
  rule a lone object must satisfy that the JSON Schemas deliberately do not
  carry (ordering, uniqueness, disjointness, real calendar dates, absolute
  origins; JSON Schema cannot express ordering and its `format` enforcement
  is inconsistent across validators). Boundary decode applies the matching
  check after structural validation, so assembler and verifier inherit every
  rule from the one shared operation. Cross-object and transition rules stay
  with the chain verifier."
  (:import (java.time LocalDate)
           (java.time.format DateTimeFormatter ResolverStyle)))

(defn- fail! [reason data]
  (throw (ex-info (str "semantic rule violated: " (name reason))
                  (assoc data :reason reason))))

(defn- sorted-unique!
  "Fails unless `xs` is strictly ascending (hence sorted and duplicate-free)."
  [reason xs data]
  (when-not (every? neg? (map compare xs (rest xs)))
    (fail! reason data))
  xs)

(def ^:private strict-iso-date
  ;; uuuu (not yyyy) so the strict resolver accepts unambiguous years, and
  ;; real calendar arithmetic rejects impossible dates like 2026-99-99 or
  ;; 2027-02-29 that the schema's digit pattern admits.
  (.withResolverStyle (DateTimeFormatter/ofPattern "uuuu-MM-dd")
                      ResolverStyle/STRICT))

(defn real-calendar-date?
  "True iff `s` is a real proleptic-Gregorian calendar date in YYYY-MM-DD."
  [s]
  (boolean
   (and (string? s)
        (re-matches #"^[0-9]{4}-[0-9]{2}-[0-9]{2}$" s)
        (try (LocalDate/parse s strict-iso-date)
             true
             (catch java.time.format.DateTimeParseException _ false)))))

(defn absolute-origin?
  "True iff `s` is an absolute URI with a scheme and a non-empty host —
  the rule for `corpus.upstream_origin`."
  [s]
  (boolean
   (and (string? s)
        (try (let [uri (java.net.URI. s)]
               (and (.isAbsolute uri)
                    (some? (.getHost uri))
                    (not (.isEmpty (.getHost uri)))))
             (catch java.net.URISyntaxException _ false)))))

(defn check-manifest!
  [manifest]
  (when-not (absolute-origin? (get-in manifest ["corpus" "upstream_origin"]))
    (fail! :invalid-upstream-origin
           {:origin (get-in manifest ["corpus" "upstream_origin"])}))
  (let [works (mapv #(get % "slug") (get manifest "works"))
        withdrawn (mapv #(get % "slug") (get manifest "withdrawn"))
        {:strs [invalid_count invalid_slugs]} (get manifest "validation_summary")]
    (sorted-unique! :works-not-sorted-unique works {})
    (sorted-unique! :withdrawn-not-sorted-unique withdrawn {})
    (when (seq (filter (set withdrawn) works))
      (fail! :works-withdrawn-overlap {}))
    (sorted-unique! :invalid-slugs-not-sorted-unique invalid_slugs {})
    (when-not (= invalid_count (count invalid_slugs))
      (fail! :invalid-count-mismatch {:declared invalid_count
                                      :actual (count invalid_slugs)}))
    (when-not (every? (set works) invalid_slugs)
      (fail! :invalid-slugs-outside-works
             {:strays (vec (remove (set works) invalid_slugs))})))
  manifest)

(defn check-snapshot!
  [snapshot]
  (sorted-unique! :candidates-not-sorted-unique
                  (mapv #(get % "slug") (get snapshot "candidates")) {})
  (doseq [{:strs [slug work_assessment contributions reliance]} (get snapshot "candidates")]
    (when reliance
      (doseq [field ["observed_at" "decision_date"]]
        (when-not (real-calendar-date? (get reliance field))
          (fail! :invalid-reliance-date {:slug slug :field field})))
      (when (pos? (compare (get reliance "observed_at") (get reliance "decision_date")))
        (fail! :reliance-observed-after-decision {:slug slug})))
    (sorted-unique! :contributions-not-sorted-unique
                    (mapv #(get % "contribution_id") contributions)
                    {:slug slug})
    (doseq [fact (cons work_assessment contributions)
            :let [date (get fact "effective_date")]
            :when (some? date)]
      (when-not (real-calendar-date? date)
        (fail! :invalid-effective-date {:slug slug :effective_date date}))))
  snapshot)

(defn check-report!
  [report]
  (let [admitted (get report "admitted")
        excluded (mapv #(get % "slug") (get report "excluded"))
        quarantined (mapv #(get % "slug") (get report "quarantined"))]
    (sorted-unique! :admitted-not-sorted-unique admitted {})
    (sorted-unique! :excluded-not-sorted-unique excluded {})
    (sorted-unique! :quarantined-not-sorted-unique quarantined {})
    (let [all (concat admitted excluded quarantined)]
      (when-not (= (count all) (count (set all)))
        (fail! :partition-sets-overlap {}))))
  report)

(defn check-event!
  [event]
  (sorted-unique! :entries-not-sorted-unique
                  (mapv #(get % "slug") (get event "entries")) {})
  event)

(def check-for
  "Artifact type -> its single-object semantic check."
  {"release-manifest" check-manifest!
   "assessment-snapshot" check-snapshot!
   "admission-report" check-report!
   "governance-event" check-event!})
