(ns soranoha.snh.semantic
  "Semantic boundary rules — checks the JSON Schemas deliberately do not
  carry because JSON Schema `format` enforcement is inconsistent across
  validators. Assembler and verifier both call these; boundary decode does
  not (it is structure + canonicality only)."
  (:import (java.time LocalDate)
           (java.time.format DateTimeFormatter ResolverStyle)))

(def ^:private strict-iso-date
  ;; uuuu (not yyyy) so the STRICT resolver accepts unambiguous years, and
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

(defn check-manifest-origin!
  "Throws unless the manifest value's corpus.upstream_origin is an absolute
  URI with a non-empty host."
  [manifest-value]
  (let [origin (get-in manifest-value ["corpus" "upstream_origin"])]
    (when-not (absolute-origin? origin)
      (throw (ex-info "upstream_origin must be an absolute URI with a host"
                      {:reason :invalid-upstream-origin :origin origin})))
    manifest-value))

(defn check-snapshot-dates!
  "Throws unless every non-null effective_date in the assessment-snapshot
  value is a real calendar date."
  [snapshot-value]
  (doseq [candidate (get snapshot-value "candidates")
          fact (cons (get candidate "work_assessment")
                     (get candidate "contributions"))
          :let [date (get fact "effective_date")]
          :when (some? date)]
    (when-not (real-calendar-date? date)
      (throw (ex-info "effective_date must be a real calendar date"
                      {:reason :invalid-effective-date
                       :slug (get candidate "slug")
                       :effective_date date}))))
  snapshot-value)
