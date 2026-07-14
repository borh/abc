(ns abc.tools.parser-maintenance-evidence)

(def required-categories
  #{"upstream_review" "selective_ports" "security_review" "missed_fixes"
    "fork_only_defects" "planned_divergence" "remeasurement"
    "maintainer_availability"})

(def required-observations
  #{"rolling_quarter_maintenance_minutes" "selective_port_samples_minutes"
    "selective_port_p90_minutes" "maximum_missed_fix_severity"
    "maintainer_available"})

(defn- calendar-date? [value]
  (and (string? value)
       (try
         (java.time.LocalDate/parse value)
         true
         (catch java.time.format.DateTimeParseException _ false))))

(defn- nonnegative-integer? [value]
  (and (integer? value) (not (neg? value))))

(defn- percentile-90 [samples]
  (if (seq samples)
    (nth (vec (sort samples))
         (dec (long (Math/ceil (* 0.9 (count samples))))))
    0))

(defn problems
  ([record]
   (problems record (get record "recorded_at")))
  ([record as-of]
   (let [categories (get record "categories")
         observations (get record "observations")
         review-after (get record "review_after")
         expires-on (get record "expires_on")
         estimate (get-in record ["trial_merge" "estimate"])
         component-minutes (map #(get % "minutes") (get estimate "components"))
         category-minutes (map #(get % "minutes") (vals categories))
         port-samples (get observations "selective_port_samples_minutes")]
     (vec
      (concat
       (when-not (= "1.0.0" (get record "protocol_version"))
         ["protocol_version must be 1.0.0"])
       (when-not (contains? #{"initial" "observed" "quarter_closed"}
                            (get record "status"))
         ["status must be initial, observed, or quarter_closed"])
       (when-not (= required-categories (set (keys categories)))
         ["categories must contain exactly the required maintenance categories"])
       (when-not (every? #(nonnegative-integer? (get % "minutes"))
                         (vals categories))
         ["every maintenance category must record non-negative integer minutes"])
       (when-not (= required-observations (set (keys observations)))
         ["observations must contain exactly the required maintenance metrics"])
       (when-not (and (every? nonnegative-integer? port-samples)
                      (nonnegative-integer?
                       (get observations "selective_port_p90_minutes"))
                      (= (percentile-90 port-samples)
                         (get observations "selective_port_p90_minutes")))
         ["selective_port_p90_minutes must be derived from selective port samples"])
       (when-not (= (reduce + 0 category-minutes)
                    (get observations "rolling_quarter_maintenance_minutes"))
         ["rolling maintenance minutes must equal the category total"])
       (when-not (<= 0 (get observations "maximum_missed_fix_severity" -1) 5)
         ["maximum missed-fix severity must be an integer from 0 through 5"])
       (when-not (boolean? (get observations "maintainer_available"))
         ["maintainer_available must be boolean"])
       (when-not (pos-int? (get-in record ["focused_session" "duration_minutes"]))
         ["focused_session.duration_minutes must be a positive integer"])
       (when-not (and (calendar-date? review-after) (calendar-date? expires-on))
         ["review_after and expires_on must be calendar dates"])
       (when (and (calendar-date? review-after) (calendar-date? expires-on)
                  (pos? (compare review-after expires-on)))
         ["review_after must be on or before expires_on"])
       (when-not (calendar-date? as-of)
         ["as-of must be an explicit calendar date"])
       (when (and (calendar-date? as-of) (calendar-date? expires-on)
                  (pos? (compare as-of expires-on)))
         [(str "maintenance evidence expired on " expires-on)])
       (when (and (calendar-date? as-of) (calendar-date? review-after)
                  (pos? (compare as-of review-after))
                  (not (pos? (compare as-of expires-on))))
         [(str "maintenance evidence review is due after " review-after)])
       (when-not (= "expert-assessment" (get estimate "evidence_kind"))
         ["trial merge estimate must be typed expert-assessment"])
       (when-not (and (every? nonnegative-integer? component-minutes)
                      (= (get estimate "estimated_minutes")
                         (reduce + 0 component-minutes)))
         ["trial merge estimate must equal the sum of its components"])
       (when-not (every? #(= "benchmark" (get % "evidence_kind"))
                         (get-in record ["trial_merge" "benchmarks"] []))
         ["trial merge measurements must be typed benchmark"])
       (when-not (seq (get-in record ["trial_merge" "procedure" "steps"]))
         ["trial merge procedure must declare disposable steps"])
       (when-not (seq (get record "revisit_predicates"))
         ["decision review predicates cannot be empty"]))))))

(defn- predicate-triggered? [observed {operator "operator" threshold "threshold"}]
  (case operator
    ">" (and (number? observed) (number? threshold) (> observed threshold))
    ">=" (and (number? observed) (number? threshold) (>= observed threshold))
    "=" (and (= (type observed) (type threshold)) (= observed threshold))
    false))

(defn triggered-reviews [record]
  (->> (get record "revisit_predicates")
       (filter (fn [{metric "metric" :as predicate}]
                 (predicate-triggered? (get-in record ["observations" metric])
                                       predicate)))
       (map #(get % "action"))
       distinct
       vec))
