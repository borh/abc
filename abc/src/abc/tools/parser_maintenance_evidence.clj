(ns abc.tools.parser-maintenance-evidence)

(def required-categories
  #{"upstream_review" "selective_ports" "security_review" "missed_fixes"
    "fork_only_defects" "planned_divergence" "remeasurement"
    "maintainer_availability"})

(defn- calendar-date? [value]
  (and (string? value)
       (try
         (java.time.LocalDate/parse value)
         true
         (catch java.time.format.DateTimeParseException _ false))))

(defn- nonnegative-integer? [value]
  (and (integer? value) (not (neg? value))))

(defn problems [record]
  (let [categories (get record "categories")
        review-after (get record "review_after")
        expires-on (get record "expires_on")
        estimate (get-in record ["trial_merge" "estimate"])
        component-minutes (map #(get % "minutes") (get estimate "components"))]
    (vec
     (concat
      (when-not (= "1.0.0" (get record "protocol_version"))
        ["protocol_version must be 1.0.0"])
      (when-not (= required-categories (set (keys categories)))
        ["categories must contain exactly the required maintenance categories"])
      (when-not (every? #(nonnegative-integer? (get % "minutes"))
                        (vals categories))
        ["every maintenance category must record non-negative integer minutes"])
      (when-not (pos-int? (get-in record ["focused_session" "duration_minutes"]))
        ["focused_session.duration_minutes must be a positive integer"])
      (when-not (and (calendar-date? review-after) (calendar-date? expires-on))
        ["review_after and expires_on must be calendar dates"])
      (when (and (calendar-date? review-after) (calendar-date? expires-on)
                 (pos? (compare review-after expires-on)))
        ["review_after must be on or before expires_on"])
      (when-not (= "expert-assessment"
                   (get estimate "evidence_kind"))
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
        ["decision review predicates cannot be empty"])))))

(defn- predicate-triggered? [observed {operator "operator" threshold "threshold"}]
  (case operator
    ">" (and (number? observed) (> observed threshold))
    ">=" (and (number? observed) (>= observed threshold))
    "=" (= observed threshold)
    false))

(defn triggered-reviews [record observations]
  (->> (get record "revisit_predicates")
       (filter (fn [{metric "metric" :as predicate}]
                 (predicate-triggered? (get observations metric) predicate)))
       (map #(get % "action"))
       distinct
       vec))
