(ns soranoha.snh.admission)

(def inclusion-rule
  "The executable rule value whose canonical bytes inclusion_rule_hash
  binds: the evaluator admits a candidate iff every fact status (the work
  assessment and each contribution) equals admit_when_all, excludes with
  exclude_reason iff any status equals exclude_when_any, and quarantines
  with quarantine_reason otherwise. Changing any decision-bearing value
  changes the hash."
  {"id" "za-public-domain-unanimous-v1"
   "admit_when_all" "public-domain"
   "exclude_when_any" "in-copyright"
   "exclude_reason" "in-copyright"
   "quarantine_reason" "not-fully-evaluated"})

(defn partition-candidates
  "Total partition of snapshot candidates under `rule` (the executable
  inclusion-rule value)."
  [rule candidates]
  (reduce
   (fn [acc {:strs [slug work_assessment contributions reliance]}]
     (let [statuses (map #(get % "status") (cons work_assessment contributions))]
       (cond
         (if reliance
           (= (get rule "admit_reliance_status") (get reliance "status"))
           (every? #{(get rule "admit_when_all")} statuses))
         (update acc :admitted conj slug)

         (and (not reliance) (some #{(get rule "exclude_when_any")} statuses))
         (update acc :excluded conj {"slug" slug
                                     "reason_code" (get rule "exclude_reason")})

         :else
         (update acc :quarantined conj
                 {"slug" slug
                  "reason_code" (get rule "quarantine_reason")}))))
   {:admitted [] :excluded [] :quarantined []}
   candidates))

(def reliance-inclusion-rule
  (assoc inclusion-rule "id" "za-assessment-or-aozora-reliance-v2"
         "admit_reliance_status" "relied-upon"))

(defn rule-for [snapshot]
  (if (= "snh-assessment-snapshot/2" (get snapshot "schema"))
    reliance-inclusion-rule
    inclusion-rule))
