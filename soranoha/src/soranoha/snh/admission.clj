(ns soranoha.snh.admission)

(def inclusion-rule
  "The executable rule value whose canonical bytes inclusion_rule_hash binds.
  Changing any decision-bearing value changes the hash.

  A candidate is decided on one of two tracks, and which one it is on is
  settled by whether the snapshot carries an edition reliance for it. A
  reliance candidate relies on Aozora Bunko's own rights determination, and it
  is admitted exactly when that reliance has admit_reliance_status; anything
  else about it quarantines with quarantine_reason. An independent candidate
  is admitted when every fact status (the work assessment and each
  contribution) equals admit_when_all, excluded with exclude_reason when any
  status equals exclude_when_any, and quarantined otherwise.

  So exclude_when_any decides the independent track only. A candidate whose
  reliance was refused is quarantined even when a status in the snapshot reads
  in-copyright, which is the case worth stating because it looks like a missed
  exclusion. It is not: the release asked Aozora Bunko about that edition and
  did not get an answer it could publish on, so the record says the question
  was not carried through rather than answering it from the track that was not
  taken. The refusal reason `restrictive-independent-assessment` is the
  sharpest instance, and it is still a reliance outcome rather than an
  independent decision. Neither bucket publishes, so nothing turns on this
  beyond what the report says happened.

  `docs/design/snh-protocol-v1.md` states the same two tracks and is the
  frozen text this implements."
  {"id" "za-assessment-or-aozora-reliance-v2"
   "admit_reliance_status" "relied-upon"
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
