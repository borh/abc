(ns soranoha.za.scaffold
  "Total quarantine baseline: the mechanical starting point of the
  owner's assessment-evidence input, and the release-time projection
  that keeps a supplied snapshot bound to the checkout it was scaffolded
  from.

  For every candidate the kernel's selection join yields, `snapshot`
  emits one snapshot candidate whose work assessment and whole
  contribution set are explicit not-evaluated facts — the statement that
  no completed assessment exists, and nothing else. No status here ever
  derives from a catalog field: the census predicate is a lexical screen,
  never assessment evidence (adopted derivation D-3), and completed
  assessments arrive later under their own authority.

  What this enumerates are CATALOG-LISTED CONTRIBUTION CANDIDATES: every
  (役割フラグ, 人物ID) row sharing the candidate's 作品ID, deduplicated.
  They are NOT the rights-relevant contribution set the protocol
  requires (spec §7). Adopted D-3 is explicit that catalog rows prove
  neither exhaustive authorship nor that every listed role holds
  copyright, so this enumeration is a starting list to assess, never a
  finding about who holds rights. Before any work's facts may become
  public-domain, its per-work assessment must ESTABLISH the exact
  rights-relevant contribution set — adding what the catalog omits and
  discharging what it wrongly lists — and the snapshot must carry that
  established set. All-not-evaluated candidates are safe because nothing
  is admitted; flipping statuses on this listing without first
  establishing the set would admit works on an unestablished
  contribution basis.

  An unknown role flag, a malformed person id, or a ragged row inside a
  selected work's row set fails closed: a row this enumeration cannot
  read is a row it must not silently drop."
  (:require [soranoha.snh.decode :as decode]
            [soranoha.yomi.catalog :as catalog]))

(def role-token
  "役割フラグ → contribution-id role token. Closed by construction: a
  catalog value outside this map fails rather than guessing at a new
  rights-relevant capacity."
  {"著者" "author"
   "翻訳者" "translator"
   "編者" "editor"
   "校訂者" "reviser"
   "その他" "other"})

(def not-evaluated
  "The explicit no-completed-assessment fact (snapshot schema: the
  not-evaluated arm carries null jurisdiction/effective_date/basis)."
  {"status" "not-evaluated"
   "jurisdiction" nil
   "effective_date" nil
   "basis" nil})

(defn- fail! [reason data]
  (throw (ex-info (str "assessment scaffold failed: " (name reason))
                  (assoc data :reason reason))))

(defn- contribution-id
  [row]
  (when (get row catalog/ragged-key)
    (fail! :ragged-contributor-row
           {:work-id (catalog/row-work-id row)
            :person-id (catalog/row-person-id row)}))
  (let [role (get row "役割フラグ")
        person (catalog/row-person-id row)
        token (or (get role-token role)
                  (fail! :unknown-role-flag
                         {:work-id (catalog/row-work-id row)
                          :role role}))]
    (when-not (and person (re-matches #"[0-9]{6}" person))
      (fail! :malformed-person-id
             {:work-id (catalog/row-work-id row) :person-id person}))
    (str token ":" person)))

(defn projection
  "{slug -> [catalog-listed contribution candidate id ...]} over a
  selection, ids sorted and deduplicated. The scaffold emits it and the
  release preflight re-derives it from the checkout under release: a
  snapshot whose projection differs no longer describes the corpus being
  published, even when every slug still matches."
  [rows candidates]
  (let [by-work-id (group-by catalog/row-work-id rows)]
    (into {}
          (map (fn [{:keys [slug row]}]
                 (let [work-id (catalog/row-work-id row)
                       work-rows (or (seq (get by-work-id work-id))
                                     ;; unreachable while the candidate's own
                                     ;; joined row groups under its work id;
                                     ;; guarded so a grouping regression cannot
                                     ;; emit an empty contribution set
                                     (fail! :no-catalog-rows-for-work
                                            {:slug slug :work-id work-id}))]
                   [slug (vec (distinct (sort (map contribution-id
                                                   work-rows))))])))
          candidates)))

(defn snapshot-projection
  "The same {slug -> [contribution-id ...]} shape read back off a decoded
  snapshot value, for comparison against `projection`."
  [snapshot-value]
  (into {}
        (map (fn [candidate]
               [(get candidate "slug")
                (mapv #(get % "contribution_id")
                      (get candidate "contributions"))]))
        (get snapshot-value "candidates")))

(defn projection-drift
  "nil when the checkout's and the snapshot's projections are identical;
  otherwise a diagnostic naming the drift — slugs on only one side, and
  slugs whose contribution-candidate lists differ (the case slug
  totality alone cannot see: a catalog revision that adds a translator
  or changes a role while every slug survives). Counts are complete and
  each listed sample is bounded and reported beside its own count, so a
  large drift stays readable without truncating silently."
  ([checkout snapshot] (projection-drift checkout snapshot 20))
  ([checkout snapshot sample-limit]
   (let [only-checkout (sort (remove (set (keys snapshot)) (keys checkout)))
         only-snapshot (sort (remove (set (keys checkout)) (keys snapshot)))
         differing (sort (for [[slug ids] checkout
                               :let [other (get snapshot slug)]
                               :when (and other (not= ids other))]
                           slug))]
     (when (or (seq only-checkout) (seq only-snapshot) (seq differing))
       {:only-in-checkout-count (count only-checkout)
        :only-in-checkout-sample (vec (take sample-limit only-checkout))
        :only-in-snapshot-count (count only-snapshot)
        :only-in-snapshot-sample (vec (take sample-limit only-snapshot))
        :contributions-differ-count (count differing)
        :contributions-differ-sample (vec (take sample-limit differing))}))))

(defn snapshot
  "Canonical snh-assessment-snapshot/1 for the selection: one
  all-not-evaluated candidate per selected slug over its catalog-listed
  contribution candidates. Returns the encoder's {:bytes :value :hex :id}
  — encode round-trips through boundary decode, so the emitted bytes are
  by construction exactly what the release driver and verifier accept."
  [rows candidates]
  (decode/encode
   "assessment-snapshot"
   {"schema" "snh-assessment-snapshot/1"
    "candidates"
    (vec (sort-by #(get % "slug")
                  (map (fn [[slug ids]]
                         {"slug" slug
                          "work_assessment" not-evaluated
                          "contributions"
                          (mapv #(assoc not-evaluated "contribution_id" %)
                                ids)})
                       (projection rows candidates))))}))
