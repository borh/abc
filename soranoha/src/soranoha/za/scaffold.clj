(ns soranoha.za.scaffold
  "Project selected catalog rows into contribution candidate identifiers.
  Catalog declarations do not establish exhaustive authorship or rights facts.
  Assessment evaluation and release drift checks share this projection;
  malformed selected rows fail rather than silently dropping candidates."
  (:require [soranoha.aozora.csv :as csv]
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
  (when (get row csv/ragged-key)
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
  selection, ids sorted and deduplicated. Assessment evaluation uses it and
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
