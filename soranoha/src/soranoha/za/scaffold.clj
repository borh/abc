(ns soranoha.za.scaffold
  "Total assessment-snapshot scaffold: the mechanical starting point of
  the owner's assessment-evidence input. For every candidate the
  kernel's selection join yields, emit one snapshot candidate whose
  work assessment and whole contribution set are explicit not-evaluated
  facts — the statement that no completed assessment exists, and
  nothing else. No status here ever derives from a catalog field: the
  census predicate is a lexical screen, never assessment evidence
  (adopted derivation D-3), and completed assessments arrive later
  under their own authority. Contribution enumeration is the catalog's
  own rights-relevant contributor set — every (役割フラグ, 人物ID) row
  sharing the candidate's 作品ID, deduplicated. An unknown role flag, a
  malformed person id, or a ragged row inside a selected work's row set
  fails the scaffold closed: a row this enumeration cannot read is a
  row it must not silently drop."
  (:require [soranoha.snh.decode :as decode]
            [soranoha.yomi.catalog :as catalog]))

(def role-token
  "役割フラグ → contribution-id role token. Closed by construction: a
  catalog value outside this map fails the scaffold rather than
  guessing at a new rights-relevant capacity."
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

(defn- candidate-value
  [rows-by-work-id {:keys [slug row]}]
  (let [work-id (catalog/row-work-id row)
        work-rows (or (seq (get rows-by-work-id work-id))
                      ;; unreachable while the candidate's own joined row
                      ;; groups under its work id; guarded so a grouping
                      ;; regression cannot emit an empty contribution set
                      (fail! :no-catalog-rows-for-work
                             {:slug slug :work-id work-id}))
        ids (distinct (sort (map contribution-id work-rows)))]
    {"slug" slug
     "work_assessment" not-evaluated
     "contributions" (mapv #(assoc not-evaluated "contribution_id" %) ids)}))

(defn snapshot
  "Canonical snh-assessment-snapshot/1 for the selection: one
  all-not-evaluated candidate per selected slug, contributions from the
  full catalog row set of each candidate's 作品ID. Returns the encoder's
  {:bytes :value :hex :id} — encode round-trips through boundary decode,
  so the emitted bytes are by construction exactly what the release
  driver and verifier will accept."
  [rows candidates]
  (let [by-work-id (group-by catalog/row-work-id rows)]
    (decode/encode
     "assessment-snapshot"
     {"schema" "snh-assessment-snapshot/1"
      "candidates" (vec (sort-by #(get % "slug")
                                 (map #(candidate-value by-work-id %)
                                      candidates)))})))
