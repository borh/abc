(ns abc.tools.parser-release-authority
  "The single boundary that authenticates the exact accepted parser release
  candidate a campaign has qualified for promotion.

  Composes abc.tools.parser-rq-campaign's promotion verification (candidate,
  evaluation, evidence, and dependency-ADR authentication) with the one
  governance decision that grants promotion its release authority, and
  reports every failure as one closed vector of problem maps rather than ad
  hoc booleans or bare strings. Later tasks (source trust, release
  admissibility) depend on this value and must not re-derive it."
  (:require [abc.tools.decisions :as decisions]
            [abc.tools.parser-rq-campaign :as campaign]))

(def release-qualification-slug
  "custom-parser-release-qualification")

(defn- problem [kind message & {:as data}]
  (merge {:kind kind :message message} data))

(defn- decision-resolution
  "Resolve the release-qualification decision from decisions-path via the
  shared strict corpus boundary (abc.tools.decisions/load-shape-valid-corpus!
  and decision-by-slug!). Never trusts a decision whose corpus failed to
  load or shape-validate, and never invents a status for a missing slug.

  Returns {:decision record :content-hash sha256} on success, or
  {:problems [...]} — never both."
  [decisions-path]
  (try
    (let [{:keys [corpus content-hash]}
          (decisions/load-shape-valid-corpus! decisions-path)]
      (try
        {:decision (decisions/decision-by-slug! corpus release-qualification-slug)
         :content-hash content-hash}
        (catch clojure.lang.ExceptionInfo error
          {:problems (mapv #(problem :missing-decision %) (:errors (ex-data error)))})))
    (catch clojure.lang.ExceptionInfo error
      {:problems (mapv #(problem :invalid-decisions-corpus %) (:errors (ex-data error)))})))

(defn- authority-problems
  "The release-authority decision must be exactly {:status :accepted
  :release-authority :publication}. :release-authority is the sole
  authorization field here; :validation-scope (e.g. the current decision's
  smoke-corpus qualification and full 17,886-work conversion audit) is a
  governed fact of this exact Accepted record, never an ordered runtime
  policy this code interprets or a hardcoded set of \"stronger\" scope
  keywords. A future scope change requires a deliberate superseding
  decision that updates this binding."
  [decision]
  (cond-> []
    (not= :accepted (:status decision))
    (conj (problem :decision-not-accepted
                   (str "decision " release-qualification-slug " is not accepted")
                   :status (:status decision)))

    (not= :publication (:release-authority decision))
    (conj (problem :release-authority-not-publication
                   (str "decision " release-qualification-slug
                        " release-authority is not :publication")
                   :release-authority (:release-authority decision)))))

(defn authenticate
  "The single authenticated parser-promotion value: the exact accepted
  candidate a campaign has qualified for release, bound to the exact
  Accepted decision that grants promotion its release authority.

  opts carries the same explicit campaign paths as
  campaign/promotion-verification (:runs_root :candidate_ref :registry_path
  :measurements_path :report_path :provenance_path :decisions_path).
  Delegates all candidate/evaluation/evidence authentication to
  campaign/promotion-verification and never re-reads or reinterprets those
  values; only projects them.

  Returns {:candidate-ref :qualification-identity-ref :qualification-identity
  :executable-provenance :decision :authority-hashes :registry-ref}.
  Throws ex-info \"parser release authority authentication failed\" carrying
  {:problems [problem-map ...]} on any failure."
  [{:keys [decisions_path] :as opts}]
  (let [verification (campaign/promotion-verification opts)
        promotion-problems (mapv #(problem :promotion-problem %) (:problems verification))
        {:keys [decision content-hash problems]} (decision-resolution decisions_path)
        release-problems (if decision (authority-problems decision) [])
        all-problems (vec (concat promotion-problems problems release-problems))]
    (if (seq all-problems)
      (throw (ex-info "parser release authority authentication failed"
                      {:problems all-problems}))
      (let [candidate (:candidate verification)]
        {:candidate-ref (:candidate_ref candidate)
         :qualification-identity-ref (:qualification_identity_ref candidate)
         :qualification-identity (:qualification_identity candidate)
         :executable-provenance (:provenance verification)
         :decision decision
         :authority-hashes {:decisions-file content-hash
                            :registry-file (:registry-file-hash verification)}
         :registry-ref (:registry-ref verification)}))))
