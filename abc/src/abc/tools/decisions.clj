(ns abc.tools.decisions
  "Authoritative decision-records corpus: strict loader, Malli shape
   schema, and corpus semantic checks over docs/adr/decisions.edn.
   Replaces the retired abc.tools.adr Markdown grammar."
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.malli :as am]
            [abc.tools.path-containment :as containment]
            [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [malli.core :as m]
            [malli.error :as me]
            [malli.registry :as mr])
  (:import [java.io PushbackReader StringReader]
           [java.nio ByteBuffer]
           [java.nio.charset CodingErrorAction StandardCharsets]))

(def corpus-file "docs/adr/decisions.edn")

(defn problem [kind file message & {:as data}]
  (merge {:kind kind :file file :message message} data))

(def ^:private eof ::eof)

(defn- decode-strict-utf8
  "Decode bytes as EDN source, failing (rather than substituting) on
   malformed or unmappable byte sequences."
  [^bytes bytes]
  (let [decoder (doto (.newDecoder StandardCharsets/UTF_8)
                  (.onMalformedInput CodingErrorAction/REPORT)
                  (.onUnmappableCharacter CodingErrorAction/REPORT))]
    (str (.decode decoder (ByteBuffer/wrap bytes)))))

(defn load-corpus
  "Read the decisions file's bytes exactly once, decode them as strict
   UTF-8, and parse exactly one EDN form (followed by EOF) from that text.
   Returns {:corpus value :bytes bytes} — the same bytes callers may hash
   without a second read — or {:problems [{:kind :invalid-edn …}]}."
  [path]
  (let [fail (fn [msg] {:problems [(problem :invalid-edn (str path) msg)]})]
    (cond
      (not (fs/exists? path)) (fail "decisions file does not exist")
      (fs/directory? path) (fail "decisions path is not a file")
      :else
      (try
        (let [bytes (fs/read-all-bytes path)
              text (decode-strict-utf8 bytes)]
          (with-open [r (PushbackReader. (StringReader. text))]
            (let [form (edn/read {:eof eof} r)
                  extra (edn/read {:eof eof} r)]
              (cond
                (= eof form) (fail "decisions file contains no EDN form")
                (not= eof extra) (fail "decisions file must contain exactly one EDN form")
                :else {:corpus form :bytes bytes}))))
        (catch Exception e
          (fail (str "decisions file is not readable EDN: "
                     (.getMessage e))))))))

;; --- shape schema ------------------------------------------------------------

(def statuses #{:draft :proposed :accepted :superseded :withdrawn})
(def validation-scopes
  #{:structural :fixture :smoke-corpus :full-corpus :operational})
(def release-authorities #{:none :development :publication})
(def lifecycle-types #{:supersedes :amends :depends-on})
(def frozen-legacy-numbers
  ;; Closed at migration: the 42 numbers the Markdown corpus ever used
  ;; (36 was never assigned). New records are slug-only.
  #{1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
    26 27 28 29 30 31 32 33 34 35 37 38 39 40 41 42 43})
(def evidence-prefixes ["test/" "fixtures/" "nix/" "docs/evidence/external/"])

(def ^:private slug-pattern #"^[a-z0-9]+(?:-[a-z0-9]+)*$")
(def ^:private date-pattern #"^\d{4}-\d{2}-\d{2}$")
(def ^:private claim-id-pattern #"^c[1-9][0-9]*$")

(def schemas
  {::slug [:re {:error/message "must be a kebab-case slug"} slug-pattern]
   ::date [:and [:re {:error/message "must be YYYY-MM-DD"} date-pattern]
           [:fn {:error/message "must be a calendar-valid date"}
            (fn [s] (try (java.time.LocalDate/parse s) true
                         (catch Exception _ false)))]]
   ::relation
   [:multi {:dispatch :class
            :error/message "relation :class must be :lifecycle or :annotation"}
    [:lifecycle
     [:map {:closed true}
      [:class [:= :lifecycle]]
      [:type (into [:enum {:error/message
                           "lifecycle type must be :supersedes, :amends, or :depends-on"}]
                   (sort lifecycle-types))]
      [:to ::slug]
      [:scope {:optional true} ::am/nonblank-string]]]
    [:annotation
     [:map {:closed true}
      [:class [:= :annotation]]
      [:type [:and :keyword
              [:fn {:error/message "annotation type must not shadow a lifecycle type"}
               (fn [t] (not (contains? lifecycle-types t)))]]]
      [:to ::slug]
      [:note {:optional true} ::am/nonblank-string]]]]
   ::claim
   [:map {:closed true}
    [:id [:and :keyword
          [:fn {:error/message "claim id must be :cN"}
           (fn [k] (boolean (re-matches claim-id-pattern (name k))))]]]
    [:kind {:optional true} :keyword]
    [:statement ::am/nonblank-string]
    [:evidence {:optional true} [:vector {:min 1} ::am/nonblank-string]]]
   ::record
   [:and
    [:map {:closed true}
     [:slug ::slug]
     [:legacy-number {:optional true}
      (into [:enum {:error/message "legacy number outside the frozen set"}]
            (sort frozen-legacy-numbers))]
     [:title ::am/nonblank-string]
     [:status (into [:enum] (sort statuses))]
     [:date ::date]
     [:accepted {:optional true} ::date]
     [:validation-scope {:optional true}
      (into [:enum] (sort validation-scopes))]
     [:release-authority {:optional true}
      (into [:enum] (sort release-authorities))]
     [:source {:optional true} ::am/nonblank-string]
     [:release-parser-identity {:optional true}
      [:map {:closed true}
       [:record_path ::am/nonblank-string]
       [:schema_version ::am/nonblank-string]
       [:candidate_ref [:re {:error/message "must be a sha256:… ref"}
                        hash/hash-pattern]]]]
     [:topics [:vector :keyword]]
     [:relations [:vector ::relation]]
     [:claims [:vector ::claim]]]
    [:fn {:error/message "claim ids must be unique within the record"}
     (fn [{:keys [claims]}]
       (or (empty? claims) (apply distinct? (map :id claims))))]
    [:fn {:error/message
          "accepted date is required for :accepted, permitted on :superseded, forbidden otherwise; :accepted also requires validation scope and release authority"}
     (fn [{:keys [status accepted validation-scope release-authority]}]
       (case status
         :accepted (boolean (and accepted validation-scope release-authority))
         :superseded true
         (nil? accepted)))]
    [:fn {:error/message
          "accepted claims require :kind and non-empty :evidence"}
     (fn [{:keys [status claims]}]
       (or (not= :accepted status)
           (every? #(and (:kind %) (seq (:evidence %))) claims)))]]
   ::corpus
   [:and
    [:map {:closed true} [:decisions [:vector ::record]]]
    [:fn {:error/message "slugs must be unique"}
     (fn [{:keys [decisions]}]
       (or (empty? decisions) (apply distinct? (map :slug decisions))))]
    [:fn {:error/message "legacy numbers must be unique"}
     (fn [{:keys [decisions]}]
       (let [numbers (keep :legacy-number decisions)]
         (or (empty? numbers) (apply distinct? numbers))))]]})

(def registry
  (mr/composite-registry (m/default-schemas) am/scalar-schemas schemas))

(defn shape-problems [corpus file]
  (if-let [explanation (m/explain ::corpus corpus {:registry registry})]
    [(problem :invalid-shape file
              (pr-str (me/humanize explanation)))]
    []))

;; --- shared strict corpus boundary -------------------------------------------
;; Composes load-corpus and shape-problems into the one authority other
;; namespaces (parser release qualification, publication admissibility, …)
;; depend on. This boundary promises strict load plus shape only; corpus
;; semantic governance (relation graphs, evidence paths, narrative files)
;; stays out of scope and lives in the checks below.

(defn- by-slug [corpus]
  (into {} (map (juxt :slug identity)) (:decisions corpus)))

(defn load-shape-valid-corpus!
  "Strictly load and shape-validate the decisions corpus at path.

   Returns {:corpus corpus :content-hash sha256}, where content-hash
   authenticates the exact bytes the corpus was parsed from. On any load or
   shape failure, throws ex-info carrying a stable :errors vector of
   message strings."
  [path]
  (let [{:keys [corpus problems bytes]} (load-corpus path)
        file (str path)
        problems (into (vec problems)
                       (when corpus (shape-problems corpus file)))]
    (if (seq problems)
      (throw (ex-info "decisions corpus invalid"
                      {:errors (mapv :message problems)}))
      {:corpus corpus
       :content-hash (hash/format-sha256 (hash/sha256-bytes bytes))})))

(defn decision-by-slug!
  "Return the one decision record in corpus with slug. Throws ex-info
   carrying a stable :errors vector when no record has that slug."
  [corpus slug]
  (or (get (by-slug corpus) slug)
      (throw (ex-info "decision slug not found"
                      {:errors [(str "no decision record has slug " (pr-str slug))]}))))

;; --- semantic checks ---------------------------------------------------------
;; These run only on a shape-valid corpus and may assume well-formed records.

(defn- lifecycle-edges [record type & {:keys [unscoped-only]}]
  (for [r (:relations record)
        :when (and (= :lifecycle (:class r)) (= type (:type r))
                   (or (not unscoped-only) (nil? (:scope r))))]
    r))

(defn- relation-problems [corpus file]
  (let [known (set (map :slug (:decisions corpus)))]
    (vec
     (concat
      (for [rec (:decisions corpus)
            [edge freq] (frequencies (map #(dissoc % :note) (:relations rec)))
            :when (< 1 freq)]
        (problem :duplicate-relation file
                 "relation must not appear more than once"
                 :slug (:slug rec) :value edge))
      (for [rec (:decisions corpus)
            r (:relations rec)
            :when (not (contains? known (:to r)))]
        (problem :missing-relation-target file
                 "relation target does not exist"
                 :slug (:slug rec) :value r))
      (for [rec (:decisions corpus)
            r (:relations rec)
            :when (and (= :lifecycle (:class r)) (= (:slug rec) (:to r)))]
        (problem :self-relation file
                 "lifecycle relation must not point at its own record"
                 :slug (:slug rec) :value r))))))

(defn- cycle-problems [corpus file]
  ;; Three-color DFS per lifecycle graph; unscoped edges only for
  ;; :supersedes (a scoped supersession replaces one slice, not the record).
  (let [index (by-slug corpus)
        graphs {:depends-on
                (fn [r] (map :to (lifecycle-edges r :depends-on)))
                :supersedes
                (fn [r] (map :to (lifecycle-edges r :supersedes
                                                  :unscoped-only true)))
                :amends
                (fn [r] (map :to (lifecycle-edges r :amends)))}]
    (vec
     (for [[type neighbors] graphs
           :let [cyclic?
                 (fn cyclic? [slug state]
                   (case (get @state slug)
                     :done false
                     :active true
                     (do (swap! state assoc slug :active)
                         (let [hit (some #(cyclic? % state)
                                         (when-let [r (get index slug)]
                                           (neighbors r)))]
                           (swap! state assoc slug :done)
                           (boolean hit)))))]
           rec (filterv #(cyclic? (:slug %) (atom {})) (:decisions corpus))]
       (problem :relation-cycle file
                (str (name type) " relations must be acyclic")
                :slug (:slug rec) :relation type)))))

(defn- supersession-problems [corpus file]
  (let [index (by-slug corpus)
        unscoped-targets (set (for [rec (:decisions corpus)
                                    r (lifecycle-edges rec :supersedes
                                                       :unscoped-only true)]
                                (:to r)))]
    (vec
     (concat
      (for [rec (:decisions corpus)
            r (lifecycle-edges rec :supersedes :unscoped-only true)
            :let [target (get index (:to r))]
            :when (and target (not= :superseded (:status target)))]
        (problem :unscoped-supersession-target-not-superseded file
                 "an unscoped supersession target must be :superseded"
                 :slug (:slug rec) :value r))
      (for [{:keys [slug status]} (:decisions corpus)
            :when (and (= :superseded status)
                       (not (contains? unscoped-targets slug)))]
        (problem :superseded-without-successor file
                 "a :superseded record requires an incoming unscoped supersession"
                 :slug slug))))))

(defn- lifecycle-date-problems [corpus file]
  (for [{:keys [slug status date accepted]} (:decisions corpus)
        :when (and (= :accepted status) date accepted
                   (.isBefore (java.time.LocalDate/parse accepted)
                              (java.time.LocalDate/parse date)))]
    (problem :accepted-before-date file
             "accepted date must not be before :date" :slug slug)))

(defn- dependency-problems [corpus file]
  (let [index (by-slug corpus)
        closure (fn [slug]
                  (loop [queue (vec (map :to (lifecycle-edges
                                              (get index slug) :depends-on)))
                         seen #{}]
                    (if-let [s (first queue)]
                      (if (contains? seen s)
                        (recur (subvec queue 1) seen)
                        (recur (into (subvec queue 1)
                                     (when-let [r (get index s)]
                                       (map :to (lifecycle-edges r :depends-on))))
                               (conj seen s)))
                      seen)))]
    (for [{:keys [slug status]} (:decisions corpus)
          :when (= :accepted status)
          dep (sort (closure slug))
          :let [target (get index dep)]
          :when (and target (not= :accepted (:status target)))]
      (problem :noncanonical-dependency-path file
               "Accepted dependency closure contains a non-Accepted record"
               :slug slug :target dep :target-status (:status target)))))

(defn- evidence-problems [corpus repo-root file]
  (vec
   (concat
    (for [{:keys [slug status claims]} (:decisions corpus)
          :when (and (= :accepted status) (empty? claims))]
      (problem :missing-claims file
               "Accepted records require at least one claim" :slug slug))
    (for [{:keys [slug claims]} (:decisions corpus)
          {:keys [id evidence]} claims
          path evidence
          :let [{:keys [state] :as contained}
                (containment/path-state repo-root path)
                normalized (some-> (:relative contained)
                                   (str/replace "\\" "/"))
                kind (case state
                       :ok (when-not (some #(str/starts-with? normalized %)
                                           evidence-prefixes)
                             :evidence-outside-roots)
                       :missing :missing-evidence-path
                       :real-path-escape :evidence-real-path-escape
                       :malformed-path :malformed-evidence-path
                       :evidence-path-traversal)]
          :when kind]
      (problem kind file
               (case kind
                 :evidence-outside-roots
                 "evidence path is outside the evidence roots"
                 :missing-evidence-path "evidence path does not exist"
                 :evidence-real-path-escape
                 "evidence real path escapes the repository"
                 :malformed-evidence-path "evidence path is malformed"
                 "evidence path contains lexical traversal")
               :slug slug :claim id :value path))
    (for [{:keys [slug claims]} (:decisions corpus)
          {:keys [id evidence]} claims
          path evidence
          :let [contained (containment/path-state repo-root path)]
          :when (and (= :ok (:state contained))
                     (files/directory? (:path contained)))
          :when (not-any?
                 (fn [companion]
                   (and (or (str/starts-with? companion "test/")
                            (str/starts-with? companion "nix/"))
                        (let [c (containment/path-state repo-root companion)]
                          (and (= :ok (:state c)) (files/file? (:path c))))))
                 evidence)]
      (problem :unverified-evidence-directory file
               "a directory evidence path requires an existing test/ or nix/ file in the same claim"
               :slug slug :claim id :value path)))))

(defn semantic-problems [corpus repo-root file]
  (vec (concat (relation-problems corpus file)
               (cycle-problems corpus file)
               (supersession-problems corpus file)
               (lifecycle-date-problems corpus file)
               (dependency-problems corpus file)
               (evidence-problems corpus repo-root file))))

(defn narrative-problems [corpus repo-root adr-dir]
  (let [dir (fs/path repo-root adr-dir)
        expected (set (map #(str (:slug %) ".md") (:decisions corpus)))
        generated #{"README.md" "INDEX.md"}
        actual (set (for [f (fs/list-dir dir)
                          :let [n (fs/file-name f)]
                          :when (and (fs/regular-file? f)
                                     (str/ends-with? n ".md"))]
                      n))]
    (vec
     (concat
      (for [n (sort expected) :when (not (contains? actual n))]
        (problem :missing-narrative (str adr-dir "/" n)
                 "decision record has no narrative file"))
      (for [n (sort actual)
            :when (and (not (contains? expected n))
                       (not (contains? generated n)))]
        (problem :orphan-narrative (str adr-dir "/" n)
                 "narrative file has no decision record"))))))

(defn validate-repository
  "Strictly validate the decisions corpus under repo-root. Shape problems
   short-circuit semantic validation; an unreadable corpus reports only
   :invalid-edn."
  ([repo-root] (validate-repository repo-root "docs/adr"))
  ([repo-root adr-dir]
   (let [path (str (fs/path repo-root adr-dir "decisions.edn"))
         {:keys [corpus problems]} (load-corpus path)]
     (if problems
       problems
       (let [shape (shape-problems corpus corpus-file)]
         (if (seq shape)
           shape
           (vec (concat (semantic-problems corpus repo-root corpus-file)
                        (narrative-problems corpus repo-root adr-dir)))))))))
