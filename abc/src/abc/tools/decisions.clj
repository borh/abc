(ns abc.tools.decisions
  "Authoritative decision-records corpus: strict loader, Malli shape
   schema, and corpus semantic checks over docs/adr/decisions.edn.
   Replaces the retired abc.tools.adr Markdown grammar."
  (:require [abc.tools.malli :as am]
            [babashka.fs :as fs]
            [clojure.edn :as edn]
            [malli.core :as m]
            [malli.error :as me]
            [malli.registry :as mr])
  (:import [java.io PushbackReader StringReader]))

(def corpus-file "docs/adr/decisions.edn")

(defn problem [kind file message & {:as data}]
  (merge {:kind kind :file file :message message} data))

(def ^:private eof ::eof)

(defn load-corpus
  "Read exactly one EDN form (followed by EOF) from path.
   Returns {:corpus value} or {:problems [{:kind :invalid-edn …}]}."
  [path]
  (let [fail (fn [msg] {:problems [(problem :invalid-edn (str path) msg)]})]
    (cond
      (not (fs/exists? path)) (fail "decisions file does not exist")
      (fs/directory? path) (fail "decisions path is not a file")
      :else
      (try
        (with-open [r (PushbackReader. (StringReader. (slurp (fs/file path))))]
          (let [form (edn/read {:eof eof} r)
                extra (edn/read {:eof eof} r)]
            (cond
              (= eof form) (fail "decisions file contains no EDN form")
              (not= eof extra) (fail "decisions file must contain exactly one EDN form")
              :else {:corpus form})))
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
