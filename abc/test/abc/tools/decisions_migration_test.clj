(ns abc.tools.decisions-migration-test
  "BRANCH-ONLY: proves decisions.edn ≡ the legacy Markdown corpus.
   Deleted together with abc.tools.adr in the cutover task."
  (:require [abc.tools.adr :as adr]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(def ^:private adr-dir "docs/adr")

(defn- slug-of [filename] (subs filename 5 (- (count filename) 3)))

(def ^:private claim-prefix
  #"^\*\*ADR-[0-9]{4}-C[1-9][0-9]* — [a-z]+(?:-[a-z]+)*:\*\*\s*")

(defn- legacy-claims [{:keys [criteria]}]
  (vec
   (map-indexed
    (fn [i {:keys [body claim-id claim-kind]}]
      (let [tokens (->> (re-seq #"`([^`]+)`" body)
                        (map second)
                        (filterv (fn [t]
                                   (some #(str/starts-with? t %)
                                         ["test/" "fixtures/" "nix/"
                                          "docs/evidence/external/"]))))]
        ;; "ADR-0043-C1" -> :c1 (digits start after "ADR-NNNN-C", index 10)
        (cond-> {:id (if claim-id
                       (keyword (str "c" (subs claim-id 10)))
                       (keyword (str "c" (inc i))))
                 :statement (str/replace body claim-prefix "")}
          claim-kind (assoc :kind claim-kind)
          (seq tokens) (assoc :evidence tokens))))
    criteria)))

(defn- legacy-relations [num->slug {:keys [relations]}]
  (set
   (for [[type key] [[:supersedes :supersedes]
                     [:amends :amends]
                     [:depends-on :depends-on]]
         {:keys [target scope]} (get relations key)]
     (cond-> {:class :lifecycle :type type :to (num->slug target)}
       scope (assoc :scope scope)))))

(defn- legacy-fact-map [_num->slug a]
  {:slug (slug-of (:file a))
   :legacy-number (:num a)
   :title (:title a)
   :status (keyword (str/lower-case (:status a)))
   :date (:date a)
   :accepted (:accepted a)
   :validation-scope (some-> (:validation-scope a) keyword)
   :release-authority (some-> (:release-authority a) keyword)
   :source (get (:fields a) "Source")
   :claims (legacy-claims a)})

(deftest decisions-edn-is-fact-equivalent-to-legacy-corpus
  (let [adrs (adr/parse-all adr-dir)
        num->slug (into {} (map (juxt :num #(slug-of (:file %))) adrs))
        sidecar (:relations (edn/read-string (slurp "docs/adr/adr-relations.edn")))
        corpus (edn/read-string (slurp "docs/adr/decisions.edn"))
        by-slug (into {} (map (juxt :slug identity) (:decisions corpus)))]
    (is (= (count adrs) (count (:decisions corpus))))
    (doseq [a adrs
            :let [slug (slug-of (:file a))
                  rec (get by-slug slug)]]
      (is (some? rec) slug)
      ;; scalar facts + claims, field by field; nil legacy value means
      ;; the key must be absent from the record
      (doseq [[k expected] (legacy-fact-map num->slug a)]
        (if (nil? expected)
          (is (not (contains? rec k)) (str slug " must omit " k))
          (is (= expected (get rec k)) (str slug " " k))))
      ;; lifecycle edges: acting side only
      (is (= (legacy-relations num->slug a)
             (set (filter #(= :lifecycle (:class %)) (:relations rec))))
          (str slug " lifecycle relations")))
    ;; annotation edges: all sidecar rows, notes verbatim
    (let [expected (set (for [r sidecar]
                          (cond-> {:class :annotation :type (:type r)
                                   :from (num->slug (:from r))
                                   :to (num->slug (:to r))}
                            (:note r) (assoc :note (:note r)))))
          actual (set (for [rec (:decisions corpus)
                            rel (:relations rec)
                            :when (= :annotation (:class rel))]
                        (assoc rel :from (:slug rec))))]
      (is (= expected actual) "annotation relations with notes"))))
