(ns migrate-decisions
  "BRANCH-ONLY extractor: legacy Markdown corpus + adr-relations.edn
   -> docs/adr/decisions.edn. Deleted in the cutover task."
  (:require [abc.tools.adr :as adr]
            [clojure.edn :as edn]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

(def adr-dir "docs/adr")

(def topics
  ;; Initial descriptive topics per legacy number; reviewed by hand.
  {1 [:identity] 2 [:parser] 3 [:nix :materialization]
   4 [:security :release] 5 [:runtime] 6 [:validation]
   7 [:parser :boundary] 8 [:runtime :tooling] 9 [:materialization]
   10 [:identity] 11 [:fixtures] 12 [:tei :validation]
   13 [:lod :publication] 14 [:iiif :publication] 15 [:temporal]
   16 [:temporal] 17 [:lod :vocabulary] 18 [:lod :vocabulary]
   19 [:governance] 20 [:person-drift] 21 [:person-drift]
   22 [:person-drift :ingest] 23 [:parser :identity]
   24 [:parser :publication] 25 [:parser :publication]
   26 [:analysis :identity] 27 [:analysis :tokenizer]
   28 [:publication :annotation] 29 [:diagrams :governance]
   30 [:parser] 31 [:governance] 32 [:parser]
   33 [:identity :source-bundle] 34 [:governance :evidence]
   35 [:rights] 37 [:publication :provenance] 38 [:parser :governance]
   39 [:parser :release] 40 [:parser :release] 41 [:parser :release]
   42 [:parser :evidence] 43 [:governance :evidence]})

(defn- slug-of [filename] (subs filename 5 (- (count filename) 3)))

(def claim-prefix
  #"^\*\*ADR-[0-9]{4}-C[1-9][0-9]* — [a-z]+(?:-[a-z]+)*:\*\*\s*")

(defn- claims [a]
  (vec
   (map-indexed
    (fn [i {:keys [body claim-id claim-kind]}]
      (let [tokens (->> (re-seq #"`([^`]+)`" body) (map second)
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
    (:criteria a))))

(defn- record [num->slug sidecar-by-from a]
  (let [n (:num a)]
    (cond->
     {:slug (num->slug n)
      :legacy-number n
      :title (:title a)
      :status (keyword (str/lower-case (:status a)))
      :date (:date a)
      :topics (get topics n [])
      :relations
      (vec
       (concat
        (for [[type key] [[:supersedes :supersedes]
                          [:amends :amends]
                          [:depends-on :depends-on]]
              {:keys [target scope]} (get-in a [:relations key])]
          (cond-> {:class :lifecycle :type type :to (num->slug target)}
            scope (assoc :scope scope)))
        (for [r (get sidecar-by-from n)]
          (cond-> {:class :annotation :type (:type r)
                   :to (num->slug (:to r))}
            (:note r) (assoc :note (:note r))))))
      :claims (claims a)}
      (get (:fields a) "Source") (assoc :source (get (:fields a) "Source"))
      (:accepted a) (assoc :accepted (:accepted a))
      (:validation-scope a) (assoc :validation-scope
                                   (keyword (:validation-scope a)))
      (:release-authority a) (assoc :release-authority
                                    (keyword (:release-authority a))))))

(defn -main [& _]
  (let [adrs (adr/parse-all adr-dir)
        num->slug (into {} (map (juxt :num #(slug-of (:file %))) adrs))
        sidecar (group-by :from
                          (:relations (edn/read-string
                                       (slurp "docs/adr/adr-relations.edn"))))]
    (spit "docs/adr/decisions.edn"
          (with-out-str
            (pp/pprint {:decisions
                        (mapv #(record num->slug sidecar %) adrs)})))
    (println "wrote" (count adrs) "records")))
