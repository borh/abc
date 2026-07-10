(ns abc.tools.adr
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def statuses #{"Draft" "Proposed" "Accepted" "Superseded" "Withdrawn"})
(def header-fields
  #{"Status" "Date" "Accepted" "Supersedes" "Superseded by"
    "Amends" "Amended by" "Depends on" "Source"})
(def relation-fields
  {"Supersedes" :supersedes
   "Superseded by" :superseded-by
   "Amends" :amends
   "Amended by" :amended-by
   "Depends on" :depends-on})
(def evidence-prefixes ["test/" "fixtures/" "nix/"])

(defn problem [kind file message & {:as data}]
  (merge {:kind kind :file file :message message} data))

(defn adr-files [dir]
  (->> (.listFiles (io/file dir))
       (filter #(.isFile %))
       (map #(.getName %))
       (filter #(re-matches #"\d{4}-.*\.md" %))
       sort
       vec))

(defn- parse-title [file line]
  (if-let [[_ digits title] (re-matches #"# ADR (\d{4}): (.+)" (or line ""))]
    {:num (Integer/parseInt digits) :title title :problems []}
    {:num nil :title nil
     :problems [(problem :invalid-title file
                         "title must be `# ADR NNNN: Title`")]}))

(defn- relation-item [file field value]
  (if-let [[_ digits scope]
           (re-matches #"ADR (\d{4})(?: \[scope: ([^\]]+)\])?" value)]
    {:value {:target (Integer/parseInt digits) :scope scope} :problems []}
    {:value nil
     :problems [(problem :invalid-relation-item file
                         "relation item must be `ADR NNNN` with optional `[scope: …]`"
                         :field field :value value)]}))

(defn- criterion-bodies [body]
  (->> (str/split-lines (or body ""))
       (reduce (fn [items line]
                 (cond
                   (str/starts-with? line "- ")
                   (conj items (subs line 2))

                   (and (seq items) (re-matches #"\s+.*" line))
                   (update items (dec (count items)) str "\n" (str/trim line))

                   :else items))
               [])))

(defn- evidence [section-bodies]
  (vec
   (for [[idx criterion]
         (map-indexed vector
                      (criterion-bodies
                       (get section-bodies "Acceptance Criteria")))
         token (map second (re-seq #"`([^`]+)`" criterion))
         :when (some #(str/starts-with? token %) evidence-prefixes)]
     {:path token
      :section "Acceptance Criteria"
      :criterion-index idx})))

(defn- header-lines [file lines]
  (let [separator (second lines)
        has-required-blank? (and (some? separator) (str/blank? separator))]
    {:lines (loop [remaining (drop (if has-required-blank? 2 1) lines)
                   result []]
              (let [line (first remaining)]
                (cond
                  (nil? line) result
                  (and (seq result) (str/blank? line)) result
                  :else (recur (rest remaining) (conj result line)))))
     :problems (if has-required-blank?
                 []
                 [(problem :missing-required-blank-line file
                           "title must be followed by a blank line")])}))

(defn- parse-fields [file lines]
  (->
   (reduce
    (fn [{:keys [seen] :as parsed} line]
      (if-let [[_ field value] (re-matches #"([^:]+):(.*)" line)]
        (cond
          (not (contains? header-fields field))
          (update parsed :problems conj
                  (problem :unknown-header-field file
                           "header field is not recognized"
                           :field field :value (str/trim value)))

          (contains? seen field)
          (update parsed :problems conj
                  (problem :duplicate-header-field file
                           "header field appears more than once"
                           :field field :value (str/trim value)))

          (str/blank? value)
          (-> parsed
              (update :seen conj field)
              (update :problems conj
                      (problem :empty-header-value file
                               "header field value must not be empty"
                               :field field :value (str/trim value))))

          :else
          (-> parsed
              (update :seen conj field)
              (assoc-in [:fields field] (str/trim value))))
        (update parsed :problems conj
                (problem :invalid-header-line file
                         "header line must be `Field: non-empty value`"
                         :value line))))
    {:fields {} :seen #{} :problems []}
    lines)
   (dissoc :seen)))

(defn- parse-relations [file fields]
  (reduce-kv
   (fn [parsed field relation-key]
     (if-let [value (get fields field)]
       (if (and (= field "Supersedes") (= value "none"))
         (assoc-in parsed [:relations relation-key] [])
         (let [items (map #(relation-item file field %)
                          (str/split value #",\s*"))]
           (-> parsed
               (assoc-in [:relations relation-key]
                         (vec (keep :value items)))
               (update :problems into (mapcat :problems items)))))
       parsed))
   {:relations {} :problems []}
   relation-fields))

(defn- parse-sections [lines]
  (let [{:keys [section sections section-bodies body]}
        (reduce
         (fn [{:keys [section body] :as parsed} line]
           (if-let [[_ heading] (re-matches #"## (.+)" line)]
             (cond-> (assoc parsed :section heading :body [])
               section (assoc-in [:section-bodies section]
                                 (str/join "\n" body))
               true (update :sections conj heading))
             (cond-> parsed
               section (update :body conj line))))
         {:section nil :sections #{} :section-bodies {} :body []}
         lines)]
    {:sections sections
     :section-bodies (cond-> section-bodies
                       section (assoc section (str/join "\n" body)))}))

(defn- filename-number [filename]
  (some-> (re-find #"^(\d{4})" filename) second Integer/parseInt))

(defn parse-adr [dir filename]
  (let [lines (str/split-lines (slurp (io/file dir filename)))
        title-result (parse-title filename (first lines))
        header-result (header-lines filename lines)
        fields-result (parse-fields filename (:lines header-result))
        fields (:fields fields-result)
        relations-result (parse-relations filename fields)
        sections-result (parse-sections lines)
        num (:num title-result)
        filename-num (filename-number filename)
        mismatch-problems (if (and filename-num num (not= filename-num num))
                            [(problem :filename-title-mismatch filename
                                      "filename and title ADR numbers must match"
                                      :value {:filename filename-num :title num})]
                            [])]
    {:num num
     :file filename
     :title (:title title-result)
     :fields fields
     :status (get fields "Status")
     :date (get fields "Date")
     :accepted (get fields "Accepted")
     :relations (:relations relations-result)
     :sections (:sections sections-result)
     :section-bodies (:section-bodies sections-result)
     :evidence (evidence (:section-bodies sections-result))
     :parse-problems (vec (concat (:problems title-result)
                                  (:problems header-result)
                                  (:problems fields-result)
                                  (:problems relations-result)
                                  mismatch-problems))}))

(defn parse-all [dir]
  (mapv #(parse-adr dir %) (adr-files dir)))
