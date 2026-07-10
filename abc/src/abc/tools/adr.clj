(ns abc.tools.adr
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.time LocalDate]))

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

(defn- duplicate-number-problems [adrs]
  (for [[num duplicates] (group-by :num adrs)
        :when (and num (< 1 (count duplicates)))
        adr duplicates]
    (problem :duplicate-number (:file adr)
             "ADR number must be unique"
             :value num)))

(defn- parse-date [value]
  (when value
    (try
      (LocalDate/parse value)
      (catch Exception _ nil))))

(defn- lifecycle-problems [{:keys [file status date accepted]}]
  (let [parsed-date (parse-date date)
        parsed-accepted (parse-date accepted)]
    (concat
     (when-not (contains? statuses status)
       [(problem :invalid-status file
                 "status must be Draft, Proposed, Accepted, Superseded, or Withdrawn"
                 :value status)])
     (when-not parsed-date
       [(problem :invalid-date file
                 "Date must be a calendar-valid YYYY-MM-DD value"
                 :field "Date" :value date)])
     (when (and accepted (nil? parsed-accepted))
       [(problem :invalid-date file
                 "Accepted must be a calendar-valid YYYY-MM-DD value"
                 :field "Accepted" :value accepted)])
     (if (= "Accepted" status)
       (concat
        (when-not accepted
          [(problem :missing-accepted-date file
                    "Accepted status requires an Accepted date")])
        (when (and parsed-date parsed-accepted
                   (.isBefore parsed-accepted parsed-date))
          [(problem :accepted-before-date file
                    "Accepted date must not be before Date"
                    :value {:date date :accepted accepted})]))
       (when accepted
         [(problem :forbidden-accepted-date file
                   "only Accepted ADRs may have an Accepted date"
                   :value accepted)])))))

(def ^:private required-accepted-sections
  {"Decision" :missing-decision
   "Implementation Status" :missing-implementation-status
   "Acceptance Criteria" :missing-acceptance-criteria})

(defn- required-section-problems [{:keys [file status sections]}]
  (when (= "Accepted" status)
    (for [[heading kind] required-accepted-sections
          :when (not (contains? sections heading))]
      (problem kind file
               (str "Accepted status requires a `## " heading "` section")))))

(defn- relation-resolution-problems [adrs]
  (let [known (set (keep :num adrs))]
    (mapcat
     (fn [{:keys [file relations]}]
       (mapcat
        (fn [[relation items]]
          (concat
           (for [[item frequency] (frequencies items)
                 :when (< 1 frequency)]
             (problem :duplicate-relation file
                      "relation item must not appear more than once"
                      :relation relation :value item))
           (for [{:keys [target] :as item} items
                 :when (not (contains? known target))]
             (problem :missing-relation-target file
                      "relation target does not exist"
                      :relation relation :value item))))
        relations))
     adrs)))

(def ^:private reciprocal-relations
  {:amends {:reciprocal :amended-by :missing-kind :missing-amended-by}
   :amended-by {:reciprocal :amends :missing-kind :missing-amends}
   :supersedes {:reciprocal :superseded-by
                :missing-kind :missing-superseded-by}
   :superseded-by {:reciprocal :supersedes
                   :missing-kind :missing-supersedes}})

(defn- relation-reciprocity-problems [adrs]
  (let [by-number (into {} (map (juxt :num identity) adrs))]
    (for [{source :num file :file relations :relations} adrs
          [relation {:keys [reciprocal missing-kind]}] reciprocal-relations
          {:keys [target scope] :as item} (get relations relation)
          :let [target-adr (get by-number target)
                expected {:target source :scope scope}]
          :when (and target-adr
                     (not (some #{expected}
                                (get-in target-adr [:relations reciprocal]))))]
      (problem missing-kind file
               "relation must have a reciprocal item with matching scope"
               :relation relation :value item))))

(defn- supersession-status-problems [adrs]
  (let [by-number (into {} (map (juxt :num identity) adrs))
        incoming-unscoped
        (set (for [{:keys [relations]} adrs
                   {:keys [target scope]} (:supersedes relations)
                   :when (nil? scope)]
               target))]
    (concat
     (for [{file :file relations :relations} adrs
           {:keys [target scope] :as item} (:supersedes relations)
           :let [target-adr (get by-number target)]
           :when (and target-adr (nil? scope)
                      (not= "Superseded" (:status target-adr)))]
       (problem :unscoped-supersession-target-not-superseded file
                "an unscoped supersession target must have Superseded status"
                :value item))
     (for [{:keys [num file status]} adrs
           :when (and (= "Superseded" status)
                      (not (contains? incoming-unscoped num)))]
       (problem :superseded-without-successor file
                "a Superseded ADR requires an incoming unscoped supersession")))))

(defn- dependency-status-problems [adrs]
  (let [by-number (into {} (map (juxt :num identity) adrs))]
    (for [{source-status :status file :file relations :relations} adrs
          {:keys [target scope] :as item} (:depends-on relations)
          :let [target-status (:status (get by-number target))]
          :when (and (= "Accepted" source-status)
                     (or (and (#{"Draft" "Proposed"} target-status)
                              (nil? scope))
                         (#{"Withdrawn" "Superseded"} target-status)))]
      (if (#{"Withdrawn" "Superseded"} target-status)
        (problem :inactive-dependency file
                 "an Accepted ADR cannot depend on a Withdrawn or Superseded ADR"
                 :value item)
        (problem :unscoped-nonaccepted-dependency file
                 "an Accepted ADR dependency on Draft or Proposed requires scope"
                 :value item)))))

(defn- existing-file? [repo-root path]
  (.isFile (io/file repo-root path)))

(defn- evidence-problems [repo-root {:keys [file status evidence]}]
  (let [by-criterion (group-by :criterion-index evidence)]
    (concat
     (when (and (= "Accepted" status) (empty? evidence))
       [(problem :missing-evidence file
                 "Accepted status requires an evidence path in Acceptance Criteria")])
     (for [{:keys [path] :as item} evidence
           :when (not (.exists (io/file repo-root path)))]
       (problem :missing-evidence-path file
                "evidence path does not exist"
                :value item))
     (for [{:keys [path criterion-index] :as item} evidence
           :when (.isDirectory (io/file repo-root path))
           :let [companions (get by-criterion criterion-index)]
           :when (not-any? (fn [{companion :path}]
                             (and (or (str/starts-with? companion "test/")
                                      (str/starts-with? companion "nix/"))
                                  (existing-file? repo-root companion)))
                           companions)]
       (problem :unverified-evidence-directory file
                "an evidence directory requires an existing test/ or nix/ file in the same criterion"
                :value item)))))

(defn validate-adrs [adrs repo-root]
  (vec
   (concat
    (mapcat :parse-problems adrs)
    (duplicate-number-problems adrs)
    (mapcat lifecycle-problems adrs)
    (mapcat required-section-problems adrs)
    (relation-resolution-problems adrs)
    (relation-reciprocity-problems adrs)
    (supersession-status-problems adrs)
    (dependency-status-problems adrs)
    (mapcat #(evidence-problems repo-root %) adrs))))

(defn validate-repository
  ([repo-root] (validate-repository repo-root "docs/adr"))
  ([repo-root adr-dir]
   (validate-adrs (parse-all (str (io/file repo-root adr-dir)))
                  (io/file repo-root))))
