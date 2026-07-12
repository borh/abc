(ns abc.tools.adr
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.nio.file InvalidPathException Paths]
           [java.time LocalDate]))

(def statuses #{"Draft" "Proposed" "Accepted" "Superseded" "Withdrawn"})
(def validation-scopes #{"structural" "fixture" "smoke-corpus"
                         "full-corpus" "operational"})
(def release-authorities #{"none" "development" "publication"})
(def claim-kinds
  #{:structural-invariant :fixture-behavior :corpus-behavior
    :performance-bound :external-semantics :implementation-agreement
    :domain-interpretation :operational-behavior})
(def header-fields
  #{"Status" "Date" "Accepted" "Supersedes" "Superseded by"
    "Amends" "Amended by" "Depends on" "Source"
    "Validation scope" "Release authority"})
(def relation-fields
  {"Supersedes" :supersedes
   "Superseded by" :superseded-by
   "Amends" :amends
   "Amended by" :amended-by
   "Depends on" :depends-on})
(def evidence-prefixes ["test/" "fixtures/" "nix/"])
(def ^:private evidence-roots #{"test" "fixtures" "nix"})

(defn problem [kind file message & {:as data}]
  (merge {:kind kind :file file :message message} data))

(defn adr-files [dir]
  (->> (or (.listFiles (io/file dir)) [])
       (filter #(.isFile %))
       (map #(.getName %))
       (filter #(re-matches #"\d{4}-.+\.md" %))
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

(def ^:private claim-header-pattern
  #"^\*\*(ADR-([0-9]{4})-C([1-9][0-9]*) — ([a-z]+(?:-[a-z]+)*)):\*\*(?:\s|$)")

(defn- criterion-claim [file num status criterion-index body]
  (if-let [[_ _ adr-digits criterion-digits kind-token]
           (re-find claim-header-pattern body)]
    (let [claim-id (str "ADR-" adr-digits "-C" criterion-digits)
          claim-kind (keyword kind-token)
          base {:criterion-index criterion-index
                :body body
                :claim-id claim-id
                :claim-kind claim-kind}]
      {:criterion base
       :problems
       (vec
        (concat
         (when (and num (not= num (Integer/parseInt adr-digits)))
           [(problem :claim-adr-mismatch file
                     "claim ID ADR number must match its containing ADR"
                     :criterion-index criterion-index
                     :claim-id claim-id)])
         (when-not (contains? claim-kinds claim-kind)
           [(problem :unknown-claim-kind file
                     "claim kind is not recognized"
                     :criterion-index criterion-index
                     :claim-id claim-id
                     :value claim-kind)])))})
    {:criterion {:criterion-index criterion-index
                 :body body
                 :claim-id nil
                 :claim-kind nil}
     :problems
     (cond
       (str/starts-with? body "**ADR-")
       [(problem :malformed-claim-header file
                 "claim header must use the exact ADR-NNNN-CN and claim-kind syntax"
                 :criterion-index criterion-index)]

       (= "Accepted" status)
       [(problem :missing-claim-header file
                 "Accepted criterion requires an exact claim header"
                 :criterion-index criterion-index)]

       :else [])}))

(defn- parse-criteria [file num status section-bodies]
  (let [parsed (mapv (fn [criterion-index body]
                       (criterion-claim file num status criterion-index body))
                     (range)
                     (criterion-bodies
                      (get section-bodies "Acceptance Criteria")))]
    {:criteria (mapv :criterion parsed)
     :problems (vec (mapcat :problems parsed))}))

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
      (if-let [[_ field raw-value] (re-matches #"([^:]+):(.*)" line)]
        (cond
          (not (contains? header-fields field))
          (update parsed :problems conj
                  (problem :unknown-header-field file
                           "header field is not recognized"
                           :field field :value raw-value))

          (contains? seen field)
          (update parsed :problems conj
                  (problem :duplicate-header-field file
                           "header field appears more than once"
                           :field field :value raw-value))

          (str/blank? raw-value)
          (-> parsed
              (update :seen conj field)
              (update :problems conj
                      (problem :empty-header-value file
                               "header field value must not be empty"
                               :field field :value raw-value)))

          (not (re-matches #" [^\s].*\S| [^\s]" raw-value))
          (-> parsed
              (update :seen conj field)
              (update :problems conj
                      (problem :invalid-header-line file
                               "header line must be exactly `Field: value`"
                               :field field :value line)))

          :else
          (-> parsed
              (update :seen conj field)
              (assoc-in [:fields field] (subs raw-value 1))))
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
        criteria-result (parse-criteria filename num (get fields "Status")
                                        (:section-bodies sections-result))
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
     :validation-scope (get fields "Validation scope")
     :release-authority (get fields "Release authority")
     :relations (:relations relations-result)
     :sections (:sections sections-result)
     :section-bodies (:section-bodies sections-result)
     :criteria (:criteria criteria-result)
     :claim-problems (:problems criteria-result)
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

(defn- duplicate-claim-id-problems [adrs]
  (let [claims (for [{:keys [file criteria]} adrs
                     {:keys [claim-id] :as criterion} criteria
                     :when claim-id]
                 (assoc criterion :file file))]
    (for [[claim-id duplicates] (sort-by key (group-by :claim-id claims))
          :when (< 1 (count duplicates))
          {:keys [file criterion-index]} duplicates]
      (problem :duplicate-claim-id file
               "claim ID must be unique across the repository"
               :criterion-index criterion-index
               :claim-id claim-id))))

(defn- parse-date [value]
  (when value
    (try
      (LocalDate/parse value)
      (catch Exception _ nil))))

(defn- lifecycle-problems
  [{:keys [file status date accepted validation-scope release-authority]}]
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
     (when (and validation-scope
                (not (contains? validation-scopes validation-scope)))
       [(problem :invalid-validation-scope file
                 "Validation scope must be structural, fixture, smoke-corpus, full-corpus, or operational"
                 :field "Validation scope" :value validation-scope)])
     (when (and release-authority
                (not (contains? release-authorities release-authority)))
       [(problem :invalid-release-authority file
                 "Release authority must be none, development, or publication"
                 :field "Release authority" :value release-authority)])
     (if (= "Accepted" status)
       (concat
        (when-not accepted
          [(problem :missing-accepted-date file
                    "Accepted status requires an Accepted date")])
        (when (and parsed-date parsed-accepted
                   (.isBefore parsed-accepted parsed-date))
          [(problem :accepted-before-date file
                    "Accepted date must not be before Date"
                    :value {:date date :accepted accepted})])
        (when-not validation-scope
          [(problem :missing-validation-scope file
                    "Accepted status requires a Validation scope")])
        (when-not release-authority
          [(problem :missing-release-authority file
                    "Accepted status requires a Release authority")]))
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

(defn- dependency-paths-from [by-number source]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [source])
         visited #{source}
         paths []]
    (if (empty? queue)
      paths
      (let [path (peek queue)
            current (get by-number (peek path))
            targets (->> (get-in current [:relations :depends-on])
                         (map :target)
                         distinct
                         sort)
            unseen (remove visited targets)
            next-paths (mapv #(conj path %) unseen)]
        (recur (into (pop queue) next-paths)
               (into visited unseen)
               (into paths next-paths))))))

(defn- dependency-status-problems [adrs]
  (let [by-number (into {} (map (juxt :num identity) adrs))]
    (for [{:keys [num file status]} adrs
          :when (= "Accepted" status)
          path (dependency-paths-from by-number num)
          :let [target (get by-number (peek path))]
          :when (and target (not= "Accepted" (:status target)))]
      (problem :noncanonical-dependency-path file
               "Accepted ADR dependency closure contains a non-Accepted ADR"
               :path path
               :target-status (:status target)))))

(defn- legacy-dependency-status-problems [adrs]
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

(defn- raw-path [path]
  (Paths/get path (make-array String 0)))

(defn- traversal? [path]
  (some #(= ".." (str %)) (iterator-seq (.iterator path))))

(defn- evidence-path-state [repo-root path]
  (try
    (let [relative (raw-path path)
          root (-> (io/file repo-root) .toPath .toAbsolutePath .normalize)
          resolved (-> root (.resolve relative) .normalize)
          allowed-root-name (when (pos? (.getNameCount relative))
                              (str (.getName relative 0)))]
      (cond
        (.isAbsolute relative) {:problem :evidence-path-traversal}
        (traversal? relative) {:problem :evidence-path-traversal}
        (not (.startsWith resolved root)) {:problem :evidence-path-traversal}
        (not (contains? evidence-roots allowed-root-name))
        {:problem :evidence-path-traversal}
        (not (.exists (.toFile resolved))) {:problem :missing-evidence-path}
        :else
        (let [real-repo-root (.toRealPath root (make-array java.nio.file.LinkOption 0))
              allowed-root (-> root (.resolve allowed-root-name) .normalize)
              real-allowed-root (.toRealPath allowed-root (make-array java.nio.file.LinkOption 0))
              real-path (.toRealPath resolved (make-array java.nio.file.LinkOption 0))]
          (if (and (.startsWith real-allowed-root real-repo-root)
                   (.startsWith real-path real-allowed-root))
            {:path (.toFile resolved)}
            {:problem :evidence-real-path-escape}))))
    (catch InvalidPathException _
      {:problem :malformed-evidence-path})))

(defn- evidence-path-problem [file item state]
  (when-let [kind (:problem state)]
    (problem kind file
             (case kind
               :evidence-path-traversal "evidence path contains lexical traversal"
               :evidence-real-path-escape "evidence real path escapes the repository"
               :malformed-evidence-path "evidence path is malformed"
               :missing-evidence-path "evidence path does not exist")
             :value item)))

(defn- evidence-problems [repo-root {:keys [file status evidence]}]
  (let [by-criterion (group-by :criterion-index evidence)
        states (into {} (map (fn [{:keys [path]}]
                               [path (evidence-path-state repo-root path)])
                             evidence))]
    (concat
     (when (and (= "Accepted" status) (empty? evidence))
       [(problem :missing-evidence file
                 "Accepted status requires an evidence path in Acceptance Criteria")])
     (keep (fn [{:keys [path] :as item}]
             (evidence-path-problem file item (get states path)))
           evidence)
     (for [{:keys [path criterion-index] :as item} evidence
           :let [state (get states path)]
           :when (and (nil? (:problem state)) (.isDirectory (:path state)))
           :let [companions (get by-criterion criterion-index)]
           :when (not-any? (fn [{companion :path}]
                             (and (or (str/starts-with? companion "test/")
                                      (str/starts-with? companion "nix/"))
                                  (let [companion-state (get states companion)]
                                    (and (nil? (:problem companion-state))
                                         (.isFile (:path companion-state))))))
                           companions)]
       (problem :unverified-evidence-directory file
                "an evidence directory requires an existing test/ or nix/ file in the same criterion"
                :value item)))))

(defn validate-adrs [adrs repo-root]
  (vec
   (concat
    (mapcat :parse-problems adrs)
    (mapcat :claim-problems adrs)
    (duplicate-number-problems adrs)
    (duplicate-claim-id-problems adrs)
    (mapcat lifecycle-problems adrs)
    (mapcat required-section-problems adrs)
    (relation-resolution-problems adrs)
    (relation-reciprocity-problems adrs)
    (supersession-status-problems adrs)
    (dependency-status-problems adrs)
    (mapcat #(evidence-problems repo-root %) adrs))))

(def ^:private audit-only-problem-kinds
  #{:missing-validation-scope
    :missing-release-authority
    :invalid-validation-scope
    :invalid-release-authority
    :noncanonical-dependency-path
    :missing-claim-header
    :malformed-claim-header
    :claim-adr-mismatch
    :duplicate-claim-id
    :unknown-claim-kind})

(defn validate-adrs-legacy
  "Validate with the pre-migration lifecycle/dependency policy. Parsing,
  evidence containment, relation integrity, and prior dependency safety remain
  enforced; audit-only lifecycle dimensions and closure do not block callers."
  [adrs repo-root]
  (vec
   (concat
    (remove #(contains? audit-only-problem-kinds (:kind %))
            (validate-adrs adrs repo-root))
    (legacy-dependency-status-problems adrs))))

(defn- validate-repository* [validate-fn repo-root adr-dir]
  (let [directory (io/file repo-root adr-dir)]
    (cond
      (not (.exists directory))
      [(problem :missing-adr-directory adr-dir "ADR directory does not exist")]

      (not (.isDirectory directory))
      [(problem :invalid-adr-directory adr-dir "ADR path is not a directory")]

      :else
      (let [markdown-files (->> (or (.listFiles directory) [])
                                (filter #(.isFile %))
                                (map #(.getName %))
                                (filter #(and (str/ends-with? % ".md")
                                              (not= "README.md" %)))
                                sort
                                vec)
            malformed (remove #(re-matches #"\d{4}-.+\.md" %) markdown-files)
            adrs (parse-all directory)]
        (vec
         (concat
          (when (empty? markdown-files)
            [(problem :empty-adr-corpus adr-dir "ADR directory contains no ADR Markdown files")])
          (for [filename malformed]
            (problem :invalid-adr-filename filename
                     "ADR filename must be `NNNN-title.md`"))
          (validate-fn adrs (io/file repo-root))))))))

(defn validate-repository
  ([repo-root] (validate-repository repo-root "docs/adr"))
  ([repo-root adr-dir]
   (validate-repository* validate-adrs repo-root adr-dir)))

(defn validate-repository-legacy
  ([repo-root] (validate-repository-legacy repo-root "docs/adr"))
  ([repo-root adr-dir]
   (validate-repository* validate-adrs-legacy repo-root adr-dir)))
