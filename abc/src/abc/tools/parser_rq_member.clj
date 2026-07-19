(ns abc.tools.parser-rq-member
  "Project authenticated instrument artifacts into assigned campaign members."
  (:require [abc.tools.files :as files]
            [abc.tools.json :as json]
            [abc.tools.parser-rq-capture :as capture]
            [abc.tools.parser-rq-core-attempt :as core]
            [abc.tools.parser-rq-diagnostic-completeness :as diagnostic]
            [abc.tools.parser-rq-parser-ir-conformance :as parser-ir]
            [abc.tools.parser-rq-publication :as publication]
            [abc.tools.parser-rq-resource :as resource]
            [abc.tools.parser-rq-source-accountability :as source]
            [clojure.string :as string]
            [clojure.walk :as walk]))

(defn project-core
  [{:keys [store policy candidate index blob_reader]}]
  (->> (core/authenticate-index policy candidate index
                                (partial (or blob_reader capture/read-blob) store))
       core/derive-aggregate
       (core/observation-envelopes candidate)))

(defn project-source-recognition
  [{:keys [store manifest aggregate identity]}]
  {:source_span_coverage
   (source/derive-source-recognition-envelope store manifest aggregate identity)})

(defn project-diagnostic-gap
  [{:keys [store manifest identity]}]
  {:silent_drops (source/silent-drops-envelope store manifest identity)})

(defn- blob-locators [value]
  (->> (tree-seq coll? seq value)
       (keep (fn [node]
               (when (and (map? node) (string? (:sha256 node))
                          (string? (:locator node)))
                 [(:sha256 node) (:locator node)])))
       (into {})))

(defn- authenticated-json [store blob]
  (let [result (capture/read-blob store blob)]
    (when (= :ok (:status result))
      (-> (:bytes result) (String. "UTF-8") json/read-json-str
          walk/keywordize-keys))))

(defn- raw-predicate-aggregates
  [{:keys [store qualification_identity_ref diagnostic_policy parser_ir_policy
           diagnostic_index parser_ir_index]}]
  (let [diagnostic-work-records
        (mapv (fn [{:keys [work_id exit_code raw_diagnostics]}]
                (:record
                 (diagnostic/derive-work
                  diagnostic_policy qualification_identity_ref
                  {:work_id work_id
                   :attempt_disposition (if (zero? exit_code) "parsed" "failed")
                   :bytes (:bytes (capture/read-blob store raw_diagnostics))})))
              (:records diagnostic_index))
        outer-locators (blob-locators parser_ir_index)
        record-values (keep #(authenticated-json store (:record %))
                            (:records parser_ir_index))
        locators (atom (merge outer-locators
                              (apply merge {} (map blob-locators record-values))))
        parser-store (assoc store :locators locators)
        parser-records
        (mapv #(parser-ir/authenticate-record parser-store parser_ir_policy
                                              qualification_identity_ref %)
              (:records parser_ir_index))]
    {:diagnostic_aggregate
     (diagnostic/aggregate diagnostic_policy
                           (:expected_work_ids diagnostic_index)
                           diagnostic-work-records)
     :parser_ir_aggregate
     (parser-ir/aggregate parser_ir_policy
                          (:expected_work_ids parser_ir_index)
                          parser-records)}))

(defn project-predicate-pair
  [{:keys [qualification_identity_ref diagnostic_policy diagnostic_aggregate
           parser_ir_policy parser_ir_aggregate diagnostic_index]
    :as inputs}]
  (let [{:keys [diagnostic_aggregate parser_ir_aggregate]}
        (if diagnostic_index (raw-predicate-aggregates inputs)
            {:diagnostic_aggregate diagnostic_aggregate
             :parser_ir_aggregate parser_ir_aggregate})]
    {:diagnostic_completeness
     (diagnostic/derive-observation diagnostic_policy qualification_identity_ref
                                    diagnostic_aggregate)
     :parser_ir_schema_validation
     (parser-ir/derive-observation parser_ir_policy qualification_identity_ref
                                   parser_ir_aggregate)}))

(defn project-publication
  [{:keys [store manifest index identity]}]
  {:publication_structure
   (publication/derive-publication-envelope store manifest index identity)})

(defn project-resource
  [{:keys [policy identity index records]}]
  {:peak_cgroup_memory_bytes
   (-> (resource/analyze policy identity index records)
       resource/observation-envelope)})

(def projectors
  {:core #'project-core
   :source-recognition #'project-source-recognition
   :diagnostic-gap #'project-diagnostic-gap
   :predicate-pair #'project-predicate-pair
   :publication #'project-publication
   :resource #'project-resource})

(def ^:private output-keys
  {:core #{:fatal_failures :wall_time_seconds :timeouts}
   :source-recognition #{:source_span_coverage}
   :diagnostic-gap #{:silent_drops}
   :predicate-pair #{:diagnostic_completeness :parser_ir_schema_validation}
   :publication #{:publication_structure}
   :resource #{:peak_cgroup_memory_bytes}})

(defn project-member
  [operation inputs]
  (let [projector (get projectors operation)
        expected (get output-keys operation)]
    (when-not projector
      (throw (ex-info "unknown parser RQ member projection" {:operation operation})))
    (let [member (projector inputs)
          envelopes (vals member)
          errors (mapcat capture/envelope-errors envelopes)]
      (when (or (not= expected (set (keys member)))
                (seq errors)
                (some #(not= (:qualification_identity_ref inputs)
                             (:identity_ref %))
                      envelopes))
        (throw (ex-info "parser RQ member projection violates its assigned contract"
                        {:operation operation
                         :expected-keys expected
                         :actual-keys (set (keys member))
                         :expected-identity-ref (:qualification_identity_ref inputs)
                         :envelopes (mapv #(select-keys % [:value :identity_ref :status :reason])
                                          envelopes)
                         :errors (vec errors)})))
      member)))

(defn- parse-options [args]
  (loop [remaining args options {}]
    (if (empty? remaining)
      options
      (let [[option value & tail] remaining]
        (when-not (and (string/starts-with? (or option "") "--") value)
          (throw (ex-info "member options must be --name value pairs" {:args args})))
        (recur tail (assoc options (keyword (subs option 2)) value))))))

(defn- required-option [options key]
  (or (get options key)
      (throw (ex-info (str "missing required option --" (name key)) {:option key}))))

(defn- json-value [value]
  (walk/postwalk
   (fn [item]
     (cond
       (keyword? item) (name item)
       (map? item) (into {} (map (fn [[key nested]]
                                   [(if (keyword? key) (name key) key) nested])
                                 item))
       :else item))
   value))

(defn- write-member! [path value]
  (json/write-deterministic-json-file! path (json-value value)))

(defn -main [& args]
  (try
    (let [[command & option-args] args
          operation (keyword command)
          options (parse-options option-args)
          inputs (-> (files/read-json (required-option options :inputs))
                     walk/keywordize-keys)
          member (project-member operation inputs)]
      (write-member! (required-option options :out) member)
      (println "ok"))
    (catch clojure.lang.ExceptionInfo error
      (binding [*out* *err*]
        (prn (ex-data error)))
      (throw error))))
