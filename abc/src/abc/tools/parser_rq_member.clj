(ns abc.tools.parser-rq-member
  "Project authenticated instrument artifacts into assigned campaign members."
  (:require [abc.tools.files :as files]
            [abc.tools.jcs :as jcs]
            [abc.tools.parser-rq-capture :as capture]
            [abc.tools.parser-rq-diagnostic-completeness :as diagnostic]
            [abc.tools.parser-rq-parser-ir-conformance :as parser-ir]
            [abc.tools.parser-rq-publication :as publication]
            [abc.tools.parser-rq-resource :as resource]
            [abc.tools.parser-rq-source-accountability :as source]
            [clojure.string :as string]
            [clojure.walk :as walk])
  (:import [java.nio.file CopyOption Files StandardCopyOption]
           [java.nio.file.attribute FileAttribute]))

(defn project-source-recognition
  [{:keys [store manifest aggregate identity]}]
  {:source_span_coverage
   (source/derive-source-recognition-envelope store manifest aggregate identity)})

(defn project-diagnostic-gap
  [{:keys [store manifest identity]}]
  {:silent_drops (source/silent-drops-envelope store manifest identity)})

(defn project-predicate-pair
  [{:keys [qualification_identity_ref diagnostic_policy diagnostic_aggregate
           parser_ir_policy parser_ir_aggregate]}]
  {:diagnostic_completeness
   (diagnostic/derive-observation diagnostic_policy qualification_identity_ref
                                  diagnostic_aggregate)
   :parser_ir_schema_validation
   (parser-ir/derive-observation parser_ir_policy qualification_identity_ref
                                 parser_ir_aggregate)})

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
  {:source-recognition #'project-source-recognition
   :diagnostic-gap #'project-diagnostic-gap
   :predicate-pair #'project-predicate-pair
   :publication #'project-publication
   :resource #'project-resource})

(def ^:private output-keys
  {:source-recognition #{:source_span_coverage}
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
                        {:operation operation :errors (vec errors)})))
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
  (files/create-parent-dirs! path)
  (let [target (.toPath (java.io.File. (str path)))
        directory (.getParent target)
        temporary (Files/createTempFile directory ".parser-rq-member-" ".tmp"
                                        (make-array FileAttribute 0))]
    (try
      (Files/write temporary (jcs/canonical-json-bytes (json-value value))
                   (make-array java.nio.file.OpenOption 0))
      (Files/move temporary target
                  (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE
                                          StandardCopyOption/REPLACE_EXISTING]))
      (finally
        (Files/deleteIfExists temporary)))))

(defn -main [& args]
  (let [[command & option-args] args
        operation (keyword command)
        options (parse-options option-args)
        inputs (-> (files/read-json (required-option options :inputs))
                   walk/keywordize-keys)
        member (project-member operation inputs)]
    (write-member! (required-option options :out) member)
    (println "ok")))
