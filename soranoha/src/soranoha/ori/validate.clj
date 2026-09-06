(ns soranoha.ori.validate
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [soranoha.core.hash :as hash]
            [soranoha.core.json :as json]
            [soranoha.ori.schematron :as schematron]
            [soranoha.ori.relaxng :as tei]))

(def logical-odd-path "schemas/tei-profile.odd")
(def logical-rng-path "schemas/tei-profile.rng")
(def logical-schematron-path "schemas/tei-profile.sch")

(defn profile-paths
  "The TEI profile trio under an explicit assets root."
  [assets-root]
  {:odd (str assets-root "/" logical-odd-path)
   :rng (str assets-root "/" logical-rng-path)
   :sch (str assets-root "/" logical-schematron-path)
   :generation (str assets-root "/schemas/tei-profile-generation.json")})

(defn- file-hash [path]
  (str "sha256:" (hash/sha256-file path)))

(defn- generation-provenance [generation hashes]
  (when (and generation (.exists (io/file generation)))
    (let [record (json/read-json-file generation)]
      (when-not (and (= (set (keys record))
                        #{"generator" "generator_build_hash" "odd_hash" "rng_hash" "schematron_hash"})
                     (string? (get record "generator"))
                     (not (string/blank? (get record "generator")))
                     (string? (get record "generator_build_hash"))
                     (re-matches #"sha256:[0-9a-f]{64}" (get record "generator_build_hash"))
                     (= hashes (select-keys record (keys hashes))))
        (throw (ex-info "TEI profile generation provenance does not match its artifacts"
                        {:reason :profile-generation-mismatch})))
      (select-keys record ["generator" "generator_build_hash"]))))

(defn- validation-layer [status validator message]
  {"status" status
   "validator" validator
   "message" message})

(defn- finding-severity [severity]
  (case severity
    :fatal "error"
    :error "error"
    :warning "warning"
    :info "info"
    (name severity)))

(defn- rng-validation [rng-path tei-file]
  (let [{:keys [violations]} (tei/validate! {:schema-path rng-path
                                             :xml-path (str tei-file)
                                             :label (str tei-file)})
        failures (filter #(#{:error :fatal} (:severity %)) violations)]
    (if (seq failures)
      {:status "failed"
       :layer (validation-layer "failed" "jing"
                                (string/join "\n" (map :message failures)))
       :findings (mapv (fn [violation]
                         {"rule_id" "relax-ng"
                          "severity" (finding-severity (:severity violation))
                          "layer" "relax_ng"
                          "message" (:message violation)
                          "location" (when-let [line (:line violation)]
                                       (str line))
                          "allowed" false})
                       failures)}
      {:status "passed"
       :layer (validation-layer "passed" "jing"
                                "Validated against ODD-derived project Relax NG target.")
       :findings []})))

(defn- schematron-validation [sch-path tei-file]
  (let [{:keys [findings]} (schematron/validate! {:schema-path sch-path
                                                  :xml-path (str tei-file)
                                                  :label (str tei-file)})
        errors (filter #(= :error (:severity %)) findings)
        warnings (filter #(= :warning (:severity %)) findings)
        status (cond
                 (seq errors) "failed"
                 (seq warnings) "warning"
                 :else "passed")]
    {:status status
     :layer (validation-layer status "soranoha.ported.schematron"
                              (case status
                                "passed" "No project Schematron findings."
                                "warning" "Only project Schematron warnings were reported."
                                "failed" "Project Schematron errors were reported."))
     :findings (mapv (fn [finding]
                       {"rule_id" (:rule-id finding)
                        "severity" (name (:severity finding))
                        "layer" "schematron"
                        "message" (:message finding)
                        "location" (:location finding)
                        "allowed" (not= :error (:severity finding))})
                     findings)}))

(defn tei-validation-result
  "Validate a TEI file against the profile trio. Include-and-flag: the
  result records failure, it never excludes a work."
  [{:keys [odd rng sch generation]} tei-file]
  (let [tei-profile-hash (file-hash odd)
        hashes {"odd_hash" tei-profile-hash
                "rng_hash" (file-hash rng)
                "schematron_hash" (file-hash sch)}
        provenance (generation-provenance generation hashes)
        rng-result (rng-validation rng tei-file)
        schematron-result (schematron-validation sch tei-file)
        status (cond
                 (some #{"failed"} [(:status rng-result)
                                    (:status schematron-result)])
                 "failed"

                 (some #{"warning"} [(:status rng-result)
                                     (:status schematron-result)])
                 "warning"

                 :else "passed")]
    {"validated_artifact" (file-hash tei-file)
     "tei_profile_hash" tei-profile-hash
     "status" status
     "layers" {"well_formed_xml" (validation-layer
                                  "passed" "clojure.data.xml"
                                  "Generated by clojure.data.xml.")
               "relax_ng" (:layer rng-result)
               "schematron" (:layer schematron-result)}
     "toolchain" (merge {"odd_path" logical-odd-path
                         "rng_path" logical-rng-path
                         "schematron_path" logical-schematron-path}
                        hashes provenance)
     "findings" (vec (concat (:findings rng-result)
                             (:findings schematron-result)))}))
