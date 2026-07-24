(ns abc.tools.soranoha-build-publication-test
  "Adapter/profile resolution for soranoha-build-publication: each
  parser_profile resolves its own adapter binary AND its own aat→parser-IR
  mapping pin (the mapping selects the AAT schema version, so it is part of
  the adapter tuple, never a build-wide global)."
  (:require [abc.tools.soranoha-build-publication :as build-publication]
            [clojure.test :refer [deftest is testing]]))

(defn- env-stub [m]
  (fn [k] (get m k)))

(deftest resolve-adapter-ab-aozora-profile-test
  (binding [build-publication/*env*
            (env-stub {"AB_AOZORA_BIN" "/nix/store/x/bin/ab-aozora"
                       "AB_AAT_TO_PARSER_IR_MAPPING_V2"
                       "/nix/store/y/aat-to-parser-ir-mapping-v2.json"})]
    (let [adapter (#'build-publication/resolve-adapter "ab-aozora")]
      (testing "the native stdin→AAT binary is the whole adapter"
        (is (= "ab-aozora" (:adapter-id adapter)))
        (is (= "/nix/store/x/bin/ab-aozora" (:wrapper adapter)))
        (is (= {} (:extra-env adapter))))
      (testing "the profile pins the v2 mapping (AAT schema 2)"
        (is (= "/nix/store/y/aat-to-parser-ir-mapping-v2.json"
               (:mapping adapter)))))))

(deftest resolve-adapter-ab-aozora-requires-binary-test
  (binding [build-publication/*env*
            (env-stub {"AB_AAT_TO_PARSER_IR_MAPPING_V2" "/m/v2.json"})]
    (let [ex (try
               (#'build-publication/resolve-adapter "ab-aozora")
               nil
               (catch clojure.lang.ExceptionInfo ex ex))]
      (is (some? ex))
      (is (= "AB_AOZORA_BIN" (:env_var (ex-data ex)))))))

(deftest resolve-adapter-ab-aozora-requires-v2-mapping-test
  (binding [build-publication/*env*
            (env-stub {"AB_AOZORA_BIN" "/bin/ab-aozora"
                       ;; A build-wide v1 mapping must NOT satisfy the
                       ;; ab-aozora profile: the v1 mapping rejects AAT v2.
                       "AB_AAT_TO_PARSER_IR_MAPPING" "/m/v1.json"})]
    (let [ex (try
               (#'build-publication/resolve-adapter "ab-aozora")
               nil
               (catch clojure.lang.ExceptionInfo ex ex))]
      (is (some? ex))
      (is (= "AB_AAT_TO_PARSER_IR_MAPPING_V2" (:env_var (ex-data ex)))))))

(deftest resolve-adapter-aozora2html-pins-v1-mapping-test
  (binding [build-publication/*env*
            (env-stub {"AB_AOZORA2HTML_ADAPTER" "/a/wrapper"
                       "AB_AOZORA2HTML_BIN" "/a/aozora2html"
                       "AB_AOZORA2HTML_MAPPER_BIN" "/a/mapper"
                       "AB_AAT_TO_PARSER_IR_MAPPING" "/m/v1.json"})]
    (let [adapter (#'build-publication/resolve-adapter "aozora2html")]
      (is (= "aozora2html" (:adapter-id adapter)))
      (is (= "/a/wrapper" (:wrapper adapter)))
      (is (= {"AB_AOZORA2HTML_BIN" "/a/aozora2html"
              "AB_AOZORA2HTML_MAPPER_BIN" "/a/mapper"}
             (:extra-env adapter)))
      (is (= "/m/v1.json" (:mapping adapter))))))

(deftest resolve-adapter-unknown-profile-lists-both-supported-test
  (binding [build-publication/*env* (env-stub {})]
    (let [ex (try
               (#'build-publication/resolve-adapter "mystery-parser")
               nil
               (catch clojure.lang.ExceptionInfo ex ex))]
      (is (some? ex))
      (is (= "mystery-parser" (:parser_profile (ex-data ex))))
      (is (= ["aozora2html" "ab-aozora"] (:supported (ex-data ex)))))))

(deftest custom-parser-config-selects-ab-aozora-profile-test
  (let [config (#'build-publication/read-config
                "config/full-corpus-publication-custom-parser-ja.json")]
    (is (= "ab-aozora" (get config "parser_profile")))
    (is (= "full-corpus" (get config "materialization_scope")))
    (is (true? (get config "continue_on_failure")))))
