(ns soranoha.ori.validate-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [clojure.string :as string]
            [soranoha.ori.validate :as validate]
            [soranoha.core.hash :as hash]
            [soranoha.core.json :as json]
            [soranoha.ori.schematron :as schematron]
            [soranoha.ori.relaxng :as tei]))

(deftest generation-provenance-is-optional-and-bound-to-profile-bytes
  (let [dir (fs/create-temp-dir {:prefix "profile-generation"})
        profile (validate/profile-paths (str dir))
        tei-file (fs/path dir "tei.xml")
        build-hash (str "sha256:" (apply str (repeat 64 "a")))]
    (try
      (fs/create-dirs (fs/path dir "schemas"))
      (doseq [key [:odd :rng :sch]] (spit (get profile key) (name key)))
      (spit (str tei-file) "<TEI/>")
      (with-redefs [tei/validate! (fn [_] {:violations []})
                    schematron/validate! (fn [_] {:findings []})]
        (let [run #(validate/tei-validation-result profile (str tei-file))
              absent (get (run) "toolchain")
              metadata (merge (select-keys absent ["odd_hash" "rng_hash" "schematron_hash"])
                              {"generator" "fixture profile generator"
                               "generator_build_hash" build-hash})
              write! #(spit (:generation profile) (json/write-deterministic-json-str %))]
          (testing "validation identifies its current implementation"
            (is (= "soranoha.ori.schematron"
                   (get-in (run) ["layers" "schematron" "validator"]))))
          (testing "local profile bytes do not imply a known generation process"
            (is (not (contains? absent "generator")))
            (is (not (contains? absent "generator_build_hash"))))
          (write! metadata)
          (testing "recorded generation is included only for the matching profile"
            (let [toolchain (get (run) "toolchain")]
              (is (= "fixture profile generator" (get toolchain "generator")))
              (is (= build-hash (get toolchain "generator_build_hash")))
              (is (= (str "sha256:" (hash/sha256-file (:odd profile)))
                     (get toolchain "odd_hash")))))
          (doseq [changed [(assoc metadata "generator_build_hash" "unknown")
                           (assoc metadata "rng_hash" build-hash)
                           (assoc metadata "unused" true)]]
            (write! changed)
            (is (thrown-with-msg? clojure.lang.ExceptionInfo #"does not match"
                                  (run))))
          (write! metadata)
          (spit (:sch profile) "changed profile")
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"does not match" (run)))))
      (finally (fs/delete-tree dir)))))

(deftest validation-layer-refuses-another-domains-status
  (doseq [status ["passed" :passed :assessment/available :aozora/available]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Invalid TEI validation status"
                          (#'validate/validation-layer status "fixture" "fixture")))))

(defn source-reference-document [n]
  (str "<TEI xmlns='http://www.tei-c.org/ns/1.0'><teiHeader><fileDesc><titleStmt><title>試験</title></titleStmt><publicationStmt><p>試験</p></publicationStmt><sourceDesc><p><idno type='aozora-work-id'>1</idno></p></sourceDesc></fileDesc><profileDesc><langUsage><language ident='ja'>Japanese</language></langUsage></profileDesc></teiHeader><text><body><p>"
       (apply str (map #(str "<seg source='#source-" % "'>本文</seg>") (range n)))
       "</p></body><back><div>"
       (apply str (map #(str "<note xml:id='source-" % "' type='source-span'>{}</note>") (range n)))
       "</div></back></text></TEI>"))

(deftest source-references-resolve-xml-ids-through-the-runtime-validator
  (let [dir (fs/create-temp-dir {:prefix "source-references"})
        file (str (fs/path dir "tei.xml"))
        source (source-reference-document 4096)
        profile (validate/profile-paths ".")
        check (fn [text]
                (spit file text)
                (validate/tei-validation-result profile file))]
    (try
      (is (= "passed" (get (check source) "status")))
      (is (= "passed" (get (check (string/replace-first source "source='#source-0'"
                                                        "source='#source-0 #source-4095'")) "status")))
      (doseq [text [(string/replace source "xml:id='source-4095'" "n='source-4095'")
                    (string/replace-first source "source='#source-0'" "source='#source-0 #missing'")]]
        (let [result (check text)]
          (is (= "failed" (get result "status")))
          (is (some #(= "snh-source-span-target-exists" (get % "rule_id")) (get result "findings")))))
      (finally (fs/delete-tree dir)))))
