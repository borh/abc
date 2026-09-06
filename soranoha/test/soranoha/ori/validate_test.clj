(ns soranoha.ori.validate-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [soranoha.ori.validate :as validate]
            [soranoha.core.hash :as hash]
            [soranoha.ported.json :as json]
            [soranoha.ported.schematron :as schematron]
            [soranoha.ported.tei :as tei]))

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
