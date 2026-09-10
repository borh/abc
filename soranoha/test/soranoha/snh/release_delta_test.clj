(ns soranoha.snh.release-delta-test
  "The release-to-release reading of two manifests. The properties under test
  are the ones a served diff would otherwise get wrong: that a re-conversion
  is never reported as a new edition, that a source change is not attributed
  to the toolchain, and that documents moving with nothing else moving is
  surfaced rather than absorbed."
  (:require [clojure.test :refer [deftest is testing]]
            [soranoha.snh.release-delta :as release-delta]))

(def ^:private base-artifacts
  ["snh:1:markdown:aa" "snh:1:plaintext:bb" "snh:1:tei:cc" "snh:1:tei-validation:dd"])

(defn- work
  [slug source & {:keys [artifacts layers]}]
  {"slug" slug
   "source_content_hash" source
   "artifacts" (or artifacts base-artifacts)
   "layers" (or layers [])})

(def ^:private one-stage
  {"convert" {"nix_closure_hash" "closure-1" "stage_code_version" "1.0.0"}})

(def ^:private one-catalog (str "snh:1:catalog:" (apply str (repeat 64 "a"))))

(def ^:private one-rights
  {"works" "public-domain" "encoding" "cc0" "statement_url" "https://example.org/rights"})

(defn- manifest
  [rev works & {:keys [toolchain withdrawn catalog rights]}]
  {"corpus" {"upstream_rev" rev}
   "toolchain" (or toolchain one-stage)
   "catalog" (or catalog one-catalog)
   "rights" (or rights one-rights)
   "works" works
   "withdrawn" (or withdrawn [])})

(deftest a-reconversion-is-documents-changed-and-never-a-source-change
  (let [earlier (manifest "rev-a" [(work "000001_1" "sha256:src")])
        later (manifest "rev-b" [(work "000001_1" "sha256:src"
                                       :artifacts ["snh:1:markdown:zz" "snh:1:plaintext:bb"
                                                   "snh:1:tei:cc" "snh:1:tei-validation:dd"])]
                        :toolchain {"convert" {"nix_closure_hash" "closure-2"
                                               "stage_code_version" "1.1.0"}})
        {:keys [works unexplained toolchain]} (release-delta/delta earlier later)]
    (is (= ["000001_1"] (:documents-changed works)))
    (is (= [] (:source-changed works)))
    (is (= 0 (:unchanged works)))
    (testing "the stage that produced the new documents is named alongside"
      (is (= #{"convert"} (set (keys toolchain))))
      (is (= "closure-1" (get-in toolchain ["convert" :from "nix_closure_hash"])))
      (is (= "closure-2" (get-in toolchain ["convert" :to "nix_closure_hash"]))))
    (testing "a moved stage coordinate explains it, so nothing is unexplained"
      (is (= [] unexplained)))))

(deftest a-source-change-is-not-also-reported-as-a-document-change
  (let [earlier (manifest "rev-a" [(work "000001_1" "sha256:old")])
        later (manifest "rev-b" [(work "000001_1" "sha256:new"
                                       :artifacts ["snh:1:markdown:zz" "snh:1:plaintext:yy"
                                                   "snh:1:tei:xx" "snh:1:tei-validation:ww"])])
        {:keys [works]} (release-delta/delta earlier later)]
    (is (= ["000001_1"] (:source-changed works)))
    (is (= [] (:documents-changed works))
        "the artifacts moved too, but the source hash already establishes the cause")))

(def ^:private moved-documents
  ["snh:1:markdown:zz" "snh:1:plaintext:bb" "snh:1:tei:cc" "snh:1:tei-validation:dd"])

(deftest documents-moving-with-no-declared-input-moving-is-surfaced
  (let [earlier (manifest "rev-a" [(work "000001_1" "sha256:src")])
        later (manifest "rev-b" [(work "000001_1" "sha256:src" :artifacts moved-documents)])
        {:keys [works unexplained]} (release-delta/delta earlier later)]
    (is (= ["000001_1"] (:documents-changed works)))
    (is (= ["000001_1"] unexplained)
        "same source, same toolchain, same catalog and same rights, different bytes")))

(deftest a-catalog-edit-explains-documents-moving-and-is-not-unexplained
  (testing "a metadata-only upstream revision moves documents for a knowable reason"
    ;; The metadata record comes from the catalog rows, so a catalog-only
    ;; commit that edits a published work's title or contributors moves that
    ;; work's documents with its source hash and every stage standing still.
    ;; About one commit in ten does, so absorbing this into the determinism
    ;; finding would bury the finding.
    (let [earlier (manifest "rev-a" [(work "000001_1" "sha256:src")])
          later (manifest "rev-b" [(work "000001_1" "sha256:src" :artifacts moved-documents)]
                          :catalog (str "snh:1:catalog:" (apply str (repeat 64 "b"))))
          {:keys [works unexplained catalog]} (release-delta/delta earlier later)]
      (is (= ["000001_1"] (:documents-changed works)))
      (is (= [] unexplained))
      (is (not= (:from catalog) (:to catalog))
          "and the input that withdrew the claim is in the report")))

  (testing "a moved rights block does the same"
    ;; Rights reach the TEI header, so they are an input to every document.
    (let [earlier (manifest "rev-a" [(work "000001_1" "sha256:src")])
          later (manifest "rev-b" [(work "000001_1" "sha256:src" :artifacts moved-documents)]
                          :rights (assoc one-rights "statement_url" "https://example.org/rights/2"))
          {:keys [unexplained rights]} (release-delta/delta earlier later)]
      (is (= [] unexplained))
      (is (not= (:from rights) (:to rights))))))

(deftest a-work-leaving-splits-by-whether-a-governance-event-removed-it
  (let [earlier (manifest "rev-a" [(work "000001_1" "sha256:one")
                                   (work "000002_2" "sha256:two")
                                   (work "000003_3" "sha256:three")])
        later (manifest "rev-b" [(work "000003_3" "sha256:three")]
                        :withdrawn [{"slug" "000001_1"
                                     "event" "snh:1:governance-event:ee"}])
        {:keys [works]} (release-delta/delta earlier later)]
    (is (= ["000001_1"] (:withdrawn works)))
    (is (= ["000002_2"] (:dropped works))
        "gone without a governance event, so it stopped being admitted")
    (is (= 1 (:unchanged works)))))

(deftest a-new-work-is-added-and-a-layer-alone-is-its-own-cause
  (let [earlier (manifest "rev-a" [(work "000001_1" "sha256:one")])
        later (manifest "rev-b" [(work "000001_1" "sha256:one"
                                       :layers [{"id" "snh:1:annotation-layer:ff" "bytes" 12}])
                                 (work "000009_9" "sha256:nine")])
        {:keys [works unexplained]} (release-delta/delta earlier later)]
    (is (= ["000009_9"] (:added works)))
    (is (= ["000001_1"] (:layers-changed works)))
    (is (= [] (:documents-changed works)))
    (is (= [] unexplained)
        "publishing a layer moves no document, so it is never unexplained")))

(deftest a-stage-entering-or-leaving-the-toolchain-is-a-change
  (let [earlier (manifest "rev-a" [(work "000001_1" "sha256:one")])
        later (manifest "rev-b" [(work "000001_1" "sha256:one")]
                        :toolchain (assoc one-stage "annotate"
                                          {"nix_closure_hash" "closure-9"
                                           "stage_code_version" "0.1.0"}))
        {:keys [toolchain]} (release-delta/delta earlier later)]
    (is (= #{"annotate"} (set (keys toolchain))))
    (is (nil? (get-in toolchain ["annotate" :from])))
    (is (= "closure-9" (get-in toolchain ["annotate" :to "nix_closure_hash"])))))

(deftest the-report-form-is-string-keyed-and-ordered
  (let [earlier (manifest "rev-a" [(work "000001_1" "sha256:one")])
        later (manifest "rev-b" [(work "000001_1" "sha256:two")])
        r (release-delta/report earlier later)]
    (is (= ["catalog" "corpus" "rights" "toolchain" "unexplained" "works"] (vec (keys r))))
    (is (= {"from" "rev-a" "to" "rev-b"} (into {} (get r "corpus"))))
    (is (= ["000001_1"] (get-in r ["works" "source-changed"])))
    (is (= 0 (get-in r ["works" "unchanged"])))))
