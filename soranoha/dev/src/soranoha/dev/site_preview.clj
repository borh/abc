(ns soranoha.dev.site-preview
  "Assemble a local serving tree from an already-built subset, so the site can
  be looked at before there is a public release.

  This is never a release, and it is on the dev path rather than the source
  path so that no shipped binary can reach it. Three things here are stand-ins,
  and everything else is the real publication code:

  - the signing keys are the checked-in conformance fixture pair, which is
    public and therefore proves nothing about anything;
  - the assessment snapshot is asserted rather than evaluated, because
    evaluating it needs live access to aozora.gr.jp;
  - the chain is a throwaway origin and clone under the output directory,
    with no relationship to the published lineage.

  What is real: the works come from an ordinary `soranoha-kernel build`, and
  the manifest, catalog, blob layout, browse layer, symlink routes and bulk
  archives are all produced by `soranoha.za.release` and `soranoha.za.serve`
  on the paths a release uses. A page that renders wrong here renders wrong in
  production."
  (:require [babashka.fs :as fs]
            [charred.api :as json]
            [clojure.java.io :as io]
            [soranoha.core.config :as config]
            [soranoha.snh.decode :as decode]
            [soranoha.snh.repo :as repo]
            [soranoha.snh.sign :as sign]
            [soranoha.snh.transact :as transact]
            [soranoha.za.release :as za-release]
            [soranoha.za.serve :as serve]))

(def ^:private fixture-keys
  (delay (get (json/read-json
               (slurp (io/resource "snh/vectors/signature-vectors.json")))
              "keys")))

(defn- pinned-keys []
  {:release (get-in @fixture-keys ["release" "pub"])
   :governance (get-in @fixture-keys ["governance" "pub"])})

(defn- asserted-fact [basis]
  {"status" "public-domain" "jurisdiction" "jp"
   "effective_date" "2026-08-01" "basis" basis})

(defn- snapshot
  "An assessment snapshot that admits every selected work. A real snapshot is
  evaluated against recorded upstream facts; the basis strings here say
  `preview` so that a snapshot from this path can never be mistaken for one."
  [slugs]
  {"schema" "snh-assessment-snapshot/2"
   "candidates" (mapv (fn [slug]
                        {"slug" slug
                         "work_assessment" (asserted-fact (str "preview:edition:" slug))
                         "contributions" [(assoc (asserted-fact (str "preview:author:" slug))
                                                 "contribution_id" (str "author:" slug))]})
                      slugs)})

(defn- publish!
  "Sign and push one release onto a fresh local chain. Returns the clone."
  [{:keys [chain-dir root-dir report slugs assets-root]}]
  (let [origin (repo/init-origin! (fs/path chain-dir "origin.git"))
        clone (repo/clone! origin (fs/path chain-dir "clone"))
        authority (za-release/rights-authority!
                   (fs/read-all-bytes (str (fs/path assets-root "data/publication-policy.edn"))))
        seed (sign/hex->bytes (get-in @fixture-keys ["release" "seed"]))]
    (transact/init-publication-branch! clone "main")
    (let [outcome (za-release/release!
                   {:selection slugs
                    :build-works! (constantly report)
                    :source-hashes (into {}
                                         (map (fn [[slug work]]
                                                [slug (get work "source_content_hash")]))
                                         (get report "works"))
                    :cas-dir (config/cas-dir (config/root (str root-dir)))
                    :upstream-origin "https://github.com/aozorabunko/aozorabunko.git"
                    :selection-params {"config" "preview"}
                    :policy-id (:policy-id authority)
                    :policy-hash (:policy-hash authority)
                    :rights (:rights authority)
                    :snapshot-bytes (:bytes (decode/encode "assessment-snapshot"
                                                           (snapshot slugs)))
                    :clone (str clone)
                    :branch "main"
                    :pinned-keys (pinned-keys)
                    :sign-release (fn [manifest-hex]
                                    (sign/sign seed (sign/manifest-message manifest-hex)))})]
      (println (str "release: " (name (:outcome outcome)) " " (:manifest-id outcome)))
      clone)))

(defn -main [& args]
  (let [{:keys [run root out assets-root]}
        (into {} (map (fn [[flag value]] [(keyword (subs flag 2)) value]))
              (partition 2 args))
        report (json/read-json (slurp (str (fs/path run "build.json"))))
        slugs (vec (sort (get report "selected_slugs")))
        chain-dir (doto (fs/path out "chain") fs/create-dirs)
        tree (fs/path out "tree")
        clone (publish! {:chain-dir chain-dir :root-dir root :report report
                         :slugs slugs :assets-root assets-root})
        exported (serve/export-tree! {:clone (str clone) :branch "main"
                                      :pinned-keys (pinned-keys)
                                      :release-doi nil
                                      :out-dir (str tree)})]
    (println (str "works: " (count slugs)
                  " blobs: " (:blobs exported)
                  " releases: " (:releases exported)))
    (println (str "tree: " tree))
    (shutdown-agents)))
