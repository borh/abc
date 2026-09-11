(ns soranoha.za.maturity
  "Corpus maturity vocabulary and release name formatting for the serving layer.

  Maturity labels and release names are editorial classifications rendered on the
  site and recorded in Zenodo deposits; they are omitted from signed manifests.
  Because the site is bilingual, labels are validated against a closed vocabulary."
  (:require [clojure.string :as string]))

(def labels
  "Maturity vocabulary entries with bilingual display names and explanatory notes."
  {"early"
   {:ja "初期リリース"
    :en "early release"
    :note-ja (str "公開したばかりのコーパスです。署名と検証は本番のものと同じで、"
                  "内容も検証済みですが、外字や図版の扱いなど、既知の未完了作業があります。"
                  "お気づきの点をお寄せください。")
    :note-en (str "Initial corpus release. Signatures and verification are active and "
                  "contents are verified, with known development remaining in gaiji "
                  "coverage and figure markup. Defect reports and feedback are welcome.")}
   "stable"
   {:ja "安定版"
    :en "stable release"
    :note-ja "内容と形式が安定した版です。"
    :note-en "The contents and artifact formats of this release are stable."}})

(defn label!
  "The vocabulary entry for `key`, or nil when no label is configured.
  Throws on an unknown key to prevent partial or unilingual label rendering."
  [key]
  (when-let [key (some-> key string/trim not-empty)]
    (or (get labels key)
        (throw (ex-info "Unknown maturity label"
                        {:reason :unknown-maturity-label
                         :maturity key
                         :known (vec (sort (keys labels)))})))))

(def ^:private name-pattern
  ;; Dotted release name pattern (e.g. "v0.1"). Narrowly constrained so citations
  ;; display well-formed release labels alongside the release hash.
  #"v\d+(\.\d+)*")

(defn release-name!
  "The corpus's release name, checked for format, or nil when unconfigured.
  Names the corpus development stage rather than an individual release commit."
  [value]
  (when-let [name (some-> value string/trim not-empty)]
    (when-not (re-matches name-pattern name)
      (throw (ex-info "Invalid release name"
                      {:reason :invalid-release-name :release-name name})))
    name))
