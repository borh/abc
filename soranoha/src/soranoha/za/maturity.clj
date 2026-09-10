(ns soranoha.za.maturity
  "The corpus's maturity label and its human-readable name, which the serving
  layer renders and no manifest carries.

  Every other field a release publishes is a fact derived from the release's
  inputs. These two are editorial judgements about how finished the corpus is,
  and signed append-only bytes could not correct one that was wrong, so under
  the publication decision they live in deployment configuration and in the
  Zenodo deposit's metadata only.

  The label is a key into a closed vocabulary rather than free text. The site
  is bilingual, so a label has to exist in both languages to be rendered at
  all, and a configuration string could supply only one of them. A deployment
  naming a label this build cannot render has outrun the code, and the export
  stops rather than serving a corpus whose maturity is stated in one language."
  (:require [clojure.string :as string]))

(def labels
  "Each label says what the reader should do with the corpus, not merely how
  far along it is: `early` is an invitation to report what is wrong, which is
  the thing a version number alone never communicates."
  {"early"
   {:ja "初期リリース"
    :en "early release"
    :note-ja (str "公開したばかりのコーパスです。署名と検証は本番のものと同じで、"
                  "内容も検証済みですが、外字や図版の扱いなど、既知の未完了作業があります。"
                  "お気づきの点をお寄せください。")
    :note-en (str "This corpus has only just been published. The signatures and the "
                  "verification are the real ones and the contents are checked, but "
                  "known work remains, notably in gaiji coverage and in the handling "
                  "of figures. Reports of what is wrong are welcome.")}
   "stable"
   {:ja "安定版"
    :en "stable release"
    :note-ja "内容と形式が安定した版です。"
    :note-en "The contents and the artifact formats of this corpus are settled."}})

(defn label!
  "The vocabulary entry for `key`, or nil when no label is configured.

  Fails closed on an unknown key: a deployment that meant to say something
  about the corpus's maturity and said nothing would be indistinguishable
  here from one that deliberately says nothing at all."
  [key]
  (when-let [key (some-> key string/trim not-empty)]
    (or (get labels key)
        (throw (ex-info "Unknown maturity label"
                        {:reason :unknown-maturity-label
                         :maturity key
                         :known (vec (sort (keys labels)))})))))

(def ^:private name-pattern
  ;; `v` and a dotted numeric series, which is every form this project has
  ;; reason to publish. Deliberately narrow: the name is rendered beside the
  ;; release head on a page that a reader takes a citation from, and a free
  ;; string there could be mistaken for the head's own name.
  #"v\d+(\.\d+)*")

(defn release-name!
  "The corpus's human-readable name, checked for shape, or nil when none is
  configured.

  It names the corpus at this stage of its life rather than the individual
  release: releases are minted whenever the upstream corpus moves, so a name
  that incremented with each one would reach three digits without telling a
  reader anything the release head does not already say. The head remains the
  identity, and citations name it."
  [value]
  (when-let [name (some-> value string/trim not-empty)]
    (when-not (re-matches name-pattern name)
      (throw (ex-info "Invalid release name"
                      {:reason :invalid-release-name :release-name name})))
    name))
