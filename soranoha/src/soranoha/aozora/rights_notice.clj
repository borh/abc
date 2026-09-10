(ns soranoha.aozora.rights-notice
  "The rights standing of one Aozora Bunko work.

  The catalog's 作品著作権フラグ says whether copyright subsists. There is no
  licence column, so for a work where it subsists the terms are only in the
  colophon: those works are on Aozora Bunko at the rightsholder's request and
  carry the licence they chose.

  A work whose notice does not resolve to a licence this build can state its
  terms in is refused. There is no default, because the fallback would be to
  publish terms nobody granted."
  (:require [clojure.string :as string]
            [soranoha.core.rights :as rights]))

(def ^:private flag-key "作品著作権フラグ")

;; Canonical element order, so one licence has one identifier whatever order
;; its notice used.
(def ^:private element-order ["BY" "NC" "ND" "SA"])

(def ^:private restricting-elements
  "Licence elements this release cannot publish under.

  NC and ND are incompatible with the grant the site makes, that everything
  here may be used commercially and adapted. SA is incompatible with the CC0
  dedication over Soranoha's encoding: ShareAlike extends to adaptations, and
  a TEI file carrying editorial decisions about an SA work is one, so it would
  owe BY-SA."
  #{"NC" "ND" "SA"})

(defn- identifier [{:keys [elements version port]}]
  (str "CC-"
       (string/join "-" (filter (set elements) element-order))
       "-" version
       (when port (str "-" port))))

;; The port is two letters at the end of the segment. Without the lookahead,
;; `licenses/by/3.0/deed.ja` yields CC-BY-3.0-DE, which is not a licence.
(def ^:private licence-url
  #"creativecommons\.org/licenses/([a-z][a-z-]*)/(\d(?:\.\d)?)(?:/([a-z]{2})(?![a-z]))?")

(defn- from-url [text]
  (when-let [[_ code version port] (re-find licence-url text)]
    {:elements (mapv string/upper-case (string/split code #"-"))
     :version version
     :port (some-> port string/upper-case)}))

;; Some notices give no URL. 058806_001955 and 058807_001955 say
;; 「この作品は、クリエイティブ・コモンズ「表示 2.1 日本」でライセンスされています。」
;; and nothing else. Spacing, interpuncts and hyphens vary between notices, so
;; match the clause and look for elements inside it.
(def ^:private licence-prose
  #"クリエイティブ[・]?[ 　]*コモンズ[「\s・]*([^」\n（(]{0,60})")

(def ^:private prose-elements
  {"表示" "BY" "非営利" "NC" "改変禁止" "ND" "継承" "SA"})

(defn- from-prose [text]
  (when-let [[_ clause] (re-find licence-prose text)]
    (when-let [[_ version] (re-find #"(\d(?:\.\d)?)" clause)]
      {:elements (keep (fn [[jp code]]
                         (when (string/includes? clause jp) code))
                       prose-elements)
       :version version
       ;; 日本 is the only ported jurisdiction in these notices. 国際 and
       ;; 非移植 are the unported deeds, whose URLs carry no port.
       :port (when (string/includes? clause "日本") "JP")})))

(defn parse-notice
  "The licence a colophon states, as `{:elements :version :port}`, or nil.
  Prefers the URL, which is unambiguous, and falls back to the prose form."
  [text]
  (when text
    (when-let [parsed (or (from-url text) (from-prose text))]
      (when (some #{"BY"} (:elements parsed))
        parsed))))

(defn copyright-flags
  "The distinct 作品著作権フラグ values across the catalog rows for one work."
  [rows]
  (into #{} (map #(get % flag-key)) rows))

(defn standing
  "The rights standing of the work `rows` describe. `text-fn` is a thunk
  returning the work's primary text. Returns `{:standing s}` if the work may
  be published, otherwise `{:refused reason}` with whatever the notice stated.

  The thunk exists so that the expired works, which are almost all of them,
  are settled by the flag alone and never open an archive."
  [rows text-fn]
  (let [flags (copyright-flags rows)]
    (cond
      ;; Disagreeing rows cannot be resolved by majority: one of them is wrong
      ;; about whether a right subsists, and there is no way to tell which.
      (not= 1 (count flags))
      {:refused :inconsistent-copyright-flag :flags (vec (sort flags))}

      (= #{"なし"} flags)
      {:standing "public-domain"}

      (not= #{"あり"} flags)
      {:refused :unknown-copyright-flag :flags (vec (sort flags))}

      :else
      (if-let [parsed (parse-notice (text-fn))]
        (let [found (identifier parsed)]
          (cond
            (some restricting-elements (:elements parsed))
            {:refused :restricted-licence :licence found}

            ;; A licence this build cannot state its terms in is refused here,
            ;; where one work is refused, rather than reaching the stage that
            ;; renders the terms and stopping the release of every other work.
            ;; Versions and ports the corpus has not carried before arrive
            ;; with an upstream bump, and nobody has decided about them yet.
            (not (contains? (rights/standings) found))
            {:refused :unpublishable-licence :licence found}

            :else {:standing found}))
        {:refused :unstated-licence}))))
