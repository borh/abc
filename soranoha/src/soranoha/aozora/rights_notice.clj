(ns soranoha.aozora.rights-notice
  "The rights standing of one Aozora Bunko work, decided from the only two
  statements upstream makes about it.

  The catalog's 作品著作権フラグ says whether copyright subsists, and nothing
  more: there is no licence column. For a work whose copyright has expired
  that is the whole answer. For a work whose copyright subsists, the terms
  exist only as the rightsholder's own notice in the colophon, because those
  works are on Aozora Bunko by the rightsholder's decision and carry the
  licence they chose.

  So this reads a notice rather than a field, and reads it strictly. A work
  whose notice does not resolve to a licence this code can name is refused,
  never given a default: guessing here would publish terms nobody granted."
  (:require [clojure.string :as string]))

(def ^:private flag-key "作品著作権フラグ")

(def ^:private element-order
  "The order Creative Commons writes licence elements in, so one licence has
  one identifier however its notice happened to spell it."
  ["BY" "NC" "ND" "SA"])

(def ^:private restricting-elements
  "Licence elements the release cannot honour as it publishes.

  NC and ND contradict the grant every page and every manifest makes: that
  anything published here may be used commercially and adapted. SA contradicts
  the CC0 dedication over Soranoha's own encoding, because a TEI file carrying
  editorial decisions about an SA work is an adaptation of it, and an
  adaptation owes BY-SA rather than CC0.

  A work under one of these is not published, rather than published under
  terms the site contradicts elsewhere."
  #{"NC" "ND" "SA"})

(defn- identifier
  "The licence identifier for a parsed notice: elements in canonical order,
  then the version, then the jurisdiction port when the deed is a ported one."
  [{:keys [elements version port]}]
  (str "CC-"
       (string/join "-" (filter (set elements) element-order))
       "-" version
       (when port (str "-" port))))

;; The URL the notice links, when it links one. The port is two letters that
;; end the segment; without that guard `licenses/by/3.0/deed.ja` reads its own
;; `deed` as a jurisdiction and mints a standing that does not exist.
(def ^:private licence-url
  #"creativecommons\.org/licenses/([a-z][a-z-]*)/(\d(?:\.\d)?)(?:/([a-z]{2})(?![a-z]))?")

(defn- from-url [text]
  (when-let [[_ code version port] (re-find licence-url text)]
    {:elements (mapv string/upper-case (string/split code #"-"))
     :version version
     :port (some-> port string/upper-case)}))

;; Some notices name the licence only in prose, with no URL to fall back on:
;; 058806_001955 and 058807_001955 say
;; 「この作品は、クリエイティブ・コモンズ「表示 2.1 日本」でライセンスされています。」
;; and stop there. Spacing, interpuncts and hyphens vary between notices, so
;; the clause is captured whole and its elements are matched inside it.
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
       ;; 日本 is the only ported jurisdiction Aozora Bunko's notices use;
       ;; 国際 and 非移植 name the unported deeds, whose URLs carry no port.
       :port (when (string/includes? clause "日本") "JP")})))

(defn parse-notice
  "The licence a colophon states, as `{:elements :version :port}`, or nil when
  it states none. The URL is preferred because it is unambiguous; the prose
  form is the fallback for notices that carry no link."
  [text]
  (when text
    (when-let [parsed (or (from-url text) (from-prose text))]
      (when (some #{"BY"} (:elements parsed))
        parsed))))

(defn copyright-flags
  "The distinct 作品著作権フラグ values the catalog rows for one work carry."
  [rows]
  (into #{} (map #(get % flag-key)) rows))

(defn standing
  "The rights standing of the work `rows` describe, given `text-fn`, a thunk
  returning the work's primary text. Returns `{:standing s}` when the work may
  be published, or `{:refused reason}` when it may not, carrying whatever the
  notice did say so the refusal names its own cause.

  `text-fn` is a thunk because the expired works never need it: their flag
  decides them, and opening an archive to confirm what the catalog already
  said would be work done for nothing."
  [rows text-fn]
  (let [flags (copyright-flags rows)]
    (cond
      ;; Rows that disagree are not a majority to be taken: one of them is
      ;; wrong about whether a right subsists, and which one is not knowable
      ;; from here.
      (not= 1 (count flags))
      {:refused :inconsistent-copyright-flag :flags (vec (sort flags))}

      (= #{"なし"} flags)
      {:standing "public-domain"}

      (not= #{"あり"} flags)
      {:refused :unknown-copyright-flag :flags (vec (sort flags))}

      :else
      (if-let [parsed (parse-notice (text-fn))]
        (let [found (identifier parsed)]
          (if (some restricting-elements (:elements parsed))
            {:refused :restricted-licence :licence found}
            {:standing found}))
        {:refused :unstated-licence}))))
