(ns soranoha.assessment.aozora
  "Capture official edition classifications and recheck their current applicability.
  Retained bytes establish the observed assertion; live responses establish that
  the selected edition still carries that assertion. Neither reconstructs law."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [soranoha.core.hash :as hash]
            [soranoha.ported.source-bundle :as bundle]
            [soranoha.yomi.catalog :as catalog]
            [soranoha.yomi.select :as select])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream StringReader]
           [java.net HttpURLConnection URI]
           [java.nio ByteBuffer]
           [java.nio.charset CodingErrorAction StandardCharsets]
           [java.time LocalDate]
           [java.util.zip ZipInputStream]
           [javax.swing.text.html HTML$Attribute HTMLEditorKit$ParserCallback]
           [javax.swing.text.html.parser ParserDelegator]))

(def catalog-url "https://www.aozora.gr.jp/index_pages/list_person_all_extended_utf8.zip")
(def rules-url "https://www.aozora.gr.jp/guide/kijyunn.html")
(def ^:private response-limit (* 32 1024 1024))

(defn- refuse! [reason]
  (throw (ex-info (str "Aozora reliance unavailable: " reason) {:reason reason})))

(defn- official-uri [url]
  (let [u (URI. url)]
    (when-not (and (= "https" (.getScheme u))
                   (= "www.aozora.gr.jp" (.getHost u))
                   (= -1 (.getPort u)) (nil? (.getUserInfo u))
                   (nil? (.getQuery u)) (nil? (.getFragment u)))
      (refuse! "unofficial-url"))
    u))

(defn- bounded-bytes [in]
  (let [out (ByteArrayOutputStream.) buffer (byte-array 8192)]
    (loop [total 0]
      (let [n (.read ^java.io.InputStream in buffer)]
        (if (neg? n)
          (.toByteArray out)
          (let [total (+ total n)]
            (when (> total response-limit) (refuse! "response-too-large"))
            (.write out buffer 0 n)
            (recur total)))))))

(defn- fetch-http [url]
  (let [^HttpURLConnection c (.openConnection (.toURL (official-uri url)))]
    (try
      (.setInstanceFollowRedirects c false)
      (.setConnectTimeout c 15000)
      (.setReadTimeout c 15000)
      (.setRequestProperty c "Accept-Encoding" "identity")
      (when-not (= 200 (.getResponseCode c)) (refuse! "http-status"))
      (with-open [in (.getInputStream c)] (bounded-bytes in))
      (finally (.disconnect c)))))

(defn- fetch [opts url]
  (official-uri url)
  (let [b ((or (:fetch opts) fetch-http) url)]
    (when-not (and (bytes? b) (pos? (alength ^bytes b))
                   (<= (alength ^bytes b) response-limit))
      (refuse! "invalid-response"))
    b))

(defn- utf8 [bytes]
  (str (.decode (doto (.newDecoder StandardCharsets/UTF_8)
                  (.onMalformedInput CodingErrorAction/REPORT)
                  (.onUnmappableCharacter CodingErrorAction/REPORT))
                (ByteBuffer/wrap bytes))))

(defn- catalog-rows [bytes]
  (with-open [in (ZipInputStream. (ByteArrayInputStream. bytes))]
    (loop [csv nil]
      (if-let [entry (.getNextEntry in)]
        (if (str/ends-with? (.getName entry) ".csv")
          (do (when csv (refuse! "ambiguous-catalog"))
              (recur (utf8 (bounded-bytes in))))
          (do (bounded-bytes in) (recur csv)))
        (if csv (catalog/read-rows-from-string csv) (refuse! "missing-catalog-csv"))))))

(defn- selection [root]
  (let [rows (catalog/read-rows-from-string (:csv-text (catalog/read-catalog-zip root)))]
    (into {} (map (juxt :slug identity))
          (:candidates (select/select-candidates root rows)))))

(defn- selected [candidates slug]
  (or (get candidates slug) (refuse! "missing-selected-work")))

(defn- assertion [rows candidate]
  (let [id (get-in candidate [:row "作品ID"])
        matches (filter #(= id (get % "作品ID")) rows)
        urls (set (map #(select-keys % ["図書カードURL" "テキストファイルURL"]) matches))
        expected-file (str "https://www.aozora.gr.jp/" (:relpath candidate))
        card (str "https://www.aozora.gr.jp/cards/"
                  (second (str/split (:relpath candidate) #"/"))
                  "/card" (Long/parseLong id) ".html")]
    (when-not (seq matches) (refuse! "missing-current-work"))
    (when (some catalog/ragged-key matches) (refuse! "malformed-catalog-row"))
    (when-not (every? #(= "なし" (get % "作品著作権フラグ")) matches)
      (refuse! "not-classified-expired"))
    (when-not (= #{{"図書カードURL" card "テキストファイルURL" expected-file}} urls)
      (refuse! "edition-link-mismatch"))
    {:card card :file expected-file :work-id id}))

(defn- check-card! [bytes {:keys [card file work-id]}]
  (let [links (atom #{}) text (StringBuilder.)
        callback (proxy [HTMLEditorKit$ParserCallback] []
                   (handleStartTag [_ attrs _]
                     (when-let [href (.getAttribute attrs HTML$Attribute/HREF)]
                       (swap! links conj (str (.resolve (URI. card) (str href))))))
                   (handleText [chars _] (.append text ^chars chars)))]
    (.parse (ParserDelegator.) (StringReader. (utf8 bytes)) callback true)
    (when (str/includes? (str text) "著作権存続") (refuse! "protected-card"))
    (when-not (re-find (re-pattern (str "図書カード[：:]\\s*No\\.\\s*"
                                        (Long/parseLong work-id) "(?![0-9])"))
                       (str text))
      (refuse! "card-identity-mismatch"))
    (when-not (contains? @links file) (refuse! "missing-card-file-link"))))

(defn- bundle-hash [bytes]
  (let [path (fs/create-temp-file {:prefix "aozora-edition" :suffix ".zip"})]
    (try
      (fs/write-bytes path bytes)
      (:bundle-hash (bundle/inspect-zip (io/file (str path))))
      (finally (fs/delete-if-exists path)))))

(defn- retain! [root bytes]
  (fs/create-dirs root)
  (let [digest (hash/sha256-bytes bytes) path (fs/path root digest)]
    (when (fs/sym-link? path) (refuse! "evidence-symlink"))
    (if (fs/exists? path)
      (when-not (= digest (hash/sha256-file (str path))) (refuse! "evidence-digest-mismatch"))
      (fs/write-bytes path bytes))
    digest))

(defn- retained [root record key]
  (let [digest (get record key)]
    (when-not (and (string? digest) (re-matches hash/hex-pattern digest))
      (refuse! "invalid-evidence-digest"))
    (when-not root (refuse! "missing-evidence-root"))
    (let [path (fs/path root digest)]
      (when-not (and (fs/regular-file? path) (not (fs/sym-link? path)))
        (refuse! "missing-retained-evidence"))
      (with-open [in (io/input-stream (str path))]
        (let [b (bounded-bytes in)]
          (when-not (= digest (hash/sha256-bytes b)) (refuse! "evidence-digest-mismatch"))
          b)))))

(defn prepare!
  "Acquire an official assertion for a selected edition and retain its evidence.
  The optional fetch function accepts an official URL and returns response bytes."
  [aozora-root evidence-root slug opts]
  (let [candidate (selected (selection aozora-root) slug)
        catalog (fetch opts catalog-url)
        a (assertion (catalog-rows catalog) candidate)
        card (fetch opts (:card a)) file (fetch opts (:file a))
        rules (fetch opts rules-url)
        content-hash (bundle-hash file)
        _ (check-card! card a)
        _ (when-not (= content-hash (:bundle-hash (bundle/inspect-zip (:file candidate))))
            (refuse! "edition-content-mismatch"))
        git (process/sh {:dir (str aozora-root)} "git" "rev-parse" "HEAD")
        _ (when-not (zero? (:exit git)) (refuse! "source-revision-unavailable"))
        today (str (LocalDate/now java.time.ZoneOffset/UTC))]
    {"slug" slug "source_content_hash" content-hash
     "source_revision" (str/trim (:out git))
     "observed_at" (or (:observed-at opts) today)
     "decision_date" (or (:decision-date opts) today)
     "basis" (or (:basis opts) "Reliance on Aozora's published copyright-expired classification for this exact edition in Japan.")
     "catalog_sha256" (retain! evidence-root catalog)
     "card_sha256" (retain! evidence-root card)
     "file_sha256" (retain! evidence-root file)
     "rules_sha256" (retain! evidence-root rules)
     "exception" nil}))

(defn- outcome [f]
  (try (f) {:state "available" :reason nil}
       (catch Exception e
         {:state "unavailable" :reason (str (or (:reason (ex-data e)) "acquisition-failed"))})))

(defn check!
  "Verify retained assertions and their live applicability. A failed current
  acquisition never falls back to a retained assertion or checkout existence."
  ([aozora-root evidence-root records] (check! aozora-root evidence-root records {}))
  ([aozora-root evidence-root records opts]
   (let [candidates (delay (selection aozora-root))
         live (delay (try {:rows (catalog-rows (fetch opts catalog-url))
                           :rules (hash/sha256-bytes (fetch opts rules-url))}
                          (catch Exception e {:error e})))]
     (into {}
           (for [record records]
             [(get record "slug")
              (outcome
               (fn []
                 (when (some? (get record "exception")) (refuse! "recorded-exception"))
                 (let [candidate (selected @candidates (get record "slug"))
                       a (assertion (catalog-rows (retained evidence-root record "catalog_sha256")) candidate)
                       pinned (get record "source_content_hash")]
                   (check-card! (retained evidence-root record "card_sha256") a)
                   (when-not (= pinned (bundle-hash (retained evidence-root record "file_sha256")))
                     (refuse! "retained-edition-mismatch"))
                   (retained evidence-root record "rules_sha256")
                   (when-not (= pinned (:bundle-hash (bundle/inspect-zip (:file candidate))))
                     (refuse! "checkout-edition-mismatch"))
                   (when-let [e (:error @live)] (throw e))
                   (when-not (= (get record "rules_sha256") (:rules @live))
                     (refuse! "rules-changed"))
                   (let [current (assertion (:rows @live) candidate)]
                     (check-card! (fetch opts (:card current)) current)
                     (when-not (= pinned (bundle-hash (fetch opts (:file current))))
                       (refuse! "current-edition-mismatch"))))))])))))
