(ns soranoha.assessment.aozora
  "Capture official edition classifications and recheck their current applicability.
  Retained bytes establish the observed assertion; live responses establish that
  the selected edition still carries that assertion. Neither reconstructs law."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [soranoha.core.hash :as hash]
            [soranoha.core.parallel :as parallel]
            [soranoha.aozora.source-bundle :as bundle]
            [soranoha.yomi.catalog :as catalog]
            [soranoha.yomi.select :as select])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream IOException StringReader]
           [java.net HttpURLConnection URI]
           [java.nio ByteBuffer]
           [java.nio.charset CodingErrorAction StandardCharsets]
           [java.time LocalDate]
           [java.util Arrays]
           [java.util.zip ZipInputStream]
           [javax.swing.text.html HTML$Attribute HTMLEditorKit$ParserCallback]
           [javax.swing.text.html.parser ParserDelegator]))

(def catalog-url "https://www.aozora.gr.jp/index_pages/list_person_all_extended_utf8.zip")
(def rules-url "https://www.aozora.gr.jp/guide/kijyunn.html")
(def ^:private response-limit (* 32 1024 1024))

(defn reason->wire [{:keys [reason detail]}]
  (case reason
    :aozora/acquisition-failed
    (if (or (nil? detail) (and (string? detail) (seq detail)))
      (or detail "acquisition-failed")
      (throw (ex-info "Invalid Aozora acquisition diagnostic"
                      {:reason :invalid-aozora-reason :value detail})))
    (:aozora/ambiguous-catalog
     :aozora/card-identity-mismatch
     :aozora/checkout-edition-mismatch
     :aozora/current-edition-mismatch
     :aozora/edition-content-mismatch
     :aozora/edition-link-mismatch
     :aozora/evidence-digest-mismatch
     :aozora/evidence-symlink
     :aozora/http-status
     :aozora/invalid-evidence-digest
     :aozora/invalid-response
     :aozora/malformed-catalog-row
     :aozora/missing-card-file-link
     :aozora/missing-catalog-csv
     :aozora/missing-current-work
     :aozora/missing-evidence-root
     :aozora/missing-retained-evidence
     :aozora/missing-selected-work
     :aozora/not-classified-expired
     :aozora/protected-card
     :aozora/recorded-exception
     :aozora/response-too-large
     :aozora/retained-edition-mismatch
     :aozora/rules-changed
     :aozora/source-revision-unavailable
     :aozora/unofficial-url) (name reason)
    (throw (ex-info "Invalid Aozora unavailability reason"
                    {:reason :invalid-aozora-reason :value reason}))))

(defn observation->wire [{:keys [state] :as observation}]
  (case state
    :aozora/available
    (if (nil? (:reason observation))
      {"state" "available" "reason" nil}
      (throw (ex-info "Available Aozora observation cannot carry a failure reason"
                      {:reason :invalid-reliance-observation :value (:reason observation)})))
    :aozora/unavailable {"state" "unavailable" "reason" (reason->wire observation)}
    (throw (ex-info "Invalid Aozora observation state"
                    {:reason :invalid-reliance-observation :value state}))))

(defn- refuse! [reason]
  (throw (ex-info (str "Aozora reliance unavailable: " (reason->wire {:reason reason})) {:reason reason})))

(defn- official-uri [url]
  (let [u (URI. url)]
    (when-not (and (= "https" (.getScheme u))
                   (= "www.aozora.gr.jp" (.getHost u))
                   (= -1 (.getPort u)) (nil? (.getUserInfo u))
                   (nil? (.getQuery u)) (nil? (.getFragment u)))
      (refuse! :aozora/unofficial-url))
    u))

(defn- bounded-bytes [in]
  (let [out (ByteArrayOutputStream.) buffer (byte-array 8192)]
    (loop [total 0]
      (let [n (.read ^java.io.InputStream in buffer)]
        (if (neg? n)
          (.toByteArray out)
          (let [total (+ total n)]
            (when (> total response-limit) (refuse! :aozora/response-too-large))
            (.write out buffer 0 n)
            (recur total)))))))

(defn- fetch-http [url]
  (let [^HttpURLConnection c (.openConnection (.toURL ^URI (official-uri url)))]
    (try
      (.setInstanceFollowRedirects c false)
      (.setConnectTimeout c 15000)
      (.setReadTimeout c 15000)
      (.setRequestProperty c "Accept-Encoding" "identity")
      (let [status (.getResponseCode c)]
        (when-not (= 200 status)
          (throw (ex-info "Aozora reliance unavailable: http-status"
                          {:reason :aozora/http-status :status status}))))
      (with-open [in (.getInputStream c)] (bounded-bytes in))
      (finally (.disconnect c)))))

(defn- fetch [opts url]
  (official-uri url)
  (let [b (loop [attempt 0]
            (let [result (try
                           {:bytes ((or (:fetch opts) fetch-http) url)}
                           (catch IOException e {:failure e})
                           (catch clojure.lang.ExceptionInfo e
                             (if (and (= :aozora/http-status (:reason (ex-data e)))
                                      (#{429 502 503 504} (:status (ex-data e))))
                               {:failure e}
                               (throw e))))]
              (if-let [failure (:failure result)]
                (if (< attempt 2)
                  (do (Thread/sleep (* 250 (inc attempt)))
                      (recur (inc attempt)))
                  (throw failure))
                (:bytes result))))]
    (when-not (and (bytes? b) (pos? (alength ^bytes b))
                   (<= (alength ^bytes b) response-limit))
      (refuse! :aozora/invalid-response))
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
          (do (when csv (refuse! :aozora/ambiguous-catalog))
              (recur (utf8 (bounded-bytes in))))
          (do (bounded-bytes in) (recur csv)))
        (if csv (catalog/read-rows-from-string csv) (refuse! :aozora/missing-catalog-csv))))))

(defn- selection [root]
  (let [rows (catalog/read-rows-from-string (:csv-text (catalog/read-catalog-zip root)))]
    (into {} (map (juxt :slug identity))
          (:candidates (select/select-candidates root rows)))))

(defn- selected [candidates slug]
  (or (get candidates slug) (refuse! :aozora/missing-selected-work)))

(defn- assertion [index candidate]
  (let [id (get-in candidate [:row "作品ID"])
        matches (get index id)
        urls (set (map #(select-keys % ["図書カードURL" "テキストファイルURL"]) matches))
        expected-file (str "https://www.aozora.gr.jp/" (:relpath candidate))
        card (str "https://www.aozora.gr.jp/cards/"
                  (second (str/split (:relpath candidate) #"/"))
                  "/card" (Long/parseLong id) ".html")]
    (when-not (seq matches) (refuse! :aozora/missing-current-work))
    (when (some catalog/ragged-key matches) (refuse! :aozora/malformed-catalog-row))
    (when-not (every? #(= "なし" (get % "作品著作権フラグ")) matches)
      (refuse! :aozora/not-classified-expired))
    (when-not (= #{{"図書カードURL" card "テキストファイルURL" expected-file}} urls)
      (refuse! :aozora/edition-link-mismatch))
    {:card card :file expected-file :work-id id}))

(defn- check-card! [bytes {:keys [card file work-id]}]
  (let [links (atom #{}) text (StringBuilder.)
        callback (proxy [HTMLEditorKit$ParserCallback] []
                   (handleStartTag [_ attrs _]
                     (when-let [href (.getAttribute ^javax.swing.text.MutableAttributeSet attrs HTML$Attribute/HREF)]
                       (try
                         (swap! links conj (str (.resolve (URI. card) (str/trim (str href)))))
                         ;; A malformed unrelated link does not change the edition assertion.
                         (catch IllegalArgumentException _ nil))))
                   (handleText [chars _] (.append text ^chars chars)))]
    (.parse (ParserDelegator.) (StringReader. (utf8 bytes)) callback true)
    (when (str/includes? (str text) "著作権存続") (refuse! :aozora/protected-card))
    (when-not (re-find (re-pattern (str "図書カード[：:]\\s*No\\.\\s*"
                                        (Long/parseLong work-id) "(?![0-9])"))
                       (str text))
      (refuse! :aozora/card-identity-mismatch))
    (when-not (contains? @links file) (refuse! :aozora/missing-card-file-link))))

(defn- bundle-hash [bytes]
  (let [path (fs/create-temp-file {:prefix "aozora-edition" :suffix ".zip"})]
    (try
      (fs/write-bytes path bytes)
      (:bundle-hash (bundle/inspect-zip (io/file (str path))))
      (finally (fs/delete-if-exists path)))))

(defn- retain! [root bytes]
  (fs/create-dirs root)
  (let [digest (hash/sha256-bytes bytes) path (fs/path root digest)]
    (when (fs/sym-link? path) (refuse! :aozora/evidence-symlink))
    (if (fs/exists? path)
      (when-not (= digest (hash/sha256-file (str path))) (refuse! :aozora/evidence-digest-mismatch))
      (fs/write-bytes path bytes))
    digest))

(defn- retained [root record key]
  (let [digest (get record key)]
    (when-not (and (string? digest) (re-matches hash/hex-pattern digest))
      (refuse! :aozora/invalid-evidence-digest))
    (when-not root (refuse! :aozora/missing-evidence-root))
    (let [path (fs/path root digest)]
      (when-not (and (fs/regular-file? path) (not (fs/sym-link? path)))
        (refuse! :aozora/missing-retained-evidence))
      (with-open [in (io/input-stream (str path))]
        (let [b (bounded-bytes in)]
          (when-not (= digest (hash/sha256-bytes b)) (refuse! :aozora/evidence-digest-mismatch))
          b)))))

(defn- catalog-index [bytes]
  (group-by #(get % "作品ID") (catalog-rows bytes)))

(defn- preparation-context [aozora-root evidence-root opts]
  (let [catalog (fetch opts catalog-url)
        index (catalog-index catalog)
        rules (delay (fetch opts rules-url))]
    {:index index
     :rules rules
     :declaration
     (delay
       (let [git (process/sh {:dir (str aozora-root)} "git" "rev-parse" "HEAD")
             today (str (LocalDate/now java.time.ZoneOffset/UTC))]
         (when-not (zero? (:exit git)) (refuse! :aozora/source-revision-unavailable))
         {"source_revision" (str/trim (:out git))
          "observed_at" (or (:observed-at opts) today)
          "decision_date" (or (:decision-date opts) today)
          "basis" (or (:basis opts) "Reliance on Aozora's published copyright-expired classification for this exact edition in Japan.")
          "catalog_sha256" (retain! evidence-root catalog)
          "rules_sha256" (retain! evidence-root @rules)
          "exception" nil}))}))

(defn- prepare-edition! [evidence-root candidate context opts]
  (let [a (assertion (:index context) candidate)
        card (fetch opts (:card a)) file (fetch opts (:file a))
        _ @(:rules context)
        content-hash (bundle-hash file)]
    (check-card! card a)
    (when-not (= content-hash (:bundle-hash (bundle/inspect-zip (:file candidate))))
      (refuse! :aozora/edition-content-mismatch))
    (locking context
      (assoc @(:declaration context)
             "slug" (:slug candidate) "source_content_hash" content-hash
             "card_sha256" (retain! evidence-root card)
             "file_sha256" (retain! evidence-root file)))))

(defn prepare!
  "Acquire an official assertion for a selected edition and retain its evidence.
  The optional fetch function accepts an official URL and returns response bytes."
  [aozora-root evidence-root slug opts]
  (let [candidate (selected (selection aozora-root) slug)]
    (prepare-edition! evidence-root candidate
                      (preparation-context aozora-root evidence-root opts) opts)))

(defn- unavailable-reason [e]
  (let [reason (:reason (ex-data e))]
    (if (and (keyword? reason) (= "aozora" (namespace reason)))
      {:reason reason}
      (cond-> {:reason :aozora/acquisition-failed}
        reason (assoc :detail (str reason))))))

(defn prepare-batch!
  "Attempt each requested slug, or all selected editions when slugs is nil.
  Return successful :records and :unavailable entries containing :slug and :reason.
  Shared evidence is acquired once per invocation; :parallelism defaults to four."
  [aozora-root evidence-root slugs opts]
  (let [candidates (selection aozora-root)
        slugs (or slugs (sort (keys candidates)))
        context (delay (preparation-context aozora-root evidence-root opts))
        results (parallel/ordered-pmap
                 (or (:parallelism opts) 4)
                 (fn [slug]
                   (try
                     {:record (prepare-edition! evidence-root (selected candidates slug)
                                                @context opts)}
                     (catch Exception e
                       {:unavailable (assoc (unavailable-reason e) :slug slug)})))
                 slugs)]
    {:records (into [] (keep :record) results)
     :unavailable (into [] (keep :unavailable) results)}))

(defn- outcome [f]
  (try (f) {:state :aozora/available :reason nil}
       (catch Exception e
         (assoc (unavailable-reason e) :state :aozora/unavailable))))

(defn check!
  "Verify retained assertions and their live applicability. A failed current
  acquisition never falls back to a retained assertion or checkout existence.
  Shared evidence is read once per invocation; :parallelism defaults to four."
  ([aozora-root evidence-root records] (check! aozora-root evidence-root records {}))
  ([aozora-root evidence-root records opts]
   (let [candidates (delay (selection aozora-root))
         live (delay {:index (catalog-index (fetch opts catalog-url))
                      :rules (hash/sha256-bytes (fetch opts rules-url))})
         rules (into {} (for [[digest group] (group-by #(get % "rules_sha256") records)]
                          [digest (delay (retained evidence-root (first group) "rules_sha256") true)]))]
     (reduce
      (fn [results [_ group]]
        (let [index (delay (catalog-index (retained evidence-root (first group) "catalog_sha256")))]
          (into results
                (parallel/ordered-pmap
                 (or (:parallelism opts) 4)
                 (fn [record]
                   [(get record "slug")
                    (outcome
                     (fn []
                       (when (some? (get record "exception")) (refuse! :aozora/recorded-exception))
                       (let [candidate (selected @candidates (get record "slug"))
                             a (assertion @index candidate)
                             pinned (get record "source_content_hash")
                             card-bytes (retained evidence-root record "card_sha256")]
                         (check-card! card-bytes a)
                         (let [file-bytes (retained evidence-root record "file_sha256")]
                           (when-not (= pinned (bundle-hash file-bytes))
                             (refuse! :aozora/retained-edition-mismatch))
                           @(get rules (get record "rules_sha256"))
                           (when-not (= pinned (:bundle-hash (bundle/inspect-zip (:file candidate))))
                             (refuse! :aozora/checkout-edition-mismatch))
                           (when-not (= (get record "rules_sha256") (:rules @live))
                             (refuse! :aozora/rules-changed))
                           (let [current (assertion (:index @live) candidate)
                                 current-card (fetch opts (:card current))]
                             (when-not (and (= a current)
                                            (Arrays/equals ^bytes card-bytes ^bytes current-card))
                               (check-card! current-card current))
                             (let [current-file (fetch opts (:file current))]
                               (when-not (or (Arrays/equals ^bytes file-bytes ^bytes current-file)
                                             (= pinned (bundle-hash current-file)))
                                 (refuse! :aozora/current-edition-mismatch))))))))])
                 group))))
      {} (group-by #(get % "catalog_sha256") records)))))
