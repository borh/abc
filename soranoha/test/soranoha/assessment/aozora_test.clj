(ns soranoha.assessment.aozora-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [soranoha.assessment.aozora :as aozora]
            [soranoha.za.corpus :as corpus]))

(def work {:work-id "000100" :person-id "000001" :card "000001"
           :book "100" :n "1001" :title "fixture" :text "Edition text.\n"})
(def card-url "https://www.aozora.gr.jp/cards/000001/card100.html")
(def file-url "https://www.aozora.gr.jp/cards/000001/files/100_ruby_1001.zip")
(defn- utf8-bytes [s] (.getBytes ^String s "UTF-8"))
(defn- catalog-bytes [dir flag extra]
  (let [p (fs/path dir "catalog.zip")]
    (corpus/write-zip! p [["catalog.csv"
                           (str "作品ID,作品著作権フラグ,人物著作権フラグ,図書カードURL,テキストファイルURL\n"
                                "000100," flag ",なし," card-url "," file-url "\n" extra)]])
    (fs/read-all-bytes p)))

(deftest exact-edition-current-reliance
  (let [root (corpus/init-corpus! [work])
        dir (fs/create-temp-dir {:prefix "aozora-reliance"})
        evidence (fs/path dir "evidence")
        responses (atom {aozora/catalog-url (catalog-bytes dir "なし" "")
                         aozora/rules-url (utf8-bytes "Published rules")
                         card-url (utf8-bytes "<h1>図書カード：No.100</h1><style>div.copyright{}</style><a href='./files/100_ruby_1001.zip'>Download</a>")
                         file-url (fs/read-all-bytes (corpus/work-zip-path root work))})
        fetch (fn [url] (or (get @responses url)
                            (throw (ex-info "HTTP404" {:reason :aozora/http-status}))))
        opts {:fetch fetch :observed-at "2026-09-06" :decision-date "2026-09-06"}
        slug (corpus/work-slug work)]
    (try
      (let [record (aozora/prepare! root evidence slug opts)
            baseline @responses
            check #(get (aozora/check! root evidence [record] {:fetch fetch}) slug)]
        (is (= {:state :aozora/available :reason nil} (check)))
        (testing "fresh identical bytes reuse validated content without skipping acquisition"
          (let [parse-card @#'aozora/check-card!
                inspect-bundle @#'aozora/bundle-hash
                measure (fn []
                          (let [calls (atom {:card 0 :bundle 0 :fetches {}})]
                            (with-redefs [aozora/check-card!
                                          (fn [bytes assertion]
                                            (swap! calls update :card inc)
                                            (parse-card bytes assertion))
                                          aozora/bundle-hash
                                          (fn [& args]
                                            (swap! calls update :bundle inc)
                                            (apply inspect-bundle args))]
                              (is (= {:state :aozora/available :reason nil}
                                     (get (aozora/check!
                                           root evidence [record]
                                           {:fetch (fn [url]
                                                     (swap! calls update-in [:fetches url] (fnil inc 0))
                                                     (fetch url))}) slug))))
                            (is (= {aozora/catalog-url 1 aozora/rules-url 1 card-url 1 file-url 1}
                                   (:fetches @calls)))
                            (select-keys @calls [:card :bundle])))]
            (is (= {:card 1 :bundle 1} (measure)))
            (testing "changed valid card bytes require a new parse"
              (swap! responses update card-url
                     #(utf8-bytes (str (String. ^bytes % "UTF-8") "\n")))
              (is (= {:card 2 :bundle 1} (measure))))
            (testing "a repacked ZIP requires inspection even when its bundle identity is unchanged"
              (corpus/write-work! root work :entry-time 1200000000000)
              (reset! responses (assoc baseline file-url (fs/read-all-bytes (corpus/work-zip-path root work))))
              (is (= {:card 1 :bundle 2} (measure))))
            (reset! responses baseline)))
        (testing "transient acquisition retries fetch fresh evidence"
          (doseq [failure [(java.io.IOException. "Temporary network failure")
                           (ex-info "Temporary HTTP failure" {:reason :aozora/http-status :status 503})]]
            (let [calls (atom 0)
                  retry-fetch (fn [url]
                                (if (and (= url aozora/catalog-url)
                                         (= 1 (swap! calls inc)))
                                  (throw failure)
                                  (fetch url)))]
              (is (= {:state :aozora/available :reason nil}
                     (get (aozora/check! root evidence [record] {:fetch retry-fetch}) slug)))
              (is (= 2 @calls)))))
        (testing "persistent transient failures have a bounded acquisition budget"
          (doseq [failure [(java.io.IOException. "Network unavailable")
                           (ex-info "Service unavailable" {:reason :aozora/http-status :status 503})]]
            (let [calls (atom 0)]
              (is (= :aozora/unavailable
                     (:state (get (aozora/check! root evidence [record]
                                                 {:fetch (fn [_] (swap! calls inc) (throw failure))}) slug))))
              (is (= 3 @calls)))))
        (testing "permanent failures and invalid bytes are not retried"
          (doseq [response [(fn [] (throw (ex-info "Not found" {:reason :aozora/http-status :status 404})))
                            (fn [] (throw (IllegalArgumentException. "Invalid caller value")))
                            (fn [] (byte-array 0))]]
            (let [calls (atom 0)]
              (is (= :aozora/unavailable
                     (:state (get (aozora/check! root evidence [record]
                                                 {:fetch (fn [_] (swap! calls inc) (response))}) slug))))
              (is (= 1 @calls)))))
        (testing "malformed unrelated links do not hide the edition link"
          (swap! responses assoc card-url
                 (utf8-bytes "<a href='https://example.org/%'>Unrelated</a><h1>図書カード：No.100</h1><a href=' ./files/100_ruby_1001.zip '>Download</a>"))
          (is (= :aozora/available (:state (check))))
          (is (= slug (get (aozora/prepare! root evidence slug opts) "slug"))))
        (testing "unrelated catalog rows and ZIP repacking preserve reliance"
          (swap! responses assoc aozora/catalog-url
                 (catalog-bytes dir "なし" "999999,あり,なし,https://www.aozora.gr.jp/cards/1/card1.html,https://www.aozora.gr.jp/cards/1/files/x.zip\n"))
          (corpus/write-work! root work :entry-time 1200000000000)
          (swap! responses assoc file-url (fs/read-all-bytes (corpus/work-zip-path root work)))
          (is (= :aozora/available (:state (check)))))
        (doseq [[label url replacement expected]
                [["protected work despite expired author" aozora/catalog-url (catalog-bytes dir "あり" "") :aozora/not-classified-expired]
                 ["unknown classification" aozora/catalog-url (catalog-bytes dir "" "") :aozora/not-classified-expired]
                 ["conflicting work rows" aozora/catalog-url (catalog-bytes dir "なし" (str "000100,あり,なし," card-url "," file-url "\n")) :aozora/not-classified-expired]
                 ["rules changed" aozora/rules-url (utf8-bytes "Changed rules") :aozora/rules-changed]
                 ["card says protected" card-url (utf8-bytes "<h1>図書カード：No.100</h1><div class='copyright'>＊著作権存続＊</div><a href='./files/100_ruby_1001.zip'>Download</a>") :aozora/protected-card]
                 ["wrong card" card-url (utf8-bytes "<h1>図書カード：No.1000</h1><a href='./files/100_ruby_1001.zip'>Download</a>") :aozora/card-identity-mismatch]
                 ["unlinked file" card-url (utf8-bytes "<h1>図書カード：No.100</h1>") :aozora/missing-card-file-link]
                 ["only malformed links" card-url (utf8-bytes "<h1>図書カード：No.100</h1><a href='https://example.org/%'>Unrelated</a>") :aozora/missing-card-file-link]
                 ["stale Git file with live404" file-url nil :aozora/http-status]
                 ["catalog network failure" aozora/catalog-url nil :aozora/http-status]]]
          (testing label
            (reset! responses (assoc baseline url replacement))
            (is (= {:state :aozora/unavailable :reason expected} (check)))))
        (testing "preparation refuses the protected classification"
          (reset! responses (assoc baseline aozora/catalog-url (catalog-bytes dir "あり" "")))
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not-classified-expired"
                                (aozora/prepare! root evidence slug opts))))
        (testing "official catalog removal wins over retained Git presence"
          (let [p (fs/path dir "removed.zip")]
            (corpus/write-zip! p [["catalog.csv" "作品ID,作品著作権フラグ,人物著作権フラグ,図書カードURL,テキストファイルURL\n"]])
            (reset! responses (assoc baseline aozora/catalog-url (fs/read-all-bytes p)))
            (is (= {:state :aozora/unavailable :reason :aozora/missing-current-work} (check)))))
        (testing "changed source content cannot use old reliance"
          (reset! responses baseline)
          (corpus/write-work! root (assoc work :text "Changed edition\n"))
          (swap! responses assoc file-url (fs/read-all-bytes (corpus/work-zip-path root work)))
          (is (= {:state :aozora/unavailable :reason :aozora/checkout-edition-mismatch} (check)))
          (corpus/write-work! root work)
          (is (= {:state :aozora/unavailable :reason :aozora/current-edition-mismatch} (check))))
        (testing "retained response corruption cannot be replaced by live bytes"
          (corpus/write-work! root work)
          (reset! responses baseline)
          (spit (str (fs/path evidence (get record "card_sha256"))) "corrupt")
          (let [calls (atom 0)]
            (is (= {:state :aozora/unavailable :reason :aozora/evidence-digest-mismatch}
                   (get (aozora/check! root evidence [record]
                                       {:fetch (fn [url] (swap! calls inc) (fetch url))}) slug)))
            (is (zero? @calls)))))
      (finally (fs/delete-tree root) (fs/delete-tree dir)))))

(deftest batch-acquisition-shares-inputs-and-isolates-failures
  (let [other (assoc work :work-id "000101" :book "101")
        root (corpus/init-corpus! [work other])
        dir (fs/create-temp-dir {:prefix "aozora-batch"})
        evidence (fs/path dir "evidence")
        other-card "https://www.aozora.gr.jp/cards/000001/card101.html"
        other-file "https://www.aozora.gr.jp/cards/000001/files/101_ruby_1001.zip"
        slugs (mapv corpus/work-slug [work other])
        responses (atom {aozora/catalog-url (catalog-bytes dir "なし" (str "000101,なし,なし," other-card "," other-file "\n"))
                         aozora/rules-url (utf8-bytes "Published rules")
                         card-url (utf8-bytes (str "<h1>図書カード：No.100</h1><a href='" file-url "'>Download</a>"))
                         file-url (fs/read-all-bytes (corpus/work-zip-path root work))
                         other-card (utf8-bytes (str "<h1>図書カード：No.101</h1><a href='" other-file "'>Download</a>"))
                         other-file (fs/read-all-bytes (corpus/work-zip-path root other))})
        baseline @responses
        calls (atom {})
        fetch (fn [url]
                (swap! calls update url (fnil inc 0))
                (or (get @responses url) (throw (ex-info "HTTP404" {:reason :aozora/http-status}))))
        opts {:fetch fetch :parallelism 2}
        parsed (atom 0)
        parse-catalog @#'aozora/catalog-rows]
    (try
      (with-redefs [aozora/catalog-rows (fn [bytes] (swap! parsed inc) (parse-catalog bytes))]
        (let [{:keys [records unavailable]} (aozora/prepare-batch! root evidence nil opts)]
          (is (= slugs (mapv #(get % "slug") records)))
          (is (empty? unavailable))
          (is (= 1 @parsed))
          (is (= 1 (get @calls aozora/catalog-url)))
          (is (= 1 (get @calls aozora/rules-url)))
          (testing "each check verifies common retained and live evidence once"
            (reset! calls {})
            (reset! parsed 0)
            (is (every? #(= :aozora/available (:state %)) (vals (aozora/check! root evidence records opts))))
            (is (= 2 @parsed))
            (is (= 1 (get @calls aozora/catalog-url)))
            (is (= 1 (get @calls aozora/rules-url)))
            (swap! responses assoc aozora/rules-url (utf8-bytes "Changed rules"))
            (is (every? #(= :aozora/rules-changed (:reason %)) (vals (aozora/check! root evidence records opts))))
            (is (= 2 (get @calls aozora/catalog-url)))
            (is (= 2 (get @calls aozora/rules-url))))
          (testing "exceptions remain unavailable without live acquisition"
            (reset! calls {})
            (let [exception-records (mapv #(assoc % "exception" "Review required") records)]
              (is (every? #(= :aozora/recorded-exception (:reason %))
                          (vals (aozora/check! root evidence exception-records opts))))
              (is (= "Review required" (get (first exception-records) "exception")))
              (is (empty? @calls))))
          (testing "a failed edition does not prevent later successful acquisition"
            (reset! responses (dissoc baseline file-url))
            (let [result (aozora/prepare-batch! root evidence slugs opts)]
              (is (= [(second slugs)] (mapv #(get % "slug") (:records result))))
              (is (= [{:slug (first slugs) :reason :aozora/http-status}] (:unavailable result)))))
          (testing "protected classifications cannot become declarations"
            (reset! responses (assoc baseline aozora/catalog-url (catalog-bytes dir "あり" (str "000101,なし,なし," other-card "," other-file "\n"))))
            (let [result (aozora/prepare-batch! root evidence slugs opts)]
              (is (= [(second slugs)] (mapv #(get % "slug") (:records result))))
              (is (= [{:slug (first slugs) :reason :aozora/not-classified-expired}] (:unavailable result)))))
          (testing "per-record retained failure leaves other editions checkable"
            (reset! responses baseline)
            (is (= {(first slugs) {:state :aozora/unavailable :reason :aozora/invalid-evidence-digest}
                    (second slugs) {:state :aozora/available :reason nil}}
                   (aozora/check! root evidence
                                  [(assoc (first records) "card_sha256" "invalid") (second records)] opts))))
          (testing "corrupt shared retained catalog refuses all dependants before acquisition"
            (reset! calls {})
            (spit (str (fs/path evidence (get (first records) "catalog_sha256"))) "corrupt")
            (is (every? #(= :aozora/evidence-digest-mismatch (:reason %))
                        (vals (aozora/check! root evidence records opts))))
            (is (empty? @calls)))
          (testing "shared acquisition failure is reported for every requested edition"
            (reset! calls {})
            (swap! responses dissoc aozora/catalog-url)
            (let [result (aozora/prepare-batch! root evidence slugs opts)]
              (is (empty? (:records result)))
              (is (= (mapv #(hash-map :slug % :reason :aozora/http-status) slugs) (:unavailable result)))
              (is (= 1 (get @calls aozora/catalog-url)))))))
      (finally (fs/delete-tree root) (fs/delete-tree dir)))))

(deftest observation-wire-boundary-preserves-domain-and-diagnostic-distinction
  (doseq [state ["available" :available :assessment/available :validation/passed]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Invalid Aozora Bunko observation state"
                          (aozora/observation->wire {:state state :reason nil}))))
  (doseq [reason [:assessment/missing-selected-work :validation/failed :aozora/unknown]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Invalid Aozora Bunko unavailability reason"
                          (aozora/observation->wire {:state :aozora/unavailable :reason reason}))))
  (is (thrown? clojure.lang.ExceptionInfo
               (aozora/observation->wire {:state :aozora/available :reason :assessment/stale-premise})))
  (is (thrown? clojure.lang.ExceptionInfo
               (aozora/observation->wire {:state :aozora/unavailable :reason :aozora/acquisition-failed
                                          :detail :assessment/stale-premise})))
  (let [observation (#'aozora/outcome
                     #(throw (ex-info "Invalid source archive" {:reason :primary-text-count})))]
    (is (= :aozora/acquisition-failed (:reason observation)))
    (is (= {"state" "unavailable" "reason" ":primary-text-count"}
           (aozora/observation->wire observation)))))
