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
                            (throw (ex-info "HTTP404" {:reason "http-status"}))))
        opts {:fetch fetch :observed-at "2026-09-06" :decision-date "2026-09-06"}
        slug (corpus/work-slug work)]
    (try
      (let [record (aozora/prepare! root evidence slug opts)
            baseline @responses
            check #(get (aozora/check! root evidence [record] {:fetch fetch}) slug)]
        (is (= {:state "available" :reason nil} (check)))
        (testing "unrelated catalog rows and ZIP repacking preserve reliance"
          (swap! responses assoc aozora/catalog-url
                 (catalog-bytes dir "なし" "999999,あり,なし,https://www.aozora.gr.jp/cards/1/card1.html,https://www.aozora.gr.jp/cards/1/files/x.zip\n"))
          (corpus/write-work! root work :entry-time 1200000000000)
          (swap! responses assoc file-url (fs/read-all-bytes (corpus/work-zip-path root work)))
          (is (= "available" (:state (check)))))
        (doseq [[label url replacement expected]
                [["protected work despite expired author" aozora/catalog-url (catalog-bytes dir "あり" "") "not-classified-expired"]
                 ["unknown classification" aozora/catalog-url (catalog-bytes dir "" "") "not-classified-expired"]
                 ["conflicting work rows" aozora/catalog-url (catalog-bytes dir "なし" (str "000100,あり,なし," card-url "," file-url "\n")) "not-classified-expired"]
                 ["rules changed" aozora/rules-url (utf8-bytes "Changed rules") "rules-changed"]
                 ["card says protected" card-url (utf8-bytes "<h1>図書カード：No.100</h1><div class='copyright'>＊著作権存続＊</div><a href='./files/100_ruby_1001.zip'>Download</a>") "protected-card"]
                 ["wrong card" card-url (utf8-bytes "<h1>図書カード：No.1000</h1><a href='./files/100_ruby_1001.zip'>Download</a>") "card-identity-mismatch"]
                 ["unlinked file" card-url (utf8-bytes "<h1>図書カード：No.100</h1>") "missing-card-file-link"]
                 ["stale Git file with live404" file-url nil "http-status"]
                 ["catalog network failure" aozora/catalog-url nil "http-status"]]]
          (testing label
            (reset! responses (assoc baseline url replacement))
            (is (= {:state "unavailable" :reason expected} (check)))))
        (testing "preparation refuses the protected classification"
          (reset! responses (assoc baseline aozora/catalog-url (catalog-bytes dir "あり" "")))
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not-classified-expired"
                                (aozora/prepare! root evidence slug opts))))
        (testing "official catalog removal wins over retained Git presence"
          (let [p (fs/path dir "removed.zip")]
            (corpus/write-zip! p [["catalog.csv" "作品ID,作品著作権フラグ,人物著作権フラグ,図書カードURL,テキストファイルURL\n"]])
            (reset! responses (assoc baseline aozora/catalog-url (fs/read-all-bytes p)))
            (is (= {:state "unavailable" :reason "missing-current-work"} (check)))))
        (testing "changed source content cannot use old reliance"
          (reset! responses baseline)
          (corpus/write-work! root (assoc work :text "Changed edition\n"))
          (swap! responses assoc file-url (fs/read-all-bytes (corpus/work-zip-path root work)))
          (is (= {:state "unavailable" :reason "checkout-edition-mismatch"} (check)))
          (corpus/write-work! root work)
          (is (= {:state "unavailable" :reason "current-edition-mismatch"} (check))))
        (testing "retained response corruption cannot be replaced by live bytes"
          (corpus/write-work! root work)
          (reset! responses baseline)
          (spit (str (fs/path evidence (get record "card_sha256"))) "corrupt")
          (let [calls (atom 0)]
            (is (= {:state "unavailable" :reason "evidence-digest-mismatch"}
                   (get (aozora/check! root evidence [record]
                                       {:fetch (fn [url] (swap! calls inc) (fetch url))}) slug)))
            (is (zero? @calls)))))
      (finally (fs/delete-tree root) (fs/delete-tree dir)))))
