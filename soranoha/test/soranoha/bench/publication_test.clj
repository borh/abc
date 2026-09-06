(ns soranoha.bench.publication-test
  (:require [babashka.fs :as fs]
            [charred.api :as json]
            [clojure.string :as string]
            [clojure.test :refer [deftest is]]
            [soranoha.assessment.aozora :as aozora]
            [soranoha.assessment.records :as records]
            [soranoha.bench.publication :as publication]
            [soranoha.core.hash :as hash]
            [soranoha.main :as main]
            [soranoha.za.corpus :as corpus]))

(deftest recorded-responses-fail-experiments-separately-from-http-failures
  (let [root (fs/create-temp-dir {:prefix "recorded-http"})
        bytes (.getBytes "body" "UTF-8")
        digest (hash/sha256-bytes bytes)
        path (fs/path root digest)]
    (try
      (fs/write-bytes path bytes)
      (let [{:keys [fetch assert-complete!]}
            (publication/recorded-provider root {"body" {"sha256" digest}
                                                 "missing" {"status" 404}
                                                 "redirect" {"status" 301}})]
        (is (= "body" (String. ^bytes (fetch "body") "UTF-8")))
        (doseq [[url status] [["missing" 404] ["redirect" 301]]]
          (is (= status (try (fetch url) (catch Exception e (:status (ex-data e)))))))
        (is (nil? (assert-complete!)))
        (is (thrown? Exception (fetch "not-recorded")))
        (is (thrown-with-msg? Exception #"Missing or changed" (assert-complete!))))
      (let [{:keys [fetch assert-complete!]}
            (publication/recorded-provider root {"body" {"sha256" digest}})]
        (spit (str path) "corrupt")
        (is (thrown-with-msg? Exception #"changed" (fetch "body")))
        (is (thrown? Exception (assert-complete!))))
      (is (thrown-with-msg? Exception #"Invalid recorded body"
                            (publication/recorded-provider root {"body" {"sha256" digest}})))
      (is (thrown-with-msg? Exception #"Invalid recorded response"
                            (publication/recorded-provider root {"body" {"sha256" "../body"}})))
      (finally (fs/delete-tree root)))))

(deftest real-publication-replay-keeps-source-and-origins-isolated
  (let [work {:work-id "000100" :person-id "000001" :card "000001"
              :book "100" :n "1001" :title "fixture" :text "Edition text.\n"}
        source (corpus/init-corpus! [work])
        first-commit (main/source-provenance! source)
        dir (fs/create-temp-dir {:prefix "publication-replay"})
        evidence (fs/path dir "evidence")
        card "https://www.aozora.gr.jp/cards/000001/card100.html"
        file "https://www.aozora.gr.jp/cards/000001/files/100_ruby_1001.zip"
        catalog (fs/path dir "catalog.zip")
        _ (corpus/write-zip! catalog [["catalog.csv"
                                       (str "作品ID,作品著作権フラグ,人物著作権フラグ,図書カードURL,テキストファイルURL\n"
                                            "000100,なし,なし," card "," file "\n")]])
        responses {aozora/catalog-url (fs/read-all-bytes catalog)
                   aozora/rules-url (.getBytes "Published rules" "UTF-8")
                   card (.getBytes "<h1>図書カード：No.100</h1><a href='./files/100_ruby_1001.zip'>Download</a>" "UTF-8")
                   file (fs/read-all-bytes (corpus/work-zip-path source work))}
        date "2026-09-06"
        record (aozora/prepare! source evidence (corpus/work-slug work)
                                {:fetch responses :observed-at date :decision-date date})
        round {"as_of" date "responses" (update-vals responses #(hash-map "sha256" (hash/sha256-bytes %)))}
        live-observation (aozora/check! source evidence [record] {:fetch responses})
        replay-observation (aozora/check! source evidence [record]
                                          {:fetch (:fetch (publication/recorded-provider evidence (get round "responses")))})
        _ (spit (str (fs/path source "unrelated.txt")) "unrelated commit")
        unavailable-commit (corpus/commit-corpus! source)
        added (assoc work :work-id "000200" :book "200" :n "2001")
        _ (corpus/write-work! source added)
        _ (corpus/write-catalog! source [work added])
        added-commit (corpus/commit-corpus! source)
        _ (corpus/write-work! source (assoc work :text "Changed edition.\n"))
        edited-commit (corpus/commit-corpus! source)
        _ (corpus/delete-work! source work)
        _ (corpus/write-catalog! source [added])
        last-commit (corpus/commit-corpus! source)
        commits [first-commit unavailable-commit added-commit edited-commit last-commit]
        write! (fn [name value] (let [path (str (fs/path dir name))] (spit path value) path))
        input {:repo source :from first-commit :to last-commit
               :out (str (fs/path dir "run")) :assets-root "."
               :clj-toolchain-id corpus/fixture-toolchain :evidence-root (str evidence)
               :assessment-source (write! "source.json" (String. ^bytes (:bytes (records/encode
                                                                                 (assoc records/empty-source "reliances" [record]))) "UTF-8"))
               :policy (write! "policy.edn" "{:rights-publication :assessment-required}")
               :recording (write! "recording.json" (json/write-json-str
                                                    {"revisions" (assoc (zipmap commits (repeat "available"))
                                                                        unavailable-commit "unavailable")
                                                     "rounds" {"available" round
                                                               "unavailable" (assoc-in round ["responses" file] {"status" 404})}}))}]
    (try
      (is (= live-observation replay-observation))
      (with-redefs-fn {#'main/build-stages (constantly (dissoc corpus/stage-set :fidelity))}
        #(is (= 5 (:revisions (publication/replay! input)))))
      (let [rows (mapv json/read-json (string/split-lines
                                       (slurp (str (fs/path (:out input) "measurements.jsonl")))))]
        (is (= 6 (count rows)))
        (is (= commits (mapv #(get % "commit") (rest rows))))
        (is (every? #(= "published" (get-in % ["result" "outcome"])) (rest rows)))
        (is (= [1 0 1 0 0] (mapv #(get-in % ["works-delta" "added"]) (rest rows))))
        (is (= [0 1 0 1 0] (mapv #(get-in % ["works-delta" "removed"]) (rest rows))))
        (is (pos? (get-in rows [1 "executed-stages"])))
        (is (every? zero? (map #(get % "executed-stages") (drop 2 rows)))))
      (is (= last-commit (main/source-provenance! source)))
      (is (thrown? java.nio.file.FileAlreadyExistsException (publication/replay! input)))
      (finally (fs/delete-tree dir) (fs/delete-tree source)))))
