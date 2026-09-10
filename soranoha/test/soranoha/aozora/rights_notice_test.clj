(ns soranoha.aozora.rights-notice-test
  (:require [clojure.test :refer [deftest is testing]]
            [soranoha.aozora.rights-notice :as notice]))

(defn- rows [& flags]
  (mapv #(hash-map "作品著作権フラグ" %) flags))

(defn- never-read []
  (throw (AssertionError. "the archive was opened for a work the flag decided")))

(deftest an-expired-work-is-decided-without-opening-its-archive
  (is (= {:standing "public-domain"} (notice/standing (rows "なし") never-read)))
  (is (= {:standing "public-domain"}
         (notice/standing (rows "なし" "なし" "なし") never-read))
      "one work filed under several contributor cards still has one standing"))

(deftest the-licence-url-decides-a-subsisting-work
  (testing "each form Aozora Bunko's notices actually carry"
    (doseq [[url expected]
            [["http://creativecommons.org/licenses/by/2.1/jp/" "CC-BY-2.1-JP"]
             ["https://creativecommons.org/licenses/by/3.0/" "CC-BY-3.0"]
             ["https://creativecommons.org/licenses/by/4.0/" "CC-BY-4.0"]]]
      (is (= {:standing expected}
             (notice/standing (rows "あり")
                              (constantly (str "※本作品は「クリエイティブ・コモンズ 表示」"
                                               "（" url "）の下に提供されています。"))))
          url))))

(deftest a-deed-link-does-not-mint-a-jurisdiction
  ;; `licenses/by/3.0/deed.ja` ends in two letters that look like a port.
  ;; Reading them as one would publish CC-BY-3.0-DE, a licence that does not
  ;; exist, and would do it silently.
  (is (= {:standing "CC-BY-3.0"}
         (notice/standing (rows "あり")
                          (constantly "https://creativecommons.org/licenses/by/3.0/deed.ja")))))

(deftest a-notice-with-no-url-is-still-a-notice
  ;; 058806_001955 and 058807_001955 state the licence in prose and link
  ;; nothing. Refusing them would drop works whose terms are stated plainly.
  (is (= {:standing "CC-BY-2.1-JP"}
         (notice/standing (rows "あり")
                          (constantly (str "この作品は、クリエイティブ・コモンズ"
                                           "「表示 2.1 日本」でライセンスされています。")))))
  (is (= {:standing "CC-BY-4.0"}
         (notice/standing (rows "あり")
                          (constantly "クリエイティブ・コモンズ 表示 4.0 国際 ライセンス")))
      "国際 names an unported deed, which carries no jurisdiction"))

(deftest terms-the-release-cannot-honour-are-refused
  (doseq [[url licence]
          [["https://creativecommons.org/licenses/by-nc/4.0/" "CC-BY-NC-4.0"]
           ["http://creativecommons.org/licenses/by-nd/2.1/jp/" "CC-BY-ND-2.1-JP"]
           ["http://creativecommons.org/licenses/by-nc-nd/2.1/jp/" "CC-BY-NC-ND-2.1-JP"]
           ["https://creativecommons.org/licenses/by-sa/3.0/" "CC-BY-SA-3.0"]]]
    (is (= {:refused :restricted-licence :licence licence}
           (notice/standing (rows "あり") (constantly url)))
        (str url " names terms the site's own grant contradicts"))))

(deftest the-prose-form-refuses-restricted-terms-too
  (is (= {:refused :restricted-licence :licence "CC-BY-NC-ND-2.1-JP"}
         (notice/standing (rows "あり")
                          (constantly "クリエイティブ・コモンズ「表示-非営利-改変禁止 2.1 日本」")))
      "element order in the identifier is canonical, not the notice's"))

(deftest a-subsisting-work-with-no-licence-is-refused
  (is (= {:refused :unstated-licence}
         (notice/standing (rows "あり") (constantly "底本：ある本\n入力：だれか")))
      "no notice means no granted terms, so there is nothing to publish under")
  (is (= {:refused :unstated-licence}
         (notice/standing (rows "あり") (constantly "")))))

(deftest a-licence-that-does-not-grant-attribution-terms-is-not-read-as-one
  ;; CC0 is not a licence a work is published under here; a colophon naming
  ;; some other creativecommons.org path must not be read as a BY grant.
  (is (= {:refused :unstated-licence}
         (notice/standing (rows "あり")
                          (constantly "https://creativecommons.org/licenses/sampling/1.0/")))))

(deftest rows-that-disagree-about-the-flag-are-refused
  (is (= {:refused :inconsistent-copyright-flag :flags ["あり" "なし"]}
         (notice/standing (rows "あり" "なし") never-read))
      "one row is wrong and which one is not knowable from here"))

(deftest a-flag-value-outside-the-vocabulary-is-refused
  (is (= {:refused :unknown-copyright-flag :flags ["不明"]}
         (notice/standing (rows "不明") never-read))))
