(ns soranoha.za.docs-test
  "The contract between the served documents and the routes they resolve to.

  Whether a link target exists at all is checked by
  `scripts/docs-link-check.py`, which runs against the repository. This suite
  runs against a copy of the `soranoha` directory, so it cannot see a README
  or a test file a document points at. What it can check is the part that
  belongs to serving: every served file is present and titled, no two entries
  claim one route, and a link either becomes a route this site publishes or
  becomes the repository path it names."
  (:require [babashka.fs :as fs]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [soranoha.za.docs :as docs]
            [soranoha.za.markdown :as markdown]))

(defn- text-of [{:keys [path]}] (docs/read-text path))

(defn- headings [text]
  (into #{}
        (keep #(some-> (second (re-matches #"#{1,6} (.*)" %)) markdown/slug))
        (string/split-lines text)))

(defn- links [text]
  (map (fn [[_ _ target]] target) (re-seq #"\[([^\]]+)\]\(([^)]+)\)" text)))

(def ^:private served
  "Every document the site renders, however its page is assembled."
  (concat docs/documents docs/generated))

(def ^:private routes
  (into #{} (map :route) (concat served docs/verbatim)))

(deftest a-generated-page-starts-its-document-at-a-heading-it-has
  (doseq [{:keys [route path from]} docs/generated]
    (testing route
      (is (contains? (headings (docs/read-text path)) from)
          "the page carries the document from this heading onward"))))

(deftest every-served-file-is-present-and-titled
  (doseq [{:keys [route path]} served]
    (testing route
      (is (fs/exists? (fs/file (docs/root) path)) path)
      (is (string? (markdown/title (docs/read-text path))))))
  (doseq [{:keys [route path]} docs/verbatim]
    (testing route
      (is (fs/exists? (fs/file (docs/root) path)) path))))

(deftest routes-are-distinct-and-usable-as-paths
  (is (= (count routes) (+ (count served) (count docs/verbatim)))
      "two entries would otherwise overwrite each other in the serving tree")
  (doseq [route routes]
    (is (re-matches #"[a-z0-9][a-z0-9./-]*" route) route)
    (is (not (string/includes? route "..")) route)))

(deftest every-link-in-a-served-document-resolves
  (let [by-route (into {} (map (juxt :route text-of)) served)
        sections (update-vals by-route headings)]
    (doseq [{:keys [route path] :as document} served
            target (links (text-of document))
            :let [resolved (docs/resolve-link path target)]]
      (testing (str route " -> " target)
        (if (string? resolved)
          ;; a repository file the site does not serve: the page names the
          ;; path instead of linking, and the path stays repository-relative
          ;; so a reader can find it in a checkout
          (is (not (string/starts-with? resolved "/")) resolved)
          (let [href (:href resolved)
                [_ named fragment] (re-matches #"/([^#]*)(?:#(.*))?" href)
                [_ own-fragment] (re-matches #"#(.*)" href)]
            (cond
              named
              (do (is (contains? routes named) href)
                  (when (and fragment (contains? sections named))
                    (is (contains? (get sections named) fragment) href)))

              own-fragment
              (is (contains? (get sections route) own-fragment) href)

              :else
              (is (re-find #"^(https?://|mailto:)" href) href))))))))
