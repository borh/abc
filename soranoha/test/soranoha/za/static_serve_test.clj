(ns soranoha.za.static-serve-test
  "Service-withdrawal acceptance at the real serving boundary: the
  checked-in Caddyfile serves a tree exported from a chain whose head
  withdraws a work, and every assertion is an HTTP request through that
  configuration. Current-corpus surfaces (the catalog pointer, per-work
  artifact downloads) exclude the withdrawn work; historical surfaces
  (its withdrawal statement, its historical manifests) stay reachable;
  the hash resolver keeps its bytes retrievable; nothing outside the
  tree is servable. The slug's absence from current `works` itself is
  the published checker's concern, not this test's."
  (:require [babashka.fs :as fs]
            [charred.api :as json]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [soranoha.core.hash :as hash]
            [soranoha.snh.fixture :as fx]
            [soranoha.snh.sign :as sign]
            [soranoha.snh.verify :as verify]
            [soranoha.za.naming :as naming]
            [soranoha.za.serve :as serve])
  (:import (java.net ServerSocket URI)
           (java.net.http HttpClient HttpClient$Redirect HttpRequest
                          HttpResponse$BodyHandlers)))

(def ^:private slug-a "hashire_merosu_000035_1567")
(def ^:private slug-b "kumo_no_ito_000879_92")

(defn- chain-with-withdrawal!
  "Two releases then a withdrawal of slug-b; returns {:clone :withdrawal}."
  []
  (let [{:keys [clone]} (fx/make-repos!)]
    (fx/publish! clone {:admitted [slug-a slug-b]})
    (fx/publish! clone {:admitted [slug-a slug-b]
                        :selection-params {"config" "fixture" "round" 2}})
    (let [withdrawal (fx/publish-event!
                      clone (fx/event-value
                             "withdrawal"
                             [{"slug" slug-b
                               "reason_code" "takedown-request"
                               "statement" "Documented request."}]))]
      {:clone clone :withdrawal withdrawal})))

(defn- free-port []
  (with-open [socket (ServerSocket. 0)]
    (.getLocalPort socket)))

(def ^:private ^HttpClient client
  ;; redirects are followed because part of what is under test is whether a
  ;; permanent identifier lands somewhere: w3id.org sends a consumer to a path
  ;; here, and a 301 to the canonical form of that path is a success, not an
  ;; answer the consumer has to interpret
  (-> (HttpClient/newBuilder)
      (.followRedirects HttpClient$Redirect/NORMAL)
      (.build)))

(defn- http-get [port path]
  (let [request (-> (HttpRequest/newBuilder)
                    (.uri (URI/create (str "http://127.0.0.1:" port path)))
                    (.build))
        response (.send client request (HttpResponse$BodyHandlers/ofByteArray))]
    {:status (.statusCode response)
     :body (.body response)
     :cache-control (.orElse (.firstValue (.headers response) "Cache-Control")
                             nil)}))

(defn- start-caddy!
  "Run the checked-in Caddyfile over `tree` on `port`; returns the
  Process once the server answers."
  ^Process [tree port]
  (let [scratch (fs/create-temp-dir {:prefix "za-caddy"})
        ^java.util.List command ["caddy" "run" "--config"
                                 (str (fs/absolutize "config/caddy/Caddyfile"))
                                 "--adapter" "caddyfile"]
        builder (ProcessBuilder. command)
        env (.environment builder)]
    (.put env "SORANOHA_SERVE_LISTEN" (str "127.0.0.1:" port))
    (.put env "SORANOHA_SERVE_ROOT" (str tree))
    (.put env "HOME" (str scratch))
    (.put env "XDG_DATA_HOME" (str (fs/path scratch "data")))
    (.put env "XDG_CONFIG_HOME" (str (fs/path scratch "config")))
    (.redirectOutput builder (fs/file (fs/path scratch "stdout.log")))
    (.redirectError builder (fs/file (fs/path scratch "stderr.log")))
    (let [process (.start builder)]
      (loop [tries 0]
        (let [up (try (= 200 (:status (http-get port "/releases/HEAD")))
                      (catch Exception _ false))]
          (cond
            up process
            (not (.isAlive process))
            (throw (ex-info "caddy exited during startup"
                            {:log (slurp (str (fs/path scratch "stderr.log")))}))
            (< 150 tries)
            (do (.destroyForcibly process)
                (throw (ex-info "caddy did not start serving" {:port port})))
            :else (do (Thread/sleep 100) (recur (inc tries)))))))))

(defn- json-body [response]
  (json/read-json (String. ^bytes (:body response) "UTF-8")))

(defn- string-body? [response ^String needle]
  (string/includes? (String. ^bytes (:body response) "UTF-8") needle))

(deftest the-static-server-serves-the-generated-tree-under-withdrawal
  (let [{:keys [clone withdrawal]} (chain-with-withdrawal!)
        parent (fs/create-temp-dir {:prefix "za-static"})
        tree (str (fs/path parent "tree"))
        _ (fs/write-bytes (fs/path parent "outside-the-tree.txt")
                          (.getBytes "never served" "UTF-8"))
        result (serve/export-tree! {:clone clone
                                    :branch fx/branch
                                    :pinned-keys (fx/pinned-keys)
                                    :out-dir tree})
        manifest-at (fn [hex]
                      (json/read-json
                       (String. ^bytes (fs/read-all-bytes
                                        (fs/path tree (verify/manifest-path hex)))
                                "UTF-8")))
        chain (loop [hex (:head result) acc []]
                (if (= (apply str (repeat 64 "0")) hex)
                  acc
                  (recur (get (manifest-at hex) "prev_manifest")
                         (conj acc hex))))
        genesis-hex (last chain)
        artifact-hexes (fn [manifest slug]
                         (into {}
                               (for [work (get manifest "works")
                                     :when (= slug (get work "slug"))
                                     artifact (get work "artifacts")]
                                 [(get artifact "type")
                                  (verify/id->hex (get artifact "id"))])))
        current-artifacts (artifact-hexes (manifest-at (:head result)) slug-a)
        catalog-entry (->> (fs/path tree "catalog.json")
                           fs/read-all-bytes
                           (#(String. ^bytes % "UTF-8"))
                           json/read-json
                           (#(get % "works"))
                           (filter #(= slug-a (get % "slug")))
                           first)
        withdrawn-artifacts (artifact-hexes (manifest-at genesis-hex) slug-b)
        port (free-port)
        process (start-caddy! tree port)]
    (try
      (testing "current corpus: the catalog pointer serves the head manifest"
        (let [response (http-get port "/releases/latest")]
          (is (= 200 (:status response)))
          (is (= "public, max-age=60" (:cache-control response)))
          (is (= [slug-a]
                 (mapv #(get % "slug") (get (json-body response) "works"))))
          (is (= (:head result)
                 (hash/sha256-bytes (:body response))))))

      (testing "current corpus: a live work's artifacts download by slug and type"
        (doseq [[type hex] current-artifacts]
          (let [response (http-get port (str "/works/" slug-a "/" type))]
            (is (= 200 (:status response)))
            (is (= "public, max-age=60" (:cache-control response)))
            (is (= hex (hash/sha256-bytes (:body response)))))))

      (testing "current corpus: the same bytes also answer under a readable name"
        ;; saving from a browser, curl -O or wget writes the last path segment,
        ;; so the type routes above all produce a file called `tei`
        (doseq [[type hex] current-artifacts]
          (let [name (naming/filename catalog-entry type)
                response (http-get port (str "/works/" slug-a "/" name))]
            (is (= 200 (:status response)) name)
            (is (= "public, max-age=60" (:cache-control response)) name)
            (is (= hex (hash/sha256-bytes (:body response))) name))))

      (testing "bulk: a pre-built selection downloads as one archive"
        (doseq [path [(naming/corpus-bundle-path "tei")
                      (naming/corpus-bundle-path "plaintext")
                      (naming/ndc-bundle-path "other" "tei")]]
          (let [response (http-get port (str "/" path))]
            (is (= 200 (:status response)) path)
            (is (= "public, max-age=60" (:cache-control response)) path)
            (is (= [0x50 0x4b 0x03 0x04]
                   (mapv #(bit-and (int %) 0xff) (take 4 (:body response))))
                (str path " is a ZIP")))))

      (testing "current corpus: the withdrawn work is absent from every surface"
        (doseq [type (keys withdrawn-artifacts)]
          (is (= 404 (:status (http-get port (str "/works/" slug-b "/" type))))))
        (is (= 404 (:status (http-get port "/works/no_such_work/tei")))))

      (testing "historical: the withdrawal statement is slug-addressable"
        (let [response (http-get port (str "/withdrawn/" slug-b ".json"))
              event (json-body response)]
          (is (= 200 (:status response)))
          (is (= "withdrawal" (get event "kind")))
          (is (= [slug-b] (mapv #(get % "slug") (get event "entries"))))
          (is (= (verify/id->hex (:event withdrawal))
                 (hash/sha256-bytes (:body response)))))
        (is (= 404 (:status (http-get port (str "/withdrawn/" slug-a ".json"))))
            "a current work has no withdrawal statement"))

      (testing "historical: the withdrawn work stays under its manifest"
        (let [response (http-get port (str "/releases/" genesis-hex ".json"))]
          (is (= 200 (:status response)))
          (is (= "public, max-age=31536000, immutable"
                 (:cache-control response)))
          (is (contains? (set (map #(get % "slug")
                                   (get (json-body response) "works")))
                         slug-b))))

      (testing "hash resolver: withdrawn bytes stay retrievable, immutable"
        (doseq [hex (vals withdrawn-artifacts)]
          (let [response (http-get port (str "/blobs/sha256/"
                                             (subs hex 0 2) "/" hex))]
            (is (= 200 (:status response)))
            (is (= "public, max-age=31536000, immutable"
                   (:cache-control response)))
            (is (= hex (hash/sha256-bytes (:body response)))))))

      (testing "an absent digest path is a 404 no client may retain"
        (let [absent-hex (apply str (repeat 64 "f"))
              blob (http-get port (str "/blobs/sha256/ff/" absent-hex))
              manifest (http-get port (str "/releases/" absent-hex ".json"))]
          (is (= 404 (:status blob)))
          (is (nil? (:cache-control blob)))
          (is (= 404 (:status manifest)))
          (is (nil? (:cache-control manifest)))))

      (testing "the head pointer serves the verified head"
        (let [response (http-get port "/releases/HEAD")]
          (is (= 200 (:status response)))
          (is (= (:head result) (sign/parse-hex64-lf (:body response))))))

      (testing "the browse layer answers at the front door and at readable URLs"
        (doseq [path ["/" "/authors/" "/titles/" "/ndc/" "/rights" "/citation"
                      "/history" "/ns/tei" "/schemas/tei-profile.odd"
                      "/schemas/person-record.schema.json"
                      "/license/cc0-1.0.txt"
                      "/style.css" "/search.js" "/copy.js" "/search-index.json"]]
          (let [response (http-get port path)]
            (is (= 200 (:status response)) path)
            (is (= "public, max-age=60" (:cache-control response)) path)))
        (testing "and a held-back document is not served at all"
          (doseq [path ["/start-here" "/glossary" "/protocol"]]
            (is (= 404 (:status (http-get port path))) path)))
        (let [response (http-get port (str "/works/" slug-a "/"))]
          (is (= 200 (:status response)))
          (is (string-body? response (str "/works/" slug-a "/tei"))
              "a work page links its own artifacts")))

      (testing "and at every path the permanent identifier service redirects into"
        ;; w3id.org/soranoha/{ns,schemas,works}/... redirect here, so a
        ;; consumer dereferencing a namespace IRI or a record's schema id has
        ;; to arrive at the document that defines it
        (doseq [path ["/ns/tei"
                      "/schemas/person-record.schema.json"
                      "/schemas/metadata-record.schema.json"
                      (str "/works/" slug-a)]]
          (let [response (http-get port path)]
            (is (= 200 (:status response)) path)))
        (is (string-body? (http-get port "/ns/tei") "https://w3id.org/soranoha/ns/tei")
            "the namespace IRI resolves to the vocabulary that defines it"))

      (testing "the citation records answer under the work's own prefix"
        ;; no route of their own: /works/* already carries the pointer layer's
        ;; cache policy, and neither name collides with an artifact type
        (doseq [name ["citation.json" "citation.bib"]]
          (let [response (http-get port (str "/works/" slug-a "/" name))]
            (is (= 200 (:status response)) name)
            (is (= "public, max-age=60" (:cache-control response)) name)))
        (is (string-body? (http-get port (str "/works/" slug-a "/citation.bib"))
                          "@incollection{soranoha-"))
        (is (= 404 (:status (http-get port (str "/works/" slug-b "/citation.json"))))
            "a withdrawn work has no citation record either"))

      (testing "a live work is readable at an extensionless URL beside its artifacts"
        (let [response (http-get port (str "/works/" slug-a "/read"))]
          (is (= 200 (:status response)))
          (is (= "public, max-age=60" (:cache-control response)))
          (is (string-body? response "id=\"tategaki\"")
              "the vertical toggle is markup, not script")
          (is (string-body? response (:head result))
              "the reading names the release it was rendered from"))
        (is (= 404 (:status (http-get port (str "/works/" slug-b "/read"))))
            "a withdrawn work has no reading page"))

      (testing "a withdrawn work explains itself instead of returning a bare 404"
        (let [response (http-get port (str "/works/" slug-b "/"))]
          (is (= 200 (:status response)))
          (is (string-body? response "takedown-request"))
          (is (string-body? response (str "/withdrawn/" slug-b ".json")))))

      (testing "generated pages never shadow chain content"
        (doseq [[type hex] current-artifacts]
          (is (= hex (hash/sha256-bytes
                      (:body (http-get port (str "/works/" slug-a "/" type)))))))
        (is (= 404 (:status (http-get port "/works/no_such_work/")))))

      (testing "nothing outside the tree is servable"
        (is (not= 200 (:status (http-get port "/works/"))))
        (is (not= 200 (:status (http-get port "/%2e%2e/outside-the-tree.txt"))))
        (is (not= 200 (:status (http-get
                                port
                                "/blobs/sha256/%2e%2e/%2e%2e/%2e%2e/outside-the-tree.txt")))))
      (finally
        (.destroyForcibly process)))))
