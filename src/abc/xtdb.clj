(ns abc.xtdb
  (:require
   [clojure.java.io :as io]
   [me.raynes.fs :as fs]
   [xtdb.api :as xt]
   [taoensso.timbre :as timbre]))

(defonce default-node (atom nil))

(defn stop!
  ([] (when-let [a-node @default-node]
        (reset! default-node nil)
        (.close a-node)))
  ([a-node] (.close a-node)))

(defn delete-db!
  [db-path]
  (when (fs/exists? db-path)
    (fs/delete-dir db-path)))

(defn start! [& {:keys [db-path clean?]
                 :or   {db-path "data/dev" clean? false}}]
  (when clean? (delete-db! db-path))
  (letfn [(kv-store [dir]
            {:kv-store {:xtdb/module 'xtdb.rocksdb/->kv-store
                        :db-dir      (io/file dir)
                        :sync?       true}})]
    (xt/start-node
     {:xtdb/tx-log         (kv-store (str db-path "/tx-log"))
      :xtdb/document-store (kv-store (str db-path "/doc-store"))
      :xtdb/index-store    (kv-store (str db-path "/index-store"))})))

(defn node []
  (or @default-node
      (locking default-node
        (or @default-node
            (reset! default-node (start!))))))

(defn submit-tx
  ([tx]
   (let [a-node (node)]
     (xt/await-tx a-node (xt/submit-tx a-node tx))))
  ([a-node tx] (xt/await-tx a-node (xt/submit-tx a-node tx))))

(defn q
  ([query] (xt/q (xt/db (node)) query))
  ([a-node query] (xt/q (xt/db a-node) query))
  ([a-node query opts] (xt/q (xt/db a-node) query opts)))

(defn all-works [& {:keys [node]}]
  (let [a-node (or node (abc.xtdb/node))]
    (into #{}
          (map first)
          (xt/q (xt/db a-node)
                '{:find  [?id]
                  :where [[?id :abc.aozora/work-id]]}))))

(defn all-persons [& {:keys [node]}]
  (let [a-node (or node (abc.xtdb/node))]
    (into #{}
          (map first)
          (xt/q (xt/db a-node)
                '{:find  [?id]
                  :where [[?id :abc.aozora/person-id]]}))))

(defn work-query [work-name author-last-name & {:keys [node]}]
  (let [a-node (or node (abc.xtdb/node))
        results (xt/q (xt/db a-node)
                      '{:find  [?work ?author]
                        #_[(eql/project ?work [:abc.aozora/title
                                               {:abc.aozora/author [:abc.aozora/name]}])]
                        :where [[?author :abc.aozora/family-name family-name]
                                [?work :abc.aozora/author ?author]
                                [?work :abc.aozora/title title]]
                        :in    [family-name title]}
                      author-last-name work-name)
        work-map (ffirst results)
        authors (mapv second results)]
    (assoc work-map :abc.aozora/authors authors)))

(defn work-id-to-url [id & {:keys [node]}]
  (let [a-node (or node (abc.xtdb/node))]
    (first
     (sequence
      (comp
       (map first)
       (filter (fn [m] (= "text/plain" (:dcterms/format m))))
       (map (fn [m] (select-keys m [:abc.aozora/url :abc.aozora/encoding]))))
      (xt/q (xt/db a-node)
            '{:find  [?sources]
              :where [[e :xt/id work-id]
                      [e :abc.aozora/sources ?sources]]
              :in    [work-id]}
            id)))))
