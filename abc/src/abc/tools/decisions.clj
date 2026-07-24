(ns abc.tools.decisions
  "Authoritative decision-records corpus: strict loader, Malli shape
   schema, and corpus semantic checks over docs/adr/decisions.edn.
   Replaces the retired abc.tools.adr Markdown grammar."
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn])
  (:import [java.io PushbackReader StringReader]))

(def corpus-file "docs/adr/decisions.edn")

(defn problem [kind file message & {:as data}]
  (merge {:kind kind :file file :message message} data))

(def ^:private eof ::eof)

(defn load-corpus
  "Read exactly one EDN form (followed by EOF) from path.
   Returns {:corpus value} or {:problems [{:kind :invalid-edn …}]}."
  [path]
  (let [fail (fn [msg] {:problems [(problem :invalid-edn (str path) msg)]})]
    (cond
      (not (fs/exists? path)) (fail "decisions file does not exist")
      (fs/directory? path) (fail "decisions path is not a file")
      :else
      (try
        (with-open [r (PushbackReader. (StringReader. (slurp (fs/file path))))]
          (let [form (edn/read {:eof eof} r)
                extra (edn/read {:eof eof} r)]
            (cond
              (= eof form) (fail "decisions file contains no EDN form")
              (not= eof extra) (fail "decisions file must contain exactly one EDN form")
              :else {:corpus form})))
        (catch Exception e
          (fail (str "decisions file is not readable EDN: "
                     (.getMessage e))))))))
