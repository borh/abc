(require '[clojure.java.shell :as sh])

(defn next-version [version]
  (when version
    (let [[a b] (next (re-matches #"(.*?)([\d]+)" version))]
      (when (and a b)
        (str a (inc (Long/parseLong b)))))))

(defn deduce-version-from-git
  "Avoid another decade of pointless, unnecessary and error-prone
  fiddling with version labels in source code."
  []
  (let [[version commits hash dirty?]
        (next (re-matches #"(.*?)-(.*?)-(.*?)(-dirty)?\n"
                          (:out (sh/sh "git" "describe" "--dirty" "--long" "--tags" #_"--match" #_"[0-9].*"))))]
    (try
      (cond
        dirty? (str (next-version version) "-" hash "-dirty")
        (pos? (Long/parseLong commits)) (str (next-version version) "-" hash)
        :otherwise version)
      (catch Exception e (println "Not a git repository or empty repository (did you forget to tag?). Please git init in this directory/make a commit and tag a version.")))))

(def project "abc")
(def version (deduce-version-from-git))

(set-env! :resource-paths #{"src" "resources"}
          :source-paths   #{"test"}
          :dependencies   '[[seancorfield/boot-tools-deps "0.4.5" :scope "test"]
                            [metosin/bat-test "RELEASE" :scope "test"]
                            [adzerk/bootlaces "0.1.13" :scope "test"]
                            [tolitius/boot-check "0.1.11" :scope "test"]])

(task-options!
 pom {:project     (symbol project)
      :version     version
      :description "Aozora Bunko corpus Conversion utilities."
      :url         "https://github.com/borh/abc"
      :scm         {:url "https://github.com/borh/abc"}
      :license     {"Eclipse Public License"
                    "http://www.eclipse.org/legal/epl-v10.html"}}
 jar {:main 'abc.core :file (str project "-" version ".jar")})

(require '[boot-tools-deps.core :refer [deps]])
(require '[adzerk.bootlaces :refer :all])
(require '[tolitius.boot-check :as check])

(bootlaces! version)
(set! *warn-on-reflection* true)

(deftask check-sources []
  (set-env! :source-paths #{"src"})
  (comp
   (deps :quick-merge true)
   (check/with-yagni)
   (check/with-eastwood)
   (check/with-kibit)
   (check/with-bikeshed)))

(deftask build []
  (comp (deps :quick-merge true) (pom) (jar) (target) (install)))

(deftask dev []
  (comp (watch) (deps :aliases [:test]) (repl :init-ns 'abc.core :server true)))

(deftask deps-test []
  (comp (deps :aliases [:test] :quick-merge true)))

(require '[metosin.bat-test :refer [bat-test]])
(deftask test []
  (comp (deps-test)
        (bat-test)))
