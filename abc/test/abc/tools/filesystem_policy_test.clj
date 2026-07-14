(ns abc.tools.filesystem-policy-test
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk])
  (:import [clojure.lang LineNumberingPushbackReader]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def forbidden-file-methods
  #{:mkdirs :exists :isFile :isDirectory :listFiles :renameTo :delete
    :getCanonicalFile :getCanonicalPath :getAbsolutePath :relativize})

(def legacy-filesystem-namespaces #{})

(def permanent-files-operations
  '{abc.tools.files
    {:operations #{Files/deleteIfExists}
     :rationale "The shared cleanup wrapper preserves no-error missing-file semantics"}
    abc.tools.json
    {:operations #{Files/createTempFile Files/setPosixFilePermissions
                   Files/move Files/deleteIfExists}
     :rationale "Atomic JSON replacement requires NIO temporary-file permissions and move semantics"}
    abc.tools.diagram.presentation-registry
    {:operations #{Files/createTempFile Files/move Files/deleteIfExists}
     :rationale "Atomic presentation registry replacement requires NIO move semantics"}
    abc.tools.source-bundle
    {:operations #{Files/createTempFile Files/copy Files/deleteIfExists}
     :rationale "Archive staging requires NIO stream copy and exception-reporting cleanup"}
    abc.tools.diagram.presentation-svg
    {:operations #{Files/readAllBytes}
     :rationale "Binary SVG asset loading is an intentional NIO byte operation"}
    abc.tools.soranoha-build-publication
    {:operations #{Files/move}
     :rationale "Publication installation requires an atomic NIO move"}
    abc.tools.evidence-output
    {:operations #{Files/exists Files/createLink Files/deleteIfExists}
     :rationale "Exclusive evidence publication requires no-follow existence, hard-link linearization, and failure cleanup"}})

(def permanent-interop-operations
  '{abc.tools.source-bundle
    {:operations #{:isDirectory}
     :rationale "ZipArchiveEntry predicate, not java.io.File"}
    abc.tools.aozora-history-audit
    {:operations #{:renameTo}
     :rationale "Preserve File.renameTo same-filesystem boolean failure contract"}})

(defn- dotted-method [x]
  (when (and (symbol? x)
             (> (count (name x)) 1)
             (not= '.. x)
             (string/starts-with? (name x) "."))
    (keyword (subs (name x) 1))))

(defn- form-methods [form]
  (when (seq? form)
    (let [head (first form)]
      (cond
        (dotted-method head) #{(dotted-method head)}
        (= '. head) (let [method-form (nth form 2 nil)
                          method (if (seq? method-form)
                                   (first method-form)
                                   method-form)]
                      (some-> method name keyword hash-set))
        (#{'-> '->>} head) (into #{} (keep dotted-method) (rest form))
        (= '.. head) (into #{}
                           (keep (fn [step]
                                   (cond
                                     (seq? step) (some-> step first name keyword)
                                     :else (dotted-method step))))
                           (drop 2 form))
        :else #{}))))

(defn- filesystem-interop-calls [forms]
  (let [found (volatile! #{})]
    (walk/postwalk
     (fn [form]
       (vswap! found into (filter forbidden-file-methods (form-methods form)))
       form)
     forms)
    @found))

(defn- normalized-files-call [x]
  (when (symbol? x)
    (let [owner (namespace x)]
      (when (or (= "Files" owner)
                (and owner (string/ends-with? owner ".Files")))
        (symbol "Files" (name x))))))

(def eof (Object.))

(defn- install-source-aliases! [ns-form]
  (doseq [clause (drop 2 ns-form)
          spec (when (and (seq? clause) (= :require (first clause)))
                 (rest clause))
          :when (vector? spec)
          :let [target (first spec)
                options (apply hash-map (rest spec))
                alias-name (:as options)]
          :when alias-name]
    (create-ns target)
    (alias alias-name target)))

(defn- form-operations [form]
  (let [interop (volatile! #{})
        files (volatile! #{})
        file-seq? (volatile! false)]
    (letfn [(visit! [x]
              (when-not (and (seq? x) (= 'quote (first x)))
                (vswap! interop into (filter forbidden-file-methods (form-methods x)))
                (when-let [operation (normalized-files-call x)]
                  (vswap! files conj operation))
                (when (= 'file-seq x)
                  (vreset! file-seq? true))
                (when (coll? x)
                  (doseq [child x]
                    (visit! child)))))]
      (visit! form))
    {:interop @interop :files @files :file-seq? @file-seq?}))

(defn- source-file? [file]
  (and (.isFile file)
       (or (string/ends-with? (.getName file) ".clj")
           (string/ends-with? (.getName file) ".cljc"))))

(defn- read-source [file]
  (let [reader-ns (create-ns (gensym "filesystem-policy-reader-"))]
    (with-open [reader (LineNumberingPushbackReader. (io/reader file))]
      (binding [*default-data-reader-fn* tagged-literal
                *ns* reader-ns]
        (loop [namespace nil
               findings []]
          (let [row (.getLineNumber reader)
                form (read {:eof eof :read-cond :allow :features #{:clj}} reader)]
            (if (identical? eof form)
              {:file file :namespace namespace :findings findings}
              (let [ns-form? (and (seq? form) (= 'ns (first form)))
                    _ (when ns-form? (install-source-aliases! form))
                    namespace (if ns-form? (second form) namespace)
                    {:keys [interop files file-seq?]} (form-operations form)
                    operations (concat (map #(vector :interop %) interop)
                                       (map #(vector :files %) files)
                                       (when file-seq? [[:symbol 'file-seq]]))]
                (recur namespace
                       (into findings
                             (map (fn [[kind operation]]
                                    {:file (.getPath file)
                                     :row (inc row)
                                     :kind kind
                                     :operation operation}))
                             operations))))))))))

(defn- source-reports [root]
  (->> (file-seq (io/file root))
       (filter source-file?)
       (map read-source)
       vec))

(defn- allowed? [namespace {:keys [kind operation]}]
  (or (contains? legacy-filesystem-namespaces namespace)
      (case kind
        :files (contains? (get-in permanent-files-operations
                                  [namespace :operations] #{})
                          operation)
        :interop (contains? (get-in permanent-interop-operations
                                    [namespace :operations] #{})
                            operation)
        false)))

(defn- policy-violations [root]
  (mapcat (fn [{:keys [namespace findings]}]
            (keep #(when-not (allowed? namespace %)
                     (assoc % :namespace namespace))
                  findings))
          (source-reports root)))

(defn- temporary-source-file [content]
  (let [path (Files/createTempFile "filesystem-policy-" ".clj"
                                   (make-array FileAttribute 0))]
    (spit (.toFile path) content)
    (.toFile path)))

(deftest filesystem-interop-call-shapes-test
  (is (= #{:exists :isFile :delete}
         (filesystem-interop-calls
          '[(.exists file)
            (. file isFile)
            (-> file .delete)])))
  (is (= #{:exists :delete :isDirectory}
         (filesystem-interop-calls
          '[(. file (exists))
            (.. file (delete) .isDirectory)]))))

(deftest filesystem-policy-ignores-lexical-collisions-test
  (is (empty? (filesystem-interop-calls
               '[".delete"
                 (delete transient-map :key)
                 (repository.delete branch)]))))

(deftest filesystem-policy-distinguishes-quoted-data-from-file-seq-calls-test
  (is (false? (:file-seq? (form-operations '(quote #{file-seq}))))
      "a forbidden-operation inventory is data, not a filesystem traversal")
  (is (true? (:file-seq? (form-operations '(file-seq root))))
      "an executable production call remains forbidden"))

(deftest files-call-normalization-test
  (is (= 'Files/move (normalized-files-call 'Files/move)))
  (is (= 'Files/createTempDirectory
         (normalized-files-call 'java.nio.file.Files/createTempDirectory)))
  (is (nil? (normalized-files-call 'ProfileFiles/read))))

(deftest source-reader-supports-conditionals-tags-and-lines-test
  (let [file (temporary-source-file
              (str "(ns temporary.reader)\n"
                   "#unknown/tag {:safe true}\n"
                   "#?(:clj (. file (exists)) :cljs (.delete file))\n"))]
    (try
      (let [{:keys [namespace findings]} (read-source file)]
        (is (= 'temporary.reader namespace))
        (is (= [{:file (.getPath file) :row 3 :kind :interop :operation :exists}]
               findings)))
      (finally
        (Files/deleteIfExists (.toPath file))))))

(deftest filesystem-policy-sensitivity-fixture-test
  (let [root (Files/createTempDirectory "filesystem-policy-src-"
                                        (make-array FileAttribute 0))
        file (.resolve root "fixture.clj")]
    (try
      (spit (.toFile file)
            (str "(ns temporary.production)\n"
                 ";; (.delete file)\n"
                 "\"(.isFile file)\"\n"
                 "(.availableProcessors (Runtime/getRuntime))\n"
                 "(.exists file)\n"))
      (is (= [{:file (.toString file) :row 5 :kind :interop
               :operation :exists :namespace 'temporary.production}]
             (vec (policy-violations (.toFile root)))))
      (finally
        (Files/deleteIfExists file)
        (Files/deleteIfExists root)))))

(deftest exception-data-is-exact-and-explained-test
  (is (= '{abc.tools.json
           #{Files/createTempFile Files/setPosixFilePermissions
             Files/move Files/deleteIfExists}
           abc.tools.diagram.presentation-registry
           #{Files/createTempFile Files/move Files/deleteIfExists}
           abc.tools.source-bundle
           #{Files/createTempFile Files/copy Files/deleteIfExists}
           abc.tools.diagram.presentation-svg #{Files/readAllBytes}
           abc.tools.soranoha-build-publication #{Files/move}
           abc.tools.evidence-output
           #{Files/exists Files/createLink Files/deleteIfExists}
           abc.tools.files #{Files/deleteIfExists}}
         (update-vals permanent-files-operations :operations)))
  (is (= '{abc.tools.source-bundle #{:isDirectory}
           abc.tools.aozora-history-audit #{:renameTo}}
         (update-vals permanent-interop-operations :operations)))
  (doseq [exceptions [permanent-files-operations permanent-interop-operations]
          [_ {:keys [rationale]}] exceptions]
    (is (not (string/blank? rationale)))))

(defn- real-operation-findings [reports namespace kind operation]
  (->> reports
       (filter #(= namespace (:namespace %)))
       (mapcat :findings)
       (filter #(and (= kind (:kind %))
                     (= operation (:operation %))))
       vec))

(defn- exposed-findings-after-removing
  [namespace kind operation real-findings]
  (with-redefs [legacy-filesystem-namespaces
                (disj legacy-filesystem-namespaces namespace)
                permanent-files-operations
                (if (= :files kind)
                  (update-in permanent-files-operations
                             [namespace :operations] disj operation)
                  permanent-files-operations)
                permanent-interop-operations
                (if (= :interop kind)
                  (update-in permanent-interop-operations
                             [namespace :operations] disj operation)
                  permanent-interop-operations)]
    (vec (remove #(allowed? namespace %) real-findings))))

(deftest permanent-exceptions-are-sensitive-test
  (let [reports (source-reports "src")]
    (doseq [[kind exceptions]
            [[:files permanent-files-operations]
             [:interop permanent-interop-operations]]
            [namespace {:keys [operations]}] exceptions
            operation operations]
      (testing (str namespace " " operation " is a live exact exception")
        (let [real-findings (real-operation-findings
                             reports namespace kind operation)]
          (is (seq real-findings))
          (is (= real-findings
                 (exposed-findings-after-removing
                  namespace kind operation real-findings))))))))

(deftest legacy-filesystem-baseline-test
  (is (empty? legacy-filesystem-namespaces)))

(deftest production-filesystem-policy-test
  (let [reports (source-reports "src")
        reports-by-namespace (into {} (map (juxt :namespace identity)) reports)]
    (is (empty? (policy-violations "src")))
    (doseq [namespace legacy-filesystem-namespaces]
      (testing (str namespace " remains a live grandfather entry")
        (is (contains? reports-by-namespace namespace))
        (is (seq (:findings (get reports-by-namespace namespace))))))))
