(ns soranoha.snh.spec-coherence-test
  "The protocol spec restates the shape of the payloads the schemas enforce,
  and the schemas are what a release is validated against. Nothing compared
  the two, so two of the spec's field lists fell behind the schemas without
  any build noticing: the manifest's rights object was listed with a member
  the schema does not define, and a work was listed without two the schema
  requires.

  What is compared is the restatement, not the prose around it. A field list
  the spec writes as `{a, b, c}`, in a table cell or a fenced block, must name
  exactly the members some object requires: the objects are read from the
  schemas, plus the projection whose membership `soranoha.snh.verify` defines
  rather than a schema. A member an object allows without requiring has to be
  named somewhere in the spec, so a field cannot be added quietly by making it
  optional. A braced span whose parts are not field names describes something
  else and is left alone, as is a payload the spec discusses without listing."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [soranoha.snh.schema :as schema]
            [soranoha.snh.schema-walk :as walk]
            [soranoha.snh.verify :as verify]
            [soranoha.za.naming :as naming]
            [soranoha.za.docs :as docs]))

(def ^:private spec-path "docs/design/snh-protocol-v1.md")

(defn- payload-objects
  "Every object a protocol schema defines that requires anything. A node
  requiring nothing is a conditional subschema rather than a payload object:
  it carries the members a rule reaches, not a shape anyone restates."
  []
  (filter (comp seq :required)
          (mapcat walk/objects (map schema/schema-for (keys schema/schema-resources)))))

(defn- payload-vocabularies
  "Every closed vocabulary the protocol schemas define."
  []
  (distinct (mapcat walk/enums (map schema/schema-for (keys schema/schema-resources)))))

(def ^:private restatable-sets
  "The field sets the spec may restate, and where each one is defined."
  (delay
    (into {(set verify/projection-keys) "soranoha.snh.verify/projection-keys"}
          (map (juxt :required (constantly "a protocol schema")))
          (payload-objects))))

(defn- brace-spans
  "The text inside every `{...}` in `text` that closes, innermost included, as
  [line inside] pairs. Depth is counted so a member carrying braces of its own
  stays inside its member rather than ending the list early."
  [text]
  (for [open (range (count text))
        :when (= \{ (nth text open))
        :let [close (loop [i (inc open) depth 1]
                      (cond
                        (>= i (count text)) nil
                        (= \{ (nth text i)) (recur (inc i) (inc depth))
                        (not= \} (nth text i)) (recur (inc i) depth)
                        (> depth 1) (recur (inc i) (dec depth))
                        :else i))]
        :when close]
    [(inc (count (re-seq #"\n" (subs text 0 open)))) (subs text (inc open) close)]))

(defn- members
  "`inside` split on the commas between its members, leaving a comma inside a
  member's own note or nested list where it was written."
  [inside]
  (loop [chars (seq inside) depth 0 member [] found []]
    (if-let [c (first chars)]
      (let [depth (+ depth (case c (\( \[ \{) 1 (\) \] \}) -1 0))]
        (if (and (= \, c) (zero? depth))
          (recur (next chars) depth [] (conj found (apply str member)))
          (recur (next chars) depth (conj member c) found)))
      (conj found (apply str member)))))

;; A member is a field name, optionally marked as not always present, and
;; optionally carrying a note: a type, a value, or a sentence about it.
(def ^:private member-pattern #"(?s)^([a-z][a-z0-9_]*)(\?)?(\s*[:(].*)?$")

(defn- restated-set
  "The names a braced span states are always present, or nil when its members
  are not field names and it is therefore not restating an object."
  [inside]
  (let [parsed (map #(re-matches member-pattern (str/trim (str/replace % #"\s+" " ")))
                    (members inside))]
    (when (every? some? parsed)
      (let [always (set (map second (remove #(nth % 2) parsed)))]
        (when (seq always) always)))))

(defn- restated-enum
  "The values a braced list of quoted strings states, or nil when its members
  are not all quoted strings and it is therefore not restating a vocabulary."
  [inside]
  (let [parsed (map #(re-matches #"\"([^\"]+)\"" (str/trim %)) (members inside))]
    (when (and (seq parsed) (every? some? parsed))
      (set (map second parsed)))))

(defn- nearest
  "The defined set closest to `claimed`, so a failure says which way the two
  moved apart rather than only that they did."
  [claimed]
  (first (sort-by #(+ (count (remove % claimed)) (count (remove claimed %)))
                  (keys @restatable-sets))))

(deftest every-field-list-in-the-spec-names-an-object-that-is-defined
  (let [spec (docs/read-text spec-path)]
    (doseq [[line inside] (brace-spans spec)
            :let [claimed (restated-set inside)]
            :when claimed]
      (testing (str spec-path ":" line)
        (is (contains? @restatable-sets claimed)
            (let [close (nearest claimed)]
              (str "the spec lists " (pr-str (sort claimed))
                   " but nothing defines that object. The nearest is "
                   (pr-str (sort close)) " (" (get @restatable-sets close)
                   "), so the spec names " (pr-str (sort (remove close claimed)))
                   " that it does not have, and omits "
                   (pr-str (sort (remove claimed close))))))))))

(deftest a-member-an-object-allows-without-requiring-is-named-in-the-spec
  ;; Otherwise a field enters a published payload by being optional, and the
  ;; check above stays quiet because the list of required members did not move.
  (let [spec (docs/read-text spec-path)
        named? #(str/includes? spec (str "`" % "`"))]
    (doseq [{:keys [required properties]} (payload-objects)
            :let [unnamed (sort (remove named? (remove required properties)))]]
      (is (empty? unnamed)
          (str "these may appear in a published payload and the spec never "
               "names them: " (pr-str unnamed))))))

(deftest every-vocabulary-the-spec-restates-is-one-a-schema-defines
  (let [spec (docs/read-text spec-path)
        defined (set (payload-vocabularies))]
    (doseq [[line inside] (brace-spans spec)
            :let [claimed (restated-enum inside)]
            :when claimed]
      (is (contains? defined claimed)
          (str spec-path ":" line " offers " (pr-str (sort claimed))
               " and no schema defines that vocabulary")))))

(deftest every-value-a-schema-will-accept-is-written-in-the-spec
  ;; Written as itself, in a code span or in quotes, rather than merely used in
  ;; a sentence: a value that reads as an ordinary word would otherwise count
  ;; itself present in prose that is not about it.
  (let [spec (docs/read-text spec-path)
        written? #(or (str/includes? spec (str "`" % "`"))
                      (str/includes? spec (str "\"" % "\"")))]
    (doseq [values (payload-vocabularies)
            :let [unwritten (sort (remove written? values))]]
      (is (empty? unwritten)
          (str "a release may carry these and the spec never states them: "
               (pr-str unwritten))))))

(defn- registry-parts
  "Section 2's two lists, split where it turns from per-work types to
  release-level ones."
  [spec]
  (let [section (some #(when (str/starts-with? % "2. ") %) (str/split spec #"(?m)^## "))
        [per-work release-level] (str/split section #"Release-level types:" 2)]
    [per-work release-level]))

(defn- plain-code-spans
  "The code spans naming a bare type, so `snh-manifest/3` and `works[].artifacts`
  are read as the things they are rather than as registry entries."
  [text]
  (set (map second (re-seq #"`([a-z][a-z-]*)`" text))))

(deftest the-closed-registry-lists-the-types-this-build-registers
  ;; Section 2 is the only list the spec calls closed, and a consumer written
  ;; against it is entitled to meet no other member.
  (let [[per-work release-level] (registry-parts (docs/read-text spec-path))]
    (is (= (set naming/artifact-kinds) (plain-code-spans per-work))
        "the per-work types the registry permits are not the ones a work carries")
    (is (= (set (keys schema/schema-resources)) (plain-code-spans release-level))
        "the release-level types the registry lists are not the ones that resolve to a schema")))
