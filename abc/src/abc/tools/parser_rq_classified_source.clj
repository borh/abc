(ns abc.tools.parser-rq-classified-source
  "Closed validators for classified-source evidence artifacts: policy
  selector closure, the characterization map, and ledger entries with
  their normalization proofs. Inputs are string-keyed JSON values."
  (:require [abc.tools.hash :as hash]
            [abc.tools.parser-rq-decoded-utf8 :as decoded-utf8]
            [clojure.string :as string]))

(defn- duplicate-errors [label values]
  (->> values frequencies
       (keep (fn [[value count]]
               (when (> count 1)
                 (str "classified-source policy contains ambiguous "
                      label " " value))))))

(defn policy-errors [policy]
  (->> (concat
        (duplicate-errors
         "selector"
         (map #(select-keys % ["construct_id"])
              (get policy "rules" [])))
        (duplicate-errors
         "accent source"
         (map #(get % "source")
              (get policy "accent_mappings" []))))
       sort vec))

(defn characterization-errors [policy mapping]
  (let [constructs (set (map #(get % "construct_id") (get policy "rules" [])))
        observations (get mapping "observations" [])]
    (vec
     (concat
      (when-not (= 36 (count observations))
        [(str "characterization map must contain 36 observations, found "
              (count observations))])
      (->> observations
           (keep (fn [observation]
                   (when-not (contains? constructs (get observation "construct_id"))
                     (str "characterization case " (get observation "case")
                          " maps to unknown construct "
                          (get observation "construct_id")))))
           sort)))))

(defn- utf8-value-hash [value]
  (hash/format-sha256 (hash/sha256-bytes (.getBytes ^String value "UTF-8"))))

(defn- accent-normalized-form [policy source-form]
  (when (and (string/starts-with? source-form "〔")
             (string/ends-with? source-form "〕"))
    (let [body (subs source-form 1 (dec (count source-form)))
          mappings (into {} (map (juxt #(get % "source")
                                       #(get % "normalized"))
                                 (get policy "accent_mappings" [])))]
      (str "〔"
           (loop [index 0 output (StringBuilder.)]
             (if (= index (count body))
               (str output)
               (let [remaining (- (count body) index)
                     source (or (when (<= 3 remaining)
                                  (let [candidate (subs body index (+ index 3))]
                                    (when (contains? mappings candidate) candidate)))
                                (when (<= 2 remaining)
                                  (let [candidate (subs body index (+ index 2))]
                                    (when (contains? mappings candidate) candidate))))]
                 (if source
                   (recur (+ index (count source))
                          (.append output ^String (get mappings source)))
                   (recur (inc index) (.append output (.charAt body index)))))))
           "〕"))))

(defn- normalization-errors [policy construct proof source-slice]
  (let [source-form (get proof "source_form")
        normalized-form (get proof "normalized_form")
        inverse-rule (get proof "inverse_rule")
        expected-rule {"crlf_normalization" "crlf"
                       "bare_cr_normalization" "bare_cr"
                       "accent_normalization" "accent_decomposition"}
        exact-forms {"crlf" ["\r\n" "\n"]
                     "bare_cr" ["\r" "\n"]}]
    (concat
     (when-not (= source-slice source-form)
       [(str "normalization source form does not match decoded bytes for "
             construct)])
     (when-not (= (get expected-rule construct) inverse-rule)
       [(str "normalization inverse rule does not match policy for " construct)])
     (when (and (contains? exact-forms inverse-rule)
                (not= (get exact-forms inverse-rule)
                      [source-form normalized-form]))
       [(str "normalization forms do not match inverse rule for " construct)])
     (when (and (= "accent_decomposition" inverse-rule)
                (or (= source-form normalized-form)
                    (not= normalized-form
                          (accent-normalized-form policy source-form))))
       [(str "accent normalization proof does not match the closed mapping for "
             construct)])
     (when (and (string? source-form)
                (not= (get proof "source_bytes_hash")
                      (utf8-value-hash source-form)))
       [(str "normalization source hash does not match proof for " construct)])
     (when (and (string? normalized-form)
                (not= (get proof "normalized_bytes_hash")
                      (utf8-value-hash normalized-form)))
       [(str "normalization target hash does not match proof for " construct)]))))

(defn ledger-errors [policy ledger decoded]
  (let [source-bytes (when (string? decoded)
                       (.getBytes ^String decoded "UTF-8"))
        rules (into {} (map (juxt #(get % "construct_id") identity)
                            (get policy "rules" [])))]
    (->> (get ledger "entries" [])
         (mapcat
          (fn [entry]
            (let [construct (get entry "construct_id")
                  rule (get rules construct)
                  witness (get entry "construct_witness")
                  proof (get entry "normalization_proof")
                  target (get entry "target_identity")
                  entry-slice (decoded-utf8/decoded-slice source-bytes
                                                          (get entry "start")
                                                          (get entry "end"))
                  expected (select-keys rule ["source_role" "disposition"
                                              "evidence_class"])
                  actual (select-keys entry ["source_role" "disposition"
                                             "evidence_class"])]
              (concat
               (when-not rule
                 [(str "ledger entry uses unknown construct " construct)])
               (when (and rule (not= expected actual))
                 [(str "ledger entry does not match policy for " construct)])
               (when (and rule (contains? rule "target_relation")
                          (not= (get rule "target_relation")
                                (get target "relation")))
                 [(str "ledger target relation does not match policy for " construct)])
               (when (and rule (contains? rule "witness_kind")
                          (not= (get rule "witness_kind")
                                (get witness "construct_id")))
                 [(str "ledger witness does not match policy for " construct)])
               (when (and (= "lossless_normalization" (get rule "disposition"))
                          (nil? proof))
                 [(str "ledger normalization proof is missing for " construct)])
               (when (and witness
                          (not= [(get entry "start") (get entry "end")]
                                [(get witness "start") (get witness "end")]))
                 [(str "ledger witness span does not match entry for " construct)])
               (when (and witness
                          (not= (get witness "source_form") entry-slice))
                 [(str "ledger witness source form does not match decoded bytes for "
                       construct)])
               (when proof
                 (normalization-errors policy construct proof entry-slice))))))
         sort
         vec)))
