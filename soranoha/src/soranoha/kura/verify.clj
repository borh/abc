;; Separate verifier entry point (R1): runs in CI, never on the build path.
;; Slice-1 scope: the ledger determinism (uniqueness) query and a fixity
;; sweep over CAS blobs referenced by traces.
(ns soranoha.kura.verify
  (:require [babashka.fs :as fs]
            [soranoha.core.hash :as hash]
            [soranoha.kura.cas :as cas]
            [soranoha.kura.trace :as trace]))

(defn fixity-sweep
  "Rehash every trace-referenced blob present in the CAS. Returns
  {:checked n :missing [hex...] :corrupt [hex...]}. Missing blobs are cache
  misses (F99), reported but not corruption; a hash mismatch is corruption."
  [{:keys [cas-dir trace]}]
  (reduce
   (fn [acc hex]
     (let [path (cas/blob-path cas-dir hex)]
       (if-not (fs/exists? path)
         (update acc :missing conj hex)
         (if (= hex (hash/sha256-file path))
           (update acc :checked inc)
           (update acc :corrupt conj hex)))))
   {:checked 0 :missing [] :corrupt []}
   (trace/all-output-hashes trace)))

(defn verify
  "Full kura verification report."
  [store]
  (let [violations (trace/determinism-violations (:trace store))
        fixity (fixity-sweep store)]
    {:determinism-violations violations
     :fixity fixity
     :ok? (and (empty? violations)
               (empty? (:corrupt fixity)))}))
