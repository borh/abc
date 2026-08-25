;; The generic build engine: constructive traces with early cutoff. It knows
;; nothing about any specific stage. A stage is {:stage-id :stage-version
;; :toolchain-id :f}; `f` receives {:blob (fn [hex] bytes)} plus the input
;; map and returns {output-name -> ^bytes}. The engine hashes inputs into the
;; derivation key, returns cached output hashes on a hit (skipping `f`
;; entirely), and on a miss executes, writes every output blob to the CAS
;; first, then commits the trace row (blob-before-trace: a crash between the
;; two leaves orphan blobs, never a trace pointing at missing bytes).
(ns soranoha.kura.engine
  (:require [babashka.fs]
            [soranoha.kura.cas :as cas]
            [soranoha.kura.trace :as trace]))

(defn open-store!
  [{:keys [cas-dir db-path]}]
  {:cas-dir cas-dir
   :trace (trace/open! db-path)})

(defn close-store! [{:keys [trace]}]
  (trace/close! trace))

(defn- validate-stage! [{:keys [stage-id stage-version toolchain-id f]}]
  (when-not (and (string? stage-id) (string? stage-version)
                 (string? toolchain-id) (fn? f))
    (throw (ex-info "Stage requires stage-id, stage-version, toolchain-id, f"
                    {:reason :invalid-stage
                     :stage-id stage-id
                     :stage-version stage-version
                     :toolchain-id toolchain-id}))))

(defn- outputs-with-missing-blobs
  [{:keys [cas-dir]} outputs]
  (into {}
        (remove (fn [[_ hex]] (cas/has-blob? cas-dir hex)))
        outputs))

(defn run-stage!
  "Run one derivation. `inputs` is a map of string names to blob hashes or
  plain JSON param values — it is hashed as-is into the derivation key, so a
  caller must pass content hashes (not paths) for anything file-like.
  Returns {:outputs {name hex} :trace-key k :cached? bool}.

  A trace hit whose output blobs are missing from the CAS is treated as a
  miss (missing blobs are recoverable cache misses) and re-executed."
  [{:keys [cas-dir trace] :as store} stage inputs]
  (validate-stage! stage)
  (let [trace-key (trace/derivation-key stage inputs)
        cached (trace/lookup trace trace-key)]
    (if (and cached (empty? (outputs-with-missing-blobs store cached)))
      {:outputs cached :trace-key trace-key :cached? true}
      (let [resolve {:blob (fn [hex]
                             (or (cas/get-bytes cas-dir hex)
                                 (throw (ex-info "Input blob missing from CAS"
                                                 {:reason :missing-input-blob
                                                  :hex hex}))))
                     ;; Read-only path into the CAS for stages whose tools
                     ;; need a file (zip readers, subprocess --flags).
                     :blob-path (fn [hex]
                                  (let [path (cas/blob-path cas-dir hex)]
                                    (when-not (babashka.fs/exists? path)
                                      (throw (ex-info "Input blob missing from CAS"
                                                      {:reason :missing-input-blob
                                                       :hex hex})))
                                    path))}
            produced ((:f stage) resolve inputs)
            _ (when-not (and (map? produced) (seq produced))
                (throw (ex-info "Stage produced no outputs"
                                {:reason :empty-stage-output
                                 :stage-id (:stage-id stage)})))
            outputs (into (sorted-map)
                          (map (fn [[name ^bytes bytes]]
                                 [name (cas/put-bytes! cas-dir bytes)]))
                          produced)]
        (trace/record-execution! trace stage inputs outputs)
        {:outputs outputs :trace-key trace-key :cached? false}))))
