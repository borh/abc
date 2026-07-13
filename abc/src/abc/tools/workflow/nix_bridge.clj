(ns abc.tools.workflow.nix-bridge
  "Target-graph leaf that realizes an explicit Nix flake output and returns a
   Soranoha-owned structured value (store path, flake output, lock nodes,
   outputs, messages). The nix subprocess is injectable via `:runner` so this
   is unit-testable inside the offline kaocha sandbox — no test may invoke
   real `nix`. A node key or branch result is never used directly as a Nix
   attribute name: `flake-output` is always caller-supplied."
  (:require [abc.tools.workflow.target :as target]
            [babashka.process :as process]
            [charred.api :as json]))

(defn default-runner
  "Real nix runner: shells out via `babashka.process/sh`."
  [args]
  (process/sh args))

(defn read-lock-nodes
  "Pure. For each name in `node-names` present under (get flake-lock \"nodes\"),
   pull its `locked` map and return {:node name :type ... :owner ... :repo ...
   :rev ... :narHash ...} (only keys present under `locked`; :node is always
   included). Names not present in the lock are skipped."
  [flake-lock node-names]
  (let [nodes (get flake-lock "nodes")]
    (into []
          (keep (fn [name]
                  (when-let [node (get nodes name)]
                    (let [locked (get node "locked")]
                      (cond-> {:node name}
                        (contains? locked "type") (assoc :type (get locked "type"))
                        (contains? locked "owner") (assoc :owner (get locked "owner"))
                        (contains? locked "repo") (assoc :repo (get locked "repo"))
                        (contains? locked "rev") (assoc :rev (get locked "rev"))
                        (contains? locked "narHash") (assoc :narHash (get locked "narHash")))))))
          node-names)))

(defn realize-flake-output!
  "Realize `flake-output` via `runner` (default: real nix through
   babashka.process/sh). `runner` is a fn [args-vector] -> {:exit :out :err}.
   Non-zero exit throws ex-info with :type :nix-bridge-failure. Returns
   {:store-path :flake-output :lock-nodes :outputs :messages}."
  [{:keys [flake-output runner flake-lock lock-node-names]
    :or {runner default-runner lock-node-names []}}]
  (let [args ["nix" "build" flake-output "--json" "--no-link"]
        {:keys [exit out err]} (runner args)]
    (when-not (zero? exit)
      (throw (ex-info "nix realization failed"
                      {:type :nix-bridge-failure
                       :flake-output flake-output
                       :exit exit
                       :err err})))
    (let [parsed (json/read-json out)
          store-path (get-in (first parsed) ["outputs" "out"])]
      {:store-path store-path
       :flake-output flake-output
       :lock-nodes (read-lock-nodes flake-lock lock-node-names)
       :outputs [{:role "out" :store-path store-path}]
       :messages []})))

(defn nix-bridge-leaf
  "A target/leaf that realizes `flake-output` via `realize-flake-output!` when
   evaluated. `flake-output` is always caller-supplied — never built from a
   node key or branch result."
  [{:keys [deps impl-id flake-output runner flake-lock lock-node-names]}]
  (target/leaf (or deps [])
               impl-id
               (fn [_deps]
                 (realize-flake-output!
                  {:flake-output flake-output
                   :runner runner
                   :flake-lock flake-lock
                   :lock-node-names lock-node-names}))))
