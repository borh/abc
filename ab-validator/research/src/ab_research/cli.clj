(ns ab-research.cli
  "Shared CLI entry harness for `-main` functions. Centralizes the
  parse -> validate-required -> dispatch -> exit-code shape that was
  copied across the tools, and guarantees every CLI installs the compact
  Telemere handler. `dispatch!` returns the exit code (no `System/exit`)
  so it is unit-testable; `run-cli!` performs the exit."
  (:require [ab-research.logging :as logging]
            [clojure.string :as string]
            [clojure.tools.cli :as tools-cli]))

(defn strip-double-dash
  "Drop a single leading \"--\" separator (as passed by `clojure -M:tool --`)."
  [args]
  (if (= "--" (first args))
    (rest args)
    args))

(defn parse
  "Parse `args` with tools.cli. Returns the tools.cli result map (incl.
  `:arguments`) plus `:missing` — the `required` option keys whose parsed
  value is nil."
  [args {:keys [cli-options required]}]
  (let [result (tools-cli/parse-opts (strip-double-dash args) cli-options)]
    (assoc result :missing (remove #(some? (get (:options result) %))
                                   (or required [])))))

(defn dispatch!
  "Map a parsed CLI result to an exit code, running `run` on success.
  `run` receives {:options :arguments}. Returns an int; never calls
  System/exit. Codes: 0 ok/help, 1 fail?, 2 usage error / arity / ExceptionInfo."
  [{:keys [options arguments errors summary missing]}
   {:keys [usage-fn run fail? min-args max-args] :or {fail? (constantly false)}}]
  (let [arg-count (count arguments)
        arity-error? (or (and min-args (< arg-count min-args))
                         (and max-args (> arg-count max-args)))]
    (cond
      (:help options)
      (do (println (usage-fn summary)) 0)

      ;; All failure output (parse errors, missing/arity, usage) goes to
      ;; stderr as plain text so the CLI contract is uniform and scriptable.
      (or (seq errors) (seq missing) arity-error?)
      (do (binding [*out* *err*]
            (doseq [e errors] (println e))
            (when (seq missing)
              (println (str "Missing required option(s): "
                            (string/join ", " (map name missing)))))
            (when arity-error?
              (println (str "Wrong number of arguments: got " arg-count)))
            (println (usage-fn summary)))
          2)

      :else
      (try
        (if (fail? (run {:options options :arguments arguments})) 1 0)
        (catch clojure.lang.ExceptionInfo ex
          (binding [*out* *err*]
            (println (ex-message ex))
            (when-let [data (seq (ex-data ex))]
              (println (pr-str data))))
          2)))))

(defn run-cli!
  "Full CLI entry: install the compact handler, parse, dispatch, exit.
  `opts` = {:cli-options :required :min-args :max-args :usage-fn :run :fail?}."
  [args opts]
  (logging/install-cli-handler!)
  (System/exit (dispatch! (parse args opts) opts)))
