(ns soranoha.ported.logging
  "Minimal stderr shim with abc.tools.logging's call surface (log!,
  install-cli-handler!). The abc original binds Telemere; the kernel keeps
  logging non-identity-bearing and dependency-free, so the shim replaces it
  rather than copying it.")

(def ^:private level-order
  {:trace 0 :debug 1 :info 2 :warn 3 :error 4 :fatal 5})

(def ^:dynamic *min-level* :info)

(defn log! [level message]
  (when (>= (get level-order level 2)
            (get level-order *min-level* 2))
    (binding [*out* *err*]
      (println (str (name level) " " message)))))

(defn install-cli-handler! []
  nil)
