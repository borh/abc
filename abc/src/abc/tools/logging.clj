(ns abc.tools.logging
  "Shared Telemere setup for CLI entry points. Installs a compact
  console handler that prints `:info`/below as the bare message line
  (to preserve `==> ...` style CLI output) and prefixes `:warn`/`:error`
  with their level."
  (:require [taoensso.telemere :as tel]))

(defn- compact-output-fn [signal]
  (let [level (:level signal)
        msg (force (:msg_ signal))
        err (:error signal)
        prefix (case level
                 (:trace :debug :info) ""
                 (str (.toUpperCase (name level)) ": "))]
    (str prefix
         msg
         (when err (str "\n  " (ex-message err)))
         "\n")))

;; Silence noisy SLF4J sources at namespace load time. SLF4J calls are
;; routed through Telemere (via com.taoensso/telemere-slf4j), so this
;; ns-filter applies to logs from Apache SSHD, Apache Jena bootstrap,
;; etc. that would otherwise pollute CLI output before -main installs
;; the compact handler.
(defonce ^:private filters-installed
  (do
    (tel/set-ns-filter! {:disallow ["org.apache.sshd.*"
                                    "org.apache.jena.riot.system.stream.*"]
                         :allow "*"})
    true))

(defn log! [level message]
  (tel/log! level message))

(defn install-cli-handler!
  "Replace the default Telemere console handler with a compact one
  suited to CLI tools. Sync dispatch so messages flush before
  System/exit. Idempotent."
  []
  (when-not filters-installed
    (throw (IllegalStateException. "Telemere namespace filters were not installed")))
  (tel/remove-handler! :default/console)
  (tel/add-handler! :abc/cli
                    (tel/handler:console
                     {:output-fn compact-output-fn})
                    {:async nil}))
