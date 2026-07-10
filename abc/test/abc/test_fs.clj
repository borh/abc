(ns abc.test-fs
  "JVM-only test helpers for exception-safe temporary filesystem use.
  Kept out of test_utils.cljc because babashka.fs is JVM-only and that
  file is deliberately .cljc."
  (:require [babashka.fs :as fs]))

(defmacro with-temp-dir
  "Bind `binding` to a fresh temp dir (java.io.File) for the body, then
  delete the whole tree afterward — even if the body throws. Prefer this
  over ad-hoc createTempDirectory + after-body cleanup, which leaks the
  tree when an assertion fails."
  [[binding] & body]
  `(fs/with-temp-dir [d# {}]
     (let [~binding (fs/file d#)]
       ~@body)))
