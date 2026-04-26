(ns abc.ndc
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import [java.io PushbackReader]
           [org.apache.commons.compress.compressors.xz XZCompressorInputStream]))

(defn- xz-reader [resource]
  (-> resource
      io/input-stream
      XZCompressorInputStream.
      io/reader))

(defonce ndc-map
  (edn/read (PushbackReader.
             (xz-reader (io/resource "abc/ndc9.edn.xz")))))
