(ns soranoha.ported.assets
  "Resolution root for abc-owned asset files the ported code reads by
  repo-relative path (record JSON Schemas, TEI profile trio). abc resolves
  them against its own checkout cwd; the kernel binds this root explicitly
  (D20: the assets stay abc-owned, consumed as an external input).")

(def ^:dynamic *root* ".")

(defn resolve-path [repo-relative]
  (str *root* "/" repo-relative))
