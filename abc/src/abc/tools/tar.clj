(ns abc.tools.tar
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [java.nio.charset StandardCharsets]
           [org.apache.commons.compress.compressors.zstandard
            ZstdCompressorInputStream
            ZstdCompressorOutputStream]))

(defn- zstd-tar? [archive-file]
  (string/ends-with? (str archive-file) ".tar.zst"))

(defn- tar-output-stream [archive-file]
  (let [out (io/output-stream archive-file)]
    (if (zstd-tar? archive-file)
      (ZstdCompressorOutputStream. out)
      out)))

(defn- tar-input-stream [archive-file]
  (let [in (io/input-stream archive-file)]
    (if (zstd-tar? archive-file)
      (ZstdCompressorInputStream. in)
      in)))

(defn- padding-size [size]
  (mod (- 512 (mod size 512)) 512))

(defn- octal-field [value width]
  (let [digits (Long/toOctalString (long value))
        padded (apply str (repeat (max 0 (- width 1 (count digits))) "0"))]
    (str padded digits "\0")))

(defn- write-ascii! [header offset width value]
  (let [bytes (.getBytes (str value) StandardCharsets/US_ASCII)]
    (System/arraycopy bytes 0 header offset (min width (alength bytes)))))

(defn- write-field! [header offset width value]
  (write-ascii! header offset width value))

(defn- header [path size]
  (let [header (byte-array 512)]
    (write-field! header 0 100 path)
    (write-field! header 100 8 (octal-field 0644 8))
    (write-field! header 108 8 (octal-field 0 8))
    (write-field! header 116 8 (octal-field 0 8))
    (write-field! header 124 12 (octal-field size 12))
    (write-field! header 136 12 (octal-field 0 12))
    (dotimes [i 8]
      (aset-byte header (+ 148 i) (byte 32)))
    (write-field! header 156 1 "0")
    (write-field! header 257 6 "ustar")
    (write-field! header 263 2 "00")
    (let [checksum (reduce + (map #(bit-and % 0xff) header))]
      (write-field! header 148 8 (str (format "%06o" checksum) "\0 ")))
    header))

(defn write-tar! [output-file entries]
  (let [output-path (fs/path output-file)
        output-file (fs/file output-path)]
    (when-let [parent (fs/parent output-path)]
      (fs/create-dirs parent))
    (with-open [out (tar-output-stream output-file)]
      (doseq [{:keys [member-path source-file]} entries
              :let [source-file (fs/file source-file)
                    size (.length source-file)]]
        (.write out (header member-path size))
        (io/copy source-file out)
        (when-let [padding (not-empty (byte-array (padding-size size)))]
          (.write out padding)))
      (.write out (byte-array 1024)))
    output-file))

(defn- read-block! [in archive-file]
  (let [buffer (byte-array 512)]
    (loop [offset 0]
      (let [read-count (.read in buffer offset (- 512 offset))]
        (cond
          (and (neg? read-count) (zero? offset))
          nil

          (neg? read-count)
          (throw (ex-info "Tar archive ended mid-header"
                          {:path (str archive-file)}))

          (= 512 (+ offset read-count))
          buffer

          :else
          (recur (+ offset read-count)))))))

(defn- zero-block? [bytes]
  (every? zero? bytes))

(defn- tar-string [header offset width]
  (let [limit (+ offset width)
        end (loop [i offset]
              (if (or (= i limit)
                      (zero? (aget header i)))
                i
                (recur (inc i))))]
    (string/trim (String. header offset (- end offset)
                          StandardCharsets/US_ASCII))))

(defn- tar-size [header]
  (let [value (tar-string header 124 12)]
    (if (string/blank? value)
      0
      (Long/parseLong value 8))))

(defn- read-exactly! [in size archive-file member-path]
  (when (> size Integer/MAX_VALUE)
    (throw (ex-info "Tar member is too large to read into memory"
                    {:path (str archive-file)
                     :member_path member-path
                     :size size})))
  (let [buffer (byte-array (int size))]
    (loop [offset 0]
      (when (< offset size)
        (let [read-count (.read in buffer offset (- size offset))]
          (when (neg? read-count)
            (throw (ex-info "Tar archive member ended early"
                            {:path (str archive-file)
                             :member_path member-path})))
          (recur (+ offset read-count)))))
    buffer))

(defn- skip-exactly! [in size archive-file]
  (loop [remaining size]
    (when (pos? remaining)
      (let [skipped (.skip in remaining)]
        (cond
          (pos? skipped)
          (recur (- remaining skipped))

          (neg? (.read in))
          (throw (ex-info "Tar archive ended mid-member"
                          {:path (str archive-file)}))

          :else
          (recur (dec remaining)))))))

(defn member-bytes [archive-file member-path]
  (with-open [in (tar-input-stream archive-file)]
    (loop []
      (let [header (read-block! in archive-file)]
        (cond
          (nil? header)
          nil

          (zero-block? header)
          nil

          :else
          (let [name (tar-string header 0 100)
                size (tar-size header)
                padding (padding-size size)]
            (if (= name member-path)
              (let [bytes (read-exactly! in size archive-file member-path)]
                (skip-exactly! in padding archive-file)
                bytes)
              (do
                (skip-exactly! in (+ size padding) archive-file)
                (recur)))))))))
