(ns soranoha.snh.sign
  "Wire encodings and key/signature operations of snh protocol v1.

  Encodings:
  - signed message = exact ASCII bytes of the domain-separated string
    (no trailing newline, no BOM, no framing);
  - .sig file = exactly 64 raw Ed25519 signature bytes;
  - .pub / releases/HEAD files = exactly 65 bytes: 64 lowercase ASCII hex
    characters + one LF;
  - key fingerprint = lowercase sha256 hex over the decoded 32 raw key bytes
    (never over .pub file bytes).

  Roles: two disjoint fixed pinned roles (release, governance); v1 pins
  exactly one key per role. Verification is bound to the artifact: the
  public operation takes the decoded artifact's type + content hex and
  constructs the domain-separated message itself; the role is selected from
  the type. An overlapping or un-roled pinned-keys configuration is
  invalid."
  (:require [soranoha.core.hash :as hash])
  (:import (java.math BigInteger)
           (java.nio.charset StandardCharsets)
           (java.security KeyFactory Signature)
           (java.security.spec EdECPoint EdECPrivateKeySpec EdECPublicKeySpec
                               NamedParameterSpec)))

;; --- hex helpers -----------------------------------------------------------

(defn hex->bytes ^bytes [^String s]
  (when-not (and (string? s) (even? (count s)) (re-matches #"^[0-9a-f]*$" s))
    (throw (ex-info "Expected lowercase hex" {:value s})))
  (byte-array (map #(unchecked-byte (Integer/parseInt (subs s % (+ % 2)) 16))
                   (range 0 (count s) 2))))

;; --- 65-byte hex+LF files (.pub and releases/HEAD share the contract) ------

(def zero-head-hex (apply str (repeat 64 "0")))

(defn hex64-lf-bytes
  "The 65-byte encoding: 64 lowercase hex + one LF."
  ^bytes [hex]
  (hash/assert-hex64 hex)
  (.getBytes (str hex "\n") StandardCharsets/US_ASCII))

(defn parse-hex64-lf
  "Parse a 65-byte hex+LF file; returns the 64-char hex. Throws unless the
  bytes are exactly 64 lowercase hex + one LF."
  [^bytes file-bytes]
  (when-not (= 65 (alength file-bytes))
    (throw (ex-info "hex+LF file must be exactly 65 bytes"
                    {:length (alength file-bytes)})))
  (let [s (String. file-bytes StandardCharsets/US_ASCII)]
    (when-not (= \newline (.charAt s 64))
      (throw (ex-info "hex+LF file must end with one LF" {})))
    (hash/assert-hex64 (subs s 0 64))))

;; --- Ed25519 ---------------------------------------------------------------

(def ^:private key-factory (KeyFactory/getInstance "Ed25519"))

(defn- raw-pub->public-key
  "Decode 32 raw Ed25519 public-key bytes (RFC 8032 encoding: little-endian y,
  high bit of the last byte = x parity) into a java.security PublicKey."
  [^bytes raw]
  (when-not (= 32 (alength raw))
    (throw (ex-info "Ed25519 public key must be 32 raw bytes"
                    {:length (alength raw)})))
  (let [le (byte-array (reverse raw))
        x-odd (pos? (bit-and (aget le 0) 0x80))
        _ (aset-byte le 0 (unchecked-byte (bit-and (aget le 0) 0x7f)))
        y (BigInteger. 1 le)]
    (.generatePublic key-factory
                     (EdECPublicKeySpec. NamedParameterSpec/ED25519
                                         (EdECPoint. x-odd y)))))

(defn fingerprint
  "Lowercase sha256 hex over the decoded 32 raw key bytes."
  [pub-hex]
  (hash/assert-hex64 pub-hex)
  (hash/sha256-bytes (hex->bytes pub-hex)))

(defn sign
  "Ed25519 signature (64 raw bytes) over the exact ASCII bytes of `message`
  with the 32-byte private seed."
  ^bytes [^bytes seed ^String message]
  (when-not (= 32 (alength seed))
    (throw (ex-info "Ed25519 seed must be 32 bytes" {:length (alength seed)})))
  (let [priv (.generatePrivate key-factory
                               (EdECPrivateKeySpec. NamedParameterSpec/ED25519 seed))
        sig (doto (Signature/getInstance "Ed25519") (.initSign priv))]
    (.update sig (.getBytes message StandardCharsets/US_ASCII))
    (.sign sig)))

(defn- verify?
  "True iff `sig-bytes` is exactly 64 bytes and verifies over `message`
  against the public key given as 64 lowercase hex. Private:
  arbitrary-message verification must not be a public operation — public
  callers go through `verify-artifact-signature?`, which constructs the
  message from the artifact itself."
  [pub-hex ^String message ^bytes sig-bytes]
  (and (= 64 (alength sig-bytes))
       (let [pub (raw-pub->public-key (hex->bytes (hash/assert-hex64 pub-hex)))
             ver (doto (Signature/getInstance "Ed25519") (.initVerify pub))]
         (.update ver (.getBytes message StandardCharsets/US_ASCII))
         (.verify ver sig-bytes))))

;; --- domain-separated messages ---------------------------------------------

(defn manifest-message [manifest-id-hex]
  (str "snh-manifest-sig/1:" (hash/assert-hex64 manifest-id-hex)))

(defn event-message [event-hex]
  (str "snh-governance-event-sig/1:" (hash/assert-hex64 event-hex)))

;; --- pinned roles ----------------------------------------------------------

(defn validate-pinned-keys!
  "Validate a pinned-keys configuration {:release pub-hex :governance pub-hex}.
  v1 pins exactly one key per role; scalar values make any other
  cardinality unrepresentable — a set-shaped configuration would silently
  permit unsupported in-chain key addition. The two roles must be
  present, each a single 64-hex public key, and distinct — an overlapping or
  un-roled configuration is invalid (an accidental flat configuration could
  otherwise authorize the online release key for governance)."
  [pinned-keys]
  (when-not (= #{:release :governance} (set (keys pinned-keys)))
    (throw (ex-info "pinned keys must assign exactly the release and governance roles"
                    {:reason :unroled-configuration
                     :roles (keys pinned-keys)})))
  (doseq [[role member] pinned-keys]
    (when-not (string? member)
      (throw (ex-info "a pinned role binds exactly one key in v1"
                      {:reason :invalid-role-cardinality :role role})))
    (hash/assert-hex64 member))
  (when (= (:release pinned-keys) (:governance pinned-keys))
    (throw (ex-info "pinned role keys must be distinct"
                    {:reason :overlapping-roles})))
  pinned-keys)

(def ^:private type->role
  {"release-manifest" :release
   "governance-event" :governance})

(def ^:private type->message-fn
  {"release-manifest" manifest-message
   "governance-event" event-message})

(defn verify-artifact-signature?
  "Role-bound verification, bound to the artifact: the caller supplies
  the decoded artifact's type and content hex; the domain-separated message is
  constructed here from that pair — a caller can never present a signature
  over one domain or subject as authority for another. The role is selected
  from the type; the 64-byte signature must verify against that role's pinned
  key. `pinned-keys` is validated on every call — verification never proceeds
  under an invalid configuration."
  [pinned-keys type artifact-hex ^bytes sig-bytes]
  (validate-pinned-keys! pinned-keys)
  (let [role (or (type->role type)
                 (throw (ex-info "no signing role is defined for this artifact type"
                                 {:type type :signed-types (keys type->role)})))
        message ((type->message-fn type) artifact-hex)]
    (verify? (get pinned-keys role) message sig-bytes)))
