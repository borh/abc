# Source Bundle Identity Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (- [ ]) syntax for tracking.

**Goal:** Replace overloaded Aozora source identity with bounded archive, bundle, member, and primary-text identities; make build → workset → snapshot composition succeed and close D7.

**Architecture:** ABC/Clojure is the sole v1 bundle-hash producer. A bounded ZIP inspector emits exact archive provenance and a canonical all-member identity; Rust accepts that authoritative bundle hash but independently carries the adapter-computed primary-text hash. Build, reuse, worksets, snapshots, and P16 consume the explicit roles without requiring unlike hashes to equal.

**Tech Stack:** Clojure/JVM 21, Apache Commons Compress 1.28.0, ICU4J 78.3, RFC 8785/JCS helpers, JSON Schema 2020-12, Rust/serde/clap, test.check/Kaocha, Nix flakes.

## Global Constraints

- abc-source-bundle-v1 identity contains construction, sorted path/member_hash records, and primary_text_member. Byte lengths are persisted metadata, never identity inputs.
- Entry-name precedence is strict bit-11 UTF-8, valid Info-ZIP Unicode Path extra field, then strict windows-31j raw bytes.
- NFC-normalized slash paths are identity. Absolute, drive-qualified, empty, dot, dot-dot, NFC-duplicate, and ICU4J-78.3-full-case-fold-colliding paths fail.
- The immutable v1 primary exclusion policy is __MACOSX/** plus basenames beginning ._. Every excluded member remains in bundle identity.
- Limits: 1,024 members; 16,777,216 actual uncompressed bytes per member; 33,554,432 total actual uncompressed bytes.
- V1 never invokes 7zz. Java-unreadable archives fail admission.
- work_content_hash equals bundle_hash. official-source source_hash aliases archive_hash. AAT meta.source_hash aliases primary_text_hash. No global alias exists.
- primary_text_hash must equal member_hash(primary_text_member), independently computed by ABC and the adapter.
- Rust accepts bundle identity but never computes JCS or bundle_hash.
- Strict builds abort atomically. Best-effort builds record derive_failures. Any nonzero derive_failed_count is not release-admissible.
- Historical readers/fixtures remain readable; old hashes are never reinterpreted.
- D7 stays open until image-change, repack, integrity, and build→workset→snapshot evidence all pass.
- Preserve unrelated user changes. Do not touch the two unrelated Python report files in this feature branch.

---

### Task 1: Bounded source-bundle inspector and schema

**Files:**
- Create: abc/schemas/source-bundle.schema.json
- Create: abc/src/abc/tools/source_bundle.clj
- Create: abc/test/abc/tools/source_bundle_test.clj
- Modify: abc/test/abc/tools/schema_test.clj

**Interfaces:**
- Produces source-bundle/inspect-zip with arities [zip-file] and [zip-file limits].
- Result keys: :identity-object, :bundle-hash, :archive-hash, :members, :primary-text-member, :primary-text-hash, :primary-text-bytes.
- Produces source-bundle/write-manifest! [path inspection].
- Admission ExceptionInfo carries :reason and :archive-path plus relevant path/limit data.

- [ ] **Step 1: Write the schema and failing tests**

Create source-bundle.schema.json:

~~~json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://w3id.org/abc/schemas/source-bundle.schema.json",
  "title": "ABC Source Bundle",
  "type": "object",
  "additionalProperties": false,
  "required": ["source_bundle_schema_id", "bundle_hash_algorithm", "bundle_hash", "archive_hash", "identity_object", "members"],
  "properties": {
    "source_bundle_schema_id": {"const": "https://w3id.org/abc/schemas/source-bundle.schema.json"},
    "bundle_hash_algorithm": {"const": "sha256-rfc8785-jcs-abc-source-bundle-v1"},
    "bundle_hash": {"$ref": "#/$defs/hash"},
    "archive_hash": {"$ref": "#/$defs/hash"},
    "identity_object": {"$ref": "#/$defs/identity"},
    "members": {"type": "array", "items": {"$ref": "#/$defs/memberMetadata"}}
  },
  "$defs": {
    "hash": {"type": "string", "pattern": "^sha256:[0-9a-f]{64}$"},
    "identity": {
      "type": "object", "additionalProperties": false,
      "required": ["construction", "members", "primary_text_member"],
      "properties": {
        "construction": {"const": "abc-source-bundle-v1"},
        "members": {"type": "array", "items": {"$ref": "#/$defs/memberIdentity"}},
        "primary_text_member": {"type": "string", "minLength": 1}
      }
    },
    "memberIdentity": {
      "type": "object", "additionalProperties": false,
      "required": ["path", "member_hash"],
      "properties": {"path": {"type": "string", "minLength": 1}, "member_hash": {"$ref": "#/$defs/hash"}}
    },
    "memberMetadata": {
      "type": "object", "additionalProperties": false,
      "required": ["path", "decoded_path", "name_source", "byte_length", "member_hash"],
      "properties": {
        "path": {"type": "string", "minLength": 1},
        "decoded_path": {"type": "string", "minLength": 1},
        "name_source": {"enum": ["efs-utf8", "unicode-extra", "windows-31j"]},
        "byte_length": {"type": "integer", "minimum": 0},
        "member_hash": {"$ref": "#/$defs/hash"}
      }
    }
  }
}
~~~

Create source_bundle_test.clj with ZIP-building helpers and tests for: stable canonical identity; repack invariance across order/time/comment/compression; sensitivity to member add/remove/rename/bytes; AppleDouble exclusion while retained; zero/multiple semantic text rejection; EFS UTF-8 names; validated Unicode Path extra fields; strict windows-31j fallback; malformed-name rejection; NFC identity; unsafe segments; NFC and ICU full-fold collisions including É/é; unreadable ZIP; declared and actual limit overruns; exact primary bytes/hash; schema validation; and absence of byte_length under identity_object.

The happy-path test must include:

~~~clojure
(deftest inspect-zip-separates-identities-test
  (with-zip [zip [["work.txt" (.getBytes "本文" StandardCharsets/UTF_8)]
                  ["fig/one.png" (byte-array [1 2 3])]]]
    (let [{:keys [identity-object bundle-hash archive-hash members
                  primary-text-member primary-text-hash primary-text-bytes]}
          (source-bundle/inspect-zip zip)
          by-path (into {} (map (juxt #(get % "path") identity) members))]
      (is (= "abc-source-bundle-v1" (get identity-object "construction")))
      (is (= "work.txt" primary-text-member))
      (is (= primary-text-hash (get-in by-path ["work.txt" "member_hash"])))
      (is (= primary-text-hash
             (hash/format-sha256 (hash/sha256-bytes primary-text-bytes))))
      (is (not= archive-hash bundle-hash))
      (is (every? #(not (contains? % "byte_length"))
                  (get identity-object "members"))))))
~~~

- [ ] **Step 2: Verify RED**

Run from abc/:

~~~sh
clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.source-bundle-test
~~~

Expected: missing abc.tools.source-bundle namespace.

- [ ] **Step 3: Implement the inspector**

Create source_bundle.clj with:

~~~clojure
(ns abc.tools.source-bundle
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [com.ibm.icu.lang UCharacter]
           [java.io ByteArrayOutputStream]
           [java.nio ByteBuffer]
           [java.nio.charset Charset CodingErrorAction StandardCharsets]
           [java.security DigestInputStream MessageDigest]
           [java.text Normalizer Normalizer$Form]
           [org.apache.commons.compress.archivers.zip
            ZipArchiveEntry ZipArchiveEntry$NameSource ZipFile]))

(def construction "abc-source-bundle-v1")
(def schema-id "https://w3id.org/abc/schemas/source-bundle.schema.json")
(def bundle-hash-algorithm "sha256-rfc8785-jcs-abc-source-bundle-v1")
(def default-limits {:max-members 1024
                     :max-member-bytes 16777216
                     :max-total-bytes 33554432})
(def ^:private legacy-name-charset (Charset/forName "windows-31j"))

(defn- fail! [reason archive-path data]
  (throw (ex-info (str "source bundle admission failed: " (name reason))
                  (merge {:reason reason :archive-path (str archive-path)} data))))

(defn- unicode-fold [s]
  (UCharacter/foldCase ^String s true))

(defn- strict-decode [charset raw]
  (str (.decode (doto (.newDecoder ^Charset charset)
                  (.onMalformedInput CodingErrorAction/REPORT)
                  (.onUnmappableCharacter CodingErrorAction/REPORT))
                (ByteBuffer/wrap raw))))

(defn- decoded-entry-name [archive-path ^ZipArchiveEntry entry]
  (try
    (case (.getNameSource entry)
      ZipArchiveEntry$NameSource/UNICODE_EXTRA_FIELD (.getName entry)
      ZipArchiveEntry$NameSource/NAME_WITH_EFS_FLAG
      (strict-decode StandardCharsets/UTF_8 (.getRawName entry))
      ZipArchiveEntry$NameSource/NAME
      (strict-decode legacy-name-charset (.getRawName entry)))
    (catch Throwable t
      (fail! :invalid-member-name-encoding archive-path
             {:name-source (str (.getNameSource entry))
              :cause (.getMessage t)}))))

(defn- normalize-member-path [archive-path decoded]
  (let [nfc (Normalizer/normalize (string/replace decoded "\\" "/")
                                  Normalizer$Form/NFC)
        segments (string/split nfc #"/" -1)]
    (when (or (string/starts-with? nfc "/")
              (re-find #"^[A-Za-z]:" nfc)
              (some #{"" "." ".."} segments))
      (fail! :unsafe-member-path archive-path
             {:decoded-path decoded :normalized-path nfc}))
    nfc))

(defn- packaging-metadata? [path]
  (or (string/starts-with? path "__MACOSX/")
      (string/starts-with? (last (string/split path #"/")) "._")))

(defn- primary-candidate? [path]
  (and (string/ends-with? (string/lower-case path) ".txt")
       (not (packaging-metadata? path))))
~~~

Implement inspect-zip by enumerating and validating names before reading data. Open with the Commons builder:

~~~clojure
(-> (ZipFile/builder)
    (.setFile (io/file zip-file))
    (.setCharset legacy-name-charset)
    (.setUseUnicodeExtraFields true)
    (.get))
~~~

Use decoded-entry-name so EFS, validated Unicode extra fields, and strict legacy fallback follow the construction. Stream each entry through DigestInputStream with an 8 KiB buffer, counting actual member and total bytes; retain bytes only for the chosen primary member. Sort member records by normalized path and use unicode-fold only for the collision map.

Hash identity and archive exactly:

~~~clojure
(hash/format-sha256 (hash/sha256-json-jcs identity-object))
(hash/format-sha256 (files/sha256-file zip-file))
~~~

Implement manifest writing:

~~~clojure
(defn write-manifest! [path inspection]
  (let [file (io/file path)]
    (json/write-deterministic-json-file!
     file
     {"source_bundle_schema_id" schema-id
      "bundle_hash_algorithm" bundle-hash-algorithm
      "bundle_hash" (:bundle-hash inspection)
      "archive_hash" (:archive-hash inspection)
      "identity_object" (:identity-object inspection)
      "members" (:members inspection)})
    file))
~~~

- [ ] **Step 4: Verify GREEN and commit**

~~~sh
clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.source-bundle-test --focus abc.tools.schema-test
git add schemas/source-bundle.schema.json src/abc/tools/source_bundle.clj test/abc/tools/source_bundle_test.clj test/abc/tools/schema_test.clj
git commit -m "feat(source): add bounded source-bundle identity inspector"
~~~

Expected: focused tests pass and diff check is clean.

---

### Task 2: Reproducible pinned-corpus evidence

**Files:**
- Create: abc/src/abc/tools/source_bundle_report.clj
- Create: abc/test/abc/tools/source_bundle_report_test.clj
- Create: abc/data/source-bundle/aozorabunko-0e9ea3e-summary.json
- Modify: abc/flake.nix
- Modify: root justfile

**Interfaces:**
- source-bundle-report/measure! [aozora-root] returns deterministic summary.
- CLI writes canonical summary JSON.
- Nix check recomputes against pinned aozorabunko-src and compares bytes.

- [ ] **Step 1: Write failing synthetic report tests**

Build a tiny cards tree containing normal, AppleDouble, asset-only, damaged, collision, and limit cases. Assert counters/maxima. Assert checked evidence has these exact values:

~~~json
{
  "aozorabunko_commit": "0e9ea3e586eb0aa34039fabfc85a407d2f98b165",
  "readable_zip_count": 17884,
  "unreadable_zip_count": 3,
  "semantic_text_member_counts": {"0": 5, "1": 17879},
  "utf8_flagged_entry_count": 0,
  "legacy_flagged_entry_count": 22860,
  "nfc_collision_bundle_count": 0,
  "unicode_case_collision_bundle_count": 0,
  "max_member_count": 778,
  "max_member_bytes": 12631833,
  "max_total_bytes": 27874310,
  "java_unreadable_7zz_recoverable_count": 1,
  "java_unreadable_7zz_unrecoverable_count": 2
}
~~~

- [ ] **Step 2: Verify RED**

~~~sh
clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.source-bundle-report-test
~~~

Expected: missing report namespace.

- [ ] **Step 3: Implement report and Nix check**

Walk only cards/*/files/*.zip. Reuse Task 1 metadata normalization. Invoke 7zz l -slt only to classify unreadable evidence, never to admit/hash. Record all damaged paths. Expose a flake check named source-bundle-corpus that runs against the pinned input and compares canonical JSON with the checked file.

Add root recipe:

~~~make
source-bundle-corpus-check:
    nix build ./abc#checks.x86_64-linux.source-bundle-corpus --print-build-logs
~~~

- [ ] **Step 4: Verify GREEN and commit**

~~~sh
clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.source-bundle-report-test
nix build ./abc#checks.x86_64-linux.source-bundle-corpus --print-build-logs
git add src/abc/tools/source_bundle_report.clj test/abc/tools/source_bundle_report_test.clj data/source-bundle/aozorabunko-0e9ea3e-summary.json flake.nix ../justfile
git commit -m "test(source): pin Aozora bundle admission evidence"
~~~

---

### Task 3: Rotate AAT and parser-IR identity protocol

**Files:**
- Modify: ab-validator/data/aat-schema.json and aat-schema-v1.json
- Modify: ab-validator/crates/ab-aozora-aat/src/lib.rs and committed goldens
- Modify: abc/schemas/parser-ir.schema.json
- Modify: both ab-validator parser-IR schema mirrors
- Modify: ab-validator/crates/ab-aat-to-parser-ir/src/convert.rs, main.rs, and integration tests
- Regenerate: ab-validator/data/aat-to-parser-ir-mapping-v1.json and v2.json

**Interfaces:**
- New AAT writers emit equal source_hash and primary_text_hash; historical source_hash-only AAT remains readable.
- ConversionOptions gains work_content_hash: Option<String>.
- CLI gains --work-content-hash.
- Parser-IR schema allows primary_text_hash for historical-reader compatibility; every new converter output emits it alongside work_content_hash.

- [ ] **Step 1: Write failing Rust tests**

~~~rust
#[test]
fn supplied_bundle_hash_is_distinct_from_primary_text_hash() {
    let (schemas, mapping) = schemas_and_mapping();
    let aat = json!({
        "version": 1,
        "work_id": "identity-test",
        "blocks": [],
        "meta": base_meta(
            "utf-8",
            "sha256:1111111111111111111111111111111111111111111111111111111111111111"
        )
    });
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat, mapping, schemas,
        options: ConversionOptions {
            work_content_hash: Some(
                "sha256:2222222222222222222222222222222222222222222222222222222222222222"
                    .to_owned()
            ),
            ..ConversionOptions::default()
        },
    }).unwrap();
    assert_eq!(
        output.parser_ir["source"]["work_content_hash"],
        "sha256:2222222222222222222222222222222222222222222222222222222222222222"
    );
    assert_eq!(
        output.parser_ir["source"]["primary_text_hash"],
        "sha256:1111111111111111111111111111111111111111111111111111111111111111"
    );
}

#[test]
fn aat_primary_text_alias_mismatch_is_rejected() {
    let (schemas, mapping) = schemas_and_mapping();
    let mut aat = json!({
        "version": 1,
        "work_id": "identity-test",
        "blocks": [],
        "meta": base_meta(
            "utf-8",
            "sha256:1111111111111111111111111111111111111111111111111111111111111111"
        )
    });
    aat["meta"]["primary_text_hash"] = json!(
        "sha256:2222222222222222222222222222222222222222222222222222222222222222"
    );
    let error = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat, mapping, schemas,
        options: ConversionOptions::default(),
    }).unwrap_err();
    assert!(error.to_string().contains("primary_text_hash"), "{error}");
}
~~~

Add CLI coverage and an ab-aozora test pinning equal emitted aliases.

- [ ] **Step 2: Verify RED**

~~~sh
nix build ./ab-validator#checks.x86_64-linux.cargo-test --print-build-logs
~~~

Expected: option/fields do not compile.

- [ ] **Step 3: Implement protocol**

Add to ConversionOptions and Default:

~~~rust
pub work_content_hash: Option<String>,
~~~

Change map_source to use meta.primary_text_hash with historical source_hash fallback, reject mismatched co-present aliases, validate supplied work hash format, and emit:

~~~rust
json!({
    "work_content_hash": options.work_content_hash.as_deref().unwrap_or(primary_text_hash),
    "primary_text_hash": primary_text_hash,
    "source_path": null,
    "encoding": encoding,
    "normalization": "source",
})
~~~

Add optional CLI field and pass it through. Add primary_text_hash to AAT emission. Allow it optionally in AAT schemas and parser-IR schema so historical documents remain valid; converter tests require every new output to emit it. Bump parser-IR version 0.6.0 → 0.7.0. Copy ABC schema byte-for-byte to both mirrors.

- [ ] **Step 4: Rotate mapping/goldens**

Update divergence recording to distinguish source.primary_text_hash and source.work_content_hash. Rotate mapping versions and computed schema hashes without hand-copying a hash:

~~~sh
cd ab-validator
python - <<'PY'
import json
from pathlib import Path
import sys

root = Path.cwd()
sys.path.insert(0, str(root / "reports/aat-fidelity/aat_parser_ir_mapping"))
import c14n

schema_hash = c14n.schema_hash(root.parent / "abc/schemas/parser-ir.schema.json")
for rel in ["data/aat-to-parser-ir-mapping-v1.json",
            "data/aat-to-parser-ir-mapping-v2.json"]:
    path = root / rel
    value = json.loads(path.read_text(encoding="utf-8"))
    value["mapping_version"] = "0.3.0"
    value["target_parser_ir_schema_hash"] = schema_hash
    path.write_text(json.dumps(value, ensure_ascii=False, indent=2) + "\n",
                    encoding="utf-8")
PY
~~~

Regenerate AAT goldens mechanically:

~~~sh
cd ab-validator
python - <<'PY'
import json
from pathlib import Path

for path in Path("crates/ab-aozora-aat/tests/goldens").glob("*.expected.json"):
    value = json.loads(path.read_text(encoding="utf-8"))
    meta = value["meta"]
    meta["primary_text_hash"] = meta["source_hash"]
    path.write_text(json.dumps(value, ensure_ascii=False, sort_keys=True,
                               separators=(",", ":")) + "\n",
                    encoding="utf-8")
PY
~~~

Inspect the diff to confirm every golden change is only the equal alias.

- [ ] **Step 5: Verify GREEN and commit**

~~~sh
nix build ./ab-validator#checks.x86_64-linux.cargo-test --print-build-logs
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy --print-build-logs
nix build ./ab-validator#checks.x86_64-linux.cargo-fmt --print-build-logs
cd abc && clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.schema-test
git add ../ab-validator/data ../ab-validator/crates/ab-aozora-aat ../ab-validator/crates/ab-aat-to-parser-ir schemas/parser-ir.schema.json test/abc/tools/schema_test.clj
git commit -m "feat(parser-ir): separate bundle and primary-text identity"
~~~

---

### Task 4: Integrate identity into publication build artifacts

**Files:**
- Modify: abc/src/abc/tools/soranoha_build_publication.clj
- Modify: abc/test/abc/tools/soranoha_test.clj
- Modify: abc/test/abc/sim/content_sim_test.clj

**Interfaces:**
- Deriver receives :work-content-hash.
- Per-work output adds source-bundle.json.
- official-source emits archive_hash, bundle_hash, primary_text_member, primary_text_hash, and source_hash=archive_hash.
- Selection records all explicit hashes; compatibility source_hash remains archive hash.

- [ ] **Step 1: Write failing build assertions**

Assert source-bundle exists; archive alias equals; official/parser bundle hashes equal; official/parser primary hashes equal; primary hash equals primary member hash. Add a repack test: archive changes while bundle/primary remain stable.

- [ ] **Step 2: Verify RED**

~~~sh
clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.soranoha-test
~~~

- [ ] **Step 3: Integrate inspector**

Require abc.tools.source-bundle. Delete sevenzip-first-text-member, read-first-text-member, and extraction-only code. In write-materialized-work!:

~~~clojure
(let [inspection (source-bundle/inspect-zip file)
      work-hash (:bundle-hash inspection)
      archive-hash (:archive-hash inspection)
      primary-text-hash (:primary-text-hash inspection)
      zip-member (:primary-text-member inspection)
      source-bytes (:primary-text-bytes inspection)
      source-bundle-file (io/file work-dir "source-bundle.json")]
  (source-bundle/write-manifest! source-bundle-file inspection)
  (*derive-parser-ir!* {:parser-profile parser-profile
                        :source-bytes source-bytes
                        :work-content-hash work-hash
                        :aat-file aat-file
                        :parser-ir-file parser-ir-file
                        :divergence-file divergence-file}))
~~~

Pass --work-content-hash to real converter. After conversion, assert parser bundle hash equals supplied hash and parser primary hash equals member hash. Throw clean ex-info with all four values on mismatch. Use bundle hash for manifests, marker, and reuse.

- [ ] **Step 4: Update stub while leaving D7 open**

Stub accepts work-content-hash, emits equal AAT aliases, and parser source:

~~~clojure
{"work_content_hash" work-content-hash
 "primary_text_hash" member-hash
 "encoding" "utf-8"
 "normalization" "source"}
~~~

D7 stays open until snapshot validation changes.

- [ ] **Step 5: Verify GREEN and commit**

~~~sh
clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.soranoha-test --focus abc.sim.content-sim-test
git add src/abc/tools/soranoha_build_publication.clj test/abc/tools/soranoha_test.clj test/abc/sim/content_sim_test.clj
git commit -m "feat(publication): materialize source-bundle identity"
~~~

---

### Task 5: Admission disposition and release gate

**Files:**
- Modify: abc/src/abc/tools/soranoha_build_publication.clj
- Modify: abc/test/abc/tools/soranoha_test.clj
- Modify: abc/test/abc/sim/content_sim_test.clj

**Interfaces:**
- Best-effort failure records expose stable reason/path/limit fields.
- release_admissible equals derive_failed_count==0.
- Strict mode remains atomic abort. A best-effort run with failures promotes its evidence, marks the selection/workflow partial, returns 1, and remains non-releaseable.

- [ ] **Step 1: Write failing strict/best-effort tests**

Use one valid work and one unsafe-path ZIP. Strict asserts throw and absent promoted output. Best-effort asserts valid work passes, invalid work appears once with reason unsafe-member-path, count 1, release_admissible false. Add damaged ZIP proof that no injected process runner/7zz path is called.

- [ ] **Step 2: Verify RED**

~~~sh
clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.soranoha-test --focus abc.sim.content-sim-test
~~~

- [ ] **Step 3: Implement disposition**

In best-effort catch, retain stable diagnostics:

~~~clojure
(let [d (ex-data t)]
  {:failed (cond-> {"work_id" (row-work-id (:row candidate))
                    "person_id" (row-person-id (:row candidate))
                    "text_zip_relpath" (:relpath candidate)
                    "error" (.getMessage t)
                    "reason" (some-> (:reason d) name)}
             (:archive-path d) (assoc "archive_path" (:archive-path d))
             (:decoded-path d) (assoc "decoded_path" (:decoded-path d))
             (:normalized-path d) (assoc "normalized_path" (:normalized-path d))
             (:limit d) (assoc "limit" (:limit d))
             (:actual d) (assoc "actual" (:actual d)))})
~~~

Add release_admissible to the report. Return :partial from the selection step when false; retain the completed workflow result, atomically promote the best-effort evidence, and return exit value 1 from build-publication! when the final workflow run is partial (0 only when passed).

- [ ] **Step 4: Verify GREEN and commit**

~~~sh
clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.soranoha-test --focus abc.sim.content-sim-test
git add src/abc/tools/soranoha_build_publication.clj test/abc/tools/soranoha_test.clj test/abc/sim/content_sim_test.clj
git commit -m "feat(publication): gate source-bundle admission failures"
~~~

---

### Task 6: Workset and snapshot role validation

**Files:**
- Modify: abc/src/abc/tools/source_snapshot_workset.clj
- Modify: abc/src/abc/tools/materialize_source_snapshot.clj
- Modify: abc/test/abc/tools/source_snapshot_fixture.clj
- Modify: abc/test/abc/tools/source_snapshot_workset_test.clj
- Modify: abc/test/abc/tools/materialize_source_snapshot_test.clj

**Interfaces:**
- New worksets resolve source_bundle_path; historical worksets remain readable only in complete legacy mode.
- Snapshot inputs record archive, bundle/work, primary, primary member, and source-bundle file hash.
- Each role validates its own construction; archive never equals bundle by requirement.

- [ ] **Step 1: Write failing composition and corruption tests**

Use a real deterministic ZIP/source-bundle fixture. Independently corrupt official archive alias, source-bundle hash, parser bundle hash, parser primary hash, and primary member hash. Assert distinct messages/keys. Add full legacy fixture coverage.

- [ ] **Step 2: Verify RED**

~~~sh
clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.source-snapshot-workset-test --focus abc.tools.materialize-source-snapshot-test
~~~

- [ ] **Step 3: Propagate paths and validate modes**

Add source-bundle-path to new required files and source_bundle_path→resolved_source_bundle_path mapping. Accept legacy only when all new fields/path are absent; reject partial mixed mode.

- [ ] **Step 4: Replace cross-role equality**

New mode asserts:

~~~clojure
(= (get official-source "source_hash") (get official-source "archive_hash"))
(= (get official-source "bundle_hash")
   (get source-bundle "bundle_hash")
   (get-in parser-ir ["source" "work_content_hash"]))
(= (get official-source "primary_text_hash")
   (get-in parser-ir ["source" "primary_text_hash"])
   (member-hash source-bundle (get official-source "primary_text_member")))
~~~

Recompute bundle hash from identity_object. Snapshot cannot rehash an absent upstream ZIP, so it validates archive alias/shape; build remains responsible for actual ZIP comparison.

Update source-manifest emission so manifest_identity_object.work_content_hash remains bundle_hash, but content.content_hash is `(manifest/file-hash source-bundle-file)`, media_type is `application/json`, and path_hint is `source-bundle.json`. This preserves ADR 0001's derivation-ID versus materialized-byte-hash distinction.

- [ ] **Step 5: Verify GREEN and commit**

~~~sh
clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.source-snapshot-workset-test --focus abc.tools.materialize-source-snapshot-test
git add src/abc/tools/source_snapshot_workset.clj src/abc/tools/materialize_source_snapshot.clj test/abc/tools/source_snapshot_fixture.clj test/abc/tools/source_snapshot_workset_test.clj test/abc/tools/materialize_source_snapshot_test.clj
git commit -m "feat(snapshot): validate source identity roles independently"
~~~

---

### Task 7: P16 image/repack evolution and D7 closure

**Files:**
- Modify: abc/test/abc/sim/model.clj, gen.clj, render.clj, oracle.clj
- Modify: abc/test/abc/sim/content_test.clj, content_sim_test.clj, divergences.clj

**Interfaces:**
- Content becomes {:text string :images sorted-map}; image events are total.
- Render can repack identical members with changed order/time/comment/compression.
- Oracle predicts archive, bundle, and primary hashes.
- D7 becomes fixed and P16.3 asserts direct success.

- [ ] **Step 1: Write failing model/oracle tests**

Add :add-image, :edit-image, :remove-image tests. Pin:

- image edit changes bundle only and rebuilds;
- metadata-only repack changes archive only and reuses;
- text edit changes bundle+primary and rebuilds;
- AppleDouble remains member but not primary;
- admission rejection strict/best-effort dispositions.

- [ ] **Step 2: Verify RED**

~~~sh
clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.content-test --focus abc.sim.content-sim-test
~~~

- [ ] **Step 3: Extend render/oracle**

Render sorted text+image members. Add zip-layout controls for order, mtime, comment, compression. The oracle independently constructs the specified sorted identity object from model members and hashes it; it must not call source-bundle/inspect-zip or its identity constructor. It independently hashes rendered archive bytes for archive_hash. Rename ambiguous :source_hash oracle data to :bundle_hash and update every consumer.

- [ ] **Step 4: Flip P16.3 and D7**

Remove expected-failure gate. Assert archive alias, recomputed bundle, parser supplied bundle, primary/member equality, and snapshot roles for every work. Set:

~~~clojure
:D7 {:case "P16.3 pin-chain" :status :fixed
     :notes "fixed 2026-07-12: ADR 0033 separates archive, bundle, member, and primary-text identity; build supplies bundle identity and snapshot validates roles independently"}
~~~

- [ ] **Step 5: Verify GREEN and commit**

~~~sh
TEI_SCHEMA_PATH=$PWD/schemas/tei-profile.rng clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.content-sim-test
TEI_SCHEMA_PATH=$PWD/schemas/tei-profile.rng clojure -M:test:kaocha -m kaocha.runner --focus :simulation
git add test/abc/sim
git commit -m "test(sim): close D7 with source-bundle evolution evidence"
~~~

Expected: no D7 gate, below 90 seconds, and deterministic image/repack cases execute per CI run.

---

### Task 8: Full verification and ADR promotion

**Files:**
- Modify: abc/docs/adr/0033-source-bundle-identity.md
- Modify: abc/docs/superpowers/specs/2026-07-12-source-bundle-identity-design.md implementation appendix

- [ ] **Step 1: Run full gates**

~~~sh
just python-quality
just nix-format-check
nix build ./abc#checks.x86_64-linux.clj-kondo --print-build-logs
nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests --print-build-logs
nix build ./abc#checks.x86_64-linux.source-bundle-corpus --print-build-logs
nix build ./ab-validator#checks.x86_64-linux.cargo-check --print-build-logs
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy --print-build-logs
nix build ./ab-validator#checks.x86_64-linux.cargo-fmt --print-build-logs
just validate-migration
~~~

From abc/ also run:

~~~sh
TEI_SCHEMA_PATH=$PWD/schemas/tei-profile.rng clojure -M:test:kaocha -m kaocha.runner --focus :simulation
TEI_SCHEMA_PATH=$PWD/schemas/tei-profile.rng clojure -M:test:kaocha -m kaocha.runner --focus :unit
~~~

Expected: every command exits 0. The two pre-existing Python formatting files are repaired on main in a separate pre-execution commit; if Python quality fails here, stop and report rather than mixing unrelated repairs into this feature branch.

- [ ] **Step 2: Audit negative requirements**

~~~sh
rg -n "sevenzip-first-text-member|7zz x" abc/src/abc/tools/soranoha_build_publication.clj
rg -n "expected-failure.*D7|:D7.*:open" abc/test
rg -n "primary_text_hash|archive_hash|bundle_hash" abc/src abc/test ab-validator/crates/ab-aat-to-parser-ir
~~~

Expected: no active 7zz fallback; no open/gated D7; all identity roles cross producer/consumer boundaries.

- [ ] **Step 3: Promote ADR**

Only after all gates pass, set:

~~~markdown
Status: Accepted
Date: 2026-07-12
Accepted: 2026-07-12
~~~

Replace Implementation Status with a dated evidence list naming the inspector, schema, corpus Nix check, Rust option/tests, build/workset/snapshot tests, P16 image/repack/composition evidence, and fixed D7. Append the same implementation summary to the spec.

Run:

~~~sh
cd abc && clojure -M:abc/adr-governance
~~~

- [ ] **Step 4: Commit and request broad review**

~~~sh
git add abc/docs/adr/0033-source-bundle-identity.md abc/docs/superpowers/specs/2026-07-12-source-bundle-identity-design.md
git commit -m "docs(adr): accept source-bundle identity contract"
~~~

Review the full merge-base..HEAD range for ZIP bounds, decoder determinism, schema/mapping rotation, historical compatibility, admission atomicity, repack reuse, image invalidation, adapter integrity, and D7 timing.
