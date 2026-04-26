(ns abc.tools.validate-design-bundle
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as string])
  (:import [java.security MessageDigest]))

(def hash-pattern #"^sha256:[0-9a-f]{64}$")

(defn repo-root []
  (.getCanonicalFile (io/file ".")))

(defn path [& segments]
  (apply io/file (repo-root) segments))

(defn read-json [file]
  (json/read-json (io/file file)))

(defn read-json-lines [file]
  (->> (string/split-lines (slurp (io/file file)))
       (remove string/blank?)
       (mapv json/read-json)))

(defn bytes->hex [bytes]
  (apply str (map #(format "%02x" (bit-and % 0xff)) bytes)))

(defn sha256-file [file]
  (with-open [in (io/input-stream (io/file file))]
    (let [digest (MessageDigest/getInstance "SHA-256")
          buffer (byte-array 8192)]
      (loop []
        (let [n (.read in buffer)]
          (when (pos? n)
            (.update digest buffer 0 n)
            (recur))))
      (bytes->hex (.digest digest)))))

(defn example-hash [suffix]
  (str "sha256:"
       (apply str (repeat (- 64 (count suffix)) "0"))
       suffix))

(def required-manifest-input-keys
  #{"producer"
    "producer_version"
    "work_id"
    "work_content_hash"
    "parser_build_hash"
    "parser_config_hash"
    "parser_ir_schema_hash"
    "warning_sidecar_hash"
    "run_summary_hash"
    "comparison_report_hash"})

(defn manifest-input-errors [manifest-inputs]
  (let [missing (sort (set/difference required-manifest-input-keys
                                      (set (keys manifest-inputs))))
        missing-error (when (seq missing)
                        (str "ab-validator manifest inputs missing keys: "
                             (string/join ", " missing)))
        hash-errors (for [[k v] (sort-by key manifest-inputs)
                          :when (string/ends-with? k "_hash")
                          :when (not (and (string? v)
                                          (re-matches hash-pattern v)))]
                      (str "ab-validator manifest input " k
                           " is not a sha256 hash: " v))]
    (vec (concat (when missing-error [missing-error])
                 hash-errors))))

(defn run-summary-errors [events]
  (let [event-types (mapv #(get % "event") events)]
    (vec
     (concat
      (when (not= ["run-start"] (subvec event-types 0 (min 1 (count event-types))))
        ["ab-validator run summary must start with run-start"])
      (when (not= ["run-complete"] (subvec event-types (max 0 (dec (count event-types)))))
        ["ab-validator run summary must end with run-complete"])
      (when-not (some #{"work-result"} event-types)
        ["ab-validator run summary must include a work-result event"])
      (for [event events
            :when (not (contains? event "run_id"))]
        (str "run summary event is missing run_id: " event))))))

(defn comparison-report-errors [comparison-report]
  (vec
   (concat
    (when (not= "abc.ab-validator-comparison.v0"
                (get comparison-report "report_schema"))
      ["ab-validator comparison report has an unexpected report_schema"])
    (when-not (seq (get comparison-report "parser_candidates"))
      ["ab-validator comparison report must list parser_candidates"]))))

(defn run-command! [& command]
  (let [process (ProcessBuilder. command)
        _ (.inheritIO process)
        started (.start process)
        exit-code (.waitFor started)]
    (when-not (zero? exit-code)
      (throw (ex-info (str "Command failed: " (string/join " " command))
                      {:command command
                       :exit-code exit-code})))))

(defn validate-json-schemas! []
  (run-command!
   "python" "-"
   (str
    "import json\n"
    "from pathlib import Path\n"
    "from jsonschema import Draft202012Validator\n"
    "\n"
    "def load_json(path):\n"
    "    return json.loads(Path(path).read_text(encoding='utf-8'))\n"
    "\n"
    "manifest_schema = load_json('schemas/manifest.schema.json')\n"
    "parser_ir_schema = load_json('schemas/parser-ir.schema.json')\n"
    "Draft202012Validator.check_schema(manifest_schema)\n"
    "Draft202012Validator.check_schema(parser_ir_schema)\n"
    "manifest_validator = Draft202012Validator(manifest_schema)\n"
    "parser_ir_validator = Draft202012Validator(parser_ir_schema)\n"
    "for path in [\n"
    "    'examples/v0/example-work/source.manifest.json',\n"
    "    'examples/v0/example-work/manifest.json',\n"
    "    'examples/v0/example-work/failure-manifest.example.json',\n"
    "]:\n"
    "    manifest_validator.validate(load_json(path))\n"
    "for path in [\n"
    "    'examples/v0/example-work/parser-ir.json',\n"
    "    'examples/ab-validator-output/parser-ir.json',\n"
    "]:\n"
    "    parser_ir_validator.validate(load_json(path))\n"
    "diagnostic_schema = {\n"
    "    '$schema': 'https://json-schema.org/draft/2020-12/schema',\n"
    "    '$defs': parser_ir_schema['$defs'],\n"
    "    '$ref': '#/$defs/diagnostic',\n"
    "}\n"
    "Draft202012Validator.check_schema(diagnostic_schema)\n"
    "diagnostic_validator = Draft202012Validator(diagnostic_schema)\n"
    "for path in [\n"
    "    'examples/v0/example-work/warnings.jsonl',\n"
    "    'examples/ab-validator-output/warnings.jsonl',\n"
    "]:\n"
    "    lines = Path(path).read_text(encoding='utf-8').splitlines()\n"
    "    if not lines:\n"
    "        raise SystemExit(f'{path} must contain at least one diagnostic')\n"
    "    for line in lines:\n"
    "        if line.strip():\n"
    "            diagnostic_validator.validate(json.loads(line))\n"
    "try:\n"
    "    manifest_validator.validate({})\n"
    "except Exception:\n"
    "    pass\n"
    "else:\n"
    "    raise SystemExit('manifest schema accepted an empty object')\n")))

(defn check-errors! [errors]
  (when (seq errors)
    (throw (ex-info (string/join "\n" errors)
                    {:errors errors}))))

(defn validate-ab-validator-output! []
  (check-errors!
   (manifest-input-errors
    (read-json (path "examples" "ab-validator-output" "manifest-inputs.json"))))
  (check-errors!
   (run-summary-errors
    (read-json-lines (path "examples" "ab-validator-output" "run-summary.jsonl"))))
  (check-errors!
   (comparison-report-errors
    (read-json (path "examples" "ab-validator-output" "comparison-report.json")))))

(defn validate-canonicalization! []
  (let [expected "9d49ff018a43ac2b24323276424cc325e3a5d0a22716144c8800f9fec0911f0a"
        actual (sha256-file (path "fixtures" "canonicalization"
                                  "manifest-identity-object.canonical.json"))
        array-a (sha256-file (path "fixtures" "canonicalization"
                                   "array-ordering-negative-a.json"))
        array-b (sha256-file (path "fixtures" "canonicalization"
                                   "array-ordering-negative-b.json"))]
    (when-not (= expected actual)
      (throw (ex-info "canonical identity fixture hash mismatch"
                      {:expected expected
                       :actual actual})))
    (when (= array-a array-b)
      (throw (ex-info "array-ordering negative fixtures produced the same digest"
                      {:digest array-a})))))

(defn validate-xml! []
  (run-command! "xmllint" "--noout"
                "schemas/tei-profile.odd"
                "examples/v0/example-work/tei.xml"))

(defn validate-git-cliff! []
  (run-command! "git-cliff" "--config" "cliff.toml" "--unreleased" "--strip" "header"
                "--output" "/tmp/abc-changelog-check.md"))

(defn validate-design-bundle! []
  (println "==> Validating JSON schemas and examples")
  (validate-json-schemas!)
  (println "json schema validation ok")
  (println "==> Checking imported ab-validator output")
  (validate-ab-validator-output!)
  (println "ab-validator output ok")
  (println "==> Checking canonicalization fixtures")
  (validate-canonicalization!)
  (println "canonicalization fixtures ok")
  (println "==> Checking XML fixtures")
  (validate-xml!)
  (println "xml fixtures ok")
  (println "==> Checking git-cliff configuration")
  (validate-git-cliff!)
  (println "git-cliff config ok")
  (println "design bundle validation ok"))

(defn -main [& _args]
  (try
    (validate-design-bundle!)
    (catch Throwable t
      (binding [*out* *err*]
        (println "design bundle validation failed")
        (println (ex-message t))
        (when-let [errors (:errors (ex-data t))]
          (doseq [error errors]
            (println "-" error))))
      (System/exit 1))))
