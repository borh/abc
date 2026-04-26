(ns abc.tools.validate-design-bundle
  (:require [abc.tools.files :as files]
            [abc.tools.materialize-import :as materialize]
            [charred.api :as json]
            [clojure.set :as set]
            [clojure.string :as string]))

(def required-manifest-input-keys
  #{"producer"
    "producer_version"
    "work_id"
    "work_content_hash"
    "parser_build_hash"
    "parser_config_hash"
    "parser_ir_schema_hash"
    "diagnostic_schema_hash"
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
                                          (re-matches files/hash-pattern v)))]
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

(defn validate-json-schemas! [extra-manifest-paths]
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
    "] + " (pr-str (vec (map str extra-manifest-paths))) ":\n"
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
    (files/read-json (files/path "examples" "ab-validator-output" "manifest-inputs.json"))))
  (check-errors!
   (run-summary-errors
    (files/read-json-lines (files/path "examples" "ab-validator-output" "run-summary.jsonl"))))
  (check-errors!
   (comparison-report-errors
    (files/read-json (files/path "examples" "ab-validator-output" "comparison-report.json")))))

(defn validate-canonicalization! []
  (let [expected "9d49ff018a43ac2b24323276424cc325e3a5d0a22716144c8800f9fec0911f0a"
        actual (files/sha256-file (files/path "fixtures" "canonicalization"
                                              "manifest-identity-object.canonical.json"))
        array-a (files/sha256-file (files/path "fixtures" "canonicalization"
                                               "array-ordering-negative-a.json"))
        array-b (files/sha256-file (files/path "fixtures" "canonicalization"
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
  (let [materialized-dir (.toFile (java.nio.file.Files/createTempDirectory
                                   "abc-materialized-import"
                                   (make-array java.nio.file.attribute.FileAttribute 0)))]
    (try
      (println "==> Materializing imported ab-validator output")
      (let [materialized (materialize/materialize-import!
                          {:input-dir (files/path "examples" "ab-validator-output")
                           :output-dir materialized-dir
                           :generated-at materialize/default-generated-at})]
        (println "materialized import ok")
        (println "==> Validating JSON schemas and examples")
        (validate-json-schemas! (vals materialized))
        (println "json schema validation ok"))
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
      (println "design bundle validation ok")
      (finally
        (doseq [file (reverse (file-seq materialized-dir))]
          (.delete file))))))

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
