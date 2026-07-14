#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
schema="$repo_root/schemas/parser-comparison-study.schema.json"
contract="$repo_root/docs/studies/aozora-parser-comparison-preregistration.json"
hashes="$repo_root/docs/studies/aozora-parser-comparison-preregistration.sha256"

uv run --isolated --no-project --with 'jsonschema[format]>=4.0' python - "$schema" "$contract" "$repo_root" <<'PY'
import hashlib
import json
import sys
from copy import deepcopy
from jsonschema import Draft202012Validator, FormatChecker, ValidationError

schema = json.load(open(sys.argv[1], encoding="utf-8"))
contract = json.load(open(sys.argv[2], encoding="utf-8"))
Draft202012Validator.check_schema(schema)
validator = Draft202012Validator(schema, format_checker=FormatChecker())
validator.validate(contract)

for fixture in (*contract["fixture_manifests"].values(),):
    digest = hashlib.sha256((__import__("pathlib").Path(sys.argv[3]) / fixture["path"]).read_bytes()).hexdigest()
    assert fixture["sha256"] == f"sha256:{digest}"
performance_path = __import__("pathlib").Path(sys.argv[3]) / "docs/studies/fixtures/performance-largest-six-v1.json"
assert contract["performance_protocol"]["workset_sha256"] == f"sha256:{hashlib.sha256(performance_path.read_bytes()).hexdigest()}"

assert {candidate["id"] for candidate in contract["candidates"]} == {
    "aozora", "aozora2", "aozora-rs", "aozora2html", "aozora-epub3",
    "aozora-parser.js", "ab-aozora"
}
assert contract["analysis"]["unqualified_aggregate_score"] == "prohibited"
assert contract["analysis"]["use_case_weighting"]["sensitivity_analysis_required"] is True
assert {axis["id"] for axis in contract["axes"]} == {
    "construct_coverage", "fidelity", "robustness", "diagnostics", "spans",
    "performance", "maintenance", "packaging", "license"
}
assert contract["fixture_manifests"]["robustness"]["sha256"].startswith("sha256:")
assert contract["fixture_manifests"]["diagnostics"]["sha256"].startswith("sha256:")
assert contract["performance_protocol"] == {
    "workset_id": "performance-largest-six-v1",
    "workset_sha256": "sha256:fa67e8b2816be8a2f9bd68ef408c02990041dc7e6e668bc1fb23bc0bc5bc568d",
    "warmup_repetitions": 1,
    "measured_repetitions": 5,
    "candidate_order": ["aozora", "aozora2", "aozora-rs", "aozora2html", "aozora-epub3"],
    "work_order": "manifest_order",
    "concurrency": 1,
    "environment": "x86_64-linux; root flake devShell; RUSTC_WRAPPER=; SCCACHE_DISABLE=1; LC_ALL=C.UTF-8; TZ=UTC; network disabled",
    "timeout_seconds": 300,
    "timeout_treatment": "right_censored_at_300_seconds_and_counted_as_timeout_failure",
    "censored_latency": {
        "estimator": "kaplan_meier",
        "substitution": "none",
        "median_rule": "report_km_median_if_survival_le_0.5_else_unavailable_with_lower_bound_300s",
        "p95_rule": "report_km_p95_if_survival_le_0.05_else_unavailable_with_lower_bound_300s",
        "bootstrap": "candidate_level_stratified_resample_works_with_replacement_then_repetitions_with_replacement_10000_seed_20260714_recompute_km_quantile_preserve_censoring_report_percentile_ci_or_unavailable_bound"
    },
    "host_capture": {
        "required_fields": ["cpu_model", "logical_cpu_count", "ram_bytes", "kernel_release", "cpu_governor", "load_1m", "isolated_cpu_set"],
        "capture_time": "immediately_before_each_candidate_run",
        "comparability": "comparable_only_when_cpu_model_logical_count_ram_kernel_governor_and_isolation_match_and_load_1m_diff_le_0.25; otherwise_report_separate_strata_no_pooled_ratio"
    }
}

for key in ("candidates", "axes", "measurement_modes"):
    duplicate = deepcopy(contract)
    duplicate[key][-1] = duplicate[key][0]
    try:
        validator.validate(duplicate)
    except ValidationError:
        pass
    else:
        raise AssertionError(f"schema accepted duplicate {key}")

bad_format = deepcopy(contract)
bad_format["frozen_at"] = "not-a-date"
try:
    validator.validate(bad_format)
except ValidationError:
    pass
else:
    raise AssertionError("format checker accepted invalid frozen_at")
PY

(cd "$repo_root" && sha256sum --check "docs/studies/$(basename "$hashes")")
