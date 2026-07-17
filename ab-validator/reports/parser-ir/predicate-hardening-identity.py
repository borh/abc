#!/usr/bin/env python3
"""Generate reviewed semantic closures and bounded P4B policies."""

from __future__ import annotations

import argparse
import hashlib
import json
import pathlib
import re
from typing import Any


class Instrument:
    def __init__(
        self,
        *,
        reviewed_sources: tuple[pathlib.Path, ...],
        artifacts: tuple[pathlib.Path, ...],
        validator_id: str,
    ) -> None:
        self.reviewed_sources = reviewed_sources
        self.artifacts = artifacts
        self.validator_id = validator_id


INSTRUMENTS = {
    "diagnostic-completeness": Instrument(
        reviewed_sources=(
            pathlib.Path("abc/src/abc/tools/parser_rq_diagnostic_completeness.clj"),
            pathlib.Path("abc/src/abc/tools/parser_rq_capture.clj"),
            pathlib.Path("abc/src/abc/tools/schema.clj"),
            pathlib.Path("abc/src/abc/tools/files.clj"),
            pathlib.Path("abc/src/abc/tools/hash.clj"),
            pathlib.Path("abc/src/abc/tools/json.clj"),
            pathlib.Path("abc/src/abc/tools/jcs.clj"),
            pathlib.Path("abc/src/abc/tools/evidence_io.clj"),
            pathlib.Path("abc/src/abc/tools/path_containment.clj"),
            pathlib.Path("abc/src/abc/tools/malli.clj"),
            pathlib.Path("abc/src/abc/annotation/schema.clj"),
        ),
        artifacts=(pathlib.Path("abc/schemas/parser-rq-ab-aozora-diagnostics-v3.schema.json"),),
        validator_id="abc/parser-rq-diagnostic-completeness/v1",
    ),
    "parser-ir-conformance": Instrument(
        reviewed_sources=(
            pathlib.Path("ab-validator/crates/ab-aat-to-parser-ir/src/qualification.rs"),
            pathlib.Path("ab-validator/crates/ab-aat-to-parser-ir/src/convert.rs"),
            pathlib.Path("ab-validator/crates/ab-aat-to-parser-ir/src/schema.rs"),
            pathlib.Path("ab-validator/crates/ab-aat-to-parser-ir/src/canonical_json.rs"),
            pathlib.Path("ab-validator/crates/ab-aat-to-parser-ir/src/divergence.rs"),
            pathlib.Path("ab-validator/crates/ab-aat-to-parser-ir/src/mapping.rs"),
            pathlib.Path("ab-validator/crates/ab-aat-to-parser-ir/src/ortho_annotations.rs"),
            pathlib.Path("ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs"),
        ),
        artifacts=(
            pathlib.Path("abc/schemas/parser-ir.schema.json"),
            pathlib.Path("ab-validator/Cargo.lock"),
        ),
        validator_id="ab-validator/parser-ir-schema-conformance/v1",
    ),
}

_CLOJURE_REQUIRE = re.compile(r"\[(abc(?:\.[A-Za-z0-9_-]+)+)(?:\s|\])")


def _clojure_namespace_path(repo_root: pathlib.Path, namespace: str) -> pathlib.Path:
    relative = namespace.replace("-", "_").replace(".", "/") + ".clj"
    return repo_root / "abc/src" / relative


def _clojure_closure(repo_root: pathlib.Path, entrypoint: pathlib.Path) -> set[pathlib.Path]:
    pending = [entrypoint.resolve()]
    visited: set[pathlib.Path] = set()
    while pending:
        current = pending.pop()
        if current in visited:
            continue
        visited.add(current)
        text = current.read_text(encoding="utf-8")
        for namespace in _CLOJURE_REQUIRE.findall(text):
            dependency = _clojure_namespace_path(repo_root, namespace)
            if dependency.is_file() and dependency.resolve() not in visited:
                pending.append(dependency.resolve())
    return visited


def _parser_ir_closure(repo_root: pathlib.Path, config: Instrument) -> set[pathlib.Path]:
    source_root = repo_root / "ab-validator/crates/ab-aat-to-parser-ir/src"
    modules = {path.stem: path.resolve() for path in source_root.glob("*.rs")}
    qualification = modules["qualification"]
    text = qualification.read_text(encoding="utf-8")
    required_symbols = {
        "ConversionOptions",
        "PreparedConverter",
        "QualificationConversion",
        "to_canonical_json_pretty",
    }
    missing = sorted(symbol for symbol in required_symbols if symbol not in text)
    if missing:
        raise ValueError(f"qualification semantic imports have drifted: missing={missing}")
    pending = ["qualification", "convert", "canonical_json"]
    visited: set[str] = set()
    while pending:
        module = pending.pop()
        if module in visited:
            continue
        visited.add(module)
        module_text = modules[module].read_text(encoding="utf-8")
        dependencies = set(re.findall(r"crate::([a-z][a-z0-9_]*)", module_text))
        for block in re.findall(r"use\s+crate\s*::\s*\{(.*?)\};", module_text, flags=re.DOTALL):
            dependencies.update(re.findall(r"\b([a-z][a-z0-9_]*)\s*::", block))
        pending.extend((dependencies & modules.keys()) - visited)
    return {modules[module] for module in visited}


def discover_owned_sources(repo_root: pathlib.Path, instrument: str) -> set[pathlib.Path]:
    repo_root = repo_root.resolve()
    config = INSTRUMENTS[instrument]
    if instrument == "diagnostic-completeness":
        return _clojure_closure(repo_root, repo_root / config.reviewed_sources[0])
    return _parser_ir_closure(repo_root, config)


def _sha256(data: bytes) -> str:
    return "sha256:" + hashlib.sha256(data).hexdigest()


def _canonical(value: Any) -> bytes:
    # Match ABC's frozen historical canonicalization, including Charred's
    # escaped solidus behavior. Existing ABC identities deliberately retain
    # this representation rather than silently switching to RFC 8785.
    return (
        json.dumps(value, ensure_ascii=False, sort_keys=True, separators=(",", ":"))
        .replace("/", "\\/")
        .encode("utf-8")
    )


def projected_hash(value: dict[str, Any], field: str) -> str:
    projected = dict(value)
    projected.pop(field, None)
    return _sha256(_canonical(projected))


def _json_logical_hash(path: pathlib.Path) -> str:
    return _sha256(_canonical(json.loads(path.read_text(encoding="utf-8"))))


def build_manifest(repo_root: pathlib.Path, instrument: str) -> dict[str, Any]:
    repo_root = repo_root.resolve()
    config = INSTRUMENTS[instrument]
    actual = discover_owned_sources(repo_root, instrument)
    reviewed = {(repo_root / path).resolve() for path in config.reviewed_sources}
    if actual != reviewed:
        unreviewed = sorted(str(path.relative_to(repo_root)) for path in actual - reviewed)
        stale = sorted(str(path.relative_to(repo_root)) for path in reviewed - actual)
        raise ValueError(
            f"reviewed semantic closure differs: unreviewed={unreviewed}, stale={stale}"
        )
    manifest: dict[str, Any] = {
        "schema_version": "parser-rq-predicate-validator-identity-v1",
        "instrument": instrument,
        "validator_id": config.validator_id,
        "runtime": (
            {
                "language": "clojure",
                "json_schema_dialect": "2020-12",
                "canonical_identity": "abc.tools.jcs/canonical-json-bytes",
            }
            if instrument == "diagnostic-completeness"
            else {
                "language": "rust",
                "edition": "2024",
                "jsonschema_crate": "0.46.9",
            }
        ),
        "sources": [
            {"path": path.as_posix(), "sha256": _sha256((repo_root / path).read_bytes())}
            for path in config.reviewed_sources
        ],
        "artifacts": [
            {"path": path.as_posix(), "sha256": _sha256((repo_root / path).read_bytes())}
            for path in config.artifacts
        ],
    }
    manifest["validator_semantics_hash"] = projected_hash(manifest, "validator_semantics_hash")
    return manifest


def build_policy(
    repo_root: pathlib.Path,
    instrument: str,
    manifest: dict[str, Any],
    expected_work_ids: list[str],
) -> dict[str, Any]:
    if not expected_work_ids or len(expected_work_ids) != len(set(expected_work_ids)):
        raise ValueError("policy work ids must be a nonempty unique list")
    repo_root = repo_root.resolve()
    common = {
        "schema_version": "1.0.0",
        "expected_work_ids": expected_work_ids,
        "expected_work_set_hash": _sha256(_canonical(expected_work_ids)),
        "validator_semantics_hash": manifest["validator_semantics_hash"],
    }
    if instrument == "diagnostic-completeness":
        policy = {
            **common,
            "schema_id": "https://w3id.org/abc/schemas/parser-rq-diagnostic-completeness-policy.schema.json",
            "policy_id": "abc/parser-rq-diagnostic-completeness/v1",
            "algorithm_version": "diagnostic-envelope-completeness-v1",
            "raw_diagnostic_schema_id": "https://w3id.org/abc/schemas/parser-rq-ab-aozora-diagnostics-v3.schema.json",
            "raw_diagnostic_schema_hash": _json_logical_hash(
                repo_root / "abc/schemas/parser-rq-ab-aozora-diagnostics-v3.schema.json"
            ),
            "diagnostic_wire_version": 3,
            "vacuity_semantics": "valid_empty_passes_with_disclosure",
            "status_mapping": {
                "allowed_statuses": ["measured", "invalid_diagnostic_envelope"],
                "values": {
                    "measured": 1.0,
                    "invalid_diagnostic_envelope": "invalid_diagnostic_envelope",
                },
            },
        }
    else:
        policy = {
            **common,
            "schema_id": "https://w3id.org/abc/schemas/parser-rq-parser-ir-conformance-policy.schema.json",
            "policy_id": "abc/parser-rq-parser-ir-conformance/v1",
            "algorithm_version": "parser-ir-schema-conformance-v1",
            "parser_ir_schema_id": "https://w3id.org/abc/schemas/parser-ir.schema.json",
            "parser_ir_schema_hash": _json_logical_hash(
                repo_root / "abc/schemas/parser-ir.schema.json"
            ),
            "generated_output_denominator": "schema_valid_plus_schema_invalid",
            "no_output_semantics": "available_failure_when_generated_outputs_zero",
            "status_mapping": {
                "allowed_statuses": ["measured", "no_parser_ir_output"],
                "values": {"measured": 1.0, "no_parser_ir_output": "no_parser_ir_output"},
            },
        }
    policy["policy_hash"] = projected_hash(policy, "policy_hash")
    return policy


def _write_json(path: pathlib.Path, value: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        json.dumps(value, ensure_ascii=False, sort_keys=True, indent=2) + "\n",
        encoding="utf-8",
    )


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo-root", required=True, type=pathlib.Path)
    parser.add_argument("--instrument", required=True, choices=sorted(INSTRUMENTS))
    parser.add_argument("--out", required=True, type=pathlib.Path)
    parser.add_argument("--policy-out", type=pathlib.Path)
    parser.add_argument("--work-id", action="append", default=[])
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    manifest = build_manifest(args.repo_root, args.instrument)
    _write_json(args.out, manifest)
    if args.policy_out:
        _write_json(
            args.policy_out,
            build_policy(args.repo_root, args.instrument, manifest, args.work_id),
        )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
