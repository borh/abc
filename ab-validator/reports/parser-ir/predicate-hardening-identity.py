#!/usr/bin/env python3
"""Generate reviewed semantic closures and bounded P4B policies.

The manifests and policies committed under `ab-validator/data/` and
`ab-validator/research/data/` are historical records of the instrument that
produced a retained capture. They are not claims about the current sources, and
a difference between one of them and a freshly generated manifest is expected
rather than drift.

`parser-rq-parser-ir-conformance-validator-v1.json` is the clearest case. It
lists `sentences.rs`, deleted in 7618e7e6, and names its artifact at
`abc/schemas/parser-ir.schema.json`, a path that no longer exists; its artifact
hash matches `parser-ir-0.7.0.schema.json`, retained beside the capture in
`ab-validator/research/test/fixtures/parser-rq/predicate-hardening-capture/`,
not the live schema. That retained schema is what keeps the historical policy
reproducible, and
`parser_rq_predicate_hardening_capture_test.clj` pins both its hash and the
policy against it.

Regenerating these files therefore invalidates the retained evidence. Generate a
new manifest under a new validator id when a new instrument is reviewed; leave
the recorded ones alone.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import pathlib
import re
import sys
import tomllib
from typing import Any

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parents[1] / "lib"))
from legacy_json_c14n import canonical_json  # noqa: E402


class Instrument:
    def __init__(
        self,
        *,
        reviewed_sources: tuple[pathlib.Path, ...],
        artifacts: tuple[pathlib.Path, ...],
        validator_id: str,
        locked_package: str | None = None,
    ) -> None:
        self.reviewed_sources = reviewed_sources
        self.artifacts = artifacts
        self.validator_id = validator_id
        self.locked_package = locked_package


INSTRUMENTS = {
    "diagnostic-completeness": Instrument(
        reviewed_sources=(
            pathlib.Path(
                "ab-validator/research/src/ab_research/parser_rq_diagnostic_completeness.clj"
            ),
            pathlib.Path("ab-validator/research/src/ab_research/parser_rq_capture.clj"),
            pathlib.Path("ab-validator/research/src/ab_research/schema.clj"),
            pathlib.Path("ab-validator/research/src/ab_research/files.clj"),
            pathlib.Path("ab-validator/research/src/ab_research/hash.clj"),
            pathlib.Path("soranoha/src/soranoha/core/jcs.clj"),
            pathlib.Path("soranoha/src/soranoha/core/canonical.clj"),
            pathlib.Path("soranoha/src/soranoha/core/json.clj"),
        ),
        artifacts=(
            pathlib.Path(
                "ab-validator/research/schemas/parser-rq-ab-aozora-diagnostics-v3.schema.json"
            ),
        ),
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
            # `convert` imports `unicode_or_placeholder` for unresolved gaiji, and
            # two of its four call sites derive a node's end offset from the
            # placeholder's UTF-8 length. The constant therefore decides emitted
            # parser-IR spans, not only its visible text.
            pathlib.Path("ab-validator/crates/ab-aat-to-parser-ir/src/content.rs"),
        ),
        artifacts=(pathlib.Path("ab-validator/research/schemas/parser-ir.schema.json"),),
        validator_id="ab-validator/parser-ir-schema-conformance/v1",
        locked_package="ab-aat-to-parser-ir",
    ),
}

_CLOJURE_REQUIRE = re.compile(r"\[((?:ab-research|soranoha)(?:\.[A-Za-z0-9_-]+)+)(?:\s|\])")


def _clojure_namespace_path(repo_root: pathlib.Path, namespace: str) -> pathlib.Path:
    relative = namespace.replace("-", "_").replace(".", "/") + ".clj"
    return (
        repo_root
        / ("soranoha/src" if namespace.startswith("soranoha.") else "ab-validator/research/src")
        / relative
    )


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


CARGO_LOCK = pathlib.Path("ab-validator/Cargo.lock")


def locked_dependency_projection(repo_root: pathlib.Path, package: str) -> list[dict[str, str]]:
    """Project one package's transitive Cargo.lock closure into a sorted value.

    The lockfile is workspace-wide, so hashing all bytes rotated this instrument's
    identity whenever an unrelated workspace package changed. This projection
    tracks the resolved dependency graph the instrument compiles against
    (including `jsonschema`, which decides schema validity) while ignoring
    unrelated workspace packages.

    Known limitation: workspace-local members carry no checksum, so their
    bytes are outside this projection and outside `reviewed_sources`. Cargo.lock
    likewise records no feature selection. Both gaps require separate source
    and feature identity inputs.
    """
    lock = tomllib.loads((repo_root / CARGO_LOCK).read_text(encoding="utf-8"))
    # Keyed by (name, version), not name. Thirty-six names in this workspace
    # resolve to more than one version at once -- `sha2`, `rand`, `hashbrown`,
    # `windows-sys` among them. Collapsing them by name would let one version's
    # entry stand in for another's, so a bump of the one actually compiled
    # could leave this projection unmoved.
    packages = {(entry["name"], entry["version"]): entry for entry in lock["package"]}
    by_name: dict[str, list[tuple[str, str]]] = {}
    for key in packages:
        by_name.setdefault(key[0], []).append(key)

    def resolve(dependency: str) -> tuple[str, str]:
        # Lock dependency entries are "name", "name version", or
        # "name version (source)". Cargo omits the version only when the name
        # is unambiguous, so a bare name with several candidates is a lockfile
        # this reader does not understand -- fail rather than guess. A named
        # version that no entry carries fails the same way: skipping it would
        # drop a package out of the identity with nothing said.
        parts = dependency.split()
        candidates = [(parts[0], parts[1])] if len(parts) > 1 else list(by_name.get(parts[0], []))
        if len(candidates) != 1 or candidates[0] not in packages:
            raise ValueError(
                f"dependency {dependency!r} does not select one locked package: {candidates}"
            )
        return candidates[0]

    pending = [resolve(package)]
    visited: set[tuple[str, str]] = set()
    while pending:
        current = pending.pop()
        if current in visited:
            continue
        visited.add(current)
        pending.extend(
            resolve(dependency) for dependency in packages[current].get("dependencies", [])
        )
    projection = []
    for key in sorted(visited):
        entry = packages[key]
        projected = {"name": key[0], "version": key[1]}
        if "checksum" in entry:
            projected["checksum"] = entry["checksum"]
        else:
            projected["origin"] = "workspace-local"
        projection.append(projected)
    return projection


def locked_version(projection: list[dict[str, str]], name: str) -> str:
    versions = sorted({entry["version"] for entry in projection if entry["name"] == name})
    if len(versions) != 1:
        raise ValueError(
            f"{name} is absent from the locked dependency projection or ambiguous: {versions}"
        )
    return versions[0]


def _sha256(data: bytes) -> str:
    return "sha256:" + hashlib.sha256(data).hexdigest()


def _canonical(value: Any) -> bytes:
    # abc-legacy-json-c14n-v0: ABC's frozen historical canonicalization,
    # including Charred's escaped solidus behavior. Existing ABC identities
    # retain this representation rather than migrating to RFC 8785.
    return canonical_json(value).encode("utf-8")


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
    projection = (
        locked_dependency_projection(repo_root, config.locked_package)
        if config.locked_package
        else None
    )
    manifest: dict[str, Any] = {
        "schema_version": "parser-rq-predicate-validator-identity-v2",
        "instrument": instrument,
        "validator_id": config.validator_id,
        "runtime": (
            {
                "language": "clojure",
                "json_schema_dialect": "2020-12",
                "canonical_identity": "ab-research.jcs/canonical-json-bytes",
            }
            if instrument == "diagnostic-completeness"
            else {
                "language": "rust",
                "edition": "2024",
                # Read out of the projection, not restated. A literal here
                # claimed 0.46.9 with nothing enforcing it, so a crate bump
                # could have left the runtime block asserting a version the
                # instrument no longer compiled against.
                "jsonschema_crate": locked_version(projection, "jsonschema"),
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
    if projection is not None:
        manifest["locked_dependencies"] = {
            "package": config.locked_package,
            "lockfile": CARGO_LOCK.as_posix(),
            "closure": projection,
        }
    manifest["validator_semantics_hash"] = projected_hash(manifest, "validator_semantics_hash")
    return manifest


def build_policy(
    repo_root: pathlib.Path,
    instrument: str,
    manifest: dict[str, Any],
    expected_work_ids: list[str],
    expected_diagnostics: dict[str, list[str]] | None = None,
) -> dict[str, Any]:
    if not expected_work_ids or len(expected_work_ids) != len(set(expected_work_ids)):
        raise ValueError("policy work ids must be a nonempty unique list")
    repo_root = repo_root.resolve()
    common = {
        "expected_work_ids": expected_work_ids,
        "expected_work_set_hash": _sha256(_canonical(expected_work_ids)),
        "validator_semantics_hash": manifest["validator_semantics_hash"],
    }
    if instrument == "diagnostic-completeness":
        # Restated from the qualification corpus, which is the authority. The
        # campaign orchestrator authenticates this restatement against the
        # governed corpus before any capture runs, so a policy that disagrees
        # with what the corpus expects cannot produce evidence at all.
        if expected_diagnostics is None or set(expected_diagnostics) != set(expected_work_ids):
            raise ValueError(
                "diagnostic-completeness requires one governed expectation per expected work"
            )
        policy = {
            **common,
            "schema_version": "2.0.0",
            "schema_id": "https://w3id.org/soranoha/schemas/parser-rq-diagnostic-completeness-policy.schema.json",
            "policy_id": "abc/parser-rq-diagnostic-completeness/v1",
            "algorithm_version": "diagnostic-expectation-conformance-v2",
            "expected_diagnostics": {
                work: sorted(codes) for work, codes in expected_diagnostics.items()
            },
            "raw_diagnostic_schema_id": "https://w3id.org/soranoha/schemas/parser-rq-ab-aozora-diagnostics-v3.schema.json",
            "raw_diagnostic_schema_hash": _json_logical_hash(
                repo_root
                / "ab-validator/research/schemas/parser-rq-ab-aozora-diagnostics-v3.schema.json"
            ),
            "diagnostic_wire_version": 3,
            "vacuity_semantics": "empty_expectation_match_is_a_positive_observation",
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
            "schema_version": "1.0.0",
            "schema_id": "https://w3id.org/soranoha/schemas/parser-rq-parser-ir-conformance-policy.schema.json",
            "policy_id": "abc/parser-rq-parser-ir-conformance/v1",
            "algorithm_version": "parser-ir-schema-conformance-v1",
            "parser_ir_schema_id": "https://w3id.org/soranoha/schemas/parser-ir.schema.json",
            "parser_ir_schema_hash": _json_logical_hash(
                repo_root / "ab-validator/research/schemas/parser-ir.schema.json"
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
    parser.add_argument(
        "--work-diagnostics",
        action="append",
        default=[],
        metavar="WORK_ID=CODE[,CODE...]",
        help=(
            "governed diagnostic expectation for one work, restated from the "
            "qualification corpus; a trailing empty list means the work must "
            "emit no diagnostics"
        ),
    )
    return parser.parse_args()


def parse_work_diagnostics(pairs: list[str]) -> dict[str, list[str]]:
    """Parse repeated WORK_ID=CODE,CODE arguments into one expectation map.

    A work named twice is rejected rather than last-wins: two disagreeing
    expectations for one work is a caller mistake, and silently keeping one of
    them would put an unreviewed expectation into the policy hash.
    """
    expectation: dict[str, list[str]] = {}
    for pair in pairs:
        work, separator, codes = pair.partition("=")
        if not separator or not work:
            raise ValueError(f"--work-diagnostics {pair!r} is not WORK_ID=CODE,CODE")
        if work in expectation:
            raise ValueError(f"--work-diagnostics names {work!r} more than once")
        expectation[work] = [code for code in codes.split(",") if code]
    return expectation


def main() -> int:
    args = parse_args()
    manifest = build_manifest(args.repo_root, args.instrument)
    _write_json(args.out, manifest)
    if args.policy_out:
        _write_json(
            args.policy_out,
            build_policy(
                args.repo_root,
                args.instrument,
                manifest,
                args.work_id,
                parse_work_diagnostics(args.work_diagnostics) or None,
            ),
        )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
