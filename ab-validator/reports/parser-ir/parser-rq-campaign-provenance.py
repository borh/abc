#!/usr/bin/env python3
"""Measure executable reproducibility and authenticate stored evidence.

This module emits evidence values. Release authority remains in the Clojure
campaign verifier, which authenticates the closed records before promotion.
"""

from __future__ import annotations

import argparse
import base64
import hashlib
import json
import os
import subprocess
import sys
from pathlib import Path, PurePosixPath
from typing import NamedTuple, Protocol


CHUNK_BYTES = 1024 * 1024
EXECUTABLE_FIELDS = (
    "name",
    "nix_output",
    "nar_hash",
    "sha256",
    "bytes",
    "adapter",
    "adapter_version",
    "parser_git_rev",
    "argv_template",
)


class LogicalBlob(NamedTuple):
    sha256: str
    bytes: int
    media_type: str
    locator: str


class CommandResult(NamedTuple):
    returncode: int
    stdout: bytes
    stderr: bytes


class RealizeRequest(NamedTuple):
    build_id: str
    store_root: Path
    store_uri: str
    drv_path: str
    output_path: str
    seed_inputs: tuple[str, ...]
    build_log: Path


class Runner(Protocol):
    def run(self, argv: list[str], *, env: dict[str, str] | None = None) -> CommandResult: ...


class SubprocessRunner:
    def run(self, argv: list[str], *, env: dict[str, str] | None = None) -> CommandResult:
        completed = subprocess.run(
            argv,
            check=False,
            capture_output=True,
            env=None if env is None else {**os.environ, **env},
        )
        return CommandResult(completed.returncode, completed.stdout, completed.stderr)


class ProvenanceUnavailable(ValueError):
    pass


def sha256_bytes(payload: bytes) -> str:
    return "sha256:" + hashlib.sha256(payload).hexdigest()


def _canonical_bytes(value: object) -> bytes:
    encoded = json.dumps(value, ensure_ascii=True, sort_keys=True, separators=(",", ":"))
    return encoded.replace("/", "\\/").encode()


def _atomic_json(path: Path, value: object) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(f".{path.name}.{os.getpid()}.tmp")
    try:
        with temporary.open("xb") as stream:
            stream.write(_canonical_bytes(value))
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temporary, path)
    finally:
        temporary.unlink(missing_ok=True)


def _content_ref(value: object) -> str:
    return sha256_bytes(_canonical_bytes(value))


def provenance_core_ref(value: dict[str, object]) -> str:
    core = {
        key: item
        for key, item in value.items()
        if key
        not in {
            "schema_id",
            "schema_version",
            "candidate_ref",
            "qualification_identity_ref",
            "provenance_core_ref",
        }
    }
    return _content_ref(core)


def direct_seed_paths(derivation: dict[str, object]) -> tuple[str, ...]:
    paths: set[str] = set()
    sources = derivation.get("inputSrcs", [])
    if not isinstance(sources, list) or not all(isinstance(path, str) for path in sources):
        raise ProvenanceUnavailable("derivation inputSrcs are malformed")
    paths.update(sources)
    input_drvs = derivation.get("inputDrvs", {})
    if not isinstance(input_drvs, dict):
        raise ProvenanceUnavailable("derivation inputDrvs are malformed")
    for outputs in input_drvs.values():
        if not isinstance(outputs, dict):
            raise ProvenanceUnavailable("direct derivation outputs are unresolved")
        for closure in outputs.values():
            if not isinstance(closure, list) or not all(isinstance(path, str) for path in closure):
                raise ProvenanceUnavailable("direct output closure is malformed")
            paths.update(closure)
    if not paths:
        raise ProvenanceUnavailable("target has no seedable direct inputs")
    return tuple(sorted(paths))


def _store_path(value: object, *, description: str) -> str:
    if not isinstance(value, str) or not value:
        raise ProvenanceUnavailable(f"{description} is absent")
    if value.startswith("/nix/store/"):
        return value
    if "/" in value:
        raise ProvenanceUnavailable(f"{description} is not a Nix store path")
    return f"/nix/store/{value}"


def _derivation_record(payload: bytes, drv_path: str) -> dict[str, object]:
    try:
        document = json.loads(payload)
        derivations = document["derivations"]
        record = derivations[Path(drv_path).name]
    except (json.JSONDecodeError, KeyError, TypeError) as error:
        raise ProvenanceUnavailable("Nix derivation output is malformed") from error
    if not isinstance(record, dict):
        raise ProvenanceUnavailable("Nix derivation record is not an object")
    return record


def _run_stdout(runner: Runner, argv: list[str], failure: str) -> bytes:
    result = runner.run(argv)
    if result.returncode != 0 or not result.stdout:
        raise ProvenanceUnavailable(failure)
    return result.stdout


def resolve_realize_request(
    candidate_tree: Path,
    build_id: str,
    store_root: Path,
    build_log: Path,
    runner: Runner,
    package_ref: str | None = None,
) -> RealizeRequest:
    package = package_ref or (
        f"{candidate_tree / 'ab-validator'}#packages.x86_64-linux.parser-rq-candidate"
    )
    drv_path = (
        _run_stdout(
            runner,
            ["nix", "eval", "--raw", f"{package}.drvPath"],
            "candidate derivation cannot be evaluated",
        )
        .decode()
        .strip()
    )
    output_path = (
        _run_stdout(
            runner,
            ["nix", "eval", "--raw", f"{package}.outPath"],
            "candidate output cannot be evaluated",
        )
        .decode()
        .strip()
    )
    target = _derivation_record(
        _run_stdout(
            runner,
            ["nix", "derivation", "show", drv_path],
            "candidate derivation cannot be inspected",
        ),
        drv_path,
    )
    inputs = target.get("inputs")
    if not isinstance(inputs, dict):
        raise ProvenanceUnavailable("candidate derivation inputs are malformed")
    sources = inputs.get("srcs", [])
    input_drvs = inputs.get("drvs", {})
    if not isinstance(sources, list) or not isinstance(input_drvs, dict):
        raise ProvenanceUnavailable("candidate derivation inputs are malformed")
    direct_sources = [_store_path(source, description="direct source input") for source in sources]
    for source in direct_sources:
        _run_stdout(
            runner,
            ["nix", "path-info", "--json", source],
            "direct source input is unavailable in the daemon store",
        )
    normalized: dict[str, object] = {
        "inputSrcs": direct_sources,
        "inputDrvs": {},
    }
    closures: dict[str, dict[str, list[str]]] = {}
    for raw_drv, request in sorted(input_drvs.items()):
        input_drv = _store_path(raw_drv, description="direct derivation input")
        if not isinstance(request, dict) or not isinstance(request.get("outputs"), list):
            raise ProvenanceUnavailable("direct derivation output request is malformed")
        input_record = _derivation_record(
            _run_stdout(
                runner,
                ["nix", "derivation", "show", input_drv],
                "direct derivation input cannot be inspected",
            ),
            input_drv,
        )
        outputs = input_record.get("outputs")
        if not isinstance(outputs, dict):
            raise ProvenanceUnavailable("direct derivation outputs are malformed")
        output_closures: dict[str, list[str]] = {}
        for output_name in sorted(request["outputs"]):
            output = outputs.get(output_name)
            if not isinstance(output, dict):
                raise ProvenanceUnavailable("requested direct output is absent")
            direct_output_path = output.get("path")
            if direct_output_path is None:
                direct_output_path = (
                    _run_stdout(
                        runner,
                        ["nix-store", "-q", "--binding", output_name, input_drv],
                        "requested direct output path cannot be resolved",
                    )
                    .decode()
                    .strip()
                )
            direct_output = _store_path(
                direct_output_path, description="requested direct output path"
            )
            if runner.run(["nix", "path-info", direct_output]).returncode != 0:
                realized = runner.run(["nix", "build", "--no-link", f"{input_drv}^{output_name}"])
                if realized.returncode != 0:
                    raise ProvenanceUnavailable("direct build input cannot be realized")
                if runner.run(["nix", "path-info", direct_output]).returncode != 0:
                    raise ProvenanceUnavailable("realized direct build input is absent")
            closure_bytes = _run_stdout(
                runner,
                [
                    "nix",
                    "path-info",
                    "--recursive",
                    "--json",
                    "--json-format",
                    "1",
                    direct_output,
                ],
                "direct output closure cannot be inspected",
            )
            try:
                closure_rows = json.loads(closure_bytes)
                if not isinstance(closure_rows, dict):
                    raise TypeError
                closure = [_store_path(path, description="closure path") for path in closure_rows]
            except (json.JSONDecodeError, KeyError, TypeError) as error:
                raise ProvenanceUnavailable("direct output closure is malformed") from error
            output_closures[output_name] = closure
        closures[input_drv] = output_closures
    normalized["inputDrvs"] = closures
    return RealizeRequest(
        build_id,
        store_root,
        f"local?root={store_root}",
        _store_path(drv_path, description="candidate derivation"),
        _store_path(output_path, description="candidate output"),
        direct_seed_paths(normalized),
        build_log,
    )


def _path_present(runner: Runner, store_uri: str, path: str) -> bool:
    result = runner.run(["nix", "path-info", "--store", store_uri, path])
    return result.returncode == 0


def realize_target(request: RealizeRequest, runner: Runner) -> dict[str, object]:
    if request.build_id not in {"build-a", "build-b"}:
        raise ProvenanceUnavailable("build ID is not closed")
    if request.store_root.exists():
        raise ProvenanceUnavailable("fresh store root already exists")
    if not request.seed_inputs:
        raise ProvenanceUnavailable("dependency seed set is empty")
    if _path_present(runner, request.store_uri, request.output_path):
        raise ProvenanceUnavailable("target was present before dependency seeding")
    copied = runner.run(
        [
            "nix",
            "copy",
            "--no-check-sigs",
            "--to",
            request.store_uri,
            request.drv_path,
            *request.seed_inputs,
        ]
    )
    if copied.returncode != 0:
        raise ProvenanceUnavailable("dependency seeding failed")
    if _path_present(runner, request.store_uri, request.output_path):
        raise ProvenanceUnavailable("target was present after dependency seeding")
    built = runner.run(
        [
            "nix",
            "build",
            "--store",
            request.store_uri,
            "--offline",
            "--no-link",
            "--json",
            f"{request.drv_path}^out",
        ],
        env={"LC_ALL": "C"},
    )
    request.build_log.parent.mkdir(parents=True, exist_ok=True)
    request.build_log.write_bytes(built.stderr)
    expected_log = f"building '{request.drv_path}'".encode()
    if built.returncode != 0 or expected_log not in built.stderr:
        raise ProvenanceUnavailable("offline target realization was not evidenced")
    if not _path_present(runner, request.store_uri, request.output_path):
        raise ProvenanceUnavailable("target is absent after offline realization")
    return {
        "build_id": request.build_id,
        "store_uri": request.store_uri,
        "initially_empty": True,
        "target_absent_before_seed": True,
        "target_absent_after_seed": True,
        "drv_path": request.drv_path,
        "output_path": request.output_path,
        "seeded_inputs": list(request.seed_inputs),
        "build_log_path": str(request.build_log),
    }


def _stream_identity(path: Path) -> tuple[str, int]:
    digest = hashlib.sha256()
    size = 0
    with path.open("rb") as stream:
        while chunk := stream.read(CHUNK_BYTES):
            digest.update(chunk)
            size += len(chunk)
    return "sha256:" + digest.hexdigest(), size


def executable_record(
    path: Path, nix_output: dict[str, object], argv: list[str]
) -> dict[str, object]:
    digest, size = _stream_identity(path)
    return {
        "name": nix_output["name"],
        "nix_output": nix_output["nix_output"],
        "nar_hash": nix_output["nar_hash"],
        "sha256": digest,
        "bytes": size,
        "adapter": nix_output["adapter"],
        "adapter_version": nix_output["adapter_version"],
        "parser_git_rev": nix_output["parser_git_rev"],
        "argv_template": list(argv),
    }


def compare_builds(first: object, second: object) -> dict[str, object]:
    if not isinstance(first, dict) or not isinstance(second, dict):
        return {"status": "unavailable", "reason": "build records are not closed objects"}
    if first.get("output_ref") != second.get("output_ref"):
        return {"status": "unavailable", "reason": "Nix output identity differs"}
    first_executables = first.get("executables")
    second_executables = second.get("executables")
    if not isinstance(first_executables, list) or not isinstance(second_executables, list):
        return {"status": "unavailable", "reason": "executable records are absent"}
    if not all(isinstance(record, dict) for record in first_executables) or not all(
        isinstance(record, dict) for record in second_executables
    ):
        return {"status": "unavailable", "reason": "executable records are malformed"}
    keyed_first = {record.get("name"): record for record in first_executables}
    keyed_second = {record.get("name"): record for record in second_executables}
    if (
        set(keyed_first) != set(keyed_second)
        or len(keyed_first) != len(first_executables)
        or len(keyed_second) != len(second_executables)
        or None in keyed_first
    ):
        return {"status": "unavailable", "reason": "executable membership differs"}
    for name in sorted(keyed_first):
        if any(
            keyed_first[name].get(field) != keyed_second[name].get(field)
            for field in EXECUTABLE_FIELDS
        ):
            return {
                "status": "unavailable",
                "reason": f"executable coordinates differ for {name}",
            }
    proof: dict[str, object] = {
        "status": "reproducible",
        "output_ref": first["output_ref"],
        "executables": [keyed_first[name] for name in sorted(keyed_first)],
    }
    if "build_id" in first or "build_id" in second:
        rows = []
        for record in (first, second):
            row = {
                key: record.get(key)
                for key in ("build_id", "store_uri", "output_ref", "build_record_ref")
            }
            if any(value is None for value in row.values()):
                return {"status": "unavailable", "reason": "build identity is incomplete"}
            rows.append(row)
        if {row["build_id"] for row in rows} != {"build-a", "build-b"}:
            return {"status": "unavailable", "reason": "build IDs are not closed"}
        if len({row["store_uri"] for row in rows}) != 2:
            return {"status": "unavailable", "reason": "build stores are not independent"}
        proof.pop("output_ref")
        proof["builds"] = sorted(rows, key=lambda row: str(row["build_id"]))
        proof["provenance_core_ref"] = provenance_core_ref(proof)
    return proof


def bind_provenance(
    proof: dict[str, object], candidate_ref: str, qualification_identity_ref: str
) -> dict[str, object]:
    if proof.get("status") != "reproducible":
        raise ProvenanceUnavailable("only reproducible proof may be bound")
    expected = provenance_core_ref(proof)
    if proof.get("provenance_core_ref") != expected:
        raise ProvenanceUnavailable("provenance core hash does not authenticate proof")
    bound = {
        **proof,
        "schema_id": "https://w3id.org/abc/schemas/parser-rq-executable-provenance.schema.json",
        "schema_version": "2.0.0",
        "candidate_ref": candidate_ref,
        "qualification_identity_ref": qualification_identity_ref,
    }
    if provenance_core_ref(bound) != expected:
        raise ProvenanceUnavailable("candidate binding changed provenance core")
    return bound


def _safe_path(root: Path, locator: str) -> Path | None:
    logical = PurePosixPath(locator)
    if logical.is_absolute() or ".." in logical.parts or not locator:
        return None
    candidate = root.joinpath(*logical.parts)
    try:
        resolved_root = root.resolve(strict=True)
        resolved_candidate = candidate.resolve(strict=True)
    except OSError:
        return None
    if resolved_root not in resolved_candidate.parents:
        return None
    return resolved_candidate


def receipt_ref(receipt: dict[str, object]) -> str:
    return _content_ref({key: value for key, value in receipt.items() if key != "receipt_ref"})


def verify_evidence(blobs: list[LogicalBlob], root: Path) -> dict[str, object]:
    try:
        resolved_root = root.resolve(strict=True)
    except OSError:
        return {"status": "unavailable", "reason": "evidence store is offline"}
    if not blobs or len(set(blobs)) != len(blobs):
        return {"status": "unavailable", "reason": "blob membership is empty or duplicated"}
    records: list[dict[str, object]] = []
    for blob in sorted(blobs, key=lambda value: (value.sha256, value.locator)):
        if not blob.media_type or blob.bytes < 0:
            return {"status": "unavailable", "reason": "blob metadata is invalid"}
        path = _safe_path(resolved_root, blob.locator)
        if path is None:
            return {"status": "unavailable", "reason": "blob locator is absent or unsafe"}
        observed_hash, observed_bytes = _stream_identity(path)
        if observed_hash != blob.sha256 or observed_bytes != blob.bytes:
            return {"status": "unavailable", "reason": "evidence bytes do not authenticate"}
        records.append(
            {
                "blob": blob._asdict(),
                "rehash": observed_hash,
                "observed_bytes": observed_bytes,
            }
        )
    return {"status": "verified", "blobs": records}


def _nix_hash(value: str) -> str:
    if value.startswith("sha256:") and len(value) == 71:
        return value
    if value.startswith("sha256-"):
        try:
            raw = base64.b64decode(value.removeprefix("sha256-"), validate=True)
        except (ValueError, base64.binascii.Error) as error:
            raise ProvenanceUnavailable("NAR hash is malformed") from error
        if len(raw) == 32:
            return "sha256:" + raw.hex()
    raise ProvenanceUnavailable("NAR hash is not SHA-256")


def _output_nar_hash(runner: Runner, store_uri: str, output_path: str) -> str:
    path_info = runner.run(
        [
            "nix",
            "path-info",
            "--json",
            "--json-format",
            "1",
            "--store",
            store_uri,
            output_path,
        ]
    )
    if path_info.returncode != 0:
        raise ProvenanceUnavailable("realized output cannot be inspected")
    try:
        rows = json.loads(path_info.stdout)
        if not isinstance(rows, dict) or set(rows) != {output_path}:
            raise TypeError
        return _nix_hash(rows[output_path]["narHash"])
    except (json.JSONDecodeError, KeyError, TypeError) as error:
        raise ProvenanceUnavailable("Nix path-info output is malformed") from error


def capture_build(
    realization: dict[str, object],
    graph: dict[str, object],
    parser_git_rev: str,
    runner: Runner,
) -> dict[str, object]:
    store_uri = realization.get("store_uri")
    output_path = realization.get("output_path")
    if not isinstance(store_uri, str) or not isinstance(output_path, str):
        raise ProvenanceUnavailable("realization store or output is absent")
    nar_hash = _output_nar_hash(runner, store_uri, output_path)
    graph_executables = graph.get("executables")
    if not isinstance(graph_executables, list) or not graph_executables:
        raise ProvenanceUnavailable("production graph executable set is empty")
    executables = []
    seen = set()
    for policy in graph_executables:
        if not isinstance(policy, dict) or not isinstance(policy.get("name"), str):
            raise ProvenanceUnavailable("graph executable entry is malformed")
        name = policy["name"]
        if name in seen:
            raise ProvenanceUnavailable("graph executable membership is duplicated")
        seen.add(name)
        streamed = runner.run(
            [
                "nix",
                "store",
                "cat",
                "--store",
                store_uri,
                f"{output_path}/bin/{name}",
            ]
        )
        if streamed.returncode != 0 or not streamed.stdout:
            raise ProvenanceUnavailable(f"candidate executable is absent: {name}")
        argv = policy.get("argv_template")
        adapter = policy.get("adapter")
        version = policy.get("adapter_version")
        if (
            not isinstance(argv, list)
            or not argv
            or not all(isinstance(token, str) for token in argv)
            or not isinstance(adapter, str)
            or not isinstance(version, str)
        ):
            raise ProvenanceUnavailable(f"graph coordinates are incomplete for {name}")
        executables.append(
            {
                "name": name,
                "nix_output": output_path,
                "nar_hash": nar_hash,
                "sha256": sha256_bytes(streamed.stdout),
                "bytes": len(streamed.stdout),
                "adapter": adapter,
                "adapter_version": version,
                "parser_git_rev": parser_git_rev,
                "argv_template": argv,
            }
        )
    build_log_path = realization.get("build_log_path")
    if not isinstance(build_log_path, str):
        raise ProvenanceUnavailable("build log path is absent")
    log_path = Path(build_log_path)
    log_hash, log_bytes = _stream_identity(log_path)
    record: dict[str, object] = {
        "schema_id": "https://w3id.org/abc/schemas/parser-rq-build-record.schema.json",
        "schema_version": "1.0.0",
        **{
            key: realization[key]
            for key in (
                "build_id",
                "store_uri",
                "initially_empty",
                "target_absent_before_seed",
                "target_absent_after_seed",
                "drv_path",
                "output_path",
                "seeded_inputs",
            )
        },
        "output_ref": nar_hash,
        "nar_hash": nar_hash,
        "build_log": {
            "sha256": log_hash,
            "bytes": log_bytes,
            "media_type": "text/plain",
            "locator": log_path.name,
        },
        "executables": sorted(executables, key=lambda item: str(item["name"])),
    }
    record["build_record_ref"] = _content_ref(record)
    return record


def verify_installed_build(proof: dict[str, object], runner: Runner) -> str:
    builds = proof.get("builds")
    executables = proof.get("executables")
    if proof.get("status") != "reproducible":
        raise ProvenanceUnavailable("build proof is not reproducible")
    if not isinstance(builds, list) or len(builds) != 2:
        raise ProvenanceUnavailable("build proof membership is malformed")
    if not isinstance(executables, list) or not executables:
        raise ProvenanceUnavailable("build proof executable set is empty")
    output_refs = {row.get("output_ref") for row in builds if isinstance(row, dict)}
    output_paths = {row.get("nix_output") for row in executables if isinstance(row, dict)}
    nar_hashes = {row.get("nar_hash") for row in executables if isinstance(row, dict)}
    if len(output_refs) != 1 or len(output_paths) != 1 or nar_hashes != output_refs:
        raise ProvenanceUnavailable("installed output identity is not closed")
    output_path = next(iter(output_paths))
    expected_nar = next(iter(output_refs))
    if not isinstance(output_path, str) or not isinstance(expected_nar, str):
        raise ProvenanceUnavailable("installed output identity is malformed")
    if _output_nar_hash(runner, "daemon", output_path) != expected_nar:
        raise ProvenanceUnavailable("installed output NAR differs from build proof")
    for row in executables:
        if not isinstance(row, dict):
            raise ProvenanceUnavailable("installed executable record is malformed")
        name = row.get("name")
        expected_hash = row.get("sha256")
        expected_bytes = row.get("bytes")
        if (
            not isinstance(name, str)
            or not name
            or "/" in name
            or not isinstance(expected_hash, str)
            or not isinstance(expected_bytes, int)
        ):
            raise ProvenanceUnavailable("installed executable identity is malformed")
        streamed = runner.run(
            [
                "nix",
                "store",
                "cat",
                "--store",
                "daemon",
                f"{output_path}/bin/{name}",
            ]
        )
        if streamed.returncode != 0:
            raise ProvenanceUnavailable(f"installed executable is unavailable: {name}")
        if sha256_bytes(streamed.stdout) != expected_hash or len(streamed.stdout) != expected_bytes:
            raise ProvenanceUnavailable(f"installed executable differs from build proof: {name}")
    return output_path


def _read_json(path: Path) -> object:
    return json.loads(path.read_bytes())


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    realize = commands.add_parser("realize-build")
    realize.add_argument("--candidate-tree", type=Path, required=True)
    realize.add_argument("--build-id", choices=("build-a", "build-b"), required=True)
    realize.add_argument("--store-root", type=Path, required=True)
    realize.add_argument("--build-log", type=Path, required=True)
    realize.add_argument("--out", type=Path, required=True)
    capture = commands.add_parser("capture-build")
    capture.add_argument("--realization", type=Path, required=True)
    capture.add_argument("--graph", type=Path, required=True)
    capture.add_argument("--parser-git-rev", required=True)
    capture.add_argument("--out", type=Path, required=True)
    compare = commands.add_parser("compare-builds")
    compare.add_argument("--first", type=Path, required=True)
    compare.add_argument("--second", type=Path, required=True)
    compare.add_argument("--out", type=Path, required=True)
    installed = commands.add_parser("verify-installed")
    installed.add_argument("--proof", type=Path, required=True)
    bind = commands.add_parser("bind-provenance")
    bind.add_argument("--proof", type=Path, required=True)
    bind.add_argument("--candidate-ref", required=True)
    bind.add_argument("--qualification-identity-ref", required=True)
    bind.add_argument("--out", type=Path, required=True)
    resolve = commands.add_parser("resolve-executable")
    resolve.add_argument("--provenance", type=Path, required=True)
    resolve.add_argument("--name", required=True)
    evidence = commands.add_parser("verify-evidence")
    evidence.add_argument("--blobs", type=Path, required=True)
    evidence.add_argument("--evidence-root", type=Path, required=True)
    evidence.add_argument("--candidate-ref", required=True)
    evidence.add_argument("--capture-generation-ref", required=True)
    evidence.add_argument("--out", type=Path, required=True)
    return parser


def main(argv: list[str] | None = None) -> int:
    parser = _parser()
    try:
        args = parser.parse_args(argv)
    except SystemExit as error:
        if argv is not None:
            return int(error.code)
        raise
    runner = SubprocessRunner()
    try:
        if args.command == "realize-build":
            request = resolve_realize_request(
                args.candidate_tree,
                args.build_id,
                args.store_root,
                args.build_log,
                runner,
            )
            value = realize_target(request, runner)
            _atomic_json(args.out, value)
        elif args.command == "capture-build":
            value = capture_build(
                _read_json(args.realization),
                _read_json(args.graph),
                args.parser_git_rev,
                runner,
            )
            _atomic_json(args.out, value)
        elif args.command == "compare-builds":
            value = compare_builds(_read_json(args.first), _read_json(args.second))
            if value.get("status") != "reproducible":
                raise ProvenanceUnavailable(str(value.get("reason")))
            _atomic_json(args.out, value)
        elif args.command == "verify-installed":
            proof = _read_json(args.proof)
            if not isinstance(proof, dict):
                raise ProvenanceUnavailable("proof is not an object")
            print(verify_installed_build(proof, runner))
        elif args.command == "bind-provenance":
            proof = _read_json(args.proof)
            if not isinstance(proof, dict):
                raise ProvenanceUnavailable("proof is not an object")
            _atomic_json(
                args.out,
                bind_provenance(proof, args.candidate_ref, args.qualification_identity_ref),
            )
        elif args.command == "resolve-executable":
            provenance = _read_json(args.provenance)
            if not isinstance(provenance, dict):
                raise ProvenanceUnavailable("provenance is not an object")
            matches = [
                row
                for row in provenance.get("executables", [])
                if isinstance(row, dict) and row.get("name") == args.name
            ]
            if len(matches) != 1:
                raise ProvenanceUnavailable("executable does not resolve uniquely")
            print(f"{matches[0]['nix_output']}/bin/{args.name}")
        elif args.command == "verify-evidence":
            raw = _read_json(args.blobs)
            if not isinstance(raw, list):
                raise ProvenanceUnavailable("blob list is not an array")
            blobs = [LogicalBlob(**row) for row in raw]
            value = verify_evidence(blobs, args.evidence_root)
            receipt: dict[str, object] = {
                "schema_id": "https://w3id.org/abc/schemas/parser-rq-evidence-integrity-receipt.schema.json",
                "schema_version": "1.0.0",
                "candidate_ref": args.candidate_ref,
                "capture_generation_ref": args.capture_generation_ref,
                **value,
            }
            receipt["receipt_ref"] = receipt_ref(receipt)
            _atomic_json(args.out, receipt)
            if value.get("status") != "verified":
                raise ProvenanceUnavailable(str(value.get("reason")))
        else:
            raise ProvenanceUnavailable("unknown provenance command")
    except (OSError, ValueError, KeyError, json.JSONDecodeError) as error:
        print(str(error), file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
