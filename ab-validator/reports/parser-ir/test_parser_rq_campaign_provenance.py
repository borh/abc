from __future__ import annotations

import importlib.util
import base64
import json
from pathlib import Path
from typing import Any


MODULE_PATH = Path(__file__).with_name("parser-rq-campaign-provenance.py")
SPEC = importlib.util.spec_from_file_location("parser_rq_campaign_provenance", MODULE_PATH)
assert SPEC and SPEC.loader
module = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(module)


class FakeRunner:
    def __init__(self, responses: list[Any]) -> None:
        self.responses = list(responses)
        self.calls: list[tuple[list[str], dict[str, str] | None]] = []

    def run(self, argv: list[str], *, env: dict[str, str] | None = None) -> Any:
        self.calls.append((list(argv), env))
        assert self.responses, f"unexpected command: {argv}"
        return self.responses.pop(0)


def result(returncode: int = 0, stdout: bytes = b"", stderr: bytes = b""):
    return module.CommandResult(returncode, stdout, stderr)


def test_direct_seed_paths_are_closed_and_sorted() -> None:
    derivation = {
        "inputSrcs": ["/nix/store/cccc-source", "/nix/store/aaaa-source"],
        "inputDrvs": {
            "/nix/store/input-a.drv": {
                "out": ["/nix/store/dddd-dependency", "/nix/store/bbbb-dependency"]
            }
        },
    }
    assert module.direct_seed_paths(derivation) == (
        "/nix/store/aaaa-source",
        "/nix/store/bbbb-dependency",
        "/nix/store/cccc-source",
        "/nix/store/dddd-dependency",
    )


def test_realize_target_builds_absent_target_offline(tmp_path: Path) -> None:
    store_root = tmp_path / "store-a"
    build_log = tmp_path / "build-a.log"
    runner = FakeRunner(
        [
            result(1),
            result(),
            result(1),
            result(stderr=b"building '/nix/store/aaaaaaaa-target.drv'\n"),
            result(),
        ]
    )
    request = module.RealizeRequest(
        build_id="build-a",
        store_root=store_root,
        store_uri=f"local?root={store_root}",
        drv_path="/nix/store/aaaaaaaa-target.drv",
        output_path="/nix/store/bbbb-target",
        seed_inputs=("/nix/store/cccc-dependency",),
        build_log=build_log,
    )
    record = module.realize_target(request, runner)
    assert runner.calls[3][0] == [
        "nix",
        "build",
        "--store",
        request.store_uri,
        "--offline",
        "--no-link",
        "--json",
        f"{request.drv_path}^out",
    ]
    assert runner.calls[3][1] == {"LC_ALL": "C"}
    assert record["build_id"] == "build-a"
    assert record["target_absent_after_seed"] is True
    assert build_log.read_bytes().startswith(b"building '")


def test_realize_target_rejects_seeded_or_unbuilt_target(tmp_path: Path) -> None:
    request = module.RealizeRequest(
        "build-a",
        tmp_path / "store",
        f"local?root={tmp_path / 'store'}",
        "/nix/store/aaaaaaaa-target.drv",
        "/nix/store/bbbb-target",
        ("/nix/store/cccc-dependency",),
        tmp_path / "build.log",
    )
    seeded = FakeRunner([result(1), result(), result(0)])
    try:
        module.realize_target(request, seeded)
    except module.ProvenanceUnavailable as error:
        assert "present after dependency seeding" in str(error)
    else:
        raise AssertionError("seeded target must fail closed")


def test_realize_target_rejects_existing_store_and_unauthenticated_build_log(
    tmp_path: Path,
) -> None:
    existing = tmp_path / "existing-store"
    existing.mkdir()
    request = module.RealizeRequest(
        "build-a",
        existing,
        f"local?root={existing}",
        "/nix/store/aaaaaaaa-target.drv",
        "/nix/store/bbbb-target",
        ("/nix/store/cccc-dependency",),
        tmp_path / "build.log",
    )
    try:
        module.realize_target(request, FakeRunner([]))
    except module.ProvenanceUnavailable as error:
        assert "already exists" in str(error)
    else:
        raise AssertionError("existing store root must fail closed")

    absent = request._replace(store_root=tmp_path / "absent-store")
    runner = FakeRunner([result(1), result(), result(1), result(stderr=b"noise\n")])
    try:
        module.realize_target(absent, runner)
    except module.ProvenanceUnavailable as error:
        assert "not evidenced" in str(error)
    else:
        raise AssertionError("unauthenticated build log must fail closed")


def test_compare_builds_rejects_raw_nix_json() -> None:
    raw = [{"outputs": {"out": "/nix/store/aaaaaaaa-output"}}]
    assert module.compare_builds(raw, raw)["status"] == "unavailable"


def test_binding_preserves_the_unbound_core_hash() -> None:
    executable = {
        "name": "ab-check",
        "nix_output": "/nix/store/a-parser",
        "nar_hash": "sha256:" + "a" * 64,
        "sha256": "sha256:" + "b" * 64,
        "bytes": 10,
        "adapter": "ab-check",
        "adapter_version": "0.1.0",
        "parser_git_rev": "a" * 40,
        "argv_template": ["{executable}"],
    }
    proof = {
        "status": "reproducible",
        "provenance_core_ref": "sha256:" + "c" * 64,
        "builds": [
            {
                "build_id": "build-a",
                "store_uri": "local?root=/tmp/a",
                "output_ref": "sha256:" + "d" * 64,
                "build_record_ref": "sha256:" + "e" * 64,
            },
            {
                "build_id": "build-b",
                "store_uri": "local?root=/tmp/b",
                "output_ref": "sha256:" + "d" * 64,
                "build_record_ref": "sha256:" + "f" * 64,
            },
        ],
        "executables": [executable],
    }
    proof["provenance_core_ref"] = module.provenance_core_ref(proof)
    bound = module.bind_provenance(proof, "sha256:" + "1" * 64, "sha256:" + "2" * 64)
    assert bound["schema_version"] == "2.0.0"
    assert bound["provenance_core_ref"] == proof["provenance_core_ref"]
    assert module.provenance_core_ref(bound) == proof["provenance_core_ref"]


def test_capture_build_streams_exact_graph_executable_set(tmp_path: Path) -> None:
    build_log = tmp_path / "build.log"
    build_log.write_text("building '/nix/store/aaaaaaaa-target.drv'\n")
    realization = {
        "build_id": "build-a",
        "store_uri": "local?root=/tmp/store-a",
        "initially_empty": True,
        "target_absent_before_seed": True,
        "target_absent_after_seed": True,
        "drv_path": "/nix/store/aaaaaaaa-target.drv",
        "output_path": "/nix/store/bbbbbbbb-target",
        "seeded_inputs": ["/nix/store/cccccccc-dependency"],
        "build_log_path": str(build_log),
    }
    graph = {
        "executables": [
            {
                "name": "ab-check",
                "adapter": "ab-check",
                "adapter_version": "0.1.0",
                "argv_template": ["{executable}", "--corpus", "{corpus_root}"],
            }
        ]
    }
    nar_hash = "sha256-" + base64.b64encode(bytes(range(32))).decode()
    runner = FakeRunner(
        [
            result(stdout=json.dumps([{"narHash": nar_hash}]).encode()),
            result(stdout=b"candidate executable"),
        ]
    )
    record = module.capture_build(realization, graph, "a" * 40, runner)
    assert record["schema_version"] == "1.0.0"
    assert record["output_ref"] == "sha256:" + bytes(range(32)).hex()
    assert [row["name"] for row in record["executables"]] == ["ab-check"]
    assert runner.calls[1][0] == [
        "nix",
        "store",
        "cat",
        "--store",
        realization["store_uri"],
        f"{realization['output_path']}/bin/ab-check",
    ]


def test_capture_build_rejects_duplicate_graph_executable(tmp_path: Path) -> None:
    build_log = tmp_path / "build.log"
    build_log.write_text("building '/nix/store/aaaaaaaa-target.drv'\n")
    realization = {
        "build_id": "build-a",
        "store_uri": "local?root=/tmp/store-a",
        "initially_empty": True,
        "target_absent_before_seed": True,
        "target_absent_after_seed": True,
        "drv_path": "/nix/store/aaaaaaaa-target.drv",
        "output_path": "/nix/store/bbbbbbbb-target",
        "seeded_inputs": ["/nix/store/cccccccc-dependency"],
        "build_log_path": str(build_log),
    }
    executable = {
        "name": "ab-check",
        "adapter": "ab-check",
        "adapter_version": "0.1.0",
        "argv_template": ["{executable}"],
    }
    nar_hash = "sha256-" + base64.b64encode(bytes(range(32))).decode()
    runner = FakeRunner(
        [
            result(stdout=json.dumps([{"narHash": nar_hash}]).encode()),
            result(stdout=b"candidate executable"),
        ]
    )
    try:
        module.capture_build(realization, {"executables": [executable] * 2}, "a" * 40, runner)
    except module.ProvenanceUnavailable as error:
        assert "duplicated" in str(error)
    else:
        raise AssertionError("duplicate graph executable must fail closed")


def test_every_cli_subcommand_has_help() -> None:
    for command in (
        "realize-build",
        "capture-build",
        "compare-builds",
        "bind-provenance",
        "resolve-executable",
        "verify-evidence",
    ):
        assert module.main([command, "--help"]) == 0


def test_realize_cli_derives_target_and_seed_set_from_candidate_tree(
    tmp_path: Path, monkeypatch
) -> None:
    drv = "/nix/store/aaaaaaaa-target.drv"
    output = "/nix/store/bbbbbbbb-target"
    source = "/nix/store/cccccccc-source"
    input_drv = "/nix/store/dddddddd-input.drv"
    dependency = "/nix/store/eeeeeeee-dependency"
    target_json = {
        "version": 4,
        "derivations": {
            Path(drv).name: {
                "outputs": {"out": {"path": Path(output).name}},
                "inputs": {
                    "srcs": [Path(source).name],
                    "drvs": {
                        Path(input_drv).name: {
                            "dynamicOutputs": {},
                            "outputs": ["out"],
                        }
                    },
                },
            }
        },
    }
    input_json = {
        "version": 4,
        "derivations": {
            Path(input_drv).name: {
                "outputs": {"out": {"path": Path(dependency).name}},
                "inputs": {"srcs": [], "drvs": {}},
            }
        },
    }
    runner = FakeRunner(
        [
            result(stdout=(drv + "\n").encode()),
            result(stdout=(output + "\n").encode()),
            result(stdout=json.dumps(target_json).encode()),
            result(stdout=json.dumps([{"path": source}]).encode()),
            result(stdout=json.dumps(input_json).encode()),
            result(stdout=json.dumps({dependency: {"narHash": "sha256-unused"}}).encode()),
            result(1),
            result(),
            result(1),
            result(stderr=f"building '{drv}'\n".encode()),
            result(),
        ]
    )
    monkeypatch.setattr(module, "SubprocessRunner", lambda: runner)
    out = tmp_path / "realization.json"
    assert (
        module.main(
            [
                "realize-build",
                "--candidate-tree",
                str(tmp_path / "candidate"),
                "--build-id",
                "build-a",
                "--store-root",
                str(tmp_path / "store"),
                "--build-log",
                str(tmp_path / "build.log"),
                "--out",
                str(out),
            ]
        )
        == 0
    )
    value = json.loads(out.read_bytes())
    assert value["drv_path"] == drv
    assert value["output_path"] == output
    assert value["seeded_inputs"] == [source, dependency]
    assert runner.calls[5][0] == [
        "nix",
        "path-info",
        "--recursive",
        "--json",
        "--json-format",
        "1",
        dependency,
    ]
    assert all("--drv" not in call[0] for call in runner.calls)


def test_realize_cli_rejects_unavailable_direct_source(tmp_path: Path, monkeypatch) -> None:
    drv = "/nix/store/aaaaaaaa-target.drv"
    source = "/nix/store/cccccccc-source"
    target_json = {
        "version": 4,
        "derivations": {
            Path(drv).name: {
                "outputs": {"out": {"path": "bbbbbbbb-target"}},
                "inputs": {"srcs": [Path(source).name], "drvs": {}},
            }
        },
    }
    runner = FakeRunner(
        [
            result(stdout=(drv + "\n").encode()),
            result(stdout=b"/nix/store/bbbbbbbb-target\n"),
            result(stdout=json.dumps(target_json).encode()),
            result(1),
        ]
    )
    monkeypatch.setattr(module, "SubprocessRunner", lambda: runner)
    out = tmp_path / "realization.json"
    assert (
        module.main(
            [
                "realize-build",
                "--candidate-tree",
                str(tmp_path / "candidate"),
                "--build-id",
                "build-a",
                "--store-root",
                str(tmp_path / "store"),
                "--build-log",
                str(tmp_path / "build.log"),
                "--out",
                str(out),
            ]
        )
        == 2
    )
    assert not out.exists()


def test_cli_help_and_atomic_compare_output(tmp_path: Path) -> None:
    assert module.main(["--help"]) == 0
    first = tmp_path / "first.json"
    second = tmp_path / "second.json"
    out = tmp_path / "proof.json"
    raw = [{"outputs": {"out": "/nix/store/aaaaaaaa-output"}}]
    first.write_text(json.dumps(raw))
    second.write_text(json.dumps(raw))
    assert (
        module.main(
            [
                "compare-builds",
                "--first",
                str(first),
                "--second",
                str(second),
                "--out",
                str(out),
            ]
        )
        != 0
    )
    assert not out.exists()


def test_executable_record_streams_and_binds_all_coordinates(tmp_path: Path) -> None:
    executable = tmp_path / "parser"
    executable.write_bytes(b"candidate bytes")
    record = module.executable_record(
        executable,
        {
            "name": "ab-check",
            "nix_output": "/nix/store/example-parser",
            "nar_hash": "sha256:" + "a" * 64,
            "adapter": "ab-aozora",
            "adapter_version": "v1",
            "parser_git_rev": "a" * 40,
        },
        ["{executable}", "--input", "{source}"],
    )
    assert record["bytes"] == len(b"candidate bytes")
    assert record["sha256"].startswith("sha256:")
    assert record["argv_template"][0] == "{executable}"


def test_compare_builds_rejects_every_semantic_difference() -> None:
    executable = {
        "name": "ab-check",
        "nix_output": "/nix/store/a-parser",
        "nar_hash": "sha256:" + "a" * 64,
        "sha256": "sha256:" + "b" * 64,
        "bytes": 10,
        "adapter": "ab-aozora",
        "adapter_version": "v1",
        "parser_git_rev": "a" * 40,
        "argv_template": ["{executable}"],
    }
    first = {"output_ref": "sha256:" + "c" * 64, "executables": [executable]}
    second = {"output_ref": first["output_ref"], "executables": [dict(executable)]}
    assert module.compare_builds(first, second)["status"] == "reproducible"
    for field, changed in [
        ("nix_output", "/nix/store/b-parser"),
        ("nar_hash", "sha256:" + "d" * 64),
        ("sha256", "sha256:" + "e" * 64),
        ("bytes", 11),
        ("adapter", "other"),
        ("adapter_version", "v2"),
        ("parser_git_rev", "b" * 40),
        ("argv_template", ["different"]),
    ]:
        altered = {"output_ref": first["output_ref"], "executables": [dict(executable)]}
        altered["executables"][0][field] = changed
        assert module.compare_builds(first, altered)["status"] == "unavailable"
    duplicated = {
        "output_ref": first["output_ref"],
        "executables": [dict(executable), dict(executable)],
    }
    assert module.compare_builds(first, duplicated)["status"] == "unavailable"
    malformed = {"output_ref": first["output_ref"], "executables": ["not-an-object"]}
    assert module.compare_builds(first, malformed)["status"] == "unavailable"


def test_compare_builds_requires_distinct_closed_build_realizations() -> None:
    executable = {
        "name": "ab-check",
        "nix_output": "/nix/store/a-parser",
        "nar_hash": "sha256:" + "a" * 64,
        "sha256": "sha256:" + "b" * 64,
        "bytes": 10,
        "adapter": "ab-aozora",
        "adapter_version": "v1",
        "parser_git_rev": "a" * 40,
        "argv_template": ["{executable}"],
    }
    first = {
        "build_id": "build-a",
        "store_uri": "local?root=/tmp/a",
        "output_ref": "sha256:" + "c" * 64,
        "build_record_ref": "sha256:" + "d" * 64,
        "executables": [executable],
    }
    same_id = {
        **first,
        "store_uri": "local?root=/tmp/b",
        "build_record_ref": "sha256:" + "e" * 64,
    }
    same_store = {
        **first,
        "build_id": "build-b",
        "build_record_ref": "sha256:" + "e" * 64,
    }
    assert module.compare_builds(first, same_id)["status"] == "unavailable"
    assert module.compare_builds(first, same_store)["status"] == "unavailable"


def test_verify_evidence_authenticates_one_closed_store(tmp_path: Path) -> None:
    payload = b"immutable evidence"
    digest = module.sha256_bytes(payload)
    blob = module.LogicalBlob(digest, len(payload), "application/json", "aa/blob")
    (tmp_path / "aa").mkdir()
    (tmp_path / "aa/blob").write_bytes(payload)
    value = module.verify_evidence([blob], tmp_path)
    assert value == {
        "status": "verified",
        "blobs": [{"blob": blob._asdict(), "rehash": digest, "observed_bytes": len(payload)}],
    }


def test_verify_evidence_fails_closed(tmp_path: Path) -> None:
    payload = b"immutable evidence"
    digest = module.sha256_bytes(payload)
    cases = [
        module.LogicalBlob(digest, len(payload), "application/json", "missing"),
        module.LogicalBlob(digest, len(payload), "application/json", "../escape"),
        module.LogicalBlob(digest, len(payload), "", "blob"),
    ]
    for blob in cases:
        assert module.verify_evidence([blob], tmp_path)["status"] == "unavailable"

    (tmp_path / "blob").write_bytes(b"wrong")
    blob = module.LogicalBlob(digest, len(payload), "application/json", "blob")
    assert module.verify_evidence([blob], tmp_path)["status"] == "unavailable"
    assert module.verify_evidence([blob, blob], tmp_path)["status"] == "unavailable"


def test_verify_evidence_cli_writes_bound_self_authenticating_receipt(
    tmp_path: Path,
) -> None:
    payload = b"immutable evidence"
    digest = module.sha256_bytes(payload)
    blob = {
        "sha256": digest,
        "bytes": len(payload),
        "media_type": "application/json",
        "locator": "blob",
    }
    (tmp_path / "blob").write_bytes(payload)
    blobs = tmp_path / "blobs.json"
    blobs.write_text(json.dumps([blob]))
    output = tmp_path / "receipt.json"
    candidate_ref = "sha256:" + "a" * 64
    capture_ref = "sha256:" + "b" * 64
    assert (
        module.main(
            [
                "verify-evidence",
                "--blobs",
                str(blobs),
                "--evidence-root",
                str(tmp_path),
                "--candidate-ref",
                candidate_ref,
                "--capture-generation-ref",
                capture_ref,
                "--out",
                str(output),
            ]
        )
        == 0
    )
    receipt = json.loads(output.read_text())
    assert receipt["candidate_ref"] == candidate_ref
    assert receipt["capture_generation_ref"] == capture_ref
    assert receipt["receipt_ref"] == module.receipt_ref(receipt)
    assert module.main(["verify" + "-replicas", "--help"]) == 2
