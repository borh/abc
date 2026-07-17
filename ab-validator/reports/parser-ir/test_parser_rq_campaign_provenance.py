from __future__ import annotations

import importlib.util
from pathlib import Path


MODULE_PATH = Path(__file__).with_name("parser-rq-campaign-provenance.py")
SPEC = importlib.util.spec_from_file_location("parser_rq_campaign_provenance", MODULE_PATH)
assert SPEC and SPEC.loader
module = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(module)


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


def test_replication_rehashes_both_failure_domains(tmp_path: Path) -> None:
    primary = tmp_path / "primary"
    replica = tmp_path / "replica"
    primary.mkdir()
    replica.mkdir()
    payload = b"immutable evidence"
    digest = module.sha256_bytes(payload)
    blob = module.LogicalBlob(digest, len(payload), "application/json", "aa/blob")
    for root in (primary, replica):
        (root / "aa").mkdir()
        (root / "aa/blob").write_bytes(payload)
    result = module.verify_replicas([blob], (primary, replica))
    assert result["status"] == "replicated"
    (replica / "aa/blob").write_bytes(b"wrong")
    assert module.verify_replicas([blob], (primary, replica))["status"] == "unavailable"


def test_replication_rejects_alias_missing_escape_and_media_type(tmp_path: Path) -> None:
    primary = tmp_path / "primary"
    replica = tmp_path / "replica"
    primary.mkdir()
    replica.mkdir()
    good = module.LogicalBlob("sha256:" + "a" * 64, 0, "application/json", "blob")
    assert module.verify_replicas([good], (primary, primary))["status"] == "unavailable"
    assert module.verify_replicas([good], (primary, replica))["status"] == "unavailable"
    escaped = module.LogicalBlob(good.sha256, 0, "application/json", "../blob")
    assert module.verify_replicas([escaped], (primary, replica))["status"] == "unavailable"
    invalid_media = module.LogicalBlob(good.sha256, 0, "", "blob")
    assert module.verify_replicas([invalid_media], (primary, replica))["status"] == "unavailable"
