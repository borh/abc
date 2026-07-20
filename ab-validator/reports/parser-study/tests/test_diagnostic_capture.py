import importlib.util
import json
import subprocess
from pathlib import Path

import pytest


SCRIPT = Path(__file__).parents[1] / "diagnostic_capture.py"
SPEC = importlib.util.spec_from_file_location("diagnostic_capture", SCRIPT)
assert SPEC and SPEC.loader
capture = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(capture)


REPO = Path(__file__).parents[3]
PREREGISTRATION = REPO / "docs/studies/aozora-parser-comparison-preregistration.json"
FIXTURE = REPO / "docs/studies/fixtures/parser-comparison-diagnostics-v1.json"
POLICY = REPO / "data/parser-study-diagnostic-lanes-v1.json"


def make_programs(tmp_path: Path, body: str) -> Path:
    tmp_path.mkdir(parents=True, exist_ok=True)
    policy = json.loads(POLICY.read_text())
    records = {}
    for basename in {lane["program"] for lane in policy["lanes"]}:
        program = tmp_path / basename
        program.write_text("#!/bin/sh\n" + body)
        program.chmod(0o755)
        records[basename] = {"path": str(program), "environment": {}}
    programs = tmp_path / "programs.json"
    programs.write_text(json.dumps(records))
    return programs


def run_capture(tmp_path: Path, body: str = "printf 'raw output\\n'") -> tuple[Path, Path, dict]:
    root = tmp_path / "bundle"
    manifest_path = tmp_path / "manifest.json"
    manifest = capture.capture(
        PREREGISTRATION,
        FIXTURE,
        POLICY,
        make_programs(tmp_path, body),
        root,
        manifest_path,
    )
    assert json.loads(manifest_path.read_bytes()) == manifest
    return root, manifest_path, manifest


def test_capture_writes_raw_bytes_before_any_projection(tmp_path: Path) -> None:
    root, _, manifest = run_capture(
        tmp_path, "printf '{not normalized}\\n'\nprintf 'raw warning\\n' >&2"
    )
    member = manifest["members"][0]
    assert (root / member["stdout"]["locator"]).read_bytes() == b"{not normalized}\n"
    assert (root / member["stderr"]["locator"]).read_bytes() == b"raw warning\n"


def test_capture_is_exact_closed_product_in_manifest_order(tmp_path: Path) -> None:
    root, manifest_path, manifest = run_capture(tmp_path)
    expected = [
        (candidate, mode, case["id"])
        for candidate, mode in capture.EXPECTED_LANES
        for case in json.loads(FIXTURE.read_text())["cases"]
    ]
    assert len(manifest["members"]) == 44
    assert [
        (member["candidate"], member["measurement_mode"], member["case_id"])
        for member in manifest["members"]
    ] == expected
    assert capture.verify(FIXTURE, POLICY, root, manifest_path) == manifest


def test_capture_records_nonzero_exit_without_normalizing_bytes(tmp_path: Path) -> None:
    _, _, manifest = run_capture(tmp_path, "printf failure >&2\nexit 17")
    assert all(member["status"] == "exited" for member in manifest["members"])
    assert all(member["returncode"] == 17 for member in manifest["members"])


def test_capture_records_timeout(tmp_path: Path) -> None:
    programs = make_programs(tmp_path, "while :; do :; done")
    root = tmp_path / "bundle"
    manifest = capture.capture(
        PREREGISTRATION,
        FIXTURE,
        POLICY,
        programs,
        root,
        tmp_path / "manifest.json",
        timeout_seconds=0.01,
    )
    assert all(member["status"] == "timed_out" for member in manifest["members"])
    assert all(member["returncode"] is None for member in manifest["members"])


def test_verify_rejects_tampering_and_missing_stream(tmp_path: Path) -> None:
    root, manifest_path, manifest = run_capture(tmp_path)
    stdout = root / manifest["members"][0]["stdout"]["locator"]
    stdout.write_bytes(b"tampered")
    with pytest.raises(ValueError, match="content mismatch"):
        capture.verify(FIXTURE, POLICY, root, manifest_path)

    manifest["members"][0].pop("stdout")
    with pytest.raises(ValueError, match="missing stdout"):
        capture.verify_members(root, manifest)


def test_verify_rejects_missing_case_duplicate_and_wrong_basename(tmp_path: Path) -> None:
    root, manifest_path, manifest = run_capture(tmp_path)
    manifest["members"].pop()
    manifest_path.write_bytes(capture.canonical_bytes(manifest))
    with pytest.raises(ValueError, match="membership or order"):
        capture.verify(FIXTURE, POLICY, root, manifest_path)

    policy = json.loads(POLICY.read_text())
    policy["lanes"][1] = policy["lanes"][0]
    bad_policy = tmp_path / "duplicate-policy.json"
    bad_policy.write_text(json.dumps(policy))
    with pytest.raises(ValueError, match="closed lane set"):
        capture.capture(
            PREREGISTRATION,
            FIXTURE,
            bad_policy,
            make_programs(tmp_path / "duplicate", "exit 0"),
            tmp_path / "duplicate-bundle",
            tmp_path / "duplicate-manifest.json",
        )

    programs = json.loads(make_programs(tmp_path / "basename", "exit 0").read_text())
    programs["aozora"]["path"] = programs["aozora-adapter"]["path"]
    bad_programs = tmp_path / "bad-programs.json"
    bad_programs.write_text(json.dumps(programs))
    with pytest.raises(ValueError, match="invalid executable identity"):
        capture.capture(
            PREREGISTRATION,
            FIXTURE,
            POLICY,
            bad_programs,
            tmp_path / "basename-bundle",
            tmp_path / "basename-manifest.json",
        )


def test_cli_requires_preregistration_and_writes_requested_manifest(tmp_path: Path) -> None:
    root = tmp_path / "bundle"
    manifest = tmp_path / "chosen-name.json"
    completed = subprocess.run(
        [
            "python",
            str(SCRIPT),
            "capture",
            "--preregistration",
            str(PREREGISTRATION),
            "--fixture",
            str(FIXTURE),
            "--lane-policy",
            str(POLICY),
            "--programs-json",
            str(make_programs(tmp_path, "exit 0")),
            "--bundle-root",
            str(root),
            "--manifest-out",
            str(manifest),
        ],
        check=False,
    )
    assert completed.returncode == 0
    assert manifest.is_file()
