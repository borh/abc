import json
import sys
from pathlib import Path

_CONFORMANCE = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(_CONFORMANCE))

# Import the module directly
import importlib.util

spec = importlib.util.spec_from_file_location(
    "run_aozora_notation_spec", _CONFORMANCE / "run-aozora-notation-spec.py"
)
scorer = importlib.util.module_from_spec(spec)
sys.modules["run_aozora_notation_spec"] = scorer
spec.loader.exec_module(scorer)


def fake_diag_adapter(tmp_path, entries, schema_version=3):
    """A stub diagnostics command: prints a fixed envelope."""
    stub = tmp_path / "diag.sh"
    envelope = json.dumps({"schemaVersion": schema_version, "data": entries})
    stub.write_text(
        "#!/bin/sh\ncat >/dev/null\nprintf '%s\\n' "
        + json.dumps(envelope).replace("%", "%%")
        + "\n"
    )
    stub.chmod(0o755)
    aat = tmp_path / "aat.sh"
    aat.write_text('#!/bin/sh\ncat >/dev/null\nprintf \'{"blocks":[],"meta":{}}\\n\'\n')
    aat.chmod(0o755)
    return scorer.Adapter(
        label="x", mode="aat", command=[str(aat)], diagnostics_command=[str(stub)]
    )


def vector(diagnostics):
    return {
        "name": "v",
        "meta": {"feature": "f", "level": "must"},
        "source": "s",
        "expected": {"diagnostics": diagnostics},
    }


FULL = {
    "kind": "unclosed_bracket",
    "code": "unclosed-bracket",
    "severity": "error",
    "source": "source",
    "span": {"start": 1, "end": 4},
}
WANT = [{"code": "unclosed-bracket", "severity": "error", "span": {"start": 1, "end": 4}}]


def test_matching_diagnostics_pass(tmp_path):
    row = scorer.evaluate(fake_diag_adapter(tmp_path, [FULL]), vector(WANT))
    assert row.status == "pass", (row.failures, row.skips)


def test_mismatching_span_fails_at_must(tmp_path):
    bad = dict(FULL, span={"start": 0, "end": 4})
    row = scorer.evaluate(fake_diag_adapter(tmp_path, [bad]), vector(WANT))
    assert row.status == "fail"


def test_missing_code_key_fails(tmp_path):
    entry = {k: v for k, v in FULL.items() if k != "code"}
    row = scorer.evaluate(fake_diag_adapter(tmp_path, [entry]), vector(WANT))
    assert row.status == "fail"


def test_wrong_schema_version_fails(tmp_path):
    row = scorer.evaluate(fake_diag_adapter(tmp_path, [FULL], schema_version=2), vector(WANT))
    assert row.status == "fail"


def test_without_diagnostics_command_still_skips(tmp_path):
    adapter = fake_diag_adapter(tmp_path, [FULL])
    adapter = scorer.Adapter(label="x", mode="aat", command=adapter.command)
    row = scorer.evaluate(adapter, vector(WANT))
    assert row.status == "skip"
    assert any("not comparable" in s for s in row.skips)


def manifest_file(tmp_path, entries):
    p = tmp_path / "manifest.json"
    p.write_text(json.dumps(entries))
    return p


def test_manifest_overrides_expected_for_listed_vector(tmp_path):
    import hashlib

    sha = hashlib.sha256(b"s").hexdigest()
    decoded_want = [
        {"code": "unclosed-bracket", "severity": "error", "span": {"start": 3, "end": 6}}
    ]
    entry = dict(FULL, span={"start": 3, "end": 6})
    adapter = fake_diag_adapter(tmp_path, [entry])
    manifest = scorer.load_span_deviation_manifest(
        manifest_file(
            tmp_path,
            [
                {
                    "vector": "v",
                    "reason": "crlf",
                    "original_expected": WANT,
                    "expected": decoded_want,
                    "source_sha256": sha,
                }
            ],
        )
    )
    row = scorer.evaluate(adapter, vector(WANT), manifest=manifest)
    assert row.status == "pass", (row.failures,)


def test_manifest_hash_mismatch_fails(tmp_path):
    manifest = scorer.load_span_deviation_manifest(
        manifest_file(
            tmp_path,
            [
                {
                    "vector": "v",
                    "reason": "crlf",
                    "original_expected": WANT,
                    "expected": WANT,
                    "source_sha256": "0" * 64,
                }
            ],
        )
    )
    row = scorer.evaluate(fake_diag_adapter(tmp_path, [FULL]), vector(WANT), manifest=manifest)
    assert row.status == "fail"
    assert any("manifest" in f for f in row.failures)


def test_unlisted_divergence_still_fails(tmp_path):
    shifted = dict(FULL, span={"start": 3, "end": 6})
    row = scorer.evaluate(fake_diag_adapter(tmp_path, [shifted]), vector(WANT), manifest={})
    assert row.status == "fail"


def entry(**kw):
    import hashlib

    base = {
        "vector": "v",
        "reason": "crlf",
        "original_expected": WANT,
        "expected": WANT,
        "source_sha256": hashlib.sha256(b"s").hexdigest(),
    }
    base.update(kw)
    return base


def test_duplicate_vector_ids_rejected(tmp_path):
    import pytest

    with pytest.raises(SystemExit, match="duplicate"):
        scorer.load_span_deviation_manifest(manifest_file(tmp_path, [entry(), entry()]))


def test_unknown_fields_rejected(tmp_path):
    import pytest

    with pytest.raises(SystemExit, match="fields must be exactly"):
        scorer.load_span_deviation_manifest(manifest_file(tmp_path, [entry(extra=1)]))


def test_stale_original_expected_fails(tmp_path):
    stale = entry(
        original_expected=[
            {"code": "unclosed-bracket", "severity": "error", "span": {"start": 0, "end": 1}}
        ]
    )
    manifest = scorer.load_span_deviation_manifest(manifest_file(tmp_path, [stale]))
    row = scorer.evaluate(fake_diag_adapter(tmp_path, [FULL]), vector(WANT), manifest=manifest)
    assert row.status == "fail"
    assert any("stale" in f for f in row.failures)


def test_unknown_and_unused_entries_rejected():
    import pytest

    manifest = {"ghost": entry(vector="ghost")}
    with pytest.raises(SystemExit, match="match no loaded vector"):
        scorer.check_manifest_consumed(manifest, {"v"}, set())
    manifest = {"v": entry()}
    with pytest.raises(SystemExit, match="never exercised"):
        scorer.check_manifest_consumed(manifest, {"v"}, set())


def test_exercised_entry_passes_consumed_check():
    scorer.check_manifest_consumed({"v": entry()}, {"v"}, {"v"})
