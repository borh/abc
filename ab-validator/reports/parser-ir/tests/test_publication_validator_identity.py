from __future__ import annotations

import importlib.util
import json
import pathlib

import pytest


AB_ROOT = pathlib.Path(__file__).resolve().parents[3]
SCRIPT = AB_ROOT / "reports/parser-ir/publication-validator-identity.py"
VALIDATOR = AB_ROOT / "reports/parser-ir/publication-bundle-validate.py"
COMMITTED = AB_ROOT / "data/parser-rq-publication-validator-v1.json"


@pytest.fixture
def module():
    spec = importlib.util.spec_from_file_location("publication_validator_identity", SCRIPT)
    assert spec and spec.loader
    loaded = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(loaded)
    return loaded


def test_reviewed_sources_equal_actual_transitive_local_imports(module):
    actual = module.transitive_local_imports(VALIDATOR, AB_ROOT / "reports")
    assert actual == {AB_ROOT / path for path in module.REVIEWED_SOURCE_PATHS}


def test_manifest_hash_recomputes_and_check_detects_drift(module):
    manifest = module.build_manifest(AB_ROOT)
    assert manifest["validator_semantics_hash"] == module.projected_hash(manifest)
    assert module.check_manifest(manifest, manifest) == []
    stale = json.loads(json.dumps(manifest))
    stale["sources"][0]["sha256"] = "sha256:" + "0" * 64
    assert module.check_manifest(stale, manifest)


def test_committed_manifest_still_describes_the_validator(module):
    """The committed manifest is what a capture cites, so it has to be current.

    Without this the manifest can only drift: --check is the sole reader of
    the committed file, and nothing ran it. Two source files had already
    changed underneath it before this test existed.
    """
    committed = json.loads(COMMITTED.read_text(encoding="utf-8"))
    assert module.check_manifest(committed, module.build_manifest(AB_ROOT)) == []


def test_unreviewed_local_import_fails_closed(module, tmp_path):
    root = tmp_path / "ab-validator"
    for path in module.REVIEWED_SOURCE_PATHS:
        destination = root / path
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_bytes((AB_ROOT / path).read_bytes())
    freshness = root / "reports/lib/freshness.py"
    freshness.write_text("x = 1\n", encoding="utf-8")
    validator = root / module.REVIEWED_SOURCE_PATHS[0]
    validator.write_text(
        validator.read_text(encoding="utf-8") + "\nfrom reports.lib.freshness import x\n",
        encoding="utf-8",
    )
    with pytest.raises(ValueError, match="reviewed semantic closure differs"):
        module.build_manifest(root)
