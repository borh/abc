"""The normative classify_line model must reproduce every shared vector.

The same vector file is consumed by the Rust mirror test
(crates/ab-aozora-aat/tests/bare_toggle_model.rs) — Task 5. Editing a
vector means BOTH sides re-verify; never edit expectations to match an
implementation."""
import importlib.util
import json
import pathlib
import sys

HERE = pathlib.Path(__file__).resolve()
AAT_FIDELITY = HERE.parents[1]
VECTORS = json.loads((AAT_FIDELITY / "bare-toggle-model-vectors.json").read_text())

spec = importlib.util.spec_from_file_location(
    "bare_toggle_placement", AAT_FIDELITY / "bare-toggle-placement.py"
)
mod = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = mod
spec.loader.exec_module(mod)


def test_every_vector_matches_model():
    for vec in VECTORS["vectors"]:
        outcome = mod.classify_line(vec["line"])
        name = vec["name"]
        assert outcome.adopted_pairs == vec["adopted"], name
        assert sorted(outcome.invalid_constructs) == sorted(vec["invalid"]), name
        assert outcome.orphan_open == vec["orphan_open"], name
        assert outcome.orphan_close == vec["orphan_close"], name
        assert outcome.reopen == vec["reopen"], name
        assert outcome.interleave_events == vec["interleave_events"], name
        assert outcome.proper_nestings == vec["proper_nestings"], name
        assert outcome.rollback_markers == vec["rollback_markers"], name
