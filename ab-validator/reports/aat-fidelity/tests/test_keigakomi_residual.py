"""Tests for reports/aat-fidelity/keigakomi-residual-attribution.py (Task 11).

Two families:

  * `scan_markers` characterization tests -- transcribed from the Rust
    `ab-source-syntax` unit tests it ports
    (crates/ab-source-syntax/src/lib.rs `#[cfg(test)] mod tests`), so a
    change to either side that breaks parity is caught without needing a
    Rust toolchain in this test run.
  * The Task-11-required pure-function tests: pattern-union construction
    (matrix alternation), dedup semantics (per-marker/per-row credit,
    composite adjacency), and a synthetic overlapping case.
"""

import importlib.util
import pathlib
import sys

HERE = pathlib.Path(__file__).resolve()
AAT_FIDELITY = HERE.parents[1]

spec = importlib.util.spec_from_file_location(
    "keigakomi_residual_attribution", AAT_FIDELITY / "keigakomi-residual-attribution.py"
)
assert spec is not None and spec.loader is not None
mod = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = mod
spec.loader.exec_module(mod)


# --------------------------------------------------------------------------
# scan_markers characterization (transcribed Rust fixtures)
# --------------------------------------------------------------------------


def test_source_markers_return_raw_marker_body_and_span():
    text = (
        "吾輩《わがはい》\n※［＃「口＋世」、U+546D］\n［＃ここから横組み］\n"
        "〔e'tude〕\n［＃地付き］（fixture）\n底本：fixture"
    )
    markers = mod.scan_markers(text)

    assert len(markers) == 5
    assert markers[0].kind == "RubyImplicit"
    assert markers[0].raw == "《わがはい》"
    assert markers[0].line == 1

    assert markers[1].kind == "GaijiFullwidth"
    assert markers[1].raw == "※［＃「口＋世」、U+546D］"
    assert markers[1].line == 2

    assert markers[2].kind == "CommandFullwidth"
    assert markers[2].raw == "［＃ここから横組み］"

    assert markers[3].kind == "AccentNotation"
    assert markers[3].raw == "〔e'tude〕"

    assert markers[4].kind == "SegmentBoundaryTerminalProvenance"
    assert markers[4].raw == "［＃地付き］（fixture）"


def test_source_markers_keep_literal_fullwidth_brackets_inside_commands():
    text = "［＃「［Ａ］のようにも」は底本では「［Ａ］ようにも」］"
    markers = mod.scan_markers(text)

    assert len(markers) == 1
    assert markers[0].kind == "CommandFullwidth"
    assert markers[0].raw == text


def test_source_markers_surface_malformed_starts():
    text = "※［＃未完了\n※[#broken\n［＃ここから割り注\n[#broken\n｜未完了\n《未完了\n〔未完了"
    markers = mod.scan_markers(text)

    assert len(markers) == 6
    assert markers[0].kind == "MalformedGaiji"
    assert markers[0].raw == "※［＃"
    assert markers[1].kind == "MalformedGaijiAscii"
    assert markers[1].raw == "※[#"
    assert markers[2].kind == "MalformedCommand"
    assert markers[2].raw == "［＃"
    assert markers[3].kind == "MalformedCommandAscii"
    assert markers[3].raw == "[#"
    assert markers[4].kind == "MalformedImplicitRuby"
    assert markers[4].raw == "《"
    assert markers[5].kind == "MalformedAccentNotation"
    assert markers[5].raw == "〔"


def test_source_markers_ignore_bare_ruby_base_bars():
    markers = mod.scan_markers("本文｜そのまま\n｜未完了\n｜吾輩《わがはい》")

    assert len(markers) == 1
    assert markers[0].kind == "RubyExplicit"
    assert markers[0].raw == "｜吾輩《わがはい》"


def test_source_markers_ignore_ruby_marker_legend_bars():
    markers = mod.scan_markers(
        "｜：ルビの付く文字列の始まりを特定する記号\n｜；ルビの付く文字列の始まりを特定する記号"
    )

    assert all(marker.kind != "MalformedRuby" for marker in markers)


def test_source_markers_ignore_empty_accent_brackets():
    markers = mod.scan_markers("〔〕：アクセント分解された欧文をかこむ\n〔e'tude〕")

    assert len(markers) == 1
    assert markers[0].kind == "AccentNotation"
    assert markers[0].raw == "〔e'tude〕"


def test_source_markers_capture_multiline_bracket_notes():
    text = (
        "〔空しき秋二十数篇は散佚して今はなし。その第十二のみ、諸井\n"
        "三郎の作曲によりて残りしものなり。〕\n［＃地付き］（fixture）\n底本：fixture"
    )
    markers = mod.scan_markers(text)

    assert len(markers) == 2
    assert markers[0].kind == "BracketNote"
    assert markers[0].raw == (
        "〔空しき秋二十数篇は散佚して今はなし。その第十二のみ、諸井\n三郎の作曲によりて残りしものなり。〕"
    )
    assert markers[0].line == 1
    assert markers[1].kind == "SegmentBoundaryTerminalProvenance"
    assert markers[1].line == 3


def test_command_end_on_same_line_skips_nested_gaiji_marker():
    """A command whose body contains a nested gaiji marker (its own ［＃…］)
    is tokenized as ONE outer CommandFullwidth marker spanning to the
    outer closing bracket -- transcribed from the Rust
    `comparison_lossy_body_removes_command_with_nested_gaiji_marker`
    fixture (a `source_events`-level test; here it exercises the same
    `command_end_on_same_line` nesting logic at the marker-tokenizer
    level)."""
    text = "豌豆《ゑんどう》［＃「豌豆」は底本では「※［＃「足＋宛」、第3水準1-92-36］豆」］の大さ"
    markers = mod.scan_markers(text)

    assert markers[0].kind == "RubyImplicit"
    assert markers[0].raw == "《ゑんどう》"
    assert markers[1].kind == "CommandFullwidth"
    assert markers[1].raw == "［＃「豌豆」は底本では「※［＃「足＋宛」、第3水準1-92-36］豆」］"


# --------------------------------------------------------------------------
# Task 11 required tests: pattern-union construction, dedup semantics,
# synthetic overlapping case.
# --------------------------------------------------------------------------


def test_load_keigakomi_patterns_returns_13_patterns_from_real_matrix():
    patterns = mod.load_keigakomi_patterns()

    assert len(patterns) == 13
    assert patterns[0] == "［＃ここから罫囲み］"
    assert all(isinstance(pattern, str) and pattern for pattern in patterns)


def test_build_matrix_alternation_preserves_pattern_order_and_identity():
    patterns = ["AAA", "BBB", "CCC"]
    alternation = mod.build_matrix_alternation(patterns)

    match = alternation.search("xxxBBByyy")
    assert match is not None
    assert match.group(0) == "BBB"
    matched_names = [name for name, value in match.groupdict().items() if value is not None]
    assert matched_names == ["p1"]


def test_matrix_matches_alternation_is_non_overlapping_and_dedups_across_patterns():
    """Two of the construct's own patterns can both describe the SAME
    marker text; `re.finditer` over the union alternation counts it once
    (non-overlapping match consumption) -- the dedup semantics that
    produced the frozen matrix-exact figure of 673."""
    patterns = [r"［＃(ここで)?罫囲み(終わり)?］", r"［＃[^］]*罫囲み[^］]*］"]
    alternation = mod.build_matrix_alternation(patterns)

    hits = mod.matrix_matches("前置き［＃罫囲み終わり］後書き", alternation)

    assert len(hits) == 1
    assert hits[0][1] == "［＃罫囲み終わり］"


def test_rust_matches_credits_a_marker_at_most_once_per_row_even_if_multiple_own_patterns_match():
    """`matching_rows`' `!rows.contains(&pattern.row_id)` dedup: a single
    marker whose raw text satisfies TWO of the row's own patterns still
    earns exactly one occurrence credit."""
    patterns = [r"［＃(ここで)?罫囲み(終わり)?］", r"［＃[^］]*罫囲み[^］]*］"]
    compiled = [__import__("re").compile(p) for p in patterns]

    hits = mod.rust_matches("前置き［＃罫囲み終わり］後書き", compiled)

    assert len(hits) == 1
    assert hits[0] == (1, "［＃罫囲み終わり］", "marker")


def test_rust_matches_composite_adjacency_credits_neither_half_alone():
    """Two textually-adjacent (zero-gap) markers, NEITHER of which alone
    satisfies any of the row's patterns, can jointly earn ONE occurrence
    credit if their COMBINED raw text matches -- `append_composite_
    matching_rows`'s exact rule. Synthetic case: pattern requires the
    literal run 'AB' with nothing between; marker one's raw ends in 'A',
    marker two's raw (an adjacent bracket note) starts with 'B'."""
    patterns = [r"AB"]
    compiled = [__import__("re").compile(p) for p in patterns]
    # 〔A〕 followed immediately (zero gap) by 〔B...〕: combined raw is
    # "〔A〕〔B〕" -- note neither individual raw ("〔A〕", "〔B〕") contains
    # the literal substring "AB" alone, but concatenated they still don't
    # either (the brackets sit in between). This first attempt shows the
    # NEGATIVE case: composite adjacency does NOT bridge marker delimiters.
    hits = mod.rust_matches("〔A〕〔B〕", compiled)
    assert hits == []

    # A genuine composite hit requires the pattern to be satisfied by the
    # literal concatenation of the two raw marker strings themselves,
    # STRADDLING the seam (marker one's closing delimiter immediately
    # followed by marker two's opening delimiter). Two adjacent
    # BracketNote markers "〔［＃A〕" and "〔B〕" concatenate to
    # "〔［＃A〕〔B〕"; a pattern anchored on the seam ("A〕〔B") matches
    # the combination but neither individual raw string contains it.
    composite_patterns = [r"A〕〔B"]
    compiled2 = [__import__("re").compile(p) for p in composite_patterns]
    text = "〔［＃A〕〔B〕"
    markers = mod.scan_markers(text)
    assert len(markers) == 2
    assert markers[0].raw == "〔［＃A〕"
    assert markers[1].raw == "〔B〕"
    assert markers[0].end == markers[1].start
    assert not any(pattern.search(markers[0].raw) for pattern in compiled2)
    assert not any(pattern.search(markers[1].raw) for pattern in compiled2)
    combined_hits = mod.rust_matches(text, compiled2)
    assert combined_hits == [(1, "〔［＃A〕〔B〕", "composite")]


def test_matrix_mode_can_match_free_prose_that_rust_mode_never_tokenizes_as_a_marker():
    """Structural asymmetry between the two modes: matrix mode's
    `re.finditer` runs over the RAW TEXT directly and can match a
    bracket-less pattern (decoration.keigakomi's own pattern 4,
    `「[^\\n]+」[のは]罫囲み`) in free body prose that `source_markers`
    never tokenizes as a marker at all (prose is only ever a `Text`
    event, never passed to `matching_rows`). This is one of the two
    documented structural causes of matrix-mode/rust-mode disagreement
    (see the script's module docstring)."""
    patterns = [r"「[^\n]+」[のは]罫囲み"]
    alternation = mod.build_matrix_alternation(patterns)
    compiled = [__import__("re").compile(p) for p in patterns]

    text = "図版部分は「見出し」の罫囲みで示した。"
    matrix_hits = mod.matrix_matches(text, alternation)
    rust_hits = mod.rust_matches(text, compiled)

    assert len(matrix_hits) == 1
    assert rust_hits == []


def test_expected_works_universe_constant_matches_frozen_totals():
    assert mod.FROZEN_MATRIX_EXACT == 673
    assert mod.FROZEN_DENOMINATOR == 717
