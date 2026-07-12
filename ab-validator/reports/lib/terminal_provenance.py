"""Stateful classifier for the terminal-provenance / colophon-metadata tail.

ABC policy v0.2.0 admits `terminal_provenance` and `colophon_metadata` as
distinct source-region classes but flags them `needs_measurement_split`
(see `reports/lib/source_region.py`). This module defines the NORMATIVE
boundary rule between them; Task 14's Rust `source_note` emission
transcribes this rule case-for-case, so it must not drift from what is
written here.

**The tail.** Every Aozora Bunko work ends (after the visible body) with a
"tail": editorial apparatus that starts at the first line whose content,
after stripping leading whitespace, starts with `底本：`. This mirrors
`aozora_body_range` in `crates/ab-aozora-aat/src/lib.rs`, which uses the
exact same `line.trim_start().starts_with("底本：")` check to end the
parser's body span. A work with no such line has no tail at all.

**Why a state machine and not a per-line predicate.** Inside the tail,
some lines are self-describing head lines (`底本：...`, `入力：...`, ...)
but many are bare continuation lines -- an edition/date line, a plain
name, a blank line -- that carry no marker of their own. The SAME
line shape means different things depending on what came before it:

    底本：「日本文学全集1　坪内逍遥・二葉亭四迷集」集英社
    1969（昭和44）年12月25日初版      <- inherits: terminal_provenance
    入力：j.utiyama
    校正：八巻美恵
    1998年7月28日公開                        <- inherits: colophon_metadata
    2006年1月6日修正

(this is a real tail, `cards/000005/files/5_ruby_21311.zip::aibiki.txt` in
the pinned aozorabunko corpus.) `1998年7月28日公開` and
`1969（昭和44）年12月25日初版` are shaped identically -- both are bare
date lines -- but the first is colophon metadata (it follows 入力：/校正：)
and the second is terminal provenance (it follows 底本：). No per-line
predicate can tell them apart; only the state carried from the most
recent head line can.

**Fail-closed.** A non-blank tail line reached before ANY state-setting
head line has been seen raises `UnclassifiableTail` -- there is no state
to inherit from, and guessing would silently mis-attribute an unknown
editorial convention. Callers (the generator) must treat this, and any
non-empty residual of unclassifiable lines from a full-corpus scan, as a
fail-closed error (exit 2), never a warning.
"""

from __future__ import annotations

State = str  # "provenance" | "colophon"
Class = str  # "terminal_provenance" | "colophon_metadata" | "blank"

PROVENANCE_STATE: State = "provenance"
COLOPHON_STATE: State = "colophon"

TERMINAL_PROVENANCE_CLASS: Class = "terminal_provenance"
COLOPHON_METADATA_CLASS: Class = "colophon_metadata"
BLANK_CLASS: Class = "blank"

# The tail-start marker: mirrors `aozora_body_range`'s
# `line.trim_start().starts_with("底本：")` check in
# crates/ab-aozora-aat/src/lib.rs. This is NOT a head in the state-machine
# sense below (it always also matches the first PROVENANCE_HEADS entry);
# it is what defines where the tail begins in the first place.
TAIL_START_MARKER = "底本："

# Ordered head-marker skeleton -- extended ONLY by corpus-scan residuals.
# Every extension beyond this skeleton is documented (with examples and
# work IDs) in
# docs/superpowers/reports/2026-07-12-terminal-provenance-colophon-split.md.
#
# A head is recognized by prefix match against the line with leading/
# trailing whitespace stripped (`line.strip()`), same as `classify_tail`
# below.
PROVENANCE_HEADS: tuple[str, ...] = (
    "底本：",
    "底本の親本：",
)
COLOPHON_HEADS: tuple[str, ...] = (
    "入力：",
    "校正：",
    "青空文庫作成ファイル：",
    "※",
)


class UnclassifiableTail(Exception):
    """A tail line could not be classified.

    Raised by `classify_tail` when a non-blank line is reached before any
    state-setting head line. Carries the offending line and its index
    within the list passed to `classify_tail` (i.e. relative to the start
    of the tail, not the start of the file) so callers can report it.
    """

    def __init__(self, line: str, index: int) -> None:
        self.line = line
        self.index = index
        super().__init__(f"unclassifiable tail line at tail-relative index {index}: {line!r}")


def is_provenance_head(stripped: str) -> bool:
    """Return whether a (whitespace-stripped) line opens a provenance block."""
    return any(stripped.startswith(marker) for marker in PROVENANCE_HEADS)


def is_colophon_head(stripped: str) -> bool:
    """Return whether a (whitespace-stripped) line opens a colophon block."""
    return any(stripped.startswith(marker) for marker in COLOPHON_HEADS)


def classify_tail(lines: list[str]) -> list[Class]:
    """Classify every line of a tail (see module docstring for what "tail"
    means and why this must be stateful).

    Fail-closed: raises `UnclassifiableTail` (carrying the line and its
    index) if a non-blank line arrives before any state-setting head line.
    """
    state: State | None = None
    out: list[Class] = []
    for index, line in enumerate(lines):
        stripped = line.strip()
        if not stripped:
            out.append(BLANK_CLASS)  # blank: class blank, state unchanged
            continue
        if is_provenance_head(stripped):
            state = PROVENANCE_STATE
            out.append(TERMINAL_PROVENANCE_CLASS)
        elif is_colophon_head(stripped):
            state = COLOPHON_STATE
            out.append(COLOPHON_METADATA_CLASS)
        elif state == PROVENANCE_STATE:
            out.append(TERMINAL_PROVENANCE_CLASS)  # continuation inherits
        elif state == COLOPHON_STATE:
            out.append(COLOPHON_METADATA_CLASS)  # continuation inherits
        else:
            raise UnclassifiableTail(line, index)
    return out


def find_tail_start(lines: list[str]) -> int | None:
    """Return the index of the first line in `lines` whose content, after
    stripping ONLY leading whitespace (`str.lstrip`, matching the Rust
    `trim_start` check), starts with `底本：`. This is the SAME boundary
    `aozora_body_range` uses to end the parser's body span, so the tail
    returned by `lines[start:]` agrees with what Task 14's parser
    transcription treats as body-end.

    Returns `None` when no such line exists in `lines` (no tail).
    """
    for index, line in enumerate(lines):
        if line.lstrip().startswith(TAIL_START_MARKER):
            return index
    return None


# The normative rule, embedded as data so the report (and Task 14's Rust
# transcription) can render/compare it directly rather than re-deriving it
# from prose.
BOUNDARY_RULE: dict[str, object] = {
    "tail_start_marker": TAIL_START_MARKER,
    "tail_start_rule": (
        "the tail is every line from the first line whose content, after "
        "stripping leading whitespace, starts with 底本： -- mirrors "
        'aozora_body_range\'s `line.trim_start().starts_with("底本：")` '
        "check in crates/ab-aozora-aat/src/lib.rs"
    ),
    "provenance_heads": list(PROVENANCE_HEADS),
    "colophon_heads": list(COLOPHON_HEADS),
    "state_machine": {
        "states": [PROVENANCE_STATE, COLOPHON_STATE],
        "transitions": [
            {
                "when": "line (stripped) starts with a provenance_heads entry",
                "set_state": PROVENANCE_STATE,
                "class": TERMINAL_PROVENANCE_CLASS,
            },
            {
                "when": "line (stripped) starts with a colophon_heads entry",
                "set_state": COLOPHON_STATE,
                "class": COLOPHON_METADATA_CLASS,
            },
            {
                "when": "line is blank",
                "set_state": None,
                "class": BLANK_CLASS,
                "note": "state is left unchanged, not cleared",
            },
            {
                "when": "line is non-blank, matches no head, and state is set",
                "set_state": None,
                "class": "inherit_from_state",
                "note": (
                    "class is terminal_provenance when state is provenance, "
                    "colophon_metadata when state is colophon"
                ),
            },
            {
                "when": "line is non-blank, matches no head, and state is unset",
                "set_state": None,
                "class": None,
                "note": "fail-closed: raises UnclassifiableTail",
            },
        ],
    },
}
