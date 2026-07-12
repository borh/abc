#!/usr/bin/env python3
"""Independent parser-visible reconciliation for the Phase 5 bare-toggle
delta gate (Task 9 rebinding, plan amendment 3).

Two different universes exist for the bare-toggle corpus-bound
expectations:

  * SOURCE-TEXT universe (the placement report,
    ``2026-07-12-bare-toggle-placement-attribution.md``, Revision 3):
    every ``［＃横組み］``/``［＃横組み終わり］``/``［＃罫囲み］``/
    ``［＃罫囲み終わり］`` token found by scanning the DECODED SOURCE TEXT
    of all 17,886 works. Totals: adopted 1,582 yokogumi + 25 keigakomi
    pairs, 24 declined markers (10 orphan opens + 14 rollback markers).

  * PARSER-VISIBLE universe: the delta gate's actual universe is the C4
    AAT dump's raw marker nodes, not the source text. A rerun discovered
    70 source-text markers that never surface as standalone raw nodes in
    the dump, so the parser-visible totals are smaller: 1,552 yokogumi +
    25 keigakomi adopted pairs, 14 declined markers (all reopen-rollback).

This instrument independently re-derives the 70-marker gap and classifies
each missing marker WITHOUT trusting any prior classification:

  (a) counts every bare-toggle token in each work's decoded source text
      (reusing ``reports/lib/corpus_reader.py`` for corpus discovery and
      ``bare-toggle-placement.py``'s ``TOKEN_KIND``/``classify_tokens``
      for the token vocabulary and per-line adoption grammar);
  (b) counts standalone raw marker nodes in the matching C4 AAT dump file
      per work (a raw node whose ``source`` is EXACTLY one of the four
      tokens -- matched to its exact source file via ``meta.source_hash``,
      never via the (non-unique) ``work_id``);
  (c) for every source-text token occurrence with no corresponding
      standalone dump node on the same line, locates that occurrence
      inside the work's AAT tree and classifies it into exactly one of
      three mechanism classes:
        - ``GAP``    -- the token text is a substring of an oversized raw
                        node (``kind: "raw"`` but ``source`` is NOT one of
                        the four exact tokens) -- a merged
                        unparsed-source-gap node swallowing multiple
                        original lines including the marker;
        - ``LEGEND`` -- the token is not present anywhere in the dump tree
                        (neither as a standalone node, nor embedded in any
                        other node's text/source) AND its source line
                        falls BEFORE the file's tail-start line (see
                        ``reports/lib/terminal_provenance.py``
                        ``find_tail_start`` -- the first line starting
                        with ``底本：``), i.e. it is inside the
                        front-matter notation-legend block
                        (``【テキスト中に現れる記号について】``);
        - ``TAIL``   -- not present anywhere in the dump tree, AND its
                        source line falls AT/AFTER the tail-start line --
                        i.e. inside the post-底本 tail. (Independent
                        finding: in every sampled case, these markers lie
                        specifically in the ``colophon_metadata``
                        sub-state of ``reports/lib/terminal_provenance.py``'s
                        state machine -- triggered by a ``※``/入力：/校正：
                        head line -- which Phase 4's ``source_note``
                        emission does not model as a node at all, unlike
                        the ``terminal_provenance`` sub-state it does
                        capture. This refines, but does not change the
                        count of, the "inside Phase 4's source_note
                        region" characterization.)

Each missing occurrence is also tagged with its SOURCE-TEXT verdict
(adopted vs. declined), from the same per-line ``classify_tokens`` used
by the placement instrument -- a token's construct is declined for that
line iff the construct is in ``outcome.invalid_constructs``.

Fail-closed: an occurrence that cannot be classified into GAP/LEGEND/TAIL
(i.e. embedded in an ordinary ``text`` node value, outside a source_note
context, and not inside an oversized raw node) is an ANOMALY -- the
script reports it distinctly rather than silently bucketing it, so a
disagreement with the prior 44/12/14 analysis surfaces instead of being
absorbed.

Usage:
    bare-toggle-visibility-reconciliation.py --corpus <store-path>
        --dump <C4 dump ab-aozora dir> [--out summary.json] [--jobs N]
"""

from __future__ import annotations

import argparse
import collections
import concurrent.futures
import importlib.util
import json
import pathlib
import sys
from typing import Any

_REPO_ROOT = pathlib.Path(__file__).resolve().parents[2]
if str(_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(_REPO_ROOT))

from reports.lib.corpus_reader import (  # noqa: E402
    classify_candidate,
    decode_source,
    discover_entries,
    sha256_hex,
)
import reports.lib.terminal_provenance as terminal_provenance  # noqa: E402

_AAT_FIDELITY = pathlib.Path(__file__).resolve().parent
_placement_spec = importlib.util.spec_from_file_location(
    "bare_toggle_placement", _AAT_FIDELITY / "bare-toggle-placement.py"
)
placement = importlib.util.module_from_spec(_placement_spec)
sys.modules[_placement_spec.name] = placement
_placement_spec.loader.exec_module(placement)

LINE_SPLIT = placement.LINE_SPLIT
TOKEN_KIND = placement.TOKEN_KIND
CONSTRUCTS = placement.CONSTRUCTS
_TOKEN_ALTERNATION = placement._TOKEN_ALTERNATION
classify_tokens = placement.classify_tokens

SCHEMA_VERSION = "bare-toggle-visibility-reconciliation-v1"


def build_dump_index(dump_dir: pathlib.Path) -> dict[str, list[pathlib.Path]]:
    """work_id -> [dump json paths]. work_id is the filename prefix before
    the LAST '-' (work_ids themselves contain '_', never '-')."""
    index: dict[str, list[pathlib.Path]] = collections.defaultdict(list)
    for path in dump_dir.glob("*.json"):
        stem = path.stem  # "<work_id>-<hash8>"
        work_id, _, _hash = stem.rpartition("-")
        if not work_id:
            continue
        index[work_id].append(path)
    return index


def find_dump_doc(
    dump_index: dict[str, list[pathlib.Path]], work_id: str, source_sha256_hex: str
) -> tuple[dict[str, Any] | None, str | None]:
    """Return (doc, error). Disambiguates duplicate work_ids (ruby / plain
    variants of the same card) by matching meta.source_hash to the exact
    candidate's own sha256 -- never by filename order.

    Two corpus zip entries occasionally carry byte-identical source text
    under the same work_id (a true content duplicate, e.g.
    ``000311_46240``'s two ``_ruby_*.zip`` cards): both dump files then
    also match the requested source_hash AND are byte-identical to each
    other (deterministic parse of identical input), so any one of them is
    the correct match. Only a genuine AMBIGUITY -- multiple matches that
    are not all identical -- is an error.
    """
    candidates = dump_index.get(work_id, [])
    if not candidates:
        return None, f"no dump file for work_id {work_id!r}"
    want = f"sha256:{source_sha256_hex}"
    matched = []
    for path in sorted(candidates):
        try:
            doc = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError) as err:
            return None, f"{path}: unreadable ({err})"
        if (doc.get("meta") or {}).get("source_hash") == want:
            matched.append(doc)
    if not matched:
        return None, (
            f"work_id {work_id!r}: none of {len(candidates)} dump file(s) "
            f"carry source_hash {want}"
        )
    if any(doc != matched[0] for doc in matched[1:]):
        return None, (
            f"work_id {work_id!r}: {len(matched)} dump files carry source_hash "
            f"{want} and are NOT byte-identical -- genuine ambiguity"
        )
    return matched[0], None


def tokenize_line_with_matches(line: str) -> list[tuple[str, str, str]]:
    """[(construct, kind, exact_token_text), ...] in document order."""
    out = []
    for match in _TOKEN_ALTERNATION.finditer(line):
        token = match.group(0)
        construct, kind = TOKEN_KIND[token]
        out.append((construct, kind, token))
    return out


def source_line_records(text: str) -> list[dict[str, Any]]:
    """Per (1-indexed) line with a marker: token list + per-line grammar
    verdict (declined iff the token's construct is invalid on that line)."""
    records = []
    for idx, line in enumerate(LINE_SPLIT.split(text), start=1):
        toks = tokenize_line_with_matches(line)
        if not toks:
            continue
        outcome = classify_tokens([(c, k) for c, k, _ in toks])
        for construct, kind, token in toks:
            records.append(
                {
                    "line": idx,
                    "construct": construct,
                    "kind": kind,
                    "token": token,
                    "declined": construct in outcome.invalid_constructs,
                }
            )
    return records


def walk_dump(doc: dict[str, Any]):
    """Yield (node_dict, ancestor_block) for every dict node in the tree.
    ancestor_block is the nearest enclosing dict carrying a top-level
    'kind' that is a block kind (paragraph/heading/source_note/...), used
    to test source_note ancestry for TAIL classification."""

    def _walk(node, ancestor_block):
        if isinstance(node, dict):
            next_ancestor = ancestor_block
            if "kind" in node and ("content" in node or "children" in node or "value" in node or "source" in node):
                # Blocks and inline nodes alike carry 'kind'; only update
                # the "nearest block" ancestor when this dict looks like a
                # structural block (has region_class/placement or is a
                # recognizable block-shaped container). We conservatively
                # treat any dict with 'kind' == 'source_note' as the block
                # of interest, and otherwise keep propagating the current
                # ancestor down through nested content.
                if node.get("kind") == "source_note" or node.get("region_class") is not None:
                    next_ancestor = node
            yield node, ancestor_block
            for key in ("blocks", "content", "children"):
                val = node.get(key)
                if isinstance(val, list):
                    for item in val:
                        yield from _walk(item, next_ancestor)
        elif isinstance(node, list):
            for item in node:
                yield from _walk(item, ancestor_block)

    yield from _walk(doc, None)


def dump_standalone_counts(doc: dict[str, Any]) -> dict[int, collections.Counter]:
    """{line_start: Counter(exact_token -> count)} over standalone raw
    marker nodes (source == exact token)."""
    per_line: dict[int, collections.Counter] = collections.defaultdict(collections.Counter)
    for node, _ in walk_dump(doc):
        if node.get("kind") != "raw":
            continue
        source = node.get("source")
        if source not in TOKEN_KIND:
            continue
        span = node.get("span") or {}
        line = span.get("line_start")
        per_line[line][source] += 1
    return per_line


def oversized_raw_sources(doc: dict[str, Any]) -> list[str]:
    """source strings of raw nodes that are NOT an exact bare-toggle
    token but may embed one as a substring (merged unparsed-gap nodes)."""
    out = []
    for node, _ in walk_dump(doc):
        if node.get("kind") != "raw":
            continue
        source = node.get("source")
        if isinstance(source, str) and source not in TOKEN_KIND:
            out.append(source)
    return out


def source_note_text_blob(doc: dict[str, Any]) -> str:
    """Concatenation of every text-kind node's value found anywhere
    beneath a source_note-classified ancestor block."""
    parts = []
    for node, ancestor in walk_dump(doc):
        if node.get("kind") != "text":
            continue
        if ancestor is not None and (
            ancestor.get("kind") == "source_note"
            or ancestor.get("region_class") == "terminal_provenance"
        ):
            value = node.get("value")
            if isinstance(value, str):
                parts.append(value)
    return "\n".join(parts)


def other_text_blob(doc: dict[str, Any]) -> str:
    """Concatenation of text-kind node values NOT under a source_note
    ancestor -- used only to detect an unexpected 4th class."""
    parts = []
    for node, ancestor in walk_dump(doc):
        if node.get("kind") != "text":
            continue
        if not (
            ancestor is not None
            and (
                ancestor.get("kind") == "source_note"
                or ancestor.get("region_class") == "terminal_provenance"
            )
        ):
            value = node.get("value")
            if isinstance(value, str):
                parts.append(value)
    return "\n".join(parts)


def reconcile_work(
    work_id: str,
    label: str,
    text: str,
    doc: dict[str, Any],
) -> dict[str, Any] | None:
    """Per-line diff between source-text tokens and dump-standalone
    tokens; classify every excess (missing) occurrence. Returns None if
    the work is fully visible (no missing markers).

    Classification is a two-stage budget consumption, not a per-line
    substring check: a merged unparsed-gap raw node's byte range need not
    align to original line boundaries (it can start/end mid-line), so
    "is this whole source line a substring of the gap node's source" is
    too strict and under-matches. Instead, for each token string, count
    how many times it appears embedded (as a substring, not a standalone
    exact-match node) inside (a) oversized raw nodes (GAP candidates) and
    (b) source_note-ancestored text values (TAIL candidates) for the
    WHOLE work; then consume those budgets, in line order, against the
    per-line missing counts. Whatever remains unconsumed is classified by
    SOURCE-TEXT POSITION relative to the file's tail-start line (`底本：`)
    -- LEGEND if strictly before, TAIL if at/after -- which needs no
    dump-tree evidence at all.
    """
    lines = LINE_SPLIT.split(text)
    tail_start = terminal_provenance.find_tail_start(lines)

    src_records = source_line_records(text)
    if not src_records:
        return None
    src_by_line: dict[int, list[dict[str, Any]]] = collections.defaultdict(list)
    for rec in src_records:
        src_by_line[rec["line"]].append(rec)

    dump_by_line = dump_standalone_counts(doc)
    gap_sources = oversized_raw_sources(doc)
    tail_blob = source_note_text_blob(doc)
    other_blob = other_text_blob(doc)

    gap_budget: collections.Counter = collections.Counter()
    for src in gap_sources:
        for token in TOKEN_KIND:
            gap_budget[token] += src.count(token)
    tail_budget: collections.Counter = collections.Counter(
        {token: tail_blob.count(token) for token in TOKEN_KIND}
    )
    other_budget: collections.Counter = collections.Counter(
        {token: other_blob.count(token) for token in TOKEN_KIND}
    )

    missing_events = []
    for line in sorted(src_by_line):
        recs = src_by_line[line]
        src_counts = collections.Counter(r["token"] for r in recs)
        dump_counts = dump_by_line.get(line, collections.Counter())
        for token, scount in src_counts.items():
            dcount = dump_counts.get(token, 0)
            missing = scount - dcount
            if missing <= 0:
                continue
            declined = next(r["declined"] for r in recs if r["token"] == token)
            remaining = missing
            for budget, cls in ((gap_budget, "GAP"), (tail_budget, "TAIL"), (other_budget, "ANOMALY_OTHER_TEXT")):
                take = min(remaining, budget.get(token, 0))
                if take:
                    budget[token] -= take
                    remaining -= take
                    missing_events.append(
                        {
                            "line": line,
                            "token": token,
                            "construct": TOKEN_KIND[token][0],
                            "kind": TOKEN_KIND[token][1],
                            "count": take,
                            "class": cls,
                            "declined_in_source_grammar": declined,
                        }
                    )
                if not remaining:
                    break
            if remaining:
                cls = "LEGEND" if (tail_start is None or (line - 1) < tail_start) else "TAIL"
                missing_events.append(
                    {
                        "line": line,
                        "token": token,
                        "construct": TOKEN_KIND[token][0],
                        "kind": TOKEN_KIND[token][1],
                        "count": remaining,
                        "class": cls,
                        "declined_in_source_grammar": declined,
                    }
                )
    if not missing_events:
        return None
    return {
        "work_id": work_id,
        "label": label,
        "tail_start_line": (tail_start + 1) if tail_start is not None else None,
        "missing": missing_events,
        "missing_total": sum(e["count"] for e in missing_events),
    }


def _grammar_from_line_token_groups(
    line_token_groups: dict[int, list[tuple[str, str]]],
) -> dict[str, Any]:
    """Run classify_tokens per line over an arbitrary (line -> ordered
    token list) mapping and total the results -- the same reduction
    bare-toggle-placement.py's scan_text applies to source-text lines,
    and audit-aat-delta.py's derive_expected applies to dump-derived
    lines. Used here for BOTH sides so the two totals are computed by
    one shared reduction, isolating any disagreement to the token lists
    themselves.
    """
    totals = placement._empty_grammar_totals()
    for tokens in line_token_groups.values():
        if not tokens:
            continue
        outcome = classify_tokens(tokens)
        totals["lines_with_markers"] += 1
        totals["total_markers"] += outcome.total_markers
        if outcome.invalid_constructs:
            totals["invalid_lines"] += 1
        for construct in CONSTRUCTS:
            totals["adopted_pairs"][construct] += outcome.adopted_pairs[construct]
            totals["orphan_open"][construct] += outcome.orphan_open[construct]
            totals["orphan_close"][construct] += outcome.orphan_close[construct]
            totals["reopen"][construct] += outcome.reopen[construct]
        totals["interleave_events"] += outcome.interleave_events
        totals["proper_nestings"] += outcome.proper_nestings
        totals["rollback_markers"] += outcome.rollback_markers
    return totals


def process_candidate(corpus_root_str: str, entry_str: str, dump_dir_str: str) -> dict[str, Any]:
    corpus_root = pathlib.Path(corpus_root_str)
    dump_dir = pathlib.Path(dump_dir_str)
    candidate = classify_candidate(corpus_root, pathlib.Path(entry_str))
    record: dict[str, Any] = {"class": candidate.klass, "rel": candidate.rel}
    if candidate.klass != "work" or candidate.data is None:
        return record
    text = decode_source(candidate.data)
    if not any(token in text for token in TOKEN_KIND):
        record["touched"] = False
        return record
    record["touched"] = True
    work_id = candidate.work_id
    record["work_id"] = work_id
    record["label"] = candidate.label

    src_counts: collections.Counter = collections.Counter(
        m.group(0) for m in _TOKEN_ALTERNATION.finditer(text)
    )
    record["source_token_counts"] = dict(src_counts)

    # Source-text grammar totals, per line -- the placement instrument's
    # own reduction (cross-checked against the frozen placement report's
    # 1,582 / 25 / 24 corpus totals in main()).
    src_line_tokens: dict[int, list[tuple[str, str]]] = {}
    for idx, line in enumerate(LINE_SPLIT.split(text), start=1):
        toks = placement.tokenize_line(line)
        if toks:
            src_line_tokens[idx] = toks
    record["source_grammar_totals"] = _grammar_from_line_token_groups(src_line_tokens)

    dump_index = build_dump_index(dump_dir)
    sha = sha256_hex(candidate.data)
    doc, error = find_dump_doc(dump_index, work_id, sha)
    if doc is None:
        record["error"] = error
        return record

    standalone = dump_standalone_counts(doc)
    dump_totals: collections.Counter = collections.Counter()
    for counts in standalone.values():
        dump_totals.update(counts)
    record["dump_token_counts"] = dict(dump_totals)

    # Dump-derived grammar totals: reconstruct each line's VISIBLE token
    # sequence from standalone raw nodes only (ordered by byte_start
    # within the line), then run the identical per-line classify_tokens
    # reduction -- independently mirroring (never importing)
    # audit-aat-delta.py's derive_expected.
    dump_line_tokens: dict[int, list[tuple[str, str]]] = {}
    for node, _ in walk_dump(doc):
        if node.get("kind") != "raw":
            continue
        source = node.get("source")
        if source not in TOKEN_KIND:
            continue
        span = node.get("span") or {}
        line = span.get("line_start")
        byte_start = span.get("byte_start", 0)
        dump_line_tokens.setdefault(line, []).append((byte_start, TOKEN_KIND[source]))
    dump_line_token_groups = {
        line: [tok for _, tok in sorted(entries, key=lambda e: e[0])]
        for line, entries in dump_line_tokens.items()
    }
    record["dump_grammar_totals"] = _grammar_from_line_token_groups(dump_line_token_groups)

    reconciliation = reconcile_work(work_id, candidate.label, text, doc)
    if reconciliation:
        record["reconciliation"] = reconciliation
    return record


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--corpus", required=True, type=pathlib.Path)
    ap.add_argument("--dump", required=True, type=pathlib.Path, help="C4 dump ab-aozora directory")
    ap.add_argument("--expected-works", type=int, default=17886)
    ap.add_argument("--out", type=pathlib.Path)
    ap.add_argument("--jobs", type=int, default=1)
    args = ap.parse_args()

    # Build the dump index once up front purely to fail fast/loud if the
    # dump dir is empty or misconfigured; process_candidate rebuilds it
    # per-process under --jobs>1 (cheap: only globs filenames, no parsing).
    if not any(args.dump.glob("*.json")):
        print(f"FAIL: no dump json files found under {args.dump}", file=sys.stderr)
        return 2

    candidates = discover_entries(args.corpus)
    if args.jobs <= 1:
        records = [
            process_candidate(str(args.corpus), str(entry), str(args.dump))
            for entry in candidates
        ]
    else:
        with concurrent.futures.ProcessPoolExecutor(max_workers=args.jobs) as pool:
            futures = [
                pool.submit(process_candidate, str(args.corpus), str(entry), str(args.dump))
                for entry in candidates
            ]
            records = [future.result() for future in futures]

    works = [r for r in records if r.get("class") == "work"]
    if len(works) != args.expected_works:
        print(
            f"FAIL: {len(works)} readable works, expected {args.expected_works}",
            file=sys.stderr,
        )
        return 2

    touched = [r for r in works if r.get("touched")]
    errors = [r for r in touched if r.get("error")]

    source_totals: collections.Counter = collections.Counter()
    dump_totals: collections.Counter = collections.Counter()
    for r in touched:
        source_totals.update(r.get("source_token_counts") or {})
        dump_totals.update(r.get("dump_token_counts") or {})

    # Corpus-wide grammar totals, summed from each work's own per-line
    # reduction (see process_candidate) -- source-text side reproduces the
    # placement report's binding 1,582/25/24; dump-derived side reproduces
    # the checkpoint's parser-visible 1,552/25/14.
    def _sum_grammar(key: str) -> dict[str, Any]:
        totals = placement._empty_grammar_totals()
        for r in touched:
            g = r.get(key)
            if not g:
                continue
            for construct in CONSTRUCTS:
                for field in ("adopted_pairs", "orphan_open", "orphan_close", "reopen"):
                    totals[field][construct] += g[field][construct]
            for field in (
                "interleave_events", "proper_nestings", "rollback_markers",
                "lines_with_markers", "invalid_lines", "total_markers",
            ):
                totals[field] += g[field]
        return totals

    src_grammar_totals = _sum_grammar("source_grammar_totals")
    dump_grammar_totals = _sum_grammar("dump_grammar_totals")

    def _declined_by_reason(g: dict[str, Any]) -> dict[str, int]:
        return {
            "orphan_open": sum(g["orphan_open"].values()),
            "orphan_close": sum(g["orphan_close"].values()),
            "reopen_rollback": g["rollback_markers"],
            "interleave": g["interleave_events"],
        }

    source_binding = {
        "adopted_yokogumi_pairs": src_grammar_totals["adopted_pairs"]["yokogumi"],
        "adopted_keigakomi_pairs": src_grammar_totals["adopted_pairs"]["keigakomi"],
        "declined_markers": sum(_declined_by_reason(src_grammar_totals).values()),
        "declined_by_reason": _declined_by_reason(src_grammar_totals),
    }
    dump_binding = {
        "adopted_yokogumi_pairs": dump_grammar_totals["adopted_pairs"]["yokogumi"],
        "adopted_keigakomi_pairs": dump_grammar_totals["adopted_pairs"]["keigakomi"],
        "declined_markers": sum(_declined_by_reason(dump_grammar_totals).values()),
        "declined_by_reason": _declined_by_reason(dump_grammar_totals),
    }

    reconciled = [r for r in touched if r.get("reconciliation")]
    missing_by_class: collections.Counter = collections.Counter()
    missing_adopted_by_class: collections.Counter = collections.Counter()
    missing_declined_by_class: collections.Counter = collections.Counter()
    per_work_summaries = []
    anomalies = []
    for r in reconciled:
        rec = r["reconciliation"]
        per_work_summaries.append(
            {
                "work_id": rec["work_id"],
                "label": rec["label"],
                "missing_total": rec["missing_total"],
                "events": rec["missing"],
            }
        )
        for e in rec["missing"]:
            missing_by_class[e["class"]] += e["count"]
            if e["declined_in_source_grammar"]:
                missing_declined_by_class[e["class"]] += e["count"]
            else:
                missing_adopted_by_class[e["class"]] += e["count"]
            if e["class"] == "ANOMALY_OTHER_TEXT":
                anomalies.append({"work_id": rec["work_id"], "event": e})

    missing_total = sum(missing_by_class.values())
    missing_adopted_total = sum(missing_adopted_by_class.values())
    missing_declined_total = sum(missing_declined_by_class.values())

    adopted_pairs_delta = {
        "yokogumi": source_binding["adopted_yokogumi_pairs"] - dump_binding["adopted_yokogumi_pairs"],
        "keigakomi": source_binding["adopted_keigakomi_pairs"] - dump_binding["adopted_keigakomi_pairs"],
    }
    declined_delta = source_binding["declined_markers"] - dump_binding["declined_markers"]

    summary = {
        "schema_version": SCHEMA_VERSION,
        "corpus": str(args.corpus),
        "dump": str(args.dump),
        "works_scanned": len(works),
        "works_touched": len(touched),
        "dump_match_errors": errors,
        "source_token_totals": dict(source_totals),
        "dump_standalone_token_totals": dict(dump_totals),
        "source_text_universe": source_binding,
        "parser_visible_universe": dump_binding,
        "arithmetic_check": {
            "adopted_pairs_delta_by_construct": adopted_pairs_delta,
            "adopted_pairs_delta_total": sum(adopted_pairs_delta.values()),
            "declined_markers_delta": declined_delta,
            "missing_adopted_pair_markers_total": missing_adopted_total,
            "missing_adopted_pairs_total_from_classification": missing_adopted_total // 2,
            "missing_declined_markers_total_from_classification": missing_declined_total,
            "closes": (
                sum(adopted_pairs_delta.values()) == missing_adopted_total // 2
                and declined_delta == missing_declined_total
            ),
        },
        "missing_markers_total": missing_total,
        "missing_by_class": dict(missing_by_class),
        "missing_adopted_pair_markers_by_class": dict(missing_adopted_by_class),
        "missing_declined_markers_by_class": dict(missing_declined_by_class),
        "missing_adopted_pair_markers_total": missing_adopted_total,
        "missing_declined_markers_total": missing_declined_total,
        "missing_adopted_pairs_total": missing_adopted_total // 2,
        "anomalies": anomalies,
        "per_work": per_work_summaries,
    }

    payload = json.dumps(summary, ensure_ascii=False, indent=1) + "\n"
    if args.out:
        args.out.write_text(payload, encoding="utf-8")
    else:
        sys.stdout.write(payload)

    if errors:
        print(f"FAIL: {len(errors)} dump-match errors, see summary", file=sys.stderr)
        return 2
    if anomalies:
        print(
            f"FAIL: {len(anomalies)} marker(s) classified as ANOMALY_OTHER_TEXT "
            "(disagreement with the LEGEND/TAIL/GAP taxonomy) -- STOP and investigate",
            file=sys.stderr,
        )
        return 2
    if not summary["arithmetic_check"]["closes"]:
        print(
            "FAIL: arithmetic does not close -- "
            f"adopted_pairs_delta={summary['arithmetic_check']['adopted_pairs_delta_total']} "
            f"vs classified {summary['arithmetic_check']['missing_adopted_pairs_total_from_classification']}, "
            f"declined_delta={summary['arithmetic_check']['declined_markers_delta']} "
            f"vs classified {summary['arithmetic_check']['missing_declined_markers_total_from_classification']}",
            file=sys.stderr,
        )
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
