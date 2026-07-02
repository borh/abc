#!/usr/bin/env python3
"""DISPOSABLE batch probe: run the AAT->parser-IR mapper over the full real
aozora-rs AAT corpus and aggregate divergence-ledger frequencies.

Imports the mapper's functions in-process (no subprocess, no file clobber).
Output: counts + a sample per-category + file-level breakdown."""
import json, sys, os, glob
from collections import Counter, defaultdict

# import the probe mapper as a module
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import map as mapper

AAT_DIR = "/home/bor/Projects/ab-validator/scratch/morph-full-corpus/aats/aozora-rs-adapter"

files = sorted(glob.glob(os.path.join(AAT_DIR, "*.json")))
print(f"scanning {len(files)} real AAT documents", file=sys.stderr)

cat_counter = Counter()              # category -> total entries
note_counter = Counter()             # (category, aat, parser_ir, note) -> count
files_with_unsupported = 0
files_with_any_ledger = 0
per_file_entry_counts = Counter()    # bucketed entry-count per file
unsupported_examples = []
sample_per_cat = defaultdict(list)
files_failed_to_parse = 0
total_nodes_emitted = 0
total_blocks_scanned = 0
files_warigaki = 0
inline_kind_counter = Counter()
block_kind_counter = Counter()

for i, path in enumerate(files):
    if i % 2000 == 0:
        print(f"  ...{i}/{len(files)}", file=sys.stderr)
    try:
        with open(path, encoding="utf-8") as f:
            aat = json.load(f)
    except Exception:
        files_failed_to_parse += 1
        continue

    # count real node kinds (sanity cross-check vs prior sample)
    for b in aat.get("blocks", []):
        total_blocks_scanned += 1
        block_kind_counter[b.get("kind")] += 1
        has_wg = False
        for n in b.get("content", []):
            inline_kind_counter[n.get("kind")] += 1
            if n.get("kind") == "warigaki":
                has_wg = True
        if has_wg:
            files_warigaki += 1

    # run the mapper
    ledger_list = []
    nodes = []
    offset = 0
    for j, block in enumerate(aat.get("blocks", [])):
        mapper.map_block(block, nodes, ledger_list, offset, f"blocks[{j}]")
    mapper.map_meta_source(aat, ledger_list)
    # the top-level INVENTION entries the main() adds:
    ledger_list.append(mapper.ledger("INVENTION", "(top-level)", "schema_id/schema_hash",
        "parser-IR requires schema_id+schema_hash; AAT supplies only version=1"))
    mapper.map_warnings(aat, ledger_list)
    ledger_list.append(mapper.ledger("INVENTION", "(none)", "errors[]",
        "parser-IR requires errors[]; AAT has no errors concept -> defaulted empty"))

    total_nodes_emitted += len(nodes)

    if ledger_list:
        files_with_any_ledger += 1
    per_file_entry_counts[len(ledger_list)] += 1

    had_unsupported = False
    for e in ledger_list:
        cat_counter[e["category"]] += 1
        key = (e["category"], e["aat"], e["parser_ir"], e["note"][:80])
        note_counter[key] += 1
        if e["category"] == "UNSUPPORTED":
            had_unsupported = True
            if len(unsupported_examples) < 10:
                unsupported_examples.append((path, e))
        if len(sample_per_cat[e["category"]]) < 3:
            sample_per_cat[e["category"]].append(e)
    if had_unsupported:
        files_with_unsupported += 1

print("", file=sys.stderr)
print("=== AGGREGATE DIVERGENCE LEDGER (full real corpus) ===")
print(f"files scanned: {len(files)}")
print(f"files failed to parse: {files_failed_to_parse}")
print(f"files with >=1 ledger entry: {files_with_any_ledger}")
print(f"files with UNSUPPORTED: {files_with_unsupported}")
print(f"files with warigaki: {files_warigaki}")
print(f"total blocks scanned: {total_blocks_scanned}")
print(f"total inline nodes scanned: {sum(inline_kind_counter.values())}")
print(f"total parser-IR nodes emitted: {total_nodes_emitted}")
print()
print("--- block kinds (real) ---")
for k, c in block_kind_counter.most_common():
    print(f"  {k}: {c}")
print("--- inline kinds (real) ---")
for k, c in inline_kind_counter.most_common():
    print(f"  {k}: {c}")
print()
print("--- DIVERGENCE CATEGORY TOTALS (corpus-scale) ---")
total = sum(cat_counter.values())
print(f"total ledger entries across corpus: {total}")
for c in ("LOSS", "AMBIGUITY", "INVENTION", "UNSUPPORTED", "STRUCTURAL"):
    print(f"  {c}: {cat_counter.get(c, 0)}  ({100*cat_counter.get(c,0)/max(total,1):.1f}%)")
print()
print("--- TOP DIVERGENCE RULES (by frequency) ---")
for (cat, aat, pir, note), c in note_counter.most_common(20):
    print(f"  [{c:>7}] {cat:12} | {aat[:40]:40} | {pir[:30]:30} | {note}")
print()
print("--- per-file entry-count distribution ---")
for cnt, files in sorted(per_file_entry_counts.items()):
    print(f"  {cnt} entries: {files} files")
print()
print("--- UNSUPPORTED examples ---")
for path, e in unsupported_examples:
    print(f"  {os.path.basename(path)}: {e}")
print()
print("--- sample entry per category ---")
for cat in ("LOSS", "AMBIGUITY", "INVENTION", "UNSUPPORTED", "STRUCTURAL"):
    for e in sample_per_cat.get(cat, []):
        print(f"  {cat}: {e}")
