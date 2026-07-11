# Terminal-Provenance / Colophon-Metadata Measurement Split

**Binding numbers below are Revision 2 (2026-07-12), the corrected/current
values. See "Revision 2" at the end of this file for the correction
narrative; the original Revision 1 numbers remain in git history
(`git log -p` on this file) and are NOT reproduced in prose here to avoid
two competing "current" figures in the same document.**

Verdict: `TERMINAL_PROVENANCE_SPLIT_OK`

- corpus_root: `/nix/store/sdr1imwrxfldvlwzs2d2fhs11vxncgpx-aozorabunko-corpus`
- works_scanned: 17886
- works_with_terminal_provenance: 17735
- works_with_colophon: 17734
- works_without_tail: 151
- works_unclassifiable: 0
- terminal_provenance_lines: 84461
- colophon_lines: 106363
- head_line_hits: 91691
- unreadable_entries: 9

## Boundary Rule

- tail_start_marker: `底本：`
- tail_start_rule: the tail is every line from the first line whose content, after stripping leading whitespace, starts with 底本： -- mirrors aozora_body_range's `line.trim_start().starts_with("底本：")` check in crates/ab-aozora-aat/src/lib.rs
- provenance_heads: 底本：, 底本の親本：
- colophon_heads: 入力：, 校正：, 青空文庫作成ファイル：, ※

## Rule Extensions

- extensions beyond the skeleton: 0
- empty: the brief's head-marker skeleton, unmodified, classified every tail line in this run with zero unclassifiable residual (works_unclassifiable == 0) -- no extension beyond 底本：/底本の親本： (provenance) and 入力：/校正：/青空文庫作成ファイル：/※ (colophon) was needed. Lines matching other colophon-shaped prefixes the reference scanner also recognizes (親本：/初出：/校閲：/作成日：/修正：/ファイル作成：) are still classified correctly: they inherit whatever state the block already carries rather than needing their own head entry, since they never open a NEW block by themselves in the corpus.

## Reference Coverage Cross-Check

- reference date: 2026-07-06
- reference terminal_provenance_occurrences: 609
- reference colophon_occurrences: 89416
- reference mechanism: terminal_provenance_occurrences counts SegmentBoundaryTerminalProvenance markers (crates/ab-source-syntax/src/lib.rs terminal_provenance_note_end): a ［＃地付き］（…） note immediately followed by a 底本： line -- a narrow structural co-occurrence, not 'has a 底本 block'. colophon_metadata_occurrences counts LINES matching a fixed ~20-prefix set including 底本：/底本の親本：/親本：/初出： AND 入力：/校正：/校閲：/作成日：/修正：/ファイル作成：/青空文庫作成ファイル： together in ONE undifferentiated bucket (is_colophon_metadata_line in crates/ab-coverage/src/bin/source_inventory.rs) -- this conflation is exactly what ABC policy v0.2.0 flags needs_measurement_split and what this instrument splits. It is also a pure per-line prefix match with no state: it never counts a bare continuation line (an edition/date line, a plain name).
- this run's unit: works_scanned/works_with_terminal_provenance/works_with_colophon count corpus TEXT ENTRIES (see works_scanned note above); terminal_provenance_lines/colophon_lines count ALL tail lines under the stateful rule (heads + inherited continuations); head_line_hits counts only the head-line subset (lines literally matching a provenance_heads/colophon_heads prefix) -- the comparable-in-kind figure to the reference's per-line prefix scan, modulo the head-list and prefix-set differences above.

## Provenance Samples

| work_id | label | terminal_provenance_lines |
|---|---|---|
| 000005_53194 | `cards/000005/files/53194_ruby_44732.zip::hatsukoi.txt` | 4 |
| 000005_55215 | `cards/000005/files/55215_ruby_49076.zip::roshiano_kotoba.txt` | 5 |
| 000005_55216 | `cards/000005/files/55216_ruby_49074.zip::asuwa_asuwa.txt` | 5 |
| 000005_55217 | `cards/000005/files/55217_ruby_49075.zip::isso.txt` | 5 |
| 000005_5 | `cards/000005/files/5_ruby_21311.zip::aibiki.txt` | 2 |
| 000006_1868 | `cards/000006/files/1868_ruby_22436.zip::shosetsu_soron.txt` | 4 |
| 000006_1869 | `cards/000006/files/1869_ruby_33306.zip::ukigumo.txt` | 5 |
| 000006_3310 | `cards/000006/files/3310_ruby_8278.zip::heibon.txt` | 4 |
| 000006_3311 | `cards/000006/files/3311_ruby_7023.zip::igonjyo_izoku_zengosaku.txt` | 4 |
| 000006_382 | `cards/000006/files/382_ruby_22429.zip::watashiwa_kaigihada.txt` | 4 |

## Colophon Samples

| work_id | label | colophon_lines |
|---|---|---|
| 000005_53194 | `cards/000005/files/53194_ruby_44732.zip::hatsukoi.txt` | 7 |
| 000005_55215 | `cards/000005/files/55215_ruby_49076.zip::roshiano_kotoba.txt` | 5 |
| 000005_55216 | `cards/000005/files/55216_ruby_49074.zip::asuwa_asuwa.txt` | 5 |
| 000005_55217 | `cards/000005/files/55217_ruby_49075.zip::isso.txt` | 5 |
| 000005_5 | `cards/000005/files/5_ruby_21311.zip::aibiki.txt` | 6 |
| 000006_1868 | `cards/000006/files/1868_ruby_22436.zip::shosetsu_soron.txt` | 6 |
| 000006_1869 | `cards/000006/files/1869_ruby_33306.zip::ukigumo.txt` | 6 |
| 000006_3310 | `cards/000006/files/3310_ruby_8278.zip::heibon.txt` | 7 |
| 000006_3311 | `cards/000006/files/3311_ruby_7023.zip::igonjyo_izoku_zengosaku.txt` | 4 |
| 000006_382 | `cards/000006/files/382_ruby_22429.zip::watashiwa_kaigihada.txt` | 6 |

## Unreadable Entries

- `cards/000050/files/57476_etc_58134.zip`
- `cards/000119/files/46429_ruby_26539.txt`
- `cards/000148/files/769_ttz.zip`
- `cards/000879/files/128_ttz.zip`
- `cards/001021/files/49355_etc_58116.zip`
- `cards/001030/files/4812_ruby_14383.txt`
- `cards/001154/files/chihobunkano_shinkensetsu.zip`
- `cards/001248/files/46751_etc_57109.zip`
- `cards/001562/files/56151_ruby_60063.zip`

Note: this list changed shape between Revision 1 and Revision 2 (same
count, 9, different two members) -- see "Revision 2" below.

## Revision 2 (2026-07-12)

**Trigger.** Task 15's C4-gate append-confinement audit compared this
scanner's Revision-1 split summary against the Rust corpus pipeline's own
ground truth (what `ab-check`/Task 14's `source_note` emitter actually
parsed and appended across the C3->C4 dump pair) and found a disagreement
on exactly **6 of the 17886** corpus text entries: Revision 1 reported
`works_with_terminal_provenance = 17733`, `works_without_tail = 153`; the
Rust ground truth (via the confinement audit's `source_note_appended` /
`identical` classes) was `17735` / `151`. Per the binding rule this was a
STOP/BLOCKED controller escalation (`.superpowers/sdd/task-15-report.md`);
Task 15 traced the disagreement to 6 specific corpus text entries and
recommended a Task 12 instrument fix, without patching anything itself
(evidence-only task).

**Root cause -- one defect, two symptoms, plus one independent defect.**
`reports/source-regions/terminal-provenance-split.py`'s `read_entry_text`
used to dispatch zip-vs-plain-text reading by FILENAME EXTENSION
(`.zip` only) rather than by content. The pinned aozorabunko corpus (git
commit `0e9ea3e586eb0aa34039fabfc85a407d2f98b165`, resolved via the
`aozorabunko-corpus` nix derivation) contains files named `*.txt` whose
actual bytes are zip archives -- both a genuinely valid zip (Class B) and
a genuinely truncated/corrupt one (Class A) turn up this way. A second,
independent defect lives in the zip-reading itself: two zip archives in
the corpus have a central directory that disagrees with (Class C, one
entry) or is entirely unparseable relative to (Class C, the other entry)
their own local file header; python's stdlib `zipfile` refuses to read
past that, while the Rust `zip` crate's `by_index_raw` + manual decompress
path (`crates/ab-index/src/index.rs`, `crates/ab-check/src/check.rs` --
the actual dump-producing read path) never cross-checks a
central-directory-sourced CRC-32 or uncompressed-size against the entry's
own local header, and reads both fine.

| Class | Entries | What was wrong | Fix |
|---|---|---|---|
| A | `001030_4812` (`cards/001030/files/4812_ruby_14383.txt`), `000119_46429` (`cards/000119/files/46429_ruby_26539.txt`) | Stray/corrupt duplicate file in the same `cards/<id>/files/` directory as the real entry, named `.txt` but actually holding truncated zip bytes; extension-based dispatch read the raw zip bytes as Shift_JIS garbage, misclassifying `no_tail` on a phantom entry the Rust pipeline never scores at all (`invalid Zip archive: No CDFH found`, excluded from `index.json`'s 17886-entry universe). | Content-sniff (zip magic bytes) for dispatch, mirroring `crates/ab-index/src/index.rs`'s `is_zip_file`; a candidate that sniffs as zip but cannot be opened/decompressed at all (even via the Class-C fallback below) is EXCLUDED (`None`), matching Rust's `push_zip_sources` skip-and-warn. |
| B | `001341_49658` (`cards/001341/files/49658_ruby_70064.txt`), `001585_53484` (`cards/001585/files/53484_ruby_56576.txt`) | Same root cause as Class A, but the zip data is genuinely valid and decompressible, containing a real `底本：` tail; extension-based dispatch missed it entirely (misclassified `no_tail` instead of `terminal_provenance`). | Same content-sniff dispatch fix as Class A recovers these -- standard `zipfile.read()` succeeds once routed correctly, no further fallback needed. |
| C | `001393_50710` (`cards/001393/files/50710_ruby_36965.zip`), `001505_58100` (`cards/001505/files/58100_txt_60357.zip`) | `50710_ruby_36965.zip`'s central directory CRC/uncompressed-size for its one text member disagrees with the entry's own (correct) local file header; `58100_txt_60357.zip`'s EOCD/central directory is not a coherent whole at all (`zipfile.ZipFile()` raises `BadZipFile` before any `ZipInfo` exists), while its wanted member's local file header (at the very front of this 26MB archive) is fully intact. Both were in Task 12's own `unreadable_entries`, silently excluded, when the Rust pipeline reads both fine (correctly finding no tail on both). | A local-header-trusting fallback (`_read_zip_member_bypassing_central_directory`) that scans the file for local file headers directly and decompresses using ONLY the wanted member's own local-header-declared method/sizes, never a central-directory CRC/size -- exactly mirroring the leniency of `by_index_raw` + manual decompress. Tried only after the fully-validated stdlib path fails, so the ~17880 well-formed entries are entirely unaffected. |

**Selection rule, made explicit.** `discover_entries` still walks every
`.zip`/`.txt` file candidate under `cards/*/files/`, exactly like Rust's
`collect_source_files` -- neither implementation deduplicates candidates
at discovery time (a card directory may legitimately hold a ruby and a
non-ruby zip edition, or a stray/corrupt duplicate). The dedup/exclusion
now happens at READ time in both implementations: `read_entry_text`
decides zip-vs-plain by content, and a zip-shaped candidate that cannot
actually be opened/decompressed is excluded rather than silently read as
plain text. This is what makes the scanner's final scored-entry set match
the Rust corpus run's 17886-entry `index.json` universe card-for-card.

**Verification.** All six entries were independently traced against both
a local aozorabunko mirror checked out at the exact pinned commit and the
actual pinned `aozorabunko-corpus` nix-store derivation used on hinoki,
producing byte-identical `read_entry_text`/`find_tail_start` results in
both places and matching Task 15's trace exactly (Class A -> excluded;
Class B -> genuine `terminal_provenance` tail found; Class C -> correctly
`no_tail`). The full-corpus re-run on hinoki (this file's headline
numbers) landed on `works_scanned=17886`,
`works_with_terminal_provenance=17735`, `works_without_tail=151`,
`works_unclassifiable=0` -- exactly the C4 confinement audit's ground
truth (`source_note_appended=17735`, `identical=151`). New binding:
`works_with_terminal_provenance = 17735`, identical (no-tail) count
`= 151`.

**What did NOT change.** The normative boundary rule --
`reports/lib/terminal_provenance.py`'s `classify_tail` state machine,
`PROVENANCE_HEADS`/`COLOPHON_HEADS`, `TAIL_START_MARKER`,
`find_tail_start` -- was not touched by this fix at all; that file has
zero diff in the fix commit. All 15 pre-existing `ClassifyTail`/
`find_tail_start` tests continue to pass unmodified against the same
module, which is direct evidence of byte-for-byte semantic equivalence
for the normative rule. Only entry selection (content-based zip
detection) and zip reading (a local-header-trusting fallback for two
corrupt-central-directory archives) changed, both in
`reports/source-regions/terminal-provenance-split.py`.
