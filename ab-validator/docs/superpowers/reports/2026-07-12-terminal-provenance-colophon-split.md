# Terminal-Provenance / Colophon-Metadata Measurement Split

Verdict: `TERMINAL_PROVENANCE_SPLIT_OK`

- corpus_root: `/nix/store/sdr1imwrxfldvlwzs2d2fhs11vxncgpx-aozorabunko-corpus`
- works_scanned: 17886
- works_with_terminal_provenance: 17733
- works_with_colophon: 17732
- works_without_tail: 153
- works_unclassifiable: 0
- terminal_provenance_lines: 84454
- colophon_lines: 106347
- head_line_hits: 91677
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
- `cards/000148/files/769_ttz.zip`
- `cards/000879/files/128_ttz.zip`
- `cards/001021/files/49355_etc_58116.zip`
- `cards/001154/files/chihobunkano_shinkensetsu.zip`
- `cards/001248/files/46751_etc_57109.zip`
- `cards/001393/files/50710_ruby_36965.zip`
- `cards/001505/files/58100_txt_60357.zip`
- `cards/001562/files/56151_ruby_60063.zip`
