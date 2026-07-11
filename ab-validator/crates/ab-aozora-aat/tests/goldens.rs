//! Committed, HAND-VERIFIED goldens (Phase 3 rotation B). These replaced
//! `tests/reference_parity.rs`: the frozen-adapter byte-parity claim ended
//! at the span rotation by design (legacy adapter emits sanitized-offset
//! spans with synthesized lines). Spans in these files were verified
//! against the decoded sources with reports/aat-fidelity/
//! verify-golden-spans.py plus manual line-number checks — see the
//! rotation B confinement report. Hermetic: plain `cargo test` builds
//! carry `(git unknown)`, which these goldens embed.
//!
//! ## Verification summary (Task 15)
//!
//! `verify-golden-spans.py` walks every node with both `value` (a string)
//! and `span`, and asserts `decoded[byte_start:byte_end] == value` plus
//! `line_start == 1 + newlines-before(byte_start)`, against the source
//! independently re-decoded in Python (`utf-8-sig` / `utf-8` / `shift_jis`).
//!
//! | sample                       | OK | FAIL | FAIL verdict                              |
//! |-------------------------------|----|------|-------------------------------------------|
//! | plain-ascii.txt               | 1  | 0    | n/a                                        |
//! | broken-ruby-utf8.txt          | 0  | 0    | n/a (only node is a `raw` node: no `value`)|
//! | full-markup-utf8.txt          | 2  | 1    | projection (CRLF→LF text normalization)    |
//! | full-markup-shift_jis.txt     | 2  | 1    | projection (CRLF→LF text normalization)    |
//!
//! The single FAIL in each markup sample is the trailing-newline `text`
//! node after the gaiji marker on line 8 (`byte_start:303, byte_end:305`,
//! `value:"\n"`, raw slice `"\r\n"`). This is NOT a span bug: the sanitize
//! stage's CR/LF normalization (`ab-aozora-pipeline/src/lexer/sanitize.rs`,
//! `normalize_line_endings_core`, doc comment ~line 649) collapses every
//! `\r\n` (and lone `\r`) to a single `\n` in the text the parser actually
//! sees, while the byte-offset map (`MapEdit`, tested at
//! `sanitize.rs:1276-1277`: `to_source_end(4) == 8` for a `\r\n`→`\n` edit)
//! deliberately expands the corresponding span back to the full 2-byte
//! `\r\n` run in decoded-source coordinates, so spans stay contiguous and
//! never drop source bytes. `value` reflects the normalized text the
//! parser operated on; `span` reflects raw source coverage. Same pattern
//! recurs, unflagged by the helper because it's on a `raw` node's
//! `source` field rather than `value`, in `broken-ruby-utf8.txt`'s sole
//! node (`source:"壊れた《ルビ\n"`, span 0..20 covering the full
//! `"壊れた《ルビ\r\n"`).
//!
//! ## Manual line-number spot checks (independent of the helper)
//!
//! - `plain-ascii.txt`: decoded text is `"plain ascii only\n"` (17 bytes,
//!   one line). `byte_start=0` gives `line_start=1`; `byte_end=17` gives
//!   `line_of(16)=1` (no `\n` before byte 16), so `line_end=1`;
//!   `slice[0:17]` equals `value` exactly.
//! - `broken-ruby-utf8.txt`: decoded text is `"壊れた《ルビ\r\n"` (20
//!   bytes). The sole `\n` sits at byte 19, so `line_starts=[0,20]`;
//!   `byte_start=0` gives `line=1`, `byte_end=20` gives `line_of(19)=1`.
//!   `slice[0:20]` equals the full raw source (the CRLF projection above).
//! - `full-markup-utf8.txt` / `full-markup-shift_jis.txt` (both decode to
//!   the identical Japanese text; only the source encoding differs):
//!   line 8 (`"吾輩《わがはい》は猫である。※［＃「けものへん＋苗」、
//!   第3水準1-87-64］\r"`) starts at decoded byte 202 — matches the ruby
//!   node's `byte_start:202, line_start:8`. `slice[226:244]` ==
//!   `"は猫である。"`, matching the following text node's `value` exactly
//!   (line 8). Line 9 (`"［＃５字下げ］一［＃「一」は中見出し］\r"`)
//!   starts at decoded byte 305; the 21-byte prefix `"［＃５字下げ］"`
//!   puts `"一"` at byte 326 — matches the heading node's
//!   `byte_start:326, line_start:9` (a line > 1 check, present in both
//!   samples including the SJIS one).
use std::fs;

#[test]
fn aat_output_matches_hand_verified_goldens() {
    let data = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/data");
    for entry in fs::read_dir(data).unwrap() {
        let path = entry.unwrap().path();
        let name = path.file_name().unwrap().to_str().unwrap();
        let golden = format!("{}/tests/goldens/{name}.expected.json",
                             env!("CARGO_MANIFEST_DIR"));
        let expected = fs::read(&golden).unwrap();
        let actual = ab_aozora_aat::aat_json_from_bytes(&fs::read(&path).unwrap()).unwrap();
        assert_eq!(actual, expected, "golden drift: {name}");
    }
}
